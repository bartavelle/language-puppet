module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Init
import Puppet.DSL.Types
import Puppet.NativeTypes
import Puppet.Interpreter.Functions
import Puppet.Interpreter.Types

import Data.List
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)
import Data.Either (lefts, rights)
import Text.Parsec.Pos
import Control.Monad (foldM)
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set

throwPosError msg = do
    pos <- getPos
    throwError (msg ++ " at " ++ show pos)

-- Int handling stuff
isInt :: String -> Bool
isInt = and . map isDigit
readint :: String -> CatalogMonad Integer
readint x = if (isInt x)
    then return (read x)
    else do
        pos <- getPos
        throwError ("Expected an integer instead of '" ++ x ++ "' at " ++ show pos)

modifyScope     f sc = sc { curScope       = f $ curScope sc }
modifyVariables f sc = sc { curVariables   = f $ curVariables sc }
modifyClasses   f sc = sc { curClasses     = f $ curClasses sc }
modifyDefaults  f sc = sc { curDefaults    = f $ curDefaults sc }
incrementResId    sc = sc { curResId       = (curResId sc) + 1 }
setStatePos  npos sc = sc { curPos         = npos }
modifyNestedTopLvl f sc = sc { netstedtoplevels = f $ netstedtoplevels sc }
emptyDefaults     sc = sc { curDefaults    = [] }
pushWarning     t sc = sc { getWarnings    = (getWarnings sc) ++ [t] }
pushCollect   r   sc = sc { curCollect     = r : (curCollect sc) }
pushUnresRel  r   sc = sc { unresolvedRels = r : (unresolvedRels sc) }

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> Facts -> IO (Either String FinalCatalog, [String])
getCatalog getstatements nodename facts = do
    let convertedfacts = Map.map
            (\fval -> (Right fval, initialPos "FACTS"))
            facts
    (output, finalstate) <- runStateT ( runErrorT ( computeCatalog getstatements nodename ) ) (ScopeState ["::"] convertedfacts Map.empty [] 1 (initialPos "dummy") Map.empty getstatements [] [] [])
    case output of
        Left x -> return (Left x, getWarnings finalstate)
        Right y -> return (output, getWarnings finalstate)

computeCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> CatalogMonad FinalCatalog
computeCatalog getstatements nodename = do
    nodestatements <- liftIO $ getstatements TopNode nodename
    case nodestatements of
        Left x -> throwError x
        Right nodestmts -> evaluateStatements nodestmts >>= finalResolution

finalizeResource :: CResource -> CatalogMonad (ResIdentifier, RResource)
finalizeResource (CResource cid cname ctype cparams _ cpos) = do
    setPos cpos
    rname <- resolveGeneralString cname
    rparams <- mapM (\(a,b) -> do { ra <- resolveGeneralString a; rb <- resolveGeneralValue b; return (ra,rb); }) cparams
    -- add collected relations
    -- TODO
    let rrelations = []
    return $ ((ctype, rname), RResource cid rname ctype rparams rrelations cpos)

collectionChecks :: CResource -> CatalogMonad CResource
collectionChecks res = do
    if (crvirtuality res == Normal)
        then return res
        else do
            isCollected <- get >>= return . curCollect >>= mapM (\x -> x res)
            case (or isCollected) of
                True -> return (res { crvirtuality = Normal })
                False -> return res

finalResolution :: Catalog -> CatalogMonad FinalCatalog
finalResolution cat = do
    collected <- mapM collectionChecks cat
    let (real,  allvirtual)  = partition (\x -> crvirtuality x == Normal)  collected
        (virtual,  exported) = partition (\x -> crvirtuality x == Virtual)  allvirtual
    resolved <- mapM finalizeResource real >>= return . Map.fromList
    get >>= return . unresolvedRels >>= liftIO . (mapM print)
    return resolved

getstatement :: TopLevelType -> String -> CatalogMonad Statement
getstatement qtype name = do
    curcontext <- get
    let stmtsfunc = getStatementsFunction curcontext
    estatement <- liftIO $ stmtsfunc qtype name
    case estatement of
        Left x -> throwError x
        Right y -> return y

-- State alteration functions
pushScope name  = modify (modifyScope (\x -> [name] ++ x))
pushDefaults name  = modify (modifyDefaults (\x -> [name] ++ x))
popScope        = modify (modifyScope tail)
getScope        = do
    scope <- get >>= return . curScope
    if (null scope)
        then throwError "empty scope, shouldn't happen"
        else return $ head scope
addLoaded name position = modify (modifyClasses (Map.insert name position))
getNextId = do
    curscope <- get
    put $ incrementResId curscope
    return (curResId curscope)
setPos pos = modify (setStatePos pos)
getPos = get >>= return . curPos
putVariable k v = do
    curscope <- getScope
    modify (modifyVariables (Map.insert (curscope ++ "::" ++ k) v))
getVariable vname = get >>= return . Map.lookup vname . curVariables
addNestedTopLevel rtype rname rstatement = modify( modifyNestedTopLvl (Map.insert (rtype, rname) rstatement) )
addWarning nwrn   = modify (pushWarning nwrn)
addCollect ncol   = modify (pushCollect ncol)
-- this pushes the relations only if they exist
-- the parameter is of the form
-- ( [dstrelations], srcresource, type, pos )
addUnresRel ncol@(rels, _, _, _)  = do
    if null rels
        then return ()
        else modify (pushUnresRel ncol)

-- finds out if a resource name refers to a define
checkDefine :: String -> CatalogMonad (Maybe Statement)
checkDefine dname = if Map.member dname nativeTypes
  then return Nothing
  else do
    curstate <- get
    let ntop = netstedtoplevels curstate
        getsmts = getStatementsFunction curstate
        check = Map.lookup (TopDefine, dname) ntop
    case check of
        Just x -> return $ Just x
        Nothing -> do
            def1 <- liftIO $ getsmts TopDefine dname
            case def1 of
                Left err -> do
                    pos <- getPos
                    throwError ("Could not find the definition of " ++ dname ++ " at " ++ show pos)
                Right s -> return $ Just s

partitionParamsRelations :: [(GeneralString, GeneralValue)] -> GeneralValue -> ([(GeneralString, GeneralValue)], [(LinkType, GeneralValue, GeneralValue)])
partitionParamsRelations rparameters resname = (realparams, relations)
    where   realparams = filteredparams
            relations = concatMap convertrelation filteredrelations
            convertrelation :: (GeneralString, GeneralValue) -> [(LinkType, GeneralValue, GeneralValue)]
            convertrelation (reltype, Right (ResolvedArray rs))         = concatMap (\x -> convertrelation (reltype, Right x)) rs
            convertrelation (reltype, Right (ResolvedRReference rt rv)) = [(fromJust $ getRelationParameterType reltype, Right $ ResolvedString rt, Right rv)]
            convertrelation (reltype, Right (ResolvedString "undef"))   = [(fromJust $ getRelationParameterType reltype, Right $ ResolvedString "undef", Right $ ResolvedString "undef")]
            convertrelation x = error (show x)
            (filteredrelations, filteredparams) = partition (isJust . getRelationParameterType . fst) rparameters -- filters relations with actual parameters

-- TODO check whether parameters changed
checkLoaded name = do
    curscope <- get
    case (Map.lookup name (curClasses curscope)) of
        Nothing      -> return False
        Just thispos -> return True

-- apply default values to a resource
applyDefaults :: CResource -> CatalogMonad CResource
applyDefaults res = do
    defs <- get >>= return . curDefaults 
    foldM applyDefaults' res defs

applyDefaults' :: CResource -> Statement -> CatalogMonad CResource            
applyDefaults' r@(CResource id rname rtype rparams rvirtuality rpos) d@(ResourceDefault dtype defs dpos) = do
    srname <- resolveGeneralString rname
    rdefs <- mapM (\(a,b) -> do { ra <- tryResolveExpressionString a; rb <- tryResolveExpression b; return (ra, rb); }) defs
    rrparams <- mapM (\(a,b) -> do { ra <- resolveGeneralString a; rb <- resolveGeneralValue b; return (ra, rb); }) rparams
    let (nparams, nrelations) = mergeParams rparams rdefs False srname
    if (dtype == rtype)
        then do
            addUnresRel (nrelations, (rtype, Right srname), UDefault, dpos)
            return $ CResource id rname rtype nparams rvirtuality rpos
        else return r
applyDefaults' r@(CResource id rname rtype rparams rvirtuality rpos) (ResourceOverride dtype dname defs dpos) = do
    srname <- resolveGeneralString rname
    sdname <- resolveExpressionString dname
    rdefs <- mapM (\(a,b) -> do { ra <- tryResolveExpressionString a; rb <- tryResolveExpression b; return (ra, rb); }) defs
    let (nparams, nrelations) = mergeParams rparams rdefs True srname
    if ((dtype == rtype) && (srname == sdname))
        then do
            addUnresRel (nrelations, (rtype, Right srname), UDefault, dpos)
            return $ CResource id rname rtype nparams rvirtuality rpos
        else return r

-- merge defaults and actual parameters depending on the override flag
mergeParams :: [(GeneralString, GeneralValue)] -> [(GeneralString, GeneralValue)] -> Bool -> String -> ([(GeneralString, GeneralValue)], [(LinkType, GeneralValue, GeneralValue)])
mergeParams srcparams defs override rname = let
    (dstparams, dstrels) = partitionParamsRelations defs (Right $ ResolvedString rname)
    srcprm = Map.fromList srcparams
    dstprm = Map.fromList dstparams
    prm = if override
        then Map.toList $ Map.union dstprm srcprm
        else Map.toList $ Map.union srcprm dstprm
    in (prm, dstrels)

-- The actual meat

evaluateDefine :: CResource -> CatalogMonad [CResource]
evaluateDefine r@(CResource id rname rtype rparams rvirtuality rpos) = do
    isdef <- checkDefine rtype
    case (rvirtuality, isdef) of
        (Normal, Just (DefineDeclaration dtype args dstmts dpos)) -> do
            oldpos <- getPos
            setPos dpos
            pushScope $ "#DEFINE#" ++ dtype
            -- add variables
            rrparams <- mapM (\(gs, gv) -> do { rgs <- resolveGeneralString gs; rgv <- tryResolveGeneralValue gv; return (rgs, (rgv, dpos)); }) rparams
            let expr = gs2gv rname
                mparams = Map.fromList rrparams
            putVariable "title" (expr, rpos)
            putVariable "name" (expr, rpos)
            mapM (loadClassVariable rpos mparams) args
 
            -- parse statements
            res <- mapM (evaluateStatements) dstmts
            nres <- handleDelayedActions (concat res)
            popScope
            return nres
        _ -> return [r]
        

-- handling delayed actions (such as defaults)
handleDelayedActions :: Catalog -> CatalogMonad Catalog
handleDelayedActions res = do
    dres <- mapM applyDefaults res >>= mapM evaluateDefine >>= return . concat
    modify emptyDefaults
    return dres

-- node
evaluateStatements :: Statement -> CatalogMonad Catalog
evaluateStatements (Node name stmts position) = do
    setPos position
    res <- mapM (evaluateStatements) stmts
    nres <- handleDelayedActions (concat res)
    return nres

-- include
evaluateStatements (Include includename position) = setPos position >> getstatement TopClass includename >>= evaluateStatements
evaluateStatements x@(ClassDeclaration _ _ _ _ _) = evaluateClass x Map.empty
evaluateStatements n@(DefineDeclaration dtype dargs dstatements dpos) = do
    addNestedTopLevel TopDefine dtype n
    return []
evaluateStatements (ConditionalStatement exprs pos) = do
    setPos pos
    trues <- filterM (\(expr, _) -> resolveBoolean (Left expr)) exprs
    case trues of
        ((_,stmts):xs) -> mapM evaluateStatements stmts >>= return . concat
        _ -> return []

evaluateStatements x@(Resource rtype rname parameters virtuality position) = do
    setPos position
    case rtype of
        -- checks whether we are handling a parametrized class
        "class" -> do
            rparameters <- mapM (\(a,b) -> do { pa <- resolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            classname <- resolveExpressionString rname
            topstatement <- getstatement TopClass classname
            let classparameters = Map.fromList $ map (\(pname, pvalue) -> (pname, (pvalue, position))) rparameters
            evaluateClass topstatement classparameters
        _ -> do
            resid <- getNextId
            rparameters <- mapM (\(a,b) -> do { pa <- tryResolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            rrname <- tryResolveGeneralValue (generalizeValueE rname)
            ername <- tryResolveExpression rname
            srname <- tryResolveExpressionString rname
            let (realparams, relations) = partitionParamsRelations rparameters rrname
            -- push all the relations
            addUnresRel (relations, (rtype, srname), UNormal, position)
            return [CResource resid srname rtype realparams virtuality position]

evaluateStatements x@(ResourceDefault _ _ _ ) = do
    pushDefaults x
    return []
evaluateStatements x@(ResourceOverride _ _ _ _) = do
    pushDefaults x
    return []
evaluateStatements (DependenceChain (srctype, srcname) (dsttype, dstname) position) = do
    setPos position
    gdstname <- tryResolveExpression dstname
    gsrcname <- tryResolveExpressionString srcname
    addUnresRel ( [(RRequire, Right $ ResolvedString dsttype, gdstname)], (srctype, gsrcname), UPlus, position )
    return []
-- <<| |>>
evaluateStatements (ResourceCollection rtype expr overrides position) = do
    setPos position
    dummy <- if null overrides
        then return 1
        else throwPosError "Collection overrides not handled"
    addWarning "TODO : ResourceCollection not handled!"
    return []
-- <| |>
evaluateStatements (VirtualResourceCollection rtype expr overrides position) = do
    setPos position
    dummy <- if null overrides
        then return 1
        else throwPosError "Collection overrides not handled"
    addWarning "TODO : VirtualResourceCollection not handled!"
    return []

evaluateStatements (VariableAssignment vname vexpr position) = do
    setPos position
    rvexpr <- tryResolveExpression vexpr
    putVariable vname (rvexpr, position)
    return []

evaluateStatements (MainFunctionCall fname fargs position) = do
    setPos position
    rargs <- mapM resolveExpression fargs
    executeFunction fname rargs

evaluateStatements x = throwError ("Can't evaluate " ++ (show x))

-- function used to load defines / class variables into the global context
loadClassVariable :: SourcePos -> Map.Map String (GeneralValue, SourcePos) -> (String, Maybe Expression) -> CatalogMonad ()
loadClassVariable position inputs (paramname, defaultvalue) = do
    let inputvalue = Map.lookup paramname inputs
    (v, vpos) <- case (inputvalue, defaultvalue) of
        (Just x , _      ) -> return x
        (Nothing, Just y ) -> return (Left y, position)
        (Nothing, Nothing) -> throwError $ "Must define parameter " ++ paramname ++ " at " ++ (show position)
    rv <- tryResolveGeneralValue v
    putVariable paramname (rv, vpos)
    return ()

-- class
-- ClassDeclaration String (Maybe String) [(String, Maybe Expression)] [Statement] SourcePos
-- nom, heritage, parametres, contenu
evaluateClass :: Statement -> Map.Map String (GeneralValue, SourcePos) -> CatalogMonad Catalog
evaluateClass (ClassDeclaration classname inherits parameters statements position) inputparams = do
    isloaded <- checkLoaded classname
    if isloaded
        then return []
        else do
        resid <- getNextId  -- get this resource id, for the dummy class that will be used to handle relations
        oldpos <- getPos    -- saves where we were at class declaration so that we known were the class was included
        setPos position    
        pushScope classname -- sets the scope
        mapM (loadClassVariable position inputparams) parameters -- add variables for parametrized classes
        
        -- load inherited classes
        inherited <- case inherits of
            Just parentclass -> do
                mystatement <- getstatement TopClass parentclass
                case mystatement of
                    ClassDeclaration _ ni np ns no -> evaluateClass (ClassDeclaration classname ni np ns no) Map.empty
                    _ -> throwError "Should not happen : TopClass return something else than a ClassDeclaration in evaluateClass"
            Nothing -> return []
        addLoaded classname oldpos

        -- parse statements
        res <- mapM (evaluateStatements) statements
        nres <- handleDelayedActions (concat res)
        mapM (addClassDependency classname) nres    -- this adds a dummy dependency to this class
                                                    -- for all resources that do not already depend on a class
                                                    -- this is probably not puppet perfect with resources that
                                                    -- depend explicitely on a class
        popScope
        return $
            [CResource resid (Right classname) "class" [] Normal position]
            ++ inherited
            ++ nres

addClassDependency :: String -> CResource -> CatalogMonad ()
addClassDependency cname (CResource _ rname rtype _ _ pos) = addUnresRel (
    [(RRequire, Right $ ResolvedString "class", Right $ ResolvedString cname)]
    , (rtype, rname)
    , UPlus, pos)

tryResolveExpression :: Expression -> CatalogMonad GeneralValue
tryResolveExpression e = tryResolveGeneralValue (Left e)

tryResolveGeneralValue :: GeneralValue -> CatalogMonad GeneralValue
tryResolveGeneralValue n@(Right _) = return n
tryResolveGeneralValue n@(Left BTrue) = return $ Right $ ResolvedBool True
tryResolveGeneralValue n@(Left BFalse) = return $ Right $ ResolvedBool False
tryResolveGeneralValue n@(Left (Value x)) = tryResolveValue x
tryResolveGeneralValue n@(Left (ResolvedResourceReference _ _)) = return n
tryResolveGeneralValue n@(Left (Error x)) = do
    pos <- getPos
    throwError (x ++ " at " ++ show pos)
tryResolveGeneralValue n@(Left (ConditionalValue checkedvalue (Value (PuppetHash (Parameters hash))))) = do
    rcheck <- resolveExpression checkedvalue
    rhash <- mapM (\(vn, vv) -> do { rvn <- resolveExpression vn; rvv <- resolveExpression vv; return (rvn, rvv) }) hash
    case (filter (\(a,_) -> (a == ResolvedString "default") || (compareRValues a rcheck)) rhash) of
        [] -> do
            pos <- getPos
            throwError ("No value could be selected at " ++ show pos ++ " when comparing to " ++ show rcheck)
        ((_,x):xs) -> return $ Right x
tryResolveGeneralValue (Left (EqualOperation a b)) = do
    ra <- tryResolveExpression a
    rb <- tryResolveExpression b
    case (ra, rb) of
        (Right rra, Right rrb) -> return $ Right $ ResolvedBool $ compareRValues rra rrb
        _ -> return $ Left $ EqualOperation a b
tryResolveGeneralValue n@(Left (OrOperation a b)) = do
    ra <- tryResolveBoolean $ Left a
    rb <- tryResolveBoolean $ Left b
    case (ra, rb) of
        (Right (ResolvedBool rra), Right (ResolvedBool rrb)) -> return $ Right $ ResolvedBool $ rra || rrb
        _ -> return n
tryResolveGeneralValue n@(Left (AndOperation a b)) = do
    ra <- tryResolveBoolean $ Left a
    rb <- tryResolveBoolean $ Left b
    case (ra, rb) of
        (Right (ResolvedBool rra), Right (ResolvedBool rrb)) -> return $ Right $ ResolvedBool $ rra && rrb
        _ -> return n
tryResolveGeneralValue n@(Left (NotOperation x)) = do
    rx <- tryResolveBoolean $ Left x
    case rx of
        Right (ResolvedBool b) -> return $ Right $ ResolvedBool $ (not b)
        _ -> return rx
tryResolveGeneralValue (Left (DifferentOperation a b)) = do
    res <- tryResolveGeneralValue (Left (EqualOperation a b))
    case res of
        Right (ResolvedBool x) -> return (Right $ ResolvedBool $ not x)
        _ -> return res
tryResolveGeneralValue (Left (LookupOperation a b)) = do
    ra <- tryResolveExpression a
    rb <- tryResolveExpressionString b
    pos <- getPos
    case (ra, rb) of
        (Right (ResolvedArray ar), Right num) -> do
            nnum <- readint num
            let nnum = read num :: Int
            if(length ar >= nnum)
                then throwError ("Invalid array index " ++ num ++ " at " ++ show pos)
                else return $ Right (ar !! nnum)
        (Right (ResolvedHash ar), Right idx) -> do
            let filtered = filter (\(a,_) -> a == idx) ar
            case filtered of
                [] -> throwError "TODO"
                [(_,x)] -> return $ Right $ x
        (_, Left y) -> throwError ("Could not resolve index " ++ show y ++ " at " ++ show pos)
        (Left x, _) -> throwError ("Could not resolve lookup " ++ show x ++ " at " ++ show pos)

            
tryResolveGeneralValue e = do
    p <- getPos
    throwError ("tryResolveGeneralValue not implemented at " ++ show p ++ " for " ++ show e)

resolveGeneralValue :: GeneralValue -> CatalogMonad ResolvedValue
resolveGeneralValue e = do
    x <- tryResolveGeneralValue e
    case x of
        Left n -> do
            pos <- getPos
            throwError ("Could not resolveGeneralValue " ++ show n ++ " at " ++ show pos)
        Right p -> return p

resolveGeneralValueString :: GeneralValue -> CatalogMonad String
resolveGeneralValueString e = do
    resolveGeneralValue e >>= rstring

tryResolveExpressionString :: Expression -> CatalogMonad GeneralString
tryResolveExpressionString s = do
    resolved <- tryResolveExpression s
    case resolved of
        Right e -> rstring e >>= return . Right
        Left  e -> return $ Left e

rstring :: ResolvedValue -> CatalogMonad String
rstring resolved = case resolved of
        ResolvedString s -> return s
        ResolvedInt i    -> return (show i)
        e                -> do
            p <- getPos
            throwError ("'" ++ show e ++ "' will not resolve to a string at " ++ show p)

resolveExpression :: Expression -> CatalogMonad ResolvedValue
resolveExpression e = do
    resolved <- tryResolveExpression e
    case resolved of
        Right r -> return r
        Left  x -> do
            p <- getPos
            throwError ("Can't resolve expression '" ++ show x ++ "' at " ++ show p)


resolveExpressionString :: Expression -> CatalogMonad String
resolveExpressionString x = do
    resolved <- resolveExpression x
    case resolved of
        ResolvedString s -> return s
        ResolvedInt i -> return (show i)
        e -> do
            p <- getPos
            throwError ("Can't resolve expression '" ++ show e ++ "' to a string at " ++ show p)

tryResolveValue :: Value -> CatalogMonad GeneralValue
tryResolveValue (Literal x) = return $ Right $ ResolvedString x
tryResolveValue (Integer x) = return $ Right $ ResolvedInt x

tryResolveValue n@(ResourceReference rtype vals) = do
    rvals <- tryResolveExpression vals
    case rvals of
        Right resolved -> return $ Right $ ResolvedRReference rtype resolved
        _              -> return $ Left $ Value n

tryResolveValue n@(VariableReference vname) = do
    -- TODO check scopes !!!
    var <- getVariable vname
    case var of
        Just (Left e, pos) -> tryResolveExpression e
        Just (Right r, pos) -> return $ Right r
        Nothing -> do
            -- TODO : check that there are no "::"
            curscp <- getScope
            let varnamescp = curscp ++ "::" ++ vname
            varscp <- getVariable varnamescp
            case varscp of
                Just (Left e, pos) -> tryResolveExpression e
                Just (Right r, pos) -> return $ Right r
                Nothing -> do
                    pos <- getPos
                    addWarning ("Could not resolveValue " ++ varnamescp ++ " at " ++ show pos)
                    return $ Left $ Value $ VariableReference varnamescp

tryResolveValue n@(Interpolable x) = do
    resolved <- mapM tryResolveValueString x
    if (null $ lefts resolved)
        then return $ Right $ ResolvedString $ concat $ rights resolved
        else return $ Left $ Value n

tryResolveValue n@(PuppetHash (Parameters x)) = do
    resolvedKeys <- mapM (tryResolveExpressionString . fst) x
    resolvedValues <- mapM (tryResolveExpression . snd) x
    if ((null $ lefts resolvedKeys) && (null $ lefts resolvedValues))
        then return $ Right $ ResolvedHash $ zip (rights resolvedKeys) (rights resolvedValues)
        else return $ Left $ Value n

tryResolveValue n@(PuppetArray expressions) = do
    resolvedExpressions <- mapM tryResolveExpression expressions
    if (null $ lefts resolvedExpressions)
        then return $ Right $ ResolvedArray $ rights resolvedExpressions
        else return $ Left $ Value n

-- TODO
tryResolveValue n@(FunctionCall "fqdn_rand" args) = if (null args)
    then do
        pos <- getPos
        throwError ("Empty argument list in fqdn_rand call at " ++ show pos)
    else do
        nargs <- mapM resolveExpressionString args
        max <- readint (head nargs)
        fqdn_rand max (tail nargs) >>= return . Right . ResolvedInt
tryResolveValue n@(FunctionCall "jbossmem" _) = return $ Right $ ResolvedString "512"
tryResolveValue n@(FunctionCall "template" _) = return $ Right $ ResolvedString "TODO"
tryResolveValue n@(FunctionCall "inline_template" _) = return $ Right $ ResolvedString "TODO"
tryResolveValue n@(FunctionCall "regsubst" [str, src, dst, flags]) = do
    rstr   <- tryResolveExpressionString str
    rsrc   <- tryResolveExpressionString src
    rdst   <- tryResolveExpressionString dst
    rflags <- tryResolveExpressionString flags
    case (rstr, rsrc, rdst, rflags) of
        (Right sstr, Right ssrc, Right sdst, Right sflags) -> regsubst sstr ssrc sdst sflags >>= return . Right . ResolvedString
tryResolveValue n@(FunctionCall "regsubst" [str, src, dst]) = tryResolveValue (FunctionCall "regsubst" [str, src, dst, Value $ Literal ""])
tryResolveValue n@(FunctionCall "regsubst" args) = do
    pos <- getPos
    throwError ("Bad argument count for regsubst " ++ show args ++ " at " ++ show pos)
       
tryResolveValue n@(FunctionCall "file" _) = return $ Right $ ResolvedString "TODO"
tryResolveValue n@(FunctionCall fname _) = do
    pos <- getPos
    throwError ("Function " ++ fname ++ " not implemented at " ++ show pos)

tryResolveValue x = do
    pos <- getPos
    throwError ("tryResolveValue not implemented at " ++ show pos ++ " for " ++ show x)

tryResolveValueString :: Value -> CatalogMonad GeneralString
tryResolveValueString x = do
    r <- tryResolveValue x
    case r of
        Right (ResolvedString v) -> return $ Right v
        Right (ResolvedInt    i) -> return $ Right (show i)
        Right v                  -> throwError ("Can't resolve valuestring for " ++ show x)
        Left  v                  -> return $ Left v

resolveValue :: Value -> CatalogMonad ResolvedValue
resolveValue v = do
    res <- tryResolveValue v
    case res of
        Left x -> do
            pos <- getPos
            throwError ("Could not resolve the value " ++ show x ++ " at " ++ show pos)
        Right y -> return y

resolveValueString :: Value -> CatalogMonad String
resolveValueString v = do
    res <- tryResolveValueString v
    case res of
        Left x -> do
            pos <- getPos
            throwError ("Could not resolve the value " ++ show x ++ " to a string at " ++ show pos)
        Right y -> return y

getRelationParameterType :: GeneralString -> Maybe LinkType
getRelationParameterType (Right "require" ) = Just RRequire
getRelationParameterType (Right "notify"  ) = Just RNotify
getRelationParameterType (Right "before"  ) = Just RBefore
getRelationParameterType (Right "register") = Just RRegister
getRelationParameterType _                  = Nothing

-- this function saves a new condition for collection
pushRealize :: ResolvedValue -> CatalogMonad ()
pushRealize (ResolvedRReference rtype (ResolvedString rname)) = do
    let myfunction :: CResource -> CatalogMonad Bool
        myfunction = (\(CResource _ crname crtype _ _ _) -> do
            srname <- resolveGeneralString crname
            return ((srname == rname) && (crtype == rtype))
            )
    addCollect myfunction
    return ()
pushRealize (ResolvedRReference rtype x) = throwPosError (show x ++ " was not resolved to a string")
pushRealize x                            = throwPosError ("A reference was expected instead of " ++ show x)

executeFunction :: String -> [ResolvedValue] -> CatalogMonad Catalog
executeFunction "fail" [ResolvedString errmsg] = do
    pos <- getPos
    throwError ("Error: " ++ errmsg ++ " at " ++ show pos)
executeFunction "fail" args = do
    pos <- getPos
    throwError ("Error: " ++ show args ++ " at " ++ show pos)
executeFunction "realize" rlist = mapM pushRealize rlist >> return []
executeFunction a b = do
    pos <- getPos
    addWarning $ "Function " ++ a ++ "(" ++ show b ++ ") not handled at " ++ show pos
    return []

compareRValues :: ResolvedValue -> ResolvedValue -> Bool
compareRValues (ResolvedString a) (ResolvedInt b) = compareRValues (ResolvedInt b) (ResolvedString a)
compareRValues (ResolvedInt a) (ResolvedString b) | isInt b = a == (read b)
                                                  | otherwise = False
compareRValues a b = a == b

-- used to handle the special cases when we know it is a boolean context
tryResolveBoolean :: GeneralValue -> CatalogMonad GeneralValue
tryResolveBoolean v = do
    rv <- tryResolveGeneralValue v
    case rv of
        Right (ResolvedString "") -> return $ Right $ ResolvedBool False
        Right (ResolvedString _) -> return $ Right $ ResolvedBool True
        Right (ResolvedInt 0) -> return $ Right $ ResolvedBool False
        Right (ResolvedInt _) -> return $ Right $ ResolvedBool True
        Left (Value (VariableReference _)) -> return $ Right $ ResolvedBool False
        Left (EqualOperation (Value (VariableReference _)) (Value (Literal ""))) -> return $ Right $ ResolvedBool True -- case where a variable was not resolved and compared to the empty string
        Left (EqualOperation (Value (VariableReference _)) (Value (Literal "true"))) -> return $ Right $ ResolvedBool False -- case where a variable was not resolved and compared to the string "true"
        Left (EqualOperation (Value (VariableReference _)) (Value (Literal "false"))) -> return $ Right $ ResolvedBool True -- case where a variable was not resolved and compared to the string "false"
        _ -> return rv
 
resolveBoolean :: GeneralValue -> CatalogMonad Bool
resolveBoolean v = do
    rv <- tryResolveBoolean v
    case rv of
        Right (ResolvedBool x) -> return x
        n -> do
            pos <- getPos
            throwError ("Could not resolve " ++ show rv ++ " as a boolean at " ++ show pos)

resolveGeneralString :: GeneralString -> CatalogMonad String
resolveGeneralString (Right x) = return x
resolveGeneralString (Left y) = resolveExpressionString y

gs2gv :: GeneralString -> GeneralValue
gs2gv (Left e)  = Left e
gs2gv (Right s) = Right $ ResolvedString s
