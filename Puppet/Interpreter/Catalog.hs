module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.DSL.Types
import Puppet.NativeTypes
import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Functions
import Puppet.Interpreter.Types

import Data.List
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)
import Data.Either (lefts, rights)
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set

qualified []  = False
qualified str = case isPrefixOf "::" str of
    False -> qualified (tail str)
    True  -> True

throwPosError msg = do
    p <- getPos
    throwError (msg ++ " at " ++ show p)

-- Int handling stuff
isInt :: String -> Bool
isInt = and . map isDigit
readint :: String -> CatalogMonad Integer
readint x = if (isInt x)
    then return (read x)
    else throwPosError $ "Expected an integer instead of '" ++ x

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

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> (String -> String -> [(String, GeneralValue)] -> IO (Either String String)) -> String -> Facts -> IO (Either String FinalCatalog, [String])
getCatalog getstatements gettemplate nodename facts = do
    let convertedfacts = Map.map
            (\fval -> (Right fval, initialPos "FACTS"))
            facts
    (output, finalstate) <- runStateT ( runErrorT ( computeCatalog getstatements nodename ) ) (ScopeState ["::"] convertedfacts Map.empty [] 1 (initialPos "dummy") Map.empty getstatements [] [] [] gettemplate)
    case output of
        Left x -> return (Left x, getWarnings finalstate)
        Right _ -> return (output, getWarnings finalstate)

computeCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> CatalogMonad FinalCatalog
computeCatalog getstatements nodename = do
    nodestatements <- liftIO $ getstatements TopNode nodename
    case nodestatements of
        Left x -> throwError x
        Right nodestmts -> evaluateStatements nodestmts >>= finalResolution


-- this validates the resolved resources
-- it should only be called with native types or the validatefunction lookup with abord with an error
finalizeResource :: CResource -> CatalogMonad (ResIdentifier, RResource)
finalizeResource (CResource cid cname ctype cparams _ cpos) = do
    setPos cpos
    rname <- resolveGeneralString cname
    rparams <- mapM (\(a,b) -> do { ra <- resolveGeneralString a; rb <- resolveGeneralValue b; return (ra,rb); }) cparams
    -- add collected relations
    -- TODO
    if Map.member ctype nativeTypes == False
        then throwPosError $ "Can't find native type " ++ ctype
        else return ()
    let mrrelations = []
        prefinalresource = RResource cid rname ctype (Map.fromList rparams) mrrelations cpos
        validatefunction = puppetvalidate (nativeTypes Map.! ctype)
        validated = validatefunction prefinalresource
    case validated of
        Left err -> throwError (err ++ " for resource " ++ ctype ++ "[" ++ rname ++ "] at " ++ show cpos)
        Right finalresource -> return $ ((ctype, rname), finalresource)

-- This checks if a resource is to be collected.
-- This returns a list as it can either return the original
-- resource, the resource with a "normal" virtuality, or both,
-- for exported resources (so that they can still be found as collected)
collectionChecks :: CResource -> CatalogMonad [CResource]
collectionChecks res = do
    if (crvirtuality res == Normal)
        then return [res]
        else do
            isCollected <- get >>= return . curCollect >>= mapM (\x -> x res)
            case (or isCollected, crvirtuality res) of
                (True, Exported)    -> return [res { crvirtuality = Normal }, res]
                (True,  _)          -> return [res { crvirtuality = Normal }     ]
                (False, _)          -> return [res                               ]

finalResolution :: Catalog -> CatalogMonad FinalCatalog
finalResolution cat = do
    liftIO $ putStrLn $ "FINAL RESOLUTION"
    collected <- mapM collectionChecks cat >>= mapM evaluateDefine . concat
    let (real,  allvirtual)  = partition (\x -> crvirtuality x == Normal)  (concat collected)
        (_,  exported) = partition (\x -> crvirtuality x == Virtual)  allvirtual
    --export stuff
    liftIO $ putStrLn "EXPORTED:"
    liftIO $ mapM print exported
    resolved <- mapM finalizeResource real >>= createResourceMap
    --get >>= return . unresolvedRels >>= liftIO . (mapM print)
    return resolved

createResourceMap :: [(ResIdentifier, RResource)] -> CatalogMonad FinalCatalog
createResourceMap = foldM insertres Map.empty
    where
        insertres curmap (resid, res) = let
            oldres = Map.lookup resid curmap
            newmap = Map.insert resid res curmap
            in case (rrtype res, oldres) of
                ("class", _) -> return newmap
                (_, Just r ) -> throwError $ "Resource already defined:"
                    ++ "\n\t" ++ (rrtype r)   ++ "[" ++ (rrname r)   ++ "] at " ++ show (rrpos r)
                    ++ "\n\t" ++ (rrtype res) ++ "[" ++ (rrname res) ++ "] at " ++ show (rrpos res)
                (_, Nothing) -> return newmap

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
setPos p = modify (setStatePos p)
getPos = get >>= return . curPos
putVariable k v = do
    curscope <- getScope
    let qual = qualified k
        kk  | qual || (curscope == "::") = k
            | otherwise = "::" ++ k
    modify (modifyVariables (Map.insert (curscope ++ kk) v))
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
                Left err -> throwPosError ("Could not find the definition of " ++ dname ++ " err = " ++ err)
                Right s -> return $ Just s

partitionParamsRelations :: [(GeneralString, GeneralValue)] -> ([(GeneralString, GeneralValue)], [(LinkType, GeneralValue, GeneralValue)])
partitionParamsRelations rparameters = (realparams, relations)
    where   realparams = filteredparams
            relations = concatMap convertrelation filteredrelations
            convertrelation :: (GeneralString, GeneralValue) -> [(LinkType, GeneralValue, GeneralValue)]
            convertrelation (_,       Right ResolvedUndefined)          = []
            convertrelation (reltype, Right (ResolvedArray rs))         = concatMap (\x -> convertrelation (reltype, Right x)) rs
            convertrelation (reltype, Right (ResolvedRReference rt rv)) = [(fromJust $ getRelationParameterType reltype, Right $ ResolvedString rt, Right rv)]
            convertrelation (reltype, Right (ResolvedString "undef"))   = [(fromJust $ getRelationParameterType reltype, Right $ ResolvedString "undef", Right $ ResolvedString "undef")]
            convertrelation x = error ("partitionParamsRelations error : " ++ show x)
            (filteredrelations, filteredparams) = partition (isJust . getRelationParameterType . fst) rparameters -- filters relations with actual parameters

-- TODO check whether parameters changed
checkLoaded name = do
    curscope <- get
    case (Map.lookup name (curClasses curscope)) of
        Nothing -> return False
        Just _  -> return True

-- apply default values to a resource
applyDefaults :: CResource -> CatalogMonad CResource
applyDefaults res = do
    defs <- get >>= return . curDefaults 
    foldM applyDefaults' res defs

applyDefaults' :: CResource -> Statement -> CatalogMonad CResource            
applyDefaults' r@(CResource i rname rtype rparams rvirtuality rpos) (ResourceDefault dtype defs dpos) = do
    srname <- resolveGeneralString rname
    rdefs <- mapM (\(a,b) -> do { ra <- tryResolveExpressionString a; rb <- tryResolveExpression b; return (ra, rb); }) defs
    let (nparams, nrelations) = mergeParams rparams rdefs False 
    if (dtype == rtype)
        then do
            addUnresRel (nrelations, (rtype, Right srname), UDefault, dpos)
            return $ CResource i rname rtype nparams rvirtuality rpos
        else return r
applyDefaults' r@(CResource i rname rtype rparams rvirtuality rpos) (ResourceOverride dtype dname defs dpos) = do
    srname <- resolveGeneralString rname
    sdname <- resolveExpressionString dname
    rdefs <- mapM (\(a,b) -> do { ra <- tryResolveExpressionString a; rb <- tryResolveExpression b; return (ra, rb); }) defs
    let (nparams, nrelations) = mergeParams rparams rdefs True
    if ((dtype == rtype) && (srname == sdname))
        then do
            addUnresRel (nrelations, (rtype, Right srname), UDefault, dpos)
            return $ CResource i rname rtype nparams rvirtuality rpos
        else return r
applyDefaults' r d = throwError $ "Can't apply non default statement " ++ show d ++ " to resource " ++ show r

-- merge defaults and actual parameters depending on the override flag
mergeParams :: [(GeneralString, GeneralValue)] -> [(GeneralString, GeneralValue)] -> Bool -> ([(GeneralString, GeneralValue)], [(LinkType, GeneralValue, GeneralValue)])
mergeParams srcparams defs override = let
    (dstparams, dstrels) = partitionParamsRelations defs
    srcprm = Map.fromList srcparams
    dstprm = Map.fromList dstparams
    prm = if override
        then Map.toList $ Map.union dstprm srcprm
        else Map.toList $ Map.union srcprm dstprm
    in (prm, dstrels)

-- The actual meat

evaluateDefine :: CResource -> CatalogMonad [CResource]
evaluateDefine r@(CResource _ rname rtype rparams rvirtuality rpos) = do
    isdef <- checkDefine rtype
    case (rvirtuality, isdef) of
        (Normal, Just (DefineDeclaration dtype args dstmts dpos)) -> do
            --oldpos <- getPos
            setPos dpos
            pushScope $ "#DEFINE#" ++ dtype
            -- add variables
            mrrparams <- mapM (\(gs, gv) -> do { rgs <- resolveGeneralString gs; rgv <- tryResolveGeneralValue gv; return (rgs, (rgv, dpos)); }) rparams
            let expr = gs2gv rname
                mparams = Map.fromList mrrparams
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
evaluateStatements (Node _ stmts position) = do
    setPos position
    res <- mapM (evaluateStatements) stmts
    nres <- handleDelayedActions (concat res)
    return nres

-- include
evaluateStatements (Include includename position) = setPos position >> getstatement TopClass includename >>= evaluateStatements
evaluateStatements x@(ClassDeclaration _ _ _ _ _) = evaluateClass x Map.empty Nothing
evaluateStatements n@(DefineDeclaration dtype _ _ _) = do
    addNestedTopLevel TopDefine dtype n
    return []
evaluateStatements (ConditionalStatement exprs position) = do
    setPos position
    trues <- filterM (\(expr, _) -> resolveBoolean (Left expr)) exprs
    case trues of
        ((_,stmts):_) -> mapM evaluateStatements stmts >>= return . concat
        _ -> return []

evaluateStatements (Resource rtype rname parameters virtuality position) = do
    setPos position
    case rtype of
        -- checks whether we are handling a parametrized class
        "class" -> do
            rparameters <- mapM (\(a,b) -> do { pa <- resolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            classname <- resolveExpressionString rname
            topstatement <- getstatement TopClass classname
            let classparameters = Map.fromList $ map (\(pname, pvalue) -> (pname, (pvalue, position))) rparameters
            evaluateClass topstatement classparameters Nothing
        _ -> do
            resid <- getNextId
            rparameters <- mapM (\(a,b) -> do { pa <- tryResolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            srname <- tryResolveExpressionString rname
            let (realparams, relations) = partitionParamsRelations rparameters
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
    if null overrides
        then return ()
        else throwPosError "Collection overrides not handled"
    func <- collectionFunction Exported rtype expr
    addCollect func
    return []
-- <| |>
evaluateStatements (VirtualResourceCollection rtype expr overrides position) = do
    setPos position
    if null overrides
        then return ()
        else throwPosError "Collection overrides not handled"
    func <- collectionFunction Virtual rtype expr
    addCollect func
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

evaluateStatements (TopContainer toplevels curstatement) = do
    mapM (\(fname, stmt) -> evaluateClass stmt Map.empty (Just fname)) toplevels
    evaluateStatements curstatement

evaluateStatements x = throwError ("Can't evaluate " ++ (show x))

-- function used to load defines / class variables into the global context
loadClassVariable :: SourcePos -> Map.Map String (GeneralValue, SourcePos) -> (String, Maybe Expression) -> CatalogMonad ()
loadClassVariable position inputs (paramname, defvalue) = do
    let inputvalue = Map.lookup paramname inputs
    (v, vpos) <- case (inputvalue, defvalue) of
        (Just x , _      ) -> return x
        (Nothing, Just y ) -> return (Left y, position)
        (Nothing, Nothing) -> throwError $ "Must define parameter " ++ paramname ++ " at " ++ (show position)
    rv <- tryResolveGeneralValue v
    putVariable paramname (rv, vpos)
    return ()

-- class
-- ClassDeclaration String (Maybe String) [(String, Maybe Expression)] [Statement] SourcePos
-- nom, heritage, parametres, contenu
evaluateClass :: Statement -> Map.Map String (GeneralValue, SourcePos) -> Maybe String -> CatalogMonad Catalog
evaluateClass (ClassDeclaration classname inherits parameters statements position) inputparams actualname = do
    isloaded <- case actualname of
        Nothing -> checkLoaded classname
        Just x  -> checkLoaded x
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
                    ClassDeclaration _ ni np ns no -> evaluateClass (ClassDeclaration classname ni np ns no) Map.empty (Just parentclass)
                    _ -> throwError "Should not happen : TopClass return something else than a ClassDeclaration in evaluateClass"
            Nothing -> return []
        case actualname of
            Nothing -> addLoaded classname oldpos
            Just x  -> addLoaded x oldpos

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

evaluateClass (TopContainer topstmts myclass) inputparams actualname = do
    mapM (\(n,x) -> evaluateClass x Map.empty (Just n)) topstmts
    evaluateClass myclass inputparams actualname

evaluateClass x _ _ = throwError ("Someone managed to run evaluateClass against " ++ show x)

addClassDependency :: String -> CResource -> CatalogMonad ()
addClassDependency cname (CResource _ rname rtype _ _ position) = addUnresRel (
    [(RRequire, Right $ ResolvedString "class", Right $ ResolvedString cname)]
    , (rtype, rname)
    , UPlus, position)

tryResolveExpression :: Expression -> CatalogMonad GeneralValue
tryResolveExpression e = tryResolveGeneralValue (Left e)

tryResolveGeneralValue :: GeneralValue -> CatalogMonad GeneralValue
tryResolveGeneralValue n@(Right _) = return n
tryResolveGeneralValue   (Left BTrue) = return $ Right $ ResolvedBool True
tryResolveGeneralValue   (Left BFalse) = return $ Right $ ResolvedBool False
tryResolveGeneralValue   (Left (Value x)) = tryResolveValue x
tryResolveGeneralValue n@(Left (ResolvedResourceReference _ _)) = return n
tryResolveGeneralValue   (Left (Error x)) = throwPosError x
tryResolveGeneralValue   (Left (ConditionalValue checkedvalue (Value (PuppetHash (Parameters hash))))) = do
    rcheck <- resolveExpression checkedvalue
    rhash <- mapM (\(vn, vv) -> do { rvn <- resolveExpression vn; return (rvn, vv) }) hash
    case (filter (\(a,_) -> (a == ResolvedString "default") || (compareRValues a rcheck)) rhash) of
        [] -> throwPosError ("No value could be selected when comparing to " ++ show rcheck)
        ((_,x):_) -> tryResolveExpression x
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
tryResolveGeneralValue   (Left (NotOperation x)) = do
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
    case (ra, rb) of
        (Right (ResolvedArray ar), Right num) -> do
            bnum <- readint num
            let nnum = fromIntegral bnum
            if(length ar >= nnum)
                then throwPosError ("Invalid array index " ++ num)
                else return $ Right (ar !! nnum)
        (Right (ResolvedHash ar), Right idx) -> do
            let filtered = filter (\(x,_) -> x == idx) ar
            case filtered of
                [] -> throwError "TODO empty filtered"
                [(_,x)] -> return $ Right $ x
                x  -> throwPosError ("Hum, WTF tryResolveGeneralValue " ++ show x)
        (_, Left y) -> throwPosError ("Could not resolve index " ++ show y)
        (Left x, _) -> throwPosError ("Could not resolve lookup " ++ show x)
        (Right x, _) -> throwPosError ("Could not resolve something that is not an array nor a hash, but " ++ show x)
tryResolveGeneralValue o@(Left (IsElementOperation b a)) = do
    ra <- tryResolveExpression a
    rb <- tryResolveExpressionString b
    case (ra, rb) of
        (Right (ResolvedArray ar), Right idx) -> do
            let filtered = filter (compareRValues (ResolvedString idx)) ar
            if null filtered
                then return $ Right $ ResolvedBool False
                else return $ Right $ ResolvedBool True
        _ -> return o
            
tryResolveGeneralValue e = throwPosError ("tryResolveGeneralValue not implemented for " ++ show e)

resolveGeneralValue :: GeneralValue -> CatalogMonad ResolvedValue
resolveGeneralValue e = do
    x <- tryResolveGeneralValue e
    case x of
        Left n -> throwPosError  ("Could not resolveGeneralValue " ++ show n)
        Right p -> return p

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
            throwError ("Can't resolve expression '" ++ show x ++ "' at " ++ show p ++ " was '" ++ show e ++ "'")

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
    case (var, qualified vname) of
        (Just (Left e, _ ), _   ) -> tryResolveExpression e
        (Just (Right r, _), _   ) -> return $ Right r
        (Nothing          , True) -> return $ Left $ Value n -- already qualified
        (Nothing          , _   ) -> do
            -- TODO : check that there are no "::"
            curscp <- getScope
            let varnamescp  | curscp == "::" = "::" ++ vname
                            | otherwise      = curscp ++ "::" ++ vname
            varscp <- getVariable varnamescp
            case varscp of
                Just (Left e , _) -> tryResolveExpression e
                Just (Right r, _) -> return $ Right r
                Nothing -> do
                    position <- getPos
                    addWarning ("Could not resolveValue " ++ varnamescp ++ " at " ++ show position)
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
tryResolveValue   (FunctionCall "fqdn_rand" args) = if (null args)
    then throwPosError "Empty argument list in fqdn_rand call"
    else do
        nargs  <- mapM resolveExpressionString args
        curmax <- readint (head nargs)
        fqdn_rand curmax (tail nargs) >>= return . Right . ResolvedInt
tryResolveValue   (FunctionCall "mysql_password" args) = if (length args /= 1)
    then throwPosError "mysql_password takes a single argument"
    else do
        es <- tryResolveExpressionString (head args)
        case es of
            Right s -> mysql_password s >>= return . Right . ResolvedString
            Left  u -> return $ Left u
tryResolveValue   (FunctionCall "jbossmem" _) = return $ Right $ ResolvedString "512"
tryResolveValue   (FunctionCall "template" [name]) = do
    fname <- tryResolveExpressionString name
    case fname of
        Left x -> throwPosError $ "Can't resolve template path " ++ show x
        Right filename -> do
            vars <- get >>= mapM (\(varname, (varval, _)) -> do { rvarval <- tryResolveGeneralValue varval; return (varname, rvarval) }) . Map.toList . curVariables
            scp <- getScope
            templatefunc <- get >>= return . computeTemplateFunction
            out <- liftIO (templatefunc filename scp vars)
            case out of
                Right x -> return $ Right $ ResolvedString x
                Left err -> throwPosError err
tryResolveValue   (FunctionCall "inline_template" _) = return $ Right $ ResolvedString "TODO"
tryResolveValue   (FunctionCall "regsubst" [str, src, dst, flags]) = do
    rstr   <- tryResolveExpressionString str
    rsrc   <- tryResolveExpressionString src
    rdst   <- tryResolveExpressionString dst
    rflags <- tryResolveExpressionString flags
    case (rstr, rsrc, rdst, rflags) of
        (Right sstr, Right ssrc, Right sdst, Right sflags) -> regsubst sstr ssrc sdst sflags >>= return . Right . ResolvedString
        x                                                  -> throwPosError ("Could not run regsubst because something here could not be resolved: " ++ show x)
tryResolveValue   (FunctionCall "regsubst" [str, src, dst]) = tryResolveValue (FunctionCall "regsubst" [str, src, dst, Value $ Literal ""])
tryResolveValue   (FunctionCall "regsubst" args) = throwPosError ("Bad argument count for regsubst " ++ show args)
       
tryResolveValue   (FunctionCall "file" _) = return $ Right $ ResolvedString "TODO file function"
tryResolveValue   (FunctionCall fname _) = throwPosError ("FunctionCall " ++ fname ++ " not implemented")

tryResolveValue Undefined = return $ Right $ ResolvedUndefined

tryResolveValue x = throwPosError ("tryResolveValue not implemented for " ++ show x)

tryResolveValueString :: Value -> CatalogMonad GeneralString
tryResolveValueString x = do
    r <- tryResolveValue x
    case r of
        Right (ResolvedString v) -> return $ Right v
        Right (ResolvedInt    i) -> return $ Right (show i)
        Right v                  -> throwError ("Can't resolve valuestring for " ++ show v)
        Left  v                  -> return $ Left v

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
        myfunction = (\(CResource _ mcrname mcrtype _ _ _) -> do
            srname <- resolveGeneralString mcrname
            return ((srname == rname) && (mcrtype == rtype))
            )
    addCollect myfunction
    return ()
pushRealize (ResolvedRReference _ x) = throwPosError (show x ++ " was not resolved to a string")
pushRealize x                        = throwPosError ("A reference was expected instead of " ++ show x)

executeFunction :: String -> [ResolvedValue] -> CatalogMonad Catalog
executeFunction "fail" [ResolvedString errmsg] = throwPosError ("Error: " ++ errmsg)
executeFunction "fail" args = throwPosError ("Error: " ++ show args)
executeFunction "realize" rlist = mapM pushRealize rlist >> return []
executeFunction "create_resources" [mrtype, rdefs] = do
    mrrtype <- case mrtype of
        ResolvedString x -> return x
        _                -> throwPosError $ "Resource type must be a string and not " ++ show mrtype
    arghash <- case rdefs of
        ResolvedHash x -> return x
        _              -> throwPosError $ "Resource definition must be a hash, and not " ++ show rdefs 
    position <- getPos
    let prestatements = map (\(rname, rargs) -> (Value $ Literal rname, resolved2expression rargs)) arghash
    resources <- mapM (\(resname, pval) -> do
            realargs <- case pval of
                Value (PuppetHash (Parameters h)) -> return h
                _                    -> throwPosError "This should not happen, create_resources argument is not a hash"
            return $ Resource mrrtype resname realargs Normal position
        ) prestatements
    mapM evaluateStatements resources >>= return . concat
executeFunction "create_resources" x = throwPosError ("Bad arguments to create_resources: " ++ show x)
executeFunction a b = do
    position <- getPos
    addWarning $ "Function " ++ a ++ "(" ++ show b ++ ") not handled at " ++ show position
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
        Right (ResolvedString "")   -> return $ Right $ ResolvedBool False
        Right (ResolvedString _)    -> return $ Right $ ResolvedBool True
        Right (ResolvedInt 0)       -> return $ Right $ ResolvedBool False
        Right (ResolvedInt _)       -> return $ Right $ ResolvedBool True
        Right (ResolvedUndefined)   -> return $ Right $ ResolvedBool False
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
        n -> throwPosError ("Could not resolve " ++ show n ++ "(was " ++ show rv ++ ") as a boolean")

resolveGeneralString :: GeneralString -> CatalogMonad String
resolveGeneralString (Right x) = return x
resolveGeneralString (Left y) = resolveExpressionString y

gs2gv :: GeneralString -> GeneralValue
gs2gv (Left e)  = Left e
gs2gv (Right s) = Right $ ResolvedString s

collectionFunction :: Virtuality -> String -> Expression -> CatalogMonad (CResource -> CatalogMonad Bool)
collectionFunction virt mrtype exprs = do
    finalfunc <- case exprs of
        BTrue -> return (\_ -> return True)
        EqualOperation a b -> do
            ra <- resolveExpression a
            rb <- resolveExpression b
            paramname <- case ra of
                ResolvedString pname -> return pname
                _ -> throwPosError $ "We only support collection of the form 'parameter == value'" 
            defstatement <- checkDefine mrtype
            paramset <- case defstatement of
                Nothing -> case (Map.lookup mrtype nativeTypes) of
                    Just (PuppetTypeMethods _ ps) -> return ps
                    Nothing -> throwPosError $ "Unknown type " ++ mrtype ++ " when trying to collect"
                Just (DefineDeclaration _ params _ _) -> return $ Set.fromList $ map fst params
                Just x -> throwPosError $ "Expected a DefineDeclaration here instead of " ++ show x
            if (Set.notMember paramname paramset) && (paramname /= "tag")
                then throwPosError $ "Parameter " ++ paramname ++ " is not a valid parameter. It should be in : " ++ show (Set.toList paramset)
                else return ()
            return (\r -> do
                let param = filter (\x -> fst x == Right paramname) (crparams r)
                if length param == 0
                    then return False
                    else do
                        cmp <- resolveGeneralValue $ snd (head param)
                        return (cmp == rb)
                )
        x -> throwPosError $ "TODO : implement collection function for " ++ show x
    return (\res ->
        if ((crtype res == mrtype) && (crvirtuality res == virt))
            then finalfunc res
            else return False
            )


resolved2expression :: ResolvedValue -> Expression
resolved2expression (ResolvedString str) = Value $ Literal str
resolved2expression (ResolvedInt i) = Value $ Integer i
resolved2expression (ResolvedBool True) = BTrue
resolved2expression (ResolvedBool False) = BFalse
resolved2expression (ResolvedRReference mrtype name) = Value $ ResourceReference mrtype (resolved2expression name)
resolved2expression (ResolvedArray vals) = Value $ PuppetArray $ map resolved2expression vals
resolved2expression (ResolvedHash hash) = Value $ PuppetHash $ Parameters $ map (\(s,v) -> (Value $ Literal s, resolved2expression v)) hash
resolved2expression (ResolvedUndefined) = Value $ Undefined
