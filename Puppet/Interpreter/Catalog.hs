module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Init
import Puppet.DSL.Types
import Puppet.Interpreter.Types

import Data.List
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)
import Data.Either (lefts, rights)
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map

type ResDefaults = Map.Map String (Map.Map String (GeneralValue, SourcePos))
type ScopeName = String

data ScopeState = ScopeState {
    curScope :: [ScopeName],
    curVariables :: Map.Map String (GeneralValue, SourcePos),
    curClasses :: Map.Map String SourcePos,
    curDefaults :: Map.Map ScopeName ResDefaults,
    curResId :: Int,
    curPos :: SourcePos,
    netstedtoplevels :: Map.Map (TopLevelType, String) Statement,
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement)
}

modifyScope     f (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState (f curscope) curvariables curclasses curdefaults rid pos ntl gsf
modifyVariables f (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope (f curvariables) curclasses curdefaults rid pos ntl gsf
modifyClasses   f (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope curvariables (f curclasses) curdefaults rid pos ntl gsf
modifyDefaults  f (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope curvariables curclasses (f curdefaults) rid pos ntl gsf
incrementResId    (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope curvariables curclasses curdefaults (rid + 1) pos ntl gsf
setStatePos  npos (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope curvariables curclasses curdefaults rid npos ntl gsf
modifyNestedTopLvl f (ScopeState curscope curvariables curclasses curdefaults rid pos ntl gsf)
    = ScopeState curscope curvariables curclasses curdefaults rid pos (f ntl) gsf

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> Facts -> IO (Either String Catalog)
getCatalog getstatements nodename facts = do
    let convertedfacts = Map.map
            (\fval -> (Right fval, initialPos "FACTS"))
            facts
    (output, finalstate) <- runStateT ( runErrorT ( computeCatalog getstatements nodename ) ) (ScopeState [] convertedfacts Map.empty Map.empty 1 (initialPos "dummy") Map.empty getstatements)
    case output of
        Left x -> return $ Left x
        Right y -> return output

type CatalogMonad = ErrorT String (StateT ScopeState IO)

computeCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> CatalogMonad Catalog
computeCatalog getstatements nodename = do
    nodestatements <- liftIO $ getstatements TopNode nodename
    case nodestatements of
        Left x -> throwError x
        Right nodestmts -> evaluateStatements nodestmts

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
popScope        = modify (modifyScope tail)
getScope        = get >>= return . head . curScope
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

-- throws an error if a class is already loaded
checkLoaded name = do
    curscope <- get
    case (Map.lookup name (curClasses curscope)) of
        Nothing -> return ()
        Just thispos -> throwError ("Class " ++ name ++ " already loaded at " ++ (show thispos))

-- The actual meat

-- node
evaluateStatements :: Statement -> CatalogMonad Catalog
evaluateStatements (Node name stmts position) = do
    setPos position
    pushScope "::"
    res <- mapM (evaluateStatements) stmts
    popScope
    return (concat res)

-- include
evaluateStatements (Include includename _) = getstatement TopClass includename >>= evaluateStatements
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
    resid <- getNextId
    case rtype of
        "class" -> do
            rparameters <- mapM (\(a,b) -> do { pa <- resolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            classname <- resolveExpressionString rname
            topstatement <- getstatement TopClass classname
            let classparameters = Map.fromList $ map (\(pname, pvalue) -> (pname, (pvalue, position))) rparameters
            evaluateClass topstatement classparameters
        _ -> do
            rparameters <- mapM (\(a,b) -> do { pa <- tryResolveExpressionString a; pb <- tryResolveExpression b; return (pa, pb) } ) parameters
            let realparams = filteredparams
                relations = map (
                    \(reltype, relval) -> 
                        (fromJust $ getRelationParameterType reltype,
                        generalizeValueE rname,
                        relval))
                    filteredrelations
                (filteredrelations, filteredparams) = partition (isJust . getRelationParameterType . fst) rparameters -- filters relations with actual parameters
            return [CResource resid (Left rname) rtype realparams relations virtuality position]

evaluateStatements (VariableAssignment vname vexpr position) = do
    setPos position
    rvexpr <- tryResolveExpression vexpr
    putVariable vname (rvexpr, position)
    return []

evaluateStatements (MainFunctionCall fname fargs position) = do
    setPos position
    executeFunction fname (map resolveExpression fargs)

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
    setPos position
    checkLoaded classname
    pushScope classname
    -- add variables
    mapM (loadClassVariable position inputparams) parameters
    
    -- load inherited classes
    inherited <- case inherits of
        Just parentclass -> do
            mystatement <- getstatement TopClass parentclass
            case mystatement of
                ClassDeclaration _ ni np ns no -> evaluateClass (ClassDeclaration classname ni np ns no) Map.empty
                _ -> throwError "Should not happen : TopClass return something else than a ClassDeclaration in evaluateClass"
        Nothing -> return []
    addLoaded classname position

    -- parse statements
    res <- mapM (evaluateStatements) statements
    popScope
    return $ inherited ++ (concat res)

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
tryResolveGeneralValue (Left (DifferentOperation a b)) = do
    res <- tryResolveGeneralValue (Left (EqualOperation a b))
    case res of
        Right (ResolvedBool x) -> return (Right $ ResolvedBool $ not x)
        _ -> return res
tryResolveGeneralValue e = do
    p <- getPos
    throwError ("tryResolveGeneralValue not implemented at " ++ show p ++ " for " ++ show e)

tryResolveExpressionString :: Expression -> CatalogMonad GeneralString
tryResolveExpressionString s = do
    resolved <- tryResolveExpression s
    case resolved of
        Right (ResolvedString s) -> return $ Right s
        Right e                       -> do
            p <- getPos
            throwError ("'" ++ show e ++ "' will not resolve to a string at " ++ show p)
        Left  e                       -> return $ Left e

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
        e                     -> do
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
            curscp <- getScope
            let varnamescp = curscp ++ "::" ++ vname
            varscp <- getVariable varnamescp
            case varscp of
                Just (Left e, pos) -> tryResolveExpression e
                Just (Right r, pos) -> return $ Right r
                Nothing -> do
                    state <- get
                    liftIO $ print ("Could not resolve " ++ varnamescp)
                    liftIO $ mapM print (Map.toList $ curVariables state)
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
tryResolveValue n@(FunctionCall "fqdn_rand" [v1, v2]) = return $ Right $ ResolvedInt 1
tryResolveValue n@(FunctionCall "jbossmem" _) = return $ Right $ ResolvedString "512"

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

executeFunction a b = do
    liftIO $ putStrLn $ "executeFunction " ++ show a
    return []

compareRValues :: ResolvedValue -> ResolvedValue -> Bool
compareRValues (ResolvedString a) (ResolvedInt b) = compareRValues (ResolvedInt b) (ResolvedString a)
compareRValues (ResolvedInt a) (ResolvedString b) | and $ map isDigit b = a == (read b)
                                                  | otherwise = False
compareRValues a b = a == b

resolveBoolean :: GeneralValue -> CatalogMonad Bool
resolveBoolean v = do
    rv <- tryResolveGeneralValue v
    case rv of
        Right (ResolvedBool x) -> return x
        n -> do
            pos <- getPos
            throwError ("Could not resolve " ++ show rv ++ " as a boolean at " ++ show pos)
