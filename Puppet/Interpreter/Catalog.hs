module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Preferences
import Puppet.DSL.Types
import Puppet.Interpreter.Types

import Data.List
import Data.Maybe (isJust, fromJust)
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
    curCatalog :: [CResource],
    curDefaults :: Map.Map ScopeName ResDefaults,
    curResId :: Int,
    curPos :: SourcePos,
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement)
}

modifyScope     f (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState (f curscope) curvariables curclasses curcatalog curdefaults rid pos gsf
modifyVariables f (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope (f curvariables) curclasses curcatalog curdefaults rid pos gsf
modifyClasses   f (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope curvariables (f curclasses) curcatalog curdefaults rid pos gsf
modifyCatalogs  f (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope curvariables curclasses (f curcatalog) curdefaults rid pos gsf
modifyDefaults  f (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope curvariables curclasses curcatalog (f curdefaults) rid pos gsf
incrementResId    (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope curvariables curclasses curcatalog curdefaults (rid + 1) pos gsf
setStatePos  npos (ScopeState curscope curvariables curclasses curcatalog curdefaults rid pos gsf)
    = ScopeState curscope curvariables curclasses curcatalog curdefaults rid npos gsf

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> Facts -> IO (Either String Catalog)
getCatalog getstatements nodename facts = do
    let convertedfacts = Map.map
            (\fval -> (Right fval, initialPos "FACTS"))
            facts
    (output, finalstate) <- runStateT ( runErrorT ( computeCatalog getstatements nodename ) ) (ScopeState [] convertedfacts Map.empty [] Map.empty 1 (initialPos "dummy") getstatements)
    case output of
        Left x -> return $ Left x
        Right y -> return $ Right (curCatalog finalstate)

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
addLoaded name position = modify (modifyClasses (Map.insert name position))
getNextId = do
    curscope <- get
    put $ incrementResId curscope
    return (curResId curscope)
setPos pos = modify (setStatePos pos)
getPos = get >>= return . curPos

-- throws an error if a class is already loaded
checkLoaded name = do
    curscope <- get
    case (Map.lookup name (curClasses curscope)) of
        Nothing -> return ()
        Just thispos -> throwError ("Class " ++ name ++ " already loaded at " ++ (show thispos))


putVariable k v = do
    throwError "Must implement scope stacking as putVariable"
    modify (modifyVariables (Map.insert k v))

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

evaluateStatements x@(Resource rtype rname parameters virtuality position) = do
    setPos position
    resid <- getNextId
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

evaluateStatements x = throwError ("Can't evaluate " ++ (show x))

-- function used to load defines / class variables into the global context
loadClassVariable :: SourcePos -> Map.Map String (GeneralValue, SourcePos) -> (String, Maybe Expression) -> CatalogMonad ()
loadClassVariable position inputs (paramname, defaultvalue) = do
    let inputvalue = Map.lookup paramname inputs
    case (inputvalue, defaultvalue) of
        (Just x , _      ) -> putVariable paramname x
        (Nothing, Just y ) -> putVariable paramname (Left y, position)
        (Nothing, Nothing) -> throwError $ "Must define parameter " ++ paramname ++ " at " ++ (show position)
    return ()

-- class
-- ClassDeclaration String (Maybe String) [(String, Maybe Expression)] [Statement] SourcePos
-- nom, heritage, parametres, contenu
evaluateClass :: Statement -> Map.Map String (GeneralValue, SourcePos) -> CatalogMonad Catalog
evaluateClass (ClassDeclaration classname inherits parameters statements position) inputparams = do
    setPos position
    checkLoaded classname
    addLoaded classname position
    pushScope classname
    -- add variables
    mapM (loadClassVariable position inputparams) parameters
    
    -- load inherited classes
    case inherits of
        Just parentclass -> throwError ("Inheritance not implemented at " ++ (show position))
        Nothing -> return ()

    -- parse statements
    res <- mapM (evaluateStatements) statements
    popScope
    return (concat res)

tryResolveExpression :: Expression -> CatalogMonad GeneralValue
tryResolveExpression e = do
    p <- getPos
    throwError ("tryResolveExpression not implemented at " ++ show p)

tryResolveExpressionString :: Expression -> CatalogMonad GeneralString
tryResolveExpressionString s = do
    resolved <- tryResolveExpression s
    case resolved of
        Right (ResolvedParamString s) -> return $ Right s
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
        ResolvedParamString s -> return s
        e                     -> do
            p <- getPos
            throwError ("Can't resolve expression '" ++ show e ++ "' to a string at " ++ show p)


getRelationParameterType :: GeneralString -> Maybe LinkType
getRelationParameterType (Right "require" ) = Just RRequire
getRelationParameterType (Right "notify"  ) = Just RNotify
getRelationParameterType (Right "before"  ) = Just RBefore
getRelationParameterType (Right "register") = Just RRegister
getRelationParameterType _                  = Nothing

