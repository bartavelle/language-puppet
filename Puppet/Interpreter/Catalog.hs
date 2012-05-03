module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Preferences
import Puppet.DSL.Types
import Puppet.Interpreter.Types

import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map

data ScopeState = ScopeState {
    curScope :: [String],
    curVariables :: Map.Map String (Either Expression ResolvedValue, SourcePos),
    curClasses :: Map.Map String SourcePos,
    curCatalog :: [CResource],
    curDefaults :: Map.Map String (Map.Map String (ResolvedValue, SourcePos)),
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement)
}

modifyScope     f (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) = ScopeState (f curscope) curvariables curclasses curcatalog curdefaults gsf
modifyVariables f (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) = ScopeState curscope (f curvariables) curclasses curcatalog curdefaults gsf
modifyClasses   f (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) = ScopeState curscope curvariables (f curclasses) curcatalog curdefaults gsf
modifyCatalogs  f (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) = ScopeState curscope curvariables curclasses (f curcatalog) curdefaults gsf
modifyDefaults  f (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) = ScopeState curscope curvariables curclasses curcatalog (f curdefaults) gsf

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> Facts -> IO (Either String Catalog)
getCatalog getstatements nodename facts = do
    let convertedfacts = Map.map
            (\fval -> (Right fval, initialPos "FACTS"))
            facts
    (output, finalstate) <- runStateT ( runErrorT ( computeCatalog getstatements nodename ) ) (ScopeState [] convertedfacts Map.empty [] Map.empty getstatements)
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
evaluateStatements (Node name stmts _) = do
    pushScope "::"
    res <- mapM (evaluateStatements) stmts
    popScope
    return (concat res)

-- include
evaluateStatements (Include includename _) = getstatement TopClass includename >>= evaluateStatements
evaluateStatements x@(ClassDeclaration _ _ _ _ _) = evaluateClass x Map.empty

evaluateStatements x = throwError ("Can't evaluate " ++ (show x))

-- function used to load defines / class variables into the global context
loadClassVariable :: SourcePos -> Map.Map String (Either Expression ResolvedValue, SourcePos) -> (String, Maybe Expression) -> CatalogMonad ()
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
evaluateClass :: Statement -> Map.Map String (Either Expression ResolvedValue, SourcePos) -> CatalogMonad Catalog
evaluateClass (ClassDeclaration classname inherits parameters statements position) inputparams = do
    checkLoaded classname
    addLoaded classname position
    pushScope classname
    -- add variables
    mapM (loadClassVariable position inputparams) parameters
    
    -- load inherited classes
    inheritedstatements <- case inherits of
        Just parentclass -> throwError ("Inheritance not implemented at " ++ (show position))
        Nothing -> return []
    -- mark as loaded
    
    -- parse statements
    popScope
    return inheritedstatements

