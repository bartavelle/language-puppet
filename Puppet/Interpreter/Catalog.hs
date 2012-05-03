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
    curClasses :: Map.Map String (String, SourcePos),
    curCatalog :: [CResource],
    curDefaults :: Map.Map String (Map.Map String (ResolvedValue, SourcePos)),
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement)
}

modifyScope (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf)     f = ScopeState (f curscope) curvariables curclasses curcatalog curdefaults gsf
modifyVariables (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf) f = ScopeState curscope (f curvariables) curclasses curcatalog curdefaults gsf
modifyClasses (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf)   f = ScopeState curscope curvariables (f curclasses) curcatalog curdefaults gsf
modifyCatalogs (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf)  f = ScopeState curscope curvariables curclasses (f curcatalog) curdefaults gsf
modifyDefaults (ScopeState curscope curvariables curclasses curcatalog curdefaults gsf)  f = ScopeState curscope curvariables curclasses curcatalog (f curdefaults) gsf

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

-- State alteration functions
getstatement :: TopLevelType -> String -> CatalogMonad Statement
getstatement qtype name = do
    curcontext <- get
    let stmtsfunc = getStatementsFunction curcontext
    estatement <- liftIO $ stmtsfunc qtype name
    case estatement of
        Left x -> throwError x
        Right y -> return y
pushScope name  = modify (\s -> modifyScope s (\x -> [name] ++ x))
popScope        = modify (\s -> modifyScope s tail)

-- The actual meat
evaluateStatements :: Statement -> CatalogMonad Catalog
evaluateStatements (Node name stmts _) = do
    pushScope "::"
    res <- mapM (evaluateStatements) stmts
    popScope
    return (concat res)

evaluateStatements (Include includename _) = getstatement TopClass includename >>= evaluateStatements

evaluateStatements x = throwError ("Can't evaluate " ++ (show x))
