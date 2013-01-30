module PuppetDB.TestDB (initTestDBFunctions) where

import PuppetDB.Query
import Puppet.Interpreter.Types
import Puppet.DSL.Types hiding (Value)

import Data.Aeson
import qualified Data.Map as Map
import Control.Concurrent.MVar

type ExportedResources = Map.Map String (FinalCatalog, EdgeMap, FinalCatalog)

initTestDBFunctions :: (String -> Query -> IO (Either String Value)) -> IO (String -> Query -> IO (Either String Value), String -> (FinalCatalog, EdgeMap, FinalCatalog) -> IO ())
initTestDBFunctions defaultquery = do
    v <- newMVar (Map.empty)
    return (queryPDB v defaultquery, updatePDB v)

updatePDB :: MVar ExportedResources -> String -> (FinalCatalog, EdgeMap, FinalCatalog) -> IO ()
updatePDB v node res = do
    ex <- takeMVar v
    let ex' = Map.insert node res ex
    putMVar v ex'

evaluateQueryResource :: Query -> (Bool -> String -> ResIdentifier -> RResource -> Either String Bool)
evaluateQueryResource (Query OAnd lst) e n rid rr = fmap and (mapM (\x -> evaluateQueryResource x e n rid rr) lst)
evaluateQueryResource (Query OOr  lst) e n rid rr = fmap or  (mapM (\x -> evaluateQueryResource x e n rid rr) lst)
evaluateQueryResource (Query OEqual [Terms ["node","active"],Term "true"]) _ _ _ _ = Right True
evaluateQueryResource (Query OEqual [Term "type",Term ctype]) _ _ _ rr = Right (capitalizeResType (rrtype rr) == ctype)
evaluateQueryResource q _ _ _ _ = Left ("Not interpreted: " ++ show q)

queryPDB :: MVar ExportedResources -> (String -> Query -> IO (Either String Value)) -> String -> Query -> IO (Either String Value)
queryPDB v _ "resources" query = do
    ex <- readMVar v
    let isSelected = evaluateQueryResource query
        sortResources :: Either String ([(String,ResIdentifier,RResource)], [(String,ResIdentifier,RResource)])
                      -> String
                      -> (FinalCatalog, EdgeMap, FinalCatalog)
                      -> Either String ([(String,ResIdentifier,RResource)], [(String,ResIdentifier,RResource)])
        sortResources (Left rr) _ _ = Left rr
        sortResources (Right (curnormal, curexported)) nodename (fnormal, _, fexported) =
            let newnormal   = Map.foldlWithKey' (sortResources' False nodename) (Right curnormal  ) fnormal
                newexported = Map.foldlWithKey' (sortResources' True  nodename) (Right curexported) fexported
            in case (newnormal, newexported) of
                   (Left r1, _)       -> Left r1
                   (_, Left r2)       -> Left r2
                   (Right n, Right e) -> Right (n,e)
        sortResources' :: Bool -> String -> Either String [(String,ResIdentifier,RResource)] -> ResIdentifier -> RResource -> Either String [(String,ResIdentifier,RResource)]
        sortResources' _ _ (Left rr) _ _ = Left rr
        sortResources' e nodename (Right curlist) resid rr = case isSelected e nodename resid rr of
                                                                 Right False -> Right curlist
                                                                 Right True  -> Right ((nodename,resid,rr) : curlist)
                                                                 Left  err   -> Left err
        jsonize :: (String,ResIdentifier,RResource) -> Value
        jsonize (h,_,r) = rr2json h r
    case Map.foldlWithKey' sortResources (Right ([], [])) ex of
        Right (n,e) -> return $ Right $ toJSON $ map jsonize ( n ++ e )
        Left rr     -> return (Left rr)

queryPDB _ _ querytype query = error (show (querytype, query))
