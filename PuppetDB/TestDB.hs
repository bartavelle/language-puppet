module PuppetDB.TestDB (initTestDBFunctions) where

import Puppet.Interpreter.Types
import Puppet.Parser.Types

import Data.Aeson
import qualified Data.HashMap.Strict as HS
import Control.Concurrent.MVar
import qualified Data.Text as T

type ExportedResources = Container (FinalCatalog, EdgeMap, FinalCatalog)

initTestDBFunctions :: (T.Text -> Query -> IO (Either String Value)) -> IO (T.Text -> Query -> IO (Either String Value), T.Text -> (FinalCatalog, EdgeMap, FinalCatalog) -> IO ())
initTestDBFunctions defaultquery = do
    v <- newMVar HM.empty
    return (queryPDB v defaultquery, updatePDB v)

updatePDB :: MVar ExportedResources -> T.Text -> (FinalCatalog, EdgeMap, FinalCatalog) -> IO ()
updatePDB v node res = do
    ex <- takeMVar v
    let ex' = HM.insert node res ex
    putMVar v ex'

toBool :: T.Text -> Either String Bool
toBool "true"  = Right True
toBool "false" = Right False
toBool x       = Left ("Is not a boolean " ++ T.unpack x)

evaluateQueryResource :: Query -> Bool -> T.Text -> ResIdentifier -> RResource -> Either String Bool
evaluateQueryResource (Query OAnd lst) e n rid rr = fmap and (mapM (\x -> evaluateQueryResource x e n rid rr) lst)
evaluateQueryResource (Query OOr  lst) e n rid rr = fmap or  (mapM (\x -> evaluateQueryResource x e n rid rr) lst)
evaluateQueryResource (Query OEqual [Terms ["node","active"], Term bool]) _ _ _ _ = toBool bool
evaluateQueryResource (Query OEqual [Terms ["node","name"], Term hname]) _ n _ _ = Right (n == hname)
evaluateQueryResource (Query OEqual [Term "type",Term ctype]) _ _ _ rr = Right (capitalizeResType (rrtype rr) == ctype)
evaluateQueryResource (Query OEqual [Term "exported",Term expo]) exported _ _ _ = toBool expo >>= \x -> return (x == exported)
evaluateQueryResource (Query OEqual [Term "tag",Term tag]) _ _ _ rr    =
    let tags = HM.findWithDefault (ResolvedArray []) "tag" (rrparams rr)
        stringEqual y (ResolvedString x) = (x == y)
        stringEqual _ _ = False
    in  case tags of
            ResolvedArray lst -> Right (any (stringEqual tag) lst)
            _ -> Right (stringEqual tag tags)
evaluateQueryResource (Query OEqual [Term "title", Term ttl]) _ _ _ rr = Right (rrname rr == ttl)
evaluateQueryResource (Query ONot [q]) e n rid rr = fmap not (evaluateQueryResource q e n rid rr)
evaluateQueryResource q _ _ _ _ = Left ("Not interpreted: " ++ show q)

queryPDB :: MVar ExportedResources -> (T.Text -> Query -> IO (Either String Value)) -> T.Text -> Query -> IO (Either String Value)
queryPDB v _ "resources" query = do
    ex <- readMVar v
    let isSelected = evaluateQueryResource query
        sortResources :: Either String ([(T.Text,ResIdentifier,RResource)], [(T.Text,ResIdentifier,RResource)])
                      -> T.Text
                      -> (FinalCatalog, EdgeMap, FinalCatalog)
                      -> Either String ([(T.Text,ResIdentifier,RResource)], [(T.Text,ResIdentifier,RResource)])
        sortResources (Left rr) _ _ = Left rr
        sortResources (Right (curnormal, curexported)) nodename (fnormal, _, fexported) =
            let newnormal   = HM.foldlWithKey' (sortResources' False nodename) (Right curnormal  ) fnormal
                newexported = HM.foldlWithKey' (sortResources' True  nodename) (Right curexported) fexported
            in case (newnormal, newexported) of
                   (Left r1, _)       -> Left r1
                   (_, Left r2)       -> Left r2
                   (Right n, Right e) -> Right (n,e)
        sortResources' :: Bool -> T.Text -> Either String [(T.Text,ResIdentifier,RResource)] -> ResIdentifier -> RResource -> Either String [(T.Text,ResIdentifier,RResource)]
        sortResources' _ _ (Left rr) _ _ = Left rr
        sortResources' e nodename (Right curlist) resid rr = case isSelected e nodename resid rr of
                                                                 Right False -> Right curlist
                                                                 Right True  -> Right ((nodename,resid,rr) : curlist)
                                                                 Left  err   -> Left err
        jsonize :: (T.Text,ResIdentifier,RResource) -> Value
        jsonize (h,_,r) = rr2json h r
    case HM.foldlWithKey' sortResources (Right ([], [])) ex of
        Right (n,e) -> return $ Right $ toJSON $ map jsonize ( n ++ e )
        Left rr     -> return (Left rr)

queryPDB _ _ querytype query = error ("queryPDB:" ++ show (querytype, query))
