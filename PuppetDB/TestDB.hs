{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.TestDB (initPuppetDB) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.Vector as V
import Control.Lens
import Control.Concurrent.STM
import Data.Monoid
import Control.Applicative

import Puppet.Interpreter.Types
import PuppetDB.Query

data PDBEntry = PDBEntry { _pdbfacts     :: Facts
                         , _lastCatalog  :: V.Vector Resource
                         , _lastExported :: V.Vector Resource
                         }

type PDBContent = Container PDBEntry

makeClassy ''PDBEntry

instance ToJSON PDBEntry where
    toJSON p = object [ ("facts"   , toJSON (p ^. pdbfacts))
                      , ("catalog" , toJSON (p ^. lastCatalog))
                      , ("exported", toJSON (p ^. lastExported))
                      ]

instance FromJSON PDBEntry where
    parseJSON (Object o) = PDBEntry    <$>
                        o .: "facts"   <*>
                        o .: "catalog" <*>
                        o .: "exported"
    parseJSON _ = mempty

initPuppetDB :: IO (T.Text -> Value -> IO (S.Either String Value))
initPuppetDB = newTVarIO mempty >>= return . queryDB

mkFilter :: ResourcesQuery3 -> (T.Text, Resource) -> Bool
mkFilter (ResOr qs)                   rp    = any (\q -> mkFilter q rp) qs
mkFilter (ResAnd qs)                  rp    = all (\q -> mkFilter q rp) qs
mkFilter (ResNot q)                   rp    = not (mkFilter q rp)
mkFilter (ResEqual RFTag v)           (_,r) = r ^. rtags . contains v
mkFilter (ResEqual RFType v)          (_,r) = r ^. rid . itype == v
mkFilter (ResEqual RFTitle v)         (_,r) = r ^. rid . iname == v
mkFilter (ResEqual RFCertname v)      (c,_) = v == c
mkFilter (ResEqual (RFParameter p) v) (_,r) = r ^. rattributes . at p == Just (PString v)
mkFilter _ _ = error "TODO: mkFilter"

resourcesEndpoint :: ResourcesQuery3 -> PDBContent -> V.Vector Resource
resourcesEndpoint q = V.map snd . V.filter (mkFilter q) . V.concatMap associate . V.fromList . itoList
    where
        associate (nodename, pdbentry) = V.map (\e -> (nodename, e)) (pdbentry ^. lastCatalog <> pdbentry ^. lastExported)

queryDB :: TVar PDBContent -> T.Text -> Value -> IO (S.Either String Value)
queryDB tv "resources" v = case fromJSON v of
                               Success x -> fmap (S.Right . toJSON . resourcesEndpoint x) (atomically (readTVar tv))
                               Error err -> return (S.Left err)
queryDB _ "nodes" _     = return (S.Left "nodes endpoint work in progress")
queryDB _ "facts" _     = return (S.Left "facts endpoint work in progress")
queryDB tv "dumpdb" _   = fmap (S.Right . toJSON) (atomically (readTVar tv))
queryDB tv "replacedb" v = case fromJSON v of
                               Success x -> atomically (writeTVar tv x) >> return (S.Right (String "success"))
                               Error s -> return (S.Left s)
queryDB tv "updatenode" v = case fromJSON v of
                                Success (nodename, pdb) -> atomically (modifyTVar tv (at nodename ?~ pdb)) >> return (S.Right (String "success"))
                                Error rr -> return (S.Left rr)
queryDB _ r _           = return (S.Left ("Unknown endpoint: " ++ show r))
