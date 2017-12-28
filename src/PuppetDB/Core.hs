{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.Core

where

import           XPrelude          hiding (Read)

import           Control.Lens
import           Data.Aeson
import qualified Data.List         as List
import           Data.Maybe        (fromJust)
import qualified Data.Maybe.Strict as S
import           Data.Time.Clock
import           GHC.Read          (Read (..))
import           Web.HttpApiData   (ToHttpApiData (..))

import           Facter
import           Puppet.Language

-- | The supported PuppetDB implementations.
data PDBType
  = PDBRemote -- ^ Your standard PuppetDB, queried through the HTTP interface.
  | PDBDummy -- ^ A stupid stub, this is the default choice.
  | PDBTest -- ^ A slow but handy PuppetDB implementation that is backed by a YAML file.
  deriving (Eq)

instance Read PDBType where
  readsPrec _ r | isJust reml = [(PDBRemote, fromJust reml)]
                | isJust rems = [(PDBRemote, fromJust rems)]
                | isJust duml = [(PDBDummy, fromJust duml)]
                | isJust dums = [(PDBDummy, fromJust dums)]
                | isJust tstl = [(PDBTest, fromJust tstl)]
                | isJust tsts = [(PDBTest, fromJust tsts)]
                | otherwise   = []
    where
      reml = List.stripPrefix "PDBRemote" r
      rems = List.stripPrefix "remote"    r
      duml = List.stripPrefix "PDBDummy"  r
      dums = List.stripPrefix "dummy"     r
      tstl = List.stripPrefix "PDBTest"   r
      tsts = List.stripPrefix "test"      r

data NodeInfo = NodeInfo
    { _nodeInfoName        :: !NodeName
    , _nodeInfoDeactivated :: !Bool
    , _nodeInfoCatalogT    :: !(S.Maybe UTCTime)
    , _nodeInfoFactsT      :: !(S.Maybe UTCTime)
    , _nodeInfoReportT     :: !(S.Maybe UTCTime)
    }

makeClassy ''NodeInfo

instance ToJSON NodeInfo where
  toJSON p = object [ ("name"             , toJSON (p ^. nodeInfoName))
                    , ("deactivated"      , toJSON (p ^. nodeInfoDeactivated))
                    , ("catalog_timestamp", toJSON (p ^. nodeInfoCatalogT))
                    , ("facts_timestamp"  , toJSON (p ^. nodeInfoFactsT))
                    , ("report_timestamp" , toJSON (p ^. nodeInfoReportT))
                    ]

instance FromJSON NodeInfo where
  parseJSON (Object v) = NodeInfo <$> v .:  "name"
                                  <*> v .:? "deactivated" .!= False
                                  <*> v .:  "catalog_timestamp"
                                  <*> v .:  "facts_timestamp"
                                  <*> v .:  "report_timestamp"
  parseJSON _ = fail "invalide node info"

-- | Pretty straightforward way to define the various PuppetDB queries
data Query a
  = QEqual a Text
  | QG a Integer
  | QL a Integer
  | QGE a Integer
  | QLE a Integer
  | QMatch Text Text
  | QAnd [Query a]
  | QOr [Query a]
  | QNot (Query a)
  | QEmpty

instance ToJSON a => ToJSON (Query a) where
  toJSON (QOr qs)          = toJSON ("or" : map toJSON qs)
  toJSON (QAnd qs)         = toJSON ("and" : map toJSON qs)
  toJSON (QNot q)          = toJSON [ "not" , toJSON q ]
  toJSON (QEqual flds val) = toJSON [ "=",  toJSON flds, toJSON val ]
  toJSON (QMatch flds val) = toJSON [ "~",  toJSON flds, toJSON val ]
  toJSON (QL     flds val) = toJSON [ "<",  toJSON flds, toJSON val ]
  toJSON (QG     flds val) = toJSON [ ">",  toJSON flds, toJSON val ]
  toJSON (QLE    flds val) = toJSON [ "<=", toJSON flds, toJSON val ]
  toJSON (QGE    flds val) = toJSON [ ">=", toJSON flds, toJSON val ]
  toJSON  QEmpty           = Null

instance ToJSON a => ToHttpApiData (Query a) where
  toHeader = Control.Lens.view strict . encode
  toUrlPiece = decodeUtf8 . toHeader

instance FromJSON a => FromJSON (Query a) where
  parseJSON Null = pure QEmpty
  parseJSON (Array elems) = case toList elems of
    ("or":xs)          -> QOr    <$> mapM parseJSON xs
    ("and":xs)         -> QAnd   <$> mapM parseJSON xs
    ["not",x]          -> QNot   <$> parseJSON x
    [ "=", flds, val ] -> QEqual <$> parseJSON flds    <*> parseJSON val
    [ "~", flds, val ] -> QEqual <$> parseJSON flds    <*> parseJSON val
    [ ">", flds, val ] -> QG     <$> parseJSON flds    <*> parseJSON val
    [ "<", flds, val ] -> QL     <$> parseJSON flds    <*> parseJSON val
    [">=", flds, val ] -> QGE    <$> parseJSON flds    <*> parseJSON val
    ["<=", flds, val ] -> QLE    <$> parseJSON flds    <*> parseJSON val
    x                  -> fail ("unknown query" ++ show x)
  parseJSON _ = fail "Expected an array"

-- | Fields for the fact endpoint
data FactField
  = FName
  | FValue
  | FCertname

instance ToJSON FactField where
  toJSON FName     = "name"
  toJSON FValue    = "value"
  toJSON FCertname = "certname"

instance FromJSON FactField where
  parseJSON "name"     = pure FName
  parseJSON "value"    = pure FValue
  parseJSON "certname" = pure FCertname
  parseJSON _          = fail "Can't parse fact field"

-- | Fields for the node endpoint
data NodeField = NName | NFact Text

instance ToJSON NodeField where
  toJSON NName     = "name"
  toJSON (NFact t) = toJSON [ "fact", t ]

instance FromJSON NodeField where
  parseJSON (Array xs) = case toList xs of
                             ["fact", x] -> NFact <$> parseJSON x
                             _           -> fail "Invalid field syntax"
  parseJSON (String "name") = pure NName
  parseJSON _ = fail "invalid field"

-- | Fields for the resource endpoint
data ResourceField
  = RTag
  | RCertname
  | RParameter Text
  | RType
  | RTitle
  | RExported
  | RFile
  | RLine

instance ToJSON ResourceField where
  toJSON RTag           = "tag"
  toJSON RCertname      = "certname"
  toJSON (RParameter t) = toJSON ["parameter", t]
  toJSON RType          = "type"
  toJSON RTitle         = "title"
  toJSON RExported      = "exported"
  toJSON RFile          = "file"
  toJSON RLine          = "line"

instance FromJSON ResourceField where
  parseJSON (Array xs) =
    case toList xs of
      ["parameter", x] -> RParameter <$> parseJSON x
      _                -> fail "Invalid field syntax"
  parseJSON (String "tag"     ) = pure RTag
  parseJSON (String "certname") = pure RCertname
  parseJSON (String "type"    ) = pure RType
  parseJSON (String "title"   ) = pure RTitle
  parseJSON (String "exported") = pure RExported
  parseJSON (String "file"    ) = pure RFile
  parseJSON (String "line"    ) = pure RLine
  parseJSON _ = fail "invalid field"

data PuppetDBAPI m = PuppetDBAPI
  { pdbInformation     :: m Doc
  , replaceCatalog     :: WireCatalog         -> ExceptT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-catalog-version-3>
  , replaceFacts       :: [(NodeName, Facts)] -> ExceptT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-facts-version-1>
  , deactivateNode     :: NodeName            -> ExceptT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#deactivate-node-version-1>
  , getFacts           :: Query FactField     -> ExceptT PrettyError m [FactInfo] -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/facts.html#get-v3facts>
  , getResources       :: Query ResourceField -> ExceptT PrettyError m [Resource] -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/resources.html#get-v3resources>
  , getNodes           :: Query NodeField     -> ExceptT PrettyError m [NodeInfo]
  , commitDB           ::                        ExceptT PrettyError m () -- ^ This is only here to tell the test PuppetDB to save its content to disk.
  , getResourcesOfNode :: NodeName -> Query ResourceField -> ExceptT PrettyError m [Resource]
  }
