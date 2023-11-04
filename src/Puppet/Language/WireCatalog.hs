{-# LANGUAGE TemplateHaskell #-}

module Puppet.Language.WireCatalog where

import Data.Aeson
import Puppet.Language.Core
import Puppet.Language.Resource
import XPrelude

-- | Used to represent a relationship between two resources within the wired format (json).
--
-- See <http://docs.puppetlabs.com/puppetdb/2.3/api/wire_format/catalog_format_v5.html#data-type-edge>
data PuppetEdge = PuppetEdge RIdentifier RIdentifier LinkType deriving (Show)

instance FromJSON PuppetEdge where
  parseJSON (Object v) = PuppetEdge <$> v .: "source" <*> v .: "target" <*> v .: "relationship"
  parseJSON _ = fail "invalid puppet edge"

instance ToJSON PuppetEdge where
  toJSON (PuppetEdge s t r) = object [("source", toJSON s), ("target", toJSON t), ("relationship", toJSON r)]

-- | See <http://docs.puppetlabs.com/puppetdb/1.5/api/wire_format/catalog_format.html puppet reference>.
data WireCatalog = WireCatalog
  { _wireCatalogNodename :: !NodeName,
    _wireCatalogVersion :: !Text,
    _wireCatalogEdges :: !(Vector PuppetEdge),
    _wireCatalogResources :: !(Vector Resource),
    _wireCatalogTransactionUUID :: !Text
  }
  deriving (Show)

makeClassy ''WireCatalog

instance FromJSON WireCatalog where
  parseJSON (Object d) =
    d .: "data" >>= \case
      (Object v) ->
        WireCatalog
          <$> v .: "name"
          <*> v .: "version"
          <*> v .: "edges"
          <*> v .: "resources"
          <*> v .: "transaction-uuid"
      _ -> fail "Data is not an object"
  parseJSON _ = fail "invalid wire catalog"

instance ToJSON WireCatalog where
  toJSON (WireCatalog n v e r t) = object [("metadata", object [("api_version", Number 1)]), ("data", object d)]
    where
      d =
        [ ("name", String n),
          ("version", String v),
          ("edges", toJSON e),
          ("resources", toJSON r),
          ("transaction-uuid", String t)
        ]
