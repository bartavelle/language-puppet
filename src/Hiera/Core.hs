{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Hiera.Core

where

import           XPrelude

import           Data.Aeson           (FromJSON, Value (..), (.!=), (.:), (.:?))
import qualified Data.Aeson           as Aeson
import           Data.Aeson.Lens
import qualified Data.Attoparsec.Text as AT
import qualified Data.Either.Strict   as S
import qualified Data.Yaml            as Yaml
import           Text.Megaparsec.Pos

import           Puppet.Language

data HieraConfigFile = HieraConfigFile
  { _version   :: Int
  , _backends  :: [Backend]
  , _hierarchy :: [InterpolableHieraString]
  } deriving (Show)

data Backend
  = YamlBackend FilePath
  | JsonBackend FilePath
  deriving (Show)

newtype InterpolableHieraString = InterpolableHieraString
  { getInterpolableHieraString :: [HieraStringPart]
  } deriving (Show)

data HieraStringPart
  = HPString Text
  | HPVariable Text
  deriving (Show)

instance Pretty HieraStringPart where
  pretty (HPString t)   = ppline t
  pretty (HPVariable v) = dullred (ppline ("%{" <> v <> "}"))
  prettyList = mconcat . map pretty


-- | The type of the Hiera API function associated to given hierarchy.
type HieraQueryFunc m = Container Text -- ^ Scope: all variables that Hiera can interpolate (the top level ones are prefixed with ::)
                     -> Text -- ^ The query
                     -> HieraQueryType
                     -> m (S.Either PrettyError (Maybe PValue))

-- | All available queries including the global and module layer
-- The environment layer is not implemented
data HieraQueryLayers m = HieraQueryLayers
  { _globalLayer :: HieraQueryFunc m
  , _moduleLayer :: Container (HieraQueryFunc m)
  }

-- | The different kind of hiera queries.
data HieraQueryType
    = QFirst   -- ^ standard hiera query
    | QUnique -- ^ hiera_array
    | QHash  -- ^ hiera_hash
    | QDeep
    { _knockoutPrefix :: Maybe Text
    , _sortMerged     :: Bool
    , _mergeHashArray :: Bool
} deriving (Show)


readQueryType :: Text -> Maybe HieraQueryType
readQueryType s =
  case s of
    "first"  -> Just QFirst
    "unique" -> Just QUnique
    "hash"   -> Just QHash
    _        -> Nothing

makeClassy ''HieraQueryLayers

data QRead = QRead
  { _qvars :: Container Text
  , _qtype :: HieraQueryType
  , _qhier :: [Value]
  }

makeClassy ''HieraConfigFile
makeLenses ''QRead

instance FromJSON HieraConfigFile where
  parseJSON =
    let
      mkHiera5 v = do
        [hierarchy_value] <- v .: "hierarchy"
        datadir <- case Object v ^? key "defaults" . key "datadir" of
          Just (String dir) -> pure dir
          Just _            -> fail "datadir should be a string"
          Nothing           -> hierarchy_value .: "datadir" .!= "hieradata"
        HieraConfigFile
            <$> pure 5
            <*> pure [ YamlBackend (toS datadir) ] -- TODO: support other backends if needed
            <*> (hierarchy_value .:? "paths" .!= [InterpolableHieraString [HPString "common.yaml"]])
      mkHiera3 v =
        HieraConfigFile
            <$> pure 3
            <*> (v .:? ":backends" .!= ["yaml"] >>= mapM mkBackend3)
            <*> (v .:? ":hierarchy" .!= [InterpolableHieraString [HPString "common"]])
       where
         mkBackend3 :: Text -> Yaml.Parser Backend
         mkBackend3 name = do
           (backendConstructor, skey) <- case name of
                                             "yaml" -> return (YamlBackend, ":yaml")
                                             "json" -> return (JsonBackend, ":json")
                                             _      -> fail ("Unknown backend " <> toS name)
           datadir <- case Object v ^? key skey . key ":datadir" of
                             Just (String dir)   -> return dir
                             Just _              -> fail ":datadir should be a string"
                             Nothing             -> return "/etc/puppet/hieradata"
           pure (backendConstructor (toS datadir))

    in
    Aeson.withObject "v3 or v5" $ \o ->
      o .:? "version" >>= \case
        Just (5::Int) -> mkHiera5 o
        Just _ -> fail "Hiera configuration version different than 5 is not supported."
        Nothing -> mkHiera3 o

instance FromJSON InterpolableHieraString where
  parseJSON (String s) = case parseInterpolableString s of
    Right x -> return (InterpolableHieraString x)
    Left rr -> fail rr
  parseJSON _ = fail "Invalid value type"



-- | An attoparsec parser that turns text into parts that are ready for interpolation
interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HPString rawPart <|> fmap HPVariable interpPart)
  where
    rawPart = AT.takeWhile1 (/= '%')
    interpPart = AT.string "%{" *> AT.takeWhile1 (/= '}') <* AT.char '}'

parseInterpolableString :: Text -> Either String [HieraStringPart]
parseInterpolableString = AT.parseOnly interpolableString

lSourceLine :: Lens' Position Pos
lSourceLine = lens sourceLine (\s l -> s { sourceLine = l })
