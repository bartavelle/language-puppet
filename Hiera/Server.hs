{-# LANGUAGE LambdaCase #-}
{- | This module runs a Hiera server that caches Hiera data. There is
a huge caveat : only the data files are watched for changes, not the main configuration file.

A minor bug is that interpolation will not work for inputs containing the % character when it isn't used for interpolation.
-}
module Hiera.Server (startHiera,dummyHiera) where

import qualified Data.FileCache as F
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import Data.Aeson (FromJSON,Value(..),(.:?),(.!=))
import qualified Data.Either.Strict as S
import qualified Data.Maybe.Strict as S
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as BS
import Control.Applicative
import Control.Lens
import Control.Lens.Aeson
import Control.Exception
import Data.Monoid

import Puppet.PP hiding ((<$>))
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve
import Puppet.Utils (strictifyEither)

data HieraConfig = HieraConfig { _hieraconfigBackends  :: [HieraBackend]
                               , _hieraconfigHierarchy :: [InterpolableHieraString]
                               } deriving Show

data HieraBackend = YamlBackend FilePath
                  | JsonBackend FilePath
                  deriving Show

newtype InterpolableHieraString = InterpolableHieraString [HieraStringPart]
                                  deriving Show

data HieraStringPart = HString T.Text
                     | HVariable T.Text
                     deriving Show

type HieraCache = F.FileCacheR Doc Y.Value

instance FromJSON InterpolableHieraString where
    parseJSON (String s) = case AT.parseOnly interpolableString s of
                               Right x -> return (InterpolableHieraString x)
                               Left rr -> fail rr
    parseJSON _ = fail "Invalid value type"

instance FromJSON HieraConfig where
    parseJSON (Object v) = do
        let genBackend :: T.Text -> Y.Parser HieraBackend
            genBackend backendname = do
                (backendConstructor, skey) <- case backendname of
                                                  "yaml" -> return (YamlBackend, ":yaml")
                                                  "json" -> return (JsonBackend, ":json")
                                                  _ -> fail ("Unknown backend " ++ T.unpack backendname)
                datadir <- case (Object v) ^? key skey . key ":datadir" of
                                  Just (String dtdir) -> return dtdir
                                  Just _              -> fail ":datadir should be a string"
                                  Nothing             -> return "/etc/puppet/hieradata"
                return (backendConstructor (T.unpack datadir))
        HieraConfig
            <$> (v .:? ":backends" .!= ["yaml"] >>= mapM genBackend)
            <*> (v .:? ":hierarchy" .!= [InterpolableHieraString [HString "common"]])
    parseJSON _ = fail "Not a valid Hiera configuration"

interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HString rawPart <|> fmap HVariable interpPart)
    where
        rawPart = AT.takeWhile1 (/= '%')
        interpPart = AT.string "%{" *> AT.takeWhile1 (/= '}') <* AT.char '}'

-- | The only method you'll ever need. It runs a Hiera server and gives you
-- a querying function. The 'Nil' output is explicitely given as a Maybe
-- type.
startHiera :: FilePath -> IO (Either String (Container ScopeInformation -> T.Text -> IO (S.Either Doc (S.Maybe PValue))))
startHiera hieraconfig = Y.decodeFileEither hieraconfig >>= \case
    Left ex -> return (Left (show ex))
    Right cfg -> do
        cache <- F.newFileCache
        return (Right (query cfg cache))

-- | A dummy hiera function that will be used when hiera is not detected
dummyHiera :: Container ScopeInformation -> T.Text -> IO (S.Either Doc (S.Maybe PValue))
dummyHiera _ _ = return (S.Right S.Nothing)

runUntilJ :: [IO (S.Maybe a)] -> IO (S.Maybe a)
runUntilJ [] = return S.Nothing
runUntilJ (x:xs) = x >>= \case
    v@(S.Just _) -> return v
    S.Nothing -> runUntilJ xs

query :: HieraConfig -> HieraCache -> Container ScopeInformation -> T.Text -> IO (S.Either Doc (S.Maybe PValue))
query (HieraConfig b h) cache vars hquery = fmap S.Right (runUntilJ (map query' h)) `catch` (\e -> return . S.Left . string . show $ (e :: SomeException))
    where
        query' :: InterpolableHieraString -> IO (S.Maybe PValue)
        query' (InterpolableHieraString strs) =
            case fmap T.concat (mapM resolveInterpolablePart strs) of
                Just s -> runUntilJ (map (query'' s) b)
                Nothing -> return S.Nothing
        resolveInterpolablePart :: HieraStringPart -> Maybe T.Text
        resolveInterpolablePart (HString x) = Just x
        resolveInterpolablePart (HVariable v) = getVariable vars "::" v ^? _Right . _PString
        query'' :: T.Text -> HieraBackend -> IO (S.Maybe PValue)
        query'' hieraname backend = do
            let (decodefunction, datadir, extension) = case backend of
                                                (JsonBackend d) -> (fmap (strictifyEither . (_Left %~ string). A.eitherDecodeStrict') . BS.readFile       , d, ".json")
                                                (YamlBackend d) -> (fmap (strictifyEither . (_Left %~ string . show))                 . Y.decodeFileEither, d, ".yaml")
                filename = datadir <> "/" <> T.unpack hieraname <> extension
                mfromJSON x = case A.fromJSON x of
                                  A.Success a -> Just a
                                  _ -> Nothing
                strictifyMaybe (Just x) = S.Just x
                strictifyMaybe _ = S.Nothing
            v <- F.query cache filename (decodefunction filename)
            case v of
                S.Left _ -> return S.Nothing
                S.Right x -> return $ strictifyMaybe (x ^? key hquery >>= mfromJSON)
