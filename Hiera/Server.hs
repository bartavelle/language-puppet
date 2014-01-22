{-# LANGUAGE LambdaCase, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{- | This module runs a Hiera server that caches Hiera data. There is
a huge caveat : only the data files are watched for changes, not the main configuration file.

A minor bug is that interpolation will not work for inputs containing the % character when it isn't used for interpolation.
-}
module Hiera.Server (startHiera,dummyHiera,HieraQueryFunc) where

import qualified Data.FileCache as F
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import Data.Aeson (FromJSON,Value(..),(.:?),(.!=))
import qualified Data.Either.Strict as S
import qualified Data.Maybe.Strict as S
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Tuple.Strict
import Control.Monad.Writer.Strict

import Control.Applicative
import Control.Lens
import Control.Lens.Aeson
import Puppet.Lens
import System.FilePath.Lens (directory)
import Control.Exception

import Puppet.PP hiding ((<$>))
import Puppet.Interpreter.Types
import Puppet.Interpreter.Resolve
import Puppet.Utils (strictifyEither)

data HieraConfig = HieraConfig { _hieraconfigBackends  :: [HieraBackend]
                               , _hieraconfigHierarchy :: [InterpolableHieraString]
                               , _hieraconfigBasedir   :: FilePath
                               } deriving Show

data HieraBackend = YamlBackend FilePath
                  | JsonBackend FilePath
                  deriving Show

newtype InterpolableHieraString = InterpolableHieraString [HieraStringPart]
                                  deriving Show

data HieraStringPart = HString T.Text
                     | HVariable T.Text
                     deriving Show

instance Pretty HieraStringPart where
    pretty (HString t) = ttext t
    pretty (HVariable v) = dullred (string "%{" <> ttext v <> string "}")
    prettyList = mconcat . map pretty

type HieraCache = F.FileCacheR Doc Y.Value

makeFields ''HieraConfig

instance FromJSON InterpolableHieraString where
    parseJSON (String s) = case parseInterpolableString s of
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
            <*> pure "/etc/puppet/hieradata"
    parseJSON _ = fail "Not a valid Hiera configuration"

-- | An attoparsec parser that turns text into parts that are ready for
-- interpolation
interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HString rawPart <|> fmap HVariable interpPart)
    where
        rawPart = AT.takeWhile1 (/= '%')
        interpPart = AT.string "%{" *> AT.takeWhile1 (/= '}') <* AT.char '}'

parseInterpolableString :: T.Text -> Either String [HieraStringPart]
parseInterpolableString t = AT.parseOnly interpolableString t

-- | The only method you'll ever need. It runs a Hiera server and gives you
-- a querying function. The 'Nil' output is explicitely given as a Maybe
-- type.
startHiera :: FilePath -> IO (Either String (HieraQueryFunc))
startHiera hieraconfig = Y.decodeFileEither hieraconfig >>= \case
    Left ex -> return (Left (show ex))
    Right cfg -> do
        let ncfg = cfg & basedir .~ (hieraconfig ^. directory) <> "/"
        cache <- F.newFileCache
        return (Right (query ncfg cache))

-- | A dummy hiera function that will be used when hiera is not detected
dummyHiera :: HieraQueryFunc
dummyHiera _ _ _ = return (S.Right ([] :!: S.Nothing))

-- | The combinator for "normal" queries
queryCombinator :: [LogWriter (S.Maybe PValue)] -> LogWriter (S.Maybe PValue)
queryCombinator [] = return S.Nothing
queryCombinator (x:xs) = x >>= \case
    v@(S.Just _) -> return v
    S.Nothing -> queryCombinator xs

-- | The combinator for hiera_array
queryCombinatorArray :: [LogWriter (S.Maybe PValue)] -> LogWriter (S.Maybe PValue)
queryCombinatorArray = fmap rejoin . sequence
    where
        rejoin = S.Just . PArray . V.concat . map toA
        toA S.Nothing = V.empty
        toA (S.Just (PArray r)) = r
        toA (S.Just a) = V.singleton a

-- | The combinator for hiera_array
queryCombinatorHash :: [LogWriter (S.Maybe PValue)] -> LogWriter (S.Maybe PValue)
queryCombinatorHash = fmap (S.Just . PHash . mconcat . map toH) . sequence
    where
        toH S.Nothing = mempty
        toH (S.Just (PHash h)) = h
        toH _ = throw (ErrorCall "The hiera value was not a hash")

interpolateText :: Container ScopeInformation -> T.Text -> T.Text
interpolateText vars t = case (parseInterpolableString t ^? _Right) >>= resolveInterpolable vars of
                             Just x -> x
                             Nothing -> t

resolveInterpolable :: Container ScopeInformation -> [HieraStringPart] -> Maybe T.Text
resolveInterpolable vars = fmap T.concat . mapM (resolveInterpolablePart vars)

resolveInterpolablePart :: Container ScopeInformation -> HieraStringPart -> Maybe T.Text
resolveInterpolablePart _ (HString x) = Just x
resolveInterpolablePart vars (HVariable v) = getVariable vars "::" v ^? _Right . _PString

interpolatePValue :: Container ScopeInformation -> PValue -> PValue
interpolatePValue v (PHash h) = PHash . HM.fromList . map ( (_1 %~ interpolateText v) . (_2 %~ interpolatePValue v) ) . HM.toList $ h
interpolatePValue v (PArray r) = PArray (fmap (interpolatePValue v) r)
interpolatePValue v (PString t) = PString (interpolateText v t)
interpolatePValue _ x = x

type LogWriter = WriterT InterpreterWriter IO

query :: HieraConfig -> HieraCache -> HieraQueryFunc
query (HieraConfig b h bd) cache vars hquery qtype = fmap (S.Right . prepout) (runWriterT (sequencerFunction (map query' h))) `catch` (\e -> return . S.Left . string . show $ (e :: SomeException))
    where
        prepout (a,s) = s :!: a
        sequencerFunction = case qtype of
                                Priority   -> queryCombinator
                                ArrayMerge -> queryCombinatorArray
                                HashMerge  -> queryCombinatorHash
        query' :: InterpolableHieraString -> LogWriter (S.Maybe PValue)
        query' (InterpolableHieraString strs) =
            case resolveInterpolable vars strs of
                Just s -> sequencerFunction (map (query'' s) b)
                Nothing -> warn ("Hiera: could not interpolate " <> pretty strs) >> return S.Nothing
        query'' :: T.Text -> HieraBackend -> LogWriter (S.Maybe PValue)
        query'' hieraname backend = do
            let (decodefunction, datadir, extension) = case backend of
                                                (JsonBackend d) -> (fmap (strictifyEither . (_Left %~ string). A.eitherDecode') . BS.readFile       , d, ".json")
                                                (YamlBackend d) -> (fmap (strictifyEither . (_Left %~ string . show))           . Y.decodeFileEither, d, ".yaml")
                filename = mbd <> datadir <> "/" <> T.unpack hieraname <> extension
                    where
                        mbd = case datadir of
                                  '/' : _ -> mempty
                                  _ -> bd
                mfromJSON :: Maybe Value -> LogWriter (S.Maybe PValue)
                mfromJSON Nothing = return S.Nothing
                mfromJSON (Just v) = case A.fromJSON v of
                                         A.Success a -> return (S.Just (interpolatePValue vars a))
                                         _ -> warn ("Hiera: could not convert this Value to a Puppet type: " <> string (show v)) >> return S.Nothing
            v <- liftIO (F.query cache filename (decodefunction filename))
            case v of
                S.Left r -> debug ("Hiera: error when reading file " <> string filename <+> r) >> return S.Nothing
                S.Right x -> mfromJSON (x ^? key hquery)
