{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}

{- | This module runs a Hiera server that caches Hiera data. There is
a huge caveat : only the data files are watched for changes, not the main configuration file.

A minor bug is that interpolation will not work for inputs containing the % character when it isn't used for interpolation.
-}
module Hiera.Server (
    startHiera
  , dummyHiera
    -- * Re-export (query API)
  , HieraQueryFunc
) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Writer.Strict
import           Data.Aeson                  (FromJSON, Value (..), (.!=), (.:?))
import qualified Data.Aeson                  as A
import           Data.Aeson.Lens
import qualified Data.Attoparsec.Text        as AT
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Either.Strict          as S
import qualified Data.FileCache              as F
import qualified Data.HashMap.Strict         as HM
import qualified Data.List                   as L
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Yaml                   as Y
import           System.FilePath.Lens        (directory)
import qualified System.Log.Logger           as LOG

import           Puppet.Interpreter.Types
import           Puppet.PP                   hiding ((<$>))
import           Puppet.Utils                (strictifyEither)

loggerName :: String
loggerName = "Hiera.Server"

data HieraConfigFile = HieraConfigFile
    { _backends  :: [Backend]
    , _hierarchy :: [InterpolableHieraString]
    } deriving (Show)

data Backend = YamlBackend FilePath
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

type Cache = F.FileCacheR String Y.Value

makeClassy ''HieraConfigFile

instance FromJSON InterpolableHieraString where
    parseJSON (String s) = case parseInterpolableString s of
                               Right x -> return (InterpolableHieraString x)
                               Left rr -> fail rr
    parseJSON _ = fail "Invalid value type"

instance FromJSON HieraConfigFile where
    parseJSON (Object v) = do
        let genBackend :: T.Text -> Y.Parser Backend
            genBackend name = do
                (backendConstructor, skey) <- case name of
                                                  "yaml" -> return (YamlBackend, ":yaml")
                                                  "json" -> return (JsonBackend, ":json")
                                                  _      -> fail ("Unknown backend " ++ T.unpack name)
                datadir <- case Object v ^? key skey . key ":datadir" of
                                  Just (String dir)   -> return dir
                                  Just _              -> fail ":datadir should be a string"
                                  Nothing             -> return "/etc/puppet/hieradata"
                return (backendConstructor (T.unpack datadir))
        HieraConfigFile
            <$> (v .:? ":backends" .!= ["yaml"] >>= mapM genBackend)
            <*> (v .:? ":hierarchy" .!= [InterpolableHieraString [HString "common"]])
    parseJSON _ = fail "Not a valid Hiera configuration"

-- | An attoparsec parser that turns text into parts that are ready for interpolation
interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HString rawPart <|> fmap HVariable interpPart)
    where
        rawPart = AT.takeWhile1 (/= '%')
        interpPart = AT.string "%{" *> AT.takeWhile1 (/= '}') <* AT.char '}'

parseInterpolableString :: T.Text -> Either String [HieraStringPart]
parseInterpolableString = AT.parseOnly interpolableString

-- | The only method you'll ever need. It runs a Hiera server and gives you
-- a querying function. The 'Nil' output is explicitely given as a Maybe
-- type.
startHiera :: FilePath -> IO (Either String (HieraQueryFunc IO))
startHiera fp = Y.decodeFileEither fp >>= \case
    Left (Y.InvalidYaml (Just (Y.YamlException "Yaml file not found: hiera.yaml"))) -> return (Right dummyHiera)
    Left ex   -> return (Left (show ex))
    Right cfg -> do
        cache <- F.newFileCache
        return (Right (query cfg fp cache))

-- | A dummy hiera function that will be used when hiera is not detected
dummyHiera :: Monad m => HieraQueryFunc m
dummyHiera _ _ _ = return $ S.Right Nothing

-- | The combinator for "normal" queries
queryCombinator :: HieraQueryType -> [IO (Maybe PValue)] -> IO (Maybe PValue)
queryCombinator Priority = foldr (liftA2 mplus) (pure mzero)
queryCombinator ArrayMerge  = fmap rejoin . sequence
    where
        rejoin = Just . PArray . V.concat . map toA
        toA Nothing = V.empty
        toA (Just (PArray r)) = r
        toA (Just a) = V.singleton a
queryCombinator HashMerge = fmap (Just . PHash . mconcat . map toH) . sequence
    where
        toH Nothing = mempty
        toH (Just (PHash h)) = h
        toH _ = error "The hiera value was not a hash"

interpolateText :: Container T.Text -> T.Text -> T.Text
interpolateText vars t = fromMaybe t ((parseInterpolableString t ^? _Right) >>= resolveInterpolable vars)

resolveInterpolable :: Container T.Text -> [HieraStringPart] -> Maybe T.Text
resolveInterpolable vars = fmap T.concat . mapM resolvePart
  where
    resolvePart :: HieraStringPart -> Maybe T.Text
    resolvePart (HString x) = Just x
    resolvePart (HVariable v) = vars ^. at v

interpolatePValue :: Container T.Text -> PValue -> PValue
interpolatePValue v (PHash h) = PHash . HM.fromList . map ( (_1 %~ interpolateText v) . (_2 %~ interpolatePValue v) ) . HM.toList $ h
interpolatePValue v (PArray r) = PArray (fmap (interpolatePValue v) r)
interpolatePValue v (PString t) = PString (interpolateText v t)
interpolatePValue _ x = x

query :: HieraConfigFile -> FilePath -> Cache -> HieraQueryFunc IO
query (HieraConfigFile {_backends, _hierarchy}) fp cache vars hquery qtype =
    fmap S.Right (queryCombinator qtype (map query' _hierarchy)) `catch` (\e -> return . S.Left . PrettyError . string . show $ (e :: SomeException))
    where
        varlist = hcat (L.intersperse comma (map (dullblue . ttext) (L.sort (HM.keys vars))))
        query' :: InterpolableHieraString -> IO (Maybe PValue)
        query' (InterpolableHieraString strs) =
            case resolveInterpolable vars strs of
                Just s  -> queryCombinator qtype (map (query'' s) _backends)
                Nothing -> do
                    LOG.noticeM loggerName (show $ "Hiera lookup: skipping using hierarchy level" <+> pretty strs
                                            <$$> "It couldn't be interpolated with known variables:" <+> varlist)
                    return Nothing
        query'' :: T.Text -> Backend -> IO (Maybe PValue)
        query'' hierastring backend = do
            let (decodefunction, datadir, extension) = case backend of
                                                (JsonBackend d) -> (fmap (strictifyEither . A.eitherDecode') . BS.readFile       , d, ".json")
                                                (YamlBackend d) -> (fmap (strictifyEither . (_Left %~ show)) . Y.decodeFileEither, d, ".yaml")
                filename = basedir <> datadir <> "/" <> T.unpack hierastring <> extension
                    where
                      basedir = case datadir of
                                  '/' : _ -> mempty
                                  _       -> fp^.directory <> "/"
                mfromJSON :: Maybe Value -> IO (Maybe PValue)
                mfromJSON Nothing = return Nothing
                mfromJSON (Just v) = case A.fromJSON v of
                                         A.Success a -> return (Just (interpolatePValue vars a))
                                         _           -> do
                                             LOG.warningM loggerName (show $ "Hiera:" <+> dullred "could not convert this Value to a Puppet type" <> ":" <+> string (show v))
                                             return Nothing
            v <- liftIO (F.query cache filename (decodefunction filename))
            case v of
                S.Left r -> do
                    let errs = "Hiera: error when reading file " <> string filename <+> string r
                    if "Yaml file not found: " `L.isInfixOf` r
                        then LOG.debugM loggerName (show errs)
                        else LOG.warningM loggerName (show errs)
                    return Nothing
                S.Right x -> mfromJSON (x ^? key hquery)
