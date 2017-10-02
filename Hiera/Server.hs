{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

{- | This module runs a Hiera server that caches Hiera data. There is
a huge caveat : only the data files are watched for changes, not the main configuration file.

A minor bug is that interpolation will not work for inputs containing the % character when it isn't used for interpolation.
-}
module Hiera.Server (
    startHiera
  , dummyHiera
  , hieraLoggerName
    -- * Query API
  , HieraQueryFunc
) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Writer.Strict
import           Data.Aeson                  (FromJSON, Value (..), (.!=), (.:?))
import qualified Data.Aeson                  as A
import           Data.Aeson.Lens
import qualified Data.Attoparsec.Text        as AT
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Either.Strict          as S
import qualified Data.FileCache              as F
import qualified Data.List                   as L
import           Data.Maybe                  (catMaybes, mapMaybe)
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Yaml                   as Y
import           System.FilePath.Lens        (directory)
import qualified System.Log.Logger           as LOG

import           Puppet.Interpreter.Types
import           Puppet.PP
import           Puppet.Utils                (strictifyEither)

hieraLoggerName :: String
hieraLoggerName = "Hiera.Server"

data HieraConfigFile = HieraConfigFile
    { _backends  :: [Backend]
    , _hierarchy :: [InterpolableHieraString]
    } deriving (Show)

data Backend = YamlBackend FilePath
             | JsonBackend FilePath
             deriving Show

newtype InterpolableHieraString = InterpolableHieraString { getInterpolableHieraString :: [HieraStringPart] }
                                  deriving Show

data HieraStringPart = HPString T.Text
                     | HPVariable T.Text
                     deriving Show

instance Pretty HieraStringPart where
    pretty (HPString t) = ttext t
    pretty (HPVariable v) = dullred (string "%{" <> ttext v <> string "}")
    prettyList = mconcat . map pretty

type Cache = F.FileCacheR String Value

data QRead
    = QRead
    { _qvars :: Container T.Text
    , _qtype :: HieraQueryType
    , _qhier :: [Value]
    }

makeClassy ''HieraConfigFile
makeLenses ''QRead

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
            <*> (v .:? ":hierarchy" .!= [InterpolableHieraString [HPString "common"]])
    parseJSON _ = fail "Not a valid Hiera configuration"

-- | An attoparsec parser that turns text into parts that are ready for interpolation
interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HPString rawPart <|> fmap HPVariable interpPart)
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

resolveString :: Container T.Text -> InterpolableHieraString -> Maybe T.Text
resolveString vars = fmap T.concat . mapM resolve . getInterpolableHieraString
  where
    resolve (HPString x) = Just x
    resolve (HPVariable v) = vars ^? ix v

query :: HieraConfigFile -> FilePath -> Cache -> HieraQueryFunc IO
query HieraConfigFile {_backends, _hierarchy} fp cache vars hquery qt = do
    -- step 1, resolve hierarchies
    let searchin = do
            mhierarchy <- resolveString vars <$> _hierarchy
            Just hier  <- [mhierarchy]
            backend    <- _backends
            let decodeInfo :: (FilePath -> IO (S.Either String Value), String, String)
                decodeInfo
                    = case backend of
                        JsonBackend d -> (fmap (strictifyEither . A.eitherDecode') . BS.readFile       , d, ".json")
                        YamlBackend d -> (fmap (strictifyEither . (_Left %~ show)) . Y.decodeFileEither, d, ".yaml")
            return (decodeInfo, hier)
    -- step 2, read all the files, returning a raw data structure
    mvals <- forM searchin $ \((decodefunction, datadir, extension), hier) -> do
        let filename = basedir <> datadir <> "/" <> T.unpack hier <> extension
            basedir = case datadir of
                '/' : _ -> mempty
                _       -> fp ^. directory <> "/"
        efilecontent <- F.query cache filename (decodefunction filename)
        case efilecontent of
            S.Left r -> do
                let errs = "Hiera: error when reading file " <> string filename <+> string r
                if "Yaml file not found: " `L.isInfixOf` r
                    then LOG.debugM hieraLoggerName (show errs)
                    else LOG.warningM hieraLoggerName (show errs)
                return Nothing
            S.Right val -> return (Just val)
    let vals = catMaybes mvals
    -- step 3, query through all the results
    return (strictifyEither $ runReader (runExceptT (recursiveQuery hquery [])) (QRead vars qt vals))

type QM a = ExceptT PrettyError (Reader QRead) a

checkLoop :: T.Text -> [T.Text] -> QM ()
checkLoop x xs =
    when (x `elem` xs) (throwError ("Loop in hiera: " <> fromString (T.unpack (T.intercalate ", " (x:xs)))))

recursiveQuery :: T.Text -> [T.Text] -> QM (Maybe PValue)
recursiveQuery curquery prevqueries = do
  checkLoop curquery prevqueries
  rawlookups <- mapMaybe (preview (key curquery)) <$> view qhier
  lookups <- mapM (resolveValue (curquery : prevqueries)) rawlookups
  case lookups of
    [] -> return Nothing
    (x:xs) -> do
        qt <- view qtype
        let evalue = foldM (mergeWith qt) x xs
        case A.fromJSON <$> evalue of
            Left _ ->  return Nothing
            Right (A.Success o) -> return o
            Right (A.Error rr) -> throwError ("Something horrible happened in recursiveQuery: " <> fromString (show rr))

resolveValue :: [T.Text] -> Value -> QM Value
resolveValue prevqueries value =
    case value of
        String t  -> String <$> resolveText prevqueries t
        Array arr -> Array <$> mapM (resolveValue prevqueries) arr
        Object hh -> Object <$> mapM (resolveValue prevqueries) hh
        _         -> return value

resolveText :: [T.Text] -> T.Text -> QM T.Text
resolveText prevqueries t
    = case parseInterpolableString t of
        Right qparts -> T.concat <$> mapM (resolveStringPart prevqueries) qparts
        Left _ -> return t

resolveStringPart :: [T.Text] -> HieraStringPart -> QM T.Text
resolveStringPart prevqueries sp
    = case sp of
        HPString s -> return s
        HPVariable varname -> do
            let varsolve = fmap PString . preview (ix varname) <$> view qvars
            r <- case T.stripPrefix "lookup('" varname >>= T.stripSuffix "')" of
                    Just lk -> recursiveQuery lk prevqueries
                    Nothing -> varsolve
            case r of
                Just (PString v) -> return v
                _ -> return mempty

mergeWith :: HieraQueryType -> Value -> Value -> Either PrettyError Value
mergeWith qt cur new
  = case qt of
    QFirst -> return cur
    QUnique ->
        let getArray x = case x of
                Array array -> V.toList array
                _ -> [x]
            curarray = getArray cur
            newarray = getArray new
        in  case new of
                Object _ -> throwError "Tried to merge a hash"
                _ -> return (Array (V.fromList (L.nub (curarray ++ newarray))))
    QHash -> case (cur, new) of
        (Object curh, Object newh) -> return (Object (curh <> newh))
        _ -> throwError (PrettyError ("Tried to merge things that are not hashes: " <> text (show cur) <+> text (show new)))
    QDeep{} -> throwError "deep queries not supported"