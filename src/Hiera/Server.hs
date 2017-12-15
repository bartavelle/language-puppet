{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

{- | This module runs a Hiera server that caches Hiera data. There is
a huge caveat : only the data files are watched for changes, not the main configuration file.

A minor bug is that interpolation will not work for inputs containing the % character when it isn't used for interpolation.
-}
module Hiera.Server (
    startHiera
  , dummyHiera
    -- * Query API
  , HieraQueryFunc
) where

import           Puppet.Prelude

import           Control.Monad.Except
import           Data.Aeson               (FromJSON, Value (..), (.!=), (.:),
                                           (.:?))
import qualified Data.Aeson               as Aeson
import           Data.Aeson.Lens
import qualified Data.Attoparsec.Text     as AT
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Either.Strict       as S
import qualified Cache.File               as Cache
import qualified Data.List                as List
import           Data.String              (fromString)
import qualified Data.Text                as Text
import qualified Data.Vector              as Vector
import qualified Data.Yaml                as Yaml
import qualified System.FilePath          as FilePath
import           System.FilePath.Lens     (directory)

import           Puppet.Interpreter.Types
import           Puppet.PP


data HieraConfigFile = HieraConfigFile
    { _version   :: Int
    , _backends  :: [Backend]
    , _hierarchy :: [InterpolableHieraString]
    } deriving (Show)


data Backend
  = YamlBackend FilePath
  | JsonBackend FilePath
  deriving Show

newtype InterpolableHieraString
  = InterpolableHieraString
  { getInterpolableHieraString :: [HieraStringPart]
  } deriving Show

data HieraStringPart = HPString Text
                     | HPVariable Text
                     deriving Show

instance Pretty HieraStringPart where
    pretty (HPString t)   = ttext t
    pretty (HPVariable v) = dullred (string "%{" <> ttext v <> string "}")
    prettyList = mconcat . map pretty

type Cache = Cache.FileCache String Value

data QRead
    = QRead
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
                                             _      -> fail ("Unknown backend " ++ Text.unpack name)
           datadir <- case Object v ^? key skey . key ":datadir" of
                             Just (String dir)   -> return dir
                             Just _              -> fail ":datadir should be a string"
                             Nothing             -> return "/etc/puppet/hieradata"
           pure (backendConstructor (Text.unpack datadir))

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

-- | The only method you'll ever need. It runs a Hiera server and gives you a querying function.
-- | All IO exceptions are thrown directly including ParsingException.
startHiera :: FilePath -> IO (HieraQueryFunc IO)
startHiera fp =
  Yaml.decodeFileEither fp >>= \case
    Left (Yaml.AesonException "Error in $: Hiera configuration version different than 5 is not supported.") -> do
      logInfoStr ("Detect a hiera configuration format in " <> fp <> " at version 4. This format is not recognized. Using a dummy hiera.")
      pure dummyHiera
    Left ex   -> panic (show ex)
    Right cfg -> do
      logInfoStr ("Detect a hiera configuration format in " <> fp <> " at version " <> show(cfg^.version))
      cache <- Cache.newFileCache
      pure (query cfg fp cache)

-- | A dummy hiera function that will be used when hiera is not detected
dummyHiera :: Monad m => HieraQueryFunc m
dummyHiera _ _ _ = return $ S.Right Nothing

resolveString :: Container Text -> InterpolableHieraString -> Maybe Text
resolveString vars = fmap Text.concat . mapM resolve . getInterpolableHieraString
  where
    resolve (HPString x)   = Just x
    resolve (HPVariable v) = vars ^? ix v

query :: HieraConfigFile -> FilePath -> Cache -> HieraQueryFunc IO
query HieraConfigFile {_version, _backends, _hierarchy} fp cache vars hquery qt = do
  -- step 1, resolve hierarchies
  let searchin = do
          mhierarchy <- resolveString vars <$> _hierarchy
          Just h  <- [mhierarchy]
          backend    <- _backends
          let decodeInfo :: (FilePath -> IO (Either String Value), String, String)
              decodeInfo =
                case backend of
                  JsonBackend dir -> (fmap Aeson.eitherDecode' . BS.readFile       , dir, ".json")
                  YamlBackend dir -> (fmap (_Left %~ show) . Yaml.decodeFileEither   , dir, ".yaml")
          pure (decodeInfo, Text.unpack h)
  -- step 2, read all the files, returning a raw data structure
  mvals <- forM searchin $ \((decodefunction, datadir, extension), h) -> do
      let extension' = if snd (FilePath.splitExtension h) == ".yaml"
                       then ""
                       else extension
          filename = basedir <> datadir <> "/" <> h <> extension'
          basedir = case datadir of
              '/' : _ -> mempty
              _       -> fp ^. directory <> "/"
      efilecontent <- Cache.query cache filename (decodefunction filename)
      case efilecontent of
          Left r -> do
              let errs = "Hiera: error when reading file " <> string filename <+> string r
              if "Yaml file not found: " `List.isInfixOf` r
                  then logDebug (show errs)
                  else logWarning (show errs)
              return Nothing
          Right val -> return (Just val)
  let vals = catMaybes mvals
  -- step 3, query through all the results
  return (strictifyEither $ runReader (runExceptT (recursiveQuery hquery [])) (QRead vars qt vals))

type QM a = ExceptT PrettyError (Reader QRead) a

checkLoop :: Text -> [Text] -> QM ()
checkLoop x xs =
    when (x `elem` xs) (throwError ("Loop in hiera: " <> fromString (Text.unpack (Text.intercalate ", " (x:xs)))))

recursiveQuery :: Text -> [Text] -> QM (Maybe PValue)
recursiveQuery curquery prevqueries = do
  checkLoop curquery prevqueries
  rawlookups <- mapMaybe (preview (key curquery)) <$> view qhier
  lookups <- mapM (resolveValue (curquery : prevqueries)) rawlookups
  case lookups of
    [] -> return Nothing
    (x:xs) -> do
        qt <- view qtype
        let evalue = foldM (mergeWith qt) x xs
        case Aeson.fromJSON <$> evalue of
            Left _ ->  return Nothing
            Right (Aeson.Success o) -> return o
            Right (Aeson.Error rr) -> throwError ("Something horrible happened in recursiveQuery: " <> fromString (show rr))

resolveValue :: [Text] -> Value -> QM Value
resolveValue prevqueries value =
    case value of
        String t  -> String <$> resolveText prevqueries t
        Array arr -> Array <$> mapM (resolveValue prevqueries) arr
        Object hh -> Object <$> mapM (resolveValue prevqueries) hh
        _         -> return value

resolveText :: [Text] -> Text -> QM Text
resolveText prevqueries t
    = case parseInterpolableString t of
        Right qparts -> Text.concat <$> mapM (resolveStringPart prevqueries) qparts
        Left _ -> return t

resolveStringPart :: [Text] -> HieraStringPart -> QM Text
resolveStringPart prevqueries sp
    = case sp of
        HPString s -> return s
        HPVariable varname -> do
            let varsolve = fmap PString . preview (ix varname) <$> view qvars
            r <- case Text.stripPrefix "lookup('" varname >>= Text.stripSuffix "')" of
                    Just lk -> recursiveQuery lk prevqueries
                    Nothing -> varsolve
            case r of
                Just (PString v) -> return v
                _                -> return mempty

mergeWith :: HieraQueryType -> Value -> Value -> Either PrettyError Value
mergeWith qt cur new
  = case qt of
    QFirst -> return cur
    QUnique ->
        let getArray x = case x of
                Array array -> Vector.toList array
                _           -> [x]
            curarray = getArray cur
            newarray = getArray new
        in  case new of
                Object _ -> throwError "Tried to merge a hash"
                _ -> return (Array (Vector.fromList (List.nub (curarray ++ newarray))))
    QHash -> case (cur, new) of
        (Object curh, Object newh) -> return (Object (curh <> newh))
        _ -> throwError (PrettyError ("Tried to merge things that are not hashes: " <> text (show cur) <+> text (show new)))
    QDeep{} -> throwError "deep queries not supported"
