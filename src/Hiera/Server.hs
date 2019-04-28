{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE RecordWildCards        #-}
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
  , HieraQueryType (..)
  , readQueryType
  , HieraQueryFunc
) where

import           XPrelude

import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Lens
import qualified Data.Attoparsec.Text       as AT
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Either.Strict         as S
import qualified Data.FileCache             as Cache
import qualified Data.List                  as List
import           Data.String                (fromString)
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector
import qualified Data.Yaml                  as Yaml
import qualified System.Directory           as Directory
import qualified System.FilePath            as FilePath
import           System.FilePath.Lens       (directory)

import           Puppet.Language


-- | The different kind of hiera queries.
data HieraQueryType
    = QFirst   -- ^ The first match in the hierarchy is returned.
    | QUnique -- ^ Combines array and scalar values to return a merged, flattened array with all duplicate removed.
    | QHash  -- ^ Combines the keys and values of any number of hashes to return a merged hash.
    -- | Use of an Hash to specify the merge behavior
    | QDeep { _knockoutPrefix :: Maybe Text
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

-- | The type of the Hiera API function associated to given hierarchy.
type HieraQueryFunc m = Container PValue -- ^ Scope: all variables that Hiera can interpolate (the top level ones are prefixed with '::')
                     -> Text -- ^ The query
                     -> HieraQueryType
                     -> m (S.Either PrettyError (Maybe PValue))

data Backend
  = YamlBackend FilePath
  | JsonBackend FilePath
  deriving (Show)

data HieraStringPart
  = HPString Text
  | HPVariable Text
  deriving (Show)

instance Pretty HieraStringPart where
  pretty (HPString t)   = ppline t
  pretty (HPVariable v) = dullred (ppline ("%{" <> v <> "}"))
  prettyList = mconcat . map pretty

newtype InterpolableHieraString = InterpolableHieraString
  { getInterpolableHieraString :: [HieraStringPart]
  } deriving (Show)

resolveString :: Container PValue -> InterpolableHieraString -> Maybe Text
resolveString vars = fmap Text.concat . mapM resolve . getInterpolableHieraString
  where
    resolve (HPString x)   = Just x
    resolve (HPVariable v) = vars ^? ix v . _PString

instance FromJSON InterpolableHieraString where
  parseJSON (String s) = case parseInterpolableString s of
    Right x -> return (InterpolableHieraString x)
    Left rr -> fail rr
  parseJSON _ = fail "Invalid value type"

-- | An attoparsec parser that turns text into parts that are ready for interpolation.
interpolableString :: AT.Parser [HieraStringPart]
interpolableString = AT.many1 (fmap HPString rawPart <|> fmap HPVariable interpPart)
  where
    rawPart = AT.takeWhile1 (/= '%')
    interpPart = AT.string "%{" *> AT.takeWhile (/= '}') <* AT.char '}'

parseInterpolableString :: Text -> Either String [HieraStringPart]
parseInterpolableString = AT.parseOnly interpolableString

data HieraConfigFile = HieraConfigFile
  { _version   :: Int
  , _backends  :: [Backend]
  , _hierarchy :: [InterpolableHieraString]
  } deriving (Show)

data QRead = QRead
  { _qvars :: Container PValue
  , _qtype :: HieraQueryType
  , _qhier :: [Value]
  }

makeLenses ''QRead

instance FromJSON HieraConfigFile where
  parseJSON =
    let
      mkHiera5 :: Object -> Yaml.Parser HieraConfigFile
      mkHiera5 v = do
        -- we currently only read the first hierarchy entry to get the hiera path
        -- TODO: change the definition of HieraConfigFile to be [(Backend, InterpolableHieraString)]
        -- to allow defining a Backend per hierarchies
        let paths = Object v ^.. key "hierarchy" . values . key "paths" . values
            path = Object v ^.. key "hierarchy" .values .key "path"
        hierarchy_value <- case Object v ^? key "hierarchy" . nth 0 of
          Just (Object h) -> pure h
          _ -> fail "Hiera config should define at least one hierarchy"
        datadir <- hierarchy_value .:? "datadir" >>= \case
          Just (String dir) -> pure dir
          Just _            -> fail "datadir should be a string"
          Nothing           -> pure $ Object v ^. key "defaults" . key "datadir" . _String
        HieraConfigFile
            <$> pure 5
            <*> pure [ YamlBackend (toS datadir) ] -- TODO: support other backends if needed
            <*> mapM parseJSON (paths <> path)
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
                             Just (String dir) -> return dir
                             Just _            -> fail ":datadir should be a string"
                             Nothing           -> return "/etc/puppet/hieradata"
           pure (backendConstructor (toS datadir))

    in
    Aeson.withObject "v3 or v5" $ \o ->
      o .:? "version" >>= \case
        Just (5::Int) -> mkHiera5 o
        Just _ -> fail "Hiera configuration version different than 5 is not supported."
        Nothing -> mkHiera3 o

type Cache = Cache.FileCacheR String Value

-- | The only method you'll ever need. It runs a Hiera server and gives you a querying function.
-- | All IO exceptions are thrown directly including ParsingException.
startHiera :: String -> FilePath -> IO (HieraQueryFunc IO)
startHiera layer fp =
  Yaml.decodeFileEither fp >>= \case
    Left (Yaml.AesonException "Error in $: Hiera configuration version different than 5 is not supported.") -> do
      logInfoStr ("Detect a hiera configuration format in " <> fp <> " at version 4. This format is not recognized. Using a dummy hiera.")
      pure dummyHiera
    Left ex   -> panic (show ex)
    Right cfg@HieraConfigFile{..} -> do
      logInfoStr ("Detect a hiera " <> layer <> " configuration format in " <> fp <> " at version " <> show _version)
      query cfg fp <$> Cache.newFileCache

-- | A dummy hiera function that will be used when hiera is not detected.
dummyHiera :: Monad m => HieraQueryFunc m
dummyHiera _ _ _ = return $ S.Right Nothing


query :: HieraConfigFile -> FilePath -> Cache -> HieraQueryFunc IO
query HieraConfigFile {_version, _backends, _hierarchy} fp cache vars hquery qt = do
  -- step 1, resolve hierarchies
  let searchin = do
        mhierarchy <- resolveString vars <$> _hierarchy
        Just h  <- [mhierarchy]
        backend    <- _backends
        let decodeInfo :: (FilePath -> IO (S.Either String Value), String, String)
            decodeInfo =
              case backend of
                JsonBackend dir -> (fmap (strictifyEither . Aeson.eitherDecode') . BS.readFile       , dir, ".json")
                YamlBackend dir -> (fmap (strictifyEither . (_Left %~ show)) . Yaml.decodeFileEither, dir, ".yaml")
        pure (decodeInfo, toS h)
  -- step 2, read all the files, returning a raw data structure
  mvals <- forM searchin $ \((decodefunction, datadir, extension), h) -> do
    let extension' = if snd (FilePath.splitExtension h) == ".yaml"
                       then ""
                       else extension
        filename = basedir <> datadir <> "/" <> h <> extension'
        basedir = case datadir of
          '/' : _ -> mempty
          _       -> fp ^. directory <> "/"
        querycache = do
          efilecontent <- Cache.query cache filename (decodefunction filename)
          case efilecontent of
            S.Left r -> do
              logWarningStr $ "Hiera: error when reading file " <> filename <> ": "<> r
              pure Nothing
            S.Right val -> pure (Just val)
    ifM (Directory.doesFileExist filename)
      querycache
      (pure Nothing)
  let vals = catMaybes mvals
  -- step 3, query through all the results
  logDebugStr ("Looking up '" <> toS hquery <> "' with backends " <> List.unwords (fmap show _backends ))
  return (strictifyEither $ runReader (runExceptT (recursiveQuery hquery [])) (QRead vars qt vals))

type QM a = ExceptT PrettyError (Reader QRead) a

checkLoop :: Text -> [Text] -> QM ()
checkLoop x xs =
    when (x `elem` xs) (throwError ("Loop in hiera: " <> fromString (Text.unpack (Text.intercalate ", " (x:xs)))))

recursiveQuery :: Text -> [Text] -> QM (Maybe PValue)
recursiveQuery curquery prevqueries = do
  let (varname:allkeys) = Text.split (== '.') curquery -- can't fail, split always returns a nonempty list
  checkLoop varname prevqueries
  rawlookups <- mapMaybe (preview (key varname)) <$> view qhier
  let lookupKeys keys v =
        case keys of
          [] -> pure v
          k:ks ->
            case v of
              Object hs ->
                case hs ^? ix k of
                  Nothing -> Nothing
                  Just v' -> lookupKeys ks v'
              _ -> Nothing
  rlookups <- mapM (resolveValue (varname : prevqueries)) rawlookups
  let lookups = mapMaybe (lookupKeys allkeys) rlookups
  case lookups of
    [] -> if null rlookups
            then return Nothing
            else throwError ("Could not lookup " <> fromString (Text.unpack curquery) <> " in " <> PrettyError (list (map (fromString . BS8.unpack . encode) rlookups)) )
    (x:xs) -> do
      qt <- view qtype
      let evalue = foldM (mergeWith qt) x xs
      case Aeson.fromJSON <$> evalue of
        Left _ ->  return Nothing
        Right (Aeson.Success o) -> return o
        Right (Aeson.Error rr) -> throwError ("Something horrible happened in recursiveQuery: " <> fromString rr)

resolveValue :: [Text] -> Value -> QM Value
resolveValue prevqueries value =
  case value of
    String t | Just alias <- Text.stripPrefix "%{alias('" t >>= Text.stripSuffix "')}" -> do
          mr <- recursiveQuery alias (("alias:" <> alias) : prevqueries)
          case mr of
            Nothing -> throwError ("Could not alias " <> fromString (Text.unpack alias))
            Just r -> pure (toJSON r)
    String t  -> String <$> resolveText prevqueries t
    Array arr -> Array <$> mapM (resolveValue prevqueries) arr
    Object hh -> Object <$> mapM (resolveValue prevqueries) hh
    _         -> return value

resolveText :: [Text] -> Text -> QM Text
resolveText prevqueries t =
  case parseInterpolableString t of
    Right qparts -> Text.concat <$> mapM (resolveStringPart prevqueries) qparts
    Left _       -> return t

resolveStringPart :: [Text] -> HieraStringPart -> QM Text
resolveStringPart prevqueries sp =
  case sp of
    HPString s -> return s
    HPVariable "" -> return ""
    HPVariable varname -> do
      let varsolve = preview (ix varname) <$> view qvars
          extractFunction txt = (Text.stripPrefix (txt <> "('") varname >>= Text.stripSuffix "')")
                            <|> (Text.stripPrefix (txt <> "(\"") varname >>= Text.stripSuffix "\")")
      r <- case extractFunction "lookup" <|> extractFunction "alias" of
        Just lk -> recursiveQuery lk prevqueries
        Nothing -> varsolve
      case r of
        Just (PString v) -> return v
        Just (PNumber s) -> pure (scientific2text s)
        Just pvalue      -> throwError (PrettyError ("Variable lookup for " <> fromString (Text.unpack varname) <> " did not return a string, but " <> pretty pvalue))
        _                -> throwError ("Could not lookup variable " <> fromString (Text.unpack varname))

mergeWith :: HieraQueryType -> Value -> Value -> Either PrettyError Value
mergeWith qt cur new =
  case qt of
    QFirst -> return cur
    QUnique ->
      let getArray x = case x of
              Array array -> Vector.toList array
              _           -> [x]
          curarray = getArray cur
          newarray = getArray new
      in  case new of
              Object _ -> throwError "Tried to merge a hash"
              _        -> return (Array (Vector.fromList (List.nub (curarray ++ newarray))))
    QHash -> case (cur, new) of
      (Object curh, Object newh) -> return (Object (curh <> newh))
      _ -> throwError (PrettyError ("Tried to merge things that are not hashes: " <> ppline (show cur) <+> ppline (show new)))
    QDeep{} -> throwError "deep queries not supported"
