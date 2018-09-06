{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE CPP               #-}
module Main where

import           XPrelude                      hiding (option, (<>))

import           Control.Concurrent.ParallelIO (parallel)
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as HS
import qualified Data.List                     as List
import           Data.Semigroup                as Sem   hiding (option)
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Text.Strict.Lens         (unpacked)
import qualified Data.Vector                   as V
import qualified Data.Version                  (showVersion)
import qualified Network.HTTP.Client           as Http
import           Options.Applicative
import qualified Paths_language_puppet         as Meta
import qualified System.FilePath.Glob          as Glob
import           System.IO                     (hIsTerminalDevice)
import qualified System.Log.Logger             as Log
import qualified Text.Regex.PCRE.String        as Reg

import           Puppet.Parser                 hiding (Parser)
import           Puppet.Runner
import           PuppetDB                      (PuppetDBAPI, dummyPuppetDB, pdbConnect,
                                                puppetDBFacts)
import qualified PuppetDB

type QueryFunc = NodeName -> IO (Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))

data MultNodes
  = MultNodes [Text]
  | AllNodes
  deriving (Show)

readMultNodes:: String -> Maybe MultNodes
readMultNodes "allnodes" = Just AllNodes
readMultNodes n          =  Just (MultNodes (Text.splitOn "," (toS n)))

data Options = Options
  { _optShowjson     :: Bool
  , _optShowContent  :: Bool
  , _optResourceType :: Maybe Text
  , _optResourceName :: Maybe Text
  , _optPuppetdir    :: Maybe FilePath
  , _optNodename     :: Maybe NodeName
  , _optMultnodes    :: Maybe MultNodes
  , _optDeadcode     :: Bool
  , _optPdburl       :: Maybe String
  , _optPdbfile      :: Maybe FilePath
  , _optLoglevel     :: Log.Priority
  , _optHieraFile    :: Maybe FilePath
  , _optCommitDB     :: Bool
  , _optCheckExport  :: Bool
  , _optIgnoredMods  :: Maybe (HS.HashSet Text)
  , _optParse        :: Maybe FilePath
  , _optStrictMode   :: Bool
  , _optNoExtraTests :: Bool
  , _optVersion      :: Bool
  , _optRebaseFile   :: Maybe FilePath
  } deriving (Show)

options :: Parser Options
options = Options
   <$> switch
       (  long "JSON"
       <> short 'j'
       <> help "Shows the output as a JSON document (useful for full catalog views)")
   <*> switch
       (  long "showcontent"
       <> short 'c'
       <> help "When specifying a file resource, only output its content (useful for testing templates)")
   <*> optional (Text.pack <$> strOption
       (  long "type"
       <> short 't'
       <> help "Filter the output by resource type (accepts a regular expression, ie '^file$')"))
   <*> optional (Text.pack <$> strOption
       (  long "name"
       <> short 'n'
       <> help "Filter the output by resource name (accepts a regular expression)"))
   <*> optional (strOption
       (  long "puppetdir"
       <> short 'p'
       <> help "Puppet directory"))
   <*> optional (Text.pack <$> strOption
            (  long "node"
            <> short 'o'
            <> help "The name of the node"))
   <*> optional (option (maybeReader readMultNodes)
       (  long "all"
       <> short 'a'
       <> help "Values are a list of nodes or \"allnodes\""))
   <*> switch
       (  long "deadcode"
       <> help "Show deadcode when the --all option is used")
   <*> optional (strOption
       (  long "pdburl"
       <> help "URL of the puppetdb (ie. http://localhost:8080/)."))
   <*> optional (strOption
       (  long "pdbfile"
       <> help "Path to the testing PuppetDB file."))
   <*> option auto
       (  long "loglevel"
       <> short 'v'
       <> help "Values are : DEBUG, INFO, NOTICE, WARNING, ERROR"
       <> value Log.WARNING)
   <*> optional (strOption
       (  long "hiera"
       <> help "Path to the Hiera configuration file (default hiera.yaml)"))
   <*> switch
       (  long "commitdb"
       <> help "Commit the computed catalogs in the puppetDB")
   <*> switch
       (  long "checkExported"
       <> help "Save exported resources in the puppetDB")
   <*> optional (HS.fromList . Text.splitOn "," . Text.pack <$>
       strOption
       (  long "ignoredmodules"
       <> help "Specify a comma-separated list of modules to ignore"))
   <*> optional (strOption
       (  long "parse"
       <> help "Parse a single file"))
   <*> switch
       (  long "strict"
       <> help "Strict mode diverges from vanillia Puppet and enforces good practices")
   <*> flag False True
       (  long "noextratests"
       <> help "Disable extra tests (eg.: check that files exist on local disk")
   <*> switch
       (  long "version"
       <> help "Output version information and exit")
   <*> optional (strOption
       (  long "rebasefile"
       <> help "Rebase all calls to the 'file' function so that absolute path are relative to the Puppet directory"))


-- | Like catMaybes, but it counts the Nothing values
catMaybesCount :: [Maybe a] -> ([a], Sum Int)
catMaybesCount = foldMap f
  where
    f Nothing  = ([ ], Sum 1)
    f (Just x) = ([x], Sum 0)

{-| Does all the work of initializing a daemon for querying.
Returns a 'QueryFunc' API, together with some stats.
Be aware it uses the locale computer to generate a set of `facts` (this is a bit hackish).
These facts can be overriden at the command line (see 'Options').
-}
initializedaemonWithPuppet :: FilePath
                           -> Options
                           -> IO (QueryFunc, PuppetDBAPI IO, MStats, MStats, MStats)
initializedaemonWithPuppet workingdir Options {..} = do
    mgr <- Http.newManager Http.defaultManagerSettings
    pdbapi <- case (_optPdburl, _optPdbfile) of
                (Nothing, Nothing) -> return dummyPuppetDB
                (Just _, Just _)   -> panic "You must choose between a testing PuppetDB and a remote one"
                (Just url, _)      -> pdbConnect mgr url >>= unwrapError "Error when connecting to the remote PuppetDB"
                (_, Just file)     -> PuppetDB.loadTestDB file >>= unwrapError "Error when initializing the PuppetDB API"
    pref <- dfPreferences workingdir <&> prefPDB .~ pdbapi
                                     <&> prefHieraPath .~ _optHieraFile
                                     <&> prefIgnoredmodules %~ (`fromMaybe` _optIgnoredMods)
                                     <&> (if _optStrictMode then prefStrictness .~ Strict else identity)
                                     <&> (if _optNoExtraTests then prefExtraTests .~ False else identity)
                                     <&> prefLogLevel .~ _optLoglevel
                                     <&> prefRebaseFile .~ _optRebaseFile
    d <- initDaemon pref
    let queryfunc = \node -> fmap (unifyFacts (pref ^. prefFactsDefault) (pref ^. prefFactsOverride)) (puppetDBFacts node pdbapi) >>= getCatalog d node
    pure (queryfunc, pdbapi, parserStats d, catalogStats d, templateStats d)
    where
      -- merge 3 sets of facts : some defaults, the original set and some override
      unifyFacts :: Container PValue -> Container PValue -> Container PValue -> Container PValue
      unifyFacts defaults override c = override `Map.union` c `Map.union` defaults

parseFile :: FilePath -> IO (V.Vector Statement)
parseFile fp = do
  s <- readFile fp
  case runPuppetParser fp s of
    Left err -> putDoc (red "ERROR:" <+> getError (prettyParseError err)) *> exitFailure
    Right r -> pure r

printContent :: Text -> FinalCatalog -> IO ()
printContent filename catalog =
  case Map.lookup (RIdentifier "file" filename) catalog of
    Nothing -> panic "File not found"
    Just r ->
      case Map.lookup "content" (r ^. rattributes) of
        Nothing          -> panic "This file has no content"
        Just (PString c) -> putText c
        Just x           -> print x

prepareForPuppetApply :: WireCatalog -> WireCatalog
prepareForPuppetApply wcat =
  let res = V.filter (\r -> r ^. rvirtuality == Normal) (wcat ^. wireCatalogResources) :: V.Vector Resource
      -- step 1 : capitalize resources types (and names in case of
      -- classes), and filter out exported stuff
      capi :: RIdentifier -> RIdentifier
      capi r = r & itype %~ capitalizeRT & if r ^. itype == "class"
                                             then iname %~ capitalizeRT
                                             else identity
      aliasMap :: HashMap RIdentifier Text
      aliasMap = Map.fromList $ concatMap genAliasList (res ^.. folded)
      genAliasList r = map (\n -> (RIdentifier (r ^. rid . itype) n, r ^. rid . iname)) (r ^. rid . iname : r ^.. ralias . folded)
      nr = V.map (rid %~ capi) res
      ne = V.map capEdge (wcat ^. wireCatalogEdges)
      capEdge (PuppetEdge a b x) = PuppetEdge (capi a) (capi b) x
      -- step 2 : replace all references with the resource title in case
      -- of aliases - yes this sucks
      knownEdge :: PuppetEdge -> Bool
      knownEdge (PuppetEdge s d _) = (aliasMap & has (ix s)) && (aliasMap & has (ix d))
      correctEdges = V.map correctEdge $ V.filter knownEdge ne
      correctResources = V.map correctResource nr
      correct :: RIdentifier -> RIdentifier
      correct ri = ri & iname %~ \n -> Map.lookupDefault n ri aliasMap
      correctEdge :: PuppetEdge -> PuppetEdge
      correctEdge (PuppetEdge s d x) = PuppetEdge (correct s) (correct d) x
      correctResource :: Resource -> Resource
      correctResource r = r & rrelations %~ Map.fromList . filter (\(x,_) -> aliasMap & has (ix x)) . map (_1 %~ correct) . Map.toList
  in wcat & (wireCatalogResources .~ correctResources)
          & (wireCatalogEdges     .~ correctEdges)


findDeadCode :: String -> [Resource] -> Set.Set FilePath -> IO ()
findDeadCode puppetdir catalogs allfiles = do
    -- first collect all files / positions from all the catalogs
    let allpositions = Set.fromList $ catalogs ^.. traverse . rpos
    -- now find all haskell files
    puppetfiles <- Set.fromList <$> Glob.globDir1 (Glob.compile "**/*.pp") (puppetdir <> "/modules")
    let deadfiles = Set.filter ("/manifests/" `List.isInfixOf`) $ puppetfiles `Set.difference`   allfiles
        usedfiles = puppetfiles `Set.intersection` allfiles
    unless (Set.null deadfiles) $ do
        putDoc ("The following files" <+> pretty (Set.size deadfiles) <+> "are not used: " <> list (map ppstring $ Set.toList deadfiles))
        putText ""
    allparses <- do
     let parsefp fp = runPuppetParser fp <$> readFile fp
     parallel (map parsefp (Set.toList usedfiles))
    let (parseFailed, parseSucceeded) = partitionEithers allparses
    unless (null parseFailed) $ do
        putDoc ("The following" <+> pretty (length parseFailed) <+> "files could not be parsed:" <> softline <> indent 4 (vcat (map (ppstring . show) parseFailed)))
        putText ""
    let getSubStatements s@ResourceDeclaration{} = [s]
        getSubStatements (ConditionalDeclaration (ConditionalDecl conds _)) = conds ^.. traverse . _2 . tgt
        getSubStatements s@ClassDeclaration{} = extractPrism s
        getSubStatements s@DefineDeclaration{} = extractPrism s
        getSubStatements s@NodeDeclaration{} = extractPrism s
        getSubStatements s@HigherOrderLambdaDeclaration{} = extractPrism s
        getSubStatements (TopContainer v s) = getSubStatements s ++ v ^.. tgt
        getSubStatements _ = []
        tgt = folded . to getSubStatements . folded
        extractPrism = toListOf (_Statements . traverse . to getSubStatements . traverse)
        allResources = parseSucceeded ^.. folded . folded . to getSubStatements . folded
        deadResources = filter isDead allResources
        isDead (ResourceDeclaration (ResDecl _ _ _ _ pp)) = not $ Set.member pp allpositions
        isDead _                                          = True
    unless (null deadResources) $
        putDoc ("The following" <+> pretty (length deadResources) <+> "resource declarations are not used:" <> softline <> indent 4 (vcat (map pretty deadResources)) <> line )

newtype Maximum a = Maximum { getMaximum :: Maybe a }

instance (Ord a) => Semigroup (Maximum a) where
  Maximum Nothing <> m2                  = m2
  m1 <> Maximum Nothing                  = m1
  Maximum (Just a1) <> Maximum (Just a2) = Maximum (Just (max a1 a2))

instance (Ord a) => Monoid (Maximum a) where
  mempty = Maximum Nothing
#if !(MIN_VERSION_base(4,11,0))
  mappend = (Sem.<>)
#endif

-- For each node, queryfunc the catalog and return stats
computeStats :: FilePath -> Options -> QueryFunc -> (MStats, MStats, MStats) -> [NodeName] -> IO ()
computeStats workingdir Options {..}
             queryfunc (parsingStats, catalogStats, templateStats)
             topnodes = do
  -- the parsing statistics, so that we known which files
  (cats, Sum failures) <- catMaybesCount <$> parallel (map (compute queryfunc) topnodes)
  pStats <- getStats parsingStats
  cStats <- getStats catalogStats
  tStats <- getStats templateStats
  let allres = (cats ^.. folded . _1 . folded) ++ (cats ^.. folded . _2 . folded)
      allfiles = Set.fromList $ map Text.unpack $ Map.keys pStats
  when _optDeadcode $ findDeadCode workingdir allres allfiles
  -- compute statistics
  let (parsing,    Just (wPName, wPMean)) = worstAndSum pStats
      (cataloging, Just (wCName, wCMean)) = worstAndSum cStats
      (templating, Just (wTName, wTMean)) = worstAndSum tStats
      parserShare = 100 * parsing / cataloging
      templateShare = 100 * templating / cataloging
      formatDouble = Text.take 5 . show -- yeah, well ...
      nbnodes = length topnodes
      worstAndSum =   (_1 %~ getSum)
                    . (_2 %~ fmap swap . getMaximum)
                    . ifoldMap (\k (StatsPoint cnt total _ _) -> (Sum total, Maximum $ Just (total / fromIntegral cnt, k)))
  putStr ("\nTested " ++ show nbnodes ++ " nodes. ")
  unless (nbnodes == 0) $ do
    putText (formatDouble parserShare <> "% of total CPU time spent parsing, " <> formatDouble templateShare <> "% spent computing templates")
    when (_optLoglevel <= Log.NOTICE) $ do
      putText ("Slowest template:           " <> wTName <> ", taking " <> formatDouble wTMean <> "s on average")
      putText ("Slowest file to parse:      " <> wPName <> ", taking " <> formatDouble wPMean <> "s on average")
      putText ("Slowest catalog to compute: " <> wCName <> ", taking " <> formatDouble wCMean <> "s on average")

  if failures > 0
     then do {putDoc ("Found" <+> red (pretty failures) <+> "failure(s)." <> line) ; exitFailure}
     else do {putDoc (dullgreen "All green."  <> line) ; exitSuccess}

  where
    compute :: QueryFunc -> NodeName -> IO (Maybe (FinalCatalog, [Resource]))
    compute func node =
      func node >>= \case
        Left err -> putDoc (line <> red "ERROR:" <+> parens (ppline node) <+> ":" <+> getError err) >> return Nothing
        Right (rawcatalog, _ , rawexported, knownRes) -> return (Just (rawcatalog <> rawexported, knownRes))

-- Queryfunc the catalog for the node and PP the result
computeNodeCatalog :: Options -> QueryFunc -> PuppetDBAPI IO -> NodeName -> IO ()
computeNodeCatalog Options {..} queryfunc pdbapi node =
  queryfunc node >>= \case
    Left rr -> do
      putDoc (line <> red "ERROR:" <+> parens (ppline node) <+> getError rr)
      exitFailure
    Right (rawcatalog, edgemap, rawexported, _) -> do
      printFunc <- hIsTerminalDevice stdout >>= \isterm -> return $ \x ->
        if isterm
          then putDoc (x <> line)
          else displayIO stdout (renderCompact (x <> line))
      catalog  <- filterCatalog _optResourceType _optResourceName rawcatalog
      exported <- filterCatalog _optResourceType _optResourceName rawexported
      let wirecatalog    = PuppetDB.generateWireCatalog node (catalog    <> exported   ) edgemap
          rawWireCatalog = PuppetDB.generateWireCatalog node (rawcatalog <> rawexported) edgemap
      when _optCheckExport $ void $ runExceptT $ PuppetDB.replaceCatalog pdbapi rawWireCatalog
      case (_optShowContent, _optShowjson) of
        (_, True) -> putLByteString (Aeson.encode (prepareForPuppetApply wirecatalog))
        (True, _) -> do
          unless (_optResourceType == Just "file" || isNothing _optResourceType) $ do
              putDoc "Show content only works with resource of type file. It is an error to provide another filter type"
              exitFailure
          case _optResourceName of
            Just f  -> printContent f catalog
            Nothing -> die "You should supply a resource name when using showcontent"
        _       -> do
            printFunc (pretty (Map.elems catalog))
            unless (Map.null exported) $ do
              printFunc (mempty <+> dullyellow "Exported:" <+> mempty)
              printFunc (pretty (Map.elems exported))


-- Filter according to the type and name regex from the command line option
filterCatalog :: Maybe Text -> Maybe Text -> FinalCatalog -> IO FinalCatalog
filterCatalog typeFilter nameFilter =
  filterC typeFilter (_1 . itype . unpacked) >=> filterC nameFilter (_1 . iname . unpacked)
  where
    -- filter catalog using the adhoc lens
    filterC Nothing _ c = return c
    filterC (Just regexp) l c = Reg.compile Reg.compBlank Reg.execBlank (Text.unpack regexp) >>= \case
      Left rr   -> panic ("Error compiling regexp 're': "  <> show rr)
      Right reg -> Map.fromList <$> filterM (filterResource reg l) (Map.toList c)
    filterResource reg l v = Reg.execute reg (v ^. l) >>= \case
      Left rr -> panic ("Error when applying regexp: " <> show rr)
      Right Nothing -> return False
      _ -> return True


run :: Options -> IO ()
run Options {_optVersion = True, ..} = putStrLn ("language-puppet " ++ Data.Version.showVersion Meta.version)

-- Parse mode
run Options {_optParse = Just fp, ..} = do
  sx <- parseFile fp
  if _optLoglevel == Log.DEBUG
    then mapM_ print  sx
    else putDoc $ ppStatements sx

run Options {_optPuppetdir = Nothing, _optParse = Nothing } =
    panic "Without a puppet dir, only the `--parse` option can be supported"
run Options {_optPuppetdir = Just _, _optNodename = Nothing, _optMultnodes = Nothing} =
    panic "You need to choose between single or multiple node"

-- Single node mode (`--node` option)
run cmd@Options {_optNodename = Just node, _optPuppetdir = Just workingdir, ..} = do
  (queryfunc, pdbapi, _, _, _ ) <- initializedaemonWithPuppet workingdir cmd
  computeNodeCatalog cmd queryfunc pdbapi node
  when _optCommitDB $ void $ runExceptT $ PuppetDB.commitDB pdbapi

-- Multiple nodes mode (`--all`) option
run cmd@Options {_optNodename = Nothing, _optMultnodes = Just nodes, _optPuppetdir = Just workingdir} = do
  (queryfunc, _, mPStats, mCStats, mTStats) <- initializedaemonWithPuppet workingdir cmd
  computeStats workingdir cmd queryfunc (mPStats, mCStats, mTStats) =<< retrieveNodes nodes
  where
    retrieveNodes :: MultNodes -> IO [NodeName]
    retrieveNodes AllNodes = do
      allstmts <- parseFile (workingdir <> "/manifests/site.pp")
      let getNodeName (NodeDeclaration (NodeDecl (NodeName n) _ _ _)) = Just n
          getNodeName _                                               = Nothing
      return $ mapMaybe getNodeName (V.toList allstmts)
    retrieveNodes (MultNodes xs) = pure xs

main :: IO ()
main = execParser opts >>= run
  where
    opts :: ParserInfo Options
    opts = info (helper <*> options)
             (fullDesc
              <> progDesc "A program for parsing puppet files, generating and inspecting catalogs"
              <> header "puppetresources - a development tool for Puppet"
              <> failureCode 3)
