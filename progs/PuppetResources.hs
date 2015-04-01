{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent.ParallelIO    (parallel)
import           Control.Lens
import           Control.Monad
import           Data.Aeson                       (encode)
import qualified Data.ByteString.Lazy.Char8       as BSL
import           Data.Either                      (partitionEithers)
import qualified Data.Either.Strict               as S
import           Data.Foldable                    (foldMap)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.List                        (isInfixOf)
import           Data.Maybe                       (isNothing, mapMaybe)
import           Data.Monoid                      hiding (First)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Text.Strict.Lens
import           Data.Traversable                 (for)
import           Data.Tuple                       (swap)
import qualified Data.Vector                      as V
import           Options.Applicative
import           System.Exit                      (exitFailure, exitSuccess)
import qualified System.FilePath.Glob             as G
import           System.IO
import           System.Log.Formatter             (simpleLogFormatter)
import           System.Log.Handler               (setFormatter)
import           System.Log.Handler.Simple        (streamHandler)
import qualified System.Log.Logger                as LOG
import qualified Text.Parsec                      as P
import           Text.Regex.PCRE.String

import           Facter
import           Puppet.Daemon
import           Puppet.Interpreter.PrettyPrinter ()
import           Puppet.Interpreter.Types
import           Puppet.Lens
import           Puppet.Parser
import           Puppet.Parser.PrettyPrinter      (ppStatements)
import           Puppet.Parser.Types
import           Puppet.PP
import           Puppet.Preferences
import           Puppet.Stats
import           Puppet.Utils
import           PuppetDB.Common
import           PuppetDB.Dummy
import           PuppetDB.Remote
import           PuppetDB.TestDB

type QueryFunc = Nodename -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))

data MultNodes =  MultNodes [T.Text] | AllNodes deriving Show

instance Read MultNodes where
    readsPrec _ "allnodes" = [(AllNodes, "")]
    readsPrec _ s = let os = (T.splitOn "," . T.pack) s
                    in [(MultNodes os, "")]

data Options = Options
    { _optShowjson     :: Bool
    , _optShowContent  :: Bool
    , _optResourceType :: Maybe T.Text
    , _optResourceName :: Maybe T.Text
    , _optPuppetdir    :: Maybe FilePath
    , _optNodename     :: Maybe Nodename
    , _optMultnodes    :: Maybe MultNodes
    , _optDeadcode     :: Bool
    , _optPdburl       :: Maybe String
    , _optPdbfile      :: Maybe FilePath
    , _optLoglevel     :: LOG.Priority
    , _optHieraFile    :: Maybe FilePath
    , _optFactsOverr   :: Maybe FilePath
    , _optFactsDefault :: Maybe FilePath
    , _optCommitDB     :: Bool
    , _optCheckExport  :: Bool
    , _optIgnoredMods  :: HS.HashSet T.Text
    , _optParse        :: Maybe FilePath
    , _optStrictMode   :: Strictness
    , _optNoExtraTests :: Bool
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
   <*> optional (T.pack <$> strOption
       (  long "type"
       <> short 't'
       <> help "Filter the output by resource type (accepts a regular expression, ie '^file$')"))
   <*> optional (T.pack <$> strOption
       (  long "name"
       <> short 'n'
       <> help "Filter the output by resource name (accepts a regular expression)"))
   <*> optional (strOption
       (  long "puppetdir"
       <> short 'p'
       <> help "Puppet directory"))
   <*> optional (T.pack <$> strOption
            (  long "node"
            <> short 'o'
            <> help "The name of the node"))
   <*> optional (option auto
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
       <> help "Values are : DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"
       <> value LOG.WARNING)
   <*> optional (strOption
       (  long "hiera"
       <> help "Path to the Hiera configuration file (default hiera.yaml)"
       <> value "hiera.yaml"))
   <*> optional (strOption
       (  long "facts-override"
       <> help "Path to a Yaml file containing a list of 'facts' that will override locally resolved facts"))
   <*> optional (strOption
       (  long "facts-defaults"
       <> help "Path to a Yaml file containing a list of 'facts' that will be used as defaults"))
   <*> switch
       (  long "commitdb"
       <> help "Commit the computed catalogs in the puppetDB")
   <*> switch
       (  long "checkExported"
       <> help "Save exported resources in the puppetDB")
   <*> (HS.fromList . T.splitOn "," . T.pack <$>
       strOption
       (  long "ignoremodules"
       <> help "Specify a comma-separated list of modules to ignore"
       <> value ""))
   <*> optional (strOption
       (  long "parse"
       <> help "Parse a single file"))
   <*> flag Permissive Strict
       (  long "strict"
       <> help "Strict mode diverges from vanillia Puppet and enforces good practices")
   <*> flag False True
       (  long "noextratests"
       <> help "Disable extra tests (eg.: check that files exist on local disk")

checkError :: Doc -> S.Either PrettyError a -> IO a
checkError r (S.Left rr) = error (show (red r <> ": " <+> getError rr))
checkError _ (S.Right x) = return x

-- | Like catMaybes, but it counts the Nothing values
catMaybesCount :: [Maybe a] -> ([a], Sum Int)
catMaybesCount = foldMap f where
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
initializedaemonWithPuppet workingdir (Options {..}) = do
    setupLogger
    pdbapi <- case (_optPdburl, _optPdbfile) of
                  (Nothing, Nothing) -> return dummyPuppetDB
                  (Just _, Just _)   -> error "You must choose between a testing PuppetDB and a remote one"
                  (Just url, _)      -> pdbConnect (T.pack url) >>= checkError "Error when connecting to the remote PuppetDB"
                  (_, Just file)     -> loadTestDB file >>= checkError "Error when initializing the PuppetDB API"
    !factsOverrides <- case (_optFactsOverr, _optFactsDefault) of
                           (Just _, Just _) -> error "You can't use --facts-override and --facts-defaults at the same time"
                           (Just p, Nothing) -> HM.union `fmap` loadYamlFile p
                           (Nothing, Just p) -> flip HM.union `fmap` loadYamlFile p
                           (Nothing, Nothing) -> return id
    q <- initDaemon =<< setupPreferences
         workingdir ((prefPDB.~ pdbapi) . (hieraPath.~ _optHieraFile) . (ignoredmodules.~ _optIgnoredMods) . (strictness.~ _optStrictMode) . (extraTests.~ not _optNoExtraTests))
    let queryfunc = \node -> fmap factsOverrides (puppetDBFacts node pdbapi) >>= _dGetCatalog q node
    return (queryfunc, pdbapi, _dParserStats q, _dCatalogStats q, _dTemplateStats q)
    where
      stdoutHandler p = setFormatter
                        <$> streamHandler stdout p
                        <*> pure (simpleLogFormatter "$prio: $msg")
      setupLogger = do
         hs <- for [LOG.DEBUG, LOG.INFO, LOG.NOTICE, LOG.WARNING] stdoutHandler
         LOG.updateGlobalLogger LOG.rootLoggerName $ LOG.setHandlers hs
         LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel _optLoglevel)
         LOG.updateGlobalLogger "Hiera.Server" (LOG.setLevel _optLoglevel)


parseFile :: FilePath -> IO (Either P.ParseError (V.Vector Statement))
parseFile = fmap . runPParser puppetParser <*> T.readFile

printContent :: T.Text -> FinalCatalog -> IO ()
printContent filename catalog =
        case HM.lookup (RIdentifier "file" filename) catalog of
            Nothing -> error "File not found"
            Just r  -> case HM.lookup "content" (r ^. rattributes) of
                           Nothing -> error "This file has no content"
                           Just (PString c)  -> T.putStrLn c
                           Just x -> print x

prepareForPuppetApply :: WireCatalog -> WireCatalog
prepareForPuppetApply w =
    let res = V.filter (\r -> r ^. rvirtuality == Normal) (w ^. wResources) :: V.Vector Resource
        -- step 1 : capitalize resources types (and names in case of
        -- classes), and filter out exported stuff
        capi :: RIdentifier -> RIdentifier
        capi r = r & itype %~ capitalizeRT & if r ^. itype == "class"
                                                then iname %~ capitalizeRT
                                                else id
        aliasMap :: HM.HashMap RIdentifier T.Text
        aliasMap = HM.fromList $ concatMap genAliasList (res ^.. folded)
        genAliasList r = map (\n -> (RIdentifier (r ^. rid . itype) n, r ^. rid . iname)) (r ^. rid . iname : r ^.. ralias . folded)
        nr = V.map (rid %~ capi) res
        ne = V.map capEdge (w ^. wEdges)
        capEdge (PuppetEdge a b x) = PuppetEdge (capi a) (capi b) x
        -- step 2 : replace all references with the resource title in case
        -- of aliases - yes this sucks
        knownEdge :: PuppetEdge -> Bool
        knownEdge (PuppetEdge s d _) = (aliasMap & has (ix s)) && (aliasMap & has (ix d))
        correctEdges = V.map correctEdge $ V.filter knownEdge ne
        correctResources = V.map correctResource nr
        correct :: RIdentifier -> RIdentifier
        correct ri = ri & iname %~ \n -> HM.lookupDefault n ri aliasMap
        correctEdge :: PuppetEdge -> PuppetEdge
        correctEdge (PuppetEdge s d x) = PuppetEdge (correct s) (correct d) x
        correctResource :: Resource -> Resource
        correctResource r = r & rrelations %~ HM.fromList . filter (\(x,_) -> aliasMap & has (ix x)) . map (_1 %~ correct) . HM.toList
    in   (wResources .~ correctResources)
       . (wEdges     .~ correctEdges)
       $ w


-- | Finds the dead code
findDeadCode :: String -> [Resource] -> Set.Set FilePath -> IO ()
findDeadCode puppetdir catalogs allfiles = do
    -- first collect all files / positions from all the catalogs
    let allpositions = Set.fromList $ catalogs ^.. traverse . rpos
    -- now find all haskell files
    puppetfiles <- Set.fromList . concat . fst <$> G.globDir [G.compile "**/*.pp"] (puppetdir <> "/modules")
    let deadfiles = Set.filter ("/manifests/" `isInfixOf`) $ puppetfiles `Set.difference`   allfiles
        usedfiles = puppetfiles `Set.intersection` allfiles
    unless (Set.null deadfiles) $ do
        putDoc ("The following files" <+> int (Set.size deadfiles) <+> "are not used: " <> list (map string $ Set.toList deadfiles))
        putStrLn ""
    allparses <- parallel (map parseFile (Set.toList usedfiles))
    let (parseFailed, parseSucceeded) = partitionEithers allparses
    unless (null parseFailed) $ do
        putDoc ("The following" <+> int (length parseFailed) <+> "files could not be parsed:" </> indent 4 (vcat (map (string . show) parseFailed)))
        putStrLn ""
    let getSubStatements s@(ResourceDeclaration{}) = [s]
        getSubStatements (ConditionalStatement (CondStatement conds _)) = conds ^.. traverse . _2 . tgt
        getSubStatements s@(ClassDeclaration{}) = extractPrism s
        getSubStatements s@(DefineDeclaration{}) = extractPrism s
        getSubStatements s@(Node{}) = extractPrism s
        getSubStatements s@(SHFunctionCall{}) = extractPrism s
        getSubStatements (TopContainer v s) = getSubStatements s ++ v ^.. tgt
        getSubStatements _ = []
        tgt = folded . to getSubStatements . folded
        extractPrism = toListOf (_Statements . traverse . to getSubStatements . traverse)
        allResources = parseSucceeded ^.. folded . folded . to getSubStatements . folded
        deadResources = filter isDead allResources
        isDead (ResourceDeclaration (ResDec _ _ _ _ pp)) = not $ Set.member pp allpositions
        isDead _ = True
    unless (null deadResources) $ do
        putDoc ("The following" <+> int (length deadResources) <+> "resource declarations are not used:" </> indent 4 (vcat (map pretty deadResources)))
        putStrLn ""

newtype Maximum a = Maximum { getMaximum :: Maybe a }

instance (Ord a) => Monoid (Maximum a) where
    mempty = Maximum Nothing
    mappend (Maximum Nothing) m2 = m2
    mappend m1 (Maximum Nothing) = m1
    mappend (Maximum (Just a1)) (Maximum (Just a2)) = Maximum (Just (max a1 a2))


-- | For each node, queryfunc the catalog and return stats
computeStats :: FilePath -> Options -> QueryFunc -> (MStats, MStats, MStats) -> [Nodename] -> IO ()
computeStats workingdir (Options {..})
             queryfunc (parsingStats, catalogStats, templateStats)
             topnodes = do
    -- the parsing statistics, so that we known which files
    (cats, Sum failures) <- catMaybesCount <$> parallel (map (computeCatalog queryfunc) topnodes)
    pStats <- getStats parsingStats
    cStats <- getStats catalogStats
    tStats <- getStats templateStats
    let allres = (cats ^.. folded . _1 . folded) ++ (cats ^.. folded . _2 . folded)
        allfiles = Set.fromList $ map T.unpack $ HM.keys pStats
    when _optDeadcode $ findDeadCode workingdir allres allfiles
    -- compute statistics
    let (parsing,    Just (wPName, wPMean)) = worstAndSum pStats
        (cataloging, Just (wCName, wCMean)) = worstAndSum cStats
        (templating, Just (wTName, wTMean)) = worstAndSum tStats
        parserShare = 100 * parsing / cataloging
        templateShare = 100 * templating / cataloging
        formatDouble = take 5 . show -- yeah, well ...
        nbnodes = length topnodes
        worstAndSum = (_1 %~ getSum)
                            . (_2 %~ fmap swap . getMaximum)
                            . ifoldMap (\k (StatsPoint cnt total _ _) -> (Sum total, Maximum $ Just (total / fromIntegral cnt, k)))
    putStr ("\nTested " ++ show nbnodes ++ " nodes. ")
    unless (nbnodes == 0) $ do
        putStrLn (formatDouble parserShare <> "% of total CPU time spent parsing, " <> formatDouble templateShare <> "% spent computing templates")
        when (_optLoglevel <= LOG.INFO) $ do
            putStrLn ("Slowest template:           " <> T.unpack wTName <> ", taking " <> formatDouble wTMean <> "s on average")
            putStrLn ("Slowest file to parse:      " <> T.unpack wPName <> ", taking " <> formatDouble wPMean <> "s on average")
            putStrLn ("Slowest catalog to compute: " <> T.unpack wCName <> ", taking " <> formatDouble wCMean <> "s on average")

    if failures > 0
       then do {putDoc ("Found" <+> red (int failures) <+> "failure(s)." <> line) ; exitFailure}
       else do {putDoc (dullgreen "All green."  <> line) ; exitSuccess}

    where
        computeCatalog :: QueryFunc -> Nodename -> IO (Maybe (FinalCatalog, [Resource]))
        computeCatalog func node =
            func node >>= \case
              S.Left err -> putDoc (line <> red "ERROR:" <+> parens (ttext node) <+> ":" <+> getError err) >> return Nothing
              S.Right (rawcatalog, _ , rawexported, knownRes) -> return (Just (rawcatalog <> rawexported, knownRes))

-- | Queryfunc the catalog for the node and PP the result
computeNodeCatalog :: Options -> QueryFunc -> PuppetDBAPI IO -> Nodename -> IO ()
computeNodeCatalog (Options {..}) queryfunc pdbapi node =
    queryfunc node >>= \case
      S.Left rr -> do
          putDoc (line <> red "ERROR:" <+> parens (ttext node) <+> getError rr)
          exitFailure
      S.Right (rawcatalog, edgemap, rawexported, _) -> do
          printFunc <- hIsTerminalDevice stdout >>= \isterm -> return $ \x ->
            if isterm
                then putDoc x >> putStrLn ""
                else displayIO stdout (renderCompact x) >> putStrLn ""
          catalog  <- filterCatalog _optResourceType _optResourceName rawcatalog
          exported <- filterCatalog _optResourceType _optResourceName rawexported
          let wireCatalog    = generateWireCatalog node (catalog    <> exported   ) edgemap
              rawWireCatalog = generateWireCatalog node (rawcatalog <> rawexported) edgemap
          when _optCheckExport $ void $ replaceCatalog pdbapi rawWireCatalog
          case (_optShowContent, _optShowjson) of
              (_, True) -> BSL.putStrLn (encode (prepareForPuppetApply wireCatalog))
              (True, _) -> do
                  unless (_optResourceType == Just "file" || isNothing _optResourceType) $ do
                      putDoc "Show content only works with resource of type file. It is an error to provide another filter type"
                      exitFailure
                  case _optResourceName of
                      Just f  -> printContent f catalog
                      Nothing -> putDoc "You should supply a resource name when using showcontent" >> exitFailure
              _         -> do
                  printFunc (pretty (HM.elems catalog))
                  unless (HM.null exported) $ do
                      printFunc (mempty <+> dullyellow "Exported:" <+> mempty)
                      printFunc (pretty (HM.elems exported))


-- | Filter according to the type and name regex from the command line option
filterCatalog :: Maybe T.Text -> Maybe T.Text -> FinalCatalog -> IO FinalCatalog
filterCatalog typeFilter nameFilter = filterC typeFilter (_1 . itype . unpacked) >=> filterC nameFilter (_1 . iname . unpacked)
    where
       -- filter catalog using the adhoc lens
       filterC Nothing _ c = return c
       filterC (Just regexp) l c = compile compBlank execBlank (T.unpack regexp) >>= \case
          Left rr   -> error ("Error compiling regexp 're': "  ++ show rr)
          Right reg -> HM.fromList <$> filterM (filterResource reg l) (HM.toList c)
       filterResource reg l v = execute reg (v ^. l) >>= \case
                                         Left rr -> error ("Error when applying regexp: " ++ show rr)
                                         Right Nothing -> return False
                                         _ -> return True


run :: Options -> IO ()
-- | Parse mode
run (Options {_optParse = Just fp}) = parseFile fp >>= \case
            Left rr -> error ("parse error:" ++ show rr)
            Right s -> putDoc $ ppStatements s

run (Options {_optPuppetdir = Nothing, _optParse = Nothing }) =
    error "Without a puppet dir, only the `--parse` option can be supported"
run (Options {_optPuppetdir = Just _, _optNodename = Nothing, _optMultnodes = Nothing}) =
    error "You need to choose between single or multiple node"

-- | Single node mode (`--node` option)
run cmd@(Options {_optNodename = Just node, _optPuppetdir = Just workingdir, ..}) = do
    (queryfunc, pdbapi, _, _, _ ) <- initializedaemonWithPuppet workingdir cmd
    computeNodeCatalog cmd queryfunc pdbapi node
    when _optCommitDB $ void $ commitDB pdbapi

-- | Multiple nodes mode (`--all`) option
run cmd@(Options {_optNodename = Nothing , _optMultnodes = Just nodes, _optPuppetdir = Just workingdir}) = do
    -- it would be really noisy to run this mode with loglevel < LOG.ERROR;
    -- even the default LOG.WARNING would clutter the output.
    -- That's why we force LOG.ERROR for the puppet daemon.
    (queryfunc, _, mPStats,mCStats,mTStats) <- initializedaemonWithPuppet workingdir (cmd {_optLoglevel = LOG.ERROR})
    computeStats workingdir cmd queryfunc (mPStats, mCStats, mTStats) =<< retrieveNodes nodes

  where
      retrieveNodes :: MultNodes -> IO [Nodename]
      retrieveNodes AllNodes = do
          allstmts <- parseFile (workingdir <> "/manifests/site.pp") >>= \case Left err -> error (show err)
                                                                               Right x  -> return x
          let getNodeName (Node (Nd (NodeName n) _ _ _)) = Just n
              getNodeName _ = Nothing
          return $ mapMaybe getNodeName (V.toList allstmts)
      retrieveNodes (MultNodes xs) = return xs

main :: IO ()
main = execParser opts >>= run
    where
        opts :: ParserInfo Options
        opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "A program for parsing puppet files, generating and inspecting catalogs"
                 <> header "puppetresources - a development tool for Puppet"
                 <> failureCode 3)
