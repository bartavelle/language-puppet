{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE  NamedFieldPuns #-}
module Main where

import           Control.Concurrent.ParallelIO (parallel)
import           Control.Lens
import           Control.Monad
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either (partitionEithers)
import qualified Data.Either.Strict as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (isInfixOf)
import           Data.Maybe (mapMaybe)
import           Data.Monoid hiding (First)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Strict.Lens
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Data.Yaml (decodeFileEither)
import           Options.Applicative
import           System.Exit (exitFailure, exitSuccess)
import qualified System.FilePath.Glob as G
import           System.IO
import qualified System.Log.Logger as LOG
import qualified Test.Hspec.Runner as H
import qualified Text.Parsec as P
import           Text.Regex.PCRE.String

import           Facter

import           Puppet.PP hiding ((<$>))
import           Puppet.Preferences
import           Puppet.Daemon
import           Puppet.Interpreter.Types
import           Puppet.Parser.Types
import           Puppet.Parser
import           Puppet.Parser.PrettyPrinter(ppStatements)
import           Puppet.Interpreter.PrettyPrinter()
import           PuppetDB.Remote
import           PuppetDB.Dummy
import           PuppetDB.TestDB
import           PuppetDB.Common
import           Puppet.Testing
import           Puppet.Stats


type QueryFunc = T.Text -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))

data Options = Options
    { _pdb :: Maybe String
    , _showjson :: Bool
    , _showContent :: Bool
    , _resourceType :: Maybe T.Text
    , _resourceName :: Maybe T.Text
    , _puppetdir :: FilePath
    , _nodename :: Maybe T.Text
    , _pdbfile :: Maybe FilePath
    , _loglevel :: LOG.Priority
    , _hieraFile :: Maybe FilePath
    , _factsOverr :: Maybe FilePath
    , _factsDefault :: Maybe FilePath
    , _commitDB :: Bool
    , _checkExport :: Bool
    , _nousergrouptest :: Bool
    , _ignoredMods :: HS.HashSet T.Text
    } deriving (Show)

options :: Parser Options
options = Options
    <$> optional (strOption
       (  long "pdburl"
       <> help "URL of the puppetdb (ie. http://localhost:8080/)."))
   <*> switch
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
   <*> strOption
       (  long "puppetdir"
       <> short 'p'
       <> help "Puppet directory")
   <*> optional (T.pack <$> strOption
       (  long "node"
       <> short 'o'
       <> help "Node name. Using 'allnodes' enables a special mode where all nodes present in site.pp are tried. \
              \ Run with +RTS -N. Using 'deadcode' will do the same, but will print warnings about code that's not being used."))
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
   <*>  switch
       (  long "nousergrouptest"
       <> help "Disable the user and group tests")
   <*> (HS.fromList . T.splitOn "," . T.pack <$>
       strOption
       (  long "ignoremodules"
       <> help "Specify a comma-separated list of modules to ignore"
       <> value ""))

checkError :: Doc -> S.Either PrettyError a -> IO a
checkError r (S.Left rr) = error (show (red r <> ": " <+> getError rr))
checkError _ (S.Right x) = return x

{-| Does all the work of initializing a daemon for querying.
Returns the final catalog when given a node name. Note that this is pretty
hackish as it will generate facts from the local computer !
-}
initializedaemonWithPuppet :: LOG.Priority -> PuppetDBAPI IO -> FilePath -> Maybe FilePath -> (Facts -> Facts) -> HS.HashSet T.Text -> IO (QueryFunc, MStats, MStats, MStats)
initializedaemonWithPuppet loglevel pdbapi puppetdir hiera overrideFacts ignoremod = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel loglevel)
    LOG.updateGlobalLogger "Hiera.Server" (LOG.setLevel loglevel)
    q <- initDaemon =<< setupPreferences puppetdir ((prefPDB.~ pdbapi) . (hieraPath.~ hiera) . (ignoredmodules.~ ignoremod))
    let f node = fmap overrideFacts (puppetDBFacts node pdbapi) >>= _dGetCatalog q node
    return (f, _dParserStats q, _dCatalogStats q, _dTemplateStats q)

parseFile :: FilePath -> IO (Either P.ParseError (V.Vector Statement))
parseFile = fmap . runPParser puppetParser <*> T.readFile

printContent :: T.Text -> FinalCatalog -> IO ()
printContent filename catalog =
        case HM.lookup (RIdentifier "file" filename) catalog of
            Nothing -> error "File not found"
            Just r  -> case HM.lookup "content" (_rattributes r) of
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


loadFactsOverrides :: FilePath -> IO Facts
loadFactsOverrides fp = decodeFileEither fp >>= \case
    Left rr -> error ("Error when parsing " ++ fp ++ ": " ++ show rr)
    Right x -> return x

-- this finds the dead code
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


run :: Options -> IO ()
run (Options {_nodename = Nothing, _puppetdir}) = parseFile _puppetdir >>= \case
            Left rr -> error ("parse error:" ++ show rr)
            Right s -> putDoc $ ppStatements s

run cmd@(Options {_nodename = Just node, _pdb, _puppetdir, _pdbfile, _loglevel, _hieraFile, _factsOverr, _factsDefault, _commitDB, _ignoredMods}) = do
    pdbapi <- case (_pdb, _pdbfile) of
                  (Nothing, Nothing) -> return dummyPuppetDB
                  (Just _, Just _)   -> error "You must choose between a testing PuppetDB and a remote one"
                  (Just url, _)      -> pdbConnect (T.pack url) >>= checkError "Error when connecting to the remote PuppetDB"
                  (_, Just file)     -> loadTestDB file >>= checkError "Error when initializing the PuppetDB API"
    !factsOverrides <- case (_factsOverr, _factsDefault) of
                           (Just _, Just _) -> error "You can't use --facts-override and --facts-defaults at the same time"
                           (Just p, Nothing) -> HM.union `fmap` loadFactsOverrides p
                           (Nothing, Just p) -> (flip HM.union) `fmap` loadFactsOverrides p
                           (Nothing, Nothing) -> return id
    (queryfunc,mPStats,mCStats,mTStats) <- initializedaemonWithPuppet _loglevel pdbapi _puppetdir _hieraFile factsOverrides _ignoredMods
    printFunc <- hIsTerminalDevice stdout >>= \isterm -> return $ \x ->
        if isterm
            then putDoc x >> putStrLn ""
            else displayIO stdout (renderCompact x) >> putStrLn ""
    let allnodes = node == "allnodes" || deadcode
        deadcode = node == "deadcode"
    exit <- if allnodes
        then do
            allstmts <- parseFile (_puppetdir <> "/manifests/site.pp") >>= \presult -> case presult of
                                                                                          Left rr -> error (show rr)
                                                                                          Right x -> return x
            let topnodes = mapMaybe getNodeName (V.toList allstmts)
                getNodeName (Node (Nd (NodeName n) _ _ _)) = Just n
                getNodeName _ = Nothing
            cats <- parallel (map (computeCatalogs True queryfunc pdbapi printFunc cmd) topnodes)
            -- the the parsing statistics, so that we known which files
            -- were parsed
            pStats <- getStats mPStats
            cStats <- getStats mCStats
            tStats <- getStats mTStats
            -- merge all the resources together
            let cc = mapMaybe fst cats
                testFailures = getSum (cats ^. traverse . _2 . _Just . to (Sum . H.summaryFailures))
                allres = (cc ^.. folded . _1 . folded) ++ (cc ^.. folded . _2 . folded)
                allfiles = Set.fromList $ map T.unpack $ HM.keys pStats
            when deadcode $ findDeadCode _puppetdir allres allfiles
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
            putStr ("Tested " ++ show nbnodes ++ " nodes. ")
            unless (nbnodes == 0) $ do
                putStrLn (formatDouble parserShare <> "% of total CPU time spent parsing, " <> formatDouble templateShare <> "% spent computing templates")
                when (_loglevel <= LOG.INFO) $ do
                    putStrLn ("Slowest template:           " <> T.unpack wTName <> ", taking " <> formatDouble wTMean <> "s on average")
                    putStrLn ("Slowest file to parse:      " <> T.unpack wPName <> ", taking " <> formatDouble wPMean <> "s on average")
                    putStrLn ("Slowest catalog to compute: " <> T.unpack wCName <> ", taking " <> formatDouble wCMean <> "s on average")
            return $ if testFailures > 0
                         then exitFailure
                         else exitSuccess
        else do
            r <- computeCatalogs False queryfunc pdbapi printFunc cmd node
            return $ case snd r of
                         Just s  -> if (H.summaryFailures s > 0)
                                       then exitFailure
                                       else exitSuccess
                         Nothing -> exitSuccess
    when _commitDB $ void $ commitDB pdbapi
    exit

computeCatalogs :: Bool -> QueryFunc -> PuppetDBAPI IO -> (Doc -> IO ()) -> Options -> T.Text -> IO (Maybe (FinalCatalog, [Resource]), Maybe H.Summary)
computeCatalogs testOnly queryfunc pdbapi printFunc (Options {_showjson, _showContent, _resourceType, _resourceName, _puppetdir, _checkExport, _nousergrouptest}) node =
    queryfunc node >>= \case
      S.Left rr -> do
          if testOnly
              then putDoc ("Problem with" <+> ttext node <+> ":" <+> getError rr </> mempty)
              else putDoc (getError rr) >> putStrLn "" >> error "error!"
          return (Nothing, Just (H.Summary 1 1))
      S.Right (rawcatalog, m, rawexported, knownRes) -> do
          let cmpMatch Nothing _ curcat = return curcat
              cmpMatch (Just rg) lns curcat = compile compBlank execBlank (T.unpack rg) >>= \case
                  Left rr   -> error ("Error compiling regexp 're': "  ++ show rr)
                  Right rec -> fmap HM.fromList $ filterM (filterResource lns rec) (HM.toList curcat)
              filterResource lns rec v = execute rec (v ^. lns) >>= \case
                                              Left rr -> error ("Error when applying regexp: " ++ show rr)
                                              Right Nothing -> return False
                                              _ -> return True
              filterCatalog = cmpMatch _resourceType (_1 . itype . unpacked) >=> cmpMatch _resourceName (_1 . iname . unpacked)
          catalog  <- filterCatalog rawcatalog
          exported <- filterCatalog rawexported
          let wireCatalog    = generateWireCatalog node (catalog    <> exported   ) m
              rawWireCatalog = generateWireCatalog node (rawcatalog <> rawexported) m
          when _checkExport $ void $ replaceCatalog pdbapi rawWireCatalog
          testResult <- case (testOnly, _showContent, _showjson) of
              (True, _, _) -> Just `fmap` testCatalog node _puppetdir rawcatalog basicTest
              (_, _, True) -> BSL.putStrLn (encode (prepareForPuppetApply wireCatalog)) >> return Nothing
              (_, True, _) -> do
                  unless (_resourceType == Just "file" || _resourceType == Nothing) (error $ "Show content only works for file, not for " ++ show _resourceType)
                  case _resourceName of
                      Just f  -> printContent f catalog
                      Nothing -> error "You should supply a resource name when using showcontent"
                  return Nothing
              _ -> do
                  r <- testCatalog node _puppetdir rawcatalog (basicTest >> unless _nousergrouptest usersGroupsDefined)
                  printFunc (pretty (HM.elems catalog))
                  unless (HM.null exported) $ do
                      printFunc (mempty <+> dullyellow "Exported:" <+> mempty)
                      printFunc (pretty (HM.elems exported))
                  return (Just r)
          return (Just (rawcatalog <> rawexported, knownRes), testResult)

main :: IO ()
main = execParser opts >>= run
    where
        opts :: ParserInfo Options
        opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "A program for parsing puppet files, generating and inspecting catalogs"
                 <> header "puppetresources - a development tool for Puppet"
                 <> failureCode 3)
