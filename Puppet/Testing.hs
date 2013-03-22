module Puppet.Testing
    ( testCatalog
    , Test(..)
    , testFileSources
    , TestResult
    , TestMonad
    , testingDaemon
    , module Puppet.Interpreter.Types
    , getFileContent
    , getResource
    , fileContent
    , isEnsure
    , isPresent
    , isAbsent
    , checkResource
    , checkResources
    , egrep
    , sha1sum
    , runTests
    , sequenceCheck
    , sequenceCheck_
    , getParameter
    , getParameterM
    , equalOrAbsentParameter
    , equalParameter
    , equalParameters
    , (.>)
    , toByteString
    , runFullTests
    ) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Control.Monad.Error
import Control.Monad.State.Strict
import System.Posix.Files
import qualified System.Log.Logger as LOG
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Regex.PCRE.ByteString
import qualified Data.ByteString as BS
import qualified Data.Set as Set

import Puppet.Interpreter.Types
import Puppet.Interpreter.Functions
import Puppet.Init
import Puppet.Daemon
import PuppetDB.TestDB
import PuppetDB.Rest
import Puppet.Utils
import Puppet.Printers

data TestsState = TestsState { getCoverage :: Set.Set ResIdentifier
                             }
                  deriving (Show)

newState :: TestsState
newState = TestsState Set.empty

type TestResult = StateT TestsState IO (Either String ())

type TestMonad = ErrorT String (StateT TestsState IO)
                 --StateT TestsState (ErrorT String IO)

data TestR
        = TestGroupR  T.Text [TestR]
        | SingleTestR T.Text (Either String ())
        deriving (Show)

data Test
        = TestGroup T.Text [Test]
        | TestFirstOk T.Text [Test]
        | SingleTest T.Text (FinalCatalog -> TestResult)

failedTests :: TestR -> Maybe TestR
failedTests (TestGroupR d tests) = case mapMaybe failedTests tests of
                                       [] -> Nothing
                                       x  -> Just (TestGroupR d x)
failedTests t@(SingleTestR _ (Left _)) = Just t
failedTests _ = Nothing

showResT :: TestR -> T.Text
showResT = showRes' 0
    where
        showRes' :: Int -> TestR -> T.Text
        showRes' dec (TestGroupR desc tsts)        = T.replicate dec " " <> desc <> "\n" <> T.unlines (map (showRes' (dec + 1)) tsts)
        showRes' dec (SingleTestR desc (Right ())) = T.replicate dec " " <> desc <> " OK"
        showRes' dec (SingleTestR desc (Left err)) = T.replicate dec " " <> desc <> " FAIL: " <> T.pack err

-- Converts a source string to a directory on dist
sourceToPath :: FilePath -> T.Text -> TestMonad FilePath
sourceToPath puppetdir src = do
    stringdir <- case T.stripPrefix "puppet:///" src of
                     Just r  -> return r
                     Nothing -> throwError "The source does not start with puppet:///"
    case T.splitOn "/" stringdir of
        ("modules":modulename:rest) -> return $ puppetdir <> "/modules/" <> T.unpack modulename <> "/files/" <> T.unpack (T.intercalate "/" rest)
        ("files":rest)              -> return $ puppetdir <> "/files/"   <> T.unpack (T.intercalate "/" rest)
        _                           -> throwError ("Invalid file source " ++ T.unpack src)

testFileSources :: T.Text -> FinalCatalog -> Test
testFileSources puppetdir cat =
    let fileresources = Map.elems $ Map.filterWithKey (\k _ -> fst k == "file") cat
        filesources = mapMaybe (Map.lookup "source" . rrparams) fileresources
        checkSrcExists :: T.Text -> FinalCatalog -> TestResult
        checkSrcExists src _ = runErrorT $ do
            place <- sourceToPath (T.unpack puppetdir) src
            liftIO (fileExist place) >>= (`unless` (throwError $ "Searched in " ++ place))
        genFileTest :: ResolvedValue -> Test
        genFileTest (ResolvedString src) = SingleTest (src <> " exists") (checkSrcExists src)
        genFileTest (ResolvedArray  arr) = TestFirstOk "First exists" (map genFileTest arr)
        genFileTest x                    = SingleTest "Valid source" (\_ -> return $ Left ("Not a valid data type: " ++ show x))
    in  TestGroup "check that all files are defined" (map genFileTest filesources)

unsingle :: TestR -> Either String ()
unsingle (SingleTestR desc (Left err)) = Left (T.unpack desc ++ " failed: " ++ err)
unsingle (SingleTestR _    _         ) = Right ()
unsingle x                             = Left ("Bad type for unsingle " ++ show x)

runTest :: FinalCatalog -> Test -> StateT TestsState IO TestR
runTest cat (SingleTest desc test) = fmap (SingleTestR desc) (test cat)
runTest cat (TestGroup desc tests) = fmap (TestGroupR desc) (mapM (runTest cat) tests)
runTest cat (TestFirstOk desc tests) = do
    allRes <- mapM (fmap unsingle . runTest cat) tests
    case lefts allRes of
        [] -> return $ SingleTestR desc (Right ())
        x  -> return $ SingleTestR desc (Left (show x))

runTests :: Test -> FinalCatalog -> StateT TestsState IO (Either String ())
runTests tsts cat = do
    tr <- fmap failedTests (runTest cat tsts)
    case tr of
        Nothing -> return $ Right ()
        Just fl -> return $ Left $ T.unpack $ showResT fl

testCatalog :: T.Text -> FinalCatalog -> [Test] -> IO (Either String (), TestsState)
testCatalog puppetdir catalog stests = runStateT (runTests (TestGroup "All Tests" ( testFileSources puppetdir catalog : stests )) catalog) newState

-- | Initializes a daemon made for running tests, using the specific test
-- puppetDB
testingDaemon :: Maybe T.Text -- ^ Might contain the URL of the actual PuppetDB, used for getting facts.
              -> T.Text -- ^ Path to the manifests
              -> (T.Text -> IO (Map.Map T.Text ResolvedValue)) -- ^ The facter function
              -> IO (T.Text -> IO (Either String (FinalCatalog, EdgeMap, FinalCatalog)))
testingDaemon purl puppetdir allFacts = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.WARNING)
    prefs <- genPrefs puppetdir
    let realPuppetDB = case purl of
                           Nothing  -> puppetDBquery prefs { compilepoolsize = 8, parsepoolsize = 3, erbpoolsize = 4 }
                           Just url -> pdbRequest url
    (queryPDB, updatePDB) <- initTestDBFunctions realPuppetDB
    let pdbr = prefs { puppetDBquery = queryPDB }
    (queryfunc, _, _, _) <- initDaemon pdbr
    return (\nodename -> do
       o <- allFacts nodename >>= queryfunc nodename
       case o of
           Right x  -> updatePDB nodename x >> return (Right x)
           x -> return x
       )

-- | Retrieves content on disk
getSource :: FilePath -> T.Text -> TestMonad BS.ByteString
getSource puppetdir source = do
    path <- sourceToPath puppetdir source
    liftIO (BS.readFile path)

getFileContent :: FilePath -> RResource -> TestMonad BS.ByteString
getFileContent puppetdir r =
    let rname = T.unpack (showRRef (rrtype r, rrname r))
    in  case Map.lookup "content" (rrparams r) of
            Just (ResolvedString s) -> return (T.encodeUtf8 s)
            Just x -> throwError ("Content of " <> rname <> " is not a string, but: " <> show x)
            Nothing -> case Map.lookup "source" (rrparams r) of
                           Just (ResolvedString s) -> getSource puppetdir s
                           Just x   -> throwError ("Source of " <> rname <> " is not a string, but: " <> show x)
                           Nothing  -> throwError (rname <> " has no content or source, can't check for it")

getResource :: T.Text -> T.Text -> FinalCatalog -> TestMonad RResource
getResource restype resname cat = case Map.lookup (restype, resname) cat of
                                      Just r  -> do
                                          modify (\s -> s { getCoverage = Set.insert (restype, resname) (getCoverage s) })
                                          return r
                                      Nothing -> throwError ("Could not find resource " <> T.unpack (showRRef (restype, resname)))

fileContent :: FilePath -> Maybe T.Text -> T.Text -> (BS.ByteString -> TestMonad ()) -> Test
fileContent puppetdir msg filename contenttest = SingleTest testmsg (runErrorT . chain)
    where testmsg = fromMaybe ("Testing file " <> filename) msg
          chain = getResource "file" filename >=> getFileContent puppetdir >=> contenttest

checkResources :: Maybe T.Text -> T.Text -> [T.Text] -> (RResource -> TestMonad ()) -> Test
checkResources msg restype resnames test = TestGroup testmsg (map (\n -> checkResource msg restype n test) resnames)
    where testmsg = fromMaybe ("Testing resources " <> resgroup) msg
          resgroup = T.intercalate ", " (map (\n -> showRRef(restype, n)) resnames)

checkResource :: Maybe T.Text -> T.Text -> T.Text -> (RResource -> TestMonad ()) -> Test
checkResource msg restype resname test = SingleTest testmsg (runErrorT . chain)
    where testmsg = fromMaybe ("Testing resource " <> showRRef (restype, resname)) msg
          chain = getResource restype resname >=> test

isEnsure :: T.Text -> RResource -> TestMonad ()
isEnsure t r =
    let rname = T.unpack $ showRRef (rrtype r, rrname r)
    in  case Map.lookup "ensure" (rrparams r) of
            Just (ResolvedString x) -> unless (x == t) $ throwError ("Resource " <> rname <> " ensure is not " <> T.unpack t <> ", it is " <> T.unpack x)
            Just x -> throwError ("Resource " <> rname <> " ensure is not " <> T.unpack t <> ", it is " <> show x)
            Nothing -> throwError ("Resource " <> rname <> " is not ensured, can't be " <> T.unpack t)

isPresent :: RResource -> TestMonad ()
isPresent = isEnsure "present"

isAbsent :: RResource -> TestMonad ()
isAbsent = isEnsure "absent"

-- | Runs a multiline regexp
egrep :: T.Text -> BS.ByteString -> TestMonad ()
egrep regexp text = do
    reg <- liftIO $ compile compMultiline execBlank (T.encodeUtf8 regexp)
    rreg <- case reg of
                Left rr -> throwError (show rr)
                Right r -> return r
    x <- liftIO $ execute rreg text
    case x of
        Left rr -> throwError (show rr)
        Right (Just _) -> return ()
        Right _ -> throwError "Regexp did not match"

sha1sum :: T.Text -> BS.ByteString -> TestMonad ()
sha1sum cs text | puppetSHA1 (T.decodeUtf8 text) == cs = return ()
                | otherwise = throwError "Checksum mismatch"

-- | Let you sequence several checks with the same input. Useful for the
-- | checkResource function
sequenceCheck :: [a -> TestMonad b] -> a -> TestMonad [b]
sequenceCheck funcs input = mapM (\f -> f input) funcs

-- | Same thing but without output, even more useful for the checkResource
-- | function
sequenceCheck_ :: [a -> TestMonad b] -> a -> TestMonad ()
sequenceCheck_ funcs input = void $ mapM (\f -> f input) funcs

-- | Gets a resource parameter value as a (Maybe Text)
getParameterM :: T.Text -> RResource -> TestMonad (Maybe ResolvedValue)
getParameterM param r = return (Map.lookup param (rrparams r))

getParameter :: T.Text -> RResource -> TestMonad ResolvedValue
getParameter param r = case Map.lookup param (rrparams r) of
                           Just x  -> return x
                           Nothing -> throwError ("Parameter " <> T.unpack param <> " is not defined")

equalParameter :: T.Text -> ResolvedValue -> RResource -> TestMonad ()
equalParameter paramname checkvalue r = do
    realvalue <- getParameter paramname r
    unless (realvalue == checkvalue) (throwError ("Values for parameter " ++ T.unpack paramname ++ " don't match. Expected: " ++ show checkvalue ++ ", had " ++ show realvalue))

equalOrAbsentParameter :: T.Text -> ResolvedValue -> RResource -> TestMonad ()
equalOrAbsentParameter paramname checkvalue r = do
    mrealvalue <- getParameterM paramname r
    case mrealvalue of
        Just _ -> equalParameter paramname checkvalue r
        Nothing -> return ()

equalParameters :: [(T.Text, ResolvedValue)] -> RResource -> TestMonad ()
equalParameters checks = sequenceCheck_ (map (uncurry equalParameter) checks)

(.>) :: T.Text -> ResolvedValue -> (T.Text, ResolvedValue)
name .> value  = (name ,value)

toByteString :: ResolvedValue -> TestMonad BS.ByteString
toByteString (ResolvedString x) = return $ T.encodeUtf8 x
toByteString x = throwError ("Could not convert " ++ show x ++ " to a bytestring")

-- | Run tests on several hosts at once
runFullTests :: [(T.Text -> Bool, Test)] -> [(T.Text, FinalCatalog)] -> IO ()
runFullTests testlist = mapM_ runFullTests'
    where
        runFullTests' :: (T.Text, FinalCatalog) -> IO ()
        runFullTests' (hostname, catalog) = do
            let tests = TestGroup hostname $ map snd $ filter (\x -> (fst x) hostname) testlist
            (r,s) <- runStateT (runTests tests catalog) newState
            putStrLn (T.unpack hostname ++ " resource coverage " ++ show (Set.size (getCoverage s)) ++ "/" ++ show (Map.size catalog))
            case r of
                Left rr -> putStrLn rr
                Right () -> return ()
