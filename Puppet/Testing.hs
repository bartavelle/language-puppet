module Puppet.Testing
    ( testCatalog
    , Test(..)
    , testFileSources
    , TestResult
    , testingDaemon
    , module Puppet.Interpreter.Types
    ) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Control.Monad.Error
import System.Posix.Files
import qualified System.Log.Logger as LOG
import qualified Data.Text as T

import Puppet.Interpreter.Types
import Puppet.Init
import Puppet.Daemon
import PuppetDB.TestDB
import PuppetDB.Rest
import Puppet.Utils

type TestResult = IO (Either String ())

data TestR
        = TestGroupR  T.Text [TestR]
        | SingleTestR T.Text (Either String ())
        deriving (Show)

data Test
        = TestGroup T.Text [Test]
        | TestFirstOk T.Text [Test]
        | SingleTest T.Text (FinalCatalog -> TestResult)

failedTests :: TestR -> Maybe TestR
failedTests (TestGroupR d tests) = case catMaybes (map failedTests tests) of
                                       [] -> Nothing
                                       x  -> Just (TestGroupR d x)
failedTests t@(SingleTestR _ (Left _)) = Just t
failedTests _ = Nothing

showRes :: TestR -> T.Text
showRes = showRes' 0
    where
        showRes' :: Int -> TestR -> T.Text
        showRes' dec (TestGroupR desc tsts)        = T.replicate dec " " <> desc <> "\n" <> T.unlines (map (showRes' (dec + 1)) tsts)
        showRes' dec (SingleTestR desc (Right ())) = T.replicate dec " " <> desc <> " OK"
        showRes' dec (SingleTestR desc (Left err)) = T.replicate dec " " <> desc <> " FAIL: " <> T.pack err

testFileSources :: T.Text -> FinalCatalog -> Test
testFileSources puppetdir cat =
    let fileresources = Map.elems $ Map.filterWithKey (\k _ -> fst k == "file") cat
        filesources = catMaybes $ map (Map.lookup "source" . rrparams) fileresources
        findPlace :: T.Text -> Maybe T.Text
        findPlace stringdir =
            case T.splitOn "/" stringdir of
                ("private":_)               -> Just puppetdir -- not handled
                ("modules":modulename:rest) -> Just $ puppetdir <> "/modules/" <> modulename <> "/files/" <> T.intercalate "/" rest
                ("files":rest)              -> Just $ puppetdir <> "/files/"   <> T.intercalate "/" rest
                _                           -> Nothing
        checkSrcExists :: T.Text -> FinalCatalog -> TestResult
        checkSrcExists src _ = runErrorT $ do
            let protostring = "puppet:///"
            unless (T.isPrefixOf protostring src) (throwError "Does not start with puppet:///")
            let stringdir = T.drop (T.length protostring) src
                place = findPlace stringdir
            case place of
                          Just dir -> liftIO (fileExist (T.unpack dir)) >>= (`unless` (throwError $ "Searched in " ++ T.unpack dir))
                          Nothing  -> throwError ("Unknown path: " ++ T.unpack stringdir)
        genFileTest :: ResolvedValue -> Test
        genFileTest (ResolvedString src) = SingleTest (src <> " exists") (checkSrcExists src)
        genFileTest (ResolvedArray  arr) = TestFirstOk "First exists" (map genFileTest arr)
        genFileTest x                    = SingleTest ("Valid source") (\_ -> return $ Left ("Not a valid data type: " ++ show x))
    in  (TestGroup "check that all files are defined" (map genFileTest filesources))

unsingle :: TestR -> Either String ()
unsingle (SingleTestR desc (Left err)) = Left (T.unpack desc ++ " failed: " ++ err)
unsingle (SingleTestR _    _         ) = Right ()
unsingle x                             = Left ("Bad type for unsingle " ++ show x)

runTest :: FinalCatalog -> Test -> IO TestR
runTest cat (SingleTest desc test) = fmap (\x -> SingleTestR desc x) (test cat)
runTest cat (TestGroup desc tests) = fmap (TestGroupR desc) (mapM (runTest cat) tests)
runTest cat (TestFirstOk desc tests) = do
    allRes <- mapM (fmap unsingle . runTest cat) tests
    case lefts allRes of
        [] -> return $ SingleTestR desc (Right ())
        x  -> return $ SingleTestR desc (Left (show x))

runTests :: Test -> FinalCatalog -> IO (Either String ())
runTests tsts cat = do
    tr <- fmap failedTests (runTest cat tsts)
    case tr of
        Nothing -> return $ Right ()
        Just fl -> return $ Left $ T.unpack $ showRes fl

testCatalog :: T.Text -> FinalCatalog -> [Test] -> IO (Either String ())
testCatalog puppetdir catalog stests = runTests (TestGroup "All Tests" ( testFileSources puppetdir catalog : stests )) catalog

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

