module Puppet.Testing (testCatalog, Test, testFileSources) where

import qualified Data.Map as Map
import Puppet.Interpreter.Types
import Data.List
import Data.Maybe
import Data.List.Utils
import Data.Either
import Control.Monad.Error
import System.Posix.Files

type TestResult = IO (Either String ())

data TestR
        = TestGroupR  String [TestR]
        | SingleTestR String (Either String ())
        deriving (Show)

data Test
        = TestGroup String [Test]
        | TestFirstOk String [Test]
        | SingleTest String (FinalCatalog -> TestResult)

failedTests :: TestR -> Maybe TestR
failedTests (TestGroupR d tests) = case catMaybes (map failedTests tests) of
                                       [] -> Nothing
                                       x  -> Just (TestGroupR d x)
failedTests t@(SingleTestR _ (Left _)) = Just t
failedTests _ = Nothing

showRes :: TestR -> String
showRes = showRes' 0
    where
        showRes' :: Int -> TestR -> String
        showRes' dec (TestGroupR desc tsts) = replicate dec ' ' ++ desc ++ "\n" ++ unlines (map (showRes' (dec + 1)) tsts)
        showRes' dec (SingleTestR desc (Right ())) = replicate dec ' ' ++ desc ++ " OK"
        showRes' dec (SingleTestR desc (Left err)) = replicate dec ' ' ++ desc ++ " FAIL: " ++ err

testFileSources :: String -> FinalCatalog -> Test
testFileSources puppetdir cat =
    let fileresources = Map.elems $ Map.filterWithKey (\k _ -> fst k == "file") cat
        filesources = catMaybes $ map (Map.lookup "source" . rrparams) fileresources
        findPlaces :: String -> [String]
        findPlaces stringdir =
            let defaultsearch = puppetdir ++ "/files/" ++ stringdir
            in case split "/" stringdir of
                ("private":_) -> [puppetdir] -- not handled
                ("modules":modulename:rest) -> [puppetdir ++ "/modules/" ++ modulename ++ "/files/" ++ intercalate "/" rest, defaultsearch]
                _ -> [defaultsearch]
        checkSrcExists :: String -> FinalCatalog -> TestResult
        checkSrcExists src _ = runErrorT $ do
            let protostring = "puppet:///"
            unless (startswith protostring src) (throwError "Does not start with puppet:///")
            let stringdir = drop (length protostring) src
                places = findPlaces stringdir
            exists <- liftIO $ foldM (\c n -> if c then return True else fileExist n) False places
            unless exists (throwError $ "Searched in " ++ show places)
            return ()
        genFileTest :: ResolvedValue -> Test
        genFileTest (ResolvedString src) = SingleTest (src ++ " exists") (checkSrcExists src)
        genFileTest (ResolvedArray  arr) = TestFirstOk "First exists" (map genFileTest arr)
        genFileTest x                    = SingleTest ("Valid source") (\_ -> return $ Left ("Not a valid data type: " ++ show x))
    in  (TestGroup "check that all files are defined" (map genFileTest filesources))

unsingle :: TestR -> Either String ()
unsingle (SingleTestR desc (Left err)) = Left (desc ++ " failed: " ++ err)
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
        Just fl -> return $ Left $ showRes fl

testCatalog :: String -> FinalCatalog -> [Test] -> IO (Either String ())
testCatalog puppetdir catalog stests = runTests (TestGroup "All Tests" ( testFileSources puppetdir catalog : stests )) catalog
