{-# LANGUAGE TemplateHaskell #-}
module Puppet.Testing
    ( module Control.Lens
    , module Data.Monoid
    , module Puppet.PP
    , module Puppet.Interpreter.Types
    , Coverage
    , TestResult(..)
    , basicTest
    , testGroup
    , setCurResources
    , testCatalog
    , failedTests
    , testResource
    , testingDaemon
    ) where

import Prelude hiding (notElem,all)
import qualified Data.HashSet as HS
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Monad.Error
import Control.Applicative hiding ((<$>))
import System.Posix.Files
import qualified Data.Either.Strict as S
import qualified Data.Text as T
import qualified System.Log.Logger as LOG

import Puppet.Preferences
import Puppet.PP
import Puppet.Daemon
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter ()
import Puppet.Parser.PrettyPrinter

type Coverage = HS.HashSet RIdentifier
data TestOutcome = Success | Failure Doc | Subtests [TestResult]
data TestResult = TestResult { _testName    :: String
                             , _testOutcome :: TestOutcome
                             }
data TestReader = TestReader { _catalog   :: FinalCatalog
                             , _moduledir :: FilePath
                             , _puppetdir :: FilePath
                             }
type TestMonad = RWST TestReader Coverage [Resource] IO

instance Pretty TestOutcome where
    pretty Success = dullgreen "OK"
    pretty (Failure d) = dullred d
    pretty (Subtests tsts) = mempty </> indent 2 (vcat (map pretty tsts))

instance Pretty TestResult where
    pretty (TestResult tn top) = string tn <+> pretty top

makeClassy ''TestResult
makeClassy ''TestReader

basicTest :: TestMonad TestResult
basicTest = testFileSources

testGroup :: String -> [TestMonad TestResult] -> TestMonad TestResult
testGroup tn tsts = TestResult `fmap` pure tn <*> (Subtests `fmap` sequence tsts)

setCurResources :: [Resource] -> TestMonad ()
setCurResources r = tell (HS.fromList (map _rid r)) >> put r

testResource :: Resource -> [TestMonad TestResult] -> TestMonad TestResult
testResource r a = setCurResources [r] >> testGroup "Resources" a

testCatalog :: FilePath -> FinalCatalog -> TestMonad TestResult -> IO (TestResult, Coverage)
testCatalog pdir catlg test = do
    (results,_,coverage) <- runRWST test (TestReader catlg (pdir <> "/modules") pdir) []
    return (results, coverage)

testFileSources :: TestMonad TestResult
testFileSources = do
    let getFiles = filter presentFile . toList
        presentFile r | r ^. rid . itype /= "file" = False
                      | (r ^. rattributes . at "ensure") `notElem` [Nothing, Just "present"] = False
                      | r ^. rattributes . at "source" == Just PUndef = False
                      | otherwise = True
        getSource = mapMaybe (\r -> (,) `fmap` pure r <*> r ^. rattributes . at "source")
    files <- fmap (getSource . getFiles) $ view catalog
    pdir <- view puppetdir
    let checkSource (r, src) = liftIO (runErrorT (checkFile src))
                >>= \rs -> return $ TestResult (show (pretty (r ^. rid))) $
                        case rs of
                            Right () -> Success
                            Left rr  -> Failure (dullred rr <+> showPPos (r ^. rpos))
        testFile :: FilePath -> ErrorT Doc IO ()
        testFile fp = liftIO (fileExist fp) >>= (`unless` (throwError $ "Searched in" <+> string fp))
        checkFile :: PValue -> ErrorT Doc IO ()
        checkFile r@(PArray ar) = asum [checkFile x | x <- toList ar] <|> throwError ("Could not find the file in" <+> pretty r)
        checkFile (PString f) = do
            stringdir <- case T.stripPrefix "puppet:///" f of
                             Just r -> return r
                             Nothing -> throwError ("The source does not start with puppet:///, but is" <+> ttext f)
            case T.splitOn "/" stringdir of
                ("modules":modulename:rest) -> testFile (pdir <> "/modules/" <> T.unpack modulename <> "/files/" <> T.unpack (T.intercalate "/" rest))
                ("files":rest) -> testFile (pdir <> "/files/" <> T.unpack (T.intercalate "/" rest))
                ("private":_) -> return ()
                _ -> throwError ("Invalid file source:" <+> ttext f)
        checkFile x = throwError ("Source was not a string, but" <+> pretty x)
    TestResult `fmap` pure "Source file testing" <*> (Subtests `fmap` mapM checkSource files)

failedTests :: TestResult -> Maybe TestResult
failedTests t@(TestResult d o) = case o of
                                     Success -> Nothing
                                     Failure _ -> Just t
                                     Subtests ts -> case mapMaybe failedTests ts of
                                                        [] -> Nothing
                                                        x -> Just (TestResult d (Subtests x))

-- | Initializes a daemon made for running tests, using the specific test
-- puppetDB
testingDaemon :: Maybe T.Text -- ^ Might contain the URL of the actual PuppetDB, used for getting facts.
              -> FilePath -- ^ Path to the manifests
              -> (T.Text -> IO (Container T.Text)) -- ^ The facter function
              -> IO (T.Text -> IO (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog)))
testingDaemon purl puppetdir allFacts = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.WARNING)
    prefs <- genPreferences puppetdir
    q <- initDaemon (prefs { _compilePoolSize = 8, _parsePoolSize = 2 })
    return (\nodename -> allFacts nodename >>= _dGetCatalog q nodename)


