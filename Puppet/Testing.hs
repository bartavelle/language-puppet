{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Puppet.Testing
    ( module Control.Lens
    , module Data.Monoid
    , module Puppet.PP
    , module Puppet.Interpreter.Types
    , H.hspec
    , basicTest
    , testingDaemon
    , defaultDaemon
    , testCatalog
    , describeCatalog
    , it
    , shouldBe
    ) where

import Prelude hiding (notElem,all)
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Lens
import Data.Foldable hiding (forM_)
import Data.Maybe
import Data.Monoid
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative hiding ((<$>))
import System.Posix.Files
import qualified Data.Either.Strict as S
import qualified Data.Text as T
import qualified System.Log.Logger as LOG
import qualified Test.Hspec as H
import qualified Test.Hspec.Formatters as H
import qualified Test.Hspec.Runner as H
import qualified Test.Hspec.Core as HC
import Facter
import PuppetDB.Common

import Puppet.Preferences
import Puppet.PP
import Puppet.Daemon
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter ()

data TestEnv = TestEnv { _catalog   :: FinalCatalog
                       , _moduledir :: FilePath
                       , _puppetdir :: FilePath
                       }
makeClassy ''TestEnv

type PSpecM = ReaderT TestEnv HC.SpecM
type PSpec = PSpecM ()

testCatalog ::  Nodename -> FilePath -> FinalCatalog -> PSpec -> IO H.Summary
testCatalog nd pdir catlg test = H.hspecWith (H.defaultConfig { H.configFormatter = H.failed_examples }) (describeCatalog nd pdir catlg test)

describeCatalog :: Nodename -> FilePath -> FinalCatalog -> PSpec -> H.Spec
describeCatalog nd pdir catlg test = H.describe (T.unpack nd) $ runReaderT test (TestEnv catlg (pdir <> "/modules") pdir)

basicTest :: PSpec
basicTest = hTestFileSources

it :: HC.Example a => String -> PSpecM a -> PSpec
it n tst = tst >>= lift . H.it n

shouldBe :: (Show a, Eq a) => a -> a -> PSpecM H.Expectation
shouldBe a b = return (a `H.shouldBe` b)

hTestFileSources :: PSpec
hTestFileSources = do
    let getFiles = filter presentFile . toList
        presentFile r | r ^. rid . itype /= "file" = False
                      | (r ^. rattributes . at "ensure") `notElem` [Nothing, Just "present"] = False
                      | r ^. rattributes . at "source" == Just PUndef = False
                      | otherwise = True
        getSource = mapMaybe (\r -> (,) `fmap` pure r <*> r ^. rattributes . at "source")
    files <- fmap (getSource . getFiles) $ view catalog
    pdir <- view puppetdir
    forM_ files $ \(r,filesource) -> it ("should have a source for " ++ r ^. rid . iname . to T.unpack) $ do
        let
            testFile :: FilePath -> ErrorT Doc IO ()
            testFile fp = liftIO (fileExist fp) >>= (`unless` (throwError $ "Searched in" <+> string fp))
            checkFile :: PValue -> ErrorT Doc IO ()
            checkFile res@(PArray ar) = asum [checkFile x | x <- toList ar] <|> throwError ("Could not find the file in" <+> pretty res)
            checkFile (PString f) = do
                stringdir <- case T.stripPrefix "puppet:///" f of
                                 Just o -> return o
                                 Nothing -> throwError ("The source does not start with puppet:///, but is" <+> ttext f)
                case T.splitOn "/" stringdir of
                    ("modules":modulename:rest) -> testFile (pdir <> "/modules/" <> T.unpack modulename <> "/files/" <> T.unpack (T.intercalate "/" rest))
                    ("files":rest) -> testFile (pdir <> "/files/" <> T.unpack (T.intercalate "/" rest))
                    ("private":_) -> return ()
                    _ -> throwError ("Invalid file source:" <+> ttext f)
            checkFile x = throwError ("Source was not a string, but" <+> pretty x)
        return $ do
            rs <- runErrorT (checkFile filesource)
            case rs of
                Right () -> return ()
                Left rr -> fail (show rr)

-- | Initializes a daemon made for running tests, using the specific test
-- puppetDB
testingDaemon :: PuppetDBAPI -- ^ Contains the puppetdb API functions
              -> FilePath -- ^ Path to the manifests
              -> (T.Text -> IO (Container T.Text)) -- ^ The facter function
              -> IO (T.Text -> IO (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog)))
testingDaemon pdb pdir allFacts = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.WARNING)
    prefs <- genPreferences pdir
    q <- initDaemon (prefs { _compilePoolSize = 8, _parsePoolSize = 2, _prefPDB = pdb })
    return (\nodname -> allFacts nodname >>= _dGetCatalog q nodname)

-- | A default testing daemon.
defaultDaemon :: FilePath -> IO (T.Text -> IO (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog)))
defaultDaemon pdir = do
    pdb <- getDefaultDB PDBTest >>= \case
                S.Left x -> error (show x)
                S.Right y -> return y
    testingDaemon pdb pdir (flip puppetDBFacts pdb)

