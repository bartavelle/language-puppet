{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Puppet.Testing
    ( module Control.Lens
    , module Data.Monoid
    , module Puppet.PP
    , module Puppet.Interpreter.Types
    , module Puppet.Lens
    , H.hspec
    , basicTest
    , usersGroupsDefined
    , testingDaemon
    , defaultDaemon
    , testCatalog
    , describeCatalog
    , it
    , shouldBe
    , PSpec
    , PSpecM
    , lCatalog
    , lModuledir
    , lPuppetdir
    , withResource
    , withParameter
    , withFileContent
    ) where

import Prelude hiding (notElem,all)
import Control.Lens
import Data.Foldable hiding (forM_,mapM_)
import Data.Maybe
import Data.Monoid
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import System.Posix.Files
import qualified Data.HashSet as HS
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
import Puppet.PP hiding ((<$>))
import Puppet.Daemon
import Puppet.Lens
import Puppet.Interpreter.Types
import Puppet.Interpreter.PrettyPrinter ()

-- | The state of the reader monad the tests run in
data TestEnv = TestEnv { _lCatalog   :: FinalCatalog
                       , _lModuledir :: FilePath
                       , _lPuppetdir :: FilePath
                       }
makeClassy ''TestEnv

type PSpecM = ReaderT TestEnv HC.SpecM
type PSpec = PSpecM ()

testCatalog ::  Nodename -> FilePath -> FinalCatalog -> PSpec -> IO H.Summary
testCatalog nd pdir catlg test = H.hspecWith (H.defaultConfig { H.configFormatter = H.silent { H.failedFormatter = fform } })
                                             (describeCatalog nd pdir catlg test)
    where
        fform = do
            failures <- H.getFailMessages
            forM_ failures $ \(H.FailureRecord path reason) -> do
                H.write ("[" ++ T.unpack nd ++ "] ")
                H.writeLine (snd path)
                let err = either (("uncaught exception: " ++) . H.formatException) id reason
                H.withFailColor $ unless (null err) $ H.writeLine err
            unless (null failures) H.newParagraph

describeCatalog :: Nodename -> FilePath -> FinalCatalog -> PSpec -> H.Spec
describeCatalog nd pdir catlg test = H.describe (T.unpack nd) $ runReaderT test (TestEnv catlg (pdir <> "/modules") pdir)

-- | This tests that file sources are valid.
basicTest :: PSpec
basicTest = hTestFileSources

-- | This tests that all users and groups used as resource parameters are
-- defined
usersGroupsDefined :: PSpec
usersGroupsDefined = do
    c <- view lCatalog
    let getResourceType t = c ^.. traverse . filtered (\r -> r ^. rid . itype == t && r ^. rattributes . at "ensure" /= Just "absent")
        users = getResourceType "user"
        groups = getResourceType "group"
        knownUsers = HS.fromList $ map (view (rid . iname)) users ++ ["root","","syslog","mysql","puppet","vagrant","nginx","www-data","nagios", "postgres"]
        knownGroups = HS.fromList $ map (view (rid . iname)) groups ++ ["root", "adm", "syslog", "mysql", "nagios","puppet","","www-data", "postgres"]
        checkResource lensU lensG = mapM_ (checkResource' lensU lensG)
        checkResource' lensU lensG res = do
            let d = "Resource " <> show (pretty res) <> " should have a valid "
            case lensU of
                Just lensU' -> do
                    let u = res ^. rattributes . lensU' . _PString
                    H.it (d <> "username (" ++ T.unpack u ++ ")") (HS.member u knownUsers)
                Nothing -> return ()
            case lensG of
                Just lensG' -> do
                    let g = res ^. rattributes . lensG' . _PString
                    H.it (d <> "group (" ++ T.unpack g ++ ")") (HS.member g knownGroups)
                Nothing -> return ()
    lift $ do
        checkResource (Just $ ix "owner") (Just $ ix "group") (getResourceType "file")
        checkResource (Just $ ix "user")  (Just $ ix "group") (getResourceType "exec")
        checkResource (Just $ ix "user")  Nothing             (getResourceType "cron")
        checkResource (Just $ ix "user")  Nothing             (getResourceType "ssh_authorized_key")
        checkResource (Just $ ix "user")  Nothing             (getResourceType "ssh_authorized_key_secure")
        checkResource (Nothing)           (Just $ ix "gid") users

it :: HC.Example a => String -> PSpecM a -> PSpec
it n tst = tst >>= lift . H.it n

shouldBe :: (Show a, Eq a) => a -> a -> PSpecM H.Expectation
shouldBe a b = return (a `H.shouldBe` b)

-- | Run tests on a specific resource
withResource :: String -- ^ The test description (the thing that goes after should)
             -> T.Text -- ^ Resource type
             -> T.Text -- ^ Resource name
             -> (Resource -> H.Expectation) -- ^ Testing function
             -> PSpec
withResource desc t n o = do
    let ridentifier = RIdentifier t n
    mr <- view (lCatalog . at ridentifier)
    lift $ case mr of
        Nothing -> H.it ("Should have resource " ++ show (pretty ridentifier)) (H.expectationFailure "Resource not found")
        Just v -> H.it ("Resource " ++ show (pretty ridentifier) ++ " should " ++ desc) (o v)

-- | Tests a specific parameter
withParameter :: T.Text   -- ^ The parameter name
              -> Resource -- ^ The resource to test
              -> (PValue -> H.Expectation) -- ^ Testing function
              -> H.Expectation
withParameter prm r o = do
    case r ^. rattributes . at prm of
        Nothing -> H.expectationFailure ("Parameter " ++ T.unpack prm ++ " not found")
        Just v -> o v

-- | Retrieves a given file content, and runs a test on it. It works on the
-- explicit "content" parameter, or can resolve the "source" parameter to
-- open the file.
withFileContent :: String -- ^ Test description (the thing that goes after should)
                -> T.Text -- ^ The file path
                -> (T.Text -> H.Expectation) -- ^ Testing function
                -> PSpec
withFileContent desc fn action = withResource desc "file" fn $ \r ->
    case r ^? rattributes . ix "content" . _PString of
        Just v  -> action v
        Nothing -> H.expectationFailure "Content not found"

hTestFileSources :: PSpec
hTestFileSources = do
    let getFiles = filter presentFile . toList
        presentFile r | r ^. rid . itype /= "file" = False
                      | (r ^. rattributes . at "ensure") `notElem` [Nothing, Just "present"] = False
                      | r ^. rattributes . at "source" == Just PUndef = False
                      | otherwise = True
        getSource = mapMaybe (\r -> (,) `fmap` pure r <*> r ^. rattributes . at "source")
    files <- fmap (getSource . getFiles) $ view lCatalog
    pdir <- view lPuppetdir
    forM_ files $ \(r,filesource) -> it ("should have a source for " ++ r ^. rid . iname . to T.unpack) $ do
        let
            testFile :: FilePath -> ErrorT PrettyError IO ()
            testFile fp = liftIO (fileExist fp) >>= (`unless` (throwError $ PrettyError $ "Searched in" <+> string fp))
            checkFile :: PValue -> ErrorT PrettyError IO ()
            checkFile res@(PArray ar) = asum [checkFile x | x <- toList ar] <|> throwError (PrettyError $ "Could not find the file in" <+> pretty res)
            checkFile (PString f) =
                case (T.stripPrefix "puppet:///" f, T.stripPrefix "file:///" f) of
                    (Just stringdir, _) -> case T.splitOn "/" stringdir of
                                               ("modules":modulename:rest) -> testFile (pdir <> "/modules/" <> T.unpack modulename <> "/files/" <> T.unpack (T.intercalate "/" rest))
                                               ("files":rest) -> testFile (pdir <> "/files/" <> T.unpack (T.intercalate "/" rest))
                                               ("private":_) -> return ()
                                               _ -> throwError (PrettyError $ "Invalid file source:" <+> ttext f)
                    (Nothing, Just _) -> return ()
                    _ -> throwError (PrettyError $ "The source does not start with puppet:///, but is" <+> ttext f)
            checkFile x = throwError (PrettyError $ "Source was not a string, but" <+> pretty x)
        return $ do
            rs <- runErrorT (checkFile filesource)
            case rs of
                Right () -> return ()
                Left rr -> fail (show (getError rr))

-- | Initializes a daemon made for running tests, using the specific test
-- puppetDB
testingDaemon :: PuppetDBAPI IO -- ^ Contains the puppetdb API functions
              -> FilePath -- ^ Path to the manifests
              -> (T.Text -> IO Facts) -- ^ The facter function
              -> IO (T.Text -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource])))
testingDaemon pdb pdir allFacts = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.WARNING)
    prefs <- genPreferences pdir
    q <- initDaemon (prefs { _prefPDB = pdb })
    return (\nodname -> allFacts nodname >>= _dGetCatalog q nodname)

-- | A default testing daemon.
defaultDaemon :: FilePath -> IO (T.Text -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource])))
defaultDaemon pdir = do
    pdb <- getDefaultDB PDBTest >>= \case
                S.Left x -> error (show (getError x))
                S.Right y -> return y
    testingDaemon pdb pdir (flip puppetDBFacts pdb)

