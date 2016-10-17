{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Lens
import           Control.Monad              (forM_, unless, (>=>))
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as BS
import qualified Data.Either.Strict         as S
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl')
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Version               (showVersion)
import           Data.Yaml                  hiding (Parser)
import           Network.HTTP.Client
import           Options.Applicative        as O
import qualified Paths_language_puppet
import           Servant.Common.BaseUrl
import           System.Exit                (exitFailure)

import           Facter
import           Puppet.Interpreter.Types
import           PuppetDB.Common
import           PuppetDB.Remote
import           PuppetDB.TestDB


data Options = Options { _pdbloc     :: Maybe FilePath
                       , _pdbtype    :: PDBType
                       , _pdbcmd     :: Maybe Command
                       , _pdbversion :: Bool
                       }

data Command = DumpFacts
             | DumpNodes
             | EditFact T.Text T.Text
             | DeactivateNode T.Text
             | DumpResources T.Text
             | CreateTestDB FilePath
             | AddFacts T.Text

options :: Parser Options
options = Options
    <$> optional (strOption
        (  long "location"
        <> short 'l'
        <> metavar "FILE|URL"
        <> help "Location of the PuppetDB, a file for type 'test' or an URL for type 'remote'"))
    <*> option auto
        (  long "pdbtype"
        <> short 't'
        <> value PDBTest
        <> help "PuppetDB types : test, remote, dummy")
    <*> optional cmd

    <*> switch
        (  long "version"
        <> help "Output version information and exit")
    where
        cmd = subparser (  command "dumpfacts" (info (pure DumpFacts)(progDesc "Dump all facts, store in /tmp/allfacts.yaml" <> failureCode 4))
                        <> command "editfact"  (info factedit (progDesc "Edit a fact corresponding to a node" <> failureCode 7 ))
                        <> command "dumpres"   (info resourcesparser (progDesc "Dump resources" <> failureCode 5))
                        <> command "delnode"   (info delnodeparser   (progDesc "Deactivate node" <> failureCode 6))
                        <> command "nodes"     (info (pure DumpNodes)(progDesc "Dump all nodes" <> failureCode 8))
                        <> command "snapshot"  (info createtestdb    (progDesc "Create a test DB from the current DB" <> failureCode 10))
                        <> command "addfacts"  (info addfacts        (progDesc "Adds facts to the test DB for the given node name, if they are not already defined" <> failureCode 11))
                        )

factedit :: Parser Command
factedit = EditFact <$> O.argument auto mempty <*> O.argument auto mempty

resourcesparser :: Parser Command
resourcesparser = DumpResources <$> fmap T.pack (O.strArgument (metavar "NODE"))

delnodeparser :: Parser Command
delnodeparser = DeactivateNode <$> O.argument auto mempty

createtestdb :: Parser Command
createtestdb = CreateTestDB <$> O.argument str (metavar "FILE")

addfacts :: Parser Command
addfacts = AddFacts <$> O.argument auto mempty


display :: (Show r, ToJSON a) => String -> Either r a -> IO ()
display s (Left rr) = error (s <> " " <> show rr)
display _ (Right a) = BS.putStrLn (encode a)

checkErrorS :: (Show r) => String -> S.Either r a -> IO a
checkErrorS s (S.Left rr) = error (s <> " " <> show rr)
checkErrorS _ (S.Right a) = return a

checkError :: (Show r) => String -> Either r a -> IO a
checkError s (Left rr) = error (s <> " " <> show rr)
checkError _ (Right a) = return a

runCheck :: Show r => String -> ExceptT r IO a -> IO a
runCheck s = runExceptT >=> checkError s

showHelpText :: ParserPrefs -> ParserInfo a -> IO ()
showHelpText pprefs pinfo = handleParseResult . Failure $
  parserFailure pprefs pinfo ShowHelpText mempty

run :: Options -> IO ()
run Options {_pdbversion = False, _pdbcmd = Nothing} =
  putStrLn "Please provide one of the available command (see --help for more information) " *> exitFailure
run Options {_pdbversion = True, ..} = putStrLn ("language-puppet " ++ Data.Version.showVersion Paths_language_puppet.version)

run Options{_pdbcmd = Just pdbcmd, ..} = do
    mgr <- newManager defaultManagerSettings
    epdbapi <- case (_pdbloc, _pdbtype) of
                   (Just l, PDBRemote) -> pdbConnect mgr $ either (error . show) id $ parseBaseUrl l
                   (Just l, PDBTest)   -> loadTestDB l
                   (_, x)              -> getDefaultDB x
    pdbapi <- case epdbapi of
                  Left r  -> error (show r)
                  Right x -> return x
    case pdbcmd of
        DumpFacts -> if _pdbtype == PDBDummy
                         then puppetDBFacts "dummy"  pdbapi >>= mapM_ print . HM.toList
                         else do
                             allfacts <- runCheck "get facts" (getFacts pdbapi QEmpty)
                             tmpdb <- loadTestDB "/tmp/allfacts.yaml" >>= checkError "load test db"
                             let groupfacts = foldl' groupfact HM.empty allfacts
                                 groupfact curmap (FactInfo ndname fctname fctval) =
                                     curmap & at ndname . non HM.empty %~ (at fctname ?~ fctval)
                             runCheck "replace facts in dummy db" (replaceFacts tmpdb (HM.toList groupfacts))
                             runCheck "commit db" (commitDB tmpdb)
        DumpNodes -> runExceptT (getNodes pdbapi QEmpty) >>= display "dump nodes"
        DumpResources n -> runExceptT (getResourcesOfNode pdbapi n QEmpty) >>= display "get resources"
        AddFacts n -> do
            unless (_pdbtype == PDBTest) (error "This option only works with the test puppetdb")
            fcts <- puppetDBFacts n pdbapi
            runCheck "replace facts" (replaceFacts pdbapi [(n, fcts)])
            runCheck "commit db" (commitDB pdbapi)
        CreateTestDB destfile -> do
            ndb <- loadTestDB destfile >>= checkError "puppetdb load"
            allnodes <- runCheck "get nodes" (getNodes pdbapi QEmpty)
            allfacts <- runCheck "get facts" (getFacts pdbapi QEmpty)
            let factsGrouped = HM.toList $ HM.fromListWith (<>) $ map (\x -> (x ^. factInfoNodename, HM.singleton (x ^. factInfoName) (x ^. factInfoVal))) allfacts
            runCheck "replace facts" (replaceFacts ndb factsGrouped)
            forM_ allnodes $ \pnodename -> do
                let ndename = pnodename ^. nodeInfoName
                res <- runCheck ("get resources for " ++ show ndename) (getResourcesOfNode pdbapi ndename QEmpty)
                let wirecatalog = WireCatalog ndename "version" V.empty (V.fromList res) ndename
                runCheck "replace catalog" (replaceCatalog ndb wirecatalog)
            runCheck "commit db" (commitDB ndb)
        _ -> error "Not yet implemented"

main :: IO ()
main = execParser opts >>= run
    where
        opts :: ParserInfo Options
        opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "A program to work with PuppetDB implementations"
                 <> header "pdbQuery - work with PuppetDB implementations"
                 <> failureCode 3)
