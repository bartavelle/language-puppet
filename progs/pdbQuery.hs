{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs      #-}
module Main where

import           Puppet.Interpreter.Types
import           PuppetDB.Common
import           PuppetDB.TestDB
import           PuppetDB.Remote
import           Facter

import           Control.Lens
import           Control.Monad (forM_,unless,(>=>))
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8 as BS
import qualified Data.Either.Strict as S
import qualified Data.HashMap.Strict as HM
import           Data.List (foldl')
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml hiding (Parser)
import           Options.Applicative as O
import           Servant.Common.BaseUrl

data Options = Options { _pdbloc :: Maybe FilePath
                       , _pdbtype :: PDBType
                       , _pdbcmd :: Command
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
    <*> cmd
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
resourcesparser = DumpResources <$> O.argument auto mempty

delnodeparser :: Parser Command
delnodeparser = DeactivateNode <$> O.argument auto mempty

createtestdb :: Parser Command
createtestdb = CreateTestDB <$> O.argument auto mempty

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

runCheck :: Show r => String -> EitherT r IO a -> IO a
runCheck s = runEitherT >=> checkError s

run :: Options -> IO ()
run cmdl = do
    epdbapi <- case (_pdbloc cmdl, _pdbtype cmdl) of
                   (Just l, PDBRemote) -> pdbConnect $ either error id $ parseBaseUrl l
                   (Just l, PDBTest)   -> loadTestDB l
                   (_, x)              -> getDefaultDB x
    pdbapi <- case epdbapi of
                  S.Left r -> error (show r)
                  S.Right x -> return x
    case _pdbcmd cmdl of
        DumpFacts -> if _pdbtype cmdl == PDBDummy
                         then puppetDBFacts "dummy"  pdbapi >>= mapM_ print . HM.toList
                         else do
                             allfacts <- runCheck "get facts" (getFacts pdbapi QEmpty)
                             tmpdb <- loadTestDB "/tmp/allfacts.yaml" >>= checkErrorS "load test db"
                             let groupfacts = foldl' groupfact HM.empty allfacts
                                 groupfact curmap (PFactInfo ndname fctname fctval) =
                                     curmap & at ndname . non HM.empty %~ (at fctname ?~ fctval)
                             runCheck "replace facts in dummy db" (replaceFacts tmpdb (HM.toList groupfacts))
                             runCheck "commit db" (commitDB tmpdb)
        DumpNodes -> runEitherT (getNodes pdbapi QEmpty) >>= display "dump nodes"
        AddFacts n -> do
            unless (_pdbtype cmdl == PDBTest) (error "This option only works with the test puppetdb")
            fcts <- puppetDBFacts n pdbapi
            runCheck "replace facts" (replaceFacts pdbapi [(n, fcts)])
            runCheck "commit db" (commitDB pdbapi)
        CreateTestDB destfile -> do
            ndb <- loadTestDB destfile >>= checkErrorS "puppetdb load"
            allnodes <- runCheck "get nodes" (getNodes pdbapi QEmpty)
            allfacts <- runCheck "get facts" (getFacts pdbapi QEmpty)
            let factsGrouped = HM.toList $ HM.fromListWith (<>) $ map (\x -> (x ^. nodename, HM.singleton (x ^. factname) (x ^. factval))) allfacts
            runCheck "replace facts" (replaceFacts ndb factsGrouped)
            forM_ allnodes $ \pnodename -> do
                let ndename = pnodename ^. nodename
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
