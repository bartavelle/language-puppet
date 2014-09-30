{-# LANGUAGE LambdaCase #-}
module Main where

import           Puppet.Interpreter.Types
import           PuppetDB.Common
import           PuppetDB.TestDB
import           PuppetDB.Remote
import           Facter

import           Control.Lens
import           Control.Monad (forM_,unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Either.Strict as S
import qualified Data.HashMap.Strict as HM
import           Data.List (foldl')
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml hiding (Parser)
import           Options.Applicative as O

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
factedit = EditFact <$> O.argument (Just . T.pack) mempty <*> O.argument (Just . T.pack) mempty

resourcesparser :: Parser Command
resourcesparser = DumpResources <$> O.argument (Just . T.pack) mempty

delnodeparser :: Parser Command
delnodeparser = DeactivateNode <$> O.argument (Just . T.pack) mempty

createtestdb :: Parser Command
createtestdb = CreateTestDB <$> O.argument Just mempty

addfacts :: Parser Command
addfacts = AddFacts <$> O.argument (Just . T.pack) mempty


display :: (Show r, ToJSON a) => String -> S.Either r a -> IO ()
display s (S.Left rr) = error (s <> " " <> show rr)
display _ (S.Right a) = BS.putStrLn (encode a)

checkError :: (Show r) => String -> S.Either r a -> IO a
checkError s (S.Left rr) = error (s <> " " <> show rr)
checkError _ (S.Right a) = return a

run :: Options -> IO ()
run cmdl = do
    epdbapi <- case (_pdbloc cmdl, _pdbtype cmdl) of
                   (Just l, PDBRemote) -> pdbConnect (T.pack l)
                   (Just l, PDBTest)   -> loadTestDB l
                   (_, x)              -> getDefaultDB x
    pdbapi <- case epdbapi of
                  S.Left r -> error (show r)
                  S.Right x -> return x
    let getOrError s (S.Left rr) = error (s <> " " <> show rr)
        getOrError _ (S.Right x) = return x
    case _pdbcmd cmdl of
        DumpFacts -> if _pdbtype cmdl == PDBDummy
                         then puppetDBFacts "dummy"  pdbapi >>= mapM_ print . HM.toList
                         else do
                             allfacts <- getFacts pdbapi QEmpty >>= checkError "get facts"
                             tmpdb <- loadTestDB "/tmp/allfacts.yaml" >>= checkError "load test db"
                             let groupfacts = foldl' groupfact HM.empty allfacts
                                 groupfact curmap (PFactInfo ndname fctname fctval) =
                                     curmap & at ndname . non HM.empty %~ (at fctname ?~ fctval)
                             replaceFacts tmpdb (HM.toList groupfacts) >>= checkError "replace facts in dummy db"
                             commitDB tmpdb >>= checkError "commit db"
        DumpNodes -> getNodes pdbapi QEmpty >>= display "dump nodes"
        AddFacts n -> do
            unless (_pdbtype cmdl == PDBTest) (error "This option only works with the test puppetdb")
            fcts <- puppetDBFacts n pdbapi
            replaceFacts pdbapi [(n, fcts)] >>= getOrError "replace facts"
            commitDB pdbapi >>= getOrError "commit db"
        CreateTestDB destfile -> do
            ndb <- loadTestDB destfile >>= getOrError "puppetdb load"
            allnodes <- getNodes pdbapi QEmpty >>= getOrError "get nodes"
            allfacts <- getFacts pdbapi QEmpty >>= getOrError "get facts"
            let factsGrouped = HM.toList $ HM.fromListWith (<>) $ map (\x -> (x ^. nodename, HM.singleton (x ^. factname) (x ^. factval))) allfacts
            replaceFacts ndb factsGrouped >>= getOrError "replace facts"
            forM_ allnodes $ \pnodename -> do
                let ndename = pnodename ^. nodename
                res <- getResourcesOfNode pdbapi ndename QEmpty >>= getOrError ("get resources for " ++ show ndename)
                let wirecatalog = WireCatalog ndename "version" V.empty (V.fromList res) ndename
                replaceCatalog ndb wirecatalog
            commitDB ndb >>= getOrError "commit db"
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
