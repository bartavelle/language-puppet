{-# LANGUAGE LambdaCase #-}
module Main where

import Puppet.Interpreter.Types
import PuppetDB.Common
import PuppetDB.TestDB
import PuppetDB.Remote
import Facter

import Options.Applicative as O
import qualified Data.Text as T
import Data.Monoid
import Data.Yaml hiding (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Either.Strict as S
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Control.Monad (forM_,unless)
import qualified Data.Vector as V

data CommandLine = CommandLine { _pdbloc :: Maybe FilePath
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

cmdlineParser :: Parser CommandLine
cmdlineParser = CommandLine <$> optional pl <*> pt <*> cmd
    where
        pl = strOption (  long "location"
                       <> short 'l'
                       <> help "Location of the PuppetDB, a file for type 'test' or an URL for type 'remote'"
                       )
        pt = option (  long "pdbtype"
                    <> short 't'
                    <> value PDBTest
                    <> help "PuppetDB types : test, remote, dummy"
                    )
        cmd = subparser (  command "dumpfacts" (ParserInfo (pure DumpFacts) True "Dump all facts"     "Dump all facts"     "" 4)
                        <> command "editfact"  (ParserInfo factedit         True "Edit a fact corresponding to a node" ""  "" 7)
                        <> command "dumpres"   (ParserInfo resourcesparser  True "Dump resources"     "Dump resources"     "" 5)
                        <> command "delnode"   (ParserInfo delnodeparser    True "Deactivate node"    "Deactivate node"    "" 6)
                        <> command "nodes"     (ParserInfo (pure DumpNodes) True "Dump all nodes"     "Dump all nodes"     "" 8)
                        <> command "snapshot"  (ParserInfo createtestdb     True "Create a test DB from the current DB" "" "" 10)
                        <> command "addfacts"  (ParserInfo addfacts         True "Adds facts to the test DB for the given node name, if they are not already defined" "" "" 11)
                        )

display :: (Show r, ToJSON a) => String -> S.Either r a -> IO ()
display s (S.Left rr) = error (s <> " " <> show rr)
display _ (S.Right a) = BS.putStrLn (encode a)

run :: CommandLine -> IO ()
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
                         else getFacts pdbapi QEmpty >>= display "get facts"
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
main = execParser pinfo >>= run
    where
        pinfo :: ParserInfo CommandLine
        pinfo = ParserInfo (helper <*> cmdlineParser) True "A program to work with PuppetDB implementations" "pdbQuery - work with PuppetDB implementations" "" 3
