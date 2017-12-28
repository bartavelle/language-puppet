{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Main where

import           XPrelude               hiding (option)

import qualified Data.Aeson             as Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector
import qualified Data.Version           as Meta
import qualified Network.HTTP.Client    as Http
import           Options.Applicative    as O
import qualified Paths_language_puppet  as Meta
import qualified Servant.Common.BaseUrl as Servant

import           Facter
import           Puppet.Interpreter
import           PuppetDB


data Options = Options
  { _pdbloc     :: Maybe FilePath
  , _pdbtype    :: PDBType
  , _pdbcmd     :: Maybe Command
  , _pdbversion :: Bool
  }

data Command
  = DumpFacts
  | DumpFact Text
  | DumpNodes
  | EditFact Text Text
  | DeactivateNode Text
  | DumpResources Text
  | CreateTestDB FilePath
  | AddFacts Text

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
      cmd = subparser (  command "resources"   (info resourcesparser (progDesc "Output resources for one node" <> failureCode 5))
                      <> command "facts"  (info factparser (progDesc "Output facts for one node" <> failureCode 12 ))
                      <> command "nodes"     (info (pure DumpNodes)(progDesc "Output all nodes" <> failureCode 8))
                      <> command "dumpfacts" (info (pure DumpFacts)(progDesc "Dump all facts, store in /tmp/allfacts.yaml" <> failureCode 4))
                      <> command "snapshot"  (info createtestdb    (progDesc "Create a test DB from the current DB" <> failureCode 10))
                      <> command "delnode"   (info delnodeparser   (progDesc "Deactivate node" <> failureCode 6))
                      <> command "editfact"  (info factedit (progDesc "Edit a fact corresponding to a node" <> failureCode 7 ))
                      <> command "addfacts"  (info addfacts        (progDesc "Adds facts to the test DB for the given node name, if they are not already defined" <> failureCode 11))
                      )

factedit :: Parser Command
factedit = EditFact <$> O.argument auto mempty <*> O.argument auto mempty

factparser :: Parser Command
factparser = DumpFact <$> fmap Text.pack (O.strArgument (metavar "NODE"))

resourcesparser :: Parser Command
resourcesparser = DumpResources <$> fmap Text.pack (O.strArgument (metavar "NODE"))

delnodeparser :: Parser Command
delnodeparser = DeactivateNode <$> O.argument auto mempty

createtestdb :: Parser Command
createtestdb = CreateTestDB <$> O.argument str (metavar "FILE")

addfacts :: Parser Command
addfacts = AddFacts <$> O.argument auto mempty


display :: (Show r, Aeson.ToJSON a) => Text -> Either r a -> IO ()
display s (Left rr) = panic (s <> " " <> show rr)
display _ (Right a) = putLByteString (Aeson.encode a)

runCheck :: Show r => Doc -> ExceptT r IO a -> IO a
runCheck s = runExceptT >=> checkError s

showHelpText :: ParserPrefs -> ParserInfo a -> IO ()
showHelpText pprefs pinfo = handleParseResult . Failure $
  parserFailure pprefs pinfo ShowHelpText mempty

run :: Options -> IO ()
run Options {_pdbversion = False, _pdbcmd = Nothing} =
  putText "Please provide one of the available command (see --help for more information) " *> exitFailure
run Options {_pdbversion = True, ..} = putStrLn ("language-puppet " ++ Meta.showVersion Meta.version)

run Options{_pdbcmd = Just pdbcmd, ..} = do
  mgr <- Http.newManager Http.defaultManagerSettings
  epdbapi <- case (_pdbloc, _pdbtype) of
    (Just l, PDBRemote) -> pdbConnect mgr $ either (panic . show) identity $ Servant.parseBaseUrl l
    (Just l, PDBTest)   -> loadTestDB l
    (_, x)              -> getDefaultDB x
  pdbapi <- case epdbapi of
    Left r  -> panic (show r)
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
      DumpFact n -> runExceptT (getFacts pdbapi (QEqual FCertname n ) ) >>= display "dump fact"
      DumpNodes -> runExceptT (getNodes pdbapi QEmpty) >>= display "dump nodes"
      DumpResources n -> runExceptT (getResourcesOfNode pdbapi n QEmpty) >>= display "get resources"
      AddFacts n -> do
        unless (_pdbtype == PDBTest) (panic "This option only works with the test puppetdb")
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
            res <- runCheck ("get resources for " <> ppline ndename) (getResourcesOfNode pdbapi ndename QEmpty)
            let wirecatalog = WireCatalog ndename "version" Vector.empty (Vector.fromList res) ndename
            runCheck "replace catalog" (replaceCatalog ndb wirecatalog)
        runCheck "commit db" (commitDB ndb)
      _ -> panic "Not yet implemented"

main :: IO ()
main =
  execParser opts >>= run
  where
    opts :: ParserInfo Options
    opts = info (helper <*> options)
            (fullDesc
             <> progDesc "A program to work with PuppetDB implementations"
             <> header "pdbQuery - work with PuppetDB implementations"
             <> failureCode 3)
