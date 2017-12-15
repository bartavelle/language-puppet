{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
module Puppet.Daemon (
    Daemon(..)
  , initDaemon
  -- * Utils
  , checkError
  -- * Re-exports
  , module Puppet.Interpreter.Types
  , module Puppet.PP
) where

import           Puppet.Prelude

import qualified Data.Either.Strict        as S
import           Cache.File                as Cache
import qualified Data.HashMap.Strict       as HM
import qualified Data.List                 as List
import qualified Data.Text                 as Text
import qualified Data.Vector               as V
import           Debug.Trace               (traceEventIO)
import           Foreign.Ruby.Safe
import qualified System.Directory          as Directory
import qualified System.Log.Formatter      as Log (simpleLogFormatter)
import qualified System.Log.Handler        as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.Log.Logger         as Log
import qualified Text.Megaparsec           as Megaparsec

import           Erb.Compute
import           Hiera.Server
import           Puppet.Interpreter
import           Puppet.Interpreter.IO
import           Puppet.Interpreter.Types
import           Puppet.Lens               (_PrettyError)
import           Puppet.Manifests
import           Puppet.OptionalTests
import           Puppet.Parser
import           Puppet.Parser.Types
import           Puppet.PP
import           Puppet.Preferences
import           Puppet.Stats


{-| API for the Daemon.
The main method is `getCatalog`: given a node and a list of facts, it returns the result of the compilation.
This will be either an error, or a tuple containing:

- all the resources in this catalog
- the dependency map
- the exported resources
- a list of known resources, that might not be up to date, but are here for code coverage tests.

Notes :

* It might be buggy when top level statements that are not class\/define\/nodes are altered.
-}
data Daemon = Daemon
    { getCatalog    :: NodeName -> Facts -> IO (Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
    , parserStats   :: MStats
    , catalogStats  :: MStats
    , templateStats :: MStats
    }

{-| Entry point to get a Daemon
It will initialize the parsing and interpretation infrastructure from the 'Preferences'.

Internally it initializes a thread for the Ruby interpreter.
It should cache the AST of every .pp file, and could use a bit of memory. As a comparison, it
fits in 60 MB with the author's manifests, but really breathes when given 300 MB
of heap space. In this configuration, even if it spawns a ruby process for every
template evaluation, it is way faster than the puppet stack.

It can optionally talk with PuppetDB, by setting an URL via the 'prefPDB'.
The recommended way to set it to http://localhost:8080 and set a SSH tunnel :

> ssh -L 8080:localhost:8080 puppet.host
-}
initDaemon :: Preferences IO
           -> IO Daemon
initDaemon pref = do
    setupLogger (pref ^. prefLogLevel)
    logDebug "Initialize daemon"
    traceEventIO "initDaemon"
    hquery      <- hQueryApis pref
    fcache      <- newFileCache
    intr        <- startRubyInterpreter
    templStats  <- newStats
    getTemplate <- initTemplateDaemon intr pref templStats
    catStats    <- newStats
    parseStats  <- newStats
    return (Daemon
                (getCatalog' pref (parseFunc (pref ^. prefPuppetPaths) fcache parseStats) getTemplate catStats hquery)
                parseStats
                catStats
                templStats
           )

hQueryApis :: Preferences IO -> IO (HieraQueryLayers IO)
hQueryApis pref = do
  api0 <- case pref ^. prefHieraPath of
    Just p  -> startHiera p
    Nothing -> pure dummyHiera
  modapis <- getModApis pref
  pure (HieraQueryLayers api0 modapis)

getModApis :: Preferences IO -> IO (Container (HieraQueryFunc IO))
getModApis pref = do
  let ignored_modules = pref^.prefIgnoredmodules
      modpath = pref^.prefPuppetPaths.modulesPath
  dirs <- Directory.listDirectory modpath
  (HM.fromList . catMaybes) <$>
    for dirs (\dir -> runMaybeT $ do
      let modname = toS dir
          path = modpath <> "/" <> dir <> "/hiera.yaml"
      guard (modname `notElem` ignored_modules)
      guard =<< liftIO (Directory.doesFileExist path)
      liftIO $ (modname, ) <$> startHiera path)

-- | In case of a Left value, print the error and exit immediately
checkError :: Show e => Doc -> Either e a -> IO a
checkError desc = either exit return
    where
      exit = \err -> putDoc (display err) >> exitFailure
      display err = red desc <> ": " <+> (string . show) err


-- Internal functions

getCatalog' :: Preferences IO
         -> ( TopLevelType -> Text -> IO (S.Either PrettyError Statement) )
         -> (Either Text Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError Text))
         -> MStats
         -> HieraQueryLayers IO
         -> NodeName
         -> Facts
         -> IO (Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
getCatalog' pref parsingfunc getTemplate stats hquery node facts = do
    logDebug ("Received query for node " <> node)
    traceEventIO ("START getCatalog' " <> Text.unpack node)
    let catalogComputation = interpretCatalog (InterpreterReader
                                                  (pref ^. prefNatTypes)
                                                  parsingfunc
                                                  getTemplate
                                                  (pref ^. prefPDB)
                                                  (pref ^. prefExtFuncs)
                                                  node
                                                  hquery
                                                  defaultImpureMethods
                                                  (pref ^. prefIgnoredmodules)
                                                  (pref ^. prefExternalmodules)
                                                  (pref ^. prefStrictness == Strict)
                                                  (pref ^. prefPuppetPaths)
                                                  (pref ^. prefRebaseFile)
                                              )
                                              node
                                              facts
                                              (pref ^. prefPuppetSettings)
    (stmts :!: warnings) <- measure stats node catalogComputation
    mapM_ (\(p :!: m) -> Log.logM loggerName p (displayS (renderCompact (ttext node <> ":" <+> m)) "")) warnings
    traceEventIO ("STOP getCatalog' " <> Text.unpack node)
    if pref ^. prefExtraTests
       then runOptionalTests stmts
       else pure stmts
    where
      runOptionalTests stm = case stm ^? _Right._1 of
          Nothing  -> pure stm
          (Just c) -> catching _PrettyError
                              (do {testCatalog pref c; pure stm})
                              (pure . Left)

-- | Return an HOF that would parse the file associated with a toplevel.
-- The toplevel is defined by the tuple (type, name)
-- The result of the parsing is a single Statement (which recursively contains others statements)
parseFunc :: PuppetDirPaths -> FileCache String (V.Vector Statement) -> MStats -> TopLevelType -> Text -> IO (S.Either PrettyError Statement)
parseFunc ppath filecache stats = \toptype topname ->
    let nameparts = Text.splitOn "::" topname in
    let topLevelFilePath :: TopLevelType -> Text -> Either PrettyError Text
        topLevelFilePath TopNode _ = Right $ Text.pack (ppath^.manifestPath <> "/site.pp")
        topLevelFilePath  _ name
            | length nameparts == 1 = Right $ Text.pack (ppath^.modulesPath) <> "/" <> name <> "/manifests/init.pp"
            | null nameparts        = Left $ PrettyError ("Invalid toplevel" <+> squotes (ttext name))
            | otherwise             = Right $ Text.pack (ppath^.modulesPath) <> "/" <> List.head nameparts <> "/manifests/" <> Text.intercalate "/" (List.tail nameparts) <> ".pp"
    in
    case topLevelFilePath toptype topname of
        Left rr     -> return (S.Left rr)
        Right fname -> do
            let sfname = Text.unpack fname
                handleFailure :: SomeException -> IO (Either String (V.Vector Statement))
                handleFailure e = pure (Left (show e))
            x <- measure stats fname (Cache.query filecache sfname (parseFile sfname `catch` handleFailure))
            case x of
                Right stmts -> filterStatements toptype topname stmts
                Left rr -> return (S.Left (PrettyError (red (text rr))))


parseFile :: FilePath -> IO (Either String (V.Vector Statement))
parseFile fname = do
  traceEventIO ("START parsing " ++ fname)
  cnt <- readFile fname
  o <- case runPParser fname cnt of
    Right r -> traceEventIO ("Stopped parsing " ++ fname) >> pure (Right r)
    Left rr -> do
      traceEventIO ("Stopped parsing " ++ fname ++ " (failure: " ++ Megaparsec.parseErrorPretty rr ++ ")")
      pure (Left (Megaparsec.parseErrorPretty rr))
  traceEventIO ("STOP parsing " ++ fname)
  pure o


setupLogger :: Log.Priority -> IO ()
setupLogger p = do
    Log.updateGlobalLogger loggerName (Log.setLevel p)
    hs <- consoleLogHandler
    Log.updateGlobalLogger Log.rootLoggerName $ Log.setHandlers [hs]
    where
      consoleLogHandler = Log.setFormatter
                         <$> Log.streamHandler stdout Log.DEBUG
                         <*> pure (Log.simpleLogFormatter "$prio: $msg")
