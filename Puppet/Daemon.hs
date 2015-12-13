{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Puppet.Daemon (
    Daemon(..)
  , initDaemon
  -- * Utils
  , checkError
  -- * Re-exports
  , module Puppet.Interpreter.Types
  , module Puppet.PP
) where

import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens              hiding (Strict)
import qualified Data.Either.Strict        as S
import           Data.FileCache            as FileCache
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Tuple.Strict
import qualified Data.Vector               as V
import           Debug.Trace               (traceEventIO)
import           Foreign.Ruby.Safe
import           System.Exit               (exitFailure)
import           System.IO                 (stdout)
import qualified System.Log.Formatter      as LOG (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import qualified System.Log.Handler.Simple as LOG (streamHandler)
import qualified System.Log.Logger         as LOG

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
import           Puppet.Plugins
import           Puppet.PP
import           Puppet.Preferences
import           Puppet.Stats
import           Puppet.Utils

{-| API for the Daemon.
The main method is `getCatalog`: given a node and a list of facts, it returns the result of the compilation.
This will be either an error, or a tuple containing:
- all the resources in this catalog
- the dependency map
- the exported resources
- a list of known resources, that might not be up to date, but are here for code coverage tests.

Notes :

* It might be buggy when top level statements that are not class\/define\/nodes
are altered, or when files loaded with require are changed.
* The catalog is not computed exactly the same way Puppet does. Some good practices are enforced, particularly in strict mode.
For instance, unknown variables are always an error. Querying a dictionary with a non existent key returns undef in puppet, whereas it would throw an error in strict mode.
-}
data Daemon = Daemon
    { getCatalog    :: NodeName -> Facts -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
    , parserStats   :: MStats
    , catalogStats  :: MStats
    , templateStats :: MStats
    }

{-| Entry point to get a Daemon
It will initialize the parsing and interpretation infrastructure from the 'Preferences'.

Internally it initializes a thread for the LUA interpreter, and a thread for the Ruby one.
It should cache the AST of every .pp file, and could use a bit of memory. As a comparison, it
fits in 60 MB with the author's manifests, but really breathes when given 300 MB
of heap space. In this configuration, even if it spawns a ruby process for every
template evaluation, it is way faster than the puppet stack.

It can optionally talk with PuppetDB, by setting an URL via the 'prefPDB'.
The recommended way to set it to http://localhost:8080 and set a SSH tunnel :

> ssh -L 8080:localhost:8080 puppet.host
-}
initDaemon :: Preferences IO -> IO Daemon
initDaemon pref0 = do
    setupLogger (pref0 ^. prefLogLevel)
    logDebug "initDaemon"
    traceEventIO "initDaemon"
    luacontainer <- initLuaMaster (T.pack (pref0 ^. prefPuppetPaths.modulesPath))
    let pref = pref0 & prefExtFuncs %~ HM.union luacontainer
    hquery <- case pref ^. prefHieraPath of
                  Just p  -> either error id <$> startHiera p
                  Nothing -> return dummyHiera
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


-- | In case of a Left value, print the error and exit immediately
checkError :: Show e => Doc -> Either e a -> IO a
checkError desc = either exit return
    where
      exit = \err -> putDoc (display err) >> exitFailure
      display err = red desc <> ": " <+> (string . show) err


-- Internal functions

getCatalog' :: Preferences IO
         -> ( TopLevelType -> T.Text -> IO (S.Either PrettyError Statement) )
         -> (Either T.Text T.Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError T.Text))
         -> MStats
         -> HieraQueryFunc IO
         -> NodeName
         -> Facts
         -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
getCatalog' pref parsingfunc getTemplate stats hquery node facts = do
    logDebug ("Received query for node " <> node)
    traceEventIO ("START getCatalog' " <> T.unpack node)
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
                                              )
                                              node
                                              facts
                                              (pref ^. prefPuppetSettings)
    (stmts :!: warnings) <- measure stats node catalogComputation
    mapM_ (\(p :!: m) -> LOG.logM daemonLoggerName p (displayS (renderCompact (ttext node <> ":" <+> m)) "")) warnings
    traceEventIO ("STOP getCatalog' " <> T.unpack node)
    if pref ^. prefExtraTests
       then runOptionalTests stmts
       else return stmts
    where
      runOptionalTests stm = case stm ^? S._Right._1 of
          Nothing  -> return stm
          (Just c) -> catching _PrettyError
                              (do {testCatalog pref c; return stm})
                              (return . S.Left)

-- | Return an HOF that would parse the file associated with a toplevel.
-- The toplevel is defined by the tuple (type, name)
-- The result of the parsing is a single Statement (which recursively contains others statements)
parseFunc :: PuppetDirPaths -> FileCache (V.Vector Statement) -> MStats -> TopLevelType -> T.Text -> IO (S.Either PrettyError Statement)
parseFunc ppath filecache stats = \toptype topname ->
    let nameparts = T.splitOn "::" topname in
    let topLevelFilePath :: TopLevelType -> T.Text -> Either PrettyError T.Text
        topLevelFilePath TopNode _ = Right $ T.pack (ppath^.manifestPath <> "/site.pp")
        topLevelFilePath  _ name
            | length nameparts == 1 = Right $ T.pack (ppath^.modulesPath) <> "/" <> name <> "/manifests/init.pp"
            | null nameparts        = Left $ PrettyError ("Invalid toplevel" <+> squotes (ttext name))
            | otherwise             = Right $ T.pack (ppath^.modulesPath) <> "/" <> head nameparts <> "/manifests/" <> T.intercalate "/" (tail nameparts) <> ".pp"
    in
    case topLevelFilePath toptype topname of
        Left rr     -> return (S.Left rr)
        Right fname -> do
            let sfname = T.unpack fname
                handleFailure :: SomeException -> IO (S.Either String (V.Vector Statement))
                handleFailure e = return (S.Left (show e))
            x <- measure stats fname (FileCache.query filecache sfname (parseFile sfname `catch` handleFailure))
            case x of
                S.Right stmts -> filterStatements toptype topname stmts
                S.Left rr -> return (S.Left (PrettyError (red (text rr))))


parseFile :: FilePath -> IO (S.Either String (V.Vector Statement))
parseFile fname = do
    traceEventIO ("START parsing " ++ fname)
    cnt <- T.readFile fname
    o <- case runPParser fname cnt of
        Right r -> traceEventIO ("Stopped parsing " ++ fname) >> return (S.Right r)
        Left rr -> traceEventIO ("Stopped parsing " ++ fname ++ " (failure: " ++ show rr ++ ")") >> return (S.Left (show rr))
    traceEventIO ("STOP parsing " ++ fname)
    return o

daemonLoggerName :: String
daemonLoggerName = "Puppet.Daemon"

logDebug :: T.Text -> IO ()
logDebug   = LOG.debugM   daemonLoggerName . T.unpack

setupLogger :: LOG.Priority -> IO ()
setupLogger p = do
    LOG.updateGlobalLogger daemonLoggerName (LOG.setLevel p)
    LOG.updateGlobalLogger hieraLoggerName (LOG.setLevel p)
    hs <- consoleLogHandler
    LOG.updateGlobalLogger LOG.rootLoggerName $ LOG.setHandlers [hs]
    where
      consoleLogHandler = setFormatter
                         <$> LOG.streamHandler stdout LOG.DEBUG
                         <*> pure (LOG.simpleLogFormatter "$prio: $msg")
