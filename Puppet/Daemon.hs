{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Puppet.Daemon (
    initDaemon
  , checkError
  -- * Re-exports
  , module Puppet.Interpreter.Types
  , module Puppet.PP
) where

import           Control.Exception
import           Control.Exception.Lens
import qualified Control.Lens              as L
import           Control.Lens.Operators
import qualified Data.Either.Strict        as S
import           Data.FileCache
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

{-| This is a high level function, that will initialize the parsing and
interpretation infrastructure from the 'Preferences', and will return 'DaemonMethods'.
From there, you have access to 'getCatalog', a function that take a node name,
and the 'Facts' to return the result of the catalog computation. 'DaemonMethods' also returns
a few IO functions that can be used to query for statistics (see "Puppet.Stats").

It will internaly initialize a thread for the LUA interpreter, and a thread for the Ruby one.
It should cache the AST of every .pp file, and could use a bit of memory. As a comparison, it
fits in 60 MB with the author's manifests, but really breathes when given 300 MB
of heap space. In this configuration, even if it spawns a ruby process for every
template evaluation, it is way faster than the puppet stack.

It can optionnaly talk with PuppetDB, by setting an URL via the 'prefPDB'.
The recommended way to set it to http://localhost:8080 and set a SSH tunnel :

> ssh -L 8080:localhost:8080 puppet.host

Canveats :

* It might be buggy when top level statements that are not class\/define\/nodes
are altered, or when files loaded with require are changed.

* The catalog is not computed exactly the same way Puppet does. Some good practices are enforced, particularly in strict mode.
For instance, unknown variables are always an error. Querying a dictionary with a non existent key returns undef in puppet, whereas it would throw an error in strict mode.

-}
initDaemon :: Preferences IO -> IO DaemonMethods
initDaemon prefs = do
    setupLogger (prefs ^. prefLogLevel)
    logDebug "initDaemon"
    traceEventIO "initDaemon"
    templateStats <- newStats
    parserStats   <- newStats
    catalogStats  <- newStats
    pfilecache    <- newFileCache
    intr          <- startRubyInterpreter
    hquery        <- case prefs ^. prefHieraPath of
                         Just p  -> either error id <$> startHiera p
                         Nothing -> return dummyHiera
    luacontainer <- initLuaMaster (T.pack (prefs ^. prefPuppetPaths.modulesPath))
    let myprefs = prefs & prefExtFuncs %~ HM.union luacontainer
        getStatements = parseFunction myprefs pfilecache parserStats
    getTemplate <- initTemplateDaemon intr myprefs templateStats
    return (DaemonMethods (gCatalog myprefs getStatements getTemplate catalogStats hquery) parserStats catalogStats templateStats)


-- Public utils func to work with the daemon --

-- | In case of a Left value, print the error and exit immediately
checkError :: Show e => Doc -> Either e a -> IO a
checkError desc = either exit return
    where
      exit = \err -> putDoc (display err) >> exitFailure
      display err = red desc <> ": " <+> (string . show) err


-- Some utils func internal to this module --

gCatalog :: Preferences IO
         -> ( TopLevelType -> T.Text -> IO (S.Either PrettyError Statement) )
         -> (Either T.Text T.Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError T.Text))
         -> MStats
         -> HieraQueryFunc IO
         -> Nodename
         -> Facts
         -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
gCatalog prefs getStatements getTemplate stats hquery node facts = do
    logDebug ("Received query for node " <> node)
    traceEventIO ("START gCatalog " <> T.unpack node)
    let catalogComputation = getCatalog (InterpreterReader
                                            (prefs ^. prefNatTypes)
                                            getStatements
                                            getTemplate
                                            (prefs ^. prefPDB)
                                            (prefs ^. prefExtFuncs)
                                            node
                                            hquery
                                            defaultImpureMethods
                                            (prefs ^. prefIgnoredmodules)
                                            (prefs ^. prefExternalmodules)
                                            (prefs ^. prefStrictness == Strict)
                                            (prefs ^. prefPuppetPaths)
                                        )
                                        node
                                        facts
                                        (prefs ^. prefPuppetSettings)
    (stmts :!: warnings) <- measure stats node catalogComputation
    mapM_ (\(p :!: m) -> LOG.logM daemonLoggerName p (displayS (renderCompact (ttext node <> ":" <+> m)) "")) warnings
    traceEventIO ("STOP gCatalog " <> T.unpack node)
    if prefs ^. prefExtraTests
       then runOptionalTests stmts
       else return stmts
    where
      runOptionalTests stm = case stm^?S._Right.L._1 of
        Nothing -> return stm
        (Just c)  -> catching _PrettyError
                              (do {testCatalog prefs c; return stm})
                              (return . S.Left)

parseFunction :: Preferences IO -> FileCache (V.Vector Statement) -> MStats -> TopLevelType -> T.Text -> IO (S.Either PrettyError Statement)
parseFunction prefs filecache stats topleveltype toplevelname =
    case compileFileList prefs topleveltype toplevelname of
        S.Left rr -> return (S.Left rr)
        S.Right fname -> do
            let sfname = T.unpack fname
                handleFailure :: SomeException -> IO (S.Either String (V.Vector Statement))
                handleFailure e = return (S.Left (show e))
            x <- measure stats fname (query filecache sfname (parseFile sfname `catch` handleFailure))
            case x of
                S.Right stmts -> filterStatements topleveltype toplevelname stmts
                S.Left rr -> return (S.Left (PrettyError (red (text rr))))

-- TODO this is wrong, see
-- http://docs.puppetlabs.com/puppet/3/reference/lang_namespaces.html#behavior
compileFileList :: Preferences IO -> TopLevelType -> T.Text -> S.Either PrettyError T.Text
compileFileList prefs TopNode _ = S.Right (T.pack (prefs ^. prefPuppetPaths.manifestPath) <> "/site.pp")
compileFileList prefs _ name = moduleInfo
    where
        moduleInfo | length nameparts == 1 = S.Right (mpath <> "/" <> name <> "/manifests/init.pp")
                   | null nameparts = S.Left "no name parts, error in compilefilelist"
                   | otherwise = S.Right (mpath <> "/" <> head nameparts <> "/manifests/" <> T.intercalate "/" (tail nameparts) <> ".pp")
        mpath = T.pack (prefs ^. prefPuppetPaths.modulesPath)
        nameparts = T.splitOn "::" name

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
