{-# LANGUAGE LambdaCase, CPP #-}
module Puppet.Daemon (initDaemon, logDebug, logInfo, logWarning, logError) where

import Puppet.Parser
import Puppet.Utils
import Puppet.Preferences
import Puppet.Stats
import Puppet.Interpreter.Types
import Puppet.Parser.Types
import Puppet.Manifests
import Puppet.Interpreter
import Puppet.Plugins
import Hiera.Server
import Erb.Compute

import Puppet.PP
import Text.Parsec
import Data.FileCache
import qualified System.Log.Logger as LOG
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Debug.Trace
import Control.Lens
import Control.Monad
import Control.Concurrent
import qualified Data.Either.Strict as S
import Data.Tuple.Strict
import Control.Exception

#ifdef HRUBY
import Foreign.Ruby.Safe
#endif

loggerName :: String
loggerName = "Puppet.Daemon"

logDebug :: T.Text -> IO ()
logDebug   = LOG.debugM   loggerName . T.unpack
logInfo :: T.Text -> IO ()
logInfo    = LOG.infoM    loggerName . T.unpack
logWarning :: T.Text -> IO ()
logWarning = LOG.warningM loggerName . T.unpack
logError :: T.Text -> IO ()
logError   = LOG.errorM   loggerName . T.unpack

{-| This is a high level function, that will initialize the parsing and
interpretation infrastructure from the 'Prefs' structure, and will return a
function that will take a node name, 'Facts' and return either an error or the
'FinalCatalog', along with the dependency graph and catalog of exported resources. It also return a few IO
functions that can be used in order to query the daemon for statistics,
following the format in "Puppet.Stats".

It will internaly initialize several threads that communicate with channels. It
should scale well, althrough it hasn't really been tested yet. It should cache
the ASL of every .pp file, and could use a bit of memory. As a comparison, it
fits in 60 MB with the author's manifests, but really breathes when given 300 MB
of heap space. In this configuration, even if it spawns a ruby process for every
template evaluation, it is way faster than the puppet stack.

It is recommended to ask for as many parser and interpreter threads as there are
CPUs.

It can optionnaly talk with PuppetDB, by setting an URL in the 'Prefs' data
structure. The recommended way to set it to http://localhost:8080 and set a SSH
tunnel :

> ssh -L 8080:localhost:8080 puppet.host

Known bugs :

* It might be buggy when top level statements that are not class\/define\/nodes
are altered, or when files loaded with require are changed.

* Exported resources are supported through the PuppetDB interface.

* The catalog is not computed exactly the same way Puppet does. Take a look at
"Puppet.Interpreter.Catalog" for a list of differences.

* Parsing incompatibilities are listed in "Puppet.DSL.Parser".

* There might be race conditions because file status are checked before they
are opened. This means the program might end with an exception when the file
is not existent. This will need fixing.

-}
initDaemon :: Preferences -> IO DaemonMethods
initDaemon prefs = do
    logDebug "initDaemon"
    traceEventIO "initDaemon"
    controlChan   <- newChan
    templateStats <- newStats
    parserStats   <- newStats
    catalogStats  <- newStats
    pfilecache    <- newFileCache
    let getStatements = parseFunction prefs pfilecache parserStats
#ifdef HRUBY
    intr          <- startRubyInterpreter
    getTemplate   <- initTemplateDaemon intr prefs templateStats
#else
    getTemplate   <- initTemplateDaemon prefs templateStats
#endif
    hquery        <- case prefs ^. hieraPath of
                         Just p -> startHiera p >>= \case
                            Left _ -> return dummyHiera
                            Right x -> return x
                         Nothing -> return dummyHiera
    let runMaster = do
            (luastate, luafunctions) <- initLua (T.pack (prefs ^. modulesPath))
            let luacontainer = HM.fromList [ (fname, puppetFunc luastate fname) | fname <- luafunctions ]
                myprefs = prefs & prefExtFuncs %~ HM.union luacontainer
            master myprefs controlChan getStatements getTemplate catalogStats hquery
    replicateM_ (prefs ^. compilePoolSize) (forkIO runMaster)
    return (DaemonMethods (gCatalog controlChan) parserStats catalogStats templateStats)

gCatalog :: Chan DaemonQuery -> T.Text -> Facts -> IO (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog))
gCatalog q ndename fcts = do
    t <- newEmptyMVar
    writeChan q (DaemonQuery ndename fcts t)
    readMVar t

data DaemonQuery = DaemonQuery
    { _qNodeName :: T.Text
    , _qFacts    :: Facts
    , _qQ        :: MVar DaemonResponse
    }

type DaemonResponse = S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog)

master :: Preferences
       -> Chan DaemonQuery
       -> ( TopLevelType -> T.Text -> IO (S.Either Doc Statement) )
       -> (Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text))
       -> MStats
       -> HieraQueryFunc
       -> IO ()
master prefs controlQ getStatements getTemplate stats hquery = forever $ do
    (DaemonQuery ndename facts q) <- readChan controlQ
    logDebug ("Received query for node " <> ndename)
    traceEventIO ("Received query for node " <> T.unpack ndename)
    (stmts :!: warnings) <- measure stats ndename $ getCatalog getStatements getTemplate (prefs ^. prefPDB) ndename facts (prefs ^. natTypes) (prefs ^. prefExtFuncs) hquery
    mapM_ (\(p :!: m) -> LOG.logM loggerName p (displayS (renderCompact m) "")) warnings
    traceEventIO ("getCatalog finished for " <> T.unpack ndename)
    putMVar q stmts

parseFunction :: Preferences -> FileCache (V.Vector Statement) -> MStats -> TopLevelType -> T.Text -> IO (S.Either Doc Statement)
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
                S.Left rr -> return (S.Left (red (text rr)))

-- TODO this is wrong, see
-- http://docs.puppetlabs.com/puppet/3/reference/lang_namespaces.html#behavior
compileFileList :: Preferences -> TopLevelType -> T.Text -> S.Either Doc T.Text
compileFileList prefs TopNode _ = S.Right (T.pack (prefs ^. manifestPath) <> "/site.pp")
compileFileList prefs _ name = moduleInfo
    where
        moduleInfo | length nameparts == 1 = S.Right (mpath <> "/" <> name <> "/manifests/init.pp")
                   | null nameparts = S.Left "no name parts, error in compilefilelist"
                   | otherwise = S.Right (mpath <> "/" <> head nameparts <> "/manifests/" <> T.intercalate "/" (tail nameparts) <> ".pp")
        mpath = T.pack (prefs ^. modulesPath)
        nameparts = T.splitOn "::" name

parseFile :: FilePath -> IO (S.Either String (V.Vector Statement))
parseFile fname = do
    traceEventIO ("Start parsing " ++ fname)
    cnt <- T.readFile fname
    runParserT puppetParser () fname cnt >>= \case
        Right r -> traceEventIO ("Stopped parsing " ++ fname) >> return (S.Right r)
        Left rr -> traceEventIO ("Stopped parsing " ++ fname ++ " (failure: " ++ show rr ++ ")") >> return (S.Left (show rr))

