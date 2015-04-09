{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Puppet.Daemon (initDaemon) where

import           Control.Applicative
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import qualified Data.Either.Strict       as S
import           Data.FileCache
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Tuple.Strict
import qualified Data.Vector              as V
import           Debug.Trace
import           Erb.Compute
import           Foreign.Ruby.Safe
import           Prelude

import           Hiera.Server
import           Puppet.Interpreter
import           Puppet.Interpreter.IO
import           Puppet.Interpreter.Types
import           Puppet.Lens              (_PrettyError)
import           Puppet.Manifests
import           Puppet.OptionalTests
import           Puppet.Parser
import           Puppet.Parser.Types
import           Puppet.Plugins
import           Puppet.PP
import           Puppet.Preferences
import           Puppet.Stats
import           Puppet.Utils
import qualified System.Log.Logger        as LOG

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
    logDebug "initDaemon"
    traceEventIO "initDaemon"
    templateStats <- newStats
    parserStats   <- newStats
    catalogStats  <- newStats
    pfilecache    <- newFileCache
    let getStatements = parseFunction prefs pfilecache parserStats
    intr          <- startRubyInterpreter
    getTemplate   <- initTemplateDaemon intr prefs templateStats
    hquery        <- case prefs ^. hieraPath of
                         Just p  -> either error id <$> startHiera p
                         Nothing -> return dummyHiera
    luacontainer <- initLuaMaster (T.pack (prefs ^. puppetPaths.modulesPath))
    let myprefs = prefs & prefExtFuncs %~ HM.union luacontainer
    return (DaemonMethods (gCatalog myprefs getStatements getTemplate catalogStats hquery) parserStats catalogStats templateStats)

gCatalog :: Preferences IO
         -> ( TopLevelType -> T.Text -> IO (S.Either PrettyError Statement) )
         -> (Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either PrettyError T.Text))
         -> MStats
         -> HieraQueryFunc IO
         -> Nodename
         -> Facts
         -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
gCatalog prefs getStatements getTemplate stats hquery ndename facts = do
    logDebug ("Received query for node " <> ndename)
    traceEventIO ("START gCatalog " <> T.unpack ndename)
    let catalogComputation = getCatalog (InterpreterReader
                                            (prefs ^. natTypes)
                                            getStatements
                                            getTemplate
                                            (prefs ^. prefPDB)
                                            (prefs ^. prefExtFuncs)
                                            ndename
                                            hquery
                                            defaultImpureMethods
                                            (prefs ^. ignoredmodules)
                                            (prefs ^. strictness == Strict))
                                        ndename
                                        facts
    (stmts :!: warnings) <- measure stats ndename catalogComputation
    mapM_ (\(p :!: m) -> LOG.logM loggerName p (displayS (renderCompact (ttext ndename <> ":" <+> m)) "")) warnings
    traceEventIO ("STOP gCatalog " <> T.unpack ndename)
    if prefs ^. extraTests
       then runOptionalTests stmts
       else return stmts
    where
      runOptionalTests stm = case stm^?S._Right._1 of
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
compileFileList prefs TopNode _ = S.Right (T.pack (prefs ^. puppetPaths.manifestPath) <> "/site.pp")
compileFileList prefs _ name = moduleInfo
    where
        moduleInfo | length nameparts == 1 = S.Right (mpath <> "/" <> name <> "/manifests/init.pp")
                   | null nameparts = S.Left "no name parts, error in compilefilelist"
                   | otherwise = S.Right (mpath <> "/" <> head nameparts <> "/manifests/" <> T.intercalate "/" (tail nameparts) <> ".pp")
        mpath = T.pack (prefs ^. puppetPaths.modulesPath)
        nameparts = T.splitOn "::" name

parseFile :: FilePath -> IO (S.Either String (V.Vector Statement))
parseFile fname = do
    traceEventIO ("START parsing " ++ fname)
    cnt <- T.readFile fname
    o <- case runPParser puppetParser fname cnt of
        Right r -> traceEventIO ("Stopped parsing " ++ fname) >> return (S.Right r)
        Left rr -> traceEventIO ("Stopped parsing " ++ fname ++ " (failure: " ++ show rr ++ ")") >> return (S.Left (show rr))
    traceEventIO ("STOP parsing " ++ fname)
    return o


-- Some utils func internal to this module
loggerName :: String
loggerName = "Puppet.Daemon"

logDebug :: T.Text -> IO ()
logDebug   = LOG.debugM   loggerName . T.unpack
