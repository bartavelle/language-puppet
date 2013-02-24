module Puppet.Daemon (initDaemon) where

import Puppet.Init
import Puppet.Interpreter.Types
import Puppet.Interpreter.Catalog
import Puppet.DSL.Types
import Puppet.DSL.Loader
import Puppet.Utils

import Erb.Compute
import Control.Concurrent
import System.Posix.Files
import System.FilePath.Glob  (globDir, compile)
import Control.Monad.State
import Control.Monad.Error
import qualified System.Log.Logger as LOG
import Data.List
import Data.Either (rights, lefts)
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import Text.Parsec.Pos (initialPos)
import Puppet.Stats
import Debug.Trace
import qualified Data.Text as T

-- this daemon returns a catalog when asked for a node and facts
data DaemonMessage
    = QCatalog (T.Text, Facts, Chan DaemonMessage)
    | RCatalog (Either String (FinalCatalog, EdgeMap, FinalCatalog))

logDebug = LOG.debugM "Puppet.Daemon" . T.unpack
logInfo = LOG.infoM "Puppet.Daemon" . T.unpack
logWarning = LOG.warningM "Puppet.Daemon" . T.unpack
logError = LOG.errorM "Puppet.Daemon" . T.unpack

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

* It might be buggy when top level statements that are not class/define/nodes
are altered, or when files loaded with require are changed.

* Exported resources are supported through the PuppetDB interface.

* The catalog is not computed exactly the same way Puppet does. Take a look at
"Puppet.Interpreter.Catalog" for a list of differences.

* Parsing incompatibilities are listed in "Puppet.DSL.Parser".

* There might be race conditions because file status are checked before they
are opened. This means the program might end with an exception when the file
is not existent. This will need fixing.

-}
initDaemon :: Prefs -> IO ( T.Text -> Facts -> IO(Either String (FinalCatalog, EdgeMap, FinalCatalog)), IO StatsTable, IO StatsTable, IO StatsTable )
initDaemon prefs = do
    logDebug "initDaemon"
    traceEventIO "initDaemon"
    controlChan   <- newChan
    templateStats <- newStats
    parserStats   <- newStats
    catalogStats  <- newStats
    getstmts      <- initParserDaemon prefs parserStats
    templatefunc  <- initTemplateDaemon prefs templateStats
    replicateM_ (compilepoolsize prefs) (forkIO (master prefs controlChan getstmts templatefunc catalogStats))
    return (gCatalog controlChan, getStats parserStats, getStats catalogStats, getStats templateStats)

master :: Prefs
    -> Chan DaemonMessage
    -> (TopLevelType -> T.Text -> IO (Either String Statement))
    -> (T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text))
    -> MStats
    -> IO ()
master prefs chan getstmts gettemplate mstats = do
    message <- readChan chan
    case message of
        QCatalog (nodename, facts, respchan) -> do
            logDebug ("Received query for node " <> nodename)
            traceEventIO ("Received query for node " <> T.unpack nodename)
            (!stmts, !warnings) <- measure mstats nodename $ getCatalog getstmts gettemplate (puppetDBquery prefs) nodename facts (Just $ modules prefs) (natTypes prefs)
            traceEventIO ("getCatalog finished for " <> T.unpack nodename)
            mapM_ logWarning warnings
            case stmts of
                Left x -> writeChan respchan (RCatalog $ Left x)
                Right !x -> writeChan respchan (RCatalog $ Right x)
        _ -> logError "Bad message type for master"
    master prefs chan getstmts gettemplate mstats

gCatalog :: Chan DaemonMessage -> T.Text -> Facts -> IO (Either String (FinalCatalog, EdgeMap, FinalCatalog))
gCatalog channel nodename facts = do
    respchan <- newChan
    writeChan channel $ QCatalog (nodename, facts, respchan)
    response <- readChan respchan
    case response of
        RCatalog x -> return x
        _ -> return $ Left "Bad answer from the master"

-- this daemon returns a list of statements when asked for a top class/define name
data ParserMessage
    = QStatement (TopLevelType, T.Text, Chan ParserMessage)
    | RStatement (Either String Statement)

initParserDaemon :: Prefs -> MStats -> IO
    ( TopLevelType -> T.Text -> IO (Either String Statement)
    )
initParserDaemon prefs mstats = do
    logDebug "initParserDaemon"
    controlChan <- newChan
    getparsed <- initParsedDaemon prefs mstats
    replicateM_ (parsepoolsize prefs) (forkIO (pmaster prefs controlChan getparsed))
    return (getStatements controlChan)

-- extracts data from a filestatus
extractFStatus fs = (deviceID fs, fileID fs, modificationTime fs, fileSize fs)

getFileInfo :: FilePath -> IO (Maybe FileStatus)
getFileInfo fpath = do
    fexists <- fileExist fpath
    if fexists
        then do
            stat <- getFileStatus fpath
            return $ Just stat
        else return Nothing

getFirstFileInfo :: Maybe (FilePath, FileStatus) -> FilePath -> IO (Maybe (FilePath, FileStatus))
getFirstFileInfo (Just x) _ = return $ Just x
getFirstFileInfo Nothing  y = do
    stat <- getFileInfo y
    case stat of
        Just x ->  return $ Just (y, x)
        Nothing -> return Nothing

-- checks whether data pointed but the filepath has the corresponding file status
checkFileInfo :: FilePath -> FileStatus -> ErrorT String IO Bool
checkFileInfo fpath fstatus = do
    stat <- liftIO $ getFileInfo fpath
    case stat of
        Just nfstatus -> return (extractFStatus nfstatus == extractFStatus fstatus)
        Nothing -> return False

compilefilelist :: Prefs -> TopLevelType -> T.Text -> [T.Text]
compilefilelist prefs TopNode _ = [manifest prefs <> "/site.pp"]
compilefilelist prefs _ name = moduleInfo
    where
        moduleInfo | length nameparts == 1 = [modules prefs <> "/" <> name <> "/manifests/init.pp"]
                   | null nameparts = ["no name parts, error in compilefilelist"]
                   | otherwise = [modules prefs <> "/" <> head nameparts <> "/manifests/" <> T.intercalate "/" (tail nameparts) <> ".pp"]
        nameparts = T.splitOn "::" name :: [T.Text]

findFile :: Prefs -> TopLevelType -> T.Text -> ErrorT String IO (FilePath, FileStatus)
findFile prefs qtype resname = do
    let filelist = map T.unpack $ compilefilelist prefs qtype resname
    fileinfo <- liftIO $ foldlM getFirstFileInfo Nothing filelist
    case fileinfo of
        Just x -> return x
        Nothing -> throwError ("Could not find file for " ++ show qtype ++ " " ++ T.unpack resname ++ " when looking in " ++ show filelist)

globImport :: FilePath -> Statement -> ErrorT String IO [FilePath]
globImport origfile (Import timportname ipos) = do
    let importname = T.unpack timportname
    importedfiles <- liftM (concat . fst) (liftIO $ globDir [compile importname] (T.unpack $ takeDirectory $ T.pack origfile))
    if null importedfiles
        then throwError $ "Could not import " ++ importname ++ " at " ++ show ipos
        else return importedfiles
globImport _ x = throwError $ "Should not run globImport on " ++ show x

{-
 given a filename and a file status, will parse this file and update the cache with the parsed values

 it must also parse all required files and store everything about them before returning
 -}
loadUpdateFile :: FilePath -> FileStatus -> (TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse ) -> ErrorT String IO ([Statement], [(TopLevelType, T.Text, Statement)])
loadUpdateFile fname fstatus updatepinfo = do
    let tfname = T.pack fname
    liftIO $ logDebug ("Loading file " <> tfname)
    parsed <- parseFile fname
    let toplevels = map convertTopLevel parsed
        oktoplevels = rights toplevels
        othertoplevels = lefts toplevels
        (imports, spurioustoplevels) = partition isImport othertoplevels
        isImport (Import _ _) = True
        isImport _ = False
    relatedtops <- liftM (map T.pack . concat) (mapM (globImport fname) imports)
        -- save this spurious top levels
    unless (null spurioustoplevels) $ do
            liftIO $ void ( updatepinfo TopSpurious tfname (ClassDeclaration "::" Nothing [] spurioustoplevels (initialPos fname), tfname, fstatus, relatedtops) )
            liftIO $ logWarning ("Spurious top level statement in file " <> tfname <> ", expect bugs in case you modify them" )
    -- saves all good top levels
    liftIO $ mapM (\(rtype, resname, resstatement) -> updatepinfo rtype resname (resstatement, tfname, fstatus, relatedtops)) oktoplevels
    return (imports, oktoplevels)

reparseStatements :: Prefs -> (TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse ) -> TopLevelType -> T.Text -> ErrorT String IO Statement
reparseStatements prefs updatepinfo qtype nodename = do
    (fname, fstatus) <- findFile prefs qtype nodename
    reparseFile fname fstatus updatepinfo qtype nodename

reparseFile :: FilePath -> FileStatus -> (TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse ) -> TopLevelType -> T.Text -> ErrorT String IO Statement
reparseFile fname fstatus updatepinfo qtype nodename = do
    (imports, oktoplevels) <- loadUpdateFile fname fstatus updatepinfo
    imported <- liftM concat (mapM (loadImport updatepinfo fname) imports)
    let searchstatement = find (\(qt,nm,_) -> (qt == qtype) && (nm == nodename)) (oktoplevels ++ imported)
    case searchstatement of
        Just (_,_,x) -> return x
        Nothing -> throwError ("Could not find correct top level statement for " ++ show qtype ++ " " ++ T.unpack nodename)

{- Given a base file name and an import statement, will load these imports.
This is not as obvious as it seems because you need to load imported's imports.
-}
loadImport :: (TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse ) -> FilePath -> Statement -> ErrorT String IO [(TopLevelType, T.Text, Statement)]
loadImport updatepinfo fdir mimport = do
    matched <- globImport fdir mimport
    fileinfos <- liftIO $ mapM getFileInfo matched
    let fpathinfos = zip matched fileinfos
        goodpathinfos = concatMap unjust fpathinfos
        unjust (a, Just b) = [(a,b)]
        unjust _ = []
    ex <- (mapM (\(fname, finfo) -> liftM (\x -> (fname, x)) $ loadUpdateFile fname finfo updatepinfo) goodpathinfos)
    -- This is a complicated and very expensive way to load imports that were imported.
    -- The only way to make it faster would be to query the cache before reloading the files ..
    let reloaded = concatMap (snd . snd) ex
        limports = map (\(fname, (mimports, _)) -> (fname, mimports)) ex
    mapM_ (\(fname, mimports) -> mapM_ (\stmt -> loadImport updatepinfo fname stmt) mimports) limports
    return $ reloaded

loadRelated ::
       ( TopLevelType -> T.Text -> ErrorT String IO (Maybe CacheEntry) )
    -> T.Text
    -> ErrorT String IO [(T.Text, Statement)]
loadRelated getpinfo filename = do
    res <- getpinfo TopSpurious filename
    case res of
        Nothing -> return []
        Just (stmts, _, _, related) -> do
            relstatements <- liftM concat (mapM (loadRelated getpinfo) related)
            return $ (filename,stmts):relstatements

handlePRequest :: Prefs ->
    ( TopLevelType -> T.Text -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse
    , T.Text -> IO ParsedCacheResponse
    ) -> TopLevelType -> T.Text -> ErrorT String IO Statement
handlePRequest prefs (getpinfo, updatepinfo, invalidateinfo) qtype nodename = do
    res <- getpinfo qtype nodename
    case res of
        Just (stmts, tfpath, fstatus, related) -> do
            -- for this to work, everything must be cached
            -- this is buggy as the required stuff will not be invalidated properly
            let fpath = T.unpack tfpath
            relstatements <- liftM concat (mapM (loadRelated getpinfo) (tfpath:related))
            isfileinfoaccurate <- checkFileInfo fpath fstatus
            statements <- if isfileinfoaccurate
                            then return stmts
                            else do
                                liftIO $ invalidateinfo tfpath
                                finfo <- liftIO $ getFileInfo fpath
                                case finfo of
                                    Just fstat -> reparseFile fpath fstat updatepinfo qtype nodename
                                    Nothing    -> reparseStatements prefs updatepinfo qtype nodename
            if null relstatements
                then return statements
                else return (TopContainer relstatements statements)
        Nothing -> reparseStatements prefs updatepinfo qtype nodename >> handlePRequest prefs (getpinfo, updatepinfo, invalidateinfo) qtype nodename

pmaster :: Prefs -> Chan ParserMessage ->
    ( TopLevelType -> T.Text -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse
    , T.Text -> IO ParsedCacheResponse
    ) -> IO ()
pmaster prefs chan cachefuncs = do
    pmessage <- readChan chan
    case pmessage of
        QStatement (qtype, name, respchan) -> do
            out <- runErrorT $ handlePRequest prefs cachefuncs qtype name
            case out of
                Left  x -> writeChan respchan $ RStatement $ Left  x
                Right y -> writeChan respchan $ RStatement $ Right y
        _ -> logError "Bad message type received by Puppet.Daemon.pmaster"
    pmaster prefs chan cachefuncs

getStatements :: Chan ParserMessage -> TopLevelType -> T.Text -> IO (Either String Statement)
getStatements channel qtype classname = do
    respchan <- newChan
    writeChan channel $ QStatement (qtype, classname, respchan)
    response <- readChan respchan
    case response of
        RStatement x -> return x
        _            -> return $ Left "Bad answer from the pmaster"

-- this is a cache entry, it stores a top level statement, the path of the corresponding file and its status
-- it also stores a list of top level statements that are related
type CacheEntry = (Statement, T.Text, FileStatus, [T.Text])
data ParsedCacheQuery
    = GetParsedData TopLevelType T.Text (Chan ParsedCacheResponse)
    | UpdateParsedData TopLevelType T.Text CacheEntry (Chan ParsedCacheResponse)
    | InvalidateCacheFile T.Text (Chan ParsedCacheResponse)
data ParsedCacheResponse
    = CacheError T.Text
    | RCacheEntry CacheEntry
    | NoCacheEntry
    | CacheUpdated

-- this one is singleton, and is used to cache de parsed values, along with the file names they depend on
initParsedDaemon :: Prefs -> MStats -> IO
    ( TopLevelType -> T.Text -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse
    , T.Text -> IO ParsedCacheResponse
    )
initParsedDaemon prefs mstats = do
    logDebug "initParsedDaemon"
    controlChan <- newChan
    forkIO ( evalStateT (parsedmaster prefs controlChan mstats) (Map.empty, Map.empty) )
    return (getParsedInformation controlChan, updateParsedInformation controlChan, invalidateCachedFile controlChan)

getParsedInformation :: Chan ParsedCacheQuery -> TopLevelType -> T.Text -> ErrorT String IO (Maybe CacheEntry)
getParsedInformation cchan qtype name = do
    respchan <- liftIO newChan
    liftIO $ writeChan cchan $ GetParsedData qtype name respchan
    out <- liftIO $ readChan respchan
    case out of
        RCacheEntry x -> return $ Just x
        NoCacheEntry  -> return Nothing
        CacheError x  -> throwError (T.unpack x)
        _             -> throwError "Unknown cache response type"

updateParsedInformation :: Chan ParsedCacheQuery -> TopLevelType -> T.Text -> CacheEntry -> IO ParsedCacheResponse
updateParsedInformation pchannel qtype name centry = do
    respchan <- newChan
    writeChan pchannel $ UpdateParsedData qtype name centry respchan
    readChan respchan

invalidateCachedFile :: Chan ParsedCacheQuery -> T.Text -> IO ParsedCacheResponse
invalidateCachedFile pchannel name = do
    respchan <- newChan
    writeChan pchannel $ InvalidateCacheFile name respchan
    readChan respchan

-- state : (parsed statements map, file association map, nbrequests)
parsedmaster :: Prefs -> Chan ParsedCacheQuery -> MStats -> StateT 
    ( Map.Map (TopLevelType, T.Text) CacheEntry
    , Map.Map T.Text (FileStatus, [(TopLevelType, T.Text)])
    ) IO ()
parsedmaster prefs controlchan mstats = do
    curmsg <- liftIO $ readChan controlchan
    case curmsg of
        GetParsedData qtype name respchan -> do
            (curmap, _) <- get
            case Map.lookup (qtype, name) curmap of
                Just x  -> liftIO $ measure mstats "hit" (writeChan respchan (RCacheEntry x))
                Nothing -> liftIO $ measure mstats "miss" (writeChan respchan NoCacheEntry)
        UpdateParsedData qtype name val@(_, filepath, filestatus, _) respchan -> do
            liftIO $ logInfo ("Updating parsed cache for " <> tshow qtype <> " " <> name)
            (mp, fm) <- get
            let
                -- retrieve the current status
                curstatus       = Map.lookup filepath fm
                fmclean         = case curstatus of
                                    Just (cs,_) -> if extractFStatus cs /= extractFStatus filestatus
                                                    then Map.delete filepath fm
                                                    else fm
                                    _       -> fm
                addsnd (a1, b1) (_, b2) = (a1, b1 ++ b2)
                fileassocmap    = Map.insertWith addsnd filepath (filestatus, [(qtype, name)]) fmclean
                statementmap    = Map.insert (qtype, name) val mp
            put (statementmap, fileassocmap)
            liftIO $ measure mstats "update" (writeChan respchan CacheUpdated)
        InvalidateCacheFile fname respchan -> do
            liftIO $ logDebug $ "Invalidating files for " <> fname
            (mp, fm) <- get
            let
                nfm = Map.delete fname fm
                nmp = case Map.lookup fname fm of
                    Just (_, remlist) -> foldl' (flip Map.delete) mp remlist
                    Nothing           -> mp
            put (nmp, nfm)
            liftIO $ measure mstats "invalidate" (writeChan respchan CacheUpdated)
    parsedmaster prefs controlchan mstats
