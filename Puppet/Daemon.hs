module Puppet.Daemon (initDaemon) where

import Puppet.Init
import Puppet.Interpreter.Types
import Puppet.Interpreter.Catalog
import Puppet.DSL.Types
import Puppet.DSL.Loader
import Control.Concurrent
import Control.Concurrent.Chan
import System.Posix.Files
import System.FilePath.Glob  (globDir, compile)
import System.FilePath.Posix (takeDirectory)
import Control.Monad.State
import Control.Monad.Error
import qualified System.Log.Logger as LOG
import Data.List 
import Data.Either (rights, lefts)
import Data.Foldable (foldlM)
import qualified Data.List.Utils as DLU
import qualified Data.Map as Map

-- this daemon returns a catalog when asked for a node and facts
data DaemonMessage
    = QCatalog (String, Facts, Chan DaemonMessage)
    | RCatalog (Either String Catalog)

logDebug = LOG.debugM "Puppet.Daemon"
logInfo = LOG.infoM "Puppet.Daemon"
logWarning = LOG.warningM "Puppet.Daemon"
logError = LOG.errorM "Puppet.Daemon"

initDaemon :: Prefs -> IO ( String -> Facts -> IO(Either String Catalog) )
initDaemon prefs = do
    logDebug "initDaemon"
    controlChan <- newChan
    getstmts <- initParserDaemon prefs
    forkIO (master prefs controlChan (getstmts))
    return (gCatalog controlChan)

master :: Prefs -> Chan DaemonMessage -> (TopLevelType -> String -> IO (Either String Statement)) -> IO ()
master prefs chan getstmts = do
    message <- readChan chan
    case message of
        QCatalog (nodename, facts, respchan) -> do
            logDebug ("Received query for node " ++ nodename)
            (stmts, warnings) <- getCatalog (getstmts) nodename facts
            mapM logWarning warnings
            case stmts of
                Left x -> writeChan respchan (RCatalog $ Left x)
                Right x -> writeChan respchan (RCatalog $ Right x)
        _ -> logError "Bad message type for master"
    master prefs chan getstmts

gCatalog :: Chan DaemonMessage -> String -> Facts -> IO (Either String Catalog)
gCatalog channel nodename facts = do
    respchan <- newChan
    writeChan channel $ QCatalog (nodename, facts, respchan)
    response <- readChan respchan
    case response of
        RCatalog x -> return x
        _ -> return $ Left "Bad answer from the master"

-- this daemon returns a list of statements when asked for a top class/define name
data ParserMessage
    = QStatement (TopLevelType, String, Chan ParserMessage)
    | RStatement (Either String Statement)

initParserDaemon :: Prefs -> IO (TopLevelType -> String -> IO (Either String Statement) )
initParserDaemon prefs = do
    logDebug "initParserDaemon"
    controlChan <- newChan
    getparsed <- initParsedDaemon prefs
    forkIO (pmaster prefs controlChan (getparsed))
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
checkFileInfo :: (FilePath, FileStatus) -> ErrorT String IO Bool
checkFileInfo (fpath, fstatus) = do
    stat <- liftIO $ getFileInfo fpath
    case stat of
        Just nfstatus -> return (extractFStatus nfstatus == extractFStatus fstatus)
        Nothing -> return False

checkFileInfos :: [(FilePath, FileStatus)] -> ErrorT String IO Bool
checkFileInfos filemap = do
    checkedmap <- mapM checkFileInfo filemap
    case checkedmap of
        [] -> return False
        x  -> return $ and x

compilefilelist :: Prefs -> TopLevelType -> String -> [FilePath]
compilefilelist prefs TopNode name = [manifest prefs ++ "/site.pp"]
compilefilelist prefs _ name = moduleInfo ++ [manifest prefs ++ "/site.pp"]
    where
        moduleInfo | length nameparts == 1 = [modules prefs ++ "/" ++ name ++ "/manifests/init.pp"]
                   | otherwise = [modules prefs ++ "/" ++ (head nameparts) ++ "/manifests/" ++ (DLU.join "/" (tail nameparts)) ++ ".pp"]
        nameparts = DLU.split "::" name

findFile :: Prefs -> TopLevelType -> String -> ErrorT String IO (FilePath, FileStatus)
findFile prefs qtype resname = do
    let filelist = compilefilelist prefs qtype resname
    fileinfo <- liftIO $ foldlM (getFirstFileInfo) Nothing filelist
    case fileinfo of
        Just x -> return x
        Nothing -> throwError ("Could not find file for " ++ show qtype ++ " " ++ resname ++ " when looking in " ++ (show filelist))

{-
 given a filename and a file status, will parse this file and update the cache with the parsed values
 -}
loadUpdateFile :: FilePath -> FileStatus -> (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> ErrorT String IO ([Statement], [(TopLevelType, String, Statement)])
loadUpdateFile fname fstatus updatepinfo = do
    liftIO $ logDebug ("Loading file " ++ fname)
    parsed <- parseFile fname
    let toplevels = map convertTopLevel parsed
        oktoplevels = rights toplevels
        othertoplevels = lefts toplevels
        (imports, badtoplevels) = partition isImport othertoplevels
        isImport (Import _ _) = True
        isImport _ = False
    liftIO $ mapM (\x -> logError ("Unsupported top level statement: " ++ (show x))) badtoplevels
    liftIO $ mapM (\(rtype, resname, resstatement) -> updatepinfo rtype resname (resstatement, [(fname, fstatus)])) oktoplevels
    return (imports, oktoplevels)

reparseStatements :: Prefs -> (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> TopLevelType -> String -> ErrorT String IO Statement
reparseStatements prefs updatepinfo qtype nodename = do
    (fname, fstatus) <- findFile prefs qtype nodename
    (imports, oktoplevels) <- loadUpdateFile fname fstatus updatepinfo
    imported <- mapM (loadImport prefs updatepinfo (takeDirectory fname)) imports >>= return . concat
    let searchstatement = find (\(qt,nm,_) -> (qt == qtype) && (nm == nodename)) (oktoplevels ++ imported)
    case searchstatement of
        Just (_,_,x) -> return x
        Nothing -> throwError ("Could not find correct top level statement for " ++ (show qtype) ++ " " ++ nodename)

loadImport :: Prefs -> (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> FilePath -> Statement -> ErrorT String IO [(TopLevelType, String, Statement)]
loadImport prefs updatepinfo fdir (Import importstring pos) = do
    matched <- liftIO $ globDir [compile importstring] fdir >>= return . concat . fst
    fileinfos <- liftIO $ mapM getFileInfo matched
    let fpathinfos = zip matched fileinfos
        goodpathinfos = concatMap unjust fpathinfos
        unjust (a, Just b) = [(a,b)]
        unjust _ = []
    mapM (\(fname, finfo) -> loadUpdateFile fname finfo updatepinfo) goodpathinfos >>= return . concatMap snd

loadImport _ _ _ _ = throwError "Bad statement type passed to loadImport"

handlePRequest :: Prefs -> ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry), TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> TopLevelType -> String -> ErrorT String IO Statement
handlePRequest prefs (getpinfo, updatepinfo) qtype nodename = do
    res <- getpinfo qtype nodename
    case res of
        Just (stmts, fileinfos) -> do
            isfileinfoaccurate <- checkFileInfos fileinfos
            if isfileinfoaccurate
                then return stmts
                else reparseStatements prefs updatepinfo qtype nodename
        Nothing -> reparseStatements prefs updatepinfo qtype nodename

pmaster :: Prefs -> Chan ParserMessage -> ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry), TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> IO ()
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

getStatements :: Chan ParserMessage -> TopLevelType -> String -> IO (Either String Statement)
getStatements channel qtype classname = do
    respchan <- newChan
    writeChan channel $ QStatement (qtype, classname, respchan)
    response <- readChan respchan
    case response of
        RStatement x -> return x
        _            -> return $ Left "Bad answer from the pmaster"

type CacheEntry = (Statement, [(FilePath, FileStatus)])
data ParsedCacheQuery
    = GetParsedData TopLevelType String (Chan ParsedCacheResponse)
    | UpdateParsedData TopLevelType String CacheEntry (Chan ParsedCacheResponse)
data ParsedCacheResponse
    = CacheError String
    | RCacheEntry CacheEntry
    | NoCacheEntry
    | CacheUpdated
    
-- this one is singleton, and is used to cache de parsed values, along with the file names they depend on
initParsedDaemon :: Prefs -> IO ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry), TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) )
initParsedDaemon prefs = do
    logDebug "initParsedDaemon"
    controlChan <- newChan
    forkIO ( evalStateT (parsedmaster prefs controlChan) Map.empty )
    return (getParsedInformation controlChan, updateParsedInformation controlChan)

getParsedInformation :: Chan ParsedCacheQuery -> TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry)
getParsedInformation cchan qtype name = do
    respchan <- liftIO newChan
    liftIO $ writeChan cchan $ GetParsedData qtype name respchan
    out <- liftIO $ readChan respchan
    case out of
        RCacheEntry x -> return $ Just x
        NoCacheEntry  -> return Nothing
        CacheError x  -> throwError x
        _             -> throwError "Unknown cache response type"

updateParsedInformation :: Chan ParsedCacheQuery -> TopLevelType -> String -> CacheEntry -> IO (ParsedCacheResponse)
updateParsedInformation pchannel qtype name centry = do
    respchan <- newChan
    writeChan pchannel $ UpdateParsedData qtype name centry respchan
    readChan respchan

parsedmaster prefs controlchan = do
    curmsg <- liftIO $ readChan controlchan
    case curmsg of
        GetParsedData qtype name respchan -> do
            curmap <- get
            case (Map.lookup (qtype, name) curmap) of
                Just x  -> liftIO $ writeChan respchan (RCacheEntry x)
                Nothing -> liftIO $ writeChan respchan NoCacheEntry
        UpdateParsedData qtype name val respchan -> do
            liftIO $ logDebug ("Updating parsed cache for " ++ show qtype ++ " " ++ name)
            modify (Map.insert (qtype, name) val)
            liftIO $ writeChan respchan CacheUpdated
    parsedmaster prefs controlchan
