module Puppet.Daemon (initDaemon) where

import Puppet.Init
import Puppet.Interpreter.Types
import Puppet.Interpreter.Catalog
import Puppet.DSL.Types
import Puppet.DSL.Loader
import Erb.Compute
import Control.Concurrent
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
import Text.Parsec.Pos (initialPos)

-- this daemon returns a catalog when asked for a node and facts
data DaemonMessage
    = QCatalog (String, Facts, Chan DaemonMessage)
    | RCatalog (Either String FinalCatalog)

-- nbstats nbrequests
data DaemonStats = DaemonStats Int Integer
    deriving (Show)


logDebug = LOG.debugM "Puppet.Daemon"
logInfo = LOG.infoM "Puppet.Daemon"
logWarning = LOG.warningM "Puppet.Daemon"
logError = LOG.errorM "Puppet.Daemon"

initDaemon :: Prefs -> IO ( String -> Facts -> IO(Either String FinalCatalog) )
initDaemon prefs = do
    logDebug "initDaemon"
    controlChan <- newChan
    getstmts <- initParserDaemon prefs
    templatefunc <- initTemplateDaemon prefs
    forkIO (master prefs controlChan getstmts templatefunc)
    return (gCatalog controlChan)

master :: Prefs
    -> Chan DaemonMessage
    -> (TopLevelType -> String -> IO (Either String Statement))
    -> (String -> String -> [(String, GeneralValue)] -> IO (Either String String))
    -> IO ()
master prefs chan getstmts gettemplate = do
    message <- readChan chan
    case message of
        QCatalog (nodename, facts, respchan) -> do
            logDebug ("Received query for node " ++ nodename)
            (stmts, warnings) <- getCatalog getstmts gettemplate nodename facts
            mapM logWarning warnings
            case stmts of
                Left x -> writeChan respchan (RCatalog $ Left x)
                Right x -> writeChan respchan (RCatalog $ Right x)
        _ -> logError "Bad message type for master"
    master prefs chan getstmts gettemplate

gCatalog :: Chan DaemonMessage -> String -> Facts -> IO (Either String FinalCatalog)
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

initParserDaemon :: Prefs -> IO
    ( TopLevelType -> String -> IO (Either String Statement)
    )
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
checkFileInfo :: FilePath -> FileStatus -> ErrorT String IO Bool
checkFileInfo fpath fstatus = do
    stat <- liftIO $ getFileInfo fpath
    case stat of
        Just nfstatus -> return (extractFStatus nfstatus == extractFStatus fstatus)
        Nothing -> return False

compilefilelist :: Prefs -> TopLevelType -> String -> [FilePath]
compilefilelist prefs TopNode _ = [manifest prefs ++ "/site.pp"]
compilefilelist prefs _ name = moduleInfo
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

globImport :: FilePath -> Statement -> IO [FilePath]
globImport origfile (Import importname _) = globDir [compile importname] (takeDirectory origfile) >>= return . concat . fst
globImport _ _ = return []

{-
 given a filename and a file status, will parse this file and update the cache with the parsed values

 it must also parse all required files and store everything about them before returning
 -}
loadUpdateFile :: FilePath -> FileStatus -> (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> ErrorT String IO ([Statement], [(TopLevelType, String, Statement)])
loadUpdateFile fname fstatus updatepinfo = do
    liftIO $ logDebug ("Loading file " ++ fname)
    parsed <- parseFile fname
    let toplevels = map convertTopLevel parsed
        oktoplevels = rights toplevels
        othertoplevels = lefts toplevels
        (imports, spurioustoplevels) = partition isImport othertoplevels
        isImport (Import _ _) = True
        isImport _ = False
    relatedtops <- liftIO $ mapM (globImport fname) imports >>= return . concat
        -- save this spurious top levels
    if null spurioustoplevels
        then return ()
        else do
            liftIO ( updatepinfo TopSpurious fname (ClassDeclaration "::" Nothing [] spurioustoplevels (initialPos fname), fname, fstatus, relatedtops) >> return () )
            liftIO $ logWarning ("Spurious top level statement in file " ++ fname ++ ", expect bugs in case you modify them" )
    -- saves all good top levels
    liftIO $ mapM (\(rtype, resname, resstatement) -> updatepinfo rtype resname (resstatement, fname, fstatus, relatedtops)) oktoplevels
    return (imports, oktoplevels)

reparseStatements :: Prefs -> (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> TopLevelType -> String -> ErrorT String IO Statement
reparseStatements prefs updatepinfo qtype nodename = do
    (fname, fstatus) <- findFile prefs qtype nodename
    (imports, oktoplevels) <- loadUpdateFile fname fstatus updatepinfo
    imported <- mapM (loadImport updatepinfo fname) imports >>= return . concat
    let searchstatement = find (\(qt,nm,_) -> (qt == qtype) && (nm == nodename)) (oktoplevels ++ imported)
    case searchstatement of
        Just (_,_,x) -> return x
        Nothing -> throwError ("Could not find correct top level statement for " ++ (show qtype) ++ " " ++ nodename)

loadImport :: (TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse ) ) -> FilePath -> Statement -> ErrorT String IO [(TopLevelType, String, Statement)]
loadImport updatepinfo fdir mimport = do
    matched <- liftIO $ globImport fdir mimport
    -- globDir [compile importstring] fdir >>= return . concat . fst
    fileinfos <- liftIO $ mapM getFileInfo matched
    let fpathinfos = zip matched fileinfos
        goodpathinfos = concatMap unjust fpathinfos
        unjust (a, Just b) = [(a,b)]
        unjust _ = []
    mapM (\(fname, finfo) -> loadUpdateFile fname finfo updatepinfo) goodpathinfos >>= return . concatMap snd

loadRelated :: 
       ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry) )
    -> FilePath
    -> ErrorT String IO [(String, Statement)]
loadRelated getpinfo filename = do
    res <- getpinfo TopSpurious filename
    case res of
        Nothing -> return []
        Just (stmts, _, _, related) -> do
            relstatements <- mapM (loadRelated getpinfo) related >>= return . concat
            return $ (filename,stmts):relstatements

handlePRequest :: Prefs ->
    ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse )
    , String -> IO ( ParsedCacheResponse )
    ) -> TopLevelType -> String -> ErrorT String IO Statement
handlePRequest prefs (getpinfo, updatepinfo, invalidateinfo) qtype nodename = do
    res <- getpinfo qtype nodename
    case res of
        Just (stmts, fpath, fstatus, related) -> do
            -- for this to work, everything must be cached
            -- this is buggy as the required stuff will not be invalidated properly
            relstatements <- mapM (loadRelated getpinfo) (fpath:related) >>= return . concat
            isfileinfoaccurate <- checkFileInfo fpath fstatus
            statements <- if isfileinfoaccurate
                            then return stmts
                            else do
                                liftIO $ invalidateinfo fpath
                                reparseStatements prefs updatepinfo qtype nodename
            if null relstatements
                then return statements
                else return (TopContainer relstatements statements)
        Nothing -> reparseStatements prefs updatepinfo qtype nodename >> handlePRequest prefs (getpinfo, updatepinfo, invalidateinfo) qtype nodename

pmaster :: Prefs -> Chan ParserMessage ->
    ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse )
    , String -> IO ( ParsedCacheResponse )
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

getStatements :: Chan ParserMessage -> TopLevelType -> String -> IO (Either String Statement)
getStatements channel qtype classname = do
    respchan <- newChan
    writeChan channel $ QStatement (qtype, classname, respchan)
    response <- readChan respchan
    case response of
        RStatement x -> return x
        _            -> return $ Left "Bad answer from the pmaster"

-- this is a cache entry, it stores a top level statement, the path of the corresponding file and its status
-- it also stores a list of top level statements that are related
type CacheEntry = (Statement, FilePath, FileStatus, [FilePath])
data ParsedCacheQuery
    = GetParsedData TopLevelType String (Chan ParsedCacheResponse)
    | UpdateParsedData TopLevelType String CacheEntry (Chan ParsedCacheResponse)
    | InvalidateCacheFile String (Chan ParsedCacheResponse)
    | GetStats (Chan DaemonStats)
data ParsedCacheResponse
    = CacheError String
    | RCacheEntry CacheEntry
    | NoCacheEntry
    | CacheUpdated
    
-- this one is singleton, and is used to cache de parsed values, along with the file names they depend on
initParsedDaemon :: Prefs -> IO
    ( TopLevelType -> String -> ErrorT String IO (Maybe CacheEntry)
    , TopLevelType -> String -> CacheEntry -> IO ( ParsedCacheResponse )
    , String -> IO ( ParsedCacheResponse )
    )
initParsedDaemon prefs = do
    logDebug "initParsedDaemon"
    controlChan <- newChan
    forkIO ( evalStateT (parsedmaster prefs controlChan) (Map.empty, Map.empty, 0 :: Integer) )
    return (getParsedInformation controlChan, updateParsedInformation controlChan, invalidateCachedFile controlChan)

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

invalidateCachedFile :: Chan ParsedCacheQuery -> String -> IO (ParsedCacheResponse)
invalidateCachedFile pchannel name = do
    respchan <- newChan
    writeChan pchannel $ InvalidateCacheFile name respchan
    readChan respchan

-- state : (parsed statements map, file association map, nbrequests)
parsedmaster :: Prefs -> Chan ParsedCacheQuery -> StateT 
    ( Map.Map (TopLevelType, String) CacheEntry
    , Map.Map FilePath (FileStatus, [(TopLevelType, String)])
    , Integer
    ) IO ()
parsedmaster prefs controlchan = do
    curmsg <- liftIO $ readChan controlchan
    case curmsg of
        GetStats respchan -> do
            (curmap, _, nbrequests) <- get
            liftIO $ writeChan respchan $ DaemonStats (Map.size curmap) nbrequests
        GetParsedData qtype name respchan -> do
            (curmap, _, _) <- get
            modify (\(mp, fm, rq) -> (mp, fm, rq + 1))
            case (Map.lookup (qtype, name) curmap) of
                Just x  -> liftIO $ writeChan respchan (RCacheEntry x)
                Nothing -> liftIO $ writeChan respchan NoCacheEntry
        UpdateParsedData qtype name val@(_, filepath, filestatus, _) respchan -> do
            liftIO $ logDebug ("Updating parsed cache for " ++ show qtype ++ " " ++ name)
            (mp, fm, rq) <- get
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
            put (statementmap, fileassocmap, rq+1)
            liftIO $ writeChan respchan CacheUpdated
        InvalidateCacheFile fname respchan -> do
            liftIO $ logDebug $ "Invalidating files for " ++ fname
            (mp, fm, rq) <- get
            let
                nfm = Map.delete fname fm
                nmp = case (Map.lookup fname fm) of
                    Just (_, remlist) -> foldl' (\mp' el -> Map.delete el mp') mp remlist
                    Nothing           -> mp
            put (nmp, nfm, rq)
            liftIO $ writeChan respchan CacheUpdated
    parsedmaster prefs controlchan
