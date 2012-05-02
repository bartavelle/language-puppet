module Puppet.Daemon (initDaemon) where

import Puppet.Preferences
import Puppet.Interpreter.Types
import Puppet.DSL.Types
import Control.Concurrent
import Control.Concurrent.Chan
import System.Posix.Files
import Control.Monad.State
import Control.Monad.Error
import System.Log.Logger
import qualified Data.Map as Map

data QType = QNode | QDefine | QClass deriving (Show, Ord, Eq)

-- this daemon returns a catalog when asked for a node and facts
data DaemonMessage
    = QCatalog (String, Facts, Chan DaemonMessage)
    | RCatalog (Either String Catalog)

initDaemon :: Prefs -> IO ( String -> Facts -> IO(Either String Catalog) )
initDaemon prefs = do
    controlChan <- newChan
    getstmts <- initParserDaemon prefs
    forkIO (master prefs controlChan (getstmts))
    return (getCatalog controlChan)

master :: Prefs -> Chan DaemonMessage -> (QType -> String -> IO (Either String [Statement])) -> IO ()
master prefs chan getstmts = do
    message <- readChan chan
    case message of
        QCatalog (nodename, facts, respchan) -> do
            stmts <- getstmts QNode nodename
            case stmts of
                Left x -> writeChan respchan (RCatalog $ Left x)
                Right x -> writeChan respchan (RCatalog $ Left $ show x)
        _ -> errorM "Puppet.Daemon.master" "Bad message type"
    master prefs chan getstmts

getCatalog :: Chan DaemonMessage -> String -> Facts -> IO (Either String Catalog)
getCatalog channel nodename facts = do
    respchan <- newChan
    writeChan channel $ QCatalog (nodename, facts, respchan)
    response <- readChan respchan
    case response of
        RCatalog x -> return x
        _ -> return $ Left "Bad answer from the master"

-- this daemon returns a list of statements when asked for a top class/define name
data ParserMessage
    = QStatement (QType, String, Chan ParserMessage)
    | RStatement (Either String [Statement])

initParserDaemon :: Prefs -> IO (QType -> String -> IO (Either String [Statement]) )
initParserDaemon prefs = do
    controlChan <- newChan
    getparsed <- initParsedDaemon prefs
    forkIO (pmaster prefs controlChan (getparsed))
    return (getStatements controlChan)


handlePRequest :: Prefs -> Chan ParserMessage -> ( QType -> String -> ErrorT String IO ParsedCacheResponse, QType -> String -> IO ( ParsedCacheResponse ) ) -> QType -> String -> ErrorT String IO [Statement]
handlePRequest prefs chan (getpinfo, updatepinfo) qtype nodename = do
    cachedinfo <- getpinfo qtype nodename
    return []


pmaster :: Prefs -> Chan ParserMessage -> ( QType -> String -> ErrorT String IO ParsedCacheResponse, QType -> String -> IO ( ParsedCacheResponse ) ) -> IO ()
pmaster prefs chan cachefuncs = do
    pmessage <- readChan chan
    case pmessage of
        QStatement (qtype, nodename, respchan) -> do
            out <- runErrorT $ handlePRequest prefs chan cachefuncs qtype nodename 
            case out of
                Left x -> writeChan respchan $ RStatement $ Left x
                Right y -> writeChan respchan $ RStatement $ Right y
        _ -> print "bad message type!"
{-
    pmessage <- readChan chan
    case pmessage of
        QStatement (qtype, nodename, respchan) -> do
            cachedinfo <- getcache qtype nodename
            case cachedinfo of
                CacheError x -> writeChan respchan $ RStatement $ Left x
                NoCacheEntry -> writeChan respchan $ RStatement $ Left "no cache entry"
                CacheEntry (statements, fileinfos) -> writeChan respchan $ RStatement $ Left "cache entry found"
        _ -> print "Bad message type!" -}
    pmaster prefs chan cachefuncs

getStatements :: Chan ParserMessage -> QType -> String -> IO (Either String [Statement])
getStatements channel qtype classname = do
    respchan <- newChan
    writeChan channel $ QStatement (qtype, classname, respchan)
    response <- readChan respchan
    case response of
        RStatement x -> return x
        _ -> return $ Left "Bad answer from the pmaster"

-- this one is unique, and is used to cache de parsed values, along with the file names they depend on
-- this is pretty complicated ...
data ParsedCacheQuery
    = GetParsedData QType String (Chan ParsedCacheResponse)
    | UpdateParsedData QType String ([Statement], Map.Map FilePath FileStatus) (Chan ParsedCacheResponse)
data ParsedCacheResponse
    = CacheError String
    | CacheEntry ([Statement], Map.Map FilePath FileStatus)
    | NoCacheEntry
    
--initParsedDaemon :: Prefs -> IO (QType -> String -> IO (Either String, ([Statement], Map.Map FilePath ClockTime) ))
initParsedDaemon :: Prefs -> IO ( QType -> String -> ErrorT String IO ParsedCacheResponse, QType -> String -> IO ( ParsedCacheResponse ) )
initParsedDaemon prefs = do
    controlChan <- newChan
    forkIO ( evalStateT (parsedmaster prefs controlChan) Map.empty )
    return (getParsedInformation controlChan, updateParsedInformation controlChan)

getParsedInformation :: Chan ParsedCacheQuery -> QType -> String -> ErrorT String IO ParsedCacheResponse
getParsedInformation cchan qtype name = do
    respchan <- liftIO newChan
    liftIO $ writeChan cchan $ GetParsedData qtype name respchan
    out <- liftIO $ readChan respchan
    case out of
        CacheError x -> throwError x
        y -> return y

updateParsedInformation :: Chan ParsedCacheQuery -> QType -> String -> IO (ParsedCacheResponse)
updateParsedInformation _ _ _ = return $ CacheError "not implemented"

parsedmaster prefs controlchan = do
    curmap <- get
    curmsg <- liftIO $ readChan controlchan
    case curmsg of
        GetParsedData qtype name respchan -> do
            --liftIO $ writeChan respchan NoCacheEntry
            liftIO $ writeChan respchan NoCacheEntry
        _ -> error "wtf"
    parsedmaster prefs controlchan
