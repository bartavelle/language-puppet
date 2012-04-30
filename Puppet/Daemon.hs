module Puppet.Daemon (initDaemon) where

import Puppet.Preferences
import Puppet.Interpreter.Types
import Puppet.DSL.Types
import Control.Concurrent
import Control.Concurrent.Chan

data DaemonMessage
    = QCatalog (String, Facts, Chan DaemonMessage)
    | RCatalog (Either String Catalog)

data ParserMessage
    = QStatement (String, Chan ParserMessage)
    | RStatement (Either String [Statement])

-- this daemon returns a catalog when asked for a node and facts
initDaemon :: Prefs -> IO ( String -> Facts -> IO(Either String Catalog) )
initDaemon prefs = do
    controlChan <- newChan
    forkIO (master prefs controlChan)
    return (getCatalog controlChan)

master :: Prefs -> Chan DaemonMessage -> IO ()
master prefs chan = do
    message <- readChan chan
    case message of
        QCatalog (nodename, facts, respchan) -> do
            writeChan respchan (RCatalog $ Left nodename)
        _ -> print "Bad message type!"
    master prefs chan

getCatalog :: Chan DaemonMessage -> String -> Facts -> IO (Either String Catalog)
getCatalog channel nodename facts = do
    respchan <- newChan
    writeChan channel $ QCatalog (nodename, facts, respchan)
    response <- readChan respchan
    case response of
        RCatalog x -> return x
        _ -> return $ Left "Bad answer from the master"

-- this daemon returns a list of statements when asked for a top class/define name
initParserDaemon :: Prefs -> IO (String -> IO (Either String [Statement]) )
initParserDaemon prefs = do
    controlChan <- newChan
    forkIO (pmaster prefs controlChan)
    return (getStatements controlChan)

pmaster :: Prefs -> Chan ParserMessage -> IO ()
pmaster prefs chan = do
    pmessage <- readChan chan
    case pmessage of
        QStatement (classname, respchan) -> do
            writeChan respchan $ RStatement $ Left "unimplemented"
        _ -> print "Bad message type!"
    pmaster prefs chan

getStatements :: Chan ParserMessage -> String -> IO (Either String [Statement])
getStatements channel classname = do
    respchan <- newChan
    writeChan channel $ QStatement (classname, respchan)
    response <- readChan respchan
    case response of
        RStatement x -> return x
        _ -> return $ Left "Bad answer from the pmaster"

