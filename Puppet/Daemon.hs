module Puppet.Daemon (initDaemon) where

import Puppet.Preferences
import Puppet.Interpreter.Types
import Control.Concurrent
import Control.Concurrent.Chan

data DaemonMessage
    = QCatalog (String, Chan DaemonMessage)
    | RCatalog (Either String Catalog)

initDaemon :: Prefs -> IO ( String -> IO(Either String Catalog) )
initDaemon prefs = do
    controlChan <- newChan
    forkIO (master prefs controlChan)
    return (getCatalog controlChan)

master :: Prefs -> Chan DaemonMessage -> IO ()
master prefs chan = do
    message <- readChan chan
    case message of
        QCatalog (nodename, respchan) -> do
            writeChan respchan (RCatalog $ Left nodename)
        _ -> print "Bad message type!"
    master prefs chan

getCatalog :: Chan DaemonMessage -> String -> IO (Either String Catalog)
getCatalog channel nodename = do
    respchan <- newChan
    writeChan channel $ QCatalog (nodename, respchan)
    response <- readChan respchan
    case response of
        RCatalog x -> return x
        _ -> return $ Left "Bad answer from the master"
