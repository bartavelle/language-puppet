-- from http://stackoverflow.com/questions/8820903/haskell-how-to-timeout-a-function-that-runs-an-external-command

module SafeProcess where

import Control.Concurrent
import Control.Exception
import System.Exit
import System.IO
import System.IO.Error
import System.Timeout
import System.Posix.Signals
import System.Process
import System.Process.Internals

safeReadProcessTimeout :: String -> [String] -> String -> Int -> IO (Maybe (Either String String))
safeReadProcessTimeout prog args input tout = timeout (tout*1000) $ safeReadProcess prog args input

safeCreateProcess :: String -> [String] -> StdStream -> StdStream -> StdStream
                  -> ( ( Maybe Handle
                       , Maybe Handle
                       , Maybe Handle
                       , ProcessHandle
                       ) -> IO a )
                  -> IO a
safeCreateProcess prog args streamIn streamOut streamErr fun = bracket
    ( do
        h <- createProcess (proc prog args) 
                 { std_in  = streamIn
                 , std_out = streamOut
                 , std_err = streamErr
                 , create_group = True }
        return h
    )
-- "interruptProcessGroupOf" is in the new System.Process. Since some
-- programs return funny exit codes i implemented a "terminateProcessGroupOf".
--    (\(_, _, _, ph) -> interruptProcessGroupOf ph >> waitForProcess ph)
    (\(_, _, _, ph) -> terminateProcessGroup ph >> waitForProcess ph)
    fun
{-# NOINLINE safeCreateProcess #-}

safeReadProcess :: String -> [String] -> String -> IO (Either String String)
safeReadProcess prog args str =
    safeCreateProcess prog args CreatePipe CreatePipe Inherit
      (\(Just inh, Just outh, _, ph) -> do
        hSetBinaryMode inh True
        hSetBinaryMode outh True
        hPutStr inh str
        hClose inh
        -- fork a thread to consume output
        output <- hGetContents outh
        outMVar <- newEmptyMVar
        forkIO $ evaluate (length output) >> putMVar outMVar ()
        -- wait on output
        takeMVar outMVar
        hClose outh
        ex <- waitForProcess ph
        case ex of
            ExitSuccess     -> return $ Right output
            ExitFailure r   -> return $ Left $ prog ++ " " ++ show args ++ " failed, errorcode = " ++ show r
      )

terminateProcessGroup :: ProcessHandle -> IO ()
terminateProcessGroup ph = do
    let (ProcessHandle pmvar) = ph
    ph_ <- readMVar pmvar
    case ph_ of
        OpenHandle pid -> do  -- pid is a POSIX pid
            signalProcessGroup 15 pid
        otherwise -> return ()
