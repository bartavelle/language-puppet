-- from http://stackoverflow.com/questions/8820903/haskell-how-to-timeout-a-function-that-runs-an-external-command

module SafeProcess where

import Control.Concurrent
import Control.Exception
import System.Exit
import System.IO
import System.Timeout
import System.Posix.Signals
import System.Process
import System.Process.Internals
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

safeReadProcessTimeout :: String -> [String] -> TL.Text -> Int -> IO (Maybe (Either String T.Text))
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

safeReadProcess :: String -> [String] -> TL.Text -> IO (Either String T.Text)
safeReadProcess prog args str =
    safeCreateProcess prog args CreatePipe CreatePipe Inherit
      (\(Just inh, Just outh, _, ph) -> do
        hSetBinaryMode inh True
        hSetBinaryMode outh True
        TL.hPutStr inh str
        hClose inh
        -- fork a thread to consume output
        output <- T.hGetContents outh
        outMVar <- newEmptyMVar
        _ <- forkIO $ evaluate (T.length output) >> putMVar outMVar ()
        -- wait on output
        takeMVar outMVar
        hClose outh
        waitForProcess ph >>= \case
            ExitSuccess     -> return $ Right output
            ExitFailure r   -> return $ Left $ prog ++ " " ++ show args ++ " failed, errorcode = " ++ show r
      )

terminateProcessGroup :: ProcessHandle -> IO ()
terminateProcessGroup ph = do
    let (ProcessHandle pmvar) = ph
    readMVar pmvar >>= \case
        OpenHandle pid -> do  -- pid is a POSIX pid
            signalProcessGroup 15 pid
        _ -> return ()
