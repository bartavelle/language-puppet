{-| A quickly done module that exports utility functions used to collect various
statistics. All statistics are stored in a MVar holding a HashMap.
-}
module Puppet.Stats where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad
import Control.Concurrent
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Lens

data StatsPoint = StatsPoint !Int !Double !Double !Double
    deriving(Show)
type StatsTable = HM.HashMap T.Text StatsPoint

type MStats = MVar StatsTable

getStats :: MStats -> IO StatsTable
getStats = readMVar

newStats :: IO MStats
newStats = newMVar HM.empty

measure :: MStats -> T.Text -> IO a -> IO a
measure mtable statsname action = do
    (!tm, !out) <- time action
    !stats <- takeMVar mtable
    let nstats :: StatsTable
        !nstats = case stats ^. at statsname of
                      Nothing -> stats & at statsname ?~ StatsPoint 1 tm tm tm
                      Just (StatsPoint sc st smi sma) ->
                          let !nmax = if tm > sma
                                          then tm
                                          else sma
                              !nmin = if tm < smi
                                          then tm
                                          else smi
                          in stats & at statsname ?~ StatsPoint (sc+1) (st+tm) nmin nmax
    putMVar mtable nstats
    return out

measure_ :: MStats -> T.Text -> IO a -> IO ()
measure_ mtable statsname action = void ( measure mtable statsname action )

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

time :: IO a -> IO (Double, a)
time action = do
    start <- getTime
    !result <- action
    end <- getTime
    let !delta = end - start
    return (delta, result)

