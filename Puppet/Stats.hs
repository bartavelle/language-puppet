{-| A quickly done module that exports utility functions used to collect various
statistics. All statistics are stored in a MVar holding a Map.
-}
module Puppet.Stats where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.Map as Map
import qualified Data.Text as T

data StatsPoint = StatsPoint !Int !Double !Double !Double
    deriving(Show)
type StatsTable = Map.Map T.Text StatsPoint

type MStats = MVar StatsTable

getStats :: MStats -> IO StatsTable
getStats = readMVar

newStats :: IO MStats
newStats = newMVar Map.empty

measure :: MStats -> T.Text -> IO a -> IO a
measure mtable statsname action = do
    (tm, out) <- time action
    !stats <- takeMVar mtable :: IO StatsTable
    let nstats :: StatsTable
        !nstats = case Map.lookup statsname stats of
                     Nothing -> Map.insert statsname (StatsPoint 1 tm tm tm) stats
                     Just (StatsPoint sc st smi sma) ->
                        let !nmax = if tm > sma
                                       then tm
                                       else sma
                            !nmin = if tm < smi
                                       then tm
                                       else smi
                            in Map.insert statsname (StatsPoint (sc+1) (st+tm) nmin nmax) stats
    putMVar mtable nstats
    return out

measure_ :: MStats -> T.Text -> IO a -> IO ()
measure_ mtable statsname act = void ( measure mtable statsname act )

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

time :: IO a -> IO (Double, a)
time act = do
    start <- getTime
    !result <- act
    end <- getTime
    let !delta = end - start
    return (delta, result)

