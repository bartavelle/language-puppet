{-| A quickly done module that exports utility functions used to collect various
statistics. All statistics are stored in a MVar holding a HashMap.

This is not accurate in the presence of lazy evaluation. Nothing is forced.
-}
module Puppet.Stats (measure, measure_, newStats, getStats, StatsTable, StatsPoint(..), MStats) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad
import Control.Concurrent
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Lens

data StatsPoint = StatsPoint { _statspointCount :: !Int    -- ^ Total number of calls to a computation
                             , _statspointTotal :: !Double -- ^ Total time spent during this computation
                             , _statspointMin   :: !Double -- ^ Minimum execution time
                             , _statspointMax   :: !Double -- ^ Maximum execution time
                             } deriving(Show)

-- | A table where keys are the names of the computations, and values are
-- 'StatsPoint's.
type StatsTable = HM.HashMap T.Text StatsPoint

newtype MStats = MStats { unMStats :: MVar StatsTable }

-- | Returns the actual statistical values.
getStats :: MStats -> IO StatsTable
getStats = readMVar . unMStats

-- | Create a new statistical container.
newStats :: IO MStats
newStats = MStats `fmap` newMVar HM.empty

-- | Wraps a computation, and measures related execution statistics.
measure :: MStats -- ^ Statistics container
        -> T.Text -- ^ Action identifier
        -> IO a   -- ^ Computation
        -> IO a
measure (MStats mtable) statsname action = do
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

-- | Just like 'measure', discarding the result value.
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

