{-# LANGUAGE StrictData #-}
{- |
This module let you create caches where keys are file names.

Unlike 'Data.FileCache' is doesn't watch for file change.

It is only useful in a client setting, not in a daemon/server one.

This is usually done in the following fashion :

> cache <- newFileCache
> o <- query cache "/path/to/file" computation

The computation will be used to populate the cache if this call results in a miss. The result is forced to WHNM.
-}
module Cache.File
  ( FileCache
  , newFileCache
  , query
  ) where

import           Protolude

import           Control.Concurrent.STM
import           Control.Exception.Lens
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.String

-- | The main opaque FileCache type.
newtype FileCache e a
  = FileCache (TVar (HashMap FilePath (Either e a)))

-- | Generates a new file cache (smart constructor).
newFileCache :: IO (FileCache r a)
newFileCache = FileCache <$> newTVarIO Map.empty

-- | Queries the cache, populating it if necessary.
--
-- Queries that fail with an 'IOExeception' will not create a cache entry.
query :: IsString e
      => FileCache e a
      -> FilePath -- ^ Path of the file entry
      -> IO (Either e a) -- ^ The computation that will be used to populate the cache
      -> IO (Either e a) -- ^ Return a "Data.Either.Strict"
query f@(FileCache fc) fp action = do
  mp <- getCache f
  case Map.lookup fp mp of
    Just x -> pure x
    Nothing -> do
      a <- catching _IOException action (pure . Left . fromString . show)
      atomically (modifyTVar fc (Map.insert fp a))
      pure a

getCache :: FileCache e a -> IO (HashMap FilePath (Either e a))
getCache (FileCache q) = atomically (readTVar q)
