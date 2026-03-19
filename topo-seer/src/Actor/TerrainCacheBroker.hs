-- | Lock-free IORef channel for terrain cache build results.
--
-- Results are published to a shared 'TerrainCacheRef' so the render thread
-- can poll via lock-free 'readIORef' without actor indirection.
module Actor.TerrainCacheBroker
  ( TerrainCacheRef
  , newTerrainCacheRef
  , writeTerrainCacheResult
  , readTerrainCacheRef
  ) where

import Actor.TerrainCacheWorker (TerrainCacheBuildResult)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Shared reference for lock-free render-thread reads.
type TerrainCacheRef = IORef (Maybe TerrainCacheBuildResult)

-- | Create a new empty 'TerrainCacheRef'.
newTerrainCacheRef :: IO TerrainCacheRef
newTerrainCacheRef = newIORef Nothing

-- | Write the latest terrain cache result to the shared IORef.
writeTerrainCacheResult :: TerrainCacheRef -> TerrainCacheBuildResult -> IO ()
writeTerrainCacheResult ref result = writeIORef ref (Just result)

-- | Read the latest terrain cache result from the shared IORef (lock-free).
readTerrainCacheRef :: TerrainCacheRef -> IO (Maybe TerrainCacheBuildResult)
readTerrainCacheRef = readIORef
