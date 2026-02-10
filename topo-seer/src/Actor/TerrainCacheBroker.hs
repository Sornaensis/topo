{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Broker for terrain cache build results, drained by the render loop.
--
-- Results are published to a shared 'TerrainCacheRef' so the render thread
-- can poll via lock-free 'readIORef' instead of a synchronous actor call.
module Actor.TerrainCacheBroker
  ( TerrainCacheBroker
  , TerrainCacheRef
  , terrainCacheBrokerActorDef
  , enqueueTerrainCacheResult
  , getLatestTerrainCacheResult
  , setTerrainCacheRef
  , readTerrainCacheRef
  ) where

import Actor.TerrainCacheWorker (TerrainCacheBuildResult, TerrainCacheResultReply)
import Data.IORef (IORef, readIORef, writeIORef)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

-- | Shared reference for lock-free render-thread reads.
type TerrainCacheRef = IORef (Maybe TerrainCacheBuildResult)

data TerrainCacheBrokerState = TerrainCacheBrokerState
  { tcbLatest :: !(Maybe TerrainCacheBuildResult)
  , tcbRef    :: !(Maybe TerrainCacheRef)
  }

emptyTerrainCacheBrokerState :: TerrainCacheBrokerState
emptyTerrainCacheBrokerState = TerrainCacheBrokerState
  { tcbLatest = Nothing
  , tcbRef    = Nothing
  }

[hyperspace|
actor TerrainCacheBroker
  state TerrainCacheBrokerState
  lifetime Singleton
  schedule pinned 4
  noDeps

  reply TerrainCacheResultReply

  mailbox Unbounded

  cast terrainCacheResult :: TerrainCacheBuildResult
  cast setRef :: TerrainCacheRef
  call latest :: () -> Maybe TerrainCacheBuildResult

  initial emptyTerrainCacheBrokerState
  on_ terrainCacheResult = \result st -> do
    let st' = st { tcbLatest = Just result }
    publishResult st' result
    pure st'
  on_ setRef = \ref st -> pure (st { tcbRef = Just ref })
  onPure latest = \() st -> (st, tcbLatest st)
|]

-- | Write the latest result to the shared IORef, if registered.
publishResult :: TerrainCacheBrokerState -> TerrainCacheBuildResult -> IO ()
publishResult st result =
  case tcbRef st of
    Nothing  -> pure ()
    Just ref -> writeIORef ref (Just result)

-- | Enqueue a terrain cache result for render-loop consumption.
enqueueTerrainCacheResult
  :: ActorHandle TerrainCacheBroker (Protocol TerrainCacheBroker)
  -> TerrainCacheBuildResult
  -> IO ()
enqueueTerrainCacheResult handle result =
  cast @"terrainCacheResult" handle #terrainCacheResult result

-- | Fetch the most recent terrain cache result (synchronous, for tests).
getLatestTerrainCacheResult
  :: ActorHandle TerrainCacheBroker (Protocol TerrainCacheBroker)
  -> IO (Maybe TerrainCacheBuildResult)
getLatestTerrainCacheResult handle =
  call @"latest" handle #latest ()

-- | Register a shared IORef for lock-free publishing.
setTerrainCacheRef
  :: ActorHandle TerrainCacheBroker (Protocol TerrainCacheBroker)
  -> TerrainCacheRef
  -> IO ()
setTerrainCacheRef handle ref =
  cast @"setRef" handle #setRef ref

-- | Read the latest terrain cache result from the shared IORef (lock-free).
readTerrainCacheRef :: TerrainCacheRef -> IO (Maybe TerrainCacheBuildResult)
readTerrainCacheRef = readIORef
