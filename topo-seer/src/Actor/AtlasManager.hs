{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Actor.AtlasManager
  ( AtlasManager
  , AtlasJob(..)
  , atlasManagerActorDef
  , enqueueAtlasBuild
  , setAtlasCache
  , drainAtlasJobs
  ) where

import Actor.AtlasCache (AtlasCache, AtlasKey(..), setAtlasKey)
import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)


data AtlasJob = AtlasJob
  { ajKey        :: !AtlasKey
  , ajViewMode   :: !ViewMode
  , ajWaterLevel :: !Float
  , ajTerrain    :: !TerrainSnapshot
  , ajHexRadius  :: !Int
  , ajAtlasScale :: !Int
  }


data AtlasManagerState = AtlasManagerState
  { amCache :: !(Maybe (ActorHandle AtlasCache (Protocol AtlasCache)))
  , amKey :: !(Maybe AtlasKey)
  , amQueue :: ![AtlasJob]
  }

emptyAtlasManagerState :: AtlasManagerState
emptyAtlasManagerState = AtlasManagerState
  { amCache = Nothing
  , amKey = Nothing
  , amQueue = []
  }

[hyperspace|
actor AtlasManager
  state AtlasManagerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast setCache :: (ActorHandle AtlasCache (Protocol AtlasCache))
  cast enqueue :: AtlasJob
  call drainJobs :: () -> [AtlasJob]

  initial emptyAtlasManagerState
  on_ setCache = \cache st -> do
    case amKey st of
      Just key -> setAtlasKey cache key
      Nothing -> pure ()
    pure st { amCache = Just cache }
  on_ enqueue = \job st -> do
    -- With multi-key caching, different keys coexist in the cache.
    -- Do not clear the queue or notify the actor-side cache on key change.
    -- Prune by (key, hexRadius) to deduplicate within the same view mode.
    let pruned = filter (\j -> not (ajKey j == ajKey job && ajHexRadius j == ajHexRadius job)) (amQueue st)
    pure st { amKey = Just (ajKey job), amQueue = pruned ++ [job] }
  onPure drainJobs = \() st -> (st { amQueue = [] }, amQueue st)
|]

setAtlasCache :: ActorHandle AtlasManager (Protocol AtlasManager) -> ActorHandle AtlasCache (Protocol AtlasCache) -> IO ()
setAtlasCache handle cache =
  cast @"setCache" handle #setCache cache

enqueueAtlasBuild :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasJob -> IO ()
enqueueAtlasBuild handle job =
  cast @"enqueue" handle #enqueue job

drainAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> IO [AtlasJob]
drainAtlasJobs handle =
  call @"drainJobs" handle #drainJobs ()
