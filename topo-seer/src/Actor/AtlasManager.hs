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
  { ajKey :: !AtlasKey
  , ajViewMode :: !ViewMode
  , ajWaterLevel :: !Float
  , ajTerrain :: !TerrainSnapshot
  , ajScale :: !Int
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
    st' <- if amKey st /= Just (ajKey job)
      then do
        case amCache st of
          Just cache -> setAtlasKey cache (ajKey job)
          Nothing -> pure ()
        pure st { amKey = Just (ajKey job), amQueue = [] }
      else pure st
    let pruned = filter ((/= ajScale job) . ajScale) (amQueue st')
    pure st' { amQueue = pruned ++ [job] }
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
