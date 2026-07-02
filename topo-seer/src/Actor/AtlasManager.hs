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
  , setAtlasManagerFreshnessRef
  , enqueueAtlasBuild
  , drainAtlasJobs
  , drainFreshAtlasJobs
  ) where

import Actor.AtlasCache (AtlasKey, atlasKeyVersion)
import Actor.AtlasFreshness (AtlasFreshness(..), AtlasFreshnessRef, writeAtlasFreshness)
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (ViewMode(..))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)


data AtlasJob = AtlasJob
  { ajKey        :: !AtlasKey
  , ajViewMode   :: !ViewMode
  , ajWaterLevel :: !Float
  , ajSnapshotVersion :: !SnapshotVersion
  , ajTerrain    :: !TerrainSnapshot
  , ajHexRadius  :: !Int
  , ajAtlasScale :: !Int
  }


data AtlasJobSlot = AtlasJobSlot !ViewMode !Float !Int !Int
  deriving (Eq, Ord, Show)

data AtlasManagerState = AtlasManagerState
  { amKey :: !(Maybe AtlasKey)
  , amQueue :: ![AtlasJob]
  , amFreshnessRef :: !(Maybe AtlasFreshnessRef)
  , amLatestVersions :: !(Map.Map AtlasJobSlot (Word64, SnapshotVersion))
  }

emptyAtlasManagerState :: AtlasManagerState
emptyAtlasManagerState = AtlasManagerState
  { amKey = Nothing
  , amQueue = []
  , amFreshnessRef = Nothing
  , amLatestVersions = Map.empty
  }

atlasJobSlot :: AtlasJob -> AtlasJobSlot
atlasJobSlot job = AtlasJobSlot (ajViewMode job) (ajWaterLevel job) (ajHexRadius job) (ajAtlasScale job)

[hyperspace|
actor AtlasManager
  state AtlasManagerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast setFreshnessRef :: AtlasFreshnessRef
  cast enqueue :: AtlasJob
  call drainJobs :: () -> [AtlasJob]
  call drainFreshJobs :: AtlasFreshness -> [AtlasJob]

  initial emptyAtlasManagerState
  onPure_ setFreshnessRef = \ref st -> st { amFreshnessRef = Just ref }
  on_ enqueue = \job st -> do
    -- Latest-wins per view/water/scale slot: a newer terrain/layer version
    -- for the same atlas slot replaces older queued or already-dispatched
    -- work. Equal-version jobs still replace older viewport/pan work.
    let slot = atlasJobSlot job
        jobStamp = (atlasKeyVersion (ajKey job), ajSnapshotVersion job)
        hasNewerAccepted = maybe False (> jobStamp) (Map.lookup slot (amLatestVersions st))
    if hasNewerAccepted
      then pure st
      else do
        case amFreshnessRef st of
          Just ref -> writeAtlasFreshness ref AtlasFreshness
            { afKey = ajKey job
            , afSnapshotVersion = ajSnapshotVersion job
            }
          Nothing -> pure ()
        let pruned = filter (\j -> atlasJobSlot j /= slot) (amQueue st)
            latestVersions' = Map.insert slot jobStamp (amLatestVersions st)
        pure st { amKey = Just (ajKey job), amQueue = pruned ++ [job], amLatestVersions = latestVersions' }
  onPure drainJobs = \() st -> (st { amQueue = [] }, amQueue st)
  on drainFreshJobs = \freshness st -> do
    let key = afKey freshness
        requestKeyVersion = atlasKeyVersion key
        requestSnapshotVersion = afSnapshotVersion freshness
        isFresh job = ajKey job == key && ajSnapshotVersion job == requestSnapshotVersion
        keepQueued job = not (isFresh job)
          && (ajSnapshotVersion job > requestSnapshotVersion || atlasKeyVersion (ajKey job) > requestKeyVersion)
        fresh = filter isFresh (amQueue st)
        queue' = filter keepQueued (amQueue st)
    case amFreshnessRef st of
      Just ref | not (null fresh) -> writeAtlasFreshness ref freshness
      _ -> pure ()
    pure (st { amQueue = queue' }, fresh)
|]

setAtlasManagerFreshnessRef :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshnessRef -> IO ()
setAtlasManagerFreshnessRef handle ref =
  cast @"setFreshnessRef" handle #setFreshnessRef ref

enqueueAtlasBuild :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasJob -> IO ()
enqueueAtlasBuild handle job =
  cast @"enqueue" handle #enqueue job

drainAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> IO [AtlasJob]
drainAtlasJobs handle =
  call @"drainJobs" handle #drainJobs ()

drainFreshAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshness -> IO [AtlasJob]
drainFreshAtlasJobs handle freshness =
  call @"drainFreshJobs" handle #drainFreshJobs freshness
