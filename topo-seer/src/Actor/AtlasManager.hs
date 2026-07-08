{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Actor.AtlasManager
  ( AtlasManager
  , AtlasJob(..)
  , AtlasDispatchJob(..)
  , AtlasManagerQueueRef
  , AtlasManagerQueueState(..)
  , newAtlasManagerQueueRef
  , atlasManagerQueuedState
  , atlasManagerQueuedCount
  , atlasManagerQueuedRevision
  , atlasManagerHasQueuedWorkFor
  , atlasManagerActorDef
  , setAtlasManagerFreshnessRef
  , setAtlasManagerQueueRef
  , enqueueAtlasBuild
  , drainAtlasJobs
  , drainFreshAtlasJobs
  ) where

import Actor.AtlasCache (AtlasKey, atlasKeyVersion)
import Actor.AtlasFreshness (AtlasFreshness(..), AtlasFreshnessRef, writeAtlasFreshnessBuild, writeAtlasFreshnessCurrent)
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildTarget(..))
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (ViewMode(..))
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

data AtlasDispatchJob = AtlasDispatchJob
  { adjBuildId :: !AtlasBuildId
  , adjJob :: !AtlasJob
  }


data AtlasJobSlot = AtlasJobSlot !ViewMode !Float !Int !Int
  deriving (Eq, Ord, Show)

-- | Non-destructive summary of manager queue state for render-loop wakeups.
data AtlasManagerQueueState = AtlasManagerQueueState
  { amqsQueuedCount :: !Int
  , amqsQueuedRevision :: !Word64
  , amqsQueuedByKey :: !(Map.Map AtlasKey Int)
  } deriving (Eq, Show)

type AtlasManagerQueueRef = IORef AtlasManagerQueueState

emptyAtlasManagerQueueState :: AtlasManagerQueueState
emptyAtlasManagerQueueState = AtlasManagerQueueState
  { amqsQueuedCount = 0
  , amqsQueuedRevision = 0
  , amqsQueuedByKey = Map.empty
  }

newAtlasManagerQueueRef :: IO AtlasManagerQueueRef
newAtlasManagerQueueRef = newIORef emptyAtlasManagerQueueState

atlasManagerQueuedState :: AtlasManagerQueueRef -> IO AtlasManagerQueueState
atlasManagerQueuedState = readIORef

atlasManagerQueuedCount :: AtlasManagerQueueRef -> IO Int
atlasManagerQueuedCount ref = amqsQueuedCount <$> readIORef ref

atlasManagerQueuedRevision :: AtlasManagerQueueRef -> IO Word64
atlasManagerQueuedRevision ref = amqsQueuedRevision <$> readIORef ref

atlasManagerHasQueuedWorkFor :: AtlasManagerQueueRef -> AtlasKey -> IO Bool
atlasManagerHasQueuedWorkFor ref key = do
  summary <- readIORef ref
  pure $ maybe False (> 0) (Map.lookup key (amqsQueuedByKey summary))

data AtlasManagerState = AtlasManagerState
  { amKey :: !(Maybe AtlasKey)
  , amQueue :: ![AtlasDispatchJob]
  , amFreshnessRef :: !(Maybe AtlasFreshnessRef)
  , amQueueRef :: !(Maybe AtlasManagerQueueRef)
  , amQueueRevision :: !Word64
  , amLatestVersions :: !(Map.Map AtlasJobSlot (Word64, SnapshotVersion))
  , amNextBuildId :: !Word64
  }

emptyAtlasManagerState :: AtlasManagerState
emptyAtlasManagerState = AtlasManagerState
  { amKey = Nothing
  , amQueue = []
  , amFreshnessRef = Nothing
  , amQueueRef = Nothing
  , amQueueRevision = 0
  , amLatestVersions = Map.empty
  , amNextBuildId = 1
  }

atlasJobSlot :: AtlasJob -> AtlasJobSlot
atlasJobSlot job = AtlasJobSlot (ajViewMode job) (ajWaterLevel job) (ajHexRadius job) (ajAtlasScale job)

atlasJobTarget :: AtlasJob -> AtlasBuildTarget
atlasJobTarget job = AtlasBuildTarget
  { abtKey = ajKey job
  , abtSnapshotVersion = ajSnapshotVersion job
  , abtHexRadius = ajHexRadius job
  , abtAtlasScale = ajAtlasScale job
  }

restampDispatchJob :: SnapshotVersion -> AtlasDispatchJob -> AtlasDispatchJob
restampDispatchJob snapshotVersion dispatchJob = dispatchJob
  { adjJob = (adjJob dispatchJob) { ajSnapshotVersion = snapshotVersion }
  }

publishDispatchFreshness :: AtlasFreshnessRef -> AtlasKey -> SnapshotVersion -> [AtlasDispatchJob] -> IO ()
publishDispatchFreshness ref key snapshotVersion dispatchJobs = do
  writeAtlasFreshnessCurrent ref key snapshotVersion
  mapM_ publishBuild dispatchJobs
  where
    publishBuild dispatchJob =
      writeAtlasFreshnessBuild ref (atlasJobTarget (adjJob dispatchJob)) (adjBuildId dispatchJob)

publishQueueState :: Maybe AtlasManagerQueueRef -> Word64 -> [AtlasDispatchJob] -> IO ()
publishQueueState Nothing _ _ = pure ()
publishQueueState (Just ref) revision queue =
  writeIORef ref AtlasManagerQueueState
    { amqsQueuedCount = length queue
    , amqsQueuedRevision = revision
    , amqsQueuedByKey = Map.fromListWith (+)
        [ (ajKey (adjJob dispatchJob), 1 :: Int)
        | dispatchJob <- queue
        ]
    }

queueSignature :: [AtlasDispatchJob] -> [(AtlasBuildId, AtlasKey, SnapshotVersion, AtlasJobSlot)]
queueSignature = map $ \dispatchJob ->
  let job = adjJob dispatchJob
  in (adjBuildId dispatchJob, ajKey job, ajSnapshotVersion job, atlasJobSlot job)

[hyperspace|
actor AtlasManager
  state AtlasManagerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast setFreshnessRef :: AtlasFreshnessRef
  cast setQueueRef :: AtlasManagerQueueRef
  cast enqueue :: AtlasJob
  call drainJobs :: () -> [AtlasJob]
  call drainFreshJobs :: AtlasFreshness -> [AtlasDispatchJob]

  initial emptyAtlasManagerState
  onPure_ setFreshnessRef = \ref st -> st { amFreshnessRef = Just ref }
  on_ setQueueRef = \ref st -> do
    publishQueueState (Just ref) (amQueueRevision st) (amQueue st)
    pure st { amQueueRef = Just ref }
  on_ enqueue = \job st -> do
    -- Latest-wins per view/water/scale slot: a newer terrain/layer version
    -- for the same atlas slot replaces older queued or already-dispatched
    -- work. Equal-version jobs still replace older viewport/pan work.  The
    -- accepted build id is published immediately so same-key obsolete viewport
    -- results cannot promote before the replacement is dispatched.
    let slot = atlasJobSlot job
        jobStamp = (atlasKeyVersion (ajKey job), ajSnapshotVersion job)
        hasNewerAccepted = maybe False (> jobStamp) (Map.lookup slot (amLatestVersions st))
    if hasNewerAccepted
      then pure st
      else do
        let buildId = AtlasBuildId (amNextBuildId st)
            dispatchJob = AtlasDispatchJob buildId job
        case amFreshnessRef st of
          Just ref -> writeAtlasFreshnessBuild ref (atlasJobTarget job) buildId
          Nothing -> pure ()
        let pruned = filter (\queued -> atlasJobSlot (adjJob queued) /= slot) (amQueue st)
            queue' = pruned ++ [dispatchJob]
            queueRevision' = amQueueRevision st + 1
            latestVersions' = Map.insert slot jobStamp (amLatestVersions st)
        publishQueueState (amQueueRef st) queueRevision' queue'
        pure st
          { amKey = Just (ajKey job)
          , amQueue = queue'
          , amQueueRevision = queueRevision'
          , amLatestVersions = latestVersions'
          , amNextBuildId = amNextBuildId st + 1
          }
  on drainJobs = \() st -> do
    let queue = amQueue st
        queueRevision' = if null queue then amQueueRevision st else amQueueRevision st + 1
    when (not (null queue)) $
      publishQueueState (amQueueRef st) queueRevision' []
    pure (st { amQueue = [], amQueueRevision = queueRevision' }, map adjJob queue)
  on drainFreshJobs = \freshness st -> do
    let key = afKey freshness
        requestKeyVersion = atlasKeyVersion key
        requestSnapshotVersion = afSnapshotVersion freshness
        isDispatchable queued =
          let job = adjJob queued
          in ajKey job == key && ajSnapshotVersion job <= requestSnapshotVersion
        keepQueued queued =
          let job = adjJob queued
          in not (isDispatchable queued)
            && (ajSnapshotVersion job > requestSnapshotVersion || atlasKeyVersion (ajKey job) > requestKeyVersion)
        fresh = map (restampDispatchJob requestSnapshotVersion) (filter isDispatchable (amQueue st))
        queue' = filter keepQueued (amQueue st)
        acceptedStamp = (requestKeyVersion, requestSnapshotVersion)
        latestVersions' = foldr
          (\dispatchJob versions -> Map.insert (atlasJobSlot (adjJob dispatchJob)) acceptedStamp versions)
          (amLatestVersions st)
          fresh
        queueChanged = queueSignature queue' /= queueSignature (amQueue st)
        queueRevision' = if queueChanged then amQueueRevision st + 1 else amQueueRevision st
    case amFreshnessRef st of
      Just ref | not (null fresh) -> publishDispatchFreshness ref key requestSnapshotVersion fresh
      _ -> pure ()
    when queueChanged $
      publishQueueState (amQueueRef st) queueRevision' queue'
    pure (st { amQueue = queue', amQueueRevision = queueRevision', amLatestVersions = latestVersions' }, fresh)
|]

setAtlasManagerFreshnessRef :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshnessRef -> IO ()
setAtlasManagerFreshnessRef handle ref =
  cast @"setFreshnessRef" handle #setFreshnessRef ref

setAtlasManagerQueueRef :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasManagerQueueRef -> IO ()
setAtlasManagerQueueRef handle ref =
  cast @"setQueueRef" handle #setQueueRef ref

enqueueAtlasBuild :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasJob -> IO ()
enqueueAtlasBuild handle job =
  cast @"enqueue" handle #enqueue job

drainAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> IO [AtlasJob]
drainAtlasJobs handle =
  call @"drainJobs" handle #drainJobs ()

drainFreshAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshness -> IO [AtlasDispatchJob]
drainFreshAtlasJobs handle freshness =
  call @"drainFreshJobs" handle #drainFreshJobs freshness
