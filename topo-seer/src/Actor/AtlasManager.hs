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
  , AtlasFreshDrainRequest(..)
  , AtlasFreshDrainStats(..)
  , AtlasQueuedTarget(..)
  , atlasJobsForSelection
  , atlasJobsForSelectionTransition
  , atlasJobsForKeys
  , AtlasManagerQueueRef
  , AtlasManagerQueueState(..)
  , emptyAtlasManagerQueueState
  , newAtlasManagerQueueRef
  , atlasManagerQueuedState
  , atlasManagerQueuedCount
  , atlasManagerQueuedRevision
  , atlasManagerHasQueuedWorkFor
  , formatAtlasManagerQueueState
  , atlasManagerActorDef
  , setAtlasManagerFreshnessRef
  , setAtlasManagerQueueRef
  , enqueueAtlasBuild
  , drainAtlasJobs
  , drainFreshAtlasJobs
  , drainFreshAtlasJobsLimited
  , drainFreshAtlasJobsLimitedWithStats
  , inspectFreshAtlasJobs
  ) where

import Actor.AtlasCache (AtlasKey, atlasKeyDataVersion, atlasKeySelectionTag, atlasKeyVersion, atlasKeyViewMode, atlasKeyWaterLevel, atlasKeysForSelection)
import Actor.AtlasFreshness (AtlasFreshness(..), AtlasFreshnessRef, writeAtlasFreshnessBuild)
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildTarget(..))
import Actor.Data (TerrainGeoContext, TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (LayeredViewState, ViewMode(..))
import Control.Monad (when)
import Data.List (intercalate)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Seer.Render.Viewport (AtlasViewportCoverage, atlasViewportCoverageCovers)
import Seer.Render.ZoomStage (ZoomStage(..))


data AtlasJob = AtlasJob
  { ajKey        :: !AtlasKey
  , ajViewMode   :: !ViewMode
  , ajViewSelection :: !LayeredViewState
  , ajWaterLevel :: !Float
  , ajSnapshotVersion :: !SnapshotVersion
  , ajTerrain    :: !TerrainSnapshot
  , ajHexRadius  :: !Int
  , ajAtlasScale :: !Int
  , ajViewportCoverage :: !(Maybe AtlasViewportCoverage)
  , ajForceRebuild :: !Bool
  }

data AtlasDispatchJob = AtlasDispatchJob
  { adjBuildId :: !AtlasBuildId
  , adjJob :: !AtlasJob
  }

-- | Request for a freshness-filtered manager drain that respects scheduler
-- capacity.  Undispatched fresh jobs remain queued so saturated workers do not
-- inherit unbounded backlog from repeated schedule passes.
data AtlasFreshDrainRequest = AtlasFreshDrainRequest
  { afdrFreshness :: !AtlasFreshness
  , afdrLimit :: !Int
  , afdrPreferredTarget :: !(Maybe (Int, Int))
  , afdrDispatchCoverage :: !(Maybe AtlasViewportCoverage)
  }

-- | Non-mutating diagnostics produced from the same freshness filter used by
-- scheduler drains.
data AtlasFreshDrainStats = AtlasFreshDrainStats
  { afdsJobsAvailable :: !Int
  , afdsJobsDispatched :: !Int
  , afdsJobsDeferred :: !Int
  , afdsJobsDroppedStale :: !Int
  , afdsCurrentStageDispatches :: !Int
  , afdsBackfillDispatches :: !Int
  } deriving (Eq, Show)

data AtlasJobSlot = AtlasJobSlot !ViewMode !Float !Int !Int
  deriving (Eq, Ord, Show)

data AtlasAcceptedJob = AtlasAcceptedJob
  { aajBuildId :: !AtlasBuildId
  , aajKey :: !AtlasKey
  , aajKeyVersion :: !Word64
  , aajKeyDataVersion :: !Word64
  , aajKeySelectionTag :: !Word64
  , aajSnapshotVersion :: !SnapshotVersion
  , aajGeoContext :: !TerrainGeoContext
  , aajCoverage :: !(Maybe AtlasViewportCoverage)
  }

-- | Per-slot target details for queued manager work.  The boolean is true for
-- jobs that carry current viewport coverage, which identifies the visible-stage
-- refresh path separately from non-visible backfill work.
data AtlasQueuedTarget = AtlasQueuedTarget
  { aqtBuildId :: !AtlasBuildId
  , aqtViewMode :: !ViewMode
  , aqtWaterLevel :: !Float
  , aqtKeyVersion :: !Word64
  , aqtSnapshotVersion :: !SnapshotVersion
  , aqtHexRadius :: !Int
  , aqtAtlasScale :: !Int
  , aqtCurrentStageVisible :: !Bool
  } deriving (Eq, Show)

-- | Non-destructive summary of manager queue state for render-loop wakeups and
-- trace diagnostics.
data AtlasManagerQueueState = AtlasManagerQueueState
  { amqsQueuedCount :: !Int
  , amqsQueuedRevision :: !Word64
  , amqsQueuedByKey :: !(Map.Map AtlasKey Int)
  , amqsQueuedTargets :: ![AtlasQueuedTarget]
  , amqsLatestAcceptedBuildId :: !(Maybe AtlasBuildId)
  , amqsStaleEnqueueDrops :: !Int
  , amqsDuplicateEnqueueDrops :: !Int
  , amqsLatestWinsPrunes :: !Int
  } deriving (Eq, Show)

type AtlasManagerQueueRef = IORef AtlasManagerQueueState

emptyAtlasManagerQueueState :: AtlasManagerQueueState
emptyAtlasManagerQueueState = AtlasManagerQueueState
  { amqsQueuedCount = 0
  , amqsQueuedRevision = 0
  , amqsQueuedByKey = Map.empty
  , amqsQueuedTargets = []
  , amqsLatestAcceptedBuildId = Nothing
  , amqsStaleEnqueueDrops = 0
  , amqsDuplicateEnqueueDrops = 0
  , amqsLatestWinsPrunes = 0
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

formatAtlasManagerQueueState :: AtlasManagerQueueState -> String
formatAtlasManagerQueueState state =
  "managerQueued=" <> show (amqsQueuedCount state)
    <> " managerQueueRev=" <> show (amqsQueuedRevision state)
    <> " managerLatestBuild=" <> maybe "none" show (amqsLatestAcceptedBuildId state)
    <> " managerStaleDrops=" <> show (amqsStaleEnqueueDrops state)
    <> " managerDuplicateDrops=" <> show (amqsDuplicateEnqueueDrops state)
    <> " managerLatestWinsPrunes=" <> show (amqsLatestWinsPrunes state)
    <> " managerTargets=" <> formatQueuedTargets (amqsQueuedTargets state)

formatQueuedTargets :: [AtlasQueuedTarget] -> String
formatQueuedTargets [] = "none"
formatQueuedTargets targets = intercalate ";" (map formatQueuedTarget targets)

formatQueuedTarget :: AtlasQueuedTarget -> String
formatQueuedTarget target =
  "build=" <> show (aqtBuildId target)
    <> "/mode=" <> show (aqtViewMode target)
    <> "/water=" <> show (aqtWaterLevel target)
    <> "/keyVer=" <> show (aqtKeyVersion target)
    <> "/snap=" <> show (aqtSnapshotVersion target)
    <> "/hex=" <> show (aqtHexRadius target)
    <> "/scale=" <> show (aqtAtlasScale target)
    <> "/visible=" <> show (aqtCurrentStageVisible target)

data AtlasManagerState = AtlasManagerState
  { amKey :: !(Maybe AtlasKey)
  , amQueue :: ![AtlasDispatchJob]
  , amFreshnessRef :: !(Maybe AtlasFreshnessRef)
  , amQueueRef :: !(Maybe AtlasManagerQueueRef)
  , amQueueRevision :: !Word64
  , amLatestAccepted :: !(Map.Map AtlasJobSlot AtlasAcceptedJob)
  , amQueuedBuildIds :: !(Map.Map AtlasJobSlot AtlasBuildId)
  , amDispatchedBuildIds :: !(Map.Map AtlasJobSlot AtlasBuildId)
  , amLatestAcceptedBuildId :: !(Maybe AtlasBuildId)
  , amStaleEnqueueDrops :: !Int
  , amDuplicateEnqueueDrops :: !Int
  , amLatestWinsPrunes :: !Int
  , amNextBuildId :: !Word64
  }

emptyAtlasManagerState :: AtlasManagerState
emptyAtlasManagerState = AtlasManagerState
  { amKey = Nothing
  , amQueue = []
  , amFreshnessRef = Nothing
  , amQueueRef = Nothing
  , amQueueRevision = 0
  , amLatestAccepted = Map.empty
  , amQueuedBuildIds = Map.empty
  , amDispatchedBuildIds = Map.empty
  , amLatestAcceptedBuildId = Nothing
  , amStaleEnqueueDrops = 0
  , amDuplicateEnqueueDrops = 0
  , amLatestWinsPrunes = 0
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

-- | Build atlas jobs for all split layer keys needed by a selection.
--
-- Stages are the outer loop so the current visible stage remains ahead of
-- backfill stages even when a selection has both base and overlay work.
atlasJobsForSelection
  :: SnapshotVersion
  -> LayeredViewState
  -> Float
  -> TerrainSnapshot
  -> [ZoomStage]
  -> Maybe AtlasViewportCoverage
  -> [AtlasJob]
atlasJobsForSelection snapshotVersion selection waterLevel terrainSnap =
  atlasJobsForKeys snapshotVersion selection waterLevel terrainSnap (atlasKeysForSelection selection waterLevel terrainSnap)

-- | Build jobs only for keys introduced by a view-selection transition.
-- This keeps overlay/weather UI changes from re-enqueueing unchanged base work.
atlasJobsForSelectionTransition
  :: SnapshotVersion
  -> LayeredViewState
  -> Float
  -> LayeredViewState
  -> Float
  -> TerrainSnapshot
  -> [ZoomStage]
  -> Maybe AtlasViewportCoverage
  -> [AtlasJob]
atlasJobsForSelectionTransition snapshotVersion oldSelection oldWaterLevel newSelection newWaterLevel terrainSnap =
  let oldKeys = atlasKeysForSelection oldSelection oldWaterLevel terrainSnap
      newKeys = atlasKeysForSelection newSelection newWaterLevel terrainSnap
      changedKeys = filter (`notElem` oldKeys) newKeys
  in atlasJobsForKeys snapshotVersion newSelection newWaterLevel terrainSnap changedKeys

atlasJobsForKeys
  :: SnapshotVersion
  -> LayeredViewState
  -> Float
  -> TerrainSnapshot
  -> [AtlasKey]
  -> [ZoomStage]
  -> Maybe AtlasViewportCoverage
  -> [AtlasJob]
atlasJobsForKeys snapshotVersion selection _waterLevel terrainSnap keys stages viewportCoverage =
  [ AtlasJob
      { ajKey = key
      , ajViewMode = atlasKeyViewMode key
      , ajViewSelection = selection
      , ajWaterLevel = atlasKeyWaterLevel key
      , ajSnapshotVersion = snapshotVersion
      , ajTerrain = terrainSnap
      , ajHexRadius = zsHexRadius stage
      , ajAtlasScale = zsAtlasScale stage
      , ajViewportCoverage = viewportCoverage
      , ajForceRebuild = False
      }
  | stage <- stages
  , key <- keys
  ]

atlasJobAccepted :: AtlasBuildId -> AtlasJob -> AtlasAcceptedJob
atlasJobAccepted buildId job = AtlasAcceptedJob
  { aajBuildId = buildId
  , aajKey = ajKey job
  , aajKeyVersion = atlasKeyVersion (ajKey job)
  , aajKeyDataVersion = atlasKeyDataVersion (ajKey job)
  , aajKeySelectionTag = atlasKeySelectionTag (ajKey job)
  , aajSnapshotVersion = ajSnapshotVersion job
  , aajGeoContext = tsGeoContext (ajTerrain job)
  , aajCoverage = ajViewportCoverage job
  }

acceptedVersionStamp :: AtlasAcceptedJob -> (Word64, SnapshotVersion)
acceptedVersionStamp accepted = (aajKeyDataVersion accepted, aajSnapshotVersion accepted)

jobVersionStamp :: AtlasJob -> (Word64, SnapshotVersion)
jobVersionStamp job = (atlasKeyDataVersion (ajKey job), ajSnapshotVersion job)

acceptedGeoMatchesJob :: AtlasAcceptedJob -> AtlasJob -> Bool
acceptedGeoMatchesJob accepted job = aajGeoContext accepted == tsGeoContext (ajTerrain job)

acceptedCoversJob :: AtlasAcceptedJob -> AtlasJob -> Bool
acceptedCoversJob accepted job =
  aajKey accepted == ajKey job
    && acceptedGeoMatchesJob accepted job
    && case (aajCoverage accepted, ajViewportCoverage job) of
      (Just covered, Just required) -> atlasViewportCoverageCovers covered required
      _ -> False

sameStampRedundant :: AtlasAcceptedJob -> AtlasJob -> Bool
sameStampRedundant accepted job =
  aajKey accepted == ajKey job
    && acceptedGeoMatchesJob accepted job
    && case (aajCoverage accepted, ajViewportCoverage job) of
      (Just covered, Just required) -> atlasViewportCoverageCovers covered required
      (Nothing, Nothing) -> True
      _ -> False

data AtlasEnqueueDropReason = AtlasEnqueueStale | AtlasEnqueueDuplicate
  deriving (Eq, Show)

enqueueDropReason :: Bool -> AtlasAcceptedJob -> AtlasJob -> Maybe AtlasEnqueueDropReason
enqueueDropReason queuedForSlot accepted job =
  let acceptedStamp = acceptedVersionStamp accepted
      jobStamp = jobVersionStamp job
      sameSelection = aajKeySelectionTag accepted == atlasKeySelectionTag (ajKey job)
  in if sameSelection && acceptedStamp > jobStamp
    then Just AtlasEnqueueStale
    else if ajForceRebuild job
      then Nothing
      else if sameSelection && sameStampRedundant accepted job && (not queuedForSlot || acceptedStamp == jobStamp)
        then Just AtlasEnqueueDuplicate
        else if sameSelection && queuedForSlot && acceptedStamp == jobStamp && acceptedCoversJob accepted job
          then Just AtlasEnqueueDuplicate
          else Nothing

restampDispatchJob :: SnapshotVersion -> Maybe AtlasViewportCoverage -> AtlasDispatchJob -> AtlasDispatchJob
restampDispatchJob snapshotVersion mbDispatchCoverage dispatchJob = dispatchJob
  { adjJob = (adjJob dispatchJob)
      { ajSnapshotVersion = snapshotVersion
      , ajViewportCoverage = case mbDispatchCoverage of
          Just coverage -> Just coverage
          Nothing -> ajViewportCoverage (adjJob dispatchJob)
      }
  }

publishDispatchFreshness :: AtlasFreshnessRef -> [AtlasDispatchJob] -> IO ()
publishDispatchFreshness ref dispatchJobs = mapM_ publishBuild dispatchJobs
  where
    publishBuild dispatchJob =
      writeAtlasFreshnessBuild ref (atlasJobTarget (adjJob dispatchJob)) (adjBuildId dispatchJob)

queuedTargetFromDispatch :: AtlasDispatchJob -> AtlasQueuedTarget
queuedTargetFromDispatch dispatchJob =
  let job = adjJob dispatchJob
  in AtlasQueuedTarget
    { aqtBuildId = adjBuildId dispatchJob
    , aqtViewMode = ajViewMode job
    , aqtWaterLevel = ajWaterLevel job
    , aqtKeyVersion = atlasKeyVersion (ajKey job)
    , aqtSnapshotVersion = ajSnapshotVersion job
    , aqtHexRadius = ajHexRadius job
    , aqtAtlasScale = ajAtlasScale job
    , aqtCurrentStageVisible = maybe False (const True) (ajViewportCoverage job)
    }

publishQueueState :: AtlasManagerState -> IO ()
publishQueueState st = case amQueueRef st of
  Nothing -> pure ()
  Just ref ->
    writeIORef ref AtlasManagerQueueState
      { amqsQueuedCount = length (amQueue st)
      , amqsQueuedRevision = amQueueRevision st
      , amqsQueuedByKey = Map.fromListWith (+)
          [ (ajKey (adjJob dispatchJob), 1 :: Int)
          | dispatchJob <- amQueue st
          ]
      , amqsQueuedTargets = map queuedTargetFromDispatch (amQueue st)
      , amqsLatestAcceptedBuildId = amLatestAcceptedBuildId st
      , amqsStaleEnqueueDrops = amStaleEnqueueDrops st
      , amqsDuplicateEnqueueDrops = amDuplicateEnqueueDrops st
      , amqsLatestWinsPrunes = amLatestWinsPrunes st
      }

queueSignature :: [AtlasDispatchJob] -> [(AtlasBuildId, AtlasKey, SnapshotVersion, AtlasJobSlot)]
queueSignature = map $ \dispatchJob ->
  let job = adjJob dispatchJob
  in (adjBuildId dispatchJob, ajKey job, ajSnapshotVersion job, atlasJobSlot job)

queuedBuildIdsFor :: [AtlasDispatchJob] -> Map.Map AtlasJobSlot AtlasBuildId
queuedBuildIdsFor queue = Map.fromList
  [ (atlasJobSlot (adjJob dispatchJob), adjBuildId dispatchJob)
  | dispatchJob <- queue
  ]

prioritizeFreshJobs :: Maybe (Int, Int) -> [AtlasDispatchJob] -> [AtlasDispatchJob]
prioritizeFreshJobs Nothing jobs = jobs
prioritizeFreshJobs (Just (preferredHex, preferredScale)) jobs =
  let isPreferred dispatchJob =
        let job = adjJob dispatchJob
        in ajHexRadius job == preferredHex && ajAtlasScale job == preferredScale
  in filter isPreferred jobs <> filter (not . isPreferred) jobs

latestDispatchableBySlot :: [AtlasDispatchJob] -> [AtlasDispatchJob]
latestDispatchableBySlot jobs =
  let latestIds = Map.fromListWith max
        [ (atlasJobSlot (adjJob dispatchJob), adjBuildId dispatchJob)
        | dispatchJob <- jobs
        ]
  in filter (\dispatchJob -> Map.lookup (atlasJobSlot (adjJob dispatchJob)) latestIds == Just (adjBuildId dispatchJob)) jobs

droppedUndispatchedSlots :: [AtlasDispatchJob] -> [AtlasDispatchJob] -> [AtlasDispatchJob] -> [AtlasJobSlot]
droppedUndispatchedSlots oldQueue fresh queue' =
  let freshIds = Set.fromList (map adjBuildId fresh)
      queuedIds = Set.fromList (map adjBuildId queue')
  in [ atlasJobSlot (adjJob dispatchJob)
     | dispatchJob <- oldQueue
     , not (Set.member (adjBuildId dispatchJob) freshIds)
     , not (Set.member (adjBuildId dispatchJob) queuedIds)
     ]

selectFreshDrain
  :: AtlasFreshDrainRequest
  -> [AtlasDispatchJob]
  -> ([AtlasDispatchJob], [AtlasDispatchJob], AtlasFreshDrainStats)
selectFreshDrain req queue =
  let freshness = afdrFreshness req
      key = afKey freshness
      requestKeyVersion = atlasKeyDataVersion key
      requestSnapshotVersion = afSnapshotVersion freshness
      safeLimit = max 0 (afdrLimit req)
      sameKeyFamily job =
        ajViewMode job == atlasKeyViewMode key
          && ajWaterLevel job == atlasKeyWaterLevel key
          && atlasKeySelectionTag (ajKey job) == atlasKeySelectionTag key
      isDispatchable queued =
        let job = adjJob queued
        in ajKey job == key && ajSnapshotVersion job <= requestSnapshotVersion
      isFuture queued =
        let job = adjJob queued
        in sameKeyFamily job
          && (ajSnapshotVersion job > requestSnapshotVersion || atlasKeyDataVersion (ajKey job) > requestKeyVersion)
      isStaleForRequest queued =
        let job = adjJob queued
        in ajSnapshotVersion job < requestSnapshotVersion || atlasKeyDataVersion (ajKey job) < requestKeyVersion
      hasFutureWork = any isFuture queue
      dispatchable = if hasFutureWork
        then []
        else latestDispatchableBySlot (filter isDispatchable queue)
      selected = take safeLimit (prioritizeFreshJobs (afdrPreferredTarget req) dispatchable)
      dispatchableIds = Set.fromList (map adjBuildId dispatchable)
      selectedIds = Set.fromList (map adjBuildId selected)
      deferredIds = dispatchableIds `Set.difference` selectedIds
      shouldKeep queued
        | Set.member (adjBuildId queued) selectedIds = False
        | isDispatchable queued = not hasFutureWork && not (isStaleForRequest queued) && Set.member (adjBuildId queued) deferredIds
        | sameKeyFamily (adjJob queued) = isFuture queued
        | otherwise = True
      queue' = filter shouldKeep queue
      droppedCount = length queue - length selected - length queue'
      currentStageDispatches = length (filter (dispatchMatchesPreferred (afdrPreferredTarget req)) selected)
      stats = AtlasFreshDrainStats
        { afdsJobsAvailable = length dispatchable
        , afdsJobsDispatched = length selected
        , afdsJobsDeferred = Set.size deferredIds
        , afdsJobsDroppedStale = max 0 droppedCount
        , afdsCurrentStageDispatches = currentStageDispatches
        , afdsBackfillDispatches = length selected - currentStageDispatches
        }
  in (map (restampDispatchJob requestSnapshotVersion (afdrDispatchCoverage req)) selected, queue', stats)

inspectFreshDrain :: AtlasFreshDrainRequest -> [AtlasDispatchJob] -> AtlasFreshDrainStats
inspectFreshDrain req queue =
  let (_, _, stats) = selectFreshDrain req { afdrLimit = 0 } queue
  in stats { afdsJobsDroppedStale = 0 }

dispatchMatchesPreferred :: Maybe (Int, Int) -> AtlasDispatchJob -> Bool
dispatchMatchesPreferred Nothing _ = False
dispatchMatchesPreferred (Just (preferredHex, preferredScale)) dispatchJob =
  let job = adjJob dispatchJob
  in ajHexRadius job == preferredHex && ajAtlasScale job == preferredScale

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
  call drainFreshJobsLimited :: AtlasFreshDrainRequest -> [AtlasDispatchJob]
  call drainFreshJobsLimitedWithStats :: AtlasFreshDrainRequest -> ([AtlasDispatchJob], AtlasFreshDrainStats)
  call inspectFreshJobs :: AtlasFreshDrainRequest -> AtlasFreshDrainStats

  initial emptyAtlasManagerState
  onPure_ setFreshnessRef = \ref st -> st { amFreshnessRef = Just ref }
  on_ setQueueRef = \ref st -> do
    let st' = st { amQueueRef = Just ref }
    publishQueueState st'
    pure st'
  on_ enqueue = \job st -> do
    -- Latest-wins per view/water/scale slot: a newer terrain/layer version
    -- for the same atlas slot replaces older queued or already-dispatched
    -- work. Equal-version jobs with identical/equivalent viewport coverage
    -- are treated as duplicates, avoiding authoritative build-id churn while
    -- the current complete atlas is still usable.
    let slot = atlasJobSlot job
        queuedForSlot = Map.member slot (amQueuedBuildIds st)
        dropReason = Map.lookup slot (amLatestAccepted st) >>= \accepted -> enqueueDropReason queuedForSlot accepted job
    case dropReason of
      Just AtlasEnqueueStale -> do
        let st' = st { amStaleEnqueueDrops = amStaleEnqueueDrops st + 1 }
        publishQueueState st'
        pure st'
      Just AtlasEnqueueDuplicate -> do
        let st' = st { amDuplicateEnqueueDrops = amDuplicateEnqueueDrops st + 1 }
        publishQueueState st'
        pure st'
      Nothing -> do
        let buildId = AtlasBuildId (amNextBuildId st)
            dispatchJob = AtlasDispatchJob buildId job
        case amFreshnessRef st of
          Just ref -> writeAtlasFreshnessBuild ref (atlasJobTarget job) buildId
          Nothing -> pure ()
        let pruned = filter (\queued -> atlasJobSlot (adjJob queued) /= slot) (amQueue st)
            prunedCount = length (amQueue st) - length pruned
            queue' = pruned ++ [dispatchJob]
            queueRevision' = amQueueRevision st + 1
            latestAccepted' = Map.insert slot (atlasJobAccepted buildId job) (amLatestAccepted st)
            queuedBuildIds' = Map.insert slot buildId (amQueuedBuildIds st)
            st' = st
              { amKey = Just (ajKey job)
              , amQueue = queue'
              , amQueueRevision = queueRevision'
              , amLatestAccepted = latestAccepted'
              , amQueuedBuildIds = queuedBuildIds'
              , amLatestAcceptedBuildId = Just buildId
              , amLatestWinsPrunes = amLatestWinsPrunes st + prunedCount
              , amNextBuildId = amNextBuildId st + 1
              }
        publishQueueState st'
        pure st'
  on drainJobs = \() st -> do
    let queue = amQueue st
        queueRevision' = if null queue then amQueueRevision st else amQueueRevision st + 1
        dispatchedBuildIds' = foldr
          (\dispatchJob buildIds -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (adjBuildId dispatchJob) buildIds)
          (amDispatchedBuildIds st)
          queue
        st' = st
          { amQueue = []
          , amQueueRevision = queueRevision'
          , amQueuedBuildIds = Map.empty
          , amDispatchedBuildIds = dispatchedBuildIds'
          }
    when (not (null queue)) $
      publishQueueState st'
    pure (st', map adjJob queue)
  on drainFreshJobs = \freshness st -> do
    let request = AtlasFreshDrainRequest
          { afdrFreshness = freshness
          , afdrLimit = length (amQueue st)
          , afdrPreferredTarget = Nothing
          , afdrDispatchCoverage = Nothing
          }
        (fresh, queue', _stats) = selectFreshDrain request (amQueue st)
        queueChanged = queueSignature queue' /= queueSignature (amQueue st)
        queueRevision' = if queueChanged then amQueueRevision st + 1 else amQueueRevision st
        droppedSlots = droppedUndispatchedSlots (amQueue st) fresh queue'
        latestAcceptedBase = foldr Map.delete (amLatestAccepted st) droppedSlots
        latestAccepted' = foldr
          (\dispatchJob accepted -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (atlasJobAccepted (adjBuildId dispatchJob) (adjJob dispatchJob)) accepted)
          latestAcceptedBase
          fresh
        dispatchedBuildIds' = foldr
          (\dispatchJob buildIds -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (adjBuildId dispatchJob) buildIds)
          (amDispatchedBuildIds st)
          fresh
        st' = st
          { amQueue = queue'
          , amQueueRevision = queueRevision'
          , amLatestAccepted = latestAccepted'
          , amQueuedBuildIds = queuedBuildIdsFor queue'
          , amDispatchedBuildIds = dispatchedBuildIds'
          }
    case amFreshnessRef st of
      Just ref | not (null fresh) -> publishDispatchFreshness ref fresh
      _ -> pure ()
    when queueChanged $
      publishQueueState st'
    pure (st', fresh)
  on drainFreshJobsLimited = \request st -> do
    let (fresh, queue', _stats) = selectFreshDrain request (amQueue st)
        queueChanged = queueSignature queue' /= queueSignature (amQueue st)
        queueRevision' = if queueChanged then amQueueRevision st + 1 else amQueueRevision st
        droppedSlots = droppedUndispatchedSlots (amQueue st) fresh queue'
        latestAcceptedBase = foldr Map.delete (amLatestAccepted st) droppedSlots
        latestAccepted' = foldr
          (\dispatchJob accepted -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (atlasJobAccepted (adjBuildId dispatchJob) (adjJob dispatchJob)) accepted)
          latestAcceptedBase
          fresh
        dispatchedBuildIds' = foldr
          (\dispatchJob buildIds -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (adjBuildId dispatchJob) buildIds)
          (amDispatchedBuildIds st)
          fresh
        st' = st
          { amQueue = queue'
          , amQueueRevision = queueRevision'
          , amLatestAccepted = latestAccepted'
          , amQueuedBuildIds = queuedBuildIdsFor queue'
          , amDispatchedBuildIds = dispatchedBuildIds'
          }
    case amFreshnessRef st of
      Just ref | not (null fresh) -> publishDispatchFreshness ref fresh
      _ -> pure ()
    when queueChanged $
      publishQueueState st'
    pure (st', fresh)
  on drainFreshJobsLimitedWithStats = \request st -> do
    let (fresh, queue', stats) = selectFreshDrain request (amQueue st)
        queueChanged = queueSignature queue' /= queueSignature (amQueue st)
        queueRevision' = if queueChanged then amQueueRevision st + 1 else amQueueRevision st
        droppedSlots = droppedUndispatchedSlots (amQueue st) fresh queue'
        latestAcceptedBase = foldr Map.delete (amLatestAccepted st) droppedSlots
        latestAccepted' = foldr
          (\dispatchJob accepted -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (atlasJobAccepted (adjBuildId dispatchJob) (adjJob dispatchJob)) accepted)
          latestAcceptedBase
          fresh
        dispatchedBuildIds' = foldr
          (\dispatchJob buildIds -> Map.insert (atlasJobSlot (adjJob dispatchJob)) (adjBuildId dispatchJob) buildIds)
          (amDispatchedBuildIds st)
          fresh
        st' = st
          { amQueue = queue'
          , amQueueRevision = queueRevision'
          , amLatestAccepted = latestAccepted'
          , amQueuedBuildIds = queuedBuildIdsFor queue'
          , amDispatchedBuildIds = dispatchedBuildIds'
          }
    case amFreshnessRef st of
      Just ref | not (null fresh) -> publishDispatchFreshness ref fresh
      _ -> pure ()
    when queueChanged $
      publishQueueState st'
    pure (st', (fresh, stats))
  onPure inspectFreshJobs = \request st -> (st, inspectFreshDrain request (amQueue st))
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

drainFreshAtlasJobsLimited :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshDrainRequest -> IO [AtlasDispatchJob]
drainFreshAtlasJobsLimited handle request =
  call @"drainFreshJobsLimited" handle #drainFreshJobsLimited request

drainFreshAtlasJobsLimitedWithStats :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshDrainRequest -> IO ([AtlasDispatchJob], AtlasFreshDrainStats)
drainFreshAtlasJobsLimitedWithStats handle request =
  call @"drainFreshJobsLimitedWithStats" handle #drainFreshJobsLimitedWithStats request

inspectFreshAtlasJobs :: ActorHandle AtlasManager (Protocol AtlasManager) -> AtlasFreshDrainRequest -> IO AtlasFreshDrainStats
inspectFreshAtlasJobs handle request =
  call @"inspectFreshJobs" handle #inspectFreshJobs request
