{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Worker manager for atlas scheduling, delegating jobs to the worker pool.
module Actor.AtlasScheduler
  ( AtlasScheduler
  , AtlasFreshness(..)
  , AtlasFreshnessRef
  , AtlasSchedulerHandles(..)
  , AtlasScheduleRequest(..)
  , atlasSchedulerActorDef
  , setAtlasSchedulerHandles
  , atlasSchedulerConfigured
  , requestAtlasSchedule
  , atlasSchedulerDayNightSpec
  , atlasViewportRefreshJob
  , atlasViewportRefreshJobs
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessCurrentKeys
  , writeAtlasFreshnessKey
  , atlasKeyIsCurrent
  , atlasTargetBuildIsFresh
  ) where

import Actor.AtlasCache (AtlasKey, atlasKeyIsBase, atlasKeysForSelection)
import Actor.AtlasFreshness
  ( AtlasFreshness(..)
  , AtlasFreshnessRef
  , emptyAtlasFreshness
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessCurrentKeys
  , writeAtlasFreshnessKey
  , atlasKeyIsCurrent
  , atlasTargetBuildIsFresh
  )
import Actor.AtlasManager
  ( AtlasJob(..)
  , AtlasDispatchJob(..)
  , AtlasFreshDrainRequest(..)
  , AtlasFreshDrainStats(..)
  , AtlasManager
  , atlasJobsForSelection
  , drainFreshAtlasJobsLimitedWithStats
  , enqueueAtlasBuild
  , inspectFreshAtlasJobs
  )
import Actor.AtlasResultBroker (AtlasResultRef)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleRef
  , AtlasScheduleReport(..)
  , emptyAtlasScheduleReport
  , writeAtlasScheduleReport
  )
import Actor.AtlasWorker
  ( AtlasBuild(..)
  , AtlasWorker
  , AtlasWorkerLoad(..)
  , AtlasWorkerLoadRef
  , atlasWorkerLoadStart
  , enqueueAtlasBuildWork
  , readAtlasWorkerLoad
  )
import Actor.Data (TerrainSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (UiState(..), effectiveViewSelection)
import Control.Monad (forM_)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Word (Word32)
import UI.DayNight (DayNightSpec, mkDayNightSpec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Seer.Render.Viewport (AtlasViewportCoverage, currentAtlasViewportCoverage)
import Seer.Render.ZoomStage (ZoomStage(..), stageForZoom)
import Seer.Timing (timedMs)
import Topo (WorldConfig(..))

-- | Handles required by the atlas scheduler.
data AtlasSchedulerHandles = AtlasSchedulerHandles
  { ashManager :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , ashWorkers :: ![ActorHandle AtlasWorker (Protocol AtlasWorker)]
  , ashWorkerNext :: !(IORef Int)
  , ashWorkerLoadRef :: !AtlasWorkerLoadRef
  , ashResultRef :: !AtlasResultRef
  , ashScheduleRef :: !AtlasScheduleRef
  , ashFreshnessRef :: !AtlasFreshnessRef
  }

-- | Request to schedule atlas work.
data AtlasScheduleRequest = AtlasScheduleRequest
  { asqSnapshotVersion :: !SnapshotVersion
  , asqRenderTargetOk :: !Bool
  , asqDataReady :: !Bool
  , asqSnapshot :: !RenderSnapshot
  , asqWindowSize :: !(Int, Int)
  , asqRefreshCurrentViewport :: !Bool
  , asqRefreshStage :: !(Maybe ZoomStage)
  }

newtype AtlasSchedulerState = AtlasSchedulerState
  { assHandles :: Maybe AtlasSchedulerHandles
  }

emptyAtlasSchedulerState :: AtlasSchedulerState
emptyAtlasSchedulerState = AtlasSchedulerState
  { assHandles = Nothing
  }

[hyperspace|
actor AtlasScheduler
  state AtlasSchedulerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast setHandles :: AtlasSchedulerHandles
  call configured :: () -> Bool
  cast schedule :: AtlasScheduleRequest

  initial emptyAtlasSchedulerState
  onPure_ setHandles = \handles _st -> AtlasSchedulerState { assHandles = Just handles }
  onPure configured = \() st -> (st, maybe False (const True) (assHandles st))
  on_ schedule = \req st -> do
    case assHandles st of
      Nothing -> pure st
      Just handles -> do
        runSchedule handles req
        pure st
|]

-- | Register dependent actor handles used by the scheduler.
setAtlasSchedulerHandles
  :: ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasSchedulerHandles
  -> IO ()
setAtlasSchedulerHandles handle handles =
  cast @"setHandles" handle #setHandles handles

-- | Return 'True' once scheduler handles have been processed. Because this is
-- an actor call, issuing it after 'setAtlasSchedulerHandles' also acts as a
-- startup mailbox barrier for headless/test setup.
atlasSchedulerConfigured :: ActorHandle AtlasScheduler (Protocol AtlasScheduler) -> IO Bool
atlasSchedulerConfigured handle =
  call @"configured" handle #configured ()

-- | Request an atlas scheduling pass.
requestAtlasSchedule
  :: ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasScheduleRequest
  -> IO ()
requestAtlasSchedule handle req =
  cast @"schedule" handle #schedule req

atlasSchedulerDayNightSpec :: UiState -> TerrainSnapshot -> Maybe DayNightSpec
atlasSchedulerDayNightSpec ui terrain
  | uiDayNightEnabled ui = mkDayNightSpec terrain
  | otherwise = Nothing

atlasViewportRefreshJob :: SnapshotVersion -> RenderSnapshot -> ZoomStage -> AtlasJob
atlasViewportRefreshJob snapshotVersion snapshot stage =
  case atlasViewportRefreshJobs snapshotVersion snapshot stage of
    job:_ -> job
    [] -> error "atlasViewportRefreshJob: expected at least a base atlas job"

atlasViewportRefreshJobs :: SnapshotVersion -> RenderSnapshot -> ZoomStage -> [AtlasJob]
atlasViewportRefreshJobs snapshotVersion snapshot stage =
  let uiSnap = rsUi snapshot
      terrainSnap = rsTerrain snapshot
      selection = effectiveViewSelection uiSnap
  in atlasJobsForSelection
      snapshotVersion
      selection
      (uiRenderWaterLevel uiSnap)
      terrainSnap
      [stage]
      Nothing

atlasViewportCoverageFor :: RenderSnapshot -> (Int, Int) -> ZoomStage -> Maybe AtlasViewportCoverage
atlasViewportCoverageFor snapshot windowSize stage =
  let uiSnap = rsUi snapshot
      terrainSnap = rsTerrain snapshot
  in if tsChunkSize terrainSnap > 0
    then Just $
      currentAtlasViewportCoverage
        (WorldConfig { wcChunkSize = tsChunkSize terrainSnap })
        (uiPanOffset uiSnap)
        (uiZoom uiSnap)
        windowSize
        terrainSnap
        stage
    else Nothing

scheduleReportFromStats
  :: SnapshotVersion
  -> Int
  -> Int
  -> AtlasWorkerLoad
  -> AtlasFreshDrainStats
  -> Word32
  -> Word32
  -> AtlasScheduleReport
scheduleReportFromStats snapshotVersion workerCount workerCapacity workerLoad stats drainMs enqueueMs =
  AtlasScheduleReport
    { asrSnapshotVersion = snapshotVersion
    , asrJobCount = afdsJobsDispatched stats
    , asrDrainMs = drainMs
    , asrEnqueueMs = enqueueMs
    , asrJobsAvailable = afdsJobsAvailable stats
    , asrJobsDispatched = afdsJobsDispatched stats
    , asrJobsDeferred = afdsJobsDeferred stats
    , asrJobsDroppedStale = afdsJobsDroppedStale stats
    , asrCurrentStageDispatches = afdsCurrentStageDispatches stats
    , asrBackfillDispatches = afdsBackfillDispatches stats
    , asrWorkerCapacity = workerCount
    , asrWorkerAvailable = workerCapacity
    , asrWorkerInFlight = awlInFlight workerLoad
    , asrWorkerStarted = awlBuildStarted workerLoad
    , asrWorkerCompleted = awlBuildCompleted workerLoad
    , asrWorkerStaleSkippedAtStart = awlStaleSkippedAtStart workerLoad
    , asrWorkerStaleCancelledDuringGeometry = awlStaleCancelledDuringGeometry workerLoad
    , asrWorkerStaleCancelledBeforePublish = awlStaleCancelledBeforePublish workerLoad
    }

emptyFreshDrainStats :: AtlasFreshDrainStats
emptyFreshDrainStats = AtlasFreshDrainStats
  { afdsJobsAvailable = 0
  , afdsJobsDispatched = 0
  , afdsJobsDeferred = 0
  , afdsJobsDroppedStale = 0
  , afdsCurrentStageDispatches = 0
  , afdsBackfillDispatches = 0
  }

appendFreshDrainStats :: AtlasFreshDrainStats -> AtlasFreshDrainStats -> AtlasFreshDrainStats
appendFreshDrainStats a b = AtlasFreshDrainStats
  { afdsJobsAvailable = afdsJobsAvailable a + afdsJobsAvailable b
  , afdsJobsDispatched = afdsJobsDispatched a + afdsJobsDispatched b
  , afdsJobsDeferred = afdsJobsDeferred a + afdsJobsDeferred b
  , afdsJobsDroppedStale = afdsJobsDroppedStale a + afdsJobsDroppedStale b
  , afdsCurrentStageDispatches = afdsCurrentStageDispatches a + afdsCurrentStageDispatches b
  , afdsBackfillDispatches = afdsBackfillDispatches a + afdsBackfillDispatches b
  }

drainFreshForKeys
  :: AtlasSchedulerHandles
  -> [AtlasKey]
  -> SnapshotVersion
  -> Int
  -> Maybe (Int, Int)
  -> Maybe AtlasViewportCoverage
  -> IO ([AtlasDispatchJob], AtlasFreshDrainStats)
drainFreshForKeys handles keys snapshotVersion capacity preferredTarget dispatchCoverage =
  go (max 0 capacity) [] emptyFreshDrainStats keys
  where
    go _ jobs stats [] = pure (jobs, stats)
    go remaining jobs stats (key:rest)
      | remaining <= 0 = do
          inspected <- inspectFreshAtlasJobs (ashManager handles) (requestFor key 0)
          go 0 jobs (appendFreshDrainStats stats inspected) rest
      | otherwise = do
          (drained, drainedStats) <- drainFreshAtlasJobsLimitedWithStats (ashManager handles) (requestFor key remaining)
          let remaining' = max 0 (remaining - length drained)
          go remaining' (jobs <> drained) (appendFreshDrainStats stats drainedStats) rest

    requestFor key limit = AtlasFreshDrainRequest
      { afdrFreshness = emptyAtlasFreshness key snapshotVersion
      , afdrLimit = limit
      , afdrPreferredTarget = preferredTarget
      , afdrDispatchCoverage = dispatchCoverage
      }

runSchedule :: AtlasSchedulerHandles -> AtlasScheduleRequest -> IO ()
runSchedule handles req = do
  let snapshot = asqSnapshot req
      uiSnap = rsUi snapshot
      terrainSnap = rsTerrain snapshot
      selection = effectiveViewSelection uiSnap
      currentKeys = atlasKeysForSelection selection (uiRenderWaterLevel uiSnap) terrainSnap
      shouldSchedule = asqRenderTargetOk req
        && asqDataReady req
        && not (uiGenerating uiSnap)
      workers = ashWorkers handles
      workerCount = length workers
      dayNightSpec = atlasSchedulerDayNightSpec uiSnap terrainSnap
  if shouldSchedule && workerCount > 0
    then do
      let refreshStage = maybe (stageForZoom (uiZoom uiSnap)) id (asqRefreshStage req)
          dispatchCoverage = atlasViewportCoverageFor snapshot (asqWindowSize req) refreshStage
          viewportRefreshJobs =
            [ job { ajViewportCoverage = dispatchCoverage }
            | job <- atlasViewportRefreshJobs (asqSnapshotVersion req) snapshot refreshStage
            ]
          preferredTarget = Just (zsHexRadius refreshStage, zsAtlasScale refreshStage)
      workerLoadBefore <- readAtlasWorkerLoad (ashWorkerLoadRef handles)
      let workerCapacity = max 0 (workerCount - awlInFlight workerLoadBefore)
      ((jobs, drainStats), drainMs) <- timedMs $ do
        if asqRefreshCurrentViewport req
          then mapM_ (enqueueAtlasBuild (ashManager handles)) viewportRefreshJobs
          else pure ()
        drainFreshForKeys handles currentKeys (asqSnapshotVersion req) workerCapacity preferredTarget dispatchCoverage
      writeAtlasFreshnessCurrentKeys (ashFreshnessRef handles) currentKeys (asqSnapshotVersion req)
      (_, enqueueMs) <- timedMs $ do
        atlasWorkerLoadStart (ashWorkerLoadRef handles) (length jobs)
        forM_ jobs $ \dispatchJob -> do
          idx <- atomicModifyIORef' (ashWorkerNext handles) (\i -> (i + 1, i))
          let worker = workers !! (idx `mod` workerCount)
              job = adjJob dispatchJob
          enqueueAtlasBuildWork worker AtlasBuild
            { abBuildId   = adjBuildId dispatchJob
            , abKey        = ajKey job
            , abViewMode   = ajViewMode job
            , abViewSelection = ajViewSelection job
            , abWaterLevel = ajWaterLevel job
            , abTerrain    = ajTerrain job
            , abHexRadius  = ajHexRadius job
            , abAtlasScale = ajAtlasScale job
            , abPanOffset  = uiPanOffset uiSnap
            , abZoom       = uiZoom uiSnap
            , abWindowSize = asqWindowSize req
            , abSnapshotVersion = ajSnapshotVersion job
            , abResultRef  = ashResultRef handles
            , abFreshnessRef = ashFreshnessRef handles
            , abDayNightSpec = if atlasKeyIsBase (ajKey job) then dayNightSpec else Nothing
            , abWorkerLoadRef = Just (ashWorkerLoadRef handles)
            }
      let report = scheduleReportFromStats
            (asqSnapshotVersion req)
            workerCount
            workerCapacity
            workerLoadBefore
            drainStats
            drainMs
            enqueueMs
      writeAtlasScheduleReport (ashScheduleRef handles) report
    else do
      writeAtlasFreshnessCurrentKeys (ashFreshnessRef handles) currentKeys (asqSnapshotVersion req)
      workerLoad <- readAtlasWorkerLoad (ashWorkerLoadRef handles)
      let workerCapacity = max 0 (workerCount - awlInFlight workerLoad)
          report = (emptyAtlasScheduleReport (asqSnapshotVersion req))
            { asrWorkerCapacity = workerCount
            , asrWorkerAvailable = workerCapacity
            , asrWorkerInFlight = awlInFlight workerLoad
            , asrWorkerStarted = awlBuildStarted workerLoad
            , asrWorkerCompleted = awlBuildCompleted workerLoad
            , asrWorkerStaleSkippedAtStart = awlStaleSkippedAtStart workerLoad
            , asrWorkerStaleCancelledDuringGeometry = awlStaleCancelledDuringGeometry workerLoad
            , asrWorkerStaleCancelledBeforePublish = awlStaleCancelledBeforePublish workerLoad
            }
      writeAtlasScheduleReport (ashScheduleRef handles) report
