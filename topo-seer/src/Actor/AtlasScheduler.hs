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
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessKey
  ) where

import Actor.AtlasCache (atlasKeyFor)
import Actor.AtlasFreshness
  ( AtlasFreshness(..)
  , AtlasFreshnessRef
  , emptyAtlasFreshness
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessKey
  )
import Actor.AtlasManager (AtlasJob(..), AtlasDispatchJob(..), AtlasManager, drainFreshAtlasJobs)
import Actor.AtlasResultBroker (AtlasResultRef)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleRef
  , AtlasScheduleReport(..)
  , writeAtlasScheduleReport
  )
import Actor.AtlasWorker (AtlasBuild(..), AtlasWorker, enqueueAtlasBuildWork)
import Actor.Data (TerrainSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (UiState(..))
import Control.Monad (forM_)
import Data.IORef (IORef, atomicModifyIORef')
import UI.DayNight (mkDayNightFn)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Seer.Timing (timedMs)

-- | Handles required by the atlas scheduler.
data AtlasSchedulerHandles = AtlasSchedulerHandles
  { ashManager :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , ashWorkers :: ![ActorHandle AtlasWorker (Protocol AtlasWorker)]
  , ashWorkerNext :: !(IORef Int)
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

runSchedule :: AtlasSchedulerHandles -> AtlasScheduleRequest -> IO ()
runSchedule handles req = do
  let snapshot = asqSnapshot req
      uiSnap = rsUi snapshot
      terrainSnap = rsTerrain snapshot
      currentKey = atlasKeyFor (uiViewMode uiSnap) (uiRenderWaterLevel uiSnap) terrainSnap
      shouldSchedule = asqRenderTargetOk req
        && asqDataReady req
        && not (uiGenerating uiSnap)
      workers = ashWorkers handles
      workerCount = length workers
  if shouldSchedule && workerCount > 0
    then do
      let currentFreshness = emptyAtlasFreshness currentKey (asqSnapshotVersion req)
      (jobs, drainMs) <- timedMs (drainFreshAtlasJobs (ashManager handles) currentFreshness)
      (_, enqueueMs) <- timedMs $
        forM_ jobs $ \dispatchJob -> do
          idx <- atomicModifyIORef' (ashWorkerNext handles) (\i -> (i + 1, i))
          let worker = workers !! (idx `mod` workerCount)
              job = adjJob dispatchJob
          enqueueAtlasBuildWork worker AtlasBuild
            { abBuildId   = adjBuildId dispatchJob
            , abKey        = ajKey job
            , abViewMode   = ajViewMode job
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
            , abDayNightFn = if uiDayNightEnabled uiSnap
                              then mkDayNightFn uiSnap (tsChunkSize (ajTerrain job))
                              else Nothing
            }
      let report = AtlasScheduleReport
            { asrSnapshotVersion = asqSnapshotVersion req
            , asrJobCount = length jobs
            , asrDrainMs = drainMs
            , asrEnqueueMs = enqueueMs
            }
      writeAtlasScheduleReport (ashScheduleRef handles) report
    else do
      writeAtlasFreshnessKey (ashFreshnessRef handles) currentKey
      let report = AtlasScheduleReport
            { asrSnapshotVersion = asqSnapshotVersion req
            , asrJobCount = 0
            , asrDrainMs = 0
            , asrEnqueueMs = 0
            }
      writeAtlasScheduleReport (ashScheduleRef handles) report
