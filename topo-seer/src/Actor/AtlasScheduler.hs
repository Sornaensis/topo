{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Worker manager for atlas scheduling, delegating jobs to the worker pool.
module Actor.AtlasScheduler
  ( AtlasScheduler
  , AtlasSchedulerHandles(..)
  , AtlasScheduleRequest(..)
  , atlasSchedulerActorDef
  , setAtlasSchedulerHandles
  , requestAtlasSchedule
  ) where

import Actor.AtlasManager (AtlasJob(..), AtlasManager, drainAtlasJobs)
import Actor.AtlasResultBroker (AtlasResultBroker)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleBroker
  , AtlasScheduleReport(..)
  , updateAtlasScheduleReport
  )
import Actor.AtlasWorker (AtlasBuild(..), AtlasWorker, enqueueAtlasBuildWork)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (UiState(..))
import Control.Monad (forM_)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

-- | Handles required by the atlas scheduler.
data AtlasSchedulerHandles = AtlasSchedulerHandles
  { ashManager :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , ashWorker :: !(ActorHandle AtlasWorker (Protocol AtlasWorker))
  , ashResultBroker :: !(ActorHandle AtlasResultBroker (Protocol AtlasResultBroker))
  , ashScheduleBroker :: !(ActorHandle AtlasScheduleBroker (Protocol AtlasScheduleBroker))
  }

-- | Request to schedule atlas work.
data AtlasScheduleRequest = AtlasScheduleRequest
  { asqSnapshotVersion :: !SnapshotVersion
  , asqRenderTargetOk :: !Bool
  , asqDataReady :: !Bool
  , asqSnapshot :: !RenderSnapshot
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
  cast schedule :: AtlasScheduleRequest

  initial emptyAtlasSchedulerState
  onPure_ setHandles = \handles _st -> AtlasSchedulerState { assHandles = Just handles }
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
      shouldSchedule = asqRenderTargetOk req
        && asqDataReady req
        && not (uiGenerating (rsUi snapshot))
  if shouldSchedule
    then do
      (jobs, drainMs) <- timedMs (drainAtlasJobs (ashManager handles))
      (_, enqueueMs) <- timedMs $
        forM_ jobs $ \job ->
          enqueueAtlasBuildWork (ashWorker handles) AtlasBuild
            { abKey = ajKey job
            , abViewMode = ajViewMode job
            , abWaterLevel = ajWaterLevel job
            , abTerrain = ajTerrain job
            , abScale = ajScale job
            , abResultBroker = ashResultBroker handles
            }
      let report = AtlasScheduleReport
            { asrSnapshotVersion = asqSnapshotVersion req
            , asrJobCount = length jobs
            , asrDrainMs = drainMs
            , asrEnqueueMs = enqueueMs
            }
      updateAtlasScheduleReport (ashScheduleBroker handles) report
    else do
      let report = AtlasScheduleReport
            { asrSnapshotVersion = asqSnapshotVersion req
            , asrJobCount = 0
            , asrDrainMs = 0
            , asrEnqueueMs = 0
            }
      updateAtlasScheduleReport (ashScheduleBroker handles) report

nsToMs :: Word64 -> Word64 -> Word32
nsToMs start end =
  fromIntegral ((end - start) `div` 1000000)

timedMs :: IO a -> IO (a, Word32)
timedMs action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure (result, nsToMs start end)
