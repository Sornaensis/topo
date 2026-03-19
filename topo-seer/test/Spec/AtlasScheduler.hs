module Spec.AtlasScheduler (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import System.Timeout (timeout)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager (AtlasJob(..), atlasManagerActorDef, enqueueAtlasBuild)
import Actor.AtlasResultBroker (newAtlasResultRef)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleRef
  , AtlasScheduleReport(..)
  , newAtlasScheduleRef
  , readAtlasScheduleRef
  )
import Actor.AtlasScheduler
  ( AtlasScheduleRequest(..)
  , AtlasSchedulerHandles(..)
  , atlasSchedulerActorDef
  , requestAtlasSchedule
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (atlasWorkerActorDef)
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (ViewMode(..), emptyUiState)
import Hyperspace.Actor
  ( ActorSystem
  , getSingleton
  , newActorSystem
  , shutdownActorSystem
  )

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "AtlasScheduler" $ do
  it "reports drained job counts" $ withSystem $ \system -> do
    managerHandle <- getSingleton system atlasManagerActorDef
    workerHandle <- getSingleton system atlasWorkerActorDef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    schedulerHandle <- getSingleton system atlasSchedulerActorDef
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorker = workerHandle
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      }
    let terrainSnap = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
        atlasKey = AtlasKey ViewElevation 0.5 (tsVersion terrainSnap)
        job = AtlasJob
          { ajKey = atlasKey
          , ajViewMode = ViewElevation
          , ajWaterLevel = 0.5
          , ajTerrain = terrainSnap
          , ajScale = 1
          }
    enqueueAtlasBuild managerHandle job
    let snapshot = RenderSnapshot
          { rsUi = emptyUiState
          , rsLog = LogSnapshot [] False 0 LogDebug
          , rsData = DataSnapshot 0 0 Nothing
          , rsTerrain = terrainSnap
          }
        version = SnapshotVersion 1
    requestAtlasSchedule schedulerHandle AtlasScheduleRequest
      { asqSnapshotVersion = version
      , asqRenderTargetOk = True
      , asqDataReady = True
      , asqSnapshot = snapshot
      }
    report <- awaitReport scheduleRef version
    asrJobCount report `shouldBe` 1

awaitReport
  :: AtlasScheduleRef
  -> SnapshotVersion
  -> IO AtlasScheduleReport
awaitReport scheduleRef version = do
  let timeoutUs = 500000
  result <- timeout timeoutUs (pollUntil version)
  case result of
    Nothing -> expectationFailure "Timed out waiting for atlas schedule report" >> pure fallback
    Just report -> pure report
  where
    pollDelayUs = 1000
    pollUntil target = do
      mbReport <- readAtlasScheduleRef scheduleRef
      case mbReport of
        Just report | asrSnapshotVersion report == target -> pure report
        _ -> do
          threadDelay pollDelayUs
          pollUntil target
    fallback = AtlasScheduleReport
      { asrSnapshotVersion = version
      , asrJobCount = 0
      , asrDrainMs = 0
      , asrEnqueueMs = 0
      }
