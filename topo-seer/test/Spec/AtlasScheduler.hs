module Spec.AtlasScheduler (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (newIORef)
import System.Timeout (timeout)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager (AtlasJob(..), atlasManagerActorDef, enqueueAtlasBuild, drainAtlasJobs)
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
import Seer.Render.ZoomStage (allZoomStages, ZoomStage(..))
import Hyperspace.Actor
  ( ActorSystem
  , getSingleton
  , newActorSystem
  , shutdownActorSystem
  , spawnActor
  )

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "AtlasScheduler" $ do
  it "reports drained job counts" $ withSystem $ \system -> do
    managerHandle <- getSingleton system atlasManagerActorDef
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    schedulerHandle <- getSingleton system atlasSchedulerActorDef
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
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
          , ajHexRadius  = 6
          , ajAtlasScale = 1
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
      , asqWindowSize = (800, 600)
      }
    report <- awaitReport scheduleRef version
    asrJobCount report `shouldBe` 1

  it "single-mode rebuild enqueues exactly one job per zoom stage" $ withSystem $ \system -> do
    managerHandle <- getSingleton system atlasManagerActorDef
    let terrainSnap = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
        mkJob stage =
          let atlasKey = AtlasKey ViewElevation 0.5 (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = ViewElevation
            , ajWaterLevel = 0.5
            , ajTerrain = terrainSnap
            , ajHexRadius  = zsHexRadius stage
            , ajAtlasScale = zsAtlasScale stage
            }
    -- Enqueue one job per zoom stage (single-mode rebuild)
    mapM_ (enqueueAtlasBuild managerHandle . mkJob) allZoomStages
    -- Small delay for the actor to process all messages
    threadDelay 10000
    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` length allZoomStages

  it "does not multiply jobs across view modes for single-mode rebuild" $ withSystem $ \system -> do
    managerHandle <- getSingleton system atlasManagerActorDef
    let terrainSnap = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
        mkJobFor mode stage =
          let atlasKey = AtlasKey mode 0.5 (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = mode
            , ajWaterLevel = 0.5
            , ajTerrain = terrainSnap
            , ajHexRadius  = zsHexRadius stage
            , ajAtlasScale = zsAtlasScale stage
            }
    -- Enqueue elevation only (simulating single-mode rebuild)
    mapM_ (enqueueAtlasBuild managerHandle . mkJobFor ViewElevation) allZoomStages
    threadDelay 10000
    jobs <- drainAtlasJobs managerHandle
    -- Should be exactly 5 (one per stage), NOT 80
    length jobs `shouldBe` length allZoomStages
    -- All jobs should be for the same view mode
    all (\j -> ajViewMode j == ViewElevation) jobs `shouldBe` True

  it "dispatches jobs to multiple workers via round-robin" $ withSystem $ \system -> do
    managerHandle <- getSingleton system atlasManagerActorDef
    w1 <- spawnActor atlasWorkerActorDef
    w2 <- spawnActor atlasWorkerActorDef
    w3 <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    schedulerHandle <- getSingleton system atlasSchedulerActorDef
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [w1, w2, w3]
      , ashWorkerNext = workerNextRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      }
    let terrainSnap = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
        mkJob i =
          let atlasKey = AtlasKey ViewElevation 0.5 (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = ViewElevation
            , ajWaterLevel = 0.5
            , ajTerrain = terrainSnap
            , ajHexRadius  = 6 + i
            , ajAtlasScale = 1
            }
    -- Enqueue 3 jobs so each worker gets one
    mapM_ (enqueueAtlasBuild managerHandle . mkJob) [0, 1, 2 :: Int]
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
      , asqWindowSize = (800, 600)
      }
    report <- awaitReport scheduleRef version
    -- All 3 jobs should have been dispatched
    asrJobCount report `shouldBe` 3

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
