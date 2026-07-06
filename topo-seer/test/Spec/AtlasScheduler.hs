{-# LANGUAGE TypeApplications #-}

module Spec.AtlasScheduler (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (newIORef)
import System.Timeout (timeout)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..), atlasKeyVersion)
import Actor.AtlasManager (AtlasManager, AtlasJob(..), enqueueAtlasBuild, drainAtlasJobs)
import Actor.AtlasResult (AtlasBuildId(..))
import Actor.AtlasResultBroker (newAtlasResultRef)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleRef
  , AtlasScheduleReport(..)
  , newAtlasScheduleRef
  , readAtlasScheduleRef
  )
import Actor.AtlasScheduler
  ( AtlasFreshness(..)
  , AtlasScheduleRequest(..)
  , AtlasScheduler
  , AtlasSchedulerHandles(..)
  , newAtlasFreshnessRef
  , requestAtlasSchedule
  , setAtlasSchedulerHandles
  , writeAtlasFreshness
  )
import Actor.AtlasWorker (AtlasBuild(..), atlasBuildIsCurrent, atlasWorkerActorDef)
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Seer.Render.ZoomStage (allZoomStages, ZoomStage(..))
import Data.Word (Word64)
import Hyperspace.Actor
  ( ActorSystem
  , get
  , newActorSystem
  , shutdownActorSystem
  , spawnActor
  )

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

defaultWaterLevel :: Float
defaultWaterLevel = uiRenderWaterLevel emptyUiState

emptyTerrainSnapshotWithVersion :: Word64 -> TerrainSnapshot
emptyTerrainSnapshotWithVersion version =
  TerrainSnapshot version 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore

atlasJobFor :: ViewMode -> Word64 -> ZoomStage -> AtlasJob
atlasJobFor mode version stage =
  let terrainSnap = emptyTerrainSnapshotWithVersion version
      atlasKey = AtlasKey mode defaultWaterLevel version
  in AtlasJob
    { ajKey = atlasKey
    , ajViewMode = mode
    , ajWaterLevel = defaultWaterLevel
    , ajSnapshotVersion = SnapshotVersion version
    , ajTerrain = terrainSnap
    , ajHexRadius = zsHexRadius stage
    , ajAtlasScale = zsAtlasScale stage
    }

spec :: Spec
spec = describe "AtlasScheduler" $ do
  it "reports drained job counts" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    let terrainSnap = emptyTerrainSnapshotWithVersion 0
        version = SnapshotVersion 1
        atlasKey = AtlasKey ViewElevation defaultWaterLevel (tsVersion terrainSnap)
        job = AtlasJob
          { ajKey = atlasKey
          , ajViewMode = ViewElevation
          , ajWaterLevel = defaultWaterLevel
          , ajSnapshotVersion = version
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
    managerHandle <- get @AtlasManager system
    let terrainSnap = emptyTerrainSnapshotWithVersion 0
        mkJob stage =
          let atlasKey = AtlasKey ViewElevation defaultWaterLevel (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = ViewElevation
            , ajWaterLevel = defaultWaterLevel
            , ajSnapshotVersion = SnapshotVersion 0
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
    managerHandle <- get @AtlasManager system
    let terrainSnap = emptyTerrainSnapshotWithVersion 0
        mkJobFor mode stage =
          let atlasKey = AtlasKey mode defaultWaterLevel (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = mode
            , ajWaterLevel = defaultWaterLevel
            , ajSnapshotVersion = SnapshotVersion 0
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
    managerHandle <- get @AtlasManager system
    w1 <- spawnActor atlasWorkerActorDef
    w2 <- spawnActor atlasWorkerActorDef
    w3 <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [w1, w2, w3]
      , ashWorkerNext = workerNextRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    let terrainSnap = emptyTerrainSnapshotWithVersion 0
        mkJob i =
          let atlasKey = AtlasKey ViewElevation defaultWaterLevel (tsVersion terrainSnap)
          in AtlasJob
            { ajKey = atlasKey
            , ajViewMode = ViewElevation
            , ajWaterLevel = defaultWaterLevel
            , ajSnapshotVersion = SnapshotVersion 1
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

  it "keeps only the latest queued atlas version per stage" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let enqueueVersion version =
          mapM_ (enqueueAtlasBuild managerHandle . atlasJobFor ViewElevation version) allZoomStages
    mapM_ enqueueVersion [1..20 :: Word64]
    enqueueVersion 10
    threadDelay 10000
    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` length allZoomStages
    all ((== 20) . atlasKeyVersion . ajKey) jobs `shouldBe` True

  it "ignores delayed older atlas versions after newer jobs were drained" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let enqueueVersion version =
          mapM_ (enqueueAtlasBuild managerHandle . atlasJobFor ViewElevation version) allZoomStages
    enqueueVersion 20
    threadDelay 10000
    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` length allZoomStages
    all ((== 20) . atlasKeyVersion . ajKey) jobs `shouldBe` True
    enqueueVersion 10
    threadDelay 10000
    staleJobs <- drainAtlasJobs managerHandle
    length staleJobs `shouldBe` 0

  it "stale scheduler requests do not drain newer same-key queued work" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    case allZoomStages of
      [] -> expectationFailure "expected at least one zoom stage"
      currentStage:_ -> do
        let terrainSnap = emptyTerrainSnapshotWithVersion 1
            newerJob = (atlasJobFor ViewElevation 1 currentStage) { ajSnapshotVersion = SnapshotVersion 2 }
            snapshot = RenderSnapshot
              { rsUi = emptyUiState
              , rsLog = LogSnapshot [] False 0 LogDebug
              , rsData = DataSnapshot 0 0 Nothing
              , rsTerrain = terrainSnap
              }
        enqueueAtlasBuild managerHandle newerJob
        threadDelay 10000
        requestAtlasSchedule schedulerHandle AtlasScheduleRequest
          { asqSnapshotVersion = SnapshotVersion 1
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          }
        staleReport <- awaitReport scheduleRef (SnapshotVersion 1)
        asrJobCount staleReport `shouldBe` 0
        requestAtlasSchedule schedulerHandle AtlasScheduleRequest
          { asqSnapshotVersion = SnapshotVersion 2
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          }
        currentReport <- awaitReport scheduleRef (SnapshotVersion 2)
        asrJobCount currentReport `shouldBe` 1

  it "drops queued jobs that do not match the current render key" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    case allZoomStages of
      [] -> expectationFailure "expected at least one zoom stage"
      currentStage:_ -> do
        let terrainSnap = emptyTerrainSnapshotWithVersion 7
            staleJob = atlasJobFor ViewBiome 7 currentStage
            currentJob = (atlasJobFor ViewElevation 7 currentStage) { ajTerrain = terrainSnap }
        enqueueAtlasBuild managerHandle staleJob
        enqueueAtlasBuild managerHandle currentJob
        threadDelay 10000
        let snapshot = RenderSnapshot
              { rsUi = emptyUiState
              , rsLog = LogSnapshot [] False 0 LogDebug
              , rsData = DataSnapshot 0 0 Nothing
              , rsTerrain = terrainSnap
              }
            version = SnapshotVersion 7
        requestAtlasSchedule schedulerHandle AtlasScheduleRequest
          { asqSnapshotVersion = version
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          }
        report <- awaitReport scheduleRef version
        asrJobCount report `shouldBe` 1
        leftovers <- drainAtlasJobs managerHandle
        length leftovers `shouldBe` 0

  it "marks worker builds stale when the scheduler freshness key advances" $ do
    freshnessRef <- newAtlasFreshnessRef
    resultRef <- newAtlasResultRef
    let terrainSnap = emptyTerrainSnapshotWithVersion 1
        key1 = AtlasKey ViewElevation defaultWaterLevel 1
        key2 = AtlasKey ViewElevation defaultWaterLevel 2
        build = AtlasBuild
          { abBuildId = AtlasBuildId 1
          , abKey = key1
          , abViewMode = ViewElevation
          , abWaterLevel = defaultWaterLevel
          , abTerrain = terrainSnap
          , abHexRadius = 6
          , abAtlasScale = 1
          , abPanOffset = (0, 0)
          , abZoom = 1
          , abWindowSize = (800, 600)
          , abSnapshotVersion = SnapshotVersion 1
          , abResultRef = resultRef
          , abFreshnessRef = freshnessRef
          , abDayNightFn = Nothing
          }
    writeAtlasFreshness freshnessRef AtlasFreshness
      { afKey = key1
      , afSnapshotVersion = SnapshotVersion 1
      , afLatestBuildIds = mempty
      }
    atlasBuildIsCurrent build `shouldReturn` True
    writeAtlasFreshness freshnessRef AtlasFreshness
      { afKey = key1
      , afSnapshotVersion = SnapshotVersion 2
      , afLatestBuildIds = mempty
      }
    atlasBuildIsCurrent build `shouldReturn` False
    writeAtlasFreshness freshnessRef AtlasFreshness
      { afKey = key2
      , afSnapshotVersion = SnapshotVersion 2
      , afLatestBuildIds = mempty
      }
    atlasBuildIsCurrent build `shouldReturn` False

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
