{-# LANGUAGE TypeApplications #-}

module Spec.AtlasScheduler (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (newIORef)
import System.Timeout (timeout)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..), atlasKeyVersion)
import Actor.AtlasManager
  ( AtlasManager
  , AtlasJob(..)
  , atlasManagerQueuedCount
  , drainAtlasJobs
  , enqueueAtlasBuild
  , newAtlasManagerQueueRef
  , setAtlasManagerQueueRef
  )
import Actor.AtlasResult (AtlasBuildId(..))
import Actor.AtlasResultBroker (atlasResultsPending, newAtlasResultRef)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleRef
  , AtlasScheduleReport(..)
  , emptyAtlasScheduleReport
  , newAtlasScheduleRef
  , readAtlasScheduleRef
  )
import Actor.AtlasScheduler
  ( AtlasFreshness(..)
  , AtlasScheduleRequest(..)
  , AtlasScheduler
  , AtlasSchedulerHandles(..)
  , atlasSchedulerDayNightSpec
  , atlasViewportRefreshJob
  , newAtlasFreshnessRef
  , requestAtlasSchedule
  , setAtlasSchedulerHandles
  , writeAtlasFreshness
  )
import Actor.AtlasWorker
  ( AtlasBuild(..)
  , AtlasWorkerBuildOutcome(..)
  , AtlasWorkerLoad(..)
  , AtlasWorkerLoadRef
  , atlasBuildIsCurrent
  , atlasWorkerActorDef
  , atlasWorkerLoadFinish
  , atlasWorkerLoadFinishOutcome
  , atlasWorkerLoadStart
  , enqueueAtlasBuildWork
  , newAtlasWorkerLoadRef
  , readAtlasWorkerLoad
  )
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Seer.Render.ZoomStage (allZoomStages, ZoomStage(..), stageForZoom)
import UI.DayNight (mkDayNightKey)
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
    , ajViewportCoverage = Nothing
    }

spec :: Spec
spec = describe "AtlasScheduler" $ do
  it "reports drained job counts" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
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
          , ajViewportCoverage = Nothing
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
      , asqRefreshCurrentViewport = False
      , asqRefreshStage = Nothing
      }
    report <- awaitReport scheduleRef version
    asrJobCount report `shouldBe` 1
    asrJobsAvailable report `shouldBe` 1
    asrJobsDispatched report `shouldBe` 1
    asrJobsDeferred report `shouldBe` 0
    asrWorkerCapacity report `shouldBe` 1
    asrWorkerAvailable report `shouldBe` 1

  it "dispatches a job queued after an empty schedule pass on the next schedule request" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    case allZoomStages of
      [] -> expectationFailure "expected at least one zoom stage"
      currentStage:_ -> do
        let terrainSnap = emptyTerrainSnapshotWithVersion 1
            version = SnapshotVersion 1
            snapshot = RenderSnapshot
              { rsUi = emptyUiState
              , rsLog = LogSnapshot [] False 0 LogDebug
              , rsData = DataSnapshot 0 0 Nothing
              , rsTerrain = terrainSnap
              }
            req = AtlasScheduleRequest
              { asqSnapshotVersion = version
              , asqRenderTargetOk = True
              , asqDataReady = True
              , asqSnapshot = snapshot
              , asqWindowSize = (800, 600)
              , asqRefreshCurrentViewport = False
              , asqRefreshStage = Nothing
              }
        requestAtlasSchedule schedulerHandle req
        emptyReport <- awaitReportMatching scheduleRef version ((== 0) . asrJobCount)
        asrJobCount emptyReport `shouldBe` 0

        enqueueAtlasBuild managerHandle (atlasJobFor ViewElevation 1 currentStage)
        requestAtlasSchedule schedulerHandle req
        report <- awaitReportMatching scheduleRef version ((== 1) . asrJobCount)
        asrJobCount report `shouldBe` 1

  it "queues only the current zoom stage for a viewport coverage refresh" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    let terrainSnap = (emptyTerrainSnapshotWithVersion 3) { tsChunkSize = 16 }
        version = SnapshotVersion 3
        ui = emptyUiState { uiZoom = 1.5 }
        snapshot = RenderSnapshot
          { rsUi = ui
          , rsLog = LogSnapshot [] False 0 LogDebug
          , rsData = DataSnapshot 0 0 Nothing
          , rsTerrain = terrainSnap
          }
        requestedStage = ZoomStage 6 1 0 1
        req = AtlasScheduleRequest
          { asqSnapshotVersion = version
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          , asqRefreshCurrentViewport = True
          , asqRefreshStage = Just requestedStage
          }
    let refreshJob = atlasViewportRefreshJob version snapshot requestedStage
    (ajHexRadius refreshJob, ajAtlasScale refreshJob) `shouldBe` (6, 1)
    requestAtlasSchedule schedulerHandle req
    report <- awaitReport scheduleRef version
    threadDelay 10000
    leftovers <- drainAtlasJobs managerHandle
    asrJobCount report + length leftovers `shouldBe` 1
    map (\job -> (ajHexRadius job, ajAtlasScale job)) leftovers `shouldSatisfy` all (== (6, 1))

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
            , ajViewportCoverage = Nothing
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
            , ajViewportCoverage = Nothing
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
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [w1, w2, w3]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
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
            , ajViewportCoverage = Nothing
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
      , asqRefreshCurrentViewport = False
      , asqRefreshStage = Nothing
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
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
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
          , asqRefreshCurrentViewport = False
          , asqRefreshStage = Nothing
          }
        staleReport <- awaitReport scheduleRef (SnapshotVersion 1)
        asrJobCount staleReport `shouldBe` 0
        requestAtlasSchedule schedulerHandle AtlasScheduleRequest
          { asqSnapshotVersion = SnapshotVersion 2
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          , asqRefreshCurrentViewport = False
          , asqRefreshStage = Nothing
          }
        currentReport <- awaitReport scheduleRef (SnapshotVersion 2)
        asrJobCount currentReport `shouldBe` 1

  it "drops queued jobs that do not match the current render key" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
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
          , asqRefreshCurrentViewport = False
          , asqRefreshStage = Nothing
          }
        report <- awaitReport scheduleRef version
        asrJobCount report `shouldBe` 1
        leftovers <- drainAtlasJobs managerHandle
        length leftovers `shouldBe` 0

  it "dispatches the current-stage job before backfill when capacity is limited" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    let terrainSnap = emptyTerrainSnapshotWithVersion 9
        version = SnapshotVersion 9
        ui = emptyUiState { uiZoom = 1.5 }
        currentStage = stageForZoom (uiZoom ui)
        backfillStage = case filter ((/= (zsHexRadius currentStage, zsAtlasScale currentStage)) . zoomStagePair) allZoomStages of
          stage:_ -> stage
          [] -> currentStage { zsHexRadius = zsHexRadius currentStage + 4 }
        snapshot = RenderSnapshot
          { rsUi = ui
          , rsLog = LogSnapshot [] False 0 LogDebug
          , rsData = DataSnapshot 0 0 Nothing
          , rsTerrain = terrainSnap
          }
    enqueueAtlasBuild managerHandle (atlasJobFor ViewElevation 9 backfillStage)
    enqueueAtlasBuild managerHandle (atlasJobFor ViewElevation 9 currentStage)
    requestAtlasSchedule schedulerHandle AtlasScheduleRequest
      { asqSnapshotVersion = version
      , asqRenderTargetOk = True
      , asqDataReady = True
      , asqSnapshot = snapshot
      , asqWindowSize = (800, 600)
      , asqRefreshCurrentViewport = False
      , asqRefreshStage = Nothing
      }
    report <- awaitReport scheduleRef version
    asrJobCount report `shouldBe` 1
    asrJobsAvailable report `shouldBe` 2
    asrJobsDispatched report `shouldBe` 1
    asrJobsDeferred report `shouldBe` 1
    asrCurrentStageDispatches report `shouldBe` 1
    asrBackfillDispatches report `shouldBe` 0
    leftovers <- drainAtlasJobs managerHandle
    map atlasJobStage leftovers `shouldBe` [zoomStagePair backfillStage]

  it "keeps viewport refresh backlog bounded while worker capacity is saturated" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    workerHandle <- spawnActor atlasWorkerActorDef
    workerNextRef <- newIORef (0 :: Int)
    workerLoadRef <- newAtlasWorkerLoadRef
    resultRef <- newAtlasResultRef
    scheduleRef <- newAtlasScheduleRef
    freshnessRef <- newAtlasFreshnessRef
    schedulerHandle <- get @AtlasScheduler system
    setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
      { ashManager = managerHandle
      , ashWorkers = [workerHandle]
      , ashWorkerNext = workerNextRef
      , ashWorkerLoadRef = workerLoadRef
      , ashResultRef = resultRef
      , ashScheduleRef = scheduleRef
      , ashFreshnessRef = freshnessRef
      }
    atlasWorkerLoadStart workerLoadRef 1
    let terrainSnap = (emptyTerrainSnapshotWithVersion 10) { tsChunkSize = 16 }
        ui = emptyUiState { uiZoom = 1.5 }
        currentStage = stageForZoom (uiZoom ui)
        snapshot = RenderSnapshot
          { rsUi = ui
          , rsLog = LogSnapshot [] False 0 LogDebug
          , rsData = DataSnapshot 0 0 Nothing
          , rsTerrain = terrainSnap
          }
        mkReq version refresh = AtlasScheduleRequest
          { asqSnapshotVersion = SnapshotVersion version
          , asqRenderTargetOk = True
          , asqDataReady = True
          , asqSnapshot = snapshot
          , asqWindowSize = (800, 600)
          , asqRefreshCurrentViewport = refresh
          , asqRefreshStage = Just currentStage
          }
    mapM_ (requestAtlasSchedule schedulerHandle . (`mkReq` True)) [1..20 :: Word64]
    saturatedReport <- awaitReportMatching scheduleRef (SnapshotVersion 20) ((== 0) . asrJobCount)
    asrJobCount saturatedReport `shouldBe` 0
    asrJobsAvailable saturatedReport `shouldBe` 1
    asrJobsDeferred saturatedReport `shouldBe` 1
    asrWorkerCapacity saturatedReport `shouldBe` 1
    asrWorkerAvailable saturatedReport `shouldBe` 0
    asrWorkerInFlight saturatedReport `shouldBe` 1
    threadDelay 50000
    atlasManagerQueuedCount queueRef `shouldReturn` 1

    atlasWorkerLoadFinish (Just workerLoadRef) False
    requestAtlasSchedule schedulerHandle (mkReq 20 False)
    dispatchReport <- awaitReportMatching scheduleRef (SnapshotVersion 20) ((== 1) . asrJobCount)
    asrJobCount dispatchReport `shouldBe` 1
    asrJobsDeferred dispatchReport `shouldBe` 0
    atlasManagerQueuedCount queueRef `shouldReturn` 0

  it "stale worker messages decrement load and skip result publication" $ withSystem $ \system -> do
    workerHandle <- spawnActor atlasWorkerActorDef
    workerLoadRef <- newAtlasWorkerLoadRef
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
          , abDayNightSpec = Nothing
          , abWorkerLoadRef = Just workerLoadRef
          }
    writeAtlasFreshness freshnessRef AtlasFreshness
      { afKey = key2
      , afSnapshotVersion = SnapshotVersion 2
      , afLatestBuildIds = mempty
      }
    atlasWorkerLoadStart workerLoadRef 1
    enqueueAtlasBuildWork workerHandle build
    skipped <- awaitWorkerLoad workerLoadRef (\load -> awlInFlight load == 0 && awlStaleSkipped load == 1)
    skipped `shouldBe` True
    load <- readAtlasWorkerLoad workerLoadRef
    awlBuildStarted load `shouldBe` 1
    awlBuildCompleted load `shouldBe` 0
    awlStaleSkippedAtStart load `shouldBe` 1
    atlasResultsPending resultRef `shouldReturn` False

  it "tracks worker stale cancellation outcome counters" $ do
    workerLoadRef <- newAtlasWorkerLoadRef
    atlasWorkerLoadStart workerLoadRef 3
    atlasWorkerLoadFinishOutcome (Just workerLoadRef) AtlasWorkerBuildStaleDuringGeometry
    atlasWorkerLoadFinishOutcome (Just workerLoadRef) AtlasWorkerBuildStaleBeforePublish
    atlasWorkerLoadFinishOutcome (Just workerLoadRef) AtlasWorkerBuildCompleted
    load <- readAtlasWorkerLoad workerLoadRef
    awlInFlight load `shouldBe` 0
    awlFinished load `shouldBe` 3
    awlStaleSkipped load `shouldBe` 2
    awlBuildCompleted load `shouldBe` 1
    awlStaleCancelledDuringGeometry load `shouldBe` 1
    awlStaleCancelledBeforePublish load `shouldBe` 1

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
          , abDayNightSpec = Nothing
          , abWorkerLoadRef = Nothing
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

  it "builds a day/night spec only when enabled and terrain chunk size is available" $ do
    resultRef <- newAtlasResultRef
    freshnessRef <- newAtlasFreshnessRef
    let terrainSnap = (emptyTerrainSnapshotWithVersion 12) { tsChunkSize = 16 }
        ui = emptyUiState { uiDayNightEnabled = True, uiSimTickCount = 5 }
        expectedKey = mkDayNightKey ui (tsChunkSize terrainSnap)
        buildWith spec = AtlasBuild
          { abBuildId = AtlasBuildId 2
          , abKey = AtlasKey ViewElevation defaultWaterLevel (tsVersion terrainSnap)
          , abViewMode = ViewElevation
          , abWaterLevel = defaultWaterLevel
          , abTerrain = terrainSnap
          , abHexRadius = 6
          , abAtlasScale = 1
          , abPanOffset = (0, 0)
          , abZoom = 1
          , abWindowSize = (800, 600)
          , abSnapshotVersion = SnapshotVersion (tsVersion terrainSnap)
          , abResultRef = resultRef
          , abFreshnessRef = freshnessRef
          , abDayNightSpec = spec
          , abWorkerLoadRef = Nothing
          }
    case (expectedKey, atlasSchedulerDayNightSpec ui terrainSnap) of
      (Just key, Just (actualKey, fn)) -> do
        actualKey `shouldBe` key
        fn 3 (-1) `shouldSatisfy` (\v -> v >= 0.15 && v <= 1.0)
      _ -> expectationFailure "expected enabled day/night spec"
    fmap fst (abDayNightSpec (buildWith (atlasSchedulerDayNightSpec ui terrainSnap))) `shouldBe` expectedKey
    fmap fst (abDayNightSpec (buildWith (atlasSchedulerDayNightSpec (ui { uiDayNightEnabled = False }) terrainSnap))) `shouldBe` Nothing
    case atlasSchedulerDayNightSpec (ui { uiDayNightEnabled = False }) terrainSnap of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected disabled scheduler day/night spec to be absent"
    case atlasSchedulerDayNightSpec ui (terrainSnap { tsChunkSize = 0 }) of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected scheduler day/night spec to require terrain chunk size"

atlasJobStage :: AtlasJob -> (Int, Int)
atlasJobStage job = (ajHexRadius job, ajAtlasScale job)

zoomStagePair :: ZoomStage -> (Int, Int)
zoomStagePair stage = (zsHexRadius stage, zsAtlasScale stage)

awaitWorkerLoad :: AtlasWorkerLoadRef -> (AtlasWorkerLoad -> Bool) -> IO Bool
awaitWorkerLoad loadRef predicate = do
  result <- timeout 500000 (pollUntil 0)
  pure (maybe False id result)
  where
    pollDelayUs = 1000
    pollUntil retries = do
      load <- readAtlasWorkerLoad loadRef
      if predicate load
        then pure True
        else if retries >= (500 :: Int)
          then pure False
          else threadDelay pollDelayUs >> pollUntil (retries + 1)

awaitReport
  :: AtlasScheduleRef
  -> SnapshotVersion
  -> IO AtlasScheduleReport
awaitReport scheduleRef version =
  awaitReportMatching scheduleRef version (const True)

awaitReportMatching
  :: AtlasScheduleRef
  -> SnapshotVersion
  -> (AtlasScheduleReport -> Bool)
  -> IO AtlasScheduleReport
awaitReportMatching scheduleRef version predicate = do
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
        Just report | asrSnapshotVersion report == target && predicate report -> pure report
        _ -> do
          threadDelay pollDelayUs
          pollUntil target
    fallback = emptyAtlasScheduleReport version
