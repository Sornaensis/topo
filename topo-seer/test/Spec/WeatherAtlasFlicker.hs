{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.WeatherAtlasFlicker (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (newIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.Ptr (Ptr, intPtrToPtr)
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, get, newActorSystem, shutdownActorSystem, spawnActor)
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Test.Hspec
import Unsafe.Coerce (unsafeCoerce)

import Actor.AtlasCache
  ( AtlasKey(..)
  , atlasBaseKeyForSelection
  , atlasKeyFor
  , atlasKeyVersion
  , atlasOverlayKeyForSelection
  , terrainSnapshotViewVersion
  )
import Actor.AtlasManager (AtlasJob(..), AtlasManager, drainAtlasJobs, setAtlasManagerFreshnessRef)
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildResult(..), AtlasTileSetManifest(..), atlasManifestTarget)
import Actor.AtlasResultBroker (AtlasResultRef, atlasResultsPending, drainFreshResultsN, newAtlasResultRef, pushAtlasResult)
import Actor.AtlasScheduleBroker (newAtlasScheduleRef)
import Actor.AtlasScheduler
  ( AtlasFreshness(..)
  , AtlasFreshnessRef
  , AtlasScheduleRequest(..)
  , AtlasScheduler
  , AtlasSchedulerHandles(..)
  , atlasSchedulerConfigured
  , newAtlasFreshnessRef
  , requestAtlasSchedule
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (AtlasWorkerLoad(..), AtlasWorkerLoadRef, atlasWorkerActorDef, newAtlasWorkerLoadRef, readAtlasWorkerLoad)
import Actor.Data (Data, DataSnapshot(..), TerrainSnapshot(..), defaultTerrainGeoContext, getTerrainSnapshot, replaceTerrainData)
import Actor.Log (Log, getLogSnapshot)
import Actor.Render (RenderSnapshot(..))
import Actor.Simulation
  ( AutoTickStepResult(..)
  , Simulation
  , SimulationDagSnapshot(..)
  , autoTickStep
  , autoTickWeatherPublishIntervalNs
  , flushSimWeatherPublication
  , getSimDagSnapshot
  , setSimHandles
  , setSimWorld
  )
import Actor.SnapshotReceiver
  ( SnapshotVersion(..)
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , newDataSnapshotRef
  , newSnapshotVersionRef
  , newTerrainSnapshotRef
  , readSnapshotVersion
  , readTerrainSnapshot
  )
import Actor.UI
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , Ui
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  , getUiSnapshot
  , setUiDayNightEnabled
  , setUiSimAutoTick
  , setUiSimTickRate
  , setUiViewMode
  , setUiViewSelection
  , setUiZoom
  , uiRenderWaterLevel
  , uiSimTickCount
  , uiZoom
  )
import Seer.Render.Atlas
  ( AtlasResolveStatus(..)
  , AtlasTextureCache(..)
  , atlasResolveNeedsRetry
  , emptyAtlasTextureCache
  , getCurrentCompleteAtlasForTarget
  , resolveAtlasPureWithFreshness
  , resolveAtlasPureWithCoverage
  , setAtlasKey
  , storeAtlasTileSet
  )
import Seer.Render.Viewport (emptyAtlasViewportCoverage)
import Seer.Render.Frame
  ( AtlasFrameStepPolicy(..)
  , applyAtlasFrameStepTimestamps
  , atlasFrameStepPolicy
  , noAtlasQueuedWork
  )
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages, orderedZoomStagesForZoom, stageForZoom)
import Topo
  ( ChunkId(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , WorldConfig(..)
  , defaultHexGridMeta
  , emptyClimateChunk
  , emptyWorldWithPlanet
  , generateTerrainChunk
  , setClimateChunk
  , setTerrainChunk
  )
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), emptyOverlayStore, insertOverlay)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Weather (weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))
import UI.TerrainAtlas (AtlasChunkGeometry(..), AtlasTileGeometry(..), TerrainAtlasTile(..))
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "headless weather/cloud atlas flicker regressions" $ do
  it "keeps rapid ViewWeather auto ticks visible-stage first while constrained uploads drain the latest target" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      _ <- installWeatherWorld dataHandle simHandle
      setUiViewMode uiHandle ViewWeather
      setUiZoom uiHandle 4.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      version0 <- readSnapshotVersion snapshotVersionRef
      let waterLevel = uiRenderWaterLevel uiSnap
          currentStage = stageForZoom (uiZoom uiSnap)
          currentStagePair = zoomStagePair currentStage
          expectedBackfillStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnap))
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      version1 <- readSnapshotVersion snapshotVersionRef
      version1 `shouldSatisfy` (> version0)
      publishedSnap1 <- readTerrainSnapshot terrainSnapshotRef
      let staleWeatherKey = atlasKeyFor ViewWeather waterLevel publishedSnap1

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3
      terrainSnap3 <- getTerrainSnapshot dataHandle
      publishedSnap3 <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion terrainSnap3 `shouldSatisfy` (> tsWeatherVersion publishedSnap1)
      tsWeatherVersion publishedSnap3 `shouldBe` tsWeatherVersion publishedSnap1

      rapidJobs <- drainAtlasJobs atlasHandle
      length rapidJobs `shouldBe` 1
      map ajViewMode rapidJobs `shouldBe` [ViewWeather]
      map atlasJobStage rapidJobs `shouldBe` [currentStagePair]
      map (atlasKeyVersion . ajKey) rapidJobs `shouldBe` [tsWeatherVersion publishedSnap1]
      map ajSnapshotVersion rapidJobs `shouldBe` [version1]

      setUiSimAutoTick uiHandle False
      flushed <- flushSimWeatherPublication simHandle
      flushed `shouldBe` True
      versionAfterFlush <- readSnapshotVersion snapshotVersionRef
      versionAfterFlush `shouldSatisfy` (> version1)
      latestTerrainSnap <- getTerrainSnapshot dataHandle
      latestPublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion latestPublishedSnap `shouldBe` tsWeatherVersion latestTerrainSnap
      let latestWeatherKey = atlasKeyFor ViewWeather waterLevel latestTerrainSnap
      atlasKeyVersion latestWeatherKey `shouldBe` tsWeatherVersion latestTerrainSnap

      backfillJobs <- drainAtlasJobs atlasHandle
      length backfillJobs `shouldBe` length allZoomStages
      map ajViewMode backfillJobs `shouldBe` replicate (length allZoomStages) ViewWeather
      map atlasJobStage backfillJobs `shouldBe` expectedBackfillStages
      map ajKey backfillJobs `shouldSatisfy` all (== latestWeatherKey)
      map ajSnapshotVersion backfillJobs `shouldSatisfy` all (== versionAfterFlush)

      assertConstrainedLatestCompletion
        LatestCompletionCase
          { lccCurrentKey = latestWeatherKey
          , lccCurrentSnapshotVersion = versionAfterFlush
          , lccStaleKey = staleWeatherKey
          , lccStaleSnapshotVersion = version1
          , lccTargetHexRadius = zsHexRadius currentStage
          , lccTargetAtlasScale = zsAtlasScale currentStage
          , lccExpectedPartialStatus = PartialExact
          , lccSeedCache = (emptyAtlasTextureCache 30) { atcKey = Just latestWeatherKey }
          , lccFinalAtcLastKey = latestWeatherKey
          }

  it "keeps rapid layered current-cloud ticks on overlay keys without rebuilding the unchanged base" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      _ <- installWeatherWorld dataHandle simHandle
      let selection = defaultLayeredViewState
            { lvsBaseView = BaseViewBiome
            , lvsSkyOverlay = Just SkyOverlayCloud
            , lvsWeatherBasis = WeatherBasisCurrent
            , lvsOverlayOpacity = 0.5
            }
      setUiDayNightEnabled uiHandle False
      setUiViewSelection uiHandle selection
      setUiZoom uiHandle 4.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      uiViewSelection uiSnap `shouldBe` selection
      let waterLevel = uiRenderWaterLevel uiSnap
          currentStage = stageForZoom (uiZoom uiSnap)
          currentStagePair = zoomStagePair currentStage
          expectedBackfillStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnap))
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      baselineVersion <- readSnapshotVersion snapshotVersionRef
      baselinePublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      let baselineBaseKey = atlasBaseKeyForSelection selection waterLevel baselinePublishedSnap
      _ <- drainAtlasJobs atlasHandle

      waitPastAutoWeatherPublishInterval
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      version1 <- readSnapshotVersion snapshotVersionRef
      version1 `shouldSatisfy` (> baselineVersion)
      publishedSnap1 <- readTerrainSnapshot terrainSnapshotRef
      let staleBaseKey = atlasBaseKeyForSelection selection waterLevel publishedSnap1
          Just staleOverlayKey = atlasOverlayKeyForSelection selection publishedSnap1
      staleBaseKey `shouldBe` baselineBaseKey
      terrainSnapshotViewVersion ViewCloud publishedSnap1 `shouldBe` tsWeatherVersion publishedSnap1
      atlasKeyVersion staleOverlayKey `shouldBe` tsWeatherVersion publishedSnap1

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 4
      latestUnpublishedSnap <- getTerrainSnapshot dataHandle
      tsWeatherVersion latestUnpublishedSnap `shouldSatisfy` (> tsWeatherVersion publishedSnap1)

      rapidJobs <- drainAtlasJobs atlasHandle
      length rapidJobs `shouldBe` 1
      map ajViewMode rapidJobs `shouldBe` [ViewCloud]
      map ajKey rapidJobs `shouldBe` [staleOverlayKey]
      map atlasJobStage rapidJobs `shouldBe` [currentStagePair]
      map ajSnapshotVersion rapidJobs `shouldBe` [version1]
      staleBaseKey `shouldNotSatisfy` (`elem` map ajKey rapidJobs)

      setUiSimAutoTick uiHandle False
      flushed <- flushSimWeatherPublication simHandle
      flushed `shouldBe` True
      versionAfterFlush <- readSnapshotVersion snapshotVersionRef
      latestTerrainSnap <- getTerrainSnapshot dataHandle
      latestPublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion latestPublishedSnap `shouldBe` tsWeatherVersion latestTerrainSnap
      let latestBaseKey = atlasBaseKeyForSelection selection waterLevel latestTerrainSnap
          Just latestOverlayKey = atlasOverlayKeyForSelection selection latestTerrainSnap
      latestBaseKey `shouldBe` staleBaseKey
      latestOverlayKey `shouldNotBe` staleOverlayKey
      atlasKeyVersion latestOverlayKey `shouldBe` tsWeatherVersion latestTerrainSnap

      backfillJobs <- drainAtlasJobs atlasHandle
      length backfillJobs `shouldBe` length allZoomStages
      map ajViewMode backfillJobs `shouldBe` replicate (length allZoomStages) ViewCloud
      map atlasJobStage backfillJobs `shouldBe` expectedBackfillStages
      map ajKey backfillJobs `shouldSatisfy` all (== latestOverlayKey)
      map ajSnapshotVersion backfillJobs `shouldSatisfy` all (== versionAfterFlush)
      latestBaseKey `shouldNotSatisfy` (`elem` map ajKey backfillJobs)

      assertConstrainedLatestCompletion
        LatestCompletionCase
          { lccCurrentKey = latestOverlayKey
          , lccCurrentSnapshotVersion = versionAfterFlush
          , lccStaleKey = staleOverlayKey
          , lccStaleSnapshotVersion = version1
          , lccTargetHexRadius = zsHexRadius currentStage
          , lccTargetAtlasScale = zsAtlasScale currentStage
          , lccExpectedPartialStatus = PartialExact
          , lccSeedCache = (emptyAtlasTextureCache 30) { atcKey = Just latestBaseKey, atcOverlayKey = Just latestOverlayKey }
          , lccFinalAtcLastKey = latestOverlayKey
          }

  it "keeps rapid ViewCloud auto ticks on weather-version keys and promotes only the latest constrained completion" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      _ <- installWeatherWorld dataHandle simHandle
      setUiViewMode uiHandle ViewCloud
      setUiZoom uiHandle 4.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      let waterLevel = uiRenderWaterLevel uiSnap
          currentStage = stageForZoom (uiZoom uiSnap)
          currentStagePair = zoomStagePair currentStage
          expectedBackfillStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnap))
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      version1 <- readSnapshotVersion snapshotVersionRef
      firstSnap <- getTerrainSnapshot dataHandle
      let staleCloudKey = atlasKeyFor ViewCloud waterLevel firstSnap
      terrainSnapshotViewVersion ViewCloud firstSnap `shouldBe` tsWeatherVersion firstSnap
      atlasKeyVersion staleCloudKey `shouldBe` tsWeatherVersion firstSnap

      waitPastAutoWeatherPublishInterval
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      version2 <- readSnapshotVersion snapshotVersionRef
      secondSnap <- getTerrainSnapshot dataHandle
      let secondCloudKey = atlasKeyFor ViewCloud waterLevel secondSnap
      secondCloudKey `shouldNotBe` staleCloudKey
      atlasKeyVersion secondCloudKey `shouldBe` tsWeatherVersion secondSnap

      rapidJobs <- drainAtlasJobs atlasHandle
      length rapidJobs `shouldBe` 1
      map ajViewMode rapidJobs `shouldBe` [ViewCloud]
      map ajKey rapidJobs `shouldBe` [secondCloudKey]
      map atlasJobStage rapidJobs `shouldBe` [currentStagePair]
      map ajSnapshotVersion rapidJobs `shouldBe` [version2]

      setUiSimTickRate uiHandle 0.1
      waitPastAutoWeatherPublishInterval
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3
      version3 <- readSnapshotVersion snapshotVersionRef
      latestTerrainSnap <- getTerrainSnapshot dataHandle
      latestPublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      uiAfterSlow <- getUiSnapshot uiHandle
      uiSimTickCount uiAfterSlow `shouldBe` 3
      tsWeatherVersion latestPublishedSnap `shouldBe` tsWeatherVersion latestTerrainSnap
      let latestCloudKey = atlasKeyFor ViewCloud waterLevel latestTerrainSnap
      latestCloudKey `shouldNotBe` secondCloudKey
      terrainSnapshotViewVersion ViewCloud latestTerrainSnap `shouldBe` tsWeatherVersion latestTerrainSnap
      atlasKeyVersion latestCloudKey `shouldBe` tsWeatherVersion latestTerrainSnap

      slowedJobs <- drainAtlasJobs atlasHandle
      length slowedJobs `shouldBe` length allZoomStages
      map ajViewMode slowedJobs `shouldBe` replicate (length allZoomStages) ViewCloud
      map atlasJobStage slowedJobs `shouldBe` expectedBackfillStages
      map ajKey slowedJobs `shouldSatisfy` all (== latestCloudKey)
      map ajSnapshotVersion slowedJobs `shouldSatisfy` all (== version3)

      let staleManifest = mkManifest version1 (AtlasBuildId 40) staleCloudKey (zsHexRadius currentStage) (zsAtlasScale currentStage) atlasResultBounds
          staleFreshness = mkFreshness staleManifest
          oldCache0 = storeAtlasTileSet staleManifest [mkTile 400 (zsHexRadius currentStage) (zsAtlasScale currentStage) testRect, mkTile 401 (zsHexRadius currentStage) (zsAtlasScale currentStage) testRect2]
            ((emptyAtlasTextureCache 30) { atcKey = Just staleCloudKey })
          (_oldTiles, oldStatus, oldResolvedCache) = resolveAtlasPureWithFreshness (Just staleFreshness) True True staleCloudKey (zsHexRadius currentStage) oldCache0
      oldStatus `shouldBe` CompleteExact
      fmap fst (atcLast oldResolvedCache) `shouldBe` Just staleCloudKey

      assertConstrainedLatestCompletion
        LatestCompletionCase
          { lccCurrentKey = latestCloudKey
          , lccCurrentSnapshotVersion = version3
          , lccStaleKey = staleCloudKey
          , lccStaleSnapshotVersion = version1
          , lccTargetHexRadius = zsHexRadius currentStage
          , lccTargetAtlasScale = zsAtlasScale currentStage
          , lccExpectedPartialStatus = LastGoodFallback
          , lccSeedCache = setAtlasKey latestCloudKey oldResolvedCache
          , lccFinalAtcLastKey = latestCloudKey
          }

      isNothing (getCurrentCompleteAtlasForTarget (Just (mkFreshness staleManifest)) latestCloudKey (zsHexRadius currentStage) (zsAtlasScale currentStage) oldResolvedCache) `shouldBe` True

  it "drives ViewCloud auto ticks through atlas scheduling, worker results, and latest render-cache promotion" $
    withConfiguredAtlasPipelineSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle schedulerHandle resultRef _freshnessRef workerLoadRef logHandle -> do
      _ <- installWeatherWorld dataHandle simHandle
      setUiViewMode uiHandle ViewCloud
      setUiZoom uiHandle 4.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiInitial <- getUiSnapshot uiHandle
      let waterLevel = uiRenderWaterLevel uiInitial
          currentStage = stageForZoom (uiZoom uiInitial)
          targetHex = zsHexRadius currentStage
          targetScale = zsAtlasScale currentStage
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      version1 <- readSnapshotVersion snapshotVersionRef
      publishedSnap1 <- readTerrainSnapshot terrainSnapshotRef
      uiPublished1 <- getUiSnapshot uiHandle
      let cloudKey1 = atlasKeyFor ViewCloud waterLevel publishedSnap1
      terrainSnapshotViewVersion ViewCloud publishedSnap1 `shouldBe` tsWeatherVersion publishedSnap1
      atlasKeyVersion cloudKey1 `shouldBe` tsWeatherVersion publishedSnap1

      results1 <- scheduleAndDrainAtlasResults schedulerHandle resultRef workerLoadRef logHandle uiPublished1 publishedSnap1 version1 1 cloudKey1
      assertAtlasResultsCompleteFor cloudKey1 version1 currentStage results1
      colors1 <- requireAtlasResultColors "first ViewCloud build" results1
      let freshness1 = atlasFreshnessFromResults results1
          cache1 = foldl storeResultTile ((emptyAtlasTextureCache 30) { atcKey = Just cloudKey1 }) results1
          (tiles1, status1, resolvedCache1) =
            resolveAtlasPureWithCoverage freshness1 emptyAtlasViewportCoverage True True cloudKey1 targetHex targetScale cache1
      status1 `shouldBe` CompleteExact
      fmap length tiles1 `shouldBe` Just (length results1)
      fmap fst (atcLast resolvedCache1) `shouldBe` Just cloudKey1

      waitPastAutoWeatherPublishInterval
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      version2 <- readSnapshotVersion snapshotVersionRef
      publishedSnap2 <- readTerrainSnapshot terrainSnapshotRef
      uiPublished2 <- getUiSnapshot uiHandle
      let cloudKey2 = atlasKeyFor ViewCloud waterLevel publishedSnap2
      version2 `shouldSatisfy` (> version1)
      tsWeatherVersion publishedSnap2 `shouldSatisfy` (> tsWeatherVersion publishedSnap1)
      cloudKey2 `shouldNotBe` cloudKey1
      terrainSnapshotViewVersion ViewCloud publishedSnap2 `shouldBe` tsWeatherVersion publishedSnap2
      atlasKeyVersion cloudKey2 `shouldBe` tsWeatherVersion publishedSnap2

      results2 <- scheduleAndDrainAtlasResults schedulerHandle resultRef workerLoadRef logHandle uiPublished2 publishedSnap2 version2 2 cloudKey2
      assertAtlasResultsCompleteFor cloudKey2 version2 currentStage results2
      colors2 <- requireAtlasResultColors "latest ViewCloud build" results2
      colors2 `shouldNotBe` colors1
      let freshness2 = atlasFreshnessFromResults results2
          cache2 = foldl storeResultTile (setAtlasKey cloudKey2 resolvedCache1) results2
          (tiles2, status2, resolvedCache2) =
            resolveAtlasPureWithCoverage freshness2 emptyAtlasViewportCoverage True True cloudKey2 targetHex targetScale cache2
      status2 `shouldBe` CompleteExact
      fmap length tiles2 `shouldBe` Just (length results2)
      fmap fst (atcLast resolvedCache2) `shouldBe` Just cloudKey2
      fmap length (getCurrentCompleteAtlasForTarget freshness2 cloudKey2 targetHex targetScale resolvedCache2) `shouldBe` Just (length results2)
      isNothing (getCurrentCompleteAtlasForTarget freshness2 cloudKey1 targetHex targetScale resolvedCache2) `shouldBe` True

  it "keeps unchanged snapshots idle unless pending atlas results or due retry scheduling make work actionable" $ do
    resultRef <- newAtlasResultRef
    let key = AtlasKey ViewElevation 0 44
        snapVersion = SnapshotVersion 44
        currentBuild = AtlasBuildId 500
        lastDrain = Just 100
        lastSchedule = Just 100
        nowMs = 150
        dueMs = 200
        mkPolicy pending retry now = atlasFrameStepPolicy now atlasDrainPollMs atlasSchedulePollMs False True pending noAtlasQueuedWork retry lastDrain lastSchedule
        idlePolicy = mkPolicy False False nowMs
        retryBeforePolicy = mkPolicy False True nowMs
        retryDuePolicy = mkPolicy False True dueMs

    afspAtlasMaintenanceDue idlePolicy `shouldBe` False
    afspShouldDrainAtlas idlePolicy `shouldBe` False
    afspShouldScheduleAtlas idlePolicy `shouldBe` False
    applyAtlasFrameStepTimestamps nowMs idlePolicy lastDrain lastSchedule `shouldBe` (lastDrain, lastSchedule)

    afspAtlasMaintenanceDue retryBeforePolicy `shouldBe` False
    afspShouldDrainAtlas retryBeforePolicy `shouldBe` False
    afspShouldScheduleAtlas retryBeforePolicy `shouldBe` False
    afspAtlasMaintenanceDue retryDuePolicy `shouldBe` True
    afspShouldScheduleAtlas retryDuePolicy `shouldBe` True
    applyAtlasFrameStepTimestamps dueMs retryDuePolicy lastDrain lastSchedule `shouldBe` (lastDrain, Just dueMs)

    mapM_
      (pushAtlasResult resultRef)
      [ mkBuildResult key snapVersion currentBuild 6 1 0 testRect atlasResultBounds
      , mkBuildResult key snapVersion currentBuild 6 1 1 testRect2 atlasResultBounds
      , mkBuildResult key snapVersion currentBuild 10 1 0 testRect3 [testRect3]
      ]
    pending0 <- atlasResultsPending resultRef
    pending0 `shouldBe` True
    let pendingPolicy = atlasFrameStepPolicy nowMs atlasDrainPollMs atlasSchedulePollMs False True pending0 noAtlasQueuedWork False lastDrain lastSchedule
    afspAtlasMaintenanceDue pendingPolicy `shouldBe` True
    afspShouldDrainAtlas pendingPolicy `shouldBe` True
    applyAtlasFrameStepTimestamps nowMs pendingPolicy lastDrain lastSchedule `shouldBe` (Just nowMs, lastSchedule)

    (drained1, staleCount1) <- drainFreshResultsN resultRef (const True) atlasUploadsPerFrame
    staleCount1 `shouldBe` 0
    length drained1 `shouldBe` atlasUploadsPerFrame
    pending1 <- atlasResultsPending resultRef
    pending1 `shouldBe` True
    let nextPolicy = atlasFrameStepPolicy (nowMs + 1) atlasDrainPollMs atlasSchedulePollMs False True pending1 noAtlasQueuedWork False (Just nowMs) lastSchedule
    afspAtlasMaintenanceDue nextPolicy `shouldBe` True
    afspShouldDrainAtlas nextPolicy `shouldBe` True

    (_drained2, _staleCount2) <- drainFreshResultsN resultRef (const True) 2
    pending2 <- atlasResultsPending resultRef
    pending2 `shouldBe` False
    let emptyAfterDrainPolicy = atlasFrameStepPolicy (nowMs + 2) atlasDrainPollMs atlasSchedulePollMs False True pending2 noAtlasQueuedWork False (Just (nowMs + 1)) lastSchedule
    afspAtlasMaintenanceDue emptyAfterDrainPolicy `shouldBe` False

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

withConfiguredSimulation
  :: ( ActorHandle Simulation (Protocol Simulation)
     -> ActorHandle Data (Protocol Data)
     -> ActorHandle Ui (Protocol Ui)
     -> TerrainSnapshotRef
     -> SnapshotVersionRef
     -> ActorHandle AtlasManager (Protocol AtlasManager)
     -> IO a
     )
  -> IO a
withConfiguredSimulation action = withSystem $ \system -> do
  simHandle <- get @Simulation system
  dataHandle <- get @Data system
  logHandle <- get @Log system
  uiHandle <- get @Ui system
  dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnap
  snapshotVersionRef <- newSnapshotVersionRef
  atlasHandle <- get @AtlasManager system

  setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle
  action simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle

withConfiguredAtlasPipelineSimulation
  :: ( ActorHandle Simulation (Protocol Simulation)
     -> ActorHandle Data (Protocol Data)
     -> ActorHandle Ui (Protocol Ui)
     -> TerrainSnapshotRef
     -> SnapshotVersionRef
     -> ActorHandle AtlasManager (Protocol AtlasManager)
     -> ActorHandle AtlasScheduler (Protocol AtlasScheduler)
     -> AtlasResultRef
     -> AtlasFreshnessRef
     -> AtlasWorkerLoadRef
     -> ActorHandle Log (Protocol Log)
     -> IO a
     )
  -> IO a
withConfiguredAtlasPipelineSimulation action = withSystem $ \system -> do
  simHandle <- get @Simulation system
  dataHandle <- get @Data system
  logHandle <- get @Log system
  uiHandle <- get @Ui system
  dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnap
  snapshotVersionRef <- newSnapshotVersionRef
  atlasHandle <- get @AtlasManager system
  schedulerHandle <- get @AtlasScheduler system
  workerHandle <- spawnActor atlasWorkerActorDef
  workerNextRef <- newIORef (0 :: Int)
  workerLoadRef <- newAtlasWorkerLoadRef
  resultRef <- newAtlasResultRef
  scheduleRef <- newAtlasScheduleRef
  freshnessRef <- newAtlasFreshnessRef

  setAtlasManagerFreshnessRef atlasHandle freshnessRef
  setAtlasSchedulerHandles schedulerHandle AtlasSchedulerHandles
    { ashManager = atlasHandle
    , ashWorkers = [workerHandle]
    , ashWorkerNext = workerNextRef
    , ashWorkerLoadRef = workerLoadRef
    , ashResultRef = resultRef
    , ashScheduleRef = scheduleRef
    , ashFreshnessRef = freshnessRef
    }
  schedulerReady <- atlasSchedulerConfigured schedulerHandle
  schedulerReady `shouldBe` True
  setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle
  action simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle schedulerHandle resultRef freshnessRef workerLoadRef logHandle

installWeatherWorld
  :: ActorHandle Data (Protocol Data)
  -> ActorHandle Simulation (Protocol Simulation)
  -> IO TerrainSnapshot
installWeatherWorld dataHandle simHandle = do
  replaceTerrainData dataHandle weatherTestWorld
  initialTerrainSnap <- getTerrainSnapshot dataHandle
  setSimWorld simHandle weatherTestWorld
  dag <- getSimDagSnapshot simHandle
  sdsAvailable dag `shouldBe` True
  pure initialTerrainSnap

waitPastAutoWeatherPublishInterval :: IO ()
waitPastAutoWeatherPublishInterval =
  threadDelay (fromIntegral (autoTickWeatherPublishIntervalNs `div` 1000 + 20000))

awaitTrue :: Int -> IO Bool -> IO Bool
awaitTrue 0 action = action
awaitTrue retries action = do
  ok <- action
  if ok
    then pure True
    else do
      threadDelay 20000
      awaitTrue (retries - 1) action

scheduleAndDrainAtlasResults
  :: ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasResultRef
  -> AtlasWorkerLoadRef
  -> ActorHandle Log (Protocol Log)
  -> UiState
  -> TerrainSnapshot
  -> SnapshotVersion
  -> Int
  -> AtlasKey
  -> IO [AtlasBuildResult]
scheduleAndDrainAtlasResults schedulerHandle resultRef workerLoadRef logHandle uiSnap terrainSnap snapshotVersion minStarted expectedKey = do
  logSnap <- getLogSnapshot logHandle
  requestAtlasSchedule schedulerHandle AtlasScheduleRequest
    { asqSnapshotVersion = snapshotVersion
    , asqRenderTargetOk = True
    , asqDataReady = True
    , asqSnapshot = RenderSnapshot
        { rsUi = uiSnap
        , rsLog = logSnap
        , rsData = DataSnapshot 0 0 Nothing
        , rsTerrain = terrainSnap
        }
    , asqWindowSize = (640, 480)
    , asqRefreshCurrentViewport = False
    , asqRefreshStage = Just (stageForZoom (uiZoom uiSnap))
    }
  finished <- awaitTrue 200 $ do
    load <- readAtlasWorkerLoad workerLoadRef
    pure (awlBuildStarted load >= minStarted && awlInFlight load == 0)
  finished `shouldBe` True
  pending <- atlasResultsPending resultRef
  pending `shouldBe` True
  (results, staleCount) <- drainFreshResultsN resultRef isExpected 100
  staleCount `shouldBe` 0
  length results `shouldSatisfy` (> 0)
  pure results
  where
    isExpected result = abrKey result == expectedKey && abrSnapshotVersion result == snapshotVersion

assertAtlasResultsCompleteFor :: AtlasKey -> SnapshotVersion -> ZoomStage -> [AtlasBuildResult] -> IO ()
assertAtlasResultsCompleteFor expectedKey snapshotVersion stage results = do
  length results `shouldSatisfy` (> 0)
  map abrKey results `shouldSatisfy` all (== expectedKey)
  map abrSnapshotVersion results `shouldSatisfy` all (== snapshotVersion)
  map abrHexRadius results `shouldSatisfy` all (== zsHexRadius stage)
  map (atsmAtlasScale . abrManifest) results `shouldSatisfy` all (== zsAtlasScale stage)
  map (atsmExpectedTileCount . abrManifest) results `shouldSatisfy` all (== length results)

atlasFreshnessFromResults :: [AtlasBuildResult] -> Maybe AtlasFreshness
atlasFreshnessFromResults results = mkFreshness . abrManifest <$> listToMaybe results

requireAtlasResultColors :: String -> [AtlasBuildResult] -> IO [(Word8, Word8, Word8, Word8)]
requireAtlasResultColors label results =
  case atlasResultColors results of
    colors@(_:_) -> pure colors
    [] -> expectationFailure ("Expected at least one vertex colour in " <> label) >> pure []

atlasResultColors :: [AtlasBuildResult] -> [(Word8, Word8, Word8, Word8)]
atlasResultColors results =
  [ rawVertexColor vertex
  | result <- results
  , chunk <- atgChunks (abrTile result)
  , vertex <- SV.toList (acgVertices chunk)
  ]

rawVertexColor :: Raw.Vertex -> (Word8, Word8, Word8, Word8)
rawVertexColor (Raw.Vertex _ (Raw.Color r g b a) _) = (r, g, b, a)

data LatestCompletionCase = LatestCompletionCase
  { lccCurrentKey :: !AtlasKey
  , lccCurrentSnapshotVersion :: !SnapshotVersion
  , lccStaleKey :: !AtlasKey
  , lccStaleSnapshotVersion :: !SnapshotVersion
  , lccTargetHexRadius :: !Int
  , lccTargetAtlasScale :: !Int
  , lccExpectedPartialStatus :: !AtlasResolveStatus
  , lccSeedCache :: !AtlasTextureCache
  , lccFinalAtcLastKey :: !AtlasKey
  }

assertConstrainedLatestCompletion :: LatestCompletionCase -> IO ()
assertConstrainedLatestCompletion tc = do
  resultRef <- newAtlasResultRef
  let currentBuild = AtlasBuildId 77
      staleBuild = AtlasBuildId 7
      currentManifest = mkManifest (lccCurrentSnapshotVersion tc) currentBuild (lccCurrentKey tc) (lccTargetHexRadius tc) (lccTargetAtlasScale tc) atlasResultBounds
      currentFreshness = mkFreshness currentManifest
      staleResult = mkBuildResult (lccStaleKey tc) (lccStaleSnapshotVersion tc) staleBuild (lccTargetHexRadius tc) (lccTargetAtlasScale tc) 0 testRect3 [testRect3]
      currentResult1 = mkBuildResult (lccCurrentKey tc) (lccCurrentSnapshotVersion tc) currentBuild (lccTargetHexRadius tc) (lccTargetAtlasScale tc) 0 testRect atlasResultBounds
      currentResult2 = mkBuildResult (lccCurrentKey tc) (lccCurrentSnapshotVersion tc) currentBuild (lccTargetHexRadius tc) (lccTargetAtlasScale tc) 1 testRect2 atlasResultBounds
      isFresh result = abrKey result == lccCurrentKey tc && abrSnapshotVersion result == lccCurrentSnapshotVersion tc
  mapM_ (pushAtlasResult resultRef) [staleResult, currentResult1, currentResult2]
  pendingBefore <- atlasResultsPending resultRef
  pendingBefore `shouldBe` True

  (drained1, staleCount1) <- drainFreshResultsN resultRef isFresh atlasUploadsPerFrame
  staleCount1 `shouldBe` 1
  length drained1 `shouldBe` atlasUploadsPerFrame
  pendingAfterFirstBudget <- atlasResultsPending resultRef
  pendingAfterFirstBudget `shouldBe` True

  let partialCache = foldl storeResultTile (lccSeedCache tc) drained1
      (partialTiles, partialStatus, partialResolvedCache) =
        resolveAtlasPureWithFreshness (Just currentFreshness) True True (lccCurrentKey tc) (lccTargetHexRadius tc) partialCache
      partialPolicy = atlasFrameStepPolicy 500 atlasDrainPollMs atlasSchedulePollMs False True pendingAfterFirstBudget noAtlasQueuedWork (atlasResolveNeedsRetry partialStatus) (Just 499) (Just 499)
      unchangedSnapshot = Just (lccCurrentSnapshotVersion tc)
  unchangedSnapshot `shouldBe` Just (lccCurrentSnapshotVersion tc)
  fmap length partialTiles `shouldSatisfy` maybe False (> 0)
  partialStatus `shouldBe` lccExpectedPartialStatus tc
  atlasResolveNeedsRetry partialStatus `shouldBe` True
  isNothing (getCurrentCompleteAtlasForTarget (Just currentFreshness) (lccCurrentKey tc) (lccTargetHexRadius tc) (lccTargetAtlasScale tc) partialResolvedCache) `shouldBe` True
  afspAtlasMaintenanceDue partialPolicy `shouldBe` True
  afspShouldDrainAtlas partialPolicy `shouldBe` True

  (drained2, staleCount2) <- drainFreshResultsN resultRef isFresh atlasUploadsPerFrame
  staleCount2 `shouldBe` 0
  length drained2 `shouldBe` atlasUploadsPerFrame
  brokerEmpty <- atlasResultsPending resultRef
  brokerEmpty `shouldBe` False

  let completeCache = foldl storeResultTile partialResolvedCache drained2
      (completeTiles, completeStatus, completeResolvedCache) =
        resolveAtlasPureWithFreshness (Just currentFreshness) True True (lccCurrentKey tc) (lccTargetHexRadius tc) completeCache
      completePolicy = atlasFrameStepPolicy 501 atlasDrainPollMs atlasSchedulePollMs False True brokerEmpty noAtlasQueuedWork (atlasResolveNeedsRetry completeStatus) (Just 500) (Just 499)
  fmap length completeTiles `shouldBe` Just 2
  completeStatus `shouldBe` CompleteExact
  atlasResolveNeedsRetry completeStatus `shouldBe` False
  fmap fst (atcLast completeResolvedCache) `shouldBe` Just (lccFinalAtcLastKey tc)
  fmap length (getCurrentCompleteAtlasForTarget (Just currentFreshness) (lccCurrentKey tc) (lccTargetHexRadius tc) (lccTargetAtlasScale tc) completeResolvedCache) `shouldBe` Just 2
  isNothing (getCurrentCompleteAtlasForTarget (Just currentFreshness) (lccStaleKey tc) (lccTargetHexRadius tc) (lccTargetAtlasScale tc) completeResolvedCache) `shouldBe` True
  afspAtlasMaintenanceDue completePolicy `shouldBe` False

storeResultTile :: AtlasTextureCache -> AtlasBuildResult -> AtlasTextureCache
storeResultTile cache result =
  storeAtlasTileSet (abrManifest result) [mkTile (fromIntegral (unAtlasBuildId (atsmBuildId (abrManifest result))) + abrTileIndex result) (atsmHexRadius (abrManifest result)) (atsmAtlasScale (abrManifest result)) (abrTileBounds result)] cache

mkFreshness :: AtlasTileSetManifest -> AtlasFreshness
mkFreshness manifest = AtlasFreshness
  { afKey = atsmKey manifest
  , afSnapshotVersion = atsmSnapshotVersion manifest
  , afLatestBuildIds = Map.singleton (atlasManifestTarget manifest) (atsmBuildId manifest)
  }

mkManifest :: SnapshotVersion -> AtlasBuildId -> AtlasKey -> Int -> Int -> [Rect] -> AtlasTileSetManifest
mkManifest snapshotVersion buildId key hexRadius atlasScale bounds = AtlasTileSetManifest
  { atsmBuildId = buildId
  , atsmKey = key
  , atsmSnapshotVersion = snapshotVersion
  , atsmHexRadius = hexRadius
  , atsmAtlasScale = atlasScale
  , atsmExpectedTileCount = length bounds
  , atsmExpectedBounds = bounds
  , atsmCoverage = emptyAtlasViewportCoverage
  }

mkBuildResult :: AtlasKey -> SnapshotVersion -> AtlasBuildId -> Int -> Int -> Int -> Rect -> [Rect] -> AtlasBuildResult
mkBuildResult key snapshotVersion buildId hexRadius atlasScale tileIndex bounds expectedBounds = AtlasBuildResult
  { abrKey = key
  , abrSnapshotVersion = snapshotVersion
  , abrHexRadius = hexRadius
  , abrManifest = mkManifest snapshotVersion buildId key hexRadius atlasScale expectedBounds
  , abrTileIndex = tileIndex
  , abrTileBounds = bounds
  , abrTile = AtlasTileGeometry
      { atgBounds = bounds
      , atgScale = atlasScale
      , atgHexRadius = hexRadius
      , atgChunks = []
      , atgRiverOverlay = []
      }
  , abrDayNightTile = Nothing
  }

mkTile :: Int -> Int -> Int -> Rect -> TerrainAtlasTile
mkTile tag hexRadius atlasScale bounds = TerrainAtlasTile
  { tatTexture = mockTexture tag
  , tatBounds = bounds
  , tatScale = atlasScale
  , tatHexRadius = hexRadius
  }

mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral n) :: Ptr ())

atlasJobStage :: AtlasJob -> (Int, Int)
atlasJobStage job = (ajHexRadius job, ajAtlasScale job)

zoomStagePair :: ZoomStage -> (Int, Int)
zoomStagePair stage = (zsHexRadius stage, zsAtlasScale stage)

atlasUploadsPerFrame :: Int
atlasUploadsPerFrame = 1

atlasDrainPollMs :: Int
atlasDrainPollMs = 1000

atlasSchedulePollMs :: Int
atlasSchedulePollMs = 100

testRect :: Rect
testRect = Rect (V2 0 0, V2 64 64)

testRect2 :: Rect
testRect2 = Rect (V2 64 0, V2 64 64)

testRect3 :: Rect
testRect3 = Rect (V2 128 0, V2 64 64)

atlasResultBounds :: [Rect]
atlasResultBounds = [testRect, testRect2]

emptyTerrainSnap :: TerrainSnapshot
emptyTerrainSnap = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext

weatherTestWorld :: TerrainWorld
weatherTestWorld = withSeedWeather
  (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0))
  (ChunkId 0)
  climate
  where
    config = WorldConfig { wcChunkSize = 8 }
    terrain = generateTerrainChunk config (const 0.5)
    climate0 = emptyClimateChunk config
    tileCount = U.length (ccTempAvg climate0)
    climate = climate0
      { ccTempAvg = U.replicate tileCount 0.6
      , ccPrecipAvg = U.replicate tileCount 0.5
      , ccWindDirAvg = U.replicate tileCount 0.4
      , ccWindSpdAvg = U.replicate tileCount 0.35
      , ccHumidityAvg = U.replicate tileCount 0.5
      }
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

withSeedWeather :: TerrainWorld -> ChunkId -> ClimateChunk -> TerrainWorld
withSeedWeather world (ChunkId chunkId) climate =
  world { twOverlays = insertOverlay weatherOverlay (twOverlays world) }
  where
    weatherOverlay = Overlay
      { ovSchema = weatherOverlaySchema
      , ovData = DenseData (IntMap.singleton chunkId (weatherChunkToOverlay (mkSeedWeatherChunk climate)))
      , ovProvenance = OverlayProvenance
          { opSeed = 0
          , opVersion = 1
          , opSource = "weather-atlas-flicker-spec"
          , opSchedule = Nothing
          }
      }

mkSeedWeatherChunk :: ClimateChunk -> WeatherChunk
mkSeedWeatherChunk climate = WeatherChunk
  { wcTemp = ccTempAvg climate
  , wcHumidity = ccHumidityAvg climate
  , wcWindDir = ccWindDirAvg climate
  , wcWindSpd = ccWindSpdAvg climate
  , wcPressure = U.replicate tileCount 0.5
  , wcPrecip = ccPrecipAvg climate
  , wcCloudCover = U.replicate tileCount 0.0
  , wcCloudWater = U.replicate tileCount 0.0
  , wcCloudCoverLow  = U.replicate tileCount 0.0
  , wcCloudCoverMid  = U.replicate tileCount 0.0
  , wcCloudCoverHigh = U.replicate tileCount 0.0
  , wcCloudWaterLow  = U.replicate tileCount 0.0
  , wcCloudWaterMid  = U.replicate tileCount 0.0
  , wcCloudWaterHigh = U.replicate tileCount 0.0
  }
  where
    tileCount = U.length (ccTempAvg climate)
