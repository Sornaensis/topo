module Spec.RenderLoopAtlasMaintenance (spec) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildResult(..), AtlasTileSetManifest(..), atlasManifestTarget)
import Actor.AtlasResultBroker (atlasResultsPending, drainFreshResultsN, newAtlasResultRef, pushAtlasResult)
import Actor.AtlasScheduler (AtlasFreshness(..))
import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Data.Maybe (isNothing)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Foreign.Ptr (Ptr, intPtrToPtr)
import Linear (V2(..))
import qualified SDL
import Seer.Render.Atlas
  ( AtlasResolveStatus(..)
  , AtlasTextureCache(..)
  , DayNightOverlayStatus(..)
  , atlasResolveNeedsRetry
  , atlasResolveNeedsViewportRefresh
  , dayNightOverlayNeedsRetry
  , emptyAtlasTextureCache
  , getCurrentCompleteAtlasForTarget
  , getCurrentCompleteAtlasForTargetWithCoverage
  , resolveAtlasPureWithFreshness
  , resolveAtlasPureWithCoverage
  , resolveDayNightOverlayForTarget
  , storeAtlasTileSet
  )
import Seer.Render.Viewport (atlasViewportCoverageFromKeys, emptyAtlasViewportCoverage)
import Seer.Render.Frame
  ( AtlasFrameStepPolicy(..)
  , AtlasQueuedWork(..)
  , applyAtlasFrameStepTimestamps
  , atlasFrameStepPolicy
  , fallbackFrameMaintenanceDue
  , noAtlasQueuedWork
  )
import Seer.Render.Terrain (TerrainCache(..), buildTerrainCache)
import Seer.Render.ZoomStage (ZoomStage(..), stageForZoom)
import Test.Hspec
import Topo (WorldConfig(..), emptyTerrainChunk)
import Topo.Calendar (WorldTime(..), simulationTickSeconds)
import Topo.Overlay (emptyOverlayStore)
import Topo.Calendar (defaultWorldTime)
import UI.DayNight (DayNightKey(..))
import UI.TerrainAtlas (AtlasTileGeometry(..), TerrainAtlasTile(..))
import UI.TerrainCache (ChunkTextureCache(..))
import UI.TerrainRender (ChunkTexture(..))
import UI.Widgets (Rect(..))
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec
spec = describe "render-loop atlas maintenance wakeups" $ do
  it "wakes an unchanged snapshot and attempts atlas drain when results are pending despite a recent drain" $ do
    resultRef <- newAtlasResultRef
    pushAtlasResult resultRef (mkBuildResult keyElevation snapVersion 6 1 testRect 1)
    atlasPending <- atlasResultsPending resultRef

    let nowMs = 110
        policy = atlasFrameStepPolicy nowMs atlasDrainPollMs atlasSchedulePollMs False True atlasPending noAtlasQueuedWork False (Just 109) (Just 109)

    lastSnapshot `shouldBe` Just snapVersion
    afspAtlasMaintenanceDue policy `shouldBe` True
    afspShouldDrainAtlas policy `shouldBe` True
    afspShouldScheduleAtlas policy `shouldBe` False

  it "leaves excess fresh atlas results pending after upload-budget exhaustion and wakes the next unchanged frame" $ do
    resultRef <- newAtlasResultRef
    mapM_ (pushAtlasResult resultRef) (mkQueuedResults 3)

    atlasPending <- atlasResultsPending resultRef
    let nowMs = 250
        policy = atlasFrameStepPolicy nowMs atlasDrainPollMs atlasSchedulePollMs False True atlasPending noAtlasQueuedWork False (Just 249) (Just 249)
    afspShouldDrainAtlas policy `shouldBe` True

    (drained, staleCount) <- drainFreshResultsN resultRef (const True) atlasUploadsPerFrame
    staleCount `shouldBe` 0
    length drained `shouldBe` atlasUploadsPerFrame

    leftoversPending <- atlasResultsPending resultRef
    leftoversPending `shouldBe` True
    let (lastDrainAfter, lastScheduleAfter) = applyAtlasFrameStepTimestamps nowMs policy (Just 249) (Just 249)
        nextNowMs = nowMs + 1
        nextPolicy = atlasFrameStepPolicy nextNowMs atlasDrainPollMs atlasSchedulePollMs False True leftoversPending noAtlasQueuedWork False lastDrainAfter lastScheduleAfter
    lastDrainAfter `shouldBe` Just nowMs
    afspAtlasMaintenanceDue nextPolicy `shouldBe` True
    afspShouldDrainAtlas nextPolicy `shouldBe` True

  it "wakes queued manager work once per revision and then waits for poll or a new revision" $ do
    let queuedRevision = AtlasQueuedWork
          { aqwQueuedForCurrentKey = True
          , aqwQueueRevision = Just 7
          , aqwLastScheduledRevision = Just 6
          }
        firstPolicy = atlasFrameStepPolicy 150 atlasDrainPollMs atlasSchedulePollMs False True False queuedRevision False (Just 100) (Just 100)

    afspAtlasMaintenanceDue firstPolicy `shouldBe` True
    afspShouldDrainAtlas firstPolicy `shouldBe` False
    afspShouldScheduleAtlas firstPolicy `shouldBe` True

    let (_lastDrainAfter, lastScheduleAfter) = applyAtlasFrameStepTimestamps 150 firstPolicy (Just 100) (Just 100)
        sameRevision = queuedRevision { aqwLastScheduledRevision = Just 7 }
        sameRevisionPolicy = atlasFrameStepPolicy 151 atlasDrainPollMs atlasSchedulePollMs False True False sameRevision False (Just 100) lastScheduleAfter
        pollDuePolicy = atlasFrameStepPolicy 250 atlasDrainPollMs atlasSchedulePollMs False True False sameRevision False (Just 100) lastScheduleAfter
        nextRevision = sameRevision { aqwQueueRevision = Just 8 }
        nextRevisionPolicy = atlasFrameStepPolicy 151 atlasDrainPollMs atlasSchedulePollMs False True False nextRevision False (Just 100) lastScheduleAfter

    lastScheduleAfter `shouldBe` Just 150
    afspAtlasMaintenanceDue sameRevisionPolicy `shouldBe` False
    afspShouldScheduleAtlas sameRevisionPolicy `shouldBe` False
    afspAtlasMaintenanceDue pollDuePolicy `shouldBe` True
    afspShouldScheduleAtlas pollDuePolicy `shouldBe` True
    afspAtlasMaintenanceDue nextRevisionPolicy `shouldBe` True
    afspShouldScheduleAtlas nextRevisionPolicy `shouldBe` True

  it "does not busy-loop when no atlas work is pending and only wakes retry at the schedule poll interval" $ do
    let beforePollMs = 150
        dueMs = 200
        idlePolicy = atlasFrameStepPolicy beforePollMs atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork False (Just 100) (Just 100)
        retryBeforePolicy = atlasFrameStepPolicy beforePollMs atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork True (Just 100) (Just 100)
        retryDuePolicy = atlasFrameStepPolicy dueMs atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork True (Just 100) (Just 100)

    afspAtlasMaintenanceDue idlePolicy `shouldBe` False
    afspAtlasMaintenanceDue retryBeforePolicy `shouldBe` False
    afspAtlasMaintenanceDue retryDuePolicy `shouldBe` True
    afspShouldScheduleAtlas retryDuePolicy `shouldBe` True

    let (lastDrainAfter, lastScheduleAfter) = applyAtlasFrameStepTimestamps dueMs retryDuePolicy (Just 100) (Just 100)
        afterSchedulePolicy = atlasFrameStepPolicy (dueMs + 1) atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork True lastDrainAfter lastScheduleAfter
    lastScheduleAfter `shouldBe` Just dueMs
    afspAtlasMaintenanceDue afterSchedulePolicy `shouldBe` False
    afspShouldScheduleAtlas afterSchedulePolicy `shouldBe` False

  it "keeps viewport coverage gaps retrying until a current-stage refresh can be scheduled" $ do
    let targetHex = 6
        manifest = (mkManifest (AtlasBuildId 66) keyElevation targetHex 1 [testRect])
          { atsmCoverage = atlasViewportCoverageFromKeys [1] }
        freshness = AtlasFreshness
          { afKey = keyElevation
          , afSnapshotVersion = snapVersion
          , afLatestBuildIds = Map.singleton (atlasManifestTarget manifest) (AtlasBuildId 66)
          }
        cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyElevation }
        completeOldViewport = storeAtlasTileSet manifest [mkTile 66 targetHex testRect] cache0
        requiredCoverage = atlasViewportCoverageFromKeys [1, 2]
        (tiles, status, resolvedCache) =
          resolveAtlasPureWithCoverage (Just freshness) requiredCoverage True True keyElevation targetHex 1 completeOldViewport
        beforePoll = atlasFrameStepPolicy 150 atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork (atlasResolveNeedsRetry status) (Just 100) (Just 100)
        atPoll = atlasFrameStepPolicy 200 atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork (atlasResolveNeedsRetry status) (Just 100) (Just 100)
    fmap length tiles `shouldBe` Just 1
    status `shouldBe` ViewportCoverageMissing
    atlasResolveNeedsRetry status `shouldBe` True
    atlasResolveNeedsViewportRefresh status `shouldBe` True
    isNothing (getCurrentCompleteAtlasForTargetWithCoverage (Just freshness) (Just requiredCoverage) keyElevation targetHex 1 resolvedCache) `shouldBe` True
    afspAtlasMaintenanceDue beforePoll `shouldBe` False
    afspShouldScheduleAtlas beforePoll `shouldBe` False
    afspAtlasMaintenanceDue atPoll `shouldBe` True
    afspShouldScheduleAtlas atPoll `shouldBe` True

  it "wakes missing day/night overlay retry at the atlas schedule poll interval" $ do
    let targetHex = 6
        targetStage = ZoomStage targetHex 1 0 1
        manifest = mkManifest (AtlasBuildId 88) keyElevation targetHex 1 [testRect]
        freshness = AtlasFreshness
          { afKey = keyElevation
          , afSnapshotVersion = snapVersion
          , afLatestBuildIds = Map.singleton (atlasManifestTarget manifest) (AtlasBuildId 88)
          }
        cacheWithBase = storeAtlasTileSet manifest [mkTile 88 targetHex testRect]
          ((emptyAtlasTextureCache 30) { atcKey = Just keyElevation })
        (_, overlayStatus) = resolveDayNightOverlayForTarget (Just freshness) True True CompleteExact (Just testDayNightKey) targetStage cacheWithBase
        overlayRetry = dayNightOverlayNeedsRetry overlayStatus
        beforePoll = atlasFrameStepPolicy 150 atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork overlayRetry (Just 100) (Just 100)
        atPoll = atlasFrameStepPolicy 200 atlasDrainPollMs atlasSchedulePollMs False True False noAtlasQueuedWork overlayRetry (Just 100) (Just 100)

    overlayStatus `shouldBe` DayNightOverlayMissing
    overlayRetry `shouldBe` True
    afspAtlasMaintenanceDue beforePoll `shouldBe` False
    afspShouldDrainAtlas beforePoll `shouldBe` False
    afspShouldScheduleAtlas beforePoll `shouldBe` False
    afspAtlasMaintenanceDue atPoll `shouldBe` True
    afspShouldDrainAtlas atPoll `shouldBe` False
    afspShouldScheduleAtlas atPoll `shouldBe` True

  it "wakes fallback maintenance for stale day/night overlay without atlas maintenance when render targets are unavailable" $ do
    let terrainSnapOld = renderableFallbackTerrainSnapshot
          { tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = WorldTime 0 simulationTickSeconds }
          }
        terrainSnapNew = terrainSnapOld
          { tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = WorldTime 1 simulationTickSeconds }
          }
        uiOld = emptyUiState { uiDayNightEnabled = True, uiSimTickCount = 0 }
        uiNew = emptyUiState { uiDayNightEnabled = True, uiSimTickCount = 1 }
        oldCache = buildTerrainCache uiOld terrainSnapOld
        scale = zsAtlasScale (stageForZoom (uiZoom uiNew))
        fallbackDue = fallbackFrameMaintenanceDue False uiNew terrainSnapNew scale oldCache (chunkTexturesFor scale oldCache)
        atlasPolicy = atlasFrameStepPolicy 101 atlasDrainPollMs atlasSchedulePollMs False False False noAtlasQueuedWork False (Just 100) (Just 100)
    fallbackDue `shouldBe` True
    afspAtlasMaintenanceDue atlasPolicy `shouldBe` False
    afspShouldDrainAtlas atlasPolicy `shouldBe` False
    afspShouldScheduleAtlas atlasPolicy `shouldBe` False

  it "updates atlas timestamps only for attempted drain and schedule steps" $ do
    let skipped = applyAtlasFrameStepTimestamps 100 (AtlasFrameStepPolicy False False False) (Just 10) (Just 20)
        drained = applyAtlasFrameStepTimestamps 101 (AtlasFrameStepPolicy True False True) (Just 10) (Just 20)
        scheduled = applyAtlasFrameStepTimestamps 102 (AtlasFrameStepPolicy False True True) (Just 10) (Just 20)
        both = applyAtlasFrameStepTimestamps 103 (AtlasFrameStepPolicy True True True) (Just 10) (Just 20)

    skipped `shouldBe` (Just 10, Just 20)
    drained `shouldBe` (Just 101, Just 20)
    scheduled `shouldBe` (Just 10, Just 102)
    both `shouldBe` (Just 103, Just 103)

  it "keeps weather atlas retry active for partial current-key completion until all pending tiles arrive" $ do
    resultRef <- newAtlasResultRef
    let weatherKey = keyWeather
        targetHex = 6
        manifest = mkManifest (AtlasBuildId 77) weatherKey targetHex 1 [testRect, testRect2]
        freshness = AtlasFreshness
          { afKey = weatherKey
          , afSnapshotVersion = snapVersion
          , afLatestBuildIds = Map.singleton (atlasManifestTarget manifest) (AtlasBuildId 77)
          }
        cache0 = (emptyAtlasTextureCache 30) { atcKey = Just weatherKey }
        partialAtlasCache = storeAtlasTileSet manifest [mkTile 10 targetHex testRect] cache0
        (partialTiles, partialStatus, partialResolvedCache) =
          resolveAtlasPureWithFreshness (Just freshness) True True weatherKey targetHex partialAtlasCache

    fmap length partialTiles `shouldBe` Just 1
    partialStatus `shouldBe` PartialExact
    atlasResolveNeedsRetry partialStatus `shouldBe` True
    isNothing (getCurrentCompleteAtlasForTarget (Just freshness) weatherKey targetHex 1 partialResolvedCache) `shouldBe` True

    pushAtlasResult resultRef (mkBuildResult weatherKey snapVersion targetHex 1 testRect2 77)
    pendingSecondTile <- atlasResultsPending resultRef
    let partialPolicy = atlasFrameStepPolicy 500 atlasDrainPollMs atlasSchedulePollMs False True pendingSecondTile noAtlasQueuedWork (atlasResolveNeedsRetry partialStatus) (Just 499) (Just 499)
    lastSnapshot `shouldBe` Just snapVersion
    pendingSecondTile `shouldBe` True
    afspAtlasMaintenanceDue partialPolicy `shouldBe` True
    afspShouldDrainAtlas partialPolicy `shouldBe` True

    (secondResults, staleCount) <- drainFreshResultsN resultRef (const True) 1
    staleCount `shouldBe` 0
    length secondResults `shouldBe` 1
    brokerEmpty <- atlasResultsPending resultRef
    brokerEmpty `shouldBe` False

    let completeAtlasCache = storeAtlasTileSet manifest [mkTile 11 targetHex testRect2] partialResolvedCache
        (completeTiles, completeStatus, completeResolvedCache) =
          resolveAtlasPureWithFreshness (Just freshness) True True weatherKey targetHex completeAtlasCache
        completePolicy = atlasFrameStepPolicy 501 atlasDrainPollMs atlasSchedulePollMs False True brokerEmpty noAtlasQueuedWork (atlasResolveNeedsRetry completeStatus) (Just 500) (Just 499)
    fmap length completeTiles `shouldBe` Just 2
    completeStatus `shouldBe` CompleteExact
    atlasResolveNeedsRetry completeStatus `shouldBe` False
    fmap length (getCurrentCompleteAtlasForTarget (Just freshness) weatherKey targetHex 1 completeResolvedCache) `shouldBe` Just 2
    afspAtlasMaintenanceDue completePolicy `shouldBe` False

renderableFallbackTerrainSnapshot :: TerrainSnapshot
renderableFallbackTerrainSnapshot = TerrainSnapshot
  { tsVersion = 1
  , tsClimateVersion = 0
  , tsWeatherVersion = 0
  , tsVegetationVersion = 0
  , tsOverlayVersion = 0
  , tsChunkSize = 1
  , tsTerrainChunks = IntMap.singleton 0 (emptyTerrainChunk (WorldConfig { wcChunkSize = 1 }))
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
  , tsRiverChunks = IntMap.empty
  , tsGroundwaterChunks = IntMap.empty
  , tsVolcanismChunks = IntMap.empty
  , tsGlacierChunks = IntMap.empty
  , tsWaterBodyChunks = IntMap.empty
  , tsVegetationChunks = IntMap.empty
  , tsOverlayStore = emptyOverlayStore
  , tsGeoContext = defaultTerrainGeoContext
  }

chunkTexturesFor :: Int -> TerrainCache -> ChunkTextureCache
chunkTexturesFor scale cache = ChunkTextureCache
  { ctcViewMode = tcViewMode cache
  , ctcWaterLevel = tcWaterLevel cache
  , ctcChunkSize = tcChunkSize cache
  , ctcScale = scale
  , ctcTerrainChunks = tcTerrainChunks cache
  , ctcClimateChunks = tcClimateChunks cache
  , ctcWeatherChunks = tcWeatherChunks cache
  , ctcTextures = IntMap.mapWithKey (\key _ -> mockChunkTexture key) (tcGeometry cache)
  }

mockChunkTexture :: Int -> ChunkTexture
mockChunkTexture key = ChunkTexture
  { ctTexture = mockTexture key
  , ctBounds = Rect (V2 0 0, V2 1 1)
  }

testDayNightKey :: DayNightKey
testDayNightKey = DayNightKey defaultWorldTime 16 6371 23.44 8 35 0

atlasUploadsPerFrame :: Int
atlasUploadsPerFrame = 1

atlasDrainPollMs :: Int
atlasDrainPollMs = 1000

atlasSchedulePollMs :: Int
atlasSchedulePollMs = 100

snapVersion :: SnapshotVersion
snapVersion = SnapshotVersion 1

lastSnapshot :: Maybe SnapshotVersion
lastSnapshot = Just snapVersion

keyElevation :: AtlasKey
keyElevation = AtlasKey ViewElevation 0 1

keyWeather :: AtlasKey
keyWeather = AtlasKey ViewWeather 0 7

testRect :: Rect
testRect = Rect (V2 0 0, V2 64 64)

testRect2 :: Rect
testRect2 = Rect (V2 64 0, V2 64 64)

mkQueuedResults :: Int -> [AtlasBuildResult]
mkQueuedResults n =
  [ mkBuildResult keyElevation snapVersion 6 idx testRect idx
  | idx <- [1 .. n]
  ]

mkBuildResult :: AtlasKey -> SnapshotVersion -> Int -> Int -> Rect -> Int -> AtlasBuildResult
mkBuildResult key snapshotVersion hexRadius tileIndex bounds buildId =
  let manifest = mkManifestFor snapshotVersion (AtlasBuildId (fromIntegral buildId)) key hexRadius 1 [bounds]
  in AtlasBuildResult
    { abrKey = key
    , abrSnapshotVersion = snapshotVersion
    , abrHexRadius = hexRadius
    , abrManifest = manifest
    , abrTileIndex = tileIndex
    , abrTileBounds = bounds
    , abrTile = mkTileGeometry hexRadius 1 bounds
    , abrDayNightTile = Nothing
    }

mkManifest :: AtlasBuildId -> AtlasKey -> Int -> Int -> [Rect] -> AtlasTileSetManifest
mkManifest = mkManifestFor snapVersion

mkManifestFor :: SnapshotVersion -> AtlasBuildId -> AtlasKey -> Int -> Int -> [Rect] -> AtlasTileSetManifest
mkManifestFor snapshotVersion buildId key hexRadius atlasScale bounds = AtlasTileSetManifest
  { atsmBuildId = buildId
  , atsmKey = key
  , atsmSnapshotVersion = snapshotVersion
  , atsmHexRadius = hexRadius
  , atsmAtlasScale = atlasScale
  , atsmExpectedTileCount = length bounds
  , atsmExpectedBounds = bounds
  , atsmCoverage = emptyAtlasViewportCoverage
  }

mkTileGeometry :: Int -> Int -> Rect -> AtlasTileGeometry
mkTileGeometry hexRadius atlasScale bounds = AtlasTileGeometry
  { atgBounds = bounds
  , atgScale = atlasScale
  , atgHexRadius = hexRadius
  , atgChunks = []
  , atgRiverOverlay = []
  }

mkTile :: Int -> Int -> Rect -> TerrainAtlasTile
mkTile tag hexRadius bounds = TerrainAtlasTile
  { tatTexture = mockTexture tag
  , tatBounds = bounds
  , tatScale = 1
  , tatHexRadius = hexRadius
  }

mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral (max 1 n)) :: Ptr ())
