module Spec.RenderLoopAtlasMaintenance (spec) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildResult(..), AtlasTileSetManifest(..), atlasManifestTarget)
import Actor.AtlasResultBroker (atlasResultsPending, drainFreshResultsN, newAtlasResultRef, pushAtlasResult)
import Actor.AtlasScheduler (AtlasFreshness(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (ViewMode(..))
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Foreign.Ptr (Ptr, intPtrToPtr)
import Linear (V2(..))
import qualified SDL
import Seer.Render.Atlas
  ( AtlasResolveStatus(..)
  , AtlasTextureCache(..)
  , atlasResolveNeedsRetry
  , emptyAtlasTextureCache
  , getCurrentCompleteAtlasForTarget
  , resolveAtlasPureWithFreshness
  , storeAtlasTileSet
  )
import Seer.Render.Frame
  ( AtlasFrameStepPolicy(..)
  , applyAtlasFrameStepTimestamps
  , atlasFrameStepPolicy
  )
import Test.Hspec
import UI.TerrainAtlas (AtlasTileGeometry(..), TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec
spec = describe "render-loop atlas maintenance wakeups" $ do
  it "wakes an unchanged snapshot and attempts atlas drain when results are pending despite a recent drain" $ do
    resultRef <- newAtlasResultRef
    pushAtlasResult resultRef (mkBuildResult keyElevation snapVersion 6 1 testRect 1)
    atlasPending <- atlasResultsPending resultRef

    let nowMs = 110
        policy = atlasFrameStepPolicy nowMs atlasDrainPollMs atlasSchedulePollMs False True atlasPending False (Just 109) (Just 109)

    lastSnapshot `shouldBe` Just snapVersion
    afspAtlasMaintenanceDue policy `shouldBe` True
    afspShouldDrainAtlas policy `shouldBe` True
    afspShouldScheduleAtlas policy `shouldBe` False

  it "leaves excess fresh atlas results pending after upload-budget exhaustion and wakes the next unchanged frame" $ do
    resultRef <- newAtlasResultRef
    mapM_ (pushAtlasResult resultRef) (mkQueuedResults 3)

    atlasPending <- atlasResultsPending resultRef
    let nowMs = 250
        policy = atlasFrameStepPolicy nowMs atlasDrainPollMs atlasSchedulePollMs False True atlasPending False (Just 249) (Just 249)
    afspShouldDrainAtlas policy `shouldBe` True

    (drained, staleCount) <- drainFreshResultsN resultRef (const True) atlasUploadsPerFrame
    staleCount `shouldBe` 0
    length drained `shouldBe` atlasUploadsPerFrame

    leftoversPending <- atlasResultsPending resultRef
    leftoversPending `shouldBe` True
    let (lastDrainAfter, lastScheduleAfter) = applyAtlasFrameStepTimestamps nowMs policy (Just 249) (Just 249)
        nextNowMs = nowMs + 1
        nextPolicy = atlasFrameStepPolicy nextNowMs atlasDrainPollMs atlasSchedulePollMs False True leftoversPending False lastDrainAfter lastScheduleAfter
    lastDrainAfter `shouldBe` Just nowMs
    afspAtlasMaintenanceDue nextPolicy `shouldBe` True
    afspShouldDrainAtlas nextPolicy `shouldBe` True

  it "does not busy-loop when no atlas work is pending and only wakes retry at the schedule poll interval" $ do
    let beforePollMs = 150
        dueMs = 200
        idlePolicy = atlasFrameStepPolicy beforePollMs atlasDrainPollMs atlasSchedulePollMs False True False False (Just 100) (Just 100)
        retryBeforePolicy = atlasFrameStepPolicy beforePollMs atlasDrainPollMs atlasSchedulePollMs False True False True (Just 100) (Just 100)
        retryDuePolicy = atlasFrameStepPolicy dueMs atlasDrainPollMs atlasSchedulePollMs False True False True (Just 100) (Just 100)

    afspAtlasMaintenanceDue idlePolicy `shouldBe` False
    afspAtlasMaintenanceDue retryBeforePolicy `shouldBe` False
    afspAtlasMaintenanceDue retryDuePolicy `shouldBe` True
    afspShouldScheduleAtlas retryDuePolicy `shouldBe` True

    let (lastDrainAfter, lastScheduleAfter) = applyAtlasFrameStepTimestamps dueMs retryDuePolicy (Just 100) (Just 100)
        afterSchedulePolicy = atlasFrameStepPolicy (dueMs + 1) atlasDrainPollMs atlasSchedulePollMs False True False True lastDrainAfter lastScheduleAfter
    lastScheduleAfter `shouldBe` Just dueMs
    afspAtlasMaintenanceDue afterSchedulePolicy `shouldBe` False
    afspShouldScheduleAtlas afterSchedulePolicy `shouldBe` False

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
    let partialPolicy = atlasFrameStepPolicy 500 atlasDrainPollMs atlasSchedulePollMs False True pendingSecondTile (atlasResolveNeedsRetry partialStatus) (Just 499) (Just 499)
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
        completePolicy = atlasFrameStepPolicy 501 atlasDrainPollMs atlasSchedulePollMs False True brokerEmpty (atlasResolveNeedsRetry completeStatus) (Just 500) (Just 499)
    fmap length completeTiles `shouldBe` Just 2
    completeStatus `shouldBe` CompleteExact
    atlasResolveNeedsRetry completeStatus `shouldBe` False
    fmap length (getCurrentCompleteAtlasForTarget (Just freshness) weatherKey targetHex 1 completeResolvedCache) `shouldBe` Just 2
    afspAtlasMaintenanceDue completePolicy `shouldBe` False

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
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral n) :: Ptr ())
