module Spec.AtlasResultBroker (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Foreign.Ptr (Ptr, intPtrToPtr)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildResult(..), AtlasDayNightTile(..), AtlasTileSetManifest(..), atlasManifestTarget)
import Actor.AtlasResultBroker
  ( AtlasResultDrainStats(..)
  , atlasResultsPending
  , atlasResultsPendingCount
  , drainAtlasResultsN
  , drainFreshResultsN
  , drainFreshResultsNWithStats
  , formatAtlasResultDrainStats
  , newAtlasResultRef
  , pushAtlasResult
  )
import Actor.AtlasScheduler (AtlasFreshness(..))
import Actor.AtlasWorker (atlasBuildResultsForTiles)
import Actor.Data (TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.UI (ViewMode(..))
import Linear (V2(..))
import qualified SDL
import Seer.Render.Viewport (atlasViewportCoverageFromKeys, emptyAtlasViewportCoverage)
import Seer.Render.Atlas
  ( AtlasResolveStatus(..)
  , AtlasTextureCache(..)
  , emptyAtlasTextureCache
  , getCurrentCompleteAtlasForTarget
  , getNearestDayNight
  , getCurrentCompleteDayNight
  , resolveAtlasPureWithFreshness
  , storeAtlasTileSet
  , storeDayNightTileSet
  )
import Topo.Calendar (defaultWorldTime)
import UI.DayNight (DayNightKey(..))
import UI.TerrainAtlas (AtlasTileGeometry(..), TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import Unsafe.Coerce (unsafeCoerce)

mkResult :: AtlasKey -> SnapshotVersion -> Int -> AtlasTileGeometry -> AtlasBuildResult
mkResult key snapshotVersion hexRadius tile =
  let bounds = atgBounds tile
      manifest = AtlasTileSetManifest
        { atsmBuildId = AtlasBuildId (fromIntegral hexRadius)
        , atsmKey = key
        , atsmSnapshotVersion = snapshotVersion
        , atsmHexRadius = hexRadius
        , atsmAtlasScale = atgScale tile
        , atsmExpectedTileCount = 1
        , atsmExpectedBounds = [bounds]
        , atsmCoverage = emptyAtlasViewportCoverage
        }
  in AtlasBuildResult
    { abrKey = key
    , abrSnapshotVersion = snapshotVersion
    , abrHexRadius = hexRadius
    , abrManifest = manifest
    , abrTileIndex = 0
    , abrTileBounds = bounds
    , abrTile = tile
    , abrDayNightTile = Nothing
    }

spec :: Spec
spec = describe "AtlasResultBroker" $ do
  it "reports pending results without draining them" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 (tsVersion sampleTerrainSnapshot)
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        result = mkResult key (SnapshotVersion 1) 6 tile
    atlasResultsPending ref `shouldReturn` False
    pushAtlasResult ref result
    atlasResultsPending ref `shouldReturn` True
    drained <- drainAtlasResultsN ref 1
    map abrHexRadius drained `shouldBe` [6]
    atlasResultsPending ref `shouldReturn` False

  it "reports pending counts without draining" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 (tsVersion sampleTerrainSnapshot)
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        result = mkResult key (SnapshotVersion 1) 6 tile
    atlasResultsPendingCount ref `shouldReturn` 0
    pushAtlasResult ref result
    pushAtlasResult ref result
    atlasResultsPendingCount ref `shouldReturn` 2
    drained <- drainAtlasResultsN ref 1
    length drained `shouldBe` 1
    atlasResultsPendingCount ref `shouldReturn` 1

  it "reports stale drops and upload-budget backlog in fresh drain stats" $ do
    ref <- newIORef []
    let freshKey = AtlasKey ViewElevation 0 1
        staleKey = AtlasKey ViewBiome 0 1
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        fresh1 = mkResult freshKey (SnapshotVersion 1) 6 tile
        fresh2 = mkResult freshKey (SnapshotVersion 1) 10 tile
        stale = mkResult staleKey (SnapshotVersion 1) 25 tile
    pushAtlasResult ref stale
    pushAtlasResult ref fresh1
    pushAtlasResult ref fresh2
    (results, stats) <- drainFreshResultsNWithStats ref (\r -> abrKey r == freshKey) 1
    map abrHexRadius results `shouldBe` [6]
    stats `shouldBe` AtlasResultDrainStats
      { ardsPendingBefore = 3
      , ardsPendingAfter = 1
      , ardsFreshDrained = 1
      , ardsStaleDropped = 1
      , ardsFreshPreserved = 1
      , ardsBudgetExhausted = True
      }
    formatAtlasResultDrainStats stats `shouldBe` "brokerBefore=3 brokerAfter=1 drained=1 staleDropped=1 preserved=1 uploadBudgetExhausted=True"

  it "drains all worker-published base results when a day/night overlay tile is missing" $ do
    ref <- newAtlasResultRef
    let key = AtlasKey ViewElevation 0 1
        snapshotVersion = SnapshotVersion 7
        buildId = AtlasBuildId 42
        hexRadius = 6
        atlasScale = 1
        baseTiles = [mkTileGeometry hexRadius atlasScale testRect, mkTileGeometry hexRadius atlasScale testRect2]
        dayNightTiles = [mkTileGeometry hexRadius atlasScale testRect]
        coverage = atlasViewportCoverageFromKeys [101, 202]
        results = atlasBuildResultsForTiles buildId key snapshotVersion hexRadius atlasScale coverage baseTiles (Just (testDayNightKey, dayNightTiles))
    length results `shouldBe` 2
    map abrTileIndex results `shouldBe` [0, 1]
    map (fmap (atgBounds . adntTile) . abrDayNightTile) results `shouldBe` [Just testRect, Nothing]
    map (fmap adntKey . abrDayNightTile) results `shouldBe` [Just testDayNightKey, Nothing]
    mapM_ (pushAtlasResult ref) results

    case results of
      [] -> expectationFailure "expected worker results"
      firstResult:_ -> do
        let manifest = abrManifest firstResult
            freshness = AtlasFreshness
              { afKey = key
              , afSnapshotVersion = snapshotVersion
              , afLatestBuildIds = Map.singleton (atlasManifestTarget manifest) buildId
              }
            isFresh result = abrKey result == key
              && abrSnapshotVersion result == snapshotVersion
              && atlasManifestTarget (abrManifest result) == atlasManifestTarget manifest
              && atsmBuildId (abrManifest result) == buildId
        (drained, stats) <- drainFreshResultsNWithStats ref isFresh 10
        map abrTileIndex drained `shouldBe` [0, 1]
        map (fmap (atgBounds . adntTile) . abrDayNightTile) drained `shouldBe` [Just testRect, Nothing]
        map (fmap adntKey . abrDayNightTile) drained `shouldBe` [Just testDayNightKey, Nothing]
        atsmCoverage (abrManifest firstResult) `shouldBe` coverage
        stats `shouldBe` AtlasResultDrainStats
          { ardsPendingBefore = 2
          , ardsPendingAfter = 0
          , ardsFreshDrained = 2
          , ardsStaleDropped = 0
          , ardsFreshPreserved = 0
          , ardsBudgetExhausted = False
          }

        let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just key }
            cache1 = foldl storeDrainedResult cache0 drained
            (resolvedTiles, status, cache2) = resolveAtlasPureWithFreshness (Just freshness) True True key hexRadius cache1
        fmap length (getCurrentCompleteAtlasForTarget (Just freshness) key hexRadius atlasScale cache1) `shouldBe` Just 2
        isNothing (getCurrentCompleteDayNight (Just freshness) testDayNightKey hexRadius atlasScale cache1) `shouldBe` True
        fmap length (getNearestDayNight testDayNightKey hexRadius cache1) `shouldBe` Just 1
        fmap length resolvedTiles `shouldBe` Just 2
        status `shouldBe` CompleteExact
        fmap length (getCurrentCompleteAtlasForTarget (Just freshness) key hexRadius atlasScale cache2) `shouldBe` Just 2

  it "does not attach a day/night key or tile when worker results have no overlay spec" $ do
    let key = AtlasKey ViewElevation 0 1
        snapshotVersion = SnapshotVersion 7
        buildId = AtlasBuildId 43
        hexRadius = 6
        atlasScale = 1
        baseTiles = [mkTileGeometry hexRadius atlasScale testRect]
        results = atlasBuildResultsForTiles buildId key snapshotVersion hexRadius atlasScale (atlasViewportCoverageFromKeys [303]) baseTiles Nothing
    map (maybe False (const True) . abrDayNightTile) results `shouldBe` [False]

  it "drains results in FIFO order" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 (tsVersion sampleTerrainSnapshot)
        tile1 = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        tile2 = AtlasTileGeometry { atgBounds = Rect (V2 1 1, V2 1 1), atgScale = 1, atgHexRadius = 25, atgChunks = [], atgRiverOverlay = [] }
        result1 = mkResult key (SnapshotVersion 1) 6 tile1
        result2 = mkResult key (SnapshotVersion 1) 25 tile2
    pushAtlasResult ref result1
    pushAtlasResult ref result2
    drained1 <- drainAtlasResultsN ref 1
    drained2 <- drainAtlasResultsN ref 1
    map abrHexRadius (drained1 <> drained2) `shouldBe` [6, 25]
    map abrKey (drained1 <> drained2) `shouldBe` [key, key]

  it "drainFreshResultsN drops stale results without returning them" $ do
    ref <- newIORef []
    let freshKey = AtlasKey ViewElevation 0 1
        staleKey = AtlasKey ViewBiome 0 1
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        freshResult = mkResult freshKey (SnapshotVersion 1) 6 tile
        staleResult = mkResult staleKey (SnapshotVersion 1) 10 tile
    pushAtlasResult ref staleResult
    pushAtlasResult ref freshResult
    (results, staleCount) <- drainFreshResultsN ref (\r -> abrKey r == freshKey) 10
    map abrKey results `shouldBe` [freshKey]
    staleCount `shouldBe` 1
    -- Queue should be empty
    leftover <- drainAtlasResultsN ref 10
    length leftover `shouldBe` 0

  it "drainFreshResultsN preserves excess fresh results for next drain" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 1
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        r1 = mkResult key (SnapshotVersion 1) 6 tile
        r2 = mkResult key (SnapshotVersion 1) 10 tile
    pushAtlasResult ref r1
    pushAtlasResult ref r2
    (results1, _) <- drainFreshResultsN ref (const True) 1
    map abrHexRadius results1 `shouldBe` [6]
    (results2, _) <- drainFreshResultsN ref (const True) 1
    map abrHexRadius results2 `shouldBe` [10]

  it "handles concurrent pushes from multiple threads" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 1
        tile = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        mkResultForRadius hr = mkResult key (SnapshotVersion 1) hr tile
        pushN n hr = replicateM_ n (pushAtlasResult ref (mkResultForRadius hr))
    -- Spawn 3 threads each pushing 100 results concurrently
    barriers <- sequence [newEmptyMVar | _ <- [1 :: Int, 2, 3]]
    forM_ (zip barriers [6, 10, 25 :: Int]) $ \(done, hr) ->
      forkIO (pushN 100 hr >> putMVar done ())
    mapM_ takeMVar barriers
    results <- drainAtlasResultsN ref 300
    length results `shouldBe` 300

testRect :: Rect
testRect = Rect (V2 0 0, V2 64 64)

testRect2 :: Rect
testRect2 = Rect (V2 64 0, V2 64 64)

testDayNightKey :: DayNightKey
testDayNightKey = DayNightKey defaultWorldTime 16 6371 23.44 8 35 0

mkTileGeometry :: Int -> Int -> Rect -> AtlasTileGeometry
mkTileGeometry hexRadius atlasScale bounds = AtlasTileGeometry
  { atgBounds = bounds
  , atgScale = atlasScale
  , atgHexRadius = hexRadius
  , atgChunks = []
  , atgRiverOverlay = []
  }

storeDrainedResult :: AtlasTextureCache -> AtlasBuildResult -> AtlasTextureCache
storeDrainedResult cache result =
  let cache' = storeAtlasTileSet (abrManifest result) [uploadedBaseTile result] cache
  in case abrDayNightTile result of
    Just dnTile -> storeDayNightTileSet (adntKey dnTile) (abrManifest result) [uploadedDayNightTile result dnTile] cache'
    Nothing -> cache'

uploadedBaseTile :: AtlasBuildResult -> TerrainAtlasTile
uploadedBaseTile result = TerrainAtlasTile
  { tatTexture = mockTexture (100 + abrTileIndex result)
  , tatBounds = abrTileBounds result
  , tatScale = atgScale (abrTile result)
  , tatHexRadius = abrHexRadius result
  }

uploadedDayNightTile :: AtlasBuildResult -> AtlasDayNightTile -> TerrainAtlasTile
uploadedDayNightTile result dnResult =
  let dnTile = adntTile dnResult
  in TerrainAtlasTile
    { tatTexture = mockTexture (200 + abrTileIndex result)
    , tatBounds = abrTileBounds result
    , tatScale = atgScale dnTile
    , tatHexRadius = atgHexRadius dnTile
    }

mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral n) :: Ptr ())

sampleTerrainSnapshot :: TerrainSnapshot
sampleTerrainSnapshot = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext
