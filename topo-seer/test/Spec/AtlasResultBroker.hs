module Spec.AtlasResultBroker (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildResult(..), AtlasTileSetManifest(..))
import Actor.AtlasResultBroker (atlasResultsPending, drainAtlasResultsN, drainFreshResultsN, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.UI (ViewMode(..))
import Linear (V2(..))
import UI.TerrainAtlas (AtlasTileGeometry(..))
import UI.Widgets (Rect(..))

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

sampleTerrainSnapshot :: TerrainSnapshot
sampleTerrainSnapshot = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore
