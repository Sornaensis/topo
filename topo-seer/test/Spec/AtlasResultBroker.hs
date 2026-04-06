module Spec.AtlasResultBroker (spec) where

import Data.IORef (newIORef)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (drainAtlasResultsN, drainFreshResultsN, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.UI (ViewMode(..))
import Linear (V2(..))
import UI.TerrainAtlas (AtlasTileGeometry(..))
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "AtlasResultBroker" $ do
  it "drains results in FIFO order" $ do
    ref <- newIORef []
    let key = AtlasKey ViewElevation 0 (tsVersion sampleTerrainSnapshot)
        tile1 = AtlasTileGeometry { atgBounds = Rect (V2 0 0, V2 1 1), atgScale = 1, atgHexRadius = 6, atgChunks = [], atgRiverOverlay = [] }
        tile2 = AtlasTileGeometry { atgBounds = Rect (V2 1 1, V2 1 1), atgScale = 1, atgHexRadius = 25, atgChunks = [], atgRiverOverlay = [] }
        result1 = AtlasBuildResult
          { abrKey = key
          , abrHexRadius = 6
          , abrTile = tile1
          , abrDayNightTile = Nothing
          }
        result2 = AtlasBuildResult
          { abrKey = key
          , abrHexRadius = 25
          , abrTile = tile2
          , abrDayNightTile = Nothing
          }
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
        freshResult = AtlasBuildResult { abrKey = freshKey, abrHexRadius = 6, abrTile = tile, abrDayNightTile = Nothing }
        staleResult = AtlasBuildResult { abrKey = staleKey, abrHexRadius = 10, abrTile = tile, abrDayNightTile = Nothing }
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
        r1 = AtlasBuildResult { abrKey = key, abrHexRadius = 6, abrTile = tile, abrDayNightTile = Nothing }
        r2 = AtlasBuildResult { abrKey = key, abrHexRadius = 10, abrTile = tile, abrDayNightTile = Nothing }
    pushAtlasResult ref r1
    pushAtlasResult ref r2
    (results1, _) <- drainFreshResultsN ref (const True) 1
    map abrHexRadius results1 `shouldBe` [6]
    (results2, _) <- drainFreshResultsN ref (const True) 1
    map abrHexRadius results2 `shouldBe` [10]

sampleTerrainSnapshot :: TerrainSnapshot
sampleTerrainSnapshot = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
