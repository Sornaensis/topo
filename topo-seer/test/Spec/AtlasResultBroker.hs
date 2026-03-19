module Spec.AtlasResultBroker (spec) where

import Data.IORef (newIORef)
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (drainAtlasResultsN, pushAtlasResult)
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
        tile1 = AtlasTileGeometry (Rect (V2 0 0, V2 1 1)) 1 [] []
        tile2 = AtlasTileGeometry (Rect (V2 1 1, V2 1 1)) 2 [] []
        result1 = AtlasBuildResult
          { abrKey = key
          , abrScale = 1
          , abrTile = tile1
          }
        result2 = AtlasBuildResult
          { abrKey = key
          , abrScale = 2
          , abrTile = tile2
          }
    pushAtlasResult ref result1
    pushAtlasResult ref result2
    drained1 <- drainAtlasResultsN ref 1
    drained2 <- drainAtlasResultsN ref 1
    map abrScale (drained1 <> drained2) `shouldBe` [1, 2]
    map abrKey (drained1 <> drained2) `shouldBe` [key, key]

sampleTerrainSnapshot :: TerrainSnapshot
sampleTerrainSnapshot = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
