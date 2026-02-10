module Spec.Mesh (spec) where

import Test.Hspec
import Topo

spec :: Spec
spec = describe "Mesh" $ do
  it "builds a mesh patch with expected counts" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world = emptyWorld config defaultHexGridMeta
        region = RegionRect (TileCoord 0 0) (TileCoord 1 1)
        mesh = meshPatch world region
    length (meshVertices mesh) `shouldBe` 9
    length (meshIndices mesh) `shouldBe` 24
