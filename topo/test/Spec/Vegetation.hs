module Spec.Vegetation (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Vegetation" $ do
  it "generates non-zero density in wet biomes" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        biomes = U.replicate n BiomeForest
        temp = U.replicate n 0.8
        precip = U.replicate n 0.9
        density = vegetationDensityChunk defaultVegetationConfig biomes temp precip
    U.any (> 0) density `shouldBe` True
