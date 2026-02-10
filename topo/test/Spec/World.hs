module Spec.World (spec) where

import Test.Hspec
import Topo

spec :: Spec
spec = describe "World" $ do
  it "rejects non-positive chunk sizes" $ do
    mkWorldConfig 0 `shouldBe` Left WorldConfigNonPositiveChunkSize
    mkWorldConfig (-1) `shouldBe` Left WorldConfigNonPositiveChunkSize

  it "accepts positive chunk sizes" $ do
    mkWorldConfig 4 `shouldBe` Right (WorldConfig { wcChunkSize = 4 })

  it "starts empty" $ do
    let config = WorldConfig { wcChunkSize = 64 }
        world = emptyWorld config defaultHexGridMeta
    getTerrainChunk (ChunkId 0) world `shouldBe` Nothing

  it "sets and gets elevation" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world = emptyWorld config defaultHexGridMeta
        world' = setElevationAt (ChunkId 0) (TileCoord 1 2) 42 world
    getElevationAt (ChunkId 0) (TileCoord 1 2) world' `shouldBe` Just 42
