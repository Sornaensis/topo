module Spec.Export (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Export" $ do
  it "selects chunks that intersect a region" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        chunkA = generateTerrainChunk config (\_ -> 1)
        chunkB = generateTerrainChunk config (\_ -> 2)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) chunkA
                  (setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) chunkB world0)
        region = RegionRect (TileCoord 0 0) (TileCoord 5 3)
    case exportTerrainChunksRegion world1 region of
      Left err -> expectationFailure (show err)
      Right exported -> do
        let ids = map fst exported
        ids `shouldContain` [chunkIdFromCoord (ChunkCoord 0 0), chunkIdFromCoord (ChunkCoord 1 0)]
        length ids `shouldBe` 2

  it "exports biome chunks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = (emptyTerrainChunk config) { tcFlags = U.replicate (chunkTileCount config) BiomeForest }
        world1 = setTerrainChunk (ChunkId 0) terrain world0
    case exportBiomeChunks world1 of
      Left err -> expectationFailure (show err)
      Right exported -> length exported `shouldBe` 1

  it "keeps climate chunk exports consistent" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        climate = (emptyClimateChunk config) { ccTempAvg = U.replicate (chunkTileCount config) 5 }
        world1 = setClimateChunk (ChunkId 0) climate world0
        region = RegionRect (TileCoord 0 0) (TileCoord 1 1)
    case (exportClimateChunks world1, exportClimateChunksRegion world1 region) of
      (Right full, Right regionExport) ->
        lookup (ChunkId 0) regionExport `shouldBe` lookup (ChunkId 0) full
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)
