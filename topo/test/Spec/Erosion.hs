module Spec.Erosion (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Erosion" $ do
  it "reduces steep peaks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        peak = generateTerrainChunk config (\(TileCoord x y) -> if x == 1 && y == 1 then 10 else 0)
        world1 = setTerrainChunk (ChunkId 0) peak world0
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyErosionStage defaultErosionConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let heights = tcElevation chunk
        (U.maximum heights) `shouldSatisfy` (< 10)

  it "erodes across chunk boundaries" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        leftChunk = generateTerrainChunk config (\_ -> 1)
        rightChunk = generateTerrainChunk config (\_ -> 0)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) leftChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) rightChunk world0
        cfg = defaultErosionConfig { ecHydraulicIterations = 1, ecThermalIterations = 0, ecRainRate = 1, ecMaxDrop = 1 }
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyErosionStage cfg 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2 of
      Nothing -> expectationFailure "missing left chunk"
      Just chunk -> do
        let TileIndex idx = maybe (TileIndex 0) id (tileIndex config (TileCoord 1 0))
            h = tcElevation chunk U.! idx
        h `shouldBe` 0

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
