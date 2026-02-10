module Spec.Biome (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Biome" $ do
  it "classifies biomes into terrain flags" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.replicate n 0.2 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.8
          , ccPrecipAvg = U.replicate n 0.9
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> do
        let flags = tcFlags chunk
        U.any (/= BiomeDesert) flags `shouldBe` True

  it "assigns ocean and mountain elevation bands" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.fromList [-0.1, 0.02, 0.8, 0.95] }
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.1]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> do
        let flags = tcFlags chunk
        flags U.! 0 `shouldBe` BiomeOcean
        flags U.! 1 `shouldBe` BiomeOcean
        flags U.! 3 `shouldBe` BiomeSnow

  it "treats low elevations as ocean when water level rises" $ do
    let config = WorldConfig { wcChunkSize = 1 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = (emptyTerrainChunk config) { tcElevation = U.fromList [0.04] }
        climate = ClimateChunk
          { ccTempAvg = U.replicate 1 0.5
          , ccPrecipAvg = U.replicate 1 0.5
          , ccWindDirAvg = U.replicate 1 0
          , ccWindSpdAvg = U.replicate 1 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.05]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> tcFlags chunk U.! 0 `shouldBe` BiomeOcean

  it "boosts vegetation fertility with volcanic ash" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.replicate n 0.3 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.7
          , ccPrecipAvg = U.replicate n 0.8
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          }
        volcanism = (emptyVolcanismChunk config)
          { vcAshPotential = U.replicate n 1
          , vcLavaPotential = U.replicate n 0
          }
        biomeCfg = defaultBiomeConfig
          { bcVolcanicAshBoost = 0.4
          , bcVolcanicLavaPenalty = 0
          }
        worldBase = setClimateChunk (ChunkId 0) climate
          (setTerrainChunk (ChunkId 0) terrain world0)
        worldAsh = setVolcanismChunk (ChunkId 0) volcanism worldBase
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage biomeCfg 0.1]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    baseResult <- runPipeline pipeline env worldBase
    ashResult <- runPipeline pipeline env worldAsh
    worldBase' <- expectPipeline baseResult
    worldAsh' <- expectPipeline ashResult
    case (getTerrainChunk (ChunkId 0) worldBase', getTerrainChunk (ChunkId 0) worldAsh') of
      (Just baseChunk, Just ashChunk) ->
        tcFertility ashChunk U.! 0 `shouldSatisfy` (> tcFertility baseChunk U.! 0)
      _ -> expectationFailure "missing terrain chunk"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
