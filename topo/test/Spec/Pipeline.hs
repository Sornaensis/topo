module Spec.Pipeline (spec) where

import Test.Hspec
import Data.Either (isRight)
import Data.Text (pack)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Pipeline" $ do
  it "creates a base height stage" $ do
    let stage = generateBaseHeightStage defaultGenConfig
    stageName stage `shouldBe` pack "generateBaseHeight"
    stageSeedTag stage `shouldBe` pack "generateBaseHeight"

  it "generates base height across multiple chunks" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateBaseHeightStage defaultGenConfig]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    IntMap.size (twTerrain world1) `shouldSatisfy` (> 1)

  it "respects chunk radius in base height" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        gen0 = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
        gen1 = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 1 }
        pipeline0 = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen0]
          , pipelineSnapshots = False
          }
        pipeline1 = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen1]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline pipeline0 env world0
    resultB <- runPipeline pipeline1 env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    IntMap.size (twTerrain worldA) `shouldBe` 1
    IntMap.size (twTerrain worldB) `shouldSatisfy` (> 1)

  it "respects rectangular world extents in base height" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        extent = worldExtentOrDefault 1 2
        gen = defaultGenConfig { gcWorldExtent = extent }
        pipeline = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        expected = (2 * 1 + 1) * (2 * 2 + 1)
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    IntMap.size (twTerrain world1) `shouldBe` expected

  it "applies worldPrecip scaling" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        cfgA = defaultWorldGenConfig
        cfgB = defaultWorldGenConfig
          { worldPrecip = PrecipConfig
              { precipEvaporation = 0
              , precipRainShadow = 1
              , precipBoundary = 1
              , precipOrographic = 1
              , precipCoastal = 1
              }
          }
        pipelineA = buildFullPipelineConfig cfgA 42
        pipelineB = buildFullPipelineConfig cfgB 42
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline pipelineA env world0
    resultB <- runPipeline pipelineB env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    let avg v = U.sum v / fromIntegral (max 1 (U.length v))
        cid = chunkIdFromCoord (ChunkCoord 0 0)
    case (getClimateChunk cid worldA, getClimateChunk cid worldB) of
      (Just ca, Just cb) -> avg (ccPrecipAvg cb) `shouldSatisfy` (<= avg (ccPrecipAvg ca))
      _ -> expectationFailure "missing climate chunks"

  it "validates world gen iteration counts" $ do
    let bad = defaultWorldGenConfig
          { worldTerrain = defaultTerrainConfig
              { terrainErosion = defaultErosionConfig { ecHydraulicIterations = -1 }
              }
          }
        badGlacier = defaultWorldGenConfig
          { worldTerrain = defaultTerrainConfig
              { terrainGlacier = defaultGlacierConfig { gcFlowIterations = -1 }
              }
          }
        ok = defaultWorldGenConfig
    validateWorldGenConfig bad `shouldBe` Left (WorldGenNegativeHydraulicIterations (-1))
    validateWorldGenConfig badGlacier `shouldBe` Left (WorldGenNegativeGlacierIterations (-1))
    isRight (validateWorldGenConfig ok) `shouldBe` True

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
