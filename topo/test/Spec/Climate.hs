module Spec.Climate (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

avgVector :: U.Vector Float -> Float
avgVector vec =
  U.sum vec / max 1 (fromIntegral (U.length vec))

spec :: Spec
spec = describe "Climate" $ do
  it "produces temperature and precipitation" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = generateTerrainChunk config (\_ -> 0.2)
        world1 = setTerrainChunk (ChunkId 0) terrain world0
        pipeline = PipelineConfig
          { pipelineSeed = 9
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getClimateChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing climate chunk"
      Just chunk -> do
        U.length (ccTempAvg chunk) `shouldBe` chunkTileCount config
        U.length (ccPrecipAvg chunk) `shouldBe` chunkTileCount config
        U.any (> 0) (ccPrecipAvg chunk) `shouldBe` True

  it "produces precipitation across multiple chunks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        low = generateTerrainChunk config (\_ -> 0)
        high = generateTerrainChunk config (\_ -> 0.6)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) low
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) high world0
        pipeline = PipelineConfig
          { pipelineSeed = 11
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) -> do
        U.any (> 0) (ccPrecipAvg left) `shouldBe` True
        U.any (> 0) (ccPrecipAvg right) `shouldBe` True
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation near convergent boundaries" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        boundaryChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryConvergent }
        plainChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryNone }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) boundaryChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) plainChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures near convergent boundaries" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        boundaryChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryConvergent }
        plainChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryNone }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) boundaryChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) plainChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures with faster boundary motion" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        slow = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 0.0
          , tcPlateVelY = U.replicate n 0.0
          }
        fast = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 1.0
          , tcPlateVelY = U.replicate n 0.0
          }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) fast
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) slow world0
        pipeline = PipelineConfig
          { pipelineSeed = 24
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation with faster boundary motion" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        waterLevel = 0.5
        motionPrecipBias = 2.0
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        slow = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 0.0
          , tcPlateVelY = U.replicate n 0.0
          }
        fast = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 1.0
          , tcPlateVelY = U.replicate n 0.0
          }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) fast
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) slow world0
        climateCfg = defaultClimateConfig
          { ccBoundaryMotionPrecip = motionPrecipBias }
        pipeline = PipelineConfig
          { pipelineSeed = 31
          , pipelineStages = [generateClimateStage climateCfg waterLevel]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures over higher plate heights" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        highPlate = baseTerrain { tcPlateHeight = U.replicate n 0.9 }
        lowPlate = baseTerrain { tcPlateHeight = U.replicate n 0.2 }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) highPlate
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) lowPlate world0
        pipeline = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation over higher plate heights" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        highPlate = baseTerrain { tcPlateHeight = U.replicate n 0.9 }
        lowPlate = baseTerrain { tcPlateHeight = U.replicate n 0.2 }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) highPlate
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) lowPlate world0
        pipeline = PipelineConfig
          { pipelineSeed = 9
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
