module Spec.Tectonics (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import qualified Data.List as List
import Topo

spec :: Spec
spec = describe "Tectonics" $ do
  it "marks plate ids in flags" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 123
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let ids = tcPlateId chunk
        U.any (/= 0) ids `shouldBe` True

  it "produces multiple plate ids in a chunk" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 987
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let ids = U.toList (tcPlateId chunk)
            uniqueCount = length (List.nub ids)
        uniqueCount `shouldSatisfy` (> 1)

  it "marks plate boundary types" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 456
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let boundaries = tcPlateBoundary chunk
        U.any (/= PlateBoundaryNone) boundaries `shouldBe` True

  it "keeps boundary density in a reasonable range" $ do
    let config = WorldConfig { wcChunkSize = 24 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 321
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let boundaries = tcPlateBoundary chunk
            total = U.length boundaries
            boundaryCount = U.length (U.filter (/= PlateBoundaryNone) boundaries)
            ratio = fromIntegral boundaryCount / max 1 (fromIntegral total)
        ratio `shouldSatisfy` (> 0.01)

  it "produces plate height variation" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 654
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let heights = tcPlateHeight chunk
            minH = U.minimum heights
            maxH = U.maximum heights
        maxH - minH `shouldSatisfy` (> 0)

  it "produces plate hardness variation" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 222
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let hardness = tcPlateHardness chunk
            minH = U.minimum hardness
            maxH = U.maximum hardness
        maxH - minH `shouldSatisfy` (> 0)

  it "stores plate crust and age" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 777
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let crust = tcPlateCrust chunk
            ages = tcPlateAge chunk
        U.any (\v -> v == 0 || v == 1) crust `shouldBe` True
        U.all (\v -> v >= 0 && v <= 1) ages `shouldBe` True

  it "stores plate velocity vectors" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 888
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let vx = tcPlateVelX chunk
            vy = tcPlateVelY chunk
            maxSpeed = U.maximum (U.zipWith (\x y -> abs x + abs y) vx vy)
        maxSpeed `shouldSatisfy` (> 0)

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
