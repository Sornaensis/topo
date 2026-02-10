module Spec.Volcanism (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Volcanism" $ do
  it "writes volcanism chunks and modifies elevation" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
        world1 = setTerrainChunk (ChunkId 0) terrain world0
        pipeline = PipelineConfig
          { pipelineSeed = 21
          , pipelineStages = [applyVolcanismStage defaultVolcanismConfig]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    getVolcanismChunk (ChunkId 0) world2 `shouldSatisfy` isJust
    getElevationAt (ChunkId 0) (TileCoord 0 0) world2 `shouldSatisfy` isJust

  it "applies lava and ash to terrain parameters" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = emptyTerrainChunk config
        volcCfg = defaultVolcanismConfig
          { vcVentDensityBase = 1
          , vcVentThreshold = 0
          , vcHotspotScale = 0
          , vcHotspotThreshold = 0
          , vcActiveThreshold = 0
          , vcEruptThreshold = 0
          , vcMagmaRecharge = 1
          , vcEruptMagmaCost = 1
          , vcLavaScale = 1
          , vcAshScale = 1
          , vcDepositScale = 1
          , vcRockDensityLavaBoost = 0.5
          , vcSoilGrainAshBoost = 0.5
          }
        world1 = setTerrainChunk (ChunkId 0) terrain world0
        pipeline = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [applyVolcanismStage volcCfg]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> do
        U.any (> 0) (tcRockDensity chunk) `shouldBe` True
        U.any (> 0) (tcSoilGrain chunk) `shouldBe` True
        U.all (<= 1) (tcRockDensity chunk) `shouldBe` True
        U.all (<= 1) (tcSoilGrain chunk) `shouldBe` True

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
