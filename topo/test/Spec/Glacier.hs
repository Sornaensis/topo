module Spec.Glacier (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo.Math (clamp01)
import Topo

spec :: Spec
spec = describe "Glacier" $ do
  it "writes glacier chunks and modifies elevation" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
        climate = ClimateChunk
          { ccTempAvg = U.replicate (chunkTileCount config) 0.1
          , ccPrecipAvg = U.replicate (chunkTileCount config) 0.8
          , ccWindDirAvg = U.replicate (chunkTileCount config) 0
          , ccWindSpdAvg = U.replicate (chunkTileCount config) 0
          }
        world1 = setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 11
          , pipelineStages = [applyGlacierStage defaultGlacierConfig]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    getGlacierChunk (ChunkId 0) world2 `shouldSatisfy` isJust
    getElevationAt (ChunkId 0) (TileCoord 0 0) world2 `shouldSatisfy` isJust

  prop "ice stays zero above melt temp" $
    forAll (choose (0.6, 1.5)) $ \temp ->
      ioProperty $ do
        let config = WorldConfig { wcChunkSize = 2 }
            world0 = emptyWorld config defaultHexGridMeta
            terrain = generateTerrainChunk config (\_ -> 1)
            climate = ClimateChunk
              { ccTempAvg = U.replicate (chunkTileCount config) temp
              , ccPrecipAvg = U.replicate (chunkTileCount config) 1
              , ccWindDirAvg = U.replicate (chunkTileCount config) 0
              , ccWindSpdAvg = U.replicate (chunkTileCount config) 0
              }
            world1 = setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0)
            pipeline = PipelineConfig
              { pipelineSeed = 12
              , pipelineStages = [applyGlacierStage defaultGlacierConfig]
              , pipelineSnapshots = False
              }
            env = TopoEnv { teLogger = \_ -> pure () }
        result <- runPipeline pipeline env world1
        world2 <- expectPipeline result
        case getGlacierChunk (ChunkId 0) world2 of
          Nothing -> pure False
          Just glacier -> pure (U.all (== 0) (glIceThickness glacier))

  prop "uniform snowpack preserves mass after flow" $
    forAll (choose (0.0, 0.2)) $ \temp ->
      forAll (choose (0.4, 1.0)) $ \precip ->
        ioProperty $ do
          let config = WorldConfig { wcChunkSize = 2 }
              world0 = emptyWorld config defaultHexGridMeta
              terrain = generateTerrainChunk config (\_ -> 1)
              climate = ClimateChunk
                { ccTempAvg = U.replicate (chunkTileCount config) temp
                , ccPrecipAvg = U.replicate (chunkTileCount config) precip
                , ccWindDirAvg = U.replicate (chunkTileCount config) 0
                , ccWindSpdAvg = U.replicate (chunkTileCount config) 0
                }
              world1 = setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0)
              pipeline = PipelineConfig
                { pipelineSeed = 13
                , pipelineStages = [applyGlacierStage defaultGlacierConfig]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
              snowExpected =
                let range = max 0.0001 (gcSnowRange defaultGlacierConfig)
                    factor = clamp01 ((gcSnowTemp defaultGlacierConfig - temp) / range)
                in precip * gcAccumScale defaultGlacierConfig * factor
              meltExpected =
                if temp > gcMeltTemp defaultGlacierConfig
                  then (temp - gcMeltTemp defaultGlacierConfig) * gcMeltRate defaultGlacierConfig
                  else 0
              iceExpected = max 0 (snowExpected - meltExpected)
          result <- runPipeline pipeline env world1
          world2 <- expectPipeline result
          case getGlacierChunk (ChunkId 0) world2 of
            Nothing -> pure False
            Just glacier -> pure (U.all (\v -> abs (v - iceExpected) < 1e-5) (glIceThickness glacier))

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
