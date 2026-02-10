module Spec.Weather (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Weather" $ do
  it "applies seasonal offsets" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate (chunkTileCount config) 0.5
          , ccPrecipAvg = U.replicate (chunkTileCount config) 0.5
          , ccWindDirAvg = U.replicate (chunkTileCount config) 0
          , ccWindSpdAvg = U.replicate (chunkTileCount config) 0
          }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
        cfgA = defaultWeatherConfig { wcSeasonPhase = 0 }
        cfgB = defaultWeatherConfig { wcSeasonPhase = 3.14159 }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipelineA = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [tickWeatherStage cfgA]
          , pipelineSnapshots = False
          }
        pipelineB = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [tickWeatherStage cfgB]
          , pipelineSnapshots = False
          }
    resultA <- runPipeline pipelineA env world0
    resultB <- runPipeline pipelineB env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    case (getWeatherChunk (ChunkId 0) worldA, getWeatherChunk (ChunkId 0) worldB) of
      (Just chunkA, Just chunkB) ->
        (wcTemp chunkA U.! 0) `shouldNotBe` (wcTemp chunkB U.! 0)
      _ -> expectationFailure "missing weather chunk"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
