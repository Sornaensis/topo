module Spec.Weather (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Topo
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, PlanetConfig(..))

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

  it "seasonal amplitude scales with axial tilt" $ do
    -- A planet with 0° tilt should have no seasonal variation,
    -- while 45° tilt should produce more variation than 23.44°.
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          }
        planetNoTilt  = defaultPlanetConfig { pcAxialTilt = 0 }
        planetEarth   = defaultPlanetConfig { pcAxialTilt = 23.44 }
        planetMaxTilt = defaultPlanetConfig { pcAxialTilt = 45 }
        worldNoTilt  = setClimateChunk (ChunkId 0) climate
                       (emptyWorldWithPlanet config defaultHexGridMeta planetNoTilt defaultWorldSlice)
        worldEarth   = setClimateChunk (ChunkId 0) climate
                       (emptyWorldWithPlanet config defaultHexGridMeta planetEarth defaultWorldSlice)
        worldMaxTilt = setClimateChunk (ChunkId 0) climate
                       (emptyWorldWithPlanet config defaultHexGridMeta planetMaxTilt defaultWorldSlice)
        weatherCfg = defaultWeatherConfig { wcSeasonPhase = 1.5708 }  -- pi/2
        env = TopoEnv { teLogger = \_ -> pure () }
        mkPipeline = PipelineConfig
          { pipelineSeed = 2
          , pipelineStages = [tickWeatherStage weatherCfg]
          , pipelineSnapshots = False
          }
    resultNoTilt  <- runPipeline mkPipeline env worldNoTilt
    resultEarth   <- runPipeline mkPipeline env worldEarth
    resultMaxTilt <- runPipeline mkPipeline env worldMaxTilt
    wNoTilt  <- expectPipeline resultNoTilt
    wEarth   <- expectPipeline resultEarth
    wMaxTilt <- expectPipeline resultMaxTilt
    case ( getWeatherChunk (ChunkId 0) wNoTilt
         , getWeatherChunk (ChunkId 0) wEarth
         , getWeatherChunk (ChunkId 0) wMaxTilt ) of
      (Just wkNoTilt, Just wkEarth, Just wkMaxTilt) -> do
        let tempRange w = U.maximum (wcTemp w) - U.minimum (wcTemp w)
            rangeNoTilt  = tempRange wkNoTilt
            rangeEarth   = tempRange wkEarth
            rangeMaxTilt = tempRange wkMaxTilt
        -- 0° tilt → near-zero range (only noise variation)
        -- 45° tilt → greater range than 23.44°
        rangeNoTilt `shouldSatisfy` (< rangeEarth)
        rangeEarth  `shouldSatisfy` (< rangeMaxTilt)
      _ -> expectationFailure "missing weather chunks"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
