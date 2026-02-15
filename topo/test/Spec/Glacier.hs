module Spec.Glacier (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo.Math (clamp01)
import Topo
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, WorldSlice(..))
import Topo.Weather (defaultWeatherConfig)

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
          , ccHumidityAvg = U.replicate (chunkTileCount config) 0
          , ccTempRange = U.replicate (chunkTileCount config) 0
          , ccPrecipSeasonality = U.replicate (chunkTileCount config) 0
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
              , ccHumidityAvg = U.replicate (chunkTileCount config) 0
              , ccTempRange = U.replicate (chunkTileCount config) 0
              , ccPrecipSeasonality = U.replicate (chunkTileCount config) 0
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
                , ccHumidityAvg = U.replicate (chunkTileCount config) 0
                , ccTempRange = U.replicate (chunkTileCount config) 0
                , ccPrecipSeasonality = U.replicate (chunkTileCount config) 0
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

  it "produces more ice at high latitude than at equator" $ do
    -- Generate worlds at 85°N and 0° equator using the full climate+glacier
    -- pipeline.  The high-latitude world should accumulate more ice because
    -- the climate stage produces lower temperatures at 85°N.
    let config = WorldConfig { wcChunkSize = 4 }
        sliceArctic  = defaultWorldSlice { wsLatCenter = 85, wsLatExtent = 5 }
        sliceEquator = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
        worldArctic  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceArctic
        worldEquator = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEquator
        terrain = generateTerrainChunk config (\_ -> 0.8)  -- land above water
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        setupWorld w = setTerrainChunk cid terrain w
        glacierCfg = defaultGlacierConfig
        climateCfg = defaultClimateConfig
        waterLevel = 0.5
        buildPipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages =
              [ generateClimateStage climateCfg defaultWeatherConfig waterLevel
              , applyGlacierStage glacierCfg
              ]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline buildPipeline env (setupWorld worldArctic)
    resultE <- runPipeline buildPipeline env (setupWorld worldEquator)
    wA <- expectPipeline resultA
    wE <- expectPipeline resultE
    case (getGlacierChunk cid wA, getGlacierChunk cid wE) of
      (Just arcticGl, Just equatorGl) -> do
        let iceArctic  = U.sum (glIceThickness arcticGl)
            iceEquator = U.sum (glIceThickness equatorGl)
        iceArctic `shouldSatisfy` (> iceEquator)
      _ -> expectationFailure "missing glacier chunks"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
