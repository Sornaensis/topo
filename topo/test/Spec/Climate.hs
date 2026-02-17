module Spec.Climate (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, PlanetConfig(..), WorldSlice(..))
import Topo.Weather (defaultWeatherConfig)

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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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
        -- Use amplified motion sensitivity and zero noise so the
        -- boundary-motion signal is not overwhelmed by spatial noise.
        climateCfg = defaultClimateConfig
          { ccBoundary = (ccBoundary defaultClimateConfig)
              { bndMotionTemp = 2.0 }
          , ccTemperature = (ccTemperature defaultClimateConfig)
              { tmpNoiseScale = 0 }
          }
        pipeline = PipelineConfig
          { pipelineSeed = 24
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
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
          { ccBoundary = (ccBoundary defaultClimateConfig)
              { bndMotionPrecip = motionPrecipBias } }
        pipeline = PipelineConfig
          { pipelineSeed = 31
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig waterLevel]
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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
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

  it "temperature at equator > temperature at 60° latitude" $ do
    -- Generate climate for a world at the equator and another at 60°N.
    -- The equator world should have higher average temperatures.
    let config = WorldConfig { wcChunkSize = 4 }
        sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
        slice60  = defaultWorldSlice { wsLatCenter = 60, wsLatExtent = 10 }
        worldEq  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq
        world60  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice60
        terrain  = generateTerrainChunk config (\_ -> 0.6)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        setupWorld w = setTerrainChunk cid terrain w
        pipeline = PipelineConfig
          { pipelineSeed = 77
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultEq <- runPipeline pipeline env (setupWorld worldEq)
    result60 <- runPipeline pipeline env (setupWorld world60)
    wEq <- expectPipeline resultEq
    w60 <- expectPipeline result60
    case (getClimateChunk cid wEq, getClimateChunk cid w60) of
      (Just cEq, Just c60) ->
        avgVector (ccTempAvg cEq) `shouldSatisfy` (> avgVector (ccTempAvg c60))
      _ -> expectationFailure "missing climate chunks"

  it "equator slice with defaults matches legacy climate" $ do
    -- A world at the equator with default planet/slice should produce
    -- the same climate as a plain `emptyWorld` (backward compatibility).
    let config = WorldConfig { wcChunkSize = 4 }
        worldDefault = emptyWorld config defaultHexGridMeta
        worldExplicit = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        terrain = generateTerrainChunk config (\_ -> 0.4)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        pipeline = PipelineConfig
          { pipelineSeed = 88
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultD <- runPipeline pipeline env (setTerrainChunk cid terrain worldDefault)
    resultE <- runPipeline pipeline env (setTerrainChunk cid terrain worldExplicit)
    wD <- expectPipeline resultD
    wE <- expectPipeline resultE
    case (getClimateChunk cid wD, getClimateChunk cid wE) of
      (Just cD, Just cE) -> do
        -- Temperature and precipitation vectors should be identical
        U.toList (ccTempAvg cD) `shouldBe` U.toList (ccTempAvg cE)
        U.toList (ccPrecipAvg cD) `shouldBe` U.toList (ccPrecipAvg cE)
      _ -> expectationFailure "missing climate chunks"

  prop "temperature at equator exceeds 60° for arbitrary valid planets" $
    forAll (choose (4778, 9557)) $ \radius ->
      forAll (choose (0.7, 1.3)) $ \insol ->
        ioProperty $ do
          let planet = defaultPlanetConfig { pcRadius = radius, pcInsolation = insol }
              config = WorldConfig { wcChunkSize = 4 }
              sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
              slice60  = defaultWorldSlice { wsLatCenter = 60, wsLatExtent = 10 }
              worldEq  = emptyWorldWithPlanet config defaultHexGridMeta planet sliceEq
              world60  = emptyWorldWithPlanet config defaultHexGridMeta planet slice60
              terrain  = generateTerrainChunk config (\_ -> 0.6)
              cid = chunkIdFromCoord (ChunkCoord 0 0)
              pipeline = PipelineConfig
                { pipelineSeed = 99
                , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          resultEq <- runPipeline pipeline env (setTerrainChunk cid terrain worldEq)
          result60 <- runPipeline pipeline env (setTerrainChunk cid terrain world60)
          wEq <- expectPipelineProp resultEq
          w60 <- expectPipelineProp result60
          case (wEq >>= getClimateChunk cid, w60 >>= getClimateChunk cid) of
            (Just cEq, Just c60) ->
              pure (avgVector (ccTempAvg cEq) > avgVector (ccTempAvg c60))
            _ -> pure False

  it "wind direction differs between trade (15°N) and westerly (45°N) belts" $ do
    -- At Earth defaults the wind belt harmonic is 3, producing 30° bands.
    -- 15°N falls in the trade wind belt and 45°N in the westerlies.
    -- The average wind directions should differ substantially.
    let config = WorldConfig { wcChunkSize = 4 }
        slice15 = defaultWorldSlice { wsLatCenter = 15, wsLatExtent = 5 }
        slice45 = defaultWorldSlice { wsLatCenter = 45, wsLatExtent = 5 }
        world15 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice15
        world45 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice45
        terrain = generateTerrainChunk config (\_ -> 0.6)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        pipeline = PipelineConfig
          { pipelineSeed = 55
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result15 <- runPipeline pipeline env (setTerrainChunk cid terrain world15)
    result45 <- runPipeline pipeline env (setTerrainChunk cid terrain world45)
    w15 <- expectPipeline result15
    w45 <- expectPipeline result45
    case (getClimateChunk cid w15, getClimateChunk cid w45) of
      (Just c15, Just c45) -> do
        -- Use circular mean: atan2(mean(sin(dir)), mean(cos(dir)))
        let circMean dirs =
              let sinMean = avgVector (U.map sin dirs)
                  cosMean = avgVector (U.map cos dirs)
              in atan2 sinMean cosMean
            mean15 = circMean (ccWindDirAvg c15)
            mean45 = circMean (ccWindDirAvg c45)
            angleDiff = abs (mean15 - mean45)
            -- Normalize to [0, pi] range
            diff = min angleDiff (2 * pi - angleDiff)
        -- Wind direction should differ by at least 0.5 radians (~29°)
        diff `shouldSatisfy` (> 0.5)
      _ -> expectationFailure "missing climate chunks"

  -- 4.7.1: equator > high-latitude for varied valid ClimateConfig parameters.
  prop "equator temp exceeds high latitude for varied climate parameters" $
    forAll (choose (0.3, 1.0)) $ \latExp ->
      forAll (choose (0.1, 1.0)) $ \lapseRate ->
        ioProperty $ do
          let climateCfg = defaultClimateConfig
                { ccTemperature = (ccTemperature defaultClimateConfig)
                    { tmpLatitudeExponent = latExp
                    , tmpLapseRate        = lapseRate
                    , tmpNoiseScale       = 0  -- deterministic comparison
                    }
                }
              config   = WorldConfig { wcChunkSize = 4 }
              sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
              slice70  = defaultWorldSlice { wsLatCenter = 70, wsLatExtent = 10 }
              worldEq  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq
              world70  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice70
              terrain  = generateTerrainChunk config (\_ -> 0.6)
              cid      = chunkIdFromCoord (ChunkCoord 0 0)
              pipeline = PipelineConfig
                { pipelineSeed   = 99
                , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          resultEq <- runPipeline pipeline env (setTerrainChunk cid terrain worldEq)
          result70 <- runPipeline pipeline env (setTerrainChunk cid terrain world70)
          wEq <- expectPipelineProp resultEq
          w70 <- expectPipelineProp result70
          case (wEq >>= getClimateChunk cid, w70 >>= getClimateChunk cid) of
            (Just cEq, Just c70) ->
              pure (avgVector (ccTempAvg cEq) > avgVector (ccTempAvg c70))
            _ -> pure False

  -- 4.7.2: ocean tiles should have equal temps regardless of depth.
  it "ocean tiles at same latitude have similar temps regardless of depth" $ do
    let config     = WorldConfig { wcChunkSize = 8 }
        climateCfg = defaultClimateConfig
          { ccTemperature = (ccTemperature defaultClimateConfig)
              { tmpNoiseScale = 0 } }
        deepOcean    = generateTerrainChunk config (\_ -> 0.1)
        shallowOcean = generateTerrainChunk config (\_ -> 0.3)
        cid     = chunkIdFromCoord (ChunkCoord 0 0)
        worldD  = setTerrainChunk cid deepOcean    (emptyWorld config defaultHexGridMeta)
        worldS  = setTerrainChunk cid shallowOcean (emptyWorld config defaultHexGridMeta)
        pipeline = PipelineConfig
          { pipelineSeed   = 42
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultD <- runPipeline pipeline env worldD
    resultS <- runPipeline pipeline env worldS
    wD <- expectPipeline resultD
    wS <- expectPipeline resultS
    case (getClimateChunk cid wD, getClimateChunk cid wS) of
      (Just cD, Just cS) ->
        abs (avgVector (ccTempAvg cD) - avgVector (ccTempAvg cS))
          `shouldSatisfy` (< 0.05)
      _ -> expectationFailure "missing climate chunks"

  -- 4.7.3: ocean moderation warms coastal land at high latitudes.
  it "coastal land is warmer than interior at high latitude" $ do
    let config     = WorldConfig { wcChunkSize = 8 }
        climateCfg = defaultClimateConfig
          { ccTemperature = (ccTemperature defaultClimateConfig)
              { tmpNoiseScale  = 0
              , tmpOceanModeration = 0.5  -- amplify to make effect reliable
              }
          }
        n          = chunkTileCount config
        oceanChunk = generateTerrainChunk config (\_ -> 0.2)
        landChunk  = generateTerrainChunk config (\_ -> 0.6)
        slice70    = defaultWorldSlice { wsLatCenter = 70, wsLatExtent = 10 }
        world0     = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice70
        world1     = setTerrainChunk (chunkIdFromCoord (ChunkCoord (-1) 0)) oceanChunk
                   $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0))    landChunk
                   $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0))    landChunk
                   $ world0
        pipeline = PipelineConfig
          { pipelineSeed   = 55
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    w <- expectPipeline result
    case ( getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) w
         , getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) w ) of
      (Just coastal, Just interior) ->
        avgVector (ccTempAvg coastal) `shouldSatisfy`
          (> avgVector (ccTempAvg interior))
      _ -> expectationFailure "missing climate chunks"

  -- 4.7.4: lapse rate affects only height above sea level.
  prop "lapse rate only affects tiles above sea level" $
    forAll (choose (0.55, 0.99)) $ \elevation ->
      forAll (choose (0.3, 1.0)) $ \lapseRate ->
        ioProperty $ do
          let climateCfg = defaultClimateConfig
                { ccTemperature = (ccTemperature defaultClimateConfig)
                    { tmpLapseRate  = lapseRate
                    , tmpNoiseScale = 0
                    }
                }
              config   = WorldConfig { wcChunkSize = 4 }
              seaLevel = generateTerrainChunk config (\_ -> 0.5)   -- at waterLevel
              elevated = generateTerrainChunk config (\_ -> elevation) -- above waterLevel
              cid      = chunkIdFromCoord (ChunkCoord 0 0)
              worldSL  = setTerrainChunk cid seaLevel (emptyWorld config defaultHexGridMeta)
              worldEl  = setTerrainChunk cid elevated (emptyWorld config defaultHexGridMeta)
              pipeline = PipelineConfig
                { pipelineSeed   = 33
                , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          resultSL <- runPipeline pipeline env worldSL
          resultEl <- runPipeline pipeline env worldEl
          wSL <- expectPipelineProp resultSL
          wEl <- expectPipelineProp resultEl
          case (wSL >>= getClimateChunk cid, wEl >>= getClimateChunk cid) of
            (Just cSL, Just cEl) ->
              -- Sea-level land (zero lapse) should be warmer than elevated land.
              pure (avgVector (ccTempAvg cSL) > avgVector (ccTempAvg cEl))
            _ -> pure False

  -- 4.7.5: integration — a mixed-elevation world has a reasonable temp spread.
  it "integration: mixed-elevation world has reasonable temperature distribution" $ do
    let config  = WorldConfig { wcChunkSize = 8 }
        slice30 = defaultWorldSlice { wsLatCenter = 30, wsLatExtent = 30 }
        world0  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice30
        ocean    = generateTerrainChunk config (\_ -> 0.2)
        coastal  = generateTerrainChunk config (\_ -> 0.55)
        highland = generateTerrainChunk config (\_ -> 0.8)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) ocean
               $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) coastal
               $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 1)) coastal
               $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 1)) highland
               $ world0
        pipeline = PipelineConfig
          { pipelineSeed   = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    w <- expectPipeline result
    let allTemps = concatMap (U.toList . ccTempAvg)
          [ chunk
          | cx <- [0, 1], cy <- [0, 1]
          , Just chunk <- [getClimateChunk (chunkIdFromCoord (ChunkCoord cx cy)) w]
          ]
        n     = length allTemps
        meanT = sum allTemps / fromIntegral n
        minT  = minimum allTemps
        maxT  = maximum allTemps
    -- Should produce non-degenerate output
    n `shouldSatisfy` (> 0)
    -- Mean temperature in a reasonable range (not all-max or all-min)
    meanT `shouldSatisfy` (\t -> t > 0.3 && t < 0.95)
    -- Some spread should exist from the elevation + latitude variation
    (maxT - minT) `shouldSatisfy` (> 0.05)

  ---------------------------------------------------------------------------
  -- Phase 2 tests: Physical Moisture Transport
  ---------------------------------------------------------------------------

  -- 2.9.1: interior tiles receive non-trivial moisture (not just coast).
  it "interior land receives moisture from transport + ITCZ" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        ocean = generateTerrainChunk config (\_ -> 0.2)
        land  = generateTerrainChunk config (\_ -> 0.6)
        world0 = emptyWorld config defaultHexGridMeta
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) ocean
               $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) land
               $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 2 0)) land
               $ world0
        pipeline = PipelineConfig
          { pipelineSeed   = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    w <- expectPipeline result
    case getClimateChunk (chunkIdFromCoord (ChunkCoord 2 0)) w of
      Nothing -> expectationFailure "missing interior climate chunk"
      Just interior ->
        -- Interior land should receive non-zero precipitation via
        -- wind transport, ITCZ boost, or both.
        avgVector (ccPrecipAvg interior) `shouldSatisfy` (> 0.02)

  -- 2.9.2: vegetation recycling materially affects interior precipitation.
  it "vegetation recycling materially affects interior precipitation" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        ocean = generateTerrainChunk config (\_ -> 0.2)
        -- Land with soil moisture (enables landEvapotranspiration)
        landWithMoist = (generateTerrainChunk config (\_ -> 0.6))
          { tcMoisture = U.replicate n 0.3 }
        vegChunk = VegetationChunk
          { vegCover   = U.replicate n 0.8
          , vegAlbedo  = U.replicate n 0.15
          , vegDensity = U.replicate n 0
          }
        setupWorld recycleRate =
          let climateCfg = defaultClimateConfig
                { ccMoisture = (ccMoisture defaultClimateConfig)
                    { moistRecycleRate = recycleRate }
                }
              w0 = emptyWorld config defaultHexGridMeta
              w1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) ocean
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) landWithMoist
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 2 0)) landWithMoist
                 $ w0
              -- Inject vegetation on both land chunks
              vk1 = let ChunkId k = chunkIdFromCoord (ChunkCoord 1 0) in k
              vk2 = let ChunkId k = chunkIdFromCoord (ChunkCoord 2 0) in k
              w2 = w1 { twVegetation = IntMap.fromList [(vk1, vegChunk), (vk2, vegChunk)] }
              pipeline = PipelineConfig
                { pipelineSeed   = 100
                , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
                , pipelineSnapshots = False
                }
          in (pipeline, w2)
        env = TopoEnv { teLogger = \_ -> pure () }
    let (pipWith, worldWith) = setupWorld 0.35
        (pipNone, worldNone) = setupWorld 0.0
    resultWith <- runPipeline pipWith env worldWith
    resultNone <- runPipeline pipNone env worldNone
    wWith <- expectPipeline resultWith
    wNone <- expectPipeline resultNone
    case ( getClimateChunk (chunkIdFromCoord (ChunkCoord 2 0)) wWith
         , getClimateChunk (chunkIdFromCoord (ChunkCoord 2 0)) wNone ) of
      (Just withRecycling, Just noRecycling) -> do
        -- Recycling mechanism is active: interior precipitation differs
        -- measurably between the recycling and no-recycling cases.
        let precipWith = avgVector (ccPrecipAvg withRecycling)
            precipNone = avgVector (ccPrecipAvg noRecycling)
        abs (precipWith - precipNone) `shouldSatisfy` (> 0.001)
        -- Interior precipitation is non-trivial in both scenarios
        precipWith `shouldSatisfy` (> 0.02)
        precipNone `shouldSatisfy` (> 0.02)
      _ -> expectationFailure "missing interior climate chunks"

  -- 2.9.3: ITCZ creates equatorial precipitation enhancement.
  it "ITCZ enhances equatorial precipitation over mid-latitude" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
        slice30  = defaultWorldSlice { wsLatCenter = 30, wsLatExtent = 10 }
        worldEq  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq
        world30  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice30
        -- An ocean chunk provides the moisture source; the land chunk
        -- receives precipitation enhanced (or not) by the ITCZ boost.
        ocean   = generateTerrainChunk config (\_ -> 0.2)
        land    = generateTerrainChunk config (\_ -> 0.6)
        oceanId = chunkIdFromCoord (ChunkCoord 0 0)
        landId  = chunkIdFromCoord (ChunkCoord 1 0)
        pipeline = PipelineConfig
          { pipelineSeed   = 77
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        buildWorld baseW = setTerrainChunk oceanId ocean
                         $ setTerrainChunk landId  land baseW
    resultEq <- runPipeline pipeline env (buildWorld worldEq)
    result30 <- runPipeline pipeline env (buildWorld world30)
    wEq <- expectPipeline resultEq
    w30 <- expectPipeline result30
    case (getClimateChunk landId wEq, getClimateChunk landId w30) of
      (Just cEq, Just c30) ->
        -- Equatorial world should have higher average precipitation
        -- on the land chunk due to ITCZ convergence boost.
        avgVector (ccPrecipAvg cEq) `shouldSatisfy`
          (> avgVector (ccPrecipAvg c30))
      _ -> expectationFailure "missing climate chunks"

  -- 2.9.4: ocean tiles provide persistent moisture source.
  it "ocean tiles maintain high moisture across iterations" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        oceanTerrain = generateTerrainChunk config (\_ -> 0.2)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        world0 = setTerrainChunk cid oceanTerrain (emptyWorld config defaultHexGridMeta)
        pipeline = PipelineConfig
          { pipelineSeed   = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    w <- expectPipeline result
    case getClimateChunk cid w of
      Nothing -> expectationFailure "missing climate chunk"
      Just chunk ->
        -- Ocean tiles with reinjection should have substantial precipitation.
        avgVector (ccPrecipAvg chunk) `shouldSatisfy` (> 0.10)

  ---------------------------------------------------------------------------
  -- Phase 3 tests: Albedo -> Temperature Feedback (Model H)
  ---------------------------------------------------------------------------

  -- 3.3.1: high-albedo surface (ice/snow) is cooler than reference.
  it "high albedo surface is cooler than reference" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        terrain = generateTerrainChunk config (\_ -> 0.6)
        -- High albedo (ice-like)
        iceVeg = VegetationChunk
          { vegCover   = U.replicate n 0.0
          , vegAlbedo  = U.replicate n 0.80
          , vegDensity = U.replicate n 0
          }
        -- Reference albedo (no correction)
        refVeg = VegetationChunk
          { vegCover   = U.replicate n 0.3
          , vegAlbedo  = U.replicate n 0.30
          , vegDensity = U.replicate n 0
          }
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        key = let ChunkId k = cid in k
        makeWorld veg =
          let w = setTerrainChunk cid terrain (emptyWorld config defaultHexGridMeta)
          in w { twVegetation = IntMap.singleton key veg }
        climateCfg = defaultClimateConfig
          { ccTemperature = (ccTemperature defaultClimateConfig)
              { tmpNoiseScale = 0 }
          }
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultIce <- runPipeline pipeline env (makeWorld iceVeg)
    resultRef <- runPipeline pipeline env (makeWorld refVeg)
    wIce <- expectPipeline resultIce
    wRef <- expectPipeline resultRef
    case (getClimateChunk cid wIce, getClimateChunk cid wRef) of
      (Just cIce, Just cRef) ->
        avgVector (ccTempAvg cIce) `shouldSatisfy`
          (< avgVector (ccTempAvg cRef))
      _ -> expectationFailure "missing climate chunks"

  -- 3.3.2: low-albedo surface (forest) is warmer than reference.
  it "low albedo surface (forest) is warmer than reference" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        terrain = generateTerrainChunk config (\_ -> 0.6)
        -- Low albedo (dense forest)
        forestVeg = VegetationChunk
          { vegCover   = U.replicate n 0.9
          , vegAlbedo  = U.replicate n 0.12
          , vegDensity = U.replicate n 0
          }
        -- Reference albedo
        refVeg = VegetationChunk
          { vegCover   = U.replicate n 0.3
          , vegAlbedo  = U.replicate n 0.30
          , vegDensity = U.replicate n 0
          }
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        key = let ChunkId k = cid in k
        makeWorld veg =
          let w = setTerrainChunk cid terrain (emptyWorld config defaultHexGridMeta)
          in w { twVegetation = IntMap.singleton key veg }
        climateCfg = defaultClimateConfig
          { ccTemperature = (ccTemperature defaultClimateConfig)
              { tmpNoiseScale = 0 }
          }
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage climateCfg defaultWeatherConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultForest <- runPipeline pipeline env (makeWorld forestVeg)
    resultRef    <- runPipeline pipeline env (makeWorld refVeg)
    wForest <- expectPipeline resultForest
    wRef    <- expectPipeline resultRef
    case (getClimateChunk cid wForest, getClimateChunk cid wRef) of
      (Just cForest, Just cRef) ->
        avgVector (ccTempAvg cForest) `shouldSatisfy`
          (> avgVector (ccTempAvg cRef))
      _ -> expectationFailure "missing climate chunks"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

-- | Property-test-friendly variant: returns 'Maybe TerrainWorld' instead
-- of failing via 'expectationFailure' (which throws inside QuickCheck).
expectPipelineProp :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO (Maybe TerrainWorld)
expectPipelineProp result =
  case result of
    Left _          -> pure Nothing
    Right (world, _) -> pure (Just world)
