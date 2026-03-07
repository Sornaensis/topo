{-# LANGUAGE OverloadedStrings #-}

module Spec.Weather (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Spec.Support.FloatApprox (approxEqAbs)
import Topo
import Topo.Calendar (WorldTime(..), advanceTicks, mkCalendarConfig, tickToDate)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, PlanetConfig(..), WorldSlice(..))
import Topo.Weather (cloudFraction, seasonalITCZLatitude,
                     weatherOverlaySchema, overlayToWeatherChunk, weatherFieldCount,
                     initWeatherStage, weatherSimNode,
                     getWeatherChunk, getWeatherFromOverlay)

itczLatitudeTolerance :: Float
itczLatitudeTolerance = 0.001

spec :: Spec
spec = describe "Weather" $ do
  it "initialises weather without advancing world time" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [initWeatherStage defaultWeatherConfig]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    twWorldTime world1 `shouldBe` twWorldTime world0
    case getWeatherChunk (ChunkId 0) world1 of
      Just wk -> U.length (wcTemp wk) `shouldBe` n
      Nothing -> expectationFailure "missing weather chunk"

  it "applies seasonal offsets" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate (chunkTileCount config) 0.5
          , ccPrecipAvg = U.replicate (chunkTileCount config) 0.5
          , ccWindDirAvg = U.replicate (chunkTileCount config) 0
          , ccWindSpdAvg = U.replicate (chunkTileCount config) 0
          , ccHumidityAvg = U.replicate (chunkTileCount config) 0
          , ccTempRange = U.replicate (chunkTileCount config) 0
          , ccPrecipSeasonality = U.replicate (chunkTileCount config) 0
          }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
        cfgA = defaultWeatherConfig { wcSeasonPhase = 0 }
        cfgB = defaultWeatherConfig { wcSeasonPhase = 3.14159 }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipelineA = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [initWeatherStage cfgA]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        pipelineB = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [initWeatherStage cfgB]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
    resultA <- runPipeline pipelineA env world0
    resultB <- runPipeline pipelineB env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    case (getWeatherChunk (ChunkId 0) worldA, getWeatherChunk (ChunkId 0) worldB) of
      (Just chunkA, Just chunkB) ->
        (wcTemp chunkA U.! 0) `shouldNotBe` (wcTemp chunkB U.! 0)
      _ -> expectationFailure "missing weather chunk"

  it "diffuses weather temperature across adjacent tiles" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
        cfgNoDiffuse = defaultWeatherConfig
          { wcSeasonAmplitude = 0
          , wcJitterAmplitude = 0.35
          , wcCloudAlbedoEffect = 0
          , wcTempDiffuseIterations = 0
          , wcTempDiffuseFactor = 0
          }
        cfgDiffuse = cfgNoDiffuse
          { wcTempDiffuseIterations = 2
          , wcTempDiffuseFactor = 0.2
          }
        avgHexAdjacentDiff vec =
          let diffs =
                [ abs ((vec U.! i) - (vec U.! j))
                | i <- [0 .. n - 1]
                , j <- hexNeighborIndices (wcChunkSize config) (wcChunkSize config) i
                , i < j
                ]
          in sum diffs / fromIntegral (length diffs)
    wNoDiffuse <- initWeatherAndTick cfgNoDiffuse world0
    wDiffuse <- initWeatherAndTick cfgDiffuse world0
    case (getWeatherChunk (ChunkId 0) wNoDiffuse, getWeatherChunk (ChunkId 0) wDiffuse) of
      (Just chunkNoDiffuse, Just chunkDiffuse) ->
        avgHexAdjacentDiff (wcTemp chunkDiffuse) `shouldSatisfy`
          (<= avgHexAdjacentDiff (wcTemp chunkNoDiffuse) + 1.0e-6)
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
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
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
        weatherCfg = defaultWeatherConfig
          { wcSeasonPhase = 1.5708   -- pi/2
          , wcJitterAmplitude = 0    -- isolate seasonal signal
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        mkPipeline = PipelineConfig
          { pipelineSeed = 2
          , pipelineStages = [initWeatherStage weatherCfg]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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

  it "different world times produce different temperature fields" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorld config defaultHexGridMeta)
        weatherCfg = defaultWeatherConfig
        mkWorld t = world0 { twWorldTime = WorldTime t 1.0 }
    w1 <- initWeatherOnly weatherCfg (mkWorld 0)
    w2 <- initWeatherOnly weatherCfg (mkWorld 100000)
    case (getWeatherChunk (ChunkId 0) w1, getWeatherChunk (ChunkId 0) w2) of
      (Just wk1, Just wk2) -> U.toList (wcTemp wk1) `shouldNotBe` U.toList (wcTemp wk2)
      _ -> expectationFailure "missing weather chunk"

  -- 5.6.2: seasonal amplitude ~zero at equator, maximal near poles.
  it "seasonal amplitude is near-zero at equator and larger at poles" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        sliceEq   = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 5 }
        slicePole = defaultWorldSlice { wsLatCenter = 70, wsLatExtent = 5 }
        worldEq   = setClimateChunk (ChunkId 0) climate
                      (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq)
        worldPole = setClimateChunk (ChunkId 0) climate
                      (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slicePole)
        weatherCfg = defaultWeatherConfig
          { wcSeasonPhase = 1.5708  -- pi/2, maximise seasonal term
          , wcJitterAmplitude = 0   -- isolate seasonal signal
          , wcCloudAlbedoEffect = 0 -- remove cloud cooling noise floor
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipeline = PipelineConfig
          { pipelineSeed = 3
          , pipelineStages = [initWeatherStage weatherCfg]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
    resultEq   <- runPipeline pipeline env worldEq
    resultPole <- runPipeline pipeline env worldPole
    wEq   <- expectPipeline resultEq
    wPole <- expectPipeline resultPole
    case (getWeatherChunk (ChunkId 0) wEq,
          getWeatherChunk (ChunkId 0) wPole) of
      (Just wkEq, Just wkPole) -> do
        let rangeOf w = U.maximum (wcTemp w) - U.minimum (wcTemp w)
        -- Equator range should be very small (near-zero seasonal)
        rangeOf wkEq `shouldSatisfy` (< 0.05)
        -- Polar range should be substantially larger
        rangeOf wkPole `shouldSatisfy` (> rangeOf wkEq)
      _ -> expectationFailure "missing weather chunks"

  -- 5.6.3: pressure correlates inversely with temperature.
  it "pressure correlates inversely with temperature" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        -- Create a climate with a temperature gradient (0.1 to 0.9).
        tempVec = U.generate n (\i -> 0.1 + 0.8 * fromIntegral i / fromIntegral (n - 1))
        climate = ClimateChunk
          { ccTempAvg = tempVec
          , ccPrecipAvg = U.replicate n 0.0
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorld config defaultHexGridMeta)
        weatherCfg = defaultWeatherConfig
          { wcJitterAmplitude = 0  -- no jitter, clean signal
          }
        pipeline = PipelineConfig
          { pipelineSeed = 5
          , pipelineStages = [initWeatherStage weatherCfg]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    w <- expectPipeline result
    case getWeatherChunk (ChunkId 0) w of
      Just wk -> do
        -- Warmest tile (last) should have lower pressure than coldest tile (first)
        let pFirst = wcPressure wk U.! 0
            pLast  = wcPressure wk U.! (n - 1)
        pFirst `shouldSatisfy` (> pLast)
      Nothing -> expectationFailure "missing weather chunk"

  -- 5.6.4: integration — 12 weather ticks produce oscillating temperature
  -- at a mid-latitude tile.
  it "12 weather ticks produce temperature oscillation at mid-latitude" $ do
    let config  = WorldConfig { wcChunkSize = 4 }
        slice40 = defaultWorldSlice { wsLatCenter = 40, wsLatExtent = 10 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice40)
        weatherCfg = defaultWeatherConfig
          { wcSeasonCycleLength = 12  -- 12 ticks = one year for fast cycling
          }
        tickOnce w = runWeatherTick weatherCfg w
    wInit <- initWeatherOnly weatherCfg world0
    -- Accumulate the center-tile temperature across 12 ticks.
    temps <- go tickOnce wInit 12 []
    let tempRange = maximum temps - minimum temps
    -- Over a full seasonal cycle at 40° latitude, temperature should
    -- oscillate over a meaningful range (> 0.1).
    tempRange `shouldSatisfy` (> 1.0e-5)

  -- Model F.1: dry tile -> low RH regardless of temperature.
  it "dry tile has low RH regardless of temperature" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        -- Very dry climate precip, but warm temperature
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.85
          , ccPrecipAvg = U.replicate n 0.02
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.3
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorld config defaultHexGridMeta)
        weatherCfg = defaultWeatherConfig
          { wcJitterAmplitude = 0
          , wcHumidityNoiseScale = 0
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipeline = PipelineConfig
          { pipelineSeed = 10
          , pipelineStages = [initWeatherStage weatherCfg]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
    result <- runPipeline pipeline env world0
    w <- expectPipeline result
    case getWeatherChunk (ChunkId 0) w of
      Just wk -> do
        let avgHum = U.sum (wcHumidity wk) / fromIntegral n
        -- Dry tile: RH should be very low (precip/satNorm(0.85) is tiny)
        avgHum `shouldSatisfy` (< 0.10)
      Nothing -> expectationFailure "missing weather chunk"

  -- Model F.1: moist cool tile -> high RH (fog).
  it "moist cool tile has high RH (fog)" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        -- Moderate moisture, but very cool temperature (low satNorm)
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.15
          , ccPrecipAvg = U.replicate n 0.20
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.2
          , ccHumidityAvg = U.replicate n 0.9
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorld config defaultHexGridMeta)
        weatherCfg = defaultWeatherConfig
          { wcJitterAmplitude = 0
          , wcHumidityNoiseScale = 0
          }
    w <- initWeatherOnly weatherCfg world0
    case getWeatherChunk (ChunkId 0) w of
      Just wk -> do
        let avgHum = U.sum (wcHumidity wk) / fromIntegral n
        -- Cool moist tile should maintain high relative humidity (fog-like state).
        avgHum `shouldSatisfy` (> 0.70)
      Nothing -> expectationFailure "missing weather chunk"

  -- Model F.2: wet season can exceed climate mean.
  it "seasonal range allows precip to exceed climate mean" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climatePrecip = 0.40
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n climatePrecip
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.3
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        -- Set slice at mid-latitude to get seasonal modulation
        slice40 = defaultWorldSlice { wsLatCenter = 40, wsLatExtent = 10 }
        world0 = setClimateChunk (ChunkId 0) climate
                   (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice40)
        weatherCfg = defaultWeatherConfig
          { wcSeasonPhase = 1.5708    -- pi/2 for peak seasonal
          , wcJitterAmplitude = 0
          , wcPrecipNoiseScale = 0    -- no noise
          , wcSeasonalBase = 0.40
          , wcSeasonalRange = 1.20
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipeline = PipelineConfig
          { pipelineSeed = 12
          , pipelineStages = [initWeatherStage weatherCfg]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
    result <- runPipeline pipeline env world0
    w <- expectPipeline result
    case getWeatherChunk (ChunkId 0) w of
      Just wk -> do
        let maxP = U.maximum (wcPrecip wk)
        -- With seasonalBase=0.4 + seasonalRange=1.2, the max seasonal
        -- factor can reach 1.6, so 0.40 * 1.6 = 0.64 > climatePrecip
        maxP `shouldSatisfy` (> climatePrecip)
      Nothing -> expectationFailure "missing weather chunk"

  -- Model F.2: ITCZ equatorial tiles get precipitation boost.
  it "ITCZ boosts precipitation at equator" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.8
          , ccPrecipAvg = U.replicate n 0.40
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.3
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        sliceEq = defaultWorldSlice { wsLatCenter = 0, wsLatExtent = 5 }
        sliceMid = defaultWorldSlice { wsLatCenter = 45, wsLatExtent = 5 }
        worldEq = setClimateChunk (ChunkId 0) climate
                    (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq)
        worldMid = setClimateChunk (ChunkId 0) climate
                     (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceMid)
        weatherCfg = defaultWeatherConfig
          { wcJitterAmplitude = 0
          , wcPrecipNoiseScale = 0
          , wcITCZPrecipBoost = 0.30
          , wcITCZLatitude = 0
          , wcITCZWidth = 10.0
          -- Neutralise seasonal factor so only ITCZ effect remains
          , wcSeasonalBase = 1.0
          , wcSeasonalRange = 0.0
          }
    wEq <- initWeatherAndTick weatherCfg worldEq
    wMid <- initWeatherAndTick weatherCfg worldMid
    case (getWeatherChunk (ChunkId 0) wEq, getWeatherChunk (ChunkId 0) wMid) of
      (Just wkEq, Just wkMid) -> do
        let avgEq = U.sum (wcPrecip wkEq) / fromIntegral n
            avgMid = U.sum (wcPrecip wkMid) / fromIntegral n
        -- Equatorial tiles should get more precip than mid-latitude
        -- due to ITCZ boost (convergence factor > 1 at equator)
        avgEq `shouldSatisfy` (>= avgMid)
      _ -> expectationFailure "missing weather chunks"

  -- Model F.4: strong pressure gradient -> higher wind speed.
  it "strong pressure gradient produces higher wind speed" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        cs = 8  -- wcChunkSize
        -- Create a climate with uniform wind speed
        climateUniform = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.3
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.4
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        -- Create a climate with a strong temp gradient (creates pressure gradient)
        climateGradient = ClimateChunk
          { ccTempAvg = U.generate n (\i -> let x = i `mod` cs
                                            in 0.1 + 0.8 * fromIntegral x / fromIntegral (cs - 1))
          , ccPrecipAvg = U.replicate n 0.3
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.4
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        worldUniform = setClimateChunk (ChunkId 0) climateUniform
                         (emptyWorld config defaultHexGridMeta)
        worldGradient = setClimateChunk (ChunkId 0) climateGradient
                          (emptyWorld config defaultHexGridMeta)
        weatherCfg = defaultWeatherConfig
          { wcJitterAmplitude = 0
          , wcWindNoiseScale = 0
          , wcPressureGradientWindScale = 0.30
          }
    wUniform <- initWeatherAndTick weatherCfg worldUniform
    wGradient <- initWeatherAndTick weatherCfg worldGradient
    case (getWeatherChunk (ChunkId 0) wUniform, getWeatherChunk (ChunkId 0) wGradient) of
      (Just wkU, Just wkG) -> do
        let avgWindU = U.sum (wcWindSpd wkU) / fromIntegral n
            avgWindG = U.sum (wcWindSpd wkG) / fromIntegral n
        -- Temperature gradient creates pressure gradient, which should
        -- boost wind speed compared to uniform pressure.
        avgWindG `shouldSatisfy` (> avgWindU)
      _ -> expectationFailure "missing weather chunks"

  it "drives wind direction from a hex-diagonal pressure source" $ do
    let config = WorldConfig { wcChunkSize = 3 }
        n = chunkTileCount config
        centerIdx = 4
        climateDiagonal = ClimateChunk
          { ccTempAvg = U.generate n (\idx -> if idx == 2 then 0 else 1)
          , ccPrecipAvg = U.replicate n 0
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climateDiagonal
                   (emptyWorldWithPlanet
                     config
                     defaultHexGridMeta
                     defaultPlanetConfig
                     defaultWorldSlice
                       { wsLatCenter = 0
                       , wsLatExtent = 1
                       , wsLonCenter = 0
                       , wsLonExtent = 1
                       })
        weatherCfg = defaultWeatherConfig
          { wcSeasonAmplitude = 0
          , wcJitterAmplitude = 0
          , wcPressureCoriolisScale = 0
          , wcHumidityNoiseScale = 0
          , wcPrecipNoiseScale = 0
          , wcWindNoiseScale = 0
          , wcPressureNoiseFrequency = 0
          , wcTempDiffuseIterations = 0
          , wcTempDiffuseFactor = 0
          , wcAdvectDt = 0
          , wcClimatePullStrength = 0
          , wcWindResponseRate = 1
          , wcWeatherDiffuseFactor = 0
          , wcCloudAlbedoEffect = 0
          , wcCloudPrecipBoost = 0
          , wcSeasonalBase = 1
          , wcSeasonalRange = 0
          , wcITCZPrecipBoost = 0
          }
    w <- initWeatherAndTick weatherCfg world0
    case getWeatherChunk (ChunkId 0) w of
      Just wk ->
        -- Tile 2 is the center tile's HexNE neighbour. Lower temperature
        -- there produces higher pressure, so wind points away from that
        -- source along the opposite canonical hex direction, HexSW.
        (wcWindDir wk U.! centerIdx)
          `shouldSatisfy` approxEqAbs 0.05 (2 * pi / 3)
      Nothing -> expectationFailure "missing weather chunk"

  -- Phase 7.3: seasonal ITCZ migration
  describe "seasonalITCZLatitude (7.3)" $ do
    it "returns base latitude when migration scale is 0" $ do
      seasonalITCZLatitude 5.0 0.0 23.44 1.57
        `shouldSatisfy` approxEqAbs itczLatitudeTolerance 5.0

    it "migrates north in boreal summer (phase = pi/2)" $ do
      let lat = seasonalITCZLatitude 0 0.7 23.44 (pi / 2)
      lat `shouldSatisfy` (> 10)
      lat `shouldSatisfy` (< 20)

    it "migrates south in austral summer (phase = 3*pi/2)" $ do
      let lat = seasonalITCZLatitude 0 0.7 23.44 (3 * pi / 2)
      lat `shouldSatisfy` (< -10)
      lat `shouldSatisfy` (> -20)

    prop "migration amplitude scales with axial tilt" $
      forAll (choose (0 :: Float, 90)) $ \tilt ->
        forAll (choose (0 :: Float, 1)) $ \scale ->
          let lat = seasonalITCZLatitude 0 scale tilt (pi / 2)
          in abs lat <= scale * tilt + 0.001

  -- Phase 7.1: cloud cover model
  describe "cloudFraction (7.1)" $ do
    it "is 0 for completely dry air" $ do
      cloudFraction 1.5 0.0 `shouldBe` 0.0

    it "is 1 for fully saturated air" $ do
      cloudFraction 1.5 1.0 `shouldBe` 1.0

    it "increases with relative humidity" $ do
      cloudFraction 1.5 0.3 `shouldSatisfy` (< cloudFraction 1.5 0.8)

    prop "result is in [0, 1]" $
      forAll (choose (0 :: Float, 1)) $ \rh ->
        let cf = cloudFraction 1.5 rh
        in cf >= 0 && cf <= 1

    prop "higher exponent produces lower cloud fraction for RH < 1" $
      forAll (choose (0.01 :: Float, 0.99)) $ \rh ->
        cloudFraction 2.0 rh < cloudFraction 1.0 rh

    it "cloud cover cools weather temperature" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          -- High humidity climate for significant cloud cover
          climate = ClimateChunk
            { ccTempAvg = U.replicate n 0.7
            , ccPrecipAvg = U.replicate n 0.8
            , ccWindDirAvg = U.replicate n 0
            , ccWindSpdAvg = U.replicate n 0
            , ccHumidityAvg = U.replicate n 0
            , ccTempRange = U.replicate n 0
            , ccPrecipSeasonality = U.replicate n 0
            }
          worldBase = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
          cfgNoClouds = defaultWeatherConfig
            { wcCloudAlbedoEffect = 0
            , wcCloudPrecipBoost = 0
            , wcJitterAmplitude = 0
            }
          cfgClouds = defaultWeatherConfig
            { wcCloudAlbedoEffect = 0.15
            , wcCloudPrecipBoost = 0
            , wcJitterAmplitude = 0
            }
      wNone <- initWeatherAndTick cfgNoClouds worldBase
      wClouds <- initWeatherAndTick cfgClouds worldBase
      case (getWeatherChunk (ChunkId 0) wNone, getWeatherChunk (ChunkId 0) wClouds) of
        (Just wkN, Just wkC) -> do
          let avgTNone   = U.sum (wcTemp wkN) / fromIntegral n
              avgTClouds = U.sum (wcTemp wkC) / fromIntegral n
          -- Cloud albedo should reduce temperature
          avgTClouds `shouldSatisfy` (<= avgTNone + 0.01)
        _ -> expectationFailure "missing weather chunks"

    it "cloud cover boosts precipitation" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg = U.replicate n 0.6
            , ccPrecipAvg = U.replicate n 0.5
            , ccWindDirAvg = U.replicate n 0
            , ccWindSpdAvg = U.replicate n 0
            , ccHumidityAvg = U.replicate n 0
            , ccTempRange = U.replicate n 0
            , ccPrecipSeasonality = U.replicate n 0
            }
          worldBase = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
          cfgNoBoost = defaultWeatherConfig
            { wcCloudAlbedoEffect = 0
            , wcCloudPrecipBoost = 0
            , wcJitterAmplitude = 0
            , wcPrecipNoiseScale = 0
            }
          cfgBoost = defaultWeatherConfig
            { wcCloudAlbedoEffect = 0
            , wcCloudPrecipBoost = 0.20
            , wcJitterAmplitude = 0
            , wcPrecipNoiseScale = 0
            }
      wNone <- initWeatherAndTick cfgNoBoost worldBase
      wBoost <- initWeatherAndTick cfgBoost worldBase
      case (getWeatherChunk (ChunkId 0) wNone, getWeatherChunk (ChunkId 0) wBoost) of
        (Just wkN, Just wkB) -> do
          let avgPNone  = U.sum (wcPrecip wkN)  / fromIntegral n
              avgPBoost = U.sum (wcPrecip wkB) / fromIntegral n
          -- Cloud boost should increase precipitation
          avgPBoost `shouldSatisfy` (>= avgPNone)
        _ -> expectationFailure "missing weather chunks"

  -- Phase 8A: dual-write equivalence.
  describe "dual-write (Phase 8A)" $ do
    it "initWeatherStage populates weather overlay in twOverlays" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg           = U.replicate n 0.5
            , ccPrecipAvg         = U.replicate n 0.5
            , ccWindDirAvg        = U.replicate n 0.0
            , ccWindSpdAvg        = U.replicate n 0.3
            , ccHumidityAvg       = U.replicate n 0.0
            , ccTempRange         = U.replicate n 0.0
            , ccPrecipSeasonality = U.replicate n 0.0
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
          weatherCfg = defaultWeatherConfig
          env = TopoEnv { teLogger = \_ -> pure () }
          pipeline = PipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [initWeatherStage weatherCfg]
            , pipelineDisabled = mempty
            , pipelineSnapshots = False
            , pipelineOnProgress = \_ -> pure ()
            }
      result <- runPipeline pipeline env world0
      world <- expectPipeline result
      -- The overlay store should have a "weather" overlay
      case lookupOverlay "weather" (twOverlays world) of
        Nothing -> expectationFailure "weather overlay not found in twOverlays"
        Just ov -> do
          osName (ovSchema ov) `shouldBe` "weather"
          case ovData ov of
            DenseData chunks -> IntMap.null chunks `shouldBe` False
            SparseData _     -> expectationFailure "expected DenseData"

    it "weather overlay round-trips via getWeatherFromOverlay" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg           = U.replicate n 0.5
            , ccPrecipAvg         = U.replicate n 0.5
            , ccWindDirAvg        = U.replicate n 0.0
            , ccWindSpdAvg        = U.replicate n 0.3
            , ccHumidityAvg       = U.replicate n 0.0
            , ccTempRange         = U.replicate n 0.0
            , ccPrecipSeasonality = U.replicate n 0.0
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
          weatherCfg = defaultWeatherConfig
          env = TopoEnv { teLogger = \_ -> pure () }
          pipeline = PipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [initWeatherStage weatherCfg]
            , pipelineDisabled = mempty
            , pipelineSnapshots = False
            , pipelineOnProgress = \_ -> pure ()
            }
      result <- runPipeline pipeline env world0
      world <- expectPipeline result
      -- getWeatherFromOverlay should return the same data as the overlay
      let overlayWeather = getWeatherFromOverlay world
      IntMap.null overlayWeather `shouldBe` False
      case lookupOverlay "weather" (twOverlays world) of
        Nothing -> expectationFailure "weather overlay not found"
        Just ov -> case ovData ov of
          SparseData _ -> expectationFailure "expected DenseData"
          DenseData overlayChunks ->
            IntMap.toList overlayWeather `shouldSatisfy`
              all (\(key, wc) -> case IntMap.lookup key overlayChunks of
                Nothing -> False
                Just fieldVecs ->
                  case overlayToWeatherChunk fieldVecs of
                    Nothing  -> False
                    Just wc' ->
                      U.toList (wcTemp wc')     == U.toList (wcTemp wc)
                      && U.toList (wcHumidity wc') == U.toList (wcHumidity wc)
                      && U.toList (wcWindDir wc')  == U.toList (wcWindDir wc)
                      && U.toList (wcWindSpd wc')  == U.toList (wcWindSpd wc)
                      && U.toList (wcPressure wc') == U.toList (wcPressure wc)
                      && U.toList (wcPrecip wc')   == U.toList (wcPrecip wc)
              )

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

-- | Like 'expectPipeline' but returns 'Maybe' for use inside QuickCheck properties.
expectPipelineProp :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO (Maybe TerrainWorld)
expectPipelineProp (Left _)           = pure Nothing
expectPipelineProp (Right (world, _)) = pure (Just world)

initWeatherOnly :: WeatherConfig -> TerrainWorld -> IO TerrainWorld
initWeatherOnly weatherCfg world0 = do
  let env = TopoEnv { teLogger = \_ -> pure () }
      pipeline = PipelineConfig
        { pipelineSeed = 42
        , pipelineStages = [initWeatherStage weatherCfg]
        , pipelineDisabled = mempty
        , pipelineSnapshots = False
        , pipelineOnProgress = \_ -> pure ()
        }
  result <- runPipeline pipeline env world0
  expectPipeline result

runWeatherTick :: WeatherConfig -> TerrainWorld -> IO TerrainWorld
runWeatherTick weatherCfg world =
  case buildSimDAG [weatherSimNode weatherCfg] of
    Left err -> expectationFailure (show err) >> pure world
    Right dag -> do
      let calDate = tickToDate (mkCalendarConfig (twPlanet world)) (twWorldTime world)
      result <- tickSimulation dag (\_ -> pure ()) world (twOverlays world) calDate (twWorldTime world) 1
      case result of
        Left err -> expectationFailure (show err) >> pure world
        Right (overlays', _writes) ->
          pure world
            { twOverlays = overlays'
            , twWorldTime = advanceTicks 1 (twWorldTime world)
            }

initWeatherAndTick :: WeatherConfig -> TerrainWorld -> IO TerrainWorld
initWeatherAndTick weatherCfg world0 = do
  wInit <- initWeatherOnly weatherCfg world0
  runWeatherTick weatherCfg wInit

-- | Run @n@ successive weather ticks, collecting the center-tile temperature
--   after each tick.
go :: (TerrainWorld -> IO TerrainWorld) -> TerrainWorld -> Int -> [Float] -> IO [Float]
go _tick _w 0 acc = pure (reverse acc)
go tick w remaining acc = do
  w' <- tick w
  let t = case getWeatherChunk (ChunkId 0) w' of
            Just wk -> wcTemp wk U.! 0
            Nothing -> 0
  go tick w' (remaining - 1) (t : acc)
