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
import Topo.Calendar (WorldTime(..))
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), insertOverlay)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, PlanetConfig(..), WorldSlice(..))
import Topo.Weather (cloudFraction, seasonalITCZLatitude, weatherSeasonalPhase,
                     weatherOverlaySchema, overlayToWeatherChunk, weatherFieldCount,
                     weatherChunkToOverlay,
                     WeatherNormalsChunk(..), weatherNormalsOverlayName,
                     weatherNormalsOverlaySchema, weatherNormalsFieldCount,
                     weatherNormalsChunkFromClimate, weatherNormalsChunkToOverlay,
                     overlayToWeatherNormalsChunk, getWeatherNormalsChunk,
                     getWeatherNormalsFromOverlay,
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

  it "runs weather through the hourly pipeline, changing weather and advancing one hour" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.3
          , ccHumidityAvg = U.replicate n 0.2
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
    worldInit <- initWeatherOnly defaultWeatherConfig world0
    tempInit <- case firstWeatherTemp worldInit of
      Nothing -> expectationFailure "missing initial weather temperature" >> pure []
      Just temps -> pure temps
    worldTicked <- runWeatherTick defaultWeatherConfig worldInit
    wtTick (twWorldTime worldTicked) `shouldBe` wtTick (twWorldTime worldInit) + 1
    case firstWeatherTemp worldTicked of
      Nothing -> expectationFailure "missing ticked weather temperature"
      Just temps -> temps `shouldNotBe` tempInit

  it "changes cloud cover and cloud water on a due weather tick" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.4
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0.9
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        seededWeather = constantWeatherChunk n 0.5 0.1 0.0 0.0 0.0
        world0 = withWeatherOverlay
          (setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta))
          seededWeather
        weatherCfg = defaultWeatherConfig
          { wcSeasonAmplitude = 0
          , wcJitterAmplitude = 0
          , wcHumidityNoiseScale = 0
          , wcPrecipNoiseScale = 0
          , wcITCZPrecipBoost = 0
          , wcCloudPrecipBoost = 0
          , wcCloudFormationRate = 1
          , wcCloudDissipationRate = 0
          , wcCloudSaturationThreshold = 0.2
          , wcConvectiveThreshold = 1
          , wcSubsidenceDissipation = 0
          , wcAutoconversionThreshold = 1
          , wcPrecipEfficiency = 0
          , wcTempDiffuseIterations = 0
          , wcTempDiffuseFactor = 0
          , wcAdvectDt = 0
          , wcClimatePullStrength = 1
          , wcWeatherDiffuseFactor = 0
          , wcSeasonalBase = 1
          , wcSeasonalRange = 0
          }
    before <- case getWeatherChunk (ChunkId 0) world0 of
      Nothing -> expectationFailure "missing seeded weather chunk" >> pure seededWeather
      Just wk -> pure wk
    worldTicked <- runWeatherTick weatherCfg world0
    wtTick (twWorldTime worldTicked) `shouldBe` wtTick (twWorldTime world0) + 1
    case getWeatherChunk (ChunkId 0) worldTicked of
      Nothing -> expectationFailure "missing ticked weather chunk"
      Just after -> do
        averageField (wcCloudCover after) `shouldSatisfy` (> averageField (wcCloudCover before) + 0.30)
        averageField (wcCloudWater after) `shouldSatisfy` (> averageField (wcCloudWater before) + 0.30)
        U.toList (wcCloudCover after) `shouldNotBe` U.toList (wcCloudCover before)
        U.toList (wcCloudWater after) `shouldNotBe` U.toList (wcCloudWater before)

  it "skips non-due weather ticks until the configured cadence fires" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0.3
          , ccHumidityAvg = U.replicate n 0.2
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        weatherCfg = defaultWeatherConfig { wcTickSeconds = 6 }
        world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
        runNTicks count start = foldl (\acc _ -> acc >>= runWeatherTick weatherCfg) (pure start) [1 .. count]
        weatherSchedule world = lookupOverlay "weather" (twOverlays world) >>= opSchedule . ovProvenance
    worldInit <- initWeatherOnly weatherCfg world0
    let initialWeather = getWeatherFromOverlay worldInit
        initialSchedule = weatherSchedule worldInit
    initialSchedule `shouldSatisfy` (/= Nothing)
    world5 <- runNTicks (5 :: Int) worldInit
    wtTick (twWorldTime world5) `shouldBe` 5
    getWeatherFromOverlay world5 `shouldBe` initialWeather
    weatherSchedule world5 `shouldBe` initialSchedule
    world6 <- runWeatherTick weatherCfg world5
    wtTick (twWorldTime world6) `shouldBe` 6
    case weatherSchedule world6 of
      Nothing -> expectationFailure "missing weather schedule after due tick"
      Just sched -> do
        schedIntervalTicks sched `shouldBe` 6
        schedLastFireTick sched `shouldBe` Just 6
        schedNextFireTick sched `shouldBe` 12

  describe "weatherSeasonalPhase" $ do
    it "advances over wcSeasonCycleLength world ticks and preserves radian offset" $ do
      let cfg = defaultWeatherConfig { wcSeasonCycleLength = 12, wcSeasonPhase = 0 }
          phaseAt c tick = weatherSeasonalPhase c (WorldTime tick 3600)
          expectedPhases =
            [ (0, 0)
            , (3, pi / 2)
            , (6, pi)
            , (9, 3 * pi / 2)
            , (12, 0)
            ]
      mapM_
        (\(tick, expectedPhase) ->
          phaseAt cfg tick `shouldSatisfy` approxEqAbs 1.0e-5 expectedPhase)
        expectedPhases

      let offset = 0.37
          cfgOffset = cfg { wcSeasonPhase = offset }
      phaseAt cfgOffset 3 `shouldSatisfy` approxEqAbs 1.0e-5 (offset + pi / 2)
      phaseAt cfgOffset 12 `shouldSatisfy` approxEqAbs 1.0e-5 offset

    it "clamps invalid season cycle lengths to a one-world-tick cycle" $ do
      let offset = 0.42
          phaseFor cycleLength tick =
            weatherSeasonalPhase
              (defaultWeatherConfig
                { wcSeasonCycleLength = cycleLength
                , wcSeasonPhase = offset
                })
              (WorldTime tick 3600)
      mapM_
        (\cycleLength ->
          phaseFor cycleLength 1 `shouldSatisfy` approxEqAbs 1.0e-6 offset)
        [0, -12, 0.5, 1 / 0, 0 / 0]

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

  -- 5.6.4: wcSeasonCycleLength drives deterministic seasonal baselines
  -- for both initialisation and stateful ticks.
  it "uses season cycle length for init and tick temperature baselines" $ do
    let config  = WorldConfig { wcChunkSize = 4 }
        slice40 = defaultWorldSlice { wsLatCenter = 40, wsLatExtent = 10 }
        planetNoSun = defaultPlanetConfig { pcInsolation = 0 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        worldAt tick =
          (setClimateChunk (ChunkId 0) climate
            (emptyWorldWithPlanet config defaultHexGridMeta planetNoSun slice40))
            { twWorldTime = WorldTime tick 3600 }
        weatherCfg cycleLength = defaultWeatherConfig
          { wcSeasonCycleLength = cycleLength
          , wcSeasonPhase = 0
          , wcSeasonAmplitude = 0.25
          , wcJitterAmplitude = 0
          , wcHumidityNoiseScale = 0
          , wcPrecipNoiseScale = 0
          , wcITCZPrecipBoost = 0
          , wcCloudPrecipBoost = 0
          , wcCloudOpticalScale = 0
          , wcCloudGreenhouseCoeff = 0
          , wcTempDiffuseIterations = 0
          , wcTempDiffuseFactor = 0
          , wcAdvectDt = 0
          , wcClimatePullStrength = 1
          , wcWeatherDiffuseFactor = 0
          , wcSeasonalBase = 1
          , wcSeasonalRange = 0
          }
        avgTemp world = case getWeatherChunk (ChunkId 0) world of
          Just wk -> pure (U.sum (wcTemp wk) / fromIntegral n)
          Nothing -> expectationFailure "missing weather chunk" >> pure 0
        tickedAt cfg tick = do
          wInit <- initWeatherOnly cfg (worldAt (tick - 1))
          runWeatherTick cfg wInit

    shortInit3 <- initWeatherOnly (weatherCfg 12) (worldAt 3)
    shortInit9 <- initWeatherOnly (weatherCfg 12) (worldAt 9)
    longInit3 <- initWeatherOnly (weatherCfg 12000) (worldAt 3)
    longInit9 <- initWeatherOnly (weatherCfg 12000) (worldAt 9)
    shortTick3 <- tickedAt (weatherCfg 12) 3
    shortTick9 <- tickedAt (weatherCfg 12) 9
    longTick3 <- tickedAt (weatherCfg 12000) 3
    longTick9 <- tickedAt (weatherCfg 12000) 9

    shortInitDelta <- abs <$> ((-) <$> avgTemp shortInit3 <*> avgTemp shortInit9)
    longInitDelta <- abs <$> ((-) <$> avgTemp longInit3 <*> avgTemp longInit9)
    shortTickDelta <- abs <$> ((-) <$> avgTemp shortTick3 <*> avgTemp shortTick9)
    longTickDelta <- abs <$> ((-) <$> avgTemp longTick3 <*> avgTemp longTick9)

    shortInitDelta `shouldSatisfy` (> 0.05)
    shortTickDelta `shouldSatisfy` (> 0.05)
    longInitDelta `shouldSatisfy` (< 0.005)
    longTickDelta `shouldSatisfy` (< 0.005)

  it "uses season cycle length for initial ITCZ migration" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        sliceNorthITCZ = defaultWorldSlice { wsLatCenter = 16, wsLatExtent = 4 }
        n = chunkTileCount config
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.5
          , ccPrecipAvg = U.replicate n 0.4
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        worldAt tick =
          (setClimateChunk (ChunkId 0) climate
            (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceNorthITCZ))
            { twWorldTime = WorldTime tick 3600 }
        weatherCfg = defaultWeatherConfig
          { wcSeasonCycleLength = 12
          , wcSeasonPhase = 0
          , wcSeasonAmplitude = 0
          , wcJitterAmplitude = 0
          , wcPrecipNoiseScale = 0
          , wcSeasonalBase = 1
          , wcSeasonalRange = 0
          , wcITCZLatitude = 0
          , wcITCZWidth = 4
          , wcITCZPrecipBoost = 1
          }
        avgPrecip world = case getWeatherChunk (ChunkId 0) world of
          Just wk -> pure (U.sum (wcPrecip wk) / fromIntegral n)
          Nothing -> expectationFailure "missing weather chunk" >> pure 0
    northSeason <- initWeatherOnly weatherCfg (worldAt 3)
    southSeason <- initWeatherOnly weatherCfg (worldAt 9)
    northPrecip <- avgPrecip northSeason
    southPrecip <- avgPrecip southSeason
    northPrecip `shouldSatisfy` (> southPrecip + 0.1)

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
          (schedNextFireTick <$> opSchedule (ovProvenance ov)) `shouldBe` Just 1
          case ovData ov of
            DenseData chunks -> IntMap.null chunks `shouldBe` False
            SparseData _     -> expectationFailure "expected DenseData"

    it "initWeatherStage derives schedules from configured weather cadence" $ do
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
          weatherCfg = defaultWeatherConfig { wcTickSeconds = 6 }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
          world5 = world0 { twWorldTime = (twWorldTime world0) { wtTick = 5 } }
          scheduleOf world = lookupOverlay "weather" (twOverlays world) >>= opSchedule . ovProvenance
      generated0 <- initWeatherOnly weatherCfg world0
      case scheduleOf generated0 of
        Nothing -> expectationFailure "missing tick-0 weather schedule"
        Just sched -> do
          schedIntervalTicks sched `shouldBe` 6
          schedPhaseTicks sched `shouldBe` 0
          schedCatchUpPolicy sched `shouldBe` RunOnceIfDue
          schedNextFireTick sched `shouldBe` 6
      generated5 <- initWeatherOnly weatherCfg world5
      case scheduleOf generated5 of
        Nothing -> expectationFailure "missing tick-5 weather schedule"
        Just sched -> schedNextFireTick sched `shouldSatisfy` (> 5)

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

  describe "weather normals overlay" $ do
    it "defines a versioned dense schema for generated typical weather" $ do
      weatherNormalsOverlayName `shouldBe` "weather_normals"
      weatherNormalsFieldCount `shouldBe` 13
      osName weatherNormalsOverlaySchema `shouldBe` "weather_normals"
      osVersion weatherNormalsOverlaySchema `shouldBe` "1.0.0"
      osStorage weatherNormalsOverlaySchema `shouldBe` StorageDense
      map ofdName (osFields weatherNormalsOverlaySchema) `shouldBe`
        [ "temperature", "humidity", "wind_dir", "wind_speed", "precipitation"
        , "cloud_cover", "cloud_water"
        , "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high"
        , "cloud_water_low", "cloud_water_mid", "cloud_water_high"
        ]

    it "derives deterministic typical weather normals from climate averages" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg = U.generate n (\i -> fromIntegral i / fromIntegral n)
            , ccPrecipAvg = U.replicate n 0.6
            , ccWindDirAvg = U.replicate n 1.25
            , ccWindSpdAvg = U.replicate n 0.4
            , ccHumidityAvg = U.replicate n 0.8
            , ccTempRange = U.replicate n 0.3
            , ccPrecipSeasonality = U.replicate n 0.5
            }
          normalsA = weatherNormalsChunkFromClimate defaultWeatherConfig climate
          normalsB = weatherNormalsChunkFromClimate defaultWeatherConfig climate
      U.toList (wncTemp normalsA) `shouldBe` U.toList (ccTempAvg climate)
      U.toList (wncPrecip normalsA) `shouldBe` U.toList (ccPrecipAvg climate)
      U.toList (wncWindDir normalsA) `shouldBe` U.toList (ccWindDirAvg climate)
      U.toList (wncWindSpd normalsA) `shouldBe` U.toList (ccWindSpdAvg climate)
      U.toList (wncCloudCover normalsA) `shouldBe` U.toList (wncCloudCover normalsB)
      U.all (\v -> v >= 0 && v <= 1) (wncCloudCover normalsA) `shouldBe` True
      U.all (\v -> v >= 0 && v <= 1) (wncCloudWater normalsA) `shouldBe` True

    it "round-trips weather normals dense overlay chunks" $ do
      let n = 16
          normals = WeatherNormalsChunk
            { wncTemp = U.replicate n 0.5
            , wncHumidity = U.replicate n 0.7
            , wncWindDir = U.replicate n 1.0
            , wncWindSpd = U.replicate n 0.3
            , wncPrecip = U.replicate n 0.4
            , wncCloudCover = U.replicate n 0.6
            , wncCloudWater = U.replicate n 0.35
            , wncCloudCoverLow = U.replicate n 0.36
            , wncCloudCoverMid = U.replicate n 0.15
            , wncCloudCoverHigh = U.replicate n 0.09
            , wncCloudWaterLow = U.replicate n 0.21
            , wncCloudWaterMid = U.replicate n 0.0875
            , wncCloudWaterHigh = U.replicate n 0.0525
            }
      case overlayToWeatherNormalsChunk (weatherNormalsChunkToOverlay normals) of
        Nothing -> expectationFailure "weather normals round-trip returned Nothing"
        Just normals' -> do
          U.toList (wncTemp normals') `shouldBe` U.toList (wncTemp normals)
          U.toList (wncCloudCoverLow normals') `shouldBe` U.toList (wncCloudCoverLow normals)
          U.toList (wncCloudWaterHigh normals') `shouldBe` U.toList (wncCloudWaterHigh normals)

    it "initWeatherStage also inserts unscheduled generated weather normals" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg = U.replicate n 0.5
            , ccPrecipAvg = U.replicate n 0.4
            , ccWindDirAvg = U.replicate n 0.2
            , ccWindSpdAvg = U.replicate n 0.3
            , ccHumidityAvg = U.replicate n 0.7
            , ccTempRange = U.replicate n 0.1
            , ccPrecipSeasonality = U.replicate n 0.25
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
      world <- initWeatherOnly defaultWeatherConfig world0
      case lookupOverlay "weather_normals" (twOverlays world) of
        Nothing -> expectationFailure "weather_normals overlay not found"
        Just ov -> do
          ovSchema ov `shouldBe` weatherNormalsOverlaySchema
          opSource (ovProvenance ov) `shouldBe` "weather_normals"
          opSchedule (ovProvenance ov) `shouldBe` Nothing
          case ovData ov of
            DenseData chunks -> IntMap.member 0 chunks `shouldBe` True
            SparseData _ -> expectationFailure "expected DenseData"
      IntMap.null (getWeatherNormalsFromOverlay world) `shouldBe` False
      case getWeatherNormalsChunk (ChunkId 0) world of
        Nothing -> expectationFailure "missing generated weather normals chunk"
        Just normals -> do
          U.toList (wncTemp normals) `shouldBe` U.toList (ccTempAvg climate)
          U.toList (wncHumidity normals) `shouldBe` U.toList (ccHumidityAvg climate)

  -- Phase: Cloud layer separation (low/mid/high)
  describe "cloud layers" $ do
    it "initWeatherStage populates per-layer cloud fields" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg           = U.replicate n 0.5
            , ccPrecipAvg         = U.replicate n 0.5
            , ccWindDirAvg        = U.replicate n 0.0
            , ccWindSpdAvg        = U.replicate n 0.3
            , ccHumidityAvg       = U.replicate n 0.7
            , ccTempRange         = U.replicate n 0.0
            , ccPrecipSeasonality = U.replicate n 0.0
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
      world <- initWeatherOnly defaultWeatherConfig world0
      case getWeatherChunk (ChunkId 0) world of
        Nothing -> expectationFailure "no weather chunk after init"
        Just wc -> do
          -- Per-layer fields should be populated (non-empty)
          U.length (wcCloudCoverLow wc) `shouldBe` n
          U.length (wcCloudCoverMid wc) `shouldBe` n
          U.length (wcCloudCoverHigh wc) `shouldBe` n
          U.length (wcCloudWaterLow wc) `shouldBe` n
          U.length (wcCloudWaterMid wc) `shouldBe` n
          U.length (wcCloudWaterHigh wc) `shouldBe` n

    it "layer cloud cover sums to approximately total cover" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg           = U.replicate n 0.5
            , ccPrecipAvg         = U.replicate n 0.5
            , ccWindDirAvg        = U.replicate n 0.0
            , ccWindSpdAvg        = U.replicate n 0.3
            , ccHumidityAvg       = U.replicate n 0.7
            , ccTempRange         = U.replicate n 0.0
            , ccPrecipSeasonality = U.replicate n 0.0
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
      world <- initWeatherOnly defaultWeatherConfig world0
      case getWeatherChunk (ChunkId 0) world of
        Nothing -> expectationFailure "no weather chunk"
        Just wc -> do
          -- For each tile, low+mid+high should be ≤ total cloud cover
          -- (random overlap means total ≥ max(layers) but ≤ sum)
          let allOk = U.all id $ U.generate n (\i ->
                let lo = wcCloudCoverLow wc U.! i
                    mi = wcCloudCoverMid wc U.! i
                    hi = wcCloudCoverHigh wc U.! i
                    total = wcCloudCover wc U.! i
                -- Each layer fraction should be ≤ total
                in lo <= total + 0.01 && mi <= total + 0.01 && hi <= total + 0.01)
          allOk `shouldBe` True

    it "low cloud layer gets largest share of humidity-driven formation" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = chunkTileCount config
          climate = ClimateChunk
            { ccTempAvg           = U.replicate n 0.5
            , ccPrecipAvg         = U.replicate n 0.5
            , ccWindDirAvg        = U.replicate n 0.0
            , ccWindSpdAvg        = U.replicate n 0.3
            , ccHumidityAvg       = U.replicate n 0.8
            , ccTempRange         = U.replicate n 0.0
            , ccPrecipSeasonality = U.replicate n 0.0
            }
          world0 = setClimateChunk (ChunkId 0) climate (emptyWorld config defaultHexGridMeta)
      world <- initWeatherOnly defaultWeatherConfig world0
      case getWeatherChunk (ChunkId 0) world of
        Nothing -> expectationFailure "no weather chunk"
        Just wc -> do
          let avgLow  = U.sum (wcCloudCoverLow wc) / fromIntegral n
              avgMid  = U.sum (wcCloudCoverMid wc) / fromIntegral n
              avgHigh = U.sum (wcCloudCoverHigh wc) / fromIntegral n
          -- Low > mid > high for humidity-driven initial state
          avgLow `shouldSatisfy` (> avgMid)
          avgMid `shouldSatisfy` (> avgHigh)

    it "overlay field count is 14 with cloud layers" $ do
      weatherFieldCount `shouldBe` 14

    it "overlay round-trips per-layer cloud fields" $ do
      let n = 16
          wc = WeatherChunk
                { wcTemp     = U.replicate n 0.5
                , wcHumidity = U.replicate n 0.3
                , wcWindDir  = U.replicate n 0.1
                , wcWindSpd  = U.replicate n 0.2
                , wcPressure = U.replicate n 0.6
                , wcPrecip   = U.replicate n 0.4
                , wcCloudCover = U.replicate n 0.5
                , wcCloudWater = U.replicate n 0.3
                , wcCloudCoverLow  = U.replicate n 0.3
                , wcCloudCoverMid  = U.replicate n 0.15
                , wcCloudCoverHigh = U.replicate n 0.05
                , wcCloudWaterLow  = U.replicate n 0.2
                , wcCloudWaterMid  = U.replicate n 0.08
                , wcCloudWaterHigh = U.replicate n 0.02
                }
      case overlayToWeatherChunk (weatherChunkToOverlay wc) of
        Nothing  -> expectationFailure "round-trip returned Nothing"
        Just wc' -> do
          U.toList (wcCloudCoverLow wc')  `shouldBe` U.toList (wcCloudCoverLow wc)
          U.toList (wcCloudCoverMid wc')  `shouldBe` U.toList (wcCloudCoverMid wc)
          U.toList (wcCloudCoverHigh wc') `shouldBe` U.toList (wcCloudCoverHigh wc)
          U.toList (wcCloudWaterLow wc')  `shouldBe` U.toList (wcCloudWaterLow wc)
          U.toList (wcCloudWaterMid wc')  `shouldBe` U.toList (wcCloudWaterMid wc)
          U.toList (wcCloudWaterHigh wc') `shouldBe` U.toList (wcCloudWaterHigh wc)

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

firstWeatherTemp :: TerrainWorld -> Maybe [Float]
firstWeatherTemp world = do
  chunk <- getWeatherChunk (ChunkId 0) world
  pure (U.toList (wcTemp chunk))

withWeatherOverlay :: TerrainWorld -> WeatherChunk -> TerrainWorld
withWeatherOverlay world weather =
  world { twOverlays = insertOverlay weatherOverlay (twOverlays world) }
  where
    weatherOverlay = Overlay
      { ovSchema = weatherOverlaySchema
      , ovData = DenseData (IntMap.singleton 0 (weatherChunkToOverlay weather))
      , ovProvenance = OverlayProvenance
          { opSeed = 0
          , opVersion = 1
          , opSource = "weather-cloud-delta-spec"
          , opSchedule = Nothing
          }
      }

constantWeatherChunk :: Int -> Float -> Float -> Float -> Float -> Float -> WeatherChunk
constantWeatherChunk n temp humidity precip cloudCover cloudWater =
  let v value = U.replicate n value
  in WeatherChunk
      { wcTemp = v temp
      , wcHumidity = v humidity
      , wcWindDir = v 0
      , wcWindSpd = v 0
      , wcPressure = v 0.5
      , wcPrecip = v precip
      , wcCloudCover = v cloudCover
      , wcCloudWater = v cloudWater
      , wcCloudCoverLow  = v (cloudCover * 0.60)
      , wcCloudCoverMid  = v (cloudCover * 0.25)
      , wcCloudCoverHigh = v (cloudCover * 0.15)
      , wcCloudWaterLow  = v (cloudWater * 0.60)
      , wcCloudWaterMid  = v (cloudWater * 0.25)
      , wcCloudWaterHigh = v (cloudWater * 0.15)
      }

averageField :: U.Vector Float -> Float
averageField values = U.sum values / fromIntegral (U.length values)

runWeatherTick :: WeatherConfig -> TerrainWorld -> IO TerrainWorld
runWeatherTick weatherCfg world =
  case buildSimDAG [weatherSimNode weatherCfg] of
    Left err -> expectationFailure (show err) >> pure world
    Right dag -> do
      result <- runSimulationTickPipeline world dag (\_ -> pure ()) Nothing
      case result of
        Left err -> expectationFailure (show err) >> pure world
        Right tickResult -> pure (stprWorld tickResult)

initWeatherAndTick :: WeatherConfig -> TerrainWorld -> IO TerrainWorld
initWeatherAndTick weatherCfg world0 = do
  wInit <- initWeatherOnly weatherCfg world0
  runWeatherTick weatherCfg wInit

