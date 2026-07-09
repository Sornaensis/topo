{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainRender (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Linear (V2(..))
import qualified SDL.Raw.Types as Raw
import Topo (ClimateChunk(..), WorldConfig(..), TerrainChunk(..), VegetationChunk(..), WeatherChunk(..), zeroDirSlope)
import Topo.Types (pattern BiomeDesert, pattern FormFlat, pattern PlateBoundaryNone)
import Topo.Weather (WeatherNormalsChunk(..))
import Actor.UI (BaseViewMode(..), LayeredViewState(..), SkyOverlayMode(..), ViewMode(..), WeatherBasis(..), baseViewModeToViewMode, defaultLayeredViewState)
import UI.DayNight (dayNightMinBrightness)
import UI.HexGeometry (hexCenterF, renderHexRadiusPx)
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry, buildChunkGeometryForSelection, buildDayNightGeometry)
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "Terrain render geometry" $ do
  it "builds expected vertex and index counts" $ do
    let size = 2
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        tileCount = size * size
    SV.length (cgVertices geometry) `shouldBe` tileCount * 7
    SV.length (cgIndices geometry) `shouldBe` tileCount * 18

  it "produces non-zero bounds" $ do
    let size = 3
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        Rect (V2 _ _ , V2 w h) = cgBounds geometry
    w `shouldSatisfy` (> 0)
    h `shouldSatisfy` (> 0)

  it "keeps fallback geometry vertices local to cgBounds" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        Rect (V2 bx by, _) = cgBounds geometry
        Raw.Vertex (Raw.FPoint localX localY) _ _ = cgVertices geometry SV.! 0
        (centerX, centerY) = hexCenterF renderHexRadiusPx 0 0
    realToFrac bx + realToFrac localX `shouldSatisfy` closeTo centerX
    realToFrac by + realToFrac localY `shouldSatisfy` closeTo centerY

  it "matches legacy base colors for base-only layered selections" $ do
    let veg = Just (testVegetationChunk 1 0.7)
        check baseMode = do
          let legacyMode = baseViewModeToViewMode baseMode
              selection = defaultLayeredViewState
                { lvsBaseView = baseMode
                , lvsSkyOverlay = Nothing
                }
              layered = selectionColor selection Nothing Nothing Nothing veg
              legacy = viewColorWithData legacyMode Nothing Nothing Nothing veg
          layered `shouldBe` legacy
    mapM_ check
      [ BaseViewElevation
      , BaseViewBiome
      , BaseViewMoisture
      , BaseViewPlateId
      , BaseViewPlateBoundary
      , BaseViewPlateHardness
      , BaseViewPlateCrust
      , BaseViewPlateAge
      , BaseViewPlateHeight
      , BaseViewPlateVelocity
      , BaseViewVegetation
      , BaseViewTerrainForm
      ]

  it "keeps production layered legacy weather selections opaque" $ do
    let weather = testWeatherChunk 1 0.85 0.00 0.00 0.65
        climate = testClimateChunk 1 0.2 0.75 0.4 0.3
        check mode selection =
          selectionColor selection (Just climate) (Just weather) Nothing Nothing
            `shouldBe` viewColorWithData mode (Just climate) (Just weather) Nothing Nothing
    check ViewWeather defaultLayeredViewState
      { lvsSkyOverlay = Just SkyOverlayWeatherTemperature
      , lvsWeatherBasis = WeatherBasisCurrent
      }
    check ViewClimate defaultLayeredViewState
      { lvsSkyOverlay = Just SkyOverlayWeatherTemperature
      , lvsWeatherBasis = WeatherBasisAverage
      }
    check ViewPrecipCurrent defaultLayeredViewState
      { lvsSkyOverlay = Just SkyOverlayPrecipitation
      , lvsWeatherBasis = WeatherBasisCurrent
      }
    check ViewCloud defaultLayeredViewState
      { lvsSkyOverlay = Just SkyOverlayCloud
      , lvsWeatherBasis = WeatherBasisCurrent
      }

  it "alpha-composites current temperature over elevation, biome, and plate boundary bases" $ do
    let weather = testWeatherChunk 1 0.9 0.00 0.00 0.00
        check baseMode = do
          let legacyMode = baseViewModeToViewMode baseMode
              selection = defaultLayeredViewState
                { lvsBaseView = baseMode
                , lvsSkyOverlay = Just SkyOverlayWeatherTemperature
                , lvsWeatherBasis = WeatherBasisCurrent
                , lvsOverlayOpacity = 0.5
                }
              blended = selectionColor selection Nothing (Just weather) Nothing Nothing
              baseOnly = viewColorWithData legacyMode Nothing (Just weather) Nothing Nothing
              legacyWeather = viewColorWithData ViewWeather Nothing (Just weather) Nothing Nothing
          blended `shouldNotBe` baseOnly
          blended `shouldNotBe` legacyWeather
          vertexAlphaTuple blended `shouldBe` 255
    mapM_ check [BaseViewElevation, BaseViewBiome, BaseViewPlateBoundary]

  it "keeps legacy scalar climate and weather modes opaque" $ do
    let climate = testClimateChunk 1 0.2 0.75 0.4 0.3
        weather = testWeatherChunk 1 0.85 0.00 0.00 0.65
        legacy mode = viewColorWithData mode (Just climate) (Just weather) Nothing Nothing
    legacy ViewClimate `shouldBe` heatTuple 0.2
    legacy ViewWeather `shouldBe` heatTuple 0.85
    legacy ViewPrecip `shouldBe` moistureTuple 0.75
    legacy ViewPrecipCurrent `shouldBe` moistureTuple 0.65

  it "maps clear, cloudy, and storm fixtures to ordered ViewCloud colours" $ do
    let clear = viewColor ViewCloud (testWeatherChunk 1 0.45 0.00 0.00 0.00)
        cloudy = viewColor ViewCloud (testWeatherChunk 1 0.45 0.60 0.25 0.00)
        storm = viewColor ViewCloud (testWeatherChunk 1 0.45 0.95 1.00 0.80)
    clear `shouldNotBe` cloudy
    cloudy `shouldNotBe` storm
    channelSum cloudy `shouldSatisfy` (> channelSum clear + 250)
    storm `shouldSatisfy` blueDominant
    vertexAlphaTuple clear `shouldBe` 255
    vertexAlphaTuple cloudy `shouldBe` 255
    vertexAlphaTuple storm `shouldBe` 255

  it "changes ViewCloud geometry colours for cover, water, and precipitation deltas" $ do
    let base = viewColor ViewCloud (testWeatherChunk 1 0.45 0.55 0.20 0.00)
        moreCover = viewColor ViewCloud (testWeatherChunk 1 0.45 0.80 0.20 0.00)
        moreWater = viewColor ViewCloud (testWeatherChunk 1 0.45 0.55 0.70 0.00)
        morePrecip = viewColor ViewCloud (testWeatherChunk 1 0.45 0.55 0.70 0.70)
    moreCover `shouldNotBe` base
    moreWater `shouldNotBe` base
    morePrecip `shouldNotBe` moreWater
    channelSum moreCover `shouldSatisfy` (> channelSum base + 100)
    channelSum moreWater `shouldSatisfy` (< channelSum base - 80)
    blueDominance morePrecip `shouldSatisfy` (> blueDominance moreWater + 20)

  it "keeps ViewWeather geometry temperature-only when cloud fields change" $ do
    let clear = viewColor ViewWeather (testWeatherChunk 1 0.45 0.00 0.00 0.00)
        storm = viewColor ViewWeather (testWeatherChunk 1 0.45 0.95 1.00 0.90)
    storm `shouldBe` clear

  it "renders current precipitation from WeatherChunk precipitation" $ do
    let dry = viewColor ViewPrecipCurrent (testWeatherChunk 1 0.45 0.00 0.00 0.00)
        storm = viewColor ViewPrecipCurrent (testWeatherChunk 1 0.45 0.00 0.00 0.90)
    storm `shouldNotBe` dry
    channelSum storm `shouldSatisfy` (> channelSum dry)

  it "renders typical cloud normals from weather_normals and never falls back to current clouds" $ do
    let currentStorm = testWeatherChunk 1 0.45 0.95 1.00 0.90
        typicalClear = testWeatherNormalsChunk 1 0.05 0.05 0.00
        currentColor = viewColor ViewCloud currentStorm
        typicalColor = viewColorWithNormals ViewCloudTypical (Just currentStorm) (Just typicalClear)
        missingColor = viewColorWithNormals ViewCloudTypical (Just currentStorm) Nothing
    typicalColor `shouldNotBe` currentColor
    missingColor `shouldNotBe` currentColor
    typicalColor `shouldNotBe` missingColor

  it "builds a transparent black day/night overlay at full brightness" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> 1.0) 0 chunk
    map vertexColor (SV.toList (cgVertices geometry)) `shouldSatisfy` all (== (0, 0, 0, 0))

  it "builds a capped-alpha black day/night overlay for deep night" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> dayNightMinBrightness) 0 chunk
    map vertexColor (SV.toList (cgVertices geometry)) `shouldSatisfy` all (== (0, 0, 0, 160))

  it "keeps twilight overlay alpha distinguishable from day and deep night" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        vertexAlphas brightness =
          let geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> brightness) 0 chunk
          in map vertexAlpha (SV.toList (cgVertices geometry))
        deepNight = vertexAlphas dayNightMinBrightness
        twilight = vertexAlphas 0.4
        daylight = vertexAlphas 1.0
    deepNight `shouldSatisfy` all (== 160)
    twilight `shouldSatisfy` all (\a -> a > 0 && a < 160)
    daylight `shouldSatisfy` all (== 0)

  it "keeps overlay alpha monotonic as brightness increases" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        alphaFor brightness =
          let geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> brightness) 0 chunk
          in vertexAlpha (cgVertices geometry SV.! 0)
        alphas = map alphaFor [dayNightMinBrightness, 0.25, 0.4, 0.7, 1.0]
    zipWith (>=) alphas (drop 1 alphas) `shouldSatisfy` and

  it "clamps out-of-range overlay inputs to the visual alpha bounds" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        vertexAlphas brightness =
          let geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> brightness) 0 chunk
          in map vertexAlpha (SV.toList (cgVertices geometry))
    vertexAlphas 0.0 `shouldSatisfy` all (== 160)
    vertexAlphas 2.0 `shouldSatisfy` all (== 0)

vertexColor :: Raw.Vertex -> (Word8, Word8, Word8, Word8)
vertexColor (Raw.Vertex _ (Raw.Color r g b a) _) = (r, g, b, a)

vertexAlpha :: Raw.Vertex -> Word8
vertexAlpha (Raw.Vertex _ (Raw.Color _ _ _ a) _) = a

viewColor :: ViewMode -> WeatherChunk -> (Word8, Word8, Word8, Word8)
viewColor mode weather =
  viewColorWithData mode Nothing (Just weather) Nothing Nothing

viewColorWithNormals :: ViewMode -> Maybe WeatherChunk -> Maybe WeatherNormalsChunk -> (Word8, Word8, Word8, Word8)
viewColorWithNormals mode mWeather mNormals =
  viewColorWithData mode Nothing mWeather mNormals Nothing

viewColorWithData :: ViewMode -> Maybe ClimateChunk -> Maybe WeatherChunk -> Maybe WeatherNormalsChunk -> Maybe VegetationChunk -> (Word8, Word8, Word8, Word8)
viewColorWithData mode mClimate mWeather mNormals mVeg =
  let size = 1
      config = WorldConfig { wcChunkSize = size }
      chunk = emptyTerrainChunk size
      climateMap = maybe IntMap.empty (IntMap.singleton 0) mClimate
      weatherMap = maybe IntMap.empty (IntMap.singleton 0) mWeather
      normalMap = maybe IntMap.empty (IntMap.singleton 0) mNormals
      vegMap = maybe IntMap.empty (IntMap.singleton 0) mVeg
      geometry = buildChunkGeometry renderHexRadiusPx config mode 0 climateMap weatherMap normalMap vegMap Nothing 0 chunk
  in vertexColor (cgVertices geometry SV.! 0)

selectionColor :: LayeredViewState -> Maybe ClimateChunk -> Maybe WeatherChunk -> Maybe WeatherNormalsChunk -> Maybe VegetationChunk -> (Word8, Word8, Word8, Word8)
selectionColor selection mClimate mWeather mNormals mVeg =
  let size = 1
      config = WorldConfig { wcChunkSize = size }
      chunk = emptyTerrainChunk size
      climateMap = maybe IntMap.empty (IntMap.singleton 0) mClimate
      weatherMap = maybe IntMap.empty (IntMap.singleton 0) mWeather
      normalMap = maybe IntMap.empty (IntMap.singleton 0) mNormals
      vegMap = maybe IntMap.empty (IntMap.singleton 0) mVeg
      geometry = buildChunkGeometryForSelection renderHexRadiusPx config selection 0 climateMap weatherMap normalMap vegMap Nothing 0 chunk
  in vertexColor (cgVertices geometry SV.! 0)

testWeatherChunk :: Int -> Float -> Float -> Float -> Float -> WeatherChunk
testWeatherChunk total temp cover water precip =
  let v value = U.replicate total value
  in WeatherChunk
      { wcTemp = v temp
      , wcHumidity = v 0.5
      , wcWindDir = v 0
      , wcWindSpd = v 0
      , wcPressure = v 0.5
      , wcPrecip = v precip
      , wcCloudCover = v cover
      , wcCloudWater = v water
      , wcCloudCoverLow  = v (cover * 0.60)
      , wcCloudCoverMid  = v (cover * 0.25)
      , wcCloudCoverHigh = v (cover * 0.15)
      , wcCloudWaterLow  = v (water * 0.60)
      , wcCloudWaterMid  = v (water * 0.25)
      , wcCloudWaterHigh = v (water * 0.15)
      }

testWeatherNormalsChunk :: Int -> Float -> Float -> Float -> WeatherNormalsChunk
testWeatherNormalsChunk total cover water precip =
  let v value = U.replicate total value
  in WeatherNormalsChunk
      { wncTemp = v 0.45
      , wncHumidity = v 0.5
      , wncWindDir = v 0
      , wncWindSpd = v 0
      , wncPrecip = v precip
      , wncCloudCover = v cover
      , wncCloudWater = v water
      , wncCloudCoverLow  = v (cover * 0.60)
      , wncCloudCoverMid  = v (cover * 0.25)
      , wncCloudCoverHigh = v (cover * 0.15)
      , wncCloudWaterLow  = v (water * 0.60)
      , wncCloudWaterMid  = v (water * 0.25)
      , wncCloudWaterHigh = v (water * 0.15)
      }

testClimateChunk :: Int -> Float -> Float -> Float -> Float -> ClimateChunk
testClimateChunk total temp precip humidity windSpd =
  let v value = U.replicate total value
  in ClimateChunk
      { ccTempAvg = v temp
      , ccPrecipAvg = v precip
      , ccWindDirAvg = v 0
      , ccWindSpdAvg = v windSpd
      , ccHumidityAvg = v humidity
      , ccTempRange = v 0
      , ccPrecipSeasonality = v 0
      }

testVegetationChunk :: Int -> Float -> VegetationChunk
testVegetationChunk total cover =
  let v value = U.replicate total value
  in VegetationChunk
      { vegCover = v cover
      , vegAlbedo = v 0.2
      , vegDensity = v cover
      }

heatTuple :: Float -> (Word8, Word8, Word8, Word8)
heatTuple value =
  let v = clamp01Local value
  in ( toByteLocal (0.5 + v * 0.5)
     , toByteLocal (0.2 + v * 0.4)
     , toByteLocal (0.2 + v * 0.2)
     , 255
     )

moistureTuple :: Float -> (Word8, Word8, Word8, Word8)
moistureTuple value =
  let v = clamp01Local value
  in ( toByteLocal (0.1 + v * 0.2)
     , toByteLocal (0.3 + v * 0.5)
     , toByteLocal (0.4 + v * 0.6)
     , 255
     )

toByteLocal :: Float -> Word8
toByteLocal value = fromIntegral (round (clamp01Local value * 255) :: Int)

clamp01Local :: Float -> Float
clamp01Local value = max 0 (min 1 value)

channelSum :: (Word8, Word8, Word8, Word8) -> Int
channelSum (r, g, b, _) = fromIntegral r + fromIntegral g + fromIntegral b

blueDominance :: (Word8, Word8, Word8, Word8) -> Int
blueDominance (r, g, b, _) = fromIntegral b - max (fromIntegral r) (fromIntegral g)

blueDominant :: (Word8, Word8, Word8, Word8) -> Bool
blueDominant color = blueDominance color > 45

vertexAlphaTuple :: (Word8, Word8, Word8, Word8) -> Word8
vertexAlphaTuple (_, _, _, a) = a

closeTo :: Float -> Float -> Bool
closeTo expected actual = abs (actual - expected) < 0.001

emptyTerrainChunk :: Int -> TerrainChunk
emptyTerrainChunk size =
  let total = size * size
      zerosF = U.replicate total 0
      zerosW = U.replicate total 0
      biomeZeros = U.replicate total BiomeDesert
      boundaryZeros = U.replicate total PlateBoundaryNone
  in TerrainChunk
      { tcElevation = zerosF
      , tcDirSlope = U.replicate total zeroDirSlope
      , tcCurvature = zerosF
      , tcHardness = zerosF
      , tcRockType = zerosW
      , tcSoilType = zerosW
      , tcSoilDepth = zerosF
      , tcMoisture = zerosF
      , tcFertility = zerosF
      , tcRoughness = zerosF
      , tcRockDensity = zerosF
      , tcSoilGrain = zerosF
      , tcRelief = zerosF
      , tcRelief2Ring = zerosF
      , tcRelief3Ring = zerosF
      , tcRuggedness = zerosF
      , tcTerrainForm = U.replicate total FormFlat
      , tcFlags = biomeZeros
      , tcMicroRelief = zerosF
      , tcPlateId = zerosW
    , tcPlateBoundary = boundaryZeros
    , tcPlateHeight = zerosF
    , tcPlateHardness = zerosF
    , tcPlateCrust = zerosW
    , tcPlateAge = zerosF
    , tcPlateVelX = zerosF
    , tcPlateVelY = zerosF
      }
