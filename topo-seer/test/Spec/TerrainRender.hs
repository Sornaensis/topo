{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainRender (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Linear (V2(..))
import qualified SDL.Raw.Types as Raw
import Topo (WorldConfig(..), TerrainChunk(..), WeatherChunk(..), zeroDirSlope)
import Topo.Types (pattern BiomeDesert, pattern FormFlat, pattern PlateBoundaryNone)
import Topo.Weather (WeatherNormalsChunk(..))
import Actor.UI (ViewMode(..))
import UI.DayNight (dayNightMinBrightness)
import UI.HexGeometry (hexCenterF, renderHexRadiusPx)
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry, buildDayNightGeometry)
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
  let size = 1
      config = WorldConfig { wcChunkSize = size }
      chunk = emptyTerrainChunk size
      geometry = buildChunkGeometry renderHexRadiusPx config mode 0 IntMap.empty (IntMap.singleton 0 weather) IntMap.empty IntMap.empty Nothing 0 chunk
  in vertexColor (cgVertices geometry SV.! 0)

viewColorWithNormals :: ViewMode -> Maybe WeatherChunk -> Maybe WeatherNormalsChunk -> (Word8, Word8, Word8, Word8)
viewColorWithNormals mode mWeather mNormals =
  let size = 1
      config = WorldConfig { wcChunkSize = size }
      chunk = emptyTerrainChunk size
      weatherMap = maybe IntMap.empty (IntMap.singleton 0) mWeather
      normalMap = maybe IntMap.empty (IntMap.singleton 0) mNormals
      geometry = buildChunkGeometry renderHexRadiusPx config mode 0 IntMap.empty weatherMap normalMap IntMap.empty Nothing 0 chunk
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
