{-# LANGUAGE OverloadedStrings #-}

-- | Generated typical weather normals stored as a dense overlay.
--
-- These fields are deterministic products of 'ClimateChunk' long-run
-- averages and 'WeatherConfig' cloud parameters.  They are intended as a
-- generated typical/average rendering basis, not observed live climate
-- normals and not rolling averages over simulated weather ticks.
module Topo.Weather.Normals
  ( WeatherNormalsChunk(..)
  , weatherNormalsOverlayName
  , weatherNormalsFieldCount
  , weatherNormalsOverlaySchema
  , weatherNormalsChunkFromClimate
  , weatherNormalsChunkToOverlay
  , overlayToWeatherNormalsChunk
  , weatherNormalsOverlayFromClimate
  , weatherNormalsOverlayForWorld
  , getWeatherNormalsFromStore
  , getWeatherNormalsFromOverlay
  , getWeatherNormalsChunkFromStore
  , getWeatherNormalsChunk
  ) where

import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)

import Topo.Math (clamp01)
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), OverlayStore, lookupOverlay)
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )
import Topo.Types (ChunkId(..), ClimateChunk(..))
import Topo.Weather.Config (WeatherConfig(..))
import Topo.World (TerrainWorld(..))

-- | Canonical dense overlay name for generated typical weather normals.
weatherNormalsOverlayName :: Text
weatherNormalsOverlayName = "weather_normals"

weatherNormalsFieldTemperature, weatherNormalsFieldHumidity,
  weatherNormalsFieldWindDir, weatherNormalsFieldWindSpeed,
  weatherNormalsFieldPrecip, weatherNormalsFieldCloudCover,
  weatherNormalsFieldCloudWater,
  weatherNormalsFieldCloudCoverLow, weatherNormalsFieldCloudCoverMid,
  weatherNormalsFieldCloudCoverHigh,
  weatherNormalsFieldCloudWaterLow, weatherNormalsFieldCloudWaterMid,
  weatherNormalsFieldCloudWaterHigh :: Int
weatherNormalsFieldTemperature    = 0
weatherNormalsFieldHumidity       = 1
weatherNormalsFieldWindDir        = 2
weatherNormalsFieldWindSpeed      = 3
weatherNormalsFieldPrecip         = 4
weatherNormalsFieldCloudCover     = 5
weatherNormalsFieldCloudWater     = 6
weatherNormalsFieldCloudCoverLow  = 7
weatherNormalsFieldCloudCoverMid  = 8
weatherNormalsFieldCloudCoverHigh = 9
weatherNormalsFieldCloudWaterLow  = 10
weatherNormalsFieldCloudWaterMid  = 11
weatherNormalsFieldCloudWaterHigh = 12

-- | Number of fields in the weather normals overlay schema.
weatherNormalsFieldCount :: Int
weatherNormalsFieldCount = 13

-- | Per-tile generated typical weather values for one chunk.
data WeatherNormalsChunk = WeatherNormalsChunk
  { wncTemp :: !(U.Vector Float)
  , wncHumidity :: !(U.Vector Float)
  , wncWindDir :: !(U.Vector Float)
  , wncWindSpd :: !(U.Vector Float)
  , wncPrecip :: !(U.Vector Float)
  , wncCloudCover :: !(U.Vector Float)
  , wncCloudWater :: !(U.Vector Float)
  , wncCloudCoverLow :: !(U.Vector Float)
  , wncCloudCoverMid :: !(U.Vector Float)
  , wncCloudCoverHigh :: !(U.Vector Float)
  , wncCloudWaterLow :: !(U.Vector Float)
  , wncCloudWaterMid :: !(U.Vector Float)
  , wncCloudWaterHigh :: !(U.Vector Float)
  } deriving (Eq, Show)

-- | Dense schema for generated typical weather normals.
weatherNormalsOverlaySchema :: OverlaySchema
weatherNormalsOverlaySchema = OverlaySchema
  { osName         = weatherNormalsOverlayName
  , osVersion      = "1.0.0"
  , osDescription  = "Generated typical weather normals derived from climate averages (not live simulated weather)"
  , osFields       = fields
  , osStorage      = StorageDense
  , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
  , osFieldIndex   = Map.fromList [(ofdName f, i) | (i, f) <- zip [0..] fields]
  }
  where
    fields =
      [ OverlayFieldDef "temperature"      OFFloat (Number 0.5) False Nothing
      , OverlayFieldDef "humidity"         OFFloat (Number 0.5) False Nothing
      , OverlayFieldDef "wind_dir"         OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "wind_speed"       OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "precipitation"    OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_cover"      OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_water"      OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_cover_low"  OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_cover_mid"  OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_cover_high" OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_water_low"  OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_water_mid"  OFFloat (Number 0.0) False Nothing
      , OverlayFieldDef "cloud_water_high" OFFloat (Number 0.0) False Nothing
      ]

-- | Generate deterministic typical weather normals for one climate chunk.
weatherNormalsChunkFromClimate :: WeatherConfig -> ClimateChunk -> WeatherNormalsChunk
weatherNormalsChunkFromClimate cfg climate =
  let n = U.length (ccTempAvg climate)
      cloudCover = U.generate n (typicalCloudCoverAt cfg climate)
      cloudWater = U.generate n $ \i ->
        let cover = cloudCover U.! i
            humidity = clamp01 (ccHumidityAvg climate U.! i)
            precip = clamp01 (ccPrecipAvg climate U.! i)
        in clamp01 (cover * (0.65 * humidity + 0.35 * precip))
  in WeatherNormalsChunk
      { wncTemp = ccTempAvg climate
      , wncHumidity = ccHumidityAvg climate
      , wncWindDir = ccWindDirAvg climate
      , wncWindSpd = ccWindSpdAvg climate
      , wncPrecip = ccPrecipAvg climate
      , wncCloudCover = cloudCover
      , wncCloudWater = cloudWater
      , wncCloudCoverLow  = U.map (\v -> clamp01 (v * 0.60)) cloudCover
      , wncCloudCoverMid  = U.map (\v -> clamp01 (v * 0.25)) cloudCover
      , wncCloudCoverHigh = U.map (\v -> clamp01 (v * 0.15)) cloudCover
      , wncCloudWaterLow  = U.map (\v -> clamp01 (v * 0.60)) cloudWater
      , wncCloudWaterMid  = U.map (\v -> clamp01 (v * 0.25)) cloudWater
      , wncCloudWaterHigh = U.map (\v -> clamp01 (v * 0.15)) cloudWater
      }

-- | Convert a generated weather normal chunk to dense overlay vectors.
weatherNormalsChunkToOverlay :: WeatherNormalsChunk -> Vector (U.Vector Float)
weatherNormalsChunkToOverlay normals = V.fromList
  [ wncTemp normals
  , wncHumidity normals
  , wncWindDir normals
  , wncWindSpd normals
  , wncPrecip normals
  , wncCloudCover normals
  , wncCloudWater normals
  , wncCloudCoverLow normals
  , wncCloudCoverMid normals
  , wncCloudCoverHigh normals
  , wncCloudWaterLow normals
  , wncCloudWaterMid normals
  , wncCloudWaterHigh normals
  ]

-- | Convert dense overlay vectors back to a weather normals chunk.
overlayToWeatherNormalsChunk :: Vector (U.Vector Float) -> Maybe WeatherNormalsChunk
overlayToWeatherNormalsChunk v
  | V.length v /= weatherNormalsFieldCount = Nothing
  | otherwise = Just WeatherNormalsChunk
      { wncTemp           = v V.! weatherNormalsFieldTemperature
      , wncHumidity       = v V.! weatherNormalsFieldHumidity
      , wncWindDir        = v V.! weatherNormalsFieldWindDir
      , wncWindSpd        = v V.! weatherNormalsFieldWindSpeed
      , wncPrecip         = v V.! weatherNormalsFieldPrecip
      , wncCloudCover     = v V.! weatherNormalsFieldCloudCover
      , wncCloudWater     = v V.! weatherNormalsFieldCloudWater
      , wncCloudCoverLow  = v V.! weatherNormalsFieldCloudCoverLow
      , wncCloudCoverMid  = v V.! weatherNormalsFieldCloudCoverMid
      , wncCloudCoverHigh = v V.! weatherNormalsFieldCloudCoverHigh
      , wncCloudWaterLow  = v V.! weatherNormalsFieldCloudWaterLow
      , wncCloudWaterMid  = v V.! weatherNormalsFieldCloudWaterMid
      , wncCloudWaterHigh = v V.! weatherNormalsFieldCloudWaterHigh
      }

-- | Build the complete generated normals overlay from climate chunks.
weatherNormalsOverlayFromClimate
  :: Word64
  -> WeatherConfig
  -> IntMap.IntMap ClimateChunk
  -> Overlay
weatherNormalsOverlayFromClimate seed cfg climateMap = Overlay
  { ovSchema = weatherNormalsOverlaySchema
  , ovData = DenseData $ IntMap.map
      (weatherNormalsChunkToOverlay . weatherNormalsChunkFromClimate cfg)
      climateMap
  , ovProvenance = OverlayProvenance
      { opSeed = seed
      , opVersion = 1
      , opSource = weatherNormalsOverlayName
      , opSchedule = Nothing
      }
  }

-- | Build the generated normals overlay for an existing world.
weatherNormalsOverlayForWorld :: WeatherConfig -> TerrainWorld -> Overlay
weatherNormalsOverlayForWorld cfg world =
  weatherNormalsOverlayFromClimate (twSeed world) cfg (twClimate world)

-- | Extract all generated weather normals chunks from an overlay store.
getWeatherNormalsFromStore :: OverlayStore -> IntMap.IntMap WeatherNormalsChunk
getWeatherNormalsFromStore store =
  case lookupOverlay weatherNormalsOverlayName store of
    Nothing -> IntMap.empty
    Just ov -> case ovData ov of
      DenseData m  -> IntMap.mapMaybe overlayToWeatherNormalsChunk m
      SparseData _ -> IntMap.empty

-- | Extract generated weather normals chunks from a world overlay store.
getWeatherNormalsFromOverlay :: TerrainWorld -> IntMap.IntMap WeatherNormalsChunk
getWeatherNormalsFromOverlay = getWeatherNormalsFromStore . twOverlays

-- | Look up one generated weather normals chunk in an overlay store.
getWeatherNormalsChunkFromStore :: ChunkId -> OverlayStore -> Maybe WeatherNormalsChunk
getWeatherNormalsChunkFromStore (ChunkId cid) store =
  IntMap.lookup cid (getWeatherNormalsFromStore store)

-- | Look up one generated weather normals chunk in a world.
getWeatherNormalsChunk :: ChunkId -> TerrainWorld -> Maybe WeatherNormalsChunk
getWeatherNormalsChunk cid world =
  getWeatherNormalsChunkFromStore cid (twOverlays world)

typicalCloudCoverAt :: WeatherConfig -> ClimateChunk -> Int -> Float
typicalCloudCoverAt cfg climate i =
  let humidity = clamp01 (ccHumidityAvg climate U.! i)
      precip = clamp01 (ccPrecipAvg climate U.! i)
      seasonality = clamp01 (ccPrecipSeasonality climate U.! i)
      humidityCloud = clamp01 (humidity ** wcCloudRHExponent cfg)
      precipCloud = sqrt precip
      seasonalityDamping = 1 - 0.20 * seasonality
      baseCloud = 0.70 * humidityCloud + 0.30 * precipCloud
  in clamp01 (baseCloud * seasonalityDamping + 0.08 * precip * (1 - seasonality))
