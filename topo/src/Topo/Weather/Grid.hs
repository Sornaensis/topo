{-# LANGUAGE OverloadedStrings #-}

-- | Weather overlay schema, conversion, and global-grid utilities.
module Topo.Weather.Grid
  ( weatherFieldCount
  , weatherOverlaySchema
  , weatherChunkToOverlay
  , overlayToWeatherChunk
  , getWeatherFromOverlay
  , getWeatherChunk
  , WeatherGridState(..)
  , buildChunkWeatherFromOverlayOrClimate
  , buildWeatherFieldGrid
  , buildClimateFieldGrid
  , globalTileXY
  , weatherGridToDenseOverlay
  ) where

import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Topo.Overlay (Overlay(..), OverlayData(..), lookupOverlay)
import Topo.Overlay.Schema
  ( OverlaySchema(..), OverlayFieldDef(..), OverlayFieldType(..)
  , OverlayStorage(..), OverlayDeps(..)
  )
import Topo.TerrainGrid (chunkGridSlice)
import Topo.Types
import Topo.World (TerrainWorld(..))

weatherFieldTemperature, weatherFieldHumidity, weatherFieldWindDir,
  weatherFieldWindSpeed, weatherFieldPressure, weatherFieldPrecip :: Int
weatherFieldTemperature = 0
weatherFieldHumidity    = 1
weatherFieldWindDir     = 2
weatherFieldWindSpeed   = 3
weatherFieldPressure    = 4
weatherFieldPrecip      = 5

-- | Number of fields in the weather overlay schema.
weatherFieldCount :: Int
weatherFieldCount = 6

-- | The canonical schema for the weather overlay.
weatherOverlaySchema :: OverlaySchema
weatherOverlaySchema = OverlaySchema
  { osName         = "weather"
  , osVersion      = "1.0.0"
  , osDescription  = "Per-tick weather snapshot (temperature, humidity, wind, pressure, precipitation)"
  , osFields       = fields
  , osStorage      = StorageDense
  , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
  , osFieldIndex   = Map.fromList [(ofdName f, i) | (i, f) <- zip [0..] fields]
  }
  where
    fields =
      [ OverlayFieldDef "temperature"   OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "humidity"      OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "wind_dir"      OFFloat (Number 0.0)  False Nothing
      , OverlayFieldDef "wind_speed"    OFFloat (Number 0.0)  False Nothing
      , OverlayFieldDef "pressure"      OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "precipitation" OFFloat (Number 0.0)  False Nothing
      ]

-- | Convert a 'WeatherChunk' to a dense overlay chunk (SoA layout).
weatherChunkToOverlay :: WeatherChunk -> Vector (U.Vector Float)
weatherChunkToOverlay wc = V.fromList
  [ wcTemp wc
  , wcHumidity wc
  , wcWindDir wc
  , wcWindSpd wc
  , wcPressure wc
  , wcPrecip wc
  ]

-- | Convert a dense overlay chunk back to a 'WeatherChunk'.
overlayToWeatherChunk :: Vector (U.Vector Float) -> Maybe WeatherChunk
overlayToWeatherChunk v
  | V.length v /= weatherFieldCount = Nothing
  | otherwise = Just WeatherChunk
      { wcTemp     = v V.! weatherFieldTemperature
      , wcHumidity = v V.! weatherFieldHumidity
      , wcWindDir  = v V.! weatherFieldWindDir
      , wcWindSpd  = v V.! weatherFieldWindSpeed
      , wcPressure = v V.! weatherFieldPressure
      , wcPrecip   = v V.! weatherFieldPrecip
      }

-- | Extract all weather chunks from the weather overlay.
getWeatherFromOverlay :: TerrainWorld -> IntMap.IntMap WeatherChunk
getWeatherFromOverlay world =
  case lookupOverlay "weather" (twOverlays world) of
    Nothing -> IntMap.empty
    Just ov -> case ovData ov of
      DenseData m  -> IntMap.mapMaybe overlayToWeatherChunk m
      SparseData _ -> IntMap.empty

-- | Look up a single weather chunk by 'ChunkId'.
getWeatherChunk :: ChunkId -> TerrainWorld -> Maybe WeatherChunk
getWeatherChunk (ChunkId cid) world =
  IntMap.lookup cid (getWeatherFromOverlay world)

-- | Stitched global-grid representation of weather fields.
data WeatherGridState = WeatherGridState
  { wgsTemp :: !(U.Vector Float)
  , wgsHumidity :: !(U.Vector Float)
  , wgsWindDir :: !(U.Vector Float)
  , wgsWindSpd :: !(U.Vector Float)
  , wgsPressure :: !(U.Vector Float)
  , wgsPrecip :: !(U.Vector Float)
  }

-- | Build per-chunk weather from existing overlay values, falling back
-- to climate-derived generation when chunks are missing.
buildChunkWeatherFromOverlayOrClimate
  :: (Int -> ClimateChunk -> WeatherChunk)
  -> IntMap.IntMap ClimateChunk
  -> OverlayData
  -> IntMap.IntMap WeatherChunk
buildChunkWeatherFromOverlayOrClimate fallback climateMap ovData =
  IntMap.mapWithKey resolve climateMap
  where
    overlayMap = case ovData of
      DenseData m -> m
      SparseData _ -> IntMap.empty
    resolve key climate =
      let fallbackChunk = fallback key climate
      in fromMaybe fallbackChunk $ do
           fields <- IntMap.lookup key overlayMap
           overlayToWeatherChunk fields

-- | Build a stitched weather field grid from per-chunk weather.
buildWeatherFieldGrid
  :: WorldConfig
  -> IntMap.IntMap WeatherChunk
  -> ChunkCoord
  -> Int
  -> Int
  -> (WeatherChunk -> U.Vector Float)
  -> U.Vector Float
buildWeatherFieldGrid config weatherChunks (ChunkCoord minCx minCy) gridW gridH pickField =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key weatherChunks of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> pickField chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a stitched climate field grid from per-chunk climate.
buildClimateFieldGrid
  :: WorldConfig
  -> IntMap.IntMap ClimateChunk
  -> ChunkCoord
  -> Int
  -> Int
  -> (ClimateChunk -> U.Vector Float)
  -> U.Vector Float
buildClimateFieldGrid config climateChunks (ChunkCoord minCx minCy) gridW gridH pickField =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key climateChunks of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> pickField chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Convert a stitched-grid index to global tile coordinates.
globalTileXY :: WorldConfig -> ChunkCoord -> Int -> Int -> (Int, Int)
globalTileXY config (ChunkCoord minCx minCy) gridW i =
  let size = wcChunkSize config
      x = i `mod` gridW
      y = i `div` gridW
  in (minCx * size + x, minCy * size + y)

-- | Slice stitched weather grids back into dense per-chunk overlay payload.
weatherGridToDenseOverlay
  :: WorldConfig
  -> ChunkCoord
  -> Int
  -> IntMap.IntMap ClimateChunk
  -> WeatherGridState
  -> IntMap.IntMap (Vector (U.Vector Float))
weatherGridToDenseOverlay config minCoord gridW climate state =
  IntMap.mapWithKey toChunk climate
  where
    toChunk key _ = V.fromList
      [ chunkGridSlice config minCoord gridW (wgsTemp state) key
      , chunkGridSlice config minCoord gridW (wgsHumidity state) key
      , chunkGridSlice config minCoord gridW (wgsWindDir state) key
      , chunkGridSlice config minCoord gridW (wgsWindSpd state) key
      , chunkGridSlice config minCoord gridW (wgsPressure state) key
      , chunkGridSlice config minCoord gridW (wgsPrecip state) key
      ]
