{-# LANGUAGE OverloadedStrings #-}

-- | Shared benchmark fixtures for topo-seer benchmarks.
module Fixtures
  ( benchWorldConfig
  , benchTerrainChunk
  , benchClimateChunk
  , benchWeatherChunk
  , benchVegetationChunk
  , benchRiverChunk
  , benchWorld
  , benchTopoEnv
  , benchChunkMap
  , largeChunkMap
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo

-- | World config with chunk size 8 (64 tiles per chunk).
benchWorldConfig :: WorldConfig
benchWorldConfig = WorldConfig { wcChunkSize = 8 }

-- | A terrain chunk with varied elevation (sine wave pattern).
benchTerrainChunk :: TerrainChunk
benchTerrainChunk =
  generateTerrainChunk benchWorldConfig
    (\(TileCoord x y) -> 0.3 + 0.2 * sin (fromIntegral x * 0.5) * cos (fromIntegral y * 0.5))

-- | Empty climate chunk.
benchClimateChunk :: ClimateChunk
benchClimateChunk = emptyClimateChunk benchWorldConfig

-- | Empty weather chunk (all vectors empty, like BiomeConfig.defaultWeatherChunk).
benchWeatherChunk :: WeatherChunk
benchWeatherChunk = WeatherChunk
  { wcTemp = U.empty, wcHumidity = U.empty
  , wcWindDir = U.empty, wcWindSpd = U.empty
  , wcPressure = U.empty, wcPrecip = U.empty
  , wcCloudCover = U.empty, wcCloudWater = U.empty
  , wcCloudCoverLow = U.empty, wcCloudCoverMid = U.empty, wcCloudCoverHigh = U.empty
  , wcCloudWaterLow = U.empty, wcCloudWaterMid = U.empty, wcCloudWaterHigh = U.empty
  }

-- | Empty vegetation chunk.
benchVegetationChunk :: VegetationChunk
benchVegetationChunk = emptyVegetationChunk benchWorldConfig

-- | Empty river chunk.
benchRiverChunk :: RiverChunk
benchRiverChunk = emptyRiverChunk benchWorldConfig

-- | A small world with a handful of terrain chunks.
benchWorld :: TerrainWorld
benchWorld = emptyWorld benchWorldConfig defaultHexGridMeta

-- | A silent TopoEnv for pipeline benchmarks.
benchTopoEnv :: TopoEnv
benchTopoEnv = TopoEnv { teLogger = \_ -> pure () }

-- | IntMap with 16 chunks (keys 0..15) for viewport culling.
benchChunkMap :: IntMap.IntMap ()
benchChunkMap = IntMap.fromList [(i, ()) | i <- [0..15]]

-- | IntMap with 256 chunks for large-world viewport culling.
largeChunkMap :: IntMap.IntMap ()
largeChunkMap = IntMap.fromList [(i, ()) | i <- [0..255]]
