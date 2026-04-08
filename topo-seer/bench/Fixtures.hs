{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , benchUiState
  , benchTerrainSnapshot
  , benchOverlayStoreDense
  , benchOverlayStoreSparse
  , benchOverlaySchema
  , populatedRiverChunk
  , terrainMap16
  , climateMap16
  , weatherMap16
  , vegMap16
  ) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.Aeson as Aeson
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word16, Word32)
import Topo

import Actor.Data (TerrainSnapshot(..))
import Actor.UI.State (UiState, emptyUiState)
import UI.TerrainRender (ChunkGeometry(..))

------------------------------------------------------------------------
-- Orphan NFData instances used across benchmark modules
------------------------------------------------------------------------

instance NFData ChunkGeometry where
  rnf (ChunkGeometry b v i) = rwhnf b `seq` rnf v `seq` rnf i

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

-- | Weather chunk with 64 tiles (chunkSize 8 * 8) of uniform values.
benchWeatherChunk :: WeatherChunk
benchWeatherChunk =
  let n = 64
      zeros = U.replicate n (0 :: Float)
  in WeatherChunk
  { wcTemp = U.replicate n 288.0, wcHumidity = U.replicate n 0.6
  , wcWindDir = zeros, wcWindSpd = zeros
  , wcPressure = U.replicate n 1013.0, wcPrecip = zeros
  , wcCloudCover = U.replicate n 0.3, wcCloudWater = zeros
  , wcCloudCoverLow = zeros, wcCloudCoverMid = zeros, wcCloudCoverHigh = zeros
  , wcCloudWaterLow = zeros, wcCloudWaterMid = zeros, wcCloudWaterHigh = zeros
  }

-- | Empty vegetation chunk.
benchVegetationChunk :: VegetationChunk
benchVegetationChunk = emptyVegetationChunk benchWorldConfig

-- | Empty river chunk.
benchRiverChunk :: RiverChunk
benchRiverChunk = emptyRiverChunk benchWorldConfig

-- | A river chunk with ~21 river segments spread across ~21 tiles.
-- Entry edge 2, exit edge 5 (opposite edges, inland flow).
populatedRiverChunk :: RiverChunk
populatedRiverChunk =
  let n = 64
      nSegs = 22
  in RiverChunk
    { rcFlowAccum        = U.generate n (\i -> fromIntegral i * 10)
    , rcDischarge         = U.generate n (\i -> fromIntegral i * 5)
    , rcChannelDepth      = U.replicate n 0.5
    , rcRiverOrder        = U.replicate n (2 :: Word16)
    , rcBasinId           = U.replicate n (1 :: Word32)
    , rcBaseflow          = U.replicate n 0
    , rcErosionPotential  = U.replicate n 0
    , rcDepositPotential  = U.replicate n 0
    , rcFlowDir           = U.generate n (\i -> if i < n-1 then i+1 else -1)
    , rcSegOffsets        = U.generate (n+1) (\i -> min nSegs (i `div` 3))
    , rcSegEntryEdge      = U.replicate nSegs (2 :: Word8)
    , rcSegExitEdge       = U.replicate nSegs (5 :: Word8)
    , rcSegDischarge      = U.generate nSegs (\i -> fromIntegral i * 5)
    , rcSegOrder          = U.replicate nSegs (2 :: Word16)
    }

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

------------------------------------------------------------------------
-- Shared terrain chunk maps
------------------------------------------------------------------------

-- | 16 terrain chunks (keys 0..15).
terrainMap16 :: IntMap.IntMap TerrainChunk
terrainMap16 = IntMap.fromList [(i, benchTerrainChunk) | i <- [0..15]]

-- | 16 climate chunks.
climateMap16 :: IntMap.IntMap ClimateChunk
climateMap16 = IntMap.fromList [(i, benchClimateChunk) | i <- [0..15]]

-- | 16 weather chunks.
weatherMap16 :: IntMap.IntMap WeatherChunk
weatherMap16 = IntMap.fromList [(i, benchWeatherChunk) | i <- [0..15]]

-- | 16 vegetation chunks.
vegMap16 :: IntMap.IntMap VegetationChunk
vegMap16 = IntMap.fromList [(i, benchVegetationChunk) | i <- [0..15]]

------------------------------------------------------------------------
-- UiState / TerrainSnapshot
------------------------------------------------------------------------

-- | Default UiState for benchmarks.
benchUiState :: UiState
benchUiState = emptyUiState

-- | TerrainSnapshot with 16 chunks of each data type.
benchTerrainSnapshot :: TerrainSnapshot
benchTerrainSnapshot = TerrainSnapshot
  { tsVersion          = 1
  , tsChunkSize        = 8
  , tsTerrainChunks    = terrainMap16
  , tsClimateChunks    = climateMap16
  , tsWeatherChunks    = weatherMap16
  , tsRiverChunks      = IntMap.fromList [(i, benchRiverChunk) | i <- [0..15]]
  , tsVegetationChunks = vegMap16
  , tsOverlayStore     = emptyOverlayStore
  }

------------------------------------------------------------------------
-- Overlay fixtures
------------------------------------------------------------------------

-- | Single-field Float overlay schema.
benchOverlaySchema :: OverlaySchema
benchOverlaySchema =
  let fields = [ OverlayFieldDef
        { ofdName       = "value"
        , ofdType       = OFFloat
        , ofdDefault    = Aeson.Number 0
        , ofdIndexed    = False
        , ofdRenamedFrom = Nothing
        } ]
  in OverlaySchema
    { osName         = "bench_overlay"
    , osVersion      = "1.0"
    , osDescription  = "Benchmark overlay"
    , osFields       = fields
    , osStorage      = StorageDense
    , osDependencies = emptyOverlayDeps
    , osFieldIndex   = Map.fromList [("value", 0)]
    }

-- | Dense overlay store: 16 chunks, 1 field, 64 floats each.
benchOverlayStoreDense :: OverlayStore
benchOverlayStoreDense =
  let schema = benchOverlaySchema { osStorage = StorageDense }
      ovl = (emptyOverlay schema)
              { ovData = DenseData $ IntMap.fromList
                  [ (i, V.singleton (U.replicate 64 (fromIntegral i * 0.1)))
                  | i <- [0..15] ]
              }
  in insertOverlay ovl emptyOverlayStore

-- | Sparse overlay store: 16 chunks, ~50 % populated.
benchOverlayStoreSparse :: OverlayStore
benchOverlayStoreSparse =
  let schema = benchOverlaySchema { osStorage = StorageSparse }
      ovl = (emptyOverlay schema)
              { ovData = SparseData $ IntMap.fromList
                  [ (i, OverlayChunk $ IntMap.fromList
                      [ (j, OverlayRecord (V.singleton (OVFloat (fromIntegral j * 0.01))))
                      | j <- [0,2..62] ]
                  ) | i <- [0..15] ]
              }
  in insertOverlay ovl emptyOverlayStore
