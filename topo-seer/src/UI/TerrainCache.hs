module UI.TerrainCache
  ( ChunkTextureCache(..)
  , emptyChunkTextureCache
  ) where

import Actor.UI (ViewMode(..))
import Data.IntMap.Strict (IntMap)
import Topo (ClimateChunk, TerrainChunk, WeatherChunk)
import UI.TerrainRender (ChunkTexture)


data ChunkTextureCache = ChunkTextureCache
  { ctcViewMode :: !ViewMode
  , ctcWaterLevel :: !Float
  , ctcChunkSize :: !Int
  , ctcScale :: !Int
  , ctcTerrainChunks :: !(IntMap TerrainChunk)
  , ctcClimateChunks :: !(IntMap ClimateChunk)
  , ctcWeatherChunks :: !(IntMap WeatherChunk)
  , ctcTextures :: !(IntMap ChunkTexture)
  }

emptyChunkTextureCache :: ChunkTextureCache
emptyChunkTextureCache = ChunkTextureCache
  { ctcViewMode = ViewElevation
  , ctcWaterLevel = 0
  , ctcChunkSize = 0
  , ctcScale = 1
  , ctcTerrainChunks = mempty
  , ctcClimateChunks = mempty
  , ctcWeatherChunks = mempty
  , ctcTextures = mempty
  }
