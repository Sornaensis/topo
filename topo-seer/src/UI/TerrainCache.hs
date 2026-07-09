module UI.TerrainCache
  ( ChunkTextureCache(..)
  , emptyChunkTextureCache
  ) where

import Actor.UI (LayeredViewState, ViewMode(..), defaultLayeredViewState)
import Data.IntMap.Strict (IntMap)
import Data.Word (Word64)
import Topo (ClimateChunk, TerrainChunk, WeatherChunk)
import UI.TerrainRender (ChunkTexture)


data ChunkTextureCache = ChunkTextureCache
  { ctcVersion :: !Word64
  , ctcViewMode :: !ViewMode
  , ctcViewSelection :: !LayeredViewState
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
  { ctcVersion = 0
  , ctcViewMode = ViewElevation
  , ctcViewSelection = defaultLayeredViewState
  , ctcWaterLevel = 0
  , ctcChunkSize = 0
  , ctcScale = 1
  , ctcTerrainChunks = mempty
  , ctcClimateChunks = mempty
  , ctcWeatherChunks = mempty
  , ctcTextures = mempty
  }
