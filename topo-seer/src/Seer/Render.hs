module Seer.Render
  ( TerrainCache(..)
  , emptyTerrainCache
  , buildTerrainCache
  , updateChunkTextures
  , terrainCacheNeedsRefresh
  , chunkTextureCacheNeedsUpdate
  , fallbackTerrainNeedsRefresh
  , drawTerrain
  , RenderContext(..)
  , renderFrame
  ) where

import Seer.Render.Context (RenderContext(..))
import Seer.Render.Frame (renderFrame)
import Seer.Render.Terrain
  ( TerrainCache(..)
  , emptyTerrainCache
  , buildTerrainCache
  , updateChunkTextures
  , terrainCacheNeedsRefresh
  , chunkTextureCacheNeedsUpdate
  , fallbackTerrainNeedsRefresh
  , drawTerrain
  )


