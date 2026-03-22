-- | Terrain access for topo plugins.
--
-- This module re-exports the subset of @Topo.World@ and @Topo.Types@
-- that plugins need to read and modify terrain data received from
-- the host.  Plugins should import this module (or the top-level
-- "Topo.Plugin.SDK") rather than depending on @topo@ directly for
-- terrain types.
--
-- === Typical usage
--
-- @
-- import Topo.Plugin.SDK
--
-- myGenerator :: PluginContext -> IO (Either Text GeneratorTickResult)
-- myGenerator ctx = do
--   case decodeTerrainPayload (pcTerrain ctx) of
--     Left err -> pure (Left err)
--     Right world -> do
--       let world' = mapChunks roughen world
--       case generatorResultFromTerrain world' of ...
-- @
module Topo.Plugin.SDK.Terrain
  ( -- * World
    TerrainWorld(..)
    -- * Chunk types
  , TerrainChunk(..)
  , ChunkId(..)
    -- * World configuration
  , WorldConfig(..)
    -- * Bulk chunk operations
  , mapChunks
    -- * Per-tile elevation access
  , getElevationAt
  , setElevationAt
    -- * Tile indexing
  , TileCoord(..)
  , TileIndex(..)
  ) where

import Topo.Types
  ( ChunkId(..)
  , TerrainChunk(..)
  , TileCoord(..)
  , TileIndex(..)
  , WorldConfig(..)
  )
import Topo.World
  ( TerrainWorld(..)
  , getElevationAt
  , mapChunks
  , setElevationAt
  )
