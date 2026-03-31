{-# LANGUAGE BangPatterns #-}

-- | Incremental recomputation of derived terrain fields after local edits.
--
-- When the terrain editor modifies elevation (or other base fields) for a
-- small set of tiles, this module recomputes only the affected chunks
-- rather than re-deriving the entire world.  A one-chunk border ring is
-- included to keep cross-chunk stencil results correct.
module Topo.Parameters.Recompute
  ( recomputeDerivedChunks
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Topo.Parameters (ParameterConfig, TerrainFormConfig, deriveChunk)
import Topo.Types
  ( ChunkCoord(..)
  , ChunkId(..)
  , TileCoord(..)
  , TerrainChunk
  , WorldConfig(..)
  , chunkCoordFromId
  , chunkCoordFromTile
  , chunkIdFromCoord
  , chunkOriginTile
  )

-- | Recompute derived terrain fields for chunks that contain at least
-- one of the given global tile coordinates.
--
-- Neighboring chunks (sharing a border with any affected chunk) are
-- also re-derived so that cross-chunk slope and relief stencils remain
-- correct.
--
-- Only chunks that actually exist in the 'IntMap' are touched; missing
-- chunk IDs are silently skipped.
recomputeDerivedChunks
  :: WorldConfig
  -> ParameterConfig
  -> TerrainFormConfig
  -> Float
     -- ^ Water level (for elevation-above-sea-level in terrain form
     -- classification).
  -> [(Int, Int)]
     -- ^ Global tile coordinates that were modified.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
recomputeDerivedChunks cfg paramCfg formCfg waterLevel tiles chunks =
  let -- Collect the set of chunk IDs that contain modified tiles.
      directIds = Set.fromList
        [ chunkIdFromCoord cc
        | (q, r) <- tiles
        , let (cc, _) = chunkCoordFromTile cfg (TileCoord q r)
        ]
      -- Expand by one chunk ring in every direction to cover stencil
      -- boundary requirements (ring-3 relief needs up to 3 extra
      -- tiles, which is within one chunk when chunkSize >= 4).
      size = wcChunkSize cfg
      expandedIds = Set.fromList
        [ chunkIdFromCoord cc'
        | cid <- Set.toList directIds
        , IntMap.member (let ChunkId k = cid in k) chunks
        , let TileCoord ox oy = chunkOriginTile cfg (chunkCoordFromId cid)
        , dx <- [-size, 0, size]
        , dy <- [-size, 0, size]
        , let (cc', _) = chunkCoordFromTile cfg
                (TileCoord (ox + dx) (oy + dy))
        ]
      -- Re-derive each affected chunk.
      rederive !acc cid =
        let ChunkId key = cid
        in case IntMap.lookup key acc of
          Nothing    -> acc
          Just chunk ->
            let !chunk' = deriveChunk cfg acc paramCfg formCfg waterLevel key chunk
            in IntMap.insert key chunk' acc
  in foldl rederive chunks (Set.toList expandedIds)
