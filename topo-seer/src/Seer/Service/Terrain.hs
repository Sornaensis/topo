{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Terrain
  ( TerrainService(..)
  , terrainServiceGroup
  , terrainServiceOperationSpecs
  ) where

import Seer.Service.Types

data TerrainService = TerrainService
  { terrainGetHex :: !ServiceHandler
  , terrainGetChunks :: !ServiceHandler
  , terrainGetChunkSummary :: !ServiceHandler
  , terrainGetStats :: !ServiceHandler
  , terrainGetOverlays :: !ServiceHandler
  , terrainFindHexes :: !ServiceHandler
  , terrainExportData :: !ServiceHandler
  }

terrainServiceGroup :: ServiceGroupSpec
terrainServiceGroup = ServiceGroupSpec "terrain" terrainServiceOperationSpecs

terrainServiceOperationSpecs :: [ServiceOperationSpec]
terrainServiceOperationSpecs =
  [ operationSpec "terrain.hex.get" "get_hex" "Read terrain data for one hex."
  , operationSpec "terrain.chunks.list" "get_chunks" "List loaded terrain chunks."
  , operationSpec "terrain.chunk.summary" "get_chunk_summary" "Summarize one terrain chunk."
  , operationSpec "terrain.stats" "get_terrain_stats" "Read aggregate terrain statistics."
  , operationSpec "terrain.overlays" "get_overlays" "List loaded overlays and metadata."
  , operationSpec "terrain.hex.find" "find_hexes" "Search terrain hexes by predicate."
  , operationSpec "terrain.export" "export_terrain_data" "Export terrain data for external consumers."
  ]
