{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Terrain
  ( TerrainService(..)
  , TerrainGetHexRequest(..)
  , TerrainHexResponse(..)
  , TerrainGetChunksRequest(..)
  , TerrainChunksResponse(..)
  , TerrainChunkBrief(..)
  , TerrainGetChunkSummaryRequest(..)
  , TerrainChunkSummaryResponse(..)
  , TerrainScalarStats(..)
  , TerrainNamedCount(..)
  , TerrainStatsRequest(..)
  , TerrainStatsResponse(..)
  , TerrainOverlaysRequest(..)
  , TerrainOverlaysResponse(..)
  , TerrainOverlaySummary(..)
  , TerrainOverlayFieldSummary(..)
  , TerrainFindHexesRequest(..)
  , TerrainFindHexesResponse(..)
  , TerrainFindFilter(..)
  , TerrainFilterOp(..)
  , TerrainHexSearchMatch(..)
  , TerrainExportRequest(..)
  , TerrainExportResponse(..)
  , terrainGetHexOperation
  , terrainGetChunksOperation
  , terrainGetChunkSummaryOperation
  , terrainGetStatsOperation
  , terrainGetOverlaysOperation
  , terrainFindHexesOperation
  , terrainExportDataOperation
  , terrainServiceGroup
  , terrainServiceOperationSpecs
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Seer.Service.Types
import Topo.Overlay.Schema (OverlayFieldType)
import Topo.Types (ChunkId, TileCoord)

data TerrainService = TerrainService
  { terrainGetHex :: !ServiceHandler
  , terrainGetChunks :: !ServiceHandler
  , terrainGetChunkSummary :: !ServiceHandler
  , terrainGetStats :: !ServiceHandler
  , terrainGetOverlays :: !ServiceHandler
  , terrainFindHexes :: !ServiceHandler
  , terrainExportData :: !ServiceHandler
  }

newtype TerrainGetHexRequest = TerrainGetHexRequest
  { terrainHexCoord :: TileCoord
  } deriving (Eq, Show)

data TerrainHexResponse = TerrainHexResponse
  { terrainHexCoord :: !TileCoord
  , terrainHexTerrain :: !Value
  , terrainHexClimate :: !(Maybe Value)
  , terrainHexWeather :: !(Maybe Value)
  , terrainHexRiver :: !(Maybe Value)
  , terrainHexWaterBody :: !(Maybe Value)
  , terrainHexWaterTable :: !(Maybe Value)
  , terrainHexVegetation :: !(Maybe Value)
  , terrainHexSections :: !Value
  } deriving (Eq, Show)

data TerrainGetChunksRequest = TerrainGetChunksRequest
  deriving (Eq, Show)

data TerrainChunkBrief = TerrainChunkBrief
  { terrainChunkBriefId :: !ChunkId
  , terrainChunkBriefTileCount :: !Int
  , terrainChunkBriefElevationMin :: !Float
  , terrainChunkBriefElevationMax :: !Float
  } deriving (Eq, Show)

data TerrainChunksResponse = TerrainChunksResponse
  { terrainChunksChunkCount :: !Int
  , terrainChunksChunkSize :: !Int
  , terrainChunksTilesPerChunk :: !Int
  , terrainChunks :: ![TerrainChunkBrief]
  } deriving (Eq, Show)

newtype TerrainGetChunkSummaryRequest = TerrainGetChunkSummaryRequest
  { terrainSummaryChunkId :: ChunkId
  } deriving (Eq, Show)

data TerrainScalarStats = TerrainScalarStats
  { terrainStatMin :: !Float
  , terrainStatMax :: !Float
  , terrainStatMean :: !Float
  } deriving (Eq, Show)

data TerrainNamedCount = TerrainNamedCount
  { terrainCountName :: !Text
  , terrainCountValue :: !Int
  , terrainCountPercent :: !(Maybe Double)
  } deriving (Eq, Show)

data TerrainChunkSummaryResponse = TerrainChunkSummaryResponse
  { terrainSummaryChunkId :: !ChunkId
  , terrainSummaryTileCount :: !Int
  , terrainSummaryElevation :: !TerrainScalarStats
  , terrainSummaryMoistureMean :: !Float
  , terrainSummaryDominantBiome :: !Text
  , terrainSummaryTerrainForms :: ![TerrainNamedCount]
  , terrainSummaryClimate :: !(Maybe Value)
  , terrainSummaryRiver :: !(Maybe Value)
  } deriving (Eq, Show)

data TerrainStatsRequest = TerrainStatsRequest
  deriving (Eq, Show)

data TerrainStatsResponse = TerrainStatsResponse
  { terrainStatsChunkCount :: !Int
  , terrainStatsTotalTiles :: !Int
  , terrainStatsElevation :: !(Maybe TerrainScalarStats)
  , terrainStatsMoistureMean :: !(Maybe Float)
  , terrainStatsBiomeDistribution :: ![TerrainNamedCount]
  , terrainStatsFormDistribution :: ![TerrainNamedCount]
  , terrainStatsTemperature :: !(Maybe TerrainScalarStats)
  , terrainStatsPrecipitation :: !(Maybe TerrainScalarStats)
  , terrainStatsVegetation :: !(Maybe Value)
  , terrainStatsRiver :: !(Maybe Value)
  } deriving (Eq, Show)

data TerrainOverlaysRequest = TerrainOverlaysRequest
  deriving (Eq, Show)

data TerrainOverlayFieldSummary = TerrainOverlayFieldSummary
  { terrainOverlayFieldIndex :: !Int
  , terrainOverlayFieldName :: !Text
  , terrainOverlayFieldType :: !OverlayFieldType
  } deriving (Eq, Show)

data TerrainOverlaySummary = TerrainOverlaySummary
  { terrainOverlayName :: !Text
  , terrainOverlayFieldCount :: !Int
  , terrainOverlayFields :: ![TerrainOverlayFieldSummary]
  } deriving (Eq, Show)

data TerrainOverlaysResponse = TerrainOverlaysResponse
  { terrainOverlayCount :: !Int
  , terrainOverlays :: ![TerrainOverlaySummary]
  } deriving (Eq, Show)

data TerrainFilterOp = TerrainOpEq | TerrainOpNeq | TerrainOpGt | TerrainOpGte | TerrainOpLt | TerrainOpLte
  deriving (Eq, Show)

data TerrainFindFilter = TerrainFindFilter
  { terrainFindFilterField :: !Text
  , terrainFindFilterOp :: !TerrainFilterOp
  , terrainFindFilterValue :: !Value
  } deriving (Eq, Show)

data TerrainFindHexesRequest = TerrainFindHexesRequest
  { terrainFindFilters :: ![TerrainFindFilter]
  , terrainFindLimit :: !(Maybe Int)
  } deriving (Eq, Show)

data TerrainHexSearchMatch = TerrainHexSearchMatch
  { terrainMatchChunk :: !ChunkId
  , terrainMatchTileIndex :: !Int
  , terrainMatchElevation :: !(Maybe Float)
  , terrainMatchBiome :: !(Maybe Text)
  , terrainMatchForm :: !(Maybe Text)
  } deriving (Eq, Show)

data TerrainFindHexesResponse = TerrainFindHexesResponse
  { terrainFindMatches :: ![TerrainHexSearchMatch]
  , terrainFindCount :: !Int
  } deriving (Eq, Show)

data TerrainExportRequest = TerrainExportRequest
  { terrainExportChunks :: !(Maybe [ChunkId])
  , terrainExportFields :: !(Maybe [Text])
  } deriving (Eq, Show)

data TerrainExportResponse = TerrainExportResponse
  { terrainExportChunkCount :: !Int
  , terrainExportedFields :: ![Text]
  , terrainExportChunkData :: !(Map ChunkId (Map Text Value))
  } deriving (Eq, Show)

terrainServiceGroup :: ServiceGroupSpec
terrainServiceGroup = ServiceGroupSpec "terrain" terrainServiceOperationSpecs

terrainServiceOperationSpecs :: [ServiceOperationSpec]
terrainServiceOperationSpecs =
  [ typedServiceOperationSpec terrainGetHexOperation
  , typedServiceOperationSpec terrainGetChunksOperation
  , typedServiceOperationSpec terrainGetChunkSummaryOperation
  , typedServiceOperationSpec terrainGetStatsOperation
  , typedServiceOperationSpec terrainGetOverlaysOperation
  , typedServiceOperationSpec terrainFindHexesOperation
  , typedServiceOperationSpec terrainExportDataOperation
  ]

terrainGetHexOperation :: TypedServiceOperation TerrainGetHexRequest TerrainHexResponse
terrainGetHexOperation = typedOperation $
  operationSpec "terrain.hex.get" "get_hex" "Read terrain data for one hex."

terrainGetChunksOperation :: TypedServiceOperation TerrainGetChunksRequest TerrainChunksResponse
terrainGetChunksOperation = typedOperation $
  operationSpec "terrain.chunks.list" "get_chunks" "List loaded terrain chunks."

terrainGetChunkSummaryOperation :: TypedServiceOperation TerrainGetChunkSummaryRequest TerrainChunkSummaryResponse
terrainGetChunkSummaryOperation = typedOperation $
  operationSpec "terrain.chunk.summary" "get_chunk_summary" "Summarize one terrain chunk."

terrainGetStatsOperation :: TypedServiceOperation TerrainStatsRequest TerrainStatsResponse
terrainGetStatsOperation = typedOperation $
  operationSpec "terrain.stats" "get_terrain_stats" "Read aggregate terrain statistics."

terrainGetOverlaysOperation :: TypedServiceOperation TerrainOverlaysRequest TerrainOverlaysResponse
terrainGetOverlaysOperation = typedOperation $
  operationSpec "terrain.overlays" "get_overlays" "List loaded overlays and metadata."

terrainFindHexesOperation :: TypedServiceOperation TerrainFindHexesRequest TerrainFindHexesResponse
terrainFindHexesOperation = typedOperation $
  operationSpec "terrain.hex.find" "find_hexes" "Search terrain hexes by predicate."

terrainExportDataOperation :: TypedServiceOperation TerrainExportRequest TerrainExportResponse
terrainExportDataOperation = typedOperation $
  operationSpec "terrain.export" "export_terrain_data" "Export terrain data for external consumers."
