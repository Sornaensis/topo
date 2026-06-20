{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.World
  ( WorldService(..)
  , WorldGenerateRequest(..)
  , WorldGenerateResponse(..)
  , WorldMetaRequest(..)
  , WorldMetaResponse(..)
  , WorldGenerationStatusRequest(..)
  , WorldGenerationStatusResponse(..)
  , WorldListRequest(..)
  , WorldListResponse(..)
  , WorldSaveRequest(..)
  , WorldSaveResponse(..)
  , WorldLoadRequest(..)
  , WorldLoadResponse(..)
  , WorldSetNameRequest(..)
  , WorldSetNameResponse(..)
  , worldGenerateOperation
  , worldGetMetaOperation
  , worldGetGenerationStatusOperation
  , worldListOperation
  , worldSaveOperation
  , worldLoadOperation
  , worldSetNameOperation
  , worldServiceGroup
  , worldServiceOperationSpecs
  ) where

import Data.Text (Text)
import Data.Word (Word64)

import Seer.World.Persist.Types (WorldSaveManifest)
import Seer.Service.Types
import Topo.Types (ChunkId)

data WorldService = WorldService
  { worldGenerate :: !ServiceHandler
  , worldGetMeta :: !ServiceHandler
  , worldGetGenerationStatus :: !ServiceHandler
  , worldList :: !ServiceHandler
  , worldSave :: !ServiceHandler
  , worldLoad :: !ServiceHandler
  , worldSetName :: !ServiceHandler
  }

data WorldGenerateRequest = WorldGenerateRequest
  { worldGenerateSeedOverride :: !(Maybe Word64)
  } deriving (Eq, Show)

data WorldGenerateResponse = WorldGenerateResponse
  { worldGenerateAccepted :: !Bool
  , worldGenerateStatus :: !Text
  } deriving (Eq, Show)

data WorldMetaRequest = WorldMetaRequest
  deriving (Eq, Show)

data WorldMetaResponse = WorldMetaResponse
  { worldMetaSeed :: !Word64
  , worldMetaChunkSize :: !Int
  , worldMetaTilesPerChunk :: !Int
  , worldMetaChunkCount :: !Int
  , worldMetaTotalTiles :: !Int
  , worldMetaChunkIds :: ![ChunkId]
  , worldMetaOverlayNames :: ![Text]
  , worldMetaName :: !Text
  , worldMetaGenerating :: !Bool
  } deriving (Eq, Show)

data WorldGenerationStatusRequest = WorldGenerationStatusRequest
  deriving (Eq, Show)

data WorldGenerationStatusResponse = WorldGenerationStatusResponse
  { worldGenerationInProgress :: !Bool
  , worldGenerationChunkCount :: !Int
  , worldGenerationSeed :: !Word64
  , worldGenerationAsyncStatus :: !AsyncStatusSnapshot
  } deriving (Eq, Show)

data WorldListRequest = WorldListRequest
  deriving (Eq, Show)

data WorldListResponse = WorldListResponse
  { worldListCount :: !Int
  , worldListWorlds :: ![WorldSaveManifest]
  } deriving (Eq, Show)

newtype WorldSaveRequest = WorldSaveRequest
  { worldSaveName :: Text
  } deriving (Eq, Show)

data WorldSaveResponse = WorldSaveResponse
  { worldSavedName :: !Text
  , worldSaved :: !Bool
  } deriving (Eq, Show)

newtype WorldLoadRequest = WorldLoadRequest
  { worldLoadName :: Text
  } deriving (Eq, Show)

data WorldLoadResponse = WorldLoadResponse
  { worldLoadedName :: !Text
  , worldLoaded :: !Bool
  } deriving (Eq, Show)

newtype WorldSetNameRequest = WorldSetNameRequest
  { worldSetNameValue :: Text
  } deriving (Eq, Show)

newtype WorldSetNameResponse = WorldSetNameResponse
  { worldRenamedTo :: Text
  } deriving (Eq, Show)

worldServiceGroup :: ServiceGroupSpec
worldServiceGroup = ServiceGroupSpec "world" worldServiceOperationSpecs

worldServiceOperationSpecs :: [ServiceOperationSpec]
worldServiceOperationSpecs =
  [ typedServiceOperationSpec worldGenerateOperation
  , typedServiceOperationSpec worldGetMetaOperation
  , typedServiceOperationSpec worldGetGenerationStatusOperation
  , typedServiceOperationSpec worldListOperation
  , typedServiceOperationSpec worldSaveOperation
  , typedServiceOperationSpec worldLoadOperation
  , typedServiceOperationSpec worldSetNameOperation
  ]

worldGenerateOperation :: TypedServiceOperation WorldGenerateRequest WorldGenerateResponse
worldGenerateOperation = typedOperation $
  operationSpec "world.generate" "generate" "Start terrain/world generation."

worldGetMetaOperation :: TypedServiceOperation WorldMetaRequest WorldMetaResponse
worldGetMetaOperation = typedOperation $
  operationSpec "world.meta" "get_world_meta" "Read world metadata."

worldGetGenerationStatusOperation :: TypedServiceOperation WorldGenerationStatusRequest WorldGenerationStatusResponse
worldGetGenerationStatusOperation = typedOperation $
  operationSpec "world.generationStatus" "get_generation_status" "Read async generation status."

worldListOperation :: TypedServiceOperation WorldListRequest WorldListResponse
worldListOperation = typedOperation $
  operationSpec "world.list" "list_worlds" "List saved worlds."

worldSaveOperation :: TypedServiceOperation WorldSaveRequest WorldSaveResponse
worldSaveOperation = typedOperation $
  operationSpec "world.save" "save_world" "Save the current world."

worldLoadOperation :: TypedServiceOperation WorldLoadRequest WorldLoadResponse
worldLoadOperation = typedOperation $
  operationSpec "world.load" "load_world" "Load a saved world."

worldSetNameOperation :: TypedServiceOperation WorldSetNameRequest WorldSetNameResponse
worldSetNameOperation = typedOperation $
  operationSpec "world.setName" "set_world_name" "Rename the current world."
