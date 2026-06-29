{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Pipeline
  ( PipelineService(..)
  , PipelineGetRequest(..)
  , PipelineGetResponse(..)
  , PipelineStageSource(..)
  , PipelineStageDocSummary(..)
  , PipelineStageRunSummary(..)
  , PipelineStageSummary(..)
  , PipelineDagNode(..)
  , PipelineDagEdge(..)
  , PipelineDagSummary(..)
  , PipelineSetStageEnabledRequest(..)
  , PipelineSetStageEnabledResponse(..)
  , pipelineGetOperation
  , pipelineSetStageEnabledOperation
  , pipelineServiceGroup
  , pipelineServiceOperationSpecs
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Word (Word64)
import Topo.Pipeline.Stage (StageId)

import Seer.Service.Types

data PipelineService = PipelineService
  { pipelineGet :: !ServiceHandler
  , pipelineSetStageEnabled :: !ServiceHandler
  }

data PipelineGetRequest = PipelineGetRequest
  deriving (Eq, Show)

data PipelineStageSource
  = PipelineStageBuiltin
  | PipelineStagePlugin !Text
  deriving (Eq, Show)

data PipelineStageDocSummary = PipelineStageDocSummary
  { pipelineStageDocId :: !StageId
  , pipelineStageDocTitle :: !Text
  , pipelineStageDocSummary :: !Text
  , pipelineStageDocSeedTag :: !Text
  , pipelineStageDocDependencies :: ![StageId]
  , pipelineStageDocConfigKeys :: ![Text]
  , pipelineStageDocOutputFields :: ![Text]
  , pipelineStageDocOutputOverlays :: ![Text]
  , pipelineStageDocDiagnosticHints :: ![Text]
  } deriving (Eq, Show)

data PipelineStageRunSummary = PipelineStageRunSummary
  { pipelineStageRunStatus :: !Text
  , pipelineStageRunSeed :: !(Maybe Word64)
  , pipelineStageRunStage :: !StageId
  , pipelineStageRunSeedTag :: !Text
  , pipelineStageRunElapsedMs :: !(Maybe Int)
  , pipelineStageRunProvenance :: !Value
  } deriving (Eq, Show)

data PipelineStageSummary = PipelineStageSummary
  { pipelineStageId :: !StageId
  , pipelineStageName :: !Text
  , pipelineStageSource :: !PipelineStageSource
  , pipelineStageEnabled :: !Bool
  , pipelineStageStatus :: !Text
  , pipelineStageExplicitlyDisabled :: !Bool
  , pipelineStageAutoDisabled :: !Bool
  , pipelineStageDependencies :: ![StageId]
  , pipelineStageDependents :: ![StageId]
  , pipelineStageDisabledBy :: ![StageId]
  , pipelineStageConfig :: !Value
  , pipelineStageOutputFields :: ![Text]
  , pipelineStageOutputOverlays :: ![Text]
  , pipelineStageLastRun :: !PipelineStageRunSummary
  , pipelineStageProvenance :: !Value
  , pipelineStageDiagnostics :: ![Text]
  , pipelineStagePluginInsertion :: !(Maybe Value)
  , pipelineStagePluginDiagnostics :: !(Maybe Value)
  , pipelineStageDoc :: !(Maybe PipelineStageDocSummary)
  } deriving (Eq, Show)

data PipelineDagNode = PipelineDagNode
  { pipelineDagNodeId :: !StageId
  , pipelineDagNodeSource :: !PipelineStageSource
  , pipelineDagNodeEnabled :: !Bool
  , pipelineDagNodeStatus :: !Text
  } deriving (Eq, Show)

data PipelineDagEdge = PipelineDagEdge
  { pipelineDagEdgeFrom :: !Text
  , pipelineDagEdgeTo :: !Text
  , pipelineDagEdgeKind :: !Text
  } deriving (Eq, Show)

data PipelineDagSummary = PipelineDagSummary
  { pipelineDagNodes :: ![PipelineDagNode]
  , pipelineDagEdges :: ![PipelineDagEdge]
  } deriving (Eq, Show)

data PipelineGetResponse = PipelineGetResponse
  { pipelineStages :: ![PipelineStageSummary]
  , pipelineDag :: !PipelineDagSummary
  , pipelineDocs :: ![PipelineStageDocSummary]
  , pipelineDiagnostics :: !Value
  } deriving (Eq, Show)

data PipelineSetStageEnabledRequest = PipelineSetStageEnabledRequest
  { pipelineStageToSet :: !StageId
  , pipelineStageEnabledValue :: !Bool
  } deriving (Eq, Show)

data PipelineSetStageEnabledResponse = PipelineSetStageEnabledResponse
  { pipelineUpdatedStage :: !StageId
  , pipelineUpdatedStageEnabled :: !Bool
  , pipelineUpdatedDisabledClosure :: ![StageId]
  , pipelineUpdatedDiagnostics :: ![Text]
  } deriving (Eq, Show)

pipelineServiceGroup :: ServiceGroupSpec
pipelineServiceGroup = ServiceGroupSpec "pipeline" pipelineServiceOperationSpecs

pipelineServiceOperationSpecs :: [ServiceOperationSpec]
pipelineServiceOperationSpecs =
  [ typedServiceOperationSpec pipelineGetOperation
  , typedServiceOperationSpec pipelineSetStageEnabledOperation
  ]

pipelineGetOperation :: TypedServiceOperation PipelineGetRequest PipelineGetResponse
pipelineGetOperation = typedOperation $
  operationSpec "pipeline.get" "get_pipeline" "Read pipeline stage state and metadata."

pipelineSetStageEnabledOperation :: TypedServiceOperation PipelineSetStageEnabledRequest PipelineSetStageEnabledResponse
pipelineSetStageEnabledOperation = typedOperation $
  operationSpec "pipeline.stage.setEnabled" "set_stage_enabled" "Enable or disable one pipeline stage."
