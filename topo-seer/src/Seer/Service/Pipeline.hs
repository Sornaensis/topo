{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Pipeline
  ( PipelineService(..)
  , PipelineGetRequest(..)
  , PipelineGetResponse(..)
  , PipelineStageSource(..)
  , PipelineStageSummary(..)
  , PipelineSetStageEnabledRequest(..)
  , PipelineSetStageEnabledResponse(..)
  , pipelineGetOperation
  , pipelineSetStageEnabledOperation
  , pipelineServiceGroup
  , pipelineServiceOperationSpecs
  ) where

import Data.Text (Text)
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

data PipelineStageSummary = PipelineStageSummary
  { pipelineStageId :: !StageId
  , pipelineStageName :: !Text
  , pipelineStageSource :: !PipelineStageSource
  , pipelineStageEnabled :: !Bool
  } deriving (Eq, Show)

newtype PipelineGetResponse = PipelineGetResponse
  { pipelineStages :: [PipelineStageSummary]
  } deriving (Eq, Show)

data PipelineSetStageEnabledRequest = PipelineSetStageEnabledRequest
  { pipelineStageToSet :: !StageId
  , pipelineStageEnabledValue :: !Bool
  } deriving (Eq, Show)

data PipelineSetStageEnabledResponse = PipelineSetStageEnabledResponse
  { pipelineUpdatedStage :: !StageId
  , pipelineUpdatedStageEnabled :: !Bool
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
