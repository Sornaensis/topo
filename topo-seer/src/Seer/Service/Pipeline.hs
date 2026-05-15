{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Pipeline
  ( PipelineService(..)
  , pipelineServiceGroup
  , pipelineServiceOperationSpecs
  ) where

import Seer.Service.Types

data PipelineService = PipelineService
  { pipelineGet :: !ServiceHandler
  , pipelineSetStageEnabled :: !ServiceHandler
  }

pipelineServiceGroup :: ServiceGroupSpec
pipelineServiceGroup = ServiceGroupSpec "pipeline" pipelineServiceOperationSpecs

pipelineServiceOperationSpecs :: [ServiceOperationSpec]
pipelineServiceOperationSpecs =
  [ operationSpec "pipeline.get" "get_pipeline" "Read pipeline stage state and metadata."
  , operationSpec "pipeline.stage.setEnabled" "set_stage_enabled" "Enable or disable one pipeline stage."
  ]
