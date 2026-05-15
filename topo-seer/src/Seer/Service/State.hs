{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.State
  ( StateService(..)
  , stateServiceGroup
  , stateServiceOperationSpecs
  ) where

import Seer.Service.Types

data StateService = StateService
  { stateGetState :: !ServiceHandler
  , stateGetViewModes :: !ServiceHandler
  , stateGetUiState :: !ServiceHandler
  }

stateServiceGroup :: ServiceGroupSpec
stateServiceGroup = ServiceGroupSpec "state" stateServiceOperationSpecs

stateServiceOperationSpecs :: [ServiceOperationSpec]
stateServiceOperationSpecs =
  [ operationSpec "state.get" "get_state" "Read current application state."
  , operationSpec "state.viewModes" "get_view_modes" "List supported view modes and active selection."
  , operationSpec "state.ui" "get_ui_state" "Read UI snapshot state."
  ]
