{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.State
  ( StateService(..)
  , StateGetRequest(..)
  , StateSummaryResponse(..)
  , StateHexCoord(..)
  , StateViewModesRequest(..)
  , StateViewModeSummary(..)
  , StateViewModesResponse(..)
  , StateUiStateRequest(..)
  , StateUiStateResponse(..)
  , StateUiViewSnapshot(..)
  , StateUiPanelsSnapshot(..)
  , StateUiLeftPanelSnapshot(..)
  , StateUiConfigPanelSnapshot(..)
  , StateUiLogPanelSnapshot(..)
  , StateUiEditorSnapshot(..)
  , StateUiDataBrowserSnapshot(..)
  , StateUiHexSelectionSnapshot(..)
  , StateUiSimulationSnapshot(..)
  , stateGetStateOperation
  , stateGetViewModesOperation
  , stateGetUiStateOperation
  , stateServiceGroup
  , stateServiceOperationSpecs
  ) where

import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.Types

data StateService = StateService
  { stateGetState :: !ServiceHandler
  , stateGetViewModes :: !ServiceHandler
  , stateGetUiState :: !ServiceHandler
  }

data StateGetRequest = StateGetRequest
  deriving (Eq, Show)

data StateHexCoord = StateHexCoord
  { stateHexQ :: !Int
  , stateHexR :: !Int
  } deriving (Eq, Show)

data StateSummaryResponse = StateSummaryResponse
  { stateSummarySeed :: !Word64
  , stateSummaryViewMode :: !Text
  , stateSummaryConfigTab :: !Text
  , stateSummaryGenerating :: !Bool
  , stateSummaryChunkSize :: !Int
  , stateSummaryShowConfig :: !Bool
  , stateSummaryWorldName :: !Text
  , stateSummaryContextHex :: !(Maybe StateHexCoord)
  } deriving (Eq, Show)

data StateViewModesRequest = StateViewModesRequest
  deriving (Eq, Show)

data StateViewModeSummary = StateViewModeSummary
  { stateViewModeName :: !Text
  , stateViewModeActive :: !Bool
  } deriving (Eq, Show)

newtype StateViewModesResponse = StateViewModesResponse
  { stateViewModes :: [StateViewModeSummary]
  } deriving (Eq, Show)

data StateUiStateRequest = StateUiStateRequest
  deriving (Eq, Show)

data StateUiViewSnapshot = StateUiViewSnapshot
  { stateUiViewMode :: !Text
  , stateUiViewOverlayName :: !(Maybe Text)
  , stateUiViewOverlayField :: !(Maybe Int)
  , stateUiViewOverlayNames :: ![Text]
  } deriving (Eq, Show)

data StateUiLeftPanelSnapshot = StateUiLeftPanelSnapshot
  { stateUiLeftPanelVisible :: !Bool
  , stateUiLeftPanelTab :: !Text
  } deriving (Eq, Show)

data StateUiConfigPanelSnapshot = StateUiConfigPanelSnapshot
  { stateUiConfigPanelVisible :: !Bool
  , stateUiConfigPanelTab :: !Text
  , stateUiConfigPanelScroll :: !Int
  } deriving (Eq, Show)

data StateUiLogPanelSnapshot = StateUiLogPanelSnapshot
  { stateUiLogPanelCollapsed :: !Bool
  , stateUiLogPanelLevel :: !Text
  } deriving (Eq, Show)

data StateUiPanelsSnapshot = StateUiPanelsSnapshot
  { stateUiPanelsLeft :: !StateUiLeftPanelSnapshot
  , stateUiPanelsConfig :: !StateUiConfigPanelSnapshot
  , stateUiPanelsLog :: !StateUiLogPanelSnapshot
  } deriving (Eq, Show)

data StateUiEditorSnapshot = StateUiEditorSnapshot
  { stateUiEditorActive :: !Bool
  , stateUiEditorTool :: !Text
  } deriving (Eq, Show)

data StateUiDataBrowserSnapshot = StateUiDataBrowserSnapshot
  { stateUiDataBrowserSelectedPlugin :: !(Maybe Text)
  , stateUiDataBrowserSelectedResource :: !(Maybe Text)
  , stateUiDataBrowserRecordCount :: !Int
  , stateUiDataBrowserTotalCount :: !(Maybe Int)
  , stateUiDataBrowserPageOffset :: !Int
  , stateUiDataBrowserLoading :: !Bool
  , stateUiDataBrowserEditMode :: !Bool
  , stateUiDataBrowserCreateMode :: !Bool
  , stateUiDataBrowserHasSelection :: !Bool
  } deriving (Eq, Show)

data StateUiHexSelectionSnapshot = StateUiHexSelectionSnapshot
  { stateUiHexSelectionContext :: !(Maybe StateHexCoord)
  , stateUiHexSelectionPinned :: !Bool
  , stateUiHexSelectionHover :: !(Maybe StateHexCoord)
  } deriving (Eq, Show)

data StateUiSimulationSnapshot = StateUiSimulationSnapshot
  { stateUiSimulationAutoTick :: !Bool
  , stateUiSimulationTickCount :: !Word64
  } deriving (Eq, Show)

data StateUiStateResponse = StateUiStateResponse
  { stateUiSeed :: !Word64
  , stateUiGenerating :: !Bool
  , stateUiWorldName :: !Text
  , stateUiChunkSize :: !Int
  , stateUiView :: !StateUiViewSnapshot
  , stateUiPanels :: !StateUiPanelsSnapshot
  , stateUiEditor :: !StateUiEditorSnapshot
  , stateUiDataBrowser :: !StateUiDataBrowserSnapshot
  , stateUiHexSelection :: !StateUiHexSelectionSnapshot
  , stateUiSimulation :: !StateUiSimulationSnapshot
  } deriving (Eq, Show)

stateServiceGroup :: ServiceGroupSpec
stateServiceGroup = ServiceGroupSpec "state" stateServiceOperationSpecs

stateServiceOperationSpecs :: [ServiceOperationSpec]
stateServiceOperationSpecs =
  [ typedServiceOperationSpec stateGetStateOperation
  , typedServiceOperationSpec stateGetViewModesOperation
  , typedServiceOperationSpec stateGetUiStateOperation
  ]

stateGetStateOperation :: TypedServiceOperation StateGetRequest StateSummaryResponse
stateGetStateOperation = typedOperation $
  operationSpec "state.get" "get_state" "Read current application state."

stateGetViewModesOperation :: TypedServiceOperation StateViewModesRequest StateViewModesResponse
stateGetViewModesOperation = typedOperation $
  operationSpec "state.viewModes" "get_view_modes" "List supported view modes and active selection."

stateGetUiStateOperation :: TypedServiceOperation StateUiStateRequest StateUiStateResponse
stateGetUiStateOperation = typedOperation $
  operationSpec "state.ui" "get_ui_state" "Read UI snapshot state."
