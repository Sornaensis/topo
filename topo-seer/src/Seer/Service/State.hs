{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.State
  ( StateService(..)
  , StateGetRequest(..)
  , StateSummaryResponse(..)
  , StateHexCoord(..)
  , StateLayeredViewSnapshot(..)
  , StateViewsRequest(..)
  , StateViewChoice(..)
  , StateWeatherBasisSummary(..)
  , StateViewsResponse(..)
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
  , stateGetViewsOperation
  , stateGetUiStateOperation
  , stateServiceGroup
  , stateServiceOperationSpecs
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.Types

data StateService = StateService
  { stateGetState :: !(ServiceHandler StateGetRequest StateSummaryResponse)
  , stateGetViews :: !(ServiceHandler StateViewsRequest StateViewsResponse)
  , stateGetUiState :: !(ServiceHandler StateUiStateRequest StateUiStateResponse)
  }

data StateGetRequest = StateGetRequest
  deriving (Eq, Show)

data StateHexCoord = StateHexCoord
  { stateHexQ :: !Int
  , stateHexR :: !Int
  } deriving (Eq, Show)

data StateSummaryResponse = StateSummaryResponse
  { stateSummarySeed :: !Word64
  , stateSummaryView :: !StateLayeredViewSnapshot
  , stateSummaryConfigTab :: !Text
  , stateSummaryGenerating :: !Bool
  , stateSummaryChunkSize :: !Int
  , stateSummaryShowConfig :: !Bool
  , stateSummaryWorldName :: !Text
  , stateSummaryContextHex :: !(Maybe StateHexCoord)
  } deriving (Eq, Show)

data StateLayeredViewSnapshot = StateLayeredViewSnapshot
  { stateLayeredBaseMode :: !Text
  , stateLayeredOverlayMode :: !(Maybe Text)
  , stateLayeredPluginOverlay :: !(Maybe Text)
  , stateLayeredOverlayField :: !(Maybe Int)
  , stateLayeredWeatherBasis :: !Text
  , stateLayeredOverlayOpacity :: !Float
  , stateLayeredTemporalBasis :: !(Maybe Text)
  , stateLayeredSourceKind :: !(Maybe Text)
  } deriving (Eq, Show)

data StateViewsRequest = StateViewsRequest
  deriving (Eq, Show)

data StateViewChoice = StateViewChoice
  { stateViewChoiceName :: !Text
  , stateViewChoiceActive :: !Bool
  , stateViewChoiceLabel :: !Text
  , stateViewChoicePluginOverlay :: !(Maybe Text)
  , stateViewChoiceFieldIndex :: !(Maybe Int)
  , stateViewChoiceMetadata :: !Value
  } deriving (Eq, Show)

data StateWeatherBasisSummary = StateWeatherBasisSummary
  { stateWeatherBasisName :: !Text
  , stateWeatherBasisActive :: !Bool
  , stateWeatherBasisTemporalBasis :: !(Maybe Text)
  , stateWeatherBasisSourceKind :: !(Maybe Text)
  } deriving (Eq, Show)

data StateViewsResponse = StateViewsResponse
  { stateViewsCurrent :: !StateLayeredViewSnapshot
  , stateViewsBaseModes :: ![StateViewChoice]
  , stateViewsOverlayModes :: ![StateViewChoice]
  , stateViewsWeatherBases :: ![StateWeatherBasisSummary]
  , stateViewsOverlayNames :: ![Text]
  } deriving (Eq, Show)

data StateUiStateRequest = StateUiStateRequest
  deriving (Eq, Show)

data StateUiViewSnapshot = StateUiViewSnapshot
  { stateUiViewLayered :: !StateLayeredViewSnapshot
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
  , typedServiceOperationSpec stateGetViewsOperation
  , typedServiceOperationSpec stateGetUiStateOperation
  ]

stateGetStateOperation :: TypedServiceOperation StateGetRequest StateSummaryResponse
stateGetStateOperation = typedOperation $
  operationSpec "state.get" "get_state" "Read current application state."

stateGetViewsOperation :: TypedServiceOperation StateViewsRequest StateViewsResponse
stateGetViewsOperation = typedOperation $
  operationSpec "state.views" "get_views" "Read layered view selection and available base/overlay choices."

stateGetUiStateOperation :: TypedServiceOperation StateUiStateRequest StateUiStateResponse
stateGetUiStateOperation = typedOperation $
  operationSpec "state.ui" "get_ui_state" "Read UI snapshot state."
