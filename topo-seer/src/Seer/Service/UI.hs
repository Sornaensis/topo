{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.UI
  ( UiService(..)
  , UiHexCoord(..)
  , UiScreenPoint(..)
  , UiSetSeedRequest(..)
  , UiSetSeedResponse(..)
  , UiSetViewRequest(..)
  , UiSetViewResponse(..)
  , UiSetConfigTabRequest(..)
  , UiSetConfigTabResponse(..)
  , UiSelectHexRequest(..)
  , UiSelectHexResponse(..)
  , UiSetOverlayRequest(..)
  , UiSetOverlayResponse(..)
  , UiListOverlayFieldsRequest(..)
  , UiListOverlayFieldsResponse(..)
  , UiOverlayFieldSummary(..)
  , UiCycleOverlayRequest(..)
  , UiCycleOverlayResponse(..)
  , UiCycleOverlayFieldRequest(..)
  , UiCycleOverlayFieldResponse(..)
  , UiSetCameraRequest(..)
  , UiSetCameraResponse(..)
  , UiGetCameraRequest(..)
  , UiCameraSnapshot(..)
  , UiZoomToChunkRequest(..)
  , UiZoomToChunkResponse(..)
  , UiSetLeftPanelRequest(..)
  , UiSetLeftPanelResponse(..)
  , UiSetLeftTabRequest(..)
  , UiSetLeftTabResponse(..)
  , UiToggleConfigPanelRequest(..)
  , UiToggleConfigPanelResponse(..)
  , UiSetLogCollapsedRequest(..)
  , UiSetLogCollapsedResponse(..)
  , UiSetLogLevelRequest(..)
  , UiSetLogLevelResponse(..)
  , UiGetPanelsRequest(..)
  , UiPanelTabState(..)
  , UiLogPanelState(..)
  , UiPanelsResponse(..)
  , UiViewportScrollRequest(..)
  , UiViewportScrollResponse(..)
  , UiViewportClickRequest(..)
  , UiViewportClickResponse(..)
  , UiViewportDragRequest(..)
  , UiViewportDragResponse(..)
  , UiViewportHoverRequest(..)
  , UiViewportHoverResponse(..)
  , UiClickWidgetRequest(..)
  , UiClickWidgetResponse(..)
  , UiListWidgetsRequest(..)
  , UiWidgetGroup(..)
  , UiWidgetArgument(..)
  , UiWidgetCapability(..)
  , UiListWidgetsResponse(..)
  , UiGetWidgetStateRequest(..)
  , UiWidgetStateResponse(..)
  , UiGetDialogStateRequest(..)
  , UiDialogStateResponse(..)
  , UiSetDialogTextRequest(..)
  , UiSetDialogTextResponse(..)
  , UiDialogConfirmRequest(..)
  , UiDialogCancelRequest(..)
  , UiDialogActionResponse(..)
  , UiSendKeyRequest(..)
  , UiSendKeyResponse(..)
  , uiSetSeedOperation
  , uiSetViewOperation
  , uiSetConfigTabOperation
  , uiSelectHexOperation
  , uiSetOverlayOperation
  , uiListOverlayFieldsOperation
  , uiCycleOverlayOperation
  , uiCycleOverlayFieldOperation
  , uiSetCameraOperation
  , uiGetCameraOperation
  , uiZoomToChunkOperation
  , uiSetLeftPanelOperation
  , uiSetLeftTabOperation
  , uiToggleConfigPanelOperation
  , uiSetLogCollapsedOperation
  , uiSetLogLevelOperation
  , uiGetPanelsOperation
  , uiViewportScrollOperation
  , uiViewportClickOperation
  , uiViewportDragOperation
  , uiViewportHoverOperation
  , uiClickWidgetOperation
  , uiListWidgetsOperation
  , uiGetWidgetStateOperation
  , uiGetDialogStateOperation
  , uiSetDialogTextOperation
  , uiDialogConfirmOperation
  , uiDialogCancelOperation
  , uiSendKeyOperation
  , uiServiceGroup
  , uiServiceOperationSpecs
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.Types
import Topo.Overlay.Schema (OverlayFieldType)
import Topo.Types (ChunkId)

data UiService = UiService
  { uiSetSeed :: !(ServiceHandler UiSetSeedRequest UiSetSeedResponse)
  , uiSetView :: !(ServiceHandler UiSetViewRequest UiSetViewResponse)
  , uiSetConfigTab :: !(ServiceHandler UiSetConfigTabRequest UiSetConfigTabResponse)
  , uiSelectHex :: !(ServiceHandler UiSelectHexRequest UiSelectHexResponse)
  , uiSetOverlay :: !(ServiceHandler UiSetOverlayRequest UiSetOverlayResponse)
  , uiListOverlayFields :: !(ServiceHandler UiListOverlayFieldsRequest UiListOverlayFieldsResponse)
  , uiCycleOverlay :: !(ServiceHandler UiCycleOverlayRequest UiCycleOverlayResponse)
  , uiCycleOverlayField :: !(ServiceHandler UiCycleOverlayFieldRequest UiCycleOverlayFieldResponse)
  , uiSetCamera :: !(ServiceHandler UiSetCameraRequest UiSetCameraResponse)
  , uiGetCamera :: !(ServiceHandler UiGetCameraRequest UiCameraSnapshot)
  , uiZoomToChunk :: !(ServiceHandler UiZoomToChunkRequest UiZoomToChunkResponse)
  , uiSetLeftPanel :: !(ServiceHandler UiSetLeftPanelRequest UiSetLeftPanelResponse)
  , uiSetLeftTab :: !(ServiceHandler UiSetLeftTabRequest UiSetLeftTabResponse)
  , uiToggleConfigPanel :: !(ServiceHandler UiToggleConfigPanelRequest UiToggleConfigPanelResponse)
  , uiSetLogCollapsed :: !(ServiceHandler UiSetLogCollapsedRequest UiSetLogCollapsedResponse)
  , uiSetLogLevel :: !(ServiceHandler UiSetLogLevelRequest UiSetLogLevelResponse)
  , uiGetPanels :: !(ServiceHandler UiGetPanelsRequest UiPanelsResponse)
  , uiViewportScroll :: !(ServiceHandler UiViewportScrollRequest UiViewportScrollResponse)
  , uiViewportClick :: !(ServiceHandler UiViewportClickRequest UiViewportClickResponse)
  , uiViewportDrag :: !(ServiceHandler UiViewportDragRequest UiViewportDragResponse)
  , uiViewportHover :: !(ServiceHandler UiViewportHoverRequest UiViewportHoverResponse)
  , uiClickWidget :: !(ServiceHandler UiClickWidgetRequest UiClickWidgetResponse)
  , uiListWidgets :: !(ServiceHandler UiListWidgetsRequest UiListWidgetsResponse)
  , uiGetWidgetState :: !(ServiceHandler UiGetWidgetStateRequest UiWidgetStateResponse)
  , uiGetDialogState :: !(ServiceHandler UiGetDialogStateRequest UiDialogStateResponse)
  , uiSetDialogText :: !(ServiceHandler UiSetDialogTextRequest UiSetDialogTextResponse)
  , uiDialogConfirm :: !(ServiceHandler UiDialogConfirmRequest UiDialogActionResponse)
  , uiDialogCancel :: !(ServiceHandler UiDialogCancelRequest UiDialogActionResponse)
  , uiSendKey :: !(ServiceHandler UiSendKeyRequest UiSendKeyResponse)
  }

data UiHexCoord = UiHexCoord
  { uiHexCoordQ :: !Int
  , uiHexCoordR :: !Int
  } deriving (Eq, Show)

data UiScreenPoint = UiScreenPoint
  { uiScreenPointX :: !Int
  , uiScreenPointY :: !Int
  } deriving (Eq, Show)

newtype UiSetSeedRequest = UiSetSeedRequest
  { uiSetSeedRequestValue :: Word64
  } deriving (Eq, Show)

newtype UiSetSeedResponse = UiSetSeedResponse
  { uiSetSeedResponseValue :: Word64
  } deriving (Eq, Show)

data UiSetViewRequest = UiSetViewRequest
  { uiSetViewRequestBaseMode :: !(Maybe Text)
  , uiSetViewRequestOverlayMode :: !(Maybe Text)
  , uiSetViewRequestPluginOverlay :: !(Maybe Text)
  , uiSetViewRequestWeatherBasis :: !(Maybe Text)
  , uiSetViewRequestOverlayOpacity :: !(Maybe Float)
  , uiSetViewRequestFieldIndex :: !(Maybe Int)
  } deriving (Eq, Show)

newtype UiSetViewResponse = UiSetViewResponse
  { uiSetViewResponseView :: Value
  } deriving (Eq, Show)

newtype UiSetConfigTabRequest = UiSetConfigTabRequest
  { uiSetConfigTabRequestName :: Text
  } deriving (Eq, Show)

newtype UiSetConfigTabResponse = UiSetConfigTabResponse
  { uiSetConfigTabResponseName :: Text
  } deriving (Eq, Show)

newtype UiSelectHexRequest = UiSelectHexRequest
  { uiSelectHexRequestCoord :: Maybe UiHexCoord
  } deriving (Eq, Show)

data UiSelectHexResponse = UiSelectHexResponse
  { uiSelectHexResponseSelected :: !Bool
  , uiSelectHexResponseCoord :: !(Maybe UiHexCoord)
  } deriving (Eq, Show)

data UiSetOverlayRequest = UiSetOverlayRequest
  { uiSetOverlayName :: !Text
  , uiSetOverlayFieldIndex :: !(Maybe Int)
  } deriving (Eq, Show)

data UiSetOverlayResponse = UiSetOverlayResponse
  { uiSelectedOverlayName :: !Text
  , uiSelectedOverlayFieldIndex :: !Int
  , uiSelectedOverlayView :: !Value
  } deriving (Eq, Show)

newtype UiListOverlayFieldsRequest = UiListOverlayFieldsRequest
  { uiListOverlayFieldsName :: Maybe Text
  } deriving (Eq, Show)

data UiOverlayFieldSummary = UiOverlayFieldSummary
  { uiOverlayFieldIndex :: !Int
  , uiOverlayFieldName :: !Text
  , uiOverlayFieldType :: !OverlayFieldType
  } deriving (Eq, Show)

data UiListOverlayFieldsResponse = UiListOverlayFieldsResponse
  { uiOverlayFieldCount :: !Int
  , uiOverlayFields :: ![UiOverlayFieldSummary]
  } deriving (Eq, Show)

newtype UiCycleOverlayRequest = UiCycleOverlayRequest
  { uiCycleOverlayDirection :: Int
  } deriving (Eq, Show)

data UiCycleOverlayResponse = UiCycleOverlayResponse
  { uiCycleOverlaySelectedName :: !(Maybe Text)
  , uiCycleOverlayView :: !Value
  } deriving (Eq, Show)

newtype UiCycleOverlayFieldRequest = UiCycleOverlayFieldRequest
  { uiCycleOverlayFieldDirection :: Int
  } deriving (Eq, Show)

data UiCycleOverlayFieldResponse = UiCycleOverlayFieldResponse
  { uiCycleOverlayFieldOverlay :: !Text
  , uiCycleOverlayFieldIndex :: !Int
  , uiCycleOverlayFieldName :: !Text
  , uiCycleOverlayFieldType :: !OverlayFieldType
  } deriving (Eq, Show)

data UiSetCameraRequest = UiSetCameraRequest
  { uiSetCameraRequestX :: !Float
  , uiSetCameraRequestY :: !Float
  , uiSetCameraRequestZoom :: !(Maybe Float)
  } deriving (Eq, Show)

data UiSetCameraResponse = UiSetCameraResponse
  { uiSetCameraResponseX :: !Float
  , uiSetCameraResponseY :: !Float
  , uiSetCameraResponseZoom :: !(Maybe Float)
  } deriving (Eq, Show)

data UiGetCameraRequest = UiGetCameraRequest
  deriving (Eq, Show)

data UiCameraSnapshot = UiCameraSnapshot
  { uiCameraSnapshotX :: !Float
  , uiCameraSnapshotY :: !Float
  , uiCameraSnapshotZoom :: !Float
  } deriving (Eq, Show)

newtype UiZoomToChunkRequest = UiZoomToChunkRequest
  { uiZoomToChunkRequestChunk :: ChunkId
  } deriving (Eq, Show)

data UiZoomToChunkResponse = UiZoomToChunkResponse
  { uiZoomToChunkResponseChunk :: !ChunkId
  , uiZoomToChunkResponseCamera :: !UiCameraSnapshot
  } deriving (Eq, Show)

newtype UiSetLeftPanelRequest = UiSetLeftPanelRequest
  { uiSetLeftPanelRequestVisible :: Bool
  } deriving (Eq, Show)

newtype UiSetLeftPanelResponse = UiSetLeftPanelResponse
  { uiSetLeftPanelResponseVisible :: Bool
  } deriving (Eq, Show)

newtype UiSetLeftTabRequest = UiSetLeftTabRequest
  { uiSetLeftTabRequestName :: Text
  } deriving (Eq, Show)

newtype UiSetLeftTabResponse = UiSetLeftTabResponse
  { uiSetLeftTabResponseName :: Text
  } deriving (Eq, Show)

newtype UiToggleConfigPanelRequest = UiToggleConfigPanelRequest
  { uiToggleConfigPanelRequestVisible :: Maybe Bool
  } deriving (Eq, Show)

newtype UiToggleConfigPanelResponse = UiToggleConfigPanelResponse
  { uiToggleConfigPanelResponseVisible :: Bool
  } deriving (Eq, Show)

newtype UiSetLogCollapsedRequest = UiSetLogCollapsedRequest
  { uiSetLogCollapsedRequestValue :: Bool
  } deriving (Eq, Show)

newtype UiSetLogCollapsedResponse = UiSetLogCollapsedResponse
  { uiSetLogCollapsedResponseValue :: Bool
  } deriving (Eq, Show)

newtype UiSetLogLevelRequest = UiSetLogLevelRequest
  { uiSetLogLevelRequestName :: Text
  } deriving (Eq, Show)

newtype UiSetLogLevelResponse = UiSetLogLevelResponse
  { uiSetLogLevelResponseName :: Text
  } deriving (Eq, Show)

data UiGetPanelsRequest = UiGetPanelsRequest
  deriving (Eq, Show)

data UiPanelTabState = UiPanelTabState
  { uiPanelTabVisible :: !Bool
  , uiPanelTabName :: !Text
  } deriving (Eq, Show)

data UiLogPanelState = UiLogPanelState
  { uiLogPanelCollapsed :: !Bool
  , uiLogPanelLevel :: !Text
  } deriving (Eq, Show)

data UiPanelsResponse = UiPanelsResponse
  { uiPanelsLeftPanel :: !UiPanelTabState
  , uiPanelsConfigPanel :: !UiPanelTabState
  , uiPanelsLogPanel :: !UiLogPanelState
  } deriving (Eq, Show)

data UiViewportScrollRequest = UiViewportScrollRequest
  { uiViewportScrollDelta :: !Int
  , uiViewportScrollPoint :: !(Maybe UiScreenPoint)
  } deriving (Eq, Show)

data UiViewportScrollResponse = UiViewportScrollResponse
  { uiViewportScrollZoom :: !Float
  , uiViewportScrollPanX :: !Float
  , uiViewportScrollPanY :: !Float
  , uiViewportScrollSteps :: !Int
  } deriving (Eq, Show)

data UiViewportClickRequest = UiViewportClickRequest
  { uiViewportClickPoint :: !UiScreenPoint
  , uiViewportClickButton :: !(Maybe Text)
  } deriving (Eq, Show)

data UiViewportClickResponse = UiViewportClickResponse
  { uiViewportClickButtonName :: !Text
  , uiViewportClickHex :: !(Maybe UiHexCoord)
  , uiViewportClickSelected :: !(Maybe Bool)
  , uiViewportClickEditorStroke :: !(Maybe Bool)
  , uiViewportClickTooltipPinned :: !(Maybe Bool)
  , uiViewportClickReason :: !(Maybe Text)
  } deriving (Eq, Show)

data UiViewportDragRequest = UiViewportDragRequest
  { uiViewportDragFrom :: !UiScreenPoint
  , uiViewportDragTo :: !UiScreenPoint
  } deriving (Eq, Show)

data UiViewportDragResponse = UiViewportDragResponse
  { uiViewportDragPanX :: !Float
  , uiViewportDragPanY :: !Float
  , uiViewportDragDeltaX :: !Int
  , uiViewportDragDeltaY :: !Int
  } deriving (Eq, Show)

newtype UiViewportHoverRequest = UiViewportHoverRequest
  { uiViewportHoverPoint :: UiScreenPoint
  } deriving (Eq, Show)

data UiViewportHoverResponse = UiViewportHoverResponse
  { uiViewportHoverHex :: !UiHexCoord
  , uiViewportHoverValid :: !Bool
  } deriving (Eq, Show)

data UiClickWidgetRequest = UiClickWidgetRequest
  { uiClickWidgetRequestId :: !Text
  , uiClickWidgetNormalizedPosition :: !(Maybe Double)
  , uiClickWidgetItemIndex :: !(Maybe Int)
  } deriving (Eq, Show)

data UiClickWidgetResponse = UiClickWidgetResponse
  { uiClickWidgetResponseId :: !Text
  , uiClickWidgetStatus :: !Text
  , uiClickWidgetInfo :: !(Maybe Text)
  , uiClickWidgetChanged :: !(Maybe Bool)
  , uiClickWidgetRequestIdAccepted :: !(Maybe Word64)
  , uiClickWidgetAcceptedOperation :: !(Maybe Text)
  } deriving (Eq, Show)

data UiListWidgetsRequest = UiListWidgetsRequest
  deriving (Eq, Show)

data UiWidgetGroup = UiWidgetGroup
  { uiWidgetGroupName :: !Text
  , uiWidgetGroupIds :: ![Text]
  } deriving (Eq, Show)

data UiWidgetArgument = UiWidgetArgument
  { uiWidgetArgumentName :: !Text
  , uiWidgetArgumentType :: !Text
  , uiWidgetArgumentMinimum :: !Int
  , uiWidgetArgumentMaximum :: !(Maybe Int)
  , uiWidgetArgumentDescription :: !Text
  } deriving (Eq, Show)

data UiWidgetCapability = UiWidgetCapability
  { uiWidgetCapabilityId :: !Text
  , uiWidgetCapabilityComponent :: !Text
  , uiWidgetCapabilityCategory :: !Text
  , uiWidgetCapabilityActive :: !(Maybe Bool)
  , uiWidgetCapabilityVisible :: !Bool
  , uiWidgetCapabilityEnabled :: !Bool
  , uiWidgetCapabilityPreconditions :: ![Text]
  , uiWidgetCapabilitySupport :: !Text
  , uiWidgetCapabilityRequiredArgument :: !(Maybe UiWidgetArgument)
  , uiWidgetCapabilityAlternative :: !(Maybe Text)
  } deriving (Eq, Show)

data UiListWidgetsResponse = UiListWidgetsResponse
  { uiWidgetIds :: ![Text]
  , uiWidgetCount :: !Int
  , uiWidgetGroups :: ![UiWidgetGroup]
  , uiWidgetCapabilities :: ![UiWidgetCapability]
  , uiWidgetDataBrowserState :: !(Maybe Value)
  } deriving (Eq, Show)

newtype UiGetWidgetStateRequest = UiGetWidgetStateRequest
  { uiGetWidgetStateRequestId :: Text
  } deriving (Eq, Show)

data UiWidgetStateResponse = UiWidgetStateResponse
  { uiWidgetStateId :: !Text
  , uiWidgetStateComponent :: !(Maybe Text)
  , uiWidgetStateCategory :: !(Maybe Text)
  , uiWidgetStateVisible :: !(Maybe Bool)
  , uiWidgetStateActive :: !(Maybe Bool)
  , uiWidgetStateEnabled :: !(Maybe Bool)
  , uiWidgetStatePreconditions :: ![Text]
  , uiWidgetStateSupport :: !(Maybe Text)
  , uiWidgetStateRequiredArgument :: !(Maybe UiWidgetArgument)
  , uiWidgetStateAlternative :: !(Maybe Text)
  , uiWidgetStateExpanded :: !(Maybe Bool)
  , uiWidgetStateEditMode :: !(Maybe Bool)
  , uiWidgetStateConfirmShown :: !(Maybe Bool)
  , uiWidgetStateLoading :: !(Maybe Bool)
  , uiWidgetStatePending :: !(Maybe Value)
  , uiWidgetStateAsyncError :: !(Maybe Value)
  , uiWidgetStateExtra :: !(Maybe Value)
  } deriving (Eq, Show)

data UiGetDialogStateRequest = UiGetDialogStateRequest
  deriving (Eq, Show)

data UiDialogStateResponse = UiDialogStateResponse
  { uiDialogMenuMode :: !Text
  , uiDialogSeedEditing :: !Bool
  , uiDialogPresetInput :: !Text
  , uiDialogPresetFilter :: !Text
  , uiDialogPresetSelected :: !Int
  , uiDialogPresetCount :: !Int
  , uiDialogWorldSaveInput :: !Text
  , uiDialogWorldFilter :: !Text
  , uiDialogWorldSelected :: !Int
  , uiDialogWorldCount :: !Int
  , uiDialogDataFocusedField :: !(Maybe Text)
  , uiDialogDataEditMode :: !Bool
  , uiDialogDataCreateMode :: !Bool
  , uiDialogDataTextCursor :: !Int
  } deriving (Eq, Show)

data UiSetDialogTextRequest = UiSetDialogTextRequest
  { uiSetDialogTextValue :: !Text
  , uiSetDialogTextTarget :: !(Maybe Text)
  } deriving (Eq, Show)

data UiSetDialogTextResponse = UiSetDialogTextResponse
  { uiSetDialogTextResponseTarget :: !Text
  , uiSetDialogTextResponseText :: !Text
  , uiSetDialogTextResponseField :: !(Maybe Text)
  } deriving (Eq, Show)

data UiDialogConfirmRequest = UiDialogConfirmRequest
  deriving (Eq, Show)

data UiDialogCancelRequest = UiDialogCancelRequest
  deriving (Eq, Show)

data UiDialogActionResponse = UiDialogActionResponse
  { uiDialogActionName :: !Text
  , uiDialogActionMenuMode :: !(Maybe Text)
  , uiDialogActionNameValue :: !(Maybe Text)
  , uiDialogActionSelectedIndex :: !(Maybe Int)
  } deriving (Eq, Show)

data UiSendKeyRequest = UiSendKeyRequest
  { uiSendKeyName :: !Text
  , uiSendKeyModifiers :: ![Text]
  } deriving (Eq, Show)

data UiSendKeyResponse = UiSendKeyResponse
  { uiSendKeyResponseKey :: !(Maybe Text)
  , uiSendKeyResponseSelected :: !(Maybe Int)
  , uiSendKeyResponseText :: !(Maybe Text)
  , uiSendKeyResponseFilter :: !(Maybe Text)
  , uiSendKeyResponseCursor :: !(Maybe Int)
  , uiSendKeyResponseField :: !(Maybe Text)
  , uiSendKeyResponseAction :: !(Maybe Text)
  , uiSendKeyResponseMenuMode :: !(Maybe Text)
  , uiSendKeyResponseName :: !(Maybe Text)
  , uiSendKeyResponseModifiers :: ![Text]
  , uiSendKeyResponseOutcome :: !Text
  } deriving (Eq, Show)

uiServiceGroup :: ServiceGroupSpec
uiServiceGroup = ServiceGroupSpec "ui" uiServiceOperationSpecs

uiServiceOperationSpecs :: [ServiceOperationSpec]
uiServiceOperationSpecs =
  [ typedServiceOperationSpec uiSetSeedOperation
  , typedServiceOperationSpec uiSetViewOperation
  , typedServiceOperationSpec uiSetConfigTabOperation
  , typedServiceOperationSpec uiSelectHexOperation
  , typedServiceOperationSpec uiSetOverlayOperation
  , typedServiceOperationSpec uiListOverlayFieldsOperation
  , typedServiceOperationSpec uiCycleOverlayOperation
  , typedServiceOperationSpec uiCycleOverlayFieldOperation
  , typedServiceOperationSpec uiSetCameraOperation
  , typedServiceOperationSpec uiGetCameraOperation
  , typedServiceOperationSpec uiZoomToChunkOperation
  , typedServiceOperationSpec uiSetLeftPanelOperation
  , typedServiceOperationSpec uiSetLeftTabOperation
  , typedServiceOperationSpec uiToggleConfigPanelOperation
  , typedServiceOperationSpec uiSetLogCollapsedOperation
  , typedServiceOperationSpec uiSetLogLevelOperation
  , typedServiceOperationSpec uiGetPanelsOperation
  , typedServiceOperationSpec uiViewportScrollOperation
  , typedServiceOperationSpec uiViewportClickOperation
  , typedServiceOperationSpec uiViewportDragOperation
  , typedServiceOperationSpec uiViewportHoverOperation
  , typedServiceOperationSpec uiClickWidgetOperation
  , typedServiceOperationSpec uiListWidgetsOperation
  , typedServiceOperationSpec uiGetWidgetStateOperation
  , typedServiceOperationSpec uiGetDialogStateOperation
  , typedServiceOperationSpec uiSetDialogTextOperation
  , typedServiceOperationSpec uiDialogConfirmOperation
  , typedServiceOperationSpec uiDialogCancelOperation
  , typedServiceOperationSpec uiSendKeyOperation
  ]

uiSetSeedOperation :: TypedServiceOperation UiSetSeedRequest UiSetSeedResponse
uiSetSeedOperation = typedOperation $
  operationSpec "ui.seed.set" "set_seed" "Set generation seed text."

uiSetViewOperation :: TypedServiceOperation UiSetViewRequest UiSetViewResponse
uiSetViewOperation = typedOperation $
  operationSpec "ui.view.set" "set_view" "Set active layered terrain view selection."

uiSetConfigTabOperation :: TypedServiceOperation UiSetConfigTabRequest UiSetConfigTabResponse
uiSetConfigTabOperation = typedOperation $
  operationSpec "ui.configTab.set" "set_config_tab" "Set active config tab."

uiSelectHexOperation :: TypedServiceOperation UiSelectHexRequest UiSelectHexResponse
uiSelectHexOperation = typedOperation $
  operationSpec "ui.hex.select" "select_hex" "Select one hex in the viewport."

uiSetOverlayOperation :: TypedServiceOperation UiSetOverlayRequest UiSetOverlayResponse
uiSetOverlayOperation = typedOperation $
  operationSpec "ui.overlay.set" "set_overlay" "Select active overlay."

uiListOverlayFieldsOperation :: TypedServiceOperation UiListOverlayFieldsRequest UiListOverlayFieldsResponse
uiListOverlayFieldsOperation = typedOperation $
  operationSpec "ui.overlay.fields" "list_overlay_fields" "List fields for the active overlay."

uiCycleOverlayOperation :: TypedServiceOperation UiCycleOverlayRequest UiCycleOverlayResponse
uiCycleOverlayOperation = typedOperation $
  operationSpec "ui.overlay.cycle" "cycle_overlay" "Cycle active overlay."

uiCycleOverlayFieldOperation :: TypedServiceOperation UiCycleOverlayFieldRequest UiCycleOverlayFieldResponse
uiCycleOverlayFieldOperation = typedOperation $
  operationSpec "ui.overlay.field.cycle" "cycle_overlay_field" "Cycle active overlay field."

uiSetCameraOperation :: TypedServiceOperation UiSetCameraRequest UiSetCameraResponse
uiSetCameraOperation = typedOperation $
  operationSpec "ui.camera.set" "set_camera" "Set viewport camera."

uiGetCameraOperation :: TypedServiceOperation UiGetCameraRequest UiCameraSnapshot
uiGetCameraOperation = typedOperation $
  operationSpec "ui.camera.get" "get_camera" "Read viewport camera."

uiZoomToChunkOperation :: TypedServiceOperation UiZoomToChunkRequest UiZoomToChunkResponse
uiZoomToChunkOperation = typedOperation $
  operationSpec "ui.camera.zoomToChunk" "zoom_to_chunk" "Move camera to a chunk."

uiSetLeftPanelOperation :: TypedServiceOperation UiSetLeftPanelRequest UiSetLeftPanelResponse
uiSetLeftPanelOperation = typedOperation $
  operationSpec "ui.panels.left.set" "set_left_panel" "Show or hide the left panel."

uiSetLeftTabOperation :: TypedServiceOperation UiSetLeftTabRequest UiSetLeftTabResponse
uiSetLeftTabOperation = typedOperation $
  operationSpec "ui.tabs.left.set" "set_left_tab" "Set the left-panel tab."

uiToggleConfigPanelOperation :: TypedServiceOperation UiToggleConfigPanelRequest UiToggleConfigPanelResponse
uiToggleConfigPanelOperation = typedOperation $
  operationSpec "ui.panels.config.toggle" "toggle_config_panel" "Toggle config panel visibility."

uiSetLogCollapsedOperation :: TypedServiceOperation UiSetLogCollapsedRequest UiSetLogCollapsedResponse
uiSetLogCollapsedOperation = typedOperation $
  operationSpec "ui.logs.collapsed.set" "set_log_collapsed" "Collapse or expand the log panel."

uiSetLogLevelOperation :: TypedServiceOperation UiSetLogLevelRequest UiSetLogLevelResponse
uiSetLogLevelOperation = typedOperation $
  operationSpec "ui.logs.level.set" "set_log_level" "Set visible log level."

uiGetPanelsOperation :: TypedServiceOperation UiGetPanelsRequest UiPanelsResponse
uiGetPanelsOperation = typedOperation $
  operationSpec "ui.panels.get" "get_ui_panels" "Read panel and tab state."

uiViewportScrollOperation :: TypedServiceOperation UiViewportScrollRequest UiViewportScrollResponse
uiViewportScrollOperation = typedOperation $
  operationSpec "ui.viewport.scroll" "viewport_scroll" "Apply viewport scroll input."

uiViewportClickOperation :: TypedServiceOperation UiViewportClickRequest UiViewportClickResponse
uiViewportClickOperation = typedOperation $
  operationSpec "ui.viewport.click" "viewport_click" "Apply viewport click input."

uiViewportDragOperation :: TypedServiceOperation UiViewportDragRequest UiViewportDragResponse
uiViewportDragOperation = typedOperation $
  operationSpec "ui.viewport.drag" "viewport_drag" "Apply viewport drag input."

uiViewportHoverOperation :: TypedServiceOperation UiViewportHoverRequest UiViewportHoverResponse
uiViewportHoverOperation = typedOperation $
  operationSpec "ui.viewport.hover" "viewport_hover" "Apply viewport hover input."

uiClickWidgetOperation :: TypedServiceOperation UiClickWidgetRequest UiClickWidgetResponse
uiClickWidgetOperation = typedOperation $
  operationSpec "ui.widgets.click" "click_widget" "Click a widget by ID."

uiListWidgetsOperation :: TypedServiceOperation UiListWidgetsRequest UiListWidgetsResponse
uiListWidgetsOperation = typedOperation $
  operationSpec "ui.widgets.list" "list_widgets" "List active widgets."

uiGetWidgetStateOperation :: TypedServiceOperation UiGetWidgetStateRequest UiWidgetStateResponse
uiGetWidgetStateOperation = typedOperation $
  operationSpec "ui.widgets.state" "get_widget_state" "Read widget state."

uiGetDialogStateOperation :: TypedServiceOperation UiGetDialogStateRequest UiDialogStateResponse
uiGetDialogStateOperation = typedOperation $
  operationSpec "ui.dialog.state" "get_dialog_state" "Read modal/dialog input state."

uiSetDialogTextOperation :: TypedServiceOperation UiSetDialogTextRequest UiSetDialogTextResponse
uiSetDialogTextOperation = typedOperation $
  operationSpec "ui.dialog.text.set" "set_dialog_text" "Set dialog text input."

uiDialogConfirmOperation :: TypedServiceOperation UiDialogConfirmRequest UiDialogActionResponse
uiDialogConfirmOperation = typedOperation $
  operationSpec "ui.dialog.confirm" "dialog_confirm" "Confirm active dialog."

uiDialogCancelOperation :: TypedServiceOperation UiDialogCancelRequest UiDialogActionResponse
uiDialogCancelOperation = typedOperation $
  operationSpec "ui.dialog.cancel" "dialog_cancel" "Cancel active dialog."

uiSendKeyOperation :: TypedServiceOperation UiSendKeyRequest UiSendKeyResponse
uiSendKeyOperation = typedOperation $
  operationSpec "ui.keyboard.send" "send_key" "Send a keyboard event to UI input handling."
