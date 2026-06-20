{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.UI
  ( UiService(..)
  , UiHexCoord(..)
  , UiSetSeedRequest(..)
  , UiSetSeedResponse(..)
  , UiSetViewModeRequest(..)
  , UiSetViewModeResponse(..)
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
  , uiSetSeedOperation
  , uiSetViewModeOperation
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
  , uiServiceGroup
  , uiServiceOperationSpecs
  ) where

import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.Types
import Topo.Overlay.Schema (OverlayFieldType)
import Topo.Types (ChunkId)

data UiService = UiService
  { uiSetSeed :: !ServiceHandler
  , uiSetViewMode :: !ServiceHandler
  , uiSetConfigTab :: !ServiceHandler
  , uiSelectHex :: !ServiceHandler
  , uiSetOverlay :: !ServiceHandler
  , uiListOverlayFields :: !ServiceHandler
  , uiCycleOverlay :: !ServiceHandler
  , uiCycleOverlayField :: !ServiceHandler
  , uiSetCamera :: !ServiceHandler
  , uiGetCamera :: !ServiceHandler
  , uiZoomToChunk :: !ServiceHandler
  , uiSetLeftPanel :: !ServiceHandler
  , uiSetLeftTab :: !ServiceHandler
  , uiToggleConfigPanel :: !ServiceHandler
  , uiSetLogCollapsed :: !ServiceHandler
  , uiSetLogLevel :: !ServiceHandler
  , uiGetPanels :: !ServiceHandler
  , uiViewportScroll :: !ServiceHandler
  , uiViewportClick :: !ServiceHandler
  , uiViewportDrag :: !ServiceHandler
  , uiViewportHover :: !ServiceHandler
  , uiClickWidget :: !ServiceHandler
  , uiListWidgets :: !ServiceHandler
  , uiGetWidgetState :: !ServiceHandler
  , uiGetDialogState :: !ServiceHandler
  , uiSetDialogText :: !ServiceHandler
  , uiDialogConfirm :: !ServiceHandler
  , uiDialogCancel :: !ServiceHandler
  , uiSendKey :: !ServiceHandler
  }

data UiHexCoord = UiHexCoord
  { uiHexCoordQ :: !Int
  , uiHexCoordR :: !Int
  } deriving (Eq, Show)

newtype UiSetSeedRequest = UiSetSeedRequest
  { uiSetSeedRequestValue :: Word64
  } deriving (Eq, Show)

newtype UiSetSeedResponse = UiSetSeedResponse
  { uiSetSeedResponseValue :: Word64
  } deriving (Eq, Show)

data UiSetViewModeRequest = UiSetViewModeRequest
  { uiSetViewModeRequestName :: !Text
  , uiSetViewModeRequestFieldIndex :: !(Maybe Int)
  } deriving (Eq, Show)

newtype UiSetViewModeResponse = UiSetViewModeResponse
  { uiSetViewModeResponseName :: Text
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
  , uiSelectedOverlayViewMode :: !Text
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
  , uiCycleOverlayViewMode :: !Text
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

uiServiceGroup :: ServiceGroupSpec
uiServiceGroup = ServiceGroupSpec "ui" uiServiceOperationSpecs

uiServiceOperationSpecs :: [ServiceOperationSpec]
uiServiceOperationSpecs =
  [ typedServiceOperationSpec uiSetSeedOperation
  , typedServiceOperationSpec uiSetViewModeOperation
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
  , operationSpec "ui.viewport.scroll" "viewport_scroll" "Apply viewport scroll input."
  , operationSpec "ui.viewport.click" "viewport_click" "Apply viewport click input."
  , operationSpec "ui.viewport.drag" "viewport_drag" "Apply viewport drag input."
  , operationSpec "ui.viewport.hover" "viewport_hover" "Apply viewport hover input."
  , operationSpec "ui.widgets.click" "click_widget" "Click a widget by ID."
  , operationSpec "ui.widgets.list" "list_widgets" "List active widgets."
  , operationSpec "ui.widgets.state" "get_widget_state" "Read widget state."
  , operationSpec "ui.dialog.state" "get_dialog_state" "Read modal/dialog input state."
  , operationSpec "ui.dialog.text.set" "set_dialog_text" "Set dialog text input."
  , operationSpec "ui.dialog.confirm" "dialog_confirm" "Confirm active dialog."
  , operationSpec "ui.dialog.cancel" "dialog_cancel" "Cancel active dialog."
  , operationSpec "ui.keyboard.send" "send_key" "Send a keyboard event to UI input handling."
  ]

uiSetSeedOperation :: TypedServiceOperation UiSetSeedRequest UiSetSeedResponse
uiSetSeedOperation = typedOperation $
  operationSpec "ui.seed.set" "set_seed" "Set generation seed text."

uiSetViewModeOperation :: TypedServiceOperation UiSetViewModeRequest UiSetViewModeResponse
uiSetViewModeOperation = typedOperation $
  operationSpec "ui.viewMode.set" "set_view_mode" "Set active terrain view mode."

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
