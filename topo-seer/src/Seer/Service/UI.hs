{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.UI
  ( UiService(..)
  , uiServiceGroup
  , uiServiceOperationSpecs
  ) where

import Seer.Service.Types

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

uiServiceGroup :: ServiceGroupSpec
uiServiceGroup = ServiceGroupSpec "ui" uiServiceOperationSpecs

uiServiceOperationSpecs :: [ServiceOperationSpec]
uiServiceOperationSpecs =
  [ operationSpec "ui.seed.set" "set_seed" "Set generation seed text."
  , operationSpec "ui.viewMode.set" "set_view_mode" "Set active terrain view mode."
  , operationSpec "ui.configTab.set" "set_config_tab" "Set active config tab."
  , operationSpec "ui.hex.select" "select_hex" "Select one hex in the viewport."
  , operationSpec "ui.overlay.set" "set_overlay" "Select active overlay."
  , operationSpec "ui.overlay.fields" "list_overlay_fields" "List fields for the active overlay."
  , operationSpec "ui.overlay.cycle" "cycle_overlay" "Cycle active overlay."
  , operationSpec "ui.overlay.field.cycle" "cycle_overlay_field" "Cycle active overlay field."
  , operationSpec "ui.camera.set" "set_camera" "Set viewport camera."
  , operationSpec "ui.camera.get" "get_camera" "Read viewport camera."
  , operationSpec "ui.camera.zoomToChunk" "zoom_to_chunk" "Move camera to a chunk."
  , operationSpec "ui.panels.left.set" "set_left_panel" "Show or hide the left panel."
  , operationSpec "ui.tabs.left.set" "set_left_tab" "Set the left-panel tab."
  , operationSpec "ui.panels.config.toggle" "toggle_config_panel" "Toggle config panel visibility."
  , operationSpec "ui.logs.collapsed.set" "set_log_collapsed" "Collapse or expand the log panel."
  , operationSpec "ui.logs.level.set" "set_log_level" "Set visible log level."
  , operationSpec "ui.panels.get" "get_ui_panels" "Read panel and tab state."
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
