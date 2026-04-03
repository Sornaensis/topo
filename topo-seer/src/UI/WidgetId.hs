module UI.WidgetId
  ( WidgetId(..)
  ) where

import Data.Text (Text)
import Seer.Config.SliderId (SliderId(..))

data WidgetId
  = WidgetGenerate
  | WidgetLeftToggle
  | WidgetLeftTabTopo
  | WidgetLeftTabView
  | WidgetSeedValue
  | WidgetSeedRandom
  | WidgetChunkMinus
  | WidgetChunkPlus
  | WidgetConfigToggle
  | WidgetConfigTabTerrain
  | WidgetConfigTabPlanet
  | WidgetConfigTabClimate
  | WidgetConfigTabWeather
  | WidgetConfigTabBiome
  | WidgetConfigTabErosion
  | WidgetConfigTabPipeline
  | WidgetConfigPresetSave
  | WidgetConfigPresetLoad
  | WidgetConfigReset
  | WidgetConfigRevert
  -- Slider minus/plus buttons, parameterized by SliderId
  | WidgetSliderMinus !SliderId
  | WidgetSliderPlus !SliderId
  | WidgetViewElevation
  | WidgetViewBiome
  | WidgetViewClimate
  | WidgetViewWeather
  | WidgetViewMoisture
  | WidgetViewPrecip
  | WidgetViewVegetation
  | WidgetViewTerrainForm
  | WidgetViewPlateId
  | WidgetViewPlateBoundary
  | WidgetViewPlateHardness
  | WidgetViewPlateCrust
  | WidgetViewPlateAge
  | WidgetViewPlateHeight
  | WidgetViewPlateVelocity
  | WidgetViewOverlayPrev
  -- ^ Previous overlay in the overlay store.
  | WidgetViewOverlayNext
  -- ^ Next overlay in the overlay store.
  | WidgetViewFieldPrev
  -- ^ Previous field within the selected overlay.
  | WidgetViewFieldNext
  -- ^ Next field within the selected overlay.
  | WidgetLogDebug
  | WidgetLogInfo
  | WidgetLogWarn
  | WidgetLogError
  | WidgetLogHeader
  | WidgetMenuSave
  | WidgetMenuLoad
  | WidgetMenuExit
  | WidgetPresetSaveOk
  | WidgetPresetSaveCancel
  | WidgetPresetLoadOk
  | WidgetPresetLoadCancel
  | WidgetPresetLoadItem
  | WidgetWorldSaveOk
  | WidgetWorldSaveCancel
  | WidgetWorldLoadOk
  | WidgetWorldLoadCancel
  | WidgetWorldLoadItem
  | WidgetPipelineToggle !Text
  | WidgetPluginMoveUp !Text
  | WidgetPluginMoveDown !Text
  | WidgetPluginToggle !Text
  | WidgetPluginExpand !Text
  | WidgetPluginParamSlider !Text !Text
  | WidgetPluginParamCheck !Text !Text
  | WidgetSimTick
  | WidgetSimAutoTick
  -- Data browser (ConfigData tab)
  | WidgetConfigTabData
  | WidgetDataPluginSelect !Text
  | WidgetDataResourceSelect !Text !Text
  | WidgetDataPagePrev !Text !Text
  | WidgetDataPageNext !Text !Text
  | WidgetDataRecordSelect !Int
    -- ^ Click a record row by index to show the detail popover.
  | WidgetDataDetailDismiss
    -- ^ Dismiss the detail popover.
  | WidgetDataFieldToggle !Text
    -- ^ Toggle expand/collapse of a nested field path in the detail popover.
    -- Data browser mutation controls
  | WidgetDataEditToggle
    -- ^ Toggle edit mode in the detail popover.
  | WidgetDataEditSave
    -- ^ Save changes (MutUpdate or MutCreate).
  | WidgetDataEditCancel
    -- ^ Cancel editing / creating, revert to read-only.
  | WidgetDataCreateNew
    -- ^ Open the popover in create-new-record mode.
  | WidgetDataDeleteBtn
    -- ^ Show the delete confirmation modal.
  | WidgetDataDeleteConfirm
    -- ^ Confirm deletion.
  | WidgetDataDeleteCancel
    -- ^ Cancel deletion.
  | WidgetDataFieldTextClick !Text
    -- ^ Click a DFText field to focus it for keyboard input.
  | WidgetDataFieldStepMinus !Text
    -- ^ Minus button for a numeric field stepper.
  | WidgetDataFieldStepPlus !Text
    -- ^ Plus button for a numeric field stepper.
  | WidgetDataFieldBoolToggle !Text
    -- ^ Toggle a DFBool field.
  | WidgetDataFieldEnumPrev !Text
    -- ^ Cycle a DFEnum field to the previous option.
  | WidgetDataFieldEnumNext !Text
    -- ^ Cycle a DFEnum field to the next option.
    -- Editor toolbar
  | WidgetEditorTool !Int
    -- ^ Tool button (0-indexed corresponding to 'EditorTool' enum order).
  | WidgetEditorRadiusMinus
  | WidgetEditorRadiusPlus
  | WidgetEditorClose
  | WidgetEditorReopen
    -- ^ Button shown when the editor toolbar is closed, to reopen it.
    -- Editor param bar
  | WidgetEditorParamMinus !Int
    -- ^ Minus button for a numeric parameter in the param bar (0-based slot).
  | WidgetEditorParamPlus !Int
    -- ^ Plus button for a numeric parameter in the param bar (0-based slot).
  | WidgetEditorCyclePrev !Int
    -- ^ Prev arrow for a cycle selector in the param bar (0-based slot).
  | WidgetEditorCycleNext !Int
    -- ^ Next arrow for a cycle selector in the param bar (0-based slot).
  | WidgetEditorFalloffPrev
    -- ^ Prev arrow for the always-visible falloff cycle selector.
  | WidgetEditorFalloffNext
    -- ^ Next arrow for the always-visible falloff cycle selector.
  deriving (Eq, Show)