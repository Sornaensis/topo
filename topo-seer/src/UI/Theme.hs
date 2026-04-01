-- | Central colour palette for the topo-seer UI.
--
-- All hard-coded 'V4' colour literals that appear in draw modules are
-- collected here so that the visual style can be adjusted from a
-- single location.  Files in @Seer.Draw.*@ and @Seer.Editor.*@ should
-- import this module instead of embedding colour literals directly.
--
-- Colours are grouped by purpose.  Naming convention:
--
-- * @col*@ – a plain opaque / semi-opaque background or fill colour.
-- * @text*@ – a foreground text colour.
-- * For compound widgets that need two colours (fill + border), the
--   border name is decorated with @Border@.
module UI.Theme
  ( -- * Panel backgrounds
    colTopBar
  , colLeftPanel
  , colConfigPanel
  , colConfigScrollArea
  , colConfigBorder
  , colToolbarBg
  , colDialogBg
  , colHexContextBg
  , colTooltipBg
  , colTooltipBorder
  , colListBg
    -- * Tabs
  , colTabActive
  , colTabInactive
    -- * Generic controls
  , colCtrlBtn
  , colCtrlValue
  , colCtrlValueActive
    -- * Scrollbars
  , colScrollbarTrack
  , colScrollbarHandle
    -- * Config panel action buttons
  , colConfigPresetSave
  , colConfigPresetLoad
  , colConfigReset
  , colConfigRevertActive
  , colConfigRevertDimmed
    -- * Config sliders
  , colSliderBtn
  , colSliderTrack
    -- * Pipeline panel
  , colPipelineCheckEnabled
  , colPipelineCheckEnabledBorder
  , colPipelineCheckDisabled
  , colPipelineCheckDisabledBorder
  , colPipelinePlugin
  , colPipelinePluginBorder
  , colPipelineArrowBg
  , colPipelineArrowBorder
  , colPipelineArrow
  , colPipelineExpandActive
  , colPipelineExpandInactive
  , colPipelineExpandBorder
  , colPipelineParamBarFill
  , colPipelineTickRateBarFill
  , colPipelineTickEnabled
  , colPipelineTickDisabled
    -- * Data browser
  , colDataListSelActive
  , colDataListSelActiveBorder
  , colDataListSelInactive
  , colDataListSelInactiveBorder
  , colDataResourceActive
  , colDataResourceActiveBorder
  , colDataResourceInactive
  , colDataResourceInactiveBorder
  , colDataRecordBg
  , colDataRecordBorder
  , colDataLoadingIndicator
    -- * Log panel
  , colLogPanel
  , colLogHeader
  , colLogDebugLineBg
  , colLogInfoLineBg
  , colLogWarnLineBg
  , colLogErrorLineBg
  , colLogDebugText
  , colLogInfoText
  , colLogWarnText
  , colLogErrorText
  , colLogDebugFilter
  , colLogInfoFilter
  , colLogWarnFilter
  , colLogErrorFilter
    -- * Status bars
  , colStatusBarTrack
  , colStatusTerrainFill
  , colStatusBiomeFill
    -- * Editor toolbar
  , colEditorCloseBg
  , colEditorReopenBg
    -- * Overlay / world view
  , colHoverHex
  , colDialogButtonActive
  , colDialogButtonDisabled
    -- * Input fields
  , colInputFieldBg
    -- * Escape menu
  , colEscapeMenuBg
    -- * Editor controls
  , colEditorRadiusBtn
  , colEditorRadiusBtnDisabled
    -- * Overlay navigation
  , colOverlayActive
  , colOverlayInactive
    -- * Text colours
  , textPrimary
  , textTopBar
  , textMuted
  , textDialogTitle
  , textDialogContent
  , textDialogContentSel
  , textTooltip
  , textHexContext
  , textEditorLabel
  , textEditorActive
  , textEditorInactive
  , textEditorClose
  , textEditorReopen
  , textOverlayBtn
  , textDialogButtonActive
  , textDialogButtonDisabled
  , textStatusBar
  , textInputField
  , textInputFieldBorder
  , textWhite
  , textLogFilterLabel
  , textConfigRevertDimmed
  , textPipelineStageDisabled
  , textPipelineStageName
  , textPipelinePluginName
  , textPipelineSimLabel
  , textPipelineTickActive
  , textPipelineTickInactive
  , textDataResourceLabel
  , textDataLoading
  ) where

import Data.Word (Word8)
import Linear (V4(..))

-------------------------------------------------------------------------------
-- Panel backgrounds
-------------------------------------------------------------------------------

-- | Top application bar background.
colTopBar :: V4 Word8
colTopBar = V4 25 30 42 230

-- | Left panel background.  Not currently used as a solid fill (the
-- transparent rendering relies on nothing being behind it), but
-- available for future solid-background renders.
colLeftPanel :: V4 Word8
colLeftPanel = V4 35 40 55 240

-- | Config (right) panel main background.
colConfigPanel :: V4 Word8
colConfigPanel = V4 35 45 60 230

-- | Config scroll area background, slightly darker than the panel.
colConfigScrollArea :: V4 Word8
colConfigScrollArea = V4 30 38 52 230

-- | Config scroll area border / outline.
colConfigBorder :: V4 Word8
colConfigBorder = V4 60 70 90 255

-- | Editor toolbar background.
colToolbarBg :: V4 Word8
colToolbarBg = V4 30 35 45 230

-- | Modal dialog panel background.
colDialogBg :: V4 Word8
colDialogBg = V4 20 25 35 240

-- | Floating hex-context popup background.
colHexContextBg :: V4 Word8
colHexContextBg = V4 20 20 25 235

-- | Tooltip background.
colTooltipBg :: V4 Word8
colTooltipBg = V4 20 20 30 240

-- | Tooltip border / outline.
colTooltipBorder :: V4 Word8
colTooltipBorder = V4 80 80 100 255

-- | Dialog list background.
colListBg :: V4 Word8
colListBg = V4 30 32 42 255

-------------------------------------------------------------------------------
-- Tabs
-------------------------------------------------------------------------------

-- | Active tab fill.
colTabActive :: V4 Word8
colTabActive = V4 70 90 120 255

-- | Inactive tab fill.
colTabInactive :: V4 Word8
colTabInactive = V4 50 60 75 255

-------------------------------------------------------------------------------
-- Generic controls
-------------------------------------------------------------------------------

-- | +/- button background for numeric controls (chunk size, seed, etc.).
colCtrlBtn :: V4 Word8
colCtrlBtn = V4 90 90 110 255

-- | Value display background for numeric controls.
colCtrlValue :: V4 Word8
colCtrlValue = V4 45 55 70 255

-- | Value display background when the control has focus / is being edited.
colCtrlValueActive :: V4 Word8
colCtrlValueActive = V4 70 90 120 255

-------------------------------------------------------------------------------
-- Scrollbars
-------------------------------------------------------------------------------

-- | Scrollbar track (trough) fill.
colScrollbarTrack :: V4 Word8
colScrollbarTrack = V4 25 25 30 255

-- | Scrollbar handle fill.
colScrollbarHandle :: V4 Word8
colScrollbarHandle = V4 160 160 170 255

-------------------------------------------------------------------------------
-- Config panel action buttons
-------------------------------------------------------------------------------

-- | Preset save button.
colConfigPresetSave :: V4 Word8
colConfigPresetSave = V4 60 120 80 255

-- | Preset load button.
colConfigPresetLoad :: V4 Word8
colConfigPresetLoad = V4 80 110 160 255

-- | Reset-to-defaults button.
colConfigReset :: V4 Word8
colConfigReset = V4 120 80 80 255

-- | Revert-to-saved-config button when a saved config is available.
colConfigRevertActive :: V4 Word8
colConfigRevertActive = V4 140 100 50 255

-- | Revert-to-saved-config button when no saved config is present.
colConfigRevertDimmed :: V4 Word8
colConfigRevertDimmed = V4 70 60 45 120

-------------------------------------------------------------------------------
-- Config sliders
-------------------------------------------------------------------------------

-- | Slider +/- button background.
colSliderBtn :: V4 Word8
colSliderBtn = V4 90 90 110 255

-- | Slider track background.
colSliderTrack :: V4 Word8
colSliderTrack = V4 45 55 70 255

-------------------------------------------------------------------------------
-- Pipeline panel
-------------------------------------------------------------------------------

-- | Checkbox fill for an enabled pipeline stage.
colPipelineCheckEnabled :: V4 Word8
colPipelineCheckEnabled = V4 70 150 90 255

-- | Checkbox border for an enabled pipeline stage.
colPipelineCheckEnabledBorder :: V4 Word8
colPipelineCheckEnabledBorder = V4 100 180 120 255

-- | Checkbox fill for a disabled pipeline stage.
colPipelineCheckDisabled :: V4 Word8
colPipelineCheckDisabled = V4 60 60 70 200

-- | Checkbox border for a disabled pipeline stage.
colPipelineCheckDisabledBorder :: V4 Word8
colPipelineCheckDisabledBorder = V4 80 80 90 200

-- | Plugin row checkbox fill (enabled).
colPipelinePlugin :: V4 Word8
colPipelinePlugin = V4 100 90 160 255

-- | Plugin row checkbox border (enabled).
colPipelinePluginBorder :: V4 Word8
colPipelinePluginBorder = V4 130 120 190 255

-- | Move-up / move-down arrow button background.
colPipelineArrowBg :: V4 Word8
colPipelineArrowBg = V4 50 50 65 255

-- | Move-up / move-down arrow button border.
colPipelineArrowBorder :: V4 Word8
colPipelineArrowBorder = V4 100 100 120 255

-- | Move-up / move-down arrow pixel colour.
colPipelineArrow :: V4 Word8
colPipelineArrow = V4 150 150 170 255

-- | Plugin expand-toggle fill when expanded.
colPipelineExpandActive :: V4 Word8
colPipelineExpandActive = V4 100 160 100 255

-- | Plugin expand-toggle fill when collapsed.
colPipelineExpandInactive :: V4 Word8
colPipelineExpandInactive = V4 100 100 120 255

-- | Plugin expand-toggle border.
colPipelineExpandBorder :: V4 Word8
colPipelineExpandBorder = V4 140 140 160 255

-- | Plugin parameter bar fill colour.
colPipelineParamBarFill :: V4 Word8
colPipelineParamBarFill = V4 120 100 180 255

-- | Simulation tick-rate bar fill colour.
colPipelineTickRateBarFill :: V4 Word8
colPipelineTickRateBarFill = V4 100 130 180 255

-- | Tick-now button when the world is ready.
colPipelineTickEnabled :: V4 Word8
colPipelineTickEnabled = V4 80 120 160 255

-- | Tick-now button when the world is not ready.
colPipelineTickDisabled :: V4 Word8
colPipelineTickDisabled = V4 55 65 80 170

-------------------------------------------------------------------------------
-- Data browser
-------------------------------------------------------------------------------

-- | Selected list item in the data browser (top-level plugin list).
colDataListSelActive :: V4 Word8
colDataListSelActive = V4 70 90 120 255

-- | Border for a selected list item.
colDataListSelActiveBorder :: V4 Word8
colDataListSelActiveBorder = V4 100 130 180 255

-- | Unselected list item.
colDataListSelInactive :: V4 Word8
colDataListSelInactive = V4 50 55 68 220

-- | Border for an unselected list item.
colDataListSelInactiveBorder :: V4 Word8
colDataListSelInactiveBorder = V4 70 75 90 200

-- | Selected data-resource row.
colDataResourceActive :: V4 Word8
colDataResourceActive = V4 80 100 130 255

-- | Border for a selected resource row.
colDataResourceActiveBorder :: V4 Word8
colDataResourceActiveBorder = V4 110 140 190 255

-- | Unselected data-resource row.
colDataResourceInactive :: V4 Word8
colDataResourceInactive = V4 45 50 62 220

-- | Border for an unselected resource row.
colDataResourceInactiveBorder :: V4 Word8
colDataResourceInactiveBorder = V4 65 70 85 200

-- | Generic data-record row.
colDataRecordBg :: V4 Word8
colDataRecordBg = V4 40 45 58 200

-- | Border for a generic data-record row.
colDataRecordBorder :: V4 Word8
colDataRecordBorder = V4 60 65 78 200

-- | Loading-in-progress row indicator.
colDataLoadingIndicator :: V4 Word8
colDataLoadingIndicator = V4 100 100 130 180

-------------------------------------------------------------------------------
-- Log panel
-------------------------------------------------------------------------------

-- | Log panel main background.
colLogPanel :: V4 Word8
colLogPanel = V4 30 30 30 220

-- | Log panel header bar background.
colLogHeader :: V4 Word8
colLogHeader = V4 45 45 45 235

colLogDebugLineBg :: V4 Word8
colLogDebugLineBg = V4 70 70 90 255

colLogInfoLineBg :: V4 Word8
colLogInfoLineBg = V4 60 120 170 255

colLogWarnLineBg :: V4 Word8
colLogWarnLineBg = V4 180 120 40 255

colLogErrorLineBg :: V4 Word8
colLogErrorLineBg = V4 180 60 60 255

colLogDebugText :: V4 Word8
colLogDebugText = V4 200 200 220 255

colLogInfoText :: V4 Word8
colLogInfoText = V4 210 230 245 255

colLogWarnText :: V4 Word8
colLogWarnText = V4 250 230 170 255

colLogErrorText :: V4 Word8
colLogErrorText = V4 250 200 200 255

-- | Base (un-boosted) colour for the Debug filter toggle button.
colLogDebugFilter :: V4 Word8
colLogDebugFilter = V4 80 80 110 255

-- | Base colour for the Info filter toggle button.
colLogInfoFilter :: V4 Word8
colLogInfoFilter = V4 60 140 200 255

-- | Base colour for the Warn filter toggle button.
colLogWarnFilter :: V4 Word8
colLogWarnFilter = V4 200 140 50 255

-- | Base colour for the Error filter toggle button.
colLogErrorFilter :: V4 Word8
colLogErrorFilter = V4 200 60 60 255

-------------------------------------------------------------------------------
-- Status bars
-------------------------------------------------------------------------------

-- | Progress-bar track (background).
colStatusBarTrack :: V4 Word8
colStatusBarTrack = V4 40 60 70 255

-- | Terrain-chunks progress-bar fill.
colStatusTerrainFill :: V4 Word8
colStatusTerrainFill = V4 40 180 90 255

-- | Biome-chunks progress-bar fill.
colStatusBiomeFill :: V4 Word8
colStatusBiomeFill = V4 180 120 40 255

-------------------------------------------------------------------------------
-- Editor toolbar
-------------------------------------------------------------------------------

-- | Close-editor button background.
colEditorCloseBg :: V4 Word8
colEditorCloseBg = V4 160 60 60 255

-- | Reopen-editor button background.
colEditorReopenBg :: V4 Word8
colEditorReopenBg = V4 50 60 80 220

-------------------------------------------------------------------------------
-- Overlay / world view
-------------------------------------------------------------------------------

-- | Hovered hex highlight colour (semi-transparent).
colHoverHex :: V4 Word8
colHoverHex = V4 220 220 220 160

-- | Active state for dialog action buttons.
colDialogButtonActive :: V4 Word8
colDialogButtonActive = V4 70 90 120 255

-- | Disabled state for dialog action buttons.
colDialogButtonDisabled :: V4 Word8
colDialogButtonDisabled = V4 55 55 65 255

-------------------------------------------------------------------------------
-- Input fields
-------------------------------------------------------------------------------

-- | Text-input field background (darker than general controls).
colInputFieldBg :: V4 Word8
colInputFieldBg = V4 40 42 55 255

-------------------------------------------------------------------------------
-- Escape menu
-------------------------------------------------------------------------------

-- | Escape / pause menu overlay background.
colEscapeMenuBg :: V4 Word8
colEscapeMenuBg = V4 20 25 35 230

-------------------------------------------------------------------------------
-- Editor radius controls
-------------------------------------------------------------------------------

-- | Editor radius +/- button background.
colEditorRadiusBtn :: V4 Word8
colEditorRadiusBtn = V4 70 80 100 255

-- | Editor radius +/- button background when at a bound (disabled appearance).
colEditorRadiusBtnDisabled :: V4 Word8
colEditorRadiusBtnDisabled = V4 40 45 55 180

-------------------------------------------------------------------------------
-- Overlay navigation buttons
-------------------------------------------------------------------------------

-- | Overlay navigation button when an overlay is active.
colOverlayActive :: V4 Word8
colOverlayActive = V4 160 100 200 255

-- | Overlay navigation button when no overlay is active.
colOverlayInactive :: V4 Word8
colOverlayInactive = V4 80 70 100 255

-------------------------------------------------------------------------------
-- Text colours
-------------------------------------------------------------------------------

-- | Default UI label / item text.
textPrimary :: V4 Word8
textPrimary = V4 235 235 235 255

-- | Top-bar world-name text.
textTopBar :: V4 Word8
textTopBar = V4 200 210 225 255

-- | Muted / secondary text (inactive items, hints).
textMuted :: V4 Word8
textMuted = V4 200 200 210 255

-- | Dialog panel title text.
textDialogTitle :: V4 Word8
textDialogTitle = V4 220 220 230 255

-- | Default list item text inside dialogs.
textDialogContent :: V4 Word8
textDialogContent = V4 180 180 190 255

-- | Selected list item text inside dialogs.
textDialogContentSel :: V4 Word8
textDialogContentSel = V4 240 240 245 255

-- | Tooltip text.
textTooltip :: V4 Word8
textTooltip = V4 220 220 230 255

-- | Hex-context popup text.
textHexContext :: V4 Word8
textHexContext = V4 230 230 235 255

-- | Editor toolbar control labels.
textEditorLabel :: V4 Word8
textEditorLabel = V4 220 220 230 255

-- | Text on an active (highlighted) tool button.
textEditorActive :: V4 Word8
textEditorActive = V4 0 0 0 255

-- | Text on an inactive tool button.
textEditorInactive :: V4 Word8
textEditorInactive = V4 200 200 210 255

-- | Text on the close-editor button.
textEditorClose :: V4 Word8
textEditorClose = V4 255 255 255 255

-- | Text on the reopen-editor button.
textEditorReopen :: V4 Word8
textEditorReopen = V4 200 210 230 255

-- | Overlay navigation button labels.
textOverlayBtn :: V4 Word8
textOverlayBtn = V4 230 230 240 255

-- | Enabled dialog button text.
textDialogButtonActive :: V4 Word8
textDialogButtonActive = V4 230 230 235 255

-- | Disabled dialog button text.
textDialogButtonDisabled :: V4 Word8
textDialogButtonDisabled = V4 140 140 150 255

-- | Status bar label text.
textStatusBar :: V4 Word8
textStatusBar = V4 230 230 235 255

-- | Text-input field content.
textInputField :: V4 Word8
textInputField = V4 220 220 230 255

-- | Text-input field border.
textInputFieldBorder :: V4 Word8
textInputFieldBorder = V4 90 100 120 255

-- | Opaque white, used for font-metrics probing and close-button labels.
textWhite :: V4 Word8
textWhite = V4 255 255 255 255

-- | Text on log-level filter toggle buttons.
textLogFilterLabel :: V4 Word8
textLogFilterLabel = V4 230 230 230 255

-- | Dimmed colour for the revert-preset label when no saved config exists.
textConfigRevertDimmed :: V4 Word8
textConfigRevertDimmed = V4 120 120 130 140

-- | Pipeline stage name text when the stage is disabled.
textPipelineStageDisabled :: V4 Word8
textPipelineStageDisabled = V4 120 120 130 180

-- | Pipeline stage name text when the stage is enabled.
textPipelineStageName :: V4 Word8
textPipelineStageName = V4 220 220 225 255

-- | Plugin name label colour in the pipeline panel.
textPipelinePluginName :: V4 Word8
textPipelinePluginName = V4 190 180 220 255

-- | Simulation control label colour.
textPipelineSimLabel :: V4 Word8
textPipelineSimLabel = V4 180 200 220 255

-- | Tick-now button label when the world is ready.
textPipelineTickActive :: V4 Word8
textPipelineTickActive = V4 220 230 240 255

-- | Tick-now button label when the world is not ready.
textPipelineTickInactive :: V4 Word8
textPipelineTickInactive = V4 140 150 165 180

-- | Data-browser resource label colour.
textDataResourceLabel :: V4 Word8
textDataResourceLabel = V4 170 200 230 255

-- | Data-browser "Loading..." indicator text.
textDataLoading :: V4 Word8
textDataLoading = V4 140 140 160 200
