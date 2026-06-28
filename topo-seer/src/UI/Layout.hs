module UI.Layout
  ( Layout
  , layoutGeometry
  , WindowSize(..)
  , LogHeight(..)
  , SeedInputWidth(..)
  , UiGeometry(..)
  , TopBarGeometry(..)
  , LeftPanelGeometry(..)
  , ConfigPanelGeometry(..)
  , LogPanelGeometry(..)
  , EditorGeometry(..)
  , PanelTabs(..)
  , ConfigTabs(..)
  , LogFilterButtons(..)
  , OverlayViewGeometry(..)
  , ConfigParamRowRects(..)
  , topBarHeight
  , minUsableWindowWidth
  , minUsableWindowHeight
  , topBarRect
  , layoutFor
  , layoutForSeed
  , leftGenButtonRect
  , leftPanelRect
  , leftToggleRect
  , leftTabRects
  , configSeedValueRect
  , configSeedLabelRect
  , configSeedRandomRect
  , leftChunkMinusRect
  , leftChunkPlusRect
  , leftViewRects
  , leftViewRowCount
  , leftViewContentHeight
  , leftViewScrollMax
  , leftControlsTop
  , overlayViewRects
  , configToggleRect
  , configPanelRect
  , configTabRects
  , configPresetSaveRect
  , configPresetLoadRect
  , configResetRect
  , configRevertRect
  , configScrollAreaRect
  , configScrollBarRect
  , configScrollRowRect
  , configParamRects
  , configParamMinusRect
  , configParamPlusRect
  , configParamBarRect
  , configRowTopPad
  , pipelineCheckboxRect
  , pipelineMoveUpRect
  , pipelineMoveDownRect
  , pipelineTickButtonRect
  , pipelineTickRateBarRect
  , pipelineExpandRect
  , pipelineParamBarRect
  , pipelineParamCheckRect
    -- Data browser (ConfigData tab)
  , dataBrowserItemRect
  , dataBrowserPagePrevRect
  , dataBrowserPageNextRect
  , dataBrowserCreateButtonRect
  , dataDetailPopoverRect
  , dataDetailFieldRect
  , dataDetailEditToggleRect
  , dataDetailSaveRect
  , dataDetailCancelRect
  , dataDetailDeleteRect
  , dataDetailFieldInputRect
  , dataDetailFieldStepMinusRect
  , dataDetailFieldStepPlusRect
  , deleteConfirmDialogRect
  , deleteConfirmOkRect
  , deleteConfirmCancelRect
  , configParamRowRect
  , configChunkMinusRect
  , configChunkPlusRect
  , configChunkValueRect
  , logPanelRect
  , logHeaderRect
  , logBodyRect
  , logFilterRects
  , menuPanelRect
  , menuSaveRect
  , menuLoadRect
  , menuExitRect
    -- Preset save dialog
  , presetSaveDialogRect
  , presetSaveInputRect
  , presetSaveOkRect
  , presetSaveCancelRect
    -- Preset load dialog
  , presetLoadDialogRect
  , presetLoadFilterRect
  , presetLoadListRect
  , presetLoadItemRect
  , presetLoadOkRect
  , presetLoadCancelRect
    -- World save dialog
  , worldSaveDialogRect
  , worldSaveInputRect
  , worldSaveOkRect
  , worldSaveCancelRect
    -- World load dialog
  , worldLoadDialogRect
  , worldLoadFilterRect
  , worldLoadListRect
  , worldLoadItemRect
  , worldLoadOkRect
  , worldLoadCancelRect
    -- Editor toolbar
  , editorToolbarRect
  , editorToolButtonRect
  , editorToolButtonCount
  , editorRadiusMinusRect
  , editorRadiusPlusRect
  , editorRadiusValueRect
  , editorCloseRect
  , editorReopenRect
    -- Editor param bar
  , editorParamBarRect
  , editorParamNumericRects
  , editorParamCycleRects
  , editorParamFalloffRects
  , dayNightToggleRect
  ) where

import Linear (V2(..))
import Seer.Editor.Types (EditorTool)
import UI.Geometry
import UI.Layout.Geometry (layoutTopBarHeight, uiGeometryForSeed)
import UI.Widgets (Rect(..))

-- | Shared geometry for one config slider row.
data ConfigParamRowRects = ConfigParamRowRects
  { configParamRowHitRect :: !Rect
  , configParamRowMinusRect :: !Rect
  , configParamRowBarRect :: !Rect
  , configParamRowPlusRect :: !Rect
  } deriving (Eq, Show)

data Layout = Layout
  { layoutSize :: V2 Int
  , layoutLogHeight :: Int
  , layoutSeedWidth :: Int
  , layoutUiGeometry :: UiGeometry
  } deriving (Eq, Show)

-- | Height of the top bar in pixels.
topBarHeight :: Int
topBarHeight = layoutTopBarHeight

-- | Minimum window width at which both side panels fit without overlap.
--
-- Below this width the config panel narrows automatically; the left panel
-- remains at its fixed size.  The value is derived from the two panel widths
-- and their margins so it stays in sync with 'leftPanelRect' and
-- 'configPanelRect'.
minUsableWindowWidth :: Int
minUsableWindowWidth = 580   -- leftX(16) + leftW(240) + gap(8) + cfgMinW(300) + rightMargin(16)

-- | Minimum window height at which the UI is usable.
minUsableWindowHeight :: Int
minUsableWindowHeight = 480

-- | Full-width bar at the top of the window displaying the world name.
topBarRect :: Layout -> Rect
topBarRect layout =
  topBarBounds (uiTopBarGeometry (layoutGeometry layout))

layoutFor :: V2 Int -> Int -> Layout
layoutFor size logHeight = layoutForSeed size logHeight 120

layoutForSeed :: V2 Int -> Int -> Int -> Layout
layoutForSeed size logHeight seedWidth = Layout
  { layoutSize = size
  , layoutLogHeight = logHeight
  , layoutSeedWidth = seedWidth
  , layoutUiGeometry = uiGeometryForSeed
      (WindowSize size)
      (LogHeight logHeight)
      (SeedInputWidth seedWidth)
  }

layoutGeometry :: Layout -> UiGeometry
layoutGeometry = layoutUiGeometry

-- | Generate button inside the left panel, below seed value (Row 4).
leftGenButtonRect :: Layout -> Rect
leftGenButtonRect layout =
  leftPanelGenerateButton (uiLeftPanelGeometry (layoutGeometry layout))

leftPanelRect :: Layout -> Rect
leftPanelRect layout =
  leftPanelBounds (uiLeftPanelGeometry (layoutGeometry layout))

leftToggleRect :: Layout -> Rect
leftToggleRect layout =
  leftPanelToggleButton (uiLeftPanelGeometry (layoutGeometry layout))

leftTabRects :: Layout -> (Rect, Rect)
leftTabRects layout =
  panelTabsToTuple (leftPanelTabs (uiLeftPanelGeometry (layoutGeometry layout)))

configToggleRect :: Layout -> Rect
configToggleRect layout =
  configPanelToggleButton (uiConfigPanelGeometry (layoutGeometry layout))

configPanelRect :: Layout -> Rect
configPanelRect layout =
  configPanelBounds (uiConfigPanelGeometry (layoutGeometry layout))

configTabRects :: Layout -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
configTabRects layout =
  configTabsToTuple (configPanelTabs (uiConfigPanelGeometry (layoutGeometry layout)))

configChunkMinusRect :: Layout -> Rect
configChunkMinusRect = leftChunkMinusRect

configChunkPlusRect :: Layout -> Rect
configChunkPlusRect = leftChunkPlusRect

configChunkValueRect :: Layout -> Rect
configChunkValueRect = leftChunkValueRect

configSeedValueRect :: Layout -> Rect
configSeedValueRect = leftSeedValueRect

configSeedLabelRect :: Layout -> Rect
configSeedLabelRect = leftSeedLabelRect

configSeedRandomRect :: Layout -> Rect
configSeedRandomRect = leftSeedRandomRect

-- | Config preset save button (top of 4-button stack).
configPresetSaveRect :: Layout -> Rect
configPresetSaveRect layout =
  configPresetSaveButton (uiConfigPanelGeometry (layoutGeometry layout))

-- | Config preset load button (second in 4-button stack).
configPresetLoadRect :: Layout -> Rect
configPresetLoadRect layout =
  configPresetLoadButton (uiConfigPanelGeometry (layoutGeometry layout))

-- | Config reset button (third in 4-button stack).
configResetRect :: Layout -> Rect
configResetRect layout =
  configResetButton (uiConfigPanelGeometry (layoutGeometry layout))

-- | Config revert button (bottom of 4-button stack).
configRevertRect :: Layout -> Rect
configRevertRect layout =
  configRevertButton (uiConfigPanelGeometry (layoutGeometry layout))

configScrollAreaRect :: Layout -> Rect
configScrollAreaRect layout =
  configScrollAreaBounds (uiConfigPanelGeometry (layoutGeometry layout))

configScrollBarRect :: Layout -> Rect
configScrollBarRect layout =
  configScrollBarBounds (uiConfigPanelGeometry (layoutGeometry layout))

configRowTopPad :: Int
configRowTopPad = 12

-- | Shared row geometry for controls that live inside the config scroll area.
configScrollRowRect :: Int -> Layout -> Rect
configScrollRowRect index layout =
  let Rect (V2 x y, V2 w _) = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      top = y + configRowTopPad + index * (rowHeight + gap)
  in Rect (V2 x top, V2 w rowHeight)

-- | Compute the full geometry for one config slider row from its row index.
configParamRects :: Int -> Layout -> ConfigParamRowRects
configParamRects index layout =
  let Rect (V2 x top, V2 w rowHeight) = configScrollRowRect index layout
      minusRect = Rect (V2 x top, V2 24 24)
      plusRect = Rect (V2 (x + w - 24) top, V2 24 24)
      barHeight = 12
      barY = top + (rowHeight - barHeight) `div` 2
      barX = x + 24 + 8
      barW = w - (24 * 2 + 16)
      barRect = Rect (V2 barX barY, V2 barW barHeight)
      labelPad = 18
      rowRect = Rect (V2 x (top - labelPad), V2 w (rowHeight + labelPad))
  in ConfigParamRowRects
       { configParamRowHitRect = rowRect
       , configParamRowMinusRect = minusRect
       , configParamRowBarRect = barRect
       , configParamRowPlusRect = plusRect
       }

configParamMinusRect :: Int -> Layout -> Rect
configParamMinusRect index layout =
  configParamRowMinusRect (configParamRects index layout)

configParamPlusRect :: Int -> Layout -> Rect
configParamPlusRect index layout =
  configParamRowPlusRect (configParamRects index layout)

configParamBarRect :: Int -> Layout -> Rect
configParamBarRect index layout =
  configParamRowBarRect (configParamRects index layout)

-- | Full row rect for a config parameter, covering buttons, bar, and label.
--
-- Extends 18 px above the standard row top to include the label text area.
-- Used for tooltip hover detection so the tooltip activates over the entire
-- slider region, not just the +/- buttons.
configParamRowRect :: Int -> Layout -> Rect
configParamRowRect index layout =
  configParamRowHitRect (configParamRects index layout)

leftControlsTop :: Layout -> Int
leftControlsTop layout =
  leftPanelControlsTopY (uiLeftPanelGeometry (layoutGeometry layout))

leftParamMinusRect :: Int -> Layout -> Rect
leftParamMinusRect index layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 24 24)

leftParamPlusRect :: Int -> Layout -> Rect
leftParamPlusRect index layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
  in Rect (V2 (x + w - pad - 24) top, V2 24 24)

leftParamBarRect :: Int -> Layout -> Rect
leftParamBarRect index layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
      barHeight = 12
      barY = top + (rowHeight - barHeight) `div` 2
      barX = x + pad + 24 + 8
      barW = w - (pad * 2 + 24 * 2 + 16)
  in Rect (V2 barX barY, V2 barW barHeight)

leftChunkMinusRect :: Layout -> Rect
leftChunkMinusRect layout =
  leftPanelChunkMinusButton (uiLeftPanelGeometry (layoutGeometry layout))

leftChunkPlusRect :: Layout -> Rect
leftChunkPlusRect layout =
  leftPanelChunkPlusButton (uiLeftPanelGeometry (layoutGeometry layout))

leftChunkValueRect :: Layout -> Rect
leftChunkValueRect layout =
  leftPanelChunkValueBounds (uiLeftPanelGeometry (layoutGeometry layout))

leftSeedMinusRect :: Layout -> Rect
leftSeedMinusRect = leftParamMinusRect 1

leftSeedPlusRect :: Layout -> Rect
leftSeedPlusRect = leftParamPlusRect 1

leftSeedLabelRect :: Layout -> Rect
leftSeedLabelRect layout =
  leftPanelSeedLabelBounds (uiLeftPanelGeometry (layoutGeometry layout))

leftSeedRandomRect :: Layout -> Rect
leftSeedRandomRect layout =
  leftPanelSeedRandomButton (uiLeftPanelGeometry (layoutGeometry layout))

leftSeedValueRect :: Layout -> Rect
leftSeedValueRect layout =
  leftPanelSeedValueBounds (uiLeftPanelGeometry (layoutGeometry layout))

-- | Number of rows used by view-mode buttons (ceil(15/2) = 8).
-- | Total pixel height of the content in the left View tab
-- (view mode buttons + overlay selector rows).
leftViewContentHeight :: Layout -> Int
leftViewContentHeight layout =
  leftPanelViewContentHeightPx (uiLeftPanelGeometry (layoutGeometry layout))

-- | Maximum scroll offset for the left View tab content.
-- Returns 0 when the content fits within the panel.
leftViewScrollMax :: Layout -> Int
leftViewScrollMax layout =
  leftPanelViewScrollMaxPx (uiLeftPanelGeometry (layoutGeometry layout))

leftViewRowCount :: Int
leftViewRowCount = 8

leftViewRects :: Layout -> [Rect]
leftViewRects layout =
  leftPanelViewButtons (uiLeftPanelGeometry (layoutGeometry layout))

-- | Overlay selector button rects in the left View panel.
--
-- Returns @(overlayPrev, overlayNext, fieldPrev, fieldNext)@ positioned
-- below the view mode buttons.
overlayViewRects :: Layout -> (Rect, Rect, Rect, Rect)
overlayViewRects layout =
  overlayViewGeometryToTuple (leftPanelOverlayButtons (uiLeftPanelGeometry (layoutGeometry layout)))

-- | Day/night toggle button rect, positioned after the overlay selector rows
-- in the left View tab.
dayNightToggleRect :: Layout -> Rect
dayNightToggleRect layout =
  leftPanelDayNightToggleButton (uiLeftPanelGeometry (layoutGeometry layout))

logPanelRect :: Layout -> Rect
logPanelRect layout =
  logPanelBounds (uiLogPanelGeometry (layoutGeometry layout))

logHeaderRect :: Layout -> Rect
logHeaderRect layout =
  logHeaderBounds (uiLogPanelGeometry (layoutGeometry layout))

logBodyRect :: Layout -> Rect
logBodyRect layout =
  logBodyBounds (uiLogPanelGeometry (layoutGeometry layout))

logFilterRects :: Layout -> (Rect, Rect, Rect, Rect)
logFilterRects layout =
  logFilterButtonsToTuple (logFilterButtons (uiLogPanelGeometry (layoutGeometry layout)))

menuPanelRect :: Layout -> Rect
menuPanelRect (Layout { layoutSize = V2 w h }) =
  let panelW = 240
      panelH = 180
      x = (w - panelW) `div` 2
      y = (h - panelH) `div` 2
  in Rect (V2 x y, V2 panelW panelH)

menuSaveRect :: Layout -> Rect
menuSaveRect layout = menuButtonRect layout 0

menuLoadRect :: Layout -> Rect
menuLoadRect layout = menuButtonRect layout 1

menuExitRect :: Layout -> Rect
menuExitRect layout = menuButtonRect layout 2

menuButtonRect :: Layout -> Int -> Rect
menuButtonRect layout index =
  let Rect (V2 x y, V2 w _) = menuPanelRect layout
      pad = 16
      buttonH = 28
      gap = 12
      top = y + pad + index * (buttonH + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) buttonH)

-- ---------------------------------------------------------------------------
-- Preset save dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the preset save dialog (280 × 150).
presetSaveDialogRect :: Layout -> Rect
presetSaveDialogRect (Layout { layoutSize = V2 w h }) =
  let dw = 280; dh = 150
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Text input field inside the preset save dialog.
presetSaveInputRect :: Layout -> Rect
presetSaveInputRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetSaveDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | \"Ok\" button in the preset save dialog.
presetSaveOkRect :: Layout -> Rect
presetSaveOkRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the preset save dialog.
presetSaveCancelRect :: Layout -> Rect
presetSaveCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- Preset load dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the preset load dialog (280 × 320).
presetLoadDialogRect :: Layout -> Rect
presetLoadDialogRect (Layout { layoutSize = V2 w h }) =
  let dw = 280; dh = 360
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Search/filter input inside the preset load dialog.
presetLoadFilterRect :: Layout -> Rect
presetLoadFilterRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | Scrollable list area inside the preset load dialog.
presetLoadListRect :: Layout -> Rect
presetLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 72), V2 (dw - pad * 2) 220)

-- | Individual item rect inside the preset load list.
presetLoadItemRect :: Layout -> Int -> Rect
presetLoadItemRect layout index =
  let Rect (V2 lx ly, V2 lw _) = presetLoadListRect layout
      itemH = 24
  in Rect (V2 lx (ly + index * itemH), V2 lw itemH)

-- | \"Load\" button in the preset load dialog.
presetLoadOkRect :: Layout -> Rect
presetLoadOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = presetLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the preset load dialog.
presetLoadCancelRect :: Layout -> Rect
presetLoadCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- World save dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the world save dialog (300 × 140).
worldSaveDialogRect :: Layout -> Rect
worldSaveDialogRect (Layout { layoutSize = V2 w h }) =
  let dw = 300; dh = 140
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Text input field inside the world save dialog.
worldSaveInputRect :: Layout -> Rect
worldSaveInputRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldSaveDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | \"Save\" button in the world save dialog.
worldSaveOkRect :: Layout -> Rect
worldSaveOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = worldSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the world save dialog.
worldSaveCancelRect :: Layout -> Rect
worldSaveCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = worldSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- World load dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the world load dialog (320 × 400).
worldLoadDialogRect :: Layout -> Rect
worldLoadDialogRect (Layout { layoutSize = V2 w h }) =
  let dw = 320; dh = 400
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Search/filter input inside the world load dialog.
worldLoadFilterRect :: Layout -> Rect
worldLoadFilterRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | Scrollable list area inside the world load dialog.
worldLoadListRect :: Layout -> Rect
worldLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 72), V2 (dw - pad * 2) 260)

-- | Individual item rect inside the world load list.
worldLoadItemRect :: Layout -> Int -> Rect
worldLoadItemRect layout index =
  let Rect (V2 lx ly, V2 lw _) = worldLoadListRect layout
      itemH = 28
  in Rect (V2 lx (ly + index * itemH), V2 lw itemH)

-- | \"Load\" button in the world load dialog.
worldLoadOkRect :: Layout -> Rect
worldLoadOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = worldLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the world load dialog.
worldLoadCancelRect :: Layout -> Rect
worldLoadCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = worldLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- | Checkbox rect for a pipeline stage toggle at the given row index.
--
-- Returns a 16x16 checkbox rect positioned inside the config scroll area.
pipelineCheckboxRect :: Int -> Layout -> Rect
pipelineCheckboxRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      gap = 10
      pad = 12
      checkboxSize = 16
      checkY = rowY + (rowHeight - checkboxSize) `div` 2
  in Rect (V2 (x + pad) checkY, V2 checkboxSize checkboxSize)

-- | Move-up arrow button rect for a plugin at the given pipeline row index.
--
-- Returns a 14x14 rect positioned to the right of the checkbox area.
pipelineMoveUpRect :: Int -> Layout -> Rect
pipelineMoveUpRect index layout =
  let Rect (V2 x rowY, V2 w rowHeight) = configScrollRowRect index layout
      btnSize = 14
      -- Position at the right side of the row, leaving room for two buttons
      rightPad = 12
      btnY = rowY + (rowHeight - btnSize) `div` 2
      btnX = x + w - rightPad - btnSize * 2 - 4
  in Rect (V2 btnX btnY, V2 btnSize btnSize)

-- | Move-down arrow button rect for a plugin at the given pipeline row index.
--
-- Returns a 14x14 rect positioned to the right of the move-up button.
pipelineMoveDownRect :: Int -> Layout -> Rect
pipelineMoveDownRect index layout =
  let Rect (V2 x rowY, V2 w rowHeight) = configScrollRowRect index layout
      btnSize = 14
      rightPad = 12
      btnY = rowY + (rowHeight - btnSize) `div` 2
      btnX = x + w - rightPad - btnSize
  in Rect (V2 btnX btnY, V2 btnSize btnSize)

-- | Tick button rect for the simulation controls inside the pipeline tab.
pipelineTickButtonRect :: Int -> Layout -> Rect
pipelineTickButtonRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      pad = 12
  in Rect (V2 (x + pad) rowY, V2 60 rowHeight)

-- | Tick-rate slider bar rect for the simulation controls inside the pipeline tab.
pipelineTickRateBarRect :: Int -> Layout -> Rect
pipelineTickRateBarRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      pad = 12
      barW = 120
  in Rect (V2 (x + pad) (rowY + 4), V2 barW (rowHeight - 8))

-- | Expand/collapse toggle rect for a plugin at the given pipeline row index.
--
-- Returns a 12x12 rect positioned to the left of the move-up button.
pipelineExpandRect :: Int -> Layout -> Rect
pipelineExpandRect index layout =
  let Rect (V2 x rowY, V2 w rowHeight) = configScrollRowRect index layout
      expandSize = 12
      -- Position just to the left of the move-up button area
      rightPad = 12
      btnSize = 14
      expandX = x + w - rightPad - btnSize * 2 - 4 - expandSize - 6
      expandY = rowY + (rowHeight - expandSize) `div` 2
  in Rect (V2 expandX expandY, V2 expandSize expandSize)

-- | Slider bar rect for a plugin parameter at the given pipeline row index.
--
-- Indented to visually nest under the plugin row, spanning most of the row width.
pipelineParamBarRect :: Int -> Layout -> Rect
pipelineParamBarRect index layout =
  let Rect (V2 x rowY, V2 w rowHeight) = configScrollRowRect index layout
      indent = 36
      rightPad = 12
      barW = w - indent - rightPad
  in Rect (V2 (x + indent) (rowY + 4), V2 barW (rowHeight - 8))

-- | Checkbox rect for a boolean plugin parameter at the given pipeline row index.
pipelineParamCheckRect :: Int -> Layout -> Rect
pipelineParamCheckRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      indent = 36
      checkboxSize = 16
      checkY = rowY + (rowHeight - checkboxSize) `div` 2
  in Rect (V2 (x + indent) checkY, V2 checkboxSize checkboxSize)

-- | Clickable item rect for a data browser row at the given row index.
--
-- Spans most of the row width with a small left pad.
dataBrowserItemRect :: Int -> Layout -> Rect
dataBrowserItemRect index layout =
  let Rect (V2 x rowY, V2 w rowHeight) = configScrollRowRect index layout
      pad = 8
  in Rect (V2 (x + pad) rowY, V2 (w - pad * 2) rowHeight)

-- | Previous-page button rect for the data browser, placed at the given row index.
dataBrowserPagePrevRect :: Int -> Layout -> Rect
dataBrowserPagePrevRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      pad = 8
      btnW = 40
      btnH = min 20 rowHeight
      btnY = rowY + (rowHeight - btnH) `div` 2
  in Rect (V2 (x + pad) btnY, V2 btnW btnH)

-- | Next-page button rect for the data browser, placed at the given row index.
dataBrowserPageNextRect :: Int -> Layout -> Rect
dataBrowserPageNextRect index layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect index layout
      pad = 8
      btnW = 40
      btnH = min 20 rowHeight
      btnY = rowY + (rowHeight - btnH) `div` 2
  in Rect (V2 (x + pad + btnW + 8) btnY, V2 btnW btnH)

-- | Detail popover rect for a selected data browser record.
--
-- Floats to the left of the config panel, vertically anchored to the
-- selected row.  The @fieldCount@ determines the popover height
-- (clamped to the available screen space).
dataDetailPopoverRect :: Int -> Int -> Layout -> Rect
dataDetailPopoverRect rowIndex fieldCount layout =
  let V2 _winW winH = layoutSize layout
      Rect (V2 cfgX _, V2 _ _) = configPanelRect layout
      Rect (V2 _ rowY, V2 _ _) = configScrollRowRect rowIndex layout
      popW = 280
      rowH = 22
      headerH = 28
      padding = 8
      desiredH = headerH + fieldCount * rowH + padding * 2
      maxH = winH - topBarHeight - 32
      popH = max 60 (min desiredH maxH)
      gap = 8
      popX = max 0 (cfgX - popW - gap)
      -- Centre on the anchor row, clamped to screen.
      rawY = rowY - popH `div` 2 + 12
      popY = max (topBarHeight + 8) (min rawY (winH - popH - 8))
  in Rect (V2 popX popY, V2 popW popH)

-- | Individual field row rect inside the detail popover.
dataDetailFieldRect :: Int -> Int -> Int -> Layout -> Rect
dataDetailFieldRect rowIndex fieldCount fieldIndex layout =
  let Rect (V2 px py, V2 pw _ph) = dataDetailPopoverRect rowIndex fieldCount layout
      headerH = 28
      rowH = 22
      padding = 8
  in Rect (V2 (px + padding) (py + headerH + fieldIndex * rowH), V2 (pw - padding * 2) rowH)

-- | Create-new-record button below the record list in the data browser.
dataBrowserCreateButtonRect :: Int -> Layout -> Rect
dataBrowserCreateButtonRect rowIndex layout =
  let Rect (V2 x rowY, V2 _w rowHeight) = configScrollRowRect rowIndex layout
      pad = 8
      btnW = 28
      btnH = min 20 rowHeight
      btnY = rowY + (rowHeight - btnH) `div` 2
  in Rect (V2 (x + pad) btnY, V2 btnW btnH)

-- | Edit/pencil toggle button in the detail popover header (right side).
dataDetailEditToggleRect :: Int -> Int -> Layout -> Rect
dataDetailEditToggleRect rowIndex fieldCount layout =
  let Rect (V2 px py, V2 pw _ph) = dataDetailPopoverRect rowIndex fieldCount layout
      btnW = 28
      btnH = 20
      gap = 4
  in Rect (V2 (px + pw - btnW - gap) (py + 4), V2 btnW btnH)

-- | Save button in the detail popover header (edit/create mode).
dataDetailSaveRect :: Int -> Int -> Layout -> Rect
dataDetailSaveRect rowIndex fieldCount layout =
  let Rect (V2 px py, V2 pw _ph) = dataDetailPopoverRect rowIndex fieldCount layout
      btnW = 40
      btnH = 20
      gap = 4
      editRect = dataDetailEditToggleRect rowIndex fieldCount layout
      Rect (V2 ex _, _) = editRect
  in Rect (V2 (ex - btnW - gap) (py + 4), V2 btnW btnH)

-- | Cancel button in the detail popover header (edit/create mode).
dataDetailCancelRect :: Int -> Int -> Layout -> Rect
dataDetailCancelRect rowIndex fieldCount layout =
  let Rect (V2 _px py, V2 _pw _ph) = dataDetailPopoverRect rowIndex fieldCount layout
      btnW = 48
      btnH = 20
      gap = 4
      saveRect = dataDetailSaveRect rowIndex fieldCount layout
      Rect (V2 sx _, _) = saveRect
  in Rect (V2 (sx - btnW - gap) (py + 4), V2 btnW btnH)

-- | Delete button in the detail popover header (left of edit toggle).
dataDetailDeleteRect :: Int -> Int -> Layout -> Rect
dataDetailDeleteRect rowIndex fieldCount layout =
  let Rect (V2 px py, V2 _pw _ph) = dataDetailPopoverRect rowIndex fieldCount layout
      btnW = 28
      btnH = 20
      gap = 4
  in Rect (V2 (px + gap) (py + 4), V2 btnW btnH)

-- | Input area for a field (value editing area — right half of a field row).
dataDetailFieldInputRect :: Int -> Int -> Int -> Layout -> Rect
dataDetailFieldInputRect rowIndex fieldCount fieldIndex layout =
  let Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect rowIndex fieldCount fieldIndex layout
      valX = fx + fw `div` 2
      valW = fw - fw `div` 2
  in Rect (V2 valX fy, V2 valW fh)

-- | Minus stepper button for a numeric field in edit mode.
dataDetailFieldStepMinusRect :: Int -> Int -> Int -> Layout -> Rect
dataDetailFieldStepMinusRect rowIndex fieldCount fieldIndex layout =
  let Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect rowIndex fieldCount fieldIndex layout
      valX = fx + fw `div` 2
      btnW = 20
      btnH = min 18 fh
      btnY = fy + (fh - btnH) `div` 2
  in Rect (V2 valX btnY, V2 btnW btnH)

-- | Plus stepper button for a numeric field in edit mode.
dataDetailFieldStepPlusRect :: Int -> Int -> Int -> Layout -> Rect
dataDetailFieldStepPlusRect rowIndex fieldCount fieldIndex layout =
  let Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect rowIndex fieldCount fieldIndex layout
      btnW = 20
      btnH = min 18 fh
      btnY = fy + (fh - btnH) `div` 2
  in Rect (V2 (fx + fw - btnW) btnY, V2 btnW btnH)

-- | Delete confirmation dialog (centered on screen).
deleteConfirmDialogRect :: Layout -> Rect
deleteConfirmDialogRect (Layout { layoutSize = V2 winW winH }) =
  let dlgW = 260
      dlgH = 100
      dx = (winW - dlgW) `div` 2
      dy = (winH - dlgH) `div` 2
  in Rect (V2 dx dy, V2 dlgW dlgH)

-- | OK button in the delete confirmation dialog.
deleteConfirmOkRect :: Layout -> Rect
deleteConfirmOkRect layout =
  let Rect (V2 dx dy, V2 dw dh) = deleteConfirmDialogRect layout
      btnW = 60
      btnH = 28
      gap = 12
      btnY = dy + dh - btnH - gap
      btnX = dx + dw `div` 2 - btnW - 6
  in Rect (V2 btnX btnY, V2 btnW btnH)

-- | Cancel button in the delete confirmation dialog.
deleteConfirmCancelRect :: Layout -> Rect
deleteConfirmCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = deleteConfirmDialogRect layout
      btnW = 60
      btnH = 28
      gap = 12
      btnY = dy + dh - btnH - gap
      btnX = dx + dw `div` 2 + 6
  in Rect (V2 btnX btnY, V2 btnW btnH)

------------------------------------------------------------------------
-- Editor toolbar
------------------------------------------------------------------------

-- | Number of tool buttons in the editor toolbar.
editorToolButtonCount :: Int
editorToolButtonCount = length ([minBound .. maxBound] :: [EditorTool])

-- | Full editor toolbar rect anchored at top-center of window.
editorToolbarRect :: Layout -> Rect
editorToolbarRect layout =
  editorToolbarBounds (uiEditorGeometry (layoutGeometry layout))

-- | Rect for the n-th tool button (0-indexed) inside the editor toolbar.
editorToolButtonRect :: Int -> Layout -> Rect
editorToolButtonRect idx layout =
  case (idx >= 0, drop idx (editorToolButtonBounds (uiEditorGeometry (layoutGeometry layout)))) of
    (True, buttonRect : _) -> buttonRect
    _ ->
      let Rect (V2 x y, V2 _ h) = editorToolbarRect layout
          pad = 12
          btnW = 64
          gap = 4
          insetY = 4
          btnH = h - insetY * 2
          btnX = x + pad + idx * (btnW + gap)
      in Rect (V2 btnX (y + insetY), V2 btnW btnH)

-- | Minus button for brush radius in the editor toolbar.
editorRadiusMinusRect :: Layout -> Rect
editorRadiusMinusRect layout =
  editorRadiusMinusButton (uiEditorGeometry (layoutGeometry layout))

-- | Display rect for the current brush radius value.
editorRadiusValueRect :: Layout -> Rect
editorRadiusValueRect layout =
  editorRadiusValueBounds (uiEditorGeometry (layoutGeometry layout))

-- | Plus button for brush radius in the editor toolbar.
editorRadiusPlusRect :: Layout -> Rect
editorRadiusPlusRect layout =
  editorRadiusPlusButton (uiEditorGeometry (layoutGeometry layout))

-- | Close button [X] at the right end of the editor toolbar.
editorCloseRect :: Layout -> Rect
editorCloseRect layout =
  editorCloseButton (uiEditorGeometry (layoutGeometry layout))

-- | Small reopen button shown when the editor toolbar is closed.
-- Positioned at the same Y as the toolbar, centered in the window.
editorReopenRect :: Layout -> Rect
editorReopenRect layout =
  editorReopenButton (uiEditorGeometry (layoutGeometry layout))

------------------------------------------------------------------------
-- Editor param bar
------------------------------------------------------------------------

-- | Parameter bar sitting directly below the editor toolbar.
-- Matches the toolbar's x/width; height is 30px with 2px gap.
editorParamBarRect :: Layout -> Rect
editorParamBarRect layout =
  editorParamBarBounds (uiEditorGeometry (layoutGeometry layout))

-- | Numeric control (−/value/+) rects for param-bar slot @n@ (0-indexed).
-- Returns @(minusRect, valueRect, plusRect)@.
editorParamNumericRects :: Int -> Layout -> (Rect, Rect, Rect)
editorParamNumericRects slot layout =
  let Rect (V2 bx by, V2 _ bh) = editorParamBarRect layout
      pad    = 12
      btnW   = 24
      valW   = 56
      gap    = 4
      slotW  = btnW + gap + valW + gap + btnW
      slotGap = 8
      x0    = bx + pad + slot * (slotW + slotGap)
      insetY = 4
      btnH  = bh - insetY * 2
      y0    = by + insetY
      minR  = Rect (V2 x0 y0, V2 btnW btnH)
      valR  = Rect (V2 (x0 + btnW + gap) y0, V2 valW btnH)
      plusR = Rect (V2 (x0 + btnW + gap + valW + gap) y0, V2 btnW btnH)
  in (minR, valR, plusR)

-- | Cycle selector (◄/label/►) rects for param-bar slot @n@ (0-indexed).
-- Returns @(prevRect, labelRect, nextRect)@.
editorParamCycleRects :: Int -> Layout -> (Rect, Rect, Rect)
editorParamCycleRects slot layout =
  let Rect (V2 bx by, V2 _ bh) = editorParamBarRect layout
      pad    = 12
      arrW   = 20
      lblW   = 96
      gap    = 4
      cycleW = arrW + gap + lblW + gap + arrW
      cycleGap = 8
      x0    = bx + pad + slot * (cycleW + cycleGap)
      insetY = 4
      btnH  = bh - insetY * 2
      y0    = by + insetY
      prevR = Rect (V2 x0 y0, V2 arrW btnH)
      lblR  = Rect (V2 (x0 + arrW + gap) y0, V2 lblW btnH)
      nextR = Rect (V2 (x0 + arrW + gap + lblW + gap) y0, V2 arrW btnH)
  in (prevR, lblR, nextR)

-- | Falloff cycle selector rects, right-justified in the param bar.
-- Returns @(prevRect, labelRect, nextRect)@.
editorParamFalloffRects :: Layout -> (Rect, Rect, Rect)
editorParamFalloffRects layout =
  let Rect (V2 bx by, V2 bw bh) = editorParamBarRect layout
      pad    = 12
      arrW   = 20
      lblW   = 72
      gap    = 4
      cycleW = arrW + gap + lblW + gap + arrW
      x0    = bx + bw - pad - cycleW
      insetY = 4
      btnH  = bh - insetY * 2
      y0    = by + insetY
      prevR = Rect (V2 x0 y0, V2 arrW btnH)
      lblR  = Rect (V2 (x0 + arrW + gap) y0, V2 lblW btnH)
      nextR = Rect (V2 (x0 + arrW + gap + lblW + gap) y0, V2 arrW btnH)
  in (prevR, lblR, nextR)
