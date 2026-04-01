module UI.Layout
  ( Layout
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
  ) where

import Linear (V2(..))
import Seer.Editor.Types (EditorTool)
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
  } deriving (Eq, Show)

-- | Height of the top bar in pixels.
topBarHeight :: Int
topBarHeight = 28

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
topBarRect (Layout (V2 w _) _ _) =
  Rect (V2 0 0, V2 w topBarHeight)

layoutFor :: V2 Int -> Int -> Layout
layoutFor size logHeight = layoutForSeed size logHeight 120

layoutForSeed :: V2 Int -> Int -> Int -> Layout
layoutForSeed size logHeight seedWidth = Layout
  { layoutSize = size
  , layoutLogHeight = logHeight
  , layoutSeedWidth = seedWidth
  }

-- | Generate button inside the left panel, below seed value (Row 4).
leftGenButtonRect :: Layout -> Rect
leftGenButtonRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 4 * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) 28)

leftPanelRect :: Layout -> Rect
leftPanelRect (Layout (V2 _ h) logHeight _) =
  let panelW = 240
      x = 16
      y = 56 + topBarHeight
  in Rect (V2 x y, V2 panelW (h - logHeight - 72 - topBarHeight))

leftToggleRect :: Layout -> Rect
leftToggleRect layout =
  let Rect (V2 x y, V2 _ _) = leftPanelRect layout
      pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

leftTabRects :: Layout -> (Rect, Rect)
leftTabRects layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      tabH = 22
      gap = 8
      toggleH = 22
      tabY = y + 8 + toggleH + 8
      available = w - pad * 2 - gap
      tabW = available `div` 2
      r1 = Rect (V2 (x + pad) tabY, V2 tabW tabH)
      r2 = Rect (V2 (x + pad + tabW + gap) tabY, V2 tabW tabH)
  in (r1, r2)

configToggleRect :: Layout -> Rect
configToggleRect layout =
  let Rect (V2 x y, V2 _ _) = configPanelRect layout
      pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

configPanelRect :: Layout -> Rect
configPanelRect layout =
  let Layout (V2 w h) logHeight seedWidth = layout
      pad = 12
      buttonW = 64
      minW = 300
      desiredW = max minW (pad * 2 + seedWidth + buttonW + 20)
      -- Clamp the left edge so the config panel never overlaps the left panel.
      Rect (V2 lpx _, V2 lpw _) = leftPanelRect layout
      leftBound = lpx + lpw + 8
      rawPanelX = w - desiredW - 16
      panelX = max leftBound rawPanelX
      actualW = max 0 (w - panelX - 16)
  in Rect (V2 panelX (16 + topBarHeight), V2 actualW (h - logHeight - 32 - topBarHeight))

configTabRects :: Layout -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
configTabRects layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      pad = 12
      tabH = 22
      gapX = 4
      gapY = 6
      toggleH = 22
      available = w - pad * 2 - gapX * 3
      tabW = available `div` 4
      y0 = y + 8 + toggleH + 8
      y1 = y0 + tabH + gapY
      mk row col = Rect (V2 (x + pad + col * (tabW + gapX)) (if row == (0 :: Int) then y0 else y1), V2 tabW tabH)
  in ( mk 0 0, mk 0 1, mk 0 2, mk 0 3
     , mk 1 0, mk 1 1, mk 1 2, mk 1 3
     )

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
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 136), V2 (w - 24) 24)

-- | Config preset load button (second in 4-button stack).
configPresetLoadRect :: Layout -> Rect
configPresetLoadRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 104), V2 (w - 24) 24)

-- | Config reset button (third in 4-button stack).
configResetRect :: Layout -> Rect
configResetRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 72), V2 (w - 24) 24)

-- | Config revert button (bottom of 4-button stack).
configRevertRect :: Layout -> Rect
configRevertRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 40), V2 (w - 24) 24)

configScrollAreaRect :: Layout -> Rect
configScrollAreaRect layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      Rect (V2 _ applyY, V2 _ _) = configPresetSaveRect layout
      pad = 16
      -- Account for two rows of tabs: toggle(22) + 8 + tabRow1(22) + gapY(6) + tabRow2(22) + 8
      tabOffset = 96
      barW = 8
      barGap = 6
      top = y + tabOffset
      bottom = applyY - 8
      height = max 0 (bottom - top)
      innerW = max 0 (w - pad * 2 - barW - barGap)
  in Rect (V2 (x + pad) top, V2 innerW height)

configScrollBarRect :: Layout -> Rect
configScrollBarRect layout =
  let Rect (V2 sx sy, V2 sw sh) = configScrollAreaRect layout
      barW = 8
      barGap = 6
  in Rect (V2 (sx + sw + barGap) sy, V2 barW sh)

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
  let Rect (V2 _ y, V2 _ _) = leftPanelRect layout
      toggleH = 22
      tabH = 22
  in y + 8 + toggleH + 8 + tabH + 16

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
leftChunkMinusRect = leftParamMinusRect 0

leftChunkPlusRect :: Layout -> Rect
leftChunkPlusRect = leftParamPlusRect 0

leftChunkValueRect :: Layout -> Rect
leftChunkValueRect = leftParamBarRect 0

leftSeedMinusRect :: Layout -> Rect
leftSeedMinusRect = leftParamMinusRect 1

leftSeedPlusRect :: Layout -> Rect
leftSeedPlusRect = leftParamPlusRect 1

leftSeedLabelRect :: Layout -> Rect
leftSeedLabelRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 2 * (rowHeight + gap)
      buttonW = 64
      valueW = min (layoutSeedWidth layout) (w - pad * 2 - buttonW - 8)
  in Rect (V2 (x + pad) top, V2 valueW rowHeight)

leftSeedRandomRect :: Layout -> Rect
leftSeedRandomRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 2 * (rowHeight + gap)
      buttonW = 64
  in Rect (V2 (x + w - pad - buttonW) top, V2 buttonW rowHeight)

leftSeedValueRect :: Layout -> Rect
leftSeedValueRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 3 * (rowHeight + gap)
      valueW = min (layoutSeedWidth layout) (w - pad * 2)
      valueX = x + (w - valueW) `div` 2
  in Rect (V2 valueX top, V2 valueW rowHeight)

-- | Number of rows used by view-mode buttons (ceil(15/2) = 8).
-- | Total pixel height of the content in the left View tab
-- (view mode buttons + overlay selector rows).
leftViewContentHeight :: Layout -> Int
leftViewContentHeight layout =
  let buttonH = 28
      gap = 8
  in leftViewRowCount * (buttonH + gap) + 2 * (buttonH + gap)

-- | Maximum scroll offset for the left View tab content.
-- Returns 0 when the content fits within the panel.
leftViewScrollMax :: Layout -> Int
leftViewScrollMax layout =
  let Rect (V2 _ panelY, V2 _ panelH) = leftPanelRect layout
      controlsTop = leftControlsTop layout
      usable = panelH - (controlsTop - panelY)
  in max 0 (leftViewContentHeight layout - usable)

leftViewRowCount :: Int
leftViewRowCount = 8

leftViewRects :: Layout -> [Rect]
leftViewRects layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      gap = 8
      buttonH = 28
      buttonW = (w - pad * 2 - gap) `div` 2
      top = leftControlsTop layout
      viewCount = 15
      gridPos i = (i `div` 2, i `mod` 2)
      rect (row, col) = Rect (V2 (x + pad + col * (buttonW + gap)) (top + row * (buttonH + gap)), V2 buttonW buttonH)
  in map (rect . gridPos) [0 .. viewCount - 1]

-- | Overlay selector button rects in the left View panel.
--
-- Returns @(overlayPrev, overlayNext, fieldPrev, fieldNext)@ positioned
-- below the view mode buttons.
overlayViewRects :: Layout -> (Rect, Rect, Rect, Rect)
overlayViewRects layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      gap = 8
      buttonH = 28
      buttonW = (w - pad * 2 - gap) `div` 2
      top = leftControlsTop layout
      rowAfterViews = top + (buttonH + gap) * leftViewRowCount
      row2Y = rowAfterViews + (buttonH + gap)
      oPrev = Rect (V2 (x + pad) rowAfterViews, V2 buttonW buttonH)
      oNext = Rect (V2 (x + pad + buttonW + gap) rowAfterViews, V2 buttonW buttonH)
      fPrev = Rect (V2 (x + pad) row2Y, V2 buttonW buttonH)
      fNext = Rect (V2 (x + pad + buttonW + gap) row2Y, V2 buttonW buttonH)
  in (oPrev, oNext, fPrev, fNext)

logPanelRect :: Layout -> Rect
logPanelRect (Layout (V2 w h) logHeight _) =
  Rect (V2 0 (h - logHeight), V2 w logHeight)

logHeaderRect :: Layout -> Rect
logHeaderRect layout =
  let Rect (V2 x y, V2 w _) = logPanelRect layout
  in Rect (V2 x y, V2 w 24)

logBodyRect :: Layout -> Rect
logBodyRect layout =
  let Rect (V2 x y, V2 w h) = logPanelRect layout
      bodyHeight = max 0 (h - 24)
  in Rect (V2 x (y + 24), V2 w bodyHeight)

logFilterRects :: Layout -> (Rect, Rect, Rect, Rect)
logFilterRects layout =
  let Rect (V2 x y, V2 w h) = logHeaderRect layout
      buttonSize = 22
      gap = 6
      total = buttonSize * 4 + gap * 3
      startX = x + w - total - 12
      y0 = y + (h - buttonSize) `div` 2
      r1 = Rect (V2 startX y0, V2 buttonSize buttonSize)
      r2 = Rect (V2 (startX + buttonSize + gap) y0, V2 buttonSize buttonSize)
      r3 = Rect (V2 (startX + (buttonSize + gap) * 2) y0, V2 buttonSize buttonSize)
      r4 = Rect (V2 (startX + (buttonSize + gap) * 3) y0, V2 buttonSize buttonSize)
  in (r1, r2, r3, r4)

menuPanelRect :: Layout -> Rect
menuPanelRect (Layout (V2 w h) _ _) =
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
presetSaveDialogRect (Layout (V2 w h) _ _) =
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
presetLoadDialogRect (Layout (V2 w h) _ _) =
  let dw = 280; dh = 320
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Scrollable list area inside the preset load dialog.
presetLoadListRect :: Layout -> Rect
presetLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 200)

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
worldSaveDialogRect (Layout (V2 w h) _ _) =
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

-- | Centered overlay for the world load dialog (320 × 360).
worldLoadDialogRect :: Layout -> Rect
worldLoadDialogRect (Layout (V2 w h) _ _) =
  let dw = 320; dh = 360
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Scrollable list area inside the world load dialog.
worldLoadListRect :: Layout -> Rect
worldLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 240)

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

------------------------------------------------------------------------
-- Editor toolbar
------------------------------------------------------------------------

-- | Number of tool buttons in the editor toolbar.
editorToolButtonCount :: Int
editorToolButtonCount = length ([minBound .. maxBound] :: [EditorTool])

-- | Full editor toolbar rect anchored at top-center of window.
editorToolbarRect :: Layout -> Rect
editorToolbarRect (Layout (V2 w _) _ _) =
  let toolBtnW = 64
      gap = 4
      pad = 12
      -- tool buttons + radius controls + close + padding
      contentW = editorToolButtonCount * toolBtnW
               + (editorToolButtonCount - 1) * gap
               + gap + 24 + gap + 40 + gap + 24  -- [−] value [+]
               + gap + 28                         -- [X] close
               + pad * 2
      barH = 36
      barX = (w - contentW) `div` 2
      barY = 4 + topBarHeight
  in Rect (V2 barX barY, V2 contentW barH)

-- | Rect for the n-th tool button (0-indexed) inside the editor toolbar.
editorToolButtonRect :: Int -> Layout -> Rect
editorToolButtonRect idx layout =
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
  let Rect (V2 x y, V2 _ h) = editorToolbarRect layout
      pad = 12
      btnW = 64
      gap = 4
      insetY = 4
      btnH = h - insetY * 2
      afterTools = x + pad + editorToolButtonCount * (btnW + gap)
  in Rect (V2 (afterTools + gap) (y + insetY), V2 24 btnH)

-- | Display rect for the current brush radius value.
editorRadiusValueRect :: Layout -> Rect
editorRadiusValueRect layout =
  let Rect (V2 rx ry, V2 _ rh) = editorRadiusMinusRect layout
  in Rect (V2 (rx + 24 + 4) ry, V2 40 rh)

-- | Plus button for brush radius in the editor toolbar.
editorRadiusPlusRect :: Layout -> Rect
editorRadiusPlusRect layout =
  let Rect (V2 rx ry, V2 _ rh) = editorRadiusValueRect layout
  in Rect (V2 (rx + 40 + 4) ry, V2 24 rh)

-- | Close button [X] at the right end of the editor toolbar.
editorCloseRect :: Layout -> Rect
editorCloseRect layout =
  let Rect (V2 _ y, V2 _ h) = editorToolbarRect layout
      Rect (V2 rx ry, V2 _ rh) = editorRadiusPlusRect layout
      insetY = 4
      btnH = h - insetY * 2
  in Rect (V2 (rx + 24 + 4) (y + insetY), V2 28 btnH)

-- | Small reopen button shown when the editor toolbar is closed.
-- Positioned at the same Y as the toolbar, centered in the window.
editorReopenRect :: Layout -> Rect
editorReopenRect (Layout (V2 w _) _ _) =
  let btnW = 64
      btnH = 28
      btnX = (w - btnW) `div` 2
      btnY = 4 + topBarHeight
  in Rect (V2 btnX btnY, V2 btnW btnH)
