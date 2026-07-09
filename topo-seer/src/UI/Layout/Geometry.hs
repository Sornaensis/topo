module UI.Layout.Geometry
  ( layoutTopBarHeight
  , uiGeometryFor
  , uiGeometryForSeed
  ) where

import Linear (V2(..))
import Seer.Editor.Types (EditorTool)
import UI.Geometry
import UI.Widgets (Rect(..))

layoutTopBarHeight :: Int
layoutTopBarHeight = 28

uiGeometryFor :: WindowSize -> LogHeight -> UiGeometry
uiGeometryFor windowSize logHeight =
  uiGeometryForSeed windowSize logHeight (SeedInputWidth 120)

uiGeometryForSeed :: WindowSize -> LogHeight -> SeedInputWidth -> UiGeometry
uiGeometryForSeed windowSize logHeight seedWidth =
  let inputs = LayoutInputs
        { layoutInputWindowSize = windowSize
        , layoutInputLogHeight = logHeight
        , layoutInputSeedWidth = seedWidth
        }
      topBar = topBarGeometryFor windowSize
      leftPanel = leftPanelGeometryFor inputs
      configPanel = configPanelGeometryFor inputs leftPanel
      logPanel = logPanelGeometryFor inputs
      editor = editorGeometryFor windowSize
  in UiGeometry
       { uiGeometryInputs = inputs
       , uiTopBarGeometry = topBar
       , uiLeftPanelGeometry = leftPanel
       , uiConfigPanelGeometry = configPanel
       , uiLogPanelGeometry = logPanel
       , uiEditorGeometry = editor
       }

topBarGeometryFor :: WindowSize -> TopBarGeometry
topBarGeometryFor (WindowSize (V2 w _)) =
  TopBarGeometry
    { topBarBounds = Rect (V2 0 0, V2 w layoutTopBarHeight)
    }

leftPanelGeometryFor :: LayoutInputs -> LeftPanelGeometry
leftPanelGeometryFor inputs =
  let bounds = leftPanelBoundsFor inputs
      controlsTop = leftControlsTopFrom bounds
      chunkMinus = leftParamMinusRectFor 0 bounds controlsTop
      chunkPlus = leftParamPlusRectFor 0 bounds controlsTop
      chunkValue = leftParamBarRectFor 0 bounds controlsTop
      seedLabel = leftSeedLabelRectFor inputs bounds controlsTop
      seedRandom = leftSeedRandomRectFor bounds controlsTop
      seedValue = leftSeedValueRectFor inputs bounds controlsTop
      viewButtons = leftViewRectsFor bounds controlsTop
      overlayButtons = overlayViewGeometryFor bounds controlsTop
      dayNight = dayNightToggleRectFor bounds controlsTop
      contentHeight = leftViewContentHeightFor
      scrollMax = leftViewScrollMaxFor bounds controlsTop contentHeight
  in LeftPanelGeometry
       { leftPanelBounds = bounds
       , leftPanelToggleButton = leftToggleRectFor bounds
       , leftPanelTabs = leftTabsFor bounds
       , leftPanelControlsTopY = controlsTop
       , leftPanelGenerateButton = leftGenerateButtonRectFor bounds controlsTop
       , leftPanelChunkMinusButton = chunkMinus
       , leftPanelChunkValueBounds = chunkValue
       , leftPanelChunkPlusButton = chunkPlus
       , leftPanelSeedLabelBounds = seedLabel
       , leftPanelSeedRandomButton = seedRandom
       , leftPanelSeedValueBounds = seedValue
       , leftPanelViewButtons = viewButtons
       , leftPanelOverlayButtons = overlayButtons
       , leftPanelDayNightToggleButton = dayNight
       , leftPanelViewContentHeightPx = contentHeight
       , leftPanelViewScrollMaxPx = scrollMax
       }

leftPanelBoundsFor :: LayoutInputs -> Rect
leftPanelBoundsFor inputs =
  let WindowSize (V2 _ h) = layoutInputWindowSize inputs
      LogHeight logHeight = layoutInputLogHeight inputs
      panelW = 240
      x = 16
      y = 56 + layoutTopBarHeight
  in Rect (V2 x y, V2 panelW (h - logHeight - 72 - layoutTopBarHeight))

leftToggleRectFor :: Rect -> Rect
leftToggleRectFor (Rect (V2 x y, _)) =
  let pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

leftTabsFor :: Rect -> PanelTabs
leftTabsFor (Rect (V2 x y, V2 w _)) =
  let pad = 12
      tabH = 22
      gap = 8
      toggleH = 22
      tabY = y + 8 + toggleH + 8
      available = w - pad * 2 - gap
      tabW = available `div` 2
      primary = Rect (V2 (x + pad) tabY, V2 tabW tabH)
      secondary = Rect (V2 (x + pad + tabW + gap) tabY, V2 tabW tabH)
  in PanelTabs primary secondary

leftControlsTopFrom :: Rect -> Int
leftControlsTopFrom (Rect (V2 _ y, _)) =
  let toggleH = 22
      tabH = 22
  in y + 8 + toggleH + 8 + tabH + 16

leftGenerateButtonRectFor :: Rect -> Int -> Rect
leftGenerateButtonRectFor (Rect (V2 x _, V2 w _)) controlsTop =
  let pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + 4 * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) 28)

leftParamMinusRectFor :: Int -> Rect -> Int -> Rect
leftParamMinusRectFor index (Rect (V2 x _, _)) controlsTop =
  let pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + index * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 24 24)

leftParamPlusRectFor :: Int -> Rect -> Int -> Rect
leftParamPlusRectFor index (Rect (V2 x _, V2 w _)) controlsTop =
  let pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + index * (rowHeight + gap)
  in Rect (V2 (x + w - pad - 24) top, V2 24 24)

leftParamBarRectFor :: Int -> Rect -> Int -> Rect
leftParamBarRectFor index (Rect (V2 x _, V2 w _)) controlsTop =
  let pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + index * (rowHeight + gap)
      barHeight = 12
      barY = top + (rowHeight - barHeight) `div` 2
      barX = x + pad + 24 + 8
      barW = w - (pad * 2 + 24 * 2 + 16)
  in Rect (V2 barX barY, V2 barW barHeight)

leftSeedLabelRectFor :: LayoutInputs -> Rect -> Int -> Rect
leftSeedLabelRectFor inputs (Rect (V2 x _, V2 w _)) controlsTop =
  let SeedInputWidth seedWidth = layoutInputSeedWidth inputs
      pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + 2 * (rowHeight + gap)
      buttonW = 64
      valueW = min seedWidth (w - pad * 2 - buttonW - 8)
  in Rect (V2 (x + pad) top, V2 valueW rowHeight)

leftSeedRandomRectFor :: Rect -> Int -> Rect
leftSeedRandomRectFor (Rect (V2 x _, V2 w _)) controlsTop =
  let pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + 2 * (rowHeight + gap)
      buttonW = 64
  in Rect (V2 (x + w - pad - buttonW) top, V2 buttonW rowHeight)

leftSeedValueRectFor :: LayoutInputs -> Rect -> Int -> Rect
leftSeedValueRectFor inputs (Rect (V2 x _, V2 w _)) controlsTop =
  let SeedInputWidth seedWidth = layoutInputSeedWidth inputs
      pad = 12
      rowHeight = 24
      gap = 10
      top = controlsTop + 3 * (rowHeight + gap)
      valueW = min seedWidth (w - pad * 2)
      valueX = x + (w - valueW) `div` 2
  in Rect (V2 valueX top, V2 valueW rowHeight)

leftViewButtonHeightFor :: Int
leftViewButtonHeightFor = 28

leftViewButtonGapFor :: Int
leftViewButtonGapFor = 8

leftViewSectionHeaderHeightFor :: Int
leftViewSectionHeaderHeightFor = 18

leftViewSectionGapFor :: Int
leftViewSectionGapFor = 8

leftViewRowStrideFor :: Int
leftViewRowStrideFor = leftViewButtonHeightFor + leftViewButtonGapFor

leftViewRowCountFor :: Int
leftViewRowCountFor = 6

leftWeatherOverlayRowCountFor :: Int
leftWeatherOverlayRowCountFor = 2

leftBaseViewTopFor :: Int -> Int
leftBaseViewTopFor controlsTop = controlsTop + leftViewSectionHeaderHeightFor

leftWeatherOverlaySectionTopFor :: Int -> Int
leftWeatherOverlaySectionTopFor controlsTop =
  leftBaseViewTopFor controlsTop + leftViewRowCountFor * leftViewRowStrideFor + leftViewSectionGapFor

leftWeatherOverlayTopFor :: Int -> Int
leftWeatherOverlayTopFor controlsTop =
  leftWeatherOverlaySectionTopFor controlsTop + leftViewSectionHeaderHeightFor

leftWeatherBasisSectionTopFor :: Int -> Int
leftWeatherBasisSectionTopFor controlsTop =
  leftWeatherOverlayTopFor controlsTop + leftWeatherOverlayRowCountFor * leftViewRowStrideFor + leftViewSectionGapFor

leftWeatherBasisTopFor :: Int -> Int
leftWeatherBasisTopFor controlsTop =
  leftWeatherBasisSectionTopFor controlsTop + leftViewSectionHeaderHeightFor

leftDayNightTopFor :: Int -> Int
leftDayNightTopFor controlsTop =
  leftWeatherBasisTopFor controlsTop + leftViewRowStrideFor

leftPluginOverlaySectionTopFor :: Int -> Int
leftPluginOverlaySectionTopFor controlsTop =
  leftDayNightTopFor controlsTop + leftViewRowStrideFor + leftViewSectionGapFor

leftPluginOverlayTopFor :: Int -> Int
leftPluginOverlayTopFor controlsTop =
  leftPluginOverlaySectionTopFor controlsTop + leftViewSectionHeaderHeightFor

leftOverlayActionTopFor :: Int -> Int
leftOverlayActionTopFor controlsTop =
  leftPluginOverlayTopFor controlsTop + 2 * leftViewRowStrideFor + leftViewSectionGapFor

leftViewContentHeightFor :: Int
leftViewContentHeightFor =
  let actionRow2Top = leftOverlayActionTopFor 0 + 2 * leftViewRowStrideFor
      actionBottom = actionRow2Top + leftViewButtonHeightFor
  in actionBottom + leftViewButtonGapFor

leftViewScrollMaxFor :: Rect -> Int -> Int -> Int
leftViewScrollMaxFor (Rect (V2 _ panelY, V2 _ panelH)) controlsTop contentHeight =
  let usable = panelH - (controlsTop - panelY)
  in max 0 (contentHeight - usable)

leftViewRectsFor :: Rect -> Int -> [Rect]
leftViewRectsFor bounds controlsTop =
  leftTwoColumnRectsFor bounds 12 (leftBaseViewTopFor controlsTop)

leftTwoColumnRectsFor :: Rect -> Int -> Int -> [Rect]
leftTwoColumnRectsFor (Rect (V2 x _, V2 w _)) count top =
  let pad = 12
      buttonW = (w - pad * 2 - leftViewButtonGapFor) `div` 2
      gridPos i = (i `div` 2, i `mod` 2)
      rect (row, col) = Rect
        ( V2 (x + pad + col * (buttonW + leftViewButtonGapFor))
             (top + row * leftViewRowStrideFor)
        , V2 buttonW leftViewButtonHeightFor
        )
  in map (rect . gridPos) [0 .. count - 1]

overlayViewGeometryFor :: Rect -> Int -> OverlayViewGeometry
overlayViewGeometryFor bounds controlsTop =
  case leftTwoColumnRectsFor bounds 4 (leftPluginOverlayTopFor controlsTop) of
    [overlayPrev, overlayNext, fieldPrev, fieldNext] ->
      OverlayViewGeometry overlayPrev overlayNext fieldPrev fieldNext
    _ -> error "overlayViewGeometryFor: expected four rects"

dayNightToggleRectFor :: Rect -> Int -> Rect
dayNightToggleRectFor (Rect (V2 x _, V2 w _)) controlsTop =
  let pad = 12
      buttonW = w - pad * 2
      rowY = leftDayNightTopFor controlsTop
  in Rect (V2 (x + pad) rowY, V2 buttonW leftViewButtonHeightFor)

configPanelGeometryFor :: LayoutInputs -> LeftPanelGeometry -> ConfigPanelGeometry
configPanelGeometryFor inputs leftGeometry =
  let bounds = configPanelBoundsFor inputs (leftPanelBounds leftGeometry)
      toggle = configToggleRectFor bounds
      tabs = configTabsFor bounds
      save = configPresetSaveRectFor bounds
      load = configPresetLoadRectFor bounds
      reset = configResetRectFor bounds
      revert = configRevertRectFor bounds
      scrollArea = configScrollAreaRectFor bounds save
      scrollBar = configScrollBarRectFor scrollArea
  in ConfigPanelGeometry
       { configPanelBounds = bounds
       , configPanelToggleButton = toggle
       , configPanelTabs = tabs
       , configPresetSaveButton = save
       , configPresetLoadButton = load
       , configResetButton = reset
       , configRevertButton = revert
       , configScrollAreaBounds = scrollArea
       , configScrollBarBounds = scrollBar
       }

configPanelBoundsFor :: LayoutInputs -> Rect -> Rect
configPanelBoundsFor inputs leftBounds =
  let WindowSize (V2 w h) = layoutInputWindowSize inputs
      LogHeight logHeight = layoutInputLogHeight inputs
      SeedInputWidth seedWidth = layoutInputSeedWidth inputs
      pad = 12
      buttonW = 64
      minW = 300
      desiredW = max minW (pad * 2 + seedWidth + buttonW + 20)
      leftBound = rectLeft leftBounds + rectWidth leftBounds + 8
      rawPanelX = w - desiredW - 16
      panelX = max leftBound rawPanelX
      actualW = max 0 (w - panelX - 16)
  in Rect (V2 panelX (16 + layoutTopBarHeight), V2 actualW (h - logHeight - 32 - layoutTopBarHeight))

configToggleRectFor :: Rect -> Rect
configToggleRectFor (Rect (V2 x y, _)) =
  let pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

configTabsFor :: Rect -> ConfigTabs
configTabsFor (Rect (V2 x y, V2 w _)) =
  let pad = 12
      tabH = 22
      gapX = 4
      gapY = 6
      toggleH = 22
      available = w - pad * 2 - gapX * 3
      tabW = available `div` 4
      y0 = y + 8 + toggleH + 8
      y1 = y0 + tabH + gapY
      mk row col = Rect (V2 (x + pad + col * (tabW + gapX)) (if row == (0 :: Int) then y0 else y1), V2 tabW tabH)
  in ConfigTabs
       (mk 0 0) (mk 0 1) (mk 0 2) (mk 0 3)
       (mk 1 0) (mk 1 1) (mk 1 2) (mk 1 3)

configPresetSaveRectFor :: Rect -> Rect
configPresetSaveRectFor (Rect (V2 x y, V2 w h)) =
  Rect (V2 (x + 12) (y + h - 136), V2 (w - 24) 24)

configPresetLoadRectFor :: Rect -> Rect
configPresetLoadRectFor (Rect (V2 x y, V2 w h)) =
  Rect (V2 (x + 12) (y + h - 104), V2 (w - 24) 24)

configResetRectFor :: Rect -> Rect
configResetRectFor (Rect (V2 x y, V2 w h)) =
  Rect (V2 (x + 12) (y + h - 72), V2 (w - 24) 24)

configRevertRectFor :: Rect -> Rect
configRevertRectFor (Rect (V2 x y, V2 w h)) =
  Rect (V2 (x + 12) (y + h - 40), V2 (w - 24) 24)

configScrollAreaRectFor :: Rect -> Rect -> Rect
configScrollAreaRectFor (Rect (V2 x y, V2 w _)) (Rect (V2 _ applyY, _)) =
  let pad = 16
      tabOffset = 96
      barW = 8
      barGap = 6
      top = y + tabOffset
      bottom = applyY - 8
      height = max 0 (bottom - top)
      innerW = max 0 (w - pad * 2 - barW - barGap)
  in Rect (V2 (x + pad) top, V2 innerW height)

configScrollBarRectFor :: Rect -> Rect
configScrollBarRectFor (Rect (V2 sx sy, V2 sw sh)) =
  let barW = 8
      barGap = 6
  in Rect (V2 (sx + sw + barGap) sy, V2 barW sh)

logPanelGeometryFor :: LayoutInputs -> LogPanelGeometry
logPanelGeometryFor inputs =
  let bounds = logPanelBoundsFor inputs
      header = logHeaderRectFor bounds
      body = logBodyRectFor bounds
      filters = logFilterButtonsFor header
  in LogPanelGeometry
       { logPanelBounds = bounds
       , logHeaderBounds = header
       , logBodyBounds = body
       , logFilterButtons = filters
       }

logPanelBoundsFor :: LayoutInputs -> Rect
logPanelBoundsFor inputs =
  let WindowSize (V2 w h) = layoutInputWindowSize inputs
      LogHeight logHeight = layoutInputLogHeight inputs
  in Rect (V2 0 (h - logHeight), V2 w logHeight)

logHeaderRectFor :: Rect -> Rect
logHeaderRectFor (Rect (V2 x y, V2 w _)) =
  Rect (V2 x y, V2 w 24)

logBodyRectFor :: Rect -> Rect
logBodyRectFor (Rect (V2 x y, V2 w h)) =
  let bodyHeight = max 0 (h - 24)
  in Rect (V2 x (y + 24), V2 w bodyHeight)

logFilterButtonsFor :: Rect -> LogFilterButtons
logFilterButtonsFor (Rect (V2 x y, V2 w h)) =
  let buttonSize = 22
      gap = 6
      total = buttonSize * 4 + gap * 3
      startX = x + w - total - 12
      y0 = y + (h - buttonSize) `div` 2
      r1 = Rect (V2 startX y0, V2 buttonSize buttonSize)
      r2 = Rect (V2 (startX + buttonSize + gap) y0, V2 buttonSize buttonSize)
      r3 = Rect (V2 (startX + (buttonSize + gap) * 2) y0, V2 buttonSize buttonSize)
      r4 = Rect (V2 (startX + (buttonSize + gap) * 3) y0, V2 buttonSize buttonSize)
  in LogFilterButtons r1 r2 r3 r4

editorGeometryFor :: WindowSize -> EditorGeometry
editorGeometryFor windowSize =
  let toolCount = editorToolButtonCountFor
      toolbar = editorToolbarRectFor toolCount windowSize
      toolButtons = map (editorToolButtonRectFor toolbar) [0 .. toolCount - 1]
      radiusMinus = editorRadiusMinusRectFor toolCount toolbar
      radiusValue = editorRadiusValueRectFor radiusMinus
      radiusPlus = editorRadiusPlusRectFor radiusValue
      closeButton = editorCloseRectFor toolbar radiusPlus
      reopenButton = editorReopenRectFor windowSize
      paramBar = editorParamBarRectFor toolbar
  in EditorGeometry
       { editorToolbarBounds = toolbar
       , editorToolButtonBounds = toolButtons
       , editorRadiusMinusButton = radiusMinus
       , editorRadiusValueBounds = radiusValue
       , editorRadiusPlusButton = radiusPlus
       , editorCloseButton = closeButton
       , editorReopenButton = reopenButton
       , editorParamBarBounds = paramBar
       }

editorToolButtonCountFor :: Int
editorToolButtonCountFor = length ([minBound .. maxBound] :: [EditorTool])

editorToolbarRectFor :: Int -> WindowSize -> Rect
editorToolbarRectFor toolCount (WindowSize (V2 w _)) =
  let toolBtnW = 64
      gap = 4
      pad = 12
      contentW = toolCount * toolBtnW
               + (toolCount - 1) * gap
               + gap + 24 + gap + 40 + gap + 24
               + gap + 28
               + pad * 2
      barH = 36
      barX = (w - contentW) `div` 2
      barY = 4 + layoutTopBarHeight
  in Rect (V2 barX barY, V2 contentW barH)

editorToolButtonRectFor :: Rect -> Int -> Rect
editorToolButtonRectFor (Rect (V2 x y, V2 _ h)) idx =
  let pad = 12
      btnW = 64
      gap = 4
      insetY = 4
      btnH = h - insetY * 2
      btnX = x + pad + idx * (btnW + gap)
  in Rect (V2 btnX (y + insetY), V2 btnW btnH)

editorRadiusMinusRectFor :: Int -> Rect -> Rect
editorRadiusMinusRectFor toolCount (Rect (V2 x y, V2 _ h)) =
  let pad = 12
      btnW = 64
      gap = 4
      insetY = 4
      btnH = h - insetY * 2
      afterTools = x + pad + toolCount * (btnW + gap)
  in Rect (V2 (afterTools + gap) (y + insetY), V2 24 btnH)

editorRadiusValueRectFor :: Rect -> Rect
editorRadiusValueRectFor (Rect (V2 rx ry, V2 _ rh)) =
  Rect (V2 (rx + 24 + 4) ry, V2 40 rh)

editorRadiusPlusRectFor :: Rect -> Rect
editorRadiusPlusRectFor (Rect (V2 rx ry, V2 _ rh)) =
  Rect (V2 (rx + 40 + 4) ry, V2 24 rh)

editorCloseRectFor :: Rect -> Rect -> Rect
editorCloseRectFor (Rect (V2 _ y, V2 _ h)) (Rect (V2 rx _, V2 _ _)) =
  let insetY = 4
      btnH = h - insetY * 2
  in Rect (V2 (rx + 24 + 4) (y + insetY), V2 28 btnH)

editorReopenRectFor :: WindowSize -> Rect
editorReopenRectFor (WindowSize (V2 w _)) =
  let btnW = 64
      btnH = 28
      btnX = (w - btnW) `div` 2
      btnY = 4 + layoutTopBarHeight
  in Rect (V2 btnX btnY, V2 btnW btnH)

editorParamBarRectFor :: Rect -> Rect
editorParamBarRectFor (Rect (V2 bx by, V2 bw bh)) =
  Rect (V2 bx (by + bh + 2), V2 bw 30)
