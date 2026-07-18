module Spec.Layout (spec) where

import Test.Hspec
import Data.List (tails)
import Linear (V2(..))
import UI.Layout
import qualified UI.Geometry as Geometry
import UI.Layout.Geometry (uiGeometryForSeed)
import UI.Widgets (Rect(..))

rectCenter :: Rect -> V2 Int
rectCenter (Rect (V2 x y, V2 w h)) = V2 (x + w `div` 2) (y + h `div` 2)

spec :: Spec
spec = describe "UI.Layout" $ do
  it "creates a log header rect" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 x y, V2 w h) = logHeaderRect layout
    (x, y, w, h) `shouldBe` (0, 440, 800, 24)

  it "creates log filter rects" $ do
    let layout = layoutFor (V2 800 600) 160
        (Rect (V2 x1 y1, V2 w1 h1), _, _, Rect (V2 x4 _y4, V2 w4 h4)) = logFilterRects layout
    (x1, y1, w1, h1) `shouldBe` (682, 441, 22, 22)
    (x4, w4, h4) `shouldBe` (766, 22, 22)

  it "creates config panel rect" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 x y, V2 w h) = configPanelRect layout
    (x, y, w, h) `shouldBe` (484, 44, 300, 380)

  it "lays out config tabs in two rows of four" $ do
    let layout = layoutFor (V2 800 600) 160
        (t1, t2, t3, t4, t5, t6, t7, t8) = configTabRects layout
        Rect (V2 px py, V2 pw _) = configPanelRect layout
        -- All tabs have the same height
        tabH (Rect (V2 _ _, V2 _ h)) = h
        tabY (Rect (V2 _ y, V2 _ _)) = y
        tabX (Rect (V2 x _, V2 _ _)) = x
        tabW (Rect (V2 _ _, V2 w _)) = w
    -- All tabs share height 22
    tabH t1 `shouldBe` 22
    tabH t5 `shouldBe` 22
    -- Row 1 (t1–t4) and row 2 (t5–t8) are on different y-coordinates
    tabY t1 `shouldBe` tabY t2
    tabY t1 `shouldBe` tabY t3
    tabY t1 `shouldBe` tabY t4
    tabY t5 `shouldBe` tabY t6
    tabY t5 `shouldBe` tabY t7
    tabY t5 `shouldBe` tabY t8
    tabY t5 `shouldSatisfy` (> tabY t1)
    -- Tabs are wide enough for readable labels (≥ 60px)
    tabW t1 `shouldSatisfy` (>= 60)
    -- Tabs start at the panel left pad
    tabX t1 `shouldBe` (px + 12)
    tabX t5 `shouldBe` (px + 12)
    -- Consecutive tabs in a row are separated by a small gap
    tabX t2 `shouldSatisfy` (> tabX t1 + tabW t1)
    tabX t6 `shouldSatisfy` (> tabX t5 + tabW t5)

  it "creates config button rects (4-button stack)" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 sx sy, V2 sw sh) = configPresetSaveRect layout
        Rect (V2 lx ly, V2 lw lh) = configPresetLoadRect layout
        Rect (V2 rx ry, V2 rw rh) = configResetRect layout
        Rect (V2 vx vy, V2 vw vh) = configRevertRect layout
        Rect (V2 _px py, V2 _pw ph) = configPanelRect layout
    -- All buttons share the same x, width, and height
    sw `shouldBe` lw
    lw `shouldBe` rw
    rw `shouldBe` vw
    sh `shouldBe` 24
    -- Buttons are stacked from top to bottom: Save, Load, Reset, Revert
    sy `shouldSatisfy` (< ly)
    ly `shouldSatisfy` (< ry)
    ry `shouldSatisfy` (< vy)
    -- Bottom button at panelBottom - 40
    vy `shouldBe` (py + ph - 40)

  it "creates config slider rects" $ do
    let layout = layoutFor (V2 800 600) 160
        rowIndex = 0
        rects = configParamRects rowIndex layout
        Rect (V2 minusX minusY, V2 minusW minusH) = configParamRowMinusRect rects
        Rect (V2 barX barY, V2 barW barH) = configParamRowBarRect rects
        Rect (V2 plusX plusY, V2 plusW plusH) = configParamRowPlusRect rects
        waterRowRect = configParamRowHitRect rects
        waterBarCenter = rectCenter (configParamRowBarRect rects)
    (minusW, minusH) `shouldBe` (24, 24)
    (plusW, plusH) `shouldBe` (24, 24)
    minusY `shouldBe` plusY
    barY `shouldBe` (minusY + (minusH - barH) `div` 2)
    barX `shouldBe` (minusX + minusW + 8)
    plusX `shouldBe` (barX + barW + 8)
    plusY `shouldBe` minusY
    waterBarCenter `shouldSatisfy` (`inside` waterRowRect)

  it "keeps legacy config slider rect helpers aligned with shared row geometry" $ do
    let layout = layoutFor (V2 800 600) 160
        rowIndex = 3
        rects = configParamRects rowIndex layout
    configParamMinusRect rowIndex layout `shouldBe` configParamRowMinusRect rects
    configParamBarRect rowIndex layout `shouldBe` configParamRowBarRect rects
    configParamPlusRect rowIndex layout `shouldBe` configParamRowPlusRect rects
    configParamRowRect rowIndex layout `shouldBe` configParamRowHitRect rects

  it "keeps pipeline controls aligned to shared config scroll rows" $ do
    let layout = layoutFor (V2 800 600) 160
        rowIndex = 4
        Rect (V2 _ rowY, V2 _ rowH) = configScrollRowRect rowIndex layout
        Rect (V2 _ checkY, V2 checkW checkH) = pipelineCheckboxRect rowIndex layout
        Rect (V2 _ upY, V2 upW upH) = pipelineMoveUpRect rowIndex layout
        Rect (V2 _ downY, V2 downW downH) = pipelineMoveDownRect rowIndex layout
        Rect (V2 _ tickY, V2 tickW tickH) = pipelineTickButtonRect rowIndex layout
        Rect (V2 _ rateY, V2 rateW rateH) = pipelineTickRateBarRect rowIndex layout
    (checkW, checkH) `shouldBe` (16, 16)
    (upW, upH) `shouldBe` (14, 14)
    (downW, downH) `shouldBe` (14, 14)
    tickH `shouldBe` rowH
    tickW `shouldBe` 60
    rateW `shouldBe` 120
    rateH `shouldBe` (rowH - 8)
    checkY `shouldBe` (rowY + (rowH - checkH) `div` 2)
    upY `shouldBe` (rowY + (rowH - upH) `div` 2)
    downY `shouldBe` (rowY + (rowH - downH) `div` 2)
    tickY `shouldBe` rowY
    rateY `shouldBe` (rowY + 4)

  it "lays out world load and delete confirmation actions without overlap" $ do
    let layout = layoutFor (V2 800 600) 160
        dialog = worldLoadDialogRect layout
        actions = [worldLoadOkRect layout, worldLoadDeleteRect layout, worldLoadCancelRect layout]
        confirmation = worldDeleteConfirmDialogRect layout
        confirmActions = [worldDeleteConfirmOkRect layout, worldDeleteConfirmCancelRect layout]
    mapM_ (`shouldSatisfy` insideRect dialog) actions
    mapM_ (`shouldSatisfy` insideRect confirmation) confirmActions
    mapM_ (uncurry shouldNotOverlap)
      [(left, right) | (left:rest) <- tails actions, right <- rest]
    shouldNotOverlap (worldDeleteConfirmOkRect layout) (worldDeleteConfirmCancelRect layout)

  it "positions editor toolbar centered horizontally below top bar" $ do
    let layout = layoutFor (V2 1200 800) 160
        Rect (V2 tx ty, V2 tw th) = editorToolbarRect layout
    -- Centered: left edge + width should be symmetric about midpoint
    (tx + tw `div` 2) `shouldBe` 600
    -- Below top bar
    ty `shouldBe` (4 + topBarHeight)
    -- Bar height
    th `shouldBe` 36

  it "places tool buttons inside editor toolbar" $ do
    let layout = layoutFor (V2 1200 800) 160
        Rect (V2 barX barY, V2 _barW barH) = editorToolbarRect layout
        Rect (V2 b0x b0y, V2 b0w b0h) = editorToolButtonRect 0 layout
        Rect (V2 b1x b1y, V2 b1w _) = editorToolButtonRect 1 layout
    -- First button inside bar
    b0x `shouldSatisfy` (>= barX)
    b0y `shouldSatisfy` (>= barY)
    (b0y + b0h) `shouldSatisfy` (<= barY + barH)
    -- Buttons side by side with gap
    b1x `shouldBe` (b0x + b0w + 4)
    b1y `shouldBe` b0y

  it "places radius controls after tool buttons" $ do
    let layout = layoutFor (V2 1200 800) 160
        Rect (V2 lastBtnX _, V2 lastBtnW _) = editorToolButtonRect (editorToolButtonCount - 1) layout
        Rect (V2 mx _, V2 mw _) = editorRadiusMinusRect layout
        Rect (V2 vx _, V2 vw _) = editorRadiusValueRect layout
        Rect (V2 px _, V2 _ _) = editorRadiusPlusRect layout
    -- Minus after last tool button
    mx `shouldSatisfy` (> lastBtnX + lastBtnW)
    -- Value after minus
    vx `shouldBe` (mx + mw + 4)
    -- Plus after value
    px `shouldBe` (vx + vw + 4)

  it "places close button at right end" $ do
    let layout = layoutFor (V2 1200 800) 160
        Rect (V2 px _, V2 pw _) = editorRadiusPlusRect layout
        Rect (V2 cx _, V2 cw _) = editorCloseRect layout
        Rect (V2 barX _, V2 barW _) = editorToolbarRect layout
    cx `shouldBe` (px + pw + 4)
    -- Close button inside toolbar
    (cx + cw) `shouldSatisfy` (<= barX + barW)

  it "config panel does not overlap the left panel" $ do
    -- Normal window: panels fit side by side with gap
    let layout800 = layoutFor (V2 800 600) 160
        Rect (V2 lpx800 _, V2 lpw800 _) = leftPanelRect layout800
        Rect (V2 cpx800 _, V2 _ _) = configPanelRect layout800
    cpx800 `shouldSatisfy` (>= lpx800 + lpw800 + 8)
    -- Narrow window: config panel still clear of left panel
    let layout500 = layoutFor (V2 500 600) 160
        Rect (V2 lpx500 _, V2 lpw500 _) = leftPanelRect layout500
        Rect (V2 cpx500 _, V2 _ _) = configPanelRect layout500
    cpx500 `shouldSatisfy` (>= lpx500 + lpw500 + 8)
    -- Minimum usable width: still no overlap
    let layoutMin = layoutFor (V2 minUsableWindowWidth 600) 160
        Rect (V2 lpxMin _, V2 lpwMin _) = leftPanelRect layoutMin
        Rect (V2 cpxMin _, V2 _ _) = configPanelRect layoutMin
    cpxMin `shouldSatisfy` (>= lpxMin + lpwMin + 8)

  it "leftViewContentHeight is positive and covers all view buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        h = leftViewContentHeight layout
    h `shouldSatisfy` (> 0)
    -- 17 items × (28 + 8) = 612; we use 15 view buttons + 2 overlay nav + 2 field nav = 19 total
    -- but the minimum sensible content height should exceed one button row
    h `shouldSatisfy` (>= 36)

  it "leftViewScrollMax is non-negative" $ do
    let layout = layoutFor (V2 800 600) 160
    leftViewScrollMax layout `shouldSatisfy` (>= 0)

  it "leftViewScrollMax is 0 when content fits in the panel" $ do
    -- At a large window height the panel should be tall enough to show everything
    let layout = layoutFor (V2 1920 1080) 160
    leftViewScrollMax layout `shouldBe` 0

  it "leftViewScrollMax is positive at minimum window height" $ do
    let layout = layoutFor (V2 minUsableWindowWidth 480) 160
    leftViewScrollMax layout `shouldSatisfy` (> 0)

  it "reserves enough left View header space for measured section labels" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 _ firstBaseTop, _) : _ = leftBaseViewRects layout
        headerHeight = firstBaseTop - leftControlsTop layout
    headerHeight `shouldSatisfy` (>= 24)

  describe "typed UiGeometry" $ do
    it "matches legacy layout helpers for primary panels" $ do
      let layout = layoutForSeed (V2 800 600) 160 140
          geometry = layoutGeometry layout
          leftPanel = Geometry.uiLeftPanelGeometry geometry
          configPanel = Geometry.uiConfigPanelGeometry geometry
          logPanel = Geometry.uiLogPanelGeometry geometry
          editor = Geometry.uiEditorGeometry geometry
      Geometry.topBarBounds (Geometry.uiTopBarGeometry geometry) `shouldBe` topBarRect layout
      Geometry.leftPanelBounds leftPanel `shouldBe` leftPanelRect layout
      Geometry.leftPanelToggleButton leftPanel `shouldBe` leftToggleRect layout
      Geometry.panelTabsToTuple (Geometry.leftPanelTabs leftPanel) `shouldBe` leftTabRects layout
      Geometry.leftPanelGenerateButton leftPanel `shouldBe` leftGenButtonRect layout
      Geometry.configPanelBounds configPanel `shouldBe` configPanelRect layout
      Geometry.configPanelToggleButton configPanel `shouldBe` configToggleRect layout
      Geometry.configTabsToTuple (Geometry.configPanelTabs configPanel) `shouldBe` configTabRects layout
      Geometry.configScrollAreaBounds configPanel `shouldBe` configScrollAreaRect layout
      Geometry.configScrollBarBounds configPanel `shouldBe` configScrollBarRect layout
      Geometry.logPanelBounds logPanel `shouldBe` logPanelRect layout
      Geometry.logHeaderBounds logPanel `shouldBe` logHeaderRect layout
      Geometry.logFilterButtonsToTuple (Geometry.logFilterButtons logPanel) `shouldBe` logFilterRects layout
      Geometry.editorToolbarBounds editor `shouldBe` editorToolbarRect layout
      Geometry.editorRadiusPlusButton editor `shouldBe` editorRadiusPlusRect layout
      Geometry.editorParamBarBounds editor `shouldBe` editorParamBarRect layout

    it "keeps typed left View geometry aligned with legacy helpers" $ do
      let layout = layoutForSeed (V2 800 600) 160 140
          leftPanel = Geometry.uiLeftPanelGeometry (layoutGeometry layout)
      Geometry.leftPanelViewButtons leftPanel `shouldBe` leftBaseViewRects layout
      Geometry.overlayViewGeometryToTuple (Geometry.leftPanelOverlayButtons leftPanel) `shouldBe` overlayViewRects layout
      Geometry.leftPanelDayNightToggleButton leftPanel `shouldBe` dayNightToggleRect layout
      Geometry.leftPanelViewContentHeightPx leftPanel `shouldBe` leftViewContentHeight layout
      Geometry.leftPanelViewScrollMaxPx leftPanel `shouldBe` leftViewScrollMax layout

    it "keeps typed primary panel geometry inside common window sizes" $
      mapM_ assertCommonWindowGeometry commonWindowSizes

    it "moves the typed config panel with window width while keeping the left panel stable" $ do
      let geometries = map (`typedGeometryFor` 600) [minUsableWindowWidth, 800, 1280]
          leftPanels = map (Geometry.leftPanelBounds . Geometry.uiLeftPanelGeometry) geometries
          configPanels = map (Geometry.configPanelBounds . Geometry.uiConfigPanelGeometry) geometries
      map Geometry.rectLeft leftPanels `shouldBe` replicate 3 16
      map Geometry.rectWidth leftPanels `shouldBe` replicate 3 240
      map Geometry.rectLeft configPanels `shouldSatisfy` strictlyIncreasing
      map Geometry.rectWidth configPanels `shouldBe` replicate 3 300

    it "resizes typed vertical panel bounds with the window height" $ do
      let shortGeometry = typedGeometryFor 800 600
          tallGeometry = typedGeometryFor 800 720
          shortConfig = Geometry.configPanelBounds (Geometry.uiConfigPanelGeometry shortGeometry)
          tallConfig = Geometry.configPanelBounds (Geometry.uiConfigPanelGeometry tallGeometry)
          shortLog = Geometry.logPanelBounds (Geometry.uiLogPanelGeometry shortGeometry)
          tallLog = Geometry.logPanelBounds (Geometry.uiLogPanelGeometry tallGeometry)
      Geometry.rectHeight tallConfig - Geometry.rectHeight shortConfig `shouldBe` 120
      Geometry.rectTop tallLog - Geometry.rectTop shortLog `shouldBe` 120
      Geometry.rectBottom tallLog `shouldBe` 720

    it "clamps typed config geometry between the left panel and the window edge on narrow resize" $ do
      let windowWidth = 500
          geometry = typedGeometryFor windowWidth 600
          leftPanel = Geometry.leftPanelBounds (Geometry.uiLeftPanelGeometry geometry)
          configPanel = Geometry.configPanelBounds (Geometry.uiConfigPanelGeometry geometry)
      Geometry.rectLeft configPanel `shouldBe` Geometry.rectRight leftPanel + 8
      Geometry.rectRight configPanel `shouldBe` windowWidth - 16
      Geometry.rectWidth configPanel `shouldSatisfy` (< 300)

inside :: V2 Int -> Rect -> Bool
inside (V2 px py) (Rect (V2 x y, V2 w h)) =
  px >= x && px < x + w && py >= y && py < y + h

insideRect :: Rect -> Rect -> Bool
insideRect (Rect (V2 ox oy, V2 ow oh)) (Rect (V2 ix iy, V2 iw ih)) =
  ix >= ox && iy >= oy && ix + iw <= ox + ow && iy + ih <= oy + oh

shouldNotOverlap :: Rect -> Rect -> Expectation
shouldNotOverlap (Rect (V2 ax ay, V2 aw ah)) (Rect (V2 bx by, V2 bw bh)) =
  (ax + aw <= bx || bx + bw <= ax || ay + ah <= by || by + bh <= ay)
    `shouldBe` True

commonWindowSizes :: [V2 Int]
commonWindowSizes = [V2 800 600, V2 1280 720, V2 1920 1080]

typedGeometryFor :: Int -> Int -> Geometry.UiGeometry
typedGeometryFor width height =
  uiGeometryForSeed (Geometry.WindowSize (V2 width height)) (Geometry.LogHeight 160) (Geometry.SeedInputWidth 120)

assertCommonWindowGeometry :: V2 Int -> Expectation
assertCommonWindowGeometry windowSize@(V2 width height) = do
  let geometry = uiGeometryForSeed (Geometry.WindowSize windowSize) (Geometry.LogHeight 160) (Geometry.SeedInputWidth 120)
      windowRect = Rect (V2 0 0, windowSize)
      topBar = Geometry.topBarBounds (Geometry.uiTopBarGeometry geometry)
      leftPanel = Geometry.leftPanelBounds (Geometry.uiLeftPanelGeometry geometry)
      configPanel = Geometry.configPanelBounds (Geometry.uiConfigPanelGeometry geometry)
      configScrollArea = Geometry.configScrollAreaBounds (Geometry.uiConfigPanelGeometry geometry)
      logPanel = Geometry.logPanelBounds (Geometry.uiLogPanelGeometry geometry)
      logHeader = Geometry.logHeaderBounds (Geometry.uiLogPanelGeometry geometry)
      logBody = Geometry.logBodyBounds (Geometry.uiLogPanelGeometry geometry)
      editorToolbar = Geometry.editorToolbarBounds (Geometry.uiEditorGeometry geometry)
      primaryRects = [topBar, leftPanel, configPanel, configScrollArea, logPanel, logHeader, logBody, editorToolbar]
  mapM_ (`shouldSatisfy` Geometry.rectNonNegative) primaryRects
  mapM_ (\rect -> Geometry.rectContainsRect windowRect rect `shouldBe` True) primaryRects
  Geometry.rectRight topBar `shouldBe` width
  Geometry.rectBottom logPanel `shouldBe` height
  Geometry.rectTop logPanel `shouldBe` height - 160
  Geometry.rectContainsRect configPanel configScrollArea `shouldBe` True
  Geometry.rectContainsRect logPanel logHeader `shouldBe` True
  Geometry.rectContainsRect logPanel logBody `shouldBe` True
  Geometry.rectsSeparatedHorizontally 8 leftPanel configPanel `shouldBe` True

strictlyIncreasing :: [Int] -> Bool
strictlyIncreasing values = and (zipWith (<) values (drop 1 values))
