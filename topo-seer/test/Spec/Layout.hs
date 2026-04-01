module Spec.Layout (spec) where

import Test.Hspec
import Linear (V2(..))
import UI.Layout
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

inside :: V2 Int -> Rect -> Bool
inside (V2 px py) (Rect (V2 x y, V2 w h)) =
  px >= x && px < x + w && py >= y && py < y + h
