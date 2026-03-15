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
    (x1, y1, w1, h1) `shouldBe` (698, 443, 18, 18)
    (x4, w4, h4) `shouldBe` (770, 18, 18)

  it "creates config panel rect" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 x y, V2 w h) = configPanelRect layout
    (x, y, w, h) `shouldBe` (484, 44, 300, 380)

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

inside :: V2 Int -> Rect -> Bool
inside (V2 px py) (Rect (V2 x y, V2 w h)) =
  px >= x && px < x + w && py >= y && py < y + h
