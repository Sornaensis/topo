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
        Rect (V2 minusX minusY, V2 minusW minusH) = configWaterMinusRect layout
        Rect (V2 barX barY, V2 barW barH) = configWaterBarRect layout
        Rect (V2 plusX plusY, V2 plusW plusH) = configWaterPlusRect layout
        waterRowRect = configParamRowRect 0 layout
        waterBarCenter = rectCenter (configWaterBarRect layout)
    (minusW, minusH) `shouldBe` (24, 24)
    (plusW, plusH) `shouldBe` (24, 24)
    minusY `shouldBe` plusY
    barY `shouldBe` (minusY + (minusH - barH) `div` 2)
    barX `shouldBe` (minusX + minusW + 8)
    plusX `shouldBe` (barX + barW + 8)
    plusY `shouldBe` minusY
    waterBarCenter `shouldSatisfy` (`inside` waterRowRect)

inside :: V2 Int -> Rect -> Bool
inside (V2 px py) (Rect (V2 x y, V2 w h)) =
  px >= x && px < x + w && py >= y && py < y + h
