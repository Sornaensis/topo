module Spec.Layout (spec) where

import Test.Hspec
import Linear (V2(..))
import UI.Layout
import UI.Widgets (Rect(..))

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
    (x, y, w, h) `shouldBe` (544, 16, 240, 408)

  it "creates config apply/reset rects" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 ax ay, V2 aw ah) = configApplyRect layout
        Rect (V2 bx by, V2 bw bh) = configReplayRect layout
        Rect (V2 rx ry, V2 rw rh) = configResetRect layout
    (ax, ay, aw, ah) `shouldBe` (556, 320, 216, 24)
    (bx, by, bw, bh) `shouldBe` (556, 352, 216, 24)
    (rx, ry, rw, rh) `shouldBe` (556, 384, 216, 24)

  it "creates config slider rects" $ do
    let layout = layoutFor (V2 800 600) 160
        Rect (V2 wx wy, V2 ww wh) = configWaterBarRect layout
        Rect (V2 ex ey, V2 ew eh) = configEvapBarRect layout
    (wx, wy, ww, wh) `shouldBe` (588, 114, 138, 12)
    (ex, ey, ew, eh) `shouldBe` (588, 148, 138, 12)
