module Spec.Widgets (spec) where

import Test.Hspec
import Linear (V2(..))
import UI.Widgets

spec :: Spec
spec = describe "UI.Widgets" $ do
  it "detects points inside rect" $ do
    let rect = Rect (V2 10 10, V2 20 20)
    containsPoint rect (V2 10 10) `shouldBe` True
    containsPoint rect (V2 29 29) `shouldBe` True
    containsPoint rect (V2 30 30) `shouldBe` False

  it "insets rect" $ do
    let rect = Rect (V2 0 0, V2 20 20)
        Rect (V2 x y, V2 w h) = insetRect 2 rect
    (x, y, w, h) `shouldBe` (2, 2, 16, 16)
