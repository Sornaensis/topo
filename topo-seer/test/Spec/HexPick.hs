module Spec.HexPick (spec) where

import Control.Monad (forM_, when)
import Test.Hspec
import UI.HexPick (axialToScreen, pointInHex, screenToAxial)

spec :: Spec
spec = describe "UI.HexPick" $ do
  it "maps hex centers back to their axial coords" $ do
    let size = 6
        cases = [(0, 0), (1, 0), (-1, 1), (2, -1), (-2, 2)]
    forM_ cases $ \(q, r) -> do
      let (sx, sy) = axialToScreen size q r
      screenToAxial size sx sy `shouldBe` (q, r)

  it "keeps interior points in the same hex" $ do
    let size = 6
        (cx, cy) = axialToScreen size 0 0
        offsets = [(dx, dy) | dx <- [-6 .. 6], dy <- [-6 .. 6]]
        neighbors =
          [ (1, 0)
          , (1, -1)
          , (0, -1)
          , (-1, 0)
          , (-1, 1)
          , (0, 1)
          ]
        isExclusiveInterior (sx, sy) =
          pointInHex size (sx, sy) (0, 0)
            && not (any (pointInHex size (sx, sy)) neighbors)
    forM_ offsets $ \(dx, dy) -> do
      let sx = cx + dx
          sy = cy + dy
      when (isExclusiveInterior (sx, sy)) $
        screenToAxial size sx sy `shouldBe` (0, 0)
