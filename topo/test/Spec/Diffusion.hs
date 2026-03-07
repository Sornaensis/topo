module Spec.Diffusion (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Spec.Support.FloatApprox (approxEqAbs)
import Topo.Grid.Diffusion

spec :: Spec
spec = describe "Topo.Grid.Diffusion" $ do
  it "uses all six in-bounds hex neighbours for an interior tile" $ do
    let field = U.fromList [0, 0, 0
                           ,0, 1, 0
                           ,0, 0, 0]
    diffuseAtGrid 3 3 1 field 4
      `shouldSatisfy` approxEqAbs 1.0e-6 (1 / 7)

  it "uses only in-bounds hex neighbours at the grid edge" $ do
    let field = U.fromList [0, 1, 0
                           ,1, 0, 0
                           ,0, 0, 0]
    diffuseAtGrid 3 3 1 field 0
      `shouldSatisfy` approxEqAbs 1.0e-6 (2 / 3)

  it "spreads a source only to hex-adjacent tiles in one full step" $ do
    let field = U.fromList [0, 0, 0
                           ,0, 1, 0
                           ,0, 0, 0]
        diffused = diffuseOnceGrid 3 3 1 field
    diffused U.! 0 `shouldBe` 0
    diffused U.! 8 `shouldBe` 0
    diffused U.! 1 `shouldSatisfy` (> 0)
    diffused U.! 2 `shouldSatisfy` (> 0)
    diffused U.! 3 `shouldSatisfy` (> 0)
    diffused U.! 5 `shouldSatisfy` (> 0)
    diffused U.! 6 `shouldSatisfy` (> 0)
    diffused U.! 7 `shouldSatisfy` (> 0)

  it "keeps coastal proximity stronger at the shoreline than inland" $ do
    let oceanMask = U.fromList [1, 0, 0]
        coastal = coastalProximityGrid 3 1 2 1 oceanMask
    coastal U.! 0 `shouldSatisfy` (> coastal U.! 1)
    coastal U.! 1 `shouldSatisfy` (> coastal U.! 2)
    coastal U.! 1 `shouldSatisfy` (> 0)

  it "diffuses angular fields by circular mean instead of scalar wrap" $ do
    let epsilon = 0.05
        field = U.fromList [6.20, 0.05,
                            0.00, 0.00]
        diffused = diffuseAngleFieldGrid 2 2 1 1 field
        angleAt0 = diffused U.! 0
    angleAt0 `shouldSatisfy` (\angle -> angle >= (2 * pi - epsilon) || angle <= epsilon)

  prop "preserves constant fields" $
    \(Positive width0) (Positive height0) (NonNegative iterations0) factor0 value0 ->
      let width = 1 + width0 `mod` 6
          height = 1 + height0 `mod` 6
          iterations = iterations0 `mod` 5
          factor = clampUnit factor0
          value = clampUnit value0
          field = U.replicate (width * height) value
          diffused = diffuseFieldGrid width height iterations factor field
      in U.all (approxEqAbs 1.0e-5 value) diffused === True

  prop "keeps outputs in the unit interval for unit inputs" $
    \(Positive width0) (Positive height0) (NonNegative iterations0) factor0 values0 ->
      let width = 1 + width0 `mod` 6
          height = 1 + height0 `mod` 6
          cellCount = width * height
          iterations = iterations0 `mod` 5
          factor = clampUnit factor0
          field = U.fromList (take cellCount (map clampUnit (values0 ++ repeat 0)))
          diffused = diffuseFieldGrid width height iterations factor field
      in U.all inUnitInterval diffused === True

clampUnit :: Float -> Float
clampUnit value
  | value < 0 = 0
  | value > 1 = 1
  | otherwise = value

inUnitInterval :: Float -> Bool
inUnitInterval value = value >= 0 && value <= 1