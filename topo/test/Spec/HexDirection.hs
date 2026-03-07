{-# LANGUAGE ScopedTypeVariables #-}

module Spec.HexDirection (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Spec.Support.FloatApprox (approxEqAbs)
import Topo.Hex
import Topo.Grid.HexDirection

instance Arbitrary HexDirection where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = describe "HexDirection" $ do
  describe "foldHexDirections" $ do
    it "visits the canonical six directions in order" $ do
      foldHexDirections (\acc dir -> acc ++ [dir]) [] `shouldBe`
        [HexE, HexNE, HexNW, HexW, HexSW, HexSE]

  describe "foldHexNeighbors / countHexNeighbors" $ do
    prop "countHexNeighbors matches hexNeighborIndices length" $
      \(Positive width0) (Positive height0) rawIdx ->
        let width = 1 + width0 `mod` 6
            height = 1 + height0 `mod` 6
            cellCount = width * height
            idx = abs rawIdx `mod` cellCount
        in countHexNeighbors width height idx ==
             length (hexNeighborIndices width height idx)

    prop "foldHexNeighbors accumulates the same neighbours as hexNeighborIndices" $
      \(Positive width0) (Positive height0) rawIdx ->
        let width = 1 + width0 `mod` 6
            height = 1 + height0 `mod` 6
            cellCount = width * height
            idx = abs rawIdx `mod` cellCount
            viaFold = reverse (foldHexNeighbors width height idx (\acc j -> j : acc) [])
        in viaFold == hexNeighborIndices width height idx

  describe "nearestHexDirection" $ do
    prop "selects the exact matching direction vector heading" $ \(dir :: HexDirection) ->
      let (dx, dy) = directionVector dir
          angle = atan2 dy dx
      in nearestHexDirection angle === dir

  describe "stepIndexInDirection / traceIndexInDirection" $ do
    prop "traceIndexInDirection 1 matches stepIndexInDirection" $
      \(Positive width0) (Positive height0) (dir :: HexDirection) rawIdx ->
        let width = 1 + width0 `mod` 6
            height = 1 + height0 `mod` 6
            cellCount = width * height
            idx = abs rawIdx `mod` cellCount
        in traceIndexInDirection width height dir 1 idx ===
             stepIndexInDirection width height dir idx

    prop "traceIndexInDirection is stable once it hits the boundary" $
      \(Positive width0) (Positive height0) (dir :: HexDirection) rawIdx ->
        let width = 1 + width0 `mod` 6
            height = 1 + height0 `mod` 6
            cellCount = width * height
            idx = abs rawIdx `mod` cellCount
            far = traceIndexInDirection width height dir 32 idx
        in traceIndexInDirection width height dir 33 idx === far

  describe "sampleUpwindHex" $ do
    it "returns the center value for a constant field" $ do
      let field = U.replicate 9 0.42
          sampled = sampleUpwindHex 3 3 (field U.!) 4 (pi / 3)
      sampled `shouldSatisfy` approxEqAbs 1.0e-6 0.42

    prop "stays within the unit interval for unit-bounded fields" $
      \angle values0 ->
        let field = U.fromList (take 9 (map clampUnit (values0 ++ repeat 0)))
            sampled = sampleUpwindHex 3 3 (field U.!) 4 angle
        in sampled >= 0 && sampled <= 1

clampUnit :: Float -> Float
clampUnit value
  | value < 0 = 0
  | value > 1 = 1
  | otherwise = value