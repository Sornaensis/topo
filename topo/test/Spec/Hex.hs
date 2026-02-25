{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Hex (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo

-- | Arbitrary instance for HexDirection (useful for property tests).
instance Arbitrary HexDirection where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = describe "Hex" $ do
  it "converts axial to world positions" $ do
    let pos = hexToWorld defaultHexGridMeta (HexAxial 3 4)
    pos `shouldBe` WorldPos 3 4

  it "converts between axial and cube" $ do
    axialToCube (HexAxial 2 (-1)) `shouldBe` HexCube 2 (-1) (-1)
    cubeToAxial (HexCube 2 (-1) (-1)) `shouldBe` HexAxial 2 (-1)

  it "computes hex distance" $ do
    hexDistance (HexAxial 0 0) (HexAxial 2 (-1)) `shouldBe` 2

  it "lists six neighbors" $ do
    let neighbors = hexNeighbors (HexAxial 0 0)
    length neighbors `shouldBe` 6
    neighbors `shouldSatisfy` \ns -> HexAxial 1 0 `elem` ns && HexAxial 0 1 `elem` ns

  ---------------------------------------------------------------------------
  -- HexDirection
  ---------------------------------------------------------------------------
  describe "HexDirection" $ do
    it "has exactly 6 directions" $
      length allHexDirections `shouldBe` 6

    it "hexDirectionCount equals length of allHexDirections" $
      hexDirectionCount `shouldBe` length allHexDirections

    prop "hexOpposite is an involution" $ \(d :: HexDirection) ->
      hexOpposite (hexOpposite d) === d

    prop "hexOpposite never returns the same direction" $ \(d :: HexDirection) ->
      hexOpposite d =/= d

    prop "hexDirOffset produces unit-distance offsets" $ \(d :: HexDirection) ->
      let (dq, dr) = hexDirOffset d
      in abs dq + abs dr `elem` [1, 2]
         -- In axial coords, hex neighbor offsets have Manhattan distance 1 or 2.

    prop "hexNeighborInDirection is consistent with hexDirOffset" $ \(d :: HexDirection) ->
      let origin = HexAxial 5 5
          neighbor = hexNeighborInDirection d origin
          (dq, dr) = hexDirOffset d
      in neighbor === HexAxial (5 + dq) (5 + dr)

  ---------------------------------------------------------------------------
  -- dsSlopeIn / dsSteepestDescent
  ---------------------------------------------------------------------------
  describe "dsSlopeIn" $ do
    it "extracts correct field for each direction" $ do
      let ds = DirectionalSlope 1.0 2.0 3.0 4.0 5.0 6.0
      dsSlopeIn HexE  ds `shouldBe` 1.0
      dsSlopeIn HexNE ds `shouldBe` 2.0
      dsSlopeIn HexNW ds `shouldBe` 3.0
      dsSlopeIn HexW  ds `shouldBe` 4.0
      dsSlopeIn HexSW ds `shouldBe` 5.0
      dsSlopeIn HexSE ds `shouldBe` 6.0

    prop "dsSlopeIn d zeroDirSlope == 0 for all d" $ \(d :: HexDirection) ->
      dsSlopeIn d zeroDirSlope === 0.0

  describe "dsSteepestDescent" $ do
    it "returns Nothing for zeroDirSlope" $
      dsSteepestDescent zeroDirSlope `shouldBe` Nothing

    it "returns Nothing when all slopes are positive (local minimum)" $ do
      let ds = DirectionalSlope 0.1 0.2 0.3 0.4 0.5 0.6
      dsSteepestDescent ds `shouldBe` Nothing

    it "returns the direction of the most negative slope" $ do
      -- W has the most negative slope (-0.5)
      let ds = DirectionalSlope 0.1 0.2 (-0.1) (-0.5) (-0.2) 0.3
      dsSteepestDescent ds `shouldBe` Just HexW

    it "returns HexE when east is the only downhill direction" $ do
      let ds = DirectionalSlope (-0.3) 0.1 0.2 0.4 0.5 0.1
      dsSteepestDescent ds `shouldBe` Just HexE

  ---------------------------------------------------------------------------
  -- hexNeighborIndices
  ---------------------------------------------------------------------------
  describe "hexNeighborIndices" $ do
    it "returns 6 neighbors for an interior tile" $ do
      -- 8x8 grid, tile at (4,4) = index 36
      length (hexNeighborIndices 8 8 36) `shouldBe` 6

    it "returns fewer neighbors at grid edges" $ do
      -- 8x8 grid, tile at (0,0) = index 0
      length (hexNeighborIndices 8 8 0) `shouldSatisfy` (< 6)

  describe "hexNeighborIndexInDirection" $ do
    it "returns Just for interior tile" $ do
      hexNeighborIndexInDirection 8 8 HexE 36 `shouldBe` Just 37

    it "returns Nothing for off-grid neighbor" $ do
      -- tile at (0,0), going west is off-grid
      hexNeighborIndexInDirection 8 8 HexW 0 `shouldBe` Nothing
