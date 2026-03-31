{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Hex (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.List (nub)
import Topo

-- | Arbitrary instance for HexDirection (useful for property tests).
instance Arbitrary HexDirection where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = describe "Hex" $ do
  it "converts axial to world positions" $ do
    let pos = hexToWorld (HexAxial 3 4)
    pos `shouldBe` WorldPos 3 4

  it "round-trips axial hex coordinates through tile-space world positions" $ do
    let coord = HexAxial 3 4
    worldToHex (hexToWorld coord) `shouldBe` coord

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

  ---------------------------------------------------------------------------
  -- hexRing / hexDisc
  ---------------------------------------------------------------------------
  describe "hexRing" $ do
    it "ring 0 returns the empty list" $
      hexRing (HexAxial 3 4) 0 `shouldBe` []

    it "ring 1 returns 6 distinct tiles" $ do
      let ring = hexRing (HexAxial 0 0) 1
      length ring `shouldBe` 6
      length (nub ring) `shouldBe` 6

    it "ring 1 tiles are all distance 1 from center" $ do
      let ring = hexRing (HexAxial 2 3) 1
      all (\h -> hexDistance (HexAxial 2 3) h == 1) ring `shouldBe` True

    it "ring 2 returns 12 distinct tiles" $ do
      let ring = hexRing (HexAxial 0 0) 2
      length ring `shouldBe` 12
      length (nub ring) `shouldBe` 12

    it "ring 2 tiles are all distance 2 from center" $ do
      let ring = hexRing (HexAxial 0 0) 2
      all (\h -> hexDistance (HexAxial 0 0) h == 2) ring `shouldBe` True

    prop "ring r has exactly max(1, 6*r) elements" $ \(Positive r) ->
      let n = min r 10  -- keep small for test speed
      in length (hexRing (HexAxial 0 0) n) === 6 * n

  describe "hexDisc" $ do
    it "disc 0 is a singleton containing the center" $
      hexDisc (HexAxial 5 5) 0 `shouldBe` [HexAxial 5 5]

    it "disc 1 returns 7 tiles (center + 6 neighbors)" $ do
      let d = hexDisc (HexAxial 0 0) 1
      length d `shouldBe` 7
      length (nub d) `shouldBe` 7

    it "disc 2 returns 19 tiles (1 + 6 + 12)" $ do
      let d = hexDisc (HexAxial 0 0) 2
      length d `shouldBe` 19
      length (nub d) `shouldBe` 19

    prop "disc r has 3*r^2 + 3*r + 1 elements" $ \(Positive r) ->
      let n = min r 10
      in length (hexDisc (HexAxial 0 0) n) === 3 * n * n + 3 * n + 1

    prop "all disc tiles are within distance r of center" $ \(Positive r) ->
      let n = min r 10
          center = HexAxial 0 0
      in all (\h -> hexDistance center h <= n) (hexDisc center n) === True
