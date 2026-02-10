module Spec.Hex (spec) where

import Test.Hspec
import Topo

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
