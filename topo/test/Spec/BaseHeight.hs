module Spec.BaseHeight (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo

spec :: Spec
spec = describe "BaseHeight" $ do
  it "disables ocean edge bias with zero falloff" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        extent = worldExtentSquareOrDefault 1
        edgeCfg = defaultOceanEdgeDepth { oedRMin = 1, oedFalloff = 0 }
        bias = oceanEdgeBiasAt config extent edgeCfg (TileCoord 0 0)
    bias `shouldBe` 0

  it "applies full axial edge bias at the boundary" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        extent = worldExtentSquareOrDefault 1
        edgeCfg = defaultOceanEdgeDepth
          { oedRMin = 0.8
          , oedRMax = 0.6
          , oedQMax = 0.4
          , oedQMin = 0.2
          , oedFalloff = 3
          }
        (minX, maxX, minY, maxY) = worldBounds config extent
        midX = (minX + maxX) `div` 2
        midY = (minY + maxY) `div` 2
        eps = 1e-6
        biasRMin = oceanEdgeBiasAt config extent edgeCfg (TileCoord midX minY)
        biasRMax = oceanEdgeBiasAt config extent edgeCfg (TileCoord midX maxY)
        biasQMin = oceanEdgeBiasAt config extent edgeCfg (TileCoord minX midY)
        biasQMax = oceanEdgeBiasAt config extent edgeCfg (TileCoord maxX midY)
    abs (biasRMin + 0.8) `shouldSatisfy` (< eps)
    abs (biasRMax + 0.6) `shouldSatisfy` (< eps)
    abs (biasQMin + 0.2) `shouldSatisfy` (< eps)
    abs (biasQMax + 0.4) `shouldSatisfy` (< eps)

  prop "edge bias is zero beyond falloff" $
    let config = WorldConfig { wcChunkSize = 8 }
        extent = worldExtentSquareOrDefault 1
        falloffTiles = 2
        edgeCfg = defaultOceanEdgeDepth
          { oedRMin = 1
          , oedFalloff = fromIntegral falloffTiles
          }
        (minX, _maxX, minY, maxY) = worldBounds config extent
        maxDist = maxY - minY
        maxK = max 0 (maxDist - falloffTiles)
    in forAll (chooseInt (0, maxK)) $ \k ->
      let gy = minY + falloffTiles + k
          gx = minX
          bias = oceanEdgeBiasAt config extent edgeCfg (TileCoord gx gy)
      in abs bias <= 1e-6

worldBounds :: WorldConfig -> WorldExtent -> (Int, Int, Int, Int)
worldBounds config extent =
  let size = wcChunkSize config
      (rx, ry) = worldExtentRadii extent
      minX = -rx * size
      maxX = rx * size + (size - 1)
      minY = -ry * size
      maxY = ry * size + (size - 1)
  in (minX, maxX, minY, maxY)
