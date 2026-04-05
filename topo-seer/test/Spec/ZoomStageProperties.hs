module Spec.ZoomStageProperties (spec) where

import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Test.Hspec
import Test.QuickCheck

import Seer.Render.Atlas (AtlasTextureCache(..), emptyAtlasTextureCache, resolveEffectiveStage)
import Seer.Render.Viewport (visibleChunkKeys)
import Seer.Render.ZoomStage
  ( ZoomStage(..)
  , allZoomStages
  , maxCameraZoom
  , stageForZoom
  )
import Topo (WorldConfig(..))
import UI.HexPick (renderHexRadiusPx)

spec :: Spec
spec = describe "ZoomStage properties" $ do

  describe "stageForZoom" $ do

    it "is monotone: z1 < z2 => hexRadius(z1) <= hexRadius(z2)" $
      property $ \(NonNegative z1) (NonNegative z2) ->
        let s1 = stageForZoom (z1 * maxCameraZoom)
            s2 = stageForZoom (z2 * maxCameraZoom)
        in if z1 <= z2
             then zsHexRadius s1 <= zsHexRadius s2
             else zsHexRadius s1 >= zsHexRadius s2

    it "covers [0, maxCameraZoom]: every zoom maps to a valid stage" $
      property $ \(NonNegative t) ->
        let z = t * maxCameraZoom
            s = stageForZoom z
        in zsHexRadius s > 0 && zsAtlasScale s >= 1

    it "returns the correct stage at each boundary" $ do
      stageForZoom 0.0    `shouldSatisfy` \s -> zsHexRadius s == 6
      stageForZoom 0.999  `shouldSatisfy` \s -> zsHexRadius s == 6
      stageForZoom 1.0    `shouldSatisfy` \s -> zsHexRadius s == 10
      stageForZoom 2.199  `shouldSatisfy` \s -> zsHexRadius s == 10
      stageForZoom 2.2    `shouldSatisfy` \s -> zsHexRadius s == 18
      stageForZoom 3.999  `shouldSatisfy` \s -> zsHexRadius s == 18
      stageForZoom 4.0    `shouldSatisfy` \s -> zsHexRadius s == 32
      stageForZoom 5.999  `shouldSatisfy` \s -> zsHexRadius s == 32
      stageForZoom 6.0    `shouldSatisfy` \s -> zsHexRadius s == 50
      stageForZoom 7.999  `shouldSatisfy` \s -> zsHexRadius s == 50
      stageForZoom maxCameraZoom `shouldSatisfy` \s -> zsHexRadius s == 50

    it "baked PPI meets screen demand at each stage midpoint" $
      mapM_ checkBakedPpi allZoomStages

  describe "viewport culling" $ do

    it "bounds visible chunks for a huge world at low zoom" $ do
      let config = WorldConfig { wcChunkSize = 16 }
          -- Build a 100x100 chunk map (10000 chunks).
          bigMap = IntMap.fromList [(i, ()) | i <- [0 .. 9999]]
          pan    = (0.0, 0.0)
          zoom   = 0.5
          window = (1920, 1080)
          visible = visibleChunkKeys config pan zoom window bigMap
      -- Should be a bounded subset, not all 10000.
      -- The analytical coordinate-range approach returns a superset of
      -- the geometrically visible set, but still a small fraction.
      length visible `shouldSatisfy` (< 1000)
      length visible `shouldSatisfy` (> 0)

    it "returns all chunks when they fit in the viewport" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          -- 2x2 = 4 chunks, easily fits at low zoom.
          smallMap = IntMap.fromList [(i, ()) | i <- [0 .. 3]]
          pan    = (0.0, 0.0)
          zoom   = 0.5
          window = (1920, 1080)
          visible = visibleChunkKeys config pan zoom window smallMap
      length visible `shouldBe` 4

  describe "hysteresis" $ do

    it "does not switch stage immediately on boundary cross" $ do
      let cache0 = emptyAtlasTextureCache 3
          t0 = 1000000000 :: Word64
          -- First call establishes committed stage at zoom 1.5 (stage 1, hexRadius=10)
          stage1 = stageForZoom 1.5
          (eff1, cache1) = resolveEffectiveStage t0 stage1 cache0
      zsHexRadius eff1 `shouldBe` zsHexRadius stage1
      -- Cross into stage 2 (zoom 2.5, hexRadius=18) only 50ms later
      let stage2 = stageForZoom 2.5
          t1 = t0 + 50000000  -- 50ms
          (eff2, _cache2) = resolveEffectiveStage t1 stage2 cache1
      -- Should still be on stage 1 (hysteresis)
      zsHexRadius eff2 `shouldBe` zsHexRadius stage1

    it "switches stage after hysteresis threshold elapses" $ do
      let cache0 = emptyAtlasTextureCache 3
          t0 = 1000000000 :: Word64
          stage1 = stageForZoom 1.5
          (_, cache1) = resolveEffectiveStage t0 stage1 cache0
          -- Cross into stage 2
          stage2 = stageForZoom 2.5
          t1 = t0 + 50000000
          (_, cache2) = resolveEffectiveStage t1 stage2 cache1
          -- Wait 350ms (total 400ms > 300ms threshold)
          t2 = t1 + 350000000
          (eff3, _cache3) = resolveEffectiveStage t2 stage2 cache2
      zsHexRadius eff3 `shouldBe` zsHexRadius stage2

    it "resets hysteresis timer when zoom returns to committed stage" $ do
      let cache0 = emptyAtlasTextureCache 3
          t0 = 1000000000 :: Word64
          stage1 = stageForZoom 1.5
          (_, cache1) = resolveEffectiveStage t0 stage1 cache0
          -- Cross into stage 2 briefly
          stage2 = stageForZoom 2.5
          t1 = t0 + 50000000
          (_, cache2) = resolveEffectiveStage t1 stage2 cache1
          -- Return to stage 1
          t2 = t1 + 100000000
          (eff3, cache3) = resolveEffectiveStage t2 stage1 cache2
      zsHexRadius eff3 `shouldBe` zsHexRadius stage1
      -- Timer should be reset
      atcStageChangeNs cache3 `shouldBe` 0

-- | Verify that baked pixels per hex >= screen demand at stage midpoint.
--
-- Screen demand at zoom z: renderHexRadiusPx * z
-- Baked PPI: zsHexRadius * zsAtlasScale
checkBakedPpi :: ZoomStage -> IO ()
checkBakedPpi stage = do
  let mid = (zsZoomMin stage + zsZoomMax stage) / 2
      screenDemand = fromIntegral renderHexRadiusPx * mid :: Float
      bakedPpi = fromIntegral (zsHexRadius stage * zsAtlasScale stage) :: Float
  bakedPpi `shouldSatisfy` (>= screenDemand)
