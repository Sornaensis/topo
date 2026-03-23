module Spec.ITCZ (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo.Climate.ITCZ

------------------------------------------------------------------------
-- Helper: build a uniform wind field on a gridW × gridH grid
------------------------------------------------------------------------

-- | Create wind grids where every tile has the same direction and speed.
uniformWind :: Int -> Int -> Float -> Float -> (U.Vector Float, U.Vector Float)
uniformWind gridW gridH dir spd =
  let n = gridW * gridH
  in (U.replicate n dir, U.replicate n spd)

-- | Build a latitude grid (degrees) for a gridW × gridH grid centered
-- on the equator, with each row separated by @degPerRow@ degrees.
latitudeGrid :: Int -> Int -> Float -> U.Vector Float
latitudeGrid gridW gridH degPerRow = U.generate (gridW * gridH) $ \i ->
  let y = i `div` gridW
      -- center the grid on the equator
      midY = fromIntegral gridH / 2
  in (fromIntegral y - midY) * degPerRow

------------------------------------------------------------------------
-- Unit tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "ITCZ" $ do
  describe "convergenceField" $ do
    it "uniform wind produces near-zero convergence in the interior" $ do
      -- All tiles have the same eastward wind → no convergence
      let gridW = 10
          gridH = 10
          (dirs, spds) = uniformWind gridW gridH 0 1.0
          conv = convergenceField gridW gridH dirs spds
          -- Interior tile (5,5) should have ~0 convergence
          idx = 5 * gridW + 5
      abs (conv U.! idx) < 0.01 `shouldBe` True

    it "opposing winds produce positive convergence" $ do
      -- Left half blows east (dir=0), right half blows west (dir=π)
      let gridW = 10
          gridH = 4
          n = gridW * gridH
          dirs = U.generate n $ \i ->
            let x = i `mod` gridW
            in if x < gridW `div` 2 then 0 else pi
          spds = U.replicate n 1.0
          conv = convergenceField gridW gridH dirs spds
          -- Tiles at the meeting column (x=4,5) should have positive
          -- convergence
          meetIdx = 2 * gridW + 5  -- row 2, col 5
      conv U.! meetIdx > 0 `shouldBe` True

    it "diverging winds produce negative convergence" $ do
      -- Left half blows west (dir=π), right half blows east (dir=0)
      let gridW = 10
          gridH = 4
          n = gridW * gridH
          dirs = U.generate n $ \i ->
            let x = i `mod` gridW
            in if x < gridW `div` 2 then pi else 0
          spds = U.replicate n 1.0
          conv = convergenceField gridW gridH dirs spds
          meetIdx = 2 * gridW + 5  -- row 2, col 5
      conv U.! meetIdx < 0 `shouldBe` True

  describe "itczBand" $ do
    it "peaks near the equator when convergence is uniform" $ do
      let gridW = 8
          gridH = 20
          n = gridW * gridH
          -- Uniform positive convergence everywhere
          conv = U.replicate n 0.5
          latDegs = latitudeGrid gridW gridH 2.0
          cfg = defaultConvergenceConfig
          band = itczBand cfg conv latDegs
          -- Equatorial row (y ~ gridH/2)
          eqIdx = (gridH `div` 2) * gridW + 4
          -- Polar row (y=0 → ~-20°)
          polarIdx = 4
      band U.! eqIdx > band U.! polarIdx `shouldBe` True

    it "is zero where convergence is below threshold" $ do
      let gridW = 4
          gridH = 4
          conv = U.replicate 16 (-0.1)  -- all negative
          latDegs = U.replicate 16 0
          cfg = defaultConvergenceConfig
          band = itczBand cfg conv latDegs
      U.all (== 0) band `shouldBe` True

    it "is non-negative everywhere" $ do
      let gridW = 8
          gridH = 8
          n = gridW * gridH
          conv = U.generate n (\i -> fromIntegral (i `mod` 7) * 0.1 - 0.3)
          latDegs = latitudeGrid gridW gridH 3.0
          cfg = defaultConvergenceConfig
          band = itczBand cfg conv latDegs
      U.all (>= 0) band `shouldBe` True

  describe "seasonalITCZShift" $ do
    it "returns baseLat when migration scale is zero" $
      seasonalITCZShift 5.0 0.0 23.44 1.5 `shouldBe` 5.0

    it "returns baseLat at equinox (phase = 0)" $
      seasonalITCZShift 0.0 0.7 23.44 0.0 `shouldBe` 0.0

    it "migrates north at northern summer (phase = π/2)" $ do
      let shifted = seasonalITCZShift 0.0 0.7 23.44 (pi / 2)
      shifted > 0 `shouldBe` True

    it "migrates south at southern summer (phase = 3π/2)" $ do
      let shifted = seasonalITCZShift 0.0 0.7 23.44 (3 * pi / 2)
      shifted < 0 `shouldBe` True

  describe "QuickCheck properties" $ do
    prop "ITCZ band is non-negative" $
      \(Positive w) (Positive h) ->
        let gridW = min 20 (w :: Int)
            gridH = min 20 (h :: Int)
            n = gridW * gridH
            conv = U.replicate n 0.3
            latDegs = latitudeGrid gridW gridH 2.0
            cfg = defaultConvergenceConfig
            band = itczBand cfg conv latDegs
        in U.all (>= 0) band

    prop "seasonal shift stays within ±(tilt × migScale) of baseLat" $
      \baseLat migScale tilt phase ->
        let bl = baseLat :: Float
            ms = abs (migScale :: Float)
            ti = abs (tilt :: Float)
            ph = phase :: Float
            shifted = seasonalITCZShift bl ms ti ph
            maxDev = ms * ti
        in abs (shifted - bl) <= maxDev + 1e-5
