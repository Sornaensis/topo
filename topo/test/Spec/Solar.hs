module Spec.Solar (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Spec.Support.FloatApprox (approxEqAbs)
import Topo.Solar

-- | Tolerance for angular comparisons (radians).
angTol :: Float
angTol = 0.01

-- | Tolerance for hour comparisons.
hourTol :: Float
hourTol = 0.01

spec :: Spec
spec = describe "Solar" $ do

  -- -----------------------------------------------------------------------
  -- solarDeclination
  -- -----------------------------------------------------------------------
  describe "solarDeclination" $ do
    it "is zero at equinoxes (yearFrac = 0 and 0.5)" $ do
      solarDeclination 23.44 0.0 `shouldSatisfy` approxEqAbs angTol 0
      solarDeclination 23.44 0.5 `shouldSatisfy` approxEqAbs angTol 0

    it "is positive at NH summer solstice (yearFrac ≈ 0.25)" $
      solarDeclination 23.44 0.25 `shouldSatisfy` (> 0)

    it "is negative at NH winter solstice (yearFrac ≈ 0.75)" $
      solarDeclination 23.44 0.75 `shouldSatisfy` (< 0)

    it "peak declination ≈ axial tilt in radians" $ do
      let tilt = 23.44
          peak = solarDeclination tilt 0.25
          tiltRad = tilt * pi / 180
      peak `shouldSatisfy` approxEqAbs 0.001 tiltRad

    it "zero tilt gives zero declination for all year fractions" $
      solarDeclination 0.0 0.25 `shouldSatisfy` approxEqAbs 1e-6 0

  -- -----------------------------------------------------------------------
  -- hourAngle
  -- -----------------------------------------------------------------------
  describe "hourAngle" $ do
    it "is 0 at solar noon" $
      hourAngle 12.0 24.0 `shouldSatisfy` approxEqAbs angTol 0

    it "is −π at midnight (hour 0)" $
      hourAngle 0.0 24.0 `shouldSatisfy` approxEqAbs angTol (-pi)

    it "is negative in the morning" $
      hourAngle 6.0 24.0 `shouldSatisfy` (< 0)

    it "is positive in the afternoon" $
      hourAngle 18.0 24.0 `shouldSatisfy` (> 0)

  -- -----------------------------------------------------------------------
  -- localSolarHour
  -- -----------------------------------------------------------------------
  describe "localSolarHour" $ do
    it "longitude 0 gives same as calendar hour" $
      localSolarHour 24.0 12.0 0.0 `shouldSatisfy` approxEqAbs hourTol 12.0

    it "longitude 180 offsets by half a day" $
      localSolarHour 24.0 12.0 180.0 `shouldSatisfy` approxEqAbs hourTol 0.0

    it "longitude −90 offsets by −6 hours" $
      localSolarHour 24.0 12.0 (-90.0) `shouldSatisfy` approxEqAbs hourTol 6.0

    it "wraps into [0, hoursPerDay)" $ do
      let h = localSolarHour 24.0 1.0 (-180.0)
      h `shouldSatisfy` (\x -> x >= 0 && x < 24)

    prop "result is always in [0, hpd)" $
      forAll (choose (0 :: Float, 24.0)) $ \calH ->
        forAll (choose (-180 :: Float, 180)) $ \lon ->
          let h = localSolarHour 24.0 calH lon
          in h >= 0 && h < 24

  -- -----------------------------------------------------------------------
  -- solarPosition
  -- -----------------------------------------------------------------------
  describe "solarPosition" $ do
    it "sun is directly overhead at equator on equinox at noon" $ do
      let pos = solarPosition 0 0 0   -- equator, 0 declination, noon
      spAltitude pos `shouldSatisfy` approxEqAbs angTol (pi / 2)
      spZenith pos `shouldSatisfy` approxEqAbs angTol 0

    it "altitude is negative at midnight near equator" $ do
      let pos = solarPosition 0 0 pi  -- equator, equinox, hour angle = π
      spAltitude pos `shouldSatisfy` (< 0)

    it "altitude decreases with latitude at noon on equinox" $ do
      let posEq  = solarPosition 0 0 0
          pos45  = solarPosition (45 * pi / 180) 0 0
      spAltitude pos45 `shouldSatisfy` (< spAltitude posEq)

  -- -----------------------------------------------------------------------
  -- dayInfo
  -- -----------------------------------------------------------------------
  describe "dayInfo" $ do
    it "equator at equinox has ≈ 12h daylight" $ do
      let di = dayInfo 0 0 24.0
      diDayLength di `shouldSatisfy` approxEqAbs 0.1 12.0

    it "polar day: high latitude + summer declination" $ do
      let dec = 23.44 * pi / 180
          lat = 70 * pi / 180
          di = dayInfo lat dec 24.0
      diDayLength di `shouldSatisfy` approxEqAbs 0.1 24.0

    it "polar night: high latitude + winter declination" $ do
      let dec = -(23.44 * pi / 180)
          lat = 70 * pi / 180
          di = dayInfo lat dec 24.0
      diDayLength di `shouldSatisfy` approxEqAbs 0.1 0.0

    it "day is longer in summer hemisphere" $ do
      let dec = 10 * pi / 180  -- some positive declination
          diNorth = dayInfo (45 * pi / 180) dec 24.0
          diSouth = dayInfo (-45 * pi / 180) dec 24.0
      diDayLength diNorth `shouldSatisfy` (> diDayLength diSouth)

  -- -----------------------------------------------------------------------
  -- tileSolarPos / tileDayInfo
  -- -----------------------------------------------------------------------
  describe "tileSolarPos" $ do
    it "matches manual pipeline at equator equinox noon" $ do
      let pos = tileSolarPos 23.44 0.0 24.0 12.0 0.0 0.0
      spAltitude pos `shouldSatisfy` approxEqAbs angTol (pi / 2)

  describe "tileDayInfo" $ do
    it "matches dayInfo at equator equinox" $ do
      let di = tileDayInfo 23.44 0.0 24.0 0.0
      diDayLength di `shouldSatisfy` approxEqAbs 0.1 12.0

  -- -----------------------------------------------------------------------
  -- solarIrradiance
  -- -----------------------------------------------------------------------
  describe "solarIrradiance" $ do
    let k = 0.2
        diff = 0.12

    it "is zero when sun is below horizon (zenith ≥ π/2)" $ do
      solarIrradiance k diff (pi / 2) `shouldBe` 0
      solarIrradiance k diff (pi * 0.75) `shouldBe` 0

    it "is maximum at zenith 0 (sun directly overhead)" $ do
      let val = solarIrradiance k diff 0
      -- direct ≈ 1*exp(-0.2) ≈ 0.818, + 0.12 ≈ 0.938
      val `shouldSatisfy` approxEqAbs 0.02 0.94

    it "decreases as zenith increases toward horizon" $ do
      let v0  = solarIrradiance k diff 0.0
          v30 = solarIrradiance k diff (pi / 6)
          v60 = solarIrradiance k diff (pi / 3)
      v0 `shouldSatisfy` (> v30)
      v30 `shouldSatisfy` (> v60)

    it "is clamped to [0,1]" $ do
      -- With diffuse = 0, overhead ≈ 0.82, well within bounds
      -- With diffuse = 0.5, overhead ≈ 1.32, should be clamped
      solarIrradiance k 0.5 0 `shouldSatisfy` (<= 1.0)
      solarIrradiance k 0.5 0 `shouldSatisfy` (>= 0.0)

    it "diffuse fraction raises floor for daytime irradiance" $ do
      let noDiff  = solarIrradiance k 0.0 (pi / 3)
          withDiff = solarIrradiance k 0.3 (pi / 3)
      withDiff `shouldSatisfy` (> noDiff)

  -- -----------------------------------------------------------------------
  -- SolarConfig / tileIrradiance
  -- -----------------------------------------------------------------------
  describe "tileIrradiance" $ do
    it "returns >0 at equator noon" $ do
      let cfg = defaultSolarConfig
      tileIrradiance cfg 23.44 0.0 24.0 12.0 0.0 0.0 `shouldSatisfy` (> 0)

    it "returns 0 at equator midnight" $ do
      let cfg = defaultSolarConfig
      tileIrradiance cfg 23.44 0.0 24.0 0.0 0.0 0.0 `shouldSatisfy` (== 0)

    it "noon irradiance > morning irradiance at same latitude" $ do
      let cfg = defaultSolarConfig
          noon    = tileIrradiance cfg 23.44 0.0 24.0 12.0 0.5 0.0
          morning = tileIrradiance cfg 23.44 0.0 24.0 8.0  0.5 0.0
      noon `shouldSatisfy` (> morning)

  -- -----------------------------------------------------------------------
  -- annualMeanInsolation
  -- -----------------------------------------------------------------------
  describe "annualMeanInsolation" $ do
    let cfg = defaultSolarConfig
        tilt = 23.44
        hpd = 24.0

    it "is highest at the equator" $ do
      let equator = annualMeanInsolation cfg tilt hpd 0.0
          lat45   = annualMeanInsolation cfg tilt hpd (pi / 4)
      equator `shouldSatisfy` (> lat45)

    it "decreases toward the poles" $ do
      let lat30 = annualMeanInsolation cfg tilt hpd (pi / 6)
          lat60 = annualMeanInsolation cfg tilt hpd (pi / 3)
          lat85 = annualMeanInsolation cfg tilt hpd (85 * pi / 180)
      lat30 `shouldSatisfy` (> lat60)
      lat60 `shouldSatisfy` (> lat85)

    it "is symmetric between hemispheres" $ do
      let north = annualMeanInsolation cfg tilt hpd (pi / 4)
          south = annualMeanInsolation cfg tilt hpd (negate (pi / 4))
      north `shouldSatisfy` approxEqAbs 0.01 south

    it "is in [0,1]" $ do
      let vals = [annualMeanInsolation cfg tilt hpd (lat * pi / 180)
                 | lat <- [-85, -60, -30, 0, 30, 60, 85]]
      mapM_ (\v -> v `shouldSatisfy` (\x -> x >= 0 && x <= 1)) vals

    it "is zero with zero axial tilt at the pole" $ do
      -- With no tilt, poles get zero annual irradiance (sun always at horizon)
      annualMeanInsolation cfg 0.0 hpd (pi / 2) `shouldSatisfy` approxEqAbs 0.01 0
