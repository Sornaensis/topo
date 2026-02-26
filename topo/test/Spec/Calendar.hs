{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Calendar (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Word (Word64)
import Topo.Calendar
import Topo.Planet (PlanetConfig(..), defaultPlanetConfig)

-- | Generate a reasonable tick count (0 to ~10 million).
genTick :: Gen Word64
genTick = choose (0, 10000000)

-- | Generate a pair of ordered ticks (a < b), both reasonable.
genOrderedTicks :: Gen (Word64, Word64)
genOrderedTicks = do
  a <- choose (0, 9999998)
  b <- choose (a + 1, 9999999)
  pure (a, b)

spec :: Spec
spec = describe "Calendar" $ do
  -- WorldTime basics
  describe "WorldTime" $ do
    it "defaultWorldTime starts at tick 0" $
      wtTick defaultWorldTime `shouldBe` 0

    it "advanceTicks increments tick counter" $
      wtTick (advanceTicks 10 defaultWorldTime) `shouldBe` 10

    it "worldTimeElapsedSeconds is tick * tickRate" $ do
      let wt = WorldTime { wtTick = 100, wtTickRate = 2.5 }
      worldTimeElapsedSeconds wt `shouldBe` 250.0

    prop "worldTimeElapsedSeconds is non-negative" $
      forAll genTick $ \t ->
        let wt = WorldTime { wtTick = t, wtTickRate = 1.0 }
        in worldTimeElapsedSeconds wt >= 0

    prop "advanceTicks is additive" $
      forAll genTick $ \a ->
        forAll genTick $ \b ->
          let wt = defaultWorldTime
          in wtTick (advanceTicks b (advanceTicks a wt))
               == wtTick (advanceTicks (a + b) wt)

  -- CalendarConfig
  describe "CalendarConfig" $ do
    it "mkCalendarConfig defaultPlanetConfig ≈ Earth defaults" $ do
      let cal = mkCalendarConfig defaultPlanetConfig
      -- Earth radius → secondsPerDay ≈ 86400
      ccSecondsPerDay cal `shouldSatisfy` (\d -> abs (d - 86400.0) < 1.0)
      -- daysPerYear ≈ 365
      ccDaysPerYear cal `shouldSatisfy` (\d -> abs (d - 365) <= 1)
      ccHoursPerDay cal `shouldBe` 24.0

    it "larger planet has longer days" $ do
      let bigPlanet = defaultPlanetConfig { pcRadius = 9000 }
          calBig = mkCalendarConfig bigPlanet
          calEarth = mkCalendarConfig defaultPlanetConfig
      ccSecondsPerDay calBig `shouldSatisfy` (> ccSecondsPerDay calEarth)

    it "larger planet has longer years" $ do
      let bigPlanet = defaultPlanetConfig { pcRadius = 9000 }
          calBig = mkCalendarConfig bigPlanet
          calEarth = mkCalendarConfig defaultPlanetConfig
      ccDaysPerYear calBig `shouldSatisfy` (>= ccDaysPerYear calEarth)

  -- tickToDate round-trip
  describe "tickToDate" $ do
    it "tick 0 → year 0, day 0, hour ~0" $ do
      let cal = defaultCalendarConfig
          d   = tickToDate cal defaultWorldTime
      cdYear d `shouldBe` 0
      cdDayOfYear d `shouldBe` 0
      cdHourOfDay d `shouldSatisfy` (< 1.0)

    it "one full year of ticks → year 1, day 0" $ do
      let cal = defaultCalendarConfig
          secPerYear = ccSecondsPerDay cal * fromIntegral (ccDaysPerYear cal)
          -- At 1 second per tick, this many ticks = 1 year
          ticks = ceiling secPerYear :: Word64
          wt = WorldTime { wtTick = ticks, wtTickRate = 1.0 }
          d  = tickToDate cal wt
      cdYear d `shouldBe` 1
      cdDayOfYear d `shouldBe` 0

    prop "cdDayOfYear is always in [0, daysPerYear)" $
      forAll genTick $ \t ->
        let cal = defaultCalendarConfig
            wt = WorldTime { wtTick = t, wtTickRate = 1.0 }
            d  = tickToDate cal wt
        in cdDayOfYear d >= 0 && cdDayOfYear d < ccDaysPerYear cal

    prop "cdHourOfDay is in [0, hoursPerDay)" $
      forAll genTick $ \t ->
        let cal = defaultCalendarConfig
            wt = WorldTime { wtTick = t, wtTickRate = 1.0 }
            d  = tickToDate cal wt
        in cdHourOfDay d >= 0 && cdHourOfDay d < ccHoursPerDay cal

  -- yearFraction
  describe "yearFraction" $ do
    it "tick 0 → fraction 0.0" $ do
      let cal = defaultCalendarConfig
      yearFraction cal defaultWorldTime `shouldSatisfy` (< 0.001)

    it "half year of ticks → fraction ≈ 0.5" $ do
      let cal = defaultCalendarConfig
          secPerYear = ccSecondsPerDay cal * fromIntegral (ccDaysPerYear cal)
          halfYearTicks = round (secPerYear / 2.0) :: Word64
          wt = WorldTime { wtTick = halfYearTicks, wtTickRate = 1.0 }
          frac = yearFraction cal wt
      frac `shouldSatisfy` (\f -> abs (f - 0.5) < 0.01)

    prop "yearFraction is in [0, 1)" $
      forAll genTick $ \t ->
        let cal = defaultCalendarConfig
            wt = WorldTime { wtTick = t, wtTickRate = 1.0 }
            frac = yearFraction cal wt
        in frac >= 0.0 && frac < 1.0

    prop "yearFraction is monotonic within a year" $
      forAll genOrderedTicks $ \(a, b) ->
        let cal = defaultCalendarConfig
            secPerYear = ccSecondsPerDay cal * fromIntegral (ccDaysPerYear cal)
            yearsA = worldTimeElapsedSeconds (WorldTime a 1.0) / secPerYear
            yearsB = worldTimeElapsedSeconds (WorldTime b 1.0) / secPerYear
            sameYear = floor yearsA == (floor yearsB :: Int)
            fracA = yearFraction cal (WorldTime a 1.0)
            fracB = yearFraction cal (WorldTime b 1.0)
        in not sameYear || fracA <= fracB

  -- PlanetAge
  describe "PlanetAge" $ do
    it "defaultPlanetAge is 0 years" $
      paYears defaultPlanetAge `shouldBe` 0.0
