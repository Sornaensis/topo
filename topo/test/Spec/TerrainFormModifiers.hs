{-# LANGUAGE OverloadedStrings #-}

-- | Tests for terrain form modifier infrastructure and its effects
-- on erosion, glacier, and hydrology stages.
module Spec.TerrainFormModifiers (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Data.Aeson (encode, decode)
import Topo
import Topo.TerrainForm.Modifiers
  ( TerrainFormModifiers(..)
  , TerrainFormModifiersConfig(..)
  , neutralModifiers
  , defaultTerrainFormModifiers
  , defaultTerrainFormModifiersConfig
  , configModifierLookup
  )
import Topo.Glacier (snowAccumGrid, diffuseGrid)
import Topo.Hydrology (flowAccumulation, piedmontSmoothGrid, carveRiversGrid, alluvialDepositGrid)
import Topo.Math (clamp01)

-- | All fifteen terrain form tags for exhaustive checks.
allForms :: [TerrainForm]
allForms =
  [ FormFlat, FormRolling, FormHilly, FormMountainous
  , FormCliff, FormValley, FormDepression, FormRidge
  , FormEscarpment, FormPlateau, FormBadlands, FormPass
  , FormCanyon, FormMesa, FormFoothill
  ]

spec :: Spec
spec = describe "TerrainFormModifiers" $ do

  -- =========================================================================
  -- Phase 1.4  JSON round-trip
  -- =========================================================================

  describe "JSON round-trip" $ do
    it "TerrainFormModifiers encodes and decodes to the same value" $ do
      let mods = defaultTerrainFormModifiers FormBadlands
          encoded = encode mods
      decode encoded `shouldBe` Just mods

    it "TerrainFormModifiersConfig encodes and decodes to the same value" $ do
      let cfg = defaultTerrainFormModifiersConfig
          encoded = encode cfg
      decode encoded `shouldBe` Just cfg

    it "configModifierLookup of default config matches defaultTerrainFormModifiers" $ do
      let cfg = defaultTerrainFormModifiersConfig
      mapM_ (\f -> configModifierLookup cfg f `shouldBe` defaultTerrainFormModifiers f) allForms

    it "partial JSON override only changes specified fields" $ do
      -- Decode a config where only badlands erosionRate is overridden
      let json = "{\"badlands\": {\"erosionRate\": 3.0}}"
          mResult = decode json :: Maybe TerrainFormModifiersConfig
      case mResult of
        Nothing -> expectationFailure "failed to decode partial JSON"
        Just cfg -> do
          -- Badlands erosion rate should be overridden
          tfmErosionRate (configModifierLookup cfg FormBadlands) `shouldBe` 3.0
          -- Other badlands fields should retain defaults
          tfmFlowBonus (configModifierLookup cfg FormBadlands)
            `shouldBe` tfmFlowBonus (defaultTerrainFormModifiers FormBadlands)
          -- Other forms should be unchanged
          configModifierLookup cfg FormCliff `shouldBe` defaultTerrainFormModifiers FormCliff

    it "neutralModifiers round-trips through JSON" $ do
      let encoded = encode neutralModifiers
      decode encoded `shouldBe` Just neutralModifiers

  -- =========================================================================
  -- Phase 2.3  Modifier field invariants
  -- =========================================================================

  describe "modifier field invariants" $ do
    it "all erosion rates are positive" $
      mapM_ (\f -> tfmErosionRate (defaultTerrainFormModifiers f)
                     `shouldSatisfy` (> 0)) allForms

    it "deposit suppression is in [0,1]" $
      mapM_ (\f -> do
        let sup = tfmDepositSuppression (defaultTerrainFormModifiers f)
        sup `shouldSatisfy` (>= 0)
        sup `shouldSatisfy` (<= 1)) allForms

    it "smooth resistance is in [0,1]" $
      mapM_ (\f -> do
        let sr = tfmSmoothResistance (defaultTerrainFormModifiers f)
        sr `shouldSatisfy` (>= 0)
        sr `shouldSatisfy` (<= 1)) allForms

    it "hardness bonus is non-negative" $
      mapM_ (\f -> tfmHardnessBonus (defaultTerrainFormModifiers f)
                     `shouldSatisfy` (>= 0)) allForms

    it "snow accumulation bonus is bounded" $
      mapM_ (\f -> do
        let b = tfmSnowAccumBonus (defaultTerrainFormModifiers f)
        b `shouldSatisfy` (>= -1)
        b `shouldSatisfy` (<= 1)) allForms

  -- =========================================================================
  -- Phase 3.5  Erosion behavior with terrain form modifiers
  -- =========================================================================

  describe "erosion behavior with modifiers" $ do

    it "badlands erode faster than flat terrain (hydraulic)" $ do
      let w = 5
          h = 5
          center = 2 * w + 2
          elev = U.generate (w * h) $ \i ->
            if i == center then 0.7 else 0.5 :: Float
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecHydraulicIterations = 4
            , ecRainRate = 0.5
            , ecMaxDrop = 0.5
            }
          -- Badlands: erosionMult = 1.5
          badlandsMult = U.replicate (w * h) (1.5 :: Float)
          -- Flat: erosionMult = 1.0
          flatMult = U.replicate (w * h) (1.0 :: Float)
          ones = U.replicate (w * h) (1.0 :: Float)
          erodedBadlands = iterate (hydraulicStepGrid w h 0.5 cfg hardness badlandsMult ones) elev !! 4
          erodedFlat     = iterate (hydraulicStepGrid w h 0.5 cfg hardness flatMult ones) elev !! 4
          peakBadlands = erodedBadlands U.! center
          peakFlat     = erodedFlat U.! center
      peakBadlands `shouldSatisfy` (< peakFlat)

    it "mesa cap rock resists erosion (thermal)" $ do
      let w = 5
          h = 5
          center = 2 * w + 2
          elev = U.generate (w * h) $ \i ->
            if i == center then 0.7 else 0.5 :: Float
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecThermalTalus = 0.04
            , ecThermalStrength = 0.3
            }
          -- Mesa: erosionMult = 0.5
          mesaMult = U.replicate (w * h) (0.5 :: Float)
          -- Normal: erosionMult = 1.0
          normalMult = U.replicate (w * h) (1.0 :: Float)
          ones = U.replicate (w * h) (1.0 :: Float)
          erodedMesa   = iterate (thermalStepGrid w h 0.5 cfg hardness mesaMult ones) elev !! 6
          erodedNormal = iterate (thermalStepGrid w h 0.5 cfg hardness normalMult ones) elev !! 6
          peakMesa   = erodedMesa U.! center
          peakNormal = erodedNormal U.! center
      -- Mesa peak should be higher (eroded less due to lower multiplier)
      peakMesa `shouldSatisfy` (> peakNormal)

    it "canyon smooth resistance preserves shape" $ do
      let w = 5
          h = 1
          elev = U.fromList [0.45, 0.45, 0.6, 0.7, 0.8 :: Float]
          cfg = defaultErosionConfig
            { ecCoastalSmoothZone = 0.15
            , ecCoastalSmoothStrength = 0.5
            }
          -- High smooth resistance (like canyon: 0.8)
          highResist = U.replicate (w * h) (0.8 :: Float)
          -- No resistance
          noResist = U.replicate (w * h) (0.0 :: Float)
          smoothedHigh = coastalSmoothGrid w h 0.5 cfg highResist elev
          smoothedNone = coastalSmoothGrid w h 0.5 cfg noResist elev
          changeHigh = abs (elev U.! 2 - smoothedHigh U.! 2)
          changeNone = abs (elev U.! 2 - smoothedNone U.! 2)
      -- High resistance should preserve the original more
      changeHigh `shouldSatisfy` (< changeNone)

    it "deposition suppression reduces material buildup" $ do
      let w = 5
          h = 1
          elev = U.fromList [0.9, 0.5, 0.5, 0.5, 0.5 :: Float]
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecThermalTalus = 0.04
            , ecThermalStrength = 0.5
            , ecThermalDepositRatio = 0.8
            }
          ones = U.replicate (w * h) (1.0 :: Float)
          -- Suppressed deposition (0.1 deposit factor)
          suppressed = U.replicate (w * h) (0.1 :: Float)
          -- Normal deposition
          normalDep = U.replicate (w * h) (1.0 :: Float)
          erodedSup  = thermalStepGrid w h 0.5 cfg hardness ones suppressed elev
          erodedNorm = thermalStepGrid w h 0.5 cfg hardness ones normalDep elev
          -- Tile 1 (cliff base) should gain less elevation with suppression
          raiseSup  = erodedSup U.! 1 - elev U.! 1
          raiseNorm = erodedNorm U.! 1 - elev U.! 1
      raiseSup `shouldSatisfy` (<= raiseNorm + 1e-6)

  -- =========================================================================
  -- Phase 4.5  Glacier behavior with terrain form modifiers
  -- =========================================================================

  describe "glacier behavior with modifiers" $ do

    it "plateau snow accumulation bonus produces more snow" $ do
      let cfg = defaultGlacierConfig
          n = 16
          temp = U.replicate n (0.2 :: Float)   -- cold
          precip = U.replicate n (0.8 :: Float)  -- wet
          -- With bonus (plateau: +0.30)
          bonus = U.replicate n (0.3 :: Float)
          -- With no bonus (flat terrain)
          noBonus = U.replicate n (0.0 :: Float)
          snowBonus   = snowAccumGrid cfg bonus temp precip
          snowNoBonus = snowAccumGrid cfg noBonus temp precip
          -- Plateau accumulation should be higher
          totalBonus   = U.sum snowBonus
          totalNoBonus = U.sum snowNoBonus
      totalBonus `shouldSatisfy` (> totalNoBonus)

    it "snow accumulation bonus of zero produces same result as neutral" $ do
      let cfg = defaultGlacierConfig
          n = 16
          temp = U.replicate n (0.15 :: Float)
          precip = U.replicate n (0.9 :: Float)
          zeroBon = U.replicate n (0.0 :: Float)
          snow = snowAccumGrid cfg zeroBon temp precip
      -- Each tile should equal precip * accumScale * tempFactor
      let range = max 0.0001 (gcSnowRange cfg)
          expected = 0.9 * gcAccumScale cfg * clamp01 ((gcSnowTemp cfg - 0.15) / range)
      U.all (\v -> abs (v - expected) < 1e-5) snow `shouldBe` True

    it "flow bonus amplifies ice diffusion (pass vs flat)" $ do
      let w = 5
          h = 5
          center = 2 * w + 2
          ice = U.generate (w * h) $ \i ->
            if i == center then 1.0 else 0.0 :: Float
          baseRate = gcFlowRate defaultGlacierConfig
          iters = gcFlowIterations defaultGlacierConfig
          -- Flat: uniform base rate
          flatRateVec = U.replicate (w * h) baseRate
          -- Pass: amplified rate (+0.30)
          passRateVec = U.replicate (w * h) (baseRate * (1 + 0.30))
          iceFlat = diffuseGrid w h flatRateVec iters ice
          icePass = diffuseGrid w h passRateVec iters ice
          -- Pass ice should spread more (center value lower)
          centerFlat = iceFlat U.! center
          centerPass = icePass U.! center
      centerPass `shouldSatisfy` (< centerFlat)

    it "negative flow bonus slows ice diffusion (plateau)" $ do
      let w = 5
          h = 5
          center = 2 * w + 2
          ice = U.generate (w * h) $ \i ->
            if i == center then 1.0 else 0.0 :: Float
          baseRate = gcFlowRate defaultGlacierConfig
          iters = gcFlowIterations defaultGlacierConfig
          -- Flat: base rate
          flatRateVec = U.replicate (w * h) baseRate
          -- Plateau: reduced rate (-0.30)
          platRateVec = U.replicate (w * h) (baseRate * (1 - 0.30))
          iceFlat = diffuseGrid w h flatRateVec iters ice
          icePlat = diffuseGrid w h platRateVec iters ice
          -- Plateau ice should spread less (center value higher)
          centerFlat = iceFlat U.! center
          centerPlat = icePlat U.! center
      centerPlat `shouldSatisfy` (> centerFlat)

  -- =========================================================================
  -- Phase 5.7  Hydrology behavior with terrain form modifiers
  -- =========================================================================

  describe "hydrology behavior with modifiers" $ do

    it "erosion multiplier amplifies river carving" $ do
      let w = 5
          h = 5
          elev = U.generate (w * h) $ \i ->
            let x = i `mod` w
                y = i `div` w
            in 0.7 - 0.03 * fromIntegral x - 0.01 * fromIntegral y :: Float
          hardness = U.replicate (w * h) (0.0 :: Float)
          acc = U.generate (w * h) $ \i ->
            fromIntegral (i + 1) :: Float
          cfg = defaultHydroConfig
            { hcRiverCarveMaxDepth = 0.1
            , hcRiverCarveScale = 0.05
            }
          highMult = U.replicate (w * h) (1.5 :: Float)
          normalMult = U.replicate (w * h) (1.0 :: Float)
          carvedHigh = carveRiversGrid cfg w h elev acc hardness highMult
          carvedNorm = carveRiversGrid cfg w h elev acc hardness normalMult
          totalHigh = U.sum carvedHigh
          totalNorm = U.sum carvedNorm
      -- Higher multiplier = more carving = lower total elevation
      totalHigh `shouldSatisfy` (< totalNorm)

    it "deposit factor suppresses alluvial deposits" $ do
      let w = 5
          h = 5
          elev = U.generate (w * h) $ \i ->
            let x = i `mod` w
                y = i `div` w
            in 0.7 - 0.02 * fromIntegral x - 0.01 * fromIntegral y :: Float
          acc = U.generate (w * h) $ \i ->
            fromIntegral (i + 1) :: Float
          cfg = defaultHydroConfig
            { hcAlluvialMaxSlope = 0.5
            , hcAlluvialDepositScale = 0.1
            }
          -- Full deposition
          fullDep = U.replicate (w * h) (1.0 :: Float)
          -- Suppressed deposition
          supDep = U.replicate (w * h) (0.1 :: Float)
          depositedFull = alluvialDepositGrid cfg w h elev acc fullDep
          depositedSup  = alluvialDepositGrid cfg w h elev acc supDep
          -- Full deposition should produce higher total elevation
          totalFull = U.sum depositedFull
          totalSup  = U.sum depositedSup
      totalFull `shouldSatisfy` (>= totalSup - 1e-6)

    it "flow bonus amplifies downstream accumulation" $ do
      let w = 5
          h = 1
          -- Sloping left-to-right
          elev = U.fromList [0.9, 0.8, 0.7, 0.6, 0.5 :: Float]
          flow = flowDirections w h elev
          cfg = defaultHydroConfig { hcBaseAccumulation = 1 }
          -- No flow bonus
          zeroBon = U.replicate (w * h) (0.0 :: Float)
          -- High flow bonus (like badlands: +0.25)
          highBon = U.replicate (w * h) (0.25 :: Float)
          accZero = flowAccumulation cfg zeroBon elev flow
          accHigh = flowAccumulation cfg highBon elev flow
          -- The rightmost tile (lowest) should accumulate more flow with bonus
          lastTileZero = accZero U.! 4
          lastTileHigh = accHigh U.! 4
      lastTileHigh `shouldSatisfy` (> lastTileZero)

    it "smooth resistance preserves piedmont shape" $ do
      let w = 7
          h = 1
          -- Mountain-to-plain transition
          elev = U.fromList [0.9, 0.85, 0.75, 0.65, 0.6, 0.58, 0.55 :: Float]
          formGrid = U.replicate (w * h) FormFoothill
          cfg = defaultHydroConfig
            { hcPiedmontSmoothStrength = 0.5
            , hcPiedmontSlopeMin = 0.02
            , hcPiedmontSlopeMax = 0.15
            }
          -- High resistance (like cliffs)
          highResist = U.replicate (w * h) (0.8 :: Float)
          -- No resistance
          noResist = U.replicate (w * h) (0.0 :: Float)
          smoothedHigh = piedmontSmoothGrid cfg w h elev formGrid highResist
          smoothedNone = piedmontSmoothGrid cfg w h elev formGrid noResist
          -- Total absolute change from original
          changeSum v = U.sum (U.zipWith (\a b -> abs (a - b)) elev v)
          changeHigh = changeSum smoothedHigh
          changeNone = changeSum smoothedNone
      -- High resistance should produce less total change
      changeHigh `shouldSatisfy` (<= changeNone + 1e-6)

  -- =========================================================================
  -- Geological motivation contracts
  -- =========================================================================

  describe "geological motivation contracts" $ do
    it "badlands have elevated erosion and flow bonus" $ do
      let m = defaultTerrainFormModifiers FormBadlands
      tfmErosionRate m `shouldSatisfy` (> 1.0)
      tfmFlowBonus m `shouldSatisfy` (> 0.0)

    it "canyon has high deposition suppression and smooth resistance" $ do
      let m = defaultTerrainFormModifiers FormCanyon
      tfmDepositSuppression m `shouldSatisfy` (>= 0.8)
      tfmSmoothResistance m `shouldSatisfy` (>= 0.7)

    it "mesa has high hardness bonus" $ do
      let m = defaultTerrainFormModifiers FormMesa
      tfmHardnessBonus m `shouldSatisfy` (>= 0.25)

    it "plateau has negative flow bonus (slow ice spread)" $ do
      let m = defaultTerrainFormModifiers FormPlateau
      tfmFlowBonus m `shouldSatisfy` (< 0.0)

    it "plateau has positive snow accumulation bonus" $ do
      let m = defaultTerrainFormModifiers FormPlateau
      tfmSnowAccumBonus m `shouldSatisfy` (> 0.0)

    it "valley has elevated erosion rate for glacial deepening" $ do
      let m = defaultTerrainFormModifiers FormValley
      tfmErosionRate m `shouldSatisfy` (> 1.0)

    it "pass has high flow bonus" $ do
      let m = defaultTerrainFormModifiers FormPass
      tfmFlowBonus m `shouldSatisfy` (>= 0.25)

    it "cliff has reduced erosion and high smooth resistance" $ do
      let m = defaultTerrainFormModifiers FormCliff
      tfmErosionRate m `shouldSatisfy` (< 1.0)
      tfmSmoothResistance m `shouldSatisfy` (>= 0.5)

    it "neutral forms have unit erosion rate" $ do
      let neutralForms = [FormFlat, FormRolling, FormHilly, FormMountainous, FormDepression, FormFoothill]
      mapM_ (\f -> tfmErosionRate (defaultTerrainFormModifiers f) `shouldBe` 1.0) neutralForms
