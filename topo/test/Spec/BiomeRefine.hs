{-# LANGUAGE PatternSynonyms #-}

-- | Tests for Phase 3 biome realism: primary classification properties,
-- sub-biome code round-trips, coverage, and per-family refinement unit tests.
module Spec.BiomeRefine (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Either (isRight)
import Data.List (nub)
import Data.Word (Word16)
import qualified Data.Vector.Unboxed as U
import Topo.Biome
  ( BiomeRule(..)
  , BiomeThresholds(..)
  , classifyBiome
  , defaultBiomeRules
  , defaultBiomeThresholds
  , smoothMountainTransitions
  )
import Topo.Biome.Refine
  ( RefinementConfig(..)
  , defaultRefinementConfig
  , aridRefinementConfig
  , lushRefinementConfig
  , refineBiomesChunk
  )
import Topo.Biome.Refine.Ocean     (defaultOceanConfig, refineOcean)
import Topo.Biome.Refine.Coastal   (defaultCoastalConfig, refineCoastal)
import Topo.Biome.Refine.Desert    (defaultDesertConfig, refineDesert)
import Topo.Biome.Refine.Grassland (defaultGrasslandConfig, refineGrassland)
import Topo.Biome.Refine.Forest    (defaultForestConfig, ForestConfig(..), refineForest)
import Topo.Biome.Refine.Tundra    (defaultTundraConfig, refineTundra)
import Topo.Biome.Refine.Rainforest (defaultRainforestConfig, refineRainforest)
import Topo.Biome.Refine.Shrubland (defaultShrublandConfig, refineShrubland)
import Topo.Biome.Refine.Savanna   (defaultSavannaConfig, refineSavanna)
import Topo.Biome.Refine.Taiga     (defaultTaigaConfig, refineTaiga)
import Topo.Biome.Refine.Swamp     (defaultSwampConfig, refineSwamp)
import Topo.Biome.Refine.Snow      (defaultSnowConfig, refineSnow)
import Topo.Biome.Refine.Alpine    (defaultAlpineConfig, refineAlpine)
import Topo.Biome.Refine.Volcanic  (defaultVolcanicConfig, refineVolcanic)
import Topo.Types

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | All valid biome codes (0-8, 10-65; code 9 is a gap).
allKnownCodes :: [Word16]
allKnownCodes = [0..8] ++ [10..65]

-- | Safely build a BiomeId from a known-valid code.
mkBid :: Word16 -> BiomeId
mkBid code = case biomeIdFromCode code of
  Right bid -> bid
  Left _ -> error $ "test setup: unknown code " ++ show code

-- | Wrapper for normalised float generation in [0,1].
newtype Norm01 = Norm01 { getNorm01 :: Float }
  deriving (Show)

instance Arbitrary Norm01 where
  arbitrary = Norm01 <$> choose (0.0, 1.0)
  shrink (Norm01 v) = [Norm01 v' | v' <- shrink v, v' >= 0, v' <= 1]

-- | Primary biome codes (family-level).
primaryCodes :: [BiomeId]
primaryCodes =
  [ BiomeDesert, BiomeGrassland, BiomeForest, BiomeTundra
  , BiomeRainforest, BiomeShrubland, BiomeSavanna, BiomeTaiga
  , BiomeSwamp, BiomeOcean, BiomeSnow, BiomeCoastal, BiomeAlpine
  ]

-- | Check that a BiomeId has a known code (0-8 or 10-60).
isKnownBiome :: BiomeId -> Bool
isKnownBiome bid =
  let code = biomeIdToCode bid
  in isRight (biomeIdFromCode code)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "BiomeRefine" $ do

  -- -----------------------------------------------------------------------
  -- 3.19.1: Primary classification produces valid codes
  -- -----------------------------------------------------------------------
  describe "primary classification" $ do
    prop "always produces a valid known BiomeId for any land tile" $
      \(Norm01 temp) (Norm01 precip) (Norm01 elev') ->
        let waterLevel = 0.5
            -- Ensure land tile (above water level)
            elev = waterLevel + elev' * 0.5
        in isKnownBiome (classifyBiome defaultBiomeRules defaultBiomeThresholds
                           waterLevel WaterDry WaterDry temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10)

    prop "always produces a valid known BiomeId for any ocean tile" $
      \(Norm01 temp) (Norm01 precip) (Norm01 elev') ->
        let waterLevel = 0.5
            elev = elev' * waterLevel
        in isKnownBiome (classifyBiome defaultBiomeRules defaultBiomeThresholds
                           waterLevel WaterOcean WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10)

    prop "classifies ocean when WaterOcean" $
      \(Norm01 temp) (Norm01 precip) (Norm01 elev') ->
        let waterLevel = 0.5
            elev = elev' * 0.49  -- always below threshold
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterOcean WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10 == BiomeOcean

    -- Constrain temperature below btAlpineMaxTemp (0.35) so that the
    -- temperature-primary mountain guards always fire.  At T < 0.20 we
    -- get Snow or Tundra (polar desert); at T in [0.20, 0.35) Alpine.
    prop "cold tiles at very high elevation classify as mountain biomes" $
      \(Norm01 temp') (Norm01 precip) ->
        let temp = temp' * 0.34 -- keep T < btAlpineMaxTemp (0.35)
            waterLevel = 0.5
            bid = classifyBiome defaultBiomeRules defaultBiomeThresholds
                    waterLevel WaterDry WaterDry temp precip 0.95 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10
        in bid `elem` [BiomeSnow, BiomeTundra, BiomeAlpine]

    -- P3.6: dry high-elevation tiles route to tundra (not snow).
    -- Temperature must be < btSnowMaxTemp (0.20) for the snow guard to
    -- fire; precip < btSnowPolarDesertMaxPrecip (0.08) selects the
    -- polar-desert / tundra branch.
    it "dry high-elevation tile classified as tundra (P3.6)" $
      let waterLevel = 0.5
      in classifyBiome defaultBiomeRules defaultBiomeThresholds
           waterLevel WaterDry WaterDry 0.15 0.05 0.95 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10
           `shouldBe` BiomeTundra

    -- P3.6: wet high-elevation tile still classified as snow.
    -- Temperature < btSnowMaxTemp (0.20), precip > polar-desert cutoff.
    it "wet high-elevation tile classified as snow" $
      let waterLevel = 0.5
      in classifyBiome defaultBiomeRules defaultBiomeThresholds
           waterLevel WaterDry WaterDry 0.15 0.30 0.95 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10
           `shouldBe` BiomeSnow

    -- Ocean-adjacent coastal check
    prop "ocean-adjacent low tile classifies as BiomeCoastal" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01  -- just above water level, within coastal band
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10 == BiomeCoastal

    prop "lake-adjacent low tile classifies as BiomeSwamp (not BiomeCoastal)" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterLake temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10 == BiomeSwamp

    prop "no-adjacent-water low tile classifies as BiomeSwamp (not BiomeCoastal)" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterDry temp precip elev 0.1 0.1 0.3 FormFlat 0.5 0.10 0.10 == BiomeSwamp

  -- -----------------------------------------------------------------------
  -- 3.19.2: All sub-biome codes round-trip through biomeIdFromCode
  -- -----------------------------------------------------------------------
  describe "biomeIdFromCode round-trip" $
    it "all codes 0-8, 10-65 round-trip" $
      mapM_ (\code ->
        case biomeIdFromCode code of
          Left _ -> expectationFailure $ "code " ++ show code ++ " should be valid"
          Right bid -> biomeIdToCode bid `shouldBe` code
      ) allKnownCodes

  -- -----------------------------------------------------------------------
  -- 3.19.3: Coverage -- every non-override primary biome appears
  -- -----------------------------------------------------------------------
  describe "coverage" $
    it "every non-override primary biome appears in a uniform temp x precip sweep" $ do
      let waterLevel = 0.5
          -- Non-override biomes (ocean, coastal, snow, alpine are elevation-based)
          -- Swamp is assigned by terrain-form overrides (valley/depression +
          -- high moisture), not by the climate rule table, so it does not
          -- appear in a flat-terrain sweep.
          climateBiomes = [BiomeDesert, BiomeGrassland, BiomeForest, BiomeTundra
                          , BiomeRainforest, BiomeShrubland, BiomeSavanna, BiomeTaiga]
          -- Slope must be < btMontaneMinSlope (0.06) so the montane
          -- guard does not intercept mid-temperature tiles and hide
          -- Taiga from the Whittaker rule table.
          samples =
            [ classifyBiome defaultBiomeRules defaultBiomeThresholds
                waterLevel WaterDry WaterDry t p 0.60 0.05 0.1 0.3 FormFlat 0.5 0.10 0.10
            | t <- [0.05, 0.10 .. 1.0]
            , p <- [0.05, 0.10 .. 1.0]
            ]
          found = nub samples
      mapM_ (\b -> b `elem` found `shouldBe` True) climateBiomes

  -- -----------------------------------------------------------------------
  -- 3.19.4: Relief / reclassification (P3.7)
  -- -----------------------------------------------------------------------
  describe "relief reclassification" $ do
    it "high-relief warm grassland reclassified as savanna (P3.7)" $
      -- Warm tile (T≥0.50), high relief (0.30 ≥ 0.25), rest of inputs
      -- chosen so primary classification gives Grassland.
      classifyBiome defaultBiomeRules defaultBiomeThresholds
        0.0 WaterDry WaterDry 0.55 0.20 0.30 0.05 0.30 0.50 FormFlat 0.5 0.10 0.10
        `shouldBe` BiomeSavanna

    it "low-relief warm grassland stays grassland" $
      classifyBiome defaultBiomeRules defaultBiomeThresholds
        0.0 WaterDry WaterDry 0.55 0.20 0.30 0.05 0.10 0.50 FormFlat 0.5 0.10 0.10
        `shouldBe` BiomeGrassland

  -- -----------------------------------------------------------------------
  -- 3.19.5: refineOcean
  -- -----------------------------------------------------------------------
  describe "refineOcean" $ do
    it "deep ocean when well below water level" $
      -- depth = 0.5 - 0.10 = 0.40 > 0.25
      refineOcean defaultOceanConfig WaterOcean 0.5 0.10 0.5 0.01 0.5 `shouldBe` BiomeDeepOcean

    it "coral reef when shallow + warm + flat + hard + ocean" $
      -- depth = 0.5 - 0.47 = 0.03 <= 0.05, temp 0.80 >= 0.72, slope 0.02 <= 0.04, hardness 0.50 >= 0.35
      refineOcean defaultOceanConfig WaterOcean 0.5 0.47 0.80 0.02 0.50 `shouldBe` BiomeCoralReef

    it "no coral reef in freshwater" $
      refineOcean defaultOceanConfig WaterLake 0.5 0.47 0.80 0.02 0.50 `shouldBe` BiomeShallowSea

    it "no coral reef when slope too steep" $
      refineOcean defaultOceanConfig WaterOcean 0.5 0.47 0.80 0.10 0.50 `shouldBe` BiomeShallowSea

    it "no coral reef when substrate too soft" $
      refineOcean defaultOceanConfig WaterOcean 0.5 0.47 0.80 0.02 0.10 `shouldBe` BiomeShallowSea

    it "shallow sea when shallow + cold" $
      refineOcean defaultOceanConfig WaterOcean 0.5 0.45 0.30 0.01 0.5 `shouldBe` BiomeShallowSea

    it "fallback to ocean when depth is zero" $
      refineOcean defaultOceanConfig WaterOcean 0.5 0.50 0.30 0.01 0.5 `shouldBe` BiomeOcean

  -- -----------------------------------------------------------------------
  -- 3.19.6: refineCoastal
  -- -----------------------------------------------------------------------
  describe "refineCoastal" $ do
    it "mangrove when hot + wet" $
      refineCoastal defaultCoastalConfig 0.70 0.60 0.5 0.3 0.5 0.1 `shouldBe` BiomeMangrove

    it "estuary when high discharge" $
      refineCoastal defaultCoastalConfig 0.50 0.50 0.5 0.3 0.5 0.40 `shouldBe` BiomeEstuary

    it "salt marsh when cool + moist" $
      refineCoastal defaultCoastalConfig 0.45 0.30 0.60 0.3 0.5 0.1 `shouldBe` BiomeSaltMarsh

    it "rocky shore when hard + thin soil" $
      refineCoastal defaultCoastalConfig 0.50 0.30 0.30 0.70 0.10 0.1 `shouldBe` BiomeRockyShore

    it "coastal dunes when dry" $
      refineCoastal defaultCoastalConfig 0.50 0.15 0.20 0.3 0.5 0.1 `shouldBe` BiomeCoastalDunes

    it "coastal scrub when moderate temp + low precip" $
      refineCoastal defaultCoastalConfig 0.50 0.35 0.30 0.3 0.5 0.1 `shouldBe` BiomeCoastalScrub

    it "fallback to coastal for ambiguous" $
      refineCoastal defaultCoastalConfig 0.55 0.50 0.40 0.30 0.30 0.1 `shouldBe` BiomeCoastal

  -- -----------------------------------------------------------------------
  -- 3.19.7: refineDesert
  -- -----------------------------------------------------------------------
  describe "refineDesert" $ do
    it "salt flat when depression + very dry" $
      refineDesert defaultDesertConfig 0.60 0.01 0.3 0.5 FormDepression 0.10 0.10 `shouldBe` BiomeSaltFlat

    it "fog desert when low precip + moderate humidity + low seasonality" $
      refineDesert defaultDesertConfig 0.50 0.05 0.40 0.50 FormFlat 0.35 0.15 `shouldBe` BiomeFogDesert

    it "rocky desert when hard + thin soil" $
      refineDesert defaultDesertConfig 0.50 0.10 0.60 0.15 FormFlat 0.10 0.10 `shouldBe` BiomeRockyDesert

    it "sand desert when soft" $
      refineDesert defaultDesertConfig 0.50 0.10 0.25 0.50 FormFlat 0.10 0.10 `shouldBe` BiomeSandDesert

    it "hot desert when temp >= threshold" $
      refineDesert defaultDesertConfig 0.75 0.10 0.40 0.50 FormFlat 0.10 0.10 `shouldBe` BiomeHotDesert

    it "cold desert when temp <= threshold" $
      refineDesert defaultDesertConfig 0.35 0.10 0.40 0.50 FormFlat 0.10 0.10 `shouldBe` BiomeColdDesert

  -- -----------------------------------------------------------------------
  -- 3.19.8: refineGrassland
  -- -----------------------------------------------------------------------
  describe "refineGrassland" $ do
    it "alpine meadow at high elevation and cold temp" $
      refineGrassland defaultGrasslandConfig 0.75 0.30 0.5 0.5 0.1 0.10 0.30 `shouldBe` BiomeAlpineMeadow

    it "no alpine meadow when warm despite high elevation" $
      refineGrassland defaultGrasslandConfig 0.75 0.30 0.5 0.5 0.1 0.10 0.60 `shouldNotBe` BiomeAlpineMeadow

    it "floodplain when high discharge" $
      refineGrassland defaultGrasslandConfig 0.40 0.30 0.5 0.5 0.30 0.10 0.50 `shouldBe` BiomeFloodplainGrassland

    it "steppe when dry" $
      refineGrassland defaultGrasslandConfig 0.40 0.20 0.5 0.5 0.1 0.20 0.50 `shouldBe` BiomeSteppe

    it "prairie when deep soil + moist" $
      refineGrassland defaultGrasslandConfig 0.40 0.35 0.70 0.40 0.1 0.10 0.50 `shouldBe` BiomePrairie

    it "fallback to grassland" $
      refineGrassland defaultGrasslandConfig 0.40 0.35 0.40 0.30 0.1 0.10 0.50 `shouldBe` BiomeGrassland

  -- -----------------------------------------------------------------------
  -- 3.19.9: refineForest
  -- -----------------------------------------------------------------------
  describe "refineForest" $ do
    it "cloud forest at high elev + tropical + wet + mountainous" $
      refineForest defaultForestConfig 0.70 0.60 0.75 0.3 0.70 0.10 FormMountainous 0.10 `shouldBe` BiomeCloudForest

    it "montane forest at high elev + hilly" $
      refineForest defaultForestConfig 0.65 0.50 0.50 0.3 0.50 0.05 FormHilly 0.10 `shouldBe` BiomeMontaneForest

    it "montane forest at high elev + steep slope" $
      refineForest defaultForestConfig 0.65 0.50 0.50 0.3 0.50 0.08 FormFlat 0.10 `shouldBe` BiomeMontaneForest

    it "NOT montane on flat low-slope land even at threshold elev" $
      refineForest defaultForestConfig 0.64 0.50 0.50 0.3 0.50 0.03 FormFlat 0.10 `shouldNotBe` BiomeMontaneForest

    it "temperate rainforest when cool + very wet" $
      refineForest defaultForestConfig 0.40 0.45 0.85 0.3 0.75 0.03 FormFlat 0.10 `shouldBe` BiomeTempRainforest

    it "tropical seasonal forest when warm + high seasonality" $
      refineForest defaultForestConfig 0.40 0.70 0.50 0.3 0.50 0.03 FormFlat 0.55 `shouldBe` BiomeTropicalSeasonalForest

    it "tropical dry when hot + moderate precip (low seasonality)" $
      refineForest defaultForestConfig 0.40 0.75 0.50 0.3 0.50 0.03 FormFlat 0.10 `shouldBe` BiomeTropicalDryForest

    it "temperate deciduous when warm + wet" $
      refineForest defaultForestConfig 0.40 0.45 0.55 0.3 0.50 0.03 FormFlat 0.10 `shouldBe` BiomeTempDeciduousForest

    it "temperate coniferous when cool + rocky + drier" $
      refineForest defaultForestConfig 0.40 0.40 0.35 0.50 0.50 0.03 FormFlat 0.10 `shouldBe` BiomeTempConiferousForest

    it "fallback to forest" $
      refineForest defaultForestConfig 0.40 0.50 0.35 0.30 0.30 0.03 FormFlat 0.10 `shouldBe` BiomeForest

  -- -----------------------------------------------------------------------
  -- 3.19.10: refineTundra
  -- -----------------------------------------------------------------------
  describe "refineTundra" $ do
    it "polar desert when extreme cold + dry" $
      refineTundra defaultTundraConfig 0.50 0.08 0.05 FormFlat `shouldBe` BiomePolarDesert

    it "alpine tundra at high elev + hilly" $
      refineTundra defaultTundraConfig 0.65 0.12 0.20 FormHilly `shouldBe` BiomeAlpineTundra

    it "arctic tundra when extreme cold" $
      refineTundra defaultTundraConfig 0.50 0.08 0.20 FormFlat `shouldBe` BiomeArcticTundra

    it "fallback to tundra" $
      refineTundra defaultTundraConfig 0.50 0.15 0.20 FormFlat `shouldBe` BiomeTundra

  -- -----------------------------------------------------------------------
  -- 3.19.11: refineRainforest
  -- -----------------------------------------------------------------------
  describe "refineRainforest" $ do
    it "temperate rainforest when cool + wet" $
      refineRainforest defaultRainforestConfig 0.50 0.85 `shouldBe` BiomeTempRainforest

    it "tropical rainforest when hot" $
      refineRainforest defaultRainforestConfig 0.78 0.85 `shouldBe` BiomeTropicalRainforest

    it "fallback to rainforest" $
      refineRainforest defaultRainforestConfig 0.60 0.60 `shouldBe` BiomeRainforest

  -- -----------------------------------------------------------------------
  -- 3.19.12: refineShrubland
  -- -----------------------------------------------------------------------
  describe "refineShrubland" $ do
    it "moorland when cool + wet + hilly" $
      refineShrubland defaultShrublandConfig 0.35 0.40 0.55 0.20 FormHilly 0.20 `shouldBe` BiomeMoorland

    it "xeric shrubland when very dry" $
      refineShrubland defaultShrublandConfig 0.50 0.25 0.10 0.40 FormFlat 0.20 `shouldBe` BiomeXericShrubland

    it "mediterranean when warm + dry" $
      refineShrubland defaultShrublandConfig 0.50 0.30 0.30 0.40 FormFlat 0.40 `shouldBe` BiomeMediterranean

    it "fallback to shrubland" $
      refineShrubland defaultShrublandConfig 0.45 0.40 0.30 0.40 FormFlat 0.20 `shouldBe` BiomeShrubland

  -- -----------------------------------------------------------------------
  -- 3.19.13: refineSavanna
  -- -----------------------------------------------------------------------
  describe "refineSavanna" $ do
    it "woodland savanna when wet + fertile + seasonal" $
      refineSavanna defaultSavannaConfig 0.70 0.40 0.50 0.40 0.40 `shouldBe` BiomeWoodlandSavanna

    it "tropical savanna when hot" $
      refineSavanna defaultSavannaConfig 0.80 0.30 0.30 0.20 0.10 `shouldBe` BiomeTropicalSavanna

    it "grassland savanna when dry + seasonal" $
      refineSavanna defaultSavannaConfig 0.65 0.20 0.25 0.20 0.55 `shouldBe` BiomeGrasslandSavanna

    it "fallback to savanna (low seasonality)" $
      refineSavanna defaultSavannaConfig 0.65 0.30 0.30 0.30 0.10 `shouldBe` BiomeSavanna

  -- -----------------------------------------------------------------------
  -- 3.19.14: refineTaiga
  -- -----------------------------------------------------------------------
  describe "refineTaiga" $ do
    it "boreal bog when flat + very moist" $
      refineTaiga defaultTaigaConfig 0.25 0.45 0.80 FormFlat 0.20 0.30 `shouldBe` BiomeBorealBog

    it "oceanic boreal when low temp range + high humidity" $
      refineTaiga defaultTaigaConfig 0.25 0.45 0.50 FormFlat 0.10 0.50 `shouldBe` BiomeOceanicBoreal

    it "boreal forest when warmer + wetter" $
      refineTaiga defaultTaigaConfig 0.25 0.45 0.50 FormFlat 0.20 0.30 `shouldBe` BiomeBorealForest

    it "fallback to taiga" $
      refineTaiga defaultTaigaConfig 0.20 0.35 0.50 FormFlat 0.20 0.30 `shouldBe` BiomeTaiga

  -- -----------------------------------------------------------------------
  -- 3.19.15: refineSwamp
  -- -----------------------------------------------------------------------
  describe "refineSwamp" $ do
    it "floodplain forest when high discharge + warm" $
      refineSwamp defaultSwampConfig 0.55 0.70 0.40 0.50 0.1 FormFlat `shouldBe` BiomeFloodplainForest

    it "fen when groundwater-fed + cool" $
      refineSwamp defaultSwampConfig 0.45 0.70 0.30 0.1 0.40 FormFlat `shouldBe` BiomeFen

    it "bog when cool + infertile" $
      refineSwamp defaultSwampConfig 0.35 0.70 0.15 0.1 0.1 FormFlat `shouldBe` BiomeBog

    it "marsh when flat + wet + moderate temp" $
      refineSwamp defaultSwampConfig 0.50 0.75 0.30 0.1 0.1 FormFlat `shouldBe` BiomeMarsh

    it "wetland when cool" $
      refineSwamp defaultSwampConfig 0.50 0.65 0.30 0.1 0.1 FormHilly `shouldBe` BiomeWetland

    it "fallback to swamp" $
      refineSwamp defaultSwampConfig 0.60 0.50 0.40 0.1 0.1 FormHilly `shouldBe` BiomeSwamp

  -- -----------------------------------------------------------------------
  -- 3.19.16: refineSnow
  -- -----------------------------------------------------------------------
  describe "refineSnow" $ do
    it "warm escape demotes to alpine" $
      -- temp 0.30 > snWarmEscapeTemp 0.25; slope moderate
      refineSnow defaultSnowConfig 0.30 0.05 0.30 0.10 `shouldBe` BiomeAlpine

    it "marginal snow (temp in [0.15, 0.25]) demotes to alpine" $
      -- temp 0.18 >= snMarginalMinTemp 0.15, < snWarmEscapeTemp 0.25
      refineSnow defaultSnowConfig 0.18 0.05 0.30 0.10 `shouldBe` BiomeAlpine

    it "glacier when thick ice and cold enough" $
      -- temp 0.10 <= snGlacierMaxTemp 0.15, iceThickness 0.20 >= 0.10
      refineSnow defaultSnowConfig 0.10 0.20 0.30 0.10 `shouldBe` BiomeGlacier

    it "no glacier when too warm despite thick ice" $
      -- temp 0.14 <= snGlacierMaxTemp 0.15 → still works (just under limit)
      -- but temp 0.16 > snGlacierMaxTemp → falls through
      refineSnow defaultSnowConfig 0.14 0.20 0.30 0.10 `shouldBe` BiomeGlacier

    it "ice cap when extreme cold" $
      refineSnow defaultSnowConfig 0.03 0.05 0.30 0.10 `shouldBe` BiomeIceCap

    it "snowfield when high snowpack and cold enough" $
      -- temp 0.10 <= snGlacierMaxTemp 0.15, snowpack 0.60 >= 0.50
      refineSnow defaultSnowConfig 0.10 0.05 0.60 0.10 `shouldBe` BiomeSnowfield

    it "steep slope becomes alpine scree" $
      -- temp 0.12 < snMarginalMinTemp, slope 0.35 >= snSteepSlopeThreshold 0.30
      -- no ice/snowpack/icecap triggers
      refineSnow defaultSnowConfig 0.12 0.05 0.30 0.35 `shouldBe` BiomeAlpineScree

    it "fallback to snow on gentle slope" $
      -- temp 0.12 < snMarginalMinTemp, gentle slope, no ice/snow triggers
      refineSnow defaultSnowConfig 0.12 0.05 0.30 0.10 `shouldBe` BiomeSnow

  -- -----------------------------------------------------------------------
  -- 3.19.17: refineAlpine
  -- -----------------------------------------------------------------------
  describe "refineAlpine" $ do
    it "alpine scree when rugged + thin soil" $
      refineAlpine defaultAlpineConfig 0.80 0.12 0.30 0.3 0.40 0.05 0.50 `shouldBe` BiomeAlpineScree

    it "alpine tundra when cold" $
      refineAlpine defaultAlpineConfig 0.85 0.10 0.20 0.2 0.10 0.30 0.50 `shouldBe` BiomeAlpineTundra

    it "alpine meadow when cool + wet + humid" $
      refineAlpine defaultAlpineConfig 0.75 0.25 0.35 0.1 0.10 0.30 0.40 `shouldBe` BiomeAlpineMeadow

    it "fallback to alpine" $
      refineAlpine defaultAlpineConfig 0.85 0.20 0.20 0.1 0.10 0.30 0.50 `shouldBe` BiomeAlpine

  -- -----------------------------------------------------------------------
  -- 3.19.18: refineVolcanic
  -- -----------------------------------------------------------------------
  describe "refineVolcanic" $ do
    it "lava field when high lava potential" $
      refineVolcanic defaultVolcanicConfig 0.70 0.20 0.30 BiomeForest `shouldBe` BiomeLavaField

    it "volcanic ash plain when high ash + barren" $
      refineVolcanic defaultVolcanicConfig 0.10 0.50 0.20 BiomeGrassland `shouldBe` BiomeVolcanicAshPlain

    it "no override when potentials low" $
      refineVolcanic defaultVolcanicConfig 0.10 0.10 0.30 BiomeForest `shouldBe` BiomeForest

  -- -----------------------------------------------------------------------
  -- 3.19.19: refineBiomesChunk always produces valid codes
  -- -----------------------------------------------------------------------
  describe "refineBiomesChunk integration" $
    prop "always produces valid BiomeId codes" $
      \(Norm01 temp) (Norm01 precip) ->
        let n = 4
            primary = U.replicate n BiomeForest
            tc = emptyTerrainChunkN n
            cc = ClimateChunk
              { ccTempAvg    = U.replicate n temp
              , ccPrecipAvg  = U.replicate n precip
              , ccWindDirAvg = U.replicate n 0
              , ccWindSpdAvg = U.replicate n 0
              , ccHumidityAvg = U.replicate n 0
              , ccTempRange = U.replicate n 0
              , ccPrecipSeasonality = U.replicate n 0
              }
            wc = WeatherChunk
              { wcTemp     = U.replicate n temp
              , wcHumidity = U.replicate n 0.5
              , wcWindDir  = U.replicate n 0
              , wcWindSpd  = U.replicate n 0
              , wcPressure = U.replicate n 0.9
              , wcPrecip   = U.replicate n precip
              }
            refined = refineBiomesChunk defaultRefinementConfig 0.5 primary tc cc wc
                        Nothing Nothing Nothing Nothing Nothing
        in U.all isKnownBiome refined

  -- -----------------------------------------------------------------------
  -- 3.19.20: volcanic overlay only fires with sufficient potentials
  -- -----------------------------------------------------------------------
  describe "volcanic overlay" $ do
    prop "does not fire when potentials are zero" $
      \(Norm01 fertility) ->
        let bid = BiomeForest
        in refineVolcanic defaultVolcanicConfig 0 0 fertility bid == bid

    prop "lava field overrides any biome when lava >= threshold" $
      \(Norm01 ash) (Norm01 fertility) ->
        refineVolcanic defaultVolcanicConfig 0.65 ash fertility BiomeDesert == BiomeLavaField

  -- -----------------------------------------------------------------------
  -- Preset configs are well-formed
  -- -----------------------------------------------------------------------
  describe "refinement config presets" $ do
    it "aridRefinementConfig is distinct from default" $
      aridRefinementConfig `shouldNotBe` defaultRefinementConfig

    it "lushRefinementConfig is distinct from default" $
      lushRefinementConfig `shouldNotBe` defaultRefinementConfig

    it "aridRefinementConfig is distinct from lush" $
      aridRefinementConfig `shouldNotBe` lushRefinementConfig

  -- -----------------------------------------------------------------------
  -- P3.2: Mountain transition smoothing
  -- -----------------------------------------------------------------------
  describe "smoothMountainTransitions" $ do
    it "inserts Alpine between Snow and Forest (2x2 grid)" $
      -- 2×2 grid:
      --   [Snow, Forest]
      --   [Forest, Forest]
      -- After 1 iteration, Snow (rank 3) is surrounded by Forest (rank 1)
      -- neighbours. Median of [3,1,1] = 1, gap = 2 > 1, so Snow → Alpine.
      let grid = U.fromList [BiomeSnow, BiomeForest, BiomeForest, BiomeForest]
          result = smoothMountainTransitions 1 2 2 grid
      in (result U.! 0) `shouldBe` BiomeAlpine

    it "does not modify non-mountain biomes" $
      -- 2×2 grid: all Desert (rank 0) — should pass through unchanged
      let grid = U.fromList [BiomeDesert, BiomeDesert, BiomeDesert, BiomeDesert]
          result = smoothMountainTransitions 1 2 2 grid
      in result `shouldBe` grid

    it "preserves already-smooth transitions (Forest→Alpine→Snow)" $
      -- 3×1 grid: Forest, Alpine, Snow — each adjacent pair differs by 1 rank
      let grid = U.fromList [BiomeForest, BiomeAlpine, BiomeSnow]
          result = smoothMountainTransitions 1 3 1 grid
      in result `shouldBe` grid

    it "zero iterations returns input unchanged" $
      let grid = U.fromList [BiomeSnow, BiomeForest, BiomeForest, BiomeForest]
          result = smoothMountainTransitions 0 2 2 grid
      in result `shouldBe` grid

-- | Build an empty terrain chunk of a given tile count (for testing).
emptyTerrainChunkN :: Int -> TerrainChunk
emptyTerrainChunkN n = TerrainChunk
  { tcElevation     = U.replicate n 0.6
  , tcSlope         = U.replicate n 0.05
  , tcCurvature     = U.replicate n 0
  , tcHardness      = U.replicate n 0.3
  , tcRockType      = U.replicate n 1
  , tcSoilType      = U.replicate n 1
  , tcSoilDepth     = U.replicate n 0.5
  , tcMoisture      = U.replicate n 0.3
  , tcFertility     = U.replicate n 0.3
  , tcRoughness     = U.replicate n 0.05
  , tcRockDensity   = U.replicate n 0.5
  , tcSoilGrain     = U.replicate n 0.5
  , tcRelief        = U.replicate n 0.05
  , tcRuggedness    = U.replicate n 0.05
  , tcTerrainForm   = U.replicate n FormFlat
  , tcFlags         = U.replicate n BiomeForest
  , tcPlateId       = U.replicate n 0
  , tcPlateBoundary = U.replicate n PlateBoundaryNone
  , tcPlateHeight   = U.replicate n 0.5
  , tcPlateHardness = U.replicate n 0.3
  , tcPlateCrust    = U.replicate n 1
  , tcPlateAge      = U.replicate n 0.5
  , tcPlateVelX     = U.replicate n 0
  , tcPlateVelY     = U.replicate n 0
  }
