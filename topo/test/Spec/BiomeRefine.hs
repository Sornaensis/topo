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

-- | All valid biome codes (0-8, 10-62; code 9 is a gap).
allKnownCodes :: [Word16]
allKnownCodes = [0..8] ++ [10..62]

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
                           waterLevel WaterDry WaterDry temp precip elev 0.1 0.1 0.3 FormFlat)

    prop "always produces a valid known BiomeId for any ocean tile" $
      \(Norm01 temp) (Norm01 precip) (Norm01 elev') ->
        let waterLevel = 0.5
            elev = elev' * waterLevel
        in isKnownBiome (classifyBiome defaultBiomeRules defaultBiomeThresholds
                           waterLevel WaterOcean WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat)

    prop "classifies ocean when WaterOcean" $
      \(Norm01 temp) (Norm01 precip) (Norm01 elev') ->
        let waterLevel = 0.5
            elev = elev' * 0.49  -- always below threshold
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterOcean WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat == BiomeOcean

    prop "classifies snow at very high elevation" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterDry temp precip 0.95 0.1 0.1 0.3 FormFlat == BiomeSnow

    -- Ocean-adjacent coastal check
    prop "ocean-adjacent low tile classifies as BiomeCoastal" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01  -- just above water level, within coastal band
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterOcean temp precip elev 0.1 0.1 0.3 FormFlat == BiomeCoastal

    prop "lake-adjacent low tile classifies as BiomeSwamp (not BiomeCoastal)" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterLake temp precip elev 0.1 0.1 0.3 FormFlat == BiomeSwamp

    prop "no-adjacent-water low tile classifies as BiomeSwamp (not BiomeCoastal)" $
      \(Norm01 temp) (Norm01 precip) ->
        let waterLevel = 0.5
            elev = waterLevel + 0.01
        in classifyBiome defaultBiomeRules defaultBiomeThresholds
             waterLevel WaterDry WaterDry temp precip elev 0.1 0.1 0.3 FormFlat == BiomeSwamp

  -- -----------------------------------------------------------------------
  -- 3.19.2: All sub-biome codes round-trip through biomeIdFromCode
  -- -----------------------------------------------------------------------
  describe "biomeIdFromCode round-trip" $
    it "all codes 0-8, 10-62 round-trip" $
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
          samples =
            [ classifyBiome defaultBiomeRules defaultBiomeThresholds
                waterLevel WaterDry WaterDry t p 0.60 0.1 0.1 0.3 FormFlat
            | t <- [0.05, 0.10 .. 1.0]
            , p <- [0.05, 0.10 .. 1.0]
            ]
          found = nub samples
      mapM_ (\b -> b `elem` found `shouldBe` True) climateBiomes

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
      refineDesert defaultDesertConfig 0.60 0.01 0.3 0.5 FormDepression `shouldBe` BiomeSaltFlat

    it "rocky desert when hard + thin soil" $
      refineDesert defaultDesertConfig 0.50 0.10 0.60 0.15 FormFlat `shouldBe` BiomeRockyDesert

    it "sand desert when soft" $
      refineDesert defaultDesertConfig 0.50 0.10 0.25 0.50 FormFlat `shouldBe` BiomeSandDesert

    it "hot desert when temp >= threshold" $
      refineDesert defaultDesertConfig 0.75 0.10 0.40 0.50 FormFlat `shouldBe` BiomeHotDesert

    it "cold desert when temp <= threshold" $
      refineDesert defaultDesertConfig 0.35 0.10 0.40 0.50 FormFlat `shouldBe` BiomeColdDesert

  -- -----------------------------------------------------------------------
  -- 3.19.8: refineGrassland
  -- -----------------------------------------------------------------------
  describe "refineGrassland" $ do
    it "alpine meadow at high elevation" $
      refineGrassland defaultGrasslandConfig 0.60 0.30 0.5 0.5 0.1 0.10 `shouldBe` BiomeAlpineMeadow

    it "floodplain when high discharge" $
      refineGrassland defaultGrasslandConfig 0.40 0.30 0.5 0.5 0.30 0.10 `shouldBe` BiomeFloodplainGrassland

    it "steppe when dry" $
      refineGrassland defaultGrasslandConfig 0.40 0.20 0.5 0.5 0.1 0.20 `shouldBe` BiomeSteppe

    it "prairie when deep soil + moist" $
      refineGrassland defaultGrasslandConfig 0.40 0.35 0.70 0.40 0.1 0.10 `shouldBe` BiomePrairie

    it "fallback to grassland" $
      refineGrassland defaultGrasslandConfig 0.40 0.35 0.40 0.30 0.1 0.10 `shouldBe` BiomeGrassland

  -- -----------------------------------------------------------------------
  -- 3.19.9: refineForest
  -- -----------------------------------------------------------------------
  describe "refineForest" $ do
    it "cloud forest at high elev + tropical + wet + mountainous" $
      refineForest defaultForestConfig 0.70 0.60 0.75 0.3 0.70 0.10 FormMountainous `shouldBe` BiomeCloudForest

    it "montane forest at high elev + hilly" $
      refineForest defaultForestConfig 0.65 0.50 0.50 0.3 0.50 0.05 FormHilly `shouldBe` BiomeMontaneForest

    it "montane forest at high elev + steep slope" $
      refineForest defaultForestConfig 0.65 0.50 0.50 0.3 0.50 0.08 FormFlat `shouldBe` BiomeMontaneForest

    it "NOT montane on flat low-slope land even at threshold elev" $
      refineForest defaultForestConfig 0.64 0.50 0.50 0.3 0.50 0.03 FormFlat `shouldNotBe` BiomeMontaneForest

    it "temperate rainforest when cool + very wet" $
      refineForest defaultForestConfig 0.40 0.45 0.85 0.3 0.75 0.03 FormFlat `shouldBe` BiomeTempRainforest

    it "tropical dry when hot + moderate precip" $
      refineForest defaultForestConfig 0.40 0.75 0.50 0.3 0.50 0.03 FormFlat `shouldBe` BiomeTropicalDryForest

    it "temperate deciduous when warm + wet" $
      refineForest defaultForestConfig 0.40 0.45 0.55 0.3 0.50 0.03 FormFlat `shouldBe` BiomeTempDeciduousForest

    it "temperate coniferous when cool + rocky + drier" $
      refineForest defaultForestConfig 0.40 0.40 0.35 0.50 0.50 0.03 FormFlat `shouldBe` BiomeTempConiferousForest

    it "fallback to forest" $
      refineForest defaultForestConfig 0.40 0.50 0.35 0.30 0.30 0.03 FormFlat `shouldBe` BiomeForest

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
    it "woodland savanna when wet + fertile" $
      refineSavanna defaultSavannaConfig 0.70 0.40 0.50 0.40 `shouldBe` BiomeWoodlandSavanna

    it "tropical savanna when hot" $
      refineSavanna defaultSavannaConfig 0.80 0.30 0.30 0.30 `shouldBe` BiomeTropicalSavanna

    it "grassland savanna when dry" $
      refineSavanna defaultSavannaConfig 0.65 0.20 0.30 0.25 `shouldBe` BiomeGrasslandSavanna

    it "fallback to savanna" $
      refineSavanna defaultSavannaConfig 0.65 0.30 0.30 0.30 `shouldBe` BiomeSavanna

  -- -----------------------------------------------------------------------
  -- 3.19.14: refineTaiga
  -- -----------------------------------------------------------------------
  describe "refineTaiga" $ do
    it "boreal bog when flat + very moist" $
      refineTaiga defaultTaigaConfig 0.25 0.45 0.80 FormFlat `shouldBe` BiomeBorealBog

    it "boreal forest when warmer + wetter" $
      refineTaiga defaultTaigaConfig 0.25 0.45 0.50 FormFlat `shouldBe` BiomeBorealForest

    it "fallback to taiga" $
      refineTaiga defaultTaigaConfig 0.20 0.35 0.50 FormFlat `shouldBe` BiomeTaiga

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
    it "glacier when thick ice" $
      refineSnow defaultSnowConfig 0.10 0.20 0.30 `shouldBe` BiomeGlacier

    it "ice cap when extreme cold" $
      refineSnow defaultSnowConfig 0.03 0.05 0.30 `shouldBe` BiomeIceCap

    it "snowfield when high snowpack" $
      refineSnow defaultSnowConfig 0.10 0.05 0.60 `shouldBe` BiomeSnowfield

    it "fallback to snow" $
      refineSnow defaultSnowConfig 0.10 0.05 0.30 `shouldBe` BiomeSnow

  -- -----------------------------------------------------------------------
  -- 3.19.17: refineAlpine
  -- -----------------------------------------------------------------------
  describe "refineAlpine" $ do
    it "alpine scree when rugged + thin soil" $
      refineAlpine defaultAlpineConfig 0.80 0.12 0.30 0.3 0.40 0.05 `shouldBe` BiomeAlpineScree

    it "alpine tundra when cold + high" $
      refineAlpine defaultAlpineConfig 0.85 0.10 0.20 0.2 0.10 0.30 `shouldBe` BiomeAlpineTundra

    it "alpine meadow at lower band + wet" $
      refineAlpine defaultAlpineConfig 0.75 0.20 0.35 0.1 0.10 0.30 `shouldBe` BiomeAlpineMeadow

    it "fallback to alpine" $
      refineAlpine defaultAlpineConfig 0.85 0.20 0.20 0.1 0.10 0.30 `shouldBe` BiomeAlpine

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
