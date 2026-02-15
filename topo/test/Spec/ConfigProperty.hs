{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | QuickCheck property tests for topo config types.
--
-- Covers:
--
--   * JSON round-trip for random values of every config type
--   * Built-in config variants pass 'validateWorldGenConfig'
module Spec.ConfigProperty (spec) where

import Data.Aeson        (FromJSON, ToJSON, eitherDecode, encode)
import Data.Proxy        (Proxy (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Arbitrary.Config ()   -- bring orphan instances into scope

-- Config types ---------------------------------------------------------
import Topo.BaseHeight        (GenConfig, OceanEdgeDepth)
import Topo.Biome             (BiomeRule, BiomeThresholds,
                               BiomeVegetationConfig)
import Topo.Biome.Refine      (RefinementConfig)
import Topo.Biome.Refine.Alpine     (AlpineConfig)
import Topo.Biome.Refine.Coastal    (CoastalConfig)
import Topo.Biome.Refine.Desert     (DesertConfig)
import Topo.Biome.Refine.Forest     (ForestConfig)
import Topo.Biome.Refine.Grassland  (GrasslandConfig)
import Topo.Biome.Refine.Ocean      (OceanConfig)
import Topo.Biome.Refine.Rainforest (RainforestConfig)
import Topo.Biome.Refine.Savanna    (SavannaConfig)
import Topo.Biome.Refine.Shrubland  (ShrublandConfig)
import Topo.Biome.Refine.Snow       (SnowConfig)
import Topo.Biome.Refine.Swamp      (SwampConfig)
import Topo.Biome.Refine.Taiga      (TaigaConfig)
import Topo.Biome.Refine.Tundra     (TundraConfig)
import Topo.Biome.Refine.Volcanic   (VolcanicConfig)
import Topo.BiomeConfig       (BiomeConfig)
import Topo.Climate.Config
  ( BoundaryConfig, ClimateConfig, MoistureConfig
  , PrecipitationConfig, SeasonalityConfig, TemperatureConfig, WindConfig
  )
import Topo.Erosion           (ErosionConfig)
import Topo.Glacier           (GlacierConfig)
import Topo.Hydrology         (GroundwaterConfig, HydroConfig, RiverConfig)
import Topo.OceanCurrent      (OceanCurrentConfig)
import Topo.Parameters        (ParameterConfig, TerrainFormConfig)
import Topo.Planet            (PlanetConfig, WorldSlice)
import Topo.River             (RiverTopologyConfig)
import Topo.Soil              (SoilConfig)
import Topo.Tectonics         (TectonicsConfig)
import Topo.Types             (WorldExtent)
import Topo.Vegetation        (BiomeFeedbackConfig, VegetationBootstrapConfig)
import Topo.Volcanism         (VolcanismConfig)
import Topo.WaterBody         (WaterBodyConfig)
import Topo.Weather           (WeatherConfig)
import Topo.WorldGen
  ( WorldGenConfig, TerrainConfig
  , defaultWorldGenConfig, aridWorldGenConfig, lushWorldGenConfig
  , validateWorldGenConfig
  )

-- -----------------------------------------------------------------------
-- Spec entry point
-- -----------------------------------------------------------------------

spec :: Spec
spec = describe "Config properties" $ do
  roundTripSpec
  validationSpec

-- -----------------------------------------------------------------------
-- JSON round-trip property tests
-- -----------------------------------------------------------------------

-- | For each config type, verify that 100 random values survive JSON
-- encode → decode without loss.
roundTripSpec :: Spec
roundTripSpec = describe "JSON round-trip (QuickCheck)" $ do
  -- Top-level aggregators
  roundTrip "WorldGenConfig"              (Proxy @WorldGenConfig)
  roundTrip "TerrainConfig"               (Proxy @TerrainConfig)
  -- Base height
  roundTrip "GenConfig"                   (Proxy @GenConfig)
  roundTrip "OceanEdgeDepth"              (Proxy @OceanEdgeDepth)
  -- Tectonics
  roundTrip "TectonicsConfig"             (Proxy @TectonicsConfig)
  -- Erosion
  roundTrip "ErosionConfig"               (Proxy @ErosionConfig)
  -- Hydrology
  roundTrip "HydroConfig"                 (Proxy @HydroConfig)
  roundTrip "RiverConfig"                 (Proxy @RiverConfig)
  roundTrip "GroundwaterConfig"           (Proxy @GroundwaterConfig)
  -- River topology
  roundTrip "RiverTopologyConfig"         (Proxy @RiverTopologyConfig)
  -- Volcanism
  roundTrip "VolcanismConfig"             (Proxy @VolcanismConfig)
  -- Water body
  roundTrip "WaterBodyConfig"             (Proxy @WaterBodyConfig)
  -- Soil
  roundTrip "SoilConfig"                  (Proxy @SoilConfig)
  -- Vegetation
  roundTrip "VegetationBootstrapConfig"   (Proxy @VegetationBootstrapConfig)
  roundTrip "BiomeFeedbackConfig"         (Proxy @BiomeFeedbackConfig)
  -- Climate
  roundTrip "ClimateConfig"               (Proxy @ClimateConfig)
  roundTrip "TemperatureConfig"           (Proxy @TemperatureConfig)
  roundTrip "WindConfig"                  (Proxy @WindConfig)
  roundTrip "MoistureConfig"              (Proxy @MoistureConfig)
  roundTrip "PrecipitationConfig"         (Proxy @PrecipitationConfig)
  roundTrip "BoundaryConfig"              (Proxy @BoundaryConfig)
  roundTrip "SeasonalityConfig"           (Proxy @SeasonalityConfig)
  -- Weather
  roundTrip "WeatherConfig"               (Proxy @WeatherConfig)
  -- Planet / world
  roundTrip "PlanetConfig"                (Proxy @PlanetConfig)
  roundTrip "WorldSlice"                  (Proxy @WorldSlice)
  roundTrip "WorldExtent"                 (Proxy @WorldExtent)
  -- Ocean currents
  roundTrip "OceanCurrentConfig"          (Proxy @OceanCurrentConfig)
  -- Glaciers
  roundTrip "GlacierConfig"               (Proxy @GlacierConfig)
  -- Parameters / terrain form
  roundTrip "ParameterConfig"             (Proxy @ParameterConfig)
  roundTrip "TerrainFormConfig"           (Proxy @TerrainFormConfig)
  -- Biome classification
  roundTrip "BiomeThresholds"             (Proxy @BiomeThresholds)
  roundTrip "BiomeRule"                   (Proxy @BiomeRule)
  roundTrip "BiomeVegetationConfig"       (Proxy @BiomeVegetationConfig)
  roundTrip "BiomeConfig"                 (Proxy @BiomeConfig)
  -- Biome refinement
  roundTrip "RefinementConfig"            (Proxy @RefinementConfig)
  roundTrip "OceanConfig"                 (Proxy @OceanConfig)
  roundTrip "CoastalConfig"               (Proxy @CoastalConfig)
  roundTrip "DesertConfig"                (Proxy @DesertConfig)
  roundTrip "ForestConfig"                (Proxy @ForestConfig)
  roundTrip "GrasslandConfig"             (Proxy @GrasslandConfig)
  roundTrip "RainforestConfig"            (Proxy @RainforestConfig)
  roundTrip "SavannaConfig"               (Proxy @SavannaConfig)
  roundTrip "ShrublandConfig"             (Proxy @ShrublandConfig)
  roundTrip "TaigaConfig"                 (Proxy @TaigaConfig)
  roundTrip "TundraConfig"                (Proxy @TundraConfig)
  roundTrip "SwampConfig"                 (Proxy @SwampConfig)
  roundTrip "SnowConfig"                  (Proxy @SnowConfig)
  roundTrip "AlpineConfig"                (Proxy @AlpineConfig)
  roundTrip "VolcanicConfig"              (Proxy @VolcanicConfig)

-- | Property: for any @x :: a@, @decode (encode x) === Right x@.
roundTrip
  :: forall a. (Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a)
  => String -> Proxy a -> Spec
roundTrip name _ =
  prop ("round-trips " ++ name) $
    \(x :: a) -> eitherDecode (encode x) === Right x

-- -----------------------------------------------------------------------
-- Validation tests
-- -----------------------------------------------------------------------

-- | Verify that every built-in 'WorldGenConfig' variant passes
-- 'validateWorldGenConfig' (non-negative iteration counts, etc.).
validationSpec :: Spec
validationSpec = describe "Built-in configs pass validation" $ do
  validates "defaultWorldGenConfig" defaultWorldGenConfig
  validates "aridWorldGenConfig"    aridWorldGenConfig
  validates "lushWorldGenConfig"    lushWorldGenConfig

-- | Helper: assert that 'validateWorldGenConfig' succeeds.
validates :: String -> WorldGenConfig -> Spec
validates name cfg =
  it (name ++ " passes validateWorldGenConfig") $
    case validateWorldGenConfig cfg of
      Right _  -> pure ()
      Left err -> expectationFailure (show err)
