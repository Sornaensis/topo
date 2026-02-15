{-# LANGUAGE ScopedTypeVariables #-}
-- | Round-trip property tests for JSON serialization of all config types.
--
-- Every config type in the topo library has 'ToJSON' / 'FromJSON' instances.
-- These tests verify:
--
-- 1. __Round-trip__: @decode (encode x) == Just x@ for the default config.
-- 2. __Forward-compatible__: An empty JSON object @{}@ decodes to the
--    default config (all fields are optional via 'mergeDefaults').
-- 3. __Preset round-trip__: Named preset variants (arid, lush) also
--    survive serialization.
module Spec.ConfigJSON (spec) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, Value(..))
import Data.Proxy (Proxy(..))
import Test.Hspec

import Topo.BaseHeight
  ( GenConfig, OceanEdgeDepth
  , defaultGenConfig, defaultOceanEdgeDepth
  )
import Topo.Biome
  ( BiomeRule, BiomeThresholds, BiomeVegetationConfig
  , defaultBiomeRules, defaultBiomeThresholds, defaultBiomeVegetationConfig
  )
import Topo.Biome.Refine
  ( RefinementConfig
  , defaultRefinementConfig, aridRefinementConfig, lushRefinementConfig
  )
import Topo.Biome.Refine.Alpine   (AlpineConfig, defaultAlpineConfig)
import Topo.Biome.Refine.Coastal  (CoastalConfig, defaultCoastalConfig)
import Topo.Biome.Refine.Desert   (DesertConfig, defaultDesertConfig)
import Topo.Biome.Refine.Forest   (ForestConfig, defaultForestConfig)
import Topo.Biome.Refine.Grassland (GrasslandConfig, defaultGrasslandConfig)
import Topo.Biome.Refine.Ocean    (OceanConfig, defaultOceanConfig)
import Topo.Biome.Refine.Rainforest (RainforestConfig, defaultRainforestConfig)
import Topo.Biome.Refine.Savanna  (SavannaConfig, defaultSavannaConfig)
import Topo.Biome.Refine.Shrubland (ShrublandConfig, defaultShrublandConfig)
import Topo.Biome.Refine.Snow     (SnowConfig, defaultSnowConfig)
import Topo.Biome.Refine.Swamp    (SwampConfig, defaultSwampConfig)
import Topo.Biome.Refine.Taiga    (TaigaConfig, defaultTaigaConfig)
import Topo.Biome.Refine.Tundra   (TundraConfig, defaultTundraConfig)
import Topo.Biome.Refine.Volcanic (VolcanicConfig, defaultVolcanicConfig)
import Topo.BiomeConfig            (BiomeConfig, defaultBiomeConfig, aridBiomeConfig, lushBiomeConfig)
import Topo.Climate.Config
  ( ClimateConfig, TemperatureConfig, WindConfig, MoistureConfig
  , PrecipitationConfig, BoundaryConfig, SeasonalityConfig
  , defaultClimateConfig, defaultTemperatureConfig, defaultWindConfig
  , defaultMoistureConfig, defaultPrecipitationConfig
  , defaultBoundaryConfig, defaultSeasonalityConfig
  )
import Topo.Erosion                (ErosionConfig, defaultErosionConfig)
import Topo.Glacier                (GlacierConfig, defaultGlacierConfig)
import Topo.Hydrology
  ( HydroConfig, RiverConfig, GroundwaterConfig
  , defaultHydroConfig, defaultRiverConfig, defaultGroundwaterConfig
  )
import Topo.OceanCurrent           (OceanCurrentConfig, defaultOceanCurrentConfig)
import Topo.Parameters             (ParameterConfig, TerrainFormConfig, defaultParameterConfig, defaultTerrainFormConfig)
import Topo.Planet                  (PlanetConfig, WorldSlice, defaultPlanetConfig, defaultWorldSlice)
import Topo.River                   (RiverTopologyConfig, defaultRiverTopologyConfig)
import Topo.Soil                    (SoilConfig, defaultSoilConfig)
import Topo.Tectonics               (TectonicsConfig, defaultTectonicsConfig)
import Topo.Vegetation
  ( VegetationBootstrapConfig, BiomeFeedbackConfig
  , defaultVegetationBootstrapConfig, defaultBiomeFeedbackConfig
  )
import Topo.Volcanism               (VolcanismConfig, defaultVolcanismConfig)
import Topo.WaterBody                (WaterBodyConfig, defaultWaterBodyConfig)
import Topo.Weather                  (WeatherConfig, defaultWeatherConfig)
import Topo.WorldGen
  ( TerrainConfig, WorldGenConfig
  , defaultTerrainConfig, defaultWorldGenConfig
  , aridWorldGenConfig, lushWorldGenConfig
  )

-- | Assert that encoding then decoding a config type yields the original.
roundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> Spec
roundTrip label x =
  it (label ++ " roundtrips through JSON") $
    decode (encode x) `shouldBe` Just x

-- | Assert that an empty JSON object decodes to the default config.
emptyDecodes :: forall a. (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> Spec
emptyDecodes label defVal =
  it (label ++ " decodes from empty JSON to default") $
    (decode (encode (Object mempty)) :: Maybe a) `shouldBe` Just defVal

spec :: Spec
spec = describe "Config JSON serialization" $ do

  describe "Top-level aggregators" $ do
    roundTrip "WorldGenConfig" defaultWorldGenConfig
    roundTrip "TerrainConfig" defaultTerrainConfig
    emptyDecodes "WorldGenConfig" defaultWorldGenConfig
    emptyDecodes "TerrainConfig" defaultTerrainConfig

  describe "Preset variants" $ do
    roundTrip "aridWorldGenConfig" aridWorldGenConfig
    roundTrip "lushWorldGenConfig" lushWorldGenConfig
    roundTrip "aridBiomeConfig" aridBiomeConfig
    roundTrip "lushBiomeConfig" lushBiomeConfig
    roundTrip "aridRefinementConfig" aridRefinementConfig
    roundTrip "lushRefinementConfig" lushRefinementConfig

  describe "Planet / World" $ do
    roundTrip "PlanetConfig" defaultPlanetConfig
    roundTrip "WorldSlice" defaultWorldSlice
    emptyDecodes "PlanetConfig" defaultPlanetConfig
    emptyDecodes "WorldSlice" defaultWorldSlice

  describe "Base Height" $ do
    roundTrip "GenConfig" defaultGenConfig
    roundTrip "OceanEdgeDepth" defaultOceanEdgeDepth
    emptyDecodes "GenConfig" defaultGenConfig
    emptyDecodes "OceanEdgeDepth" defaultOceanEdgeDepth

  describe "Tectonics" $ do
    roundTrip "TectonicsConfig" defaultTectonicsConfig
    emptyDecodes "TectonicsConfig" defaultTectonicsConfig

  describe "Erosion" $ do
    roundTrip "ErosionConfig" defaultErosionConfig
    emptyDecodes "ErosionConfig" defaultErosionConfig

  describe "Hydrology" $ do
    roundTrip "HydroConfig" defaultHydroConfig
    roundTrip "RiverConfig" defaultRiverConfig
    roundTrip "GroundwaterConfig" defaultGroundwaterConfig
    roundTrip "RiverTopologyConfig" defaultRiverTopologyConfig
    emptyDecodes "HydroConfig" defaultHydroConfig
    emptyDecodes "RiverConfig" defaultRiverConfig
    emptyDecodes "GroundwaterConfig" defaultGroundwaterConfig
    emptyDecodes "RiverTopologyConfig" defaultRiverTopologyConfig

  describe "Volcanism" $ do
    roundTrip "VolcanismConfig" defaultVolcanismConfig
    emptyDecodes "VolcanismConfig" defaultVolcanismConfig

  describe "Water Body" $ do
    roundTrip "WaterBodyConfig" defaultWaterBodyConfig
    emptyDecodes "WaterBodyConfig" defaultWaterBodyConfig

  describe "Soil" $ do
    roundTrip "SoilConfig" defaultSoilConfig
    emptyDecodes "SoilConfig" defaultSoilConfig

  describe "Vegetation" $ do
    roundTrip "VegetationBootstrapConfig" defaultVegetationBootstrapConfig
    roundTrip "BiomeFeedbackConfig" defaultBiomeFeedbackConfig
    emptyDecodes "VegetationBootstrapConfig" defaultVegetationBootstrapConfig
    emptyDecodes "BiomeFeedbackConfig" defaultBiomeFeedbackConfig

  describe "Climate" $ do
    roundTrip "ClimateConfig" defaultClimateConfig
    roundTrip "TemperatureConfig" defaultTemperatureConfig
    roundTrip "WindConfig" defaultWindConfig
    roundTrip "MoistureConfig" defaultMoistureConfig
    roundTrip "PrecipitationConfig" defaultPrecipitationConfig
    roundTrip "BoundaryConfig" defaultBoundaryConfig
    roundTrip "SeasonalityConfig" defaultSeasonalityConfig
    emptyDecodes "ClimateConfig" defaultClimateConfig
    emptyDecodes "TemperatureConfig" defaultTemperatureConfig
    emptyDecodes "WindConfig" defaultWindConfig
    emptyDecodes "MoistureConfig" defaultMoistureConfig
    emptyDecodes "PrecipitationConfig" defaultPrecipitationConfig
    emptyDecodes "BoundaryConfig" defaultBoundaryConfig
    emptyDecodes "SeasonalityConfig" defaultSeasonalityConfig

  describe "Ocean Currents" $ do
    roundTrip "OceanCurrentConfig" defaultOceanCurrentConfig
    emptyDecodes "OceanCurrentConfig" defaultOceanCurrentConfig

  describe "Glaciers" $ do
    roundTrip "GlacierConfig" defaultGlacierConfig
    emptyDecodes "GlacierConfig" defaultGlacierConfig

  describe "Parameters / Terrain Form" $ do
    roundTrip "ParameterConfig" defaultParameterConfig
    roundTrip "TerrainFormConfig" defaultTerrainFormConfig
    emptyDecodes "ParameterConfig" defaultParameterConfig
    emptyDecodes "TerrainFormConfig" defaultTerrainFormConfig

  describe "Biome Classification" $ do
    roundTrip "BiomeConfig" defaultBiomeConfig
    roundTrip "BiomeThresholds" defaultBiomeThresholds
    roundTrip "BiomeRule" defaultBiomeRules
    roundTrip "BiomeVegetationConfig" defaultBiomeVegetationConfig
    emptyDecodes "BiomeConfig" defaultBiomeConfig
    emptyDecodes "BiomeThresholds" defaultBiomeThresholds
    emptyDecodes "BiomeRule" defaultBiomeRules
    emptyDecodes "BiomeVegetationConfig" defaultBiomeVegetationConfig

  describe "Biome Refinement" $ do
    roundTrip "RefinementConfig" defaultRefinementConfig
    emptyDecodes "RefinementConfig" defaultRefinementConfig

  describe "Biome Refinement sub-configs" $ do
    roundTrip "AlpineConfig"     defaultAlpineConfig
    roundTrip "CoastalConfig"    defaultCoastalConfig
    roundTrip "DesertConfig"     defaultDesertConfig
    roundTrip "ForestConfig"     defaultForestConfig
    roundTrip "GrasslandConfig"  defaultGrasslandConfig
    roundTrip "OceanConfig"      defaultOceanConfig
    roundTrip "RainforestConfig" defaultRainforestConfig
    roundTrip "SavannaConfig"    defaultSavannaConfig
    roundTrip "ShrublandConfig"  defaultShrublandConfig
    roundTrip "SnowConfig"       defaultSnowConfig
    roundTrip "SwampConfig"      defaultSwampConfig
    roundTrip "TaigaConfig"      defaultTaigaConfig
    roundTrip "TundraConfig"     defaultTundraConfig
    roundTrip "VolcanicConfig"   defaultVolcanicConfig
    emptyDecodes "AlpineConfig"     defaultAlpineConfig
    emptyDecodes "CoastalConfig"    defaultCoastalConfig
    emptyDecodes "DesertConfig"     defaultDesertConfig
    emptyDecodes "ForestConfig"     defaultForestConfig
    emptyDecodes "GrasslandConfig"  defaultGrasslandConfig
    emptyDecodes "OceanConfig"      defaultOceanConfig
    emptyDecodes "RainforestConfig" defaultRainforestConfig
    emptyDecodes "SavannaConfig"    defaultSavannaConfig
    emptyDecodes "ShrublandConfig"  defaultShrublandConfig
    emptyDecodes "SnowConfig"       defaultSnowConfig
    emptyDecodes "SwampConfig"      defaultSwampConfig
    emptyDecodes "TaigaConfig"      defaultTaigaConfig
    emptyDecodes "TundraConfig"     defaultTundraConfig
    emptyDecodes "VolcanicConfig"   defaultVolcanicConfig

  describe "Weather" $ do
    roundTrip "WeatherConfig" defaultWeatherConfig
    emptyDecodes "WeatherConfig" defaultWeatherConfig
