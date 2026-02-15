{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan 'Arbitrary' instances for all topo config types.
--
-- Uses a minimal GHC.Generics-based derivation scheme that generates
-- finite 'Float' values (in @[-1e6, 1e6]@) and non-negative 'Int' values
-- (in @[0, 100]@).  Manual instances are provided for types without
-- 'Generic' ('BiomeId') and for 'BiomeRule' whose generic representation
-- contains 'Float' nested inside tuples that the override cannot reach.
module Arbitrary.Config () where

import Data.Word              (Word16)
import GHC.Generics
import Test.QuickCheck

-- Config types ---------------------------------------------------------
import Topo.BaseHeight        (GenConfig, OceanEdgeDepth)
import Topo.Biome             (BiomeRule (..), BiomeThresholds,
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
import Topo.Types             (BiomeId, WorldExtent, biomeIdFromCode)
import Topo.Vegetation        (BiomeFeedbackConfig, VegetationBootstrapConfig)
import Topo.Volcanism         (VolcanismConfig)
import Topo.WaterBody         (WaterBodyConfig)
import Topo.Weather           (WeatherConfig)
import Topo.WorldGen          (TerrainConfig, WorldGenConfig)

-- -----------------------------------------------------------------------
-- Generic Arbitrary derivation
-- -----------------------------------------------------------------------

-- | Typeclass for generating arbitrary values from Generic representations.
class GArbitrary f where
  garbitrary :: Gen (f p)

instance GArbitrary U1 where
  garbitrary = pure U1

-- | Default for leaf fields: delegate to 'Arbitrary'.
instance {-# OVERLAPPABLE #-} Arbitrary c => GArbitrary (K1 i c) where
  garbitrary = K1 <$> arbitrary

-- | Override for 'Float' fields: finite values only.
instance {-# OVERLAPPING #-} GArbitrary (K1 i Float) where
  garbitrary = K1 <$> choose (-1e6, 1e6)

-- | Override for 'Int' fields: non-negative values only.
instance {-# OVERLAPPING #-} GArbitrary (K1 i Int) where
  garbitrary = K1 <$> choose (0, 100)

instance (GArbitrary f, GArbitrary g) => GArbitrary (f :*: g) where
  garbitrary = (:*:) <$> garbitrary <*> garbitrary

instance GArbitrary f => GArbitrary (M1 i t f) where
  garbitrary = M1 <$> garbitrary

-- | Derive @Gen a@ from the 'Generic' representation of @a@.
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> garbitrary

-- -----------------------------------------------------------------------
-- Manual instances
-- -----------------------------------------------------------------------

-- | Known valid biome codes (0–8, 10–65; code 9 is unused).
validBiomeCodes :: [Word16]
validBiomeCodes = [0..8] ++ [10..65]

instance Arbitrary BiomeId where
  arbitrary = do
    code <- elements validBiomeCodes
    case biomeIdFromCode code of
      Right bid -> pure bid
      Left  _   -> error ("validBiomeCodes contains invalid code: " ++ show code)

-- | Manual instance: the inner list contains @(Float, Float)@ tuples that
-- would bypass the 'GArbitrary' 'Float' override.
instance Arbitrary BiomeRule where
  arbitrary = BiomeRule <$> listOf genEntry
    where
      genEntry = (,,) <$> arbitrary <*> genRange <*> genRange
      genRange = (,) <$> genFloat <*> genFloat
      genFloat = choose (-1e6 :: Float, 1e6)

-- -----------------------------------------------------------------------
-- Generic Arbitrary instances for all config types
-- -----------------------------------------------------------------------

-- Top-level aggregators
instance Arbitrary WorldGenConfig              where arbitrary = genericArbitrary
instance Arbitrary TerrainConfig               where arbitrary = genericArbitrary

-- Base height / terrain generation
instance Arbitrary GenConfig                   where arbitrary = genericArbitrary
instance Arbitrary OceanEdgeDepth              where arbitrary = genericArbitrary

-- Tectonics
instance Arbitrary TectonicsConfig             where arbitrary = genericArbitrary

-- Erosion
instance Arbitrary ErosionConfig               where arbitrary = genericArbitrary

-- Hydrology
instance Arbitrary HydroConfig                 where arbitrary = genericArbitrary
instance Arbitrary RiverConfig                 where arbitrary = genericArbitrary
instance Arbitrary GroundwaterConfig           where arbitrary = genericArbitrary

-- River topology
instance Arbitrary RiverTopologyConfig         where arbitrary = genericArbitrary

-- Volcanism
instance Arbitrary VolcanismConfig             where arbitrary = genericArbitrary

-- Water body
instance Arbitrary WaterBodyConfig             where arbitrary = genericArbitrary

-- Soil
instance Arbitrary SoilConfig                  where arbitrary = genericArbitrary

-- Vegetation
instance Arbitrary VegetationBootstrapConfig   where arbitrary = genericArbitrary
instance Arbitrary BiomeFeedbackConfig         where arbitrary = genericArbitrary

-- Climate
instance Arbitrary ClimateConfig               where arbitrary = genericArbitrary
instance Arbitrary TemperatureConfig           where arbitrary = genericArbitrary
instance Arbitrary WindConfig                  where arbitrary = genericArbitrary
instance Arbitrary MoistureConfig              where arbitrary = genericArbitrary
instance Arbitrary PrecipitationConfig         where arbitrary = genericArbitrary
instance Arbitrary BoundaryConfig              where arbitrary = genericArbitrary
instance Arbitrary SeasonalityConfig           where arbitrary = genericArbitrary

-- Weather
instance Arbitrary WeatherConfig               where arbitrary = genericArbitrary

-- Planet / world
instance Arbitrary PlanetConfig                where arbitrary = genericArbitrary
instance Arbitrary WorldSlice                  where arbitrary = genericArbitrary
instance Arbitrary WorldExtent                 where arbitrary = genericArbitrary

-- Ocean currents
instance Arbitrary OceanCurrentConfig          where arbitrary = genericArbitrary

-- Glaciers
instance Arbitrary GlacierConfig               where arbitrary = genericArbitrary

-- Parameters / terrain form
instance Arbitrary ParameterConfig             where arbitrary = genericArbitrary
instance Arbitrary TerrainFormConfig           where arbitrary = genericArbitrary

-- Biome classification
instance Arbitrary BiomeThresholds             where arbitrary = genericArbitrary
instance Arbitrary BiomeVegetationConfig       where arbitrary = genericArbitrary
instance Arbitrary BiomeConfig                 where arbitrary = genericArbitrary

-- Biome refinement
instance Arbitrary RefinementConfig            where arbitrary = genericArbitrary
instance Arbitrary OceanConfig                 where arbitrary = genericArbitrary
instance Arbitrary CoastalConfig               where arbitrary = genericArbitrary
instance Arbitrary DesertConfig                where arbitrary = genericArbitrary
instance Arbitrary ForestConfig                where arbitrary = genericArbitrary
instance Arbitrary GrasslandConfig             where arbitrary = genericArbitrary
instance Arbitrary RainforestConfig            where arbitrary = genericArbitrary
instance Arbitrary SavannaConfig               where arbitrary = genericArbitrary
instance Arbitrary ShrublandConfig             where arbitrary = genericArbitrary
instance Arbitrary TaigaConfig                 where arbitrary = genericArbitrary
instance Arbitrary TundraConfig                where arbitrary = genericArbitrary
instance Arbitrary SwampConfig                 where arbitrary = genericArbitrary
instance Arbitrary SnowConfig                  where arbitrary = genericArbitrary
instance Arbitrary AlpineConfig                where arbitrary = genericArbitrary
instance Arbitrary VolcanicConfig              where arbitrary = genericArbitrary
