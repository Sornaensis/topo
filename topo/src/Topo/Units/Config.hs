{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Real-world-unit config types and bidirectional conversion.
--
-- This module provides \"real-world\" variants of the internal normalised
-- config types (e.g. 'TemperatureConfig', 'BiomeThresholds').  Each
-- @Real*@ record expresses the same parameters in physical units (°C,
-- metres, mm\/yr, °C\/km) while the internal records stay normalised
-- @[0,1]@.
--
-- Conversion is parameterised by 'UnitScales' so that custom planet
-- definitions work without code changes.
--
-- == Usage
--
-- @
-- -- Build a config from real-world values:
-- let real = defaultRealTemperatureConfig
--               { rtcEquatorTemp = 26.0 }  -- 26 °C
--     norm = realToTemperatureConfig defaultUnitScales real
--
-- -- Extract real-world values from a normalised config:
-- let real' = temperatureConfigToReal defaultUnitScales defaultTemperatureConfig
-- @
--
-- == Design invariant
--
-- For every config section:
--
-- @
-- realTo*Config s . *ConfigToReal s ≡ id      (modulo float rounding)
-- @
module Topo.Units.Config
  ( -- * Temperature
    RealTemperatureConfig(..)
  , defaultRealTemperatureConfig
  , realToTemperatureConfig
  , temperatureConfigToReal
    -- * Glacier
  , RealGlacierConfig(..)
  , defaultRealGlacierConfig
  , realToGlacierConfig
  , glacierConfigToReal
    -- * Biome thresholds
  , RealBiomeThresholds(..)
  , defaultRealBiomeThresholds
  , realToBiomeThresholds
  , biomeThresholdsToReal
    -- * Vegetation bootstrap
  , RealVegetationBootstrapConfig(..)
  , defaultRealVegetationBootstrapConfig
  , realToVegetationBootstrapConfig
  , vegetationBootstrapConfigToReal
    -- * Forest refinement
  , RealForestConfig(..)
  , defaultRealForestConfig
  , realToForestConfig
  , forestConfigToReal
    -- * Desert refinement
  , RealDesertConfig(..)
  , defaultRealDesertConfig
  , realToDesertConfig
  , desertConfigToReal
    -- * Ocean refinement
  , RealOceanConfig(..)
  , defaultRealOceanConfig
  , realToOceanConfig
  , oceanConfigToReal
    -- * Tundra refinement
  , RealTundraConfig(..)
  , defaultRealTundraConfig
  , realToTundraConfig
  , tundraConfigToReal
    -- * Snow refinement
  , RealSnowConfig(..)
  , defaultRealSnowConfig
  , realToSnowConfig
  , snowConfigToReal
    -- * Lapse-rate helpers
  , lapseRateCPerKm
  , lapseRateFromCPerKm
  ) where

import GHC.Generics (Generic)

import Topo.Units
  ( UnitScales(..)
  , defaultUnitScales
  , normToC, cToNorm
  , normToMetres, metresToNorm
  )
import Topo.Climate.Config
  ( TemperatureConfig(..)
  , defaultTemperatureConfig
  )
import Topo.Glacier (GlacierConfig(..), defaultGlacierConfig)
import Topo.Biome
  ( BiomeThresholds(..)
  , defaultBiomeThresholds
  )
import Topo.Types (BiomeId)
import Topo.Biome.Refine.Forest (ForestConfig(..), defaultForestConfig)
import Topo.Biome.Refine.Desert (DesertConfig(..), defaultDesertConfig)
import Topo.Biome.Refine.Ocean  (OceanConfig(..), defaultOceanConfig)
import Topo.Biome.Refine.Tundra (TundraConfig(..), defaultTundraConfig)
import Topo.Biome.Refine.Snow   (SnowConfig(..), defaultSnowConfig)
import Topo.Vegetation
  ( VegetationBootstrapConfig(..)
  , defaultVegetationBootstrapConfig
  )

-- =========================================================================
-- Lapse-rate helpers
-- =========================================================================

-- | Convert a normalised lapse rate to °C\/km.
--
-- The internal lapse rate is \"normalised temp drop per normalised
-- elevation above sea level\".  Converting to °C\/km requires the
-- temperature scale and the real-world elevation half-range (sea level
-- to peak).
--
-- @
-- lapse_{°C\/km} = lapse_{norm} × tempScale / (elevRange / 2000)
-- @
lapseRateCPerKm :: UnitScales -> Float -> Float
lapseRateCPerKm s normLapse =
  let halfRangeKm = usElevRange s / 2000.0
  in  normLapse * usTempScale s / halfRangeKm
{-# INLINE lapseRateCPerKm #-}

-- | Convert °C\/km lapse rate to normalised.
--
-- Inverse of 'lapseRateCPerKm'.
lapseRateFromCPerKm :: UnitScales -> Float -> Float
lapseRateFromCPerKm s cPerKm =
  let halfRangeKm = usElevRange s / 2000.0
  in  cPerKm * halfRangeKm / usTempScale s
{-# INLINE lapseRateFromCPerKm #-}

-- =========================================================================
-- RealTemperatureConfig
-- =========================================================================

-- | Temperature configuration expressed in real-world units.
--
-- Temperature fields are in °C, the lapse rate is in °C\/km, and
-- dimensionless fields (exponents, fractions) are passed through
-- unchanged.
data RealTemperatureConfig = RealTemperatureConfig
  { rtcEquatorTemp       :: !Float
    -- ^ Equatorial base temperature (°C).  Default: @24.6@.
  , rtcPoleTemp          :: !Float
    -- ^ Polar base temperature (°C).  Default: @−30.0@.
  , rtcLapseRate         :: !Float
    -- ^ Environmental lapse rate (°C\/km).  Default: @7.58@.
  , rtcLatitudeExponent  :: !Float
    -- ^ @cos(lat)@ exponent (dimensionless).  Default: @1.0@.
  , rtcPlateHeightCooling :: !Float
    -- ^ Continental-elevation cooling coefficient (dimensionless).
    -- Default: @0.05@.
  , rtcNoiseScale        :: !Float
    -- ^ Temperature noise amplitude (dimensionless fraction of full
    -- range).  Default: @0.1@.
  , rtcOceanModeration   :: !Float
    -- ^ Ocean moderation strength (dimensionless 0–1).  Default: @0.3@.
  , rtcOceanModerateTemp :: !Float
    -- ^ Ocean moderation offset (°C).  Default: @0.0@.
  , rtcAlbedoSensitivity :: !Float
    -- ^ Albedo sensitivity (dimensionless).  Default: @0.20@.
  , rtcAlbedoReference   :: !Float
    -- ^ Reference albedo (dimensionless).  Default: @0.30@.
  , rtcOceanEquatorSST   :: !Float
    -- ^ Equatorial ocean SST (°C).  Default: @28.1@.
  , rtcOceanPoleSST      :: !Float
    -- ^ Polar ocean SST (°C).  Default: @0.1@.
  , rtcOceanLatExponent  :: !Float
    -- ^ Ocean latitude exponent (dimensionless).  Default: @2.0@.
  } deriving stock (Show, Eq, Generic)

-- | Default real-world temperature config matching
-- 'defaultTemperatureConfig' under 'defaultUnitScales'.
defaultRealTemperatureConfig :: RealTemperatureConfig
defaultRealTemperatureConfig =
  temperatureConfigToReal defaultUnitScales defaultTemperatureConfig

-- | Convert real-world temperature config to normalised.
realToTemperatureConfig :: UnitScales -> RealTemperatureConfig -> TemperatureConfig
realToTemperatureConfig s r = TemperatureConfig
  { tmpEquatorTemp       = cToNorm s (rtcEquatorTemp r)
  , tmpPoleTemp          = cToNorm s (rtcPoleTemp r)
  , tmpLapseRate         = lapseRateFromCPerKm s (rtcLapseRate r)
  , tmpLatitudeExponent  = rtcLatitudeExponent r
  , tmpPlateHeightCooling = rtcPlateHeightCooling r
  , tmpNoiseScale        = rtcNoiseScale r
  , tmpOceanModeration   = rtcOceanModeration r
  , tmpOceanModerateTemp = cToNorm s (rtcOceanModerateTemp r)
  , tmpAlbedoSensitivity = rtcAlbedoSensitivity r
  , tmpAlbedoReference   = rtcAlbedoReference r
  , tmpOceanEquatorSST   = cToNorm s (rtcOceanEquatorSST r)
  , tmpOceanPoleSST      = cToNorm s (rtcOceanPoleSST r)
  , tmpOceanLatExponent  = rtcOceanLatExponent r
  }

-- | Convert normalised temperature config to real-world.
temperatureConfigToReal :: UnitScales -> TemperatureConfig -> RealTemperatureConfig
temperatureConfigToReal s t = RealTemperatureConfig
  { rtcEquatorTemp       = normToC s (tmpEquatorTemp t)
  , rtcPoleTemp          = normToC s (tmpPoleTemp t)
  , rtcLapseRate         = lapseRateCPerKm s (tmpLapseRate t)
  , rtcLatitudeExponent  = tmpLatitudeExponent t
  , rtcPlateHeightCooling = tmpPlateHeightCooling t
  , rtcNoiseScale        = tmpNoiseScale t
  , rtcOceanModeration   = tmpOceanModeration t
  , rtcOceanModerateTemp = normToC s (tmpOceanModerateTemp t)
  , rtcAlbedoSensitivity = tmpAlbedoSensitivity t
  , rtcAlbedoReference   = tmpAlbedoReference t
  , rtcOceanEquatorSST   = normToC s (tmpOceanEquatorSST t)
  , rtcOceanPoleSST      = normToC s (tmpOceanPoleSST t)
  , rtcOceanLatExponent  = tmpOceanLatExponent t
  }

-- =========================================================================
-- RealGlacierConfig
-- =========================================================================

-- | Glacier config with temperature fields in °C.
--
-- Non-temperature fields (iteration counts, rates, scales) are
-- dimensionless and passed through unchanged.
data RealGlacierConfig = RealGlacierConfig
  { rgcSnowTemp              :: !Float
    -- ^ Snow accumulation temperature (°C).  Default: @−12.5@.
  , rgcSnowRange             :: !Float
    -- ^ Snow accumulation fadeout range (°C span).  Default: @24.5@.
  , rgcMeltTemp              :: !Float
    -- ^ Melt onset temperature (°C).  Default: @−2.0@.
  , rgcMeltRate              :: !Float
    -- ^ Melt rate per °C above threshold (dimensionless).  Default: @0.2@.
  , rgcAccumScale            :: !Float
    -- ^ Precipitation→snow scale (dimensionless).  Default: @1.0@.
  , rgcFlowIterations        :: !Int
    -- ^ Ice diffusion iterations.  Default: @3@.
  , rgcFlowRate              :: !Float
    -- ^ Ice diffusion rate (dimensionless).  Default: @0.2@.
  , rgcHardnessErosionWeight :: !Float
    -- ^ Hardness vs erosion scaling (dimensionless 0–1).  Default: @0.6@.
  , rgcErosionScale          :: !Float
    -- ^ Glacial erosion scale (dimensionless).  Default: @0.25@.
  , rgcDepositScale          :: !Float
    -- ^ Glacial deposition scale (dimensionless).  Default: @0.2@.
  , rgcDepositMaxSlope       :: !Float
    -- ^ Slope threshold for deposition (dimensionless).  Default: @0.1@.
  , rgcCarveScale            :: !Float
    -- ^ Elevation carving scale (dimensionless).  Default: @0.01@.
  , rgcDepositRaiseScale     :: !Float
    -- ^ Elevation raising scale (dimensionless).  Default: @0.01@.
  } deriving stock (Show, Eq, Generic)

-- | Default real-world glacier config matching 'defaultGlacierConfig'.
defaultRealGlacierConfig :: RealGlacierConfig
defaultRealGlacierConfig =
  glacierConfigToReal defaultUnitScales defaultGlacierConfig

-- | Convert real-world glacier config to normalised.
realToGlacierConfig :: UnitScales -> RealGlacierConfig -> GlacierConfig
realToGlacierConfig s r = GlacierConfig
  { gcSnowTemp              = cToNorm s (rgcSnowTemp r)
  , gcSnowRange             = rgcSnowRange r / usTempScale s
  , gcMeltTemp              = cToNorm s (rgcMeltTemp r)
  , gcMeltRate              = rgcMeltRate r
  , gcAccumScale            = rgcAccumScale r
  , gcFlowIterations        = rgcFlowIterations r
  , gcFlowRate              = rgcFlowRate r
  , gcHardnessErosionWeight = rgcHardnessErosionWeight r
  , gcErosionScale          = rgcErosionScale r
  , gcDepositScale          = rgcDepositScale r
  , gcDepositMaxSlope       = rgcDepositMaxSlope r
  , gcCarveScale            = rgcCarveScale r
  , gcDepositRaiseScale     = rgcDepositRaiseScale r
  }

-- | Convert normalised glacier config to real-world.
glacierConfigToReal :: UnitScales -> GlacierConfig -> RealGlacierConfig
glacierConfigToReal s g = RealGlacierConfig
  { rgcSnowTemp              = normToC s (gcSnowTemp g)
  , rgcSnowRange             = gcSnowRange g * usTempScale s
  , rgcMeltTemp              = normToC s (gcMeltTemp g)
  , rgcMeltRate              = gcMeltRate g
  , rgcAccumScale            = gcAccumScale g
  , rgcFlowIterations        = gcFlowIterations g
  , rgcFlowRate              = gcFlowRate g
  , rgcHardnessErosionWeight = gcHardnessErosionWeight g
  , rgcErosionScale          = gcErosionScale g
  , rgcDepositScale          = gcDepositScale g
  , rgcDepositMaxSlope       = gcDepositMaxSlope g
  , rgcCarveScale            = gcCarveScale g
  , rgcDepositRaiseScale     = gcDepositRaiseScale g
  }

-- =========================================================================
-- RealBiomeThresholds
-- =========================================================================

-- | Biome thresholds with temperature in °C and elevation in metres.
--
-- Moisture, precipitation, and slope fields are dimensionless and
-- passed through unchanged.
data RealBiomeThresholds = RealBiomeThresholds
  { rbtCoastalBand       :: !Float
    -- ^ Coastal band width (metres above sea level).  Default: @360@.
  , rbtFallbackBiome     :: !BiomeId
    -- ^ Fallback biome (unchanged).
  , rbtIceCapTemp        :: !Float
    -- ^ Ice-cap max temperature (°C).  Default: @−26.5@.
  , rbtMontanePrecip     :: !Float
    -- ^ Montane forest min precipitation (dimensionless).  Default: @0.30@.
  , rbtCliffSlope        :: !Float
    -- ^ Cliff slope threshold (dimensionless).  Default: @0.40@.
  , rbtValleyMoisture    :: !Float
    -- ^ Valley wetland moisture (dimensionless).  Default: @0.70@.
  , rbtDepressionMoisture :: !Float
    -- ^ Depression wetland moisture (dimensionless).  Default: @0.60@.
  , rbtLakeshoreMoisture :: !Float
    -- ^ Lakeshore wetland moisture (dimensionless).  Default: @0.40@.
  , rbtLakeshoreMinTemp  :: !Float
    -- ^ Lakeshore wetland min temperature (°C).  Default: @−9.0@.
  , rbtPrecipWeight      :: !Float
    -- ^ Precipitation weight (dimensionless).  Default: @2.0@.
  , rbtSeasonalitySavannaThreshold :: !Float
    -- ^ Savanna seasonality threshold (dimensionless).  Default: @0.40@.
  , rbtTempRangeGrasslandThreshold :: !Float
    -- ^ Grassland continentality threshold (dimensionless).  Default: @0.15@.
  , rbtSnowPolarDesertMaxPrecip :: !Float
    -- ^ Polar desert max precipitation (dimensionless).  Default: @0.08@.
  , rbtReliefSavannaThreshold :: !Float
    -- ^ Savanna relief threshold (dimensionless).  Default: @0.25@.
  , rbtSnowMaxTemp       :: !Float
    -- ^ Snow guard max temperature (°C).  Default: @−16.0@.
  , rbtSnowMinASL        :: !Float
    -- ^ Snow guard min elevation (metres above sea level).  Default: @240@.
  , rbtAlpineMaxTemp     :: !Float
    -- ^ Alpine guard max temperature (°C).  Default: @−5.5@.
  , rbtAlpineMinASL      :: !Float
    -- ^ Alpine guard min elevation (metres a.s.l.).  Default: @480@.
  , rbtMontaneMaxTemp    :: !Float
    -- ^ Montane guard max temperature (°C).  Default: @8.5@.
  , rbtMontaneMinASL     :: !Float
    -- ^ Montane guard min elevation (metres a.s.l.).  Default: @360@.
  , rbtMontaneMinSlope   :: !Float
    -- ^ Montane min slope (dimensionless).  Default: @0.06@.
  , rbtMontaneMinHumidity :: !Float
    -- ^ Montane min humidity (dimensionless).  Default: @0.40@.
  } deriving stock (Show, Eq, Generic)

-- | Default real-world biome thresholds matching 'defaultBiomeThresholds'.
defaultRealBiomeThresholds :: RealBiomeThresholds
defaultRealBiomeThresholds =
  biomeThresholdsToReal defaultUnitScales defaultBiomeThresholds

-- | Convert \"above sea level\" normalised offset to metres.
-- ASL offset is relative to 'usWaterLevel', so
-- @aslMetres = offset × elevRange@.
normASLToMetres :: UnitScales -> Float -> Float
normASLToMetres s n = n * usElevRange s

-- | Convert metres above sea level to normalised offset.
metresToNormASL :: UnitScales -> Float -> Float
metresToNormASL s m = m / usElevRange s

-- | Convert real-world biome thresholds to normalised.
realToBiomeThresholds :: UnitScales -> RealBiomeThresholds -> BiomeThresholds
realToBiomeThresholds s r = BiomeThresholds
  { btCoastalBand       = metresToNormASL s (rbtCoastalBand r)
  , btFallbackBiome     = rbtFallbackBiome r
  , btIceCapTemp        = cToNorm s (rbtIceCapTemp r)
  , btMontanePrecip     = rbtMontanePrecip r
  , btCliffSlope        = rbtCliffSlope r
  , btValleyMoisture    = rbtValleyMoisture r
  , btDepressionMoisture = rbtDepressionMoisture r
  , btLakeshoreMoisture = rbtLakeshoreMoisture r
  , btLakeshoreMinTemp  = cToNorm s (rbtLakeshoreMinTemp r)
  , btPrecipWeight      = rbtPrecipWeight r
  , btSeasonalitySavannaThreshold = rbtSeasonalitySavannaThreshold r
  , btTempRangeGrasslandThreshold = rbtTempRangeGrasslandThreshold r
  , btSnowPolarDesertMaxPrecip = rbtSnowPolarDesertMaxPrecip r
  , btReliefSavannaThreshold = rbtReliefSavannaThreshold r
  , btSnowMaxTemp       = cToNorm s (rbtSnowMaxTemp r)
  , btSnowMinASL        = metresToNormASL s (rbtSnowMinASL r)
  , btAlpineMaxTemp     = cToNorm s (rbtAlpineMaxTemp r)
  , btAlpineMinASL      = metresToNormASL s (rbtAlpineMinASL r)
  , btMontaneMaxTemp    = cToNorm s (rbtMontaneMaxTemp r)
  , btMontaneMinASL     = metresToNormASL s (rbtMontaneMinASL r)
  , btMontaneMinSlope   = rbtMontaneMinSlope r
  , btMontaneMinHumidity = rbtMontaneMinHumidity r
  }

-- | Convert normalised biome thresholds to real-world.
biomeThresholdsToReal :: UnitScales -> BiomeThresholds -> RealBiomeThresholds
biomeThresholdsToReal s bt = RealBiomeThresholds
  { rbtCoastalBand       = normASLToMetres s (btCoastalBand bt)
  , rbtFallbackBiome     = btFallbackBiome bt
  , rbtIceCapTemp        = normToC s (btIceCapTemp bt)
  , rbtMontanePrecip     = btMontanePrecip bt
  , rbtCliffSlope        = btCliffSlope bt
  , rbtValleyMoisture    = btValleyMoisture bt
  , rbtDepressionMoisture = btDepressionMoisture bt
  , rbtLakeshoreMoisture = btLakeshoreMoisture bt
  , rbtLakeshoreMinTemp  = normToC s (btLakeshoreMinTemp bt)
  , rbtPrecipWeight      = btPrecipWeight bt
  , rbtSeasonalitySavannaThreshold = btSeasonalitySavannaThreshold bt
  , rbtTempRangeGrasslandThreshold = btTempRangeGrasslandThreshold bt
  , rbtSnowPolarDesertMaxPrecip = btSnowPolarDesertMaxPrecip bt
  , rbtReliefSavannaThreshold = btReliefSavannaThreshold bt
  , rbtSnowMaxTemp       = normToC s (btSnowMaxTemp bt)
  , rbtSnowMinASL        = normASLToMetres s (btSnowMinASL bt)
  , rbtAlpineMaxTemp     = normToC s (btAlpineMaxTemp bt)
  , rbtAlpineMinASL      = normASLToMetres s (btAlpineMinASL bt)
  , rbtMontaneMaxTemp    = normToC s (btMontaneMaxTemp bt)
  , rbtMontaneMinASL     = normASLToMetres s (btMontaneMinASL bt)
  , rbtMontaneMinSlope   = btMontaneMinSlope bt
  , rbtMontaneMinHumidity = btMontaneMinHumidity bt
  }

-- =========================================================================
-- RealVegetationBootstrapConfig
-- =========================================================================

-- | Vegetation bootstrap config with temperature fields in °C.
data RealVegetationBootstrapConfig = RealVegetationBootstrapConfig
  { rvbcTempMin          :: !Float
    -- ^ Minimum temperature for vegetation (°C).  Default: @−24.4@.
  , rvbcTempRange        :: !Float
    -- ^ Temperature range for vegetation 0→1 (°C span).  Default: @35.0@.
  , rvbcFertilityBoost   :: !Float
    -- ^ Fertility boost (dimensionless).  Default: @0.50@.
  , rvbcTempWeight       :: !Float
    -- ^ Weight of temperature factor in bootstrap blend (dimensionless).
    -- Default: @0.40@.
  , rvbcMoistWeight      :: !Float
    -- ^ Weight of moisture factor in bootstrap blend (dimensionless).
    -- Default: @0.35@.
  , rvbcSoilWeight       :: !Float
    -- ^ Weight of soil factor in bootstrap blend (dimensionless).
    -- Default: @0.25@.
  , rvbcAlbedoBase       :: !Float
    -- ^ Base albedo (dimensionless).  Default: @0.15@.
  , rvbcAlbedoBare       :: !Float
    -- ^ Bare ground albedo (dimensionless).  Default: @0.25@.
  , rvbcAlbedoVeg        :: !Float
    -- ^ Vegetated albedo (dimensionless).  Default: @0.10@.
  , rvbcOceanAlbedo      :: !Float
    -- ^ Ocean albedo (dimensionless).  Default: @0.06@.
  , rvbcIceAlbedo        :: !Float
    -- ^ Ice albedo (dimensionless).  Default: @0.80@.
  , rvbcMinMoisture      :: !Float
    -- ^ Minimum moisture factor (dimensionless).  Default: @0.25@.
  , rvbcCoastalIterations :: !Int
    -- ^ Coastal diffusion iterations.  Default: @30@.
  , rvbcCoastalDiffuse   :: !Float
    -- ^ Coastal diffusion rate (dimensionless).  Default: @0.6@.
  , rvbcCoastalBoost     :: !Float
    -- ^ Coastal vegetation boost (dimensionless).  Default: @0.30@.
  } deriving stock (Show, Eq, Generic)

-- | Default real-world vegetation bootstrap config.
defaultRealVegetationBootstrapConfig :: RealVegetationBootstrapConfig
defaultRealVegetationBootstrapConfig =
  vegetationBootstrapConfigToReal defaultUnitScales defaultVegetationBootstrapConfig

-- | Convert real-world vegetation config to normalised.
realToVegetationBootstrapConfig
  :: UnitScales -> RealVegetationBootstrapConfig -> VegetationBootstrapConfig
realToVegetationBootstrapConfig s r = VegetationBootstrapConfig
  { vbcTempMin          = cToNorm s (rvbcTempMin r)
  , vbcTempRange        = rvbcTempRange r / usTempScale s
  , vbcFertilityBoost   = rvbcFertilityBoost r
  , vbcTempWeight       = rvbcTempWeight r
  , vbcMoistWeight      = rvbcMoistWeight r
  , vbcSoilWeight       = rvbcSoilWeight r
  , vbcAlbedoBase       = rvbcAlbedoBase r
  , vbcAlbedoBare       = rvbcAlbedoBare r
  , vbcAlbedoVeg        = rvbcAlbedoVeg r
  , vbcOceanAlbedo      = rvbcOceanAlbedo r
  , vbcIceAlbedo        = rvbcIceAlbedo r
  , vbcMinMoisture      = rvbcMinMoisture r
  , vbcCoastalIterations = rvbcCoastalIterations r
  , vbcCoastalDiffuse   = rvbcCoastalDiffuse r
  , vbcCoastalBoost     = rvbcCoastalBoost r
  }

-- | Convert normalised vegetation config to real-world.
vegetationBootstrapConfigToReal
  :: UnitScales -> VegetationBootstrapConfig -> RealVegetationBootstrapConfig
vegetationBootstrapConfigToReal s v = RealVegetationBootstrapConfig
  { rvbcTempMin          = normToC s (vbcTempMin v)
  , rvbcTempRange        = vbcTempRange v * usTempScale s
  , rvbcFertilityBoost   = vbcFertilityBoost v
  , rvbcTempWeight       = vbcTempWeight v
  , rvbcMoistWeight      = vbcMoistWeight v
  , rvbcSoilWeight       = vbcSoilWeight v
  , rvbcAlbedoBase       = vbcAlbedoBase v
  , rvbcAlbedoBare       = vbcAlbedoBare v
  , rvbcAlbedoVeg        = vbcAlbedoVeg v
  , rvbcOceanAlbedo      = vbcOceanAlbedo v
  , rvbcIceAlbedo        = vbcIceAlbedo v
  , rvbcMinMoisture      = vbcMinMoisture v
  , rvbcCoastalIterations = vbcCoastalIterations v
  , rvbcCoastalDiffuse   = vbcCoastalDiffuse v
  , rvbcCoastalBoost     = vbcCoastalBoost v
  }

-- =========================================================================
-- RealForestConfig
-- =========================================================================

-- | Forest refinement config with temperature in °C and elevation in m.
data RealForestConfig = RealForestConfig
  { rfcTropicalDryMinTemp        :: !Float   -- ^ °C.  Default: @21.8@.
  , rfcTropicalDryMaxPrecip      :: !Float   -- ^ Dimensionless.  Default: @0.60@.
  , rfcDeciduousMinTemp          :: !Float   -- ^ °C.  Default: @−5.5@.
  , rfcDeciduousMinPrecip        :: !Float   -- ^ Dimensionless.  Default: @0.40@.
  , rfcConiferousMaxTemp         :: !Float   -- ^ °C.  Default: @10.6@.
  , rfcConiferousMinHardness     :: !Float   -- ^ Dimensionless.  Default: @0.40@.
  , rfcMontaneMinElev            :: !Float   -- ^ Metres a.s.l.  Default: @720@.
  , rfcMontaneMinSlope           :: !Float   -- ^ Dimensionless.  Default: @0.06@.
  , rfcCloudForestMinElev        :: !Float   -- ^ Metres a.s.l.  Default: @1200@.
  , rfcCloudForestMinPrecip      :: !Float   -- ^ Dimensionless.  Default: @0.70@.
  , rfcCloudForestMinTemp        :: !Float   -- ^ °C.  Default: @8.5@.
  , rfcCloudForestMinHumidity    :: !Float   -- ^ Dimensionless.  Default: @0.65@.
  , rfcTempRainforestMinPrecip   :: !Float   -- ^ Dimensionless.  Default: @0.80@.
  , rfcTempRainforestMaxTemp     :: !Float   -- ^ °C.  Default: @8.5@.
  , rfcTempRainforestMinHumidity :: !Float   -- ^ Dimensionless.  Default: @0.70@.
  , rfcSeasonalForestMinSeason   :: !Float   -- ^ Dimensionless.  Default: @0.45@.
  , rfcSeasonalForestMinTemp     :: !Float   -- ^ °C.  Default: @15.5@.
  } deriving stock (Show, Eq, Generic)

-- | Default real-world forest config.
defaultRealForestConfig :: RealForestConfig
defaultRealForestConfig =
  forestConfigToReal defaultUnitScales defaultForestConfig

-- | Convert real-world forest config to normalised.
realToForestConfig :: UnitScales -> RealForestConfig -> ForestConfig
realToForestConfig s r = ForestConfig
  { fcTropicalDryMinTemp        = cToNorm s (rfcTropicalDryMinTemp r)
  , fcTropicalDryMaxPrecip      = rfcTropicalDryMaxPrecip r
  , fcDeciduousMinTemp          = cToNorm s (rfcDeciduousMinTemp r)
  , fcDeciduousMinPrecip        = rfcDeciduousMinPrecip r
  , fcConiferousMaxTemp         = cToNorm s (rfcConiferousMaxTemp r)
  , fcConiferousMinHardness     = rfcConiferousMinHardness r
  , fcMontaneMinElev            = metresToNorm s (rfcMontaneMinElev r)
  , fcMontaneMinSlope           = rfcMontaneMinSlope r
  , fcCloudForestMinElev        = metresToNorm s (rfcCloudForestMinElev r)
  , fcCloudForestMinPrecip      = rfcCloudForestMinPrecip r
  , fcCloudForestMinTemp        = cToNorm s (rfcCloudForestMinTemp r)
  , fcCloudForestMinHumidity    = rfcCloudForestMinHumidity r
  , fcTempRainforestMinPrecip   = rfcTempRainforestMinPrecip r
  , fcTempRainforestMaxTemp     = cToNorm s (rfcTempRainforestMaxTemp r)
  , fcTempRainforestMinHumidity = rfcTempRainforestMinHumidity r
  , fcSeasonalForestMinSeason   = rfcSeasonalForestMinSeason r
  , fcSeasonalForestMinTemp     = cToNorm s (rfcSeasonalForestMinTemp r)
  }

-- | Convert normalised forest config to real-world.
forestConfigToReal :: UnitScales -> ForestConfig -> RealForestConfig
forestConfigToReal s f = RealForestConfig
  { rfcTropicalDryMinTemp        = normToC s (fcTropicalDryMinTemp f)
  , rfcTropicalDryMaxPrecip      = fcTropicalDryMaxPrecip f
  , rfcDeciduousMinTemp          = normToC s (fcDeciduousMinTemp f)
  , rfcDeciduousMinPrecip        = fcDeciduousMinPrecip f
  , rfcConiferousMaxTemp         = normToC s (fcConiferousMaxTemp f)
  , rfcConiferousMinHardness     = fcConiferousMinHardness f
  , rfcMontaneMinElev            = normToMetres s (fcMontaneMinElev f)
  , rfcMontaneMinSlope           = fcMontaneMinSlope f
  , rfcCloudForestMinElev        = normToMetres s (fcCloudForestMinElev f)
  , rfcCloudForestMinPrecip      = fcCloudForestMinPrecip f
  , rfcCloudForestMinTemp        = normToC s (fcCloudForestMinTemp f)
  , rfcCloudForestMinHumidity    = fcCloudForestMinHumidity f
  , rfcTempRainforestMinPrecip   = fcTempRainforestMinPrecip f
  , rfcTempRainforestMaxTemp     = normToC s (fcTempRainforestMaxTemp f)
  , rfcTempRainforestMinHumidity = fcTempRainforestMinHumidity f
  , rfcSeasonalForestMinSeason   = fcSeasonalForestMinSeason f
  , rfcSeasonalForestMinTemp     = normToC s (fcSeasonalForestMinTemp f)
  }

-- =========================================================================
-- RealDesertConfig
-- =========================================================================

-- | Desert refinement config with temperature in °C.
data RealDesertConfig = RealDesertConfig
  { rdcHotMinTemp           :: !Float   -- ^ °C.  Default: @19.0@.
  , rdcColdMaxTemp          :: !Float   -- ^ °C.  Default: @−2.0@.
  , rdcRockyMinHardness     :: !Float   -- ^ Dimensionless.  Default: @0.55@.
  , rdcRockyMaxSoilDepth    :: !Float   -- ^ Dimensionless.  Default: @0.20@.
  , rdcSandMaxHardness      :: !Float   -- ^ Dimensionless.  Default: @0.30@.
  , rdcSaltFlatMaxMoist     :: !Float   -- ^ Dimensionless.  Default: @0.02@.
  , rdcFogDesertMinHumidity :: !Float   -- ^ Dimensionless.  Default: @0.30@.
  , rdcFogDesertMaxPrecip   :: !Float   -- ^ Dimensionless.  Default: @0.08@.
  , rdcFogDesertMaxSeason   :: !Float   -- ^ Dimensionless.  Default: @0.25@.
  } deriving stock (Show, Eq, Generic)

defaultRealDesertConfig :: RealDesertConfig
defaultRealDesertConfig =
  desertConfigToReal defaultUnitScales defaultDesertConfig

realToDesertConfig :: UnitScales -> RealDesertConfig -> DesertConfig
realToDesertConfig s r = DesertConfig
  { dcHotMinTemp           = cToNorm s (rdcHotMinTemp r)
  , dcColdMaxTemp          = cToNorm s (rdcColdMaxTemp r)
  , dcRockyMinHardness     = rdcRockyMinHardness r
  , dcRockyMaxSoilDepth    = rdcRockyMaxSoilDepth r
  , dcSandMaxHardness      = rdcSandMaxHardness r
  , dcSaltFlatMaxMoist     = rdcSaltFlatMaxMoist r
  , dcFogDesertMinHumidity = rdcFogDesertMinHumidity r
  , dcFogDesertMaxPrecip   = rdcFogDesertMaxPrecip r
  , dcFogDesertMaxSeason   = rdcFogDesertMaxSeason r
  }

desertConfigToReal :: UnitScales -> DesertConfig -> RealDesertConfig
desertConfigToReal s d = RealDesertConfig
  { rdcHotMinTemp           = normToC s (dcHotMinTemp d)
  , rdcColdMaxTemp          = normToC s (dcColdMaxTemp d)
  , rdcRockyMinHardness     = dcRockyMinHardness d
  , rdcRockyMaxSoilDepth    = dcRockyMaxSoilDepth d
  , rdcSandMaxHardness      = dcSandMaxHardness d
  , rdcSaltFlatMaxMoist     = dcSaltFlatMaxMoist d
  , rdcFogDesertMinHumidity = dcFogDesertMinHumidity d
  , rdcFogDesertMaxPrecip   = dcFogDesertMaxPrecip d
  , rdcFogDesertMaxSeason   = dcFogDesertMaxSeason d
  }

-- =========================================================================
-- RealOceanConfig
-- =========================================================================

-- | Ocean refinement config with temperature in °C and depth in metres.
data RealOceanConfig = RealOceanConfig
  { rocDeepThreshold    :: !Float  -- ^ Deep ocean depth threshold (metres below s.l.). Default: @−3000@.
  , rocCoralMinTemp     :: !Float  -- ^ °C.  Default: @20.4@.
  , rocCoralMaxDepth    :: !Float  -- ^ Coral max depth (metres below s.l.). Default: @−600@.
  , rocCoralMaxSlope    :: !Float  -- ^ Dimensionless.  Default: @0.04@.
  , rocCoralMinHardness :: !Float  -- ^ Dimensionless.  Default: @0.35@.
  } deriving stock (Show, Eq, Generic)

defaultRealOceanConfig :: RealOceanConfig
defaultRealOceanConfig =
  oceanConfigToReal defaultUnitScales defaultOceanConfig

realToOceanConfig :: UnitScales -> RealOceanConfig -> OceanConfig
realToOceanConfig s r = OceanConfig
  { ocDeepThreshold    = metresToNorm s (rocDeepThreshold r)
  , ocCoralMinTemp     = cToNorm s (rocCoralMinTemp r)
  , ocCoralMaxDepth    = metresToNorm s (rocCoralMaxDepth r)
  , ocCoralMaxSlope    = rocCoralMaxSlope r
  , ocCoralMinHardness = rocCoralMinHardness r
  }

oceanConfigToReal :: UnitScales -> OceanConfig -> RealOceanConfig
oceanConfigToReal s o = RealOceanConfig
  { rocDeepThreshold    = normToMetres s (ocDeepThreshold o)
  , rocCoralMinTemp     = normToC s (ocCoralMinTemp o)
  , rocCoralMaxDepth    = normToMetres s (ocCoralMaxDepth o)
  , rocCoralMaxSlope    = ocCoralMaxSlope o
  , rocCoralMinHardness = ocCoralMinHardness o
  }

-- =========================================================================
-- RealTundraConfig
-- =========================================================================

-- | Tundra refinement config with temperature in °C and elevation in m.
data RealTundraConfig = RealTundraConfig
  { rtcArcticMaxTemp        :: !Float  -- ^ °C.  Default: @−23.0@.
  , rtcAlpineTundraMinElev  :: !Float  -- ^ Metres a.s.l.  Default: @1200@.
  , rtcPolarDesertMaxPrecip :: !Float  -- ^ Dimensionless.  Default: @0.10@.
  } deriving stock (Show, Eq, Generic)

defaultRealTundraConfig :: RealTundraConfig
defaultRealTundraConfig =
  tundraConfigToReal defaultUnitScales defaultTundraConfig

realToTundraConfig :: UnitScales -> RealTundraConfig -> TundraConfig
realToTundraConfig s r = TundraConfig
  { tcArcticMaxTemp        = cToNorm s (rtcArcticMaxTemp r)
  , tcAlpineTundraMinElev  = metresToNorm s (rtcAlpineTundraMinElev r)
  , tcPolarDesertMaxPrecip = rtcPolarDesertMaxPrecip r
  }

tundraConfigToReal :: UnitScales -> TundraConfig -> RealTundraConfig
tundraConfigToReal s t = RealTundraConfig
  { rtcArcticMaxTemp        = normToC s (tcArcticMaxTemp t)
  , rtcAlpineTundraMinElev  = normToMetres s (tcAlpineTundraMinElev t)
  , rtcPolarDesertMaxPrecip = tcPolarDesertMaxPrecip t
  }

-- =========================================================================
-- RealSnowConfig
-- =========================================================================

-- | Snow refinement config with temperature in °C.
data RealSnowConfig = RealSnowConfig
  { rsnIceCapMaxTemp        :: !Float  -- ^ °C.  Default: @−26.5@.
  , rsnGlacierMinIceThick   :: !Float  -- ^ Dimensionless.  Default: @0.10@.
  , rsnGlacierMaxTemp       :: !Float  -- ^ °C.  Default: @−19.5@.
  , rsnSnowfieldMinSnowpack :: !Float  -- ^ Dimensionless.  Default: @0.50@.
  , rsnMarginalMinTemp      :: !Float  -- ^ °C.  Default: @−19.5@.
  , rsnSteepSlopeThreshold  :: !Float  -- ^ Dimensionless.  Default: @0.30@.
  , rsnWarmEscapeTemp       :: !Float  -- ^ °C.  Default: @−12.5@.
  } deriving stock (Show, Eq, Generic)

defaultRealSnowConfig :: RealSnowConfig
defaultRealSnowConfig =
  snowConfigToReal defaultUnitScales defaultSnowConfig

realToSnowConfig :: UnitScales -> RealSnowConfig -> SnowConfig
realToSnowConfig s r = SnowConfig
  { snIceCapMaxTemp        = cToNorm s (rsnIceCapMaxTemp r)
  , snGlacierMinIceThick   = rsnGlacierMinIceThick r
  , snGlacierMaxTemp       = cToNorm s (rsnGlacierMaxTemp r)
  , snSnowfieldMinSnowpack = rsnSnowfieldMinSnowpack r
  , snMarginalMinTemp      = cToNorm s (rsnMarginalMinTemp r)
  , snSteepSlopeThreshold  = rsnSteepSlopeThreshold r
  , snWarmEscapeTemp       = cToNorm s (rsnWarmEscapeTemp r)
  }

snowConfigToReal :: UnitScales -> SnowConfig -> RealSnowConfig
snowConfigToReal s sn = RealSnowConfig
  { rsnIceCapMaxTemp        = normToC s (snIceCapMaxTemp sn)
  , rsnGlacierMinIceThick   = snGlacierMinIceThick sn
  , rsnGlacierMaxTemp       = normToC s (snGlacierMaxTemp sn)
  , rsnSnowfieldMinSnowpack = snSnowfieldMinSnowpack sn
  , rsnMarginalMinTemp      = normToC s (snMarginalMinTemp sn)
  , rsnSteepSlopeThreshold  = snSteepSlopeThreshold sn
  , rsnWarmEscapeTemp       = normToC s (snWarmEscapeTemp sn)
  }
