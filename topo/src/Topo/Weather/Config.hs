{-# LANGUAGE DeriveGeneric #-}

-- | Weather simulation configuration types and defaults.
module Topo.Weather.Config
  ( WeatherConfig(..)
  , defaultWeatherConfig
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Topo.Config.JSON
  (configOptions, genericParseJSON, genericToJSON, mergeDefaults)

-- | Weather update configuration.
--
-- This record controls seasonal modulation, stochastic perturbations,
-- pressure/wind coupling, and stateful operator strengths used by
-- weather generation and simulation.
data WeatherConfig = WeatherConfig
  { -- | Simulation seconds per weather tick. Also used as the
    -- 'wtTickRate' when initialising 'WorldTime'.
    wcTickSeconds :: !Float
    -- | Initial seasonal phase offset (radians), combined with dynamic
    -- year-fraction phase.
  , wcSeasonPhase :: !Float
    -- | Base seasonal temperature amplitude before latitude/tilt scaling.
  , wcSeasonAmplitude :: !Float
    -- | Number of weather ticks per full seasonal cycle.
  , wcSeasonCycleLength :: !Float
    -- | Temperature jitter amplitude from time-varying noise.
  , wcJitterAmplitude :: !Float
    -- | Baseline pressure level.
  , wcPressureBase :: !Float
    -- | Temperature to pressure coupling strength.
  , wcPressureTempScale :: !Float
    -- | Latitude-band (Coriolis-like) pressure modulation strength.
  , wcPressureCoriolisScale :: !Float
    -- | Dry-season minimum precipitation seasonal multiplier.
  , wcSeasonalBase :: !Float
    -- | Added range above 'wcSeasonalBase' for wet season.
  , wcSeasonalRange :: !Float
    -- | Humidity noise amplitude.
  , wcHumidityNoiseScale :: !Float
    -- | Precipitation noise amplitude.
  , wcPrecipNoiseScale :: !Float
    -- | ITCZ centre latitude (degrees).
  , wcITCZLatitude :: !Float
    -- | ITCZ half-width (degrees).
  , wcITCZWidth :: !Float
    -- | ITCZ precipitation boost at centreline.
  , wcITCZPrecipBoost :: !Float
    -- | Humidity to pressure coupling strength.
  , wcPressureHumidityScale :: !Float
    -- | Pressure-gradient contribution to wind speed.
  , wcPressureGradientWindScale :: !Float
    -- | Wind-speed noise amplitude.
  , wcWindNoiseScale :: !Float
    -- | Seasonal ITCZ migration scale.
  , wcITCZMigrationScale :: !Float
    -- | Relative-humidity exponent for cloud fraction derivation.
  , wcCloudRHExponent :: !Float
    -- | Cloud-albedo cooling strength.
  , wcCloudAlbedoEffect :: !Float
    -- | Cloud-enhanced precipitation strength.
  , wcCloudPrecipBoost :: !Float
    -- | Cloud formation rate from humidity exceeding saturation threshold.
  , wcCloudFormationRate :: !Float
    -- | Cloud dissipation rate when humidity drops below saturation.
  , wcCloudDissipationRate :: !Float
    -- | Relative humidity threshold above which clouds form.
  , wcCloudSaturationThreshold :: !Float
    -- | Temperature excess (above seasonal baseline) triggering convective
    -- cloud formation.  Lower values → more convective clouds.
  , wcConvectiveThreshold :: !Float
    -- | High-pressure subsidence cloud suppression strength.
    -- Controls how much high-pressure systems dissolve clouds.
  , wcSubsidenceDissipation :: !Float
    -- | Cloud water threshold for autoconversion to precipitation.
    -- Precipitation only forms when cloud water exceeds this value.
  , wcAutoconversionThreshold :: !Float
    -- | Fraction of excess cloud water above threshold that precipitates
    -- per tick.  Higher values produce heavier rain from thick clouds.
  , wcPrecipEfficiency :: !Float
    -- | Cloud optical depth scale: optical depth = cloudWater × this value.
    -- Controls how much radiation thick clouds block.  Higher = opaquer.
  , wcCloudOpticalScale :: !Float
    -- | Cloud longwave greenhouse coefficient.  Cloudy skies trap outgoing
    -- longwave radiation, warming the surface at night.  Higher = stronger.
  , wcCloudGreenhouseCoeff :: !Float
    -- | Temperature diffusion passes in chunk weather builder.
  , wcTempDiffuseIterations :: !Int
    -- | Temperature diffusion factor in chunk weather builder.
  , wcTempDiffuseFactor :: !Float
    -- | Coherent temperature noise frequency.
  , wcTempNoiseFrequency :: !Float
    -- | Coherent humidity noise frequency.
  , wcHumidityNoiseFrequency :: !Float
    -- | Coherent pressure noise frequency.
  , wcPressureNoiseFrequency :: !Float
    -- | Advection time-step parameter (CFL-safe; expected < 0.5).
  , wcAdvectDt :: !Float
    -- | Relaxation strength toward seasonal climate baselines.
  , wcClimatePullStrength :: !Float
    -- | Humidity excess condensation rate.
  , wcCondensationRate :: !Float
    -- | Wind-direction response rate to pressure gradients.
  , wcWindResponseRate :: !Float
    -- | Diffusion factor used by global weather operators.
  , wcWeatherDiffuseFactor :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherConfig where
  toJSON = genericToJSON (configOptions "wc")

instance FromJSON WeatherConfig where
  parseJSON v =
    genericParseJSON (configOptions "wc")
      (mergeDefaults (toJSON defaultWeatherConfig) v)

-- | Default weather configuration tuned for stable, smooth weather evolution.
defaultWeatherConfig :: WeatherConfig
defaultWeatherConfig = WeatherConfig
  { wcTickSeconds               = 1
  , wcSeasonPhase               = 0
  , wcSeasonAmplitude           = 0.21
  , wcSeasonCycleLength         = 365
  , wcJitterAmplitude           = 0.126
  , wcPressureBase              = 0.7
  , wcPressureTempScale         = 0.4
  , wcPressureCoriolisScale     = 0.1
  , wcSeasonalBase              = 0.40
  , wcSeasonalRange             = 1.20
  , wcHumidityNoiseScale        = 0.10
  , wcPrecipNoiseScale          = 0.15
  , wcITCZLatitude              = 0
  , wcITCZWidth                 = 10.0
  , wcITCZPrecipBoost           = 0.30
  , wcPressureHumidityScale     = 0.10
  , wcPressureGradientWindScale = 0.30
  , wcWindNoiseScale            = 0.10
  , wcITCZMigrationScale        = 0.70
  , wcCloudRHExponent           = 1.50
  , wcCloudAlbedoEffect         = 0.08
  , wcCloudPrecipBoost          = 0.12
  , wcCloudFormationRate        = 0.15
  , wcCloudDissipationRate      = 0.08
  , wcCloudSaturationThreshold  = 0.60
  , wcConvectiveThreshold       = 0.08
  , wcSubsidenceDissipation     = 0.10
  , wcAutoconversionThreshold   = 0.30
  , wcPrecipEfficiency          = 0.40
  , wcCloudOpticalScale         = 3.0
  , wcCloudGreenhouseCoeff      = 0.02
  , wcTempDiffuseIterations     = 1
  , wcTempDiffuseFactor         = 0.2
  , wcTempNoiseFrequency        = 0.08
  , wcHumidityNoiseFrequency    = 0.08
  , wcPressureNoiseFrequency    = 0.08
  , wcAdvectDt                  = 0.3
  , wcClimatePullStrength       = 0.05
  , wcCondensationRate          = 0.4
  , wcWindResponseRate          = 0.3
  , wcWeatherDiffuseFactor      = 0.15
  }
