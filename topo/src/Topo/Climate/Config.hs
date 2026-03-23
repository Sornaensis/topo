{-# LANGUAGE DeriveGeneric #-}
-- | Climate sub-configuration types.
--
-- Groups the 40+ fields of the old flat 'ClimateConfig' into five
-- cohesive sub-configs, each modelling a distinct physical domain:
--
-- * 'TemperatureConfig' — latitude-to-temperature curve, lapse rate,
--   ocean moderation, insolation.
-- * 'WindConfig' — atmospheric wind belt simulation parameters.
-- * 'MoistureConfig' — evaporation, advection, condensation.
-- * 'PrecipitationConfig' — orographic uplift, rain shadow, coastal
--   moisture boost.
-- * 'BoundaryConfig' — legacy tectonic-bias knobs (currently no-op).
--
-- 'ClimateConfig' bundles the five sub-configs and is the single type
-- threaded through the climate pipeline.
module Topo.Climate.Config
  ( -- * Top-level climate configuration
    ClimateConfig(..)
  , defaultClimateConfig
    -- * Temperature
  , TemperatureConfig(..)
  , defaultTemperatureConfig
    -- * Wind
  , WindConfig(..)
  , defaultWindConfig
    -- * Moisture
  , MoistureConfig(..)
  , defaultMoistureConfig
    -- * Precipitation
  , PrecipitationConfig(..)
  , defaultPrecipitationConfig
    -- * Boundary
  , BoundaryConfig(..)
  , defaultBoundaryConfig
    -- * Seasonality
  , SeasonalityConfig(..)
  , defaultSeasonalityConfig
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  ( configOptions, multiPrefixOptions, mergeDefaults
  , ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, Value
  )

------------------------------------------------------------------------
-- Temperature
------------------------------------------------------------------------

-- | Temperature model configuration.
--
-- Controls how latitude, elevation, plate height, ocean proximity, and
-- insolation combine into the per-tile temperature field.
--
-- __Latitude curve:__ @cos(lat) ^ 'tmpLatitudeExponent'@.  Lower
-- exponents flatten the curve (more atmospheric heat redistribution
-- towards the poles); higher values steepen it.
--
-- __Lapse rate:__ Applied only to height /above/ sea level.  Ocean
-- tiles receive zero lapse cooling.
--
-- __Ocean moderation:__ Coastal tiles are pulled towards
-- 'tmpOceanModerateTemp' by the factor 'tmpOceanModeration' ×
-- coastal proximity.
data TemperatureConfig = TemperatureConfig
  { tmpEquatorTemp       :: !Float
  -- ^ Base temperature at the equator (0–1).  Default: @0.78@
  -- (28 °C with the [−50,+50] °C scale).
  , tmpPoleTemp          :: !Float
  -- ^ Base temperature at the poles (0–1).  Default: @0.0@
  -- (−50 °C, realistic Antarctic interior).
  , tmpLapseRate         :: !Float
  -- ^ Cooling per unit of normalised height above sea level.
  -- Targets ~5.5 °C\/km over the 12 000 m elevation range.
  -- Default: @0.66@.
  , tmpLatitudeExponent  :: !Float
  -- ^ Exponent applied to @cos(latitude)@ for the latitude–temperature
  -- curve.  An Earth-empirical fit is ≈ 1.0.  Default: @1.0@.
  , tmpPlateHeightCooling :: !Float
  -- ^ Continental-elevation cooling coefficient.  Models the mild
  -- cooling of high plateaus /without/ duplicating the main lapse
  -- rate.  Default: @0.035@.
  , tmpNoiseScale        :: !Float
  -- ^ Random spatial-noise amplitude added to temperature (0–1).
  -- Default: @0.07@.
  , tmpNoiseOctaves      :: !Int
  -- ^ Number of coherent-noise octaves for terrain temperature
  -- perturbation.  @1@ uses a single octave; larger values add
  -- multi-scale detail while preserving spatial continuity.
  -- Default: @2@.
  , tmpNoiseFrequency    :: !Float
  -- ^ Base world-space frequency for coherent temperature noise.
  -- Lower values produce broader structures; higher values produce
  -- finer variation.  Default: @0.08@.
  , tmpDiffuseIterations :: !Int
  -- ^ Number of local diffusion passes for the temperature field.
  -- Higher values increase smoothing and reduce tile-to-tile variance.
  -- Default: @3@.
  , tmpDiffuseFactor     :: !Float
  -- ^ Per-pass diffusion strength for temperature smoothing (0–1).
  -- Default: @0.35@.
  , tmpCoastalBlendWidth :: !Float
  -- ^ Elevation half-width around sea level used to smoothly blend
  -- ocean and land temperature curves (and lapse effect).  Larger
  -- values broaden the shoreline transition and reduce abrupt thermal
  -- jumps at the coast.  Default: @0.02@.
  , tmpOceanModeration   :: !Float
  -- ^ Strength of ocean thermal moderation on coastal tiles.
  -- 0 = no moderation, 1 = full pull towards 'tmpOceanModerateTemp'.
  -- Default: @0.3@.
  , tmpOceanModerateTemp :: !Float
  -- ^ Offset added to the latitude-dependent SST when computing the
  -- ocean moderation target for coastal tiles.  The target is
  -- @SST(lat) + tmpOceanModerateTemp@.  Default: @0@ (pure SST pull).
  , tmpAlbedoSensitivity :: !Float
  -- ^ How strongly surface albedo modifies temperature (Model H).
  -- @temp_corrected = temp × (1 - sensitivity × (albedo - reference))@.
  -- Default: @0.20@.
  , tmpAlbedoReference   :: !Float
  -- ^ Reference albedo at which no temperature adjustment occurs.
  -- Earth-average land surface ≈ 0.30.  Default: @0.30@.
  , tmpOceanEquatorSST   :: !Float
  -- ^ Normalised equatorial ocean SST ceiling.  Maps to ~28 °C.
  -- The ocean SST profile is:
  -- @lerp(poleSST, equatorSST, cos(lat) ^ oceanLatExponent)@.
  -- Default: @0.781@.
  , tmpOceanPoleSST      :: !Float
  -- ^ Normalised polar ocean SST floor.  Maps to ~0 °C.
  -- Default: @0.501@.
  , tmpOceanLatExponent  :: !Float
  -- ^ Latitude exponent for the ocean SST curve.  Higher than land
  -- because evaporative cooling steepens the tropical→polar gradient.
  -- Default: @2.0@.
  } deriving (Eq, Show, Generic)

-- | Serialise with @tmp@ prefix stripped from field names.
instance ToJSON TemperatureConfig where
  toJSON = genericToJSON (configOptions "tmp")

-- | Deserialise with defaults for any missing field.
instance FromJSON TemperatureConfig where
  parseJSON v = genericParseJSON (configOptions "tmp") (mergeDefaults (toJSON defaultTemperatureConfig) v)

-- | Sensible Earth-like defaults for the temperature model.
defaultTemperatureConfig :: TemperatureConfig
defaultTemperatureConfig = TemperatureConfig
  { tmpEquatorTemp       = 0.78  -- 28 °C (matches slider default & ocean SST)
  , tmpPoleTemp          = 0
  , tmpLapseRate         = 0.66
  , tmpLatitudeExponent  = 1.0
  , tmpPlateHeightCooling = 0.035
  , tmpNoiseScale        = 0.07
  , tmpNoiseOctaves      = 2
  , tmpNoiseFrequency    = 0.08
  , tmpDiffuseIterations = 3
  , tmpDiffuseFactor     = 0.35
  , tmpCoastalBlendWidth = 0.02
  , tmpOceanModeration   = 0.3
  , tmpOceanModerateTemp = 0
  , tmpAlbedoSensitivity = 0.20
  , tmpAlbedoReference   = 0.30
  , tmpOceanEquatorSST   = 0.781
  , tmpOceanPoleSST      = 0.501
  , tmpOceanLatExponent  = 2.0
  }

------------------------------------------------------------------------
-- Wind
------------------------------------------------------------------------

-- | Atmospheric wind simulation configuration.
--
-- Wind direction and speed are derived from zonal wind belts
-- (trade winds, westerlies, polar easterlies) modulated by noise,
-- then smoothed via iterative diffusion.
--
-- __Belt model:__ A three-band Coriolis model computes the prevailing
-- wind direction based on latitude: trade winds (0–30°), westerlies
-- (30–60°), and polar easterlies (60–90°), each with realistic
-- meridional deflection controlled by 'windCoriolisDeflection'.
-- 'windBeltStrength' controls how strongly the zonal belt dominates
-- over random noise.
data WindConfig = WindConfig
  { windIterations      :: !Int
  -- ^ Number of diffusion\/smoothing passes.  Default: @4@.
  , windDiffuse         :: !Float
  -- ^ Smoothing factor per diffusion pass (0–1).  Default: @0.5@.
  , windBeltStrength    :: !Float
  -- ^ Influence of zonal wind belts on direction (0–1).
  -- Default: @0.6@.
  , windBeltHarmonics   :: !Float
  -- ^ Number of harmonic cycles in the belt model.  Default: @3@.
  , windBeltBase        :: !Float
  -- ^ Base wind speed contributed by belts (0–1).  Default: @0.4@.
  , windBeltRange       :: !Float
  -- ^ Wind-speed variation range from belts (0–1).  Default: @0.6@.
  , windBeltSpeedScale  :: !Float
  -- ^ Speed multiplier from belt influence (0–1).  Default: @0.6@.
  , windCoriolisDeflection :: !Float
  -- ^ Angular deflection from pure zonal flow (radians) due to
  -- Coriolis effect.  Applied hemisphere-aware: NH deflects right,
  -- SH deflects left.  Default: @0.45@ (≈26°).
  -- 0 = purely east\/west winds; π\/2 = purely meridional.
  } deriving (Eq, Show, Generic)

-- | Serialise with @wind@ prefix stripped from field names.
instance ToJSON WindConfig where
  toJSON = genericToJSON (configOptions "wind")

-- | Deserialise with defaults for any missing field.
instance FromJSON WindConfig where
  parseJSON v = genericParseJSON (configOptions "wind") (mergeDefaults (toJSON defaultWindConfig) v)

-- | Sensible Earth-like defaults for the wind model.
defaultWindConfig :: WindConfig
defaultWindConfig = WindConfig
  { windIterations      = 4
  , windDiffuse         = 0.5
  , windBeltStrength    = 0.6
  , windBeltHarmonics   = 3
  , windBeltBase        = 0.4
  , windBeltRange       = 0.6
  , windBeltSpeedScale  = 0.6
  , windCoriolisDeflection = 0.45
    -- ^ ~26° deflection from zonal; produces realistic NE trades,
    -- SW westerlies (NH), and mirror images in SH.
  }

------------------------------------------------------------------------
-- Moisture
------------------------------------------------------------------------

-- | Moisture and evaporation configuration.
--
-- Moisture originates from physics-based ocean evaporation (Dalton's
-- Law, Model B) and land evapotranspiration (Penman-Monteith-inspired,
-- Model C).  Both use the Clausius-Clapeyron saturation curve
-- ('Topo.Climate.Evaporation.satNorm') for temperature dependence.
--
-- Moisture is transported by wind via iterative advection, with
-- saturation-based condensation (Model E.2), per-iteration
-- evapotranspiration recycling (Model E.3), and ITCZ convergence
-- boosting (Model E.5).
--
-- The advection balance is controlled by 'moistAdvect' (fraction of
-- upwind moisture carried forward) and 'moistLocal' (fraction of
-- local moisture retained).
data MoistureConfig = MoistureConfig
  { moistIterations         :: !Int
  -- ^ Number of moisture-transport iterations.  Default: @6@.
  , moistAdvect             :: !Float
  -- ^ Fraction of upwind moisture advected per step (0–1).
  -- Default: @0.85@.
  , moistLocal              :: !Float
  -- ^ Fraction of local moisture retained per step (0–1).
  -- Default: @0.15@.
  , moistWindEvapScale      :: !Float
  -- ^ Wind enhancement factor for ocean evaporation.
  -- Default: @0.30@.
  , moistEvapNoiseScale     :: !Float
  -- ^ Random noise amplitude on evaporation (0–1).  Default: @0.05@.
  , moistBareEvapFrac       :: !Float
  -- ^ Bare-soil evaporation fraction (0–1).  Even bare soil
  -- evaporates some moisture.  Default: @0.15@.
  , moistVegTranspFrac      :: !Float
  -- ^ Vegetation transpiration fraction (0–1).  Dominant ET pathway
  -- on densely vegetated tiles.  Default: @0.85@.
  , moistWindETScale        :: !Float
  -- ^ Wind enhancement factor for land evapotranspiration.
  -- Default: @0.20@.
  , moistCondensationRate   :: !Float
  -- ^ Precipitation rate from temperature-drop condensation.
  -- When moist air advects from a warm to a cool tile, excess
  -- moisture precipitates at this rate.  Default: @0.20@.
  , moistRecycleRate        :: !Float
  -- ^ Per-iteration evapotranspiration recycling efficiency.
  -- Vegetated land reintroduces a fraction of condensed moisture.
  -- Default: @0.35@.
  , moistITCZStrength       :: !Float
  -- ^ ITCZ convergence zone moisture boost strength.  Divided by the
  -- iteration count before use; at 36 iterations this gives ~0.007
  -- per iteration.  Default: @0.25@.
  , moistITCZWidth          :: !Float
  -- ^ ITCZ convergence zone width in degrees latitude.
  -- Default: @8.0@.
  , ccTempToC_Scale         :: !Float
  -- ^ Normalised-temperature → °C scale factor.  Default: @100.0@
  -- (i.e. @T ∈ [0,1]@ maps to @[offset, offset+scale]@ °C).
  , ccTempToC_Offset        :: !Float
  -- ^ Normalised-temperature → °C offset.  Default: @−50.0@.
  , moistInternalLandBase   :: !Float
  -- ^ Baseline soil water availability for land evapotranspiration,
  -- independent of hydrology-stage 'tcMoisture'.  Interior land tiles
  -- that are far from rivers have near-zero 'tcMoisture' from flow
  -- accumulation; this floor ensures the climate model's Penman-Monteith
  -- ET can still produce atmospheric moisture over land.
  -- Default: @0.25@.
  , moistAdvectSpeed        :: !Float
  -- ^ Wind speed multiplier for moisture advection distance.  Each
  -- transport iteration moves moisture by @windSpeed × moistAdvectSpeed@
  -- tiles.  Higher values let moisture penetrate deeper into continental
  -- interiors.  Default: @2.0@.
  , moistBaseRecycleRate    :: !Float
  -- ^ Per-iteration land evapotranspiration injection rate.  On each
  -- transport step, vegetated land tiles inject
  -- @moistBaseRecycleRate × vegCover × satNorm(T)@ moisture, modelling
  -- direct leaf and soil surface evaporation (\"flying rivers\").
  -- Default: @0.10@.
  , moistConvectiveThreshold :: !Float
  -- ^ Relative-humidity threshold for convective precipitation
  -- (Model E.7).  When @totalMoisture / satNorm(T) > threshold@,
  -- warm moist air undergoes spontaneous convective uplift, producing
  -- precipitation even on flat terrain.  Default: @0.75@
  -- (~75 % RH, allowing tropical convection to fire over land).
  , moistConvectiveRate     :: !Float
  -- ^ Convective precipitation rate per unit excess RH (Model E.7).
  -- Scales with temperature (warmer → more CAPE → more convective
  -- rain) and local saturation capacity.  Default: @0.12@.
  , moistMinVegFloor        :: !Float
  -- ^ Minimum vegetation cover assumed for land ET reinjection.
  -- Breaks the vegetation cold-start problem: even barren land has
  -- some ground cover, leaf litter, or soil surface evaporation.
  -- Without this floor, interior land gets zero moisture → zero
  -- vegetation → zero ET recycling → permanent desert.
  -- Default: @0.15@.
  } deriving (Eq, Show, Generic)

-- | Serialise with @moist@\/@cc@ prefixes stripped from field names.
-- Uses 'multiPrefixOptions' because two fields use the @cc@ prefix
-- ('ccTempToC_Scale', 'ccTempToC_Offset') while the rest use @moist@.
instance ToJSON MoistureConfig where
  toJSON = genericToJSON (multiPrefixOptions ["moist", "cc"])

-- | Deserialise with defaults for any missing field.
instance FromJSON MoistureConfig where
  parseJSON v = genericParseJSON (multiPrefixOptions ["moist", "cc"]) (mergeDefaults (toJSON defaultMoistureConfig) v)

-- | Sensible Earth-like defaults for the moisture model.
defaultMoistureConfig :: MoistureConfig
defaultMoistureConfig = MoistureConfig
  { moistIterations         = 36
  , moistAdvect             = 0.85
  , moistLocal              = 0.15
  , moistWindEvapScale      = 0.30
  , moistEvapNoiseScale     = 0.05
  , moistBareEvapFrac       = 0.15
  , moistVegTranspFrac      = 0.85
  , moistWindETScale        = 0.20
  , moistCondensationRate   = 0.20
    -- ^ Controls how much excess moisture condenses per iteration.
    -- At 0.20, moisture survives ~20 tiles inland before drying out,
    -- balancing precipitation reach against coastal concentration.
  , moistRecycleRate        = 0.35
    -- ^ Per-iteration evapotranspiration recycling.  Vegetated land
    -- reintroduces 35% of condensed moisture, extending inland reach.
  , moistITCZStrength       = 0.25
    -- ^ Raised from 0.15; divided by 36 iterations this gives ~0.007
    -- per iteration — a meaningful equatorial precipitation boost
    -- via convergence-driven uplift.
  , moistITCZWidth          = 8.0
  , ccTempToC_Scale         = 100.0
  , ccTempToC_Offset        = -50.0
  , moistInternalLandBase   = 0.35
    -- ^ Raised from 0.25; higher baseline soil moisture gives land
    -- ET something to work with even before hydrology runs.
  , moistAdvectSpeed        = 3.0
    -- ^ Raised from 2.0; each iteration moves moisture 3 tiles
    -- instead of 2, increasing max inland reach to ~108 tiles.
  , moistBaseRecycleRate    = 0.10
    -- ^ Land ET reinjection rate per iteration; vegetated tiles inject
    -- moistBaseRecycleRate * vegCover * satNorm(T) moisture.
  , moistConvectiveThreshold = 0.75
    -- ^ Lowered from 0.80; tropical convection begins around
    -- 75% RH, allowing warm humid coastal and equatorial tiles
    -- to produce convective rain while keeping interior tiles dry.
  , moistConvectiveRate     = 0.10
    -- ^ Raised from 0.08; moderate convective precipitation helps
    -- fill the upper precipitation bands (Forest/Rainforest).
  , moistMinVegFloor        = 0.15
    -- ^ Breaks the vegetation cold-start: assumes at least some
    -- ground cover for ET recycling.
  }

------------------------------------------------------------------------
-- Precipitation
------------------------------------------------------------------------

-- | Precipitation and orographic-effects configuration.
--
-- Controls orographic uplift (windward precipitation enhancement),
-- rain-shadow moisture loss (leeward transport barrier), and the
-- coastal proximity moisture boost that keeps coastal tiles wetter
-- than interiors.
--
-- __Orographic model:__ Precipitation increases on the windward side
-- of elevation rises by @rise × 'precOrographicLift' ×
-- 'precOrographicScale'@.  The per-iteration moisture sink on the
-- leeward side is controlled separately by 'precRainShadowLoss',
-- preventing exponential drying over many transport iterations.
--
-- Prior to Phase 2.1, a single @precRainShadow@ field controlled
-- both effects, causing excessive drying in mountain chains.
data PrecipitationConfig = PrecipitationConfig
  { precOrographicLift   :: !Float
  -- ^ Orographic precipitation enhancement strength.  Scales the
  -- windward-side uplift precipitation (used in 'orographicAt',
  -- 'boundaryOrogenyAt', and 'plateHeightPrecipBiasAt').
  -- Higher values produce more rain on the windward side of
  -- mountains and at plate boundaries.  Default: @0.35@.
  , precRainShadowLoss   :: !Float
  -- ^ Per-iteration moisture sink from elevation barriers.  When
  -- air flows across a terrain rise, moisture is removed at
  -- @max 0 (upwindElev − localElev) × precRainShadowLoss@ per
  -- transport iteration.  Kept deliberately low (relative to the
  -- old combined @precRainShadow = 0.4@) to avoid exponential
  -- drying over 36 iterations.  Default: @0.15@.
  , precOrographicScale  :: !Float
  -- ^ Orographic uplift scaling factor.  Default: @0.6@.
  , precOrographicStep   :: !Float
  -- ^ Upwind sampling distance for orography (tiles).
  -- Default: @1@.
  , precCoastalIterations :: !Int
  -- ^ Number of coastal-proximity diffusion passes.  Default: @8@.
  , precCoastalDiffuse   :: !Float
  -- ^ Diffusion factor per coastal-proximity pass (0–1).
  -- Default: @0.5@.
  , precCoastalMoistureBoost :: !Float
  -- ^ Moisture boost from coastal proximity (0–1).  Default: @0.20@.
  , precPolarFloor       :: !Float
  -- ^ Minimum precipitation at extreme latitudes.  Earth's polar
  -- regions receive ~100–200 mm\/yr, which is low but non-zero.
  -- This floor prevents \"false deserts\" in polar regions where
  -- evaporation\/transport produces near-zero moisture.
  -- Default: @0.05@.
  , precPolarLatitude    :: !Float
  -- ^ Latitude (in degrees) beyond which the polar precipitation
  -- floor is applied.  The floor ramps in linearly from this
  -- latitude to the pole.  Default: @60.0@.
  } deriving (Eq, Show, Generic)

-- | Serialise with @prec@ prefix stripped from field names.
instance ToJSON PrecipitationConfig where
  toJSON = genericToJSON (configOptions "prec")

-- | Deserialise with defaults for any missing field.
instance FromJSON PrecipitationConfig where
  parseJSON v = genericParseJSON (configOptions "prec") (mergeDefaults (toJSON defaultPrecipitationConfig) v)

-- | Sensible Earth-like defaults for the precipitation model.
defaultPrecipitationConfig :: PrecipitationConfig
defaultPrecipitationConfig = PrecipitationConfig
  { precOrographicLift   = 0.35
  , precRainShadowLoss   = 0.08
    -- ^ Reduced from 0.15; over 36 iterations the old value drained
    -- moisture completely over even gentle terrain.  0.08 still
    -- produces visible rain shadows on real mountains.
  , precOrographicScale  = 0.6
  , precOrographicStep   = 1
  , precCoastalIterations = 8
  , precCoastalDiffuse   = 0.5
  , precCoastalMoistureBoost = 0.20
  , precPolarFloor       = 0.05
  , precPolarLatitude    = 60.0
  }

------------------------------------------------------------------------
-- Boundary (tectonic plate-boundary influence)
------------------------------------------------------------------------

-- | Legacy tectonic-bias configuration.
--
-- These fields remain in the public config schema for backward
-- compatibility, but direct boundary-driven temperature/precipitation
-- bias is disabled in the climate model.
--
-- Deprecated: retained only for JSON compatibility with older configs.
data BoundaryConfig = BoundaryConfig
  { bndMotionTemp        :: !Float
  -- ^ Velocity → temperature-bias scaling.  Default: @0.5@.
  , bndMotionPrecip      :: !Float
  -- ^ Velocity → precipitation-bias scaling.  Default: @0.5@.
  , bndLandRange         :: !Float
  -- ^ Normalising range for the land-fraction calculation (must be
  -- > 0).  Default: @0.6@.
  , bndTempConvergent    :: !Float
  -- ^ Temperature bias at convergent boundaries (typically negative
  -- = cooling).  Default: @−0.042@.
  , bndTempDivergent     :: !Float
  -- ^ Temperature bias at divergent boundaries.  Default: @0.014@.
  , bndTempTransform     :: !Float
  -- ^ Temperature bias at transform boundaries.  Default: @−0.007@.
  , bndPrecipConvergent  :: !Float
  -- ^ Precipitation bias at convergent boundaries.  Default: @0.08@.
  , bndPrecipDivergent   :: !Float
  -- ^ Precipitation bias at divergent boundaries.
  -- Default: @−0.05@.
  , bndPrecipTransform   :: !Float
  -- ^ Precipitation bias at transform boundaries.  Default: @0.02@.
  } deriving (Eq, Show, Generic)

-- | Serialise with @bnd@ prefix stripped from field names.
instance ToJSON BoundaryConfig where
  toJSON = genericToJSON (configOptions "bnd")

-- | Deserialise with defaults for any missing field.
instance FromJSON BoundaryConfig where
  parseJSON v = genericParseJSON (configOptions "bnd") (mergeDefaults (toJSON defaultBoundaryConfig) v)

-- | Neutral defaults for legacy boundary-bias fields.
defaultBoundaryConfig :: BoundaryConfig
defaultBoundaryConfig = BoundaryConfig
  { bndMotionTemp        = 0
  , bndMotionPrecip      = 0
  , bndLandRange         = 0.6
  , bndTempConvergent    = 0
  , bndTempDivergent     = 0
  , bndTempTransform     = 0
  , bndPrecipConvergent  = 0
  , bndPrecipDivergent   = 0
  , bndPrecipTransform   = 0
  }

------------------------------------------------------------------------
-- Seasonality
------------------------------------------------------------------------

-- | Analytical seasonality estimation parameters.
--
-- These mirror the relevant fields from 'Topo.Weather.WeatherConfig'
-- so that 'Topo.Climate.generateClimateStage' can compute temperature
-- range and precipitation seasonality /before/ the weather stage runs.
-- The pipeline wires the weather config values into this sub-config
-- at construction time (see 'Topo.WorldGen').
--
-- __Temperature range:__
-- @tempRange = clamp01(tempAvg + amp × |sin lat|)
--            - clamp01(tempAvg - amp × |sin lat|)@
-- where @amp = 'scSeasonAmplitude' × tiltScale@.
--
-- __Precipitation seasonality:__
-- @precipSeasonality = 1 - minFactor / max 0.001 maxFactor@
-- where the seasonal factor extremes are derived from
-- 'scSeasonalBase' and 'scSeasonalRange'.
data SeasonalityConfig = SeasonalityConfig
  { scHumiditySoilContribution :: !Float
  -- ^ Additive humidity contribution from soil moisture.
  -- Tiles with high soil moisture (from the hydrology stage) receive
  -- @soilMoisture × scHumiditySoilContribution@ extra humidity,
  -- capturing surface boundary-layer effects (fog, dew, microclimate)
  -- that the column-average atmospheric transport model does not resolve.
  -- Default: @0.10@.
  , scSeasonalityContinentalityFactor :: !Float
  -- ^ Continentality amplification of precipitation seasonality.
  -- Interior (non-coastal) tiles have their seasonality multiplied by
  -- @1 + scSeasonalityContinentalityFactor × (1 - coastalProximity)@.
  -- Coastal tiles receive near-baseline seasonality; continental
  -- interiors receive amplified seasonality.
  -- Default: @0.40@.
  , scSeasonalityRainShadowBoost :: !Float
  -- ^ Rain-shadow amplification of precipitation seasonality.
  -- On the leeward side of elevation rises, precipitation becomes
  -- more seasonal because orographic rain falls primarily in the
  -- wet season.  The boost is:
  -- @scSeasonalityRainShadowBoost × max 0 (upwindElev − localElev)@.
  -- Default: @0.15@.
  , scTempRangeOceanDamping :: !Float
  -- ^ Ocean damping of annual temperature range.
  -- Coastal tiles have their temperature range damped by
  -- @1 - scTempRangeOceanDamping × coastalProximity@, modelling
  -- the moderating influence of oceanic thermal mass.
  -- Default: @0.50@.
  } deriving (Eq, Show, Generic)

-- | Serialise with @sc@ prefix stripped from field names.
instance ToJSON SeasonalityConfig where
  toJSON = genericToJSON (configOptions "sc")

-- | Deserialise with defaults for any missing field.
instance FromJSON SeasonalityConfig where
  parseJSON v = genericParseJSON (configOptions "sc") (mergeDefaults (toJSON defaultSeasonalityConfig) v)

-- | Sensible Earth-like defaults for the seasonality model.
defaultSeasonalityConfig :: SeasonalityConfig
defaultSeasonalityConfig = SeasonalityConfig
  { scHumiditySoilContribution = 0.10
  , scSeasonalityContinentalityFactor = 0.40
  , scSeasonalityRainShadowBoost = 0.15
  , scTempRangeOceanDamping = 0.50
  }

------------------------------------------------------------------------
-- ClimateConfig (top-level bundle)
------------------------------------------------------------------------

-- | Top-level climate configuration.
--
-- Groups six sub-configs covering temperature, wind, moisture,
-- precipitation, plate-boundary influence, and seasonality.
--
-- Use 'defaultClimateConfig' for Earth-like defaults.  To tweak
-- individual knobs, update the relevant sub-config via nested record
-- update:
--
-- @
-- cfg = defaultClimateConfig
--   { ccTemperature = (ccTemperature defaultClimateConfig)
--       { tmpLapseRate = 0.5 }
--   }
-- @
data ClimateConfig = ClimateConfig
  { ccTemperature    :: !TemperatureConfig
  -- ^ Temperature model parameters.
  , ccWind           :: !WindConfig
  -- ^ Wind simulation parameters.
  , ccMoisture       :: !MoistureConfig
  -- ^ Moisture and evaporation parameters.
  , ccPrecipitation  :: !PrecipitationConfig
  -- ^ Precipitation and orographic parameters.
  , ccBoundary       :: !BoundaryConfig
  -- ^ Plate-boundary climate influence parameters.
  , ccSeasonality    :: !SeasonalityConfig
  -- ^ Analytical seasonality estimation parameters.
  } deriving (Eq, Show, Generic)

-- | Serialise with @cc@ prefix stripped from field names.
instance ToJSON ClimateConfig where
  toJSON = genericToJSON (configOptions "cc")

-- | Deserialise with defaults for any missing field.
instance FromJSON ClimateConfig where
  parseJSON v = genericParseJSON (configOptions "cc") (mergeDefaults (toJSON defaultClimateConfig) v)

-- | Earth-like default climate configuration.
defaultClimateConfig :: ClimateConfig
defaultClimateConfig = ClimateConfig
  { ccTemperature    = defaultTemperatureConfig
  , ccWind           = defaultWindConfig
  , ccMoisture       = defaultMoistureConfig
  , ccPrecipitation  = defaultPrecipitationConfig
  , ccBoundary       = defaultBoundaryConfig
  , ccSeasonality    = defaultSeasonalityConfig
  }
