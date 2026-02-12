{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Core types for config presets.
--
-- This module is kept free of 'Actor.UI' imports to avoid module
-- cycles.  The conversion functions ('presetFromUi', 'applyPresetToUi')
-- live in "Seer.Config.Preset".
module Seer.Config.Preset.Types
  ( ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
  , presetJsonOptions
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:?)
  , (.!=)
  , genericToJSON
  , withObject
  , defaultOptions
  , Options(..)
  )
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Version tag
-- ---------------------------------------------------------------------------

-- | Monotonically increasing version tag for forward compatibility.
-- Bump this when the set of fields changes.
currentPresetVersion :: Int
currentPresetVersion = 1

-- ---------------------------------------------------------------------------
-- ConfigPreset data type
-- ---------------------------------------------------------------------------

-- | A snapshot of all generation-relevant slider values from the UI.
--
-- All @Float@ fields store normalised slider positions in @[0, 1]@.
-- Metadata fields ('cpName', 'cpVersion') are not restored by
-- @applyPresetToUi@.
data ConfigPreset = ConfigPreset
  { -- | Human-readable preset name.
    cpName                    :: Text
    -- | Schema version for forward compatibility.
  , cpVersion                 :: Int
    -- | RNG seed.
  , cpSeed                    :: Word64
    -- | Chunk size (8–256).
  , cpChunkSize               :: Int
    -- Hydrology
  , cpWaterLevel              :: Float
  , cpRenderWaterLevel        :: Float
    -- Climate
  , cpEvaporation             :: Float
  , cpRainShadow              :: Float
  , cpWindDiffuse             :: Float
  , cpEquatorTemp             :: Float
  , cpPoleTemp                :: Float
  , cpLapseRate               :: Float
  , cpWindIterations          :: Float
  , cpMoistureIterations      :: Float
  , cpBoundaryMotionTemp      :: Float
  , cpBoundaryMotionPrecip    :: Float
  , cpLatitudeExponent        :: Float
  , cpPlateHeightCooling      :: Float
  , cpTempNoiseScale          :: Float
  , cpOceanModeration         :: Float
  , cpOceanModerateTemp       :: Float
  , cpAlbedoSensitivity       :: Float
  , cpAlbedoReference         :: Float
    -- Moisture
  , cpMoistAdvect             :: Float
  , cpMoistLocal              :: Float
  , cpMoistWindEvapScale      :: Float
  , cpMoistEvapNoiseScale     :: Float
  , cpMoistLandETCoeff        :: Float
  , cpMoistBareEvapFrac       :: Float
  , cpMoistVegTranspFrac      :: Float
  , cpMoistWindETScale        :: Float
  , cpMoistCondensationRate   :: Float
  , cpMoistRecycleRate        :: Float
  , cpMoistITCZStrength       :: Float
  , cpMoistITCZWidth          :: Float
    -- Precipitation
  , cpOrographicScale         :: Float
  , cpOrographicStep          :: Float
  , cpCoastalIterations       :: Float
  , cpCoastalDiffuse          :: Float
  , cpCoastalMoistureBoost    :: Float
    -- Wind
  , cpWindBeltStrength        :: Float
  , cpWindBeltHarmonics       :: Float
  , cpWindBeltBase            :: Float
  , cpWindBeltRange           :: Float
  , cpWindBeltSpeedScale      :: Float
    -- Boundary model
  , cpBndLandRange            :: Float
  , cpBndTempConvergent       :: Float
  , cpBndTempDivergent        :: Float
  , cpBndTempTransform        :: Float
  , cpBndPrecipConvergent     :: Float
  , cpBndPrecipDivergent      :: Float
  , cpBndPrecipTransform      :: Float
    -- Erosion
  , cpRainRate                :: Float
  , cpErosionHydraulic        :: Float
  , cpErosionThermal          :: Float
  , cpErosionTalus            :: Float
  , cpErosionMaxDrop          :: Float
    -- Glacier
  , cpGlacierSnowTemp         :: Float
  , cpGlacierSnowRange        :: Float
  , cpGlacierMeltTemp         :: Float
  , cpGlacierMeltRate         :: Float
  , cpGlacierAccumScale       :: Float
  , cpGlacierFlowIters        :: Float
  , cpGlacierFlowRate         :: Float
  , cpGlacierErosionScale     :: Float
  , cpGlacierCarveScale       :: Float
  , cpGlacierDepositScale     :: Float
    -- Volcanism
  , cpVentDensity             :: Float
  , cpVentThreshold           :: Float
  , cpHotspotScale            :: Float
  , cpHotspotThreshold        :: Float
  , cpMagmaRecharge           :: Float
  , cpLavaScale               :: Float
  , cpAshScale                :: Float
  , cpVolcanicDepositScale    :: Float
    -- Soil
  , cpSoilMoistureThreshold   :: Float
  , cpSoilHardnessThreshold   :: Float
  , cpSoilFertilityMoistWeight :: Float
  , cpSoilFertilityDepthWeight :: Float
    -- Hydrology / WaterBody / Parameters
  , cpSinkBreachDepth         :: Float
  , cpStreamPowerMaxErosion   :: Float
  , cpRiverCarveMaxDepth      :: Float
  , cpCoastalErodeStrength    :: Float
  , cpHydroHardnessWeight     :: Float
  , cpMinLakeSize             :: Float
  , cpInlandSeaMinSize        :: Float
  , cpRoughnessScale          :: Float
    -- Generation
  , cpGenScale                :: Float
  , cpGenCoordScale           :: Float
  , cpGenOffsetX              :: Float
  , cpGenOffsetY              :: Float
  , cpGenFrequency            :: Float
  , cpGenOctaves              :: Float
  , cpGenLacunarity           :: Float
  , cpGenGain                 :: Float
  , cpGenWarpScale            :: Float
  , cpGenWarpStrength         :: Float
  , cpWorldExtentX            :: Float
  , cpWorldExtentY            :: Float
    -- Ocean edge depth
  , cpEdgeDepthNorth          :: Float
  , cpEdgeDepthSouth          :: Float
  , cpEdgeDepthEast           :: Float
  , cpEdgeDepthWest           :: Float
  , cpEdgeDepthFalloff        :: Float
    -- Tectonics
  , cpPlateSize               :: Float
  , cpPlateSpeed              :: Float
  , cpBoundarySharpness       :: Float
  , cpBoundaryNoiseScale      :: Float
  , cpBoundaryNoiseStrength   :: Float
  , cpBoundaryWarpOctaves     :: Float
  , cpBoundaryWarpLacunarity  :: Float
  , cpBoundaryWarpGain        :: Float
  , cpPlateMergeScale         :: Float
  , cpPlateMergeBias          :: Float
  , cpPlateDetailScale        :: Float
  , cpPlateDetailStrength     :: Float
  , cpPlateRidgeStrength      :: Float
  , cpPlateHeightBase         :: Float
  , cpPlateHeightVariance     :: Float
  , cpPlateHardnessBase       :: Float
  , cpPlateHardnessVariance   :: Float
  , cpUplift                  :: Float
  , cpRiftDepth               :: Float
  , cpTrenchDepth             :: Float
  , cpRidgeHeight             :: Float
  , cpDetailScale             :: Float
  , cpPlateBiasStrength       :: Float
  , cpPlateBiasCenter         :: Float
  , cpPlateBiasEdge           :: Float
  , cpPlateBiasNorth          :: Float
  , cpPlateBiasSouth          :: Float
  , cpTfcCliffSlope           :: Float
  , cpTfcMountainSlope        :: Float
  , cpTfcMountainRelief       :: Float
  , cpTfcHillSlope            :: Float
  , cpTfcRollingSlope         :: Float
  , cpValleyCurvature         :: Float
  , cpRockElevationThreshold  :: Float
  , cpRockHardnessThreshold   :: Float
  , cpRockHardnessSecondary   :: Float
    -- Weather
  , cpWeatherTick             :: Float
  , cpWeatherPhase            :: Float
  , cpWeatherAmplitude        :: Float
  , cpSeasonCycleLength        :: Float
  , cpJitterAmplitude          :: Float
  , cpPressureBase             :: Float
  , cpPressureTempScale        :: Float
  , cpPressureCoriolisScale    :: Float
  , cpSeasonalBase             :: Float
  , cpSeasonalRange            :: Float
  , cpHumidityNoiseScale       :: Float
  , cpPrecipNoiseScale         :: Float
  , cpWeatherITCZWidth         :: Float
  , cpWeatherITCZPrecipBoost   :: Float
  , cpPressureHumidityScale    :: Float
  , cpPressureGradientWindScale :: Float
  , cpWindNoiseScale           :: Float
  , cpITCZMigrationScale       :: Float
  , cpCloudRHExponent          :: Float
  , cpCloudAlbedoEffect        :: Float
  , cpCloudPrecipBoost         :: Float
    -- Vegetation
  , cpVegBase                 :: Float
  , cpVegBoost                :: Float
  , cpVegTempWeight           :: Float
  , cpVegPrecipWeight         :: Float
    -- Biome Thresholds
  , cpBtCoastalBand           :: Float
  , cpBtSnowElevation         :: Float
  , cpBtAlpineElevation       :: Float
  , cpBtIceCapTemp            :: Float
  , cpBtMontaneLow            :: Float
  , cpBtMontanePrecip         :: Float
  , cpBtCliffSlope            :: Float
  , cpBtValleyMoisture        :: Float
  , cpBtDepressionMoisture    :: Float
  , cpBtPrecipWeight          :: Float
    -- Vegetation Bootstrap
  , cpVbcTempMin              :: Float
  , cpVbcTempRange            :: Float
  , cpVbcFertilityBoost       :: Float
  , cpVbcAlbedoBase           :: Float
  , cpVbcAlbedoBare           :: Float
  , cpVbcAlbedoVeg            :: Float
  , cpVbcOceanAlbedo          :: Float
  , cpVbcIceAlbedo            :: Float
    -- Biome Misc
  , cpBiomeSmoothing          :: Float
  , cpVolcanicAshBoost        :: Float
  , cpVolcanicLavaPenalty     :: Float
  , cpBiomeFeedbackBlend      :: Float
    -- Planet
  , cpPlanetRadius            :: Float
  , cpAxialTilt               :: Float
  , cpInsolation              :: Float
    -- Ocean currents
  , cpOccWarmScale            :: Float
  , cpOccColdScale            :: Float
  , cpOccLatPeakDeg           :: Float
  , cpOccLatWidthDeg          :: Float
    -- World slice
  , cpSliceLatCenter          :: Float
  , cpSliceLonCenter          :: Float
  } deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

-- | JSON field names strip the @cp@ prefix and lower-case the first letter.
-- E.g. @cpWaterLevel@ becomes @\"waterLevel\"@ in JSON.
presetJsonOptions :: Options
presetJsonOptions = defaultOptions
  { fieldLabelModifier = dropPrefix }
  where
    dropPrefix ('c':'p':c:rest) = toLowerFirst (c : rest)
    dropPrefix other            = other
    toLowerFirst []     = []
    toLowerFirst (c:cs) = toLower c : cs
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise             = c

instance ToJSON ConfigPreset where
  toJSON = genericToJSON presetJsonOptions

-- | All fields are optional in JSON — missing keys fall back to
-- 'defaultPreset' values, making old presets forward-compatible.
instance FromJSON ConfigPreset where
  parseJSON = withObject "ConfigPreset" $ \o -> do
    let d = defaultPreset
    ConfigPreset
      <$> o .:? "name"                    .!= cpName d
      <*> o .:? "version"                 .!= cpVersion d
      <*> o .:? "seed"                    .!= cpSeed d
      <*> o .:? "chunkSize"               .!= cpChunkSize d
      -- Hydrology
      <*> o .:? "waterLevel"              .!= cpWaterLevel d
      <*> o .:? "renderWaterLevel"        .!= cpRenderWaterLevel d
      -- Climate
      <*> o .:? "evaporation"             .!= cpEvaporation d
      <*> o .:? "rainShadow"              .!= cpRainShadow d
      <*> o .:? "windDiffuse"             .!= cpWindDiffuse d
      <*> o .:? "equatorTemp"             .!= cpEquatorTemp d
      <*> o .:? "poleTemp"                .!= cpPoleTemp d
      <*> o .:? "lapseRate"               .!= cpLapseRate d
      <*> o .:? "windIterations"          .!= cpWindIterations d
      <*> o .:? "moistureIterations"      .!= cpMoistureIterations d
      <*> o .:? "boundaryMotionTemp"      .!= cpBoundaryMotionTemp d
      <*> o .:? "boundaryMotionPrecip"    .!= cpBoundaryMotionPrecip d
      <*> o .:? "latitudeExponent"        .!= cpLatitudeExponent d
      <*> o .:? "plateHeightCooling"      .!= cpPlateHeightCooling d
      <*> o .:? "tempNoiseScale"          .!= cpTempNoiseScale d
      <*> o .:? "oceanModeration"         .!= cpOceanModeration d
      <*> o .:? "oceanModerateTemp"       .!= cpOceanModerateTemp d
      <*> o .:? "albedoSensitivity"       .!= cpAlbedoSensitivity d
      <*> o .:? "albedoReference"         .!= cpAlbedoReference d
      -- Moisture
      <*> o .:? "moistAdvect"             .!= cpMoistAdvect d
      <*> o .:? "moistLocal"              .!= cpMoistLocal d
      <*> o .:? "moistWindEvapScale"      .!= cpMoistWindEvapScale d
      <*> o .:? "moistEvapNoiseScale"     .!= cpMoistEvapNoiseScale d
      <*> o .:? "moistLandETCoeff"        .!= cpMoistLandETCoeff d
      <*> o .:? "moistBareEvapFrac"       .!= cpMoistBareEvapFrac d
      <*> o .:? "moistVegTranspFrac"      .!= cpMoistVegTranspFrac d
      <*> o .:? "moistWindETScale"        .!= cpMoistWindETScale d
      <*> o .:? "moistCondensationRate"   .!= cpMoistCondensationRate d
      <*> o .:? "moistRecycleRate"        .!= cpMoistRecycleRate d
      <*> o .:? "moistITCZStrength"       .!= cpMoistITCZStrength d
      <*> o .:? "moistITCZWidth"          .!= cpMoistITCZWidth d
      -- Precipitation
      <*> o .:? "orographicScale"         .!= cpOrographicScale d
      <*> o .:? "orographicStep"          .!= cpOrographicStep d
      <*> o .:? "coastalIterations"       .!= cpCoastalIterations d
      <*> o .:? "coastalDiffuse"          .!= cpCoastalDiffuse d
      <*> o .:? "coastalMoistureBoost"    .!= cpCoastalMoistureBoost d
      -- Wind
      <*> o .:? "windBeltStrength"        .!= cpWindBeltStrength d
      <*> o .:? "windBeltHarmonics"       .!= cpWindBeltHarmonics d
      <*> o .:? "windBeltBase"            .!= cpWindBeltBase d
      <*> o .:? "windBeltRange"           .!= cpWindBeltRange d
      <*> o .:? "windBeltSpeedScale"      .!= cpWindBeltSpeedScale d
      -- Boundary model
      <*> o .:? "bndLandRange"            .!= cpBndLandRange d
      <*> o .:? "bndTempConvergent"       .!= cpBndTempConvergent d
      <*> o .:? "bndTempDivergent"        .!= cpBndTempDivergent d
      <*> o .:? "bndTempTransform"        .!= cpBndTempTransform d
      <*> o .:? "bndPrecipConvergent"     .!= cpBndPrecipConvergent d
      <*> o .:? "bndPrecipDivergent"      .!= cpBndPrecipDivergent d
      <*> o .:? "bndPrecipTransform"      .!= cpBndPrecipTransform d
      -- Erosion
      <*> o .:? "rainRate"                .!= cpRainRate d
      <*> o .:? "erosionHydraulic"        .!= cpErosionHydraulic d
      <*> o .:? "erosionThermal"          .!= cpErosionThermal d
      <*> o .:? "erosionTalus"            .!= cpErosionTalus d
      <*> o .:? "erosionMaxDrop"          .!= cpErosionMaxDrop d
      <*> o .:? "glacierSnowTemp"         .!= cpGlacierSnowTemp d
      <*> o .:? "glacierSnowRange"        .!= cpGlacierSnowRange d
      <*> o .:? "glacierMeltTemp"         .!= cpGlacierMeltTemp d
      <*> o .:? "glacierMeltRate"         .!= cpGlacierMeltRate d
      <*> o .:? "glacierAccumScale"       .!= cpGlacierAccumScale d
      <*> o .:? "glacierFlowIters"        .!= cpGlacierFlowIters d
      <*> o .:? "glacierFlowRate"         .!= cpGlacierFlowRate d
      <*> o .:? "glacierErosionScale"     .!= cpGlacierErosionScale d
      <*> o .:? "glacierCarveScale"       .!= cpGlacierCarveScale d
      <*> o .:? "glacierDepositScale"     .!= cpGlacierDepositScale d
      <*> o .:? "ventDensity"             .!= cpVentDensity d
      <*> o .:? "ventThreshold"           .!= cpVentThreshold d
      <*> o .:? "hotspotScale"            .!= cpHotspotScale d
      <*> o .:? "hotspotThreshold"        .!= cpHotspotThreshold d
      <*> o .:? "magmaRecharge"           .!= cpMagmaRecharge d
      <*> o .:? "lavaScale"               .!= cpLavaScale d
      <*> o .:? "ashScale"                .!= cpAshScale d
      <*> o .:? "volcanicDepositScale"    .!= cpVolcanicDepositScale d
      <*> o .:? "soilMoistureThreshold"   .!= cpSoilMoistureThreshold d
      <*> o .:? "soilHardnessThreshold"   .!= cpSoilHardnessThreshold d
      <*> o .:? "soilFertilityMoistWeight" .!= cpSoilFertilityMoistWeight d
      <*> o .:? "soilFertilityDepthWeight" .!= cpSoilFertilityDepthWeight d
      <*> o .:? "sinkBreachDepth"         .!= cpSinkBreachDepth d
      <*> o .:? "streamPowerMaxErosion"   .!= cpStreamPowerMaxErosion d
      <*> o .:? "riverCarveMaxDepth"      .!= cpRiverCarveMaxDepth d
      <*> o .:? "coastalErodeStrength"    .!= cpCoastalErodeStrength d
      <*> o .:? "hydroHardnessWeight"     .!= cpHydroHardnessWeight d
      <*> o .:? "minLakeSize"             .!= cpMinLakeSize d
      <*> o .:? "inlandSeaMinSize"        .!= cpInlandSeaMinSize d
      <*> o .:? "roughnessScale"          .!= cpRoughnessScale d
      -- Generation
      <*> o .:? "genScale"                .!= cpGenScale d
      <*> o .:? "genCoordScale"           .!= cpGenCoordScale d
      <*> o .:? "genOffsetX"              .!= cpGenOffsetX d
      <*> o .:? "genOffsetY"              .!= cpGenOffsetY d
      <*> o .:? "genFrequency"            .!= cpGenFrequency d
      <*> o .:? "genOctaves"              .!= cpGenOctaves d
      <*> o .:? "genLacunarity"           .!= cpGenLacunarity d
      <*> o .:? "genGain"                 .!= cpGenGain d
      <*> o .:? "genWarpScale"            .!= cpGenWarpScale d
      <*> o .:? "genWarpStrength"         .!= cpGenWarpStrength d
      <*> o .:? "worldExtentX"            .!= cpWorldExtentX d
      <*> o .:? "worldExtentY"            .!= cpWorldExtentY d
      -- Ocean edge depth
      <*> o .:? "edgeDepthNorth"          .!= cpEdgeDepthNorth d
      <*> o .:? "edgeDepthSouth"          .!= cpEdgeDepthSouth d
      <*> o .:? "edgeDepthEast"           .!= cpEdgeDepthEast d
      <*> o .:? "edgeDepthWest"           .!= cpEdgeDepthWest d
      <*> o .:? "edgeDepthFalloff"        .!= cpEdgeDepthFalloff d
      -- Tectonics
      <*> o .:? "plateSize"               .!= cpPlateSize d
      <*> o .:? "plateSpeed"              .!= cpPlateSpeed d
      <*> o .:? "boundarySharpness"       .!= cpBoundarySharpness d
      <*> o .:? "boundaryNoiseScale"      .!= cpBoundaryNoiseScale d
      <*> o .:? "boundaryNoiseStrength"   .!= cpBoundaryNoiseStrength d
      <*> o .:? "boundaryWarpOctaves"     .!= cpBoundaryWarpOctaves d
      <*> o .:? "boundaryWarpLacunarity"  .!= cpBoundaryWarpLacunarity d
      <*> o .:? "boundaryWarpGain"        .!= cpBoundaryWarpGain d
      <*> o .:? "plateMergeScale"         .!= cpPlateMergeScale d
      <*> o .:? "plateMergeBias"          .!= cpPlateMergeBias d
      <*> o .:? "plateDetailScale"        .!= cpPlateDetailScale d
      <*> o .:? "plateDetailStrength"     .!= cpPlateDetailStrength d
      <*> o .:? "plateRidgeStrength"      .!= cpPlateRidgeStrength d
      <*> o .:? "plateHeightBase"         .!= cpPlateHeightBase d
      <*> o .:? "plateHeightVariance"     .!= cpPlateHeightVariance d
      <*> o .:? "plateHardnessBase"       .!= cpPlateHardnessBase d
      <*> o .:? "plateHardnessVariance"   .!= cpPlateHardnessVariance d
      <*> o .:? "uplift"                  .!= cpUplift d
      <*> o .:? "riftDepth"               .!= cpRiftDepth d
      <*> o .:? "trenchDepth"             .!= cpTrenchDepth d
      <*> o .:? "ridgeHeight"             .!= cpRidgeHeight d
      <*> o .:? "detailScale"             .!= cpDetailScale d
      <*> o .:? "plateBiasStrength"       .!= cpPlateBiasStrength d
      <*> o .:? "plateBiasCenter"         .!= cpPlateBiasCenter d
      <*> o .:? "plateBiasEdge"           .!= cpPlateBiasEdge d
      <*> o .:? "plateBiasNorth"          .!= cpPlateBiasNorth d
      <*> o .:? "plateBiasSouth"          .!= cpPlateBiasSouth d
      <*> o .:? "tfcCliffSlope"            .!= cpTfcCliffSlope d
      <*> o .:? "tfcMountainSlope"         .!= cpTfcMountainSlope d
      <*> o .:? "tfcMountainRelief"        .!= cpTfcMountainRelief d
      <*> o .:? "tfcHillSlope"             .!= cpTfcHillSlope d
      <*> o .:? "tfcRollingSlope"          .!= cpTfcRollingSlope d
      <*> o .:? "valleyCurvature"          .!= cpValleyCurvature d
      <*> o .:? "rockElevationThreshold"   .!= cpRockElevationThreshold d
      <*> o .:? "rockHardnessThreshold"    .!= cpRockHardnessThreshold d
      <*> o .:? "rockHardnessSecondary"    .!= cpRockHardnessSecondary d
      -- Weather
      <*> o .:? "weatherTick"             .!= cpWeatherTick d
      <*> o .:? "weatherPhase"            .!= cpWeatherPhase d
      <*> o .:? "weatherAmplitude"        .!= cpWeatherAmplitude d
      <*> o .:? "seasonCycleLength"        .!= cpSeasonCycleLength d
      <*> o .:? "jitterAmplitude"          .!= cpJitterAmplitude d
      <*> o .:? "pressureBase"             .!= cpPressureBase d
      <*> o .:? "pressureTempScale"        .!= cpPressureTempScale d
      <*> o .:? "pressureCoriolisScale"    .!= cpPressureCoriolisScale d
      <*> o .:? "seasonalBase"             .!= cpSeasonalBase d
      <*> o .:? "seasonalRange"            .!= cpSeasonalRange d
      <*> o .:? "humidityNoiseScale"       .!= cpHumidityNoiseScale d
      <*> o .:? "precipNoiseScale"         .!= cpPrecipNoiseScale d
      <*> o .:? "weatherITCZWidth"         .!= cpWeatherITCZWidth d
      <*> o .:? "weatherITCZPrecipBoost"   .!= cpWeatherITCZPrecipBoost d
      <*> o .:? "pressureHumidityScale"    .!= cpPressureHumidityScale d
      <*> o .:? "pressureGradientWindScale" .!= cpPressureGradientWindScale d
      <*> o .:? "windNoiseScale"           .!= cpWindNoiseScale d
      <*> o .:? "itczMigrationScale"       .!= cpITCZMigrationScale d
      <*> o .:? "cloudRHExponent"          .!= cpCloudRHExponent d
      <*> o .:? "cloudAlbedoEffect"        .!= cpCloudAlbedoEffect d
      <*> o .:? "cloudPrecipBoost"         .!= cpCloudPrecipBoost d
      -- Vegetation
      <*> o .:? "vegBase"                 .!= cpVegBase d
      <*> o .:? "vegBoost"                .!= cpVegBoost d
      <*> o .:? "vegTempWeight"           .!= cpVegTempWeight d
      <*> o .:? "vegPrecipWeight"         .!= cpVegPrecipWeight d
      -- Biome Thresholds
      <*> o .:? "btCoastalBand"           .!= cpBtCoastalBand d
      <*> o .:? "btSnowElevation"         .!= cpBtSnowElevation d
      <*> o .:? "btAlpineElevation"       .!= cpBtAlpineElevation d
      <*> o .:? "btIceCapTemp"            .!= cpBtIceCapTemp d
      <*> o .:? "btMontaneLow"            .!= cpBtMontaneLow d
      <*> o .:? "btMontanePrecip"         .!= cpBtMontanePrecip d
      <*> o .:? "btCliffSlope"            .!= cpBtCliffSlope d
      <*> o .:? "btValleyMoisture"        .!= cpBtValleyMoisture d
      <*> o .:? "btDepressionMoisture"    .!= cpBtDepressionMoisture d
      <*> o .:? "btPrecipWeight"          .!= cpBtPrecipWeight d
      -- Vegetation Bootstrap
      <*> o .:? "vbcTempMin"              .!= cpVbcTempMin d
      <*> o .:? "vbcTempRange"            .!= cpVbcTempRange d
      <*> o .:? "vbcFertilityBoost"       .!= cpVbcFertilityBoost d
      <*> o .:? "vbcAlbedoBase"           .!= cpVbcAlbedoBase d
      <*> o .:? "vbcAlbedoBare"           .!= cpVbcAlbedoBare d
      <*> o .:? "vbcAlbedoVeg"            .!= cpVbcAlbedoVeg d
      <*> o .:? "vbcOceanAlbedo"          .!= cpVbcOceanAlbedo d
      <*> o .:? "vbcIceAlbedo"            .!= cpVbcIceAlbedo d
      -- Biome Misc
      <*> o .:? "biomeSmoothing"          .!= cpBiomeSmoothing d
      <*> o .:? "volcanicAshBoost"        .!= cpVolcanicAshBoost d
      <*> o .:? "volcanicLavaPenalty"     .!= cpVolcanicLavaPenalty d
      <*> o .:? "biomeFeedbackBlend"      .!= cpBiomeFeedbackBlend d
      -- Planet
      <*> o .:? "planetRadius"            .!= cpPlanetRadius d
      <*> o .:? "axialTilt"               .!= cpAxialTilt d
      <*> o .:? "insolation"              .!= cpInsolation d
      -- Ocean currents
      <*> o .:? "occWarmScale"            .!= cpOccWarmScale d
      <*> o .:? "occColdScale"            .!= cpOccColdScale d
      <*> o .:? "occLatPeakDeg"           .!= cpOccLatPeakDeg d
      <*> o .:? "occLatWidthDeg"          .!= cpOccLatWidthDeg d
      -- World slice
      <*> o .:? "sliceLatCenter"          .!= cpSliceLatCenter d
      <*> o .:? "sliceLonCenter"          .!= cpSliceLonCenter d

-- ---------------------------------------------------------------------------
-- Defaults
-- ---------------------------------------------------------------------------

-- | Default preset with hardcoded values matching @emptyUiState@.
-- Kept in this cycle-free module so 'Actor.UI' can import the type
-- without pulling in the UI setter functions.
defaultPreset :: ConfigPreset
defaultPreset = ConfigPreset
  { cpName                    = "default"
  , cpVersion                 = currentPresetVersion
  , cpSeed                    = 0
  , cpChunkSize               = 64
  , cpWaterLevel              = 0.5
  , cpRenderWaterLevel        = 0.5
  , cpEvaporation             = 0.25
  , cpRainShadow              = 0.4
  , cpWindDiffuse             = 0.5
  , cpEquatorTemp             = 1
  , cpPoleTemp                = 0
  , cpLapseRate               = 0.25
  , cpWindIterations          = 0.5
  , cpMoistureIterations      = 0.5
  , cpBoundaryMotionTemp      = 0.5
  , cpBoundaryMotionPrecip    = 0.5
  , cpLatitudeExponent        = 0.31
  , cpPlateHeightCooling      = 0.25
  , cpTempNoiseScale          = 0.33
  , cpOceanModeration         = 0.3
  , cpOceanModerateTemp       = 0.55
  , cpAlbedoSensitivity       = 0.4
  , cpAlbedoReference         = 0.6
  , cpMoistAdvect             = 0.85
  , cpMoistLocal              = 0.15
  , cpMoistWindEvapScale      = 0.3
  , cpMoistEvapNoiseScale     = 0.25
  , cpMoistLandETCoeff        = 0.65
  , cpMoistBareEvapFrac       = 0.15
  , cpMoistVegTranspFrac      = 0.85
  , cpMoistWindETScale        = 0.2
  , cpMoistCondensationRate   = 0.4
  , cpMoistRecycleRate        = 0.35
  , cpMoistITCZStrength       = 0.3
  , cpMoistITCZWidth          = 0.333
  , cpOrographicScale         = 0.3
  , cpOrographicStep          = 0.2
  , cpCoastalIterations       = 0.375
  , cpCoastalDiffuse          = 0.5
  , cpCoastalMoistureBoost    = 0.2
  , cpWindBeltStrength        = 0.6
  , cpWindBeltHarmonics       = 0.4
  , cpWindBeltBase            = 0.4
  , cpWindBeltRange           = 0.6
  , cpWindBeltSpeedScale      = 0.6
  , cpBndLandRange            = 0.357
  , cpBndTempConvergent       = 0.467
  , cpBndTempDivergent        = 0.4
  , cpBndTempTransform        = 0.45
  , cpBndPrecipConvergent     = 0.6
  , cpBndPrecipDivergent      = 0.5
  , cpBndPrecipTransform      = 0.6
  , cpRainRate                = 0.2
  , cpErosionHydraulic        = 0.5
  , cpErosionThermal          = 0.4
  , cpErosionTalus            = 0.5
  , cpErosionMaxDrop          = 0.5
  , cpGlacierSnowTemp         = 0.5
  , cpGlacierSnowRange        = 0.417
  , cpGlacierMeltTemp         = 0.429
  , cpGlacierMeltRate         = 0.2
  , cpGlacierAccumScale       = 0.333
  , cpGlacierFlowIters        = 0.3
  , cpGlacierFlowRate         = 0.2
  , cpGlacierErosionScale     = 0.25
  , cpGlacierCarveScale       = 0.1
  , cpGlacierDepositScale     = 0.2
  , cpVentDensity             = 0.25
  , cpVentThreshold           = 0.5
  , cpHotspotScale            = 0.5
  , cpHotspotThreshold        = 0.615
  , cpMagmaRecharge           = 0.333
  , cpLavaScale               = 0.6
  , cpAshScale                = 0.4
  , cpVolcanicDepositScale    = 0.8
  , cpSoilMoistureThreshold   = 0.7
  , cpSoilHardnessThreshold   = 0.45
  , cpSoilFertilityMoistWeight = 0.6
  , cpSoilFertilityDepthWeight = 0.4
  , cpSinkBreachDepth         = 0.2
  , cpStreamPowerMaxErosion   = 0.25
  , cpRiverCarveMaxDepth      = 0.25
  , cpCoastalErodeStrength    = 0.2
  , cpHydroHardnessWeight     = 0.7
  , cpMinLakeSize             = 0.061
  , cpInlandSeaMinSize        = 0.333
  , cpRoughnessScale          = 0.375
  , cpGenScale                = 0.4444
  , cpGenCoordScale           = 0.3333
  , cpGenOffsetX              = 0.5
  , cpGenOffsetY              = 0.5
  , cpGenFrequency            = 0.1837
  , cpGenOctaves              = 0.5
  , cpGenLacunarity           = 0.25
  , cpGenGain                 = 0.4
  , cpGenWarpScale            = 0.3333
  , cpGenWarpStrength         = 0.5556
  , cpWorldExtentX            = 0.125
  , cpWorldExtentY            = 0.125
  , cpEdgeDepthNorth          = 0
  , cpEdgeDepthSouth          = 0
  , cpEdgeDepthEast           = 0
  , cpEdgeDepthWest           = 0
  , cpEdgeDepthFalloff        = 0
  , cpPlateSize               = 0.45
  , cpPlateSpeed              = 0.38
  , cpBoundarySharpness       = 0.35
  , cpBoundaryNoiseScale      = 0.33
  , cpBoundaryNoiseStrength   = 0.45
  , cpBoundaryWarpOctaves     = 0.5
  , cpBoundaryWarpLacunarity  = 0.25
  , cpBoundaryWarpGain        = 0.4
  , cpPlateMergeScale         = 0.3
  , cpPlateMergeBias          = 0.44
  , cpPlateDetailScale        = 0.33
  , cpPlateDetailStrength     = 0.35
  , cpPlateRidgeStrength      = 0.25
  , cpPlateHeightBase         = 0.62
  , cpPlateHeightVariance     = 0.65
  , cpPlateHardnessBase       = 0.42
  , cpPlateHardnessVariance   = 0.4
  , cpUplift                  = 0.3
  , cpRiftDepth               = 0.35
  , cpTrenchDepth             = 0.38
  , cpRidgeHeight             = 0.33
  , cpDetailScale             = 0.5
  , cpPlateBiasStrength       = 0.42
  , cpPlateBiasCenter         = 0.5
  , cpPlateBiasEdge           = 0.5
  , cpPlateBiasNorth          = 0.5
  , cpPlateBiasSouth          = 0.5
  , cpTfcCliffSlope           = 0.4286
  , cpTfcMountainSlope        = 0.3333
  , cpTfcMountainRelief       = 0.4444
  , cpTfcHillSlope            = 0.3333
  , cpTfcRollingSlope         = 0.1579
  , cpValleyCurvature         = 0.2857
  , cpRockElevationThreshold  = 0.5714
  , cpRockHardnessThreshold   = 0.5714
  , cpRockHardnessSecondary   = 0.5
  , cpWeatherTick             = 0.2
  , cpWeatherPhase            = 0
  , cpWeatherAmplitude        = 0.3
  , cpSeasonCycleLength        = 0.4786
  , cpJitterAmplitude          = 0.36
  , cpPressureBase             = 0.5714
  , cpPressureTempScale        = 0.4
  , cpPressureCoriolisScale    = 0.2
  , cpSeasonalBase             = 0.4
  , cpSeasonalRange            = 0.6
  , cpHumidityNoiseScale       = 0.3333
  , cpPrecipNoiseScale         = 0.3
  , cpWeatherITCZWidth         = 0.4444
  , cpWeatherITCZPrecipBoost   = 0.3
  , cpPressureHumidityScale    = 0.2
  , cpPressureGradientWindScale = 0.3
  , cpWindNoiseScale           = 0.3333
  , cpITCZMigrationScale       = 0.4667
  , cpCloudRHExponent          = 0.4
  , cpCloudAlbedoEffect        = 0.2667
  , cpCloudPrecipBoost         = 0.24
  , cpVegBase                 = 0.2
  , cpVegBoost                = 0.6
  , cpVegTempWeight           = 0.6
  , cpVegPrecipWeight         = 0.4
  , cpBtCoastalBand           = 0.3
  , cpBtSnowElevation         = 0.8
  , cpBtAlpineElevation       = 0.7
  , cpBtIceCapTemp            = 0.25
  , cpBtMontaneLow            = 0.44
  , cpBtMontanePrecip         = 0.5
  , cpBtCliffSlope            = 0.3333
  , cpBtValleyMoisture        = 0.5714
  , cpBtDepressionMoisture    = 0.5
  , cpBtPrecipWeight          = 0.3333
  , cpVbcTempMin              = 0.2667
  , cpVbcTempRange            = 0.4444
  , cpVbcFertilityBoost       = 0.5
  , cpVbcAlbedoBase           = 0.5
  , cpVbcAlbedoBare           = 0.375
  , cpVbcAlbedoVeg            = 0.3333
  , cpVbcOceanAlbedo          = 0.3
  , cpVbcIceAlbedo            = 0.7143
  , cpBiomeSmoothing          = 0.2
  , cpVolcanicAshBoost        = 0.4
  , cpVolcanicLavaPenalty     = 0.4375
  , cpBiomeFeedbackBlend      = 0.85
  , cpPlanetRadius            = 0.3333
  , cpAxialTilt               = 0.5209
  , cpInsolation              = 0.5
  , cpOccWarmScale            = 0.3
  , cpOccColdScale            = 0.2
  , cpOccLatPeakDeg           = 0.5833
  , cpOccLatWidthDeg          = 0.5
  , cpSliceLatCenter          = 0.5
  , cpSliceLonCenter          = 0.5
  }
