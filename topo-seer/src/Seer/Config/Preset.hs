{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Config preset persistence for topo-seer.
--
-- A 'ConfigPreset' captures all slider values from the UI so they can be
-- saved to disk as JSON and restored later.  The JSON format uses optional
-- fields with defaults from 'defaultPreset', making it forward-compatible:
-- presets created before new sliders were added will load with sensible
-- defaults for the new fields.
--
-- Presets are stored as individual @.json@ files under @~\/.topo\/configs\/@.
module Seer.Config.Preset
  ( -- * Re-exports from Types (cycle-free)
    ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
    -- * Conversion
  , presetFromUi
  , applyPresetToUi
    -- * File I/O
  , presetDir
  , savePreset
  , loadPreset
  , listPresets
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.List (sort)
import Data.Text (Text)
import System.Directory
  ( createDirectoryIfMissing
  , getHomeDirectory
  , listDirectory
  , renameFile
  , removeFile
  , doesFileExist
  )
import System.FilePath ((</>), takeExtension, dropExtension)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import Seer.Config.Preset.Types
  ( ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
  )

import Actor.UI
  ( Ui
  , UiState(..)
  , setUiSeed
  , setUiChunkSize
  , setUiWaterLevel
  , setUiRenderWaterLevel
  , setUiEvaporation
  , setUiRainShadow
  , setUiWindDiffuse
  , setUiRainRate
  , setUiErosionHydraulic
  , setUiErosionThermal
  , setUiErosionTalus
  , setUiErosionMaxDrop
  , setUiGlacierSnowTemp
  , setUiGlacierSnowRange
  , setUiGlacierMeltTemp
  , setUiGlacierMeltRate
  , setUiGlacierAccumScale
  , setUiGlacierFlowIters
  , setUiGlacierFlowRate
  , setUiGlacierErosionScale
  , setUiGlacierCarveScale
  , setUiGlacierDepositScale
  , setUiVentDensity
  , setUiVentThreshold
  , setUiHotspotScale
  , setUiHotspotThreshold
  , setUiMagmaRecharge
  , setUiLavaScale
  , setUiAshScale
  , setUiVolcanicDepositScale
  , setUiSoilMoistureThreshold
  , setUiSoilHardnessThreshold
  , setUiSoilFertilityMoistWeight
  , setUiSoilFertilityDepthWeight
  , setUiSinkBreachDepth
  , setUiStreamPowerMaxErosion
  , setUiRiverCarveMaxDepth
  , setUiCoastalErodeStrength
  , setUiHydroHardnessWeight
  , setUiMinLakeSize
  , setUiInlandSeaMinSize
  , setUiRoughnessScale
  , setUiEquatorTemp
  , setUiPoleTemp
  , setUiLapseRate
  , setUiGenScale
  , setUiGenCoordScale
  , setUiGenOffsetX
  , setUiGenOffsetY
  , setUiGenFrequency
  , setUiGenOctaves
  , setUiGenLacunarity
  , setUiGenGain
  , setUiGenWarpScale
  , setUiGenWarpStrength
  , setUiWorldExtentX
  , setUiWorldExtentY
  , setUiEdgeDepthNorth
  , setUiEdgeDepthSouth
  , setUiEdgeDepthEast
  , setUiEdgeDepthWest
  , setUiEdgeDepthFalloff
  , setUiPlateSize
  , setUiPlateSpeed
  , setUiBoundarySharpness
  , setUiBoundaryNoiseScale
  , setUiBoundaryNoiseStrength
  , setUiBoundaryWarpOctaves
  , setUiBoundaryWarpLacunarity
  , setUiBoundaryWarpGain
  , setUiPlateMergeScale
  , setUiPlateMergeBias
  , setUiPlateDetailScale
  , setUiPlateDetailStrength
  , setUiPlateRidgeStrength
  , setUiPlateHeightBase
  , setUiPlateHeightVariance
  , setUiPlateHardnessBase
  , setUiPlateHardnessVariance
  , setUiUplift
  , setUiRiftDepth
  , setUiTrenchDepth
  , setUiRidgeHeight
  , setUiDetailScale
  , setUiPlateBiasStrength
  , setUiPlateBiasCenter
  , setUiPlateBiasEdge
  , setUiPlateBiasNorth
  , setUiPlateBiasSouth
  , setUiTfcCliffSlope
  , setUiTfcMountainSlope
  , setUiTfcMountainRelief
  , setUiTfcHillSlope
  , setUiTfcRollingSlope
  , setUiValleyCurvature
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiWindIterations
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiLatitudeExponent
  , setUiPlateHeightCooling
  , setUiTempNoiseScale
  , setUiOceanModeration
  , setUiOceanModerateTemp
  , setUiAlbedoSensitivity
  , setUiAlbedoReference
  , setUiMoistAdvect
  , setUiMoistLocal
  , setUiMoistWindEvapScale
  , setUiMoistEvapNoiseScale
  , setUiMoistLandETCoeff
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiOrographicScale
  , setUiOrographicStep
  , setUiCoastalIterations
  , setUiCoastalDiffuse
  , setUiCoastalMoistureBoost
  , setUiWindBeltStrength
  , setUiWindBeltHarmonics
  , setUiWindBeltBase
  , setUiWindBeltRange
  , setUiWindBeltSpeedScale
  , setUiBndLandRange
  , setUiBndTempConvergent
  , setUiBndTempDivergent
  , setUiBndTempTransform
  , setUiBndPrecipConvergent
  , setUiBndPrecipDivergent
  , setUiBndPrecipTransform
  , setUiWeatherTick
  , setUiWeatherPhase
  , setUiWeatherAmplitude
  , setUiSeasonCycleLength
  , setUiJitterAmplitude
  , setUiPressureBase
  , setUiPressureTempScale
  , setUiPressureCoriolisScale
  , setUiSeasonalBase
  , setUiSeasonalRange
  , setUiHumidityNoiseScale
  , setUiPrecipNoiseScale
  , setUiWeatherITCZWidth
  , setUiWeatherITCZPrecipBoost
  , setUiPressureHumidityScale
  , setUiPressureGradientWindScale
  , setUiWindNoiseScale
  , setUiITCZMigrationScale
  , setUiCloudRHExponent
  , setUiCloudAlbedoEffect
  , setUiCloudPrecipBoost
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiBtCoastalBand
  , setUiBtSnowElevation
  , setUiBtAlpineElevation
  , setUiBtIceCapTemp
  , setUiBtMontaneLow
  , setUiBtMontanePrecip
  , setUiBtCliffSlope
  , setUiBtValleyMoisture
  , setUiBtDepressionMoisture
  , setUiBtPrecipWeight
  , setUiVbcTempMin
  , setUiVbcTempRange
  , setUiVbcFertilityBoost
  , setUiVbcAlbedoBase
  , setUiVbcAlbedoBare
  , setUiVbcAlbedoVeg
  , setUiVbcOceanAlbedo
  , setUiVbcIceAlbedo
  , setUiBiomeSmoothing
  , setUiVolcanicAshBoost
  , setUiVolcanicLavaPenalty
  , setUiBiomeFeedbackBlend
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiOccWarmScale
  , setUiOccColdScale
  , setUiOccLatPeakDeg
  , setUiOccLatWidthDeg
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  )

import Hyperspace.Actor (ActorHandle, Protocol)

-- ---------------------------------------------------------------------------
-- Conversion
-- ---------------------------------------------------------------------------

-- | Capture all slider values from a 'UiState' into a 'ConfigPreset'
-- with the given name.
presetFromUi :: UiState -> Text -> ConfigPreset
presetFromUi ui name = ConfigPreset
  { cpName                    = name
  , cpVersion                 = currentPresetVersion
  , cpSeed                    = uiSeed ui
  , cpChunkSize               = uiChunkSize ui
  , cpWaterLevel              = uiWaterLevel ui
  , cpRenderWaterLevel        = uiRenderWaterLevel ui
  , cpEvaporation             = uiEvaporation ui
  , cpRainShadow              = uiRainShadow ui
  , cpWindDiffuse             = uiWindDiffuse ui
  , cpEquatorTemp             = uiEquatorTemp ui
  , cpPoleTemp                = uiPoleTemp ui
  , cpLapseRate               = uiLapseRate ui
  , cpWindIterations          = uiWindIterations ui
  , cpMoistureIterations      = uiMoistureIterations ui
  , cpBoundaryMotionTemp      = uiBoundaryMotionTemp ui
  , cpBoundaryMotionPrecip    = uiBoundaryMotionPrecip ui
  , cpLatitudeExponent        = uiLatitudeExponent ui
  , cpPlateHeightCooling      = uiPlateHeightCooling ui
  , cpTempNoiseScale          = uiTempNoiseScale ui
  , cpOceanModeration         = uiOceanModeration ui
  , cpOceanModerateTemp       = uiOceanModerateTemp ui
  , cpAlbedoSensitivity       = uiAlbedoSensitivity ui
  , cpAlbedoReference         = uiAlbedoReference ui
  , cpMoistAdvect             = uiMoistAdvect ui
  , cpMoistLocal              = uiMoistLocal ui
  , cpMoistWindEvapScale      = uiMoistWindEvapScale ui
  , cpMoistEvapNoiseScale     = uiMoistEvapNoiseScale ui
  , cpMoistLandETCoeff        = uiMoistLandETCoeff ui
  , cpMoistBareEvapFrac       = uiMoistBareEvapFrac ui
  , cpMoistVegTranspFrac      = uiMoistVegTranspFrac ui
  , cpMoistWindETScale        = uiMoistWindETScale ui
  , cpMoistCondensationRate   = uiMoistCondensationRate ui
  , cpMoistRecycleRate        = uiMoistRecycleRate ui
  , cpMoistITCZStrength       = uiMoistITCZStrength ui
  , cpMoistITCZWidth          = uiMoistITCZWidth ui
  , cpOrographicScale         = uiOrographicScale ui
  , cpOrographicStep          = uiOrographicStep ui
  , cpCoastalIterations       = uiCoastalIterations ui
  , cpCoastalDiffuse          = uiCoastalDiffuse ui
  , cpCoastalMoistureBoost    = uiCoastalMoistureBoost ui
  , cpWindBeltStrength        = uiWindBeltStrength ui
  , cpWindBeltHarmonics       = uiWindBeltHarmonics ui
  , cpWindBeltBase            = uiWindBeltBase ui
  , cpWindBeltRange           = uiWindBeltRange ui
  , cpWindBeltSpeedScale      = uiWindBeltSpeedScale ui
  , cpBndLandRange            = uiBndLandRange ui
  , cpBndTempConvergent       = uiBndTempConvergent ui
  , cpBndTempDivergent        = uiBndTempDivergent ui
  , cpBndTempTransform        = uiBndTempTransform ui
  , cpBndPrecipConvergent     = uiBndPrecipConvergent ui
  , cpBndPrecipDivergent      = uiBndPrecipDivergent ui
  , cpBndPrecipTransform      = uiBndPrecipTransform ui
  , cpRainRate                = uiRainRate ui
  , cpErosionHydraulic        = uiErosionHydraulic ui
  , cpErosionThermal          = uiErosionThermal ui
  , cpErosionTalus            = uiErosionTalus ui
  , cpErosionMaxDrop          = uiErosionMaxDrop ui
  , cpGlacierSnowTemp         = uiGlacierSnowTemp ui
  , cpGlacierSnowRange        = uiGlacierSnowRange ui
  , cpGlacierMeltTemp         = uiGlacierMeltTemp ui
  , cpGlacierMeltRate         = uiGlacierMeltRate ui
  , cpGlacierAccumScale       = uiGlacierAccumScale ui
  , cpGlacierFlowIters        = uiGlacierFlowIters ui
  , cpGlacierFlowRate         = uiGlacierFlowRate ui
  , cpGlacierErosionScale     = uiGlacierErosionScale ui
  , cpGlacierCarveScale       = uiGlacierCarveScale ui
  , cpGlacierDepositScale     = uiGlacierDepositScale ui
  , cpVentDensity             = uiVentDensity ui
  , cpVentThreshold           = uiVentThreshold ui
  , cpHotspotScale            = uiHotspotScale ui
  , cpHotspotThreshold        = uiHotspotThreshold ui
  , cpMagmaRecharge           = uiMagmaRecharge ui
  , cpLavaScale               = uiLavaScale ui
  , cpAshScale                = uiAshScale ui
  , cpVolcanicDepositScale    = uiVolcanicDepositScale ui
  , cpSoilMoistureThreshold   = uiSoilMoistureThreshold ui
  , cpSoilHardnessThreshold   = uiSoilHardnessThreshold ui
  , cpSoilFertilityMoistWeight = uiSoilFertilityMoistWeight ui
  , cpSoilFertilityDepthWeight = uiSoilFertilityDepthWeight ui
  , cpSinkBreachDepth         = uiSinkBreachDepth ui
  , cpStreamPowerMaxErosion   = uiStreamPowerMaxErosion ui
  , cpRiverCarveMaxDepth      = uiRiverCarveMaxDepth ui
  , cpCoastalErodeStrength    = uiCoastalErodeStrength ui
  , cpHydroHardnessWeight     = uiHydroHardnessWeight ui
  , cpMinLakeSize             = uiMinLakeSize ui
  , cpInlandSeaMinSize        = uiInlandSeaMinSize ui
  , cpRoughnessScale          = uiRoughnessScale ui
  , cpGenScale                = uiGenScale ui
  , cpGenCoordScale           = uiGenCoordScale ui
  , cpGenOffsetX              = uiGenOffsetX ui
  , cpGenOffsetY              = uiGenOffsetY ui
  , cpGenFrequency            = uiGenFrequency ui
  , cpGenOctaves              = uiGenOctaves ui
  , cpGenLacunarity           = uiGenLacunarity ui
  , cpGenGain                 = uiGenGain ui
  , cpGenWarpScale            = uiGenWarpScale ui
  , cpGenWarpStrength         = uiGenWarpStrength ui
  , cpWorldExtentX            = uiWorldExtentX ui
  , cpWorldExtentY            = uiWorldExtentY ui
  , cpEdgeDepthNorth          = uiEdgeDepthNorth ui
  , cpEdgeDepthSouth          = uiEdgeDepthSouth ui
  , cpEdgeDepthEast           = uiEdgeDepthEast ui
  , cpEdgeDepthWest           = uiEdgeDepthWest ui
  , cpEdgeDepthFalloff        = uiEdgeDepthFalloff ui
  , cpPlateSize               = uiPlateSize ui
  , cpPlateSpeed              = uiPlateSpeed ui
  , cpBoundarySharpness       = uiBoundarySharpness ui
  , cpBoundaryNoiseScale      = uiBoundaryNoiseScale ui
  , cpBoundaryNoiseStrength   = uiBoundaryNoiseStrength ui
  , cpBoundaryWarpOctaves     = uiBoundaryWarpOctaves ui
  , cpBoundaryWarpLacunarity  = uiBoundaryWarpLacunarity ui
  , cpBoundaryWarpGain        = uiBoundaryWarpGain ui
  , cpPlateMergeScale         = uiPlateMergeScale ui
  , cpPlateMergeBias          = uiPlateMergeBias ui
  , cpPlateDetailScale        = uiPlateDetailScale ui
  , cpPlateDetailStrength     = uiPlateDetailStrength ui
  , cpPlateRidgeStrength      = uiPlateRidgeStrength ui
  , cpPlateHeightBase         = uiPlateHeightBase ui
  , cpPlateHeightVariance     = uiPlateHeightVariance ui
  , cpPlateHardnessBase       = uiPlateHardnessBase ui
  , cpPlateHardnessVariance   = uiPlateHardnessVariance ui
  , cpUplift                  = uiUplift ui
  , cpRiftDepth               = uiRiftDepth ui
  , cpTrenchDepth             = uiTrenchDepth ui
  , cpRidgeHeight             = uiRidgeHeight ui
  , cpDetailScale             = uiDetailScale ui
  , cpPlateBiasStrength       = uiPlateBiasStrength ui
  , cpPlateBiasCenter         = uiPlateBiasCenter ui
  , cpPlateBiasEdge           = uiPlateBiasEdge ui
  , cpPlateBiasNorth          = uiPlateBiasNorth ui
  , cpPlateBiasSouth          = uiPlateBiasSouth ui
  , cpTfcCliffSlope           = uiTfcCliffSlope ui
  , cpTfcMountainSlope        = uiTfcMountainSlope ui
  , cpTfcMountainRelief       = uiTfcMountainRelief ui
  , cpTfcHillSlope            = uiTfcHillSlope ui
  , cpTfcRollingSlope         = uiTfcRollingSlope ui
  , cpValleyCurvature         = uiValleyCurvature ui
  , cpRockElevationThreshold  = uiRockElevationThreshold ui
  , cpRockHardnessThreshold   = uiRockHardnessThreshold ui
  , cpRockHardnessSecondary   = uiRockHardnessSecondary ui
  , cpWeatherTick             = uiWeatherTick ui
  , cpWeatherPhase            = uiWeatherPhase ui
  , cpWeatherAmplitude        = uiWeatherAmplitude ui
  , cpSeasonCycleLength        = uiSeasonCycleLength ui
  , cpJitterAmplitude          = uiJitterAmplitude ui
  , cpPressureBase             = uiPressureBase ui
  , cpPressureTempScale        = uiPressureTempScale ui
  , cpPressureCoriolisScale    = uiPressureCoriolisScale ui
  , cpSeasonalBase             = uiSeasonalBase ui
  , cpSeasonalRange            = uiSeasonalRange ui
  , cpHumidityNoiseScale       = uiHumidityNoiseScale ui
  , cpPrecipNoiseScale         = uiPrecipNoiseScale ui
  , cpWeatherITCZWidth         = uiWeatherITCZWidth ui
  , cpWeatherITCZPrecipBoost   = uiWeatherITCZPrecipBoost ui
  , cpPressureHumidityScale    = uiPressureHumidityScale ui
  , cpPressureGradientWindScale = uiPressureGradientWindScale ui
  , cpWindNoiseScale           = uiWindNoiseScale ui
  , cpITCZMigrationScale       = uiITCZMigrationScale ui
  , cpCloudRHExponent          = uiCloudRHExponent ui
  , cpCloudAlbedoEffect        = uiCloudAlbedoEffect ui
  , cpCloudPrecipBoost         = uiCloudPrecipBoost ui
  , cpVegBase                 = uiVegBase ui
  , cpVegBoost                = uiVegBoost ui
  , cpVegTempWeight           = uiVegTempWeight ui
  , cpVegPrecipWeight         = uiVegPrecipWeight ui
  , cpBtCoastalBand           = uiBtCoastalBand ui
  , cpBtSnowElevation         = uiBtSnowElevation ui
  , cpBtAlpineElevation       = uiBtAlpineElevation ui
  , cpBtIceCapTemp            = uiBtIceCapTemp ui
  , cpBtMontaneLow            = uiBtMontaneLow ui
  , cpBtMontanePrecip         = uiBtMontanePrecip ui
  , cpBtCliffSlope            = uiBtCliffSlope ui
  , cpBtValleyMoisture        = uiBtValleyMoisture ui
  , cpBtDepressionMoisture    = uiBtDepressionMoisture ui
  , cpBtPrecipWeight          = uiBtPrecipWeight ui
  , cpVbcTempMin              = uiVbcTempMin ui
  , cpVbcTempRange            = uiVbcTempRange ui
  , cpVbcFertilityBoost       = uiVbcFertilityBoost ui
  , cpVbcAlbedoBase           = uiVbcAlbedoBase ui
  , cpVbcAlbedoBare           = uiVbcAlbedoBare ui
  , cpVbcAlbedoVeg            = uiVbcAlbedoVeg ui
  , cpVbcOceanAlbedo          = uiVbcOceanAlbedo ui
  , cpVbcIceAlbedo            = uiVbcIceAlbedo ui
  , cpBiomeSmoothing          = uiBiomeSmoothing ui
  , cpVolcanicAshBoost        = uiVolcanicAshBoost ui
  , cpVolcanicLavaPenalty     = uiVolcanicLavaPenalty ui
  , cpBiomeFeedbackBlend      = uiBiomeFeedbackBlend ui
  , cpPlanetRadius            = uiPlanetRadius ui
  , cpAxialTilt               = uiAxialTilt ui
  , cpInsolation              = uiInsolation ui
  , cpOccWarmScale            = uiOccWarmScale ui
  , cpOccColdScale            = uiOccColdScale ui
  , cpOccLatPeakDeg           = uiOccLatPeakDeg ui
  , cpOccLatWidthDeg          = uiOccLatWidthDeg ui
  , cpSliceLatCenter          = uiSliceLatCenter ui
  , cpSliceLonCenter          = uiSliceLonCenter ui
  }

-- | Restore all slider values from a 'ConfigPreset' by sending
-- individual setter messages to the UI actor.  Metadata fields
-- ('cpName', 'cpVersion') are not applied.
applyPresetToUi :: ConfigPreset -> ActorHandle Ui (Protocol Ui) -> IO ()
applyPresetToUi cp h = do
  setUiSeed h (cpSeed cp)
  setUiChunkSize h (cpChunkSize cp)
  -- Hydrology
  setUiWaterLevel h (cpWaterLevel cp)
  setUiRenderWaterLevel h (cpRenderWaterLevel cp)
  -- Climate
  setUiEvaporation h (cpEvaporation cp)
  setUiRainShadow h (cpRainShadow cp)
  setUiWindDiffuse h (cpWindDiffuse cp)
  setUiEquatorTemp h (cpEquatorTemp cp)
  setUiPoleTemp h (cpPoleTemp cp)
  setUiLapseRate h (cpLapseRate cp)
  setUiWindIterations h (cpWindIterations cp)
  setUiMoistureIterations h (cpMoistureIterations cp)
  setUiBoundaryMotionTemp h (cpBoundaryMotionTemp cp)
  setUiBoundaryMotionPrecip h (cpBoundaryMotionPrecip cp)
  -- Temperature (extended)
  setUiLatitudeExponent h (cpLatitudeExponent cp)
  setUiPlateHeightCooling h (cpPlateHeightCooling cp)
  setUiTempNoiseScale h (cpTempNoiseScale cp)
  setUiOceanModeration h (cpOceanModeration cp)
  setUiOceanModerateTemp h (cpOceanModerateTemp cp)
  setUiAlbedoSensitivity h (cpAlbedoSensitivity cp)
  setUiAlbedoReference h (cpAlbedoReference cp)
  -- Moisture
  setUiMoistAdvect h (cpMoistAdvect cp)
  setUiMoistLocal h (cpMoistLocal cp)
  setUiMoistWindEvapScale h (cpMoistWindEvapScale cp)
  setUiMoistEvapNoiseScale h (cpMoistEvapNoiseScale cp)
  setUiMoistLandETCoeff h (cpMoistLandETCoeff cp)
  setUiMoistBareEvapFrac h (cpMoistBareEvapFrac cp)
  setUiMoistVegTranspFrac h (cpMoistVegTranspFrac cp)
  setUiMoistWindETScale h (cpMoistWindETScale cp)
  setUiMoistCondensationRate h (cpMoistCondensationRate cp)
  setUiMoistRecycleRate h (cpMoistRecycleRate cp)
  setUiMoistITCZStrength h (cpMoistITCZStrength cp)
  setUiMoistITCZWidth h (cpMoistITCZWidth cp)
  -- Precipitation
  setUiOrographicScale h (cpOrographicScale cp)
  setUiOrographicStep h (cpOrographicStep cp)
  setUiCoastalIterations h (cpCoastalIterations cp)
  setUiCoastalDiffuse h (cpCoastalDiffuse cp)
  setUiCoastalMoistureBoost h (cpCoastalMoistureBoost cp)
  -- Wind
  setUiWindBeltStrength h (cpWindBeltStrength cp)
  setUiWindBeltHarmonics h (cpWindBeltHarmonics cp)
  setUiWindBeltBase h (cpWindBeltBase cp)
  setUiWindBeltRange h (cpWindBeltRange cp)
  setUiWindBeltSpeedScale h (cpWindBeltSpeedScale cp)
  -- Boundary model
  setUiBndLandRange h (cpBndLandRange cp)
  setUiBndTempConvergent h (cpBndTempConvergent cp)
  setUiBndTempDivergent h (cpBndTempDivergent cp)
  setUiBndTempTransform h (cpBndTempTransform cp)
  setUiBndPrecipConvergent h (cpBndPrecipConvergent cp)
  setUiBndPrecipDivergent h (cpBndPrecipDivergent cp)
  setUiBndPrecipTransform h (cpBndPrecipTransform cp)
  -- Erosion
  setUiRainRate h (cpRainRate cp)
  setUiErosionHydraulic h (cpErosionHydraulic cp)
  setUiErosionThermal h (cpErosionThermal cp)
  setUiErosionTalus h (cpErosionTalus cp)
  setUiErosionMaxDrop h (cpErosionMaxDrop cp)
  setUiGlacierSnowTemp h (cpGlacierSnowTemp cp)
  setUiGlacierSnowRange h (cpGlacierSnowRange cp)
  setUiGlacierMeltTemp h (cpGlacierMeltTemp cp)
  setUiGlacierMeltRate h (cpGlacierMeltRate cp)
  setUiGlacierAccumScale h (cpGlacierAccumScale cp)
  setUiGlacierFlowIters h (cpGlacierFlowIters cp)
  setUiGlacierFlowRate h (cpGlacierFlowRate cp)
  setUiGlacierErosionScale h (cpGlacierErosionScale cp)
  setUiGlacierCarveScale h (cpGlacierCarveScale cp)
  setUiGlacierDepositScale h (cpGlacierDepositScale cp)
  setUiVentDensity h (cpVentDensity cp)
  setUiVentThreshold h (cpVentThreshold cp)
  setUiHotspotScale h (cpHotspotScale cp)
  setUiHotspotThreshold h (cpHotspotThreshold cp)
  setUiMagmaRecharge h (cpMagmaRecharge cp)
  setUiLavaScale h (cpLavaScale cp)
  setUiAshScale h (cpAshScale cp)
  setUiVolcanicDepositScale h (cpVolcanicDepositScale cp)
  setUiSoilMoistureThreshold h (cpSoilMoistureThreshold cp)
  setUiSoilHardnessThreshold h (cpSoilHardnessThreshold cp)
  setUiSoilFertilityMoistWeight h (cpSoilFertilityMoistWeight cp)
  setUiSoilFertilityDepthWeight h (cpSoilFertilityDepthWeight cp)
  setUiSinkBreachDepth h (cpSinkBreachDepth cp)
  setUiStreamPowerMaxErosion h (cpStreamPowerMaxErosion cp)
  setUiRiverCarveMaxDepth h (cpRiverCarveMaxDepth cp)
  setUiCoastalErodeStrength h (cpCoastalErodeStrength cp)
  setUiHydroHardnessWeight h (cpHydroHardnessWeight cp)
  setUiMinLakeSize h (cpMinLakeSize cp)
  setUiInlandSeaMinSize h (cpInlandSeaMinSize cp)
  setUiRoughnessScale h (cpRoughnessScale cp)
  -- Generation
  setUiGenScale h (cpGenScale cp)
  setUiGenCoordScale h (cpGenCoordScale cp)
  setUiGenOffsetX h (cpGenOffsetX cp)
  setUiGenOffsetY h (cpGenOffsetY cp)
  setUiGenFrequency h (cpGenFrequency cp)
  setUiGenOctaves h (cpGenOctaves cp)
  setUiGenLacunarity h (cpGenLacunarity cp)
  setUiGenGain h (cpGenGain cp)
  setUiGenWarpScale h (cpGenWarpScale cp)
  setUiGenWarpStrength h (cpGenWarpStrength cp)
  setUiWorldExtentX h (cpWorldExtentX cp)
  setUiWorldExtentY h (cpWorldExtentY cp)
  -- Ocean edge depth
  setUiEdgeDepthNorth h (cpEdgeDepthNorth cp)
  setUiEdgeDepthSouth h (cpEdgeDepthSouth cp)
  setUiEdgeDepthEast h (cpEdgeDepthEast cp)
  setUiEdgeDepthWest h (cpEdgeDepthWest cp)
  setUiEdgeDepthFalloff h (cpEdgeDepthFalloff cp)
  -- Tectonics
  setUiPlateSize h (cpPlateSize cp)
  setUiPlateSpeed h (cpPlateSpeed cp)
  setUiBoundarySharpness h (cpBoundarySharpness cp)
  setUiBoundaryNoiseScale h (cpBoundaryNoiseScale cp)
  setUiBoundaryNoiseStrength h (cpBoundaryNoiseStrength cp)
  setUiBoundaryWarpOctaves h (cpBoundaryWarpOctaves cp)
  setUiBoundaryWarpLacunarity h (cpBoundaryWarpLacunarity cp)
  setUiBoundaryWarpGain h (cpBoundaryWarpGain cp)
  setUiPlateMergeScale h (cpPlateMergeScale cp)
  setUiPlateMergeBias h (cpPlateMergeBias cp)
  setUiPlateDetailScale h (cpPlateDetailScale cp)
  setUiPlateDetailStrength h (cpPlateDetailStrength cp)
  setUiPlateRidgeStrength h (cpPlateRidgeStrength cp)
  setUiPlateHeightBase h (cpPlateHeightBase cp)
  setUiPlateHeightVariance h (cpPlateHeightVariance cp)
  setUiPlateHardnessBase h (cpPlateHardnessBase cp)
  setUiPlateHardnessVariance h (cpPlateHardnessVariance cp)
  setUiUplift h (cpUplift cp)
  setUiRiftDepth h (cpRiftDepth cp)
  setUiTrenchDepth h (cpTrenchDepth cp)
  setUiRidgeHeight h (cpRidgeHeight cp)
  setUiDetailScale h (cpDetailScale cp)
  setUiPlateBiasStrength h (cpPlateBiasStrength cp)
  setUiPlateBiasCenter h (cpPlateBiasCenter cp)
  setUiPlateBiasEdge h (cpPlateBiasEdge cp)
  setUiPlateBiasNorth h (cpPlateBiasNorth cp)
  setUiPlateBiasSouth h (cpPlateBiasSouth cp)
  setUiTfcCliffSlope h (cpTfcCliffSlope cp)
  setUiTfcMountainSlope h (cpTfcMountainSlope cp)
  setUiTfcMountainRelief h (cpTfcMountainRelief cp)
  setUiTfcHillSlope h (cpTfcHillSlope cp)
  setUiTfcRollingSlope h (cpTfcRollingSlope cp)
  setUiValleyCurvature h (cpValleyCurvature cp)
  setUiRockElevationThreshold h (cpRockElevationThreshold cp)
  setUiRockHardnessThreshold h (cpRockHardnessThreshold cp)
  setUiRockHardnessSecondary h (cpRockHardnessSecondary cp)
  -- Weather
  setUiWeatherTick h (cpWeatherTick cp)
  setUiWeatherPhase h (cpWeatherPhase cp)
  setUiWeatherAmplitude h (cpWeatherAmplitude cp)
  setUiSeasonCycleLength h (cpSeasonCycleLength cp)
  setUiJitterAmplitude h (cpJitterAmplitude cp)
  setUiPressureBase h (cpPressureBase cp)
  setUiPressureTempScale h (cpPressureTempScale cp)
  setUiPressureCoriolisScale h (cpPressureCoriolisScale cp)
  setUiSeasonalBase h (cpSeasonalBase cp)
  setUiSeasonalRange h (cpSeasonalRange cp)
  setUiHumidityNoiseScale h (cpHumidityNoiseScale cp)
  setUiPrecipNoiseScale h (cpPrecipNoiseScale cp)
  setUiWeatherITCZWidth h (cpWeatherITCZWidth cp)
  setUiWeatherITCZPrecipBoost h (cpWeatherITCZPrecipBoost cp)
  setUiPressureHumidityScale h (cpPressureHumidityScale cp)
  setUiPressureGradientWindScale h (cpPressureGradientWindScale cp)
  setUiWindNoiseScale h (cpWindNoiseScale cp)
  setUiITCZMigrationScale h (cpITCZMigrationScale cp)
  setUiCloudRHExponent h (cpCloudRHExponent cp)
  setUiCloudAlbedoEffect h (cpCloudAlbedoEffect cp)
  setUiCloudPrecipBoost h (cpCloudPrecipBoost cp)
  -- Vegetation
  setUiVegBase h (cpVegBase cp)
  setUiVegBoost h (cpVegBoost cp)
  setUiVegTempWeight h (cpVegTempWeight cp)
  setUiVegPrecipWeight h (cpVegPrecipWeight cp)
  -- Biome Thresholds
  setUiBtCoastalBand h (cpBtCoastalBand cp)
  setUiBtSnowElevation h (cpBtSnowElevation cp)
  setUiBtAlpineElevation h (cpBtAlpineElevation cp)
  setUiBtIceCapTemp h (cpBtIceCapTemp cp)
  setUiBtMontaneLow h (cpBtMontaneLow cp)
  setUiBtMontanePrecip h (cpBtMontanePrecip cp)
  setUiBtCliffSlope h (cpBtCliffSlope cp)
  setUiBtValleyMoisture h (cpBtValleyMoisture cp)
  setUiBtDepressionMoisture h (cpBtDepressionMoisture cp)
  setUiBtPrecipWeight h (cpBtPrecipWeight cp)
  -- Vegetation Bootstrap
  setUiVbcTempMin h (cpVbcTempMin cp)
  setUiVbcTempRange h (cpVbcTempRange cp)
  setUiVbcFertilityBoost h (cpVbcFertilityBoost cp)
  setUiVbcAlbedoBase h (cpVbcAlbedoBase cp)
  setUiVbcAlbedoBare h (cpVbcAlbedoBare cp)
  setUiVbcAlbedoVeg h (cpVbcAlbedoVeg cp)
  setUiVbcOceanAlbedo h (cpVbcOceanAlbedo cp)
  setUiVbcIceAlbedo h (cpVbcIceAlbedo cp)
  -- Biome Misc
  setUiBiomeSmoothing h (cpBiomeSmoothing cp)
  setUiVolcanicAshBoost h (cpVolcanicAshBoost cp)
  setUiVolcanicLavaPenalty h (cpVolcanicLavaPenalty cp)
  setUiBiomeFeedbackBlend h (cpBiomeFeedbackBlend cp)
  -- Planet
  setUiPlanetRadius h (cpPlanetRadius cp)
  setUiAxialTilt h (cpAxialTilt cp)
  setUiInsolation h (cpInsolation cp)
  -- Ocean currents
  setUiOccWarmScale h (cpOccWarmScale cp)
  setUiOccColdScale h (cpOccColdScale cp)
  setUiOccLatPeakDeg h (cpOccLatPeakDeg cp)
  setUiOccLatWidthDeg h (cpOccLatWidthDeg cp)
  -- World slice
  setUiSliceLatCenter h (cpSliceLatCenter cp)
  setUiSliceLonCenter h (cpSliceLonCenter cp)

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Return the preset directory (@~\/.topo\/configs\/@), creating it if
-- it does not exist.
presetDir :: IO FilePath
presetDir = do
  home <- getHomeDirectory
  let dir = home </> ".topo" </> "configs"
  createDirectoryIfMissing True dir
  pure dir

-- | Write a 'ConfigPreset' to a JSON file atomically (write to temp
-- file, then rename).  Returns @Left@ on IO error.
savePreset :: FilePath -> ConfigPreset -> IO (Either Text ())
savePreset path cp = do
  result <- try @IOException $ do
    let tmpPath = path <> ".tmp"
    BSL.writeFile tmpPath (encode cp)
    -- On Windows renameFile fails if the target exists; remove first.
    targetExists <- doesFileExist path
    if targetExists
      then removeFile path >> renameFile tmpPath path
      else renameFile tmpPath path
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-- | Read and decode a 'ConfigPreset' from a JSON file.
-- Returns @Left@ on IO or parse error.
loadPreset :: FilePath -> IO (Either Text ConfigPreset)
loadPreset path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right bs -> case eitherDecodeStrict' bs of
      Left parseErr -> Left (Text.pack parseErr)
      Right cp      -> Right cp

-- | List all preset names in the preset directory, sorted
-- alphabetically.  Names are returned without the @.json@ extension.
listPresets :: IO [Text]
listPresets = do
  dir <- presetDir
  entries <- listDirectory dir
  let names = sort
        [ Text.pack (dropExtension f)
        | f <- entries
        , takeExtension f == ".json"
        ]
  pure names
