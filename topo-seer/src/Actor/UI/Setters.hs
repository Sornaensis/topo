{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Thin wrappers for sending 'UiUpdate' mutations to the UI actor.
module Actor.UI.Setters
  ( setUiSeed
  , setUiGenerating
  , setUiViewMode
  , setUiChunkSize
  , setUiShowConfig
  , setUiConfigScroll
  , setUiShowLeftPanel
  , setUiWaterLevel
  , setUiRenderWaterLevel
  , setUiOrographicLift
  , setUiRainShadowLoss
  , setUiWindDiffuse
  , setUiRainRate
  , setUiErosionHydraulic
  , setUiErosionThermal
  , setUiErosionTalus
  , setUiErosionMaxDrop
  , setUiErosionHydDeposit
  , setUiErosionDepositSlope
  , setUiErosionThermDeposit
  , setUiErosionCoastZone
  , setUiErosionCoastStrength
  , setUiErosionCoastIter
  , setUiHypsometryEnabled
  , setUiHypsometryLowlandExp
  , setUiHypsometryHighlandExp
  , setUiHypsometryPlateauBreak
  , setUiHypsometryOceanExp
  , setUiHypsometryCoastalRampWidth
  , setUiHypsometryCoastalRampStr
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
  , setUiPiedmontSmooth
  , setUiPiedmontSlopeMin
  , setUiPiedmontSlopeMax
  , setUiMinLakeSize
  , setUiInlandSeaMinSize
  , setUiRoughnessScale
  , setUiEquatorTemp
  , setUiPoleTemp
  , setUiLapseRate
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
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiMoistMinVegFloor
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
  , setUiWindCoriolisDeflection
  , setUiBndLandRange
  , setUiBndTempConvergent
  , setUiBndTempDivergent
  , setUiBndTempTransform
  , setUiBndPrecipConvergent
  , setUiBndPrecipDivergent
  , setUiBndPrecipTransform
  , setUiConfigTab
  , setUiLeftTab
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
  , setUiPanOffset
  , setUiZoom
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
  , setUiTfcElevGradient
  , setUiTfcPlateauMaxRelief2Ring
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiWindIterations
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
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
  , setUiBtSnowMaxTemp
  , setUiBtAlpineMaxTemp
  , setUiBtIceCapTemp
  , setUiBtMontaneMaxTemp
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
  , setUiHoverHex
  , setUiHoverWidget
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiWorldConfig
  , setUiWorldName
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiContextHex
  , setUiContextPos
  , setUiHexTooltipPinned
  , setUiDisabledStages
  , setUiPluginParam
  , setUiPluginNames
  , setUiSimAutoTick
  , setUiSimTickRate
  , setUiSimTickCount
  , setUiSeedEditing
  , setUiSeedInput
  , setUiOverlayNames
  , setUiOverlayFields
  ) where

import Data.Aeson (Value)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Hyperspace.Actor
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.World.Persist.Types (WorldSaveManifest)
import Topo.Overlay.Schema (OverlayFieldType)
import Topo.Pipeline.Stage (StageId)
import UI.WidgetTree (WidgetId)

import Actor.UI.State
  ( ConfigTab
  , LeftTab
  , Ui
  , UiMenuMode
  , UiUpdate(..)
  , ViewMode
  )

type UiHandle = ActorHandle Ui (Protocol Ui)

sendUpdate :: UiHandle -> UiUpdate -> IO ()
sendUpdate handle update =
  cast @"update" handle #update update

sendUnary :: (a -> UiUpdate) -> UiHandle -> a -> IO ()
sendUnary constructor handle value =
  sendUpdate handle (constructor value)

setUiSeed = sendUnary SetSeed
setUiGenerating = sendUnary SetGenerating
setUiViewMode = sendUnary SetViewMode
setUiChunkSize = sendUnary SetChunkSize
setUiShowConfig = sendUnary SetShowConfig
setUiConfigScroll = sendUnary SetConfigScroll
setUiShowLeftPanel = sendUnary SetShowLeftPanel
setUiWaterLevel = sendUnary SetWaterLevel
setUiRenderWaterLevel = sendUnary SetRenderWaterLevel
setUiOrographicLift = sendUnary SetOrographicLift
setUiRainShadowLoss = sendUnary SetRainShadowLoss
setUiWindDiffuse = sendUnary SetWindDiffuse
setUiRainRate = sendUnary SetRainRate
setUiErosionHydraulic = sendUnary SetErosionHydraulic
setUiErosionThermal = sendUnary SetErosionThermal
setUiErosionTalus = sendUnary SetErosionTalus
setUiErosionMaxDrop = sendUnary SetErosionMaxDrop
setUiErosionHydDeposit = sendUnary SetErosionHydDeposit
setUiErosionDepositSlope = sendUnary SetErosionDepositSlope
setUiErosionThermDeposit = sendUnary SetErosionThermDeposit
setUiErosionCoastZone = sendUnary SetErosionCoastZone
setUiErosionCoastStrength = sendUnary SetErosionCoastStrength
setUiErosionCoastIter = sendUnary SetErosionCoastIter
setUiHypsometryEnabled = sendUnary SetHypsometryEnabled
setUiHypsometryLowlandExp = sendUnary SetHypsometryLowlandExp
setUiHypsometryHighlandExp = sendUnary SetHypsometryHighlandExp
setUiHypsometryPlateauBreak = sendUnary SetHypsometryPlateauBreak
setUiHypsometryOceanExp = sendUnary SetHypsometryOceanExp
setUiHypsometryCoastalRampWidth = sendUnary SetHypsometryCoastalRampWidth
setUiHypsometryCoastalRampStr = sendUnary SetHypsometryCoastalRampStr
setUiGlacierSnowTemp = sendUnary SetGlacierSnowTemp
setUiGlacierSnowRange = sendUnary SetGlacierSnowRange
setUiGlacierMeltTemp = sendUnary SetGlacierMeltTemp
setUiGlacierMeltRate = sendUnary SetGlacierMeltRate
setUiGlacierAccumScale = sendUnary SetGlacierAccumScale
setUiGlacierFlowIters = sendUnary SetGlacierFlowIters
setUiGlacierFlowRate = sendUnary SetGlacierFlowRate
setUiGlacierErosionScale = sendUnary SetGlacierErosionScale
setUiGlacierCarveScale = sendUnary SetGlacierCarveScale
setUiGlacierDepositScale = sendUnary SetGlacierDepositScale
setUiVentDensity = sendUnary SetVentDensity
setUiVentThreshold = sendUnary SetVentThreshold
setUiHotspotScale = sendUnary SetHotspotScale
setUiHotspotThreshold = sendUnary SetHotspotThreshold
setUiMagmaRecharge = sendUnary SetMagmaRecharge
setUiLavaScale = sendUnary SetLavaScale
setUiAshScale = sendUnary SetAshScale
setUiVolcanicDepositScale = sendUnary SetVolcanicDepositScale
setUiSoilMoistureThreshold = sendUnary SetSoilMoistureThreshold
setUiSoilHardnessThreshold = sendUnary SetSoilHardnessThreshold
setUiSoilFertilityMoistWeight = sendUnary SetSoilFertilityMoistWeight
setUiSoilFertilityDepthWeight = sendUnary SetSoilFertilityDepthWeight
setUiSinkBreachDepth = sendUnary SetSinkBreachDepth
setUiStreamPowerMaxErosion = sendUnary SetStreamPowerMaxErosion
setUiRiverCarveMaxDepth = sendUnary SetRiverCarveMaxDepth
setUiCoastalErodeStrength = sendUnary SetCoastalErodeStrength
setUiHydroHardnessWeight = sendUnary SetHydroHardnessWeight
setUiPiedmontSmooth = sendUnary SetPiedmontSmooth
setUiPiedmontSlopeMin = sendUnary SetPiedmontSlopeMin
setUiPiedmontSlopeMax = sendUnary SetPiedmontSlopeMax
setUiMinLakeSize = sendUnary SetMinLakeSize
setUiInlandSeaMinSize = sendUnary SetInlandSeaMinSize
setUiRoughnessScale = sendUnary SetRoughnessScale
setUiEquatorTemp = sendUnary SetEquatorTemp
setUiPoleTemp = sendUnary SetPoleTemp
setUiLapseRate = sendUnary SetLapseRate
setUiLatitudeExponent = sendUnary SetLatitudeExponent
setUiPlateHeightCooling = sendUnary SetPlateHeightCooling
setUiTempNoiseScale = sendUnary SetTempNoiseScale
setUiOceanModeration = sendUnary SetOceanModeration
setUiOceanModerateTemp = sendUnary SetOceanModerateTemp
setUiAlbedoSensitivity = sendUnary SetAlbedoSensitivity
setUiAlbedoReference = sendUnary SetAlbedoReference
setUiMoistAdvect = sendUnary SetMoistAdvect
setUiMoistLocal = sendUnary SetMoistLocal
setUiMoistWindEvapScale = sendUnary SetMoistWindEvapScale
setUiMoistEvapNoiseScale = sendUnary SetMoistEvapNoiseScale
setUiMoistBareEvapFrac = sendUnary SetMoistBareEvapFrac
setUiMoistVegTranspFrac = sendUnary SetMoistVegTranspFrac
setUiMoistWindETScale = sendUnary SetMoistWindETScale
setUiMoistCondensationRate = sendUnary SetMoistCondensationRate
setUiMoistRecycleRate = sendUnary SetMoistRecycleRate
setUiMoistITCZStrength = sendUnary SetMoistITCZStrength
setUiMoistITCZWidth = sendUnary SetMoistITCZWidth
setUiMoistMinVegFloor = sendUnary SetMoistMinVegFloor
setUiOrographicScale = sendUnary SetOrographicScale
setUiOrographicStep = sendUnary SetOrographicStep
setUiCoastalIterations = sendUnary SetCoastalIterations
setUiCoastalDiffuse = sendUnary SetCoastalDiffuse
setUiCoastalMoistureBoost = sendUnary SetCoastalMoistureBoost
setUiWindBeltStrength = sendUnary SetWindBeltStrength
setUiWindBeltHarmonics = sendUnary SetWindBeltHarmonics
setUiWindBeltBase = sendUnary SetWindBeltBase
setUiWindBeltRange = sendUnary SetWindBeltRange
setUiWindBeltSpeedScale = sendUnary SetWindBeltSpeedScale
setUiWindCoriolisDeflection = sendUnary SetWindCoriolisDeflection
setUiBndLandRange = sendUnary SetBndLandRange
setUiBndTempConvergent = sendUnary SetBndTempConvergent
setUiBndTempDivergent = sendUnary SetBndTempDivergent
setUiBndTempTransform = sendUnary SetBndTempTransform
setUiBndPrecipConvergent = sendUnary SetBndPrecipConvergent
setUiBndPrecipDivergent = sendUnary SetBndPrecipDivergent
setUiBndPrecipTransform = sendUnary SetBndPrecipTransform
setUiConfigTab = sendUnary SetConfigTab
setUiLeftTab = sendUnary SetLeftTab
setUiGenScale = sendUnary SetGenScale
setUiGenCoordScale = sendUnary SetGenCoordScale
setUiGenOffsetX = sendUnary SetGenOffsetX
setUiGenOffsetY = sendUnary SetGenOffsetY
setUiGenFrequency = sendUnary SetGenFrequency
setUiGenOctaves = sendUnary SetGenOctaves
setUiGenLacunarity = sendUnary SetGenLacunarity
setUiGenGain = sendUnary SetGenGain
setUiGenWarpScale = sendUnary SetGenWarpScale
setUiGenWarpStrength = sendUnary SetGenWarpStrength
setUiWorldExtentX = sendUnary SetWorldExtentX
setUiWorldExtentY = sendUnary SetWorldExtentY
setUiEdgeDepthNorth = sendUnary SetEdgeDepthNorth
setUiEdgeDepthSouth = sendUnary SetEdgeDepthSouth
setUiEdgeDepthEast = sendUnary SetEdgeDepthEast
setUiEdgeDepthWest = sendUnary SetEdgeDepthWest
setUiEdgeDepthFalloff = sendUnary SetEdgeDepthFalloff
setUiPanOffset = sendUnary SetPanOffset
setUiZoom = sendUnary SetZoom
setUiPlateSize = sendUnary SetPlateSize
setUiPlateSpeed = sendUnary SetPlateSpeed
setUiBoundarySharpness = sendUnary SetBoundarySharpness
setUiBoundaryNoiseScale = sendUnary SetBoundaryNoiseScale
setUiBoundaryNoiseStrength = sendUnary SetBoundaryNoiseStrength
setUiBoundaryWarpOctaves = sendUnary SetBoundaryWarpOctaves
setUiBoundaryWarpLacunarity = sendUnary SetBoundaryWarpLacunarity
setUiBoundaryWarpGain = sendUnary SetBoundaryWarpGain
setUiPlateMergeScale = sendUnary SetPlateMergeScale
setUiPlateMergeBias = sendUnary SetPlateMergeBias
setUiPlateDetailScale = sendUnary SetPlateDetailScale
setUiPlateDetailStrength = sendUnary SetPlateDetailStrength
setUiPlateRidgeStrength = sendUnary SetPlateRidgeStrength
setUiPlateHeightBase = sendUnary SetPlateHeightBase
setUiPlateHeightVariance = sendUnary SetPlateHeightVariance
setUiPlateHardnessBase = sendUnary SetPlateHardnessBase
setUiPlateHardnessVariance = sendUnary SetPlateHardnessVariance
setUiUplift = sendUnary SetUplift
setUiRiftDepth = sendUnary SetRiftDepth
setUiTrenchDepth = sendUnary SetTrenchDepth
setUiRidgeHeight = sendUnary SetRidgeHeight
setUiDetailScale = sendUnary SetDetailScale
setUiPlateBiasStrength = sendUnary SetPlateBiasStrength
setUiPlateBiasCenter = sendUnary SetPlateBiasCenter
setUiPlateBiasEdge = sendUnary SetPlateBiasEdge
setUiPlateBiasNorth = sendUnary SetPlateBiasNorth
setUiPlateBiasSouth = sendUnary SetPlateBiasSouth
setUiTfcCliffSlope = sendUnary SetTfcCliffSlope
setUiTfcMountainSlope = sendUnary SetTfcMountainSlope
setUiTfcMountainRelief = sendUnary SetTfcMountainRelief
setUiTfcHillSlope = sendUnary SetTfcHillSlope
setUiTfcRollingSlope = sendUnary SetTfcRollingSlope
setUiValleyCurvature = sendUnary SetValleyCurvature
setUiTfcElevGradient = sendUnary SetTfcElevGradient
setUiTfcPlateauMaxRelief2Ring = sendUnary SetTfcPlateauMaxRelief2Ring
setUiRockElevationThreshold = sendUnary SetRockElevationThreshold
setUiRockHardnessThreshold = sendUnary SetRockHardnessThreshold
setUiRockHardnessSecondary = sendUnary SetRockHardnessSecondary
setUiWindIterations = sendUnary SetWindIterations
setUiMoistureIterations = sendUnary SetMoistureIterations
setUiBoundaryMotionTemp = sendUnary SetBoundaryMotionTemp
setUiBoundaryMotionPrecip = sendUnary SetBoundaryMotionPrecip
setUiWeatherTick = sendUnary SetWeatherTick
setUiWeatherPhase = sendUnary SetWeatherPhase
setUiWeatherAmplitude = sendUnary SetWeatherAmplitude
setUiSeasonCycleLength = sendUnary SetSeasonCycleLength
setUiJitterAmplitude = sendUnary SetJitterAmplitude
setUiPressureBase = sendUnary SetPressureBase
setUiPressureTempScale = sendUnary SetPressureTempScale
setUiPressureCoriolisScale = sendUnary SetPressureCoriolisScale
setUiSeasonalBase = sendUnary SetSeasonalBase
setUiSeasonalRange = sendUnary SetSeasonalRange
setUiHumidityNoiseScale = sendUnary SetHumidityNoiseScale
setUiPrecipNoiseScale = sendUnary SetPrecipNoiseScale
setUiWeatherITCZWidth = sendUnary SetWeatherITCZWidth
setUiWeatherITCZPrecipBoost = sendUnary SetWeatherITCZPrecipBoost
setUiPressureHumidityScale = sendUnary SetPressureHumidityScale
setUiPressureGradientWindScale = sendUnary SetPressureGradientWindScale
setUiWindNoiseScale = sendUnary SetWindNoiseScale
setUiITCZMigrationScale = sendUnary SetITCZMigrationScale
setUiCloudRHExponent = sendUnary SetCloudRHExponent
setUiCloudAlbedoEffect = sendUnary SetCloudAlbedoEffect
setUiCloudPrecipBoost = sendUnary SetCloudPrecipBoost
setUiVegBase = sendUnary SetVegBase
setUiVegBoost = sendUnary SetVegBoost
setUiVegTempWeight = sendUnary SetVegTempWeight
setUiVegPrecipWeight = sendUnary SetVegPrecipWeight
setUiBtCoastalBand = sendUnary SetBtCoastalBand
setUiBtSnowMaxTemp = sendUnary SetBtSnowMaxTemp
setUiBtAlpineMaxTemp = sendUnary SetBtAlpineMaxTemp
setUiBtIceCapTemp = sendUnary SetBtIceCapTemp
setUiBtMontaneMaxTemp = sendUnary SetBtMontaneMaxTemp
setUiBtMontanePrecip = sendUnary SetBtMontanePrecip
setUiBtCliffSlope = sendUnary SetBtCliffSlope
setUiBtValleyMoisture = sendUnary SetBtValleyMoisture
setUiBtDepressionMoisture = sendUnary SetBtDepressionMoisture
setUiBtPrecipWeight = sendUnary SetBtPrecipWeight
setUiVbcTempMin = sendUnary SetVbcTempMin
setUiVbcTempRange = sendUnary SetVbcTempRange
setUiVbcFertilityBoost = sendUnary SetVbcFertilityBoost
setUiVbcAlbedoBase = sendUnary SetVbcAlbedoBase
setUiVbcAlbedoBare = sendUnary SetVbcAlbedoBare
setUiVbcAlbedoVeg = sendUnary SetVbcAlbedoVeg
setUiVbcOceanAlbedo = sendUnary SetVbcOceanAlbedo
setUiVbcIceAlbedo = sendUnary SetVbcIceAlbedo
setUiBiomeSmoothing = sendUnary SetBiomeSmoothing
setUiVolcanicAshBoost = sendUnary SetVolcanicAshBoost
setUiVolcanicLavaPenalty = sendUnary SetVolcanicLavaPenalty
setUiBiomeFeedbackBlend = sendUnary SetBiomeFeedbackBlend
setUiPlanetRadius = sendUnary SetPlanetRadius
setUiAxialTilt = sendUnary SetAxialTilt
setUiInsolation = sendUnary SetInsolation
setUiOccWarmScale = sendUnary SetOccWarmScale
setUiOccColdScale = sendUnary SetOccColdScale
setUiOccLatPeakDeg = sendUnary SetOccLatPeakDeg
setUiOccLatWidthDeg = sendUnary SetOccLatWidthDeg
setUiSliceLatCenter = sendUnary SetSliceLatCenter
setUiSliceLonCenter = sendUnary SetSliceLonCenter
setUiHoverHex = sendUnary SetHoverHex
setUiHoverWidget = sendUnary SetHoverWidget
setUiMenuMode = sendUnary SetMenuMode
setUiPresetInput = sendUnary SetPresetInput
setUiPresetList = sendUnary SetPresetList
setUiPresetSelected = sendUnary SetPresetSelected
setUiWorldConfig = sendUnary SetWorldConfig
setUiWorldName = sendUnary SetWorldName
setUiWorldSaveInput = sendUnary SetWorldSaveInput
setUiWorldList = sendUnary SetWorldList
setUiWorldSelected = sendUnary SetWorldSelected
setUiContextHex = sendUnary SetContextHex
setUiContextPos = sendUnary SetContextPos
setUiHexTooltipPinned = sendUnary SetHexTooltipPinned
setUiDisabledStages = sendUnary SetDisabledStages
setUiPluginNames = sendUnary SetPluginNames
setUiSimAutoTick = sendUnary SetSimAutoTick
setUiSimTickRate = sendUnary SetSimTickRate
setUiSimTickCount = sendUnary SetSimTickCount
setUiSeedEditing = sendUnary SetSeedEditing
setUiSeedInput = sendUnary SetSeedInput
setUiOverlayNames = sendUnary SetOverlayNames
setUiOverlayFields = sendUnary SetOverlayFields

setUiPluginParam handle pluginName paramName value =
  sendUpdate handle (SetPluginParam pluginName paramName value)
