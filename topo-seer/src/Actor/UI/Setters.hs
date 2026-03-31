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
  , setUiSliderValue
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
  , setUiTfcPlateauMaxMicroRelief
  , setUiTfcRollingNearFactor
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
  , setUiHexSizeKm
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
  , setUiDisabledPlugins
  , setUiPluginParam
  , setUiPluginNames
  , setUiPluginExpanded
  , setUiPluginParamSpecs
  , setUiSimAutoTick
  , setUiSimTickRate
  , setUiSimTickCount
  , setUiSeedEditing
  , setUiSeedInput
  , setUiOverlayNames
  , setUiOverlayFields
  , setUiDataBrowser
  , setUiDataResources
  , setUiEditor
  ) where

import Data.Aeson (Value)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Hyperspace.Actor
import Seer.Config.SliderRegistry (SliderId(..))
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.Editor.Types (EditorState)
import Seer.World.Persist.Types (WorldSaveManifest)
import Topo.Overlay.Schema (OverlayFieldType)
import Topo.Pipeline.Stage (StageId)
import Topo.Plugin.DataResource (DataResourceSchema)
import UI.WidgetTree (WidgetId)

import Actor.UI.State
  ( ConfigTab
  , DataBrowserState
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

setUiSliderValue :: UiHandle -> SliderId -> Float -> IO ()
setUiSliderValue handle sliderIdValue value =
  sendUpdate handle (SetSliderValue sliderIdValue value)

sendSlider :: SliderId -> UiHandle -> Float -> IO ()
sendSlider sliderIdValue handle value =
  setUiSliderValue handle sliderIdValue value

setUiSeed = sendUnary SetSeed
setUiGenerating = sendUnary SetGenerating
setUiViewMode = sendUnary SetViewMode
setUiChunkSize = sendUnary SetChunkSize
setUiShowConfig = sendUnary SetShowConfig
setUiConfigScroll = sendUnary SetConfigScroll
setUiShowLeftPanel = sendUnary SetShowLeftPanel
setUiWaterLevel = sendSlider SliderWaterLevel
setUiRenderWaterLevel = sendUnary SetRenderWaterLevel
setUiOrographicLift = sendSlider SliderOrographicLift
setUiRainShadowLoss = sendSlider SliderRainShadowLoss
setUiWindDiffuse = sendSlider SliderWindDiffuse
setUiRainRate = sendSlider SliderErosionRainRate
setUiErosionHydraulic = sendSlider SliderErosionHydraulic
setUiErosionThermal = sendSlider SliderErosionThermal
setUiErosionTalus = sendSlider SliderErosionTalus
setUiErosionMaxDrop = sendSlider SliderErosionMaxDrop
setUiErosionHydDeposit = sendSlider SliderErosionHydDeposit
setUiErosionDepositSlope = sendSlider SliderErosionDepositSlope
setUiErosionThermDeposit = sendSlider SliderErosionThermDeposit
setUiErosionCoastZone = sendSlider SliderErosionCoastZone
setUiErosionCoastStrength = sendSlider SliderErosionCoastStrength
setUiErosionCoastIter = sendSlider SliderErosionCoastIter
setUiHypsometryEnabled = sendSlider SliderHypsometryEnabled
setUiHypsometryLowlandExp = sendSlider SliderHypsometryLowlandExp
setUiHypsometryHighlandExp = sendSlider SliderHypsometryHighlandExp
setUiHypsometryPlateauBreak = sendSlider SliderHypsometryPlateauBreak
setUiHypsometryOceanExp = sendSlider SliderHypsometryOceanExp
setUiHypsometryCoastalRampWidth = sendSlider SliderHypsometryCoastalRampWidth
setUiHypsometryCoastalRampStr = sendSlider SliderHypsometryCoastalRampStr
setUiGlacierSnowTemp = sendSlider SliderGlacierSnowTemp
setUiGlacierSnowRange = sendSlider SliderGlacierSnowRange
setUiGlacierMeltTemp = sendSlider SliderGlacierMeltTemp
setUiGlacierMeltRate = sendSlider SliderGlacierMeltRate
setUiGlacierAccumScale = sendSlider SliderGlacierAccumScale
setUiGlacierFlowIters = sendSlider SliderGlacierFlowIters
setUiGlacierFlowRate = sendSlider SliderGlacierFlowRate
setUiGlacierErosionScale = sendSlider SliderGlacierErosionScale
setUiGlacierCarveScale = sendSlider SliderGlacierCarveScale
setUiGlacierDepositScale = sendSlider SliderGlacierDepositScale
setUiVentDensity = sendSlider SliderVentDensity
setUiVentThreshold = sendSlider SliderVentThreshold
setUiHotspotScale = sendSlider SliderHotspotScale
setUiHotspotThreshold = sendSlider SliderHotspotThreshold
setUiMagmaRecharge = sendSlider SliderMagmaRecharge
setUiLavaScale = sendSlider SliderLavaScale
setUiAshScale = sendSlider SliderAshScale
setUiVolcanicDepositScale = sendSlider SliderVolcanicDepositScale
setUiSoilMoistureThreshold = sendSlider SliderSoilMoistureThreshold
setUiSoilHardnessThreshold = sendSlider SliderSoilHardnessThreshold
setUiSoilFertilityMoistWeight = sendSlider SliderSoilFertilityMoistWeight
setUiSoilFertilityDepthWeight = sendSlider SliderSoilFertilityDepthWeight
setUiSinkBreachDepth = sendSlider SliderSinkBreachDepth
setUiStreamPowerMaxErosion = sendSlider SliderStreamPowerMaxErosion
setUiRiverCarveMaxDepth = sendSlider SliderRiverCarveMaxDepth
setUiCoastalErodeStrength = sendSlider SliderCoastalErodeStrength
setUiHydroHardnessWeight = sendSlider SliderHydroHardnessWeight
setUiPiedmontSmooth = sendSlider SliderPiedmontSmooth
setUiPiedmontSlopeMin = sendSlider SliderPiedmontSlopeMin
setUiPiedmontSlopeMax = sendSlider SliderPiedmontSlopeMax
setUiMinLakeSize = sendSlider SliderMinLakeSize
setUiInlandSeaMinSize = sendSlider SliderInlandSeaMinSize
setUiRoughnessScale = sendSlider SliderRoughnessScale
setUiEquatorTemp = sendSlider SliderEquatorTemp
setUiPoleTemp = sendSlider SliderPoleTemp
setUiLapseRate = sendSlider SliderLapseRate
setUiLatitudeExponent = sendSlider SliderLatitudeExponent
setUiPlateHeightCooling = sendSlider SliderPlateHeightCooling
setUiTempNoiseScale = sendSlider SliderTempNoiseScale
setUiOceanModeration = sendSlider SliderOceanModeration
setUiOceanModerateTemp = sendSlider SliderOceanModerateTemp
setUiAlbedoSensitivity = sendSlider SliderAlbedoSensitivity
setUiAlbedoReference = sendSlider SliderAlbedoReference
setUiMoistAdvect = sendSlider SliderMoistAdvect
setUiMoistLocal = sendSlider SliderMoistLocal
setUiMoistWindEvapScale = sendSlider SliderMoistWindEvapScale
setUiMoistEvapNoiseScale = sendSlider SliderMoistEvapNoiseScale
setUiMoistBareEvapFrac = sendSlider SliderMoistBareEvapFrac
setUiMoistVegTranspFrac = sendSlider SliderMoistVegTranspFrac
setUiMoistWindETScale = sendSlider SliderMoistWindETScale
setUiMoistCondensationRate = sendSlider SliderMoistCondensationRate
setUiMoistRecycleRate = sendSlider SliderMoistRecycleRate
setUiMoistITCZStrength = sendSlider SliderMoistITCZStrength
setUiMoistITCZWidth = sendSlider SliderMoistITCZWidth
setUiMoistMinVegFloor = sendSlider SliderMoistMinVegFloor
setUiOrographicScale = sendSlider SliderOrographicScale
setUiOrographicStep = sendSlider SliderOrographicStep
setUiCoastalIterations = sendSlider SliderCoastalIterations
setUiCoastalDiffuse = sendSlider SliderCoastalDiffuse
setUiCoastalMoistureBoost = sendSlider SliderCoastalMoistureBoost
setUiWindBeltStrength = sendSlider SliderWindBeltStrength
setUiWindBeltHarmonics = sendSlider SliderWindBeltHarmonics
setUiWindBeltBase = sendSlider SliderWindBeltBase
setUiWindBeltRange = sendSlider SliderWindBeltRange
setUiWindBeltSpeedScale = sendSlider SliderWindBeltSpeedScale
setUiWindCoriolisDeflection = sendSlider SliderWindCoriolisDeflection
setUiBndLandRange = sendSlider SliderBndLandRange
setUiBndTempConvergent = sendUnary SetBndTempConvergent
setUiBndTempDivergent = sendUnary SetBndTempDivergent
setUiBndTempTransform = sendUnary SetBndTempTransform
setUiBndPrecipConvergent = sendUnary SetBndPrecipConvergent
setUiBndPrecipDivergent = sendUnary SetBndPrecipDivergent
setUiBndPrecipTransform = sendUnary SetBndPrecipTransform
setUiConfigTab = sendUnary SetConfigTab
setUiLeftTab = sendUnary SetLeftTab
setUiGenScale = sendSlider SliderGenScale
setUiGenCoordScale = sendSlider SliderGenCoordScale
setUiGenOffsetX = sendSlider SliderGenOffsetX
setUiGenOffsetY = sendSlider SliderGenOffsetY
setUiGenFrequency = sendSlider SliderGenFrequency
setUiGenOctaves = sendSlider SliderGenOctaves
setUiGenLacunarity = sendSlider SliderGenLacunarity
setUiGenGain = sendSlider SliderGenGain
setUiGenWarpScale = sendSlider SliderGenWarpScale
setUiGenWarpStrength = sendSlider SliderGenWarpStrength
setUiWorldExtentX = sendSlider SliderExtentX
setUiWorldExtentY = sendSlider SliderExtentY
setUiEdgeDepthNorth = sendSlider SliderEdgeNorth
setUiEdgeDepthSouth = sendSlider SliderEdgeSouth
setUiEdgeDepthEast = sendSlider SliderEdgeEast
setUiEdgeDepthWest = sendSlider SliderEdgeWest
setUiEdgeDepthFalloff = sendSlider SliderEdgeFalloff
setUiPanOffset = sendUnary SetPanOffset
setUiZoom = sendUnary SetZoom
setUiPlateSize = sendSlider SliderPlateSize
setUiPlateSpeed = sendSlider SliderPlateSpeed
setUiBoundarySharpness = sendSlider SliderBoundarySharpness
setUiBoundaryNoiseScale = sendSlider SliderBoundaryNoiseScale
setUiBoundaryNoiseStrength = sendSlider SliderBoundaryNoiseStrength
setUiBoundaryWarpOctaves = sendSlider SliderBoundaryWarpOctaves
setUiBoundaryWarpLacunarity = sendSlider SliderBoundaryWarpLacunarity
setUiBoundaryWarpGain = sendSlider SliderBoundaryWarpGain
setUiPlateMergeScale = sendSlider SliderPlateMergeScale
setUiPlateMergeBias = sendSlider SliderPlateMergeBias
setUiPlateDetailScale = sendSlider SliderPlateDetailScale
setUiPlateDetailStrength = sendSlider SliderPlateDetailStrength
setUiPlateRidgeStrength = sendSlider SliderPlateRidgeStrength
setUiPlateHeightBase = sendSlider SliderPlateHeightBase
setUiPlateHeightVariance = sendSlider SliderPlateHeightVariance
setUiPlateHardnessBase = sendSlider SliderPlateHardnessBase
setUiPlateHardnessVariance = sendSlider SliderPlateHardnessVariance
setUiUplift = sendSlider SliderUplift
setUiRiftDepth = sendSlider SliderRiftDepth
setUiTrenchDepth = sendSlider SliderTrenchDepth
setUiRidgeHeight = sendSlider SliderRidgeHeight
setUiDetailScale = sendSlider SliderDetailScale
setUiPlateBiasStrength = sendSlider SliderPlateBiasStrength
setUiPlateBiasCenter = sendSlider SliderPlateBiasCenter
setUiPlateBiasEdge = sendSlider SliderPlateBiasEdge
setUiPlateBiasNorth = sendSlider SliderPlateBiasNorth
setUiPlateBiasSouth = sendSlider SliderPlateBiasSouth
setUiTfcCliffSlope = sendSlider SliderTfcCliffSlope
setUiTfcMountainSlope = sendSlider SliderTfcMountainSlope
setUiTfcMountainRelief = sendSlider SliderTfcMountainRelief
setUiTfcHillSlope = sendSlider SliderTfcHillSlope
setUiTfcRollingSlope = sendSlider SliderTfcRollingSlope
setUiValleyCurvature = sendSlider SliderValleyCurvature
setUiTfcElevGradient = sendSlider SliderTfcElevGradient
setUiTfcPlateauMaxRelief2Ring = sendSlider SliderTfcPlateauMaxRelief2Ring
setUiTfcPlateauMaxMicroRelief = sendSlider SliderTfcPlateauMaxMicroRelief
setUiTfcRollingNearFactor = sendSlider SliderTfcRollingNearFactor
setUiRockElevationThreshold = sendSlider SliderRockElevationThreshold
setUiRockHardnessThreshold = sendSlider SliderRockHardnessThreshold
setUiRockHardnessSecondary = sendSlider SliderRockHardnessSecondary
setUiWindIterations = sendSlider SliderWindIterations
setUiMoistureIterations = sendSlider SliderMoistureIterations
setUiBoundaryMotionTemp = sendSlider SliderBoundaryMotionTemp
setUiBoundaryMotionPrecip = sendSlider SliderBoundaryMotionPrecip
setUiWeatherTick = sendSlider SliderWeatherTick
setUiWeatherPhase = sendSlider SliderWeatherPhase
setUiWeatherAmplitude = sendSlider SliderWeatherAmplitude
setUiSeasonCycleLength = sendSlider SliderSeasonCycleLength
setUiJitterAmplitude = sendSlider SliderJitterAmplitude
setUiPressureBase = sendSlider SliderPressureBase
setUiPressureTempScale = sendSlider SliderPressureTempScale
setUiPressureCoriolisScale = sendSlider SliderPressureCoriolisScale
setUiSeasonalBase = sendSlider SliderSeasonalBase
setUiSeasonalRange = sendSlider SliderSeasonalRange
setUiHumidityNoiseScale = sendSlider SliderHumidityNoiseScale
setUiPrecipNoiseScale = sendSlider SliderPrecipNoiseScale
setUiWeatherITCZWidth = sendSlider SliderWeatherITCZWidth
setUiWeatherITCZPrecipBoost = sendSlider SliderWeatherITCZPrecipBoost
setUiPressureHumidityScale = sendSlider SliderPressureHumidityScale
setUiPressureGradientWindScale = sendSlider SliderPressureGradientWindScale
setUiWindNoiseScale = sendSlider SliderWindNoiseScale
setUiITCZMigrationScale = sendSlider SliderITCZMigrationScale
setUiCloudRHExponent = sendSlider SliderCloudRHExponent
setUiCloudAlbedoEffect = sendSlider SliderCloudAlbedoEffect
setUiCloudPrecipBoost = sendSlider SliderCloudPrecipBoost
setUiVegBase = sendSlider SliderVegBase
setUiVegBoost = sendSlider SliderVegBoost
setUiVegTempWeight = sendSlider SliderVegTempWeight
setUiVegPrecipWeight = sendSlider SliderVegPrecipWeight
setUiBtCoastalBand = sendSlider SliderBtCoastalBand
setUiBtSnowMaxTemp = sendSlider SliderBtSnowMaxTemp
setUiBtAlpineMaxTemp = sendSlider SliderBtAlpineMaxTemp
setUiBtIceCapTemp = sendSlider SliderBtIceCapTemp
setUiBtMontaneMaxTemp = sendSlider SliderBtMontaneMaxTemp
setUiBtMontanePrecip = sendSlider SliderBtMontanePrecip
setUiBtCliffSlope = sendSlider SliderBtCliffSlope
setUiBtValleyMoisture = sendSlider SliderBtValleyMoisture
setUiBtDepressionMoisture = sendSlider SliderBtDepressionMoisture
setUiBtPrecipWeight = sendSlider SliderBtPrecipWeight
setUiVbcTempMin = sendSlider SliderVbcTempMin
setUiVbcTempRange = sendSlider SliderVbcTempRange
setUiVbcFertilityBoost = sendSlider SliderVbcFertilityBoost
setUiVbcAlbedoBase = sendSlider SliderVbcAlbedoBase
setUiVbcAlbedoBare = sendSlider SliderVbcAlbedoBare
setUiVbcAlbedoVeg = sendSlider SliderVbcAlbedoVeg
setUiVbcOceanAlbedo = sendSlider SliderVbcOceanAlbedo
setUiVbcIceAlbedo = sendSlider SliderVbcIceAlbedo
setUiBiomeSmoothing = sendSlider SliderBiomeSmoothing
setUiVolcanicAshBoost = sendSlider SliderVolcanicAshBoost
setUiVolcanicLavaPenalty = sendSlider SliderVolcanicLavaPenalty
setUiBiomeFeedbackBlend = sendSlider SliderBiomeFeedbackBlend
setUiPlanetRadius = sendSlider SliderPlanetRadius
setUiAxialTilt = sendSlider SliderAxialTilt
setUiInsolation = sendSlider SliderInsolation
setUiOccWarmScale = sendSlider SliderOccWarmScale
setUiOccColdScale = sendSlider SliderOccColdScale
setUiOccLatPeakDeg = sendSlider SliderOccLatPeakDeg
setUiOccLatWidthDeg = sendSlider SliderOccLatWidthDeg
setUiHexSizeKm = sendSlider SliderHexSizeKm
setUiSliceLatCenter = sendSlider SliderSliceLatCenter
setUiSliceLonCenter = sendSlider SliderSliceLonCenter
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
setUiDisabledPlugins = sendUnary SetDisabledPlugins
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

setUiPluginExpanded handle pluginName expanded =
  sendUpdate handle (SetPluginExpanded pluginName expanded)

setUiPluginParamSpecs = sendUnary SetPluginParamSpecs
setUiDataBrowser = sendUnary SetDataBrowser
setUiDataResources = sendUnary SetDataResources
setUiEditor = sendUnary SetEditor
