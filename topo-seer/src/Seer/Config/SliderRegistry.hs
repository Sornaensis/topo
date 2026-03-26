module Seer.Config.SliderRegistry
  ( SliderId(..)
  , SliderPart(..)
  , SliderTab(..)
  , SliderDef(..)
  , allSliderDefs
  , sliderDefaultValueForId
  , sliderDefsForTab
  , sliderRowCountForTab
  , sliderDefForWidget
  , sliderWidgetPart
  ) where

import Data.List (find)
import UI.WidgetId

data SliderTab
  = SliderTabTerrain
  | SliderTabPlanet
  | SliderTabClimate
  | SliderTabWeather
  | SliderTabBiome
  | SliderTabErosion
  deriving (Eq, Ord, Show, Enum, Bounded)

data SliderId
  = SliderGenScale
  | SliderGenCoordScale
  | SliderGenOffsetX
  | SliderGenOffsetY
  | SliderGenFrequency
  | SliderGenOctaves
  | SliderGenLacunarity
  | SliderGenGain
  | SliderGenWarpScale
  | SliderGenWarpStrength
  | SliderExtentX
  | SliderExtentY
  | SliderEdgeNorth
  | SliderEdgeSouth
  | SliderEdgeEast
  | SliderEdgeWest
  | SliderEdgeFalloff
  | SliderPlateSize
  | SliderUplift
  | SliderRiftDepth
  | SliderDetailScale
  | SliderPlateSpeed
  | SliderBoundarySharpness
  | SliderBoundaryNoiseScale
  | SliderBoundaryNoiseStrength
  | SliderBoundaryWarpOctaves
  | SliderBoundaryWarpLacunarity
  | SliderBoundaryWarpGain
  | SliderPlateMergeScale
  | SliderPlateMergeBias
  | SliderPlateDetailScale
  | SliderPlateDetailStrength
  | SliderPlateRidgeStrength
  | SliderPlateHeightBase
  | SliderPlateHeightVariance
  | SliderPlateHardnessBase
  | SliderPlateHardnessVariance
  | SliderTrenchDepth
  | SliderRidgeHeight
  | SliderPlateBiasStrength
  | SliderPlateBiasCenter
  | SliderPlateBiasEdge
  | SliderPlateBiasNorth
  | SliderPlateBiasSouth
  | SliderTfcCliffSlope
  | SliderTfcMountainSlope
  | SliderTfcMountainRelief
  | SliderTfcHillSlope
  | SliderTfcRollingSlope
  | SliderValleyCurvature
  | SliderTfcElevGradient
  | SliderTfcPlateauMaxRelief2Ring
  | SliderTfcPlateauMaxMicroRelief
  | SliderTfcRollingNearFactor
  | SliderRockElevationThreshold
  | SliderRockHardnessThreshold
  | SliderRockHardnessSecondary
  | SliderPlanetRadius
  | SliderAxialTilt
  | SliderInsolation
  | SliderOccWarmScale
  | SliderOccColdScale
  | SliderOccLatPeakDeg
  | SliderOccLatWidthDeg
  | SliderWaterLevel
  | SliderOrographicLift
  | SliderRainShadowLoss
  | SliderWindDiffuse
  | SliderEquatorTemp
  | SliderPoleTemp
  | SliderLapseRate
  | SliderWindIterations
  | SliderMoistureIterations
  | SliderBoundaryMotionTemp
  | SliderBoundaryMotionPrecip
  | SliderSliceLatCenter
  | SliderSliceLonCenter
  | SliderLatitudeExponent
  | SliderPlateHeightCooling
  | SliderTempNoiseScale
  | SliderOceanModeration
  | SliderOceanModerateTemp
  | SliderAlbedoSensitivity
  | SliderAlbedoReference
  | SliderMoistAdvect
  | SliderMoistLocal
  | SliderMoistWindEvapScale
  | SliderMoistEvapNoiseScale
  | SliderMoistBareEvapFrac
  | SliderMoistVegTranspFrac
  | SliderMoistWindETScale
  | SliderMoistCondensationRate
  | SliderMoistRecycleRate
  | SliderMoistITCZStrength
  | SliderMoistITCZWidth
  | SliderOrographicScale
  | SliderOrographicStep
  | SliderCoastalIterations
  | SliderCoastalDiffuse
  | SliderCoastalMoistureBoost
  | SliderWindBeltStrength
  | SliderWindBeltHarmonics
  | SliderWindBeltBase
  | SliderWindBeltRange
  | SliderWindBeltSpeedScale
  | SliderBndLandRange
  | SliderPiedmontSmooth
  | SliderPiedmontSlopeMin
  | SliderPiedmontSlopeMax
  | SliderWindCoriolisDeflection
  | SliderMoistMinVegFloor
  | SliderWeatherTick
  | SliderWeatherPhase
  | SliderWeatherAmplitude
  | SliderSeasonCycleLength
  | SliderJitterAmplitude
  | SliderPressureBase
  | SliderPressureTempScale
  | SliderPressureCoriolisScale
  | SliderSeasonalBase
  | SliderSeasonalRange
  | SliderHumidityNoiseScale
  | SliderPrecipNoiseScale
  | SliderWeatherITCZWidth
  | SliderWeatherITCZPrecipBoost
  | SliderPressureHumidityScale
  | SliderPressureGradientWindScale
  | SliderWindNoiseScale
  | SliderITCZMigrationScale
  | SliderCloudRHExponent
  | SliderCloudAlbedoEffect
  | SliderCloudPrecipBoost
  | SliderVegBase
  | SliderVegBoost
  | SliderVegTempWeight
  | SliderVegPrecipWeight
  | SliderBtCoastalBand
  | SliderBtSnowMaxTemp
  | SliderBtAlpineMaxTemp
  | SliderBtIceCapTemp
  | SliderBtMontaneMaxTemp
  | SliderBtMontanePrecip
  | SliderBtCliffSlope
  | SliderBtValleyMoisture
  | SliderBtDepressionMoisture
  | SliderBtPrecipWeight
  | SliderVbcTempMin
  | SliderVbcTempRange
  | SliderVbcFertilityBoost
  | SliderVbcAlbedoBase
  | SliderVbcAlbedoBare
  | SliderVbcAlbedoVeg
  | SliderVbcOceanAlbedo
  | SliderVbcIceAlbedo
  | SliderBiomeSmoothing
  | SliderVolcanicAshBoost
  | SliderVolcanicLavaPenalty
  | SliderBiomeFeedbackBlend
  | SliderErosionHydraulic
  | SliderErosionThermal
  | SliderErosionRainRate
  | SliderErosionTalus
  | SliderErosionMaxDrop
  | SliderErosionHydDeposit
  | SliderErosionDepositSlope
  | SliderErosionThermDeposit
  | SliderErosionCoastZone
  | SliderErosionCoastStrength
  | SliderErosionCoastIter
  | SliderHypsometryEnabled
  | SliderHypsometryLowlandExp
  | SliderHypsometryHighlandExp
  | SliderHypsometryPlateauBreak
  | SliderHypsometryOceanExp
  | SliderHypsometryCoastalRampWidth
  | SliderHypsometryCoastalRampStr
  | SliderGlacierSnowTemp
  | SliderGlacierSnowRange
  | SliderGlacierMeltTemp
  | SliderGlacierMeltRate
  | SliderGlacierAccumScale
  | SliderGlacierFlowIters
  | SliderGlacierFlowRate
  | SliderGlacierErosionScale
  | SliderGlacierCarveScale
  | SliderGlacierDepositScale
  | SliderVentDensity
  | SliderVentThreshold
  | SliderHotspotScale
  | SliderHotspotThreshold
  | SliderMagmaRecharge
  | SliderLavaScale
  | SliderAshScale
  | SliderVolcanicDepositScale
  | SliderSoilMoistureThreshold
  | SliderSoilHardnessThreshold
  | SliderSoilFertilityMoistWeight
  | SliderSoilFertilityDepthWeight
  | SliderSinkBreachDepth
  | SliderStreamPowerMaxErosion
  | SliderRiverCarveMaxDepth
  | SliderCoastalErodeStrength
  | SliderHydroHardnessWeight
  | SliderMinLakeSize
  | SliderInlandSeaMinSize
  | SliderRoughnessScale
  deriving (Eq, Ord, Show, Enum, Bounded)

data SliderPart
  = SliderPartMinus
  | SliderPartPlus
  deriving (Eq, Ord, Show)

data SliderDef = SliderDef
  { sliderId :: !SliderId
  , sliderTab :: !SliderTab
  , sliderRowIndex :: !Int
  , sliderMinusWidgetId :: !WidgetId
  , sliderPlusWidgetId :: !WidgetId
  } deriving (Eq, Show)

allSliderDefs :: [SliderDef]
allSliderDefs =
  [ def SliderGenScale SliderTabTerrain 0 WidgetConfigGenScaleMinus WidgetConfigGenScalePlus
  , def SliderGenCoordScale SliderTabTerrain 1 WidgetConfigGenCoordScaleMinus WidgetConfigGenCoordScalePlus
  , def SliderGenOffsetX SliderTabTerrain 2 WidgetConfigGenOffsetXMinus WidgetConfigGenOffsetXPlus
  , def SliderGenOffsetY SliderTabTerrain 3 WidgetConfigGenOffsetYMinus WidgetConfigGenOffsetYPlus
  , def SliderGenFrequency SliderTabTerrain 4 WidgetConfigGenFrequencyMinus WidgetConfigGenFrequencyPlus
  , def SliderGenOctaves SliderTabTerrain 5 WidgetConfigGenOctavesMinus WidgetConfigGenOctavesPlus
  , def SliderGenLacunarity SliderTabTerrain 6 WidgetConfigGenLacunarityMinus WidgetConfigGenLacunarityPlus
  , def SliderGenGain SliderTabTerrain 7 WidgetConfigGenGainMinus WidgetConfigGenGainPlus
  , def SliderGenWarpScale SliderTabTerrain 8 WidgetConfigGenWarpScaleMinus WidgetConfigGenWarpScalePlus
  , def SliderGenWarpStrength SliderTabTerrain 9 WidgetConfigGenWarpStrengthMinus WidgetConfigGenWarpStrengthPlus
  , def SliderExtentX SliderTabTerrain 10 WidgetConfigExtentXMinus WidgetConfigExtentXPlus
  , def SliderExtentY SliderTabTerrain 11 WidgetConfigExtentYMinus WidgetConfigExtentYPlus
  , def SliderEdgeNorth SliderTabTerrain 12 WidgetConfigEdgeNorthMinus WidgetConfigEdgeNorthPlus
  , def SliderEdgeSouth SliderTabTerrain 13 WidgetConfigEdgeSouthMinus WidgetConfigEdgeSouthPlus
  , def SliderEdgeEast SliderTabTerrain 14 WidgetConfigEdgeEastMinus WidgetConfigEdgeEastPlus
  , def SliderEdgeWest SliderTabTerrain 15 WidgetConfigEdgeWestMinus WidgetConfigEdgeWestPlus
  , def SliderEdgeFalloff SliderTabTerrain 16 WidgetConfigEdgeFalloffMinus WidgetConfigEdgeFalloffPlus
  , def SliderPlateSize SliderTabTerrain 17 WidgetConfigPlateSizeMinus WidgetConfigPlateSizePlus
  , def SliderUplift SliderTabTerrain 18 WidgetConfigUpliftMinus WidgetConfigUpliftPlus
  , def SliderRiftDepth SliderTabTerrain 19 WidgetConfigRiftDepthMinus WidgetConfigRiftDepthPlus
  , def SliderDetailScale SliderTabTerrain 20 WidgetConfigDetailScaleMinus WidgetConfigDetailScalePlus
  , def SliderPlateSpeed SliderTabTerrain 21 WidgetConfigPlateSpeedMinus WidgetConfigPlateSpeedPlus
  , def SliderBoundarySharpness SliderTabTerrain 22 WidgetConfigBoundarySharpnessMinus WidgetConfigBoundarySharpnessPlus
  , def SliderBoundaryNoiseScale SliderTabTerrain 23 WidgetConfigBoundaryNoiseScaleMinus WidgetConfigBoundaryNoiseScalePlus
  , def SliderBoundaryNoiseStrength SliderTabTerrain 24 WidgetConfigBoundaryNoiseStrengthMinus WidgetConfigBoundaryNoiseStrengthPlus
  , def SliderBoundaryWarpOctaves SliderTabTerrain 25 WidgetConfigBoundaryWarpOctavesMinus WidgetConfigBoundaryWarpOctavesPlus
  , def SliderBoundaryWarpLacunarity SliderTabTerrain 26 WidgetConfigBoundaryWarpLacunarityMinus WidgetConfigBoundaryWarpLacunarityPlus
  , def SliderBoundaryWarpGain SliderTabTerrain 27 WidgetConfigBoundaryWarpGainMinus WidgetConfigBoundaryWarpGainPlus
  , def SliderPlateMergeScale SliderTabTerrain 28 WidgetConfigPlateMergeScaleMinus WidgetConfigPlateMergeScalePlus
  , def SliderPlateMergeBias SliderTabTerrain 29 WidgetConfigPlateMergeBiasMinus WidgetConfigPlateMergeBiasPlus
  , def SliderPlateDetailScale SliderTabTerrain 30 WidgetConfigPlateDetailScaleMinus WidgetConfigPlateDetailScalePlus
  , def SliderPlateDetailStrength SliderTabTerrain 31 WidgetConfigPlateDetailStrengthMinus WidgetConfigPlateDetailStrengthPlus
  , def SliderPlateRidgeStrength SliderTabTerrain 32 WidgetConfigPlateRidgeStrengthMinus WidgetConfigPlateRidgeStrengthPlus
  , def SliderPlateHeightBase SliderTabTerrain 33 WidgetConfigPlateHeightBaseMinus WidgetConfigPlateHeightBasePlus
  , def SliderPlateHeightVariance SliderTabTerrain 34 WidgetConfigPlateHeightVarianceMinus WidgetConfigPlateHeightVariancePlus
  , def SliderPlateHardnessBase SliderTabTerrain 35 WidgetConfigPlateHardnessBaseMinus WidgetConfigPlateHardnessBasePlus
  , def SliderPlateHardnessVariance SliderTabTerrain 36 WidgetConfigPlateHardnessVarianceMinus WidgetConfigPlateHardnessVariancePlus
  , def SliderTrenchDepth SliderTabTerrain 37 WidgetConfigTrenchDepthMinus WidgetConfigTrenchDepthPlus
  , def SliderRidgeHeight SliderTabTerrain 38 WidgetConfigRidgeHeightMinus WidgetConfigRidgeHeightPlus
  , def SliderPlateBiasStrength SliderTabTerrain 39 WidgetConfigPlateBiasStrengthMinus WidgetConfigPlateBiasStrengthPlus
  , def SliderPlateBiasCenter SliderTabTerrain 40 WidgetConfigPlateBiasCenterMinus WidgetConfigPlateBiasCenterPlus
  , def SliderPlateBiasEdge SliderTabTerrain 41 WidgetConfigPlateBiasEdgeMinus WidgetConfigPlateBiasEdgePlus
  , def SliderPlateBiasNorth SliderTabTerrain 42 WidgetConfigPlateBiasNorthMinus WidgetConfigPlateBiasNorthPlus
  , def SliderPlateBiasSouth SliderTabTerrain 43 WidgetConfigPlateBiasSouthMinus WidgetConfigPlateBiasSouthPlus
  , def SliderTfcCliffSlope SliderTabTerrain 44 WidgetConfigTfcCliffSlopeMinus WidgetConfigTfcCliffSlopePlus
  , def SliderTfcMountainSlope SliderTabTerrain 45 WidgetConfigTfcMountainSlopeMinus WidgetConfigTfcMountainSlopePlus
  , def SliderTfcMountainRelief SliderTabTerrain 46 WidgetConfigTfcMountainReliefMinus WidgetConfigTfcMountainReliefPlus
  , def SliderTfcHillSlope SliderTabTerrain 47 WidgetConfigTfcHillSlopeMinus WidgetConfigTfcHillSlopePlus
  , def SliderTfcRollingSlope SliderTabTerrain 48 WidgetConfigTfcRollingSlopeMinus WidgetConfigTfcRollingSlopePlus
  , def SliderValleyCurvature SliderTabTerrain 49 WidgetConfigValleyCurvatureMinus WidgetConfigValleyCurvaturePlus
  , def SliderTfcElevGradient SliderTabTerrain 50 WidgetConfigTfcElevGradientMinus WidgetConfigTfcElevGradientPlus
  , def SliderTfcPlateauMaxRelief2Ring SliderTabTerrain 51 WidgetConfigTfcPlateauMaxRelief2RingMinus WidgetConfigTfcPlateauMaxRelief2RingPlus
  , def SliderTfcPlateauMaxMicroRelief SliderTabTerrain 52 WidgetConfigTfcPlateauMaxMicroReliefMinus WidgetConfigTfcPlateauMaxMicroReliefPlus
  , def SliderTfcRollingNearFactor SliderTabTerrain 53 WidgetConfigTfcRollingNearFactorMinus WidgetConfigTfcRollingNearFactorPlus
  , def SliderRockElevationThreshold SliderTabTerrain 54 WidgetConfigRockElevationThresholdMinus WidgetConfigRockElevationThresholdPlus
  , def SliderRockHardnessThreshold SliderTabTerrain 55 WidgetConfigRockHardnessThresholdMinus WidgetConfigRockHardnessThresholdPlus
  , def SliderRockHardnessSecondary SliderTabTerrain 56 WidgetConfigRockHardnessSecondaryMinus WidgetConfigRockHardnessSecondaryPlus
  , def SliderPlanetRadius SliderTabPlanet 0 WidgetConfigPlanetRadiusMinus WidgetConfigPlanetRadiusPlus
  , def SliderAxialTilt SliderTabPlanet 1 WidgetConfigAxialTiltMinus WidgetConfigAxialTiltPlus
  , def SliderInsolation SliderTabPlanet 2 WidgetConfigInsolationMinus WidgetConfigInsolationPlus
  , def SliderOccWarmScale SliderTabPlanet 3 WidgetConfigOccWarmScaleMinus WidgetConfigOccWarmScalePlus
  , def SliderOccColdScale SliderTabPlanet 4 WidgetConfigOccColdScaleMinus WidgetConfigOccColdScalePlus
  , def SliderOccLatPeakDeg SliderTabPlanet 5 WidgetConfigOccLatPeakDegMinus WidgetConfigOccLatPeakDegPlus
  , def SliderOccLatWidthDeg SliderTabPlanet 6 WidgetConfigOccLatWidthDegMinus WidgetConfigOccLatWidthDegPlus
  , def SliderWaterLevel SliderTabClimate 0 WidgetConfigWaterMinus WidgetConfigWaterPlus
  , def SliderOrographicLift SliderTabClimate 1 WidgetConfigOrographicLiftMinus WidgetConfigOrographicLiftPlus
  , def SliderRainShadowLoss SliderTabClimate 2 WidgetConfigRainShadowLossMinus WidgetConfigRainShadowLossPlus
  , def SliderWindDiffuse SliderTabClimate 3 WidgetConfigWindDiffuseMinus WidgetConfigWindDiffusePlus
  , def SliderEquatorTemp SliderTabClimate 4 WidgetConfigEquatorTempMinus WidgetConfigEquatorTempPlus
  , def SliderPoleTemp SliderTabClimate 5 WidgetConfigPoleTempMinus WidgetConfigPoleTempPlus
  , def SliderLapseRate SliderTabClimate 6 WidgetConfigLapseRateMinus WidgetConfigLapseRatePlus
  , def SliderWindIterations SliderTabClimate 7 WidgetConfigWindIterationsMinus WidgetConfigWindIterationsPlus
  , def SliderMoistureIterations SliderTabClimate 8 WidgetConfigMoistureIterationsMinus WidgetConfigMoistureIterationsPlus
  , def SliderBoundaryMotionTemp SliderTabClimate 9 WidgetConfigBoundaryMotionTempMinus WidgetConfigBoundaryMotionTempPlus
  , def SliderBoundaryMotionPrecip SliderTabClimate 10 WidgetConfigBoundaryMotionPrecipMinus WidgetConfigBoundaryMotionPrecipPlus
  , def SliderSliceLatCenter SliderTabClimate 11 WidgetConfigSliceLatCenterMinus WidgetConfigSliceLatCenterPlus
  , def SliderSliceLonCenter SliderTabClimate 12 WidgetConfigSliceLonCenterMinus WidgetConfigSliceLonCenterPlus
  , def SliderLatitudeExponent SliderTabClimate 13 WidgetConfigLatitudeExponentMinus WidgetConfigLatitudeExponentPlus
  , def SliderPlateHeightCooling SliderTabClimate 14 WidgetConfigPlateHeightCoolingMinus WidgetConfigPlateHeightCoolingPlus
  , def SliderTempNoiseScale SliderTabClimate 15 WidgetConfigTempNoiseScaleMinus WidgetConfigTempNoiseScalePlus
  , def SliderOceanModeration SliderTabClimate 16 WidgetConfigOceanModerationMinus WidgetConfigOceanModerationPlus
  , def SliderOceanModerateTemp SliderTabClimate 17 WidgetConfigOceanModerateTempMinus WidgetConfigOceanModerateTempPlus
  , def SliderAlbedoSensitivity SliderTabClimate 18 WidgetConfigAlbedoSensitivityMinus WidgetConfigAlbedoSensitivityPlus
  , def SliderAlbedoReference SliderTabClimate 19 WidgetConfigAlbedoReferenceMinus WidgetConfigAlbedoReferencePlus
  , def SliderMoistAdvect SliderTabClimate 20 WidgetConfigMoistAdvectMinus WidgetConfigMoistAdvectPlus
  , def SliderMoistLocal SliderTabClimate 21 WidgetConfigMoistLocalMinus WidgetConfigMoistLocalPlus
  , def SliderMoistWindEvapScale SliderTabClimate 22 WidgetConfigMoistWindEvapScaleMinus WidgetConfigMoistWindEvapScalePlus
  , def SliderMoistEvapNoiseScale SliderTabClimate 23 WidgetConfigMoistEvapNoiseScaleMinus WidgetConfigMoistEvapNoiseScalePlus
  , def SliderMoistBareEvapFrac SliderTabClimate 24 WidgetConfigMoistBareEvapFracMinus WidgetConfigMoistBareEvapFracPlus
  , def SliderMoistVegTranspFrac SliderTabClimate 25 WidgetConfigMoistVegTranspFracMinus WidgetConfigMoistVegTranspFracPlus
  , def SliderMoistWindETScale SliderTabClimate 26 WidgetConfigMoistWindETScaleMinus WidgetConfigMoistWindETScalePlus
  , def SliderMoistCondensationRate SliderTabClimate 27 WidgetConfigMoistCondensationRateMinus WidgetConfigMoistCondensationRatePlus
  , def SliderMoistRecycleRate SliderTabClimate 28 WidgetConfigMoistRecycleRateMinus WidgetConfigMoistRecycleRatePlus
  , def SliderMoistITCZStrength SliderTabClimate 29 WidgetConfigMoistITCZStrengthMinus WidgetConfigMoistITCZStrengthPlus
  , def SliderMoistITCZWidth SliderTabClimate 30 WidgetConfigMoistITCZWidthMinus WidgetConfigMoistITCZWidthPlus
  , def SliderOrographicScale SliderTabClimate 31 WidgetConfigOrographicScaleMinus WidgetConfigOrographicScalePlus
  , def SliderOrographicStep SliderTabClimate 32 WidgetConfigOrographicStepMinus WidgetConfigOrographicStepPlus
  , def SliderCoastalIterations SliderTabClimate 33 WidgetConfigCoastalIterationsMinus WidgetConfigCoastalIterationsPlus
  , def SliderCoastalDiffuse SliderTabClimate 34 WidgetConfigCoastalDiffuseMinus WidgetConfigCoastalDiffusePlus
  , def SliderCoastalMoistureBoost SliderTabClimate 35 WidgetConfigCoastalMoistureBoostMinus WidgetConfigCoastalMoistureBoostPlus
  , def SliderWindBeltStrength SliderTabClimate 36 WidgetConfigWindBeltStrengthMinus WidgetConfigWindBeltStrengthPlus
  , def SliderWindBeltHarmonics SliderTabClimate 37 WidgetConfigWindBeltHarmonicsMinus WidgetConfigWindBeltHarmonicsPlus
  , def SliderWindBeltBase SliderTabClimate 38 WidgetConfigWindBeltBaseMinus WidgetConfigWindBeltBasePlus
  , def SliderWindBeltRange SliderTabClimate 39 WidgetConfigWindBeltRangeMinus WidgetConfigWindBeltRangePlus
  , def SliderWindBeltSpeedScale SliderTabClimate 40 WidgetConfigWindBeltSpeedScaleMinus WidgetConfigWindBeltSpeedScalePlus
  , def SliderBndLandRange SliderTabClimate 41 WidgetConfigBndLandRangeMinus WidgetConfigBndLandRangePlus
  , def SliderPiedmontSmooth SliderTabClimate 42 WidgetConfigPiedmontSmoothMinus WidgetConfigPiedmontSmoothPlus
  , def SliderPiedmontSlopeMin SliderTabClimate 43 WidgetConfigPiedmontSlopeMinMinus WidgetConfigPiedmontSlopeMinPlus
  , def SliderPiedmontSlopeMax SliderTabClimate 44 WidgetConfigPiedmontSlopeMaxMinus WidgetConfigPiedmontSlopeMaxPlus
  , def SliderWindCoriolisDeflection SliderTabClimate 45 WidgetConfigWindCoriolisDeflectionMinus WidgetConfigWindCoriolisDeflectionPlus
  , def SliderMoistMinVegFloor SliderTabClimate 46 WidgetConfigMoistMinVegFloorMinus WidgetConfigMoistMinVegFloorPlus
  , def SliderWeatherTick SliderTabWeather 0 WidgetConfigWeatherTickMinus WidgetConfigWeatherTickPlus
  , def SliderWeatherPhase SliderTabWeather 1 WidgetConfigWeatherPhaseMinus WidgetConfigWeatherPhasePlus
  , def SliderWeatherAmplitude SliderTabWeather 2 WidgetConfigWeatherAmplitudeMinus WidgetConfigWeatherAmplitudePlus
  , def SliderSeasonCycleLength SliderTabWeather 3 WidgetConfigSeasonCycleLengthMinus WidgetConfigSeasonCycleLengthPlus
  , def SliderJitterAmplitude SliderTabWeather 4 WidgetConfigJitterAmplitudeMinus WidgetConfigJitterAmplitudePlus
  , def SliderPressureBase SliderTabWeather 5 WidgetConfigPressureBaseMinus WidgetConfigPressureBasePlus
  , def SliderPressureTempScale SliderTabWeather 6 WidgetConfigPressureTempScaleMinus WidgetConfigPressureTempScalePlus
  , def SliderPressureCoriolisScale SliderTabWeather 7 WidgetConfigPressureCoriolisScaleMinus WidgetConfigPressureCoriolisScalePlus
  , def SliderSeasonalBase SliderTabWeather 8 WidgetConfigSeasonalBaseMinus WidgetConfigSeasonalBasePlus
  , def SliderSeasonalRange SliderTabWeather 9 WidgetConfigSeasonalRangeMinus WidgetConfigSeasonalRangePlus
  , def SliderHumidityNoiseScale SliderTabWeather 10 WidgetConfigHumidityNoiseScaleMinus WidgetConfigHumidityNoiseScalePlus
  , def SliderPrecipNoiseScale SliderTabWeather 11 WidgetConfigPrecipNoiseScaleMinus WidgetConfigPrecipNoiseScalePlus
  , def SliderWeatherITCZWidth SliderTabWeather 12 WidgetConfigWeatherITCZWidthMinus WidgetConfigWeatherITCZWidthPlus
  , def SliderWeatherITCZPrecipBoost SliderTabWeather 13 WidgetConfigWeatherITCZPrecipBoostMinus WidgetConfigWeatherITCZPrecipBoostPlus
  , def SliderPressureHumidityScale SliderTabWeather 14 WidgetConfigPressureHumidityScaleMinus WidgetConfigPressureHumidityScalePlus
  , def SliderPressureGradientWindScale SliderTabWeather 15 WidgetConfigPressureGradientWindScaleMinus WidgetConfigPressureGradientWindScalePlus
  , def SliderWindNoiseScale SliderTabWeather 16 WidgetConfigWindNoiseScaleMinus WidgetConfigWindNoiseScalePlus
  , def SliderITCZMigrationScale SliderTabWeather 17 WidgetConfigITCZMigrationScaleMinus WidgetConfigITCZMigrationScalePlus
  , def SliderCloudRHExponent SliderTabWeather 18 WidgetConfigCloudRHExponentMinus WidgetConfigCloudRHExponentPlus
  , def SliderCloudAlbedoEffect SliderTabWeather 19 WidgetConfigCloudAlbedoEffectMinus WidgetConfigCloudAlbedoEffectPlus
  , def SliderCloudPrecipBoost SliderTabWeather 20 WidgetConfigCloudPrecipBoostMinus WidgetConfigCloudPrecipBoostPlus
  , def SliderVegBase SliderTabBiome 0 WidgetConfigVegBaseMinus WidgetConfigVegBasePlus
  , def SliderVegBoost SliderTabBiome 1 WidgetConfigVegBoostMinus WidgetConfigVegBoostPlus
  , def SliderVegTempWeight SliderTabBiome 2 WidgetConfigVegTempWeightMinus WidgetConfigVegTempWeightPlus
  , def SliderVegPrecipWeight SliderTabBiome 3 WidgetConfigVegPrecipWeightMinus WidgetConfigVegPrecipWeightPlus
  , def SliderBtCoastalBand SliderTabBiome 4 WidgetConfigBtCoastalBandMinus WidgetConfigBtCoastalBandPlus
  , def SliderBtSnowMaxTemp SliderTabBiome 5 WidgetConfigBtSnowMaxTempMinus WidgetConfigBtSnowMaxTempPlus
  , def SliderBtAlpineMaxTemp SliderTabBiome 6 WidgetConfigBtAlpineMaxTempMinus WidgetConfigBtAlpineMaxTempPlus
  , def SliderBtIceCapTemp SliderTabBiome 7 WidgetConfigBtIceCapTempMinus WidgetConfigBtIceCapTempPlus
  , def SliderBtMontaneMaxTemp SliderTabBiome 8 WidgetConfigBtMontaneMaxTempMinus WidgetConfigBtMontaneMaxTempPlus
  , def SliderBtMontanePrecip SliderTabBiome 9 WidgetConfigBtMontanePrecipMinus WidgetConfigBtMontanePrecipPlus
  , def SliderBtCliffSlope SliderTabBiome 10 WidgetConfigBtCliffSlopeMinus WidgetConfigBtCliffSlopePlus
  , def SliderBtValleyMoisture SliderTabBiome 11 WidgetConfigBtValleyMoistureMinus WidgetConfigBtValleyMoisturePlus
  , def SliderBtDepressionMoisture SliderTabBiome 12 WidgetConfigBtDepressionMoistureMinus WidgetConfigBtDepressionMoisturePlus
  , def SliderBtPrecipWeight SliderTabBiome 13 WidgetConfigBtPrecipWeightMinus WidgetConfigBtPrecipWeightPlus
  , def SliderVbcTempMin SliderTabBiome 14 WidgetConfigVbcTempMinMinus WidgetConfigVbcTempMinPlus
  , def SliderVbcTempRange SliderTabBiome 15 WidgetConfigVbcTempRangeMinus WidgetConfigVbcTempRangePlus
  , def SliderVbcFertilityBoost SliderTabBiome 16 WidgetConfigVbcFertilityBoostMinus WidgetConfigVbcFertilityBoostPlus
  , def SliderVbcAlbedoBase SliderTabBiome 17 WidgetConfigVbcAlbedoBaseMinus WidgetConfigVbcAlbedoBasePlus
  , def SliderVbcAlbedoBare SliderTabBiome 18 WidgetConfigVbcAlbedoBareMinus WidgetConfigVbcAlbedoBarePlus
  , def SliderVbcAlbedoVeg SliderTabBiome 19 WidgetConfigVbcAlbedoVegMinus WidgetConfigVbcAlbedoVegPlus
  , def SliderVbcOceanAlbedo SliderTabBiome 20 WidgetConfigVbcOceanAlbedoMinus WidgetConfigVbcOceanAlbedoPlus
  , def SliderVbcIceAlbedo SliderTabBiome 21 WidgetConfigVbcIceAlbedoMinus WidgetConfigVbcIceAlbedoPlus
  , def SliderBiomeSmoothing SliderTabBiome 22 WidgetConfigBiomeSmoothingMinus WidgetConfigBiomeSmoothingPlus
  , def SliderVolcanicAshBoost SliderTabBiome 23 WidgetConfigVolcanicAshBoostMinus WidgetConfigVolcanicAshBoostPlus
  , def SliderVolcanicLavaPenalty SliderTabBiome 24 WidgetConfigVolcanicLavaPenaltyMinus WidgetConfigVolcanicLavaPenaltyPlus
  , def SliderBiomeFeedbackBlend SliderTabBiome 25 WidgetConfigBiomeFeedbackBlendMinus WidgetConfigBiomeFeedbackBlendPlus
  , def SliderErosionHydraulic SliderTabErosion 0 WidgetConfigErosionHydraulicMinus WidgetConfigErosionHydraulicPlus
  , def SliderErosionThermal SliderTabErosion 1 WidgetConfigErosionThermalMinus WidgetConfigErosionThermalPlus
  , def SliderErosionRainRate SliderTabErosion 2 WidgetConfigErosionRainRateMinus WidgetConfigErosionRainRatePlus
  , def SliderErosionTalus SliderTabErosion 3 WidgetConfigErosionTalusMinus WidgetConfigErosionTalusPlus
  , def SliderErosionMaxDrop SliderTabErosion 4 WidgetConfigErosionMaxDropMinus WidgetConfigErosionMaxDropPlus
  , def SliderErosionHydDeposit SliderTabErosion 5 WidgetConfigErosionHydDepositMinus WidgetConfigErosionHydDepositPlus
  , def SliderErosionDepositSlope SliderTabErosion 6 WidgetConfigErosionDepositSlopeMinus WidgetConfigErosionDepositSlopePlus
  , def SliderErosionThermDeposit SliderTabErosion 7 WidgetConfigErosionThermDepositMinus WidgetConfigErosionThermDepositPlus
  , def SliderErosionCoastZone SliderTabErosion 8 WidgetConfigErosionCoastZoneMinus WidgetConfigErosionCoastZonePlus
  , def SliderErosionCoastStrength SliderTabErosion 9 WidgetConfigErosionCoastStrengthMinus WidgetConfigErosionCoastStrengthPlus
  , def SliderErosionCoastIter SliderTabErosion 10 WidgetConfigErosionCoastIterMinus WidgetConfigErosionCoastIterPlus
  , def SliderHypsometryEnabled SliderTabErosion 11 WidgetConfigHypsometryEnabledMinus WidgetConfigHypsometryEnabledPlus
  , def SliderHypsometryLowlandExp SliderTabErosion 12 WidgetConfigHypsometryLowlandExpMinus WidgetConfigHypsometryLowlandExpPlus
  , def SliderHypsometryHighlandExp SliderTabErosion 13 WidgetConfigHypsometryHighlandExpMinus WidgetConfigHypsometryHighlandExpPlus
  , def SliderHypsometryPlateauBreak SliderTabErosion 14 WidgetConfigHypsometryPlateauBreakMinus WidgetConfigHypsometryPlateauBreakPlus
  , def SliderHypsometryOceanExp SliderTabErosion 15 WidgetConfigHypsometryOceanExpMinus WidgetConfigHypsometryOceanExpPlus
  , def SliderHypsometryCoastalRampWidth SliderTabErosion 16 WidgetConfigHypsometryCoastalRampWidthMinus WidgetConfigHypsometryCoastalRampWidthPlus
  , def SliderHypsometryCoastalRampStr SliderTabErosion 17 WidgetConfigHypsometryCoastalRampStrMinus WidgetConfigHypsometryCoastalRampStrPlus
  , def SliderGlacierSnowTemp SliderTabErosion 18 WidgetConfigGlacierSnowTempMinus WidgetConfigGlacierSnowTempPlus
  , def SliderGlacierSnowRange SliderTabErosion 19 WidgetConfigGlacierSnowRangeMinus WidgetConfigGlacierSnowRangePlus
  , def SliderGlacierMeltTemp SliderTabErosion 20 WidgetConfigGlacierMeltTempMinus WidgetConfigGlacierMeltTempPlus
  , def SliderGlacierMeltRate SliderTabErosion 21 WidgetConfigGlacierMeltRateMinus WidgetConfigGlacierMeltRatePlus
  , def SliderGlacierAccumScale SliderTabErosion 22 WidgetConfigGlacierAccumScaleMinus WidgetConfigGlacierAccumScalePlus
  , def SliderGlacierFlowIters SliderTabErosion 23 WidgetConfigGlacierFlowItersMinus WidgetConfigGlacierFlowItersPlus
  , def SliderGlacierFlowRate SliderTabErosion 24 WidgetConfigGlacierFlowRateMinus WidgetConfigGlacierFlowRatePlus
  , def SliderGlacierErosionScale SliderTabErosion 25 WidgetConfigGlacierErosionScaleMinus WidgetConfigGlacierErosionScalePlus
  , def SliderGlacierCarveScale SliderTabErosion 26 WidgetConfigGlacierCarveScaleMinus WidgetConfigGlacierCarveScalePlus
  , def SliderGlacierDepositScale SliderTabErosion 27 WidgetConfigGlacierDepositScaleMinus WidgetConfigGlacierDepositScalePlus
  , def SliderVentDensity SliderTabErosion 28 WidgetConfigVentDensityMinus WidgetConfigVentDensityPlus
  , def SliderVentThreshold SliderTabErosion 29 WidgetConfigVentThresholdMinus WidgetConfigVentThresholdPlus
  , def SliderHotspotScale SliderTabErosion 30 WidgetConfigHotspotScaleMinus WidgetConfigHotspotScalePlus
  , def SliderHotspotThreshold SliderTabErosion 31 WidgetConfigHotspotThresholdMinus WidgetConfigHotspotThresholdPlus
  , def SliderMagmaRecharge SliderTabErosion 32 WidgetConfigMagmaRechargeMinus WidgetConfigMagmaRechargePlus
  , def SliderLavaScale SliderTabErosion 33 WidgetConfigLavaScaleMinus WidgetConfigLavaScalePlus
  , def SliderAshScale SliderTabErosion 34 WidgetConfigAshScaleMinus WidgetConfigAshScalePlus
  , def SliderVolcanicDepositScale SliderTabErosion 35 WidgetConfigVolcanicDepositScaleMinus WidgetConfigVolcanicDepositScalePlus
  , def SliderSoilMoistureThreshold SliderTabErosion 36 WidgetConfigSoilMoistureThresholdMinus WidgetConfigSoilMoistureThresholdPlus
  , def SliderSoilHardnessThreshold SliderTabErosion 37 WidgetConfigSoilHardnessThresholdMinus WidgetConfigSoilHardnessThresholdPlus
  , def SliderSoilFertilityMoistWeight SliderTabErosion 38 WidgetConfigSoilFertilityMoistWeightMinus WidgetConfigSoilFertilityMoistWeightPlus
  , def SliderSoilFertilityDepthWeight SliderTabErosion 39 WidgetConfigSoilFertilityDepthWeightMinus WidgetConfigSoilFertilityDepthWeightPlus
  , def SliderSinkBreachDepth SliderTabErosion 40 WidgetConfigSinkBreachDepthMinus WidgetConfigSinkBreachDepthPlus
  , def SliderStreamPowerMaxErosion SliderTabErosion 41 WidgetConfigStreamPowerMaxErosionMinus WidgetConfigStreamPowerMaxErosionPlus
  , def SliderRiverCarveMaxDepth SliderTabErosion 42 WidgetConfigRiverCarveMaxDepthMinus WidgetConfigRiverCarveMaxDepthPlus
  , def SliderCoastalErodeStrength SliderTabErosion 43 WidgetConfigCoastalErodeStrengthMinus WidgetConfigCoastalErodeStrengthPlus
  , def SliderHydroHardnessWeight SliderTabErosion 44 WidgetConfigHydroHardnessWeightMinus WidgetConfigHydroHardnessWeightPlus
  , def SliderMinLakeSize SliderTabErosion 45 WidgetConfigMinLakeSizeMinus WidgetConfigMinLakeSizePlus
  , def SliderInlandSeaMinSize SliderTabErosion 46 WidgetConfigInlandSeaMinSizeMinus WidgetConfigInlandSeaMinSizePlus
  , def SliderRoughnessScale SliderTabErosion 47 WidgetConfigRoughnessScaleMinus WidgetConfigRoughnessScalePlus
  ]

sliderDefsForTab :: SliderTab -> [SliderDef]
sliderDefsForTab tab = filter ((== tab) . sliderTab) allSliderDefs

-- | Number of layout rows consumed by slider definitions in a tab.
sliderRowCountForTab :: SliderTab -> Int
sliderRowCountForTab = length . sliderDefsForTab

sliderDefaultValueForId :: SliderId -> Float
sliderDefaultValueForId sliderIdValue = case sliderIdValue of
  SliderGenScale -> 0.4444
  SliderGenCoordScale -> 0.3333
  SliderGenOffsetX -> 0.5
  SliderGenOffsetY -> 0.5
  SliderGenFrequency -> 0.1837
  SliderGenOctaves -> 0.5
  SliderGenLacunarity -> 0.25
  SliderGenGain -> 0.4
  SliderGenWarpScale -> 0.3333
  SliderGenWarpStrength -> 0.5556
  SliderExtentX -> 0.125
  SliderExtentY -> 0.125
  SliderEdgeNorth -> 0.0
  SliderEdgeSouth -> 0.0
  SliderEdgeEast -> 0.0
  SliderEdgeWest -> 0.0
  SliderEdgeFalloff -> 0.0
  SliderPlateSize -> 0.45
  SliderUplift -> 0.3
  SliderRiftDepth -> 0.35
  SliderDetailScale -> 0.5
  SliderPlateSpeed -> 0.38
  SliderBoundarySharpness -> 0.35
  SliderBoundaryNoiseScale -> 0.33
  SliderBoundaryNoiseStrength -> 0.45
  SliderBoundaryWarpOctaves -> 0.5
  SliderBoundaryWarpLacunarity -> 0.25
  SliderBoundaryWarpGain -> 0.4
  SliderPlateMergeScale -> 0.3
  SliderPlateMergeBias -> 0.44
  SliderPlateDetailScale -> 0.33
  SliderPlateDetailStrength -> 0.35
  SliderPlateRidgeStrength -> 0.25
  SliderPlateHeightBase -> 0.62
  SliderPlateHeightVariance -> 0.65
  SliderPlateHardnessBase -> 0.42
  SliderPlateHardnessVariance -> 0.4
  SliderTrenchDepth -> 0.38
  SliderRidgeHeight -> 0.33
  SliderPlateBiasStrength -> 0.42
  SliderPlateBiasCenter -> 0.5
  SliderPlateBiasEdge -> 0.5
  SliderPlateBiasNorth -> 0.5
  SliderPlateBiasSouth -> 0.5
  SliderTfcCliffSlope -> 0.2222
  SliderTfcMountainSlope -> 0.2222
  SliderTfcMountainRelief -> 0.2143
  SliderTfcHillSlope -> 0.0316
  SliderTfcRollingSlope -> 0.0526
  SliderValleyCurvature -> 0.2857
  SliderTfcElevGradient -> 0.2495
  SliderTfcPlateauMaxRelief2Ring -> 0.2632
  SliderTfcPlateauMaxMicroRelief -> 0.5
  SliderTfcRollingNearFactor -> 0.7
  SliderRockElevationThreshold -> 0.5714
  SliderRockHardnessThreshold -> 0.5714
  SliderRockHardnessSecondary -> 0.5
  SliderPlanetRadius -> 0.3333
  SliderAxialTilt -> 0.5209
  SliderInsolation -> 0.5
  SliderOccWarmScale -> 0.3
  SliderOccColdScale -> 0.2
  SliderOccLatPeakDeg -> 0.5833
  SliderOccLatWidthDeg -> 0.5
  SliderWaterLevel -> 0.5
  SliderOrographicLift -> 0.35
  SliderRainShadowLoss -> 0.08
  SliderWindDiffuse -> 0.5
  SliderEquatorTemp -> 0.78
  SliderPoleTemp -> 0.0
  SliderLapseRate -> 0.65
  SliderWindIterations -> 0.5
  SliderMoistureIterations -> 0.486
  SliderBoundaryMotionTemp -> 0.5
  SliderBoundaryMotionPrecip -> 0.5
  SliderSliceLatCenter -> 0.5
  SliderSliceLonCenter -> 0.5
  SliderLatitudeExponent -> 0.615
  SliderPlateHeightCooling -> 0.25
  SliderTempNoiseScale -> 0.33
  SliderOceanModeration -> 0.3
  SliderOceanModerateTemp -> 0.5
  SliderAlbedoSensitivity -> 0.2
  SliderAlbedoReference -> 0.6
  SliderMoistAdvect -> 0.85
  SliderMoistLocal -> 0.15
  SliderMoistWindEvapScale -> 0.3
  SliderMoistEvapNoiseScale -> 0.25
  SliderMoistBareEvapFrac -> 0.15
  SliderMoistVegTranspFrac -> 0.85
  SliderMoistWindETScale -> 0.2
  SliderMoistCondensationRate -> 0.2
  SliderMoistRecycleRate -> 0.35
  SliderMoistITCZStrength -> 0.3
  SliderMoistITCZWidth -> 0.333
  SliderOrographicScale -> 0.3
  SliderOrographicStep -> 0.2
  SliderCoastalIterations -> 0.5
  SliderCoastalDiffuse -> 0.5
  SliderCoastalMoistureBoost -> 0.4
  SliderWindBeltStrength -> 0.6
  SliderWindBeltHarmonics -> 0.4
  SliderWindBeltBase -> 0.4
  SliderWindBeltRange -> 0.6
  SliderWindBeltSpeedScale -> 0.6
  SliderBndLandRange -> 0.357
  SliderPiedmontSmooth -> 0.4167
  SliderPiedmontSlopeMin -> 0.2857
  SliderPiedmontSlopeMax -> 0.35
  SliderWindCoriolisDeflection -> 0.287
  SliderMoistMinVegFloor -> 0.15
  SliderWeatherTick -> 0.2
  SliderWeatherPhase -> 0.0
  SliderWeatherAmplitude -> 0.3
  SliderSeasonCycleLength -> 0.4786
  SliderJitterAmplitude -> 0.36
  SliderPressureBase -> 0.5714
  SliderPressureTempScale -> 0.4
  SliderPressureCoriolisScale -> 0.2
  SliderSeasonalBase -> 0.4
  SliderSeasonalRange -> 0.6
  SliderHumidityNoiseScale -> 0.3333
  SliderPrecipNoiseScale -> 0.3
  SliderWeatherITCZWidth -> 0.4444
  SliderWeatherITCZPrecipBoost -> 0.3
  SliderPressureHumidityScale -> 0.2
  SliderPressureGradientWindScale -> 0.3
  SliderWindNoiseScale -> 0.3333
  SliderITCZMigrationScale -> 0.4667
  SliderCloudRHExponent -> 0.4
  SliderCloudAlbedoEffect -> 0.2667
  SliderCloudPrecipBoost -> 0.24
  SliderVegBase -> 0.2
  SliderVegBoost -> 0.6
  SliderVegTempWeight -> 0.6
  SliderVegPrecipWeight -> 0.4
  SliderBtCoastalBand -> 0.3
  SliderBtSnowMaxTemp -> 0.4
  SliderBtAlpineMaxTemp -> 0.42
  SliderBtIceCapTemp -> 0.25
  SliderBtMontaneMaxTemp -> 0.58
  SliderBtMontanePrecip -> 0.5
  SliderBtCliffSlope -> 0.3333
  SliderBtValleyMoisture -> 0.5714
  SliderBtDepressionMoisture -> 0.5
  SliderBtPrecipWeight -> 0.3333
  SliderVbcTempMin -> 0.2667
  SliderVbcTempRange -> 0.4444
  SliderVbcFertilityBoost -> 0.5
  SliderVbcAlbedoBase -> 0.5
  SliderVbcAlbedoBare -> 0.375
  SliderVbcAlbedoVeg -> 0.3333
  SliderVbcOceanAlbedo -> 0.3
  SliderVbcIceAlbedo -> 0.7143
  SliderBiomeSmoothing -> 0.2
  SliderVolcanicAshBoost -> 0.4
  SliderVolcanicLavaPenalty -> 0.4375
  SliderBiomeFeedbackBlend -> 0.85
  SliderErosionHydraulic -> 0.5
  SliderErosionThermal -> 0.4
  SliderErosionRainRate -> 0.2
  SliderErosionTalus -> 0.5
  SliderErosionMaxDrop -> 0.5
  SliderErosionHydDeposit -> 0.375
  SliderErosionDepositSlope -> 0.357
  SliderErosionThermDeposit -> 0.5
  SliderErosionCoastZone -> 0.286
  SliderErosionCoastStrength -> 0.375
  SliderErosionCoastIter -> 0.429
  SliderHypsometryEnabled -> 1.0
  SliderHypsometryLowlandExp -> 0.2
  SliderHypsometryHighlandExp -> 0.333
  SliderHypsometryPlateauBreak -> 0.348
  SliderHypsometryOceanExp -> 0.5
  SliderHypsometryCoastalRampWidth -> 0.467
  SliderHypsometryCoastalRampStr -> 0.6
  SliderGlacierSnowTemp -> 0.5
  SliderGlacierSnowRange -> 0.417
  SliderGlacierMeltTemp -> 0.429
  SliderGlacierMeltRate -> 0.2
  SliderGlacierAccumScale -> 0.333
  SliderGlacierFlowIters -> 0.3
  SliderGlacierFlowRate -> 0.2
  SliderGlacierErosionScale -> 0.25
  SliderGlacierCarveScale -> 0.1
  SliderGlacierDepositScale -> 0.2
  SliderVentDensity -> 0.25
  SliderVentThreshold -> 0.5
  SliderHotspotScale -> 0.5
  SliderHotspotThreshold -> 0.615
  SliderMagmaRecharge -> 0.333
  SliderLavaScale -> 0.6
  SliderAshScale -> 0.4
  SliderVolcanicDepositScale -> 0.8
  SliderSoilMoistureThreshold -> 0.7
  SliderSoilHardnessThreshold -> 0.45
  SliderSoilFertilityMoistWeight -> 0.6
  SliderSoilFertilityDepthWeight -> 0.4
  SliderSinkBreachDepth -> 0.2
  SliderStreamPowerMaxErosion -> 0.25
  SliderRiverCarveMaxDepth -> 0.25
  SliderCoastalErodeStrength -> 0.2
  SliderHydroHardnessWeight -> 0.7
  SliderMinLakeSize -> 0.061
  SliderInlandSeaMinSize -> 0.333
  SliderRoughnessScale -> 0.375

sliderDefForWidget :: WidgetId -> Maybe SliderDef
sliderDefForWidget widgetId =
  find
    (\sliderDef ->
      widgetId == sliderMinusWidgetId sliderDef
        || widgetId == sliderPlusWidgetId sliderDef
    )
    allSliderDefs

sliderWidgetPart :: WidgetId -> Maybe (SliderDef, SliderPart)
sliderWidgetPart widgetId = do
  sliderDef <- sliderDefForWidget widgetId
  if widgetId == sliderMinusWidgetId sliderDef
    then Just (sliderDef, SliderPartMinus)
    else if widgetId == sliderPlusWidgetId sliderDef
      then Just (sliderDef, SliderPartPlus)
      else Nothing

def :: SliderId -> SliderTab -> Int -> WidgetId -> WidgetId -> SliderDef
def sliderDefId tab rowIndex minusWidget plusWidget =
  SliderDef
    { sliderId = sliderDefId
    , sliderTab = tab
    , sliderRowIndex = rowIndex
    , sliderMinusWidgetId = minusWidget
    , sliderPlusWidgetId = plusWidget
    }