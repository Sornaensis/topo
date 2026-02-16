module UI.WidgetTree
  ( WidgetId(..)
  , Widget(..)
  , buildWidgets
  , buildSliderRowWidgets
  , hitTest
  ) where

import Linear (V2(..))
import UI.Layout
import UI.Widgets (Rect(..), containsPoint)

data WidgetId
  = WidgetGenerate
  | WidgetLeftToggle
  | WidgetLeftTabTopo
  | WidgetLeftTabView
  | WidgetSeedValue
  | WidgetSeedRandom
  | WidgetChunkMinus
  | WidgetChunkPlus
  | WidgetConfigToggle
  | WidgetConfigTabTerrain
  | WidgetConfigTabPlanet
  | WidgetConfigTabClimate
  | WidgetConfigTabWeather
  | WidgetConfigTabBiome
  | WidgetConfigTabErosion
  | WidgetConfigPresetSave
  | WidgetConfigPresetLoad
  | WidgetConfigReset
  | WidgetConfigRevert
  | WidgetConfigWaterMinus
  | WidgetConfigWaterPlus
  | WidgetConfigEvapMinus
  | WidgetConfigEvapPlus
  | WidgetConfigRainShadowMinus
  | WidgetConfigRainShadowPlus
  | WidgetConfigWindDiffuseMinus
  | WidgetConfigWindDiffusePlus
  | WidgetConfigEquatorTempMinus
  | WidgetConfigEquatorTempPlus
  | WidgetConfigPoleTempMinus
  | WidgetConfigPoleTempPlus
  | WidgetConfigLapseRateMinus
  | WidgetConfigLapseRatePlus
  | WidgetConfigWindIterationsMinus
  | WidgetConfigWindIterationsPlus
  | WidgetConfigMoistureIterationsMinus
  | WidgetConfigMoistureIterationsPlus
  | WidgetConfigWeatherTickMinus
  | WidgetConfigWeatherTickPlus
  | WidgetConfigWeatherPhaseMinus
  | WidgetConfigWeatherPhasePlus
  | WidgetConfigWeatherAmplitudeMinus
  | WidgetConfigWeatherAmplitudePlus
  | WidgetConfigSeasonCycleLengthMinus
  | WidgetConfigSeasonCycleLengthPlus
  | WidgetConfigJitterAmplitudeMinus
  | WidgetConfigJitterAmplitudePlus
  | WidgetConfigPressureBaseMinus
  | WidgetConfigPressureBasePlus
  | WidgetConfigPressureTempScaleMinus
  | WidgetConfigPressureTempScalePlus
  | WidgetConfigPressureCoriolisScaleMinus
  | WidgetConfigPressureCoriolisScalePlus
  | WidgetConfigSeasonalBaseMinus
  | WidgetConfigSeasonalBasePlus
  | WidgetConfigSeasonalRangeMinus
  | WidgetConfigSeasonalRangePlus
  | WidgetConfigHumidityNoiseScaleMinus
  | WidgetConfigHumidityNoiseScalePlus
  | WidgetConfigPrecipNoiseScaleMinus
  | WidgetConfigPrecipNoiseScalePlus
  | WidgetConfigWeatherITCZWidthMinus
  | WidgetConfigWeatherITCZWidthPlus
  | WidgetConfigWeatherITCZPrecipBoostMinus
  | WidgetConfigWeatherITCZPrecipBoostPlus
  | WidgetConfigPressureHumidityScaleMinus
  | WidgetConfigPressureHumidityScalePlus
  | WidgetConfigPressureGradientWindScaleMinus
  | WidgetConfigPressureGradientWindScalePlus
  | WidgetConfigWindNoiseScaleMinus
  | WidgetConfigWindNoiseScalePlus
  | WidgetConfigITCZMigrationScaleMinus
  | WidgetConfigITCZMigrationScalePlus
  | WidgetConfigCloudRHExponentMinus
  | WidgetConfigCloudRHExponentPlus
  | WidgetConfigCloudAlbedoEffectMinus
  | WidgetConfigCloudAlbedoEffectPlus
  | WidgetConfigCloudPrecipBoostMinus
  | WidgetConfigCloudPrecipBoostPlus
  | WidgetConfigVegBaseMinus
  | WidgetConfigVegBasePlus
  | WidgetConfigVegBoostMinus
  | WidgetConfigVegBoostPlus
  | WidgetConfigVegTempWeightMinus
  | WidgetConfigVegTempWeightPlus
  | WidgetConfigVegPrecipWeightMinus
  | WidgetConfigVegPrecipWeightPlus
  | WidgetConfigBtCoastalBandMinus
  | WidgetConfigBtCoastalBandPlus
  | WidgetConfigBtSnowMaxTempMinus
  | WidgetConfigBtSnowMaxTempPlus
  | WidgetConfigBtAlpineMaxTempMinus
  | WidgetConfigBtAlpineMaxTempPlus
  | WidgetConfigBtIceCapTempMinus
  | WidgetConfigBtIceCapTempPlus
  | WidgetConfigBtMontaneMaxTempMinus
  | WidgetConfigBtMontaneMaxTempPlus
  | WidgetConfigBtMontanePrecipMinus
  | WidgetConfigBtMontanePrecipPlus
  | WidgetConfigBtCliffSlopeMinus
  | WidgetConfigBtCliffSlopePlus
  | WidgetConfigBtValleyMoistureMinus
  | WidgetConfigBtValleyMoisturePlus
  | WidgetConfigBtDepressionMoistureMinus
  | WidgetConfigBtDepressionMoisturePlus
  | WidgetConfigBtPrecipWeightMinus
  | WidgetConfigBtPrecipWeightPlus
  | WidgetConfigVbcTempMinMinus
  | WidgetConfigVbcTempMinPlus
  | WidgetConfigVbcTempRangeMinus
  | WidgetConfigVbcTempRangePlus
  | WidgetConfigVbcFertilityBoostMinus
  | WidgetConfigVbcFertilityBoostPlus
  | WidgetConfigVbcAlbedoBaseMinus
  | WidgetConfigVbcAlbedoBasePlus
  | WidgetConfigVbcAlbedoBareMinus
  | WidgetConfigVbcAlbedoBarePlus
  | WidgetConfigVbcAlbedoVegMinus
  | WidgetConfigVbcAlbedoVegPlus
  | WidgetConfigVbcOceanAlbedoMinus
  | WidgetConfigVbcOceanAlbedoPlus
  | WidgetConfigVbcIceAlbedoMinus
  | WidgetConfigVbcIceAlbedoPlus
  | WidgetConfigBiomeSmoothingMinus
  | WidgetConfigBiomeSmoothingPlus
  | WidgetConfigVolcanicAshBoostMinus
  | WidgetConfigVolcanicAshBoostPlus
  | WidgetConfigVolcanicLavaPenaltyMinus
  | WidgetConfigVolcanicLavaPenaltyPlus
  | WidgetConfigBiomeFeedbackBlendMinus
  | WidgetConfigBiomeFeedbackBlendPlus
  | WidgetConfigBoundaryMotionTempMinus
  | WidgetConfigBoundaryMotionTempPlus
  | WidgetConfigBoundaryMotionPrecipMinus
  | WidgetConfigBoundaryMotionPrecipPlus
  | WidgetConfigPlanetRadiusMinus
  | WidgetConfigPlanetRadiusPlus
  | WidgetConfigAxialTiltMinus
  | WidgetConfigAxialTiltPlus
  | WidgetConfigInsolationMinus
  | WidgetConfigInsolationPlus
  | WidgetConfigOccWarmScaleMinus
  | WidgetConfigOccWarmScalePlus
  | WidgetConfigOccColdScaleMinus
  | WidgetConfigOccColdScalePlus
  | WidgetConfigOccLatPeakDegMinus
  | WidgetConfigOccLatPeakDegPlus
  | WidgetConfigOccLatWidthDegMinus
  | WidgetConfigOccLatWidthDegPlus
  | WidgetConfigSliceLatCenterMinus
  | WidgetConfigSliceLatCenterPlus
  | WidgetConfigSliceLonCenterMinus
  | WidgetConfigSliceLonCenterPlus
  | WidgetConfigLatitudeExponentMinus
  | WidgetConfigLatitudeExponentPlus
  | WidgetConfigPlateHeightCoolingMinus
  | WidgetConfigPlateHeightCoolingPlus
  | WidgetConfigTempNoiseScaleMinus
  | WidgetConfigTempNoiseScalePlus
  | WidgetConfigOceanModerationMinus
  | WidgetConfigOceanModerationPlus
  | WidgetConfigOceanModerateTempMinus
  | WidgetConfigOceanModerateTempPlus
  | WidgetConfigAlbedoSensitivityMinus
  | WidgetConfigAlbedoSensitivityPlus
  | WidgetConfigAlbedoReferenceMinus
  | WidgetConfigAlbedoReferencePlus
  | WidgetConfigMoistAdvectMinus
  | WidgetConfigMoistAdvectPlus
  | WidgetConfigMoistLocalMinus
  | WidgetConfigMoistLocalPlus
  | WidgetConfigMoistWindEvapScaleMinus
  | WidgetConfigMoistWindEvapScalePlus
  | WidgetConfigMoistEvapNoiseScaleMinus
  | WidgetConfigMoistEvapNoiseScalePlus
  | WidgetConfigMoistLandETCoeffMinus
  | WidgetConfigMoistLandETCoeffPlus
  | WidgetConfigMoistBareEvapFracMinus
  | WidgetConfigMoistBareEvapFracPlus
  | WidgetConfigMoistVegTranspFracMinus
  | WidgetConfigMoistVegTranspFracPlus
  | WidgetConfigMoistWindETScaleMinus
  | WidgetConfigMoistWindETScalePlus
  | WidgetConfigMoistCondensationRateMinus
  | WidgetConfigMoistCondensationRatePlus
  | WidgetConfigMoistRecycleRateMinus
  | WidgetConfigMoistRecycleRatePlus
  | WidgetConfigMoistITCZStrengthMinus
  | WidgetConfigMoistITCZStrengthPlus
  | WidgetConfigMoistITCZWidthMinus
  | WidgetConfigMoistITCZWidthPlus
  | WidgetConfigOrographicScaleMinus
  | WidgetConfigOrographicScalePlus
  | WidgetConfigOrographicStepMinus
  | WidgetConfigOrographicStepPlus
  | WidgetConfigCoastalIterationsMinus
  | WidgetConfigCoastalIterationsPlus
  | WidgetConfigCoastalDiffuseMinus
  | WidgetConfigCoastalDiffusePlus
  | WidgetConfigCoastalMoistureBoostMinus
  | WidgetConfigCoastalMoistureBoostPlus
  | WidgetConfigWindBeltStrengthMinus
  | WidgetConfigWindBeltStrengthPlus
  | WidgetConfigWindBeltHarmonicsMinus
  | WidgetConfigWindBeltHarmonicsPlus
  | WidgetConfigWindBeltBaseMinus
  | WidgetConfigWindBeltBasePlus
  | WidgetConfigWindBeltRangeMinus
  | WidgetConfigWindBeltRangePlus
  | WidgetConfigWindBeltSpeedScaleMinus
  | WidgetConfigWindBeltSpeedScalePlus
  | WidgetConfigBndLandRangeMinus
  | WidgetConfigBndLandRangePlus
  | WidgetConfigBndTempConvergentMinus
  | WidgetConfigBndTempConvergentPlus
  | WidgetConfigBndTempDivergentMinus
  | WidgetConfigBndTempDivergentPlus
  | WidgetConfigBndTempTransformMinus
  | WidgetConfigBndTempTransformPlus
  | WidgetConfigBndPrecipConvergentMinus
  | WidgetConfigBndPrecipConvergentPlus
  | WidgetConfigBndPrecipDivergentMinus
  | WidgetConfigBndPrecipDivergentPlus
  | WidgetConfigBndPrecipTransformMinus
  | WidgetConfigBndPrecipTransformPlus
  | WidgetConfigErosionHydraulicMinus
  | WidgetConfigErosionHydraulicPlus
  | WidgetConfigErosionThermalMinus
  | WidgetConfigErosionThermalPlus
  | WidgetConfigErosionRainRateMinus
  | WidgetConfigErosionRainRatePlus
  | WidgetConfigErosionTalusMinus
  | WidgetConfigErosionTalusPlus
  | WidgetConfigErosionMaxDropMinus
  | WidgetConfigErosionMaxDropPlus
  | WidgetConfigGlacierSnowTempMinus
  | WidgetConfigGlacierSnowTempPlus
  | WidgetConfigGlacierSnowRangeMinus
  | WidgetConfigGlacierSnowRangePlus
  | WidgetConfigGlacierMeltTempMinus
  | WidgetConfigGlacierMeltTempPlus
  | WidgetConfigGlacierMeltRateMinus
  | WidgetConfigGlacierMeltRatePlus
  | WidgetConfigGlacierAccumScaleMinus
  | WidgetConfigGlacierAccumScalePlus
  | WidgetConfigGlacierFlowItersMinus
  | WidgetConfigGlacierFlowItersPlus
  | WidgetConfigGlacierFlowRateMinus
  | WidgetConfigGlacierFlowRatePlus
  | WidgetConfigGlacierErosionScaleMinus
  | WidgetConfigGlacierErosionScalePlus
  | WidgetConfigGlacierCarveScaleMinus
  | WidgetConfigGlacierCarveScalePlus
  | WidgetConfigGlacierDepositScaleMinus
  | WidgetConfigGlacierDepositScalePlus
  | WidgetConfigVentDensityMinus
  | WidgetConfigVentDensityPlus
  | WidgetConfigVentThresholdMinus
  | WidgetConfigVentThresholdPlus
  | WidgetConfigHotspotScaleMinus
  | WidgetConfigHotspotScalePlus
  | WidgetConfigHotspotThresholdMinus
  | WidgetConfigHotspotThresholdPlus
  | WidgetConfigMagmaRechargeMinus
  | WidgetConfigMagmaRechargePlus
  | WidgetConfigLavaScaleMinus
  | WidgetConfigLavaScalePlus
  | WidgetConfigAshScaleMinus
  | WidgetConfigAshScalePlus
  | WidgetConfigVolcanicDepositScaleMinus
  | WidgetConfigVolcanicDepositScalePlus
  | WidgetConfigSoilMoistureThresholdMinus
  | WidgetConfigSoilMoistureThresholdPlus
  | WidgetConfigSoilHardnessThresholdMinus
  | WidgetConfigSoilHardnessThresholdPlus
  | WidgetConfigSoilFertilityMoistWeightMinus
  | WidgetConfigSoilFertilityMoistWeightPlus
  | WidgetConfigSoilFertilityDepthWeightMinus
  | WidgetConfigSoilFertilityDepthWeightPlus
  | WidgetConfigSinkBreachDepthMinus
  | WidgetConfigSinkBreachDepthPlus
  | WidgetConfigStreamPowerMaxErosionMinus
  | WidgetConfigStreamPowerMaxErosionPlus
  | WidgetConfigRiverCarveMaxDepthMinus
  | WidgetConfigRiverCarveMaxDepthPlus
  | WidgetConfigCoastalErodeStrengthMinus
  | WidgetConfigCoastalErodeStrengthPlus
  | WidgetConfigHydroHardnessWeightMinus
  | WidgetConfigHydroHardnessWeightPlus
  | WidgetConfigMinLakeSizeMinus
  | WidgetConfigMinLakeSizePlus
  | WidgetConfigInlandSeaMinSizeMinus
  | WidgetConfigInlandSeaMinSizePlus
  | WidgetConfigRoughnessScaleMinus
  | WidgetConfigRoughnessScalePlus
  | WidgetConfigGenScaleMinus
  | WidgetConfigGenScalePlus
  | WidgetConfigGenCoordScaleMinus
  | WidgetConfigGenCoordScalePlus
  | WidgetConfigGenOffsetXMinus
  | WidgetConfigGenOffsetXPlus
  | WidgetConfigGenOffsetYMinus
  | WidgetConfigGenOffsetYPlus
  | WidgetConfigGenFrequencyMinus
  | WidgetConfigGenFrequencyPlus
  | WidgetConfigGenOctavesMinus
  | WidgetConfigGenOctavesPlus
  | WidgetConfigGenLacunarityMinus
  | WidgetConfigGenLacunarityPlus
  | WidgetConfigGenGainMinus
  | WidgetConfigGenGainPlus
  | WidgetConfigGenWarpScaleMinus
  | WidgetConfigGenWarpScalePlus
  | WidgetConfigGenWarpStrengthMinus
  | WidgetConfigGenWarpStrengthPlus
  | WidgetConfigExtentXMinus
  | WidgetConfigExtentXPlus
  | WidgetConfigExtentYMinus
  | WidgetConfigExtentYPlus
  | WidgetConfigEdgeNorthMinus
  | WidgetConfigEdgeNorthPlus
  | WidgetConfigEdgeSouthMinus
  | WidgetConfigEdgeSouthPlus
  | WidgetConfigEdgeEastMinus
  | WidgetConfigEdgeEastPlus
  | WidgetConfigEdgeWestMinus
  | WidgetConfigEdgeWestPlus
  | WidgetConfigEdgeFalloffMinus
  | WidgetConfigEdgeFalloffPlus
  | WidgetConfigPlateSizeMinus
  | WidgetConfigPlateSizePlus
  | WidgetConfigUpliftMinus
  | WidgetConfigUpliftPlus
  | WidgetConfigRiftDepthMinus
  | WidgetConfigRiftDepthPlus
  | WidgetConfigDetailScaleMinus
  | WidgetConfigDetailScalePlus
  | WidgetConfigPlateSpeedMinus
  | WidgetConfigPlateSpeedPlus
  | WidgetConfigBoundarySharpnessMinus
  | WidgetConfigBoundarySharpnessPlus
  | WidgetConfigBoundaryNoiseScaleMinus
  | WidgetConfigBoundaryNoiseScalePlus
  | WidgetConfigBoundaryNoiseStrengthMinus
  | WidgetConfigBoundaryNoiseStrengthPlus
  | WidgetConfigBoundaryWarpOctavesMinus
  | WidgetConfigBoundaryWarpOctavesPlus
  | WidgetConfigBoundaryWarpLacunarityMinus
  | WidgetConfigBoundaryWarpLacunarityPlus
  | WidgetConfigBoundaryWarpGainMinus
  | WidgetConfigBoundaryWarpGainPlus
  | WidgetConfigPlateMergeScaleMinus
  | WidgetConfigPlateMergeScalePlus
  | WidgetConfigPlateMergeBiasMinus
  | WidgetConfigPlateMergeBiasPlus
  | WidgetConfigPlateDetailScaleMinus
  | WidgetConfigPlateDetailScalePlus
  | WidgetConfigPlateDetailStrengthMinus
  | WidgetConfigPlateDetailStrengthPlus
  | WidgetConfigPlateRidgeStrengthMinus
  | WidgetConfigPlateRidgeStrengthPlus
  | WidgetConfigPlateHeightBaseMinus
  | WidgetConfigPlateHeightBasePlus
  | WidgetConfigPlateHeightVarianceMinus
  | WidgetConfigPlateHeightVariancePlus
  | WidgetConfigPlateHardnessBaseMinus
  | WidgetConfigPlateHardnessBasePlus
  | WidgetConfigPlateHardnessVarianceMinus
  | WidgetConfigPlateHardnessVariancePlus
  | WidgetConfigTrenchDepthMinus
  | WidgetConfigTrenchDepthPlus
  | WidgetConfigRidgeHeightMinus
  | WidgetConfigRidgeHeightPlus
  | WidgetConfigPlateBiasStrengthMinus
  | WidgetConfigPlateBiasStrengthPlus
  | WidgetConfigPlateBiasCenterMinus
  | WidgetConfigPlateBiasCenterPlus
  | WidgetConfigPlateBiasEdgeMinus
  | WidgetConfigPlateBiasEdgePlus
  | WidgetConfigPlateBiasNorthMinus
  | WidgetConfigPlateBiasNorthPlus
  | WidgetConfigPlateBiasSouthMinus
  | WidgetConfigPlateBiasSouthPlus
  | WidgetConfigTfcCliffSlopeMinus
  | WidgetConfigTfcCliffSlopePlus
  | WidgetConfigTfcMountainSlopeMinus
  | WidgetConfigTfcMountainSlopePlus
  | WidgetConfigTfcMountainReliefMinus
  | WidgetConfigTfcMountainReliefPlus
  | WidgetConfigTfcHillSlopeMinus
  | WidgetConfigTfcHillSlopePlus
  | WidgetConfigTfcRollingSlopeMinus
  | WidgetConfigTfcRollingSlopePlus
  | WidgetConfigValleyCurvatureMinus
  | WidgetConfigValleyCurvaturePlus
  | WidgetConfigRockElevationThresholdMinus
  | WidgetConfigRockElevationThresholdPlus
  | WidgetConfigRockHardnessThresholdMinus
  | WidgetConfigRockHardnessThresholdPlus
  | WidgetConfigRockHardnessSecondaryMinus
  | WidgetConfigRockHardnessSecondaryPlus
  | WidgetViewElevation
  | WidgetViewBiome
  | WidgetViewClimate
  | WidgetViewMoisture
  | WidgetViewPrecip
  | WidgetViewPlateId
  | WidgetViewPlateBoundary
  | WidgetViewPlateHardness
  | WidgetViewPlateCrust
  | WidgetViewPlateAge
  | WidgetViewPlateHeight
  | WidgetViewPlateVelocity
  | WidgetLogDebug
  | WidgetLogInfo
  | WidgetLogWarn
  | WidgetLogError
  | WidgetLogHeader
  | WidgetMenuSave
  | WidgetMenuLoad
  | WidgetMenuExit
    -- Preset save dialog
  | WidgetPresetSaveOk
  | WidgetPresetSaveCancel
    -- Preset load dialog
  | WidgetPresetLoadOk
  | WidgetPresetLoadCancel
  | WidgetPresetLoadItem
    -- World save dialog
  | WidgetWorldSaveOk
  | WidgetWorldSaveCancel
    -- World load dialog
  | WidgetWorldLoadOk
  | WidgetWorldLoadCancel
  | WidgetWorldLoadItem
  deriving (Eq, Show)

data Widget = Widget
  { widgetId :: !WidgetId
  , widgetRect :: !Rect
  } deriving (Eq, Show)

buildWidgets :: Layout -> [Widget]
buildWidgets layout =
  let (view1, view2, view3, view4, view5, view6, view7, view8, view9, view10, view11, view12) = leftViewRects layout
      (logDebug, logInfo, logWarn, logError) = logFilterRects layout
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) = configTabRects layout
      (leftTabTopo, leftTabView) = leftTabRects layout
  in [ Widget WidgetGenerate (leftGenButtonRect layout)
     , Widget WidgetLeftToggle (leftToggleRect layout)
     , Widget WidgetLeftTabTopo leftTabTopo
     , Widget WidgetLeftTabView leftTabView
      , Widget WidgetSeedValue (configSeedValueRect layout)
      , Widget WidgetSeedRandom (configSeedRandomRect layout)
     , Widget WidgetChunkMinus (leftChunkMinusRect layout)
     , Widget WidgetChunkPlus (leftChunkPlusRect layout)
     , Widget WidgetConfigToggle (configToggleRect layout)
     , Widget WidgetConfigTabTerrain tabTerrain
     , Widget WidgetConfigTabPlanet tabPlanet
     , Widget WidgetConfigTabClimate tabClimate
     , Widget WidgetConfigTabWeather tabWeather
     , Widget WidgetConfigTabBiome tabBiome
     , Widget WidgetConfigTabErosion tabErosion
     , Widget WidgetConfigPresetSave (configPresetSaveRect layout)
     , Widget WidgetConfigPresetLoad (configPresetLoadRect layout)
     , Widget WidgetConfigReset (configResetRect layout)
     , Widget WidgetConfigRevert (configRevertRect layout)
       , Widget WidgetConfigWaterMinus (configWaterMinusRect layout)
       , Widget WidgetConfigWaterPlus (configWaterPlusRect layout)
       , Widget WidgetConfigEvapMinus (configEvapMinusRect layout)
       , Widget WidgetConfigEvapPlus (configEvapPlusRect layout)
      , Widget WidgetConfigRainShadowMinus (configRainShadowMinusRect layout)
      , Widget WidgetConfigRainShadowPlus (configRainShadowPlusRect layout)
      , Widget WidgetConfigWindDiffuseMinus (configWindDiffuseMinusRect layout)
      , Widget WidgetConfigWindDiffusePlus (configWindDiffusePlusRect layout)
      , Widget WidgetConfigEquatorTempMinus (configEquatorTempMinusRect layout)
      , Widget WidgetConfigEquatorTempPlus (configEquatorTempPlusRect layout)
      , Widget WidgetConfigPoleTempMinus (configPoleTempMinusRect layout)
      , Widget WidgetConfigPoleTempPlus (configPoleTempPlusRect layout)
      , Widget WidgetConfigLapseRateMinus (configLapseRateMinusRect layout)
      , Widget WidgetConfigLapseRatePlus (configLapseRatePlusRect layout)
      , Widget WidgetConfigWindIterationsMinus (configWindIterationsMinusRect layout)
      , Widget WidgetConfigWindIterationsPlus (configWindIterationsPlusRect layout)
      , Widget WidgetConfigMoistureIterationsMinus (configMoistureIterationsMinusRect layout)
      , Widget WidgetConfigMoistureIterationsPlus (configMoistureIterationsPlusRect layout)
      , Widget WidgetConfigWeatherTickMinus (configWeatherTickMinusRect layout)
      , Widget WidgetConfigWeatherTickPlus (configWeatherTickPlusRect layout)
      , Widget WidgetConfigWeatherPhaseMinus (configWeatherPhaseMinusRect layout)
      , Widget WidgetConfigWeatherPhasePlus (configWeatherPhasePlusRect layout)
      , Widget WidgetConfigWeatherAmplitudeMinus (configWeatherAmplitudeMinusRect layout)
      , Widget WidgetConfigWeatherAmplitudePlus (configWeatherAmplitudePlusRect layout)
      , Widget WidgetConfigSeasonCycleLengthMinus (configSeasonCycleLengthMinusRect layout)
      , Widget WidgetConfigSeasonCycleLengthPlus (configSeasonCycleLengthPlusRect layout)
      , Widget WidgetConfigJitterAmplitudeMinus (configJitterAmplitudeMinusRect layout)
      , Widget WidgetConfigJitterAmplitudePlus (configJitterAmplitudePlusRect layout)
      , Widget WidgetConfigPressureBaseMinus (configPressureBaseMinusRect layout)
      , Widget WidgetConfigPressureBasePlus (configPressureBasePlusRect layout)
      , Widget WidgetConfigPressureTempScaleMinus (configPressureTempScaleMinusRect layout)
      , Widget WidgetConfigPressureTempScalePlus (configPressureTempScalePlusRect layout)
      , Widget WidgetConfigPressureCoriolisScaleMinus (configPressureCoriolisScaleMinusRect layout)
      , Widget WidgetConfigPressureCoriolisScalePlus (configPressureCoriolisScalePlusRect layout)
      , Widget WidgetConfigSeasonalBaseMinus (configSeasonalBaseMinusRect layout)
      , Widget WidgetConfigSeasonalBasePlus (configSeasonalBasePlusRect layout)
      , Widget WidgetConfigSeasonalRangeMinus (configSeasonalRangeMinusRect layout)
      , Widget WidgetConfigSeasonalRangePlus (configSeasonalRangePlusRect layout)
      , Widget WidgetConfigHumidityNoiseScaleMinus (configHumidityNoiseScaleMinusRect layout)
      , Widget WidgetConfigHumidityNoiseScalePlus (configHumidityNoiseScalePlusRect layout)
      , Widget WidgetConfigPrecipNoiseScaleMinus (configPrecipNoiseScaleMinusRect layout)
      , Widget WidgetConfigPrecipNoiseScalePlus (configPrecipNoiseScalePlusRect layout)
      , Widget WidgetConfigWeatherITCZWidthMinus (configWeatherITCZWidthMinusRect layout)
      , Widget WidgetConfigWeatherITCZWidthPlus (configWeatherITCZWidthPlusRect layout)
      , Widget WidgetConfigWeatherITCZPrecipBoostMinus (configWeatherITCZPrecipBoostMinusRect layout)
      , Widget WidgetConfigWeatherITCZPrecipBoostPlus (configWeatherITCZPrecipBoostPlusRect layout)
      , Widget WidgetConfigPressureHumidityScaleMinus (configPressureHumidityScaleMinusRect layout)
      , Widget WidgetConfigPressureHumidityScalePlus (configPressureHumidityScalePlusRect layout)
      , Widget WidgetConfigPressureGradientWindScaleMinus (configPressureGradientWindScaleMinusRect layout)
      , Widget WidgetConfigPressureGradientWindScalePlus (configPressureGradientWindScalePlusRect layout)
      , Widget WidgetConfigWindNoiseScaleMinus (configWindNoiseScaleMinusRect layout)
      , Widget WidgetConfigWindNoiseScalePlus (configWindNoiseScalePlusRect layout)
      , Widget WidgetConfigITCZMigrationScaleMinus (configITCZMigrationScaleMinusRect layout)
      , Widget WidgetConfigITCZMigrationScalePlus (configITCZMigrationScalePlusRect layout)
      , Widget WidgetConfigCloudRHExponentMinus (configCloudRHExponentMinusRect layout)
      , Widget WidgetConfigCloudRHExponentPlus (configCloudRHExponentPlusRect layout)
      , Widget WidgetConfigCloudAlbedoEffectMinus (configCloudAlbedoEffectMinusRect layout)
      , Widget WidgetConfigCloudAlbedoEffectPlus (configCloudAlbedoEffectPlusRect layout)
      , Widget WidgetConfigCloudPrecipBoostMinus (configCloudPrecipBoostMinusRect layout)
      , Widget WidgetConfigCloudPrecipBoostPlus (configCloudPrecipBoostPlusRect layout)
      , Widget WidgetConfigVegBaseMinus (configVegBaseMinusRect layout)
      , Widget WidgetConfigVegBasePlus (configVegBasePlusRect layout)
      , Widget WidgetConfigVegBoostMinus (configVegBoostMinusRect layout)
      , Widget WidgetConfigVegBoostPlus (configVegBoostPlusRect layout)
      , Widget WidgetConfigVegTempWeightMinus (configVegTempWeightMinusRect layout)
      , Widget WidgetConfigVegTempWeightPlus (configVegTempWeightPlusRect layout)
      , Widget WidgetConfigVegPrecipWeightMinus (configVegPrecipWeightMinusRect layout)
      , Widget WidgetConfigVegPrecipWeightPlus (configVegPrecipWeightPlusRect layout)
      , Widget WidgetConfigBtCoastalBandMinus (configBtCoastalBandMinusRect layout)
      , Widget WidgetConfigBtCoastalBandPlus (configBtCoastalBandPlusRect layout)
      , Widget WidgetConfigBtSnowMaxTempMinus (configBtSnowMaxTempMinusRect layout)
      , Widget WidgetConfigBtSnowMaxTempPlus (configBtSnowMaxTempPlusRect layout)
      , Widget WidgetConfigBtAlpineMaxTempMinus (configBtAlpineMaxTempMinusRect layout)
      , Widget WidgetConfigBtAlpineMaxTempPlus (configBtAlpineMaxTempPlusRect layout)
      , Widget WidgetConfigBtIceCapTempMinus (configBtIceCapTempMinusRect layout)
      , Widget WidgetConfigBtIceCapTempPlus (configBtIceCapTempPlusRect layout)
      , Widget WidgetConfigBtMontaneMaxTempMinus (configBtMontaneMaxTempMinusRect layout)
      , Widget WidgetConfigBtMontaneMaxTempPlus (configBtMontaneMaxTempPlusRect layout)
      , Widget WidgetConfigBtMontanePrecipMinus (configBtMontanePrecipMinusRect layout)
      , Widget WidgetConfigBtMontanePrecipPlus (configBtMontanePrecipPlusRect layout)
      , Widget WidgetConfigBtCliffSlopeMinus (configBtCliffSlopeMinusRect layout)
      , Widget WidgetConfigBtCliffSlopePlus (configBtCliffSlopePlusRect layout)
      , Widget WidgetConfigBtValleyMoistureMinus (configBtValleyMoistureMinusRect layout)
      , Widget WidgetConfigBtValleyMoisturePlus (configBtValleyMoisturePlusRect layout)
      , Widget WidgetConfigBtDepressionMoistureMinus (configBtDepressionMoistureMinusRect layout)
      , Widget WidgetConfigBtDepressionMoisturePlus (configBtDepressionMoisturePlusRect layout)
      , Widget WidgetConfigBtPrecipWeightMinus (configBtPrecipWeightMinusRect layout)
      , Widget WidgetConfigBtPrecipWeightPlus (configBtPrecipWeightPlusRect layout)
      , Widget WidgetConfigVbcTempMinMinus (configVbcTempMinMinusRect layout)
      , Widget WidgetConfigVbcTempMinPlus (configVbcTempMinPlusRect layout)
      , Widget WidgetConfigVbcTempRangeMinus (configVbcTempRangeMinusRect layout)
      , Widget WidgetConfigVbcTempRangePlus (configVbcTempRangePlusRect layout)
      , Widget WidgetConfigVbcFertilityBoostMinus (configVbcFertilityBoostMinusRect layout)
      , Widget WidgetConfigVbcFertilityBoostPlus (configVbcFertilityBoostPlusRect layout)
      , Widget WidgetConfigVbcAlbedoBaseMinus (configVbcAlbedoBaseMinusRect layout)
      , Widget WidgetConfigVbcAlbedoBasePlus (configVbcAlbedoBasePlusRect layout)
      , Widget WidgetConfigVbcAlbedoBareMinus (configVbcAlbedoBareMinusRect layout)
      , Widget WidgetConfigVbcAlbedoBarePlus (configVbcAlbedoBarePlusRect layout)
      , Widget WidgetConfigVbcAlbedoVegMinus (configVbcAlbedoVegMinusRect layout)
      , Widget WidgetConfigVbcAlbedoVegPlus (configVbcAlbedoVegPlusRect layout)
      , Widget WidgetConfigVbcOceanAlbedoMinus (configVbcOceanAlbedoMinusRect layout)
      , Widget WidgetConfigVbcOceanAlbedoPlus (configVbcOceanAlbedoPlusRect layout)
      , Widget WidgetConfigVbcIceAlbedoMinus (configVbcIceAlbedoMinusRect layout)
      , Widget WidgetConfigVbcIceAlbedoPlus (configVbcIceAlbedoPlusRect layout)
      , Widget WidgetConfigBiomeSmoothingMinus (configBiomeSmoothingMinusRect layout)
      , Widget WidgetConfigBiomeSmoothingPlus (configBiomeSmoothingPlusRect layout)
      , Widget WidgetConfigVolcanicAshBoostMinus (configVolcanicAshBoostMinusRect layout)
      , Widget WidgetConfigVolcanicAshBoostPlus (configVolcanicAshBoostPlusRect layout)
      , Widget WidgetConfigVolcanicLavaPenaltyMinus (configVolcanicLavaPenaltyMinusRect layout)
      , Widget WidgetConfigVolcanicLavaPenaltyPlus (configVolcanicLavaPenaltyPlusRect layout)
      , Widget WidgetConfigBiomeFeedbackBlendMinus (configBiomeFeedbackBlendMinusRect layout)
      , Widget WidgetConfigBiomeFeedbackBlendPlus (configBiomeFeedbackBlendPlusRect layout)
      , Widget WidgetConfigBoundaryMotionTempMinus (configBoundaryMotionTempMinusRect layout)
      , Widget WidgetConfigBoundaryMotionTempPlus (configBoundaryMotionTempPlusRect layout)
      , Widget WidgetConfigBoundaryMotionPrecipMinus (configBoundaryMotionPrecipMinusRect layout)
      , Widget WidgetConfigBoundaryMotionPrecipPlus (configBoundaryMotionPrecipPlusRect layout)
      , Widget WidgetConfigPlanetRadiusMinus (configPlanetRadiusMinusRect layout)
      , Widget WidgetConfigPlanetRadiusPlus (configPlanetRadiusPlusRect layout)
      , Widget WidgetConfigAxialTiltMinus (configAxialTiltMinusRect layout)
      , Widget WidgetConfigAxialTiltPlus (configAxialTiltPlusRect layout)
      , Widget WidgetConfigInsolationMinus (configInsolationMinusRect layout)
      , Widget WidgetConfigInsolationPlus (configInsolationPlusRect layout)
      , Widget WidgetConfigOccWarmScaleMinus (configOccWarmScaleMinusRect layout)
      , Widget WidgetConfigOccWarmScalePlus (configOccWarmScalePlusRect layout)
      , Widget WidgetConfigOccColdScaleMinus (configOccColdScaleMinusRect layout)
      , Widget WidgetConfigOccColdScalePlus (configOccColdScalePlusRect layout)
      , Widget WidgetConfigOccLatPeakDegMinus (configOccLatPeakDegMinusRect layout)
      , Widget WidgetConfigOccLatPeakDegPlus (configOccLatPeakDegPlusRect layout)
      , Widget WidgetConfigOccLatWidthDegMinus (configOccLatWidthDegMinusRect layout)
      , Widget WidgetConfigOccLatWidthDegPlus (configOccLatWidthDegPlusRect layout)
      , Widget WidgetConfigSliceLatCenterMinus (configSliceLatCenterMinusRect layout)
      , Widget WidgetConfigSliceLatCenterPlus (configSliceLatCenterPlusRect layout)
      , Widget WidgetConfigSliceLonCenterMinus (configSliceLonCenterMinusRect layout)
      , Widget WidgetConfigSliceLonCenterPlus (configSliceLonCenterPlusRect layout)
      , Widget WidgetConfigLatitudeExponentMinus (configLatitudeExponentMinusRect layout)
      , Widget WidgetConfigLatitudeExponentPlus (configLatitudeExponentPlusRect layout)
      , Widget WidgetConfigPlateHeightCoolingMinus (configPlateHeightCoolingMinusRect layout)
      , Widget WidgetConfigPlateHeightCoolingPlus (configPlateHeightCoolingPlusRect layout)
      , Widget WidgetConfigTempNoiseScaleMinus (configTempNoiseScaleMinusRect layout)
      , Widget WidgetConfigTempNoiseScalePlus (configTempNoiseScalePlusRect layout)
      , Widget WidgetConfigOceanModerationMinus (configOceanModerationMinusRect layout)
      , Widget WidgetConfigOceanModerationPlus (configOceanModerationPlusRect layout)
      , Widget WidgetConfigOceanModerateTempMinus (configOceanModerateTempMinusRect layout)
      , Widget WidgetConfigOceanModerateTempPlus (configOceanModerateTempPlusRect layout)
      , Widget WidgetConfigAlbedoSensitivityMinus (configAlbedoSensitivityMinusRect layout)
      , Widget WidgetConfigAlbedoSensitivityPlus (configAlbedoSensitivityPlusRect layout)
      , Widget WidgetConfigAlbedoReferenceMinus (configAlbedoReferenceMinusRect layout)
      , Widget WidgetConfigAlbedoReferencePlus (configAlbedoReferencePlusRect layout)
      , Widget WidgetConfigMoistAdvectMinus (configMoistAdvectMinusRect layout)
      , Widget WidgetConfigMoistAdvectPlus (configMoistAdvectPlusRect layout)
      , Widget WidgetConfigMoistLocalMinus (configMoistLocalMinusRect layout)
      , Widget WidgetConfigMoistLocalPlus (configMoistLocalPlusRect layout)
      , Widget WidgetConfigMoistWindEvapScaleMinus (configMoistWindEvapScaleMinusRect layout)
      , Widget WidgetConfigMoistWindEvapScalePlus (configMoistWindEvapScalePlusRect layout)
      , Widget WidgetConfigMoistEvapNoiseScaleMinus (configMoistEvapNoiseScaleMinusRect layout)
      , Widget WidgetConfigMoistEvapNoiseScalePlus (configMoistEvapNoiseScalePlusRect layout)
      , Widget WidgetConfigMoistLandETCoeffMinus (configMoistLandETCoeffMinusRect layout)
      , Widget WidgetConfigMoistLandETCoeffPlus (configMoistLandETCoeffPlusRect layout)
      , Widget WidgetConfigMoistBareEvapFracMinus (configMoistBareEvapFracMinusRect layout)
      , Widget WidgetConfigMoistBareEvapFracPlus (configMoistBareEvapFracPlusRect layout)
      , Widget WidgetConfigMoistVegTranspFracMinus (configMoistVegTranspFracMinusRect layout)
      , Widget WidgetConfigMoistVegTranspFracPlus (configMoistVegTranspFracPlusRect layout)
      , Widget WidgetConfigMoistWindETScaleMinus (configMoistWindETScaleMinusRect layout)
      , Widget WidgetConfigMoistWindETScalePlus (configMoistWindETScalePlusRect layout)
      , Widget WidgetConfigMoistCondensationRateMinus (configMoistCondensationRateMinusRect layout)
      , Widget WidgetConfigMoistCondensationRatePlus (configMoistCondensationRatePlusRect layout)
      , Widget WidgetConfigMoistRecycleRateMinus (configMoistRecycleRateMinusRect layout)
      , Widget WidgetConfigMoistRecycleRatePlus (configMoistRecycleRatePlusRect layout)
      , Widget WidgetConfigMoistITCZStrengthMinus (configMoistITCZStrengthMinusRect layout)
      , Widget WidgetConfigMoistITCZStrengthPlus (configMoistITCZStrengthPlusRect layout)
      , Widget WidgetConfigMoistITCZWidthMinus (configMoistITCZWidthMinusRect layout)
      , Widget WidgetConfigMoistITCZWidthPlus (configMoistITCZWidthPlusRect layout)
      , Widget WidgetConfigOrographicScaleMinus (configOrographicScaleMinusRect layout)
      , Widget WidgetConfigOrographicScalePlus (configOrographicScalePlusRect layout)
      , Widget WidgetConfigOrographicStepMinus (configOrographicStepMinusRect layout)
      , Widget WidgetConfigOrographicStepPlus (configOrographicStepPlusRect layout)
      , Widget WidgetConfigCoastalIterationsMinus (configCoastalIterationsMinusRect layout)
      , Widget WidgetConfigCoastalIterationsPlus (configCoastalIterationsPlusRect layout)
      , Widget WidgetConfigCoastalDiffuseMinus (configCoastalDiffuseMinusRect layout)
      , Widget WidgetConfigCoastalDiffusePlus (configCoastalDiffusePlusRect layout)
      , Widget WidgetConfigCoastalMoistureBoostMinus (configCoastalMoistureBoostMinusRect layout)
      , Widget WidgetConfigCoastalMoistureBoostPlus (configCoastalMoistureBoostPlusRect layout)
      , Widget WidgetConfigWindBeltStrengthMinus (configWindBeltStrengthMinusRect layout)
      , Widget WidgetConfigWindBeltStrengthPlus (configWindBeltStrengthPlusRect layout)
      , Widget WidgetConfigWindBeltHarmonicsMinus (configWindBeltHarmonicsMinusRect layout)
      , Widget WidgetConfigWindBeltHarmonicsPlus (configWindBeltHarmonicsPlusRect layout)
      , Widget WidgetConfigWindBeltBaseMinus (configWindBeltBaseMinusRect layout)
      , Widget WidgetConfigWindBeltBasePlus (configWindBeltBasePlusRect layout)
      , Widget WidgetConfigWindBeltRangeMinus (configWindBeltRangeMinusRect layout)
      , Widget WidgetConfigWindBeltRangePlus (configWindBeltRangePlusRect layout)
      , Widget WidgetConfigWindBeltSpeedScaleMinus (configWindBeltSpeedScaleMinusRect layout)
      , Widget WidgetConfigWindBeltSpeedScalePlus (configWindBeltSpeedScalePlusRect layout)
      , Widget WidgetConfigBndLandRangeMinus (configBndLandRangeMinusRect layout)
      , Widget WidgetConfigBndLandRangePlus (configBndLandRangePlusRect layout)
      , Widget WidgetConfigBndTempConvergentMinus (configBndTempConvergentMinusRect layout)
      , Widget WidgetConfigBndTempConvergentPlus (configBndTempConvergentPlusRect layout)
      , Widget WidgetConfigBndTempDivergentMinus (configBndTempDivergentMinusRect layout)
      , Widget WidgetConfigBndTempDivergentPlus (configBndTempDivergentPlusRect layout)
      , Widget WidgetConfigBndTempTransformMinus (configBndTempTransformMinusRect layout)
      , Widget WidgetConfigBndTempTransformPlus (configBndTempTransformPlusRect layout)
      , Widget WidgetConfigBndPrecipConvergentMinus (configBndPrecipConvergentMinusRect layout)
      , Widget WidgetConfigBndPrecipConvergentPlus (configBndPrecipConvergentPlusRect layout)
      , Widget WidgetConfigBndPrecipDivergentMinus (configBndPrecipDivergentMinusRect layout)
      , Widget WidgetConfigBndPrecipDivergentPlus (configBndPrecipDivergentPlusRect layout)
      , Widget WidgetConfigBndPrecipTransformMinus (configBndPrecipTransformMinusRect layout)
      , Widget WidgetConfigBndPrecipTransformPlus (configBndPrecipTransformPlusRect layout)
      , Widget WidgetConfigErosionHydraulicMinus (configErosionHydraulicMinusRect layout)
      , Widget WidgetConfigErosionHydraulicPlus (configErosionHydraulicPlusRect layout)
      , Widget WidgetConfigErosionThermalMinus (configErosionThermalMinusRect layout)
      , Widget WidgetConfigErosionThermalPlus (configErosionThermalPlusRect layout)
      , Widget WidgetConfigErosionRainRateMinus (configErosionRainRateMinusRect layout)
      , Widget WidgetConfigErosionRainRatePlus (configErosionRainRatePlusRect layout)
      , Widget WidgetConfigErosionTalusMinus (configErosionTalusMinusRect layout)
      , Widget WidgetConfigErosionTalusPlus (configErosionTalusPlusRect layout)
      , Widget WidgetConfigErosionMaxDropMinus (configErosionMaxDropMinusRect layout)
      , Widget WidgetConfigErosionMaxDropPlus (configErosionMaxDropPlusRect layout)
      , Widget WidgetConfigGlacierSnowTempMinus (configGlacierSnowTempMinusRect layout)
      , Widget WidgetConfigGlacierSnowTempPlus (configGlacierSnowTempPlusRect layout)
      , Widget WidgetConfigGlacierSnowRangeMinus (configGlacierSnowRangeMinusRect layout)
      , Widget WidgetConfigGlacierSnowRangePlus (configGlacierSnowRangePlusRect layout)
      , Widget WidgetConfigGlacierMeltTempMinus (configGlacierMeltTempMinusRect layout)
      , Widget WidgetConfigGlacierMeltTempPlus (configGlacierMeltTempPlusRect layout)
      , Widget WidgetConfigGlacierMeltRateMinus (configGlacierMeltRateMinusRect layout)
      , Widget WidgetConfigGlacierMeltRatePlus (configGlacierMeltRatePlusRect layout)
      , Widget WidgetConfigGlacierAccumScaleMinus (configGlacierAccumScaleMinusRect layout)
      , Widget WidgetConfigGlacierAccumScalePlus (configGlacierAccumScalePlusRect layout)
      , Widget WidgetConfigGlacierFlowItersMinus (configGlacierFlowItersMinusRect layout)
      , Widget WidgetConfigGlacierFlowItersPlus (configGlacierFlowItersPlusRect layout)
      , Widget WidgetConfigGlacierFlowRateMinus (configGlacierFlowRateMinusRect layout)
      , Widget WidgetConfigGlacierFlowRatePlus (configGlacierFlowRatePlusRect layout)
      , Widget WidgetConfigGlacierErosionScaleMinus (configGlacierErosionScaleMinusRect layout)
      , Widget WidgetConfigGlacierErosionScalePlus (configGlacierErosionScalePlusRect layout)
      , Widget WidgetConfigGlacierCarveScaleMinus (configGlacierCarveScaleMinusRect layout)
      , Widget WidgetConfigGlacierCarveScalePlus (configGlacierCarveScalePlusRect layout)
      , Widget WidgetConfigGlacierDepositScaleMinus (configGlacierDepositScaleMinusRect layout)
      , Widget WidgetConfigGlacierDepositScalePlus (configGlacierDepositScalePlusRect layout)
      , Widget WidgetConfigVentDensityMinus (configVentDensityMinusRect layout)
      , Widget WidgetConfigVentDensityPlus (configVentDensityPlusRect layout)
      , Widget WidgetConfigVentThresholdMinus (configVentThresholdMinusRect layout)
      , Widget WidgetConfigVentThresholdPlus (configVentThresholdPlusRect layout)
      , Widget WidgetConfigHotspotScaleMinus (configHotspotScaleMinusRect layout)
      , Widget WidgetConfigHotspotScalePlus (configHotspotScalePlusRect layout)
      , Widget WidgetConfigHotspotThresholdMinus (configHotspotThresholdMinusRect layout)
      , Widget WidgetConfigHotspotThresholdPlus (configHotspotThresholdPlusRect layout)
      , Widget WidgetConfigMagmaRechargeMinus (configMagmaRechargeMinusRect layout)
      , Widget WidgetConfigMagmaRechargePlus (configMagmaRechargePlusRect layout)
      , Widget WidgetConfigLavaScaleMinus (configLavaScaleMinusRect layout)
      , Widget WidgetConfigLavaScalePlus (configLavaScalePlusRect layout)
      , Widget WidgetConfigAshScaleMinus (configAshScaleMinusRect layout)
      , Widget WidgetConfigAshScalePlus (configAshScalePlusRect layout)
      , Widget WidgetConfigVolcanicDepositScaleMinus (configVolcanicDepositScaleMinusRect layout)
      , Widget WidgetConfigVolcanicDepositScalePlus (configVolcanicDepositScalePlusRect layout)
      , Widget WidgetConfigSoilMoistureThresholdMinus (configSoilMoistureThresholdMinusRect layout)
      , Widget WidgetConfigSoilMoistureThresholdPlus (configSoilMoistureThresholdPlusRect layout)
      , Widget WidgetConfigSoilHardnessThresholdMinus (configSoilHardnessThresholdMinusRect layout)
      , Widget WidgetConfigSoilHardnessThresholdPlus (configSoilHardnessThresholdPlusRect layout)
      , Widget WidgetConfigSoilFertilityMoistWeightMinus (configSoilFertilityMoistWeightMinusRect layout)
      , Widget WidgetConfigSoilFertilityMoistWeightPlus (configSoilFertilityMoistWeightPlusRect layout)
      , Widget WidgetConfigSoilFertilityDepthWeightMinus (configSoilFertilityDepthWeightMinusRect layout)
      , Widget WidgetConfigSoilFertilityDepthWeightPlus (configSoilFertilityDepthWeightPlusRect layout)
      , Widget WidgetConfigSinkBreachDepthMinus (configSinkBreachDepthMinusRect layout)
      , Widget WidgetConfigSinkBreachDepthPlus (configSinkBreachDepthPlusRect layout)
      , Widget WidgetConfigStreamPowerMaxErosionMinus (configStreamPowerMaxErosionMinusRect layout)
      , Widget WidgetConfigStreamPowerMaxErosionPlus (configStreamPowerMaxErosionPlusRect layout)
      , Widget WidgetConfigRiverCarveMaxDepthMinus (configRiverCarveMaxDepthMinusRect layout)
      , Widget WidgetConfigRiverCarveMaxDepthPlus (configRiverCarveMaxDepthPlusRect layout)
      , Widget WidgetConfigCoastalErodeStrengthMinus (configCoastalErodeStrengthMinusRect layout)
      , Widget WidgetConfigCoastalErodeStrengthPlus (configCoastalErodeStrengthPlusRect layout)
      , Widget WidgetConfigHydroHardnessWeightMinus (configHydroHardnessWeightMinusRect layout)
      , Widget WidgetConfigHydroHardnessWeightPlus (configHydroHardnessWeightPlusRect layout)
      , Widget WidgetConfigMinLakeSizeMinus (configMinLakeSizeMinusRect layout)
      , Widget WidgetConfigMinLakeSizePlus (configMinLakeSizePlusRect layout)
      , Widget WidgetConfigInlandSeaMinSizeMinus (configInlandSeaMinSizeMinusRect layout)
      , Widget WidgetConfigInlandSeaMinSizePlus (configInlandSeaMinSizePlusRect layout)
      , Widget WidgetConfigRoughnessScaleMinus (configRoughnessScaleMinusRect layout)
      , Widget WidgetConfigRoughnessScalePlus (configRoughnessScalePlusRect layout)
      , Widget WidgetConfigGenScaleMinus (configGenScaleMinusRect layout)
      , Widget WidgetConfigGenScalePlus (configGenScalePlusRect layout)
      , Widget WidgetConfigGenCoordScaleMinus (configGenCoordScaleMinusRect layout)
      , Widget WidgetConfigGenCoordScalePlus (configGenCoordScalePlusRect layout)
      , Widget WidgetConfigGenOffsetXMinus (configGenOffsetXMinusRect layout)
      , Widget WidgetConfigGenOffsetXPlus (configGenOffsetXPlusRect layout)
      , Widget WidgetConfigGenOffsetYMinus (configGenOffsetYMinusRect layout)
      , Widget WidgetConfigGenOffsetYPlus (configGenOffsetYPlusRect layout)
      , Widget WidgetConfigGenFrequencyMinus (configGenFrequencyMinusRect layout)
      , Widget WidgetConfigGenFrequencyPlus (configGenFrequencyPlusRect layout)
      , Widget WidgetConfigGenOctavesMinus (configGenOctavesMinusRect layout)
      , Widget WidgetConfigGenOctavesPlus (configGenOctavesPlusRect layout)
      , Widget WidgetConfigGenLacunarityMinus (configGenLacunarityMinusRect layout)
      , Widget WidgetConfigGenLacunarityPlus (configGenLacunarityPlusRect layout)
      , Widget WidgetConfigGenGainMinus (configGenGainMinusRect layout)
      , Widget WidgetConfigGenGainPlus (configGenGainPlusRect layout)
      , Widget WidgetConfigGenWarpScaleMinus (configGenWarpScaleMinusRect layout)
      , Widget WidgetConfigGenWarpScalePlus (configGenWarpScalePlusRect layout)
      , Widget WidgetConfigGenWarpStrengthMinus (configGenWarpStrengthMinusRect layout)
      , Widget WidgetConfigGenWarpStrengthPlus (configGenWarpStrengthPlusRect layout)
      , Widget WidgetConfigExtentXMinus (configExtentXMinusRect layout)
      , Widget WidgetConfigExtentXPlus (configExtentXPlusRect layout)
      , Widget WidgetConfigExtentYMinus (configExtentYMinusRect layout)
      , Widget WidgetConfigExtentYPlus (configExtentYPlusRect layout)
      , Widget WidgetConfigEdgeNorthMinus (configEdgeNorthMinusRect layout)
      , Widget WidgetConfigEdgeNorthPlus (configEdgeNorthPlusRect layout)
      , Widget WidgetConfigEdgeSouthMinus (configEdgeSouthMinusRect layout)
      , Widget WidgetConfigEdgeSouthPlus (configEdgeSouthPlusRect layout)
      , Widget WidgetConfigEdgeEastMinus (configEdgeEastMinusRect layout)
      , Widget WidgetConfigEdgeEastPlus (configEdgeEastPlusRect layout)
      , Widget WidgetConfigEdgeWestMinus (configEdgeWestMinusRect layout)
      , Widget WidgetConfigEdgeWestPlus (configEdgeWestPlusRect layout)
      , Widget WidgetConfigEdgeFalloffMinus (configEdgeFalloffMinusRect layout)
      , Widget WidgetConfigEdgeFalloffPlus (configEdgeFalloffPlusRect layout)
      , Widget WidgetConfigPlateSizeMinus (configPlateSizeMinusRect layout)
      , Widget WidgetConfigPlateSizePlus (configPlateSizePlusRect layout)
      , Widget WidgetConfigUpliftMinus (configUpliftMinusRect layout)
      , Widget WidgetConfigUpliftPlus (configUpliftPlusRect layout)
      , Widget WidgetConfigRiftDepthMinus (configRiftDepthMinusRect layout)
      , Widget WidgetConfigRiftDepthPlus (configRiftDepthPlusRect layout)
      , Widget WidgetConfigDetailScaleMinus (configDetailScaleMinusRect layout)
      , Widget WidgetConfigDetailScalePlus (configDetailScalePlusRect layout)
      , Widget WidgetConfigPlateSpeedMinus (configPlateSpeedMinusRect layout)
      , Widget WidgetConfigPlateSpeedPlus (configPlateSpeedPlusRect layout)
      , Widget WidgetConfigBoundarySharpnessMinus (configBoundarySharpnessMinusRect layout)
      , Widget WidgetConfigBoundarySharpnessPlus (configBoundarySharpnessPlusRect layout)
      , Widget WidgetConfigBoundaryNoiseScaleMinus (configBoundaryNoiseScaleMinusRect layout)
      , Widget WidgetConfigBoundaryNoiseScalePlus (configBoundaryNoiseScalePlusRect layout)
      , Widget WidgetConfigBoundaryNoiseStrengthMinus (configBoundaryNoiseStrengthMinusRect layout)
      , Widget WidgetConfigBoundaryNoiseStrengthPlus (configBoundaryNoiseStrengthPlusRect layout)
      , Widget WidgetConfigBoundaryWarpOctavesMinus (configBoundaryWarpOctavesMinusRect layout)
      , Widget WidgetConfigBoundaryWarpOctavesPlus (configBoundaryWarpOctavesPlusRect layout)
      , Widget WidgetConfigBoundaryWarpLacunarityMinus (configBoundaryWarpLacunarityMinusRect layout)
      , Widget WidgetConfigBoundaryWarpLacunarityPlus (configBoundaryWarpLacunarityPlusRect layout)
      , Widget WidgetConfigBoundaryWarpGainMinus (configBoundaryWarpGainMinusRect layout)
      , Widget WidgetConfigBoundaryWarpGainPlus (configBoundaryWarpGainPlusRect layout)
      , Widget WidgetConfigPlateMergeScaleMinus (configPlateMergeScaleMinusRect layout)
      , Widget WidgetConfigPlateMergeScalePlus (configPlateMergeScalePlusRect layout)
      , Widget WidgetConfigPlateMergeBiasMinus (configPlateMergeBiasMinusRect layout)
      , Widget WidgetConfigPlateMergeBiasPlus (configPlateMergeBiasPlusRect layout)
      , Widget WidgetConfigPlateDetailScaleMinus (configPlateDetailScaleMinusRect layout)
      , Widget WidgetConfigPlateDetailScalePlus (configPlateDetailScalePlusRect layout)
      , Widget WidgetConfigPlateDetailStrengthMinus (configPlateDetailStrengthMinusRect layout)
      , Widget WidgetConfigPlateDetailStrengthPlus (configPlateDetailStrengthPlusRect layout)
      , Widget WidgetConfigPlateRidgeStrengthMinus (configPlateRidgeStrengthMinusRect layout)
      , Widget WidgetConfigPlateRidgeStrengthPlus (configPlateRidgeStrengthPlusRect layout)
      , Widget WidgetConfigPlateHeightBaseMinus (configPlateHeightBaseMinusRect layout)
      , Widget WidgetConfigPlateHeightBasePlus (configPlateHeightBasePlusRect layout)
      , Widget WidgetConfigPlateHeightVarianceMinus (configPlateHeightVarianceMinusRect layout)
      , Widget WidgetConfigPlateHeightVariancePlus (configPlateHeightVariancePlusRect layout)
      , Widget WidgetConfigPlateHardnessBaseMinus (configPlateHardnessBaseMinusRect layout)
      , Widget WidgetConfigPlateHardnessBasePlus (configPlateHardnessBasePlusRect layout)
      , Widget WidgetConfigPlateHardnessVarianceMinus (configPlateHardnessVarianceMinusRect layout)
      , Widget WidgetConfigPlateHardnessVariancePlus (configPlateHardnessVariancePlusRect layout)
      , Widget WidgetConfigTrenchDepthMinus (configTrenchDepthMinusRect layout)
      , Widget WidgetConfigTrenchDepthPlus (configTrenchDepthPlusRect layout)
      , Widget WidgetConfigRidgeHeightMinus (configRidgeHeightMinusRect layout)
      , Widget WidgetConfigRidgeHeightPlus (configRidgeHeightPlusRect layout)
      , Widget WidgetConfigPlateBiasStrengthMinus (configPlateBiasStrengthMinusRect layout)
      , Widget WidgetConfigPlateBiasStrengthPlus (configPlateBiasStrengthPlusRect layout)
      , Widget WidgetConfigPlateBiasCenterMinus (configPlateBiasCenterMinusRect layout)
      , Widget WidgetConfigPlateBiasCenterPlus (configPlateBiasCenterPlusRect layout)
      , Widget WidgetConfigPlateBiasEdgeMinus (configPlateBiasEdgeMinusRect layout)
      , Widget WidgetConfigPlateBiasEdgePlus (configPlateBiasEdgePlusRect layout)
      , Widget WidgetConfigPlateBiasNorthMinus (configPlateBiasNorthMinusRect layout)
      , Widget WidgetConfigPlateBiasNorthPlus (configPlateBiasNorthPlusRect layout)
      , Widget WidgetConfigPlateBiasSouthMinus (configPlateBiasSouthMinusRect layout)
      , Widget WidgetConfigPlateBiasSouthPlus (configPlateBiasSouthPlusRect layout)
      , Widget WidgetConfigTfcCliffSlopeMinus (configTfcCliffSlopeMinusRect layout)
      , Widget WidgetConfigTfcCliffSlopePlus (configTfcCliffSlopePlusRect layout)
      , Widget WidgetConfigTfcMountainSlopeMinus (configTfcMountainSlopeMinusRect layout)
      , Widget WidgetConfigTfcMountainSlopePlus (configTfcMountainSlopePlusRect layout)
      , Widget WidgetConfigTfcMountainReliefMinus (configTfcMountainReliefMinusRect layout)
      , Widget WidgetConfigTfcMountainReliefPlus (configTfcMountainReliefPlusRect layout)
      , Widget WidgetConfigTfcHillSlopeMinus (configTfcHillSlopeMinusRect layout)
      , Widget WidgetConfigTfcHillSlopePlus (configTfcHillSlopePlusRect layout)
      , Widget WidgetConfigTfcRollingSlopeMinus (configTfcRollingSlopeMinusRect layout)
      , Widget WidgetConfigTfcRollingSlopePlus (configTfcRollingSlopePlusRect layout)
      , Widget WidgetConfigValleyCurvatureMinus (configValleyCurvatureMinusRect layout)
      , Widget WidgetConfigValleyCurvaturePlus (configValleyCurvaturePlusRect layout)
      , Widget WidgetConfigRockElevationThresholdMinus (configRockElevationThresholdMinusRect layout)
      , Widget WidgetConfigRockElevationThresholdPlus (configRockElevationThresholdPlusRect layout)
      , Widget WidgetConfigRockHardnessThresholdMinus (configRockHardnessThresholdMinusRect layout)
      , Widget WidgetConfigRockHardnessThresholdPlus (configRockHardnessThresholdPlusRect layout)
      , Widget WidgetConfigRockHardnessSecondaryMinus (configRockHardnessSecondaryMinusRect layout)
      , Widget WidgetConfigRockHardnessSecondaryPlus (configRockHardnessSecondaryPlusRect layout)
    , Widget WidgetViewElevation view1
    , Widget WidgetViewBiome view2
    , Widget WidgetViewClimate view3
    , Widget WidgetViewMoisture view4
    , Widget WidgetViewPrecip view5
    , Widget WidgetViewPlateId view6
    , Widget WidgetViewPlateBoundary view7
    , Widget WidgetViewPlateHardness view8
    , Widget WidgetViewPlateCrust view9
    , Widget WidgetViewPlateAge view10
    , Widget WidgetViewPlateHeight view11
    , Widget WidgetViewPlateVelocity view12
     , Widget WidgetLogDebug logDebug
     , Widget WidgetLogInfo logInfo
     , Widget WidgetLogWarn logWarn
     , Widget WidgetLogError logError
     , Widget WidgetLogHeader (logHeaderRect layout)
    , Widget WidgetMenuSave (menuSaveRect layout)
    , Widget WidgetMenuLoad (menuLoadRect layout)
    , Widget WidgetMenuExit (menuExitRect layout)
      -- Preset save dialog
    , Widget WidgetPresetSaveOk (presetSaveOkRect layout)
    , Widget WidgetPresetSaveCancel (presetSaveCancelRect layout)
      -- Preset load dialog
    , Widget WidgetPresetLoadOk (presetLoadOkRect layout)
    , Widget WidgetPresetLoadCancel (presetLoadCancelRect layout)
      -- World save dialog
    , Widget WidgetWorldSaveOk (worldSaveOkRect layout)
    , Widget WidgetWorldSaveCancel (worldSaveCancelRect layout)
      -- World load dialog
    , Widget WidgetWorldLoadOk (worldLoadOkRect layout)
    , Widget WidgetWorldLoadCancel (worldLoadCancelRect layout)
     ]

-- | Build full-row tooltip hit areas for config sliders, grouped by tab.
--
-- Returns @(terrain, climate, erosion)@ widget lists. Each slider row
-- is represented by a single 'Widget' using the slider's minus 'WidgetId',
-- with a 'configParamRowRect' that covers buttons, bar, and label area.
-- The caller selects the appropriate list based on the active config tab
-- and applies scroll offset before 'hitTest'.
buildSliderRowWidgets :: Layout -> ([Widget], [Widget], [Widget], [Widget], [Widget], [Widget])
buildSliderRowWidgets layout = (terrain, planet, climate, weather, biome, erosion)
  where
    row :: WidgetId -> Int -> Widget
    row wid idx = Widget wid (configParamRowRect idx layout)

    terrain =
      [ row WidgetConfigGenScaleMinus 0
      , row WidgetConfigGenFrequencyMinus 1
      , row WidgetConfigGenOctavesMinus 2
      , row WidgetConfigGenLacunarityMinus 3
      , row WidgetConfigGenGainMinus 4
      , row WidgetConfigGenWarpScaleMinus 5
      , row WidgetConfigGenWarpStrengthMinus 6
      , row WidgetConfigPlateSizeMinus 7
      , row WidgetConfigUpliftMinus 8
      , row WidgetConfigRiftDepthMinus 9
      , row WidgetConfigDetailScaleMinus 10
      , row WidgetConfigPlateSpeedMinus 11
      , row WidgetConfigBoundarySharpnessMinus 12
      , row WidgetConfigBoundaryNoiseScaleMinus 13
      , row WidgetConfigBoundaryNoiseStrengthMinus 14
      , row WidgetConfigBoundaryWarpOctavesMinus 15
      , row WidgetConfigBoundaryWarpLacunarityMinus 16
      , row WidgetConfigBoundaryWarpGainMinus 17
      , row WidgetConfigPlateMergeScaleMinus 18
      , row WidgetConfigPlateMergeBiasMinus 19
      , row WidgetConfigPlateDetailScaleMinus 20
      , row WidgetConfigPlateDetailStrengthMinus 21
      , row WidgetConfigPlateRidgeStrengthMinus 22
      , row WidgetConfigPlateHeightBaseMinus 23
      , row WidgetConfigPlateHeightVarianceMinus 24
      , row WidgetConfigPlateHardnessBaseMinus 25
      , row WidgetConfigPlateHardnessVarianceMinus 26
      , row WidgetConfigTrenchDepthMinus 27
      , row WidgetConfigRidgeHeightMinus 28
      , row WidgetConfigPlateBiasStrengthMinus 29
      , row WidgetConfigPlateBiasCenterMinus 30
      , row WidgetConfigPlateBiasEdgeMinus 31
      , row WidgetConfigPlateBiasNorthMinus 32
      , row WidgetConfigPlateBiasSouthMinus 33
      , row WidgetConfigTfcCliffSlopeMinus 34
      , row WidgetConfigTfcMountainSlopeMinus 35
      , row WidgetConfigTfcMountainReliefMinus 36
      , row WidgetConfigTfcHillSlopeMinus 37
      , row WidgetConfigTfcRollingSlopeMinus 38
      , row WidgetConfigValleyCurvatureMinus 39
      , row WidgetConfigRockElevationThresholdMinus 40
      , row WidgetConfigRockHardnessThresholdMinus 41
      , row WidgetConfigRockHardnessSecondaryMinus 42
      , row WidgetConfigGenCoordScaleMinus 43
      , row WidgetConfigGenOffsetXMinus 44
      , row WidgetConfigGenOffsetYMinus 45
      , row WidgetConfigExtentXMinus 46
      , row WidgetConfigExtentYMinus 47
      , row WidgetConfigEdgeNorthMinus 48
      , row WidgetConfigEdgeSouthMinus 49
      , row WidgetConfigEdgeEastMinus 50
      , row WidgetConfigEdgeWestMinus 51
      , row WidgetConfigEdgeFalloffMinus 52
      ]

    planet =
      [ row WidgetConfigPlanetRadiusMinus 0
      , row WidgetConfigAxialTiltMinus 1
      , row WidgetConfigInsolationMinus 2
      , row WidgetConfigOccWarmScaleMinus 3
      , row WidgetConfigOccColdScaleMinus 4
      , row WidgetConfigOccLatPeakDegMinus 5
      , row WidgetConfigOccLatWidthDegMinus 6
      ]

    climate =
      [ row WidgetConfigWaterMinus 0
      , row WidgetConfigEvapMinus 1
      , row WidgetConfigRainShadowMinus 2
      , row WidgetConfigWindDiffuseMinus 3
      , row WidgetConfigEquatorTempMinus 4
      , row WidgetConfigPoleTempMinus 5
      , row WidgetConfigLapseRateMinus 6
      , row WidgetConfigWindIterationsMinus 7
      , row WidgetConfigMoistureIterationsMinus 8
      , row WidgetConfigBoundaryMotionTempMinus 9
      , row WidgetConfigBoundaryMotionPrecipMinus 10
      , row WidgetConfigSliceLatCenterMinus 11
      , row WidgetConfigSliceLonCenterMinus 12
      , row WidgetConfigLatitudeExponentMinus 13
      , row WidgetConfigPlateHeightCoolingMinus 14
      , row WidgetConfigTempNoiseScaleMinus 15
      , row WidgetConfigOceanModerationMinus 16
      , row WidgetConfigOceanModerateTempMinus 17
      , row WidgetConfigAlbedoSensitivityMinus 18
      , row WidgetConfigAlbedoReferenceMinus 19
      , row WidgetConfigMoistAdvectMinus 20
      , row WidgetConfigMoistLocalMinus 21
      , row WidgetConfigMoistWindEvapScaleMinus 22
      , row WidgetConfigMoistEvapNoiseScaleMinus 23
      , row WidgetConfigMoistLandETCoeffMinus 24
      , row WidgetConfigMoistBareEvapFracMinus 25
      , row WidgetConfigMoistVegTranspFracMinus 26
      , row WidgetConfigMoistWindETScaleMinus 27
      , row WidgetConfigMoistCondensationRateMinus 28
      , row WidgetConfigMoistRecycleRateMinus 29
      , row WidgetConfigMoistITCZStrengthMinus 30
      , row WidgetConfigMoistITCZWidthMinus 31
      , row WidgetConfigOrographicScaleMinus 32
      , row WidgetConfigOrographicStepMinus 33
      , row WidgetConfigCoastalIterationsMinus 34
      , row WidgetConfigCoastalDiffuseMinus 35
      , row WidgetConfigCoastalMoistureBoostMinus 36
      , row WidgetConfigWindBeltStrengthMinus 37
      , row WidgetConfigWindBeltHarmonicsMinus 38
      , row WidgetConfigWindBeltBaseMinus 39
      , row WidgetConfigWindBeltRangeMinus 40
      , row WidgetConfigWindBeltSpeedScaleMinus 41
      , row WidgetConfigBndLandRangeMinus 42
      , row WidgetConfigBndTempConvergentMinus 43
      , row WidgetConfigBndTempDivergentMinus 44
      , row WidgetConfigBndTempTransformMinus 45
      , row WidgetConfigBndPrecipConvergentMinus 46
      , row WidgetConfigBndPrecipDivergentMinus 47
      , row WidgetConfigBndPrecipTransformMinus 48
      ]

    weather =
      [ row WidgetConfigWeatherTickMinus 0
      , row WidgetConfigWeatherPhaseMinus 1
      , row WidgetConfigWeatherAmplitudeMinus 2
      , row WidgetConfigSeasonCycleLengthMinus 3
      , row WidgetConfigJitterAmplitudeMinus 4
      , row WidgetConfigPressureBaseMinus 5
      , row WidgetConfigPressureTempScaleMinus 6
      , row WidgetConfigPressureCoriolisScaleMinus 7
      , row WidgetConfigSeasonalBaseMinus 8
      , row WidgetConfigSeasonalRangeMinus 9
      , row WidgetConfigHumidityNoiseScaleMinus 10
      , row WidgetConfigPrecipNoiseScaleMinus 11
      , row WidgetConfigWeatherITCZWidthMinus 12
      , row WidgetConfigWeatherITCZPrecipBoostMinus 13
      , row WidgetConfigPressureHumidityScaleMinus 14
      , row WidgetConfigPressureGradientWindScaleMinus 15
      , row WidgetConfigWindNoiseScaleMinus 16
      , row WidgetConfigITCZMigrationScaleMinus 17
      , row WidgetConfigCloudRHExponentMinus 18
      , row WidgetConfigCloudAlbedoEffectMinus 19
      , row WidgetConfigCloudPrecipBoostMinus 20
      ]

    biome =
      [ row WidgetConfigVegBaseMinus 0
      , row WidgetConfigVegBoostMinus 1
      , row WidgetConfigVegTempWeightMinus 2
      , row WidgetConfigVegPrecipWeightMinus 3
      , row WidgetConfigBtCoastalBandMinus 4
      , row WidgetConfigBtSnowMaxTempMinus 5
      , row WidgetConfigBtAlpineMaxTempMinus 6
      , row WidgetConfigBtIceCapTempMinus 7
      , row WidgetConfigBtMontaneMaxTempMinus 8
      , row WidgetConfigBtMontanePrecipMinus 9
      , row WidgetConfigBtCliffSlopeMinus 10
      , row WidgetConfigBtValleyMoistureMinus 11
      , row WidgetConfigBtDepressionMoistureMinus 12
      , row WidgetConfigBtPrecipWeightMinus 13
      , row WidgetConfigVbcTempMinMinus 14
      , row WidgetConfigVbcTempRangeMinus 15
      , row WidgetConfigVbcFertilityBoostMinus 16
      , row WidgetConfigVbcAlbedoBaseMinus 17
      , row WidgetConfigVbcAlbedoBareMinus 18
      , row WidgetConfigVbcAlbedoVegMinus 19
      , row WidgetConfigVbcOceanAlbedoMinus 20
      , row WidgetConfigVbcIceAlbedoMinus 21
      , row WidgetConfigBiomeSmoothingMinus 22
      , row WidgetConfigVolcanicAshBoostMinus 23
      , row WidgetConfigVolcanicLavaPenaltyMinus 24
      , row WidgetConfigBiomeFeedbackBlendMinus 25
      ]

    erosion =
      [ row WidgetConfigErosionHydraulicMinus 0
      , row WidgetConfigErosionThermalMinus 1
      , row WidgetConfigErosionRainRateMinus 2
      , row WidgetConfigErosionTalusMinus 3
      , row WidgetConfigErosionMaxDropMinus 4
      , row WidgetConfigGlacierSnowTempMinus 5
      , row WidgetConfigGlacierSnowRangeMinus 6
      , row WidgetConfigGlacierMeltTempMinus 7
      , row WidgetConfigGlacierMeltRateMinus 8
      , row WidgetConfigGlacierAccumScaleMinus 9
      , row WidgetConfigGlacierFlowItersMinus 10
      , row WidgetConfigGlacierFlowRateMinus 11
      , row WidgetConfigGlacierErosionScaleMinus 12
      , row WidgetConfigGlacierCarveScaleMinus 13
      , row WidgetConfigGlacierDepositScaleMinus 14
      , row WidgetConfigVentDensityMinus 15
      , row WidgetConfigVentThresholdMinus 16
      , row WidgetConfigHotspotScaleMinus 17
      , row WidgetConfigHotspotThresholdMinus 18
      , row WidgetConfigMagmaRechargeMinus 19
      , row WidgetConfigLavaScaleMinus 20
      , row WidgetConfigAshScaleMinus 21
      , row WidgetConfigVolcanicDepositScaleMinus 22
      , row WidgetConfigSoilMoistureThresholdMinus 23
      , row WidgetConfigSoilHardnessThresholdMinus 24
      , row WidgetConfigSoilFertilityMoistWeightMinus 25
      , row WidgetConfigSoilFertilityDepthWeightMinus 26
      , row WidgetConfigSinkBreachDepthMinus 27
      , row WidgetConfigStreamPowerMaxErosionMinus 28
      , row WidgetConfigRiverCarveMaxDepthMinus 29
      , row WidgetConfigCoastalErodeStrengthMinus 30
      , row WidgetConfigHydroHardnessWeightMinus 31
      , row WidgetConfigMinLakeSizeMinus 32
      , row WidgetConfigInlandSeaMinSizeMinus 33
      , row WidgetConfigRoughnessScaleMinus 34
      ]

hitTest :: [Widget] -> V2 Int -> Maybe WidgetId
hitTest widgets point =
  case filter (\w -> containsPoint (widgetRect w) point) widgets of
    (w:_) -> Just (widgetId w)
    [] -> Nothing
