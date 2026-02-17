module UI.Layout
  ( Layout
  , topBarHeight
  , topBarRect
  , layoutFor
  , layoutForSeed
  , leftGenButtonRect
  , leftPanelRect
  , leftToggleRect
  , leftTabRects
  , configSeedValueRect
  , configSeedLabelRect
  , configSeedRandomRect
  , leftChunkMinusRect
  , leftChunkPlusRect
  , leftViewRects
  , configToggleRect
  , configPanelRect
  , configTabRects
  , configPresetSaveRect
  , configPresetLoadRect
  , configResetRect
  , configRevertRect
  , configScrollAreaRect
  , configScrollBarRect
  , configRowTopPad
  , configWaterMinusRect
  , configWaterPlusRect
  , configWaterBarRect
  , configOrographicLiftMinusRect
  , configOrographicLiftPlusRect
  , configOrographicLiftBarRect
  , configRainShadowLossMinusRect
  , configRainShadowLossPlusRect
  , configRainShadowLossBarRect
  , configWindDiffuseMinusRect
  , configWindDiffusePlusRect
  , configWindDiffuseBarRect
  , configEquatorTempMinusRect
  , configEquatorTempPlusRect
  , configEquatorTempBarRect
  , configPoleTempMinusRect
  , configPoleTempPlusRect
  , configPoleTempBarRect
  , configLapseRateMinusRect
  , configLapseRatePlusRect
  , configLapseRateBarRect
  , configWindIterationsMinusRect
  , configWindIterationsPlusRect
  , configWindIterationsBarRect
  , configMoistureIterationsMinusRect
  , configMoistureIterationsPlusRect
  , configMoistureIterationsBarRect
  , configWeatherTickMinusRect
  , configWeatherTickPlusRect
  , configWeatherTickBarRect
  , configWeatherPhaseMinusRect
  , configWeatherPhasePlusRect
  , configWeatherPhaseBarRect
  , configWeatherAmplitudeMinusRect
  , configWeatherAmplitudePlusRect
  , configWeatherAmplitudeBarRect
  , configSeasonCycleLengthMinusRect
  , configSeasonCycleLengthPlusRect
  , configSeasonCycleLengthBarRect
  , configJitterAmplitudeMinusRect
  , configJitterAmplitudePlusRect
  , configJitterAmplitudeBarRect
  , configPressureBaseMinusRect
  , configPressureBasePlusRect
  , configPressureBaseBarRect
  , configPressureTempScaleMinusRect
  , configPressureTempScalePlusRect
  , configPressureTempScaleBarRect
  , configPressureCoriolisScaleMinusRect
  , configPressureCoriolisScalePlusRect
  , configPressureCoriolisScaleBarRect
  , configSeasonalBaseMinusRect
  , configSeasonalBasePlusRect
  , configSeasonalBaseBarRect
  , configSeasonalRangeMinusRect
  , configSeasonalRangePlusRect
  , configSeasonalRangeBarRect
  , configHumidityNoiseScaleMinusRect
  , configHumidityNoiseScalePlusRect
  , configHumidityNoiseScaleBarRect
  , configPrecipNoiseScaleMinusRect
  , configPrecipNoiseScalePlusRect
  , configPrecipNoiseScaleBarRect
  , configWeatherITCZWidthMinusRect
  , configWeatherITCZWidthPlusRect
  , configWeatherITCZWidthBarRect
  , configWeatherITCZPrecipBoostMinusRect
  , configWeatherITCZPrecipBoostPlusRect
  , configWeatherITCZPrecipBoostBarRect
  , configPressureHumidityScaleMinusRect
  , configPressureHumidityScalePlusRect
  , configPressureHumidityScaleBarRect
  , configPressureGradientWindScaleMinusRect
  , configPressureGradientWindScalePlusRect
  , configPressureGradientWindScaleBarRect
  , configWindNoiseScaleMinusRect
  , configWindNoiseScalePlusRect
  , configWindNoiseScaleBarRect
  , configITCZMigrationScaleMinusRect
  , configITCZMigrationScalePlusRect
  , configITCZMigrationScaleBarRect
  , configCloudRHExponentMinusRect
  , configCloudRHExponentPlusRect
  , configCloudRHExponentBarRect
  , configCloudAlbedoEffectMinusRect
  , configCloudAlbedoEffectPlusRect
  , configCloudAlbedoEffectBarRect
  , configCloudPrecipBoostMinusRect
  , configCloudPrecipBoostPlusRect
  , configCloudPrecipBoostBarRect
  , configVegBaseMinusRect
  , configVegBasePlusRect
  , configVegBaseBarRect
  , configVegBoostMinusRect
  , configVegBoostPlusRect
  , configVegBoostBarRect
  , configVegTempWeightMinusRect
  , configVegTempWeightPlusRect
  , configVegTempWeightBarRect
  , configVegPrecipWeightMinusRect
  , configVegPrecipWeightPlusRect
  , configVegPrecipWeightBarRect
  , configBtCoastalBandMinusRect
  , configBtCoastalBandPlusRect
  , configBtCoastalBandBarRect
  , configBtSnowMaxTempMinusRect
  , configBtSnowMaxTempPlusRect
  , configBtSnowMaxTempBarRect
  , configBtAlpineMaxTempMinusRect
  , configBtAlpineMaxTempPlusRect
  , configBtAlpineMaxTempBarRect
  , configBtIceCapTempMinusRect
  , configBtIceCapTempPlusRect
  , configBtIceCapTempBarRect
  , configBtMontaneMaxTempMinusRect
  , configBtMontaneMaxTempPlusRect
  , configBtMontaneMaxTempBarRect
  , configBtMontanePrecipMinusRect
  , configBtMontanePrecipPlusRect
  , configBtMontanePrecipBarRect
  , configBtCliffSlopeMinusRect
  , configBtCliffSlopePlusRect
  , configBtCliffSlopeBarRect
  , configBtValleyMoistureMinusRect
  , configBtValleyMoisturePlusRect
  , configBtValleyMoistureBarRect
  , configBtDepressionMoistureMinusRect
  , configBtDepressionMoisturePlusRect
  , configBtDepressionMoistureBarRect
  , configBtPrecipWeightMinusRect
  , configBtPrecipWeightPlusRect
  , configBtPrecipWeightBarRect
  , configVbcTempMinMinusRect
  , configVbcTempMinPlusRect
  , configVbcTempMinBarRect
  , configVbcTempRangeMinusRect
  , configVbcTempRangePlusRect
  , configVbcTempRangeBarRect
  , configVbcFertilityBoostMinusRect
  , configVbcFertilityBoostPlusRect
  , configVbcFertilityBoostBarRect
  , configVbcAlbedoBaseMinusRect
  , configVbcAlbedoBasePlusRect
  , configVbcAlbedoBaseBarRect
  , configVbcAlbedoBareMinusRect
  , configVbcAlbedoBarePlusRect
  , configVbcAlbedoBareBarRect
  , configVbcAlbedoVegMinusRect
  , configVbcAlbedoVegPlusRect
  , configVbcAlbedoVegBarRect
  , configVbcOceanAlbedoMinusRect
  , configVbcOceanAlbedoPlusRect
  , configVbcOceanAlbedoBarRect
  , configVbcIceAlbedoMinusRect
  , configVbcIceAlbedoPlusRect
  , configVbcIceAlbedoBarRect
  , configBiomeSmoothingMinusRect
  , configBiomeSmoothingPlusRect
  , configBiomeSmoothingBarRect
  , configVolcanicAshBoostMinusRect
  , configVolcanicAshBoostPlusRect
  , configVolcanicAshBoostBarRect
  , configVolcanicLavaPenaltyMinusRect
  , configVolcanicLavaPenaltyPlusRect
  , configVolcanicLavaPenaltyBarRect
  , configBiomeFeedbackBlendMinusRect
  , configBiomeFeedbackBlendPlusRect
  , configBiomeFeedbackBlendBarRect
  , configBoundaryMotionTempMinusRect
  , configBoundaryMotionTempPlusRect
  , configBoundaryMotionTempBarRect
  , configBoundaryMotionPrecipMinusRect
  , configBoundaryMotionPrecipPlusRect
  , configBoundaryMotionPrecipBarRect
  , configPlanetRadiusMinusRect
  , configPlanetRadiusPlusRect
  , configPlanetRadiusBarRect
  , configAxialTiltMinusRect
  , configAxialTiltPlusRect
  , configAxialTiltBarRect
  , configInsolationMinusRect
  , configInsolationPlusRect
  , configInsolationBarRect
  , configOccWarmScaleMinusRect
  , configOccWarmScalePlusRect
  , configOccWarmScaleBarRect
  , configOccColdScaleMinusRect
  , configOccColdScalePlusRect
  , configOccColdScaleBarRect
  , configOccLatPeakDegMinusRect
  , configOccLatPeakDegPlusRect
  , configOccLatPeakDegBarRect
  , configOccLatWidthDegMinusRect
  , configOccLatWidthDegPlusRect
  , configOccLatWidthDegBarRect
  , configSliceLatCenterMinusRect
  , configSliceLatCenterPlusRect
  , configSliceLatCenterBarRect
  , configSliceLonCenterMinusRect
  , configSliceLonCenterPlusRect
  , configSliceLonCenterBarRect
  , configLatitudeExponentMinusRect
  , configLatitudeExponentPlusRect
  , configLatitudeExponentBarRect
  , configPlateHeightCoolingMinusRect
  , configPlateHeightCoolingPlusRect
  , configPlateHeightCoolingBarRect
  , configTempNoiseScaleMinusRect
  , configTempNoiseScalePlusRect
  , configTempNoiseScaleBarRect
  , configOceanModerationMinusRect
  , configOceanModerationPlusRect
  , configOceanModerationBarRect
  , configOceanModerateTempMinusRect
  , configOceanModerateTempPlusRect
  , configOceanModerateTempBarRect
  , configAlbedoSensitivityMinusRect
  , configAlbedoSensitivityPlusRect
  , configAlbedoSensitivityBarRect
  , configAlbedoReferenceMinusRect
  , configAlbedoReferencePlusRect
  , configAlbedoReferenceBarRect
  , configMoistAdvectMinusRect
  , configMoistAdvectPlusRect
  , configMoistAdvectBarRect
  , configMoistLocalMinusRect
  , configMoistLocalPlusRect
  , configMoistLocalBarRect
  , configMoistWindEvapScaleMinusRect
  , configMoistWindEvapScalePlusRect
  , configMoistWindEvapScaleBarRect
  , configMoistEvapNoiseScaleMinusRect
  , configMoistEvapNoiseScalePlusRect
  , configMoistEvapNoiseScaleBarRect
  , configMoistBareEvapFracMinusRect
  , configMoistBareEvapFracPlusRect
  , configMoistBareEvapFracBarRect
  , configMoistVegTranspFracMinusRect
  , configMoistVegTranspFracPlusRect
  , configMoistVegTranspFracBarRect
  , configMoistWindETScaleMinusRect
  , configMoistWindETScalePlusRect
  , configMoistWindETScaleBarRect
  , configMoistCondensationRateMinusRect
  , configMoistCondensationRatePlusRect
  , configMoistCondensationRateBarRect
  , configMoistRecycleRateMinusRect
  , configMoistRecycleRatePlusRect
  , configMoistRecycleRateBarRect
  , configMoistITCZStrengthMinusRect
  , configMoistITCZStrengthPlusRect
  , configMoistITCZStrengthBarRect
  , configMoistITCZWidthMinusRect
  , configMoistITCZWidthPlusRect
  , configMoistITCZWidthBarRect
  , configOrographicScaleMinusRect
  , configOrographicScalePlusRect
  , configOrographicScaleBarRect
  , configOrographicStepMinusRect
  , configOrographicStepPlusRect
  , configOrographicStepBarRect
  , configCoastalIterationsMinusRect
  , configCoastalIterationsPlusRect
  , configCoastalIterationsBarRect
  , configCoastalDiffuseMinusRect
  , configCoastalDiffusePlusRect
  , configCoastalDiffuseBarRect
  , configCoastalMoistureBoostMinusRect
  , configCoastalMoistureBoostPlusRect
  , configCoastalMoistureBoostBarRect
  , configWindBeltStrengthMinusRect
  , configWindBeltStrengthPlusRect
  , configWindBeltStrengthBarRect
  , configWindBeltHarmonicsMinusRect
  , configWindBeltHarmonicsPlusRect
  , configWindBeltHarmonicsBarRect
  , configWindBeltBaseMinusRect
  , configWindBeltBasePlusRect
  , configWindBeltBaseBarRect
  , configWindBeltRangeMinusRect
  , configWindBeltRangePlusRect
  , configWindBeltRangeBarRect
  , configWindBeltSpeedScaleMinusRect
  , configWindBeltSpeedScalePlusRect
  , configWindBeltSpeedScaleBarRect
  , configBndLandRangeMinusRect
  , configBndLandRangePlusRect
  , configBndLandRangeBarRect
  , configBndTempConvergentMinusRect
  , configBndTempConvergentPlusRect
  , configBndTempConvergentBarRect
  , configBndTempDivergentMinusRect
  , configBndTempDivergentPlusRect
  , configBndTempDivergentBarRect
  , configBndTempTransformMinusRect
  , configBndTempTransformPlusRect
  , configBndTempTransformBarRect
  , configBndPrecipConvergentMinusRect
  , configBndPrecipConvergentPlusRect
  , configBndPrecipConvergentBarRect
  , configBndPrecipDivergentMinusRect
  , configBndPrecipDivergentPlusRect
  , configBndPrecipDivergentBarRect
  , configBndPrecipTransformMinusRect
  , configBndPrecipTransformPlusRect
  , configBndPrecipTransformBarRect
  , configErosionHydraulicMinusRect
  , configErosionHydraulicPlusRect
  , configErosionHydraulicBarRect
  , configErosionThermalMinusRect
  , configErosionThermalPlusRect
  , configErosionThermalBarRect
  , configErosionRainRateMinusRect
  , configErosionRainRatePlusRect
  , configErosionRainRateBarRect
  , configErosionTalusMinusRect
  , configErosionTalusPlusRect
  , configErosionTalusBarRect
  , configErosionMaxDropMinusRect
  , configErosionMaxDropPlusRect
  , configErosionMaxDropBarRect
  , configGlacierSnowTempMinusRect
  , configGlacierSnowTempPlusRect
  , configGlacierSnowTempBarRect
  , configGlacierSnowRangeMinusRect
  , configGlacierSnowRangePlusRect
  , configGlacierSnowRangeBarRect
  , configGlacierMeltTempMinusRect
  , configGlacierMeltTempPlusRect
  , configGlacierMeltTempBarRect
  , configGlacierMeltRateMinusRect
  , configGlacierMeltRatePlusRect
  , configGlacierMeltRateBarRect
  , configGlacierAccumScaleMinusRect
  , configGlacierAccumScalePlusRect
  , configGlacierAccumScaleBarRect
  , configGlacierFlowItersMinusRect
  , configGlacierFlowItersPlusRect
  , configGlacierFlowItersBarRect
  , configGlacierFlowRateMinusRect
  , configGlacierFlowRatePlusRect
  , configGlacierFlowRateBarRect
  , configGlacierErosionScaleMinusRect
  , configGlacierErosionScalePlusRect
  , configGlacierErosionScaleBarRect
  , configGlacierCarveScaleMinusRect
  , configGlacierCarveScalePlusRect
  , configGlacierCarveScaleBarRect
  , configGlacierDepositScaleMinusRect
  , configGlacierDepositScalePlusRect
  , configGlacierDepositScaleBarRect
  , configVentDensityMinusRect
  , configVentDensityPlusRect
  , configVentDensityBarRect
  , configVentThresholdMinusRect
  , configVentThresholdPlusRect
  , configVentThresholdBarRect
  , configHotspotScaleMinusRect
  , configHotspotScalePlusRect
  , configHotspotScaleBarRect
  , configHotspotThresholdMinusRect
  , configHotspotThresholdPlusRect
  , configHotspotThresholdBarRect
  , configMagmaRechargeMinusRect
  , configMagmaRechargePlusRect
  , configMagmaRechargeBarRect
  , configLavaScaleMinusRect
  , configLavaScalePlusRect
  , configLavaScaleBarRect
  , configAshScaleMinusRect
  , configAshScalePlusRect
  , configAshScaleBarRect
  , configVolcanicDepositScaleMinusRect
  , configVolcanicDepositScalePlusRect
  , configVolcanicDepositScaleBarRect
  , configSoilMoistureThresholdMinusRect
  , configSoilMoistureThresholdPlusRect
  , configSoilMoistureThresholdBarRect
  , configSoilHardnessThresholdMinusRect
  , configSoilHardnessThresholdPlusRect
  , configSoilHardnessThresholdBarRect
  , configSoilFertilityMoistWeightMinusRect
  , configSoilFertilityMoistWeightPlusRect
  , configSoilFertilityMoistWeightBarRect
  , configSoilFertilityDepthWeightMinusRect
  , configSoilFertilityDepthWeightPlusRect
  , configSoilFertilityDepthWeightBarRect
  , configSinkBreachDepthMinusRect
  , configSinkBreachDepthPlusRect
  , configSinkBreachDepthBarRect
  , configStreamPowerMaxErosionMinusRect
  , configStreamPowerMaxErosionPlusRect
  , configStreamPowerMaxErosionBarRect
  , configRiverCarveMaxDepthMinusRect
  , configRiverCarveMaxDepthPlusRect
  , configRiverCarveMaxDepthBarRect
  , configCoastalErodeStrengthMinusRect
  , configCoastalErodeStrengthPlusRect
  , configCoastalErodeStrengthBarRect
  , configHydroHardnessWeightMinusRect
  , configHydroHardnessWeightPlusRect
  , configHydroHardnessWeightBarRect
  , configMinLakeSizeMinusRect
  , configMinLakeSizePlusRect
  , configMinLakeSizeBarRect
  , configInlandSeaMinSizeMinusRect
  , configInlandSeaMinSizePlusRect
  , configInlandSeaMinSizeBarRect
  , configRoughnessScaleMinusRect
  , configRoughnessScalePlusRect
  , configRoughnessScaleBarRect
  , configGenScaleMinusRect
  , configGenScalePlusRect
  , configGenScaleBarRect
  , configGenCoordScaleMinusRect
  , configGenCoordScalePlusRect
  , configGenCoordScaleBarRect
  , configGenOffsetXMinusRect
  , configGenOffsetXPlusRect
  , configGenOffsetXBarRect
  , configGenOffsetYMinusRect
  , configGenOffsetYPlusRect
  , configGenOffsetYBarRect
  , configGenFrequencyMinusRect
  , configGenFrequencyPlusRect
  , configGenFrequencyBarRect
  , configGenOctavesMinusRect
  , configGenOctavesPlusRect
  , configGenOctavesBarRect
  , configGenLacunarityMinusRect
  , configGenLacunarityPlusRect
  , configGenLacunarityBarRect
  , configGenGainMinusRect
  , configGenGainPlusRect
  , configGenGainBarRect
  , configGenWarpScaleMinusRect
  , configGenWarpScalePlusRect
  , configGenWarpScaleBarRect
  , configGenWarpStrengthMinusRect
  , configGenWarpStrengthPlusRect
  , configGenWarpStrengthBarRect
  , configExtentXMinusRect
  , configExtentXPlusRect
  , configExtentXBarRect
  , configExtentYMinusRect
  , configExtentYPlusRect
  , configExtentYBarRect
  , configEdgeNorthMinusRect
  , configEdgeNorthPlusRect
  , configEdgeNorthBarRect
  , configEdgeSouthMinusRect
  , configEdgeSouthPlusRect
  , configEdgeSouthBarRect
  , configEdgeEastMinusRect
  , configEdgeEastPlusRect
  , configEdgeEastBarRect
  , configEdgeWestMinusRect
  , configEdgeWestPlusRect
  , configEdgeWestBarRect
  , configEdgeFalloffMinusRect
  , configEdgeFalloffPlusRect
  , configEdgeFalloffBarRect
  , configPlateSizeMinusRect
  , configPlateSizePlusRect
  , configPlateSizeBarRect
  , configUpliftMinusRect
  , configUpliftPlusRect
  , configUpliftBarRect
  , configRiftDepthMinusRect
  , configRiftDepthPlusRect
  , configRiftDepthBarRect
  , configDetailScaleMinusRect
  , configDetailScalePlusRect
  , configDetailScaleBarRect
  , configPlateSpeedMinusRect
  , configPlateSpeedPlusRect
  , configPlateSpeedBarRect
  , configBoundarySharpnessMinusRect
  , configBoundarySharpnessPlusRect
  , configBoundarySharpnessBarRect
  , configBoundaryNoiseScaleMinusRect
  , configBoundaryNoiseScalePlusRect
  , configBoundaryNoiseScaleBarRect
  , configBoundaryNoiseStrengthMinusRect
  , configBoundaryNoiseStrengthPlusRect
  , configBoundaryNoiseStrengthBarRect
  , configBoundaryWarpOctavesMinusRect
  , configBoundaryWarpOctavesPlusRect
  , configBoundaryWarpOctavesBarRect
  , configBoundaryWarpLacunarityMinusRect
  , configBoundaryWarpLacunarityPlusRect
  , configBoundaryWarpLacunarityBarRect
  , configBoundaryWarpGainMinusRect
  , configBoundaryWarpGainPlusRect
  , configBoundaryWarpGainBarRect
  , configPlateMergeScaleMinusRect
  , configPlateMergeScalePlusRect
  , configPlateMergeScaleBarRect
  , configPlateMergeBiasMinusRect
  , configPlateMergeBiasPlusRect
  , configPlateMergeBiasBarRect
  , configPlateDetailScaleMinusRect
  , configPlateDetailScalePlusRect
  , configPlateDetailScaleBarRect
  , configPlateDetailStrengthMinusRect
  , configPlateDetailStrengthPlusRect
  , configPlateDetailStrengthBarRect
  , configPlateRidgeStrengthMinusRect
  , configPlateRidgeStrengthPlusRect
  , configPlateRidgeStrengthBarRect
  , configPlateHeightBaseMinusRect
  , configPlateHeightBasePlusRect
  , configPlateHeightBaseBarRect
  , configPlateHeightVarianceMinusRect
  , configPlateHeightVariancePlusRect
  , configPlateHeightVarianceBarRect
  , configPlateHardnessBaseMinusRect
  , configPlateHardnessBasePlusRect
  , configPlateHardnessBaseBarRect
  , configPlateHardnessVarianceMinusRect
  , configPlateHardnessVariancePlusRect
  , configPlateHardnessVarianceBarRect
  , configTrenchDepthMinusRect
  , configTrenchDepthPlusRect
  , configTrenchDepthBarRect
  , configRidgeHeightMinusRect
  , configRidgeHeightPlusRect
  , configRidgeHeightBarRect
  , configPlateBiasStrengthMinusRect
  , configPlateBiasStrengthPlusRect
  , configPlateBiasStrengthBarRect
  , configPlateBiasCenterMinusRect
  , configPlateBiasCenterPlusRect
  , configPlateBiasCenterBarRect
  , configPlateBiasEdgeMinusRect
  , configPlateBiasEdgePlusRect
  , configPlateBiasEdgeBarRect
  , configPlateBiasNorthMinusRect
  , configPlateBiasNorthPlusRect
  , configPlateBiasNorthBarRect
  , configPlateBiasSouthMinusRect
  , configPlateBiasSouthPlusRect
  , configPlateBiasSouthBarRect
  , configTfcCliffSlopeMinusRect
  , configTfcCliffSlopePlusRect
  , configTfcCliffSlopeBarRect
  , configTfcMountainSlopeMinusRect
  , configTfcMountainSlopePlusRect
  , configTfcMountainSlopeBarRect
  , configTfcMountainReliefMinusRect
  , configTfcMountainReliefPlusRect
  , configTfcMountainReliefBarRect
  , configTfcHillSlopeMinusRect
  , configTfcHillSlopePlusRect
  , configTfcHillSlopeBarRect
  , configTfcRollingSlopeMinusRect
  , configTfcRollingSlopePlusRect
  , configTfcRollingSlopeBarRect
  , configValleyCurvatureMinusRect
  , configValleyCurvaturePlusRect
  , configValleyCurvatureBarRect
  , configRockElevationThresholdMinusRect
  , configRockElevationThresholdPlusRect
  , configRockElevationThresholdBarRect
  , configRockHardnessThresholdMinusRect
  , configRockHardnessThresholdPlusRect
  , configRockHardnessThresholdBarRect
  , configRockHardnessSecondaryMinusRect
  , configRockHardnessSecondaryPlusRect
  , configRockHardnessSecondaryBarRect
  , configParamRowRect
  , configChunkMinusRect
  , configChunkPlusRect
  , configChunkValueRect
  , logPanelRect
  , logHeaderRect
  , logBodyRect
  , logFilterRects
  , menuPanelRect
  , menuSaveRect
  , menuLoadRect
  , menuExitRect
    -- Preset save dialog
  , presetSaveDialogRect
  , presetSaveInputRect
  , presetSaveOkRect
  , presetSaveCancelRect
    -- Preset load dialog
  , presetLoadDialogRect
  , presetLoadListRect
  , presetLoadItemRect
  , presetLoadOkRect
  , presetLoadCancelRect
    -- World save dialog
  , worldSaveDialogRect
  , worldSaveInputRect
  , worldSaveOkRect
  , worldSaveCancelRect
    -- World load dialog
  , worldLoadDialogRect
  , worldLoadListRect
  , worldLoadItemRect
  , worldLoadOkRect
  , worldLoadCancelRect
  ) where

import Linear (V2(..))
import UI.Widgets (Rect(..))

data Layout = Layout
  { layoutSize :: V2 Int
  , layoutLogHeight :: Int
  , layoutSeedWidth :: Int
  } deriving (Eq, Show)

-- | Height of the top bar in pixels.
topBarHeight :: Int
topBarHeight = 28

-- | Full-width bar at the top of the window displaying the world name.
topBarRect :: Layout -> Rect
topBarRect (Layout (V2 w _) _ _) =
  Rect (V2 0 0, V2 w topBarHeight)

layoutFor :: V2 Int -> Int -> Layout
layoutFor size logHeight = layoutForSeed size logHeight 120

layoutForSeed :: V2 Int -> Int -> Int -> Layout
layoutForSeed size logHeight seedWidth = Layout
  { layoutSize = size
  , layoutLogHeight = logHeight
  , layoutSeedWidth = seedWidth
  }

-- | Generate button inside the left panel, below seed value (Row 4).
leftGenButtonRect :: Layout -> Rect
leftGenButtonRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 4 * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) 28)

leftPanelRect :: Layout -> Rect
leftPanelRect (Layout (V2 _ h) logHeight _) =
  let panelW = 240
      x = 16
      y = 56 + topBarHeight
  in Rect (V2 x y, V2 panelW (h - logHeight - 72 - topBarHeight))

leftToggleRect :: Layout -> Rect
leftToggleRect layout =
  let Rect (V2 x y, V2 _ _) = leftPanelRect layout
      pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

leftTabRects :: Layout -> (Rect, Rect)
leftTabRects layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      tabH = 22
      gap = 8
      toggleH = 22
      tabY = y + 8 + toggleH + 8
      available = w - pad * 2 - gap
      tabW = available `div` 2
      r1 = Rect (V2 (x + pad) tabY, V2 tabW tabH)
      r2 = Rect (V2 (x + pad + tabW + gap) tabY, V2 tabW tabH)
  in (r1, r2)

configToggleRect :: Layout -> Rect
configToggleRect layout =
  let Rect (V2 x y, V2 _ _) = configPanelRect layout
      pad = 12
      buttonW = 44
      buttonH = 22
  in Rect (V2 (x + pad) (y + 8), V2 buttonW buttonH)

configPanelRect :: Layout -> Rect
configPanelRect (Layout (V2 w h) logHeight seedWidth) =
  let pad = 12
      buttonW = 64
      minW = 300
      desiredW = max minW (pad * 2 + seedWidth + buttonW + 20)
      panelX = w - desiredW - 16
  in Rect (V2 panelX (16 + topBarHeight), V2 desiredW (h - logHeight - 32 - topBarHeight))

configTabRects :: Layout -> (Rect, Rect, Rect, Rect, Rect, Rect)
configTabRects layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      pad = 12
      tabH = 22
      gap = 4
      toggleH = 22
      available = w - pad * 2 - gap * 5
      tabW = available `div` 6
      y0 = y + 8 + toggleH + 8
      r1 = Rect (V2 (x + pad) y0, V2 tabW tabH)
      r2 = Rect (V2 (x + pad + (tabW + gap)) y0, V2 tabW tabH)
      r3 = Rect (V2 (x + pad + (tabW + gap) * 2) y0, V2 tabW tabH)
      r4 = Rect (V2 (x + pad + (tabW + gap) * 3) y0, V2 tabW tabH)
      r5 = Rect (V2 (x + pad + (tabW + gap) * 4) y0, V2 tabW tabH)
      r6 = Rect (V2 (x + pad + (tabW + gap) * 5) y0, V2 tabW tabH)
  in (r1, r2, r3, r4, r5, r6)

configWaterMinusRect :: Layout -> Rect
configWaterMinusRect = configParamMinusRect 0

configWaterPlusRect :: Layout -> Rect
configWaterPlusRect = configParamPlusRect 0

configWaterBarRect :: Layout -> Rect
configWaterBarRect = configParamBarRect 0

configOrographicLiftMinusRect :: Layout -> Rect
configOrographicLiftMinusRect = configParamMinusRect 1

configOrographicLiftPlusRect :: Layout -> Rect
configOrographicLiftPlusRect = configParamPlusRect 1

configOrographicLiftBarRect :: Layout -> Rect
configOrographicLiftBarRect = configParamBarRect 1

configRainShadowLossMinusRect :: Layout -> Rect
configRainShadowLossMinusRect = configParamMinusRect 2

configRainShadowLossPlusRect :: Layout -> Rect
configRainShadowLossPlusRect = configParamPlusRect 2

configRainShadowLossBarRect :: Layout -> Rect
configRainShadowLossBarRect = configParamBarRect 2

configWindDiffuseMinusRect :: Layout -> Rect
configWindDiffuseMinusRect = configParamMinusRect 3

configWindDiffusePlusRect :: Layout -> Rect
configWindDiffusePlusRect = configParamPlusRect 3

configWindDiffuseBarRect :: Layout -> Rect
configWindDiffuseBarRect = configParamBarRect 3

configEquatorTempMinusRect :: Layout -> Rect
configEquatorTempMinusRect = configParamMinusRect 4

configEquatorTempPlusRect :: Layout -> Rect
configEquatorTempPlusRect = configParamPlusRect 4

configEquatorTempBarRect :: Layout -> Rect
configEquatorTempBarRect = configParamBarRect 4

configPoleTempMinusRect :: Layout -> Rect
configPoleTempMinusRect = configParamMinusRect 5

configPoleTempPlusRect :: Layout -> Rect
configPoleTempPlusRect = configParamPlusRect 5

configPoleTempBarRect :: Layout -> Rect
configPoleTempBarRect = configParamBarRect 5

configLapseRateMinusRect :: Layout -> Rect
configLapseRateMinusRect = configParamMinusRect 6

configLapseRatePlusRect :: Layout -> Rect
configLapseRatePlusRect = configParamPlusRect 6

configLapseRateBarRect :: Layout -> Rect
configLapseRateBarRect = configParamBarRect 6

configWindIterationsMinusRect :: Layout -> Rect
configWindIterationsMinusRect = configParamMinusRect 7

configWindIterationsPlusRect :: Layout -> Rect
configWindIterationsPlusRect = configParamPlusRect 7

configWindIterationsBarRect :: Layout -> Rect
configWindIterationsBarRect = configParamBarRect 7

configMoistureIterationsMinusRect :: Layout -> Rect
configMoistureIterationsMinusRect = configParamMinusRect 8

configMoistureIterationsPlusRect :: Layout -> Rect
configMoistureIterationsPlusRect = configParamPlusRect 8

configMoistureIterationsBarRect :: Layout -> Rect
configMoistureIterationsBarRect = configParamBarRect 8

configWeatherTickMinusRect :: Layout -> Rect
configWeatherTickMinusRect = configParamMinusRect 0

configWeatherTickPlusRect :: Layout -> Rect
configWeatherTickPlusRect = configParamPlusRect 0

configWeatherTickBarRect :: Layout -> Rect
configWeatherTickBarRect = configParamBarRect 0

configWeatherPhaseMinusRect :: Layout -> Rect
configWeatherPhaseMinusRect = configParamMinusRect 1

configWeatherPhasePlusRect :: Layout -> Rect
configWeatherPhasePlusRect = configParamPlusRect 1

configWeatherPhaseBarRect :: Layout -> Rect
configWeatherPhaseBarRect = configParamBarRect 1

configWeatherAmplitudeMinusRect :: Layout -> Rect
configWeatherAmplitudeMinusRect = configParamMinusRect 1

configWeatherAmplitudePlusRect :: Layout -> Rect
configWeatherAmplitudePlusRect = configParamPlusRect 1

configWeatherAmplitudeBarRect :: Layout -> Rect
configWeatherAmplitudeBarRect = configParamBarRect 1

configSeasonCycleLengthMinusRect :: Layout -> Rect
configSeasonCycleLengthMinusRect = configParamMinusRect 2

configSeasonCycleLengthPlusRect :: Layout -> Rect
configSeasonCycleLengthPlusRect = configParamPlusRect 2

configSeasonCycleLengthBarRect :: Layout -> Rect
configSeasonCycleLengthBarRect = configParamBarRect 2

configJitterAmplitudeMinusRect :: Layout -> Rect
configJitterAmplitudeMinusRect = configParamMinusRect 3

configJitterAmplitudePlusRect :: Layout -> Rect
configJitterAmplitudePlusRect = configParamPlusRect 3

configJitterAmplitudeBarRect :: Layout -> Rect
configJitterAmplitudeBarRect = configParamBarRect 3

configPressureBaseMinusRect :: Layout -> Rect
configPressureBaseMinusRect = configParamMinusRect 4

configPressureBasePlusRect :: Layout -> Rect
configPressureBasePlusRect = configParamPlusRect 4

configPressureBaseBarRect :: Layout -> Rect
configPressureBaseBarRect = configParamBarRect 4

configPressureTempScaleMinusRect :: Layout -> Rect
configPressureTempScaleMinusRect = configParamMinusRect 5

configPressureTempScalePlusRect :: Layout -> Rect
configPressureTempScalePlusRect = configParamPlusRect 5

configPressureTempScaleBarRect :: Layout -> Rect
configPressureTempScaleBarRect = configParamBarRect 5

configPressureCoriolisScaleMinusRect :: Layout -> Rect
configPressureCoriolisScaleMinusRect = configParamMinusRect 6

configPressureCoriolisScalePlusRect :: Layout -> Rect
configPressureCoriolisScalePlusRect = configParamPlusRect 6

configPressureCoriolisScaleBarRect :: Layout -> Rect
configPressureCoriolisScaleBarRect = configParamBarRect 6

configSeasonalBaseMinusRect :: Layout -> Rect
configSeasonalBaseMinusRect = configParamMinusRect 7

configSeasonalBasePlusRect :: Layout -> Rect
configSeasonalBasePlusRect = configParamPlusRect 7

configSeasonalBaseBarRect :: Layout -> Rect
configSeasonalBaseBarRect = configParamBarRect 7

configSeasonalRangeMinusRect :: Layout -> Rect
configSeasonalRangeMinusRect = configParamMinusRect 8

configSeasonalRangePlusRect :: Layout -> Rect
configSeasonalRangePlusRect = configParamPlusRect 8

configSeasonalRangeBarRect :: Layout -> Rect
configSeasonalRangeBarRect = configParamBarRect 8

configHumidityNoiseScaleMinusRect :: Layout -> Rect
configHumidityNoiseScaleMinusRect = configParamMinusRect 9

configHumidityNoiseScalePlusRect :: Layout -> Rect
configHumidityNoiseScalePlusRect = configParamPlusRect 9

configHumidityNoiseScaleBarRect :: Layout -> Rect
configHumidityNoiseScaleBarRect = configParamBarRect 9

configPrecipNoiseScaleMinusRect :: Layout -> Rect
configPrecipNoiseScaleMinusRect = configParamMinusRect 10

configPrecipNoiseScalePlusRect :: Layout -> Rect
configPrecipNoiseScalePlusRect = configParamPlusRect 10

configPrecipNoiseScaleBarRect :: Layout -> Rect
configPrecipNoiseScaleBarRect = configParamBarRect 10

configWeatherITCZWidthMinusRect :: Layout -> Rect
configWeatherITCZWidthMinusRect = configParamMinusRect 11

configWeatherITCZWidthPlusRect :: Layout -> Rect
configWeatherITCZWidthPlusRect = configParamPlusRect 11

configWeatherITCZWidthBarRect :: Layout -> Rect
configWeatherITCZWidthBarRect = configParamBarRect 11

configWeatherITCZPrecipBoostMinusRect :: Layout -> Rect
configWeatherITCZPrecipBoostMinusRect = configParamMinusRect 12

configWeatherITCZPrecipBoostPlusRect :: Layout -> Rect
configWeatherITCZPrecipBoostPlusRect = configParamPlusRect 12

configWeatherITCZPrecipBoostBarRect :: Layout -> Rect
configWeatherITCZPrecipBoostBarRect = configParamBarRect 12

configPressureHumidityScaleMinusRect :: Layout -> Rect
configPressureHumidityScaleMinusRect = configParamMinusRect 13

configPressureHumidityScalePlusRect :: Layout -> Rect
configPressureHumidityScalePlusRect = configParamPlusRect 13

configPressureHumidityScaleBarRect :: Layout -> Rect
configPressureHumidityScaleBarRect = configParamBarRect 13

configPressureGradientWindScaleMinusRect :: Layout -> Rect
configPressureGradientWindScaleMinusRect = configParamMinusRect 14

configPressureGradientWindScalePlusRect :: Layout -> Rect
configPressureGradientWindScalePlusRect = configParamPlusRect 14

configPressureGradientWindScaleBarRect :: Layout -> Rect
configPressureGradientWindScaleBarRect = configParamBarRect 14

configWindNoiseScaleMinusRect :: Layout -> Rect
configWindNoiseScaleMinusRect = configParamMinusRect 15

configWindNoiseScalePlusRect :: Layout -> Rect
configWindNoiseScalePlusRect = configParamPlusRect 15

configWindNoiseScaleBarRect :: Layout -> Rect
configWindNoiseScaleBarRect = configParamBarRect 15

configITCZMigrationScaleMinusRect :: Layout -> Rect
configITCZMigrationScaleMinusRect = configParamMinusRect 16

configITCZMigrationScalePlusRect :: Layout -> Rect
configITCZMigrationScalePlusRect = configParamPlusRect 16

configITCZMigrationScaleBarRect :: Layout -> Rect
configITCZMigrationScaleBarRect = configParamBarRect 16

configCloudRHExponentMinusRect :: Layout -> Rect
configCloudRHExponentMinusRect = configParamMinusRect 17

configCloudRHExponentPlusRect :: Layout -> Rect
configCloudRHExponentPlusRect = configParamPlusRect 17

configCloudRHExponentBarRect :: Layout -> Rect
configCloudRHExponentBarRect = configParamBarRect 17

configCloudAlbedoEffectMinusRect :: Layout -> Rect
configCloudAlbedoEffectMinusRect = configParamMinusRect 18

configCloudAlbedoEffectPlusRect :: Layout -> Rect
configCloudAlbedoEffectPlusRect = configParamPlusRect 18

configCloudAlbedoEffectBarRect :: Layout -> Rect
configCloudAlbedoEffectBarRect = configParamBarRect 18

configCloudPrecipBoostMinusRect :: Layout -> Rect
configCloudPrecipBoostMinusRect = configParamMinusRect 19

configCloudPrecipBoostPlusRect :: Layout -> Rect
configCloudPrecipBoostPlusRect = configParamPlusRect 19

configCloudPrecipBoostBarRect :: Layout -> Rect
configCloudPrecipBoostBarRect = configParamBarRect 19

configVegBaseMinusRect :: Layout -> Rect
configVegBaseMinusRect = configParamMinusRect 0

configVegBasePlusRect :: Layout -> Rect
configVegBasePlusRect = configParamPlusRect 0

configVegBaseBarRect :: Layout -> Rect
configVegBaseBarRect = configParamBarRect 0

configVegBoostMinusRect :: Layout -> Rect
configVegBoostMinusRect = configParamMinusRect 1

configVegBoostPlusRect :: Layout -> Rect
configVegBoostPlusRect = configParamPlusRect 1

configVegBoostBarRect :: Layout -> Rect
configVegBoostBarRect = configParamBarRect 1

configVegTempWeightMinusRect :: Layout -> Rect
configVegTempWeightMinusRect = configParamMinusRect 1

configVegTempWeightPlusRect :: Layout -> Rect
configVegTempWeightPlusRect = configParamPlusRect 1

configVegTempWeightBarRect :: Layout -> Rect
configVegTempWeightBarRect = configParamBarRect 1

configVegPrecipWeightMinusRect :: Layout -> Rect
configVegPrecipWeightMinusRect = configParamMinusRect 2

configVegPrecipWeightPlusRect :: Layout -> Rect
configVegPrecipWeightPlusRect = configParamPlusRect 2

configVegPrecipWeightBarRect :: Layout -> Rect
configVegPrecipWeightBarRect = configParamBarRect 2

configBtCoastalBandMinusRect :: Layout -> Rect
configBtCoastalBandMinusRect = configParamMinusRect 3
configBtCoastalBandPlusRect :: Layout -> Rect
configBtCoastalBandPlusRect = configParamPlusRect 3
configBtCoastalBandBarRect :: Layout -> Rect
configBtCoastalBandBarRect = configParamBarRect 3

configBtSnowMaxTempMinusRect :: Layout -> Rect
configBtSnowMaxTempMinusRect = configParamMinusRect 4
configBtSnowMaxTempPlusRect :: Layout -> Rect
configBtSnowMaxTempPlusRect = configParamPlusRect 4
configBtSnowMaxTempBarRect :: Layout -> Rect
configBtSnowMaxTempBarRect = configParamBarRect 4

configBtAlpineMaxTempMinusRect :: Layout -> Rect
configBtAlpineMaxTempMinusRect = configParamMinusRect 5
configBtAlpineMaxTempPlusRect :: Layout -> Rect
configBtAlpineMaxTempPlusRect = configParamPlusRect 5
configBtAlpineMaxTempBarRect :: Layout -> Rect
configBtAlpineMaxTempBarRect = configParamBarRect 5

configBtIceCapTempMinusRect :: Layout -> Rect
configBtIceCapTempMinusRect = configParamMinusRect 6
configBtIceCapTempPlusRect :: Layout -> Rect
configBtIceCapTempPlusRect = configParamPlusRect 6
configBtIceCapTempBarRect :: Layout -> Rect
configBtIceCapTempBarRect = configParamBarRect 6

configBtMontaneMaxTempMinusRect :: Layout -> Rect
configBtMontaneMaxTempMinusRect = configParamMinusRect 7
configBtMontaneMaxTempPlusRect :: Layout -> Rect
configBtMontaneMaxTempPlusRect = configParamPlusRect 7
configBtMontaneMaxTempBarRect :: Layout -> Rect
configBtMontaneMaxTempBarRect = configParamBarRect 7

configBtMontanePrecipMinusRect :: Layout -> Rect
configBtMontanePrecipMinusRect = configParamMinusRect 8
configBtMontanePrecipPlusRect :: Layout -> Rect
configBtMontanePrecipPlusRect = configParamPlusRect 8
configBtMontanePrecipBarRect :: Layout -> Rect
configBtMontanePrecipBarRect = configParamBarRect 8

configBtCliffSlopeMinusRect :: Layout -> Rect
configBtCliffSlopeMinusRect = configParamMinusRect 9
configBtCliffSlopePlusRect :: Layout -> Rect
configBtCliffSlopePlusRect = configParamPlusRect 9
configBtCliffSlopeBarRect :: Layout -> Rect
configBtCliffSlopeBarRect = configParamBarRect 9

configBtValleyMoistureMinusRect :: Layout -> Rect
configBtValleyMoistureMinusRect = configParamMinusRect 10
configBtValleyMoisturePlusRect :: Layout -> Rect
configBtValleyMoisturePlusRect = configParamPlusRect 10
configBtValleyMoistureBarRect :: Layout -> Rect
configBtValleyMoistureBarRect = configParamBarRect 10

configBtDepressionMoistureMinusRect :: Layout -> Rect
configBtDepressionMoistureMinusRect = configParamMinusRect 11
configBtDepressionMoisturePlusRect :: Layout -> Rect
configBtDepressionMoisturePlusRect = configParamPlusRect 11
configBtDepressionMoistureBarRect :: Layout -> Rect
configBtDepressionMoistureBarRect = configParamBarRect 11

configBtPrecipWeightMinusRect :: Layout -> Rect
configBtPrecipWeightMinusRect = configParamMinusRect 12
configBtPrecipWeightPlusRect :: Layout -> Rect
configBtPrecipWeightPlusRect = configParamPlusRect 12
configBtPrecipWeightBarRect :: Layout -> Rect
configBtPrecipWeightBarRect = configParamBarRect 12

configVbcTempMinMinusRect :: Layout -> Rect
configVbcTempMinMinusRect = configParamMinusRect 13
configVbcTempMinPlusRect :: Layout -> Rect
configVbcTempMinPlusRect = configParamPlusRect 13
configVbcTempMinBarRect :: Layout -> Rect
configVbcTempMinBarRect = configParamBarRect 13

configVbcTempRangeMinusRect :: Layout -> Rect
configVbcTempRangeMinusRect = configParamMinusRect 14
configVbcTempRangePlusRect :: Layout -> Rect
configVbcTempRangePlusRect = configParamPlusRect 14
configVbcTempRangeBarRect :: Layout -> Rect
configVbcTempRangeBarRect = configParamBarRect 14

configVbcFertilityBoostMinusRect :: Layout -> Rect
configVbcFertilityBoostMinusRect = configParamMinusRect 15
configVbcFertilityBoostPlusRect :: Layout -> Rect
configVbcFertilityBoostPlusRect = configParamPlusRect 15
configVbcFertilityBoostBarRect :: Layout -> Rect
configVbcFertilityBoostBarRect = configParamBarRect 15

configVbcAlbedoBaseMinusRect :: Layout -> Rect
configVbcAlbedoBaseMinusRect = configParamMinusRect 16
configVbcAlbedoBasePlusRect :: Layout -> Rect
configVbcAlbedoBasePlusRect = configParamPlusRect 16
configVbcAlbedoBaseBarRect :: Layout -> Rect
configVbcAlbedoBaseBarRect = configParamBarRect 16

configVbcAlbedoBareMinusRect :: Layout -> Rect
configVbcAlbedoBareMinusRect = configParamMinusRect 17
configVbcAlbedoBarePlusRect :: Layout -> Rect
configVbcAlbedoBarePlusRect = configParamPlusRect 17
configVbcAlbedoBareBarRect :: Layout -> Rect
configVbcAlbedoBareBarRect = configParamBarRect 17

configVbcAlbedoVegMinusRect :: Layout -> Rect
configVbcAlbedoVegMinusRect = configParamMinusRect 18
configVbcAlbedoVegPlusRect :: Layout -> Rect
configVbcAlbedoVegPlusRect = configParamPlusRect 18
configVbcAlbedoVegBarRect :: Layout -> Rect
configVbcAlbedoVegBarRect = configParamBarRect 18

configVbcOceanAlbedoMinusRect :: Layout -> Rect
configVbcOceanAlbedoMinusRect = configParamMinusRect 19
configVbcOceanAlbedoPlusRect :: Layout -> Rect
configVbcOceanAlbedoPlusRect = configParamPlusRect 19
configVbcOceanAlbedoBarRect :: Layout -> Rect
configVbcOceanAlbedoBarRect = configParamBarRect 19

configVbcIceAlbedoMinusRect :: Layout -> Rect
configVbcIceAlbedoMinusRect = configParamMinusRect 20
configVbcIceAlbedoPlusRect :: Layout -> Rect
configVbcIceAlbedoPlusRect = configParamPlusRect 20
configVbcIceAlbedoBarRect :: Layout -> Rect
configVbcIceAlbedoBarRect = configParamBarRect 20

configBiomeSmoothingMinusRect :: Layout -> Rect
configBiomeSmoothingMinusRect = configParamMinusRect 21
configBiomeSmoothingPlusRect :: Layout -> Rect
configBiomeSmoothingPlusRect = configParamPlusRect 21
configBiomeSmoothingBarRect :: Layout -> Rect
configBiomeSmoothingBarRect = configParamBarRect 21

configVolcanicAshBoostMinusRect :: Layout -> Rect
configVolcanicAshBoostMinusRect = configParamMinusRect 22
configVolcanicAshBoostPlusRect :: Layout -> Rect
configVolcanicAshBoostPlusRect = configParamPlusRect 22
configVolcanicAshBoostBarRect :: Layout -> Rect
configVolcanicAshBoostBarRect = configParamBarRect 22

configVolcanicLavaPenaltyMinusRect :: Layout -> Rect
configVolcanicLavaPenaltyMinusRect = configParamMinusRect 23
configVolcanicLavaPenaltyPlusRect :: Layout -> Rect
configVolcanicLavaPenaltyPlusRect = configParamPlusRect 23
configVolcanicLavaPenaltyBarRect :: Layout -> Rect
configVolcanicLavaPenaltyBarRect = configParamBarRect 23

configBiomeFeedbackBlendMinusRect :: Layout -> Rect
configBiomeFeedbackBlendMinusRect = configParamMinusRect 25
configBiomeFeedbackBlendPlusRect :: Layout -> Rect
configBiomeFeedbackBlendPlusRect = configParamPlusRect 25
configBiomeFeedbackBlendBarRect :: Layout -> Rect
configBiomeFeedbackBlendBarRect = configParamBarRect 25

configBoundaryMotionTempMinusRect :: Layout -> Rect
configBoundaryMotionTempMinusRect = configParamMinusRect 9

configBoundaryMotionTempPlusRect :: Layout -> Rect
configBoundaryMotionTempPlusRect = configParamPlusRect 9

configBoundaryMotionTempBarRect :: Layout -> Rect
configBoundaryMotionTempBarRect = configParamBarRect 9

configBoundaryMotionPrecipMinusRect :: Layout -> Rect
configBoundaryMotionPrecipMinusRect = configParamMinusRect 10

configBoundaryMotionPrecipPlusRect :: Layout -> Rect
configBoundaryMotionPrecipPlusRect = configParamPlusRect 10

configBoundaryMotionPrecipBarRect :: Layout -> Rect
configBoundaryMotionPrecipBarRect = configParamBarRect 10

-- Planet / Slice sliders (planet tab rows 0–2, climate tab rows 11–12)

configPlanetRadiusMinusRect :: Layout -> Rect
configPlanetRadiusMinusRect = configParamMinusRect 0

configPlanetRadiusPlusRect :: Layout -> Rect
configPlanetRadiusPlusRect = configParamPlusRect 0

configPlanetRadiusBarRect :: Layout -> Rect
configPlanetRadiusBarRect = configParamBarRect 0

configAxialTiltMinusRect :: Layout -> Rect
configAxialTiltMinusRect = configParamMinusRect 1

configAxialTiltPlusRect :: Layout -> Rect
configAxialTiltPlusRect = configParamPlusRect 1

configAxialTiltBarRect :: Layout -> Rect
configAxialTiltBarRect = configParamBarRect 1

configInsolationMinusRect :: Layout -> Rect
configInsolationMinusRect = configParamMinusRect 1

configInsolationPlusRect :: Layout -> Rect
configInsolationPlusRect = configParamPlusRect 1

configInsolationBarRect :: Layout -> Rect
configInsolationBarRect = configParamBarRect 1

configOccWarmScaleMinusRect :: Layout -> Rect
configOccWarmScaleMinusRect = configParamMinusRect 2

configOccWarmScalePlusRect :: Layout -> Rect
configOccWarmScalePlusRect = configParamPlusRect 2

configOccWarmScaleBarRect :: Layout -> Rect
configOccWarmScaleBarRect = configParamBarRect 2

configOccColdScaleMinusRect :: Layout -> Rect
configOccColdScaleMinusRect = configParamMinusRect 3

configOccColdScalePlusRect :: Layout -> Rect
configOccColdScalePlusRect = configParamPlusRect 3

configOccColdScaleBarRect :: Layout -> Rect
configOccColdScaleBarRect = configParamBarRect 3

configOccLatPeakDegMinusRect :: Layout -> Rect
configOccLatPeakDegMinusRect = configParamMinusRect 4

configOccLatPeakDegPlusRect :: Layout -> Rect
configOccLatPeakDegPlusRect = configParamPlusRect 4

configOccLatPeakDegBarRect :: Layout -> Rect
configOccLatPeakDegBarRect = configParamBarRect 4

configOccLatWidthDegMinusRect :: Layout -> Rect
configOccLatWidthDegMinusRect = configParamMinusRect 5

configOccLatWidthDegPlusRect :: Layout -> Rect
configOccLatWidthDegPlusRect = configParamPlusRect 5

configOccLatWidthDegBarRect :: Layout -> Rect
configOccLatWidthDegBarRect = configParamBarRect 5

configSliceLatCenterMinusRect :: Layout -> Rect
configSliceLatCenterMinusRect = configParamMinusRect 11

configSliceLatCenterPlusRect :: Layout -> Rect
configSliceLatCenterPlusRect = configParamPlusRect 11

configSliceLatCenterBarRect :: Layout -> Rect
configSliceLatCenterBarRect = configParamBarRect 11

configSliceLonCenterMinusRect :: Layout -> Rect
configSliceLonCenterMinusRect = configParamMinusRect 12

configSliceLonCenterPlusRect :: Layout -> Rect
configSliceLonCenterPlusRect = configParamPlusRect 12

configSliceLonCenterBarRect :: Layout -> Rect
configSliceLonCenterBarRect = configParamBarRect 12

configLatitudeExponentMinusRect :: Layout -> Rect
configLatitudeExponentMinusRect = configParamMinusRect 13

configLatitudeExponentPlusRect :: Layout -> Rect
configLatitudeExponentPlusRect = configParamPlusRect 13

configLatitudeExponentBarRect :: Layout -> Rect
configLatitudeExponentBarRect = configParamBarRect 13

configPlateHeightCoolingMinusRect :: Layout -> Rect
configPlateHeightCoolingMinusRect = configParamMinusRect 14

configPlateHeightCoolingPlusRect :: Layout -> Rect
configPlateHeightCoolingPlusRect = configParamPlusRect 14

configPlateHeightCoolingBarRect :: Layout -> Rect
configPlateHeightCoolingBarRect = configParamBarRect 14

configTempNoiseScaleMinusRect :: Layout -> Rect
configTempNoiseScaleMinusRect = configParamMinusRect 15

configTempNoiseScalePlusRect :: Layout -> Rect
configTempNoiseScalePlusRect = configParamPlusRect 15

configTempNoiseScaleBarRect :: Layout -> Rect
configTempNoiseScaleBarRect = configParamBarRect 15

configOceanModerationMinusRect :: Layout -> Rect
configOceanModerationMinusRect = configParamMinusRect 16

configOceanModerationPlusRect :: Layout -> Rect
configOceanModerationPlusRect = configParamPlusRect 16

configOceanModerationBarRect :: Layout -> Rect
configOceanModerationBarRect = configParamBarRect 16

configOceanModerateTempMinusRect :: Layout -> Rect
configOceanModerateTempMinusRect = configParamMinusRect 17

configOceanModerateTempPlusRect :: Layout -> Rect
configOceanModerateTempPlusRect = configParamPlusRect 17

configOceanModerateTempBarRect :: Layout -> Rect
configOceanModerateTempBarRect = configParamBarRect 17

configAlbedoSensitivityMinusRect :: Layout -> Rect
configAlbedoSensitivityMinusRect = configParamMinusRect 18

configAlbedoSensitivityPlusRect :: Layout -> Rect
configAlbedoSensitivityPlusRect = configParamPlusRect 18

configAlbedoSensitivityBarRect :: Layout -> Rect
configAlbedoSensitivityBarRect = configParamBarRect 18

configAlbedoReferenceMinusRect :: Layout -> Rect
configAlbedoReferenceMinusRect = configParamMinusRect 19

configAlbedoReferencePlusRect :: Layout -> Rect
configAlbedoReferencePlusRect = configParamPlusRect 19

configAlbedoReferenceBarRect :: Layout -> Rect
configAlbedoReferenceBarRect = configParamBarRect 19

configMoistAdvectMinusRect :: Layout -> Rect
configMoistAdvectMinusRect = configParamMinusRect 20

configMoistAdvectPlusRect :: Layout -> Rect
configMoistAdvectPlusRect = configParamPlusRect 20

configMoistAdvectBarRect :: Layout -> Rect
configMoistAdvectBarRect = configParamBarRect 20

configMoistLocalMinusRect :: Layout -> Rect
configMoistLocalMinusRect = configParamMinusRect 21

configMoistLocalPlusRect :: Layout -> Rect
configMoistLocalPlusRect = configParamPlusRect 21

configMoistLocalBarRect :: Layout -> Rect
configMoistLocalBarRect = configParamBarRect 21

configMoistWindEvapScaleMinusRect :: Layout -> Rect
configMoistWindEvapScaleMinusRect = configParamMinusRect 22

configMoistWindEvapScalePlusRect :: Layout -> Rect
configMoistWindEvapScalePlusRect = configParamPlusRect 22

configMoistWindEvapScaleBarRect :: Layout -> Rect
configMoistWindEvapScaleBarRect = configParamBarRect 22

configMoistEvapNoiseScaleMinusRect :: Layout -> Rect
configMoistEvapNoiseScaleMinusRect = configParamMinusRect 23

configMoistEvapNoiseScalePlusRect :: Layout -> Rect
configMoistEvapNoiseScalePlusRect = configParamPlusRect 23

configMoistEvapNoiseScaleBarRect :: Layout -> Rect
configMoistEvapNoiseScaleBarRect = configParamBarRect 23

configMoistBareEvapFracMinusRect :: Layout -> Rect
configMoistBareEvapFracMinusRect = configParamMinusRect 24

configMoistBareEvapFracPlusRect :: Layout -> Rect
configMoistBareEvapFracPlusRect = configParamPlusRect 24

configMoistBareEvapFracBarRect :: Layout -> Rect
configMoistBareEvapFracBarRect = configParamBarRect 24

configMoistVegTranspFracMinusRect :: Layout -> Rect
configMoistVegTranspFracMinusRect = configParamMinusRect 25

configMoistVegTranspFracPlusRect :: Layout -> Rect
configMoistVegTranspFracPlusRect = configParamPlusRect 25

configMoistVegTranspFracBarRect :: Layout -> Rect
configMoistVegTranspFracBarRect = configParamBarRect 25

configMoistWindETScaleMinusRect :: Layout -> Rect
configMoistWindETScaleMinusRect = configParamMinusRect 26

configMoistWindETScalePlusRect :: Layout -> Rect
configMoistWindETScalePlusRect = configParamPlusRect 26

configMoistWindETScaleBarRect :: Layout -> Rect
configMoistWindETScaleBarRect = configParamBarRect 26

configMoistCondensationRateMinusRect :: Layout -> Rect
configMoistCondensationRateMinusRect = configParamMinusRect 27

configMoistCondensationRatePlusRect :: Layout -> Rect
configMoistCondensationRatePlusRect = configParamPlusRect 27

configMoistCondensationRateBarRect :: Layout -> Rect
configMoistCondensationRateBarRect = configParamBarRect 27

configMoistRecycleRateMinusRect :: Layout -> Rect
configMoistRecycleRateMinusRect = configParamMinusRect 28

configMoistRecycleRatePlusRect :: Layout -> Rect
configMoistRecycleRatePlusRect = configParamPlusRect 28

configMoistRecycleRateBarRect :: Layout -> Rect
configMoistRecycleRateBarRect = configParamBarRect 28

configMoistITCZStrengthMinusRect :: Layout -> Rect
configMoistITCZStrengthMinusRect = configParamMinusRect 29

configMoistITCZStrengthPlusRect :: Layout -> Rect
configMoistITCZStrengthPlusRect = configParamPlusRect 29

configMoistITCZStrengthBarRect :: Layout -> Rect
configMoistITCZStrengthBarRect = configParamBarRect 29

configMoistITCZWidthMinusRect :: Layout -> Rect
configMoistITCZWidthMinusRect = configParamMinusRect 30

configMoistITCZWidthPlusRect :: Layout -> Rect
configMoistITCZWidthPlusRect = configParamPlusRect 30

configMoistITCZWidthBarRect :: Layout -> Rect
configMoistITCZWidthBarRect = configParamBarRect 30

configOrographicScaleMinusRect :: Layout -> Rect
configOrographicScaleMinusRect = configParamMinusRect 31

configOrographicScalePlusRect :: Layout -> Rect
configOrographicScalePlusRect = configParamPlusRect 31

configOrographicScaleBarRect :: Layout -> Rect
configOrographicScaleBarRect = configParamBarRect 31

configOrographicStepMinusRect :: Layout -> Rect
configOrographicStepMinusRect = configParamMinusRect 32

configOrographicStepPlusRect :: Layout -> Rect
configOrographicStepPlusRect = configParamPlusRect 32

configOrographicStepBarRect :: Layout -> Rect
configOrographicStepBarRect = configParamBarRect 32

configCoastalIterationsMinusRect :: Layout -> Rect
configCoastalIterationsMinusRect = configParamMinusRect 33

configCoastalIterationsPlusRect :: Layout -> Rect
configCoastalIterationsPlusRect = configParamPlusRect 33

configCoastalIterationsBarRect :: Layout -> Rect
configCoastalIterationsBarRect = configParamBarRect 33

configCoastalDiffuseMinusRect :: Layout -> Rect
configCoastalDiffuseMinusRect = configParamMinusRect 34

configCoastalDiffusePlusRect :: Layout -> Rect
configCoastalDiffusePlusRect = configParamPlusRect 34

configCoastalDiffuseBarRect :: Layout -> Rect
configCoastalDiffuseBarRect = configParamBarRect 34

configCoastalMoistureBoostMinusRect :: Layout -> Rect
configCoastalMoistureBoostMinusRect = configParamMinusRect 35

configCoastalMoistureBoostPlusRect :: Layout -> Rect
configCoastalMoistureBoostPlusRect = configParamPlusRect 35

configCoastalMoistureBoostBarRect :: Layout -> Rect
configCoastalMoistureBoostBarRect = configParamBarRect 35

configWindBeltStrengthMinusRect :: Layout -> Rect
configWindBeltStrengthMinusRect = configParamMinusRect 36

configWindBeltStrengthPlusRect :: Layout -> Rect
configWindBeltStrengthPlusRect = configParamPlusRect 36

configWindBeltStrengthBarRect :: Layout -> Rect
configWindBeltStrengthBarRect = configParamBarRect 36

configWindBeltHarmonicsMinusRect :: Layout -> Rect
configWindBeltHarmonicsMinusRect = configParamMinusRect 37

configWindBeltHarmonicsPlusRect :: Layout -> Rect
configWindBeltHarmonicsPlusRect = configParamPlusRect 37

configWindBeltHarmonicsBarRect :: Layout -> Rect
configWindBeltHarmonicsBarRect = configParamBarRect 37

configWindBeltBaseMinusRect :: Layout -> Rect
configWindBeltBaseMinusRect = configParamMinusRect 38

configWindBeltBasePlusRect :: Layout -> Rect
configWindBeltBasePlusRect = configParamPlusRect 38

configWindBeltBaseBarRect :: Layout -> Rect
configWindBeltBaseBarRect = configParamBarRect 38

configWindBeltRangeMinusRect :: Layout -> Rect
configWindBeltRangeMinusRect = configParamMinusRect 39

configWindBeltRangePlusRect :: Layout -> Rect
configWindBeltRangePlusRect = configParamPlusRect 39

configWindBeltRangeBarRect :: Layout -> Rect
configWindBeltRangeBarRect = configParamBarRect 39

configWindBeltSpeedScaleMinusRect :: Layout -> Rect
configWindBeltSpeedScaleMinusRect = configParamMinusRect 40

configWindBeltSpeedScalePlusRect :: Layout -> Rect
configWindBeltSpeedScalePlusRect = configParamPlusRect 40

configWindBeltSpeedScaleBarRect :: Layout -> Rect
configWindBeltSpeedScaleBarRect = configParamBarRect 40

configBndLandRangeMinusRect :: Layout -> Rect
configBndLandRangeMinusRect = configParamMinusRect 41

configBndLandRangePlusRect :: Layout -> Rect
configBndLandRangePlusRect = configParamPlusRect 41

configBndLandRangeBarRect :: Layout -> Rect
configBndLandRangeBarRect = configParamBarRect 41

configBndTempConvergentMinusRect :: Layout -> Rect
configBndTempConvergentMinusRect = configParamMinusRect 42

configBndTempConvergentPlusRect :: Layout -> Rect
configBndTempConvergentPlusRect = configParamPlusRect 42

configBndTempConvergentBarRect :: Layout -> Rect
configBndTempConvergentBarRect = configParamBarRect 42

configBndTempDivergentMinusRect :: Layout -> Rect
configBndTempDivergentMinusRect = configParamMinusRect 43

configBndTempDivergentPlusRect :: Layout -> Rect
configBndTempDivergentPlusRect = configParamPlusRect 43

configBndTempDivergentBarRect :: Layout -> Rect
configBndTempDivergentBarRect = configParamBarRect 43

configBndTempTransformMinusRect :: Layout -> Rect
configBndTempTransformMinusRect = configParamMinusRect 44

configBndTempTransformPlusRect :: Layout -> Rect
configBndTempTransformPlusRect = configParamPlusRect 44

configBndTempTransformBarRect :: Layout -> Rect
configBndTempTransformBarRect = configParamBarRect 44

configBndPrecipConvergentMinusRect :: Layout -> Rect
configBndPrecipConvergentMinusRect = configParamMinusRect 45

configBndPrecipConvergentPlusRect :: Layout -> Rect
configBndPrecipConvergentPlusRect = configParamPlusRect 45

configBndPrecipConvergentBarRect :: Layout -> Rect
configBndPrecipConvergentBarRect = configParamBarRect 45

configBndPrecipDivergentMinusRect :: Layout -> Rect
configBndPrecipDivergentMinusRect = configParamMinusRect 46

configBndPrecipDivergentPlusRect :: Layout -> Rect
configBndPrecipDivergentPlusRect = configParamPlusRect 46

configBndPrecipDivergentBarRect :: Layout -> Rect
configBndPrecipDivergentBarRect = configParamBarRect 46

configBndPrecipTransformMinusRect :: Layout -> Rect
configBndPrecipTransformMinusRect = configParamMinusRect 47

configBndPrecipTransformPlusRect :: Layout -> Rect
configBndPrecipTransformPlusRect = configParamPlusRect 47

configBndPrecipTransformBarRect :: Layout -> Rect
configBndPrecipTransformBarRect = configParamBarRect 47

configErosionHydraulicMinusRect :: Layout -> Rect
configErosionHydraulicMinusRect = configParamMinusRect 0

configErosionHydraulicPlusRect :: Layout -> Rect
configErosionHydraulicPlusRect = configParamPlusRect 0

configErosionHydraulicBarRect :: Layout -> Rect
configErosionHydraulicBarRect = configParamBarRect 0

configErosionThermalMinusRect :: Layout -> Rect
configErosionThermalMinusRect = configParamMinusRect 1

configErosionThermalPlusRect :: Layout -> Rect
configErosionThermalPlusRect = configParamPlusRect 1

configErosionThermalBarRect :: Layout -> Rect
configErosionThermalBarRect = configParamBarRect 1

configErosionRainRateMinusRect :: Layout -> Rect
configErosionRainRateMinusRect = configParamMinusRect 1

configErosionRainRatePlusRect :: Layout -> Rect
configErosionRainRatePlusRect = configParamPlusRect 1

configErosionRainRateBarRect :: Layout -> Rect
configErosionRainRateBarRect = configParamBarRect 1

configErosionTalusMinusRect :: Layout -> Rect
configErosionTalusMinusRect = configParamMinusRect 2

configErosionTalusPlusRect :: Layout -> Rect
configErosionTalusPlusRect = configParamPlusRect 2

configErosionTalusBarRect :: Layout -> Rect
configErosionTalusBarRect = configParamBarRect 2

configErosionMaxDropMinusRect :: Layout -> Rect
configErosionMaxDropMinusRect = configParamMinusRect 3

configErosionMaxDropPlusRect :: Layout -> Rect
configErosionMaxDropPlusRect = configParamPlusRect 3

configErosionMaxDropBarRect :: Layout -> Rect
configErosionMaxDropBarRect = configParamBarRect 3

configGlacierSnowTempMinusRect :: Layout -> Rect
configGlacierSnowTempMinusRect = configParamMinusRect 4
configGlacierSnowTempPlusRect :: Layout -> Rect
configGlacierSnowTempPlusRect = configParamPlusRect 4
configGlacierSnowTempBarRect :: Layout -> Rect
configGlacierSnowTempBarRect = configParamBarRect 4

configGlacierSnowRangeMinusRect :: Layout -> Rect
configGlacierSnowRangeMinusRect = configParamMinusRect 5
configGlacierSnowRangePlusRect :: Layout -> Rect
configGlacierSnowRangePlusRect = configParamPlusRect 5
configGlacierSnowRangeBarRect :: Layout -> Rect
configGlacierSnowRangeBarRect = configParamBarRect 5

configGlacierMeltTempMinusRect :: Layout -> Rect
configGlacierMeltTempMinusRect = configParamMinusRect 6
configGlacierMeltTempPlusRect :: Layout -> Rect
configGlacierMeltTempPlusRect = configParamPlusRect 6
configGlacierMeltTempBarRect :: Layout -> Rect
configGlacierMeltTempBarRect = configParamBarRect 6

configGlacierMeltRateMinusRect :: Layout -> Rect
configGlacierMeltRateMinusRect = configParamMinusRect 7
configGlacierMeltRatePlusRect :: Layout -> Rect
configGlacierMeltRatePlusRect = configParamPlusRect 7
configGlacierMeltRateBarRect :: Layout -> Rect
configGlacierMeltRateBarRect = configParamBarRect 7

configGlacierAccumScaleMinusRect :: Layout -> Rect
configGlacierAccumScaleMinusRect = configParamMinusRect 8
configGlacierAccumScalePlusRect :: Layout -> Rect
configGlacierAccumScalePlusRect = configParamPlusRect 8
configGlacierAccumScaleBarRect :: Layout -> Rect
configGlacierAccumScaleBarRect = configParamBarRect 8

configGlacierFlowItersMinusRect :: Layout -> Rect
configGlacierFlowItersMinusRect = configParamMinusRect 9
configGlacierFlowItersPlusRect :: Layout -> Rect
configGlacierFlowItersPlusRect = configParamPlusRect 9
configGlacierFlowItersBarRect :: Layout -> Rect
configGlacierFlowItersBarRect = configParamBarRect 9

configGlacierFlowRateMinusRect :: Layout -> Rect
configGlacierFlowRateMinusRect = configParamMinusRect 10
configGlacierFlowRatePlusRect :: Layout -> Rect
configGlacierFlowRatePlusRect = configParamPlusRect 10
configGlacierFlowRateBarRect :: Layout -> Rect
configGlacierFlowRateBarRect = configParamBarRect 10

configGlacierErosionScaleMinusRect :: Layout -> Rect
configGlacierErosionScaleMinusRect = configParamMinusRect 11
configGlacierErosionScalePlusRect :: Layout -> Rect
configGlacierErosionScalePlusRect = configParamPlusRect 11
configGlacierErosionScaleBarRect :: Layout -> Rect
configGlacierErosionScaleBarRect = configParamBarRect 11

configGlacierCarveScaleMinusRect :: Layout -> Rect
configGlacierCarveScaleMinusRect = configParamMinusRect 12
configGlacierCarveScalePlusRect :: Layout -> Rect
configGlacierCarveScalePlusRect = configParamPlusRect 12
configGlacierCarveScaleBarRect :: Layout -> Rect
configGlacierCarveScaleBarRect = configParamBarRect 12

configGlacierDepositScaleMinusRect :: Layout -> Rect
configGlacierDepositScaleMinusRect = configParamMinusRect 13
configGlacierDepositScalePlusRect :: Layout -> Rect
configGlacierDepositScalePlusRect = configParamPlusRect 13
configGlacierDepositScaleBarRect :: Layout -> Rect
configGlacierDepositScaleBarRect = configParamBarRect 13

configVentDensityMinusRect :: Layout -> Rect
configVentDensityMinusRect = configParamMinusRect 14
configVentDensityPlusRect :: Layout -> Rect
configVentDensityPlusRect = configParamPlusRect 14
configVentDensityBarRect :: Layout -> Rect
configVentDensityBarRect = configParamBarRect 14

configVentThresholdMinusRect :: Layout -> Rect
configVentThresholdMinusRect = configParamMinusRect 15
configVentThresholdPlusRect :: Layout -> Rect
configVentThresholdPlusRect = configParamPlusRect 15
configVentThresholdBarRect :: Layout -> Rect
configVentThresholdBarRect = configParamBarRect 15

configHotspotScaleMinusRect :: Layout -> Rect
configHotspotScaleMinusRect = configParamMinusRect 16
configHotspotScalePlusRect :: Layout -> Rect
configHotspotScalePlusRect = configParamPlusRect 16
configHotspotScaleBarRect :: Layout -> Rect
configHotspotScaleBarRect = configParamBarRect 16

configHotspotThresholdMinusRect :: Layout -> Rect
configHotspotThresholdMinusRect = configParamMinusRect 17
configHotspotThresholdPlusRect :: Layout -> Rect
configHotspotThresholdPlusRect = configParamPlusRect 17
configHotspotThresholdBarRect :: Layout -> Rect
configHotspotThresholdBarRect = configParamBarRect 17

configMagmaRechargeMinusRect :: Layout -> Rect
configMagmaRechargeMinusRect = configParamMinusRect 18
configMagmaRechargePlusRect :: Layout -> Rect
configMagmaRechargePlusRect = configParamPlusRect 18
configMagmaRechargeBarRect :: Layout -> Rect
configMagmaRechargeBarRect = configParamBarRect 18

configLavaScaleMinusRect :: Layout -> Rect
configLavaScaleMinusRect = configParamMinusRect 19
configLavaScalePlusRect :: Layout -> Rect
configLavaScalePlusRect = configParamPlusRect 19
configLavaScaleBarRect :: Layout -> Rect
configLavaScaleBarRect = configParamBarRect 19

configAshScaleMinusRect :: Layout -> Rect
configAshScaleMinusRect = configParamMinusRect 20
configAshScalePlusRect :: Layout -> Rect
configAshScalePlusRect = configParamPlusRect 20
configAshScaleBarRect :: Layout -> Rect
configAshScaleBarRect = configParamBarRect 20

configVolcanicDepositScaleMinusRect :: Layout -> Rect
configVolcanicDepositScaleMinusRect = configParamMinusRect 21
configVolcanicDepositScalePlusRect :: Layout -> Rect
configVolcanicDepositScalePlusRect = configParamPlusRect 21
configVolcanicDepositScaleBarRect :: Layout -> Rect
configVolcanicDepositScaleBarRect = configParamBarRect 21

configSoilMoistureThresholdMinusRect :: Layout -> Rect
configSoilMoistureThresholdMinusRect = configParamMinusRect 22
configSoilMoistureThresholdPlusRect :: Layout -> Rect
configSoilMoistureThresholdPlusRect = configParamPlusRect 22
configSoilMoistureThresholdBarRect :: Layout -> Rect
configSoilMoistureThresholdBarRect = configParamBarRect 22

configSoilHardnessThresholdMinusRect :: Layout -> Rect
configSoilHardnessThresholdMinusRect = configParamMinusRect 23
configSoilHardnessThresholdPlusRect :: Layout -> Rect
configSoilHardnessThresholdPlusRect = configParamPlusRect 23
configSoilHardnessThresholdBarRect :: Layout -> Rect
configSoilHardnessThresholdBarRect = configParamBarRect 23

configSoilFertilityMoistWeightMinusRect :: Layout -> Rect
configSoilFertilityMoistWeightMinusRect = configParamMinusRect 25
configSoilFertilityMoistWeightPlusRect :: Layout -> Rect
configSoilFertilityMoistWeightPlusRect = configParamPlusRect 25
configSoilFertilityMoistWeightBarRect :: Layout -> Rect
configSoilFertilityMoistWeightBarRect = configParamBarRect 25

configSoilFertilityDepthWeightMinusRect :: Layout -> Rect
configSoilFertilityDepthWeightMinusRect = configParamMinusRect 24
configSoilFertilityDepthWeightPlusRect :: Layout -> Rect
configSoilFertilityDepthWeightPlusRect = configParamPlusRect 24
configSoilFertilityDepthWeightBarRect :: Layout -> Rect
configSoilFertilityDepthWeightBarRect = configParamBarRect 24

configSinkBreachDepthMinusRect :: Layout -> Rect
configSinkBreachDepthMinusRect = configParamMinusRect 25
configSinkBreachDepthPlusRect :: Layout -> Rect
configSinkBreachDepthPlusRect = configParamPlusRect 25
configSinkBreachDepthBarRect :: Layout -> Rect
configSinkBreachDepthBarRect = configParamBarRect 25

configStreamPowerMaxErosionMinusRect :: Layout -> Rect
configStreamPowerMaxErosionMinusRect = configParamMinusRect 26
configStreamPowerMaxErosionPlusRect :: Layout -> Rect
configStreamPowerMaxErosionPlusRect = configParamPlusRect 26
configStreamPowerMaxErosionBarRect :: Layout -> Rect
configStreamPowerMaxErosionBarRect = configParamBarRect 26

configRiverCarveMaxDepthMinusRect :: Layout -> Rect
configRiverCarveMaxDepthMinusRect = configParamMinusRect 27
configRiverCarveMaxDepthPlusRect :: Layout -> Rect
configRiverCarveMaxDepthPlusRect = configParamPlusRect 27
configRiverCarveMaxDepthBarRect :: Layout -> Rect
configRiverCarveMaxDepthBarRect = configParamBarRect 27

configCoastalErodeStrengthMinusRect :: Layout -> Rect
configCoastalErodeStrengthMinusRect = configParamMinusRect 28
configCoastalErodeStrengthPlusRect :: Layout -> Rect
configCoastalErodeStrengthPlusRect = configParamPlusRect 28
configCoastalErodeStrengthBarRect :: Layout -> Rect
configCoastalErodeStrengthBarRect = configParamBarRect 28

configHydroHardnessWeightMinusRect :: Layout -> Rect
configHydroHardnessWeightMinusRect = configParamMinusRect 29
configHydroHardnessWeightPlusRect :: Layout -> Rect
configHydroHardnessWeightPlusRect = configParamPlusRect 29
configHydroHardnessWeightBarRect :: Layout -> Rect
configHydroHardnessWeightBarRect = configParamBarRect 29

configMinLakeSizeMinusRect :: Layout -> Rect
configMinLakeSizeMinusRect = configParamMinusRect 30
configMinLakeSizePlusRect :: Layout -> Rect
configMinLakeSizePlusRect = configParamPlusRect 30
configMinLakeSizeBarRect :: Layout -> Rect
configMinLakeSizeBarRect = configParamBarRect 30

configInlandSeaMinSizeMinusRect :: Layout -> Rect
configInlandSeaMinSizeMinusRect = configParamMinusRect 31
configInlandSeaMinSizePlusRect :: Layout -> Rect
configInlandSeaMinSizePlusRect = configParamPlusRect 31
configInlandSeaMinSizeBarRect :: Layout -> Rect
configInlandSeaMinSizeBarRect = configParamBarRect 31

configRoughnessScaleMinusRect :: Layout -> Rect
configRoughnessScaleMinusRect = configParamMinusRect 32
configRoughnessScalePlusRect :: Layout -> Rect
configRoughnessScalePlusRect = configParamPlusRect 32
configRoughnessScaleBarRect :: Layout -> Rect
configRoughnessScaleBarRect = configParamBarRect 32

configGenScaleMinusRect :: Layout -> Rect
configGenScaleMinusRect = configParamMinusRect 0

configGenScalePlusRect :: Layout -> Rect
configGenScalePlusRect = configParamPlusRect 0

configGenScaleBarRect :: Layout -> Rect
configGenScaleBarRect = configParamBarRect 0

configGenCoordScaleMinusRect :: Layout -> Rect
configGenCoordScaleMinusRect = configParamMinusRect 41

configGenCoordScalePlusRect :: Layout -> Rect
configGenCoordScalePlusRect = configParamPlusRect 41

configGenCoordScaleBarRect :: Layout -> Rect
configGenCoordScaleBarRect = configParamBarRect 41

configGenOffsetXMinusRect :: Layout -> Rect
configGenOffsetXMinusRect = configParamMinusRect 42

configGenOffsetXPlusRect :: Layout -> Rect
configGenOffsetXPlusRect = configParamPlusRect 42

configGenOffsetXBarRect :: Layout -> Rect
configGenOffsetXBarRect = configParamBarRect 42

configGenOffsetYMinusRect :: Layout -> Rect
configGenOffsetYMinusRect = configParamMinusRect 43

configGenOffsetYPlusRect :: Layout -> Rect
configGenOffsetYPlusRect = configParamPlusRect 43

configGenOffsetYBarRect :: Layout -> Rect
configGenOffsetYBarRect = configParamBarRect 43

configGenFrequencyMinusRect :: Layout -> Rect
configGenFrequencyMinusRect = configParamMinusRect 1

configGenFrequencyPlusRect :: Layout -> Rect
configGenFrequencyPlusRect = configParamPlusRect 1

configGenFrequencyBarRect :: Layout -> Rect
configGenFrequencyBarRect = configParamBarRect 1

configGenOctavesMinusRect :: Layout -> Rect
configGenOctavesMinusRect = configParamMinusRect 1

configGenOctavesPlusRect :: Layout -> Rect
configGenOctavesPlusRect = configParamPlusRect 1

configGenOctavesBarRect :: Layout -> Rect
configGenOctavesBarRect = configParamBarRect 1

configGenLacunarityMinusRect :: Layout -> Rect
configGenLacunarityMinusRect = configParamMinusRect 2

configGenLacunarityPlusRect :: Layout -> Rect
configGenLacunarityPlusRect = configParamPlusRect 2

configGenLacunarityBarRect :: Layout -> Rect
configGenLacunarityBarRect = configParamBarRect 2

configGenGainMinusRect :: Layout -> Rect
configGenGainMinusRect = configParamMinusRect 3

configGenGainPlusRect :: Layout -> Rect
configGenGainPlusRect = configParamPlusRect 3

configGenGainBarRect :: Layout -> Rect
configGenGainBarRect = configParamBarRect 3

configGenWarpScaleMinusRect :: Layout -> Rect
configGenWarpScaleMinusRect = configParamMinusRect 4

configGenWarpScalePlusRect :: Layout -> Rect
configGenWarpScalePlusRect = configParamPlusRect 4

configGenWarpScaleBarRect :: Layout -> Rect
configGenWarpScaleBarRect = configParamBarRect 4

configGenWarpStrengthMinusRect :: Layout -> Rect
configGenWarpStrengthMinusRect = configParamMinusRect 5

configGenWarpStrengthPlusRect :: Layout -> Rect
configGenWarpStrengthPlusRect = configParamPlusRect 5

configGenWarpStrengthBarRect :: Layout -> Rect
configGenWarpStrengthBarRect = configParamBarRect 5

configExtentXMinusRect :: Layout -> Rect
configExtentXMinusRect = configParamMinusRect 44

configExtentXPlusRect :: Layout -> Rect
configExtentXPlusRect = configParamPlusRect 44

configExtentXBarRect :: Layout -> Rect
configExtentXBarRect = configParamBarRect 44

configExtentYMinusRect :: Layout -> Rect
configExtentYMinusRect = configParamMinusRect 45

configExtentYPlusRect :: Layout -> Rect
configExtentYPlusRect = configParamPlusRect 45

configExtentYBarRect :: Layout -> Rect
configExtentYBarRect = configParamBarRect 45

configEdgeNorthMinusRect :: Layout -> Rect
configEdgeNorthMinusRect = configParamMinusRect 46

configEdgeNorthPlusRect :: Layout -> Rect
configEdgeNorthPlusRect = configParamPlusRect 46

configEdgeNorthBarRect :: Layout -> Rect
configEdgeNorthBarRect = configParamBarRect 46

configEdgeSouthMinusRect :: Layout -> Rect
configEdgeSouthMinusRect = configParamMinusRect 47

configEdgeSouthPlusRect :: Layout -> Rect
configEdgeSouthPlusRect = configParamPlusRect 47

configEdgeSouthBarRect :: Layout -> Rect
configEdgeSouthBarRect = configParamBarRect 47

configEdgeEastMinusRect :: Layout -> Rect
configEdgeEastMinusRect = configParamMinusRect 50

configEdgeEastPlusRect :: Layout -> Rect
configEdgeEastPlusRect = configParamPlusRect 50

configEdgeEastBarRect :: Layout -> Rect
configEdgeEastBarRect = configParamBarRect 50

configEdgeWestMinusRect :: Layout -> Rect
configEdgeWestMinusRect = configParamMinusRect 51

configEdgeWestPlusRect :: Layout -> Rect
configEdgeWestPlusRect = configParamPlusRect 51

configEdgeWestBarRect :: Layout -> Rect
configEdgeWestBarRect = configParamBarRect 51

configEdgeFalloffMinusRect :: Layout -> Rect
configEdgeFalloffMinusRect = configParamMinusRect 52

configEdgeFalloffPlusRect :: Layout -> Rect
configEdgeFalloffPlusRect = configParamPlusRect 52

configEdgeFalloffBarRect :: Layout -> Rect
configEdgeFalloffBarRect = configParamBarRect 52

configPlateSizeMinusRect :: Layout -> Rect
configPlateSizeMinusRect = configParamMinusRect 6

configPlateSizePlusRect :: Layout -> Rect
configPlateSizePlusRect = configParamPlusRect 6

configPlateSizeBarRect :: Layout -> Rect
configPlateSizeBarRect = configParamBarRect 6

configUpliftMinusRect :: Layout -> Rect
configUpliftMinusRect = configParamMinusRect 7

configUpliftPlusRect :: Layout -> Rect
configUpliftPlusRect = configParamPlusRect 7

configUpliftBarRect :: Layout -> Rect
configUpliftBarRect = configParamBarRect 7

configRiftDepthMinusRect :: Layout -> Rect
configRiftDepthMinusRect = configParamMinusRect 8

configRiftDepthPlusRect :: Layout -> Rect
configRiftDepthPlusRect = configParamPlusRect 8

configRiftDepthBarRect :: Layout -> Rect
configRiftDepthBarRect = configParamBarRect 8

configDetailScaleMinusRect :: Layout -> Rect
configDetailScaleMinusRect = configParamMinusRect 9

configDetailScalePlusRect :: Layout -> Rect
configDetailScalePlusRect = configParamPlusRect 9

configDetailScaleBarRect :: Layout -> Rect
configDetailScaleBarRect = configParamBarRect 9

configPlateSpeedMinusRect :: Layout -> Rect
configPlateSpeedMinusRect = configParamMinusRect 10

configPlateSpeedPlusRect :: Layout -> Rect
configPlateSpeedPlusRect = configParamPlusRect 10

configPlateSpeedBarRect :: Layout -> Rect
configPlateSpeedBarRect = configParamBarRect 10

configBoundarySharpnessMinusRect :: Layout -> Rect
configBoundarySharpnessMinusRect = configParamMinusRect 11

configBoundarySharpnessPlusRect :: Layout -> Rect
configBoundarySharpnessPlusRect = configParamPlusRect 11

configBoundarySharpnessBarRect :: Layout -> Rect
configBoundarySharpnessBarRect = configParamBarRect 11

configBoundaryNoiseScaleMinusRect :: Layout -> Rect
configBoundaryNoiseScaleMinusRect = configParamMinusRect 12

configBoundaryNoiseScalePlusRect :: Layout -> Rect
configBoundaryNoiseScalePlusRect = configParamPlusRect 12

configBoundaryNoiseScaleBarRect :: Layout -> Rect
configBoundaryNoiseScaleBarRect = configParamBarRect 12

configBoundaryNoiseStrengthMinusRect :: Layout -> Rect
configBoundaryNoiseStrengthMinusRect = configParamMinusRect 13

configBoundaryNoiseStrengthPlusRect :: Layout -> Rect
configBoundaryNoiseStrengthPlusRect = configParamPlusRect 13

configBoundaryNoiseStrengthBarRect :: Layout -> Rect
configBoundaryNoiseStrengthBarRect = configParamBarRect 13

configBoundaryWarpOctavesMinusRect :: Layout -> Rect
configBoundaryWarpOctavesMinusRect = configParamMinusRect 14

configBoundaryWarpOctavesPlusRect :: Layout -> Rect
configBoundaryWarpOctavesPlusRect = configParamPlusRect 14

configBoundaryWarpOctavesBarRect :: Layout -> Rect
configBoundaryWarpOctavesBarRect = configParamBarRect 14

configBoundaryWarpLacunarityMinusRect :: Layout -> Rect
configBoundaryWarpLacunarityMinusRect = configParamMinusRect 15

configBoundaryWarpLacunarityPlusRect :: Layout -> Rect
configBoundaryWarpLacunarityPlusRect = configParamPlusRect 15

configBoundaryWarpLacunarityBarRect :: Layout -> Rect
configBoundaryWarpLacunarityBarRect = configParamBarRect 15

configBoundaryWarpGainMinusRect :: Layout -> Rect
configBoundaryWarpGainMinusRect = configParamMinusRect 16

configBoundaryWarpGainPlusRect :: Layout -> Rect
configBoundaryWarpGainPlusRect = configParamPlusRect 16

configBoundaryWarpGainBarRect :: Layout -> Rect
configBoundaryWarpGainBarRect = configParamBarRect 16

configPlateMergeScaleMinusRect :: Layout -> Rect
configPlateMergeScaleMinusRect = configParamMinusRect 17

configPlateMergeScalePlusRect :: Layout -> Rect
configPlateMergeScalePlusRect = configParamPlusRect 17

configPlateMergeScaleBarRect :: Layout -> Rect
configPlateMergeScaleBarRect = configParamBarRect 17

configPlateMergeBiasMinusRect :: Layout -> Rect
configPlateMergeBiasMinusRect = configParamMinusRect 18

configPlateMergeBiasPlusRect :: Layout -> Rect
configPlateMergeBiasPlusRect = configParamPlusRect 18

configPlateMergeBiasBarRect :: Layout -> Rect
configPlateMergeBiasBarRect = configParamBarRect 18

configPlateDetailScaleMinusRect :: Layout -> Rect
configPlateDetailScaleMinusRect = configParamMinusRect 19

configPlateDetailScalePlusRect :: Layout -> Rect
configPlateDetailScalePlusRect = configParamPlusRect 19

configPlateDetailScaleBarRect :: Layout -> Rect
configPlateDetailScaleBarRect = configParamBarRect 19

configPlateDetailStrengthMinusRect :: Layout -> Rect
configPlateDetailStrengthMinusRect = configParamMinusRect 20

configPlateDetailStrengthPlusRect :: Layout -> Rect
configPlateDetailStrengthPlusRect = configParamPlusRect 20

configPlateDetailStrengthBarRect :: Layout -> Rect
configPlateDetailStrengthBarRect = configParamBarRect 20

configPlateRidgeStrengthMinusRect :: Layout -> Rect
configPlateRidgeStrengthMinusRect = configParamMinusRect 21

configPlateRidgeStrengthPlusRect :: Layout -> Rect
configPlateRidgeStrengthPlusRect = configParamPlusRect 21

configPlateRidgeStrengthBarRect :: Layout -> Rect
configPlateRidgeStrengthBarRect = configParamBarRect 21

configPlateHeightBaseMinusRect :: Layout -> Rect
configPlateHeightBaseMinusRect = configParamMinusRect 22

configPlateHeightBasePlusRect :: Layout -> Rect
configPlateHeightBasePlusRect = configParamPlusRect 22

configPlateHeightBaseBarRect :: Layout -> Rect
configPlateHeightBaseBarRect = configParamBarRect 22

configPlateHeightVarianceMinusRect :: Layout -> Rect
configPlateHeightVarianceMinusRect = configParamMinusRect 23

configPlateHeightVariancePlusRect :: Layout -> Rect
configPlateHeightVariancePlusRect = configParamPlusRect 23

configPlateHeightVarianceBarRect :: Layout -> Rect
configPlateHeightVarianceBarRect = configParamBarRect 23

configPlateHardnessBaseMinusRect :: Layout -> Rect
configPlateHardnessBaseMinusRect = configParamMinusRect 25

configPlateHardnessBasePlusRect :: Layout -> Rect
configPlateHardnessBasePlusRect = configParamPlusRect 25

configPlateHardnessBaseBarRect :: Layout -> Rect
configPlateHardnessBaseBarRect = configParamBarRect 25

configPlateHardnessVarianceMinusRect :: Layout -> Rect
configPlateHardnessVarianceMinusRect = configParamMinusRect 24

configPlateHardnessVariancePlusRect :: Layout -> Rect
configPlateHardnessVariancePlusRect = configParamPlusRect 24

configPlateHardnessVarianceBarRect :: Layout -> Rect
configPlateHardnessVarianceBarRect = configParamBarRect 24

configTrenchDepthMinusRect :: Layout -> Rect
configTrenchDepthMinusRect = configParamMinusRect 25

configTrenchDepthPlusRect :: Layout -> Rect
configTrenchDepthPlusRect = configParamPlusRect 25

configTrenchDepthBarRect :: Layout -> Rect
configTrenchDepthBarRect = configParamBarRect 25

configRidgeHeightMinusRect :: Layout -> Rect
configRidgeHeightMinusRect = configParamMinusRect 26

configRidgeHeightPlusRect :: Layout -> Rect
configRidgeHeightPlusRect = configParamPlusRect 26

configRidgeHeightBarRect :: Layout -> Rect
configRidgeHeightBarRect = configParamBarRect 26

configPlateBiasStrengthMinusRect :: Layout -> Rect
configPlateBiasStrengthMinusRect = configParamMinusRect 27

configPlateBiasStrengthPlusRect :: Layout -> Rect
configPlateBiasStrengthPlusRect = configParamPlusRect 27

configPlateBiasStrengthBarRect :: Layout -> Rect
configPlateBiasStrengthBarRect = configParamBarRect 27

configPlateBiasCenterMinusRect :: Layout -> Rect
configPlateBiasCenterMinusRect = configParamMinusRect 28

configPlateBiasCenterPlusRect :: Layout -> Rect
configPlateBiasCenterPlusRect = configParamPlusRect 28

configPlateBiasCenterBarRect :: Layout -> Rect
configPlateBiasCenterBarRect = configParamBarRect 28

configPlateBiasEdgeMinusRect :: Layout -> Rect
configPlateBiasEdgeMinusRect = configParamMinusRect 29

configPlateBiasEdgePlusRect :: Layout -> Rect
configPlateBiasEdgePlusRect = configParamPlusRect 29

configPlateBiasEdgeBarRect :: Layout -> Rect
configPlateBiasEdgeBarRect = configParamBarRect 29

configPlateBiasNorthMinusRect :: Layout -> Rect
configPlateBiasNorthMinusRect = configParamMinusRect 30

configPlateBiasNorthPlusRect :: Layout -> Rect
configPlateBiasNorthPlusRect = configParamPlusRect 30

configPlateBiasNorthBarRect :: Layout -> Rect
configPlateBiasNorthBarRect = configParamBarRect 30

configPlateBiasSouthMinusRect :: Layout -> Rect
configPlateBiasSouthMinusRect = configParamMinusRect 31

configPlateBiasSouthPlusRect :: Layout -> Rect
configPlateBiasSouthPlusRect = configParamPlusRect 31

configPlateBiasSouthBarRect :: Layout -> Rect
configPlateBiasSouthBarRect = configParamBarRect 31

configTfcCliffSlopeMinusRect :: Layout -> Rect
configTfcCliffSlopeMinusRect = configParamMinusRect 32

configTfcCliffSlopePlusRect :: Layout -> Rect
configTfcCliffSlopePlusRect = configParamPlusRect 32

configTfcCliffSlopeBarRect :: Layout -> Rect
configTfcCliffSlopeBarRect = configParamBarRect 32

configTfcMountainSlopeMinusRect :: Layout -> Rect
configTfcMountainSlopeMinusRect = configParamMinusRect 33

configTfcMountainSlopePlusRect :: Layout -> Rect
configTfcMountainSlopePlusRect = configParamPlusRect 33

configTfcMountainSlopeBarRect :: Layout -> Rect
configTfcMountainSlopeBarRect = configParamBarRect 33

configTfcMountainReliefMinusRect :: Layout -> Rect
configTfcMountainReliefMinusRect = configParamMinusRect 34

configTfcMountainReliefPlusRect :: Layout -> Rect
configTfcMountainReliefPlusRect = configParamPlusRect 34

configTfcMountainReliefBarRect :: Layout -> Rect
configTfcMountainReliefBarRect = configParamBarRect 34

configTfcHillSlopeMinusRect :: Layout -> Rect
configTfcHillSlopeMinusRect = configParamMinusRect 35

configTfcHillSlopePlusRect :: Layout -> Rect
configTfcHillSlopePlusRect = configParamPlusRect 35

configTfcHillSlopeBarRect :: Layout -> Rect
configTfcHillSlopeBarRect = configParamBarRect 35

configTfcRollingSlopeMinusRect :: Layout -> Rect
configTfcRollingSlopeMinusRect = configParamMinusRect 36

configTfcRollingSlopePlusRect :: Layout -> Rect
configTfcRollingSlopePlusRect = configParamPlusRect 36

configTfcRollingSlopeBarRect :: Layout -> Rect
configTfcRollingSlopeBarRect = configParamBarRect 36

configValleyCurvatureMinusRect :: Layout -> Rect
configValleyCurvatureMinusRect = configParamMinusRect 37

configValleyCurvaturePlusRect :: Layout -> Rect
configValleyCurvaturePlusRect = configParamPlusRect 37

configValleyCurvatureBarRect :: Layout -> Rect
configValleyCurvatureBarRect = configParamBarRect 37

configRockElevationThresholdMinusRect :: Layout -> Rect
configRockElevationThresholdMinusRect = configParamMinusRect 38

configRockElevationThresholdPlusRect :: Layout -> Rect
configRockElevationThresholdPlusRect = configParamPlusRect 38

configRockElevationThresholdBarRect :: Layout -> Rect
configRockElevationThresholdBarRect = configParamBarRect 38

configRockHardnessThresholdMinusRect :: Layout -> Rect
configRockHardnessThresholdMinusRect = configParamMinusRect 39

configRockHardnessThresholdPlusRect :: Layout -> Rect
configRockHardnessThresholdPlusRect = configParamPlusRect 39

configRockHardnessThresholdBarRect :: Layout -> Rect
configRockHardnessThresholdBarRect = configParamBarRect 39

configRockHardnessSecondaryMinusRect :: Layout -> Rect
configRockHardnessSecondaryMinusRect = configParamMinusRect 40

configRockHardnessSecondaryPlusRect :: Layout -> Rect
configRockHardnessSecondaryPlusRect = configParamPlusRect 40

configRockHardnessSecondaryBarRect :: Layout -> Rect
configRockHardnessSecondaryBarRect = configParamBarRect 40

configChunkMinusRect :: Layout -> Rect
configChunkMinusRect = leftChunkMinusRect

configChunkPlusRect :: Layout -> Rect
configChunkPlusRect = leftChunkPlusRect

configChunkValueRect :: Layout -> Rect
configChunkValueRect = leftChunkValueRect

configSeedValueRect :: Layout -> Rect
configSeedValueRect = leftSeedValueRect

configSeedLabelRect :: Layout -> Rect
configSeedLabelRect = leftSeedLabelRect

configSeedRandomRect :: Layout -> Rect
configSeedRandomRect = leftSeedRandomRect

-- | Config preset save button (top of 4-button stack).
configPresetSaveRect :: Layout -> Rect
configPresetSaveRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 136), V2 (w - 24) 24)

-- | Config preset load button (second in 4-button stack).
configPresetLoadRect :: Layout -> Rect
configPresetLoadRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 104), V2 (w - 24) 24)

-- | Config reset button (third in 4-button stack).
configResetRect :: Layout -> Rect
configResetRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 72), V2 (w - 24) 24)

-- | Config revert button (bottom of 4-button stack).
configRevertRect :: Layout -> Rect
configRevertRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 40), V2 (w - 24) 24)

configScrollAreaRect :: Layout -> Rect
configScrollAreaRect layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      Rect (V2 _ applyY, V2 _ _) = configPresetSaveRect layout
      pad = 16
      tabOffset = 80
      barW = 8
      barGap = 6
      top = y + tabOffset
      bottom = applyY - 8
      height = max 0 (bottom - top)
      innerW = max 0 (w - pad * 2 - barW - barGap)
  in Rect (V2 (x + pad) top, V2 innerW height)

configScrollBarRect :: Layout -> Rect
configScrollBarRect layout =
  let Rect (V2 sx sy, V2 sw sh) = configScrollAreaRect layout
      barW = 8
      barGap = 6
  in Rect (V2 (sx + sw + barGap) sy, V2 barW sh)

configRowTopPad :: Int
configRowTopPad = 12

configParamMinusRect :: Int -> Layout -> Rect
configParamMinusRect index layout =
  let Rect (V2 x y, V2 w _) = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      top = y + configRowTopPad + index * (rowHeight + gap)
  in Rect (V2 x top, V2 24 24)

configParamPlusRect :: Int -> Layout -> Rect
configParamPlusRect index layout =
  let Rect (V2 x y, V2 w _) = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      top = y + configRowTopPad + index * (rowHeight + gap)
  in Rect (V2 (x + w - 24) top, V2 24 24)

configParamBarRect :: Int -> Layout -> Rect
configParamBarRect index layout =
  let Rect (V2 x y, V2 w _) = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      top = y + configRowTopPad + index * (rowHeight + gap)
      barHeight = 12
      barY = top + (rowHeight - barHeight) `div` 2
      barX = x + 24 + 8
      barW = w - (24 * 2 + 16)
  in Rect (V2 barX barY, V2 barW barHeight)

-- | Full row rect for a config parameter, covering buttons, bar, and label.
--
-- Extends 18 px above the standard row top to include the label text area.
-- Used for tooltip hover detection so the tooltip activates over the entire
-- slider region, not just the +/- buttons.
configParamRowRect :: Int -> Layout -> Rect
configParamRowRect index layout =
  let Rect (V2 x y, V2 w _) = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      top = y + configRowTopPad + index * (rowHeight + gap)
      labelPad = 18
  in Rect (V2 x (top - labelPad), V2 w (rowHeight + labelPad))

leftControlsTop :: Layout -> Int
leftControlsTop layout =
  let Rect (V2 _ y, V2 _ _) = leftPanelRect layout
      toggleH = 22
      tabH = 22
  in y + 8 + toggleH + 8 + tabH + 16

leftParamMinusRect :: Int -> Layout -> Rect
leftParamMinusRect index layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 24 24)

leftParamPlusRect :: Int -> Layout -> Rect
leftParamPlusRect index layout =
  let Rect (V2 x y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
  in Rect (V2 (x + w - pad - 24) top, V2 24 24)

leftParamBarRect :: Int -> Layout -> Rect
leftParamBarRect index layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + index * (rowHeight + gap)
      barHeight = 12
      barY = top + (rowHeight - barHeight) `div` 2
      barX = x + pad + 24 + 8
      barW = w - (pad * 2 + 24 * 2 + 16)
  in Rect (V2 barX barY, V2 barW barHeight)

leftChunkMinusRect :: Layout -> Rect
leftChunkMinusRect = leftParamMinusRect 0

leftChunkPlusRect :: Layout -> Rect
leftChunkPlusRect = leftParamPlusRect 0

leftChunkValueRect :: Layout -> Rect
leftChunkValueRect = leftParamBarRect 0

leftSeedMinusRect :: Layout -> Rect
leftSeedMinusRect = leftParamMinusRect 1

leftSeedPlusRect :: Layout -> Rect
leftSeedPlusRect = leftParamPlusRect 1

leftSeedLabelRect :: Layout -> Rect
leftSeedLabelRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 2 * (rowHeight + gap)
      buttonW = 64
      valueW = min (layoutSeedWidth layout) (w - pad * 2 - buttonW - 8)
  in Rect (V2 (x + pad) top, V2 valueW rowHeight)

leftSeedRandomRect :: Layout -> Rect
leftSeedRandomRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 2 * (rowHeight + gap)
      buttonW = 64
  in Rect (V2 (x + w - pad - buttonW) top, V2 buttonW rowHeight)

leftSeedValueRect :: Layout -> Rect
leftSeedValueRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 3 * (rowHeight + gap)
      valueW = min (layoutSeedWidth layout) (w - pad * 2)
      valueX = x + (w - valueW) `div` 2
  in Rect (V2 valueX top, V2 valueW rowHeight)

leftViewRects :: Layout -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
leftViewRects layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      gap = 8
      buttonH = 28
      buttonW = (w - pad * 2 - gap) `div` 2
      top = leftControlsTop layout
      r1 = Rect (V2 (x + pad) top, V2 buttonW buttonH)
      r2 = Rect (V2 (x + pad + buttonW + gap) top, V2 buttonW buttonH)
      r3 = Rect (V2 (x + pad) (top + buttonH + gap), V2 buttonW buttonH)
      r4 = Rect (V2 (x + pad + buttonW + gap) (top + buttonH + gap), V2 buttonW buttonH)
      r5 = Rect (V2 (x + pad) (top + (buttonH + gap) * 2), V2 buttonW buttonH)
      r6 = Rect (V2 (x + pad + buttonW + gap) (top + (buttonH + gap) * 2), V2 buttonW buttonH)
      r7 = Rect (V2 (x + pad) (top + (buttonH + gap) * 3), V2 buttonW buttonH)
      r8 = Rect (V2 (x + pad + buttonW + gap) (top + (buttonH + gap) * 3), V2 buttonW buttonH)
      r9 = Rect (V2 (x + pad) (top + (buttonH + gap) * 4), V2 buttonW buttonH)
      r10 = Rect (V2 (x + pad + buttonW + gap) (top + (buttonH + gap) * 4), V2 buttonW buttonH)
      r11 = Rect (V2 (x + pad) (top + (buttonH + gap) * 5), V2 buttonW buttonH)
      r12 = Rect (V2 (x + pad + buttonW + gap) (top + (buttonH + gap) * 5), V2 buttonW buttonH)
      in (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)

logPanelRect :: Layout -> Rect
logPanelRect (Layout (V2 w h) logHeight _) =
  Rect (V2 0 (h - logHeight), V2 w logHeight)

logHeaderRect :: Layout -> Rect
logHeaderRect layout =
  let Rect (V2 x y, V2 w _) = logPanelRect layout
  in Rect (V2 x y, V2 w 24)

logBodyRect :: Layout -> Rect
logBodyRect layout =
  let Rect (V2 x y, V2 w h) = logPanelRect layout
      bodyHeight = max 0 (h - 24)
  in Rect (V2 x (y + 24), V2 w bodyHeight)

logFilterRects :: Layout -> (Rect, Rect, Rect, Rect)
logFilterRects layout =
  let Rect (V2 x y, V2 w h) = logHeaderRect layout
      buttonSize = 18
      gap = 6
      total = buttonSize * 4 + gap * 3
      startX = x + w - total - 12
      y0 = y + (h - buttonSize) `div` 2
      r1 = Rect (V2 startX y0, V2 buttonSize buttonSize)
      r2 = Rect (V2 (startX + buttonSize + gap) y0, V2 buttonSize buttonSize)
      r3 = Rect (V2 (startX + (buttonSize + gap) * 2) y0, V2 buttonSize buttonSize)
      r4 = Rect (V2 (startX + (buttonSize + gap) * 3) y0, V2 buttonSize buttonSize)
  in (r1, r2, r3, r4)

menuPanelRect :: Layout -> Rect
menuPanelRect (Layout (V2 w h) _ _) =
  let panelW = 240
      panelH = 180
      x = (w - panelW) `div` 2
      y = (h - panelH) `div` 2
  in Rect (V2 x y, V2 panelW panelH)

menuSaveRect :: Layout -> Rect
menuSaveRect layout = menuButtonRect layout 0

menuLoadRect :: Layout -> Rect
menuLoadRect layout = menuButtonRect layout 1

menuExitRect :: Layout -> Rect
menuExitRect layout = menuButtonRect layout 2

menuButtonRect :: Layout -> Int -> Rect
menuButtonRect layout index =
  let Rect (V2 x y, V2 w _) = menuPanelRect layout
      pad = 16
      buttonH = 28
      gap = 12
      top = y + pad + index * (buttonH + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) buttonH)

-- ---------------------------------------------------------------------------
-- Preset save dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the preset save dialog (280 × 150).
presetSaveDialogRect :: Layout -> Rect
presetSaveDialogRect (Layout (V2 w h) _ _) =
  let dw = 280; dh = 150
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Text input field inside the preset save dialog.
presetSaveInputRect :: Layout -> Rect
presetSaveInputRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetSaveDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | \"Ok\" button in the preset save dialog.
presetSaveOkRect :: Layout -> Rect
presetSaveOkRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the preset save dialog.
presetSaveCancelRect :: Layout -> Rect
presetSaveCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- Preset load dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the preset load dialog (280 × 320).
presetLoadDialogRect :: Layout -> Rect
presetLoadDialogRect (Layout (V2 w h) _ _) =
  let dw = 280; dh = 320
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Scrollable list area inside the preset load dialog.
presetLoadListRect :: Layout -> Rect
presetLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = presetLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 200)

-- | Individual item rect inside the preset load list.
presetLoadItemRect :: Layout -> Int -> Rect
presetLoadItemRect layout index =
  let Rect (V2 lx ly, V2 lw _) = presetLoadListRect layout
      itemH = 24
  in Rect (V2 lx (ly + index * itemH), V2 lw itemH)

-- | \"Load\" button in the preset load dialog.
presetLoadOkRect :: Layout -> Rect
presetLoadOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = presetLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the preset load dialog.
presetLoadCancelRect :: Layout -> Rect
presetLoadCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = presetLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- World save dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the world save dialog (300 × 140).
worldSaveDialogRect :: Layout -> Rect
worldSaveDialogRect (Layout (V2 w h) _ _) =
  let dw = 300; dh = 140
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Text input field inside the world save dialog.
worldSaveInputRect :: Layout -> Rect
worldSaveInputRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldSaveDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 24)

-- | \"Save\" button in the world save dialog.
worldSaveOkRect :: Layout -> Rect
worldSaveOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = worldSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the world save dialog.
worldSaveCancelRect :: Layout -> Rect
worldSaveCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = worldSaveDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)

-- ---------------------------------------------------------------------------
-- World load dialog
-- ---------------------------------------------------------------------------

-- | Centered overlay for the world load dialog (320 × 360).
worldLoadDialogRect :: Layout -> Rect
worldLoadDialogRect (Layout (V2 w h) _ _) =
  let dw = 320; dh = 360
      dx = (w - dw) `div` 2
      dy = (h - dh) `div` 2
  in Rect (V2 dx dy, V2 dw dh)

-- | Scrollable list area inside the world load dialog.
worldLoadListRect :: Layout -> Rect
worldLoadListRect layout =
  let Rect (V2 dx dy, V2 dw _) = worldLoadDialogRect layout
      pad = 16
  in Rect (V2 (dx + pad) (dy + 40), V2 (dw - pad * 2) 240)

-- | Individual item rect inside the world load list.
worldLoadItemRect :: Layout -> Int -> Rect
worldLoadItemRect layout index =
  let Rect (V2 lx ly, V2 lw _) = worldLoadListRect layout
      itemH = 28
  in Rect (V2 lx (ly + index * itemH), V2 lw itemH)

-- | \"Load\" button in the world load dialog.
worldLoadOkRect :: Layout -> Rect
worldLoadOkRect layout =
  let Rect (V2 dx dy, V2 _dw dh) = worldLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + pad) (dy + dh - pad - btnH), V2 btnW btnH)

-- | \"Cancel\" button in the world load dialog.
worldLoadCancelRect :: Layout -> Rect
worldLoadCancelRect layout =
  let Rect (V2 dx dy, V2 dw dh) = worldLoadDialogRect layout
      pad = 16; btnW = 100; btnH = 28
  in Rect (V2 (dx + dw - pad - btnW) (dy + dh - pad - btnH), V2 btnW btnH)
