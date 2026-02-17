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
configWeatherAmplitudeMinusRect = configParamMinusRect 2

configWeatherAmplitudePlusRect :: Layout -> Rect
configWeatherAmplitudePlusRect = configParamPlusRect 2

configWeatherAmplitudeBarRect :: Layout -> Rect
configWeatherAmplitudeBarRect = configParamBarRect 2

configSeasonCycleLengthMinusRect :: Layout -> Rect
configSeasonCycleLengthMinusRect = configParamMinusRect 3

configSeasonCycleLengthPlusRect :: Layout -> Rect
configSeasonCycleLengthPlusRect = configParamPlusRect 3

configSeasonCycleLengthBarRect :: Layout -> Rect
configSeasonCycleLengthBarRect = configParamBarRect 3

configJitterAmplitudeMinusRect :: Layout -> Rect
configJitterAmplitudeMinusRect = configParamMinusRect 4

configJitterAmplitudePlusRect :: Layout -> Rect
configJitterAmplitudePlusRect = configParamPlusRect 4

configJitterAmplitudeBarRect :: Layout -> Rect
configJitterAmplitudeBarRect = configParamBarRect 4

configPressureBaseMinusRect :: Layout -> Rect
configPressureBaseMinusRect = configParamMinusRect 5

configPressureBasePlusRect :: Layout -> Rect
configPressureBasePlusRect = configParamPlusRect 5

configPressureBaseBarRect :: Layout -> Rect
configPressureBaseBarRect = configParamBarRect 5

configPressureTempScaleMinusRect :: Layout -> Rect
configPressureTempScaleMinusRect = configParamMinusRect 6

configPressureTempScalePlusRect :: Layout -> Rect
configPressureTempScalePlusRect = configParamPlusRect 6

configPressureTempScaleBarRect :: Layout -> Rect
configPressureTempScaleBarRect = configParamBarRect 6

configPressureCoriolisScaleMinusRect :: Layout -> Rect
configPressureCoriolisScaleMinusRect = configParamMinusRect 7

configPressureCoriolisScalePlusRect :: Layout -> Rect
configPressureCoriolisScalePlusRect = configParamPlusRect 7

configPressureCoriolisScaleBarRect :: Layout -> Rect
configPressureCoriolisScaleBarRect = configParamBarRect 7

configSeasonalBaseMinusRect :: Layout -> Rect
configSeasonalBaseMinusRect = configParamMinusRect 8

configSeasonalBasePlusRect :: Layout -> Rect
configSeasonalBasePlusRect = configParamPlusRect 8

configSeasonalBaseBarRect :: Layout -> Rect
configSeasonalBaseBarRect = configParamBarRect 8

configSeasonalRangeMinusRect :: Layout -> Rect
configSeasonalRangeMinusRect = configParamMinusRect 9

configSeasonalRangePlusRect :: Layout -> Rect
configSeasonalRangePlusRect = configParamPlusRect 9

configSeasonalRangeBarRect :: Layout -> Rect
configSeasonalRangeBarRect = configParamBarRect 9

configHumidityNoiseScaleMinusRect :: Layout -> Rect
configHumidityNoiseScaleMinusRect = configParamMinusRect 10

configHumidityNoiseScalePlusRect :: Layout -> Rect
configHumidityNoiseScalePlusRect = configParamPlusRect 10

configHumidityNoiseScaleBarRect :: Layout -> Rect
configHumidityNoiseScaleBarRect = configParamBarRect 10

configPrecipNoiseScaleMinusRect :: Layout -> Rect
configPrecipNoiseScaleMinusRect = configParamMinusRect 11

configPrecipNoiseScalePlusRect :: Layout -> Rect
configPrecipNoiseScalePlusRect = configParamPlusRect 11

configPrecipNoiseScaleBarRect :: Layout -> Rect
configPrecipNoiseScaleBarRect = configParamBarRect 11

configWeatherITCZWidthMinusRect :: Layout -> Rect
configWeatherITCZWidthMinusRect = configParamMinusRect 12

configWeatherITCZWidthPlusRect :: Layout -> Rect
configWeatherITCZWidthPlusRect = configParamPlusRect 12

configWeatherITCZWidthBarRect :: Layout -> Rect
configWeatherITCZWidthBarRect = configParamBarRect 12

configWeatherITCZPrecipBoostMinusRect :: Layout -> Rect
configWeatherITCZPrecipBoostMinusRect = configParamMinusRect 13

configWeatherITCZPrecipBoostPlusRect :: Layout -> Rect
configWeatherITCZPrecipBoostPlusRect = configParamPlusRect 13

configWeatherITCZPrecipBoostBarRect :: Layout -> Rect
configWeatherITCZPrecipBoostBarRect = configParamBarRect 13

configPressureHumidityScaleMinusRect :: Layout -> Rect
configPressureHumidityScaleMinusRect = configParamMinusRect 14

configPressureHumidityScalePlusRect :: Layout -> Rect
configPressureHumidityScalePlusRect = configParamPlusRect 14

configPressureHumidityScaleBarRect :: Layout -> Rect
configPressureHumidityScaleBarRect = configParamBarRect 14

configPressureGradientWindScaleMinusRect :: Layout -> Rect
configPressureGradientWindScaleMinusRect = configParamMinusRect 15

configPressureGradientWindScalePlusRect :: Layout -> Rect
configPressureGradientWindScalePlusRect = configParamPlusRect 15

configPressureGradientWindScaleBarRect :: Layout -> Rect
configPressureGradientWindScaleBarRect = configParamBarRect 15

configWindNoiseScaleMinusRect :: Layout -> Rect
configWindNoiseScaleMinusRect = configParamMinusRect 16

configWindNoiseScalePlusRect :: Layout -> Rect
configWindNoiseScalePlusRect = configParamPlusRect 16

configWindNoiseScaleBarRect :: Layout -> Rect
configWindNoiseScaleBarRect = configParamBarRect 16

configITCZMigrationScaleMinusRect :: Layout -> Rect
configITCZMigrationScaleMinusRect = configParamMinusRect 17

configITCZMigrationScalePlusRect :: Layout -> Rect
configITCZMigrationScalePlusRect = configParamPlusRect 17

configITCZMigrationScaleBarRect :: Layout -> Rect
configITCZMigrationScaleBarRect = configParamBarRect 17

configCloudRHExponentMinusRect :: Layout -> Rect
configCloudRHExponentMinusRect = configParamMinusRect 18

configCloudRHExponentPlusRect :: Layout -> Rect
configCloudRHExponentPlusRect = configParamPlusRect 18

configCloudRHExponentBarRect :: Layout -> Rect
configCloudRHExponentBarRect = configParamBarRect 18

configCloudAlbedoEffectMinusRect :: Layout -> Rect
configCloudAlbedoEffectMinusRect = configParamMinusRect 19

configCloudAlbedoEffectPlusRect :: Layout -> Rect
configCloudAlbedoEffectPlusRect = configParamPlusRect 19

configCloudAlbedoEffectBarRect :: Layout -> Rect
configCloudAlbedoEffectBarRect = configParamBarRect 19

configCloudPrecipBoostMinusRect :: Layout -> Rect
configCloudPrecipBoostMinusRect = configParamMinusRect 20

configCloudPrecipBoostPlusRect :: Layout -> Rect
configCloudPrecipBoostPlusRect = configParamPlusRect 20

configCloudPrecipBoostBarRect :: Layout -> Rect
configCloudPrecipBoostBarRect = configParamBarRect 20

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
configVegTempWeightMinusRect = configParamMinusRect 2

configVegTempWeightPlusRect :: Layout -> Rect
configVegTempWeightPlusRect = configParamPlusRect 2

configVegTempWeightBarRect :: Layout -> Rect
configVegTempWeightBarRect = configParamBarRect 2

configVegPrecipWeightMinusRect :: Layout -> Rect
configVegPrecipWeightMinusRect = configParamMinusRect 3

configVegPrecipWeightPlusRect :: Layout -> Rect
configVegPrecipWeightPlusRect = configParamPlusRect 3

configVegPrecipWeightBarRect :: Layout -> Rect
configVegPrecipWeightBarRect = configParamBarRect 3

configBtCoastalBandMinusRect :: Layout -> Rect
configBtCoastalBandMinusRect = configParamMinusRect 4
configBtCoastalBandPlusRect :: Layout -> Rect
configBtCoastalBandPlusRect = configParamPlusRect 4
configBtCoastalBandBarRect :: Layout -> Rect
configBtCoastalBandBarRect = configParamBarRect 4

configBtSnowMaxTempMinusRect :: Layout -> Rect
configBtSnowMaxTempMinusRect = configParamMinusRect 5
configBtSnowMaxTempPlusRect :: Layout -> Rect
configBtSnowMaxTempPlusRect = configParamPlusRect 5
configBtSnowMaxTempBarRect :: Layout -> Rect
configBtSnowMaxTempBarRect = configParamBarRect 5

configBtAlpineMaxTempMinusRect :: Layout -> Rect
configBtAlpineMaxTempMinusRect = configParamMinusRect 6
configBtAlpineMaxTempPlusRect :: Layout -> Rect
configBtAlpineMaxTempPlusRect = configParamPlusRect 6
configBtAlpineMaxTempBarRect :: Layout -> Rect
configBtAlpineMaxTempBarRect = configParamBarRect 6

configBtIceCapTempMinusRect :: Layout -> Rect
configBtIceCapTempMinusRect = configParamMinusRect 7
configBtIceCapTempPlusRect :: Layout -> Rect
configBtIceCapTempPlusRect = configParamPlusRect 7
configBtIceCapTempBarRect :: Layout -> Rect
configBtIceCapTempBarRect = configParamBarRect 7

configBtMontaneMaxTempMinusRect :: Layout -> Rect
configBtMontaneMaxTempMinusRect = configParamMinusRect 8
configBtMontaneMaxTempPlusRect :: Layout -> Rect
configBtMontaneMaxTempPlusRect = configParamPlusRect 8
configBtMontaneMaxTempBarRect :: Layout -> Rect
configBtMontaneMaxTempBarRect = configParamBarRect 8

configBtMontanePrecipMinusRect :: Layout -> Rect
configBtMontanePrecipMinusRect = configParamMinusRect 9
configBtMontanePrecipPlusRect :: Layout -> Rect
configBtMontanePrecipPlusRect = configParamPlusRect 9
configBtMontanePrecipBarRect :: Layout -> Rect
configBtMontanePrecipBarRect = configParamBarRect 9

configBtCliffSlopeMinusRect :: Layout -> Rect
configBtCliffSlopeMinusRect = configParamMinusRect 10
configBtCliffSlopePlusRect :: Layout -> Rect
configBtCliffSlopePlusRect = configParamPlusRect 10
configBtCliffSlopeBarRect :: Layout -> Rect
configBtCliffSlopeBarRect = configParamBarRect 10

configBtValleyMoistureMinusRect :: Layout -> Rect
configBtValleyMoistureMinusRect = configParamMinusRect 11
configBtValleyMoisturePlusRect :: Layout -> Rect
configBtValleyMoisturePlusRect = configParamPlusRect 11
configBtValleyMoistureBarRect :: Layout -> Rect
configBtValleyMoistureBarRect = configParamBarRect 11

configBtDepressionMoistureMinusRect :: Layout -> Rect
configBtDepressionMoistureMinusRect = configParamMinusRect 12
configBtDepressionMoisturePlusRect :: Layout -> Rect
configBtDepressionMoisturePlusRect = configParamPlusRect 12
configBtDepressionMoistureBarRect :: Layout -> Rect
configBtDepressionMoistureBarRect = configParamBarRect 12

configBtPrecipWeightMinusRect :: Layout -> Rect
configBtPrecipWeightMinusRect = configParamMinusRect 13
configBtPrecipWeightPlusRect :: Layout -> Rect
configBtPrecipWeightPlusRect = configParamPlusRect 13
configBtPrecipWeightBarRect :: Layout -> Rect
configBtPrecipWeightBarRect = configParamBarRect 13

configVbcTempMinMinusRect :: Layout -> Rect
configVbcTempMinMinusRect = configParamMinusRect 14
configVbcTempMinPlusRect :: Layout -> Rect
configVbcTempMinPlusRect = configParamPlusRect 14
configVbcTempMinBarRect :: Layout -> Rect
configVbcTempMinBarRect = configParamBarRect 14

configVbcTempRangeMinusRect :: Layout -> Rect
configVbcTempRangeMinusRect = configParamMinusRect 15
configVbcTempRangePlusRect :: Layout -> Rect
configVbcTempRangePlusRect = configParamPlusRect 15
configVbcTempRangeBarRect :: Layout -> Rect
configVbcTempRangeBarRect = configParamBarRect 15

configVbcFertilityBoostMinusRect :: Layout -> Rect
configVbcFertilityBoostMinusRect = configParamMinusRect 16
configVbcFertilityBoostPlusRect :: Layout -> Rect
configVbcFertilityBoostPlusRect = configParamPlusRect 16
configVbcFertilityBoostBarRect :: Layout -> Rect
configVbcFertilityBoostBarRect = configParamBarRect 16

configVbcAlbedoBaseMinusRect :: Layout -> Rect
configVbcAlbedoBaseMinusRect = configParamMinusRect 17
configVbcAlbedoBasePlusRect :: Layout -> Rect
configVbcAlbedoBasePlusRect = configParamPlusRect 17
configVbcAlbedoBaseBarRect :: Layout -> Rect
configVbcAlbedoBaseBarRect = configParamBarRect 17

configVbcAlbedoBareMinusRect :: Layout -> Rect
configVbcAlbedoBareMinusRect = configParamMinusRect 18
configVbcAlbedoBarePlusRect :: Layout -> Rect
configVbcAlbedoBarePlusRect = configParamPlusRect 18
configVbcAlbedoBareBarRect :: Layout -> Rect
configVbcAlbedoBareBarRect = configParamBarRect 18

configVbcAlbedoVegMinusRect :: Layout -> Rect
configVbcAlbedoVegMinusRect = configParamMinusRect 19
configVbcAlbedoVegPlusRect :: Layout -> Rect
configVbcAlbedoVegPlusRect = configParamPlusRect 19
configVbcAlbedoVegBarRect :: Layout -> Rect
configVbcAlbedoVegBarRect = configParamBarRect 19

configVbcOceanAlbedoMinusRect :: Layout -> Rect
configVbcOceanAlbedoMinusRect = configParamMinusRect 20
configVbcOceanAlbedoPlusRect :: Layout -> Rect
configVbcOceanAlbedoPlusRect = configParamPlusRect 20
configVbcOceanAlbedoBarRect :: Layout -> Rect
configVbcOceanAlbedoBarRect = configParamBarRect 20

configVbcIceAlbedoMinusRect :: Layout -> Rect
configVbcIceAlbedoMinusRect = configParamMinusRect 21
configVbcIceAlbedoPlusRect :: Layout -> Rect
configVbcIceAlbedoPlusRect = configParamPlusRect 21
configVbcIceAlbedoBarRect :: Layout -> Rect
configVbcIceAlbedoBarRect = configParamBarRect 21

configBiomeSmoothingMinusRect :: Layout -> Rect
configBiomeSmoothingMinusRect = configParamMinusRect 22
configBiomeSmoothingPlusRect :: Layout -> Rect
configBiomeSmoothingPlusRect = configParamPlusRect 22
configBiomeSmoothingBarRect :: Layout -> Rect
configBiomeSmoothingBarRect = configParamBarRect 22

configVolcanicAshBoostMinusRect :: Layout -> Rect
configVolcanicAshBoostMinusRect = configParamMinusRect 23
configVolcanicAshBoostPlusRect :: Layout -> Rect
configVolcanicAshBoostPlusRect = configParamPlusRect 23
configVolcanicAshBoostBarRect :: Layout -> Rect
configVolcanicAshBoostBarRect = configParamBarRect 23

configVolcanicLavaPenaltyMinusRect :: Layout -> Rect
configVolcanicLavaPenaltyMinusRect = configParamMinusRect 24
configVolcanicLavaPenaltyPlusRect :: Layout -> Rect
configVolcanicLavaPenaltyPlusRect = configParamPlusRect 24
configVolcanicLavaPenaltyBarRect :: Layout -> Rect
configVolcanicLavaPenaltyBarRect = configParamBarRect 24

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
configInsolationMinusRect = configParamMinusRect 2

configInsolationPlusRect :: Layout -> Rect
configInsolationPlusRect = configParamPlusRect 2

configInsolationBarRect :: Layout -> Rect
configInsolationBarRect = configParamBarRect 2

configOccWarmScaleMinusRect :: Layout -> Rect
configOccWarmScaleMinusRect = configParamMinusRect 3

configOccWarmScalePlusRect :: Layout -> Rect
configOccWarmScalePlusRect = configParamPlusRect 3

configOccWarmScaleBarRect :: Layout -> Rect
configOccWarmScaleBarRect = configParamBarRect 3

configOccColdScaleMinusRect :: Layout -> Rect
configOccColdScaleMinusRect = configParamMinusRect 4

configOccColdScalePlusRect :: Layout -> Rect
configOccColdScalePlusRect = configParamPlusRect 4

configOccColdScaleBarRect :: Layout -> Rect
configOccColdScaleBarRect = configParamBarRect 4

configOccLatPeakDegMinusRect :: Layout -> Rect
configOccLatPeakDegMinusRect = configParamMinusRect 5

configOccLatPeakDegPlusRect :: Layout -> Rect
configOccLatPeakDegPlusRect = configParamPlusRect 5

configOccLatPeakDegBarRect :: Layout -> Rect
configOccLatPeakDegBarRect = configParamBarRect 5

configOccLatWidthDegMinusRect :: Layout -> Rect
configOccLatWidthDegMinusRect = configParamMinusRect 6

configOccLatWidthDegPlusRect :: Layout -> Rect
configOccLatWidthDegPlusRect = configParamPlusRect 6

configOccLatWidthDegBarRect :: Layout -> Rect
configOccLatWidthDegBarRect = configParamBarRect 6

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
configErosionRainRateMinusRect = configParamMinusRect 2

configErosionRainRatePlusRect :: Layout -> Rect
configErosionRainRatePlusRect = configParamPlusRect 2

configErosionRainRateBarRect :: Layout -> Rect
configErosionRainRateBarRect = configParamBarRect 2

configErosionTalusMinusRect :: Layout -> Rect
configErosionTalusMinusRect = configParamMinusRect 3

configErosionTalusPlusRect :: Layout -> Rect
configErosionTalusPlusRect = configParamPlusRect 3

configErosionTalusBarRect :: Layout -> Rect
configErosionTalusBarRect = configParamBarRect 3

configErosionMaxDropMinusRect :: Layout -> Rect
configErosionMaxDropMinusRect = configParamMinusRect 4

configErosionMaxDropPlusRect :: Layout -> Rect
configErosionMaxDropPlusRect = configParamPlusRect 4

configErosionMaxDropBarRect :: Layout -> Rect
configErosionMaxDropBarRect = configParamBarRect 4

configGlacierSnowTempMinusRect :: Layout -> Rect
configGlacierSnowTempMinusRect = configParamMinusRect 5
configGlacierSnowTempPlusRect :: Layout -> Rect
configGlacierSnowTempPlusRect = configParamPlusRect 5
configGlacierSnowTempBarRect :: Layout -> Rect
configGlacierSnowTempBarRect = configParamBarRect 5

configGlacierSnowRangeMinusRect :: Layout -> Rect
configGlacierSnowRangeMinusRect = configParamMinusRect 6
configGlacierSnowRangePlusRect :: Layout -> Rect
configGlacierSnowRangePlusRect = configParamPlusRect 6
configGlacierSnowRangeBarRect :: Layout -> Rect
configGlacierSnowRangeBarRect = configParamBarRect 6

configGlacierMeltTempMinusRect :: Layout -> Rect
configGlacierMeltTempMinusRect = configParamMinusRect 7
configGlacierMeltTempPlusRect :: Layout -> Rect
configGlacierMeltTempPlusRect = configParamPlusRect 7
configGlacierMeltTempBarRect :: Layout -> Rect
configGlacierMeltTempBarRect = configParamBarRect 7

configGlacierMeltRateMinusRect :: Layout -> Rect
configGlacierMeltRateMinusRect = configParamMinusRect 8
configGlacierMeltRatePlusRect :: Layout -> Rect
configGlacierMeltRatePlusRect = configParamPlusRect 8
configGlacierMeltRateBarRect :: Layout -> Rect
configGlacierMeltRateBarRect = configParamBarRect 8

configGlacierAccumScaleMinusRect :: Layout -> Rect
configGlacierAccumScaleMinusRect = configParamMinusRect 9
configGlacierAccumScalePlusRect :: Layout -> Rect
configGlacierAccumScalePlusRect = configParamPlusRect 9
configGlacierAccumScaleBarRect :: Layout -> Rect
configGlacierAccumScaleBarRect = configParamBarRect 9

configGlacierFlowItersMinusRect :: Layout -> Rect
configGlacierFlowItersMinusRect = configParamMinusRect 10
configGlacierFlowItersPlusRect :: Layout -> Rect
configGlacierFlowItersPlusRect = configParamPlusRect 10
configGlacierFlowItersBarRect :: Layout -> Rect
configGlacierFlowItersBarRect = configParamBarRect 10

configGlacierFlowRateMinusRect :: Layout -> Rect
configGlacierFlowRateMinusRect = configParamMinusRect 11
configGlacierFlowRatePlusRect :: Layout -> Rect
configGlacierFlowRatePlusRect = configParamPlusRect 11
configGlacierFlowRateBarRect :: Layout -> Rect
configGlacierFlowRateBarRect = configParamBarRect 11

configGlacierErosionScaleMinusRect :: Layout -> Rect
configGlacierErosionScaleMinusRect = configParamMinusRect 12
configGlacierErosionScalePlusRect :: Layout -> Rect
configGlacierErosionScalePlusRect = configParamPlusRect 12
configGlacierErosionScaleBarRect :: Layout -> Rect
configGlacierErosionScaleBarRect = configParamBarRect 12

configGlacierCarveScaleMinusRect :: Layout -> Rect
configGlacierCarveScaleMinusRect = configParamMinusRect 13
configGlacierCarveScalePlusRect :: Layout -> Rect
configGlacierCarveScalePlusRect = configParamPlusRect 13
configGlacierCarveScaleBarRect :: Layout -> Rect
configGlacierCarveScaleBarRect = configParamBarRect 13

configGlacierDepositScaleMinusRect :: Layout -> Rect
configGlacierDepositScaleMinusRect = configParamMinusRect 14
configGlacierDepositScalePlusRect :: Layout -> Rect
configGlacierDepositScalePlusRect = configParamPlusRect 14
configGlacierDepositScaleBarRect :: Layout -> Rect
configGlacierDepositScaleBarRect = configParamBarRect 14

configVentDensityMinusRect :: Layout -> Rect
configVentDensityMinusRect = configParamMinusRect 15
configVentDensityPlusRect :: Layout -> Rect
configVentDensityPlusRect = configParamPlusRect 15
configVentDensityBarRect :: Layout -> Rect
configVentDensityBarRect = configParamBarRect 15

configVentThresholdMinusRect :: Layout -> Rect
configVentThresholdMinusRect = configParamMinusRect 16
configVentThresholdPlusRect :: Layout -> Rect
configVentThresholdPlusRect = configParamPlusRect 16
configVentThresholdBarRect :: Layout -> Rect
configVentThresholdBarRect = configParamBarRect 16

configHotspotScaleMinusRect :: Layout -> Rect
configHotspotScaleMinusRect = configParamMinusRect 17
configHotspotScalePlusRect :: Layout -> Rect
configHotspotScalePlusRect = configParamPlusRect 17
configHotspotScaleBarRect :: Layout -> Rect
configHotspotScaleBarRect = configParamBarRect 17

configHotspotThresholdMinusRect :: Layout -> Rect
configHotspotThresholdMinusRect = configParamMinusRect 18
configHotspotThresholdPlusRect :: Layout -> Rect
configHotspotThresholdPlusRect = configParamPlusRect 18
configHotspotThresholdBarRect :: Layout -> Rect
configHotspotThresholdBarRect = configParamBarRect 18

configMagmaRechargeMinusRect :: Layout -> Rect
configMagmaRechargeMinusRect = configParamMinusRect 19
configMagmaRechargePlusRect :: Layout -> Rect
configMagmaRechargePlusRect = configParamPlusRect 19
configMagmaRechargeBarRect :: Layout -> Rect
configMagmaRechargeBarRect = configParamBarRect 19

configLavaScaleMinusRect :: Layout -> Rect
configLavaScaleMinusRect = configParamMinusRect 20
configLavaScalePlusRect :: Layout -> Rect
configLavaScalePlusRect = configParamPlusRect 20
configLavaScaleBarRect :: Layout -> Rect
configLavaScaleBarRect = configParamBarRect 20

configAshScaleMinusRect :: Layout -> Rect
configAshScaleMinusRect = configParamMinusRect 21
configAshScalePlusRect :: Layout -> Rect
configAshScalePlusRect = configParamPlusRect 21
configAshScaleBarRect :: Layout -> Rect
configAshScaleBarRect = configParamBarRect 21

configVolcanicDepositScaleMinusRect :: Layout -> Rect
configVolcanicDepositScaleMinusRect = configParamMinusRect 22
configVolcanicDepositScalePlusRect :: Layout -> Rect
configVolcanicDepositScalePlusRect = configParamPlusRect 22
configVolcanicDepositScaleBarRect :: Layout -> Rect
configVolcanicDepositScaleBarRect = configParamBarRect 22

configSoilMoistureThresholdMinusRect :: Layout -> Rect
configSoilMoistureThresholdMinusRect = configParamMinusRect 23
configSoilMoistureThresholdPlusRect :: Layout -> Rect
configSoilMoistureThresholdPlusRect = configParamPlusRect 23
configSoilMoistureThresholdBarRect :: Layout -> Rect
configSoilMoistureThresholdBarRect = configParamBarRect 23

configSoilHardnessThresholdMinusRect :: Layout -> Rect
configSoilHardnessThresholdMinusRect = configParamMinusRect 24
configSoilHardnessThresholdPlusRect :: Layout -> Rect
configSoilHardnessThresholdPlusRect = configParamPlusRect 24
configSoilHardnessThresholdBarRect :: Layout -> Rect
configSoilHardnessThresholdBarRect = configParamBarRect 24

configSoilFertilityMoistWeightMinusRect :: Layout -> Rect
configSoilFertilityMoistWeightMinusRect = configParamMinusRect 25
configSoilFertilityMoistWeightPlusRect :: Layout -> Rect
configSoilFertilityMoistWeightPlusRect = configParamPlusRect 25
configSoilFertilityMoistWeightBarRect :: Layout -> Rect
configSoilFertilityMoistWeightBarRect = configParamBarRect 25

configSoilFertilityDepthWeightMinusRect :: Layout -> Rect
configSoilFertilityDepthWeightMinusRect = configParamMinusRect 26
configSoilFertilityDepthWeightPlusRect :: Layout -> Rect
configSoilFertilityDepthWeightPlusRect = configParamPlusRect 26
configSoilFertilityDepthWeightBarRect :: Layout -> Rect
configSoilFertilityDepthWeightBarRect = configParamBarRect 26

configSinkBreachDepthMinusRect :: Layout -> Rect
configSinkBreachDepthMinusRect = configParamMinusRect 27
configSinkBreachDepthPlusRect :: Layout -> Rect
configSinkBreachDepthPlusRect = configParamPlusRect 27
configSinkBreachDepthBarRect :: Layout -> Rect
configSinkBreachDepthBarRect = configParamBarRect 27

configStreamPowerMaxErosionMinusRect :: Layout -> Rect
configStreamPowerMaxErosionMinusRect = configParamMinusRect 28
configStreamPowerMaxErosionPlusRect :: Layout -> Rect
configStreamPowerMaxErosionPlusRect = configParamPlusRect 28
configStreamPowerMaxErosionBarRect :: Layout -> Rect
configStreamPowerMaxErosionBarRect = configParamBarRect 28

configRiverCarveMaxDepthMinusRect :: Layout -> Rect
configRiverCarveMaxDepthMinusRect = configParamMinusRect 29
configRiverCarveMaxDepthPlusRect :: Layout -> Rect
configRiverCarveMaxDepthPlusRect = configParamPlusRect 29
configRiverCarveMaxDepthBarRect :: Layout -> Rect
configRiverCarveMaxDepthBarRect = configParamBarRect 29

configCoastalErodeStrengthMinusRect :: Layout -> Rect
configCoastalErodeStrengthMinusRect = configParamMinusRect 30
configCoastalErodeStrengthPlusRect :: Layout -> Rect
configCoastalErodeStrengthPlusRect = configParamPlusRect 30
configCoastalErodeStrengthBarRect :: Layout -> Rect
configCoastalErodeStrengthBarRect = configParamBarRect 30

configHydroHardnessWeightMinusRect :: Layout -> Rect
configHydroHardnessWeightMinusRect = configParamMinusRect 31
configHydroHardnessWeightPlusRect :: Layout -> Rect
configHydroHardnessWeightPlusRect = configParamPlusRect 31
configHydroHardnessWeightBarRect :: Layout -> Rect
configHydroHardnessWeightBarRect = configParamBarRect 31

configMinLakeSizeMinusRect :: Layout -> Rect
configMinLakeSizeMinusRect = configParamMinusRect 32
configMinLakeSizePlusRect :: Layout -> Rect
configMinLakeSizePlusRect = configParamPlusRect 32
configMinLakeSizeBarRect :: Layout -> Rect
configMinLakeSizeBarRect = configParamBarRect 32

configInlandSeaMinSizeMinusRect :: Layout -> Rect
configInlandSeaMinSizeMinusRect = configParamMinusRect 33
configInlandSeaMinSizePlusRect :: Layout -> Rect
configInlandSeaMinSizePlusRect = configParamPlusRect 33
configInlandSeaMinSizeBarRect :: Layout -> Rect
configInlandSeaMinSizeBarRect = configParamBarRect 33

configRoughnessScaleMinusRect :: Layout -> Rect
configRoughnessScaleMinusRect = configParamMinusRect 34
configRoughnessScalePlusRect :: Layout -> Rect
configRoughnessScalePlusRect = configParamPlusRect 34
configRoughnessScaleBarRect :: Layout -> Rect
configRoughnessScaleBarRect = configParamBarRect 34

configGenScaleMinusRect :: Layout -> Rect
configGenScaleMinusRect = configParamMinusRect 0

configGenScalePlusRect :: Layout -> Rect
configGenScalePlusRect = configParamPlusRect 0

configGenScaleBarRect :: Layout -> Rect
configGenScaleBarRect = configParamBarRect 0

configGenCoordScaleMinusRect :: Layout -> Rect
configGenCoordScaleMinusRect = configParamMinusRect 1

configGenCoordScalePlusRect :: Layout -> Rect
configGenCoordScalePlusRect = configParamPlusRect 1

configGenCoordScaleBarRect :: Layout -> Rect
configGenCoordScaleBarRect = configParamBarRect 1

configGenOffsetXMinusRect :: Layout -> Rect
configGenOffsetXMinusRect = configParamMinusRect 2

configGenOffsetXPlusRect :: Layout -> Rect
configGenOffsetXPlusRect = configParamPlusRect 2

configGenOffsetXBarRect :: Layout -> Rect
configGenOffsetXBarRect = configParamBarRect 2

configGenOffsetYMinusRect :: Layout -> Rect
configGenOffsetYMinusRect = configParamMinusRect 3

configGenOffsetYPlusRect :: Layout -> Rect
configGenOffsetYPlusRect = configParamPlusRect 3

configGenOffsetYBarRect :: Layout -> Rect
configGenOffsetYBarRect = configParamBarRect 3

configGenFrequencyMinusRect :: Layout -> Rect
configGenFrequencyMinusRect = configParamMinusRect 4

configGenFrequencyPlusRect :: Layout -> Rect
configGenFrequencyPlusRect = configParamPlusRect 4

configGenFrequencyBarRect :: Layout -> Rect
configGenFrequencyBarRect = configParamBarRect 4

configGenOctavesMinusRect :: Layout -> Rect
configGenOctavesMinusRect = configParamMinusRect 5

configGenOctavesPlusRect :: Layout -> Rect
configGenOctavesPlusRect = configParamPlusRect 5

configGenOctavesBarRect :: Layout -> Rect
configGenOctavesBarRect = configParamBarRect 5

configGenLacunarityMinusRect :: Layout -> Rect
configGenLacunarityMinusRect = configParamMinusRect 6

configGenLacunarityPlusRect :: Layout -> Rect
configGenLacunarityPlusRect = configParamPlusRect 6

configGenLacunarityBarRect :: Layout -> Rect
configGenLacunarityBarRect = configParamBarRect 6

configGenGainMinusRect :: Layout -> Rect
configGenGainMinusRect = configParamMinusRect 7

configGenGainPlusRect :: Layout -> Rect
configGenGainPlusRect = configParamPlusRect 7

configGenGainBarRect :: Layout -> Rect
configGenGainBarRect = configParamBarRect 7

configGenWarpScaleMinusRect :: Layout -> Rect
configGenWarpScaleMinusRect = configParamMinusRect 8

configGenWarpScalePlusRect :: Layout -> Rect
configGenWarpScalePlusRect = configParamPlusRect 8

configGenWarpScaleBarRect :: Layout -> Rect
configGenWarpScaleBarRect = configParamBarRect 8

configGenWarpStrengthMinusRect :: Layout -> Rect
configGenWarpStrengthMinusRect = configParamMinusRect 9

configGenWarpStrengthPlusRect :: Layout -> Rect
configGenWarpStrengthPlusRect = configParamPlusRect 9

configGenWarpStrengthBarRect :: Layout -> Rect
configGenWarpStrengthBarRect = configParamBarRect 9

configExtentXMinusRect :: Layout -> Rect
configExtentXMinusRect = configParamMinusRect 10

configExtentXPlusRect :: Layout -> Rect
configExtentXPlusRect = configParamPlusRect 10

configExtentXBarRect :: Layout -> Rect
configExtentXBarRect = configParamBarRect 10

configExtentYMinusRect :: Layout -> Rect
configExtentYMinusRect = configParamMinusRect 11

configExtentYPlusRect :: Layout -> Rect
configExtentYPlusRect = configParamPlusRect 11

configExtentYBarRect :: Layout -> Rect
configExtentYBarRect = configParamBarRect 11

configEdgeNorthMinusRect :: Layout -> Rect
configEdgeNorthMinusRect = configParamMinusRect 12

configEdgeNorthPlusRect :: Layout -> Rect
configEdgeNorthPlusRect = configParamPlusRect 12

configEdgeNorthBarRect :: Layout -> Rect
configEdgeNorthBarRect = configParamBarRect 12

configEdgeSouthMinusRect :: Layout -> Rect
configEdgeSouthMinusRect = configParamMinusRect 13

configEdgeSouthPlusRect :: Layout -> Rect
configEdgeSouthPlusRect = configParamPlusRect 13

configEdgeSouthBarRect :: Layout -> Rect
configEdgeSouthBarRect = configParamBarRect 13

configEdgeEastMinusRect :: Layout -> Rect
configEdgeEastMinusRect = configParamMinusRect 14

configEdgeEastPlusRect :: Layout -> Rect
configEdgeEastPlusRect = configParamPlusRect 14

configEdgeEastBarRect :: Layout -> Rect
configEdgeEastBarRect = configParamBarRect 14

configEdgeWestMinusRect :: Layout -> Rect
configEdgeWestMinusRect = configParamMinusRect 15

configEdgeWestPlusRect :: Layout -> Rect
configEdgeWestPlusRect = configParamPlusRect 15

configEdgeWestBarRect :: Layout -> Rect
configEdgeWestBarRect = configParamBarRect 15

configEdgeFalloffMinusRect :: Layout -> Rect
configEdgeFalloffMinusRect = configParamMinusRect 16

configEdgeFalloffPlusRect :: Layout -> Rect
configEdgeFalloffPlusRect = configParamPlusRect 16

configEdgeFalloffBarRect :: Layout -> Rect
configEdgeFalloffBarRect = configParamBarRect 16

configPlateSizeMinusRect :: Layout -> Rect
configPlateSizeMinusRect = configParamMinusRect 17

configPlateSizePlusRect :: Layout -> Rect
configPlateSizePlusRect = configParamPlusRect 17

configPlateSizeBarRect :: Layout -> Rect
configPlateSizeBarRect = configParamBarRect 17

configUpliftMinusRect :: Layout -> Rect
configUpliftMinusRect = configParamMinusRect 18

configUpliftPlusRect :: Layout -> Rect
configUpliftPlusRect = configParamPlusRect 18

configUpliftBarRect :: Layout -> Rect
configUpliftBarRect = configParamBarRect 18

configRiftDepthMinusRect :: Layout -> Rect
configRiftDepthMinusRect = configParamMinusRect 19

configRiftDepthPlusRect :: Layout -> Rect
configRiftDepthPlusRect = configParamPlusRect 19

configRiftDepthBarRect :: Layout -> Rect
configRiftDepthBarRect = configParamBarRect 19

configDetailScaleMinusRect :: Layout -> Rect
configDetailScaleMinusRect = configParamMinusRect 20

configDetailScalePlusRect :: Layout -> Rect
configDetailScalePlusRect = configParamPlusRect 20

configDetailScaleBarRect :: Layout -> Rect
configDetailScaleBarRect = configParamBarRect 20

configPlateSpeedMinusRect :: Layout -> Rect
configPlateSpeedMinusRect = configParamMinusRect 21

configPlateSpeedPlusRect :: Layout -> Rect
configPlateSpeedPlusRect = configParamPlusRect 21

configPlateSpeedBarRect :: Layout -> Rect
configPlateSpeedBarRect = configParamBarRect 21

configBoundarySharpnessMinusRect :: Layout -> Rect
configBoundarySharpnessMinusRect = configParamMinusRect 22

configBoundarySharpnessPlusRect :: Layout -> Rect
configBoundarySharpnessPlusRect = configParamPlusRect 22

configBoundarySharpnessBarRect :: Layout -> Rect
configBoundarySharpnessBarRect = configParamBarRect 22

configBoundaryNoiseScaleMinusRect :: Layout -> Rect
configBoundaryNoiseScaleMinusRect = configParamMinusRect 23

configBoundaryNoiseScalePlusRect :: Layout -> Rect
configBoundaryNoiseScalePlusRect = configParamPlusRect 23

configBoundaryNoiseScaleBarRect :: Layout -> Rect
configBoundaryNoiseScaleBarRect = configParamBarRect 23

configBoundaryNoiseStrengthMinusRect :: Layout -> Rect
configBoundaryNoiseStrengthMinusRect = configParamMinusRect 24

configBoundaryNoiseStrengthPlusRect :: Layout -> Rect
configBoundaryNoiseStrengthPlusRect = configParamPlusRect 24

configBoundaryNoiseStrengthBarRect :: Layout -> Rect
configBoundaryNoiseStrengthBarRect = configParamBarRect 24

configBoundaryWarpOctavesMinusRect :: Layout -> Rect
configBoundaryWarpOctavesMinusRect = configParamMinusRect 25

configBoundaryWarpOctavesPlusRect :: Layout -> Rect
configBoundaryWarpOctavesPlusRect = configParamPlusRect 25

configBoundaryWarpOctavesBarRect :: Layout -> Rect
configBoundaryWarpOctavesBarRect = configParamBarRect 25

configBoundaryWarpLacunarityMinusRect :: Layout -> Rect
configBoundaryWarpLacunarityMinusRect = configParamMinusRect 26

configBoundaryWarpLacunarityPlusRect :: Layout -> Rect
configBoundaryWarpLacunarityPlusRect = configParamPlusRect 26

configBoundaryWarpLacunarityBarRect :: Layout -> Rect
configBoundaryWarpLacunarityBarRect = configParamBarRect 26

configBoundaryWarpGainMinusRect :: Layout -> Rect
configBoundaryWarpGainMinusRect = configParamMinusRect 27

configBoundaryWarpGainPlusRect :: Layout -> Rect
configBoundaryWarpGainPlusRect = configParamPlusRect 27

configBoundaryWarpGainBarRect :: Layout -> Rect
configBoundaryWarpGainBarRect = configParamBarRect 27

configPlateMergeScaleMinusRect :: Layout -> Rect
configPlateMergeScaleMinusRect = configParamMinusRect 28

configPlateMergeScalePlusRect :: Layout -> Rect
configPlateMergeScalePlusRect = configParamPlusRect 28

configPlateMergeScaleBarRect :: Layout -> Rect
configPlateMergeScaleBarRect = configParamBarRect 28

configPlateMergeBiasMinusRect :: Layout -> Rect
configPlateMergeBiasMinusRect = configParamMinusRect 29

configPlateMergeBiasPlusRect :: Layout -> Rect
configPlateMergeBiasPlusRect = configParamPlusRect 29

configPlateMergeBiasBarRect :: Layout -> Rect
configPlateMergeBiasBarRect = configParamBarRect 29

configPlateDetailScaleMinusRect :: Layout -> Rect
configPlateDetailScaleMinusRect = configParamMinusRect 30

configPlateDetailScalePlusRect :: Layout -> Rect
configPlateDetailScalePlusRect = configParamPlusRect 30

configPlateDetailScaleBarRect :: Layout -> Rect
configPlateDetailScaleBarRect = configParamBarRect 30

configPlateDetailStrengthMinusRect :: Layout -> Rect
configPlateDetailStrengthMinusRect = configParamMinusRect 31

configPlateDetailStrengthPlusRect :: Layout -> Rect
configPlateDetailStrengthPlusRect = configParamPlusRect 31

configPlateDetailStrengthBarRect :: Layout -> Rect
configPlateDetailStrengthBarRect = configParamBarRect 31

configPlateRidgeStrengthMinusRect :: Layout -> Rect
configPlateRidgeStrengthMinusRect = configParamMinusRect 32

configPlateRidgeStrengthPlusRect :: Layout -> Rect
configPlateRidgeStrengthPlusRect = configParamPlusRect 32

configPlateRidgeStrengthBarRect :: Layout -> Rect
configPlateRidgeStrengthBarRect = configParamBarRect 32

configPlateHeightBaseMinusRect :: Layout -> Rect
configPlateHeightBaseMinusRect = configParamMinusRect 33

configPlateHeightBasePlusRect :: Layout -> Rect
configPlateHeightBasePlusRect = configParamPlusRect 33

configPlateHeightBaseBarRect :: Layout -> Rect
configPlateHeightBaseBarRect = configParamBarRect 33

configPlateHeightVarianceMinusRect :: Layout -> Rect
configPlateHeightVarianceMinusRect = configParamMinusRect 34

configPlateHeightVariancePlusRect :: Layout -> Rect
configPlateHeightVariancePlusRect = configParamPlusRect 34

configPlateHeightVarianceBarRect :: Layout -> Rect
configPlateHeightVarianceBarRect = configParamBarRect 34

configPlateHardnessBaseMinusRect :: Layout -> Rect
configPlateHardnessBaseMinusRect = configParamMinusRect 35

configPlateHardnessBasePlusRect :: Layout -> Rect
configPlateHardnessBasePlusRect = configParamPlusRect 35

configPlateHardnessBaseBarRect :: Layout -> Rect
configPlateHardnessBaseBarRect = configParamBarRect 35

configPlateHardnessVarianceMinusRect :: Layout -> Rect
configPlateHardnessVarianceMinusRect = configParamMinusRect 36

configPlateHardnessVariancePlusRect :: Layout -> Rect
configPlateHardnessVariancePlusRect = configParamPlusRect 36

configPlateHardnessVarianceBarRect :: Layout -> Rect
configPlateHardnessVarianceBarRect = configParamBarRect 36

configTrenchDepthMinusRect :: Layout -> Rect
configTrenchDepthMinusRect = configParamMinusRect 37

configTrenchDepthPlusRect :: Layout -> Rect
configTrenchDepthPlusRect = configParamPlusRect 37

configTrenchDepthBarRect :: Layout -> Rect
configTrenchDepthBarRect = configParamBarRect 37

configRidgeHeightMinusRect :: Layout -> Rect
configRidgeHeightMinusRect = configParamMinusRect 38

configRidgeHeightPlusRect :: Layout -> Rect
configRidgeHeightPlusRect = configParamPlusRect 38

configRidgeHeightBarRect :: Layout -> Rect
configRidgeHeightBarRect = configParamBarRect 38

configPlateBiasStrengthMinusRect :: Layout -> Rect
configPlateBiasStrengthMinusRect = configParamMinusRect 39

configPlateBiasStrengthPlusRect :: Layout -> Rect
configPlateBiasStrengthPlusRect = configParamPlusRect 39

configPlateBiasStrengthBarRect :: Layout -> Rect
configPlateBiasStrengthBarRect = configParamBarRect 39

configPlateBiasCenterMinusRect :: Layout -> Rect
configPlateBiasCenterMinusRect = configParamMinusRect 40

configPlateBiasCenterPlusRect :: Layout -> Rect
configPlateBiasCenterPlusRect = configParamPlusRect 40

configPlateBiasCenterBarRect :: Layout -> Rect
configPlateBiasCenterBarRect = configParamBarRect 40

configPlateBiasEdgeMinusRect :: Layout -> Rect
configPlateBiasEdgeMinusRect = configParamMinusRect 41

configPlateBiasEdgePlusRect :: Layout -> Rect
configPlateBiasEdgePlusRect = configParamPlusRect 41

configPlateBiasEdgeBarRect :: Layout -> Rect
configPlateBiasEdgeBarRect = configParamBarRect 41

configPlateBiasNorthMinusRect :: Layout -> Rect
configPlateBiasNorthMinusRect = configParamMinusRect 42

configPlateBiasNorthPlusRect :: Layout -> Rect
configPlateBiasNorthPlusRect = configParamPlusRect 42

configPlateBiasNorthBarRect :: Layout -> Rect
configPlateBiasNorthBarRect = configParamBarRect 42

configPlateBiasSouthMinusRect :: Layout -> Rect
configPlateBiasSouthMinusRect = configParamMinusRect 43

configPlateBiasSouthPlusRect :: Layout -> Rect
configPlateBiasSouthPlusRect = configParamPlusRect 43

configPlateBiasSouthBarRect :: Layout -> Rect
configPlateBiasSouthBarRect = configParamBarRect 43

configTfcCliffSlopeMinusRect :: Layout -> Rect
configTfcCliffSlopeMinusRect = configParamMinusRect 44

configTfcCliffSlopePlusRect :: Layout -> Rect
configTfcCliffSlopePlusRect = configParamPlusRect 44

configTfcCliffSlopeBarRect :: Layout -> Rect
configTfcCliffSlopeBarRect = configParamBarRect 44

configTfcMountainSlopeMinusRect :: Layout -> Rect
configTfcMountainSlopeMinusRect = configParamMinusRect 45

configTfcMountainSlopePlusRect :: Layout -> Rect
configTfcMountainSlopePlusRect = configParamPlusRect 45

configTfcMountainSlopeBarRect :: Layout -> Rect
configTfcMountainSlopeBarRect = configParamBarRect 45

configTfcMountainReliefMinusRect :: Layout -> Rect
configTfcMountainReliefMinusRect = configParamMinusRect 46

configTfcMountainReliefPlusRect :: Layout -> Rect
configTfcMountainReliefPlusRect = configParamPlusRect 46

configTfcMountainReliefBarRect :: Layout -> Rect
configTfcMountainReliefBarRect = configParamBarRect 46

configTfcHillSlopeMinusRect :: Layout -> Rect
configTfcHillSlopeMinusRect = configParamMinusRect 47

configTfcHillSlopePlusRect :: Layout -> Rect
configTfcHillSlopePlusRect = configParamPlusRect 47

configTfcHillSlopeBarRect :: Layout -> Rect
configTfcHillSlopeBarRect = configParamBarRect 47

configTfcRollingSlopeMinusRect :: Layout -> Rect
configTfcRollingSlopeMinusRect = configParamMinusRect 48

configTfcRollingSlopePlusRect :: Layout -> Rect
configTfcRollingSlopePlusRect = configParamPlusRect 48

configTfcRollingSlopeBarRect :: Layout -> Rect
configTfcRollingSlopeBarRect = configParamBarRect 48

configValleyCurvatureMinusRect :: Layout -> Rect
configValleyCurvatureMinusRect = configParamMinusRect 49

configValleyCurvaturePlusRect :: Layout -> Rect
configValleyCurvaturePlusRect = configParamPlusRect 49

configValleyCurvatureBarRect :: Layout -> Rect
configValleyCurvatureBarRect = configParamBarRect 49

configRockElevationThresholdMinusRect :: Layout -> Rect
configRockElevationThresholdMinusRect = configParamMinusRect 50

configRockElevationThresholdPlusRect :: Layout -> Rect
configRockElevationThresholdPlusRect = configParamPlusRect 50

configRockElevationThresholdBarRect :: Layout -> Rect
configRockElevationThresholdBarRect = configParamBarRect 50

configRockHardnessThresholdMinusRect :: Layout -> Rect
configRockHardnessThresholdMinusRect = configParamMinusRect 51

configRockHardnessThresholdPlusRect :: Layout -> Rect
configRockHardnessThresholdPlusRect = configParamPlusRect 51

configRockHardnessThresholdBarRect :: Layout -> Rect
configRockHardnessThresholdBarRect = configParamBarRect 51

configRockHardnessSecondaryMinusRect :: Layout -> Rect
configRockHardnessSecondaryMinusRect = configParamMinusRect 52

configRockHardnessSecondaryPlusRect :: Layout -> Rect
configRockHardnessSecondaryPlusRect = configParamPlusRect 52

configRockHardnessSecondaryBarRect :: Layout -> Rect
configRockHardnessSecondaryBarRect = configParamBarRect 52

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
