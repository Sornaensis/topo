{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Config
  ( drawConfigPanel
  , drawConfigTabs
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.UI (ConfigTab(..), UiState(..), configRowCount)
import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Topo.Pipeline.Stage (allBuiltinStageIds)
import UI.Layout (configRowTopPad)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline) = do
  drawTab tabTerrain (uiConfigTab ui == ConfigTerrain)
  drawTab tabPlanet (uiConfigTab ui == ConfigPlanet)
  drawTab tabClimate (uiConfigTab ui == ConfigClimate)
  drawTab tabWeather (uiConfigTab ui == ConfigWeather)
  drawTab tabBiome (uiConfigTab ui == ConfigBiome)
  drawTab tabErosion (uiConfigTab ui == ConfigErosion)
  drawTab tabPipeline (uiConfigTab ui == ConfigPipeline)
  where
    drawTab rect isActive = do
      let fill = if isActive then V4 70 90 120 255 else V4 50 60 75 255
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

drawConfigPanel renderer ui dataSnap rect tabs presetSaveRect presetLoadRect resetRect revertRect scrollAreaRect scrollBarRect
  waterRects orographicLiftRects rainShadowLossRects windDiffuseRects equatorTempRects poleTempRects lapseRateRects windIterationsRects moistureIterationsRects
  weatherTickRects weatherPhaseRects weatherAmplitudeRects seasonCycleLengthRects jitterAmplitudeRects pressureBaseRects pressureTempScaleRects pressureCoriolisScaleRects
  seasonalBaseRects seasonalRangeRects humidityNoiseScaleRects precipNoiseScaleRects weatherITCZWidthRects weatherITCZPrecipBoostRects pressureHumidityScaleRects
  pressureGradientWindScaleRects windNoiseScaleRects itczMigrationScaleRects cloudRHExponentRects cloudAlbedoEffectRects cloudPrecipBoostRects vegBaseRects vegBoostRects
  vegTempWeightRects vegPrecipWeightRects btCoastalBandRects btSnowMaxTempRects btAlpineMaxTempRects btIceCapTempRects btMontaneMaxTempRects btMontanePrecipRects
  btCliffSlopeRects btValleyMoistureRects btDepressionMoistureRects btPrecipWeightRects vbcTempMinRects vbcTempRangeRects vbcFertilityBoostRects vbcAlbedoBaseRects
  vbcAlbedoBareRects vbcAlbedoVegRects vbcOceanAlbedoRects vbcIceAlbedoRects biomeSmoothingRects volcanicAshBoostRects volcanicLavaPenaltyRects biomeFeedbackBlendRects
  boundaryMotionTempRects boundaryMotionPrecipRects planetRadiusRects axialTiltRects insolationRects occWarmScaleRects occColdScaleRects occLatPeakDegRects occLatWidthDegRects
  sliceLatCenterRects sliceLonCenterRects latitudeExponentRects plateHeightCoolingRects tempNoiseScaleRects oceanModerationRects oceanModerateTempRects albedoSensitivityRects
  albedoReferenceRects moistAdvectRects moistLocalRects moistWindEvapScaleRects moistEvapNoiseScaleRects moistBareEvapFracRects moistVegTranspFracRects moistWindETScaleRects
  moistCondensationRateRects moistRecycleRateRects moistITCZStrengthRects moistITCZWidthRects orographicScaleRects orographicStepRects coastalIterationsRects coastalDiffuseRects
  coastalMoistureBoostRects windBeltStrengthRects windBeltHarmonicsRects windBeltBaseRects windBeltRangeRects windBeltSpeedScaleRects bndLandRangeRects piedmontSmoothRects
  piedmontSlopeMinRects piedmontSlopeMaxRects windCoriolisDeflectionRects moistMinVegFloorRects genScaleRects genCoordScaleRects genOffsetXRects genOffsetYRects genFrequencyRects
  genOctavesRects genLacunarityRects genGainRects genWarpScaleRects genWarpStrengthRects extentXRects extentYRects edgeNorthRects edgeSouthRects edgeEastRects edgeWestRects
  edgeFalloffRects plateSizeRects upliftRects riftDepthRects detailScaleRects plateSpeedRects boundarySharpnessRects boundaryNoiseScaleRects boundaryNoiseStrengthRects
  boundaryWarpOctavesRects boundaryWarpLacunarityRects boundaryWarpGainRects plateMergeScaleRects plateMergeBiasRects plateDetailScaleRects plateDetailStrengthRects
  plateRidgeStrengthRects plateHeightBaseRects plateHeightVarianceRects plateHardnessBaseRects plateHardnessVarianceRects trenchDepthRects ridgeHeightRects plateBiasStrengthRects
  plateBiasCenterRects plateBiasEdgeRects plateBiasNorthRects plateBiasSouthRects tfcCliffSlopeRects tfcMountainSlopeRects tfcMountainReliefRects tfcHillSlopeRects
  tfcRollingSlopeRects valleyCurvatureRects tfcElevGradientRects tfcPlateauMaxRelief2RingRects rockElevationThresholdRects rockHardnessThresholdRects rockHardnessSecondaryRects
  erosionHydraulicRects erosionThermalRects erosionRainRateRects erosionTalusRects erosionMaxDropRects erosionHydDepositRects erosionDepositSlopeRects erosionThermDepositRects
  erosionCoastZoneRects erosionCoastStrengthRects erosionCoastIterRects hypsometryEnabledRects hypsometryLowlandExpRects hypsometryHighlandExpRects hypsometryPlateauBreakRects
  hypsometryOceanExpRects hypsometryCoastalRampWidthRects hypsometryCoastalRampStrRects glacierSnowTempRects glacierSnowRangeRects glacierMeltTempRects glacierMeltRateRects
  glacierAccumScaleRects glacierFlowItersRects glacierFlowRateRects glacierErosionScaleRects glacierCarveScaleRects glacierDepositScaleRects ventDensityRects ventThresholdRects
  hotspotScaleRects hotspotThresholdRects magmaRechargeRects lavaScaleRects ashScaleRects volcanicDepositScaleRects soilMoistureThresholdRects soilHardnessThresholdRects
  soilFertilityMoistWeightRects soilFertilityDepthWeightRects sinkBreachDepthRects streamPowerMaxErosionRects riverCarveMaxDepthRects coastalErodeStrengthRects hydroHardnessWeightRects
  minLakeSizeRects inlandSeaMinSizeRects roughnessScaleRects =
  let (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline) = tabs
      (waterMinus, waterBar, waterPlus) = waterRects
      (orographicLiftMinus, orographicLiftBar, orographicLiftPlus) = orographicLiftRects
      (rainShadowLossMinus, rainShadowLossBar, rainShadowLossPlus) = rainShadowLossRects
      (windDiffuseMinus, windDiffuseBar, windDiffusePlus) = windDiffuseRects
      (equatorTempMinus, equatorTempBar, equatorTempPlus) = equatorTempRects
      (poleTempMinus, poleTempBar, poleTempPlus) = poleTempRects
      (lapseRateMinus, lapseRateBar, lapseRatePlus) = lapseRateRects
      (windIterationsMinus, windIterationsBar, windIterationsPlus) = windIterationsRects
      (moistureIterationsMinus, moistureIterationsBar, moistureIterationsPlus) = moistureIterationsRects
      (weatherTickMinus, weatherTickBar, weatherTickPlus) = weatherTickRects
      (weatherPhaseMinus, weatherPhaseBar, weatherPhasePlus) = weatherPhaseRects
      (weatherAmplitudeMinus, weatherAmplitudeBar, weatherAmplitudePlus) = weatherAmplitudeRects
      (seasonCycleLengthMinus, seasonCycleLengthBar, seasonCycleLengthPlus) = seasonCycleLengthRects
      (jitterAmplitudeMinus, jitterAmplitudeBar, jitterAmplitudePlus) = jitterAmplitudeRects
      (pressureBaseMinus, pressureBaseBar, pressureBasePlus) = pressureBaseRects
      (pressureTempScaleMinus, pressureTempScaleBar, pressureTempScalePlus) = pressureTempScaleRects
      (pressureCoriolisScaleMinus, pressureCoriolisScaleBar, pressureCoriolisScalePlus) = pressureCoriolisScaleRects
      (seasonalBaseMinus, seasonalBaseBar, seasonalBasePlus) = seasonalBaseRects
      (seasonalRangeMinus, seasonalRangeBar, seasonalRangePlus) = seasonalRangeRects
      (humidityNoiseScaleMinus, humidityNoiseScaleBar, humidityNoiseScalePlus) = humidityNoiseScaleRects
      (precipNoiseScaleMinus, precipNoiseScaleBar, precipNoiseScalePlus) = precipNoiseScaleRects
      (weatherITCZWidthMinus, weatherITCZWidthBar, weatherITCZWidthPlus) = weatherITCZWidthRects
      (weatherITCZPrecipBoostMinus, weatherITCZPrecipBoostBar, weatherITCZPrecipBoostPlus) = weatherITCZPrecipBoostRects
      (pressureHumidityScaleMinus, pressureHumidityScaleBar, pressureHumidityScalePlus) = pressureHumidityScaleRects
      (pressureGradientWindScaleMinus, pressureGradientWindScaleBar, pressureGradientWindScalePlus) = pressureGradientWindScaleRects
      (windNoiseScaleMinus, windNoiseScaleBar, windNoiseScalePlus) = windNoiseScaleRects
      (itczMigrationScaleMinus, itczMigrationScaleBar, itczMigrationScalePlus) = itczMigrationScaleRects
      (cloudRHExponentMinus, cloudRHExponentBar, cloudRHExponentPlus) = cloudRHExponentRects
      (cloudAlbedoEffectMinus, cloudAlbedoEffectBar, cloudAlbedoEffectPlus) = cloudAlbedoEffectRects
      (cloudPrecipBoostMinus, cloudPrecipBoostBar, cloudPrecipBoostPlus) = cloudPrecipBoostRects
      (vegBaseMinus, vegBaseBar, vegBasePlus) = vegBaseRects
      (vegBoostMinus, vegBoostBar, vegBoostPlus) = vegBoostRects
      (vegTempWeightMinus, vegTempWeightBar, vegTempWeightPlus) = vegTempWeightRects
      (vegPrecipWeightMinus, vegPrecipWeightBar, vegPrecipWeightPlus) = vegPrecipWeightRects
      (btCoastalBandMinus, btCoastalBandBar, btCoastalBandPlus) = btCoastalBandRects
      (btSnowMaxTempMinus, btSnowMaxTempBar, btSnowMaxTempPlus) = btSnowMaxTempRects
      (btAlpineMaxTempMinus, btAlpineMaxTempBar, btAlpineMaxTempPlus) = btAlpineMaxTempRects
      (btIceCapTempMinus, btIceCapTempBar, btIceCapTempPlus) = btIceCapTempRects
      (btMontaneMaxTempMinus, btMontaneMaxTempBar, btMontaneMaxTempPlus) = btMontaneMaxTempRects
      (btMontanePrecipMinus, btMontanePrecipBar, btMontanePrecipPlus) = btMontanePrecipRects
      (btCliffSlopeMinus, btCliffSlopeBar, btCliffSlopePlus) = btCliffSlopeRects
      (btValleyMoistureMinus, btValleyMoistureBar, btValleyMoisturePlus) = btValleyMoistureRects
      (btDepressionMoistureMinus, btDepressionMoistureBar, btDepressionMoisturePlus) = btDepressionMoistureRects
      (btPrecipWeightMinus, btPrecipWeightBar, btPrecipWeightPlus) = btPrecipWeightRects
      (vbcTempMinMinus, vbcTempMinBar, vbcTempMinPlus) = vbcTempMinRects
      (vbcTempRangeMinus, vbcTempRangeBar, vbcTempRangePlus) = vbcTempRangeRects
      (vbcFertilityBoostMinus, vbcFertilityBoostBar, vbcFertilityBoostPlus) = vbcFertilityBoostRects
      (vbcAlbedoBaseMinus, vbcAlbedoBaseBar, vbcAlbedoBasePlus) = vbcAlbedoBaseRects
      (vbcAlbedoBareMinus, vbcAlbedoBareBar, vbcAlbedoBarePlus) = vbcAlbedoBareRects
      (vbcAlbedoVegMinus, vbcAlbedoVegBar, vbcAlbedoVegPlus) = vbcAlbedoVegRects
      (vbcOceanAlbedoMinus, vbcOceanAlbedoBar, vbcOceanAlbedoPlus) = vbcOceanAlbedoRects
      (vbcIceAlbedoMinus, vbcIceAlbedoBar, vbcIceAlbedoPlus) = vbcIceAlbedoRects
      (biomeSmoothingMinus, biomeSmoothingBar, biomeSmoothingPlus) = biomeSmoothingRects
      (volcanicAshBoostMinus, volcanicAshBoostBar, volcanicAshBoostPlus) = volcanicAshBoostRects
      (volcanicLavaPenaltyMinus, volcanicLavaPenaltyBar, volcanicLavaPenaltyPlus) = volcanicLavaPenaltyRects
      (biomeFeedbackBlendMinus, biomeFeedbackBlendBar, biomeFeedbackBlendPlus) = biomeFeedbackBlendRects
      (boundaryMotionTempMinus, boundaryMotionTempBar, boundaryMotionTempPlus) = boundaryMotionTempRects
      (boundaryMotionPrecipMinus, boundaryMotionPrecipBar, boundaryMotionPrecipPlus) = boundaryMotionPrecipRects
      (planetRadiusMinus, planetRadiusBar, planetRadiusPlus) = planetRadiusRects
      (axialTiltMinus, axialTiltBar, axialTiltPlus) = axialTiltRects
      (insolationMinus, insolationBar, insolationPlus) = insolationRects
      (occWarmScaleMinus, occWarmScaleBar, occWarmScalePlus) = occWarmScaleRects
      (occColdScaleMinus, occColdScaleBar, occColdScalePlus) = occColdScaleRects
      (occLatPeakDegMinus, occLatPeakDegBar, occLatPeakDegPlus) = occLatPeakDegRects
      (occLatWidthDegMinus, occLatWidthDegBar, occLatWidthDegPlus) = occLatWidthDegRects
      (sliceLatCenterMinus, sliceLatCenterBar, sliceLatCenterPlus) = sliceLatCenterRects
      (sliceLonCenterMinus, sliceLonCenterBar, sliceLonCenterPlus) = sliceLonCenterRects
      (latitudeExponentMinus, latitudeExponentBar, latitudeExponentPlus) = latitudeExponentRects
      (plateHeightCoolingMinus, plateHeightCoolingBar, plateHeightCoolingPlus) = plateHeightCoolingRects
      (tempNoiseScaleMinus, tempNoiseScaleBar, tempNoiseScalePlus) = tempNoiseScaleRects
      (oceanModerationMinus, oceanModerationBar, oceanModerationPlus) = oceanModerationRects
      (oceanModerateTempMinus, oceanModerateTempBar, oceanModerateTempPlus) = oceanModerateTempRects
      (albedoSensitivityMinus, albedoSensitivityBar, albedoSensitivityPlus) = albedoSensitivityRects
      (albedoReferenceMinus, albedoReferenceBar, albedoReferencePlus) = albedoReferenceRects
      (moistAdvectMinus, moistAdvectBar, moistAdvectPlus) = moistAdvectRects
      (moistLocalMinus, moistLocalBar, moistLocalPlus) = moistLocalRects
      (moistWindEvapScaleMinus, moistWindEvapScaleBar, moistWindEvapScalePlus) = moistWindEvapScaleRects
      (moistEvapNoiseScaleMinus, moistEvapNoiseScaleBar, moistEvapNoiseScalePlus) = moistEvapNoiseScaleRects
      (moistBareEvapFracMinus, moistBareEvapFracBar, moistBareEvapFracPlus) = moistBareEvapFracRects
      (moistVegTranspFracMinus, moistVegTranspFracBar, moistVegTranspFracPlus) = moistVegTranspFracRects
      (moistWindETScaleMinus, moistWindETScaleBar, moistWindETScalePlus) = moistWindETScaleRects
      (moistCondensationRateMinus, moistCondensationRateBar, moistCondensationRatePlus) = moistCondensationRateRects
      (moistRecycleRateMinus, moistRecycleRateBar, moistRecycleRatePlus) = moistRecycleRateRects
      (moistITCZStrengthMinus, moistITCZStrengthBar, moistITCZStrengthPlus) = moistITCZStrengthRects
      (moistITCZWidthMinus, moistITCZWidthBar, moistITCZWidthPlus) = moistITCZWidthRects
      (orographicScaleMinus, orographicScaleBar, orographicScalePlus) = orographicScaleRects
      (orographicStepMinus, orographicStepBar, orographicStepPlus) = orographicStepRects
      (coastalIterationsMinus, coastalIterationsBar, coastalIterationsPlus) = coastalIterationsRects
      (coastalDiffuseMinus, coastalDiffuseBar, coastalDiffusePlus) = coastalDiffuseRects
      (coastalMoistureBoostMinus, coastalMoistureBoostBar, coastalMoistureBoostPlus) = coastalMoistureBoostRects
      (windBeltStrengthMinus, windBeltStrengthBar, windBeltStrengthPlus) = windBeltStrengthRects
      (windBeltHarmonicsMinus, windBeltHarmonicsBar, windBeltHarmonicsPlus) = windBeltHarmonicsRects
      (windBeltBaseMinus, windBeltBaseBar, windBeltBasePlus) = windBeltBaseRects
      (windBeltRangeMinus, windBeltRangeBar, windBeltRangePlus) = windBeltRangeRects
      (windBeltSpeedScaleMinus, windBeltSpeedScaleBar, windBeltSpeedScalePlus) = windBeltSpeedScaleRects
      (bndLandRangeMinus, bndLandRangeBar, bndLandRangePlus) = bndLandRangeRects
      (piedmontSmoothMinus, piedmontSmoothBar, piedmontSmoothPlus) = piedmontSmoothRects
      (piedmontSlopeMinMinus, piedmontSlopeMinBar, piedmontSlopeMinPlus) = piedmontSlopeMinRects
      (piedmontSlopeMaxMinus, piedmontSlopeMaxBar, piedmontSlopeMaxPlus) = piedmontSlopeMaxRects
      (windCoriolisDeflectionMinus, windCoriolisDeflectionBar, windCoriolisDeflectionPlus) = windCoriolisDeflectionRects
      (moistMinVegFloorMinus, moistMinVegFloorBar, moistMinVegFloorPlus) = moistMinVegFloorRects
      (genScaleMinus, genScaleBar, genScalePlus) = genScaleRects
      (genCoordScaleMinus, genCoordScaleBar, genCoordScalePlus) = genCoordScaleRects
      (genOffsetXMinus, genOffsetXBar, genOffsetXPlus) = genOffsetXRects
      (genOffsetYMinus, genOffsetYBar, genOffsetYPlus) = genOffsetYRects
      (genFrequencyMinus, genFrequencyBar, genFrequencyPlus) = genFrequencyRects
      (genOctavesMinus, genOctavesBar, genOctavesPlus) = genOctavesRects
      (genLacunarityMinus, genLacunarityBar, genLacunarityPlus) = genLacunarityRects
      (genGainMinus, genGainBar, genGainPlus) = genGainRects
      (genWarpScaleMinus, genWarpScaleBar, genWarpScalePlus) = genWarpScaleRects
      (genWarpStrengthMinus, genWarpStrengthBar, genWarpStrengthPlus) = genWarpStrengthRects
      (extentXMinus, extentXBar, extentXPlus) = extentXRects
      (extentYMinus, extentYBar, extentYPlus) = extentYRects
      (edgeNorthMinus, edgeNorthBar, edgeNorthPlus) = edgeNorthRects
      (edgeSouthMinus, edgeSouthBar, edgeSouthPlus) = edgeSouthRects
      (edgeEastMinus, edgeEastBar, edgeEastPlus) = edgeEastRects
      (edgeWestMinus, edgeWestBar, edgeWestPlus) = edgeWestRects
      (edgeFalloffMinus, edgeFalloffBar, edgeFalloffPlus) = edgeFalloffRects
      (plateSizeMinus, plateSizeBar, plateSizePlus) = plateSizeRects
      (upliftMinus, upliftBar, upliftPlus) = upliftRects
      (riftDepthMinus, riftDepthBar, riftDepthPlus) = riftDepthRects
      (detailScaleMinus, detailScaleBar, detailScalePlus) = detailScaleRects
      (plateSpeedMinus, plateSpeedBar, plateSpeedPlus) = plateSpeedRects
      (boundarySharpnessMinus, boundarySharpnessBar, boundarySharpnessPlus) = boundarySharpnessRects
      (boundaryNoiseScaleMinus, boundaryNoiseScaleBar, boundaryNoiseScalePlus) = boundaryNoiseScaleRects
      (boundaryNoiseStrengthMinus, boundaryNoiseStrengthBar, boundaryNoiseStrengthPlus) = boundaryNoiseStrengthRects
      (boundaryWarpOctavesMinus, boundaryWarpOctavesBar, boundaryWarpOctavesPlus) = boundaryWarpOctavesRects
      (boundaryWarpLacunarityMinus, boundaryWarpLacunarityBar, boundaryWarpLacunarityPlus) = boundaryWarpLacunarityRects
      (boundaryWarpGainMinus, boundaryWarpGainBar, boundaryWarpGainPlus) = boundaryWarpGainRects
      (plateMergeScaleMinus, plateMergeScaleBar, plateMergeScalePlus) = plateMergeScaleRects
      (plateMergeBiasMinus, plateMergeBiasBar, plateMergeBiasPlus) = plateMergeBiasRects
      (plateDetailScaleMinus, plateDetailScaleBar, plateDetailScalePlus) = plateDetailScaleRects
      (plateDetailStrengthMinus, plateDetailStrengthBar, plateDetailStrengthPlus) = plateDetailStrengthRects
      (plateRidgeStrengthMinus, plateRidgeStrengthBar, plateRidgeStrengthPlus) = plateRidgeStrengthRects
      (plateHeightBaseMinus, plateHeightBaseBar, plateHeightBasePlus) = plateHeightBaseRects
      (plateHeightVarianceMinus, plateHeightVarianceBar, plateHeightVariancePlus) = plateHeightVarianceRects
      (plateHardnessBaseMinus, plateHardnessBaseBar, plateHardnessBasePlus) = plateHardnessBaseRects
      (plateHardnessVarianceMinus, plateHardnessVarianceBar, plateHardnessVariancePlus) = plateHardnessVarianceRects
      (trenchDepthMinus, trenchDepthBar, trenchDepthPlus) = trenchDepthRects
      (ridgeHeightMinus, ridgeHeightBar, ridgeHeightPlus) = ridgeHeightRects
      (plateBiasStrengthMinus, plateBiasStrengthBar, plateBiasStrengthPlus) = plateBiasStrengthRects
      (plateBiasCenterMinus, plateBiasCenterBar, plateBiasCenterPlus) = plateBiasCenterRects
      (plateBiasEdgeMinus, plateBiasEdgeBar, plateBiasEdgePlus) = plateBiasEdgeRects
      (plateBiasNorthMinus, plateBiasNorthBar, plateBiasNorthPlus) = plateBiasNorthRects
      (plateBiasSouthMinus, plateBiasSouthBar, plateBiasSouthPlus) = plateBiasSouthRects
      (tfcCliffSlopeMinus, tfcCliffSlopeBar, tfcCliffSlopePlus) = tfcCliffSlopeRects
      (tfcMountainSlopeMinus, tfcMountainSlopeBar, tfcMountainSlopePlus) = tfcMountainSlopeRects
      (tfcMountainReliefMinus, tfcMountainReliefBar, tfcMountainReliefPlus) = tfcMountainReliefRects
      (tfcHillSlopeMinus, tfcHillSlopeBar, tfcHillSlopePlus) = tfcHillSlopeRects
      (tfcRollingSlopeMinus, tfcRollingSlopeBar, tfcRollingSlopePlus) = tfcRollingSlopeRects
      (valleyCurvatureMinus, valleyCurvatureBar, valleyCurvaturePlus) = valleyCurvatureRects
      (tfcElevGradientMinus, tfcElevGradientBar, tfcElevGradientPlus) = tfcElevGradientRects
      (tfcPlateauMaxRelief2RingMinus, tfcPlateauMaxRelief2RingBar, tfcPlateauMaxRelief2RingPlus) = tfcPlateauMaxRelief2RingRects
      (rockElevationThresholdMinus, rockElevationThresholdBar, rockElevationThresholdPlus) = rockElevationThresholdRects
      (rockHardnessThresholdMinus, rockHardnessThresholdBar, rockHardnessThresholdPlus) = rockHardnessThresholdRects
      (rockHardnessSecondaryMinus, rockHardnessSecondaryBar, rockHardnessSecondaryPlus) = rockHardnessSecondaryRects
      (erosionHydraulicMinus, erosionHydraulicBar, erosionHydraulicPlus) = erosionHydraulicRects
      (erosionThermalMinus, erosionThermalBar, erosionThermalPlus) = erosionThermalRects
      (erosionRainRateMinus, erosionRainRateBar, erosionRainRatePlus) = erosionRainRateRects
      (erosionTalusMinus, erosionTalusBar, erosionTalusPlus) = erosionTalusRects
      (erosionMaxDropMinus, erosionMaxDropBar, erosionMaxDropPlus) = erosionMaxDropRects
      (erosionHydDepositMinus, erosionHydDepositBar, erosionHydDepositPlus) = erosionHydDepositRects
      (erosionDepositSlopeMinus, erosionDepositSlopeBar, erosionDepositSlopePlus) = erosionDepositSlopeRects
      (erosionThermDepositMinus, erosionThermDepositBar, erosionThermDepositPlus) = erosionThermDepositRects
      (erosionCoastZoneMinus, erosionCoastZoneBar, erosionCoastZonePlus) = erosionCoastZoneRects
      (erosionCoastStrengthMinus, erosionCoastStrengthBar, erosionCoastStrengthPlus) = erosionCoastStrengthRects
      (erosionCoastIterMinus, erosionCoastIterBar, erosionCoastIterPlus) = erosionCoastIterRects
      (hypsometryEnabledMinus, hypsometryEnabledBar, hypsometryEnabledPlus) = hypsometryEnabledRects
      (hypsometryLowlandExpMinus, hypsometryLowlandExpBar, hypsometryLowlandExpPlus) = hypsometryLowlandExpRects
      (hypsometryHighlandExpMinus, hypsometryHighlandExpBar, hypsometryHighlandExpPlus) = hypsometryHighlandExpRects
      (hypsometryPlateauBreakMinus, hypsometryPlateauBreakBar, hypsometryPlateauBreakPlus) = hypsometryPlateauBreakRects
      (hypsometryOceanExpMinus, hypsometryOceanExpBar, hypsometryOceanExpPlus) = hypsometryOceanExpRects
      (hypsometryCoastalRampWidthMinus, hypsometryCoastalRampWidthBar, hypsometryCoastalRampWidthPlus) = hypsometryCoastalRampWidthRects
      (hypsometryCoastalRampStrMinus, hypsometryCoastalRampStrBar, hypsometryCoastalRampStrPlus) = hypsometryCoastalRampStrRects
      (glacierSnowTempMinus, glacierSnowTempBar, glacierSnowTempPlus) = glacierSnowTempRects
      (glacierSnowRangeMinus, glacierSnowRangeBar, glacierSnowRangePlus) = glacierSnowRangeRects
      (glacierMeltTempMinus, glacierMeltTempBar, glacierMeltTempPlus) = glacierMeltTempRects
      (glacierMeltRateMinus, glacierMeltRateBar, glacierMeltRatePlus) = glacierMeltRateRects
      (glacierAccumScaleMinus, glacierAccumScaleBar, glacierAccumScalePlus) = glacierAccumScaleRects
      (glacierFlowItersMinus, glacierFlowItersBar, glacierFlowItersPlus) = glacierFlowItersRects
      (glacierFlowRateMinus, glacierFlowRateBar, glacierFlowRatePlus) = glacierFlowRateRects
      (glacierErosionScaleMinus, glacierErosionScaleBar, glacierErosionScalePlus) = glacierErosionScaleRects
      (glacierCarveScaleMinus, glacierCarveScaleBar, glacierCarveScalePlus) = glacierCarveScaleRects
      (glacierDepositScaleMinus, glacierDepositScaleBar, glacierDepositScalePlus) = glacierDepositScaleRects
      (ventDensityMinus, ventDensityBar, ventDensityPlus) = ventDensityRects
      (ventThresholdMinus, ventThresholdBar, ventThresholdPlus) = ventThresholdRects
      (hotspotScaleMinus, hotspotScaleBar, hotspotScalePlus) = hotspotScaleRects
      (hotspotThresholdMinus, hotspotThresholdBar, hotspotThresholdPlus) = hotspotThresholdRects
      (magmaRechargeMinus, magmaRechargeBar, magmaRechargePlus) = magmaRechargeRects
      (lavaScaleMinus, lavaScaleBar, lavaScalePlus) = lavaScaleRects
      (ashScaleMinus, ashScaleBar, ashScalePlus) = ashScaleRects
      (volcanicDepositScaleMinus, volcanicDepositScaleBar, volcanicDepositScalePlus) = volcanicDepositScaleRects
      (soilMoistureThresholdMinus, soilMoistureThresholdBar, soilMoistureThresholdPlus) = soilMoistureThresholdRects
      (soilHardnessThresholdMinus, soilHardnessThresholdBar, soilHardnessThresholdPlus) = soilHardnessThresholdRects
      (soilFertilityMoistWeightMinus, soilFertilityMoistWeightBar, soilFertilityMoistWeightPlus) = soilFertilityMoistWeightRects
      (soilFertilityDepthWeightMinus, soilFertilityDepthWeightBar, soilFertilityDepthWeightPlus) = soilFertilityDepthWeightRects
      (sinkBreachDepthMinus, sinkBreachDepthBar, sinkBreachDepthPlus) = sinkBreachDepthRects
      (streamPowerMaxErosionMinus, streamPowerMaxErosionBar, streamPowerMaxErosionPlus) = streamPowerMaxErosionRects
      (riverCarveMaxDepthMinus, riverCarveMaxDepthBar, riverCarveMaxDepthPlus) = riverCarveMaxDepthRects
      (coastalErodeStrengthMinus, coastalErodeStrengthBar, coastalErodeStrengthPlus) = coastalErodeStrengthRects
      (hydroHardnessWeightMinus, hydroHardnessWeightBar, hydroHardnessWeightPlus) = hydroHardnessWeightRects
      (minLakeSizeMinus, minLakeSizeBar, minLakeSizePlus) = minLakeSizeRects
      (inlandSeaMinSizeMinus, inlandSeaMinSizeBar, inlandSeaMinSizePlus) = inlandSeaMinSizeRects
      (roughnessScaleMinus, roughnessScaleBar, roughnessScalePlus) = roughnessScaleRects
  in if uiShowConfig ui
    then do
      SDL.rendererDrawColor renderer SDL.$= V4 35 45 60 230
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawConfigTabs renderer ui (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline)
      SDL.rendererDrawColor renderer SDL.$= V4 30 38 52 230
      SDL.fillRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererDrawColor renderer SDL.$= V4 60 70 90 255
      SDL.drawRect renderer (Just (rectToSDL scrollAreaRect))
      let rowHeight = 24
          gap = 10
          rows = configRowCount (uiConfigTab ui) ui
          contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
          Rect (V2 _ _, V2 _ scrollH) = scrollAreaRect
          maxOffset = max 0 (contentHeight - scrollH)
          scrollY = min maxOffset (uiConfigScroll ui)
          scrollRect (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollAreaRect)
      case uiConfigTab ui of
        ConfigTerrain -> do
          drawConfigSlider renderer (uiGenScale ui) (scrollRect genScaleMinus) (scrollRect genScaleBar) (scrollRect genScalePlus) (V4 90 140 180 255)
          drawConfigSlider renderer (uiGenCoordScale ui) (scrollRect genCoordScaleMinus) (scrollRect genCoordScaleBar) (scrollRect genCoordScalePlus) (V4 80 120 170 255)
          drawConfigSlider renderer (uiGenOffsetX ui) (scrollRect genOffsetXMinus) (scrollRect genOffsetXBar) (scrollRect genOffsetXPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenOffsetY ui) (scrollRect genOffsetYMinus) (scrollRect genOffsetYBar) (scrollRect genOffsetYPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenFrequency ui) (scrollRect genFrequencyMinus) (scrollRect genFrequencyBar) (scrollRect genFrequencyPlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiGenOctaves ui) (scrollRect genOctavesMinus) (scrollRect genOctavesBar) (scrollRect genOctavesPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenLacunarity ui) (scrollRect genLacunarityMinus) (scrollRect genLacunarityBar) (scrollRect genLacunarityPlus) (V4 130 110 170 255)
          drawConfigSlider renderer (uiGenGain ui) (scrollRect genGainMinus) (scrollRect genGainBar) (scrollRect genGainPlus) (V4 140 120 160 255)
          drawConfigSlider renderer (uiGenWarpScale ui) (scrollRect genWarpScaleMinus) (scrollRect genWarpScaleBar) (scrollRect genWarpScalePlus) (V4 90 150 140 255)
          drawConfigSlider renderer (uiGenWarpStrength ui) (scrollRect genWarpStrengthMinus) (scrollRect genWarpStrengthBar) (scrollRect genWarpStrengthPlus) (V4 110 150 110 255)
          drawConfigSlider renderer (uiWorldExtentX ui) (scrollRect extentXMinus) (scrollRect extentXBar) (scrollRect extentXPlus) (V4 130 140 120 255)
          drawConfigSlider renderer (uiWorldExtentY ui) (scrollRect extentYMinus) (scrollRect extentYBar) (scrollRect extentYPlus) (V4 120 140 140 255)
          drawConfigSlider renderer (uiEdgeDepthNorth ui) (scrollRect edgeNorthMinus) (scrollRect edgeNorthBar) (scrollRect edgeNorthPlus) (V4 120 150 180 255)
          drawConfigSlider renderer (uiEdgeDepthSouth ui) (scrollRect edgeSouthMinus) (scrollRect edgeSouthBar) (scrollRect edgeSouthPlus) (V4 120 130 200 255)
          drawConfigSlider renderer (uiEdgeDepthEast ui) (scrollRect edgeEastMinus) (scrollRect edgeEastBar) (scrollRect edgeEastPlus) (V4 110 140 190 255)
          drawConfigSlider renderer (uiEdgeDepthWest ui) (scrollRect edgeWestMinus) (scrollRect edgeWestBar) (scrollRect edgeWestPlus) (V4 110 140 170 255)
          drawConfigSlider renderer (uiEdgeDepthFalloff ui) (scrollRect edgeFalloffMinus) (scrollRect edgeFalloffBar) (scrollRect edgeFalloffPlus) (V4 140 150 110 255)
          drawConfigSlider renderer (uiPlateSize ui) (scrollRect plateSizeMinus) (scrollRect plateSizeBar) (scrollRect plateSizePlus) (V4 120 120 170 255)
          drawConfigSlider renderer (uiUplift ui) (scrollRect upliftMinus) (scrollRect upliftBar) (scrollRect upliftPlus) (V4 150 120 120 255)
          drawConfigSlider renderer (uiRiftDepth ui) (scrollRect riftDepthMinus) (scrollRect riftDepthBar) (scrollRect riftDepthPlus) (V4 120 130 160 255)
          drawConfigSlider renderer (uiDetailScale ui) (scrollRect detailScaleMinus) (scrollRect detailScaleBar) (scrollRect detailScalePlus) (V4 90 140 160 255)
          drawConfigSlider renderer (uiPlateSpeed ui) (scrollRect plateSpeedMinus) (scrollRect plateSpeedBar) (scrollRect plateSpeedPlus) (V4 110 130 180 255)
          drawConfigSlider renderer (uiBoundarySharpness ui) (scrollRect boundarySharpnessMinus) (scrollRect boundarySharpnessBar) (scrollRect boundarySharpnessPlus) (V4 130 120 170 255)
          drawConfigSlider renderer (uiBoundaryNoiseScale ui) (scrollRect boundaryNoiseScaleMinus) (scrollRect boundaryNoiseScaleBar) (scrollRect boundaryNoiseScalePlus) (V4 100 140 150 255)
          drawConfigSlider renderer (uiBoundaryNoiseStrength ui) (scrollRect boundaryNoiseStrengthMinus) (scrollRect boundaryNoiseStrengthBar) (scrollRect boundaryNoiseStrengthPlus) (V4 140 130 140 255)
          drawConfigSlider renderer (uiBoundaryWarpOctaves ui) (scrollRect boundaryWarpOctavesMinus) (scrollRect boundaryWarpOctavesBar) (scrollRect boundaryWarpOctavesPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiBoundaryWarpLacunarity ui) (scrollRect boundaryWarpLacunarityMinus) (scrollRect boundaryWarpLacunarityBar) (scrollRect boundaryWarpLacunarityPlus) (V4 120 120 170 255)
          drawConfigSlider renderer (uiBoundaryWarpGain ui) (scrollRect boundaryWarpGainMinus) (scrollRect boundaryWarpGainBar) (scrollRect boundaryWarpGainPlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiPlateMergeScale ui) (scrollRect plateMergeScaleMinus) (scrollRect plateMergeScaleBar) (scrollRect plateMergeScalePlus) (V4 120 140 120 255)
          drawConfigSlider renderer (uiPlateMergeBias ui) (scrollRect plateMergeBiasMinus) (scrollRect plateMergeBiasBar) (scrollRect plateMergeBiasPlus) (V4 140 130 120 255)
          drawConfigSlider renderer (uiPlateDetailScale ui) (scrollRect plateDetailScaleMinus) (scrollRect plateDetailScaleBar) (scrollRect plateDetailScalePlus) (V4 90 140 160 255)
          drawConfigSlider renderer (uiPlateDetailStrength ui) (scrollRect plateDetailStrengthMinus) (scrollRect plateDetailStrengthBar) (scrollRect plateDetailStrengthPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiPlateRidgeStrength ui) (scrollRect plateRidgeStrengthMinus) (scrollRect plateRidgeStrengthBar) (scrollRect plateRidgeStrengthPlus) (V4 150 130 120 255)
          drawConfigSlider renderer (uiPlateHeightBase ui) (scrollRect plateHeightBaseMinus) (scrollRect plateHeightBaseBar) (scrollRect plateHeightBasePlus) (V4 110 150 160 255)
          drawConfigSlider renderer (uiPlateHeightVariance ui) (scrollRect plateHeightVarianceMinus) (scrollRect plateHeightVarianceBar) (scrollRect plateHeightVariancePlus) (V4 140 140 160 255)
          drawConfigSlider renderer (uiPlateHardnessBase ui) (scrollRect plateHardnessBaseMinus) (scrollRect plateHardnessBaseBar) (scrollRect plateHardnessBasePlus) (V4 100 130 160 255)
          drawConfigSlider renderer (uiPlateHardnessVariance ui) (scrollRect plateHardnessVarianceMinus) (scrollRect plateHardnessVarianceBar) (scrollRect plateHardnessVariancePlus) (V4 120 140 160 255)
          drawConfigSlider renderer (uiTrenchDepth ui) (scrollRect trenchDepthMinus) (scrollRect trenchDepthBar) (scrollRect trenchDepthPlus) (V4 120 130 170 255)
          drawConfigSlider renderer (uiRidgeHeight ui) (scrollRect ridgeHeightMinus) (scrollRect ridgeHeightBar) (scrollRect ridgeHeightPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiPlateBiasStrength ui) (scrollRect plateBiasStrengthMinus) (scrollRect plateBiasStrengthBar) (scrollRect plateBiasStrengthPlus) (V4 140 120 160 255)
          drawConfigSlider renderer (uiPlateBiasCenter ui) (scrollRect plateBiasCenterMinus) (scrollRect plateBiasCenterBar) (scrollRect plateBiasCenterPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasEdge ui) (scrollRect plateBiasEdgeMinus) (scrollRect plateBiasEdgeBar) (scrollRect plateBiasEdgePlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasNorth ui) (scrollRect plateBiasNorthMinus) (scrollRect plateBiasNorthBar) (scrollRect plateBiasNorthPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasSouth ui) (scrollRect plateBiasSouthMinus) (scrollRect plateBiasSouthBar) (scrollRect plateBiasSouthPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiTfcCliffSlope ui) (scrollRect tfcCliffSlopeMinus) (scrollRect tfcCliffSlopeBar) (scrollRect tfcCliffSlopePlus) (V4 150 100 100 255)
          drawConfigSlider renderer (uiTfcMountainSlope ui) (scrollRect tfcMountainSlopeMinus) (scrollRect tfcMountainSlopeBar) (scrollRect tfcMountainSlopePlus) (V4 140 110 100 255)
          drawConfigSlider renderer (uiTfcMountainRelief ui) (scrollRect tfcMountainReliefMinus) (scrollRect tfcMountainReliefBar) (scrollRect tfcMountainReliefPlus) (V4 130 120 100 255)
          drawConfigSlider renderer (uiTfcHillSlope ui) (scrollRect tfcHillSlopeMinus) (scrollRect tfcHillSlopeBar) (scrollRect tfcHillSlopePlus) (V4 120 130 110 255)
          drawConfigSlider renderer (uiTfcRollingSlope ui) (scrollRect tfcRollingSlopeMinus) (scrollRect tfcRollingSlopeBar) (scrollRect tfcRollingSlopePlus) (V4 110 140 120 255)
          drawConfigSlider renderer (uiValleyCurvature ui) (scrollRect valleyCurvatureMinus) (scrollRect valleyCurvatureBar) (scrollRect valleyCurvaturePlus) (V4 100 140 140 255)
          drawConfigSlider renderer (uiTfcElevGradient ui) (scrollRect tfcElevGradientMinus) (scrollRect tfcElevGradientBar) (scrollRect tfcElevGradientPlus) (V4 100 130 150 255)
          drawConfigSlider renderer (uiTfcPlateauMaxRelief2Ring ui) (scrollRect tfcPlateauMaxRelief2RingMinus) (scrollRect tfcPlateauMaxRelief2RingBar) (scrollRect tfcPlateauMaxRelief2RingPlus) (V4 110 120 150 255)
          drawConfigSlider renderer (uiRockElevationThreshold ui) (scrollRect rockElevationThresholdMinus) (scrollRect rockElevationThresholdBar) (scrollRect rockElevationThresholdPlus) (V4 140 130 120 255)
          drawConfigSlider renderer (uiRockHardnessThreshold ui) (scrollRect rockHardnessThresholdMinus) (scrollRect rockHardnessThresholdBar) (scrollRect rockHardnessThresholdPlus) (V4 130 130 130 255)
          drawConfigSlider renderer (uiRockHardnessSecondary ui) (scrollRect rockHardnessSecondaryMinus) (scrollRect rockHardnessSecondaryBar) (scrollRect rockHardnessSecondaryPlus) (V4 120 130 140 255)
        ConfigClimate -> do
          drawConfigSlider renderer (uiWaterLevel ui) (scrollRect waterMinus) (scrollRect waterBar) (scrollRect waterPlus) (V4 70 120 180 255)
          drawConfigSlider renderer (uiOrographicLift ui) (scrollRect orographicLiftMinus) (scrollRect orographicLiftBar) (scrollRect orographicLiftPlus) (V4 110 140 190 255)
          drawConfigSlider renderer (uiRainShadowLoss ui) (scrollRect rainShadowLossMinus) (scrollRect rainShadowLossBar) (scrollRect rainShadowLossPlus) (V4 130 120 170 255)
          drawConfigSlider renderer (uiWindDiffuse ui) (scrollRect windDiffuseMinus) (scrollRect windDiffuseBar) (scrollRect windDiffusePlus) (V4 90 140 120 255)
          drawConfigSlider renderer (uiEquatorTemp ui) (scrollRect equatorTempMinus) (scrollRect equatorTempBar) (scrollRect equatorTempPlus) (V4 180 120 90 255)
          drawConfigSlider renderer (uiPoleTemp ui) (scrollRect poleTempMinus) (scrollRect poleTempBar) (scrollRect poleTempPlus) (V4 90 150 200 255)
          drawConfigSlider renderer (uiLapseRate ui) (scrollRect lapseRateMinus) (scrollRect lapseRateBar) (scrollRect lapseRatePlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiWindIterations ui) (scrollRect windIterationsMinus) (scrollRect windIterationsBar) (scrollRect windIterationsPlus) (V4 120 140 200 255)
          drawConfigSlider renderer (uiMoistureIterations ui) (scrollRect moistureIterationsMinus) (scrollRect moistureIterationsBar) (scrollRect moistureIterationsPlus) (V4 120 150 160 255)
          drawConfigSlider renderer (uiBoundaryMotionTemp ui) (scrollRect boundaryMotionTempMinus) (scrollRect boundaryMotionTempBar) (scrollRect boundaryMotionTempPlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiBoundaryMotionPrecip ui) (scrollRect boundaryMotionPrecipMinus) (scrollRect boundaryMotionPrecipBar) (scrollRect boundaryMotionPrecipPlus) (V4 120 140 170 255)
          drawConfigSlider renderer (uiSliceLatCenter ui) (scrollRect sliceLatCenterMinus) (scrollRect sliceLatCenterBar) (scrollRect sliceLatCenterPlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiSliceLonCenter ui) (scrollRect sliceLonCenterMinus) (scrollRect sliceLonCenterBar) (scrollRect sliceLonCenterPlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiLatitudeExponent ui) (scrollRect latitudeExponentMinus) (scrollRect latitudeExponentBar) (scrollRect latitudeExponentPlus) (V4 150 110 130 255)
          drawConfigSlider renderer (uiPlateHeightCooling ui) (scrollRect plateHeightCoolingMinus) (scrollRect plateHeightCoolingBar) (scrollRect plateHeightCoolingPlus) (V4 130 130 150 255)
          drawConfigSlider renderer (uiTempNoiseScale ui) (scrollRect tempNoiseScaleMinus) (scrollRect tempNoiseScaleBar) (scrollRect tempNoiseScalePlus) (V4 140 120 150 255)
          drawConfigSlider renderer (uiOceanModeration ui) (scrollRect oceanModerationMinus) (scrollRect oceanModerationBar) (scrollRect oceanModerationPlus) (V4 80 140 170 255)
          drawConfigSlider renderer (uiOceanModerateTemp ui) (scrollRect oceanModerateTempMinus) (scrollRect oceanModerateTempBar) (scrollRect oceanModerateTempPlus) (V4 90 130 180 255)
          drawConfigSlider renderer (uiAlbedoSensitivity ui) (scrollRect albedoSensitivityMinus) (scrollRect albedoSensitivityBar) (scrollRect albedoSensitivityPlus) (V4 170 150 100 255)
          drawConfigSlider renderer (uiAlbedoReference ui) (scrollRect albedoReferenceMinus) (scrollRect albedoReferenceBar) (scrollRect albedoReferencePlus) (V4 160 140 110 255)
          drawConfigSlider renderer (uiMoistAdvect ui) (scrollRect moistAdvectMinus) (scrollRect moistAdvectBar) (scrollRect moistAdvectPlus) (V4 100 150 160 255)
          drawConfigSlider renderer (uiMoistLocal ui) (scrollRect moistLocalMinus) (scrollRect moistLocalBar) (scrollRect moistLocalPlus) (V4 110 140 170 255)
          drawConfigSlider renderer (uiMoistWindEvapScale ui) (scrollRect moistWindEvapScaleMinus) (scrollRect moistWindEvapScaleBar) (scrollRect moistWindEvapScalePlus) (V4 90 160 150 255)
          drawConfigSlider renderer (uiMoistEvapNoiseScale ui) (scrollRect moistEvapNoiseScaleMinus) (scrollRect moistEvapNoiseScaleBar) (scrollRect moistEvapNoiseScalePlus) (V4 120 140 160 255)
          drawConfigSlider renderer (uiMoistBareEvapFrac ui) (scrollRect moistBareEvapFracMinus) (scrollRect moistBareEvapFracBar) (scrollRect moistBareEvapFracPlus) (V4 130 150 120 255)
          drawConfigSlider renderer (uiMoistVegTranspFrac ui) (scrollRect moistVegTranspFracMinus) (scrollRect moistVegTranspFracBar) (scrollRect moistVegTranspFracPlus) (V4 80 160 130 255)
          drawConfigSlider renderer (uiMoistWindETScale ui) (scrollRect moistWindETScaleMinus) (scrollRect moistWindETScaleBar) (scrollRect moistWindETScalePlus) (V4 110 150 150 255)
          drawConfigSlider renderer (uiMoistCondensationRate ui) (scrollRect moistCondensationRateMinus) (scrollRect moistCondensationRateBar) (scrollRect moistCondensationRatePlus) (V4 90 140 170 255)
          drawConfigSlider renderer (uiMoistRecycleRate ui) (scrollRect moistRecycleRateMinus) (scrollRect moistRecycleRateBar) (scrollRect moistRecycleRatePlus) (V4 100 130 160 255)
          drawConfigSlider renderer (uiMoistITCZStrength ui) (scrollRect moistITCZStrengthMinus) (scrollRect moistITCZStrengthBar) (scrollRect moistITCZStrengthPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiMoistITCZWidth ui) (scrollRect moistITCZWidthMinus) (scrollRect moistITCZWidthBar) (scrollRect moistITCZWidthPlus) (V4 110 160 130 255)
          drawConfigSlider renderer (uiOrographicScale ui) (scrollRect orographicScaleMinus) (scrollRect orographicScaleBar) (scrollRect orographicScalePlus) (V4 160 130 80 255)
          drawConfigSlider renderer (uiOrographicStep ui) (scrollRect orographicStepMinus) (scrollRect orographicStepBar) (scrollRect orographicStepPlus) (V4 150 140 90 255)
          drawConfigSlider renderer (uiCoastalIterations ui) (scrollRect coastalIterationsMinus) (scrollRect coastalIterationsBar) (scrollRect coastalIterationsPlus) (V4 100 160 160 255)
          drawConfigSlider renderer (uiCoastalDiffuse ui) (scrollRect coastalDiffuseMinus) (scrollRect coastalDiffuseBar) (scrollRect coastalDiffusePlus) (V4 90 150 170 255)
          drawConfigSlider renderer (uiCoastalMoistureBoost ui) (scrollRect coastalMoistureBoostMinus) (scrollRect coastalMoistureBoostBar) (scrollRect coastalMoistureBoostPlus) (V4 80 140 180 255)
          drawConfigSlider renderer (uiWindBeltStrength ui) (scrollRect windBeltStrengthMinus) (scrollRect windBeltStrengthBar) (scrollRect windBeltStrengthPlus) (V4 130 150 180 255)
          drawConfigSlider renderer (uiWindBeltHarmonics ui) (scrollRect windBeltHarmonicsMinus) (scrollRect windBeltHarmonicsBar) (scrollRect windBeltHarmonicsPlus) (V4 120 140 170 255)
          drawConfigSlider renderer (uiWindBeltBase ui) (scrollRect windBeltBaseMinus) (scrollRect windBeltBaseBar) (scrollRect windBeltBasePlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiWindBeltRange ui) (scrollRect windBeltRangeMinus) (scrollRect windBeltRangeBar) (scrollRect windBeltRangePlus) (V4 100 140 175 255)
          drawConfigSlider renderer (uiWindBeltSpeedScale ui) (scrollRect windBeltSpeedScaleMinus) (scrollRect windBeltSpeedScaleBar) (scrollRect windBeltSpeedScalePlus) (V4 90 135 185 255)
          drawConfigSlider renderer (uiBndLandRange ui) (scrollRect bndLandRangeMinus) (scrollRect bndLandRangeBar) (scrollRect bndLandRangePlus) (V4 140 130 100 255)
          drawConfigSlider renderer (uiPiedmontSmooth ui) (scrollRect piedmontSmoothMinus) (scrollRect piedmontSmoothBar) (scrollRect piedmontSmoothPlus) (V4 120 150 130 255)
          drawConfigSlider renderer (uiPiedmontSlopeMin ui) (scrollRect piedmontSlopeMinMinus) (scrollRect piedmontSlopeMinBar) (scrollRect piedmontSlopeMinPlus) (V4 130 140 120 255)
          drawConfigSlider renderer (uiPiedmontSlopeMax ui) (scrollRect piedmontSlopeMaxMinus) (scrollRect piedmontSlopeMaxBar) (scrollRect piedmontSlopeMaxPlus) (V4 140 130 120 255)
          drawConfigSlider renderer (uiWindCoriolisDeflection ui) (scrollRect windCoriolisDeflectionMinus) (scrollRect windCoriolisDeflectionBar) (scrollRect windCoriolisDeflectionPlus) (V4 130 130 170 255)
          drawConfigSlider renderer (uiMoistMinVegFloor ui) (scrollRect moistMinVegFloorMinus) (scrollRect moistMinVegFloorBar) (scrollRect moistMinVegFloorPlus) (V4 110 150 110 255)
        ConfigPlanet -> do
          drawConfigSlider renderer (uiPlanetRadius ui) (scrollRect planetRadiusMinus) (scrollRect planetRadiusBar) (scrollRect planetRadiusPlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiAxialTilt ui) (scrollRect axialTiltMinus) (scrollRect axialTiltBar) (scrollRect axialTiltPlus) (V4 140 140 100 255)
          drawConfigSlider renderer (uiInsolation ui) (scrollRect insolationMinus) (scrollRect insolationBar) (scrollRect insolationPlus) (V4 180 150 80 255)
          drawConfigSlider renderer (uiOccWarmScale ui) (scrollRect occWarmScaleMinus) (scrollRect occWarmScaleBar) (scrollRect occWarmScalePlus) (V4 180 120 100 255)
          drawConfigSlider renderer (uiOccColdScale ui) (scrollRect occColdScaleMinus) (scrollRect occColdScaleBar) (scrollRect occColdScalePlus) (V4 100 140 180 255)
          drawConfigSlider renderer (uiOccLatPeakDeg ui) (scrollRect occLatPeakDegMinus) (scrollRect occLatPeakDegBar) (scrollRect occLatPeakDegPlus) (V4 140 130 160 255)
          drawConfigSlider renderer (uiOccLatWidthDeg ui) (scrollRect occLatWidthDegMinus) (scrollRect occLatWidthDegBar) (scrollRect occLatWidthDegPlus) (V4 130 150 140 255)
        ConfigWeather -> do
          drawConfigSlider renderer (uiWeatherTick ui) (scrollRect weatherTickMinus) (scrollRect weatherTickBar) (scrollRect weatherTickPlus) (V4 140 120 140 255)
          drawConfigSlider renderer (uiWeatherPhase ui) (scrollRect weatherPhaseMinus) (scrollRect weatherPhaseBar) (scrollRect weatherPhasePlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiWeatherAmplitude ui) (scrollRect weatherAmplitudeMinus) (scrollRect weatherAmplitudeBar) (scrollRect weatherAmplitudePlus) (V4 140 120 90 255)
          drawConfigSlider renderer (uiSeasonCycleLength ui) (scrollRect seasonCycleLengthMinus) (scrollRect seasonCycleLengthBar) (scrollRect seasonCycleLengthPlus) (V4 130 110 140 255)
          drawConfigSlider renderer (uiJitterAmplitude ui) (scrollRect jitterAmplitudeMinus) (scrollRect jitterAmplitudeBar) (scrollRect jitterAmplitudePlus) (V4 120 130 130 255)
          drawConfigSlider renderer (uiPressureBase ui) (scrollRect pressureBaseMinus) (scrollRect pressureBaseBar) (scrollRect pressureBasePlus) (V4 110 140 120 255)
          drawConfigSlider renderer (uiPressureTempScale ui) (scrollRect pressureTempScaleMinus) (scrollRect pressureTempScaleBar) (scrollRect pressureTempScalePlus) (V4 100 150 110 255)
          drawConfigSlider renderer (uiPressureCoriolisScale ui) (scrollRect pressureCoriolisScaleMinus) (scrollRect pressureCoriolisScaleBar) (scrollRect pressureCoriolisScalePlus) (V4 130 120 150 255)
          drawConfigSlider renderer (uiSeasonalBase ui) (scrollRect seasonalBaseMinus) (scrollRect seasonalBaseBar) (scrollRect seasonalBasePlus) (V4 140 110 130 255)
          drawConfigSlider renderer (uiSeasonalRange ui) (scrollRect seasonalRangeMinus) (scrollRect seasonalRangeBar) (scrollRect seasonalRangePlus) (V4 120 140 140 255)
          drawConfigSlider renderer (uiHumidityNoiseScale ui) (scrollRect humidityNoiseScaleMinus) (scrollRect humidityNoiseScaleBar) (scrollRect humidityNoiseScalePlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiPrecipNoiseScale ui) (scrollRect precipNoiseScaleMinus) (scrollRect precipNoiseScaleBar) (scrollRect precipNoiseScalePlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiWeatherITCZWidth ui) (scrollRect weatherITCZWidthMinus) (scrollRect weatherITCZWidthBar) (scrollRect weatherITCZWidthPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiWeatherITCZPrecipBoost ui) (scrollRect weatherITCZPrecipBoostMinus) (scrollRect weatherITCZPrecipBoostBar) (scrollRect weatherITCZPrecipBoostPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiPressureHumidityScale ui) (scrollRect pressureHumidityScaleMinus) (scrollRect pressureHumidityScaleBar) (scrollRect pressureHumidityScalePlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiPressureGradientWindScale ui) (scrollRect pressureGradientWindScaleMinus) (scrollRect pressureGradientWindScaleBar) (scrollRect pressureGradientWindScalePlus) (V4 100 130 150 255)
          drawConfigSlider renderer (uiWindNoiseScale ui) (scrollRect windNoiseScaleMinus) (scrollRect windNoiseScaleBar) (scrollRect windNoiseScalePlus) (V4 130 120 160 255)
          drawConfigSlider renderer (uiITCZMigrationScale ui) (scrollRect itczMigrationScaleMinus) (scrollRect itczMigrationScaleBar) (scrollRect itczMigrationScalePlus) (V4 140 130 170 255)
          drawConfigSlider renderer (uiCloudRHExponent ui) (scrollRect cloudRHExponentMinus) (scrollRect cloudRHExponentBar) (scrollRect cloudRHExponentPlus) (V4 120 120 140 255)
          drawConfigSlider renderer (uiCloudAlbedoEffect ui) (scrollRect cloudAlbedoEffectMinus) (scrollRect cloudAlbedoEffectBar) (scrollRect cloudAlbedoEffectPlus) (V4 110 110 150 255)
          drawConfigSlider renderer (uiCloudPrecipBoost ui) (scrollRect cloudPrecipBoostMinus) (scrollRect cloudPrecipBoostBar) (scrollRect cloudPrecipBoostPlus) (V4 100 100 160 255)
        ConfigBiome -> do
          drawConfigSlider renderer (uiVegBase ui) (scrollRect vegBaseMinus) (scrollRect vegBaseBar) (scrollRect vegBasePlus) (V4 100 130 120 255)
          drawConfigSlider renderer (uiVegBoost ui) (scrollRect vegBoostMinus) (scrollRect vegBoostBar) (scrollRect vegBoostPlus) (V4 120 150 120 255)
          drawConfigSlider renderer (uiVegTempWeight ui) (scrollRect vegTempWeightMinus) (scrollRect vegTempWeightBar) (scrollRect vegTempWeightPlus) (V4 130 140 170 255)
          drawConfigSlider renderer (uiVegPrecipWeight ui) (scrollRect vegPrecipWeightMinus) (scrollRect vegPrecipWeightBar) (scrollRect vegPrecipWeightPlus) (V4 120 140 190 255)
          drawConfigSlider renderer (uiBtCoastalBand ui) (scrollRect btCoastalBandMinus) (scrollRect btCoastalBandBar) (scrollRect btCoastalBandPlus) (V4 100 160 180 255)
          drawConfigSlider renderer (uiBtSnowMaxTemp ui) (scrollRect btSnowMaxTempMinus) (scrollRect btSnowMaxTempBar) (scrollRect btSnowMaxTempPlus) (V4 180 180 200 255)
          drawConfigSlider renderer (uiBtAlpineMaxTemp ui) (scrollRect btAlpineMaxTempMinus) (scrollRect btAlpineMaxTempBar) (scrollRect btAlpineMaxTempPlus) (V4 160 160 180 255)
          drawConfigSlider renderer (uiBtIceCapTemp ui) (scrollRect btIceCapTempMinus) (scrollRect btIceCapTempBar) (scrollRect btIceCapTempPlus) (V4 180 200 220 255)
          drawConfigSlider renderer (uiBtMontaneMaxTemp ui) (scrollRect btMontaneMaxTempMinus) (scrollRect btMontaneMaxTempBar) (scrollRect btMontaneMaxTempPlus) (V4 140 130 110 255)
          drawConfigSlider renderer (uiBtMontanePrecip ui) (scrollRect btMontanePrecipMinus) (scrollRect btMontanePrecipBar) (scrollRect btMontanePrecipPlus) (V4 120 140 120 255)
          drawConfigSlider renderer (uiBtCliffSlope ui) (scrollRect btCliffSlopeMinus) (scrollRect btCliffSlopeBar) (scrollRect btCliffSlopePlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiBtValleyMoisture ui) (scrollRect btValleyMoistureMinus) (scrollRect btValleyMoistureBar) (scrollRect btValleyMoisturePlus) (V4 100 150 130 255)
          drawConfigSlider renderer (uiBtDepressionMoisture ui) (scrollRect btDepressionMoistureMinus) (scrollRect btDepressionMoistureBar) (scrollRect btDepressionMoisturePlus) (V4 110 140 140 255)
          drawConfigSlider renderer (uiBtPrecipWeight ui) (scrollRect btPrecipWeightMinus) (scrollRect btPrecipWeightBar) (scrollRect btPrecipWeightPlus) (V4 120 130 150 255)
          drawConfigSlider renderer (uiVbcTempMin ui) (scrollRect vbcTempMinMinus) (scrollRect vbcTempMinBar) (scrollRect vbcTempMinPlus) (V4 130 120 100 255)
          drawConfigSlider renderer (uiVbcTempRange ui) (scrollRect vbcTempRangeMinus) (scrollRect vbcTempRangeBar) (scrollRect vbcTempRangePlus) (V4 140 130 110 255)
          drawConfigSlider renderer (uiVbcFertilityBoost ui) (scrollRect vbcFertilityBoostMinus) (scrollRect vbcFertilityBoostBar) (scrollRect vbcFertilityBoostPlus) (V4 100 150 100 255)
          drawConfigSlider renderer (uiVbcAlbedoBase ui) (scrollRect vbcAlbedoBaseMinus) (scrollRect vbcAlbedoBaseBar) (scrollRect vbcAlbedoBasePlus) (V4 160 160 140 255)
          drawConfigSlider renderer (uiVbcAlbedoBare ui) (scrollRect vbcAlbedoBareMinus) (scrollRect vbcAlbedoBareBar) (scrollRect vbcAlbedoBarePlus) (V4 170 160 130 255)
          drawConfigSlider renderer (uiVbcAlbedoVeg ui) (scrollRect vbcAlbedoVegMinus) (scrollRect vbcAlbedoVegBar) (scrollRect vbcAlbedoVegPlus) (V4 100 140 100 255)
          drawConfigSlider renderer (uiVbcOceanAlbedo ui) (scrollRect vbcOceanAlbedoMinus) (scrollRect vbcOceanAlbedoBar) (scrollRect vbcOceanAlbedoPlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiVbcIceAlbedo ui) (scrollRect vbcIceAlbedoMinus) (scrollRect vbcIceAlbedoBar) (scrollRect vbcIceAlbedoPlus) (V4 190 200 210 255)
          drawConfigSlider renderer (uiBiomeSmoothing ui) (scrollRect biomeSmoothingMinus) (scrollRect biomeSmoothingBar) (scrollRect biomeSmoothingPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiVolcanicAshBoost ui) (scrollRect volcanicAshBoostMinus) (scrollRect volcanicAshBoostBar) (scrollRect volcanicAshBoostPlus) (V4 160 120 80 255)
          drawConfigSlider renderer (uiVolcanicLavaPenalty ui) (scrollRect volcanicLavaPenaltyMinus) (scrollRect volcanicLavaPenaltyBar) (scrollRect volcanicLavaPenaltyPlus) (V4 180 100 60 255)
          drawConfigSlider renderer (uiBiomeFeedbackBlend ui) (scrollRect biomeFeedbackBlendMinus) (scrollRect biomeFeedbackBlendBar) (scrollRect biomeFeedbackBlendPlus) (V4 120 130 140 255)
        ConfigErosion -> do
          drawConfigSlider renderer (uiErosionHydraulic ui) (scrollRect erosionHydraulicMinus) (scrollRect erosionHydraulicBar) (scrollRect erosionHydraulicPlus) (V4 90 140 180 255)
          drawConfigSlider renderer (uiErosionThermal ui) (scrollRect erosionThermalMinus) (scrollRect erosionThermalBar) (scrollRect erosionThermalPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiRainRate ui) (scrollRect erosionRainRateMinus) (scrollRect erosionRainRateBar) (scrollRect erosionRainRatePlus) (V4 100 110 170 255)
          drawConfigSlider renderer (uiErosionTalus ui) (scrollRect erosionTalusMinus) (scrollRect erosionTalusBar) (scrollRect erosionTalusPlus) (V4 160 120 90 255)
          drawConfigSlider renderer (uiErosionMaxDrop ui) (scrollRect erosionMaxDropMinus) (scrollRect erosionMaxDropBar) (scrollRect erosionMaxDropPlus) (V4 140 120 120 255)
          drawConfigSlider renderer (uiErosionHydDeposit ui) (scrollRect erosionHydDepositMinus) (scrollRect erosionHydDepositBar) (scrollRect erosionHydDepositPlus) (V4 130 130 110 255)
          drawConfigSlider renderer (uiErosionDepositSlope ui) (scrollRect erosionDepositSlopeMinus) (scrollRect erosionDepositSlopeBar) (scrollRect erosionDepositSlopePlus) (V4 120 130 120 255)
          drawConfigSlider renderer (uiErosionThermDeposit ui) (scrollRect erosionThermDepositMinus) (scrollRect erosionThermDepositBar) (scrollRect erosionThermDepositPlus) (V4 150 110 100 255)
          drawConfigSlider renderer (uiErosionCoastZone ui) (scrollRect erosionCoastZoneMinus) (scrollRect erosionCoastZoneBar) (scrollRect erosionCoastZonePlus) (V4 90 150 180 255)
          drawConfigSlider renderer (uiErosionCoastStrength ui) (scrollRect erosionCoastStrengthMinus) (scrollRect erosionCoastStrengthBar) (scrollRect erosionCoastStrengthPlus) (V4 80 140 170 255)
          drawConfigSlider renderer (uiErosionCoastIter ui) (scrollRect erosionCoastIterMinus) (scrollRect erosionCoastIterBar) (scrollRect erosionCoastIterPlus) (V4 70 130 160 255)
          drawConfigSlider renderer (uiHypsometryEnabled ui) (scrollRect hypsometryEnabledMinus) (scrollRect hypsometryEnabledBar) (scrollRect hypsometryEnabledPlus) (V4 200 180 140 255)
          drawConfigSlider renderer (uiHypsometryLowlandExp ui) (scrollRect hypsometryLowlandExpMinus) (scrollRect hypsometryLowlandExpBar) (scrollRect hypsometryLowlandExpPlus) (V4 190 175 135 255)
          drawConfigSlider renderer (uiHypsometryHighlandExp ui) (scrollRect hypsometryHighlandExpMinus) (scrollRect hypsometryHighlandExpBar) (scrollRect hypsometryHighlandExpPlus) (V4 180 170 130 255)
          drawConfigSlider renderer (uiHypsometryPlateauBreak ui) (scrollRect hypsometryPlateauBreakMinus) (scrollRect hypsometryPlateauBreakBar) (scrollRect hypsometryPlateauBreakPlus) (V4 170 165 125 255)
          drawConfigSlider renderer (uiHypsometryOceanExp ui) (scrollRect hypsometryOceanExpMinus) (scrollRect hypsometryOceanExpBar) (scrollRect hypsometryOceanExpPlus) (V4 160 160 120 255)
          drawConfigSlider renderer (uiHypsometryCoastalRampWidth ui) (scrollRect hypsometryCoastalRampWidthMinus) (scrollRect hypsometryCoastalRampWidthBar) (scrollRect hypsometryCoastalRampWidthPlus) (V4 150 155 115 255)
          drawConfigSlider renderer (uiHypsometryCoastalRampStr ui) (scrollRect hypsometryCoastalRampStrMinus) (scrollRect hypsometryCoastalRampStrBar) (scrollRect hypsometryCoastalRampStrPlus) (V4 140 150 110 255)
          drawConfigSlider renderer (uiGlacierSnowTemp ui) (scrollRect glacierSnowTempMinus) (scrollRect glacierSnowTempBar) (scrollRect glacierSnowTempPlus) (V4 180 210 240 255)
          drawConfigSlider renderer (uiGlacierSnowRange ui) (scrollRect glacierSnowRangeMinus) (scrollRect glacierSnowRangeBar) (scrollRect glacierSnowRangePlus) (V4 170 200 235 255)
          drawConfigSlider renderer (uiGlacierMeltTemp ui) (scrollRect glacierMeltTempMinus) (scrollRect glacierMeltTempBar) (scrollRect glacierMeltTempPlus) (V4 160 195 230 255)
          drawConfigSlider renderer (uiGlacierMeltRate ui) (scrollRect glacierMeltRateMinus) (scrollRect glacierMeltRateBar) (scrollRect glacierMeltRatePlus) (V4 150 190 225 255)
          drawConfigSlider renderer (uiGlacierAccumScale ui) (scrollRect glacierAccumScaleMinus) (scrollRect glacierAccumScaleBar) (scrollRect glacierAccumScalePlus) (V4 140 185 220 255)
          drawConfigSlider renderer (uiGlacierFlowIters ui) (scrollRect glacierFlowItersMinus) (scrollRect glacierFlowItersBar) (scrollRect glacierFlowItersPlus) (V4 130 180 215 255)
          drawConfigSlider renderer (uiGlacierFlowRate ui) (scrollRect glacierFlowRateMinus) (scrollRect glacierFlowRateBar) (scrollRect glacierFlowRatePlus) (V4 120 175 210 255)
          drawConfigSlider renderer (uiGlacierErosionScale ui) (scrollRect glacierErosionScaleMinus) (scrollRect glacierErosionScaleBar) (scrollRect glacierErosionScalePlus) (V4 110 170 210 255)
          drawConfigSlider renderer (uiGlacierCarveScale ui) (scrollRect glacierCarveScaleMinus) (scrollRect glacierCarveScaleBar) (scrollRect glacierCarveScalePlus) (V4 100 165 205 255)
          drawConfigSlider renderer (uiGlacierDepositScale ui) (scrollRect glacierDepositScaleMinus) (scrollRect glacierDepositScaleBar) (scrollRect glacierDepositScalePlus) (V4 90 160 200 255)
          drawConfigSlider renderer (uiVentDensity ui) (scrollRect ventDensityMinus) (scrollRect ventDensityBar) (scrollRect ventDensityPlus) (V4 200 80 60 255)
          drawConfigSlider renderer (uiVentThreshold ui) (scrollRect ventThresholdMinus) (scrollRect ventThresholdBar) (scrollRect ventThresholdPlus) (V4 190 90 70 255)
          drawConfigSlider renderer (uiHotspotScale ui) (scrollRect hotspotScaleMinus) (scrollRect hotspotScaleBar) (scrollRect hotspotScalePlus) (V4 210 100 50 255)
          drawConfigSlider renderer (uiHotspotThreshold ui) (scrollRect hotspotThresholdMinus) (scrollRect hotspotThresholdBar) (scrollRect hotspotThresholdPlus) (V4 200 110 60 255)
          drawConfigSlider renderer (uiMagmaRecharge ui) (scrollRect magmaRechargeMinus) (scrollRect magmaRechargeBar) (scrollRect magmaRechargePlus) (V4 220 90 40 255)
          drawConfigSlider renderer (uiLavaScale ui) (scrollRect lavaScaleMinus) (scrollRect lavaScaleBar) (scrollRect lavaScalePlus) (V4 230 80 30 255)
          drawConfigSlider renderer (uiAshScale ui) (scrollRect ashScaleMinus) (scrollRect ashScaleBar) (scrollRect ashScalePlus) (V4 170 130 100 255)
          drawConfigSlider renderer (uiVolcanicDepositScale ui) (scrollRect volcanicDepositScaleMinus) (scrollRect volcanicDepositScaleBar) (scrollRect volcanicDepositScalePlus) (V4 180 120 80 255)
          drawConfigSlider renderer (uiSoilMoistureThreshold ui) (scrollRect soilMoistureThresholdMinus) (scrollRect soilMoistureThresholdBar) (scrollRect soilMoistureThresholdPlus) (V4 140 120 80 255)
          drawConfigSlider renderer (uiSoilHardnessThreshold ui) (scrollRect soilHardnessThresholdMinus) (scrollRect soilHardnessThresholdBar) (scrollRect soilHardnessThresholdPlus) (V4 150 130 90 255)
          drawConfigSlider renderer (uiSoilFertilityMoistWeight ui) (scrollRect soilFertilityMoistWeightMinus) (scrollRect soilFertilityMoistWeightBar) (scrollRect soilFertilityMoistWeightPlus) (V4 120 140 80 255)
          drawConfigSlider renderer (uiSoilFertilityDepthWeight ui) (scrollRect soilFertilityDepthWeightMinus) (scrollRect soilFertilityDepthWeightBar) (scrollRect soilFertilityDepthWeightPlus) (V4 130 140 90 255)
          drawConfigSlider renderer (uiSinkBreachDepth ui) (scrollRect sinkBreachDepthMinus) (scrollRect sinkBreachDepthBar) (scrollRect sinkBreachDepthPlus) (V4 70 130 170 255)
          drawConfigSlider renderer (uiStreamPowerMaxErosion ui) (scrollRect streamPowerMaxErosionMinus) (scrollRect streamPowerMaxErosionBar) (scrollRect streamPowerMaxErosionPlus) (V4 80 140 170 255)
          drawConfigSlider renderer (uiRiverCarveMaxDepth ui) (scrollRect riverCarveMaxDepthMinus) (scrollRect riverCarveMaxDepthBar) (scrollRect riverCarveMaxDepthPlus) (V4 60 120 160 255)
          drawConfigSlider renderer (uiCoastalErodeStrength ui) (scrollRect coastalErodeStrengthMinus) (scrollRect coastalErodeStrengthBar) (scrollRect coastalErodeStrengthPlus) (V4 70 110 150 255)
          drawConfigSlider renderer (uiHydroHardnessWeight ui) (scrollRect hydroHardnessWeightMinus) (scrollRect hydroHardnessWeightBar) (scrollRect hydroHardnessWeightPlus) (V4 90 130 160 255)
          drawConfigSlider renderer (uiMinLakeSize ui) (scrollRect minLakeSizeMinus) (scrollRect minLakeSizeBar) (scrollRect minLakeSizePlus) (V4 60 140 180 255)
          drawConfigSlider renderer (uiInlandSeaMinSize ui) (scrollRect inlandSeaMinSizeMinus) (scrollRect inlandSeaMinSizeBar) (scrollRect inlandSeaMinSizePlus) (V4 50 130 170 255)
          drawConfigSlider renderer (uiRoughnessScale ui) (scrollRect roughnessScaleMinus) (scrollRect roughnessScaleBar) (scrollRect roughnessScalePlus) (V4 110 130 140 255)
        ConfigPipeline -> do
          let stages = allBuiltinStageIds
              disabled = uiDisabledStages ui
              plugins = uiPluginNames ui
              checkboxSize = 16
              Rect (V2 sx sy, V2 sw _sh) = scrollAreaRect
              pad = 12
          forM_ (zip [0 ..] stages) $ \(idx, sid) -> do
            let baseY = sy + configRowTopPad + idx * (rowHeight + gap)
                ry = baseY - scrollY
                isDisabled = Set.member sid disabled
                checkX = sx + pad
                checkY = ry + (rowHeight - checkboxSize) `div` 2
                checkColor = if isDisabled then V4 60 60 70 200 else V4 70 150 90 255
                borderColor = if isDisabled then V4 80 80 90 200 else V4 100 180 120 255
            SDL.rendererDrawColor renderer SDL.$= checkColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= borderColor
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
          let pluginOffset = length stages
          forM_ (zip [0 ..] plugins) $ \(idx, _pName) -> do
            let baseY = sy + configRowTopPad + (pluginOffset + idx) * (rowHeight + gap)
                ry = baseY - scrollY
                checkX = sx + pad
                checkY = ry + (rowHeight - checkboxSize) `div` 2
                pluginColor = V4 100 90 160 255
                pluginBorder = V4 130 120 190 255
                btnSize = 14
                rightPad' = 12
                btnY = ry + (rowHeight - btnSize) `div` 2
                upX = sx + sw - rightPad' - btnSize * 2 - 4
                downX = sx + sw - rightPad' - btnSize
                arrowColor = V4 150 150 170 255
                arrowBorder = V4 100 100 120 255
            SDL.rendererDrawColor renderer SDL.$= pluginColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= pluginBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= V4 50 50 65 255
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 upX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 upX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowColor
            let upMidX = upX + btnSize `div` 2
                upTop = btnY + 3
                upBot = btnY + btnSize - 3
            forM_ [upTop .. upBot] $ \row -> do
              let halfW = (row - upTop) * (btnSize `div` 2 - 2) `div` max 1 (upBot - upTop)
              SDL.drawLine renderer
                (SDL.P (V2 (fromIntegral (upMidX - halfW)) (fromIntegral row)))
                (SDL.P (V2 (fromIntegral (upMidX + halfW)) (fromIntegral row)))
            SDL.rendererDrawColor renderer SDL.$= V4 50 50 65 255
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 downX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 downX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowColor
            let dnMidX = downX + btnSize `div` 2
                dnTop = btnY + 3
                dnBot = btnY + btnSize - 3
            forM_ [dnTop .. dnBot] $ \row -> do
              let halfW = (dnBot - row) * (btnSize `div` 2 - 2) `div` max 1 (dnBot - dnTop)
              SDL.drawLine renderer
                (SDL.P (V2 (fromIntegral (dnMidX - halfW)) (fromIntegral row)))
                (SDL.P (V2 (fromIntegral (dnMidX + halfW)) (fromIntegral row)))
          let simOffset = length stages + length plugins
              simWorldReady = dsTerrainChunks dataSnap > 0
              tickBtnY = sy + configRowTopPad + simOffset * (rowHeight + gap) - scrollY
              tickBtnRect = Rect (V2 (sx + pad) tickBtnY, V2 60 rowHeight)
              tickBtnColor = if simWorldReady then V4 80 120 160 255 else V4 55 65 80 170
              autoTickY = sy + configRowTopPad + (simOffset + 1) * (rowHeight + gap) - scrollY
              autoTickCheckX = sx + pad
              autoTickCheckY = autoTickY + (rowHeight - checkboxSize) `div` 2
              autoTickColor = if uiSimAutoTick ui then V4 70 150 90 255 else V4 60 60 70 200
              autoTickBorder = if uiSimAutoTick ui then V4 100 180 120 255 else V4 80 80 90 200
              tickRateY = sy + configRowTopPad + (simOffset + 2) * (rowHeight + gap) - scrollY
              tickRateBarW = 120
              tickRateBarRect = Rect (V2 (sx + pad) (tickRateY + 4), V2 tickRateBarW (rowHeight - 8))
          SDL.rendererDrawColor renderer SDL.$= tickBtnColor
          SDL.fillRect renderer (Just (rectToSDL tickBtnRect))
          SDL.rendererDrawColor renderer SDL.$= autoTickColor
          SDL.fillRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= autoTickBorder
          SDL.drawRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
          SDL.fillRect renderer (Just (rectToSDL tickRateBarRect))
          drawBarFill renderer (uiSimTickRate ui) tickRateBarRect (V4 100 130 180 255)
      SDL.rendererClipRect renderer SDL.$= Nothing
      let Rect (V2 bx by, V2 bw bh) = scrollBarRect
          handleH = if maxOffset == 0 then bh else max 12 (bh * scrollH `div` max 1 contentHeight)
          handleY = if maxOffset == 0 then by else by + (bh - handleH) * scrollY `div` maxOffset
      SDL.rendererDrawColor renderer SDL.$= V4 25 25 30 255
      SDL.fillRect renderer (Just (rectToSDL scrollBarRect))
      SDL.rendererDrawColor renderer SDL.$= V4 160 160 170 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 bx handleY, V2 bw handleH))))
      SDL.rendererDrawColor renderer SDL.$= V4 60 120 80 255
      SDL.fillRect renderer (Just (rectToSDL presetSaveRect))
      SDL.rendererDrawColor renderer SDL.$= V4 80 110 160 255
      SDL.fillRect renderer (Just (rectToSDL presetLoadRect))
      SDL.rendererDrawColor renderer SDL.$= V4 120 80 80 255
      SDL.fillRect renderer (Just (rectToSDL resetRect))
      let revertColor = case uiWorldConfig ui of
            Just _ -> V4 140 100 50 255
            Nothing -> V4 70 60 45 120
      SDL.rendererDrawColor renderer SDL.$= revertColor
      SDL.fillRect renderer (Just (rectToSDL revertRect))
    else pure ()

drawConfigSlider :: SDL.Renderer -> Float -> Rect -> Rect -> Rect -> V4 Word8 -> IO ()
drawConfigSlider renderer value minusRect barRect plusRect fillColor = do
  SDL.rendererDrawColor renderer SDL.$= V4 90 90 110 255
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
  SDL.fillRect renderer (Just (rectToSDL barRect))
  drawBarFill renderer value barRect fillColor

drawBarFill :: SDL.Renderer -> Float -> Rect -> V4 Word8 -> IO ()
drawBarFill renderer value (Rect (V2 x y, V2 w h)) color = do
  let fillW = max 0 (min w (round (fromIntegral w * value)))
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 x y, V2 fillW h))))