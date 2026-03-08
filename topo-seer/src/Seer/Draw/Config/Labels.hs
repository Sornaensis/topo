{-# LANGUAGE OverloadedStrings #-}

-- | Config-panel text labels: tab names, slider value labels,
-- pipeline stage names, and simulation controls.
module Seer.Draw.Config.Labels
  ( drawConfigLabels
  ) where

import Actor.UI (ConfigTab(..), UiState(..), configRowCount)
import Control.Monad (forM_, when)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderSpec
import Topo.Pipeline.Stage (allBuiltinStageIds, stageCanonicalName)
import UI.Font (FontCache)
import UI.Layout
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, drawLabelAbove, drawTextLine, rectToSDL)

-- | Render all config-panel text labels when the config panel is visible.
drawConfigLabels :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawConfigLabels renderer fontCache ui layout = when (uiShowConfig ui) $ do
  let lc = V4 235 235 235 255
      scrollArea = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      rows = configRowCount (uiConfigTab ui) ui
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _, V2 _ scrollH) = scrollArea
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
      sr (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      -- | Draw a full slider's labels: minus, plus, bar value.
      sl minF barF plusF spec val = do
        drawCentered fontCache lc (sr (minF layout)) "-"
        drawCentered fontCache lc (sr (plusF layout)) "+"
        drawLabelAbove fontCache lc (sr (barF layout)) (sliderLabel spec val)
      -- | Draw only the bar value label (for sliders without button text).
      lbl barF spec val =
        drawLabelAbove fontCache lc (sr (barF layout)) (sliderLabel spec val)
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline) = configTabRects layout

  -- Tab labels
  drawCentered fontCache lc tabTerrain "Terrain"
  drawCentered fontCache lc tabPlanet "Planet"
  drawCentered fontCache lc tabClimate "Climate"
  drawCentered fontCache lc tabWeather "Weather"
  drawCentered fontCache lc tabBiome "Biome"
  drawCentered fontCache lc tabErosion "Erosion"
  drawCentered fontCache lc tabPipeline "Pipe"

  -- Preset / reset / revert
  drawCentered fontCache lc (configPresetSaveRect layout) "Save"
  drawCentered fontCache lc (configPresetLoadRect layout) "Load"
  drawCentered fontCache lc (configResetRect layout) "Reset"
  let revertLabelColor = case uiWorldConfig ui of
        Just _  -> lc
        Nothing -> V4 120 120 130 140
  drawCentered fontCache revertLabelColor (configRevertRect layout) "Revert"

  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollArea)

  case uiConfigTab ui of
    ConfigTerrain -> do
      sl configGenScaleMinusRect configGenScaleBarRect configGenScalePlusRect specGenScale (uiGenScale ui)
      sl configGenCoordScaleMinusRect configGenCoordScaleBarRect configGenCoordScalePlusRect specGenCoordScale (uiGenCoordScale ui)
      sl configGenOffsetXMinusRect configGenOffsetXBarRect configGenOffsetXPlusRect specGenOffsetX (uiGenOffsetX ui)
      sl configGenOffsetYMinusRect configGenOffsetYBarRect configGenOffsetYPlusRect specGenOffsetY (uiGenOffsetY ui)
      sl configGenFrequencyMinusRect configGenFrequencyBarRect configGenFrequencyPlusRect specGenFrequency (uiGenFrequency ui)
      sl configGenOctavesMinusRect configGenOctavesBarRect configGenOctavesPlusRect specGenOctaves (uiGenOctaves ui)
      sl configGenLacunarityMinusRect configGenLacunarityBarRect configGenLacunarityPlusRect specGenLacunarity (uiGenLacunarity ui)
      sl configGenGainMinusRect configGenGainBarRect configGenGainPlusRect specGenGain (uiGenGain ui)
      sl configGenWarpScaleMinusRect configGenWarpScaleBarRect configGenWarpScalePlusRect specGenWarpScale (uiGenWarpScale ui)
      sl configGenWarpStrengthMinusRect configGenWarpStrengthBarRect configGenWarpStrengthPlusRect specGenWarpStrength (uiGenWarpStrength ui)
      sl configExtentXMinusRect configExtentXBarRect configExtentXPlusRect specExtentX (uiWorldExtentX ui)
      sl configExtentYMinusRect configExtentYBarRect configExtentYPlusRect specExtentY (uiWorldExtentY ui)
      sl configEdgeNorthMinusRect configEdgeNorthBarRect configEdgeNorthPlusRect specEdgeNorth (uiEdgeDepthNorth ui)
      sl configEdgeSouthMinusRect configEdgeSouthBarRect configEdgeSouthPlusRect specEdgeSouth (uiEdgeDepthSouth ui)
      sl configEdgeEastMinusRect configEdgeEastBarRect configEdgeEastPlusRect specEdgeEast (uiEdgeDepthEast ui)
      sl configEdgeWestMinusRect configEdgeWestBarRect configEdgeWestPlusRect specEdgeWest (uiEdgeDepthWest ui)
      sl configEdgeFalloffMinusRect configEdgeFalloffBarRect configEdgeFalloffPlusRect specEdgeFalloff (uiEdgeDepthFalloff ui)
      sl configPlateSizeMinusRect configPlateSizeBarRect configPlateSizePlusRect specPlateSize (uiPlateSize ui)
      sl configUpliftMinusRect configUpliftBarRect configUpliftPlusRect specUplift (uiUplift ui)
      sl configRiftDepthMinusRect configRiftDepthBarRect configRiftDepthPlusRect specRiftDepth (uiRiftDepth ui)
      sl configDetailScaleMinusRect configDetailScaleBarRect configDetailScalePlusRect specDetailScale (uiDetailScale ui)
      sl configPlateSpeedMinusRect configPlateSpeedBarRect configPlateSpeedPlusRect specPlateSpeed (uiPlateSpeed ui)
      sl configBoundarySharpnessMinusRect configBoundarySharpnessBarRect configBoundarySharpnessPlusRect specBoundarySharpness (uiBoundarySharpness ui)
      sl configBoundaryNoiseScaleMinusRect configBoundaryNoiseScaleBarRect configBoundaryNoiseScalePlusRect specBoundaryNoiseScale (uiBoundaryNoiseScale ui)
      sl configBoundaryNoiseStrengthMinusRect configBoundaryNoiseStrengthBarRect configBoundaryNoiseStrengthPlusRect specBoundaryNoiseStrength (uiBoundaryNoiseStrength ui)
      sl configBoundaryWarpOctavesMinusRect configBoundaryWarpOctavesBarRect configBoundaryWarpOctavesPlusRect specBoundaryWarpOctaves (uiBoundaryWarpOctaves ui)
      sl configBoundaryWarpLacunarityMinusRect configBoundaryWarpLacunarityBarRect configBoundaryWarpLacunarityPlusRect specBoundaryWarpLacunarity (uiBoundaryWarpLacunarity ui)
      sl configBoundaryWarpGainMinusRect configBoundaryWarpGainBarRect configBoundaryWarpGainPlusRect specBoundaryWarpGain (uiBoundaryWarpGain ui)
      sl configPlateMergeScaleMinusRect configPlateMergeScaleBarRect configPlateMergeScalePlusRect specPlateMergeScale (uiPlateMergeScale ui)
      sl configPlateMergeBiasMinusRect configPlateMergeBiasBarRect configPlateMergeBiasPlusRect specPlateMergeBias (uiPlateMergeBias ui)
      sl configPlateDetailScaleMinusRect configPlateDetailScaleBarRect configPlateDetailScalePlusRect specPlateDetailScale (uiPlateDetailScale ui)
      sl configPlateDetailStrengthMinusRect configPlateDetailStrengthBarRect configPlateDetailStrengthPlusRect specPlateDetailStrength (uiPlateDetailStrength ui)
      sl configPlateRidgeStrengthMinusRect configPlateRidgeStrengthBarRect configPlateRidgeStrengthPlusRect specPlateRidgeStrength (uiPlateRidgeStrength ui)
      sl configPlateHeightBaseMinusRect configPlateHeightBaseBarRect configPlateHeightBasePlusRect specPlateHeightBase (uiPlateHeightBase ui)
      sl configPlateHeightVarianceMinusRect configPlateHeightVarianceBarRect configPlateHeightVariancePlusRect specPlateHeightVariance (uiPlateHeightVariance ui)
      sl configPlateHardnessBaseMinusRect configPlateHardnessBaseBarRect configPlateHardnessBasePlusRect specPlateHardnessBase (uiPlateHardnessBase ui)
      sl configPlateHardnessVarianceMinusRect configPlateHardnessVarianceBarRect configPlateHardnessVariancePlusRect specPlateHardnessVariance (uiPlateHardnessVariance ui)
      sl configTrenchDepthMinusRect configTrenchDepthBarRect configTrenchDepthPlusRect specTrenchDepth (uiTrenchDepth ui)
      sl configRidgeHeightMinusRect configRidgeHeightBarRect configRidgeHeightPlusRect specRidgeHeight (uiRidgeHeight ui)
      sl configPlateBiasStrengthMinusRect configPlateBiasStrengthBarRect configPlateBiasStrengthPlusRect specPlateBiasStrength (uiPlateBiasStrength ui)
      sl configPlateBiasCenterMinusRect configPlateBiasCenterBarRect configPlateBiasCenterPlusRect specPlateBiasCenter (uiPlateBiasCenter ui)
      sl configPlateBiasEdgeMinusRect configPlateBiasEdgeBarRect configPlateBiasEdgePlusRect specPlateBiasEdge (uiPlateBiasEdge ui)
      sl configPlateBiasNorthMinusRect configPlateBiasNorthBarRect configPlateBiasNorthPlusRect specPlateBiasNorth (uiPlateBiasNorth ui)
      sl configPlateBiasSouthMinusRect configPlateBiasSouthBarRect configPlateBiasSouthPlusRect specPlateBiasSouth (uiPlateBiasSouth ui)
      sl configTfcCliffSlopeMinusRect configTfcCliffSlopeBarRect configTfcCliffSlopePlusRect specTfcCliffSlope (uiTfcCliffSlope ui)
      sl configTfcMountainSlopeMinusRect configTfcMountainSlopeBarRect configTfcMountainSlopePlusRect specTfcMountainSlope (uiTfcMountainSlope ui)
      sl configTfcMountainReliefMinusRect configTfcMountainReliefBarRect configTfcMountainReliefPlusRect specTfcMountainRelief (uiTfcMountainRelief ui)
      sl configTfcHillSlopeMinusRect configTfcHillSlopeBarRect configTfcHillSlopePlusRect specTfcHillSlope (uiTfcHillSlope ui)
      sl configTfcRollingSlopeMinusRect configTfcRollingSlopeBarRect configTfcRollingSlopePlusRect specTfcRollingSlope (uiTfcRollingSlope ui)
      sl configValleyCurvatureMinusRect configValleyCurvatureBarRect configValleyCurvaturePlusRect specValleyCurvature (uiValleyCurvature ui)
      sl configTfcElevGradientMinusRect configTfcElevGradientBarRect configTfcElevGradientPlusRect specTfcElevGradient (uiTfcElevGradient ui)
      sl configTfcPlateauMaxRelief2RingMinusRect configTfcPlateauMaxRelief2RingBarRect configTfcPlateauMaxRelief2RingPlusRect specTfcPlateauMaxRelief2Ring (uiTfcPlateauMaxRelief2Ring ui)
      sl configRockElevationThresholdMinusRect configRockElevationThresholdBarRect configRockElevationThresholdPlusRect specRockElevationThreshold (uiRockElevationThreshold ui)
      sl configRockHardnessThresholdMinusRect configRockHardnessThresholdBarRect configRockHardnessThresholdPlusRect specRockHardnessThreshold (uiRockHardnessThreshold ui)
      sl configRockHardnessSecondaryMinusRect configRockHardnessSecondaryBarRect configRockHardnessSecondaryPlusRect specRockHardnessSecondary (uiRockHardnessSecondary ui)

    ConfigClimate -> do
      sl configWaterMinusRect configWaterBarRect configWaterPlusRect specWaterLevel (uiWaterLevel ui)
      sl configOrographicLiftMinusRect configOrographicLiftBarRect configOrographicLiftPlusRect specOrographicLift (uiOrographicLift ui)
      sl configRainShadowLossMinusRect configRainShadowLossBarRect configRainShadowLossPlusRect specRainShadowLoss (uiRainShadowLoss ui)
      sl configWindDiffuseMinusRect configWindDiffuseBarRect configWindDiffusePlusRect specWindDiffuse (uiWindDiffuse ui)
      sl configEquatorTempMinusRect configEquatorTempBarRect configEquatorTempPlusRect specEquatorTemp (uiEquatorTemp ui)
      sl configPoleTempMinusRect configPoleTempBarRect configPoleTempPlusRect specPoleTemp (uiPoleTemp ui)
      sl configLapseRateMinusRect configLapseRateBarRect configLapseRatePlusRect specLapseRate (uiLapseRate ui)
      sl configWindIterationsMinusRect configWindIterationsBarRect configWindIterationsPlusRect specWindIterations (uiWindIterations ui)
      sl configMoistureIterationsMinusRect configMoistureIterationsBarRect configMoistureIterationsPlusRect specMoistureIterations (uiMoistureIterations ui)
      sl configBoundaryMotionTempMinusRect configBoundaryMotionTempBarRect configBoundaryMotionTempPlusRect specBoundaryMotionTemp (uiBoundaryMotionTemp ui)
      sl configBoundaryMotionPrecipMinusRect configBoundaryMotionPrecipBarRect configBoundaryMotionPrecipPlusRect specBoundaryMotionPrecip (uiBoundaryMotionPrecip ui)
      sl configSliceLatCenterMinusRect configSliceLatCenterBarRect configSliceLatCenterPlusRect specSliceLatCenter (uiSliceLatCenter ui)
      sl configSliceLonCenterMinusRect configSliceLonCenterBarRect configSliceLonCenterPlusRect specSliceLonCenter (uiSliceLonCenter ui)
      sl configLatitudeExponentMinusRect configLatitudeExponentBarRect configLatitudeExponentPlusRect specLatitudeExponent (uiLatitudeExponent ui)
      sl configPlateHeightCoolingMinusRect configPlateHeightCoolingBarRect configPlateHeightCoolingPlusRect specPlateHeightCooling (uiPlateHeightCooling ui)
      sl configTempNoiseScaleMinusRect configTempNoiseScaleBarRect configTempNoiseScalePlusRect specTempNoiseScale (uiTempNoiseScale ui)
      sl configOceanModerationMinusRect configOceanModerationBarRect configOceanModerationPlusRect specOceanModeration (uiOceanModeration ui)
      sl configOceanModerateTempMinusRect configOceanModerateTempBarRect configOceanModerateTempPlusRect specOceanModerateTemp (uiOceanModerateTemp ui)
      sl configAlbedoSensitivityMinusRect configAlbedoSensitivityBarRect configAlbedoSensitivityPlusRect specAlbedoSensitivity (uiAlbedoSensitivity ui)
      sl configAlbedoReferenceMinusRect configAlbedoReferenceBarRect configAlbedoReferencePlusRect specAlbedoReference (uiAlbedoReference ui)
      sl configMoistAdvectMinusRect configMoistAdvectBarRect configMoistAdvectPlusRect specMoistAdvect (uiMoistAdvect ui)
      sl configMoistLocalMinusRect configMoistLocalBarRect configMoistLocalPlusRect specMoistLocal (uiMoistLocal ui)
      sl configMoistWindEvapScaleMinusRect configMoistWindEvapScaleBarRect configMoistWindEvapScalePlusRect specMoistWindEvapScale (uiMoistWindEvapScale ui)
      sl configMoistEvapNoiseScaleMinusRect configMoistEvapNoiseScaleBarRect configMoistEvapNoiseScalePlusRect specMoistEvapNoiseScale (uiMoistEvapNoiseScale ui)
      sl configMoistBareEvapFracMinusRect configMoistBareEvapFracBarRect configMoistBareEvapFracPlusRect specMoistBareEvapFrac (uiMoistBareEvapFrac ui)
      sl configMoistVegTranspFracMinusRect configMoistVegTranspFracBarRect configMoistVegTranspFracPlusRect specMoistVegTranspFrac (uiMoistVegTranspFrac ui)
      sl configMoistWindETScaleMinusRect configMoistWindETScaleBarRect configMoistWindETScalePlusRect specMoistWindETScale (uiMoistWindETScale ui)
      sl configMoistCondensationRateMinusRect configMoistCondensationRateBarRect configMoistCondensationRatePlusRect specMoistCondensationRate (uiMoistCondensationRate ui)
      sl configMoistRecycleRateMinusRect configMoistRecycleRateBarRect configMoistRecycleRatePlusRect specMoistRecycleRate (uiMoistRecycleRate ui)
      sl configMoistITCZStrengthMinusRect configMoistITCZStrengthBarRect configMoistITCZStrengthPlusRect specMoistITCZStrength (uiMoistITCZStrength ui)
      sl configMoistITCZWidthMinusRect configMoistITCZWidthBarRect configMoistITCZWidthPlusRect specMoistITCZWidth (uiMoistITCZWidth ui)
      sl configOrographicScaleMinusRect configOrographicScaleBarRect configOrographicScalePlusRect specOrographicScale (uiOrographicScale ui)
      sl configOrographicStepMinusRect configOrographicStepBarRect configOrographicStepPlusRect specOrographicStep (uiOrographicStep ui)
      sl configCoastalIterationsMinusRect configCoastalIterationsBarRect configCoastalIterationsPlusRect specCoastalIterations (uiCoastalIterations ui)
      sl configCoastalDiffuseMinusRect configCoastalDiffuseBarRect configCoastalDiffusePlusRect specCoastalDiffuse (uiCoastalDiffuse ui)
      sl configCoastalMoistureBoostMinusRect configCoastalMoistureBoostBarRect configCoastalMoistureBoostPlusRect specCoastalMoistureBoost (uiCoastalMoistureBoost ui)
      sl configWindBeltStrengthMinusRect configWindBeltStrengthBarRect configWindBeltStrengthPlusRect specWindBeltStrength (uiWindBeltStrength ui)
      sl configWindBeltHarmonicsMinusRect configWindBeltHarmonicsBarRect configWindBeltHarmonicsPlusRect specWindBeltHarmonics (uiWindBeltHarmonics ui)
      sl configWindBeltBaseMinusRect configWindBeltBaseBarRect configWindBeltBasePlusRect specWindBeltBase (uiWindBeltBase ui)
      sl configWindBeltRangeMinusRect configWindBeltRangeBarRect configWindBeltRangePlusRect specWindBeltRange (uiWindBeltRange ui)
      sl configWindBeltSpeedScaleMinusRect configWindBeltSpeedScaleBarRect configWindBeltSpeedScalePlusRect specWindBeltSpeedScale (uiWindBeltSpeedScale ui)
      sl configBndLandRangeMinusRect configBndLandRangeBarRect configBndLandRangePlusRect specBndLandRange (uiBndLandRange ui)
      sl configPiedmontSmoothMinusRect configPiedmontSmoothBarRect configPiedmontSmoothPlusRect specPiedmontSmooth (uiPiedmontSmooth ui)
      sl configPiedmontSlopeMinMinusRect configPiedmontSlopeMinBarRect configPiedmontSlopeMinPlusRect specPiedmontSlopeMin (uiPiedmontSlopeMin ui)
      sl configPiedmontSlopeMaxMinusRect configPiedmontSlopeMaxBarRect configPiedmontSlopeMaxPlusRect specPiedmontSlopeMax (uiPiedmontSlopeMax ui)
      sl configWindCoriolisDeflectionMinusRect configWindCoriolisDeflectionBarRect configWindCoriolisDeflectionPlusRect specWindCoriolisDeflection (uiWindCoriolisDeflection ui)
      sl configMoistMinVegFloorMinusRect configMoistMinVegFloorBarRect configMoistMinVegFloorPlusRect specMoistMinVegFloor (uiMoistMinVegFloor ui)

    ConfigPlanet -> do
      sl configPlanetRadiusMinusRect configPlanetRadiusBarRect configPlanetRadiusPlusRect specPlanetRadius (uiPlanetRadius ui)
      sl configAxialTiltMinusRect configAxialTiltBarRect configAxialTiltPlusRect specAxialTilt (uiAxialTilt ui)
      sl configInsolationMinusRect configInsolationBarRect configInsolationPlusRect specInsolation (uiInsolation ui)
      sl configOccWarmScaleMinusRect configOccWarmScaleBarRect configOccWarmScalePlusRect specOccWarmScale (uiOccWarmScale ui)
      sl configOccColdScaleMinusRect configOccColdScaleBarRect configOccColdScalePlusRect specOccColdScale (uiOccColdScale ui)
      sl configOccLatPeakDegMinusRect configOccLatPeakDegBarRect configOccLatPeakDegPlusRect specOccLatPeakDeg (uiOccLatPeakDeg ui)
      sl configOccLatWidthDegMinusRect configOccLatWidthDegBarRect configOccLatWidthDegPlusRect specOccLatWidthDeg (uiOccLatWidthDeg ui)

    ConfigWeather -> do
      sl configWeatherTickMinusRect configWeatherTickBarRect configWeatherTickPlusRect specWeatherTick (uiWeatherTick ui)
      sl configWeatherPhaseMinusRect configWeatherPhaseBarRect configWeatherPhasePlusRect specWeatherPhase (uiWeatherPhase ui)
      sl configWeatherAmplitudeMinusRect configWeatherAmplitudeBarRect configWeatherAmplitudePlusRect specWeatherAmplitude (uiWeatherAmplitude ui)
      sl configSeasonCycleLengthMinusRect configSeasonCycleLengthBarRect configSeasonCycleLengthPlusRect specSeasonCycleLength (uiSeasonCycleLength ui)
      sl configJitterAmplitudeMinusRect configJitterAmplitudeBarRect configJitterAmplitudePlusRect specJitterAmplitude (uiJitterAmplitude ui)
      sl configPressureBaseMinusRect configPressureBaseBarRect configPressureBasePlusRect specPressureBase (uiPressureBase ui)
      sl configPressureTempScaleMinusRect configPressureTempScaleBarRect configPressureTempScalePlusRect specPressureTempScale (uiPressureTempScale ui)
      sl configPressureCoriolisScaleMinusRect configPressureCoriolisScaleBarRect configPressureCoriolisScalePlusRect specPressureCoriolisScale (uiPressureCoriolisScale ui)
      sl configSeasonalBaseMinusRect configSeasonalBaseBarRect configSeasonalBasePlusRect specSeasonalBase (uiSeasonalBase ui)
      sl configSeasonalRangeMinusRect configSeasonalRangeBarRect configSeasonalRangePlusRect specSeasonalRange (uiSeasonalRange ui)
      sl configHumidityNoiseScaleMinusRect configHumidityNoiseScaleBarRect configHumidityNoiseScalePlusRect specHumidityNoiseScale (uiHumidityNoiseScale ui)
      sl configPrecipNoiseScaleMinusRect configPrecipNoiseScaleBarRect configPrecipNoiseScalePlusRect specPrecipNoiseScale (uiPrecipNoiseScale ui)
      sl configWeatherITCZWidthMinusRect configWeatherITCZWidthBarRect configWeatherITCZWidthPlusRect specWeatherITCZWidth (uiWeatherITCZWidth ui)
      sl configWeatherITCZPrecipBoostMinusRect configWeatherITCZPrecipBoostBarRect configWeatherITCZPrecipBoostPlusRect specWeatherITCZPrecipBoost (uiWeatherITCZPrecipBoost ui)
      sl configPressureHumidityScaleMinusRect configPressureHumidityScaleBarRect configPressureHumidityScalePlusRect specPressureHumidityScale (uiPressureHumidityScale ui)
      sl configPressureGradientWindScaleMinusRect configPressureGradientWindScaleBarRect configPressureGradientWindScalePlusRect specPressureGradientWindScale (uiPressureGradientWindScale ui)
      sl configWindNoiseScaleMinusRect configWindNoiseScaleBarRect configWindNoiseScalePlusRect specWindNoiseScale (uiWindNoiseScale ui)
      sl configITCZMigrationScaleMinusRect configITCZMigrationScaleBarRect configITCZMigrationScalePlusRect specITCZMigrationScale (uiITCZMigrationScale ui)
      sl configCloudRHExponentMinusRect configCloudRHExponentBarRect configCloudRHExponentPlusRect specCloudRHExponent (uiCloudRHExponent ui)
      sl configCloudAlbedoEffectMinusRect configCloudAlbedoEffectBarRect configCloudAlbedoEffectPlusRect specCloudAlbedoEffect (uiCloudAlbedoEffect ui)
      sl configCloudPrecipBoostMinusRect configCloudPrecipBoostBarRect configCloudPrecipBoostPlusRect specCloudPrecipBoost (uiCloudPrecipBoost ui)

    ConfigBiome -> do
      sl configVegBaseMinusRect configVegBaseBarRect configVegBasePlusRect specVegBase (uiVegBase ui)
      sl configVegBoostMinusRect configVegBoostBarRect configVegBoostPlusRect specVegBoost (uiVegBoost ui)
      sl configVegTempWeightMinusRect configVegTempWeightBarRect configVegTempWeightPlusRect specVegTempWeight (uiVegTempWeight ui)
      sl configVegPrecipWeightMinusRect configVegPrecipWeightBarRect configVegPrecipWeightPlusRect specVegPrecipWeight (uiVegPrecipWeight ui)
      sl configBtCoastalBandMinusRect configBtCoastalBandBarRect configBtCoastalBandPlusRect specBtCoastalBand (uiBtCoastalBand ui)
      sl configBtSnowMaxTempMinusRect configBtSnowMaxTempBarRect configBtSnowMaxTempPlusRect specBtSnowMaxTemp (uiBtSnowMaxTemp ui)
      sl configBtAlpineMaxTempMinusRect configBtAlpineMaxTempBarRect configBtAlpineMaxTempPlusRect specBtAlpineMaxTemp (uiBtAlpineMaxTemp ui)
      sl configBtIceCapTempMinusRect configBtIceCapTempBarRect configBtIceCapTempPlusRect specBtIceCapTemp (uiBtIceCapTemp ui)
      sl configBtMontaneMaxTempMinusRect configBtMontaneMaxTempBarRect configBtMontaneMaxTempPlusRect specBtMontaneMaxTemp (uiBtMontaneMaxTemp ui)
      sl configBtMontanePrecipMinusRect configBtMontanePrecipBarRect configBtMontanePrecipPlusRect specBtMontanePrecip (uiBtMontanePrecip ui)
      sl configBtCliffSlopeMinusRect configBtCliffSlopeBarRect configBtCliffSlopePlusRect specBtCliffSlope (uiBtCliffSlope ui)
      sl configBtValleyMoistureMinusRect configBtValleyMoistureBarRect configBtValleyMoisturePlusRect specBtValleyMoisture (uiBtValleyMoisture ui)
      sl configBtDepressionMoistureMinusRect configBtDepressionMoistureBarRect configBtDepressionMoisturePlusRect specBtDepressionMoisture (uiBtDepressionMoisture ui)
      sl configBtPrecipWeightMinusRect configBtPrecipWeightBarRect configBtPrecipWeightPlusRect specBtPrecipWeight (uiBtPrecipWeight ui)
      sl configVbcTempMinMinusRect configVbcTempMinBarRect configVbcTempMinPlusRect specVbcTempMin (uiVbcTempMin ui)
      sl configVbcTempRangeMinusRect configVbcTempRangeBarRect configVbcTempRangePlusRect specVbcTempRange (uiVbcTempRange ui)
      sl configVbcFertilityBoostMinusRect configVbcFertilityBoostBarRect configVbcFertilityBoostPlusRect specVbcFertilityBoost (uiVbcFertilityBoost ui)
      sl configVbcAlbedoBaseMinusRect configVbcAlbedoBaseBarRect configVbcAlbedoBasePlusRect specVbcAlbedoBase (uiVbcAlbedoBase ui)
      sl configVbcAlbedoBareMinusRect configVbcAlbedoBareBarRect configVbcAlbedoBarePlusRect specVbcAlbedoBare (uiVbcAlbedoBare ui)
      sl configVbcAlbedoVegMinusRect configVbcAlbedoVegBarRect configVbcAlbedoVegPlusRect specVbcAlbedoVeg (uiVbcAlbedoVeg ui)
      sl configVbcOceanAlbedoMinusRect configVbcOceanAlbedoBarRect configVbcOceanAlbedoPlusRect specVbcOceanAlbedo (uiVbcOceanAlbedo ui)
      sl configVbcIceAlbedoMinusRect configVbcIceAlbedoBarRect configVbcIceAlbedoPlusRect specVbcIceAlbedo (uiVbcIceAlbedo ui)
      sl configBiomeSmoothingMinusRect configBiomeSmoothingBarRect configBiomeSmoothingPlusRect specBiomeSmoothing (uiBiomeSmoothing ui)
      sl configVolcanicAshBoostMinusRect configVolcanicAshBoostBarRect configVolcanicAshBoostPlusRect specVolcanicAshBoost (uiVolcanicAshBoost ui)
      sl configVolcanicLavaPenaltyMinusRect configVolcanicLavaPenaltyBarRect configVolcanicLavaPenaltyPlusRect specVolcanicLavaPenalty (uiVolcanicLavaPenalty ui)
      sl configBiomeFeedbackBlendMinusRect configBiomeFeedbackBlendBarRect configBiomeFeedbackBlendPlusRect specBiomeFeedbackBlend (uiBiomeFeedbackBlend ui)

    ConfigErosion -> do
      sl configErosionHydraulicMinusRect configErosionHydraulicBarRect configErosionHydraulicPlusRect specErosionHydraulic (uiErosionHydraulic ui)
      sl configErosionThermalMinusRect configErosionThermalBarRect configErosionThermalPlusRect specErosionThermal (uiErosionThermal ui)
      sl configErosionRainRateMinusRect configErosionRainRateBarRect configErosionRainRatePlusRect specErosionRainRate (uiRainRate ui)
      sl configErosionTalusMinusRect configErosionTalusBarRect configErosionTalusPlusRect specErosionTalus (uiErosionTalus ui)
      sl configErosionMaxDropMinusRect configErosionMaxDropBarRect configErosionMaxDropPlusRect specErosionMaxDrop (uiErosionMaxDrop ui)
      -- Label-only sliders (no minus/plus button text in original)
      lbl configErosionHydDepositBarRect specErosionHydDeposit (uiErosionHydDeposit ui)
      lbl configErosionDepositSlopeBarRect specErosionDepositSlope (uiErosionDepositSlope ui)
      lbl configErosionThermDepositBarRect specErosionThermDeposit (uiErosionThermDeposit ui)
      lbl configErosionCoastZoneBarRect specErosionCoastZone (uiErosionCoastZone ui)
      lbl configErosionCoastStrengthBarRect specErosionCoastStrength (uiErosionCoastStrength ui)
      lbl configErosionCoastIterBarRect specErosionCoastIter (uiErosionCoastIter ui)
      sl configHypsometryEnabledMinusRect configHypsometryEnabledBarRect configHypsometryEnabledPlusRect specHypsometryEnabled (uiHypsometryEnabled ui)
      sl configHypsometryLowlandExpMinusRect configHypsometryLowlandExpBarRect configHypsometryLowlandExpPlusRect specHypsometryLowlandExp (uiHypsometryLowlandExp ui)
      sl configHypsometryHighlandExpMinusRect configHypsometryHighlandExpBarRect configHypsometryHighlandExpPlusRect specHypsometryHighlandExp (uiHypsometryHighlandExp ui)
      sl configHypsometryPlateauBreakMinusRect configHypsometryPlateauBreakBarRect configHypsometryPlateauBreakPlusRect specHypsometryPlateauBreak (uiHypsometryPlateauBreak ui)
      sl configHypsometryOceanExpMinusRect configHypsometryOceanExpBarRect configHypsometryOceanExpPlusRect specHypsometryOceanExp (uiHypsometryOceanExp ui)
      sl configHypsometryCoastalRampWidthMinusRect configHypsometryCoastalRampWidthBarRect configHypsometryCoastalRampWidthPlusRect specHypsometryCoastalRampWidth (uiHypsometryCoastalRampWidth ui)
      sl configHypsometryCoastalRampStrMinusRect configHypsometryCoastalRampStrBarRect configHypsometryCoastalRampStrPlusRect specHypsometryCoastalRampStr (uiHypsometryCoastalRampStr ui)
      sl configGlacierSnowTempMinusRect configGlacierSnowTempBarRect configGlacierSnowTempPlusRect specGlacierSnowTemp (uiGlacierSnowTemp ui)
      sl configGlacierSnowRangeMinusRect configGlacierSnowRangeBarRect configGlacierSnowRangePlusRect specGlacierSnowRange (uiGlacierSnowRange ui)
      sl configGlacierMeltTempMinusRect configGlacierMeltTempBarRect configGlacierMeltTempPlusRect specGlacierMeltTemp (uiGlacierMeltTemp ui)
      sl configGlacierMeltRateMinusRect configGlacierMeltRateBarRect configGlacierMeltRatePlusRect specGlacierMeltRate (uiGlacierMeltRate ui)
      sl configGlacierAccumScaleMinusRect configGlacierAccumScaleBarRect configGlacierAccumScalePlusRect specGlacierAccumScale (uiGlacierAccumScale ui)
      sl configGlacierFlowItersMinusRect configGlacierFlowItersBarRect configGlacierFlowItersPlusRect specGlacierFlowIters (uiGlacierFlowIters ui)
      sl configGlacierFlowRateMinusRect configGlacierFlowRateBarRect configGlacierFlowRatePlusRect specGlacierFlowRate (uiGlacierFlowRate ui)
      sl configGlacierErosionScaleMinusRect configGlacierErosionScaleBarRect configGlacierErosionScalePlusRect specGlacierErosionScale (uiGlacierErosionScale ui)
      sl configGlacierCarveScaleMinusRect configGlacierCarveScaleBarRect configGlacierCarveScalePlusRect specGlacierCarveScale (uiGlacierCarveScale ui)
      sl configGlacierDepositScaleMinusRect configGlacierDepositScaleBarRect configGlacierDepositScalePlusRect specGlacierDepositScale (uiGlacierDepositScale ui)
      sl configVentDensityMinusRect configVentDensityBarRect configVentDensityPlusRect specVentDensity (uiVentDensity ui)
      sl configVentThresholdMinusRect configVentThresholdBarRect configVentThresholdPlusRect specVentThreshold (uiVentThreshold ui)
      sl configHotspotScaleMinusRect configHotspotScaleBarRect configHotspotScalePlusRect specHotspotScale (uiHotspotScale ui)
      sl configHotspotThresholdMinusRect configHotspotThresholdBarRect configHotspotThresholdPlusRect specHotspotThreshold (uiHotspotThreshold ui)
      sl configMagmaRechargeMinusRect configMagmaRechargeBarRect configMagmaRechargePlusRect specMagmaRecharge (uiMagmaRecharge ui)
      sl configLavaScaleMinusRect configLavaScaleBarRect configLavaScalePlusRect specLavaScale (uiLavaScale ui)
      sl configAshScaleMinusRect configAshScaleBarRect configAshScalePlusRect specAshScale (uiAshScale ui)
      sl configVolcanicDepositScaleMinusRect configVolcanicDepositScaleBarRect configVolcanicDepositScalePlusRect specVolcanicDepositScale (uiVolcanicDepositScale ui)
      sl configSoilMoistureThresholdMinusRect configSoilMoistureThresholdBarRect configSoilMoistureThresholdPlusRect specSoilMoistureThreshold (uiSoilMoistureThreshold ui)
      sl configSoilHardnessThresholdMinusRect configSoilHardnessThresholdBarRect configSoilHardnessThresholdPlusRect specSoilHardnessThreshold (uiSoilHardnessThreshold ui)
      sl configSoilFertilityMoistWeightMinusRect configSoilFertilityMoistWeightBarRect configSoilFertilityMoistWeightPlusRect specSoilFertilityMoistWeight (uiSoilFertilityMoistWeight ui)
      sl configSoilFertilityDepthWeightMinusRect configSoilFertilityDepthWeightBarRect configSoilFertilityDepthWeightPlusRect specSoilFertilityDepthWeight (uiSoilFertilityDepthWeight ui)
      sl configSinkBreachDepthMinusRect configSinkBreachDepthBarRect configSinkBreachDepthPlusRect specSinkBreachDepth (uiSinkBreachDepth ui)
      sl configStreamPowerMaxErosionMinusRect configStreamPowerMaxErosionBarRect configStreamPowerMaxErosionPlusRect specStreamPowerMaxErosion (uiStreamPowerMaxErosion ui)
      sl configRiverCarveMaxDepthMinusRect configRiverCarveMaxDepthBarRect configRiverCarveMaxDepthPlusRect specRiverCarveMaxDepth (uiRiverCarveMaxDepth ui)
      sl configCoastalErodeStrengthMinusRect configCoastalErodeStrengthBarRect configCoastalErodeStrengthPlusRect specCoastalErodeStrength (uiCoastalErodeStrength ui)
      sl configHydroHardnessWeightMinusRect configHydroHardnessWeightBarRect configHydroHardnessWeightPlusRect specHydroHardnessWeight (uiHydroHardnessWeight ui)
      sl configMinLakeSizeMinusRect configMinLakeSizeBarRect configMinLakeSizePlusRect specMinLakeSize (uiMinLakeSize ui)
      sl configInlandSeaMinSizeMinusRect configInlandSeaMinSizeBarRect configInlandSeaMinSizePlusRect specInlandSeaMinSize (uiInlandSeaMinSize ui)
      sl configRoughnessScaleMinusRect configRoughnessScaleBarRect configRoughnessScalePlusRect specRoughnessScale (uiRoughnessScale ui)

    ConfigPipeline -> do
      let stages = allBuiltinStageIds
          disabled = uiDisabledStages ui
          plugins = uiPluginNames ui
          checkboxSize = 16
          Rect (V2 sx sy, V2 _sw _sh) = scrollArea
          pad = 12
      forM_ (zip [0..] stages) $ \(idx, sid) -> do
        let baseY = sy + configRowTopPad + idx * (rowHeight + gap)
            ry = baseY - scrollY
            isDisabled = Set.member sid disabled
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            name = stageCanonicalName sid
            textColor = if isDisabled
              then V4 120 120 130 180
              else V4 220 220 225 255
        drawTextLine fontCache (V2 labelX labelY) textColor name
      -- Plugin name labels after built-in stages
      let pluginOffset = length stages
      forM_ (zip [0..] plugins) $ \(idx, pName) -> do
        let baseY = sy + configRowTopPad + (pluginOffset + idx) * (rowHeight + gap)
            ry = baseY - scrollY
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            pluginTextColor = V4 190 180 220 255
        drawTextLine fontCache (V2 labelX labelY) pluginTextColor pName
      -- Simulation control labels
      let simOffset = length stages + length plugins
          simWorldReady = maybe False (const True) (uiWorldConfig ui)
          simLabelX = sx + pad + 68
          simAutoLabelX = sx + pad + checkboxSize + 8
          simRateLabelX = sx + pad + 128
          simLabelColor = V4 180 200 220 255
      -- Tick button label
      let tickLabelY = sy + configRowTopPad + simOffset * (rowHeight + gap) - scrollY + 4
          tickLabelColor = if simWorldReady then V4 220 230 240 255 else V4 140 150 165 180
      drawTextLine fontCache (V2 (sx + pad + 8) tickLabelY) tickLabelColor "Tick"
      -- Auto-tick label
      let autoTickLabelY = sy + configRowTopPad + (simOffset + 1) * (rowHeight + gap) - scrollY + 4
      drawTextLine fontCache (V2 simAutoLabelX autoTickLabelY) simLabelColor "Auto-tick"
      -- Tick rate label
      let tickRateLabelY = sy + configRowTopPad + (simOffset + 2) * (rowHeight + gap) - scrollY + 4
          rateText = "Rate: " <> Text.pack (show (round (uiSimTickRate ui * 10) :: Int)) <> "/s"
      drawTextLine fontCache (V2 simRateLabelX tickRateLabelY) simLabelColor rateText

  SDL.rendererClipRect renderer SDL.$= Nothing
