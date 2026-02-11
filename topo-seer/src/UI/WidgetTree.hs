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
  | WidgetConfigTabClimate
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
  | WidgetConfigVegBaseMinus
  | WidgetConfigVegBasePlus
  | WidgetConfigVegBoostMinus
  | WidgetConfigVegBoostPlus
  | WidgetConfigVegTempWeightMinus
  | WidgetConfigVegTempWeightPlus
  | WidgetConfigVegPrecipWeightMinus
  | WidgetConfigVegPrecipWeightPlus
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
  | WidgetConfigSliceLatCenterMinus
  | WidgetConfigSliceLatCenterPlus
  | WidgetConfigSliceLonCenterMinus
  | WidgetConfigSliceLonCenterPlus
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
  let (view1, view2, view3, view4, view5, view6, view7, view8, view9, view10, view11, view12) = viewRects layout
      (logDebug, logInfo, logWarn, logError) = logFilterRects layout
      (tabTerrain, tabClimate, tabErosion) = configTabRects layout
      (leftTabTopo, leftTabView) = leftTabRects layout
  in [ Widget WidgetGenerate (genButtonRect layout)
     , Widget WidgetLeftToggle (leftToggleRect layout)
     , Widget WidgetLeftTabTopo leftTabTopo
     , Widget WidgetLeftTabView leftTabView
      , Widget WidgetSeedValue (configSeedValueRect layout)
      , Widget WidgetSeedRandom (configSeedRandomRect layout)
     , Widget WidgetChunkMinus (chunkMinusRect layout)
     , Widget WidgetChunkPlus (chunkPlusRect layout)
     , Widget WidgetConfigToggle (configToggleRect layout)
     , Widget WidgetConfigTabTerrain tabTerrain
     , Widget WidgetConfigTabClimate tabClimate
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
      , Widget WidgetConfigVegBaseMinus (configVegBaseMinusRect layout)
      , Widget WidgetConfigVegBasePlus (configVegBasePlusRect layout)
      , Widget WidgetConfigVegBoostMinus (configVegBoostMinusRect layout)
      , Widget WidgetConfigVegBoostPlus (configVegBoostPlusRect layout)
      , Widget WidgetConfigVegTempWeightMinus (configVegTempWeightMinusRect layout)
      , Widget WidgetConfigVegTempWeightPlus (configVegTempWeightPlusRect layout)
      , Widget WidgetConfigVegPrecipWeightMinus (configVegPrecipWeightMinusRect layout)
      , Widget WidgetConfigVegPrecipWeightPlus (configVegPrecipWeightPlusRect layout)
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
      , Widget WidgetConfigSliceLatCenterMinus (configSliceLatCenterMinusRect layout)
      , Widget WidgetConfigSliceLatCenterPlus (configSliceLatCenterPlusRect layout)
      , Widget WidgetConfigSliceLonCenterMinus (configSliceLonCenterMinusRect layout)
      , Widget WidgetConfigSliceLonCenterPlus (configSliceLonCenterPlusRect layout)
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
buildSliderRowWidgets :: Layout -> ([Widget], [Widget], [Widget])
buildSliderRowWidgets layout = (terrain, climate, erosion)
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
      , row WidgetConfigGenCoordScaleMinus 34
      , row WidgetConfigGenOffsetXMinus 35
      , row WidgetConfigGenOffsetYMinus 36
      , row WidgetConfigExtentXMinus 37
      , row WidgetConfigExtentYMinus 38
      , row WidgetConfigEdgeNorthMinus 39
      , row WidgetConfigEdgeSouthMinus 40
      , row WidgetConfigEdgeEastMinus 41
      , row WidgetConfigEdgeWestMinus 42
      , row WidgetConfigEdgeFalloffMinus 43
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
      , row WidgetConfigWeatherTickMinus 9
      , row WidgetConfigWeatherPhaseMinus 10
      , row WidgetConfigWeatherAmplitudeMinus 11
      , row WidgetConfigVegBaseMinus 12
      , row WidgetConfigVegBoostMinus 13
      , row WidgetConfigVegTempWeightMinus 14
      , row WidgetConfigVegPrecipWeightMinus 15
      , row WidgetConfigBoundaryMotionTempMinus 16
      , row WidgetConfigBoundaryMotionPrecipMinus 17
      , row WidgetConfigPlanetRadiusMinus 18
      , row WidgetConfigAxialTiltMinus 19
      , row WidgetConfigInsolationMinus 20
      , row WidgetConfigSliceLatCenterMinus 21
      , row WidgetConfigSliceLonCenterMinus 22
      ]

    erosion =
      [ row WidgetConfigErosionHydraulicMinus 0
      , row WidgetConfigErosionThermalMinus 1
      , row WidgetConfigErosionRainRateMinus 2
      , row WidgetConfigErosionTalusMinus 3
      , row WidgetConfigErosionMaxDropMinus 4
      ]

hitTest :: [Widget] -> V2 Int -> Maybe WidgetId
hitTest widgets point =
  case filter (\w -> containsPoint (widgetRect w) point) widgets of
    (w:_) -> Just (widgetId w)
    [] -> Nothing
