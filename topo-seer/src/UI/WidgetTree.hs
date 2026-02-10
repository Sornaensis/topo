module UI.WidgetTree
  ( WidgetId(..)
  , Widget(..)
  , buildWidgets
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
  | WidgetConfigApply
  | WidgetConfigReplay
  | WidgetConfigReset
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
  | WidgetConfigLatitudeBiasMinus
  | WidgetConfigLatitudeBiasPlus
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
  | WidgetConfigSliceLatExtentMinus
  | WidgetConfigSliceLatExtentPlus
  | WidgetConfigSliceLonCenterMinus
  | WidgetConfigSliceLonCenterPlus
  | WidgetConfigSliceLonExtentMinus
  | WidgetConfigSliceLonExtentPlus
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
     , Widget WidgetConfigApply (configApplyRect layout)
    , Widget WidgetConfigReplay (configReplayRect layout)
     , Widget WidgetConfigReset (configResetRect layout)
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
      , Widget WidgetConfigLatitudeBiasMinus (configLatitudeBiasMinusRect layout)
      , Widget WidgetConfigLatitudeBiasPlus (configLatitudeBiasPlusRect layout)
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
      , Widget WidgetConfigSliceLatExtentMinus (configSliceLatExtentMinusRect layout)
      , Widget WidgetConfigSliceLatExtentPlus (configSliceLatExtentPlusRect layout)
      , Widget WidgetConfigSliceLonCenterMinus (configSliceLonCenterMinusRect layout)
      , Widget WidgetConfigSliceLonCenterPlus (configSliceLonCenterPlusRect layout)
      , Widget WidgetConfigSliceLonExtentMinus (configSliceLonExtentMinusRect layout)
      , Widget WidgetConfigSliceLonExtentPlus (configSliceLonExtentPlusRect layout)
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
     ]

hitTest :: [Widget] -> V2 Int -> Maybe WidgetId
hitTest widgets point =
  case filter (\w -> containsPoint (widgetRect w) point) widgets of
    (w:_) -> Just (widgetId w)
    [] -> Nothing
