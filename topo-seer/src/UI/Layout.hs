module UI.Layout
  ( Layout
  , topBarHeight
  , topBarRect
  , layoutFor
  , layoutForSeed
  , genButtonRect
  , leftGenButtonRect
  , seedMinusRect
  , seedPlusRect
  , seedValueRect
  , seedRandomRect
  , leftPanelRect
  , leftToggleRect
  , leftTabRects
  , configSeedValueRect
  , configSeedLabelRect
  , configSeedRandomRect
  , chunkMinusRect
  , chunkPlusRect
  , viewRects
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
  , configEvapMinusRect
  , configEvapPlusRect
  , configEvapBarRect
  , configRainShadowMinusRect
  , configRainShadowPlusRect
  , configRainShadowBarRect
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
  , configSliceLatCenterMinusRect
  , configSliceLatCenterPlusRect
  , configSliceLatCenterBarRect
  , configSliceLonCenterMinusRect
  , configSliceLonCenterPlusRect
  , configSliceLonCenterBarRect
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

-- | Legacy generate button rect (now redirects to leftGenButtonRect).
genButtonRect :: Layout -> Rect
genButtonRect = leftGenButtonRect

-- | Generate button inside the left panel, below seed value (Row 4).
leftGenButtonRect :: Layout -> Rect
leftGenButtonRect layout =
  let Rect (V2 x _y, V2 w _) = leftPanelRect layout
      pad = 12
      rowHeight = 24
      gap = 10
      top = leftControlsTop layout + 4 * (rowHeight + gap)
  in Rect (V2 (x + pad) top, V2 (w - pad * 2) 28)

seedMinusRect :: Layout -> Rect
seedMinusRect = leftSeedMinusRect

seedPlusRect :: Layout -> Rect
seedPlusRect = leftSeedPlusRect

seedValueRect :: Layout -> Rect
seedValueRect = leftSeedValueRect

seedRandomRect :: Layout -> Rect
seedRandomRect = leftSeedRandomRect

chunkMinusRect :: Layout -> Rect
chunkMinusRect = leftChunkMinusRect

chunkPlusRect :: Layout -> Rect
chunkPlusRect = leftChunkPlusRect

viewRects :: Layout -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
viewRects = leftViewRects

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

configTabRects :: Layout -> (Rect, Rect, Rect)
configTabRects layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      pad = 12
      tabH = 22
      gap = 6
      toggleH = 22
      available = w - pad * 2 - gap * 2
      tabW = available `div` 3
      y0 = y + 8 + toggleH + 8
      r1 = Rect (V2 (x + pad) y0, V2 tabW tabH)
      r2 = Rect (V2 (x + pad + tabW + gap) y0, V2 tabW tabH)
      r3 = Rect (V2 (x + pad + (tabW + gap) * 2) y0, V2 tabW tabH)
  in (r1, r2, r3)

configWaterMinusRect :: Layout -> Rect
configWaterMinusRect = configParamMinusRect 0

configWaterPlusRect :: Layout -> Rect
configWaterPlusRect = configParamPlusRect 0

configWaterBarRect :: Layout -> Rect
configWaterBarRect = configParamBarRect 0

configEvapMinusRect :: Layout -> Rect
configEvapMinusRect = configParamMinusRect 1

configEvapPlusRect :: Layout -> Rect
configEvapPlusRect = configParamPlusRect 1

configEvapBarRect :: Layout -> Rect
configEvapBarRect = configParamBarRect 1

configRainShadowMinusRect :: Layout -> Rect
configRainShadowMinusRect = configParamMinusRect 2

configRainShadowPlusRect :: Layout -> Rect
configRainShadowPlusRect = configParamPlusRect 2

configRainShadowBarRect :: Layout -> Rect
configRainShadowBarRect = configParamBarRect 2

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
configWeatherTickMinusRect = configParamMinusRect 9

configWeatherTickPlusRect :: Layout -> Rect
configWeatherTickPlusRect = configParamPlusRect 9

configWeatherTickBarRect :: Layout -> Rect
configWeatherTickBarRect = configParamBarRect 9

configWeatherPhaseMinusRect :: Layout -> Rect
configWeatherPhaseMinusRect = configParamMinusRect 10

configWeatherPhasePlusRect :: Layout -> Rect
configWeatherPhasePlusRect = configParamPlusRect 10

configWeatherPhaseBarRect :: Layout -> Rect
configWeatherPhaseBarRect = configParamBarRect 10

configWeatherAmplitudeMinusRect :: Layout -> Rect
configWeatherAmplitudeMinusRect = configParamMinusRect 11

configWeatherAmplitudePlusRect :: Layout -> Rect
configWeatherAmplitudePlusRect = configParamPlusRect 11

configWeatherAmplitudeBarRect :: Layout -> Rect
configWeatherAmplitudeBarRect = configParamBarRect 11

configVegBaseMinusRect :: Layout -> Rect
configVegBaseMinusRect = configParamMinusRect 12

configVegBasePlusRect :: Layout -> Rect
configVegBasePlusRect = configParamPlusRect 12

configVegBaseBarRect :: Layout -> Rect
configVegBaseBarRect = configParamBarRect 12

configVegBoostMinusRect :: Layout -> Rect
configVegBoostMinusRect = configParamMinusRect 13

configVegBoostPlusRect :: Layout -> Rect
configVegBoostPlusRect = configParamPlusRect 13

configVegBoostBarRect :: Layout -> Rect
configVegBoostBarRect = configParamBarRect 13

configVegTempWeightMinusRect :: Layout -> Rect
configVegTempWeightMinusRect = configParamMinusRect 14

configVegTempWeightPlusRect :: Layout -> Rect
configVegTempWeightPlusRect = configParamPlusRect 14

configVegTempWeightBarRect :: Layout -> Rect
configVegTempWeightBarRect = configParamBarRect 14

configVegPrecipWeightMinusRect :: Layout -> Rect
configVegPrecipWeightMinusRect = configParamMinusRect 15

configVegPrecipWeightPlusRect :: Layout -> Rect
configVegPrecipWeightPlusRect = configParamPlusRect 15

configVegPrecipWeightBarRect :: Layout -> Rect
configVegPrecipWeightBarRect = configParamBarRect 15

configBoundaryMotionTempMinusRect :: Layout -> Rect
configBoundaryMotionTempMinusRect = configParamMinusRect 16

configBoundaryMotionTempPlusRect :: Layout -> Rect
configBoundaryMotionTempPlusRect = configParamPlusRect 16

configBoundaryMotionTempBarRect :: Layout -> Rect
configBoundaryMotionTempBarRect = configParamBarRect 16

configBoundaryMotionPrecipMinusRect :: Layout -> Rect
configBoundaryMotionPrecipMinusRect = configParamMinusRect 17

configBoundaryMotionPrecipPlusRect :: Layout -> Rect
configBoundaryMotionPrecipPlusRect = configParamPlusRect 17

configBoundaryMotionPrecipBarRect :: Layout -> Rect
configBoundaryMotionPrecipBarRect = configParamBarRect 17

-- Planet / Slice sliders (climate tab rows 18–22)

configPlanetRadiusMinusRect :: Layout -> Rect
configPlanetRadiusMinusRect = configParamMinusRect 18

configPlanetRadiusPlusRect :: Layout -> Rect
configPlanetRadiusPlusRect = configParamPlusRect 18

configPlanetRadiusBarRect :: Layout -> Rect
configPlanetRadiusBarRect = configParamBarRect 18

configAxialTiltMinusRect :: Layout -> Rect
configAxialTiltMinusRect = configParamMinusRect 19

configAxialTiltPlusRect :: Layout -> Rect
configAxialTiltPlusRect = configParamPlusRect 19

configAxialTiltBarRect :: Layout -> Rect
configAxialTiltBarRect = configParamBarRect 19

configInsolationMinusRect :: Layout -> Rect
configInsolationMinusRect = configParamMinusRect 20

configInsolationPlusRect :: Layout -> Rect
configInsolationPlusRect = configParamPlusRect 20

configInsolationBarRect :: Layout -> Rect
configInsolationBarRect = configParamBarRect 20

configSliceLatCenterMinusRect :: Layout -> Rect
configSliceLatCenterMinusRect = configParamMinusRect 21

configSliceLatCenterPlusRect :: Layout -> Rect
configSliceLatCenterPlusRect = configParamPlusRect 21

configSliceLatCenterBarRect :: Layout -> Rect
configSliceLatCenterBarRect = configParamBarRect 21

configSliceLonCenterMinusRect :: Layout -> Rect
configSliceLonCenterMinusRect = configParamMinusRect 22

configSliceLonCenterPlusRect :: Layout -> Rect
configSliceLonCenterPlusRect = configParamPlusRect 22

configSliceLonCenterBarRect :: Layout -> Rect
configSliceLonCenterBarRect = configParamBarRect 22

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

configGenScaleMinusRect :: Layout -> Rect
configGenScaleMinusRect = configParamMinusRect 0

configGenScalePlusRect :: Layout -> Rect
configGenScalePlusRect = configParamPlusRect 0

configGenScaleBarRect :: Layout -> Rect
configGenScaleBarRect = configParamBarRect 0

configGenCoordScaleMinusRect :: Layout -> Rect
configGenCoordScaleMinusRect = configParamMinusRect 34

configGenCoordScalePlusRect :: Layout -> Rect
configGenCoordScalePlusRect = configParamPlusRect 34

configGenCoordScaleBarRect :: Layout -> Rect
configGenCoordScaleBarRect = configParamBarRect 34

configGenOffsetXMinusRect :: Layout -> Rect
configGenOffsetXMinusRect = configParamMinusRect 35

configGenOffsetXPlusRect :: Layout -> Rect
configGenOffsetXPlusRect = configParamPlusRect 35

configGenOffsetXBarRect :: Layout -> Rect
configGenOffsetXBarRect = configParamBarRect 35

configGenOffsetYMinusRect :: Layout -> Rect
configGenOffsetYMinusRect = configParamMinusRect 36

configGenOffsetYPlusRect :: Layout -> Rect
configGenOffsetYPlusRect = configParamPlusRect 36

configGenOffsetYBarRect :: Layout -> Rect
configGenOffsetYBarRect = configParamBarRect 36

configGenFrequencyMinusRect :: Layout -> Rect
configGenFrequencyMinusRect = configParamMinusRect 1

configGenFrequencyPlusRect :: Layout -> Rect
configGenFrequencyPlusRect = configParamPlusRect 1

configGenFrequencyBarRect :: Layout -> Rect
configGenFrequencyBarRect = configParamBarRect 1

configGenOctavesMinusRect :: Layout -> Rect
configGenOctavesMinusRect = configParamMinusRect 2

configGenOctavesPlusRect :: Layout -> Rect
configGenOctavesPlusRect = configParamPlusRect 2

configGenOctavesBarRect :: Layout -> Rect
configGenOctavesBarRect = configParamBarRect 2

configGenLacunarityMinusRect :: Layout -> Rect
configGenLacunarityMinusRect = configParamMinusRect 3

configGenLacunarityPlusRect :: Layout -> Rect
configGenLacunarityPlusRect = configParamPlusRect 3

configGenLacunarityBarRect :: Layout -> Rect
configGenLacunarityBarRect = configParamBarRect 3

configGenGainMinusRect :: Layout -> Rect
configGenGainMinusRect = configParamMinusRect 4

configGenGainPlusRect :: Layout -> Rect
configGenGainPlusRect = configParamPlusRect 4

configGenGainBarRect :: Layout -> Rect
configGenGainBarRect = configParamBarRect 4

configGenWarpScaleMinusRect :: Layout -> Rect
configGenWarpScaleMinusRect = configParamMinusRect 5

configGenWarpScalePlusRect :: Layout -> Rect
configGenWarpScalePlusRect = configParamPlusRect 5

configGenWarpScaleBarRect :: Layout -> Rect
configGenWarpScaleBarRect = configParamBarRect 5

configGenWarpStrengthMinusRect :: Layout -> Rect
configGenWarpStrengthMinusRect = configParamMinusRect 6

configGenWarpStrengthPlusRect :: Layout -> Rect
configGenWarpStrengthPlusRect = configParamPlusRect 6

configGenWarpStrengthBarRect :: Layout -> Rect
configGenWarpStrengthBarRect = configParamBarRect 6

configExtentXMinusRect :: Layout -> Rect
configExtentXMinusRect = configParamMinusRect 37

configExtentXPlusRect :: Layout -> Rect
configExtentXPlusRect = configParamPlusRect 37

configExtentXBarRect :: Layout -> Rect
configExtentXBarRect = configParamBarRect 37

configExtentYMinusRect :: Layout -> Rect
configExtentYMinusRect = configParamMinusRect 38

configExtentYPlusRect :: Layout -> Rect
configExtentYPlusRect = configParamPlusRect 38

configExtentYBarRect :: Layout -> Rect
configExtentYBarRect = configParamBarRect 38

configEdgeNorthMinusRect :: Layout -> Rect
configEdgeNorthMinusRect = configParamMinusRect 39

configEdgeNorthPlusRect :: Layout -> Rect
configEdgeNorthPlusRect = configParamPlusRect 39

configEdgeNorthBarRect :: Layout -> Rect
configEdgeNorthBarRect = configParamBarRect 39

configEdgeSouthMinusRect :: Layout -> Rect
configEdgeSouthMinusRect = configParamMinusRect 40

configEdgeSouthPlusRect :: Layout -> Rect
configEdgeSouthPlusRect = configParamPlusRect 40

configEdgeSouthBarRect :: Layout -> Rect
configEdgeSouthBarRect = configParamBarRect 40

configEdgeEastMinusRect :: Layout -> Rect
configEdgeEastMinusRect = configParamMinusRect 41

configEdgeEastPlusRect :: Layout -> Rect
configEdgeEastPlusRect = configParamPlusRect 41

configEdgeEastBarRect :: Layout -> Rect
configEdgeEastBarRect = configParamBarRect 41

configEdgeWestMinusRect :: Layout -> Rect
configEdgeWestMinusRect = configParamMinusRect 42

configEdgeWestPlusRect :: Layout -> Rect
configEdgeWestPlusRect = configParamPlusRect 42

configEdgeWestBarRect :: Layout -> Rect
configEdgeWestBarRect = configParamBarRect 42

configEdgeFalloffMinusRect :: Layout -> Rect
configEdgeFalloffMinusRect = configParamMinusRect 43

configEdgeFalloffPlusRect :: Layout -> Rect
configEdgeFalloffPlusRect = configParamPlusRect 43

configEdgeFalloffBarRect :: Layout -> Rect
configEdgeFalloffBarRect = configParamBarRect 43

configPlateSizeMinusRect :: Layout -> Rect
configPlateSizeMinusRect = configParamMinusRect 7

configPlateSizePlusRect :: Layout -> Rect
configPlateSizePlusRect = configParamPlusRect 7

configPlateSizeBarRect :: Layout -> Rect
configPlateSizeBarRect = configParamBarRect 7

configUpliftMinusRect :: Layout -> Rect
configUpliftMinusRect = configParamMinusRect 8

configUpliftPlusRect :: Layout -> Rect
configUpliftPlusRect = configParamPlusRect 8

configUpliftBarRect :: Layout -> Rect
configUpliftBarRect = configParamBarRect 8

configRiftDepthMinusRect :: Layout -> Rect
configRiftDepthMinusRect = configParamMinusRect 9

configRiftDepthPlusRect :: Layout -> Rect
configRiftDepthPlusRect = configParamPlusRect 9

configRiftDepthBarRect :: Layout -> Rect
configRiftDepthBarRect = configParamBarRect 9

configDetailScaleMinusRect :: Layout -> Rect
configDetailScaleMinusRect = configParamMinusRect 10

configDetailScalePlusRect :: Layout -> Rect
configDetailScalePlusRect = configParamPlusRect 10

configDetailScaleBarRect :: Layout -> Rect
configDetailScaleBarRect = configParamBarRect 10

configPlateSpeedMinusRect :: Layout -> Rect
configPlateSpeedMinusRect = configParamMinusRect 11

configPlateSpeedPlusRect :: Layout -> Rect
configPlateSpeedPlusRect = configParamPlusRect 11

configPlateSpeedBarRect :: Layout -> Rect
configPlateSpeedBarRect = configParamBarRect 11

configBoundarySharpnessMinusRect :: Layout -> Rect
configBoundarySharpnessMinusRect = configParamMinusRect 12

configBoundarySharpnessPlusRect :: Layout -> Rect
configBoundarySharpnessPlusRect = configParamPlusRect 12

configBoundarySharpnessBarRect :: Layout -> Rect
configBoundarySharpnessBarRect = configParamBarRect 12

configBoundaryNoiseScaleMinusRect :: Layout -> Rect
configBoundaryNoiseScaleMinusRect = configParamMinusRect 13

configBoundaryNoiseScalePlusRect :: Layout -> Rect
configBoundaryNoiseScalePlusRect = configParamPlusRect 13

configBoundaryNoiseScaleBarRect :: Layout -> Rect
configBoundaryNoiseScaleBarRect = configParamBarRect 13

configBoundaryNoiseStrengthMinusRect :: Layout -> Rect
configBoundaryNoiseStrengthMinusRect = configParamMinusRect 14

configBoundaryNoiseStrengthPlusRect :: Layout -> Rect
configBoundaryNoiseStrengthPlusRect = configParamPlusRect 14

configBoundaryNoiseStrengthBarRect :: Layout -> Rect
configBoundaryNoiseStrengthBarRect = configParamBarRect 14

configBoundaryWarpOctavesMinusRect :: Layout -> Rect
configBoundaryWarpOctavesMinusRect = configParamMinusRect 15

configBoundaryWarpOctavesPlusRect :: Layout -> Rect
configBoundaryWarpOctavesPlusRect = configParamPlusRect 15

configBoundaryWarpOctavesBarRect :: Layout -> Rect
configBoundaryWarpOctavesBarRect = configParamBarRect 15

configBoundaryWarpLacunarityMinusRect :: Layout -> Rect
configBoundaryWarpLacunarityMinusRect = configParamMinusRect 16

configBoundaryWarpLacunarityPlusRect :: Layout -> Rect
configBoundaryWarpLacunarityPlusRect = configParamPlusRect 16

configBoundaryWarpLacunarityBarRect :: Layout -> Rect
configBoundaryWarpLacunarityBarRect = configParamBarRect 16

configBoundaryWarpGainMinusRect :: Layout -> Rect
configBoundaryWarpGainMinusRect = configParamMinusRect 17

configBoundaryWarpGainPlusRect :: Layout -> Rect
configBoundaryWarpGainPlusRect = configParamPlusRect 17

configBoundaryWarpGainBarRect :: Layout -> Rect
configBoundaryWarpGainBarRect = configParamBarRect 17

configPlateMergeScaleMinusRect :: Layout -> Rect
configPlateMergeScaleMinusRect = configParamMinusRect 18

configPlateMergeScalePlusRect :: Layout -> Rect
configPlateMergeScalePlusRect = configParamPlusRect 18

configPlateMergeScaleBarRect :: Layout -> Rect
configPlateMergeScaleBarRect = configParamBarRect 18

configPlateMergeBiasMinusRect :: Layout -> Rect
configPlateMergeBiasMinusRect = configParamMinusRect 19

configPlateMergeBiasPlusRect :: Layout -> Rect
configPlateMergeBiasPlusRect = configParamPlusRect 19

configPlateMergeBiasBarRect :: Layout -> Rect
configPlateMergeBiasBarRect = configParamBarRect 19

configPlateDetailScaleMinusRect :: Layout -> Rect
configPlateDetailScaleMinusRect = configParamMinusRect 20

configPlateDetailScalePlusRect :: Layout -> Rect
configPlateDetailScalePlusRect = configParamPlusRect 20

configPlateDetailScaleBarRect :: Layout -> Rect
configPlateDetailScaleBarRect = configParamBarRect 20

configPlateDetailStrengthMinusRect :: Layout -> Rect
configPlateDetailStrengthMinusRect = configParamMinusRect 21

configPlateDetailStrengthPlusRect :: Layout -> Rect
configPlateDetailStrengthPlusRect = configParamPlusRect 21

configPlateDetailStrengthBarRect :: Layout -> Rect
configPlateDetailStrengthBarRect = configParamBarRect 21

configPlateRidgeStrengthMinusRect :: Layout -> Rect
configPlateRidgeStrengthMinusRect = configParamMinusRect 22

configPlateRidgeStrengthPlusRect :: Layout -> Rect
configPlateRidgeStrengthPlusRect = configParamPlusRect 22

configPlateRidgeStrengthBarRect :: Layout -> Rect
configPlateRidgeStrengthBarRect = configParamBarRect 22

configPlateHeightBaseMinusRect :: Layout -> Rect
configPlateHeightBaseMinusRect = configParamMinusRect 23

configPlateHeightBasePlusRect :: Layout -> Rect
configPlateHeightBasePlusRect = configParamPlusRect 23

configPlateHeightBaseBarRect :: Layout -> Rect
configPlateHeightBaseBarRect = configParamBarRect 23

configPlateHeightVarianceMinusRect :: Layout -> Rect
configPlateHeightVarianceMinusRect = configParamMinusRect 24

configPlateHeightVariancePlusRect :: Layout -> Rect
configPlateHeightVariancePlusRect = configParamPlusRect 24

configPlateHeightVarianceBarRect :: Layout -> Rect
configPlateHeightVarianceBarRect = configParamBarRect 24

configPlateHardnessBaseMinusRect :: Layout -> Rect
configPlateHardnessBaseMinusRect = configParamMinusRect 25

configPlateHardnessBasePlusRect :: Layout -> Rect
configPlateHardnessBasePlusRect = configParamPlusRect 25

configPlateHardnessBaseBarRect :: Layout -> Rect
configPlateHardnessBaseBarRect = configParamBarRect 25

configPlateHardnessVarianceMinusRect :: Layout -> Rect
configPlateHardnessVarianceMinusRect = configParamMinusRect 26

configPlateHardnessVariancePlusRect :: Layout -> Rect
configPlateHardnessVariancePlusRect = configParamPlusRect 26

configPlateHardnessVarianceBarRect :: Layout -> Rect
configPlateHardnessVarianceBarRect = configParamBarRect 26

configTrenchDepthMinusRect :: Layout -> Rect
configTrenchDepthMinusRect = configParamMinusRect 27

configTrenchDepthPlusRect :: Layout -> Rect
configTrenchDepthPlusRect = configParamPlusRect 27

configTrenchDepthBarRect :: Layout -> Rect
configTrenchDepthBarRect = configParamBarRect 27

configRidgeHeightMinusRect :: Layout -> Rect
configRidgeHeightMinusRect = configParamMinusRect 28

configRidgeHeightPlusRect :: Layout -> Rect
configRidgeHeightPlusRect = configParamPlusRect 28

configRidgeHeightBarRect :: Layout -> Rect
configRidgeHeightBarRect = configParamBarRect 28

configPlateBiasStrengthMinusRect :: Layout -> Rect
configPlateBiasStrengthMinusRect = configParamMinusRect 29

configPlateBiasStrengthPlusRect :: Layout -> Rect
configPlateBiasStrengthPlusRect = configParamPlusRect 29

configPlateBiasStrengthBarRect :: Layout -> Rect
configPlateBiasStrengthBarRect = configParamBarRect 29

configPlateBiasCenterMinusRect :: Layout -> Rect
configPlateBiasCenterMinusRect = configParamMinusRect 30

configPlateBiasCenterPlusRect :: Layout -> Rect
configPlateBiasCenterPlusRect = configParamPlusRect 30

configPlateBiasCenterBarRect :: Layout -> Rect
configPlateBiasCenterBarRect = configParamBarRect 30

configPlateBiasEdgeMinusRect :: Layout -> Rect
configPlateBiasEdgeMinusRect = configParamMinusRect 31

configPlateBiasEdgePlusRect :: Layout -> Rect
configPlateBiasEdgePlusRect = configParamPlusRect 31

configPlateBiasEdgeBarRect :: Layout -> Rect
configPlateBiasEdgeBarRect = configParamBarRect 31

configPlateBiasNorthMinusRect :: Layout -> Rect
configPlateBiasNorthMinusRect = configParamMinusRect 32

configPlateBiasNorthPlusRect :: Layout -> Rect
configPlateBiasNorthPlusRect = configParamPlusRect 32

configPlateBiasNorthBarRect :: Layout -> Rect
configPlateBiasNorthBarRect = configParamBarRect 32

configPlateBiasSouthMinusRect :: Layout -> Rect
configPlateBiasSouthMinusRect = configParamMinusRect 33

configPlateBiasSouthPlusRect :: Layout -> Rect
configPlateBiasSouthPlusRect = configParamPlusRect 33

configPlateBiasSouthBarRect :: Layout -> Rect
configPlateBiasSouthBarRect = configParamBarRect 33

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
