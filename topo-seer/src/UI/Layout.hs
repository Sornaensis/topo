module UI.Layout
  ( Layout
  , layoutFor
  , layoutForSeed
  , genButtonRect
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
  , configApplyRect
  , configReplayRect
  , configResetRect
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
  , configLatitudeBiasMinusRect
  , configLatitudeBiasPlusRect
  , configLatitudeBiasBarRect
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
  , configSliceLatExtentMinusRect
  , configSliceLatExtentPlusRect
  , configSliceLatExtentBarRect
  , configSliceLonCenterMinusRect
  , configSliceLonCenterPlusRect
  , configSliceLonCenterBarRect
  , configSliceLonExtentMinusRect
  , configSliceLonExtentPlusRect
  , configSliceLonExtentBarRect
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
  ) where

import Linear (V2(..))
import UI.Widgets (Rect(..))

data Layout = Layout
  { layoutSize :: V2 Int
  , layoutLogHeight :: Int
  , layoutSeedWidth :: Int
  } deriving (Eq, Show)

layoutFor :: V2 Int -> Int -> Layout
layoutFor size logHeight = layoutForSeed size logHeight 120

layoutForSeed :: V2 Int -> Int -> Int -> Layout
layoutForSeed size logHeight seedWidth = Layout
  { layoutSize = size
  , layoutLogHeight = logHeight
  , layoutSeedWidth = seedWidth
  }

genButtonRect :: Layout -> Rect
genButtonRect _ = Rect (V2 16 16, V2 140 32)

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
      y = 56
  in Rect (V2 x y, V2 panelW (h - logHeight - 72))

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
      minW = 240
      desiredW = max minW (pad * 2 + seedWidth + buttonW + 20)
      panelX = w - desiredW - 16
  in Rect (V2 panelX 16, V2 desiredW (h - logHeight - 32))

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

configLatitudeBiasMinusRect :: Layout -> Rect
configLatitudeBiasMinusRect = configParamMinusRect 7

configLatitudeBiasPlusRect :: Layout -> Rect
configLatitudeBiasPlusRect = configParamPlusRect 7

configLatitudeBiasBarRect :: Layout -> Rect
configLatitudeBiasBarRect = configParamBarRect 7

configWindIterationsMinusRect :: Layout -> Rect
configWindIterationsMinusRect = configParamMinusRect 8

configWindIterationsPlusRect :: Layout -> Rect
configWindIterationsPlusRect = configParamPlusRect 8

configWindIterationsBarRect :: Layout -> Rect
configWindIterationsBarRect = configParamBarRect 8

configMoistureIterationsMinusRect :: Layout -> Rect
configMoistureIterationsMinusRect = configParamMinusRect 9

configMoistureIterationsPlusRect :: Layout -> Rect
configMoistureIterationsPlusRect = configParamPlusRect 9

configMoistureIterationsBarRect :: Layout -> Rect
configMoistureIterationsBarRect = configParamBarRect 9

configWeatherTickMinusRect :: Layout -> Rect
configWeatherTickMinusRect = configParamMinusRect 10

configWeatherTickPlusRect :: Layout -> Rect
configWeatherTickPlusRect = configParamPlusRect 10

configWeatherTickBarRect :: Layout -> Rect
configWeatherTickBarRect = configParamBarRect 10

configWeatherPhaseMinusRect :: Layout -> Rect
configWeatherPhaseMinusRect = configParamMinusRect 11

configWeatherPhasePlusRect :: Layout -> Rect
configWeatherPhasePlusRect = configParamPlusRect 11

configWeatherPhaseBarRect :: Layout -> Rect
configWeatherPhaseBarRect = configParamBarRect 11

configWeatherAmplitudeMinusRect :: Layout -> Rect
configWeatherAmplitudeMinusRect = configParamMinusRect 12

configWeatherAmplitudePlusRect :: Layout -> Rect
configWeatherAmplitudePlusRect = configParamPlusRect 12

configWeatherAmplitudeBarRect :: Layout -> Rect
configWeatherAmplitudeBarRect = configParamBarRect 12

configVegBaseMinusRect :: Layout -> Rect
configVegBaseMinusRect = configParamMinusRect 13

configVegBasePlusRect :: Layout -> Rect
configVegBasePlusRect = configParamPlusRect 13

configVegBaseBarRect :: Layout -> Rect
configVegBaseBarRect = configParamBarRect 13

configVegBoostMinusRect :: Layout -> Rect
configVegBoostMinusRect = configParamMinusRect 14

configVegBoostPlusRect :: Layout -> Rect
configVegBoostPlusRect = configParamPlusRect 14

configVegBoostBarRect :: Layout -> Rect
configVegBoostBarRect = configParamBarRect 14

configVegTempWeightMinusRect :: Layout -> Rect
configVegTempWeightMinusRect = configParamMinusRect 15

configVegTempWeightPlusRect :: Layout -> Rect
configVegTempWeightPlusRect = configParamPlusRect 15

configVegTempWeightBarRect :: Layout -> Rect
configVegTempWeightBarRect = configParamBarRect 15

configVegPrecipWeightMinusRect :: Layout -> Rect
configVegPrecipWeightMinusRect = configParamMinusRect 16

configVegPrecipWeightPlusRect :: Layout -> Rect
configVegPrecipWeightPlusRect = configParamPlusRect 16

configVegPrecipWeightBarRect :: Layout -> Rect
configVegPrecipWeightBarRect = configParamBarRect 16

configBoundaryMotionTempMinusRect :: Layout -> Rect
configBoundaryMotionTempMinusRect = configParamMinusRect 17

configBoundaryMotionTempPlusRect :: Layout -> Rect
configBoundaryMotionTempPlusRect = configParamPlusRect 17

configBoundaryMotionTempBarRect :: Layout -> Rect
configBoundaryMotionTempBarRect = configParamBarRect 17

configBoundaryMotionPrecipMinusRect :: Layout -> Rect
configBoundaryMotionPrecipMinusRect = configParamMinusRect 18

configBoundaryMotionPrecipPlusRect :: Layout -> Rect
configBoundaryMotionPrecipPlusRect = configParamPlusRect 18

configBoundaryMotionPrecipBarRect :: Layout -> Rect
configBoundaryMotionPrecipBarRect = configParamBarRect 18

-- Planet / Slice sliders (climate tab rows 19–25)

configPlanetRadiusMinusRect :: Layout -> Rect
configPlanetRadiusMinusRect = configParamMinusRect 19

configPlanetRadiusPlusRect :: Layout -> Rect
configPlanetRadiusPlusRect = configParamPlusRect 19

configPlanetRadiusBarRect :: Layout -> Rect
configPlanetRadiusBarRect = configParamBarRect 19

configAxialTiltMinusRect :: Layout -> Rect
configAxialTiltMinusRect = configParamMinusRect 20

configAxialTiltPlusRect :: Layout -> Rect
configAxialTiltPlusRect = configParamPlusRect 20

configAxialTiltBarRect :: Layout -> Rect
configAxialTiltBarRect = configParamBarRect 20

configInsolationMinusRect :: Layout -> Rect
configInsolationMinusRect = configParamMinusRect 21

configInsolationPlusRect :: Layout -> Rect
configInsolationPlusRect = configParamPlusRect 21

configInsolationBarRect :: Layout -> Rect
configInsolationBarRect = configParamBarRect 21

configSliceLatCenterMinusRect :: Layout -> Rect
configSliceLatCenterMinusRect = configParamMinusRect 22

configSliceLatCenterPlusRect :: Layout -> Rect
configSliceLatCenterPlusRect = configParamPlusRect 22

configSliceLatCenterBarRect :: Layout -> Rect
configSliceLatCenterBarRect = configParamBarRect 22

configSliceLatExtentMinusRect :: Layout -> Rect
configSliceLatExtentMinusRect = configParamMinusRect 23

configSliceLatExtentPlusRect :: Layout -> Rect
configSliceLatExtentPlusRect = configParamPlusRect 23

configSliceLatExtentBarRect :: Layout -> Rect
configSliceLatExtentBarRect = configParamBarRect 23

configSliceLonCenterMinusRect :: Layout -> Rect
configSliceLonCenterMinusRect = configParamMinusRect 24

configSliceLonCenterPlusRect :: Layout -> Rect
configSliceLonCenterPlusRect = configParamPlusRect 24

configSliceLonCenterBarRect :: Layout -> Rect
configSliceLonCenterBarRect = configParamBarRect 24

configSliceLonExtentMinusRect :: Layout -> Rect
configSliceLonExtentMinusRect = configParamMinusRect 25

configSliceLonExtentPlusRect :: Layout -> Rect
configSliceLonExtentPlusRect = configParamPlusRect 25

configSliceLonExtentBarRect :: Layout -> Rect
configSliceLonExtentBarRect = configParamBarRect 25

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
configGenCoordScaleMinusRect = configParamMinusRect 35

configGenCoordScalePlusRect :: Layout -> Rect
configGenCoordScalePlusRect = configParamPlusRect 35

configGenCoordScaleBarRect :: Layout -> Rect
configGenCoordScaleBarRect = configParamBarRect 35

configGenOffsetXMinusRect :: Layout -> Rect
configGenOffsetXMinusRect = configParamMinusRect 36

configGenOffsetXPlusRect :: Layout -> Rect
configGenOffsetXPlusRect = configParamPlusRect 36

configGenOffsetXBarRect :: Layout -> Rect
configGenOffsetXBarRect = configParamBarRect 36

configGenOffsetYMinusRect :: Layout -> Rect
configGenOffsetYMinusRect = configParamMinusRect 37

configGenOffsetYPlusRect :: Layout -> Rect
configGenOffsetYPlusRect = configParamPlusRect 37

configGenOffsetYBarRect :: Layout -> Rect
configGenOffsetYBarRect = configParamBarRect 37

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
configExtentXMinusRect = configParamMinusRect 38

configExtentXPlusRect :: Layout -> Rect
configExtentXPlusRect = configParamPlusRect 38

configExtentXBarRect :: Layout -> Rect
configExtentXBarRect = configParamBarRect 38

configExtentYMinusRect :: Layout -> Rect
configExtentYMinusRect = configParamMinusRect 39

configExtentYPlusRect :: Layout -> Rect
configExtentYPlusRect = configParamPlusRect 39

configExtentYBarRect :: Layout -> Rect
configExtentYBarRect = configParamBarRect 39

configEdgeNorthMinusRect :: Layout -> Rect
configEdgeNorthMinusRect = configParamMinusRect 40

configEdgeNorthPlusRect :: Layout -> Rect
configEdgeNorthPlusRect = configParamPlusRect 40

configEdgeNorthBarRect :: Layout -> Rect
configEdgeNorthBarRect = configParamBarRect 40

configEdgeSouthMinusRect :: Layout -> Rect
configEdgeSouthMinusRect = configParamMinusRect 41

configEdgeSouthPlusRect :: Layout -> Rect
configEdgeSouthPlusRect = configParamPlusRect 41

configEdgeSouthBarRect :: Layout -> Rect
configEdgeSouthBarRect = configParamBarRect 41

configEdgeEastMinusRect :: Layout -> Rect
configEdgeEastMinusRect = configParamMinusRect 42

configEdgeEastPlusRect :: Layout -> Rect
configEdgeEastPlusRect = configParamPlusRect 42

configEdgeEastBarRect :: Layout -> Rect
configEdgeEastBarRect = configParamBarRect 42

configEdgeWestMinusRect :: Layout -> Rect
configEdgeWestMinusRect = configParamMinusRect 43

configEdgeWestPlusRect :: Layout -> Rect
configEdgeWestPlusRect = configParamPlusRect 43

configEdgeWestBarRect :: Layout -> Rect
configEdgeWestBarRect = configParamBarRect 43

configEdgeFalloffMinusRect :: Layout -> Rect
configEdgeFalloffMinusRect = configParamMinusRect 44

configEdgeFalloffPlusRect :: Layout -> Rect
configEdgeFalloffPlusRect = configParamPlusRect 44

configEdgeFalloffBarRect :: Layout -> Rect
configEdgeFalloffBarRect = configParamBarRect 44

configPlateSizeMinusRect :: Layout -> Rect
configPlateSizeMinusRect = configParamMinusRect 8

configPlateSizePlusRect :: Layout -> Rect
configPlateSizePlusRect = configParamPlusRect 8

configPlateSizeBarRect :: Layout -> Rect
configPlateSizeBarRect = configParamBarRect 8

configUpliftMinusRect :: Layout -> Rect
configUpliftMinusRect = configParamMinusRect 9

configUpliftPlusRect :: Layout -> Rect
configUpliftPlusRect = configParamPlusRect 9

configUpliftBarRect :: Layout -> Rect
configUpliftBarRect = configParamBarRect 9

configRiftDepthMinusRect :: Layout -> Rect
configRiftDepthMinusRect = configParamMinusRect 10

configRiftDepthPlusRect :: Layout -> Rect
configRiftDepthPlusRect = configParamPlusRect 10

configRiftDepthBarRect :: Layout -> Rect
configRiftDepthBarRect = configParamBarRect 10

configDetailScaleMinusRect :: Layout -> Rect
configDetailScaleMinusRect = configParamMinusRect 11

configDetailScalePlusRect :: Layout -> Rect
configDetailScalePlusRect = configParamPlusRect 11

configDetailScaleBarRect :: Layout -> Rect
configDetailScaleBarRect = configParamBarRect 11

configPlateSpeedMinusRect :: Layout -> Rect
configPlateSpeedMinusRect = configParamMinusRect 12

configPlateSpeedPlusRect :: Layout -> Rect
configPlateSpeedPlusRect = configParamPlusRect 12

configPlateSpeedBarRect :: Layout -> Rect
configPlateSpeedBarRect = configParamBarRect 12

configBoundarySharpnessMinusRect :: Layout -> Rect
configBoundarySharpnessMinusRect = configParamMinusRect 13

configBoundarySharpnessPlusRect :: Layout -> Rect
configBoundarySharpnessPlusRect = configParamPlusRect 13

configBoundarySharpnessBarRect :: Layout -> Rect
configBoundarySharpnessBarRect = configParamBarRect 13

configBoundaryNoiseScaleMinusRect :: Layout -> Rect
configBoundaryNoiseScaleMinusRect = configParamMinusRect 14

configBoundaryNoiseScalePlusRect :: Layout -> Rect
configBoundaryNoiseScalePlusRect = configParamPlusRect 14

configBoundaryNoiseScaleBarRect :: Layout -> Rect
configBoundaryNoiseScaleBarRect = configParamBarRect 14

configBoundaryNoiseStrengthMinusRect :: Layout -> Rect
configBoundaryNoiseStrengthMinusRect = configParamMinusRect 15

configBoundaryNoiseStrengthPlusRect :: Layout -> Rect
configBoundaryNoiseStrengthPlusRect = configParamPlusRect 15

configBoundaryNoiseStrengthBarRect :: Layout -> Rect
configBoundaryNoiseStrengthBarRect = configParamBarRect 15

configBoundaryWarpOctavesMinusRect :: Layout -> Rect
configBoundaryWarpOctavesMinusRect = configParamMinusRect 16

configBoundaryWarpOctavesPlusRect :: Layout -> Rect
configBoundaryWarpOctavesPlusRect = configParamPlusRect 16

configBoundaryWarpOctavesBarRect :: Layout -> Rect
configBoundaryWarpOctavesBarRect = configParamBarRect 16

configBoundaryWarpLacunarityMinusRect :: Layout -> Rect
configBoundaryWarpLacunarityMinusRect = configParamMinusRect 17

configBoundaryWarpLacunarityPlusRect :: Layout -> Rect
configBoundaryWarpLacunarityPlusRect = configParamPlusRect 17

configBoundaryWarpLacunarityBarRect :: Layout -> Rect
configBoundaryWarpLacunarityBarRect = configParamBarRect 17

configBoundaryWarpGainMinusRect :: Layout -> Rect
configBoundaryWarpGainMinusRect = configParamMinusRect 18

configBoundaryWarpGainPlusRect :: Layout -> Rect
configBoundaryWarpGainPlusRect = configParamPlusRect 18

configBoundaryWarpGainBarRect :: Layout -> Rect
configBoundaryWarpGainBarRect = configParamBarRect 18

configPlateMergeScaleMinusRect :: Layout -> Rect
configPlateMergeScaleMinusRect = configParamMinusRect 19

configPlateMergeScalePlusRect :: Layout -> Rect
configPlateMergeScalePlusRect = configParamPlusRect 19

configPlateMergeScaleBarRect :: Layout -> Rect
configPlateMergeScaleBarRect = configParamBarRect 19

configPlateMergeBiasMinusRect :: Layout -> Rect
configPlateMergeBiasMinusRect = configParamMinusRect 20

configPlateMergeBiasPlusRect :: Layout -> Rect
configPlateMergeBiasPlusRect = configParamPlusRect 20

configPlateMergeBiasBarRect :: Layout -> Rect
configPlateMergeBiasBarRect = configParamBarRect 20

configPlateDetailScaleMinusRect :: Layout -> Rect
configPlateDetailScaleMinusRect = configParamMinusRect 21

configPlateDetailScalePlusRect :: Layout -> Rect
configPlateDetailScalePlusRect = configParamPlusRect 21

configPlateDetailScaleBarRect :: Layout -> Rect
configPlateDetailScaleBarRect = configParamBarRect 21

configPlateDetailStrengthMinusRect :: Layout -> Rect
configPlateDetailStrengthMinusRect = configParamMinusRect 22

configPlateDetailStrengthPlusRect :: Layout -> Rect
configPlateDetailStrengthPlusRect = configParamPlusRect 22

configPlateDetailStrengthBarRect :: Layout -> Rect
configPlateDetailStrengthBarRect = configParamBarRect 22

configPlateRidgeStrengthMinusRect :: Layout -> Rect
configPlateRidgeStrengthMinusRect = configParamMinusRect 23

configPlateRidgeStrengthPlusRect :: Layout -> Rect
configPlateRidgeStrengthPlusRect = configParamPlusRect 23

configPlateRidgeStrengthBarRect :: Layout -> Rect
configPlateRidgeStrengthBarRect = configParamBarRect 23

configPlateHeightBaseMinusRect :: Layout -> Rect
configPlateHeightBaseMinusRect = configParamMinusRect 24

configPlateHeightBasePlusRect :: Layout -> Rect
configPlateHeightBasePlusRect = configParamPlusRect 24

configPlateHeightBaseBarRect :: Layout -> Rect
configPlateHeightBaseBarRect = configParamBarRect 24

configPlateHeightVarianceMinusRect :: Layout -> Rect
configPlateHeightVarianceMinusRect = configParamMinusRect 25

configPlateHeightVariancePlusRect :: Layout -> Rect
configPlateHeightVariancePlusRect = configParamPlusRect 25

configPlateHeightVarianceBarRect :: Layout -> Rect
configPlateHeightVarianceBarRect = configParamBarRect 25

configPlateHardnessBaseMinusRect :: Layout -> Rect
configPlateHardnessBaseMinusRect = configParamMinusRect 26

configPlateHardnessBasePlusRect :: Layout -> Rect
configPlateHardnessBasePlusRect = configParamPlusRect 26

configPlateHardnessBaseBarRect :: Layout -> Rect
configPlateHardnessBaseBarRect = configParamBarRect 26

configPlateHardnessVarianceMinusRect :: Layout -> Rect
configPlateHardnessVarianceMinusRect = configParamMinusRect 27

configPlateHardnessVariancePlusRect :: Layout -> Rect
configPlateHardnessVariancePlusRect = configParamPlusRect 27

configPlateHardnessVarianceBarRect :: Layout -> Rect
configPlateHardnessVarianceBarRect = configParamBarRect 27

configTrenchDepthMinusRect :: Layout -> Rect
configTrenchDepthMinusRect = configParamMinusRect 28

configTrenchDepthPlusRect :: Layout -> Rect
configTrenchDepthPlusRect = configParamPlusRect 28

configTrenchDepthBarRect :: Layout -> Rect
configTrenchDepthBarRect = configParamBarRect 28

configRidgeHeightMinusRect :: Layout -> Rect
configRidgeHeightMinusRect = configParamMinusRect 29

configRidgeHeightPlusRect :: Layout -> Rect
configRidgeHeightPlusRect = configParamPlusRect 29

configRidgeHeightBarRect :: Layout -> Rect
configRidgeHeightBarRect = configParamBarRect 29

configPlateBiasStrengthMinusRect :: Layout -> Rect
configPlateBiasStrengthMinusRect = configParamMinusRect 30

configPlateBiasStrengthPlusRect :: Layout -> Rect
configPlateBiasStrengthPlusRect = configParamPlusRect 30

configPlateBiasStrengthBarRect :: Layout -> Rect
configPlateBiasStrengthBarRect = configParamBarRect 30

configPlateBiasCenterMinusRect :: Layout -> Rect
configPlateBiasCenterMinusRect = configParamMinusRect 31

configPlateBiasCenterPlusRect :: Layout -> Rect
configPlateBiasCenterPlusRect = configParamPlusRect 31

configPlateBiasCenterBarRect :: Layout -> Rect
configPlateBiasCenterBarRect = configParamBarRect 31

configPlateBiasEdgeMinusRect :: Layout -> Rect
configPlateBiasEdgeMinusRect = configParamMinusRect 32

configPlateBiasEdgePlusRect :: Layout -> Rect
configPlateBiasEdgePlusRect = configParamPlusRect 32

configPlateBiasEdgeBarRect :: Layout -> Rect
configPlateBiasEdgeBarRect = configParamBarRect 32

configPlateBiasNorthMinusRect :: Layout -> Rect
configPlateBiasNorthMinusRect = configParamMinusRect 33

configPlateBiasNorthPlusRect :: Layout -> Rect
configPlateBiasNorthPlusRect = configParamPlusRect 33

configPlateBiasNorthBarRect :: Layout -> Rect
configPlateBiasNorthBarRect = configParamBarRect 33

configPlateBiasSouthMinusRect :: Layout -> Rect
configPlateBiasSouthMinusRect = configParamMinusRect 34

configPlateBiasSouthPlusRect :: Layout -> Rect
configPlateBiasSouthPlusRect = configParamPlusRect 34

configPlateBiasSouthBarRect :: Layout -> Rect
configPlateBiasSouthBarRect = configParamBarRect 34

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

configApplyRect :: Layout -> Rect
configApplyRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
      baseY = y + h - 40
  in Rect (V2 (x + 12) (baseY - 64), V2 (w - 24) 24)

configReplayRect :: Layout -> Rect
configReplayRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
      baseY = y + h - 40
  in Rect (V2 (x + 12) (baseY - 32), V2 (w - 24) 24)

configResetRect :: Layout -> Rect
configResetRect layout =
  let Rect (V2 x y, V2 w h) = configPanelRect layout
  in Rect (V2 (x + 12) (y + h - 40), V2 (w - 24) 24)

configScrollAreaRect :: Layout -> Rect
configScrollAreaRect layout =
  let Rect (V2 x y, V2 w _) = configPanelRect layout
      Rect (V2 _ applyY, V2 _ _) = configApplyRect layout
      pad = 12
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
