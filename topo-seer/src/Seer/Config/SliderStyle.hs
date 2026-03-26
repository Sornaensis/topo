module Seer.Config.SliderStyle
  ( SliderLabelMode(..)
  , SliderStyle(..)
  , sliderStyleForId
  ) where

import Data.Word (Word8)
import Linear (V4(..))
import Seer.Config.SliderRegistry (SliderId(..))

-- | How a slider row renders its label chrome in the config panel.
data SliderLabelMode
  = SliderLabelFull
  | SliderLabelBarOnly
  deriving (Eq, Ord, Show)

data SliderStyle = SliderStyle
  { sliderStyleStep :: !Float
  , sliderStyleLabelMode :: !SliderLabelMode
  , sliderStyleFillColor :: !(V4 Word8)
  } deriving (Eq, Show)

sliderStyleForId :: SliderId -> SliderStyle
sliderStyleForId sliderIdValue = SliderStyle
  { sliderStyleStep = sliderStepForId sliderIdValue
  , sliderStyleLabelMode = sliderLabelModeForId sliderIdValue
  , sliderStyleFillColor = sliderFillColorForId sliderIdValue
  }

sliderStepForId :: SliderId -> Float
sliderStepForId sliderIdValue = case sliderIdValue of
  SliderExtentX -> 1 / 16
  SliderExtentY -> 1 / 16
  SliderEdgeFalloff -> 0.01
  SliderBoundaryWarpOctaves -> 0.1
  SliderWindIterations -> 0.1
  SliderMoistureIterations -> 0.1
  _ -> 0.05

-- | Preserve sliders whose labels intentionally omit minus/plus button text.
sliderLabelModeForId :: SliderId -> SliderLabelMode
sliderLabelModeForId sliderIdValue
  | sliderIdValue `elem`
      [ SliderErosionHydDeposit
      , SliderErosionDepositSlope
      , SliderErosionThermDeposit
      , SliderErosionCoastZone
      , SliderErosionCoastStrength
      , SliderErosionCoastIter
      ] = SliderLabelBarOnly
  | otherwise = SliderLabelFull

sliderFillColorForId :: SliderId -> V4 Word8
sliderFillColorForId sliderIdValue = case sliderIdValue of
  SliderGenScale -> V4 90 140 180 255
  SliderGenCoordScale -> V4 80 120 170 255
  SliderGenOffsetX -> V4 120 120 150 255
  SliderGenOffsetY -> V4 120 120 150 255
  SliderGenFrequency -> V4 100 120 170 255
  SliderGenOctaves -> V4 120 120 150 255
  SliderGenLacunarity -> V4 130 110 170 255
  SliderGenGain -> V4 140 120 160 255
  SliderGenWarpScale -> V4 90 150 140 255
  SliderGenWarpStrength -> V4 110 150 110 255
  SliderExtentX -> V4 130 140 120 255
  SliderExtentY -> V4 120 140 140 255
  SliderEdgeNorth -> V4 120 150 180 255
  SliderEdgeSouth -> V4 120 130 200 255
  SliderEdgeEast -> V4 110 140 190 255
  SliderEdgeWest -> V4 110 140 170 255
  SliderEdgeFalloff -> V4 140 150 110 255
  SliderPlateSize -> V4 120 120 170 255
  SliderUplift -> V4 150 120 120 255
  SliderRiftDepth -> V4 120 130 160 255
  SliderDetailScale -> V4 90 140 160 255
  SliderPlateSpeed -> V4 110 130 180 255
  SliderBoundarySharpness -> V4 130 120 170 255
  SliderBoundaryNoiseScale -> V4 100 140 150 255
  SliderBoundaryNoiseStrength -> V4 140 130 140 255
  SliderBoundaryWarpOctaves -> V4 120 120 150 255
  SliderBoundaryWarpLacunarity -> V4 120 120 170 255
  SliderBoundaryWarpGain -> V4 110 140 130 255
  SliderPlateMergeScale -> V4 120 140 120 255
  SliderPlateMergeBias -> V4 140 130 120 255
  SliderPlateDetailScale -> V4 90 140 160 255
  SliderPlateDetailStrength -> V4 120 150 140 255
  SliderPlateRidgeStrength -> V4 150 130 120 255
  SliderPlateHeightBase -> V4 110 150 160 255
  SliderPlateHeightVariance -> V4 140 140 160 255
  SliderPlateHardnessBase -> V4 100 130 160 255
  SliderPlateHardnessVariance -> V4 120 140 160 255
  SliderTrenchDepth -> V4 120 130 170 255
  SliderRidgeHeight -> V4 130 140 150 255
  SliderPlateBiasStrength -> V4 140 120 160 255
  SliderPlateBiasCenter -> V4 120 120 160 255
  SliderPlateBiasEdge -> V4 120 120 160 255
  SliderPlateBiasNorth -> V4 120 120 160 255
  SliderPlateBiasSouth -> V4 120 120 160 255
  SliderTfcCliffSlope -> V4 150 100 100 255
  SliderTfcMountainSlope -> V4 140 110 100 255
  SliderTfcMountainRelief -> V4 130 120 100 255
  SliderTfcHillSlope -> V4 120 130 110 255
  SliderTfcRollingSlope -> V4 110 140 120 255
  SliderValleyCurvature -> V4 100 140 140 255
  SliderTfcElevGradient -> V4 100 130 150 255
  SliderTfcPlateauMaxRelief2Ring -> V4 110 120 150 255
  SliderTfcPlateauMaxMicroRelief -> V4 110 115 155 255
  SliderTfcRollingNearFactor -> V4 115 135 115 255
  SliderRockElevationThreshold -> V4 140 130 120 255
  SliderRockHardnessThreshold -> V4 130 130 130 255
  SliderRockHardnessSecondary -> V4 120 130 140 255
  SliderPlanetRadius -> V4 150 130 100 255
  SliderAxialTilt -> V4 140 140 100 255
  SliderInsolation -> V4 180 150 80 255
  SliderOccWarmScale -> V4 180 120 100 255
  SliderOccColdScale -> V4 100 140 180 255
  SliderOccLatPeakDeg -> V4 140 130 160 255
  SliderOccLatWidthDeg -> V4 130 150 140 255
  SliderWaterLevel -> V4 70 120 180 255
  SliderOrographicLift -> V4 110 140 190 255
  SliderRainShadowLoss -> V4 130 120 170 255
  SliderWindDiffuse -> V4 90 140 120 255
  SliderEquatorTemp -> V4 180 120 90 255
  SliderPoleTemp -> V4 90 150 200 255
  SliderLapseRate -> V4 120 120 160 255
  SliderWindIterations -> V4 120 140 200 255
  SliderMoistureIterations -> V4 120 150 160 255
  SliderBoundaryMotionTemp -> V4 110 130 160 255
  SliderBoundaryMotionPrecip -> V4 120 140 170 255
  SliderSliceLatCenter -> V4 110 140 130 255
  SliderSliceLonCenter -> V4 110 130 150 255
  SliderLatitudeExponent -> V4 150 110 130 255
  SliderPlateHeightCooling -> V4 130 130 150 255
  SliderTempNoiseScale -> V4 140 120 150 255
  SliderOceanModeration -> V4 80 140 170 255
  SliderOceanModerateTemp -> V4 90 130 180 255
  SliderAlbedoSensitivity -> V4 170 150 100 255
  SliderAlbedoReference -> V4 160 140 110 255
  SliderMoistAdvect -> V4 100 150 160 255
  SliderMoistLocal -> V4 110 140 170 255
  SliderMoistWindEvapScale -> V4 90 160 150 255
  SliderMoistEvapNoiseScale -> V4 120 140 160 255
  SliderMoistBareEvapFrac -> V4 130 150 120 255
  SliderMoistVegTranspFrac -> V4 80 160 130 255
  SliderMoistWindETScale -> V4 110 150 150 255
  SliderMoistCondensationRate -> V4 90 140 170 255
  SliderMoistRecycleRate -> V4 100 130 160 255
  SliderMoistITCZStrength -> V4 120 150 140 255
  SliderMoistITCZWidth -> V4 110 160 130 255
  SliderOrographicScale -> V4 160 130 80 255
  SliderOrographicStep -> V4 150 140 90 255
  SliderCoastalIterations -> V4 100 160 160 255
  SliderCoastalDiffuse -> V4 90 150 170 255
  SliderCoastalMoistureBoost -> V4 80 140 180 255
  SliderWindBeltStrength -> V4 130 150 180 255
  SliderWindBeltHarmonics -> V4 120 140 170 255
  SliderWindBeltBase -> V4 110 130 160 255
  SliderWindBeltRange -> V4 100 140 175 255
  SliderWindBeltSpeedScale -> V4 90 135 185 255
  SliderBndLandRange -> V4 140 130 100 255
  SliderPiedmontSmooth -> V4 120 150 130 255
  SliderPiedmontSlopeMin -> V4 130 140 120 255
  SliderPiedmontSlopeMax -> V4 140 130 120 255
  SliderWindCoriolisDeflection -> V4 130 130 170 255
  SliderMoistMinVegFloor -> V4 110 150 110 255
  SliderWeatherTick -> V4 140 120 140 255
  SliderWeatherPhase -> V4 110 130 150 255
  SliderWeatherAmplitude -> V4 140 120 90 255
  SliderSeasonCycleLength -> V4 130 110 140 255
  SliderJitterAmplitude -> V4 120 130 130 255
  SliderPressureBase -> V4 110 140 120 255
  SliderPressureTempScale -> V4 100 150 110 255
  SliderPressureCoriolisScale -> V4 130 120 150 255
  SliderSeasonalBase -> V4 140 110 130 255
  SliderSeasonalRange -> V4 120 140 140 255
  SliderHumidityNoiseScale -> V4 110 130 160 255
  SliderPrecipNoiseScale -> V4 100 120 170 255
  SliderWeatherITCZWidth -> V4 130 140 150 255
  SliderWeatherITCZPrecipBoost -> V4 120 150 140 255
  SliderPressureHumidityScale -> V4 110 140 130 255
  SliderPressureGradientWindScale -> V4 100 130 150 255
  SliderWindNoiseScale -> V4 130 120 160 255
  SliderITCZMigrationScale -> V4 140 130 170 255
  SliderCloudRHExponent -> V4 120 120 140 255
  SliderCloudAlbedoEffect -> V4 110 110 150 255
  SliderCloudPrecipBoost -> V4 100 100 160 255
  SliderVegBase -> V4 100 130 120 255
  SliderVegBoost -> V4 120 150 120 255
  SliderVegTempWeight -> V4 130 140 170 255
  SliderVegPrecipWeight -> V4 120 140 190 255
  SliderBtCoastalBand -> V4 100 160 180 255
  SliderBtSnowMaxTemp -> V4 180 180 200 255
  SliderBtAlpineMaxTemp -> V4 160 160 180 255
  SliderBtIceCapTemp -> V4 180 200 220 255
  SliderBtMontaneMaxTemp -> V4 140 130 110 255
  SliderBtMontanePrecip -> V4 120 140 120 255
  SliderBtCliffSlope -> V4 150 130 100 255
  SliderBtValleyMoisture -> V4 100 150 130 255
  SliderBtDepressionMoisture -> V4 110 140 140 255
  SliderBtPrecipWeight -> V4 120 130 150 255
  SliderVbcTempMin -> V4 130 120 100 255
  SliderVbcTempRange -> V4 140 130 110 255
  SliderVbcFertilityBoost -> V4 100 150 100 255
  SliderVbcAlbedoBase -> V4 160 160 140 255
  SliderVbcAlbedoBare -> V4 170 160 130 255
  SliderVbcAlbedoVeg -> V4 100 140 100 255
  SliderVbcOceanAlbedo -> V4 100 120 170 255
  SliderVbcIceAlbedo -> V4 190 200 210 255
  SliderBiomeSmoothing -> V4 130 140 150 255
  SliderVolcanicAshBoost -> V4 160 120 80 255
  SliderVolcanicLavaPenalty -> V4 180 100 60 255
  SliderBiomeFeedbackBlend -> V4 120 130 140 255
  SliderErosionHydraulic -> V4 90 140 180 255
  SliderErosionThermal -> V4 120 120 160 255
  SliderErosionRainRate -> V4 100 110 170 255
  SliderErosionTalus -> V4 160 120 90 255
  SliderErosionMaxDrop -> V4 140 120 120 255
  SliderErosionHydDeposit -> V4 130 130 110 255
  SliderErosionDepositSlope -> V4 120 130 120 255
  SliderErosionThermDeposit -> V4 150 110 100 255
  SliderErosionCoastZone -> V4 90 150 180 255
  SliderErosionCoastStrength -> V4 80 140 170 255
  SliderErosionCoastIter -> V4 70 130 160 255
  SliderHypsometryEnabled -> V4 200 180 140 255
  SliderHypsometryLowlandExp -> V4 190 175 135 255
  SliderHypsometryHighlandExp -> V4 180 170 130 255
  SliderHypsometryPlateauBreak -> V4 170 165 125 255
  SliderHypsometryOceanExp -> V4 160 160 120 255
  SliderHypsometryCoastalRampWidth -> V4 150 155 115 255
  SliderHypsometryCoastalRampStr -> V4 140 150 110 255
  SliderGlacierSnowTemp -> V4 180 210 240 255
  SliderGlacierSnowRange -> V4 170 200 235 255
  SliderGlacierMeltTemp -> V4 160 195 230 255
  SliderGlacierMeltRate -> V4 150 190 225 255
  SliderGlacierAccumScale -> V4 140 185 220 255
  SliderGlacierFlowIters -> V4 130 180 215 255
  SliderGlacierFlowRate -> V4 120 175 210 255
  SliderGlacierErosionScale -> V4 110 170 210 255
  SliderGlacierCarveScale -> V4 100 165 205 255
  SliderGlacierDepositScale -> V4 90 160 200 255
  SliderVentDensity -> V4 200 80 60 255
  SliderVentThreshold -> V4 190 90 70 255
  SliderHotspotScale -> V4 210 100 50 255
  SliderHotspotThreshold -> V4 200 110 60 255
  SliderMagmaRecharge -> V4 220 90 40 255
  SliderLavaScale -> V4 230 80 30 255
  SliderAshScale -> V4 170 130 100 255
  SliderVolcanicDepositScale -> V4 180 120 80 255
  SliderSoilMoistureThreshold -> V4 140 120 80 255
  SliderSoilHardnessThreshold -> V4 150 130 90 255
  SliderSoilFertilityMoistWeight -> V4 120 140 80 255
  SliderSoilFertilityDepthWeight -> V4 130 140 90 255
  SliderSinkBreachDepth -> V4 70 130 170 255
  SliderStreamPowerMaxErosion -> V4 80 140 170 255
  SliderRiverCarveMaxDepth -> V4 60 120 160 255
  SliderCoastalErodeStrength -> V4 70 110 150 255
  SliderHydroHardnessWeight -> V4 90 130 160 255
  SliderMinLakeSize -> V4 60 140 180 255
  SliderInlandSeaMinSize -> V4 50 130 170 255
  SliderRoughnessScale -> V4 110 130 140 255