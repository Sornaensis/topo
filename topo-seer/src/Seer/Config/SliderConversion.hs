-- | Shared slider-domain conversion rules used by config generation and
-- snapshot restore.
module Seer.Config.SliderConversion
  ( SliderLabelDomain(..)
  , SliderValueKind(..)
  , sliderToDomainFloat
  , sliderToDomainInt
  , sliderToDomainBool
  , sliderFromDomainFloat
  , sliderFromDomainInt
  , sliderFromDomainBool
  , sliderLabelDomain
  ) where

import Seer.Config.Range (mapIntRange, mapRange, unmapIntRange, unmapRange)
import Seer.Config.SliderRegistry (SliderId(..))

data SliderValueKind
  = SliderValueFloat
  | SliderValueInt
  | SliderValueToggle
  deriving (Eq, Ord, Show)

data SliderLabelDomain = SliderLabelDomain
  { sldLo :: !Float
  , sldHi :: !Float
  , sldValueKind :: !SliderValueKind
  } deriving (Eq, Show)

data SliderConversion
  = SliderFloatRange !Float !Float
  | SliderIntRange !Int !Int
  | SliderToggle

-- | Shared display-domain metadata for registry-backed sliders.
--
-- Returns the domain lower bound, upper bound, and explicit value kind used
-- by labels and other display-oriented callers.
sliderLabelDomain :: SliderId -> SliderLabelDomain
sliderLabelDomain sliderIdValue =
  case sliderConversion sliderIdValue of
    SliderFloatRange lo hi -> SliderLabelDomain lo hi SliderValueFloat
    SliderIntRange lo hi -> SliderLabelDomain (fromIntegral lo) (fromIntegral hi) SliderValueInt
    SliderToggle -> SliderLabelDomain 0.0 1.0 SliderValueToggle

sliderToDomainFloat :: SliderId -> Float -> Float
sliderToDomainFloat sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderFloatRange lo hi -> mapRange lo hi value
    SliderIntRange lo hi -> fromIntegral (mapIntRange lo hi value)
    SliderToggle -> if value >= 0.5 then 1.0 else 0.0

sliderToDomainInt :: SliderId -> Float -> Int
sliderToDomainInt sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderIntRange lo hi -> mapIntRange lo hi value
    SliderFloatRange lo hi -> round (mapRange lo hi value)
    SliderToggle -> if value >= 0.5 then 1 else 0

sliderToDomainBool :: SliderId -> Float -> Bool
sliderToDomainBool sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderToggle -> value >= 0.5
    SliderFloatRange _ _ -> value >= 0.5
    SliderIntRange lo hi -> mapIntRange lo hi value > lo

sliderFromDomainFloat :: SliderId -> Float -> Float
sliderFromDomainFloat sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderFloatRange lo hi -> unmapRange lo hi value
    SliderIntRange lo hi -> unmapIntRange lo hi (round value)
    SliderToggle -> if value >= 0.5 then 1.0 else 0.0

sliderFromDomainInt :: SliderId -> Int -> Float
sliderFromDomainInt sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderIntRange lo hi -> unmapIntRange lo hi value
    SliderFloatRange lo hi -> unmapRange lo hi (fromIntegral value)
    SliderToggle -> if value /= 0 then 1.0 else 0.0

sliderFromDomainBool :: SliderId -> Bool -> Float
sliderFromDomainBool sliderIdValue value =
  case sliderConversion sliderIdValue of
    SliderToggle -> if value then 1.0 else 0.0
    SliderFloatRange _ _ -> if value then 1.0 else 0.0
    SliderIntRange lo _ -> if value then unmapIntRange lo (lo + 1) (lo + 1) else 0.0

sliderConversion :: SliderId -> SliderConversion
sliderConversion sliderIdValue = case sliderIdValue of
  SliderGenScale -> SliderFloatRange 0.2 2.0
  SliderGenCoordScale -> SliderFloatRange 0.5 2.0
  SliderGenOffsetX -> SliderFloatRange (-10000) 10000
  SliderGenOffsetY -> SliderFloatRange (-10000) 10000
  SliderGenFrequency -> SliderFloatRange 0.001 0.05
  SliderGenOctaves -> SliderIntRange 2 8
  SliderGenLacunarity -> SliderFloatRange 1.5 3.5
  SliderGenGain -> SliderFloatRange 0.3 0.8
  SliderGenWarpScale -> SliderFloatRange 0.005 0.08
  SliderGenWarpStrength -> SliderFloatRange 2 20
  SliderExtentX -> SliderIntRange 0 16
  SliderExtentY -> SliderIntRange 0 16
  SliderEdgeNorth -> SliderFloatRange 0 2
  SliderEdgeSouth -> SliderFloatRange 0 2
  SliderEdgeEast -> SliderFloatRange 0 2
  SliderEdgeWest -> SliderFloatRange 0 2
  SliderEdgeFalloff -> SliderFloatRange 0 512
  SliderPlateSize -> SliderIntRange 16 128
  SliderUplift -> SliderFloatRange 0.05 0.4
  SliderRiftDepth -> SliderFloatRange 0.05 0.6
  SliderDetailScale -> SliderFloatRange 0.5 2.5
  SliderPlateSpeed -> SliderFloatRange 0.1 1.5
  SliderBoundarySharpness -> SliderFloatRange 0.5 2.5
  SliderBoundaryNoiseScale -> SliderFloatRange 0.002 0.02
  SliderBoundaryNoiseStrength -> SliderFloatRange 2 24
  SliderBoundaryWarpOctaves -> SliderIntRange 1 5
  SliderBoundaryWarpLacunarity -> SliderFloatRange 1.5 3.5
  SliderBoundaryWarpGain -> SliderFloatRange 0.3 0.8
  SliderPlateMergeScale -> SliderFloatRange 0.05 0.25
  SliderPlateMergeBias -> SliderFloatRange 0.3 0.8
  SliderPlateDetailScale -> SliderFloatRange 0.005 0.05
  SliderPlateDetailStrength -> SliderFloatRange 0.05 0.5
  SliderPlateRidgeStrength -> SliderFloatRange 0.05 0.4
  SliderPlateHeightBase -> SliderFloatRange 0.0 0.4
  SliderPlateHeightVariance -> SliderFloatRange 0.1 0.8
  SliderPlateHardnessBase -> SliderFloatRange 0.2 0.8
  SliderPlateHardnessVariance -> SliderFloatRange 0.1 0.6
  SliderTrenchDepth -> SliderFloatRange 0.1 0.5
  SliderRidgeHeight -> SliderFloatRange 0.02 0.2
  SliderPlateBiasStrength -> SliderFloatRange 0 0.6
  SliderPlateBiasCenter -> SliderFloatRange (-1) 1
  SliderPlateBiasEdge -> SliderFloatRange (-1) 1
  SliderPlateBiasNorth -> SliderFloatRange (-1) 1
  SliderPlateBiasSouth -> SliderFloatRange (-1) 1
  SliderTfcCliffSlope -> SliderFloatRange 0.05 0.50
  SliderTfcMountainSlope -> SliderFloatRange 0.02 0.20
  SliderTfcMountainRelief -> SliderFloatRange 0.02 0.30
  SliderTfcHillSlope -> SliderFloatRange 0.005 0.10
  SliderTfcRollingSlope -> SliderFloatRange 0.002 0.04
  SliderValleyCurvature -> SliderFloatRange 0.05 0.4
  SliderTfcElevGradient -> SliderFloatRange 0.1 2.0
  SliderTfcPlateauMaxRelief2Ring -> SliderFloatRange 0.005 0.10
  SliderRockElevationThreshold -> SliderFloatRange 0.2 0.9
  SliderRockHardnessThreshold -> SliderFloatRange 0.2 0.9
  SliderRockHardnessSecondary -> SliderFloatRange 0.1 0.8
  SliderPlanetRadius -> SliderFloatRange 4778 9557
  SliderAxialTilt -> SliderFloatRange 0 45
  SliderInsolation -> SliderFloatRange 0.7 1.3
  SliderOccWarmScale -> SliderFloatRange 0 0.2
  SliderOccColdScale -> SliderFloatRange 0 0.2
  SliderOccLatPeakDeg -> SliderFloatRange 0 60
  SliderOccLatWidthDeg -> SliderFloatRange 5 45
  SliderWaterLevel -> SliderFloatRange 0 1
  SliderOrographicLift -> SliderFloatRange 0 1
  SliderRainShadowLoss -> SliderFloatRange 0 1
  SliderWindDiffuse -> SliderFloatRange 0 1
  SliderEquatorTemp -> SliderFloatRange (-50) 50
  SliderPoleTemp -> SliderFloatRange (-50) 50
  SliderLapseRate -> SliderFloatRange 0 1
  SliderWindIterations -> SliderIntRange 1 8
  SliderMoistureIterations -> SliderIntRange 2 72
  SliderBoundaryMotionTemp -> SliderFloatRange 0 2
  SliderBoundaryMotionPrecip -> SliderFloatRange 0 2
  SliderSliceLatCenter -> SliderFloatRange (-90) 90
  SliderSliceLonCenter -> SliderFloatRange (-180) 180
  SliderLatitudeExponent -> SliderFloatRange 0.2 1.5
  SliderPlateHeightCooling -> SliderFloatRange 0 0.2
  SliderTempNoiseScale -> SliderFloatRange 0 0.3
  SliderOceanModeration -> SliderFloatRange 0 1
  SliderOceanModerateTemp -> SliderFloatRange (-0.1) 0.1
  SliderAlbedoSensitivity -> SliderFloatRange 0 1
  SliderAlbedoReference -> SliderFloatRange 0 0.5
  SliderMoistAdvect -> SliderFloatRange 0 1
  SliderMoistLocal -> SliderFloatRange 0 1
  SliderMoistWindEvapScale -> SliderFloatRange 0 1
  SliderMoistEvapNoiseScale -> SliderFloatRange 0 0.2
  SliderMoistBareEvapFrac -> SliderFloatRange 0 1
  SliderMoistVegTranspFrac -> SliderFloatRange 0 1
  SliderMoistWindETScale -> SliderFloatRange 0 1
  SliderMoistCondensationRate -> SliderFloatRange 0 1
  SliderMoistRecycleRate -> SliderFloatRange 0 1
  SliderMoistITCZStrength -> SliderFloatRange 0 0.5
  SliderMoistITCZWidth -> SliderFloatRange 2 20
  SliderOrographicScale -> SliderFloatRange 0 2
  SliderOrographicStep -> SliderFloatRange 0.5 3
  SliderCoastalIterations -> SliderIntRange 0 16
  SliderCoastalDiffuse -> SliderFloatRange 0 1
  SliderCoastalMoistureBoost -> SliderFloatRange 0 0.5
  SliderWindBeltStrength -> SliderFloatRange 0 1
  SliderWindBeltHarmonics -> SliderFloatRange 1 6
  SliderWindBeltBase -> SliderFloatRange 0 1
  SliderWindBeltRange -> SliderFloatRange 0 1
  SliderWindBeltSpeedScale -> SliderFloatRange 0 1
  SliderBndLandRange -> SliderFloatRange 0.1 1.5
  SliderPiedmontSmooth -> SliderFloatRange 0.0 0.6
  SliderPiedmontSlopeMin -> SliderFloatRange 0.01 0.08
  SliderPiedmontSlopeMax -> SliderFloatRange 0.05 0.25
  SliderWindCoriolisDeflection -> SliderFloatRange 0 1.57
  SliderMoistMinVegFloor -> SliderFloatRange 0 1
  SliderWeatherTick -> SliderFloatRange 0.1 5
  SliderWeatherPhase -> SliderFloatRange 0 1
  SliderWeatherAmplitude -> SliderFloatRange 0 0.5
  SliderSeasonCycleLength -> SliderFloatRange 30 730
  SliderJitterAmplitude -> SliderFloatRange 0 0.5
  SliderPressureBase -> SliderFloatRange 0.3 1.0
  SliderPressureTempScale -> SliderFloatRange 0 1
  SliderPressureCoriolisScale -> SliderFloatRange 0 0.5
  SliderSeasonalBase -> SliderFloatRange 0 1
  SliderSeasonalRange -> SliderFloatRange 0 2
  SliderHumidityNoiseScale -> SliderFloatRange 0 0.3
  SliderPrecipNoiseScale -> SliderFloatRange 0 0.5
  SliderWeatherITCZWidth -> SliderFloatRange 2 20
  SliderWeatherITCZPrecipBoost -> SliderFloatRange 0 1
  SliderPressureHumidityScale -> SliderFloatRange 0 0.5
  SliderPressureGradientWindScale -> SliderFloatRange 0 1
  SliderWindNoiseScale -> SliderFloatRange 0 0.3
  SliderITCZMigrationScale -> SliderFloatRange 0 1.5
  SliderCloudRHExponent -> SliderFloatRange 0.5 3.0
  SliderCloudAlbedoEffect -> SliderFloatRange 0 0.3
  SliderCloudPrecipBoost -> SliderFloatRange 0 0.5
  SliderVegBase -> SliderFloatRange 0.1 3.0
  SliderVegBoost -> SliderFloatRange 0.1 3.0
  SliderVegTempWeight -> SliderFloatRange 0 1
  SliderVegPrecipWeight -> SliderFloatRange 0 1
  SliderBtCoastalBand -> SliderFloatRange 0 0.1
  SliderBtSnowMaxTemp -> SliderFloatRange 0.0 0.5
  SliderBtAlpineMaxTemp -> SliderFloatRange 0.1 0.7
  SliderBtIceCapTemp -> SliderFloatRange 0 0.2
  SliderBtMontaneMaxTemp -> SliderFloatRange 0.2 0.8
  SliderBtMontanePrecip -> SliderFloatRange 0.1 0.6
  SliderBtCliffSlope -> SliderFloatRange 0.2 0.8
  SliderBtValleyMoisture -> SliderFloatRange 0.3 1.0
  SliderBtDepressionMoisture -> SliderFloatRange 0.2 1.0
  SliderBtPrecipWeight -> SliderFloatRange 0.5 5.0
  SliderVbcTempMin -> SliderFloatRange 0 0.3
  SliderVbcTempRange -> SliderFloatRange 0.1 1.0
  SliderVbcFertilityBoost -> SliderFloatRange 0 1
  SliderVbcAlbedoBase -> SliderFloatRange 0 0.3
  SliderVbcAlbedoBare -> SliderFloatRange 0.1 0.5
  SliderVbcAlbedoVeg -> SliderFloatRange 0 0.3
  SliderVbcOceanAlbedo -> SliderFloatRange 0 0.2
  SliderVbcIceAlbedo -> SliderFloatRange 0.3 1.0
  SliderBiomeSmoothing -> SliderIntRange 0 5
  SliderVolcanicAshBoost -> SliderFloatRange 0 0.5
  SliderVolcanicLavaPenalty -> SliderFloatRange 0 0.8
  SliderBiomeFeedbackBlend -> SliderFloatRange 0 1
  SliderErosionHydraulic -> SliderIntRange 1 12
  SliderErosionThermal -> SliderIntRange 1 12
  SliderErosionRainRate -> SliderFloatRange 0.05 0.5
  SliderErosionTalus -> SliderFloatRange 0.01 0.15
  SliderErosionMaxDrop -> SliderFloatRange 0.1 1.0
  SliderErosionHydDeposit -> SliderFloatRange 0.0 0.8
  SliderErosionDepositSlope -> SliderFloatRange 0.01 0.15
  SliderErosionThermDeposit -> SliderFloatRange 0.0 1.0
  SliderErosionCoastZone -> SliderFloatRange 0.005 0.12
  SliderErosionCoastStrength -> SliderFloatRange 0.0 0.8
  SliderErosionCoastIter -> SliderIntRange 1 8
  SliderHypsometryEnabled -> SliderToggle
  SliderHypsometryLowlandExp -> SliderFloatRange 0.5 5.0
  SliderHypsometryHighlandExp -> SliderFloatRange 0.3 3.0
  SliderHypsometryPlateauBreak -> SliderFloatRange 0.52 0.75
  SliderHypsometryOceanExp -> SliderFloatRange 0.2 1.0
  SliderHypsometryCoastalRampWidth -> SliderFloatRange 0.005 0.20
  SliderHypsometryCoastalRampStr -> SliderFloatRange 0.0 1.0
  SliderGlacierSnowTemp -> SliderFloatRange 0.0 0.5
  SliderGlacierSnowRange -> SliderFloatRange 0.1 0.7
  SliderGlacierMeltTemp -> SliderFloatRange 0.1 0.8
  SliderGlacierMeltRate -> SliderFloatRange 0.0 1.0
  SliderGlacierAccumScale -> SliderFloatRange 0.0 3.0
  SliderGlacierFlowIters -> SliderIntRange 0 10
  SliderGlacierFlowRate -> SliderFloatRange 0.0 1.0
  SliderGlacierErosionScale -> SliderFloatRange 0.0 1.0
  SliderGlacierCarveScale -> SliderFloatRange 0.0 0.1
  SliderGlacierDepositScale -> SliderFloatRange 0.0 1.0
  SliderVentDensity -> SliderFloatRange 0.0 0.2
  SliderVentThreshold -> SliderFloatRange 0.2 0.9
  SliderHotspotScale -> SliderFloatRange 0.0 1.0
  SliderHotspotThreshold -> SliderFloatRange 0.3 0.95
  SliderMagmaRecharge -> SliderFloatRange 0.0 3.0
  SliderLavaScale -> SliderFloatRange 0.0 1.0
  SliderAshScale -> SliderFloatRange 0.0 1.0
  SliderVolcanicDepositScale -> SliderFloatRange 0.0 1.0
  SliderSoilMoistureThreshold -> SliderFloatRange 0.0 1.0
  SliderSoilHardnessThreshold -> SliderFloatRange 0.0 1.0
  SliderSoilFertilityMoistWeight -> SliderFloatRange 0.0 1.0
  SliderSoilFertilityDepthWeight -> SliderFloatRange 0.0 1.0
  SliderSinkBreachDepth -> SliderFloatRange 0.0 0.1
  SliderStreamPowerMaxErosion -> SliderFloatRange 0.0 0.2
  SliderRiverCarveMaxDepth -> SliderFloatRange 0.0 0.2
  SliderCoastalErodeStrength -> SliderFloatRange 0.0 0.1
  SliderHydroHardnessWeight -> SliderFloatRange 0.0 1.0
  SliderMinLakeSize -> SliderIntRange 1 50
  SliderInlandSeaMinSize -> SliderIntRange 50 500
  SliderRoughnessScale -> SliderFloatRange 0.0 2.0