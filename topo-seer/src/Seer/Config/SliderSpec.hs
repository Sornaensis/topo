{-# LANGUAGE OverloadedStrings #-}

-- | Slider specifications for the config UI.
--
-- Each 'SliderSpec' bundles a slider's display name, tooltip text,
-- domain range @[lo..hi]@, and whether to format as an integer.
-- The central 'sliderLabel' function produces labels in the format
-- @(?) Name: x\/y@ used by the config panel.
--
-- Tooltip text is keyed by 'WidgetId' via 'tooltipForWidget', which
-- maps each minus/plus/bar widget to the same description.
module Seer.Config.SliderSpec
  ( SliderSpec(..)
  , sliderLabel
  , tooltipForWidget
    -- * Terrain tab specs
  , specGenScale
  , specGenCoordScale
  , specGenOffsetX
  , specGenOffsetY
  , specGenFrequency
  , specGenOctaves
  , specGenLacunarity
  , specGenGain
  , specGenWarpScale
  , specGenWarpStrength
  , specExtentX
  , specExtentY
  , specEdgeNorth
  , specEdgeSouth
  , specEdgeEast
  , specEdgeWest
  , specEdgeFalloff
  , specPlateSize
  , specUplift
  , specRiftDepth
  , specDetailScale
  , specPlateSpeed
  , specBoundarySharpness
  , specBoundaryNoiseScale
  , specBoundaryNoiseStrength
  , specBoundaryWarpOctaves
  , specBoundaryWarpLacunarity
  , specBoundaryWarpGain
  , specPlateMergeScale
  , specPlateMergeBias
  , specPlateDetailScale
  , specPlateDetailStrength
  , specPlateRidgeStrength
  , specPlateHeightBase
  , specPlateHeightVariance
  , specPlateHardnessBase
  , specPlateHardnessVariance
  , specTrenchDepth
  , specRidgeHeight
  , specPlateBiasStrength
  , specPlateBiasCenter
  , specPlateBiasEdge
  , specPlateBiasNorth
  , specPlateBiasSouth
  , specTfcCliffSlope
  , specTfcMountainSlope
  , specTfcMountainRelief
  , specTfcHillSlope
  , specTfcRollingSlope
  , specValleyCurvature
  , specRockElevationThreshold
  , specRockHardnessThreshold
  , specRockHardnessSecondary
    -- * Climate tab specs
  , specWaterLevel
  , specEvaporation
  , specRainShadow
  , specWindDiffuse
  , specEquatorTemp
  , specPoleTemp
  , specLapseRate
  , specWindIterations
  , specMoistureIterations
  , specWeatherTick
  , specWeatherPhase
  , specWeatherAmplitude
  , specSeasonCycleLength
  , specJitterAmplitude
  , specPressureBase
  , specPressureTempScale
  , specPressureCoriolisScale
  , specSeasonalBase
  , specSeasonalRange
  , specHumidityNoiseScale
  , specPrecipNoiseScale
  , specWeatherITCZWidth
  , specWeatherITCZPrecipBoost
  , specPressureHumidityScale
  , specPressureGradientWindScale
  , specWindNoiseScale
  , specITCZMigrationScale
  , specCloudRHExponent
  , specCloudAlbedoEffect
  , specCloudPrecipBoost
  , specVegBase
  , specVegBoost
  , specVegTempWeight
  , specVegPrecipWeight
  , specBtCoastalBand
  , specBtSnowElevation
  , specBtAlpineElevation
  , specBtIceCapTemp
  , specBtMontaneLow
  , specBtMontanePrecip
  , specBtCliffSlope
  , specBtValleyMoisture
  , specBtDepressionMoisture
  , specBtPrecipWeight
  , specVbcTempMin
  , specVbcTempRange
  , specVbcFertilityBoost
  , specVbcAlbedoBase
  , specVbcAlbedoBare
  , specVbcAlbedoVeg
  , specVbcOceanAlbedo
  , specVbcIceAlbedo
  , specBiomeSmoothing
  , specVolcanicAshBoost
  , specVolcanicLavaPenalty
  , specBiomeFeedbackBlend
  , specBoundaryMotionTemp
  , specBoundaryMotionPrecip
  , specPlanetRadius
  , specAxialTilt
  , specInsolation
  , specOccWarmScale
  , specOccColdScale
  , specOccLatPeakDeg
  , specOccLatWidthDeg
  , specSliceLatCenter
  , specSliceLonCenter
  , specLatitudeExponent
  , specPlateHeightCooling
  , specTempNoiseScale
  , specOceanModeration
  , specOceanModerateTemp
  , specAlbedoSensitivity
  , specAlbedoReference
  , specMoistAdvect
  , specMoistLocal
  , specMoistWindEvapScale
  , specMoistEvapNoiseScale
  , specMoistLandETCoeff
  , specMoistBareEvapFrac
  , specMoistVegTranspFrac
  , specMoistWindETScale
  , specMoistCondensationRate
  , specMoistRecycleRate
  , specMoistITCZStrength
  , specMoistITCZWidth
    -- * Precipitation tab specs
  , specOrographicScale
  , specOrographicStep
  , specCoastalIterations
  , specCoastalDiffuse
  , specCoastalMoistureBoost
    -- * Wind specs
  , specWindBeltStrength
  , specWindBeltHarmonics
  , specWindBeltBase
  , specWindBeltRange
  , specWindBeltSpeedScale
    -- * Boundary model specs
  , specBndLandRange
  , specBndTempConvergent
  , specBndTempDivergent
  , specBndTempTransform
  , specBndPrecipConvergent
  , specBndPrecipDivergent
  , specBndPrecipTransform
    -- * Erosion tab specs
  , specErosionHydraulic
  , specErosionThermal
  , specErosionRainRate
  , specErosionTalus
  , specErosionMaxDrop
  , specGlacierSnowTemp
  , specGlacierSnowRange
  , specGlacierMeltTemp
  , specGlacierMeltRate
  , specGlacierAccumScale
  , specGlacierFlowIters
  , specGlacierFlowRate
  , specGlacierErosionScale
  , specGlacierCarveScale
  , specGlacierDepositScale
  , specVentDensity
  , specVentThreshold
  , specHotspotScale
  , specHotspotThreshold
  , specMagmaRecharge
  , specLavaScale
  , specAshScale
  , specVolcanicDepositScale
  , specSoilMoistureThreshold
  , specSoilHardnessThreshold
  , specSoilFertilityMoistWeight
  , specSoilFertilityDepthWeight
  , specSinkBreachDepth
  , specStreamPowerMaxErosion
  , specRiverCarveMaxDepth
  , specCoastalErodeStrength
  , specHydroHardnessWeight
  , specMinLakeSize
  , specInlandSeaMinSize
  , specRoughnessScale
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showFFloat)
import Seer.Config (mapRange)
import UI.WidgetTree (WidgetId(..))

-- | Specification for a single config slider.
data SliderSpec = SliderSpec
  { ssName    :: !Text
  -- ^ Display name shown in the label.
  , ssTooltip :: !Text
  -- ^ Tooltip text shown on hover.
  , ssLo      :: !Float
  -- ^ Minimum of the domain range.
  , ssHi      :: !Float
  -- ^ Maximum of the domain range.
  , ssIsInt   :: !Bool
  -- ^ When 'True', format values without decimals.
  } deriving (Eq, Show)

-- | Format a slider label as @(?) Name: x\/y@.
--
-- The normalised value @t ∈ [0,1]@ is mapped through the spec's range.
-- Integer specs display without decimals; float specs use 2 decimal places.
--
-- >>> sliderLabel (SliderSpec "Octaves" "" 2 8 True) 0.5
-- "(?) Octaves: 5/8"
-- >>> sliderLabel (SliderSpec "Scale" "" 0.2 2.0 False) 0.0
-- "(?) Scale: 0.20/2.00"
sliderLabel :: SliderSpec -> Float -> Text
sliderLabel spec t =
  let val = mapRange (ssLo spec) (ssHi spec) t
      maxVal = ssHi spec
      fmt = if ssIsInt spec then fmtInt else fmtFloat
  in "(?) " <> ssName spec <> ": " <> fmt val <> "/" <> fmt maxVal

-- | Format a float with 2 decimal places.
fmtFloat :: Float -> Text
fmtFloat x = Text.pack (showFFloat (Just 2) x "")

-- | Format a float as a rounded integer.
fmtInt :: Float -> Text
fmtInt x = Text.pack (show (round x :: Int))

-- | Look up tooltip text for a widget.
--
-- Returns 'Just' for every config slider widget (minus, plus, and bar),
-- 'Nothing' for non-config widgets.
tooltipForWidget :: WidgetId -> Maybe Text
tooltipForWidget wid = case wid of
  -- Terrain tab
  WidgetConfigGenScaleMinus          -> tip specGenScale
  WidgetConfigGenScalePlus           -> tip specGenScale
  WidgetConfigGenCoordScaleMinus     -> tip specGenCoordScale
  WidgetConfigGenCoordScalePlus      -> tip specGenCoordScale
  WidgetConfigGenOffsetXMinus        -> tip specGenOffsetX
  WidgetConfigGenOffsetXPlus         -> tip specGenOffsetX
  WidgetConfigGenOffsetYMinus        -> tip specGenOffsetY
  WidgetConfigGenOffsetYPlus         -> tip specGenOffsetY
  WidgetConfigGenFrequencyMinus      -> tip specGenFrequency
  WidgetConfigGenFrequencyPlus       -> tip specGenFrequency
  WidgetConfigGenOctavesMinus        -> tip specGenOctaves
  WidgetConfigGenOctavesPlus         -> tip specGenOctaves
  WidgetConfigGenLacunarityMinus     -> tip specGenLacunarity
  WidgetConfigGenLacunarityPlus      -> tip specGenLacunarity
  WidgetConfigGenGainMinus           -> tip specGenGain
  WidgetConfigGenGainPlus            -> tip specGenGain
  WidgetConfigGenWarpScaleMinus      -> tip specGenWarpScale
  WidgetConfigGenWarpScalePlus       -> tip specGenWarpScale
  WidgetConfigGenWarpStrengthMinus   -> tip specGenWarpStrength
  WidgetConfigGenWarpStrengthPlus    -> tip specGenWarpStrength
  WidgetConfigExtentXMinus           -> tip specExtentX
  WidgetConfigExtentXPlus            -> tip specExtentX
  WidgetConfigExtentYMinus           -> tip specExtentY
  WidgetConfigExtentYPlus            -> tip specExtentY
  WidgetConfigEdgeNorthMinus         -> tip specEdgeNorth
  WidgetConfigEdgeNorthPlus          -> tip specEdgeNorth
  WidgetConfigEdgeSouthMinus         -> tip specEdgeSouth
  WidgetConfigEdgeSouthPlus          -> tip specEdgeSouth
  WidgetConfigEdgeEastMinus          -> tip specEdgeEast
  WidgetConfigEdgeEastPlus           -> tip specEdgeEast
  WidgetConfigEdgeWestMinus          -> tip specEdgeWest
  WidgetConfigEdgeWestPlus           -> tip specEdgeWest
  WidgetConfigEdgeFalloffMinus       -> tip specEdgeFalloff
  WidgetConfigEdgeFalloffPlus        -> tip specEdgeFalloff
  WidgetConfigPlateSizeMinus         -> tip specPlateSize
  WidgetConfigPlateSizePlus          -> tip specPlateSize
  WidgetConfigUpliftMinus            -> tip specUplift
  WidgetConfigUpliftPlus             -> tip specUplift
  WidgetConfigRiftDepthMinus         -> tip specRiftDepth
  WidgetConfigRiftDepthPlus          -> tip specRiftDepth
  WidgetConfigDetailScaleMinus       -> tip specDetailScale
  WidgetConfigDetailScalePlus        -> tip specDetailScale
  WidgetConfigPlateSpeedMinus        -> tip specPlateSpeed
  WidgetConfigPlateSpeedPlus         -> tip specPlateSpeed
  WidgetConfigBoundarySharpnessMinus -> tip specBoundarySharpness
  WidgetConfigBoundarySharpnessPlus  -> tip specBoundarySharpness
  WidgetConfigBoundaryNoiseScaleMinus -> tip specBoundaryNoiseScale
  WidgetConfigBoundaryNoiseScalePlus  -> tip specBoundaryNoiseScale
  WidgetConfigBoundaryNoiseStrengthMinus -> tip specBoundaryNoiseStrength
  WidgetConfigBoundaryNoiseStrengthPlus  -> tip specBoundaryNoiseStrength
  WidgetConfigBoundaryWarpOctavesMinus -> tip specBoundaryWarpOctaves
  WidgetConfigBoundaryWarpOctavesPlus  -> tip specBoundaryWarpOctaves
  WidgetConfigBoundaryWarpLacunarityMinus -> tip specBoundaryWarpLacunarity
  WidgetConfigBoundaryWarpLacunarityPlus  -> tip specBoundaryWarpLacunarity
  WidgetConfigBoundaryWarpGainMinus  -> tip specBoundaryWarpGain
  WidgetConfigBoundaryWarpGainPlus   -> tip specBoundaryWarpGain
  WidgetConfigPlateMergeScaleMinus   -> tip specPlateMergeScale
  WidgetConfigPlateMergeScalePlus    -> tip specPlateMergeScale
  WidgetConfigPlateMergeBiasMinus    -> tip specPlateMergeBias
  WidgetConfigPlateMergeBiasPlus     -> tip specPlateMergeBias
  WidgetConfigPlateDetailScaleMinus  -> tip specPlateDetailScale
  WidgetConfigPlateDetailScalePlus   -> tip specPlateDetailScale
  WidgetConfigPlateDetailStrengthMinus -> tip specPlateDetailStrength
  WidgetConfigPlateDetailStrengthPlus  -> tip specPlateDetailStrength
  WidgetConfigPlateRidgeStrengthMinus -> tip specPlateRidgeStrength
  WidgetConfigPlateRidgeStrengthPlus  -> tip specPlateRidgeStrength
  WidgetConfigPlateHeightBaseMinus   -> tip specPlateHeightBase
  WidgetConfigPlateHeightBasePlus    -> tip specPlateHeightBase
  WidgetConfigPlateHeightVarianceMinus -> tip specPlateHeightVariance
  WidgetConfigPlateHeightVariancePlus  -> tip specPlateHeightVariance
  WidgetConfigPlateHardnessBaseMinus -> tip specPlateHardnessBase
  WidgetConfigPlateHardnessBasePlus  -> tip specPlateHardnessBase
  WidgetConfigPlateHardnessVarianceMinus -> tip specPlateHardnessVariance
  WidgetConfigPlateHardnessVariancePlus  -> tip specPlateHardnessVariance
  WidgetConfigTrenchDepthMinus       -> tip specTrenchDepth
  WidgetConfigTrenchDepthPlus        -> tip specTrenchDepth
  WidgetConfigRidgeHeightMinus       -> tip specRidgeHeight
  WidgetConfigRidgeHeightPlus        -> tip specRidgeHeight
  WidgetConfigPlateBiasStrengthMinus -> tip specPlateBiasStrength
  WidgetConfigPlateBiasStrengthPlus  -> tip specPlateBiasStrength
  WidgetConfigPlateBiasCenterMinus   -> tip specPlateBiasCenter
  WidgetConfigPlateBiasCenterPlus    -> tip specPlateBiasCenter
  WidgetConfigPlateBiasEdgeMinus     -> tip specPlateBiasEdge
  WidgetConfigPlateBiasEdgePlus      -> tip specPlateBiasEdge
  WidgetConfigPlateBiasNorthMinus    -> tip specPlateBiasNorth
  WidgetConfigPlateBiasNorthPlus     -> tip specPlateBiasNorth
  WidgetConfigPlateBiasSouthMinus    -> tip specPlateBiasSouth
  WidgetConfigPlateBiasSouthPlus     -> tip specPlateBiasSouth
  WidgetConfigTfcCliffSlopeMinus     -> tip specTfcCliffSlope
  WidgetConfigTfcCliffSlopePlus      -> tip specTfcCliffSlope
  WidgetConfigTfcMountainSlopeMinus  -> tip specTfcMountainSlope
  WidgetConfigTfcMountainSlopePlus   -> tip specTfcMountainSlope
  WidgetConfigTfcMountainReliefMinus -> tip specTfcMountainRelief
  WidgetConfigTfcMountainReliefPlus  -> tip specTfcMountainRelief
  WidgetConfigTfcHillSlopeMinus      -> tip specTfcHillSlope
  WidgetConfigTfcHillSlopePlus       -> tip specTfcHillSlope
  WidgetConfigTfcRollingSlopeMinus   -> tip specTfcRollingSlope
  WidgetConfigTfcRollingSlopePlus    -> tip specTfcRollingSlope
  WidgetConfigValleyCurvatureMinus   -> tip specValleyCurvature
  WidgetConfigValleyCurvaturePlus    -> tip specValleyCurvature
  WidgetConfigRockElevationThresholdMinus -> tip specRockElevationThreshold
  WidgetConfigRockElevationThresholdPlus  -> tip specRockElevationThreshold
  WidgetConfigRockHardnessThresholdMinus  -> tip specRockHardnessThreshold
  WidgetConfigRockHardnessThresholdPlus   -> tip specRockHardnessThreshold
  WidgetConfigRockHardnessSecondaryMinus  -> tip specRockHardnessSecondary
  WidgetConfigRockHardnessSecondaryPlus   -> tip specRockHardnessSecondary
  -- Climate tab
  WidgetConfigWaterMinus             -> tip specWaterLevel
  WidgetConfigWaterPlus              -> tip specWaterLevel
  WidgetConfigEvapMinus              -> tip specEvaporation
  WidgetConfigEvapPlus               -> tip specEvaporation
  WidgetConfigRainShadowMinus        -> tip specRainShadow
  WidgetConfigRainShadowPlus         -> tip specRainShadow
  WidgetConfigWindDiffuseMinus       -> tip specWindDiffuse
  WidgetConfigWindDiffusePlus        -> tip specWindDiffuse
  WidgetConfigEquatorTempMinus       -> tip specEquatorTemp
  WidgetConfigEquatorTempPlus        -> tip specEquatorTemp
  WidgetConfigPoleTempMinus          -> tip specPoleTemp
  WidgetConfigPoleTempPlus           -> tip specPoleTemp
  WidgetConfigLapseRateMinus         -> tip specLapseRate
  WidgetConfigLapseRatePlus          -> tip specLapseRate
  WidgetConfigWindIterationsMinus    -> tip specWindIterations
  WidgetConfigWindIterationsPlus     -> tip specWindIterations
  WidgetConfigMoistureIterationsMinus -> tip specMoistureIterations
  WidgetConfigMoistureIterationsPlus  -> tip specMoistureIterations
  WidgetConfigWeatherTickMinus       -> tip specWeatherTick
  WidgetConfigWeatherTickPlus        -> tip specWeatherTick
  WidgetConfigWeatherPhaseMinus      -> tip specWeatherPhase
  WidgetConfigWeatherPhasePlus       -> tip specWeatherPhase
  WidgetConfigWeatherAmplitudeMinus  -> tip specWeatherAmplitude
  WidgetConfigWeatherAmplitudePlus   -> tip specWeatherAmplitude
  WidgetConfigSeasonCycleLengthMinus -> tip specSeasonCycleLength
  WidgetConfigSeasonCycleLengthPlus  -> tip specSeasonCycleLength
  WidgetConfigJitterAmplitudeMinus   -> tip specJitterAmplitude
  WidgetConfigJitterAmplitudePlus    -> tip specJitterAmplitude
  WidgetConfigPressureBaseMinus      -> tip specPressureBase
  WidgetConfigPressureBasePlus       -> tip specPressureBase
  WidgetConfigPressureTempScaleMinus -> tip specPressureTempScale
  WidgetConfigPressureTempScalePlus  -> tip specPressureTempScale
  WidgetConfigPressureCoriolisScaleMinus -> tip specPressureCoriolisScale
  WidgetConfigPressureCoriolisScalePlus  -> tip specPressureCoriolisScale
  WidgetConfigSeasonalBaseMinus      -> tip specSeasonalBase
  WidgetConfigSeasonalBasePlus       -> tip specSeasonalBase
  WidgetConfigSeasonalRangeMinus     -> tip specSeasonalRange
  WidgetConfigSeasonalRangePlus      -> tip specSeasonalRange
  WidgetConfigHumidityNoiseScaleMinus -> tip specHumidityNoiseScale
  WidgetConfigHumidityNoiseScalePlus  -> tip specHumidityNoiseScale
  WidgetConfigPrecipNoiseScaleMinus  -> tip specPrecipNoiseScale
  WidgetConfigPrecipNoiseScalePlus   -> tip specPrecipNoiseScale
  WidgetConfigWeatherITCZWidthMinus  -> tip specWeatherITCZWidth
  WidgetConfigWeatherITCZWidthPlus   -> tip specWeatherITCZWidth
  WidgetConfigWeatherITCZPrecipBoostMinus -> tip specWeatherITCZPrecipBoost
  WidgetConfigWeatherITCZPrecipBoostPlus  -> tip specWeatherITCZPrecipBoost
  WidgetConfigPressureHumidityScaleMinus -> tip specPressureHumidityScale
  WidgetConfigPressureHumidityScalePlus  -> tip specPressureHumidityScale
  WidgetConfigPressureGradientWindScaleMinus -> tip specPressureGradientWindScale
  WidgetConfigPressureGradientWindScalePlus  -> tip specPressureGradientWindScale
  WidgetConfigWindNoiseScaleMinus    -> tip specWindNoiseScale
  WidgetConfigWindNoiseScalePlus     -> tip specWindNoiseScale
  WidgetConfigITCZMigrationScaleMinus -> tip specITCZMigrationScale
  WidgetConfigITCZMigrationScalePlus  -> tip specITCZMigrationScale
  WidgetConfigCloudRHExponentMinus   -> tip specCloudRHExponent
  WidgetConfigCloudRHExponentPlus    -> tip specCloudRHExponent
  WidgetConfigCloudAlbedoEffectMinus -> tip specCloudAlbedoEffect
  WidgetConfigCloudAlbedoEffectPlus  -> tip specCloudAlbedoEffect
  WidgetConfigCloudPrecipBoostMinus  -> tip specCloudPrecipBoost
  WidgetConfigCloudPrecipBoostPlus   -> tip specCloudPrecipBoost
  WidgetConfigVegBaseMinus           -> tip specVegBase
  WidgetConfigVegBasePlus            -> tip specVegBase
  WidgetConfigVegBoostMinus          -> tip specVegBoost
  WidgetConfigVegBoostPlus           -> tip specVegBoost
  WidgetConfigVegTempWeightMinus     -> tip specVegTempWeight
  WidgetConfigVegTempWeightPlus      -> tip specVegTempWeight
  WidgetConfigVegPrecipWeightMinus   -> tip specVegPrecipWeight
  WidgetConfigVegPrecipWeightPlus    -> tip specVegPrecipWeight
  WidgetConfigBtCoastalBandMinus     -> tip specBtCoastalBand
  WidgetConfigBtCoastalBandPlus      -> tip specBtCoastalBand
  WidgetConfigBtSnowElevationMinus   -> tip specBtSnowElevation
  WidgetConfigBtSnowElevationPlus    -> tip specBtSnowElevation
  WidgetConfigBtAlpineElevationMinus -> tip specBtAlpineElevation
  WidgetConfigBtAlpineElevationPlus  -> tip specBtAlpineElevation
  WidgetConfigBtIceCapTempMinus      -> tip specBtIceCapTemp
  WidgetConfigBtIceCapTempPlus       -> tip specBtIceCapTemp
  WidgetConfigBtMontaneLowMinus      -> tip specBtMontaneLow
  WidgetConfigBtMontaneLowPlus       -> tip specBtMontaneLow
  WidgetConfigBtMontanePrecipMinus   -> tip specBtMontanePrecip
  WidgetConfigBtMontanePrecipPlus    -> tip specBtMontanePrecip
  WidgetConfigBtCliffSlopeMinus      -> tip specBtCliffSlope
  WidgetConfigBtCliffSlopePlus       -> tip specBtCliffSlope
  WidgetConfigBtValleyMoistureMinus  -> tip specBtValleyMoisture
  WidgetConfigBtValleyMoisturePlus   -> tip specBtValleyMoisture
  WidgetConfigBtDepressionMoistureMinus -> tip specBtDepressionMoisture
  WidgetConfigBtDepressionMoisturePlus  -> tip specBtDepressionMoisture
  WidgetConfigBtPrecipWeightMinus    -> tip specBtPrecipWeight
  WidgetConfigBtPrecipWeightPlus     -> tip specBtPrecipWeight
  WidgetConfigVbcTempMinMinus        -> tip specVbcTempMin
  WidgetConfigVbcTempMinPlus         -> tip specVbcTempMin
  WidgetConfigVbcTempRangeMinus      -> tip specVbcTempRange
  WidgetConfigVbcTempRangePlus       -> tip specVbcTempRange
  WidgetConfigVbcFertilityBoostMinus -> tip specVbcFertilityBoost
  WidgetConfigVbcFertilityBoostPlus  -> tip specVbcFertilityBoost
  WidgetConfigVbcAlbedoBaseMinus     -> tip specVbcAlbedoBase
  WidgetConfigVbcAlbedoBasePlus      -> tip specVbcAlbedoBase
  WidgetConfigVbcAlbedoBareMinus     -> tip specVbcAlbedoBare
  WidgetConfigVbcAlbedoBarePlus      -> tip specVbcAlbedoBare
  WidgetConfigVbcAlbedoVegMinus      -> tip specVbcAlbedoVeg
  WidgetConfigVbcAlbedoVegPlus       -> tip specVbcAlbedoVeg
  WidgetConfigVbcOceanAlbedoMinus    -> tip specVbcOceanAlbedo
  WidgetConfigVbcOceanAlbedoPlus     -> tip specVbcOceanAlbedo
  WidgetConfigVbcIceAlbedoMinus      -> tip specVbcIceAlbedo
  WidgetConfigVbcIceAlbedoPlus       -> tip specVbcIceAlbedo
  WidgetConfigBiomeSmoothingMinus    -> tip specBiomeSmoothing
  WidgetConfigBiomeSmoothingPlus     -> tip specBiomeSmoothing
  WidgetConfigVolcanicAshBoostMinus  -> tip specVolcanicAshBoost
  WidgetConfigVolcanicAshBoostPlus   -> tip specVolcanicAshBoost
  WidgetConfigVolcanicLavaPenaltyMinus -> tip specVolcanicLavaPenalty
  WidgetConfigVolcanicLavaPenaltyPlus  -> tip specVolcanicLavaPenalty
  WidgetConfigBiomeFeedbackBlendMinus -> tip specBiomeFeedbackBlend
  WidgetConfigBiomeFeedbackBlendPlus  -> tip specBiomeFeedbackBlend
  WidgetConfigBoundaryMotionTempMinus -> tip specBoundaryMotionTemp
  WidgetConfigBoundaryMotionTempPlus  -> tip specBoundaryMotionTemp
  WidgetConfigBoundaryMotionPrecipMinus -> tip specBoundaryMotionPrecip
  WidgetConfigBoundaryMotionPrecipPlus  -> tip specBoundaryMotionPrecip
  WidgetConfigPlanetRadiusMinus      -> tip specPlanetRadius
  WidgetConfigPlanetRadiusPlus       -> tip specPlanetRadius
  WidgetConfigAxialTiltMinus         -> tip specAxialTilt
  WidgetConfigAxialTiltPlus          -> tip specAxialTilt
  WidgetConfigInsolationMinus        -> tip specInsolation
  WidgetConfigInsolationPlus         -> tip specInsolation
  WidgetConfigOccWarmScaleMinus      -> tip specOccWarmScale
  WidgetConfigOccWarmScalePlus       -> tip specOccWarmScale
  WidgetConfigOccColdScaleMinus      -> tip specOccColdScale
  WidgetConfigOccColdScalePlus       -> tip specOccColdScale
  WidgetConfigOccLatPeakDegMinus     -> tip specOccLatPeakDeg
  WidgetConfigOccLatPeakDegPlus      -> tip specOccLatPeakDeg
  WidgetConfigOccLatWidthDegMinus    -> tip specOccLatWidthDeg
  WidgetConfigOccLatWidthDegPlus     -> tip specOccLatWidthDeg
  WidgetConfigSliceLatCenterMinus    -> tip specSliceLatCenter
  WidgetConfigSliceLatCenterPlus     -> tip specSliceLatCenter
  WidgetConfigSliceLonCenterMinus    -> tip specSliceLonCenter
  WidgetConfigSliceLonCenterPlus     -> tip specSliceLonCenter
  WidgetConfigLatitudeExponentMinus  -> tip specLatitudeExponent
  WidgetConfigLatitudeExponentPlus   -> tip specLatitudeExponent
  WidgetConfigPlateHeightCoolingMinus -> tip specPlateHeightCooling
  WidgetConfigPlateHeightCoolingPlus -> tip specPlateHeightCooling
  WidgetConfigTempNoiseScaleMinus    -> tip specTempNoiseScale
  WidgetConfigTempNoiseScalePlus     -> tip specTempNoiseScale
  WidgetConfigOceanModerationMinus   -> tip specOceanModeration
  WidgetConfigOceanModerationPlus    -> tip specOceanModeration
  WidgetConfigOceanModerateTempMinus -> tip specOceanModerateTemp
  WidgetConfigOceanModerateTempPlus  -> tip specOceanModerateTemp
  WidgetConfigAlbedoSensitivityMinus -> tip specAlbedoSensitivity
  WidgetConfigAlbedoSensitivityPlus  -> tip specAlbedoSensitivity
  WidgetConfigAlbedoReferenceMinus   -> tip specAlbedoReference
  WidgetConfigAlbedoReferencePlus    -> tip specAlbedoReference
  -- Moisture
  WidgetConfigMoistAdvectMinus       -> tip specMoistAdvect
  WidgetConfigMoistAdvectPlus        -> tip specMoistAdvect
  WidgetConfigMoistLocalMinus        -> tip specMoistLocal
  WidgetConfigMoistLocalPlus         -> tip specMoistLocal
  WidgetConfigMoistWindEvapScaleMinus -> tip specMoistWindEvapScale
  WidgetConfigMoistWindEvapScalePlus  -> tip specMoistWindEvapScale
  WidgetConfigMoistEvapNoiseScaleMinus -> tip specMoistEvapNoiseScale
  WidgetConfigMoistEvapNoiseScalePlus  -> tip specMoistEvapNoiseScale
  WidgetConfigMoistLandETCoeffMinus  -> tip specMoistLandETCoeff
  WidgetConfigMoistLandETCoeffPlus   -> tip specMoistLandETCoeff
  WidgetConfigMoistBareEvapFracMinus -> tip specMoistBareEvapFrac
  WidgetConfigMoistBareEvapFracPlus  -> tip specMoistBareEvapFrac
  WidgetConfigMoistVegTranspFracMinus -> tip specMoistVegTranspFrac
  WidgetConfigMoistVegTranspFracPlus  -> tip specMoistVegTranspFrac
  WidgetConfigMoistWindETScaleMinus  -> tip specMoistWindETScale
  WidgetConfigMoistWindETScalePlus   -> tip specMoistWindETScale
  WidgetConfigMoistCondensationRateMinus -> tip specMoistCondensationRate
  WidgetConfigMoistCondensationRatePlus  -> tip specMoistCondensationRate
  WidgetConfigMoistRecycleRateMinus  -> tip specMoistRecycleRate
  WidgetConfigMoistRecycleRatePlus   -> tip specMoistRecycleRate
  WidgetConfigMoistITCZStrengthMinus -> tip specMoistITCZStrength
  WidgetConfigMoistITCZStrengthPlus  -> tip specMoistITCZStrength
  WidgetConfigMoistITCZWidthMinus    -> tip specMoistITCZWidth
  WidgetConfigMoistITCZWidthPlus     -> tip specMoistITCZWidth
  -- Precipitation
  WidgetConfigOrographicScaleMinus   -> tip specOrographicScale
  WidgetConfigOrographicScalePlus    -> tip specOrographicScale
  WidgetConfigOrographicStepMinus    -> tip specOrographicStep
  WidgetConfigOrographicStepPlus     -> tip specOrographicStep
  WidgetConfigCoastalIterationsMinus -> tip specCoastalIterations
  WidgetConfigCoastalIterationsPlus  -> tip specCoastalIterations
  WidgetConfigCoastalDiffuseMinus    -> tip specCoastalDiffuse
  WidgetConfigCoastalDiffusePlus     -> tip specCoastalDiffuse
  WidgetConfigCoastalMoistureBoostMinus -> tip specCoastalMoistureBoost
  WidgetConfigCoastalMoistureBoostPlus  -> tip specCoastalMoistureBoost
  -- Wind
  WidgetConfigWindBeltStrengthMinus  -> tip specWindBeltStrength
  WidgetConfigWindBeltStrengthPlus   -> tip specWindBeltStrength
  WidgetConfigWindBeltHarmonicsMinus -> tip specWindBeltHarmonics
  WidgetConfigWindBeltHarmonicsPlus  -> tip specWindBeltHarmonics
  WidgetConfigWindBeltBaseMinus      -> tip specWindBeltBase
  WidgetConfigWindBeltBasePlus       -> tip specWindBeltBase
  WidgetConfigWindBeltRangeMinus     -> tip specWindBeltRange
  WidgetConfigWindBeltRangePlus      -> tip specWindBeltRange
  WidgetConfigWindBeltSpeedScaleMinus -> tip specWindBeltSpeedScale
  WidgetConfigWindBeltSpeedScalePlus  -> tip specWindBeltSpeedScale
  -- Boundary model
  WidgetConfigBndLandRangeMinus       -> tip specBndLandRange
  WidgetConfigBndLandRangePlus        -> tip specBndLandRange
  WidgetConfigBndTempConvergentMinus  -> tip specBndTempConvergent
  WidgetConfigBndTempConvergentPlus   -> tip specBndTempConvergent
  WidgetConfigBndTempDivergentMinus   -> tip specBndTempDivergent
  WidgetConfigBndTempDivergentPlus    -> tip specBndTempDivergent
  WidgetConfigBndTempTransformMinus   -> tip specBndTempTransform
  WidgetConfigBndTempTransformPlus    -> tip specBndTempTransform
  WidgetConfigBndPrecipConvergentMinus -> tip specBndPrecipConvergent
  WidgetConfigBndPrecipConvergentPlus  -> tip specBndPrecipConvergent
  WidgetConfigBndPrecipDivergentMinus -> tip specBndPrecipDivergent
  WidgetConfigBndPrecipDivergentPlus  -> tip specBndPrecipDivergent
  WidgetConfigBndPrecipTransformMinus -> tip specBndPrecipTransform
  WidgetConfigBndPrecipTransformPlus  -> tip specBndPrecipTransform
  -- Erosion tab
  WidgetConfigErosionHydraulicMinus  -> tip specErosionHydraulic
  WidgetConfigErosionHydraulicPlus   -> tip specErosionHydraulic
  WidgetConfigErosionThermalMinus    -> tip specErosionThermal
  WidgetConfigErosionThermalPlus     -> tip specErosionThermal
  WidgetConfigErosionRainRateMinus   -> tip specErosionRainRate
  WidgetConfigErosionRainRatePlus    -> tip specErosionRainRate
  WidgetConfigErosionTalusMinus      -> tip specErosionTalus
  WidgetConfigErosionTalusPlus       -> tip specErosionTalus
  WidgetConfigErosionMaxDropMinus    -> tip specErosionMaxDrop
  WidgetConfigErosionMaxDropPlus     -> tip specErosionMaxDrop
  WidgetConfigGlacierSnowTempMinus   -> tip specGlacierSnowTemp
  WidgetConfigGlacierSnowTempPlus    -> tip specGlacierSnowTemp
  WidgetConfigGlacierSnowRangeMinus  -> tip specGlacierSnowRange
  WidgetConfigGlacierSnowRangePlus   -> tip specGlacierSnowRange
  WidgetConfigGlacierMeltTempMinus   -> tip specGlacierMeltTemp
  WidgetConfigGlacierMeltTempPlus    -> tip specGlacierMeltTemp
  WidgetConfigGlacierMeltRateMinus   -> tip specGlacierMeltRate
  WidgetConfigGlacierMeltRatePlus    -> tip specGlacierMeltRate
  WidgetConfigGlacierAccumScaleMinus -> tip specGlacierAccumScale
  WidgetConfigGlacierAccumScalePlus  -> tip specGlacierAccumScale
  WidgetConfigGlacierFlowItersMinus  -> tip specGlacierFlowIters
  WidgetConfigGlacierFlowItersPlus   -> tip specGlacierFlowIters
  WidgetConfigGlacierFlowRateMinus   -> tip specGlacierFlowRate
  WidgetConfigGlacierFlowRatePlus    -> tip specGlacierFlowRate
  WidgetConfigGlacierErosionScaleMinus -> tip specGlacierErosionScale
  WidgetConfigGlacierErosionScalePlus  -> tip specGlacierErosionScale
  WidgetConfigGlacierCarveScaleMinus -> tip specGlacierCarveScale
  WidgetConfigGlacierCarveScalePlus  -> tip specGlacierCarveScale
  WidgetConfigGlacierDepositScaleMinus -> tip specGlacierDepositScale
  WidgetConfigGlacierDepositScalePlus  -> tip specGlacierDepositScale
  WidgetConfigVentDensityMinus       -> tip specVentDensity
  WidgetConfigVentDensityPlus        -> tip specVentDensity
  WidgetConfigVentThresholdMinus     -> tip specVentThreshold
  WidgetConfigVentThresholdPlus      -> tip specVentThreshold
  WidgetConfigHotspotScaleMinus      -> tip specHotspotScale
  WidgetConfigHotspotScalePlus       -> tip specHotspotScale
  WidgetConfigHotspotThresholdMinus  -> tip specHotspotThreshold
  WidgetConfigHotspotThresholdPlus   -> tip specHotspotThreshold
  WidgetConfigMagmaRechargeMinus     -> tip specMagmaRecharge
  WidgetConfigMagmaRechargePlus      -> tip specMagmaRecharge
  WidgetConfigLavaScaleMinus         -> tip specLavaScale
  WidgetConfigLavaScalePlus          -> tip specLavaScale
  WidgetConfigAshScaleMinus          -> tip specAshScale
  WidgetConfigAshScalePlus           -> tip specAshScale
  WidgetConfigVolcanicDepositScaleMinus -> tip specVolcanicDepositScale
  WidgetConfigVolcanicDepositScalePlus  -> tip specVolcanicDepositScale
  WidgetConfigSoilMoistureThresholdMinus -> tip specSoilMoistureThreshold
  WidgetConfigSoilMoistureThresholdPlus  -> tip specSoilMoistureThreshold
  WidgetConfigSoilHardnessThresholdMinus -> tip specSoilHardnessThreshold
  WidgetConfigSoilHardnessThresholdPlus  -> tip specSoilHardnessThreshold
  WidgetConfigSoilFertilityMoistWeightMinus -> tip specSoilFertilityMoistWeight
  WidgetConfigSoilFertilityMoistWeightPlus  -> tip specSoilFertilityMoistWeight
  WidgetConfigSoilFertilityDepthWeightMinus -> tip specSoilFertilityDepthWeight
  WidgetConfigSoilFertilityDepthWeightPlus  -> tip specSoilFertilityDepthWeight
  WidgetConfigSinkBreachDepthMinus   -> tip specSinkBreachDepth
  WidgetConfigSinkBreachDepthPlus    -> tip specSinkBreachDepth
  WidgetConfigStreamPowerMaxErosionMinus -> tip specStreamPowerMaxErosion
  WidgetConfigStreamPowerMaxErosionPlus  -> tip specStreamPowerMaxErosion
  WidgetConfigRiverCarveMaxDepthMinus -> tip specRiverCarveMaxDepth
  WidgetConfigRiverCarveMaxDepthPlus  -> tip specRiverCarveMaxDepth
  WidgetConfigCoastalErodeStrengthMinus -> tip specCoastalErodeStrength
  WidgetConfigCoastalErodeStrengthPlus  -> tip specCoastalErodeStrength
  WidgetConfigHydroHardnessWeightMinus -> tip specHydroHardnessWeight
  WidgetConfigHydroHardnessWeightPlus  -> tip specHydroHardnessWeight
  WidgetConfigMinLakeSizeMinus       -> tip specMinLakeSize
  WidgetConfigMinLakeSizePlus        -> tip specMinLakeSize
  WidgetConfigInlandSeaMinSizeMinus  -> tip specInlandSeaMinSize
  WidgetConfigInlandSeaMinSizePlus   -> tip specInlandSeaMinSize
  WidgetConfigRoughnessScaleMinus    -> tip specRoughnessScale
  WidgetConfigRoughnessScalePlus     -> tip specRoughnessScale
  -- Non-config widgets — explicit Nothing to enable -Wincomplete-patterns
  WidgetGenerate                     -> Nothing
  WidgetLeftToggle                   -> Nothing
  WidgetLeftTabTopo                  -> Nothing
  WidgetLeftTabView                  -> Nothing
  WidgetSeedValue                    -> Nothing
  WidgetSeedRandom                   -> Nothing
  WidgetConfigToggle                 -> Nothing
  WidgetConfigTabTerrain             -> Nothing
  WidgetConfigTabPlanet              -> Nothing
  WidgetConfigTabClimate             -> Nothing
  WidgetConfigTabWeather             -> Nothing
  WidgetConfigTabBiome               -> Nothing
  WidgetConfigTabErosion             -> Nothing
  WidgetConfigPresetSave             -> Nothing
  WidgetConfigPresetLoad             -> Nothing
  WidgetConfigReset                  -> Nothing
  WidgetConfigRevert                 -> Nothing
  WidgetViewElevation                -> Nothing
  WidgetViewBiome                    -> Nothing
  WidgetViewClimate                  -> Nothing
  WidgetViewMoisture                 -> Nothing
  WidgetViewPrecip                   -> Nothing
  WidgetViewPlateId                  -> Nothing
  WidgetViewPlateBoundary            -> Nothing
  WidgetViewPlateHardness            -> Nothing
  WidgetViewPlateCrust               -> Nothing
  WidgetViewPlateAge                 -> Nothing
  WidgetViewPlateHeight              -> Nothing
  WidgetViewPlateVelocity            -> Nothing
  WidgetLogDebug                     -> Nothing
  WidgetLogInfo                      -> Nothing
  WidgetLogWarn                      -> Nothing
  WidgetLogError                     -> Nothing
  WidgetLogHeader                    -> Nothing
  WidgetMenuSave                     -> Nothing
  WidgetMenuLoad                     -> Nothing
  WidgetMenuExit                     -> Nothing
  WidgetPresetSaveOk                 -> Nothing
  WidgetPresetSaveCancel             -> Nothing
  WidgetPresetLoadOk                 -> Nothing
  WidgetPresetLoadCancel             -> Nothing
  WidgetPresetLoadItem               -> Nothing
  WidgetWorldSaveOk                  -> Nothing
  WidgetWorldSaveCancel              -> Nothing
  WidgetWorldLoadOk                  -> Nothing
  WidgetWorldLoadCancel              -> Nothing
  WidgetWorldLoadItem                -> Nothing
  where
    tip :: SliderSpec -> Maybe Text
    tip = Just . ssTooltip

------------------------------------------------------------------------
-- Terrain tab specs
------------------------------------------------------------------------

specGenScale :: SliderSpec
specGenScale = SliderSpec
  "Scale" "Overall terrain height multiplier" 0.2 2.0 False

specGenCoordScale :: SliderSpec
specGenCoordScale = SliderSpec
  "Noise Scale" "Coordinate scaling for noise sampling" 0.5 2.0 False

specGenOffsetX :: SliderSpec
specGenOffsetX = SliderSpec
  "Noise Off X" "Horizontal noise offset for panning the noise field" (-10000) 10000 False

specGenOffsetY :: SliderSpec
specGenOffsetY = SliderSpec
  "Noise Off Y" "Vertical noise offset for panning the noise field" (-10000) 10000 False

specGenFrequency :: SliderSpec
specGenFrequency = SliderSpec
  "Frequency" "Base frequency of terrain noise; lower = broader features" 0.001 0.05 False

specGenOctaves :: SliderSpec
specGenOctaves = SliderSpec
  "Octaves" "Number of fractal noise octaves; more = finer detail" 2 8 True

specGenLacunarity :: SliderSpec
specGenLacunarity = SliderSpec
  "Lacunarity" "Frequency multiplier per octave; higher = more detail contrast" 1.5 3.5 False

specGenGain :: SliderSpec
specGenGain = SliderSpec
  "Gain" "Amplitude multiplier per octave; lower = smoother terrain" 0.3 0.8 False

specGenWarpScale :: SliderSpec
specGenWarpScale = SliderSpec
  "Warp Scale" "Frequency of the domain-warp noise" 0.005 0.08 False

specGenWarpStrength :: SliderSpec
specGenWarpStrength = SliderSpec
  "Warp Strength" "Magnitude of domain warping; higher = more distorted terrain" 2 20 False

specExtentX :: SliderSpec
specExtentX = SliderSpec
  "Extent X" "World width in chunk radii" 0 16 True

specExtentY :: SliderSpec
specExtentY = SliderSpec
  "Extent Y" "World height in chunk radii" 0 16 True

specEdgeNorth :: SliderSpec
specEdgeNorth = SliderSpec
  "Edge North" "Ocean depth bias along the north border" 0 2 False

specEdgeSouth :: SliderSpec
specEdgeSouth = SliderSpec
  "Edge South" "Ocean depth bias along the south border" 0 2 False

specEdgeEast :: SliderSpec
specEdgeEast = SliderSpec
  "Edge East" "Ocean depth bias along the east border" 0 2 False

specEdgeWest :: SliderSpec
specEdgeWest = SliderSpec
  "Edge West" "Ocean depth bias along the west border" 0 2 False

specEdgeFalloff :: SliderSpec
specEdgeFalloff = SliderSpec
  "Edge Falloff" "Falloff width (in tiles) for ocean edge depth bias" 0 512 False

specPlateSize :: SliderSpec
specPlateSize = SliderSpec
  "Plate Size" "Average tectonic plate size in tiles" 16 128 True

specUplift :: SliderSpec
specUplift = SliderSpec
  "Uplift" "Convergent boundary uplift strength" 0.05 0.4 False

specRiftDepth :: SliderSpec
specRiftDepth = SliderSpec
  "Rift Depth" "Divergent boundary rift depth" 0.05 0.6 False

specDetailScale :: SliderSpec
specDetailScale = SliderSpec
  "Detail Scale" "Scale of per-tile detail noise applied after tectonics" 0.5 2.5 False

specPlateSpeed :: SliderSpec
specPlateSpeed = SliderSpec
  "Plate Speed" "Plate velocity multiplier affecting boundary intensity" 0.1 1.5 False

specBoundarySharpness :: SliderSpec
specBoundarySharpness = SliderSpec
  "Boundary Sharp" "Sharpness of tectonic plate boundaries" 0.5 2.5 False

specBoundaryNoiseScale :: SliderSpec
specBoundaryNoiseScale = SliderSpec
  "Boundary Scale" "Noise frequency for boundary roughness" 0.002 0.02 False

specBoundaryNoiseStrength :: SliderSpec
specBoundaryNoiseStrength = SliderSpec
  "Boundary Strength" "Amplitude of boundary noise displacement" 2 24 False

specBoundaryWarpOctaves :: SliderSpec
specBoundaryWarpOctaves = SliderSpec
  "Warp Octaves" "Fractal octaves for boundary domain warping" 1 5 True

specBoundaryWarpLacunarity :: SliderSpec
specBoundaryWarpLacunarity = SliderSpec
  "Warp Lac" "Lacunarity for boundary warp noise" 1.5 3.5 False

specBoundaryWarpGain :: SliderSpec
specBoundaryWarpGain = SliderSpec
  "Warp Gain" "Gain for boundary warp noise" 0.3 0.8 False

specPlateMergeScale :: SliderSpec
specPlateMergeScale = SliderSpec
  "Merge Scale" "Smooth-merge radius between adjacent plates" 0.05 0.25 False

specPlateMergeBias :: SliderSpec
specPlateMergeBias = SliderSpec
  "Merge Bias" "Blend bias towards plate centres vs boundaries" 0.3 0.8 False

specPlateDetailScale :: SliderSpec
specPlateDetailScale = SliderSpec
  "Plate Detail S" "Fine-grained noise scale per plate" 0.005 0.05 False

specPlateDetailStrength :: SliderSpec
specPlateDetailStrength = SliderSpec
  "Plate Detail St" "Strength of per-plate detail noise" 0 1 False

specPlateRidgeStrength :: SliderSpec
specPlateRidgeStrength = SliderSpec
  "Plate Ridge St" "Height contribution of convergent ridges" 0 1 False

specPlateHeightBase :: SliderSpec
specPlateHeightBase = SliderSpec
  "Plate Height B" "Base elevation assigned to each plate" (-0.1) 0.35 False

specPlateHeightVariance :: SliderSpec
specPlateHeightVariance = SliderSpec
  "Plate Height V" "Variance in plate base elevation" 0.2 1.2 False

specPlateHardnessBase :: SliderSpec
specPlateHardnessBase = SliderSpec
  "Plate Hard B" "Base hardness per plate (erosion resistance)" 0.2 0.8 False

specPlateHardnessVariance :: SliderSpec
specPlateHardnessVariance = SliderSpec
  "Plate Hard V" "Variance in plate hardness" 0.1 0.6 False

specTrenchDepth :: SliderSpec
specTrenchDepth = SliderSpec
  "Trench Depth" "Depth of oceanic trenches at convergent boundaries" 0.1 0.5 False

specRidgeHeight :: SliderSpec
specRidgeHeight = SliderSpec
  "Ridge Height" "Height of mid-ocean ridges at divergent boundaries" 0.02 0.2 False

specPlateBiasStrength :: SliderSpec
specPlateBiasStrength = SliderSpec
  "Bias Strength" "Strength of directional plate elevation bias" 0 0.6 False

specPlateBiasCenter :: SliderSpec
specPlateBiasCenter = SliderSpec
  "Bias Center" "Elevation bias at world centre" (-1) 1 False

specPlateBiasEdge :: SliderSpec
specPlateBiasEdge = SliderSpec
  "Bias Edge" "Elevation bias at world edges" (-1) 1 False

specPlateBiasNorth :: SliderSpec
specPlateBiasNorth = SliderSpec
  "Bias North" "Elevation bias towards the north" (-1) 1 False

specPlateBiasSouth :: SliderSpec
specPlateBiasSouth = SliderSpec
  "Bias South" "Elevation bias towards the south" (-1) 1 False

specTfcCliffSlope :: SliderSpec
specTfcCliffSlope = SliderSpec
  "Cliff Slope" "Minimum slope threshold for cliff terrain classification" 0.1 0.8 False

specTfcMountainSlope :: SliderSpec
specTfcMountainSlope = SliderSpec
  "Mountain Slope" "Minimum slope threshold for mountain terrain" 0.05 0.5 False

specTfcMountainRelief :: SliderSpec
specTfcMountainRelief = SliderSpec
  "Mountain Relief" "Minimum local relief for mountain classification" 0.05 0.5 False

specTfcHillSlope :: SliderSpec
specTfcHillSlope = SliderSpec
  "Hill Slope" "Minimum slope threshold for hill terrain" 0.02 0.2 False

specTfcRollingSlope :: SliderSpec
specTfcRollingSlope = SliderSpec
  "Rolling Slope" "Minimum slope threshold for rolling terrain" 0.005 0.1 False

specValleyCurvature :: SliderSpec
specValleyCurvature = SliderSpec
  "Valley Curvature" "Curvature threshold for valley detection" 0.05 0.4 False

specRockElevationThreshold :: SliderSpec
specRockElevationThreshold = SliderSpec
  "Rock Elev Threshold" "Elevation threshold for exposed rock terrain" 0.2 0.9 False

specRockHardnessThreshold :: SliderSpec
specRockHardnessThreshold = SliderSpec
  "Rock Hardness" "Hardness threshold for exposed rock terrain" 0.2 0.9 False

specRockHardnessSecondary :: SliderSpec
specRockHardnessSecondary = SliderSpec
  "Rock Hardness 2nd" "Secondary hardness threshold for rock classification" 0.1 0.8 False

------------------------------------------------------------------------
-- Climate tab specs
------------------------------------------------------------------------

specWaterLevel :: SliderSpec
specWaterLevel = SliderSpec
  "Water Level" "Sea level threshold; tiles below this are ocean" 0 1 False

specEvaporation :: SliderSpec
specEvaporation = SliderSpec
  "Evaporation" "Evaporation rate feeding atmospheric moisture" 0 1 False

specRainShadow :: SliderSpec
specRainShadow = SliderSpec
  "Rain Shadow" "Strength of orographic rain-shadow effect" 0 1 False

specWindDiffuse :: SliderSpec
specWindDiffuse = SliderSpec
  "Wind Diffuse" "Diffusion of wind-carried moisture across tiles" 0 1 False

specEquatorTemp :: SliderSpec
specEquatorTemp = SliderSpec
  "Equator Temp" "Normalised temperature at the equator" 0 1 False

specPoleTemp :: SliderSpec
specPoleTemp = SliderSpec
  "Pole Temp" "Normalised temperature at the poles" 0 1 False

specLapseRate :: SliderSpec
specLapseRate = SliderSpec
  "Lapse Rate" "Temperature decrease per unit of elevation" 0 1 False

specWindIterations :: SliderSpec
specWindIterations = SliderSpec
  "Wind Iter" "Number of wind simulation iterations" 1 8 True

specMoistureIterations :: SliderSpec
specMoistureIterations = SliderSpec
  "Moist Iter" "Number of moisture transport iterations" 2 12 True

specWeatherTick :: SliderSpec
specWeatherTick = SliderSpec
  "Weather Tick" "Simulated time per weather step (seconds)" 0.1 5 False

specWeatherPhase :: SliderSpec
specWeatherPhase = SliderSpec
  "Weather Phase" "Season phase offset (0 = spring equinox)" 0 1 False

specWeatherAmplitude :: SliderSpec
specWeatherAmplitude = SliderSpec
  "Weather Amp" "Seasonal temperature amplitude" 0 0.5 False

specSeasonCycleLength :: SliderSpec
specSeasonCycleLength = SliderSpec
  "Season Len" "Season cycle length in ticks" 30 730 False

specJitterAmplitude :: SliderSpec
specJitterAmplitude = SliderSpec
  "Jitter Amp" "Temperature jitter noise amplitude" 0 0.5 False

specPressureBase :: SliderSpec
specPressureBase = SliderSpec
  "Press Base" "Base atmospheric pressure level" 0.3 1.0 False

specPressureTempScale :: SliderSpec
specPressureTempScale = SliderSpec
  "Press Temp" "Temperature-to-pressure coupling" 0 1 False

specPressureCoriolisScale :: SliderSpec
specPressureCoriolisScale = SliderSpec
  "Coriolis" "Coriolis band pressure strength" 0 0.5 False

specSeasonalBase :: SliderSpec
specSeasonalBase = SliderSpec
  "Seas Base" "Dry-season minimum multiplier" 0 1 False

specSeasonalRange :: SliderSpec
specSeasonalRange = SliderSpec
  "Seas Range" "Wet-season peak range" 0 2 False

specHumidityNoiseScale :: SliderSpec
specHumidityNoiseScale = SliderSpec
  "Humid Noise" "Humidity noise amplitude" 0 0.3 False

specPrecipNoiseScale :: SliderSpec
specPrecipNoiseScale = SliderSpec
  "Precip Noise" "Precipitation noise amplitude" 0 0.5 False

specWeatherITCZWidth :: SliderSpec
specWeatherITCZWidth = SliderSpec
  "W ITCZ Width" "ITCZ precipitation band width (degrees)" 2 20 False

specWeatherITCZPrecipBoost :: SliderSpec
specWeatherITCZPrecipBoost = SliderSpec
  "ITCZ Precip" "ITCZ peak precipitation boost" 0 1 False

specPressureHumidityScale :: SliderSpec
specPressureHumidityScale = SliderSpec
  "Press Humid" "Humidity-to-pressure coupling" 0 0.5 False

specPressureGradientWindScale :: SliderSpec
specPressureGradientWindScale = SliderSpec
  "Grad Wind" "Pressure gradient wind speed scale" 0 1 False

specWindNoiseScale :: SliderSpec
specWindNoiseScale = SliderSpec
  "Wind Noise" "Wind speed noise amplitude" 0 0.3 False

specITCZMigrationScale :: SliderSpec
specITCZMigrationScale = SliderSpec
  "ITCZ Migr" "Seasonal ITCZ migration scale" 0 1.5 False

specCloudRHExponent :: SliderSpec
specCloudRHExponent = SliderSpec
  "Cloud RH Exp" "Cloud formation RH exponent" 0.5 3.0 False

specCloudAlbedoEffect :: SliderSpec
specCloudAlbedoEffect = SliderSpec
  "Cloud Albedo" "Cloud albedo cooling strength" 0 0.3 False

specCloudPrecipBoost :: SliderSpec
specCloudPrecipBoost = SliderSpec
  "Cloud Precip" "Cloud precipitation enhancement" 0 0.5 False

specVegBase :: SliderSpec
specVegBase = SliderSpec
  "Veg Base" "Base vegetation density" 0.02 0.6 False

specVegBoost :: SliderSpec
specVegBoost = SliderSpec
  "Veg Boost" "Biome-specific vegetation density boost" 0.1 1.0 False

specVegTempWeight :: SliderSpec
specVegTempWeight = SliderSpec
  "Veg Temp W" "Temperature weight in vegetation suitability" 0 1 False

specVegPrecipWeight :: SliderSpec
specVegPrecipWeight = SliderSpec
  "Veg Precip W" "Precipitation weight in vegetation suitability" 0 1 False

specBtCoastalBand :: SliderSpec
specBtCoastalBand = SliderSpec
  "Coast Band" "Coastal biome detection band width" 0 0.1 False

specBtSnowElevation :: SliderSpec
specBtSnowElevation = SliderSpec
  "Snow Elev" "Minimum elevation for permanent snow" 0.5 1.0 False

specBtAlpineElevation :: SliderSpec
specBtAlpineElevation = SliderSpec
  "Alpine Elev" "Minimum elevation for alpine biome" 0.4 0.9 False

specBtIceCapTemp :: SliderSpec
specBtIceCapTemp = SliderSpec
  "IceCap Temp" "Maximum temperature for ice cap biome" 0 0.2 False

specBtMontaneLow :: SliderSpec
specBtMontaneLow = SliderSpec
  "Montane Low" "Lower elevation bound for montane biome" 0.3 0.8 False

specBtMontanePrecip :: SliderSpec
specBtMontanePrecip = SliderSpec
  "Montane Prec" "Minimum precipitation for montane biome" 0.1 0.6 False

specBtCliffSlope :: SliderSpec
specBtCliffSlope = SliderSpec
  "Cliff Slope" "Slope threshold for cliff biome" 0.2 0.8 False

specBtValleyMoisture :: SliderSpec
specBtValleyMoisture = SliderSpec
  "Valley Moist" "Minimum moisture for valley biome" 0.3 1.0 False

specBtDepressionMoisture :: SliderSpec
specBtDepressionMoisture = SliderSpec
  "Depress Moist" "Minimum moisture for depression biome" 0.2 1.0 False

specBtPrecipWeight :: SliderSpec
specBtPrecipWeight = SliderSpec
  "Precip Wt" "Precipitation importance in biome classification" 0.5 5.0 False

specVbcTempMin :: SliderSpec
specVbcTempMin = SliderSpec
  "Veg Temp Min" "Minimum temperature for vegetation growth" 0 0.3 False

specVbcTempRange :: SliderSpec
specVbcTempRange = SliderSpec
  "Veg T Range" "Temperature range for vegetation growth" 0.1 1.0 False

specVbcFertilityBoost :: SliderSpec
specVbcFertilityBoost = SliderSpec
  "Fert Boost" "Fertility boost to vegetation density" 0 1 False

specVbcAlbedoBase :: SliderSpec
specVbcAlbedoBase = SliderSpec
  "Albedo Base" "Base surface albedo" 0 0.3 False

specVbcAlbedoBare :: SliderSpec
specVbcAlbedoBare = SliderSpec
  "Albedo Bare" "Bare ground albedo" 0.1 0.5 False

specVbcAlbedoVeg :: SliderSpec
specVbcAlbedoVeg = SliderSpec
  "Albedo Veg" "Vegetation albedo reduction" 0 0.3 False

specVbcOceanAlbedo :: SliderSpec
specVbcOceanAlbedo = SliderSpec
  "Ocean Albedo" "Ocean surface albedo" 0 0.2 False

specVbcIceAlbedo :: SliderSpec
specVbcIceAlbedo = SliderSpec
  "Ice Albedo" "Ice surface albedo" 0.3 1.0 False

specBiomeSmoothing :: SliderSpec
specBiomeSmoothing = SliderSpec
  "Biome Smooth" "Biome classification smoothing passes" 0 5 False

specVolcanicAshBoost :: SliderSpec
specVolcanicAshBoost = SliderSpec
  "Volc Ash" "Volcanic ash fertility boost" 0 0.5 False

specVolcanicLavaPenalty :: SliderSpec
specVolcanicLavaPenalty = SliderSpec
  "Volc Lava" "Volcanic lava vegetation penalty" 0 0.8 False

specBiomeFeedbackBlend :: SliderSpec
specBiomeFeedbackBlend = SliderSpec
  "Biome Blend" "Biome feedback vs bootstrap blend" 0 1 False

specBoundaryMotionTemp :: SliderSpec
specBoundaryMotionTemp = SliderSpec
  "Bound Temp" "Plate-boundary influence on local temperature" 0 2 False

specBoundaryMotionPrecip :: SliderSpec
specBoundaryMotionPrecip = SliderSpec
  "Bound Precip" "Plate-boundary influence on local precipitation" 0 2 False

specPlanetRadius :: SliderSpec
specPlanetRadius = SliderSpec
  "Planet R" "Planet radius in km" 4778 9557 False

specAxialTilt :: SliderSpec
specAxialTilt = SliderSpec
  "Axial Tilt" "Axial tilt in degrees; affects seasonal extremes" 0 45 False

specInsolation :: SliderSpec
specInsolation = SliderSpec
  "Insolation" "Stellar irradiance multiplier (1.0 = Earth-like)" 0.7 1.3 False

specOccWarmScale :: SliderSpec
specOccWarmScale = SliderSpec
  "Warm Scale" "Warm western boundary current boost" 0 0.2 False

specOccColdScale :: SliderSpec
specOccColdScale = SliderSpec
  "Cold Scale" "Cold eastern boundary current reduction" 0 0.2 False

specOccLatPeakDeg :: SliderSpec
specOccLatPeakDeg = SliderSpec
  "Lat Peak" "Latitude of peak ocean current strength (degrees)" 0 60 False

specOccLatWidthDeg :: SliderSpec
specOccLatWidthDeg = SliderSpec
  "Lat Width" "Latitude Gaussian half-width of current (degrees)" 5 45 False

specSliceLatCenter :: SliderSpec
specSliceLatCenter = SliderSpec
  "Lat Center" "Latitude of the world-slice centre in degrees" (-90) 90 False

specSliceLonCenter :: SliderSpec
specSliceLonCenter = SliderSpec
  "Lon Center" "Longitude of the world-slice centre in degrees" (-180) 180 False

specLatitudeExponent :: SliderSpec
specLatitudeExponent = SliderSpec
  "Lat Exponent" "Exponent for cos(latitude) temperature curve" 0.2 1.5 False

specPlateHeightCooling :: SliderSpec
specPlateHeightCooling = SliderSpec
  "Height Cooling" "Continental elevation cooling coefficient" 0 0.2 False

specTempNoiseScale :: SliderSpec
specTempNoiseScale = SliderSpec
  "Temp Noise" "Random temperature noise amplitude" 0 0.3 False

specOceanModeration :: SliderSpec
specOceanModeration = SliderSpec
  "Ocean Moderation" "Ocean thermal moderation strength" 0 1 False

specOceanModerateTemp :: SliderSpec
specOceanModerateTemp = SliderSpec
  "Ocean Mod Temp" "Maritime target temperature" 0 1 False

specAlbedoSensitivity :: SliderSpec
specAlbedoSensitivity = SliderSpec
  "Albedo Sens" "Albedo-to-temperature feedback strength" 0 1 False

specAlbedoReference :: SliderSpec
specAlbedoReference = SliderSpec
  "Albedo Ref" "Albedo neutral reference point" 0 0.5 False

------------------------------------------------------------------------
-- Moisture specs
------------------------------------------------------------------------

specMoistAdvect :: SliderSpec
specMoistAdvect = SliderSpec
  "Advection" "Fraction of moisture transported by wind" 0 1 False

specMoistLocal :: SliderSpec
specMoistLocal = SliderSpec
  "Local Mix" "Fraction of moisture from local evaporation" 0 1 False

specMoistWindEvapScale :: SliderSpec
specMoistWindEvapScale = SliderSpec
  "Wind Evap" "Wind contribution to ocean evaporation" 0 1 False

specMoistEvapNoiseScale :: SliderSpec
specMoistEvapNoiseScale = SliderSpec
  "Evap Noise" "Spatial noise added to evaporation" 0 0.2 False

specMoistLandETCoeff :: SliderSpec
specMoistLandETCoeff = SliderSpec
  "Land ET" "Land evapotranspiration coefficient" 0 1 False

specMoistBareEvapFrac :: SliderSpec
specMoistBareEvapFrac = SliderSpec
  "Bare Evap" "Bare-soil evaporation fraction" 0 1 False

specMoistVegTranspFrac :: SliderSpec
specMoistVegTranspFrac = SliderSpec
  "Veg Transp" "Vegetation transpiration fraction" 0 1 False

specMoistWindETScale :: SliderSpec
specMoistWindETScale = SliderSpec
  "Wind ET" "Wind contribution to evapotranspiration" 0 1 False

specMoistCondensationRate :: SliderSpec
specMoistCondensationRate = SliderSpec
  "Condensation" "Moisture condensation rate" 0 1 False

specMoistRecycleRate :: SliderSpec
specMoistRecycleRate = SliderSpec
  "Recycle" "Moisture recycling rate" 0 1 False

specMoistITCZStrength :: SliderSpec
specMoistITCZStrength = SliderSpec
  "ITCZ Str" "Strength of ITCZ convergence zone" 0 0.5 False

specMoistITCZWidth :: SliderSpec
specMoistITCZWidth = SliderSpec
  "ITCZ Width" "Width of the ITCZ band in degrees" 2 20 False

------------------------------------------------------------------------
-- Precipitation specs
------------------------------------------------------------------------

specOrographicScale :: SliderSpec
specOrographicScale = SliderSpec
  "Orog Scale" "Orographic uplift scaling factor" 0 2 False

specOrographicStep :: SliderSpec
specOrographicStep = SliderSpec
  "Orog Step" "Upwind sampling distance for orographic precip" 0.5 3 False

specCoastalIterations :: SliderSpec
specCoastalIterations = SliderSpec
  "Coast Iters" "Number of coastal diffusion passes" 0 8 True

specCoastalDiffuse :: SliderSpec
specCoastalDiffuse = SliderSpec
  "Coast Diffuse" "Coastal precipitation diffusion rate" 0 1 False

specCoastalMoistureBoost :: SliderSpec
specCoastalMoistureBoost = SliderSpec
  "Coast Boost" "Coastal moisture boost factor" 0 0.5 False

------------------------------------------------------------------------
-- Wind specs
------------------------------------------------------------------------

specWindBeltStrength :: SliderSpec
specWindBeltStrength = SliderSpec
  "Belt Str" "Wind belt strength" 0 1 False

specWindBeltHarmonics :: SliderSpec
specWindBeltHarmonics = SliderSpec
  "Belt Harm" "Number of wind belt harmonics" 1 6 False

specWindBeltBase :: SliderSpec
specWindBeltBase = SliderSpec
  "Belt Base" "Wind belt base speed" 0 1 False

specWindBeltRange :: SliderSpec
specWindBeltRange = SliderSpec
  "Belt Range" "Wind belt speed range" 0 1 False

specWindBeltSpeedScale :: SliderSpec
specWindBeltSpeedScale = SliderSpec
  "Belt Speed" "Wind belt speed scaling" 0 1 False

------------------------------------------------------------------------
-- Boundary model specs
------------------------------------------------------------------------

specBndLandRange :: SliderSpec
specBndLandRange = SliderSpec
  "Bnd Land R" "Land-fraction normalising range for boundary effects" 0.1 1.5 False

specBndTempConvergent :: SliderSpec
specBndTempConvergent = SliderSpec
  "Bnd T Conv" "Convergent boundary temperature bias" (-0.2) 0.1 False

specBndTempDivergent :: SliderSpec
specBndTempDivergent = SliderSpec
  "Bnd T Div" "Divergent boundary temperature bias" (-0.1) 0.2 False

specBndTempTransform :: SliderSpec
specBndTempTransform = SliderSpec
  "Bnd T Trans" "Transform boundary temperature bias" (-0.1) 0.1 False

specBndPrecipConvergent :: SliderSpec
specBndPrecipConvergent = SliderSpec
  "Bnd P Conv" "Convergent boundary precipitation bias" (-0.1) 0.2 False

specBndPrecipDivergent :: SliderSpec
specBndPrecipDivergent = SliderSpec
  "Bnd P Div" "Divergent boundary precipitation bias" (-0.2) 0.1 False

specBndPrecipTransform :: SliderSpec
specBndPrecipTransform = SliderSpec
  "Bnd P Trans" "Transform boundary precipitation bias" (-0.1) 0.1 False

------------------------------------------------------------------------
-- Erosion tab specs
------------------------------------------------------------------------

specErosionHydraulic :: SliderSpec
specErosionHydraulic = SliderSpec
  "Hydraulic Iters" "Number of hydraulic erosion iterations" 1 12 True

specErosionThermal :: SliderSpec
specErosionThermal = SliderSpec
  "Thermal Iters" "Number of thermal erosion iterations" 1 12 True

specErosionRainRate :: SliderSpec
specErosionRainRate = SliderSpec
  "Rain Rate" "Erosion rainfall intensity" 0.05 0.5 False

specErosionTalus :: SliderSpec
specErosionTalus = SliderSpec
  "Thermal Talus" "Angle of repose for thermal erosion" 0.1 1.0 False

specErosionMaxDrop :: SliderSpec
specErosionMaxDrop = SliderSpec
  "Max Drop" "Maximum height drop per erosion step" 0.1 1.0 False

specGlacierSnowTemp :: SliderSpec
specGlacierSnowTemp = SliderSpec
  "Snow Temp" "Temperature threshold for snow accumulation" 0.0 0.5 False

specGlacierSnowRange :: SliderSpec
specGlacierSnowRange = SliderSpec
  "Snow Range" "Temperature fade range for snow" 0.1 0.7 False

specGlacierMeltTemp :: SliderSpec
specGlacierMeltTemp = SliderSpec
  "Melt Temp" "Temperature onset for glacial melting" 0.1 0.8 False

specGlacierMeltRate :: SliderSpec
specGlacierMeltRate = SliderSpec
  "Melt Rate" "Rate of glacial melt per temperature unit" 0.0 1.0 False

specGlacierAccumScale :: SliderSpec
specGlacierAccumScale = SliderSpec
  "Accum Scale" "Snow accumulation scale factor" 0.0 3.0 False

specGlacierFlowIters :: SliderSpec
specGlacierFlowIters = SliderSpec
  "Flow Iters" "Ice-flow diffusion iterations" 0.0 10.0 True

specGlacierFlowRate :: SliderSpec
specGlacierFlowRate = SliderSpec
  "Flow Rate" "Ice-flow diffusion rate" 0.0 1.0 False

specGlacierErosionScale :: SliderSpec
specGlacierErosionScale = SliderSpec
  "Glacial Erosion" "Glacial erosion potential scale" 0.0 1.0 False

specGlacierCarveScale :: SliderSpec
specGlacierCarveScale = SliderSpec
  "Carve Scale" "Elevation carving factor from glaciers" 0.0 0.1 False

specGlacierDepositScale :: SliderSpec
specGlacierDepositScale = SliderSpec
  "Deposit Scale" "Glacial deposition factor" 0.0 1.0 False

specVentDensity :: SliderSpec
specVentDensity = SliderSpec
  "Vent Density" "Volcanic vent density per plate" 0.0 0.2 False

specVentThreshold :: SliderSpec
specVentThreshold = SliderSpec
  "Vent Threshold" "Vent activation convergence threshold" 0.2 0.9 False

specHotspotScale :: SliderSpec
specHotspotScale = SliderSpec
  "Hotspot Scale" "Hotspot noise scale" 0.0 1.0 False

specHotspotThreshold :: SliderSpec
specHotspotThreshold = SliderSpec
  "Hotspot Thresh" "Hotspot vent activation threshold" 0.3 0.95 False

specMagmaRecharge :: SliderSpec
specMagmaRecharge = SliderSpec
  "Magma Recharge" "Magma recharge rate" 0.0 3.0 False

specLavaScale :: SliderSpec
specLavaScale = SliderSpec
  "Lava Scale" "Lava output scale" 0.0 1.0 False

specAshScale :: SliderSpec
specAshScale = SliderSpec
  "Ash Scale" "Ash output scale" 0.0 1.0 False

specVolcanicDepositScale :: SliderSpec
specVolcanicDepositScale = SliderSpec
  "Volcanic Deposit" "Volcanic deposit scale" 0.0 1.0 False

specSoilMoistureThreshold :: SliderSpec
specSoilMoistureThreshold = SliderSpec
  "Moisture Thresh" "Wet soil classification threshold" 0.0 1.0 False

specSoilHardnessThreshold :: SliderSpec
specSoilHardnessThreshold = SliderSpec
  "Hardness Thresh" "Rocky soil classification threshold" 0.0 1.0 False

specSoilFertilityMoistWeight :: SliderSpec
specSoilFertilityMoistWeight = SliderSpec
  "Fert Moist Wt" "Moisture weight in fertility calculation" 0.0 1.0 False

specSoilFertilityDepthWeight :: SliderSpec
specSoilFertilityDepthWeight = SliderSpec
  "Fert Depth Wt" "Soil depth weight in fertility calculation" 0.0 1.0 False

specSinkBreachDepth :: SliderSpec
specSinkBreachDepth = SliderSpec
  "Breach Depth" "Depression-filling breach depth" 0.0 0.1 False

specStreamPowerMaxErosion :: SliderSpec
specStreamPowerMaxErosion = SliderSpec
  "Stream Erosion" "Stream power erosion cap" 0.0 0.2 False

specRiverCarveMaxDepth :: SliderSpec
specRiverCarveMaxDepth = SliderSpec
  "River Carve" "River channel carve depth" 0.0 0.2 False

specCoastalErodeStrength :: SliderSpec
specCoastalErodeStrength = SliderSpec
  "Coastal Erosion" "Coastal erosion strength" 0.0 0.1 False

specHydroHardnessWeight :: SliderSpec
specHydroHardnessWeight = SliderSpec
  "Hardness Weight" "Hardness vs erosion weight" 0.0 1.0 False

specMinLakeSize :: SliderSpec
specMinLakeSize = SliderSpec
  "Min Lake Size" "Minimum tile count for lake" 1.0 50.0 True

specInlandSeaMinSize :: SliderSpec
specInlandSeaMinSize = SliderSpec
  "Inland Sea Size" "Minimum tile count for inland sea" 50.0 500.0 True

specRoughnessScale :: SliderSpec
specRoughnessScale = SliderSpec
  "Roughness Scale" "Roughness derivation scale" 0.0 2.0 False
