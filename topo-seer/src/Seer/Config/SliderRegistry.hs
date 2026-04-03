module Seer.Config.SliderRegistry
  ( SliderId(..)
  , SliderPart(..)
  , SliderTab(..)
  , SliderDef(..)
  , allSliderDefs
  , sliderDefaultValueForId
  , sliderDefsForTab
  , sliderRowCountForTab
  , sliderDefForWidget
  , sliderDefForId
  , sliderMinusWidgetId
  , sliderPlusWidgetId
  , sliderWidgetPart
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Seer.Config.SliderId
import UI.WidgetId (WidgetId(..))

data SliderTab
  = SliderTabTerrain
  | SliderTabPlanet
  | SliderTabClimate
  | SliderTabWeather
  | SliderTabBiome
  | SliderTabErosion
  deriving (Eq, Ord, Show, Enum, Bounded)

data SliderPart
  = SliderPartMinus
  | SliderPartPlus
  deriving (Eq, Ord, Show)

data SliderDef = SliderDef
  { sliderId :: !SliderId
  , sliderTab :: !SliderTab
  , sliderRowIndex :: !Int
  } deriving (Eq, Show)

-- | Derive the minus widget ID from a slider definition.
sliderMinusWidgetId :: SliderDef -> WidgetId
sliderMinusWidgetId = WidgetSliderMinus . sliderId

-- | Derive the plus widget ID from a slider definition.
sliderPlusWidgetId :: SliderDef -> WidgetId
sliderPlusWidgetId = WidgetSliderPlus . sliderId

allSliderDefs :: [SliderDef]
allSliderDefs =
  [ def SliderGenScale SliderTabTerrain 0
  , def SliderGenCoordScale SliderTabTerrain 1
  , def SliderGenOffsetX SliderTabTerrain 2
  , def SliderGenOffsetY SliderTabTerrain 3
  , def SliderGenFrequency SliderTabTerrain 4
  , def SliderGenOctaves SliderTabTerrain 5
  , def SliderGenLacunarity SliderTabTerrain 6
  , def SliderGenGain SliderTabTerrain 7
  , def SliderGenWarpScale SliderTabTerrain 8
  , def SliderGenWarpStrength SliderTabTerrain 9
  , def SliderExtentX SliderTabTerrain 10
  , def SliderExtentY SliderTabTerrain 11
  , def SliderEdgeNorth SliderTabTerrain 12
  , def SliderEdgeSouth SliderTabTerrain 13
  , def SliderEdgeEast SliderTabTerrain 14
  , def SliderEdgeWest SliderTabTerrain 15
  , def SliderEdgeFalloff SliderTabTerrain 16
  , def SliderPlateSize SliderTabTerrain 17
  , def SliderUplift SliderTabTerrain 18
  , def SliderRiftDepth SliderTabTerrain 19
  , def SliderDetailScale SliderTabTerrain 20
  , def SliderPlateSpeed SliderTabTerrain 21
  , def SliderBoundarySharpness SliderTabTerrain 22
  , def SliderBoundaryNoiseScale SliderTabTerrain 23
  , def SliderBoundaryNoiseStrength SliderTabTerrain 24
  , def SliderBoundaryWarpOctaves SliderTabTerrain 25
  , def SliderBoundaryWarpLacunarity SliderTabTerrain 26
  , def SliderBoundaryWarpGain SliderTabTerrain 27
  , def SliderPlateMergeScale SliderTabTerrain 28
  , def SliderPlateMergeBias SliderTabTerrain 29
  , def SliderPlateDetailScale SliderTabTerrain 30
  , def SliderPlateDetailStrength SliderTabTerrain 31
  , def SliderPlateRidgeStrength SliderTabTerrain 32
  , def SliderPlateHeightBase SliderTabTerrain 33
  , def SliderPlateHeightVariance SliderTabTerrain 34
  , def SliderPlateHardnessBase SliderTabTerrain 35
  , def SliderPlateHardnessVariance SliderTabTerrain 36
  , def SliderTrenchDepth SliderTabTerrain 37
  , def SliderRidgeHeight SliderTabTerrain 38
  , def SliderPlateBiasStrength SliderTabTerrain 39
  , def SliderPlateBiasCenter SliderTabTerrain 40
  , def SliderPlateBiasEdge SliderTabTerrain 41
  , def SliderPlateBiasNorth SliderTabTerrain 42
  , def SliderPlateBiasSouth SliderTabTerrain 43
  , def SliderTfcCliffSlope SliderTabTerrain 44
  , def SliderTfcMountainSlope SliderTabTerrain 45
  , def SliderTfcMountainRelief SliderTabTerrain 46
  , def SliderTfcHillSlope SliderTabTerrain 47
  , def SliderTfcRollingSlope SliderTabTerrain 48
  , def SliderValleyCurvature SliderTabTerrain 49
  , def SliderTfcElevGradient SliderTabTerrain 50
  , def SliderTfcPlateauMaxRelief2Ring SliderTabTerrain 51
  , def SliderTfcPlateauMaxMicroRelief SliderTabTerrain 52
  , def SliderTfcRollingNearFactor SliderTabTerrain 53
  , def SliderRockElevationThreshold SliderTabTerrain 54
  , def SliderRockHardnessThreshold SliderTabTerrain 55
  , def SliderRockHardnessSecondary SliderTabTerrain 56
  , def SliderPlanetRadius SliderTabPlanet 0
  , def SliderAxialTilt SliderTabPlanet 1
  , def SliderInsolation SliderTabPlanet 2
  , def SliderOccWarmScale SliderTabPlanet 3
  , def SliderOccColdScale SliderTabPlanet 4
  , def SliderOccLatPeakDeg SliderTabPlanet 5
  , def SliderOccLatWidthDeg SliderTabPlanet 6
  , def SliderHexSizeKm SliderTabPlanet 7
  , def SliderWaterLevel SliderTabClimate 0
  , def SliderOrographicLift SliderTabClimate 1
  , def SliderRainShadowLoss SliderTabClimate 2
  , def SliderWindDiffuse SliderTabClimate 3
  , def SliderEquatorTemp SliderTabClimate 4
  , def SliderPoleTemp SliderTabClimate 5
  , def SliderLapseRate SliderTabClimate 6
  , def SliderWindIterations SliderTabClimate 7
  , def SliderMoistureIterations SliderTabClimate 8
  , def SliderBoundaryMotionTemp SliderTabClimate 9
  , def SliderBoundaryMotionPrecip SliderTabClimate 10
  , def SliderSliceLatCenter SliderTabClimate 11
  , def SliderSliceLonCenter SliderTabClimate 12
  , def SliderLatitudeExponent SliderTabClimate 13
  , def SliderPlateHeightCooling SliderTabClimate 14
  , def SliderTempNoiseScale SliderTabClimate 15
  , def SliderOceanModeration SliderTabClimate 16
  , def SliderOceanModerateTemp SliderTabClimate 17
  , def SliderAlbedoSensitivity SliderTabClimate 18
  , def SliderAlbedoReference SliderTabClimate 19
  , def SliderMoistAdvect SliderTabClimate 20
  , def SliderMoistLocal SliderTabClimate 21
  , def SliderMoistWindEvapScale SliderTabClimate 22
  , def SliderMoistEvapNoiseScale SliderTabClimate 23
  , def SliderMoistBareEvapFrac SliderTabClimate 24
  , def SliderMoistVegTranspFrac SliderTabClimate 25
  , def SliderMoistWindETScale SliderTabClimate 26
  , def SliderMoistCondensationRate SliderTabClimate 27
  , def SliderMoistRecycleRate SliderTabClimate 28
  , def SliderMoistITCZStrength SliderTabClimate 29
  , def SliderMoistITCZWidth SliderTabClimate 30
  , def SliderOrographicScale SliderTabClimate 31
  , def SliderOrographicStep SliderTabClimate 32
  , def SliderCoastalIterations SliderTabClimate 33
  , def SliderCoastalDiffuse SliderTabClimate 34
  , def SliderCoastalMoistureBoost SliderTabClimate 35
  , def SliderWindBeltStrength SliderTabClimate 36
  , def SliderWindBeltHarmonics SliderTabClimate 37
  , def SliderWindBeltBase SliderTabClimate 38
  , def SliderWindBeltRange SliderTabClimate 39
  , def SliderWindBeltSpeedScale SliderTabClimate 40
  , def SliderBndLandRange SliderTabClimate 41
  , def SliderPiedmontSmooth SliderTabClimate 42
  , def SliderPiedmontSlopeMin SliderTabClimate 43
  , def SliderPiedmontSlopeMax SliderTabClimate 44
  , def SliderWindCoriolisDeflection SliderTabClimate 45
  , def SliderMoistMinVegFloor SliderTabClimate 46
  , def SliderWeatherTick SliderTabWeather 0
  , def SliderWeatherPhase SliderTabWeather 1
  , def SliderWeatherAmplitude SliderTabWeather 2
  , def SliderSeasonCycleLength SliderTabWeather 3
  , def SliderJitterAmplitude SliderTabWeather 4
  , def SliderPressureBase SliderTabWeather 5
  , def SliderPressureTempScale SliderTabWeather 6
  , def SliderPressureCoriolisScale SliderTabWeather 7
  , def SliderSeasonalBase SliderTabWeather 8
  , def SliderSeasonalRange SliderTabWeather 9
  , def SliderHumidityNoiseScale SliderTabWeather 10
  , def SliderPrecipNoiseScale SliderTabWeather 11
  , def SliderWeatherITCZWidth SliderTabWeather 12
  , def SliderWeatherITCZPrecipBoost SliderTabWeather 13
  , def SliderPressureHumidityScale SliderTabWeather 14
  , def SliderPressureGradientWindScale SliderTabWeather 15
  , def SliderWindNoiseScale SliderTabWeather 16
  , def SliderITCZMigrationScale SliderTabWeather 17
  , def SliderCloudRHExponent SliderTabWeather 18
  , def SliderCloudAlbedoEffect SliderTabWeather 19
  , def SliderCloudPrecipBoost SliderTabWeather 20
  , def SliderVegBase SliderTabBiome 0
  , def SliderVegBoost SliderTabBiome 1
  , def SliderVegTempWeight SliderTabBiome 2
  , def SliderVegPrecipWeight SliderTabBiome 3
  , def SliderBtCoastalBand SliderTabBiome 4
  , def SliderBtSnowMaxTemp SliderTabBiome 5
  , def SliderBtAlpineMaxTemp SliderTabBiome 6
  , def SliderBtIceCapTemp SliderTabBiome 7
  , def SliderBtMontaneMaxTemp SliderTabBiome 8
  , def SliderBtMontanePrecip SliderTabBiome 9
  , def SliderBtCliffSlope SliderTabBiome 10
  , def SliderBtValleyMoisture SliderTabBiome 11
  , def SliderBtDepressionMoisture SliderTabBiome 12
  , def SliderBtPrecipWeight SliderTabBiome 13
  , def SliderVbcTempMin SliderTabBiome 14
  , def SliderVbcTempRange SliderTabBiome 15
  , def SliderVbcFertilityBoost SliderTabBiome 16
  , def SliderVbcAlbedoBase SliderTabBiome 17
  , def SliderVbcAlbedoBare SliderTabBiome 18
  , def SliderVbcAlbedoVeg SliderTabBiome 19
  , def SliderVbcOceanAlbedo SliderTabBiome 20
  , def SliderVbcIceAlbedo SliderTabBiome 21
  , def SliderBiomeSmoothing SliderTabBiome 22
  , def SliderVolcanicAshBoost SliderTabBiome 23
  , def SliderVolcanicLavaPenalty SliderTabBiome 24
  , def SliderBiomeFeedbackBlend SliderTabBiome 25
  , def SliderErosionHydraulic SliderTabErosion 0
  , def SliderErosionThermal SliderTabErosion 1
  , def SliderErosionRainRate SliderTabErosion 2
  , def SliderErosionTalus SliderTabErosion 3
  , def SliderErosionMaxDrop SliderTabErosion 4
  , def SliderErosionHydDeposit SliderTabErosion 5
  , def SliderErosionDepositSlope SliderTabErosion 6
  , def SliderErosionThermDeposit SliderTabErosion 7
  , def SliderErosionCoastZone SliderTabErosion 8
  , def SliderErosionCoastStrength SliderTabErosion 9
  , def SliderErosionCoastIter SliderTabErosion 10
  , def SliderHypsometryEnabled SliderTabErosion 11
  , def SliderHypsometryLowlandExp SliderTabErosion 12
  , def SliderHypsometryHighlandExp SliderTabErosion 13
  , def SliderHypsometryPlateauBreak SliderTabErosion 14
  , def SliderHypsometryOceanExp SliderTabErosion 15
  , def SliderHypsometryCoastalRampWidth SliderTabErosion 16
  , def SliderHypsometryCoastalRampStr SliderTabErosion 17
  , def SliderGlacierSnowTemp SliderTabErosion 18
  , def SliderGlacierSnowRange SliderTabErosion 19
  , def SliderGlacierMeltTemp SliderTabErosion 20
  , def SliderGlacierMeltRate SliderTabErosion 21
  , def SliderGlacierAccumScale SliderTabErosion 22
  , def SliderGlacierFlowIters SliderTabErosion 23
  , def SliderGlacierFlowRate SliderTabErosion 24
  , def SliderGlacierErosionScale SliderTabErosion 25
  , def SliderGlacierCarveScale SliderTabErosion 26
  , def SliderGlacierDepositScale SliderTabErosion 27
  , def SliderVentDensity SliderTabErosion 28
  , def SliderVentThreshold SliderTabErosion 29
  , def SliderHotspotScale SliderTabErosion 30
  , def SliderHotspotThreshold SliderTabErosion 31
  , def SliderMagmaRecharge SliderTabErosion 32
  , def SliderLavaScale SliderTabErosion 33
  , def SliderAshScale SliderTabErosion 34
  , def SliderVolcanicDepositScale SliderTabErosion 35
  , def SliderSoilMoistureThreshold SliderTabErosion 36
  , def SliderSoilHardnessThreshold SliderTabErosion 37
  , def SliderSoilFertilityMoistWeight SliderTabErosion 38
  , def SliderSoilFertilityDepthWeight SliderTabErosion 39
  , def SliderSinkBreachDepth SliderTabErosion 40
  , def SliderStreamPowerMaxErosion SliderTabErosion 41
  , def SliderRiverCarveMaxDepth SliderTabErosion 42
  , def SliderCoastalErodeStrength SliderTabErosion 43
  , def SliderHydroHardnessWeight SliderTabErosion 44
  , def SliderMinLakeSize SliderTabErosion 45
  , def SliderInlandSeaMinSize SliderTabErosion 46
  , def SliderRoughnessScale SliderTabErosion 47
  ]

sliderDefsForTab :: SliderTab -> [SliderDef]
sliderDefsForTab tab = filter ((== tab) . sliderTab) allSliderDefs

-- | Number of layout rows consumed by slider definitions in a tab.
sliderRowCountForTab :: SliderTab -> Int
sliderRowCountForTab = length . sliderDefsForTab

sliderDefaultValueForId :: SliderId -> Float
sliderDefaultValueForId sliderIdValue = case sliderIdValue of
  SliderGenScale -> 0.4444
  SliderGenCoordScale -> 0.3333
  SliderGenOffsetX -> 0.5
  SliderGenOffsetY -> 0.5
  SliderGenFrequency -> 0.1837
  SliderGenOctaves -> 0.5
  SliderGenLacunarity -> 0.25
  SliderGenGain -> 0.4
  SliderGenWarpScale -> 0.3333
  SliderGenWarpStrength -> 0.5556
  SliderExtentX -> 0.125
  SliderExtentY -> 0.125
  SliderEdgeNorth -> 0.0
  SliderEdgeSouth -> 0.0
  SliderEdgeEast -> 0.0
  SliderEdgeWest -> 0.0
  SliderEdgeFalloff -> 0.0
  SliderPlateSize -> 0.45
  SliderUplift -> 0.3
  SliderRiftDepth -> 0.35
  SliderDetailScale -> 0.5
  SliderPlateSpeed -> 0.38
  SliderBoundarySharpness -> 0.35
  SliderBoundaryNoiseScale -> 0.33
  SliderBoundaryNoiseStrength -> 0.45
  SliderBoundaryWarpOctaves -> 0.5
  SliderBoundaryWarpLacunarity -> 0.25
  SliderBoundaryWarpGain -> 0.4
  SliderPlateMergeScale -> 0.3
  SliderPlateMergeBias -> 0.44
  SliderPlateDetailScale -> 0.33
  SliderPlateDetailStrength -> 0.35
  SliderPlateRidgeStrength -> 0.25
  SliderPlateHeightBase -> 0.62
  SliderPlateHeightVariance -> 0.65
  SliderPlateHardnessBase -> 0.42
  SliderPlateHardnessVariance -> 0.4
  SliderTrenchDepth -> 0.38
  SliderRidgeHeight -> 0.33
  SliderPlateBiasStrength -> 0.42
  SliderPlateBiasCenter -> 0.5
  SliderPlateBiasEdge -> 0.5
  SliderPlateBiasNorth -> 0.5
  SliderPlateBiasSouth -> 0.5
  SliderTfcCliffSlope -> 0.2222
  SliderTfcMountainSlope -> 0.2222
  SliderTfcMountainRelief -> 0.2143
  SliderTfcHillSlope -> 0.0316
  SliderTfcRollingSlope -> 0.0526
  SliderValleyCurvature -> 0.2857
  SliderTfcElevGradient -> 0.2495
  SliderTfcPlateauMaxRelief2Ring -> 0.2632
  SliderTfcPlateauMaxMicroRelief -> 0.5
  SliderTfcRollingNearFactor -> 0.7
  SliderRockElevationThreshold -> 0.5714
  SliderRockHardnessThreshold -> 0.5714
  SliderRockHardnessSecondary -> 0.5
  SliderPlanetRadius -> 0.3333
  SliderAxialTilt -> 0.5209
  SliderInsolation -> 0.5
  SliderOccWarmScale -> 0.3
  SliderOccColdScale -> 0.2
  SliderOccLatPeakDeg -> 0.5833
  SliderOccLatWidthDeg -> 0.5
  SliderWaterLevel -> 0.5
  SliderOrographicLift -> 0.35
  SliderRainShadowLoss -> 0.08
  SliderWindDiffuse -> 0.5
  SliderEquatorTemp -> 0.78
  SliderPoleTemp -> 0.0
  SliderLapseRate -> 0.65
  SliderWindIterations -> 0.5
  SliderMoistureIterations -> 0.486
  SliderBoundaryMotionTemp -> 0.5
  SliderBoundaryMotionPrecip -> 0.5
  SliderSliceLatCenter -> 0.5
  SliderSliceLonCenter -> 0.5
  SliderLatitudeExponent -> 0.615
  SliderPlateHeightCooling -> 0.25
  SliderTempNoiseScale -> 0.33
  SliderOceanModeration -> 0.3
  SliderOceanModerateTemp -> 0.5
  SliderAlbedoSensitivity -> 0.2
  SliderAlbedoReference -> 0.6
  SliderMoistAdvect -> 0.85
  SliderMoistLocal -> 0.15
  SliderMoistWindEvapScale -> 0.3
  SliderMoistEvapNoiseScale -> 0.25
  SliderMoistBareEvapFrac -> 0.15
  SliderMoistVegTranspFrac -> 0.85
  SliderMoistWindETScale -> 0.2
  SliderMoistCondensationRate -> 0.2
  SliderMoistRecycleRate -> 0.35
  SliderMoistITCZStrength -> 0.3
  SliderMoistITCZWidth -> 0.333
  SliderOrographicScale -> 0.3
  SliderOrographicStep -> 0.2
  SliderCoastalIterations -> 0.5
  SliderCoastalDiffuse -> 0.5
  SliderCoastalMoistureBoost -> 0.4
  SliderWindBeltStrength -> 0.6
  SliderWindBeltHarmonics -> 0.4
  SliderWindBeltBase -> 0.4
  SliderWindBeltRange -> 0.6
  SliderWindBeltSpeedScale -> 0.6
  SliderBndLandRange -> 0.357
  SliderPiedmontSmooth -> 0.4167
  SliderPiedmontSlopeMin -> 0.2857
  SliderPiedmontSlopeMax -> 0.35
  SliderWindCoriolisDeflection -> 0.287
  SliderMoistMinVegFloor -> 0.15
  SliderWeatherTick -> 0.2
  SliderWeatherPhase -> 0.0
  SliderWeatherAmplitude -> 0.3
  SliderSeasonCycleLength -> 0.4786
  SliderJitterAmplitude -> 0.36
  SliderPressureBase -> 0.5714
  SliderPressureTempScale -> 0.4
  SliderPressureCoriolisScale -> 0.2
  SliderSeasonalBase -> 0.4
  SliderSeasonalRange -> 0.6
  SliderHumidityNoiseScale -> 0.3333
  SliderPrecipNoiseScale -> 0.3
  SliderWeatherITCZWidth -> 0.4444
  SliderWeatherITCZPrecipBoost -> 0.3
  SliderPressureHumidityScale -> 0.2
  SliderPressureGradientWindScale -> 0.3
  SliderWindNoiseScale -> 0.3333
  SliderITCZMigrationScale -> 0.4667
  SliderCloudRHExponent -> 0.4
  SliderCloudAlbedoEffect -> 0.2667
  SliderCloudPrecipBoost -> 0.24
  SliderVegBase -> 0.2
  SliderVegBoost -> 0.6
  SliderVegTempWeight -> 0.6
  SliderVegPrecipWeight -> 0.4
  SliderBtCoastalBand -> 0.3
  SliderBtSnowMaxTemp -> 0.4
  SliderBtAlpineMaxTemp -> 0.42
  SliderBtIceCapTemp -> 0.25
  SliderBtMontaneMaxTemp -> 0.58
  SliderBtMontanePrecip -> 0.5
  SliderBtCliffSlope -> 0.3333
  SliderBtValleyMoisture -> 0.5714
  SliderBtDepressionMoisture -> 0.5
  SliderBtPrecipWeight -> 0.3333
  SliderVbcTempMin -> 0.2667
  SliderVbcTempRange -> 0.4444
  SliderVbcFertilityBoost -> 0.5
  SliderVbcAlbedoBase -> 0.5
  SliderVbcAlbedoBare -> 0.375
  SliderVbcAlbedoVeg -> 0.3333
  SliderVbcOceanAlbedo -> 0.3
  SliderVbcIceAlbedo -> 0.7143
  SliderBiomeSmoothing -> 0.2
  SliderVolcanicAshBoost -> 0.4
  SliderVolcanicLavaPenalty -> 0.4375
  SliderBiomeFeedbackBlend -> 0.85
  SliderErosionHydraulic -> 0.5
  SliderErosionThermal -> 0.4
  SliderErosionRainRate -> 0.2
  SliderErosionTalus -> 0.5
  SliderErosionMaxDrop -> 0.5
  SliderErosionHydDeposit -> 0.375
  SliderErosionDepositSlope -> 0.357
  SliderErosionThermDeposit -> 0.5
  SliderErosionCoastZone -> 0.286
  SliderErosionCoastStrength -> 0.375
  SliderErosionCoastIter -> 0.429
  SliderHypsometryEnabled -> 1.0
  SliderHypsometryLowlandExp -> 0.2
  SliderHypsometryHighlandExp -> 0.333
  SliderHypsometryPlateauBreak -> 0.348
  SliderHypsometryOceanExp -> 0.5
  SliderHypsometryCoastalRampWidth -> 0.467
  SliderHypsometryCoastalRampStr -> 0.6
  SliderGlacierSnowTemp -> 0.5
  SliderGlacierSnowRange -> 0.417
  SliderGlacierMeltTemp -> 0.429
  SliderGlacierMeltRate -> 0.2
  SliderGlacierAccumScale -> 0.333
  SliderGlacierFlowIters -> 0.3
  SliderGlacierFlowRate -> 0.2
  SliderGlacierErosionScale -> 0.25
  SliderGlacierCarveScale -> 0.1
  SliderGlacierDepositScale -> 0.2
  SliderVentDensity -> 0.25
  SliderVentThreshold -> 0.5
  SliderHotspotScale -> 0.5
  SliderHotspotThreshold -> 0.615
  SliderMagmaRecharge -> 0.333
  SliderLavaScale -> 0.6
  SliderAshScale -> 0.4
  SliderVolcanicDepositScale -> 0.8
  SliderSoilMoistureThreshold -> 0.7
  SliderSoilHardnessThreshold -> 0.45
  SliderSoilFertilityMoistWeight -> 0.6
  SliderSoilFertilityDepthWeight -> 0.4
  SliderSinkBreachDepth -> 0.2
  SliderStreamPowerMaxErosion -> 0.25
  SliderRiverCarveMaxDepth -> 0.25
  SliderCoastalErodeStrength -> 0.2
  SliderHydroHardnessWeight -> 0.7
  SliderMinLakeSize -> 0.061
  SliderInlandSeaMinSize -> 0.333
  SliderRoughnessScale -> 0.375
  SliderHexSizeKm -> 1 / 3

sliderDefMap :: IntMap SliderDef
sliderDefMap = IntMap.fromList [(fromEnum (sliderId sd), sd) | sd <- allSliderDefs]

sliderDefForId :: SliderId -> Maybe SliderDef
sliderDefForId sid = IntMap.lookup (fromEnum sid) sliderDefMap

sliderDefForWidget :: WidgetId -> Maybe SliderDef
sliderDefForWidget (WidgetSliderMinus sid) = sliderDefForId sid
sliderDefForWidget (WidgetSliderPlus  sid) = sliderDefForId sid
sliderDefForWidget _                       = Nothing

sliderWidgetPart :: WidgetId -> Maybe (SliderDef, SliderPart)
sliderWidgetPart (WidgetSliderMinus sid) = fmap (\d -> (d, SliderPartMinus)) (sliderDefForId sid)
sliderWidgetPart (WidgetSliderPlus  sid) = fmap (\d -> (d, SliderPartPlus))  (sliderDefForId sid)
sliderWidgetPart _                       = Nothing

def :: SliderId -> SliderTab -> Int -> SliderDef
def sid tab rowIndex =
  SliderDef
    { sliderId = sid
    , sliderTab = tab
    , sliderRowIndex = rowIndex
    }