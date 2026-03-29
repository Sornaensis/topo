{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Internal slider presentation metadata.
module Seer.Config.SliderSpec.Data
  ( SliderSpec(ssName, ssTooltip)
  , sliderSpecEntries
  , fallbackSliderSpec
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Config.SliderRegistry (SliderId(..))

-- | Specification for a single config slider.
data SliderSpec = SliderSpecData
  { ssName    :: !Text
  , ssTooltip :: !Text
  } deriving (Eq, Show)

-- | Preserve the large ordered spec dataset while ignoring duplicated
-- domain/format metadata that now lives in 'Seer.Config.SliderConversion'.
pattern SliderSpec :: Text -> Text -> Float -> Float -> Bool -> SliderSpec
pattern SliderSpec name tooltip lo hi isInt <- (matchSliderSpec -> (name, tooltip, lo, hi, isInt))
  where
    SliderSpec name tooltip _ _ _ = SliderSpecData name tooltip

{-# COMPLETE SliderSpec #-}

matchSliderSpec :: SliderSpec -> (Text, Text, Float, Float, Bool)
matchSliderSpec (SliderSpecData name tooltip) = (name, tooltip, 0, 1, False)

-- | Slider presentation metadata in 'SliderId' enum order.
sliderSpecEntries :: [(SliderId, SliderSpec)]
sliderSpecEntries = zip [minBound .. maxBound] sliderSpecs

fallbackSliderSpec :: SliderId -> SliderSpec
fallbackSliderSpec sliderIdValue =
  SliderSpecData
    { ssName = Text.pack (show sliderIdValue)
    , ssTooltip = ""
    }

sliderSpecs :: [SliderSpec]
sliderSpecs =
  [ specGenScale
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
  , specTfcElevGradient
  , specTfcPlateauMaxRelief2Ring
  , specTfcPlateauMaxMicroRelief
  , specTfcRollingNearFactor
  , specRockElevationThreshold
  , specRockHardnessThreshold
  , specRockHardnessSecondary
  , specPlanetRadius
  , specAxialTilt
  , specInsolation
  , specOccWarmScale
  , specOccColdScale
  , specOccLatPeakDeg
  , specOccLatWidthDeg
  , specWaterLevel
  , specOrographicLift
  , specRainShadowLoss
  , specWindDiffuse
  , specEquatorTemp
  , specPoleTemp
  , specLapseRate
  , specWindIterations
  , specMoistureIterations
  , specBoundaryMotionTemp
  , specBoundaryMotionPrecip
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
  , specMoistBareEvapFrac
  , specMoistVegTranspFrac
  , specMoistWindETScale
  , specMoistCondensationRate
  , specMoistRecycleRate
  , specMoistITCZStrength
  , specMoistITCZWidth
  , specOrographicScale
  , specOrographicStep
  , specCoastalIterations
  , specCoastalDiffuse
  , specCoastalMoistureBoost
  , specWindBeltStrength
  , specWindBeltHarmonics
  , specWindBeltBase
  , specWindBeltRange
  , specWindBeltSpeedScale
  , specBndLandRange
  , specPiedmontSmooth
  , specPiedmontSlopeMin
  , specPiedmontSlopeMax
  , specWindCoriolisDeflection
  , specMoistMinVegFloor
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
  , specBtSnowMaxTemp
  , specBtAlpineMaxTemp
  , specBtIceCapTemp
  , specBtMontaneMaxTemp
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
  , specErosionHydraulic
  , specErosionThermal
  , specErosionRainRate
  , specErosionTalus
  , specErosionMaxDrop
  , specErosionHydDeposit
  , specErosionDepositSlope
  , specErosionThermDeposit
  , specErosionCoastZone
  , specErosionCoastStrength
  , specErosionCoastIter
  , specHypsometryEnabled
  , specHypsometryLowlandExp
  , specHypsometryHighlandExp
  , specHypsometryPlateauBreak
  , specHypsometryOceanExp
  , specHypsometryCoastalRampWidth
  , specHypsometryCoastalRampStr
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
  , specHexSizeKm
  ]

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
  "Plate Detail St" "Strength of per-plate detail noise" 0.05 0.5 False

specPlateRidgeStrength :: SliderSpec
specPlateRidgeStrength = SliderSpec
  "Plate Ridge St" "Height contribution of convergent ridges" 0.05 0.4 False

specPlateHeightBase :: SliderSpec
specPlateHeightBase = SliderSpec
  "Plate Height B" "Base elevation assigned to each plate" 0.0 0.4 False

specPlateHeightVariance :: SliderSpec
specPlateHeightVariance = SliderSpec
  "Plate Height V" "Variance in plate base elevation" 0.1 0.8 False

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
  "Cliff Slope" "Physical slope threshold for cliff classification" 0.05 0.50 False

specTfcMountainSlope :: SliderSpec
specTfcMountainSlope = SliderSpec
  "Mountain Slope" "Physical top-3 slope threshold for mountain terrain" 0.02 0.20 False

specTfcMountainRelief :: SliderSpec
specTfcMountainRelief = SliderSpec
  "Mountain Relief" "3-ring relief threshold for mountain classification" 0.02 0.30 False

specTfcHillSlope :: SliderSpec
specTfcHillSlope = SliderSpec
  "Hill Slope" "Physical top-3 slope threshold for hill terrain" 0.005 0.10 False

specTfcRollingSlope :: SliderSpec
specTfcRollingSlope = SliderSpec
  "Rolling Slope" "Physical avg slope threshold for rolling terrain" 0.002 0.04 False

specValleyCurvature :: SliderSpec
specValleyCurvature = SliderSpec
  "Valley Curvature" "Curvature threshold for valley detection" 0.05 0.4 False

specTfcElevGradient :: SliderSpec
specTfcElevGradient = SliderSpec
  "Elev Gradient" "Elevation gradient multiplier converting raw slope to physical slope" 0.1 2.0 False

specTfcPlateauMaxRelief2Ring :: SliderSpec
specTfcPlateauMaxRelief2Ring = SliderSpec
  "Plateau Relief" "Max 2-ring relief for plateau classification" 0.005 0.10 False

specTfcPlateauMaxMicroRelief :: SliderSpec
specTfcPlateauMaxMicroRelief = SliderSpec
  "Plateau Micro-Relief" "Max micro-relief index for plateau classification; inhibits over-classification of rough elevated flats" 0.0 1.0 False

specTfcRollingNearFactor :: SliderSpec
specTfcRollingNearFactor = SliderSpec
  "Rolling Near Factor" "Near-threshold multiplier for micro-relief-assisted rolling promotion" 0.5 1.0 False

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

specOrographicLift :: SliderSpec
specOrographicLift = SliderSpec
  "Orographic Lift" "Windward precipitation enhancement from terrain rise" 0 1 False

specRainShadowLoss :: SliderSpec
specRainShadowLoss = SliderSpec
  "Rain Shadow Loss" "Per-iteration moisture sink from elevation barriers" 0 1 False

specWindDiffuse :: SliderSpec
specWindDiffuse = SliderSpec
  "Wind Diffuse" "Diffusion of wind-carried moisture across tiles" 0 1 False

specEquatorTemp :: SliderSpec
specEquatorTemp = SliderSpec
  "Equator °C" "Temperature at the equator (-50 to 50 °C)" (-50) 50 False

specPoleTemp :: SliderSpec
specPoleTemp = SliderSpec
  "Pole °C" "Temperature at the poles (-50 to 50 °C)" (-50) 50 False

specLapseRate :: SliderSpec
specLapseRate = SliderSpec
  "Lapse Rate" "Temperature decrease with altitude (~6-8 °C/km typical)" 0 1 False

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
  "Veg Scale" "Global vegetation density scale multiplier" 0.1 3.0 False

specVegBoost :: SliderSpec
specVegBoost = SliderSpec
  "Clim Slope" "Global climate-slope scale for vegetation density" 0.1 3.0 False

specVegTempWeight :: SliderSpec
specVegTempWeight = SliderSpec
  "Veg Temp W" "Temperature weight in vegetation suitability" 0 1 False

specVegPrecipWeight :: SliderSpec
specVegPrecipWeight = SliderSpec
  "Veg Precip W" "Precipitation weight in vegetation suitability" 0 1 False

specBtCoastalBand :: SliderSpec
specBtCoastalBand = SliderSpec
  "Coast Band" "Coastal biome detection band width" 0 0.1 False

specBtSnowMaxTemp :: SliderSpec
specBtSnowMaxTemp = SliderSpec
  "Snow Max °C" "Max temperature for snow biome (-50 to 50 °C)" (-50) 50 False

specBtAlpineMaxTemp :: SliderSpec
specBtAlpineMaxTemp = SliderSpec
  "Alpine Max °C" "Max temperature for alpine biome (-50 to 50 °C)" (-50) 50 False

specBtIceCapTemp :: SliderSpec
specBtIceCapTemp = SliderSpec
  "IceCap °C" "Maximum temperature for ice cap biome (-50 to 50 °C)" (-50) 50 False

specBtMontaneMaxTemp :: SliderSpec
specBtMontaneMaxTemp = SliderSpec
  "Montane °C" "Max temperature for montane biome (-50 to 50 °C)" (-50) 50 False

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
  "Veg Min °C" "Minimum temperature for vegetation growth (-50 to 50 °C)" (-50) 50 False

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

specWindCoriolisDeflection :: SliderSpec
specWindCoriolisDeflection = SliderSpec
  "Coriolis Defl" "Max Coriolis deflection angle (radians)" 0 1.57 False

------------------------------------------------------------------------
-- Boundary model specs
------------------------------------------------------------------------

specBndLandRange :: SliderSpec
specBndLandRange = SliderSpec
  "Bnd Land R" "Land-fraction normalising range for boundary effects" 0.1 1.5 False

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
  "Thermal Talus" "Angle of repose for thermal erosion" 0.01 0.15 False

specErosionMaxDrop :: SliderSpec
specErosionMaxDrop = SliderSpec
  "Max Drop" "Maximum height drop per erosion step" 0.1 1.0 False

specErosionHydDeposit :: SliderSpec
specErosionHydDeposit = SliderSpec
  "Hyd Deposit" "Fraction of hydraulic erosion deposited at lowest neighbor" 0.0 0.8 False

specErosionDepositSlope :: SliderSpec
specErosionDepositSlope = SliderSpec
  "Deposit Slope" "Max slope for deposition (steeper = no deposit)" 0.01 0.15 False

specErosionThermDeposit :: SliderSpec
specErosionThermDeposit = SliderSpec
  "Therm Deposit" "Fraction of thermal erosion deposited as talus" 0.0 1.0 False

specErosionCoastZone :: SliderSpec
specErosionCoastZone = SliderSpec
  "Coast Zone" "Elevation band above sea level for coastal smoothing" 0.005 0.12 False

specErosionCoastStrength :: SliderSpec
specErosionCoastStrength = SliderSpec
  "Coast Smooth" "Blend strength for coastal smoothing" 0.0 0.8 False

specErosionCoastIter :: SliderSpec
specErosionCoastIter = SliderSpec
  "Coast Iters" "Number of iterative coastal erosion/deposition passes" 1.0 8.0 True

specHypsometryEnabled :: SliderSpec
specHypsometryEnabled = SliderSpec
  "Hypsometry" "Enable/disable hypsometric elevation reshaping" 0.0 1.0 True

specHypsometryLowlandExp :: SliderSpec
specHypsometryLowlandExp = SliderSpec
  "Lowland Exp" "Power exponent for lowland compression (higher = flatter)" 0.5 5.0 False

specHypsometryHighlandExp :: SliderSpec
specHypsometryHighlandExp = SliderSpec
  "Highland Exp" "Power exponent for highland compression (higher = flatter)" 0.3 3.0 False

specHypsometryPlateauBreak :: SliderSpec
specHypsometryPlateauBreak = SliderSpec
  "Plateau Break" "Normalized elevation for lowland/highland transition" 0.52 0.75 False

specHypsometryOceanExp :: SliderSpec
specHypsometryOceanExp = SliderSpec
  "Ocean Exp" "Power exponent for ocean depth shaping" 0.2 1.0 False

specHypsometryCoastalRampWidth :: SliderSpec
specHypsometryCoastalRampWidth = SliderSpec
  "Coast Width" "Elevation band for coastal ramp flattening" 0.005 0.08 False

specHypsometryCoastalRampStr :: SliderSpec
specHypsometryCoastalRampStr = SliderSpec
  "Coast Ramp" "Strength of coastal ramp flattening" 0.0 1.0 False

specGlacierSnowTemp :: SliderSpec
specGlacierSnowTemp = SliderSpec
  "Snow °C" "Temperature threshold for snow accumulation (-50 to 50 °C)" (-50) 50 False

specGlacierSnowRange :: SliderSpec
specGlacierSnowRange = SliderSpec
  "Snow Range" "Temperature fade range for snow" 0.1 0.7 False

specGlacierMeltTemp :: SliderSpec
specGlacierMeltTemp = SliderSpec
  "Melt °C" "Temperature onset for glacial melting (-50 to 50 °C)" (-50) 50 False

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

specPiedmontSmooth :: SliderSpec
specPiedmontSmooth = SliderSpec
  "Piedmont Smooth" "Piedmont smoothing strength" 0.0 0.6 False

specPiedmontSlopeMin :: SliderSpec
specPiedmontSlopeMin = SliderSpec
  "Piedmont Slope Min" "Piedmont minimum slope threshold" 0.01 0.08 False

specPiedmontSlopeMax :: SliderSpec
specPiedmontSlopeMax = SliderSpec
  "Piedmont Slope Max" "Piedmont maximum slope threshold" 0.05 0.25 False

specMoistMinVegFloor :: SliderSpec
specMoistMinVegFloor = SliderSpec
  "Veg Floor" "Minimum vegetation floor for moisture recycling" 0 1 False

specMinLakeSize :: SliderSpec
specMinLakeSize = SliderSpec
  "Min Lake Size" "Minimum tile count for lake" 1.0 50.0 True

specInlandSeaMinSize :: SliderSpec
specInlandSeaMinSize = SliderSpec
  "Inland Sea Size" "Minimum tile count for inland sea" 50.0 500.0 True

specRoughnessScale :: SliderSpec
specRoughnessScale = SliderSpec
  "Roughness Scale" "Roughness derivation scale" 0.0 2.0 False

specHexSizeKm :: SliderSpec
specHexSizeKm = SliderSpec
  "Hex Size" "Flat-to-flat hex size in km; affects geographic scale" 2.0 20.0 False