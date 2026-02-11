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
    -- * Climate tab specs
  , specWaterLevel
  , specEvaporation
  , specRainShadow
  , specWindDiffuse
  , specEquatorTemp
  , specPoleTemp
  , specLapseRate
  , specLatitudeBias
  , specWindIterations
  , specMoistureIterations
  , specWeatherTick
  , specWeatherPhase
  , specWeatherAmplitude
  , specVegBase
  , specVegBoost
  , specVegTempWeight
  , specVegPrecipWeight
  , specBoundaryMotionTemp
  , specBoundaryMotionPrecip
  , specPlanetRadius
  , specAxialTilt
  , specInsolation
  , specSliceLatCenter
  , specSliceLatExtent
  , specSliceLonCenter
  , specSliceLonExtent
    -- * Erosion tab specs
  , specErosionHydraulic
  , specErosionThermal
  , specErosionRainRate
  , specErosionTalus
  , specErosionMaxDrop
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
  WidgetConfigLatitudeBiasMinus      -> tip specLatitudeBias
  WidgetConfigLatitudeBiasPlus       -> tip specLatitudeBias
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
  WidgetConfigVegBaseMinus           -> tip specVegBase
  WidgetConfigVegBasePlus            -> tip specVegBase
  WidgetConfigVegBoostMinus          -> tip specVegBoost
  WidgetConfigVegBoostPlus           -> tip specVegBoost
  WidgetConfigVegTempWeightMinus     -> tip specVegTempWeight
  WidgetConfigVegTempWeightPlus      -> tip specVegTempWeight
  WidgetConfigVegPrecipWeightMinus   -> tip specVegPrecipWeight
  WidgetConfigVegPrecipWeightPlus    -> tip specVegPrecipWeight
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
  WidgetConfigSliceLatCenterMinus    -> tip specSliceLatCenter
  WidgetConfigSliceLatCenterPlus     -> tip specSliceLatCenter
  WidgetConfigSliceLatExtentMinus    -> tip specSliceLatExtent
  WidgetConfigSliceLatExtentPlus     -> tip specSliceLatExtent
  WidgetConfigSliceLonCenterMinus    -> tip specSliceLonCenter
  WidgetConfigSliceLonCenterPlus     -> tip specSliceLonCenter
  WidgetConfigSliceLonExtentMinus    -> tip specSliceLonExtent
  WidgetConfigSliceLonExtentPlus     -> tip specSliceLonExtent
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
  -- Non-config widgets
  _                                  -> Nothing
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

specLatitudeBias :: SliderSpec
specLatitudeBias = SliderSpec
  "Latitude Bias" "Shift of the temperature gradient north or south" (-1) 1 False

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

specSliceLatCenter :: SliderSpec
specSliceLatCenter = SliderSpec
  "Lat Center" "Latitude of the world-slice centre in degrees" (-90) 90 False

specSliceLatExtent :: SliderSpec
specSliceLatExtent = SliderSpec
  "Lat Extent" "Latitude span of the world slice in degrees" 0.1 180 False

specSliceLonCenter :: SliderSpec
specSliceLonCenter = SliderSpec
  "Lon Center" "Longitude of the world-slice centre in degrees" (-180) 180 False

specSliceLonExtent :: SliderSpec
specSliceLonExtent = SliderSpec
  "Lon Extent" "Longitude span of the world slice in degrees" 0.1 360 False

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
