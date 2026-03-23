{-# LANGUAGE OverloadedStrings #-}

module Seer.Config.SliderConfig.Data
  ( SliderConfigUpdate
  , lookupSliderConfigUpdate
  , snapshotSliderValueForId
  , updateClimateBoundary
  , updateTerrainGen
  , updateWorldSlice
  ) where

import Actor.UI (UiState(..))
import Seer.Config.SliderConversion
  ( sliderFromDomainBool
  , sliderFromDomainFloat
  , sliderFromDomainInt
  , sliderToDomainBool
  , sliderToDomainFloat
  , sliderToDomainInt
  )
import Seer.Config.SliderRegistry (SliderId(..))
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth(..))
import Topo.Biome (BiomeThresholds(..), BiomeVegetationConfig(..))
import Topo.BiomeConfig (BiomeConfig(..))
import Topo.Climate
  ( BoundaryConfig(..)
  , ClimateConfig(..)
  , MoistureConfig(..)
  , PrecipitationConfig(..)
  , TemperatureConfig(..)
  , WindConfig(..)
  )
import Topo.Erosion (ErosionConfig(..))
import Topo.Glacier (GlacierConfig(..))
import Topo.Hydrology (HydroConfig(..))
import Topo.Hypsometry (HypsometryConfig(..))
import Topo.OceanCurrent (OceanCurrentConfig(..))
import Topo.Parameters (ParameterConfig(..), TerrainFormConfig(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..))
import Topo.Soil (SoilConfig(..))
import Topo.Tectonics (TectonicsConfig(..))
import Topo.Types (WorldExtent, worldExtentRadiusX, worldExtentRadiusY)
import Topo.Vegetation (BiomeFeedbackConfig(..), VegetationBootstrapConfig(..))
import Topo.Volcanism (VolcanismConfig(..))
import Topo.WaterBody (WaterBodyConfig(..))
import Topo.Weather (WeatherConfig(..))
import Topo.WorldGen (TerrainConfig(..), WorldGenConfig(..))

type SliderConfigUpdate = UiState -> WorldGenConfig -> WorldGenConfig

data SnapshotContext = SnapshotContext
  { scGen :: !GenConfig
  , scEdge :: !OceanEdgeDepth
  , scExtent :: !WorldExtent
  , scTectonics :: !TectonicsConfig
  , scParameters :: !ParameterConfig
  , scForm :: !TerrainFormConfig
  , scHydrology :: !HydroConfig
  , scErosion :: !ErosionConfig
  , scHypsometry :: !HypsometryConfig
  , scGlacier :: !GlacierConfig
  , scVolcanism :: !VolcanismConfig
  , scSoil :: !SoilConfig
  , scWaterBody :: !WaterBodyConfig
  , scVegetationBootstrap :: !VegetationBootstrapConfig
  , scTemperature :: !TemperatureConfig
  , scWind :: !WindConfig
  , scMoisture :: !MoistureConfig
  , scPrecipitation :: !PrecipitationConfig
  , scBoundary :: !BoundaryConfig
  , scWeather :: !WeatherConfig
  , scBiome :: !BiomeConfig
  , scBiomeVegetation :: !BiomeVegetationConfig
  , scBiomeThresholds :: !BiomeThresholds
  , scBiomeFeedback :: !BiomeFeedbackConfig
  , scPlanet :: !PlanetConfig
  , scOceanCurrent :: !OceanCurrentConfig
  , scWorldSlice :: !WorldSlice
  }

data SnapshotSource
  = SnapshotFloat !(SnapshotContext -> Float)
  | SnapshotInt !(SnapshotContext -> Int)
  | SnapshotBool !(SnapshotContext -> Bool)

data SliderBinding = SliderBinding
  { sbConfigUpdate :: !(Maybe SliderConfigUpdate)
  , sbSnapshotSource :: !(Maybe SnapshotSource)
  }

lookupSliderConfigUpdate :: SliderId -> Maybe SliderConfigUpdate
lookupSliderConfigUpdate sliderIdValue = sbConfigUpdate =<< lookupSliderBinding sliderIdValue

snapshotSliderValueForId :: WorldGenConfig -> SliderId -> Float
snapshotSliderValueForId cfg sliderIdValue =
  maybe 0.0 (readSnapshotSource sliderIdValue snapshotContext) (sbSnapshotSource =<< lookupSliderBinding sliderIdValue)
  where
    snapshotContext = mkSnapshotContext cfg

lookupSliderBinding :: SliderId -> Maybe SliderBinding
lookupSliderBinding sliderIdValue = lookup sliderIdValue sliderBindings

sliderBindings :: [(SliderId, SliderBinding)]
sliderBindings =
  [ bindConfigAndSnapshot
    (terrainGenFloat SliderGenScale uiGenScale (\value gen -> gen { gcScale = value }))
    (snapshotFloat SliderGenScale (gcScale . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenCoordScale uiGenCoordScale (\value gen -> gen { gcCoordScale = value }))
    (snapshotFloat SliderGenCoordScale (gcCoordScale . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenOffsetX uiGenOffsetX (\value gen -> gen { gcOffsetX = value }))
    (snapshotFloat SliderGenOffsetX (gcOffsetX . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenOffsetY uiGenOffsetY (\value gen -> gen { gcOffsetY = value }))
    (snapshotFloat SliderGenOffsetY (gcOffsetY . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenFrequency uiGenFrequency (\value gen -> gen { gcFrequency = value }))
    (snapshotFloat SliderGenFrequency (gcFrequency . scGen))
  , bindConfigAndSnapshot
    (terrainGenInt SliderGenOctaves uiGenOctaves (\value gen -> gen { gcOctaves = value }))
    (snapshotInt SliderGenOctaves (gcOctaves . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenLacunarity uiGenLacunarity (\value gen -> gen { gcLacunarity = value }))
    (snapshotFloat SliderGenLacunarity (gcLacunarity . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenGain uiGenGain (\value gen -> gen { gcGain = value }))
    (snapshotFloat SliderGenGain (gcGain . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenWarpScale uiGenWarpScale (\value gen -> gen { gcWarpScale = value }))
    (snapshotFloat SliderGenWarpScale (gcWarpScale . scGen))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderGenWarpStrength uiGenWarpStrength (\value gen -> gen { gcWarpStrength = value }))
    (snapshotFloat SliderGenWarpStrength (gcWarpStrength . scGen))
  , bindSnapshotOnly (snapshotInt SliderExtentX (worldExtentRadiusX . scExtent))
  , bindSnapshotOnly (snapshotInt SliderExtentY (worldExtentRadiusY . scExtent))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderEdgeNorth uiEdgeDepthNorth (\value gen ->
    gen { gcOceanEdgeDepth = (gcOceanEdgeDepth gen) { oedRMin = value } }))
    (snapshotFloat SliderEdgeNorth (oedRMin . scEdge))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderEdgeSouth uiEdgeDepthSouth (\value gen ->
    gen { gcOceanEdgeDepth = (gcOceanEdgeDepth gen) { oedRMax = value } }))
    (snapshotFloat SliderEdgeSouth (oedRMax . scEdge))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderEdgeEast uiEdgeDepthEast (\value gen ->
    gen { gcOceanEdgeDepth = (gcOceanEdgeDepth gen) { oedQMax = value } }))
    (snapshotFloat SliderEdgeEast (oedQMax . scEdge))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderEdgeWest uiEdgeDepthWest (\value gen ->
    gen { gcOceanEdgeDepth = (gcOceanEdgeDepth gen) { oedQMin = value } }))
    (snapshotFloat SliderEdgeWest (oedQMin . scEdge))
  , bindConfigAndSnapshot
    (terrainGenFloat SliderEdgeFalloff uiEdgeDepthFalloff (\value gen ->
    gen { gcOceanEdgeDepth = (gcOceanEdgeDepth gen) { oedFalloff = value } }))
    (snapshotFloat SliderEdgeFalloff (oedFalloff . scEdge))
  , bindConfigAndSnapshot (tectonicsInt SliderPlateSize uiPlateSize (\value tectonics -> tectonics { tcPlateSize = value })) (snapshotInt SliderPlateSize (tcPlateSize . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderUplift uiUplift (\value tectonics -> tectonics { tcUplift = value })) (snapshotFloat SliderUplift (tcUplift . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderRiftDepth uiRiftDepth (\value tectonics -> tectonics { tcRiftDepth = value })) (snapshotFloat SliderRiftDepth (tcRiftDepth . scTectonics))
  , bindConfigAndSnapshot (parametersFloat SliderDetailScale uiDetailScale (\value parameters -> parameters { pcDetailScale = value })) (snapshotFloat SliderDetailScale (pcDetailScale . scParameters))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateSpeed uiPlateSpeed (\value tectonics -> tectonics { tcPlateSpeed = value })) (snapshotFloat SliderPlateSpeed (tcPlateSpeed . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderBoundarySharpness uiBoundarySharpness (\value tectonics -> tectonics { tcBoundarySharpness = value })) (snapshotFloat SliderBoundarySharpness (tcBoundarySharpness . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderBoundaryNoiseScale uiBoundaryNoiseScale (\value tectonics -> tectonics { tcBoundaryNoiseScale = value })) (snapshotFloat SliderBoundaryNoiseScale (tcBoundaryNoiseScale . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderBoundaryNoiseStrength uiBoundaryNoiseStrength (\value tectonics -> tectonics { tcBoundaryNoiseStrength = value })) (snapshotFloat SliderBoundaryNoiseStrength (tcBoundaryNoiseStrength . scTectonics))
  , bindConfigAndSnapshot (tectonicsInt SliderBoundaryWarpOctaves uiBoundaryWarpOctaves (\value tectonics -> tectonics { tcBoundaryWarpOctaves = value })) (snapshotInt SliderBoundaryWarpOctaves (tcBoundaryWarpOctaves . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderBoundaryWarpLacunarity uiBoundaryWarpLacunarity (\value tectonics -> tectonics { tcBoundaryWarpLacunarity = value })) (snapshotFloat SliderBoundaryWarpLacunarity (tcBoundaryWarpLacunarity . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderBoundaryWarpGain uiBoundaryWarpGain (\value tectonics -> tectonics { tcBoundaryWarpGain = value })) (snapshotFloat SliderBoundaryWarpGain (tcBoundaryWarpGain . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateMergeScale uiPlateMergeScale (\value tectonics -> tectonics { tcPlateMergeScale = value })) (snapshotFloat SliderPlateMergeScale (tcPlateMergeScale . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateMergeBias uiPlateMergeBias (\value tectonics -> tectonics { tcPlateMergeBias = value })) (snapshotFloat SliderPlateMergeBias (tcPlateMergeBias . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateDetailScale uiPlateDetailScale (\value tectonics -> tectonics { tcPlateDetailScale = value })) (snapshotFloat SliderPlateDetailScale (tcPlateDetailScale . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateDetailStrength uiPlateDetailStrength (\value tectonics -> tectonics { tcPlateDetailStrength = value })) (snapshotFloat SliderPlateDetailStrength (tcPlateDetailStrength . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateRidgeStrength uiPlateRidgeStrength (\value tectonics -> tectonics { tcPlateRidgeStrength = value })) (snapshotFloat SliderPlateRidgeStrength (tcPlateRidgeStrength . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateHeightBase uiPlateHeightBase (\value tectonics -> tectonics { tcPlateHeightBase = value })) (snapshotFloat SliderPlateHeightBase (tcPlateHeightBase . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateHeightVariance uiPlateHeightVariance (\value tectonics -> tectonics { tcPlateHeightVariance = value })) (snapshotFloat SliderPlateHeightVariance (tcPlateHeightVariance . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateHardnessBase uiPlateHardnessBase (\value tectonics -> tectonics { tcPlateHardnessBase = value })) (snapshotFloat SliderPlateHardnessBase (tcPlateHardnessBase . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateHardnessVariance uiPlateHardnessVariance (\value tectonics -> tectonics { tcPlateHardnessVariance = value })) (snapshotFloat SliderPlateHardnessVariance (tcPlateHardnessVariance . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderTrenchDepth uiTrenchDepth (\value tectonics -> tectonics { tcTrenchDepth = value })) (snapshotFloat SliderTrenchDepth (tcTrenchDepth . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderRidgeHeight uiRidgeHeight (\value tectonics -> tectonics { tcRidgeHeight = value })) (snapshotFloat SliderRidgeHeight (tcRidgeHeight . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateBiasStrength uiPlateBiasStrength (\value tectonics -> tectonics { tcPlateBiasStrength = value })) (snapshotFloat SliderPlateBiasStrength (tcPlateBiasStrength . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateBiasCenter uiPlateBiasCenter (\value tectonics -> tectonics { tcPlateBiasCenter = value })) (snapshotFloat SliderPlateBiasCenter (tcPlateBiasCenter . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateBiasEdge uiPlateBiasEdge (\value tectonics -> tectonics { tcPlateBiasEdge = value })) (snapshotFloat SliderPlateBiasEdge (tcPlateBiasEdge . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateBiasNorth uiPlateBiasNorth (\value tectonics -> tectonics { tcPlateBiasNorth = value })) (snapshotFloat SliderPlateBiasNorth (tcPlateBiasNorth . scTectonics))
  , bindConfigAndSnapshot (tectonicsFloat SliderPlateBiasSouth uiPlateBiasSouth (\value tectonics -> tectonics { tcPlateBiasSouth = value })) (snapshotFloat SliderPlateBiasSouth (tcPlateBiasSouth . scTectonics))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcCliffSlope uiTfcCliffSlope (\value terrainForm -> terrainForm { tfcCliffSlope = value })) (snapshotFloat SliderTfcCliffSlope (tfcCliffSlope . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcMountainSlope uiTfcMountainSlope (\value terrainForm -> terrainForm { tfcMountainSlope = value })) (snapshotFloat SliderTfcMountainSlope (tfcMountainSlope . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcMountainRelief uiTfcMountainRelief (\value terrainForm -> terrainForm { tfcMountainRelief = value })) (snapshotFloat SliderTfcMountainRelief (tfcMountainRelief . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcHillSlope uiTfcHillSlope (\value terrainForm -> terrainForm { tfcHillSlope = value })) (snapshotFloat SliderTfcHillSlope (tfcHillSlope . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcRollingSlope uiTfcRollingSlope (\value terrainForm -> terrainForm { tfcRollingSlope = value })) (snapshotFloat SliderTfcRollingSlope (tfcRollingSlope . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderValleyCurvature uiValleyCurvature (\value terrainForm -> terrainForm { tfcValleyCurvature = value })) (snapshotFloat SliderValleyCurvature (tfcValleyCurvature . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcElevGradient uiTfcElevGradient (\value terrainForm -> terrainForm { tfcElevGradient = value })) (snapshotFloat SliderTfcElevGradient (tfcElevGradient . scForm))
  , bindConfigAndSnapshot (terrainFormFloat SliderTfcPlateauMaxRelief2Ring uiTfcPlateauMaxRelief2Ring (\value terrainForm -> terrainForm { tfcPlateauMaxRelief2Ring = value })) (snapshotFloat SliderTfcPlateauMaxRelief2Ring (tfcPlateauMaxRelief2Ring . scForm))
  , bindConfigAndSnapshot (parametersFloat SliderRockElevationThreshold uiRockElevationThreshold (\value parameters -> parameters { pcRockElevationThreshold = value })) (snapshotFloat SliderRockElevationThreshold (pcRockElevationThreshold . scParameters))
  , bindConfigAndSnapshot (parametersFloat SliderRockHardnessThreshold uiRockHardnessThreshold (\value parameters -> parameters { pcRockHardnessThreshold = value })) (snapshotFloat SliderRockHardnessThreshold (pcRockHardnessThreshold . scParameters))
  , bindConfigAndSnapshot (parametersFloat SliderRockHardnessSecondary uiRockHardnessSecondary (\value parameters -> parameters { pcRockHardnessSecondary = value })) (snapshotFloat SliderRockHardnessSecondary (pcRockHardnessSecondary . scParameters))
  , bindConfigAndSnapshot (planetFloat SliderPlanetRadius uiPlanetRadius (\value planet -> planet { pcRadius = value })) (snapshotFloat SliderPlanetRadius (pcRadius . scPlanet))
  , bindConfigAndSnapshot (planetFloat SliderAxialTilt uiAxialTilt (\value planet -> planet { pcAxialTilt = value })) (snapshotFloat SliderAxialTilt (pcAxialTilt . scPlanet))
  , bindConfigAndSnapshot (planetFloat SliderInsolation uiInsolation (\value planet -> planet { pcInsolation = value })) (snapshotFloat SliderInsolation (pcInsolation . scPlanet))
  , bindConfigAndSnapshot (oceanCurrentFloat SliderOccWarmScale uiOccWarmScale (\value oceanCurrent -> oceanCurrent { occWarmScale = value })) (snapshotFloat SliderOccWarmScale (occWarmScale . scOceanCurrent))
  , bindConfigAndSnapshot (oceanCurrentFloat SliderOccColdScale uiOccColdScale (\value oceanCurrent -> oceanCurrent { occColdScale = value })) (snapshotFloat SliderOccColdScale (occColdScale . scOceanCurrent))
  , bindConfigAndSnapshot (oceanCurrentFloat SliderOccLatPeakDeg uiOccLatPeakDeg (\value oceanCurrent -> oceanCurrent { occLatPeakDeg = value })) (snapshotFloat SliderOccLatPeakDeg (occLatPeakDeg . scOceanCurrent))
  , bindConfigAndSnapshot (oceanCurrentFloat SliderOccLatWidthDeg uiOccLatWidthDeg (\value oceanCurrent -> oceanCurrent { occLatWidthDeg = value })) (snapshotFloat SliderOccLatWidthDeg (occLatWidthDeg . scOceanCurrent))
  , bindConfigAndSnapshot (hydroFloat SliderWaterLevel uiWaterLevel (\value hydro -> hydro { hcWaterLevel = value })) (snapshotFloat SliderWaterLevel (hcWaterLevel . scHydrology))
  , bindConfigAndSnapshot (climatePrecipFloat SliderOrographicLift uiOrographicLift (\value precipitation -> precipitation { precOrographicLift = value })) (snapshotFloat SliderOrographicLift (precOrographicLift . scPrecipitation))
  , bindConfigAndSnapshot (climatePrecipFloat SliderRainShadowLoss uiRainShadowLoss (\value precipitation -> precipitation { precRainShadowLoss = value })) (snapshotFloat SliderRainShadowLoss (precRainShadowLoss . scPrecipitation))
  , bindConfigAndSnapshot (climateWindFloat SliderWindDiffuse uiWindDiffuse (\value wind -> wind { windDiffuse = value })) (snapshotFloat SliderWindDiffuse (windDiffuse . scWind))
  , bindConfigAndSnapshot (climateTempFloat SliderEquatorTemp uiEquatorTemp (\value temperature -> temperature { tmpEquatorTemp = tempCelsiusToNorm value })) (snapshotFloat SliderEquatorTemp (tempNormToCelsius . tmpEquatorTemp . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderPoleTemp uiPoleTemp (\value temperature -> temperature { tmpPoleTemp = tempCelsiusToNorm value })) (snapshotFloat SliderPoleTemp (tempNormToCelsius . tmpPoleTemp . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderLapseRate uiLapseRate (\value temperature -> temperature { tmpLapseRate = value })) (snapshotFloat SliderLapseRate (tmpLapseRate . scTemperature))
  , bindConfigAndSnapshot (climateWindInt SliderWindIterations uiWindIterations (\value wind -> wind { windIterations = value })) (snapshotInt SliderWindIterations (windIterations . scWind))
  , bindConfigAndSnapshot (climateMoistInt SliderMoistureIterations uiMoistureIterations (\value moisture -> moisture { moistIterations = value })) (snapshotInt SliderMoistureIterations (moistIterations . scMoisture))
  , bindConfigAndSnapshot (climateBoundaryFloat SliderBoundaryMotionTemp uiBoundaryMotionTemp (\value boundary -> boundary { bndMotionTemp = value })) (snapshotFloat SliderBoundaryMotionTemp (bndMotionTemp . scBoundary))
  , bindConfigAndSnapshot (climateBoundaryFloat SliderBoundaryMotionPrecip uiBoundaryMotionPrecip (\value boundary -> boundary { bndMotionPrecip = value })) (snapshotFloat SliderBoundaryMotionPrecip (bndMotionPrecip . scBoundary))
  , bindConfigAndSnapshot (worldSliceFloat SliderSliceLatCenter uiSliceLatCenter (\value worldSliceConfig -> worldSliceConfig { wsLatCenter = value })) (snapshotFloat SliderSliceLatCenter (wsLatCenter . scWorldSlice))
  , bindConfigAndSnapshot (worldSliceFloat SliderSliceLonCenter uiSliceLonCenter (\value worldSliceConfig -> worldSliceConfig { wsLonCenter = value })) (snapshotFloat SliderSliceLonCenter (wsLonCenter . scWorldSlice))
  , bindConfigAndSnapshot (climateTempFloat SliderLatitudeExponent uiLatitudeExponent (\value temperature -> temperature { tmpLatitudeExponent = value })) (snapshotFloat SliderLatitudeExponent (tmpLatitudeExponent . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderPlateHeightCooling uiPlateHeightCooling (\value temperature -> temperature { tmpPlateHeightCooling = value })) (snapshotFloat SliderPlateHeightCooling (tmpPlateHeightCooling . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderTempNoiseScale uiTempNoiseScale (\value temperature -> temperature { tmpNoiseScale = value })) (snapshotFloat SliderTempNoiseScale (tmpNoiseScale . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderOceanModeration uiOceanModeration (\value temperature -> temperature { tmpOceanModeration = value })) (snapshotFloat SliderOceanModeration (tmpOceanModeration . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderOceanModerateTemp uiOceanModerateTemp (\value temperature -> temperature { tmpOceanModerateTemp = value })) (snapshotFloat SliderOceanModerateTemp (tmpOceanModerateTemp . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderAlbedoSensitivity uiAlbedoSensitivity (\value temperature -> temperature { tmpAlbedoSensitivity = value })) (snapshotFloat SliderAlbedoSensitivity (tmpAlbedoSensitivity . scTemperature))
  , bindConfigAndSnapshot (climateTempFloat SliderAlbedoReference uiAlbedoReference (\value temperature -> temperature { tmpAlbedoReference = value })) (snapshotFloat SliderAlbedoReference (tmpAlbedoReference . scTemperature))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistAdvect uiMoistAdvect (\value moisture -> moisture { moistAdvect = value })) (snapshotFloat SliderMoistAdvect (moistAdvect . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistLocal uiMoistLocal (\value moisture -> moisture { moistLocal = value })) (snapshotFloat SliderMoistLocal (moistLocal . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistWindEvapScale uiMoistWindEvapScale (\value moisture -> moisture { moistWindEvapScale = value })) (snapshotFloat SliderMoistWindEvapScale (moistWindEvapScale . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistEvapNoiseScale uiMoistEvapNoiseScale (\value moisture -> moisture { moistEvapNoiseScale = value })) (snapshotFloat SliderMoistEvapNoiseScale (moistEvapNoiseScale . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistBareEvapFrac uiMoistBareEvapFrac (\value moisture -> moisture { moistBareEvapFrac = value })) (snapshotFloat SliderMoistBareEvapFrac (moistBareEvapFrac . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistVegTranspFrac uiMoistVegTranspFrac (\value moisture -> moisture { moistVegTranspFrac = value })) (snapshotFloat SliderMoistVegTranspFrac (moistVegTranspFrac . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistWindETScale uiMoistWindETScale (\value moisture -> moisture { moistWindETScale = value })) (snapshotFloat SliderMoistWindETScale (moistWindETScale . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistCondensationRate uiMoistCondensationRate (\value moisture -> moisture { moistCondensationRate = value })) (snapshotFloat SliderMoistCondensationRate (moistCondensationRate . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistRecycleRate uiMoistRecycleRate (\value moisture -> moisture { moistRecycleRate = value })) (snapshotFloat SliderMoistRecycleRate (moistRecycleRate . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistITCZStrength uiMoistITCZStrength (\value moisture -> moisture { moistITCZStrength = value })) (snapshotFloat SliderMoistITCZStrength (moistITCZStrength . scMoisture))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistITCZWidth uiMoistITCZWidth (\value moisture -> moisture { moistITCZWidth = value })) (snapshotFloat SliderMoistITCZWidth (moistITCZWidth . scMoisture))
  , bindConfigAndSnapshot (climatePrecipFloat SliderOrographicScale uiOrographicScale (\value precipitation -> precipitation { precOrographicScale = value })) (snapshotFloat SliderOrographicScale (precOrographicScale . scPrecipitation))
  , bindConfigAndSnapshot (climatePrecipFloat SliderOrographicStep uiOrographicStep (\value precipitation -> precipitation { precOrographicStep = value })) (snapshotFloat SliderOrographicStep (precOrographicStep . scPrecipitation))
  , bindConfigAndSnapshot (climatePrecipInt SliderCoastalIterations uiCoastalIterations (\value precipitation -> precipitation { precCoastalIterations = value })) (snapshotInt SliderCoastalIterations (precCoastalIterations . scPrecipitation))
  , bindConfigAndSnapshot (climatePrecipFloat SliderCoastalDiffuse uiCoastalDiffuse (\value precipitation -> precipitation { precCoastalDiffuse = value })) (snapshotFloat SliderCoastalDiffuse (precCoastalDiffuse . scPrecipitation))
  , bindConfigAndSnapshot (climatePrecipFloat SliderCoastalMoistureBoost uiCoastalMoistureBoost (\value precipitation -> precipitation { precCoastalMoistureBoost = value })) (snapshotFloat SliderCoastalMoistureBoost (precCoastalMoistureBoost . scPrecipitation))
  , bindConfigAndSnapshot (climateWindFloat SliderWindBeltStrength uiWindBeltStrength (\value wind -> wind { windBeltStrength = value })) (snapshotFloat SliderWindBeltStrength (windBeltStrength . scWind))
  , bindConfigAndSnapshot (climateWindFloat SliderWindBeltHarmonics uiWindBeltHarmonics (\value wind -> wind { windBeltHarmonics = value })) (snapshotFloat SliderWindBeltHarmonics (windBeltHarmonics . scWind))
  , bindConfigAndSnapshot (climateWindFloat SliderWindBeltBase uiWindBeltBase (\value wind -> wind { windBeltBase = value })) (snapshotFloat SliderWindBeltBase (windBeltBase . scWind))
  , bindConfigAndSnapshot (climateWindFloat SliderWindBeltRange uiWindBeltRange (\value wind -> wind { windBeltRange = value })) (snapshotFloat SliderWindBeltRange (windBeltRange . scWind))
  , bindConfigAndSnapshot (climateWindFloat SliderWindBeltSpeedScale uiWindBeltSpeedScale (\value wind -> wind { windBeltSpeedScale = value })) (snapshotFloat SliderWindBeltSpeedScale (windBeltSpeedScale . scWind))
  , bindConfigAndSnapshot (climateBoundaryFloat SliderBndLandRange uiBndLandRange (\value boundary -> boundary { bndLandRange = value })) (snapshotFloat SliderBndLandRange (bndLandRange . scBoundary))
  , bindConfigAndSnapshot (hydroFloat SliderPiedmontSmooth uiPiedmontSmooth (\value hydro -> hydro { hcPiedmontSmoothStrength = value })) (snapshotFloat SliderPiedmontSmooth (hcPiedmontSmoothStrength . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderPiedmontSlopeMin uiPiedmontSlopeMin (\value hydro -> hydro { hcPiedmontSlopeMin = value })) (snapshotFloat SliderPiedmontSlopeMin (hcPiedmontSlopeMin . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderPiedmontSlopeMax uiPiedmontSlopeMax (\value hydro -> hydro { hcPiedmontSlopeMax = value })) (snapshotFloat SliderPiedmontSlopeMax (hcPiedmontSlopeMax . scHydrology))
  , bindConfigAndSnapshot (climateWindFloat SliderWindCoriolisDeflection uiWindCoriolisDeflection (\value wind -> wind { windCoriolisDeflection = value })) (snapshotFloat SliderWindCoriolisDeflection (windCoriolisDeflection . scWind))
  , bindConfigAndSnapshot (climateMoistFloat SliderMoistMinVegFloor uiMoistMinVegFloor (\value moisture -> moisture { moistMinVegFloor = value })) (snapshotFloat SliderMoistMinVegFloor (moistMinVegFloor . scMoisture))
  , bindConfigAndSnapshot (weatherFloat SliderWeatherTick uiWeatherTick (\value weather -> weather { wcTickSeconds = value })) (snapshotFloat SliderWeatherTick (wcTickSeconds . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderWeatherPhase uiWeatherPhase (\value weather -> weather { wcSeasonPhase = value })) (snapshotFloat SliderWeatherPhase (wcSeasonPhase . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderWeatherAmplitude uiWeatherAmplitude (\value weather -> weather { wcSeasonAmplitude = value })) (snapshotFloat SliderWeatherAmplitude (wcSeasonAmplitude . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderSeasonCycleLength uiSeasonCycleLength (\value weather -> weather { wcSeasonCycleLength = value })) (snapshotFloat SliderSeasonCycleLength (wcSeasonCycleLength . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderJitterAmplitude uiJitterAmplitude (\value weather -> weather { wcJitterAmplitude = value })) (snapshotFloat SliderJitterAmplitude (wcJitterAmplitude . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPressureBase uiPressureBase (\value weather -> weather { wcPressureBase = value })) (snapshotFloat SliderPressureBase (wcPressureBase . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPressureTempScale uiPressureTempScale (\value weather -> weather { wcPressureTempScale = value })) (snapshotFloat SliderPressureTempScale (wcPressureTempScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPressureCoriolisScale uiPressureCoriolisScale (\value weather -> weather { wcPressureCoriolisScale = value })) (snapshotFloat SliderPressureCoriolisScale (wcPressureCoriolisScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderSeasonalBase uiSeasonalBase (\value weather -> weather { wcSeasonalBase = value })) (snapshotFloat SliderSeasonalBase (wcSeasonalBase . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderSeasonalRange uiSeasonalRange (\value weather -> weather { wcSeasonalRange = value })) (snapshotFloat SliderSeasonalRange (wcSeasonalRange . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderHumidityNoiseScale uiHumidityNoiseScale (\value weather -> weather { wcHumidityNoiseScale = value })) (snapshotFloat SliderHumidityNoiseScale (wcHumidityNoiseScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPrecipNoiseScale uiPrecipNoiseScale (\value weather -> weather { wcPrecipNoiseScale = value })) (snapshotFloat SliderPrecipNoiseScale (wcPrecipNoiseScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderWeatherITCZWidth uiWeatherITCZWidth (\value weather -> weather { wcITCZWidth = value })) (snapshotFloat SliderWeatherITCZWidth (wcITCZWidth . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderWeatherITCZPrecipBoost uiWeatherITCZPrecipBoost (\value weather -> weather { wcITCZPrecipBoost = value })) (snapshotFloat SliderWeatherITCZPrecipBoost (wcITCZPrecipBoost . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPressureHumidityScale uiPressureHumidityScale (\value weather -> weather { wcPressureHumidityScale = value })) (snapshotFloat SliderPressureHumidityScale (wcPressureHumidityScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderPressureGradientWindScale uiPressureGradientWindScale (\value weather -> weather { wcPressureGradientWindScale = value })) (snapshotFloat SliderPressureGradientWindScale (wcPressureGradientWindScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderWindNoiseScale uiWindNoiseScale (\value weather -> weather { wcWindNoiseScale = value })) (snapshotFloat SliderWindNoiseScale (wcWindNoiseScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderITCZMigrationScale uiITCZMigrationScale (\value weather -> weather { wcITCZMigrationScale = value })) (snapshotFloat SliderITCZMigrationScale (wcITCZMigrationScale . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderCloudRHExponent uiCloudRHExponent (\value weather -> weather { wcCloudRHExponent = value })) (snapshotFloat SliderCloudRHExponent (wcCloudRHExponent . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderCloudAlbedoEffect uiCloudAlbedoEffect (\value weather -> weather { wcCloudAlbedoEffect = value })) (snapshotFloat SliderCloudAlbedoEffect (wcCloudAlbedoEffect . scWeather))
  , bindConfigAndSnapshot (weatherFloat SliderCloudPrecipBoost uiCloudPrecipBoost (\value weather -> weather { wcCloudPrecipBoost = value })) (snapshotFloat SliderCloudPrecipBoost (wcCloudPrecipBoost . scWeather))
  , bindConfigAndSnapshot (biomeVegetationFloat SliderVegBase uiVegBase (\value vegetation -> vegetation { vcDensityScale = value })) (snapshotFloat SliderVegBase (vcDensityScale . scBiomeVegetation))
  , bindConfigAndSnapshot (biomeVegetationFloat SliderVegBoost uiVegBoost (\value vegetation -> vegetation { vcClimateSlopeScale = value })) (snapshotFloat SliderVegBoost (vcClimateSlopeScale . scBiomeVegetation))
  , bindConfigAndSnapshot (biomeVegetationFloat SliderVegTempWeight uiVegTempWeight (\value vegetation -> vegetation { vcTempWeight = value })) (snapshotFloat SliderVegTempWeight (vcTempWeight . scBiomeVegetation))
  , bindConfigAndSnapshot (biomeVegetationFloat SliderVegPrecipWeight uiVegPrecipWeight (\value vegetation -> vegetation { vcPrecipWeight = value })) (snapshotFloat SliderVegPrecipWeight (vcPrecipWeight . scBiomeVegetation))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtCoastalBand uiBtCoastalBand (\value thresholds -> thresholds { btCoastalBand = value })) (snapshotFloat SliderBtCoastalBand (btCoastalBand . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtSnowMaxTemp uiBtSnowMaxTemp (\value thresholds -> thresholds { btSnowMaxTemp = value })) (snapshotFloat SliderBtSnowMaxTemp (btSnowMaxTemp . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtAlpineMaxTemp uiBtAlpineMaxTemp (\value thresholds -> thresholds { btAlpineMaxTemp = value })) (snapshotFloat SliderBtAlpineMaxTemp (btAlpineMaxTemp . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtIceCapTemp uiBtIceCapTemp (\value thresholds -> thresholds { btIceCapTemp = value })) (snapshotFloat SliderBtIceCapTemp (btIceCapTemp . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtMontaneMaxTemp uiBtMontaneMaxTemp (\value thresholds -> thresholds { btMontaneMaxTemp = value })) (snapshotFloat SliderBtMontaneMaxTemp (btMontaneMaxTemp . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtMontanePrecip uiBtMontanePrecip (\value thresholds -> thresholds { btMontanePrecip = value })) (snapshotFloat SliderBtMontanePrecip (btMontanePrecip . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtCliffSlope uiBtCliffSlope (\value thresholds -> thresholds { btCliffSlope = value })) (snapshotFloat SliderBtCliffSlope (btCliffSlope . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtValleyMoisture uiBtValleyMoisture (\value thresholds -> thresholds { btValleyMoisture = value })) (snapshotFloat SliderBtValleyMoisture (btValleyMoisture . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtDepressionMoisture uiBtDepressionMoisture (\value thresholds -> thresholds { btDepressionMoisture = value })) (snapshotFloat SliderBtDepressionMoisture (btDepressionMoisture . scBiomeThresholds))
  , bindConfigAndSnapshot (biomeThresholdFloat SliderBtPrecipWeight uiBtPrecipWeight (\value thresholds -> thresholds { btPrecipWeight = value })) (snapshotFloat SliderBtPrecipWeight (btPrecipWeight . scBiomeThresholds))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcTempMin uiVbcTempMin (\value vegetation -> vegetation { vbcTempMin = value })) (snapshotFloat SliderVbcTempMin (vbcTempMin . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcTempRange uiVbcTempRange (\value vegetation -> vegetation { vbcTempRange = value })) (snapshotFloat SliderVbcTempRange (vbcTempRange . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcFertilityBoost uiVbcFertilityBoost (\value vegetation -> vegetation { vbcFertilityBoost = value })) (snapshotFloat SliderVbcFertilityBoost (vbcFertilityBoost . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcAlbedoBase uiVbcAlbedoBase (\value vegetation -> vegetation { vbcAlbedoBase = value })) (snapshotFloat SliderVbcAlbedoBase (vbcAlbedoBase . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcAlbedoBare uiVbcAlbedoBare (\value vegetation -> vegetation { vbcAlbedoBare = value })) (snapshotFloat SliderVbcAlbedoBare (vbcAlbedoBare . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcAlbedoVeg uiVbcAlbedoVeg (\value vegetation -> vegetation { vbcAlbedoVeg = value })) (snapshotFloat SliderVbcAlbedoVeg (vbcAlbedoVeg . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcOceanAlbedo uiVbcOceanAlbedo (\value vegetation -> vegetation { vbcOceanAlbedo = value })) (snapshotFloat SliderVbcOceanAlbedo (vbcOceanAlbedo . scVegetationBootstrap))
  , bindConfigAndSnapshot (vegetationBootstrapFloat SliderVbcIceAlbedo uiVbcIceAlbedo (\value vegetation -> vegetation { vbcIceAlbedo = value })) (snapshotFloat SliderVbcIceAlbedo (vbcIceAlbedo . scVegetationBootstrap))
  , bindConfigAndSnapshot (biomeInt SliderBiomeSmoothing uiBiomeSmoothing (\value biome -> biome { bcSmoothingIterations = value })) (snapshotInt SliderBiomeSmoothing (bcSmoothingIterations . scBiome))
  , bindConfigAndSnapshot (biomeFloat SliderVolcanicAshBoost uiVolcanicAshBoost (\value biome -> biome { bcVolcanicAshBoost = value })) (snapshotFloat SliderVolcanicAshBoost (bcVolcanicAshBoost . scBiome))
  , bindConfigAndSnapshot (biomeFloat SliderVolcanicLavaPenalty uiVolcanicLavaPenalty (\value biome -> biome { bcVolcanicLavaPenalty = value })) (snapshotFloat SliderVolcanicLavaPenalty (bcVolcanicLavaPenalty . scBiome))
  , bindConfigAndSnapshot (biomeFeedbackFloat SliderBiomeFeedbackBlend uiBiomeFeedbackBlend (\value feedback -> feedback { bfcBlendWeight = value })) (snapshotFloat SliderBiomeFeedbackBlend (bfcBlendWeight . scBiomeFeedback))
  , bindConfigAndSnapshot (erosionInt SliderErosionHydraulic uiErosionHydraulic (\value erosion -> erosion { ecHydraulicIterations = value })) (snapshotInt SliderErosionHydraulic (ecHydraulicIterations . scErosion))
  , bindConfigAndSnapshot (erosionInt SliderErosionThermal uiErosionThermal (\value erosion -> erosion { ecThermalIterations = value })) (snapshotInt SliderErosionThermal (ecThermalIterations . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionRainRate uiRainRate (\value erosion -> erosion { ecRainRate = value })) (snapshotFloat SliderErosionRainRate (ecRainRate . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionTalus uiErosionTalus (\value erosion -> erosion { ecThermalTalus = value })) (snapshotFloat SliderErosionTalus (ecThermalTalus . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionMaxDrop uiErosionMaxDrop (\value erosion -> erosion { ecMaxDrop = value })) (snapshotFloat SliderErosionMaxDrop (ecMaxDrop . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionHydDeposit uiErosionHydDeposit (\value erosion -> erosion { ecHydraulicDepositRatio = value })) (snapshotFloat SliderErosionHydDeposit (ecHydraulicDepositRatio . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionDepositSlope uiErosionDepositSlope (\value erosion -> erosion { ecHydraulicDepositMaxSlope = value })) (snapshotFloat SliderErosionDepositSlope (ecHydraulicDepositMaxSlope . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionThermDeposit uiErosionThermDeposit (\value erosion -> erosion { ecThermalDepositRatio = value })) (snapshotFloat SliderErosionThermDeposit (ecThermalDepositRatio . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionCoastZone uiErosionCoastZone (\value erosion -> erosion { ecCoastalSmoothZone = value })) (snapshotFloat SliderErosionCoastZone (ecCoastalSmoothZone . scErosion))
  , bindConfigAndSnapshot (erosionFloat SliderErosionCoastStrength uiErosionCoastStrength (\value erosion -> erosion { ecCoastalSmoothStrength = value })) (snapshotFloat SliderErosionCoastStrength (ecCoastalSmoothStrength . scErosion))
  , bindConfigAndSnapshot (erosionInt SliderErosionCoastIter uiErosionCoastIter (\value erosion -> erosion { ecCoastalSmoothIterations = value })) (snapshotInt SliderErosionCoastIter (ecCoastalSmoothIterations . scErosion))
  , bindConfigAndSnapshot (hypsometryBool SliderHypsometryEnabled uiHypsometryEnabled (\value hypsometry -> hypsometry { hpEnabled = value })) (snapshotBool SliderHypsometryEnabled (hpEnabled . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryLowlandExp uiHypsometryLowlandExp (\value hypsometry -> hypsometry { hpLowlandExponent = value })) (snapshotFloat SliderHypsometryLowlandExp (hpLowlandExponent . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryHighlandExp uiHypsometryHighlandExp (\value hypsometry -> hypsometry { hpHighlandExponent = value })) (snapshotFloat SliderHypsometryHighlandExp (hpHighlandExponent . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryPlateauBreak uiHypsometryPlateauBreak (\value hypsometry -> hypsometry { hpPlateauBreak = value })) (snapshotFloat SliderHypsometryPlateauBreak (hpPlateauBreak . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryOceanExp uiHypsometryOceanExp (\value hypsometry -> hypsometry { hpOceanExponent = value })) (snapshotFloat SliderHypsometryOceanExp (hpOceanExponent . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryCoastalRampWidth uiHypsometryCoastalRampWidth (\value hypsometry -> hypsometry { hpCoastalRampWidth = value })) (snapshotFloat SliderHypsometryCoastalRampWidth (hpCoastalRampWidth . scHypsometry))
  , bindConfigAndSnapshot (hypsometryFloat SliderHypsometryCoastalRampStr uiHypsometryCoastalRampStr (\value hypsometry -> hypsometry { hpCoastalRampStrength = value })) (snapshotFloat SliderHypsometryCoastalRampStr (hpCoastalRampStrength . scHypsometry))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierSnowTemp uiGlacierSnowTemp (\value glacier -> glacier { gcSnowTemp = value })) (snapshotFloat SliderGlacierSnowTemp (gcSnowTemp . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierSnowRange uiGlacierSnowRange (\value glacier -> glacier { gcSnowRange = value })) (snapshotFloat SliderGlacierSnowRange (gcSnowRange . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierMeltTemp uiGlacierMeltTemp (\value glacier -> glacier { gcMeltTemp = value })) (snapshotFloat SliderGlacierMeltTemp (gcMeltTemp . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierMeltRate uiGlacierMeltRate (\value glacier -> glacier { gcMeltRate = value })) (snapshotFloat SliderGlacierMeltRate (gcMeltRate . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierAccumScale uiGlacierAccumScale (\value glacier -> glacier { gcAccumScale = value })) (snapshotFloat SliderGlacierAccumScale (gcAccumScale . scGlacier))
  , bindConfigAndSnapshot (glacierInt SliderGlacierFlowIters uiGlacierFlowIters (\value glacier -> glacier { gcFlowIterations = value })) (snapshotInt SliderGlacierFlowIters (gcFlowIterations . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierFlowRate uiGlacierFlowRate (\value glacier -> glacier { gcFlowRate = value })) (snapshotFloat SliderGlacierFlowRate (gcFlowRate . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierErosionScale uiGlacierErosionScale (\value glacier -> glacier { gcErosionScale = value })) (snapshotFloat SliderGlacierErosionScale (gcErosionScale . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierCarveScale uiGlacierCarveScale (\value glacier -> glacier { gcCarveScale = value })) (snapshotFloat SliderGlacierCarveScale (gcCarveScale . scGlacier))
  , bindConfigAndSnapshot (glacierFloat SliderGlacierDepositScale uiGlacierDepositScale (\value glacier -> glacier { gcDepositScale = value })) (snapshotFloat SliderGlacierDepositScale (gcDepositScale . scGlacier))
  , bindConfigAndSnapshot (volcanismFloat SliderVentDensity uiVentDensity (\value volcanism -> volcanism { vcVentDensityBase = value })) (snapshotFloat SliderVentDensity (vcVentDensityBase . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderVentThreshold uiVentThreshold (\value volcanism -> volcanism { vcVentThreshold = value })) (snapshotFloat SliderVentThreshold (vcVentThreshold . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderHotspotScale uiHotspotScale (\value volcanism -> volcanism { vcHotspotScale = value })) (snapshotFloat SliderHotspotScale (vcHotspotScale . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderHotspotThreshold uiHotspotThreshold (\value volcanism -> volcanism { vcHotspotThreshold = value })) (snapshotFloat SliderHotspotThreshold (vcHotspotThreshold . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderMagmaRecharge uiMagmaRecharge (\value volcanism -> volcanism { vcMagmaRecharge = value })) (snapshotFloat SliderMagmaRecharge (vcMagmaRecharge . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderLavaScale uiLavaScale (\value volcanism -> volcanism { vcLavaScale = value })) (snapshotFloat SliderLavaScale (vcLavaScale . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderAshScale uiAshScale (\value volcanism -> volcanism { vcAshScale = value })) (snapshotFloat SliderAshScale (vcAshScale . scVolcanism))
  , bindConfigAndSnapshot (volcanismFloat SliderVolcanicDepositScale uiVolcanicDepositScale (\value volcanism -> volcanism { vcDepositScale = value })) (snapshotFloat SliderVolcanicDepositScale (vcDepositScale . scVolcanism))
  , bindConfigAndSnapshot (soilFloat SliderSoilMoistureThreshold uiSoilMoistureThreshold (\value soil -> soil { scMoistureThreshold = value })) (snapshotFloat SliderSoilMoistureThreshold (scMoistureThreshold . scSoil))
  , bindConfigAndSnapshot (soilFloat SliderSoilHardnessThreshold uiSoilHardnessThreshold (\value soil -> soil { scHardnessThreshold = value })) (snapshotFloat SliderSoilHardnessThreshold (scHardnessThreshold . scSoil))
  , bindConfigAndSnapshot (soilFloat SliderSoilFertilityMoistWeight uiSoilFertilityMoistWeight (\value soil -> soil { scFertilityMoistWeight = value })) (snapshotFloat SliderSoilFertilityMoistWeight (scFertilityMoistWeight . scSoil))
  , bindConfigAndSnapshot (soilFloat SliderSoilFertilityDepthWeight uiSoilFertilityDepthWeight (\value soil -> soil { scFertilityDepthWeight = value })) (snapshotFloat SliderSoilFertilityDepthWeight (scFertilityDepthWeight . scSoil))
  , bindConfigAndSnapshot (hydroFloat SliderSinkBreachDepth uiSinkBreachDepth (\value hydro -> hydro { hcSinkBreachDepth = value })) (snapshotFloat SliderSinkBreachDepth (hcSinkBreachDepth . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderStreamPowerMaxErosion uiStreamPowerMaxErosion (\value hydro -> hydro { hcStreamPowerMaxErosion = value })) (snapshotFloat SliderStreamPowerMaxErosion (hcStreamPowerMaxErosion . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderRiverCarveMaxDepth uiRiverCarveMaxDepth (\value hydro -> hydro { hcRiverCarveMaxDepth = value })) (snapshotFloat SliderRiverCarveMaxDepth (hcRiverCarveMaxDepth . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderCoastalErodeStrength uiCoastalErodeStrength (\value hydro -> hydro { hcCoastalErodeStrength = value })) (snapshotFloat SliderCoastalErodeStrength (hcCoastalErodeStrength . scHydrology))
  , bindConfigAndSnapshot (hydroFloat SliderHydroHardnessWeight uiHydroHardnessWeight (\value hydro -> hydro { hcHardnessErodeWeight = value })) (snapshotFloat SliderHydroHardnessWeight (hcHardnessErodeWeight . scHydrology))
  , bindConfigAndSnapshot (waterBodyInt SliderMinLakeSize uiMinLakeSize (\value waterBody -> waterBody { wbcMinLakeSize = value })) (snapshotInt SliderMinLakeSize (wbcMinLakeSize . scWaterBody))
  , bindConfigAndSnapshot (waterBodyInt SliderInlandSeaMinSize uiInlandSeaMinSize (\value waterBody -> waterBody { wbcInlandSeaMinSize = value })) (snapshotInt SliderInlandSeaMinSize (wbcInlandSeaMinSize . scWaterBody))
  , bindConfigAndSnapshot (parametersFloat SliderRoughnessScale uiRoughnessScale (\value parameters -> parameters { pcRoughnessScale = value })) (snapshotFloat SliderRoughnessScale (pcRoughnessScale . scParameters))
  ]

sliderFloat :: SliderId -> (UiState -> Float) -> (Float -> WorldGenConfig -> WorldGenConfig) -> (SliderId, SliderConfigUpdate)
sliderFloat sliderIdValue readUi writeConfig =
  ( sliderIdValue
  , \ui cfg -> writeConfig (sliderToDomainFloat sliderIdValue (readUi ui)) cfg
  )

sliderInt :: SliderId -> (UiState -> Float) -> (Int -> WorldGenConfig -> WorldGenConfig) -> (SliderId, SliderConfigUpdate)
sliderInt sliderIdValue readUi writeConfig =
  ( sliderIdValue
  , \ui cfg -> writeConfig (sliderToDomainInt sliderIdValue (readUi ui)) cfg
  )

sliderBool :: SliderId -> (UiState -> Float) -> (Bool -> WorldGenConfig -> WorldGenConfig) -> (SliderId, SliderConfigUpdate)
sliderBool sliderIdValue readUi writeConfig =
  ( sliderIdValue
  , \ui cfg -> writeConfig (sliderToDomainBool sliderIdValue (readUi ui)) cfg
  )

terrainGenFloat :: SliderId -> (UiState -> Float) -> (Float -> GenConfig -> GenConfig) -> (SliderId, SliderConfigUpdate)
terrainGenFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainGen . updateField)

terrainGenInt :: SliderId -> (UiState -> Float) -> (Int -> GenConfig -> GenConfig) -> (SliderId, SliderConfigUpdate)
terrainGenInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateTerrainGen . updateField)

tectonicsFloat :: SliderId -> (UiState -> Float) -> (Float -> TectonicsConfig -> TectonicsConfig) -> (SliderId, SliderConfigUpdate)
tectonicsFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainTectonics . updateField)

tectonicsInt :: SliderId -> (UiState -> Float) -> (Int -> TectonicsConfig -> TectonicsConfig) -> (SliderId, SliderConfigUpdate)
tectonicsInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateTerrainTectonics . updateField)

parametersFloat :: SliderId -> (UiState -> Float) -> (Float -> ParameterConfig -> ParameterConfig) -> (SliderId, SliderConfigUpdate)
parametersFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainParameters . updateField)

terrainFormFloat :: SliderId -> (UiState -> Float) -> (Float -> TerrainFormConfig -> TerrainFormConfig) -> (SliderId, SliderConfigUpdate)
terrainFormFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainForm . updateField)

hydroFloat :: SliderId -> (UiState -> Float) -> (Float -> HydroConfig -> HydroConfig) -> (SliderId, SliderConfigUpdate)
hydroFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainHydrology . updateField)

erosionFloat :: SliderId -> (UiState -> Float) -> (Float -> ErosionConfig -> ErosionConfig) -> (SliderId, SliderConfigUpdate)
erosionFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainErosion . updateField)

erosionInt :: SliderId -> (UiState -> Float) -> (Int -> ErosionConfig -> ErosionConfig) -> (SliderId, SliderConfigUpdate)
erosionInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateTerrainErosion . updateField)

hypsometryFloat :: SliderId -> (UiState -> Float) -> (Float -> HypsometryConfig -> HypsometryConfig) -> (SliderId, SliderConfigUpdate)
hypsometryFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainHypsometry . updateField)

hypsometryBool :: SliderId -> (UiState -> Float) -> (Bool -> HypsometryConfig -> HypsometryConfig) -> (SliderId, SliderConfigUpdate)
hypsometryBool sliderIdValue readUi updateField =
  sliderBool sliderIdValue readUi (updateTerrainHypsometry . updateField)

glacierFloat :: SliderId -> (UiState -> Float) -> (Float -> GlacierConfig -> GlacierConfig) -> (SliderId, SliderConfigUpdate)
glacierFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainGlacier . updateField)

glacierInt :: SliderId -> (UiState -> Float) -> (Int -> GlacierConfig -> GlacierConfig) -> (SliderId, SliderConfigUpdate)
glacierInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateTerrainGlacier . updateField)

volcanismFloat :: SliderId -> (UiState -> Float) -> (Float -> VolcanismConfig -> VolcanismConfig) -> (SliderId, SliderConfigUpdate)
volcanismFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainVolcanism . updateField)

soilFloat :: SliderId -> (UiState -> Float) -> (Float -> SoilConfig -> SoilConfig) -> (SliderId, SliderConfigUpdate)
soilFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainSoil . updateField)

waterBodyInt :: SliderId -> (UiState -> Float) -> (Int -> WaterBodyConfig -> WaterBodyConfig) -> (SliderId, SliderConfigUpdate)
waterBodyInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateTerrainWaterBody . updateField)

vegetationBootstrapFloat :: SliderId -> (UiState -> Float) -> (Float -> VegetationBootstrapConfig -> VegetationBootstrapConfig) -> (SliderId, SliderConfigUpdate)
vegetationBootstrapFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateTerrainVegetation . updateField)

climateTempFloat :: SliderId -> (UiState -> Float) -> (Float -> TemperatureConfig -> TemperatureConfig) -> (SliderId, SliderConfigUpdate)
climateTempFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateClimateTemperature . updateField)

climateWindFloat :: SliderId -> (UiState -> Float) -> (Float -> WindConfig -> WindConfig) -> (SliderId, SliderConfigUpdate)
climateWindFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateClimateWind . updateField)

climateWindInt :: SliderId -> (UiState -> Float) -> (Int -> WindConfig -> WindConfig) -> (SliderId, SliderConfigUpdate)
climateWindInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateClimateWind . updateField)

climateMoistFloat :: SliderId -> (UiState -> Float) -> (Float -> MoistureConfig -> MoistureConfig) -> (SliderId, SliderConfigUpdate)
climateMoistFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateClimateMoisture . updateField)

climateMoistInt :: SliderId -> (UiState -> Float) -> (Int -> MoistureConfig -> MoistureConfig) -> (SliderId, SliderConfigUpdate)
climateMoistInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateClimateMoisture . updateField)

climatePrecipFloat :: SliderId -> (UiState -> Float) -> (Float -> PrecipitationConfig -> PrecipitationConfig) -> (SliderId, SliderConfigUpdate)
climatePrecipFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateClimatePrecipitation . updateField)

climatePrecipInt :: SliderId -> (UiState -> Float) -> (Int -> PrecipitationConfig -> PrecipitationConfig) -> (SliderId, SliderConfigUpdate)
climatePrecipInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateClimatePrecipitation . updateField)

climateBoundaryFloat :: SliderId -> (UiState -> Float) -> (Float -> BoundaryConfig -> BoundaryConfig) -> (SliderId, SliderConfigUpdate)
climateBoundaryFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateClimateBoundary . updateField)

weatherFloat :: SliderId -> (UiState -> Float) -> (Float -> WeatherConfig -> WeatherConfig) -> (SliderId, SliderConfigUpdate)
weatherFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldWeather . updateField)

biomeFloat :: SliderId -> (UiState -> Float) -> (Float -> BiomeConfig -> BiomeConfig) -> (SliderId, SliderConfigUpdate)
biomeFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldBiome . updateField)

biomeInt :: SliderId -> (UiState -> Float) -> (Int -> BiomeConfig -> BiomeConfig) -> (SliderId, SliderConfigUpdate)
biomeInt sliderIdValue readUi updateField =
  sliderInt sliderIdValue readUi (updateWorldBiome . updateField)

biomeVegetationFloat :: SliderId -> (UiState -> Float) -> (Float -> BiomeVegetationConfig -> BiomeVegetationConfig) -> (SliderId, SliderConfigUpdate)
biomeVegetationFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateBiomeVegetation . updateField)

biomeThresholdFloat :: SliderId -> (UiState -> Float) -> (Float -> BiomeThresholds -> BiomeThresholds) -> (SliderId, SliderConfigUpdate)
biomeThresholdFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateBiomeThresholds . updateField)

planetFloat :: SliderId -> (UiState -> Float) -> (Float -> PlanetConfig -> PlanetConfig) -> (SliderId, SliderConfigUpdate)
planetFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldPlanet . updateField)

worldSliceFloat :: SliderId -> (UiState -> Float) -> (Float -> WorldSlice -> WorldSlice) -> (SliderId, SliderConfigUpdate)
worldSliceFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldSlice . updateField)

oceanCurrentFloat :: SliderId -> (UiState -> Float) -> (Float -> OceanCurrentConfig -> OceanCurrentConfig) -> (SliderId, SliderConfigUpdate)
oceanCurrentFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldOceanCurrent . updateField)

biomeFeedbackFloat :: SliderId -> (UiState -> Float) -> (Float -> BiomeFeedbackConfig -> BiomeFeedbackConfig) -> (SliderId, SliderConfigUpdate)
biomeFeedbackFloat sliderIdValue readUi updateField =
  sliderFloat sliderIdValue readUi (updateWorldBiomeFeedback . updateField)

updateTerrainGen :: (GenConfig -> GenConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainGen updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainGen = updateField (terrainGen terrain) } }

updateTerrainTectonics :: (TectonicsConfig -> TectonicsConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainTectonics updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainTectonics = updateField (terrainTectonics terrain) } }

updateTerrainParameters :: (ParameterConfig -> ParameterConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainParameters updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainParameters = updateField (terrainParameters terrain) } }

updateTerrainForm :: (TerrainFormConfig -> TerrainFormConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainForm updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainFormConfig = updateField (terrainFormConfig terrain) } }

updateTerrainHydrology :: (HydroConfig -> HydroConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainHydrology updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainHydrology = updateField (terrainHydrology terrain) } }

updateTerrainErosion :: (ErosionConfig -> ErosionConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainErosion updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainErosion = updateField (terrainErosion terrain) } }

updateTerrainHypsometry :: (HypsometryConfig -> HypsometryConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainHypsometry updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainHypsometry = updateField (terrainHypsometry terrain) } }

updateTerrainGlacier :: (GlacierConfig -> GlacierConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainGlacier updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainGlacier = updateField (terrainGlacier terrain) } }

updateTerrainVolcanism :: (VolcanismConfig -> VolcanismConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainVolcanism updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainVolcanism = updateField (terrainVolcanism terrain) } }

updateTerrainSoil :: (SoilConfig -> SoilConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainSoil updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainSoil = updateField (terrainSoil terrain) } }

updateTerrainWaterBody :: (WaterBodyConfig -> WaterBodyConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainWaterBody updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainWaterBody = updateField (terrainWaterBody terrain) } }

updateTerrainVegetation :: (VegetationBootstrapConfig -> VegetationBootstrapConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainVegetation updateField cfg =
  let terrain = worldTerrain cfg
  in cfg { worldTerrain = terrain { terrainVegetation = updateField (terrainVegetation terrain) } }

updateClimateTemperature :: (TemperatureConfig -> TemperatureConfig) -> WorldGenConfig -> WorldGenConfig
updateClimateTemperature updateField cfg =
  let climate = worldClimate cfg
  in cfg { worldClimate = climate { ccTemperature = updateField (ccTemperature climate) } }

updateClimateWind :: (WindConfig -> WindConfig) -> WorldGenConfig -> WorldGenConfig
updateClimateWind updateField cfg =
  let climate = worldClimate cfg
  in cfg { worldClimate = climate { ccWind = updateField (ccWind climate) } }

updateClimateMoisture :: (MoistureConfig -> MoistureConfig) -> WorldGenConfig -> WorldGenConfig
updateClimateMoisture updateField cfg =
  let climate = worldClimate cfg
  in cfg { worldClimate = climate { ccMoisture = updateField (ccMoisture climate) } }

updateClimatePrecipitation :: (PrecipitationConfig -> PrecipitationConfig) -> WorldGenConfig -> WorldGenConfig
updateClimatePrecipitation updateField cfg =
  let climate = worldClimate cfg
  in cfg { worldClimate = climate { ccPrecipitation = updateField (ccPrecipitation climate) } }

updateClimateBoundary :: (BoundaryConfig -> BoundaryConfig) -> WorldGenConfig -> WorldGenConfig
updateClimateBoundary updateField cfg =
  let climate = worldClimate cfg
  in cfg { worldClimate = climate { ccBoundary = updateField (ccBoundary climate) } }

updateWorldWeather :: (WeatherConfig -> WeatherConfig) -> WorldGenConfig -> WorldGenConfig
updateWorldWeather updateField cfg =
  cfg { worldWeather = updateField (worldWeather cfg) }

updateWorldBiome :: (BiomeConfig -> BiomeConfig) -> WorldGenConfig -> WorldGenConfig
updateWorldBiome updateField cfg =
  cfg { worldBiome = updateField (worldBiome cfg) }

updateBiomeVegetation :: (BiomeVegetationConfig -> BiomeVegetationConfig) -> WorldGenConfig -> WorldGenConfig
updateBiomeVegetation updateField cfg =
  let biome = worldBiome cfg
  in cfg { worldBiome = biome { bcVegetation = updateField (bcVegetation biome) } }

updateBiomeThresholds :: (BiomeThresholds -> BiomeThresholds) -> WorldGenConfig -> WorldGenConfig
updateBiomeThresholds updateField cfg =
  let biome = worldBiome cfg
  in cfg { worldBiome = biome { bcThresholds = updateField (bcThresholds biome) } }

updateWorldPlanet :: (PlanetConfig -> PlanetConfig) -> WorldGenConfig -> WorldGenConfig
updateWorldPlanet updateField cfg =
  cfg { worldPlanet = updateField (worldPlanet cfg) }

updateWorldSlice :: (WorldSlice -> WorldSlice) -> WorldGenConfig -> WorldGenConfig
updateWorldSlice updateField cfg =
  cfg { worldSlice = updateField (worldSlice cfg) }

updateWorldOceanCurrent :: (OceanCurrentConfig -> OceanCurrentConfig) -> WorldGenConfig -> WorldGenConfig
updateWorldOceanCurrent updateField cfg =
  cfg { worldOceanCurrent = updateField (worldOceanCurrent cfg) }

updateWorldBiomeFeedback :: (BiomeFeedbackConfig -> BiomeFeedbackConfig) -> WorldGenConfig -> WorldGenConfig
updateWorldBiomeFeedback updateField cfg =
  cfg { worldBiomeFeedback = updateField (worldBiomeFeedback cfg) }

readSnapshotSource :: SliderId -> SnapshotContext -> SnapshotSource -> Float
readSnapshotSource sliderIdValue snapshotContext snapshotSource =
  case snapshotSource of
    SnapshotFloat readFloat -> sliderFromDomainFloat sliderIdValue (readFloat snapshotContext)
    SnapshotInt readInt -> sliderFromDomainInt sliderIdValue (readInt snapshotContext)
    SnapshotBool readBool -> sliderFromDomainBool sliderIdValue (readBool snapshotContext)

mkSnapshotContext :: WorldGenConfig -> SnapshotContext
mkSnapshotContext cfg =
  let terrain = worldTerrain cfg
      gen = terrainGen terrain
      climate = worldClimate cfg
      biome = worldBiome cfg
  in SnapshotContext
      { scGen = gen
      , scEdge = gcOceanEdgeDepth gen
      , scExtent = gcWorldExtent gen
      , scTectonics = terrainTectonics terrain
      , scParameters = terrainParameters terrain
      , scForm = terrainFormConfig terrain
      , scHydrology = terrainHydrology terrain
      , scErosion = terrainErosion terrain
      , scHypsometry = terrainHypsometry terrain
      , scGlacier = terrainGlacier terrain
      , scVolcanism = terrainVolcanism terrain
      , scSoil = terrainSoil terrain
      , scWaterBody = terrainWaterBody terrain
      , scVegetationBootstrap = terrainVegetation terrain
      , scTemperature = ccTemperature climate
      , scWind = ccWind climate
      , scMoisture = ccMoisture climate
      , scPrecipitation = ccPrecipitation climate
      , scBoundary = ccBoundary climate
      , scWeather = worldWeather cfg
      , scBiome = biome
      , scBiomeVegetation = bcVegetation biome
      , scBiomeThresholds = bcThresholds biome
      , scBiomeFeedback = worldBiomeFeedback cfg
      , scPlanet = worldPlanet cfg
      , scOceanCurrent = worldOceanCurrent cfg
      , scWorldSlice = worldSlice cfg
      }

bindConfigAndSnapshot :: (SliderId, SliderConfigUpdate) -> (SliderId, SnapshotSource) -> (SliderId, SliderBinding)
bindConfigAndSnapshot (sliderIdValue, configUpdate) (_, snapshotSource) =
  ( sliderIdValue
  , SliderBinding
      { sbConfigUpdate = Just configUpdate
      , sbSnapshotSource = Just snapshotSource
      }
  )

bindSnapshotOnly :: (SliderId, SnapshotSource) -> (SliderId, SliderBinding)
bindSnapshotOnly (sliderIdValue, snapshotSource) =
  ( sliderIdValue
  , SliderBinding
      { sbConfigUpdate = Nothing
      , sbSnapshotSource = Just snapshotSource
      }
  )

snapshotFloat :: SliderId -> (SnapshotContext -> Float) -> (SliderId, SnapshotSource)
snapshotFloat sliderIdValue readFloat = (sliderIdValue, SnapshotFloat readFloat)

snapshotInt :: SliderId -> (SnapshotContext -> Int) -> (SliderId, SnapshotSource)
snapshotInt sliderIdValue readInt = (sliderIdValue, SnapshotInt readInt)

snapshotBool :: SliderId -> (SnapshotContext -> Bool) -> (SliderId, SnapshotSource)
snapshotBool sliderIdValue readBool = (sliderIdValue, SnapshotBool readBool)

-- ---------------------------------------------------------------------------
-- Temperature normalisation helpers
-- ---------------------------------------------------------------------------

-- | Slider temperature domain \([-50, +50]\) °C to config normalised \([0, 1]\).
tempCelsiusToNorm :: Float -> Float
tempCelsiusToNorm c = (c + 50) / 100
{-# INLINE tempCelsiusToNorm #-}

-- | Config normalised \([0, 1]\) to slider temperature domain \([-50, +50]\) °C.
tempNormToCelsius :: Float -> Float
tempNormToCelsius n = n * 100 - 50
{-# INLINE tempNormToCelsius #-}