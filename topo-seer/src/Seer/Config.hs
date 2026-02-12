{-# LANGUAGE OverloadedStrings #-}

module Seer.Config
  ( applyUiConfig
  , configSummary
  , mapRange
  , mapIntRange
  ) where

import Actor.UI (UiState(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth(..))
import Topo.Biome (BiomeThresholds(..), VegetationConfig(..))
import Topo.BiomeConfig (BiomeConfig(..))
import Topo.Climate (ClimateConfig(..), TemperatureConfig(..), WindConfig(..), MoistureConfig(..), PrecipitationConfig(..), BoundaryConfig(..))
import Topo.Erosion (ErosionConfig(..))
import Topo.Glacier (GlacierConfig(..))
import Topo.Hydrology (HydroConfig(..))
import Topo.Parameters (ParameterConfig(..), TerrainFormConfig(..))
import Topo.OceanCurrent (OceanCurrentConfig(..), defaultOceanCurrentConfig)
import Topo.Soil (SoilConfig(..))
import Topo.Volcanism (VolcanismConfig(..))
import Topo.WaterBody (WaterBodyConfig(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice, hexesPerDegreeLatitude, hexesPerDegreeLongitude)
import Topo.Tectonics (TectonicsConfig(..))
import Topo.Vegetation (VegetationBootstrapConfig(..), BiomeFeedbackConfig(..))
import Topo.Weather (WeatherConfig(..))
import Topo.WorldGen (TerrainConfig(..), WorldGenConfig(..))
import Topo.Types (worldExtentOrDefault)

applyUiConfig :: UiState -> WorldGenConfig -> WorldGenConfig
applyUiConfig ui cfg =
  let terrain = worldTerrain cfg
      gen = terrainGen terrain
      extentX = mapIntRange 0 16 (uiWorldExtentX ui)
      extentY = mapIntRange 0 16 (uiWorldExtentY ui)
      edgeDepth = OceanEdgeDepth
        { oedNorth = mapRange 0 2 (uiEdgeDepthNorth ui)
        , oedSouth = mapRange 0 2 (uiEdgeDepthSouth ui)
        , oedEast = mapRange 0 2 (uiEdgeDepthEast ui)
        , oedWest = mapRange 0 2 (uiEdgeDepthWest ui)
        , oedFalloff = mapRange 0 512 (uiEdgeDepthFalloff ui)
        }
      gen' = gen
        { gcScale = mapRange 0.2 2.0 (uiGenScale ui)
        , gcCoordScale = mapRange 0.5 2.0 (uiGenCoordScale ui)
        , gcOffsetX = mapRange (-10000) 10000 (uiGenOffsetX ui)
        , gcOffsetY = mapRange (-10000) 10000 (uiGenOffsetY ui)
        , gcFrequency = mapRange 0.001 0.05 (uiGenFrequency ui)
        , gcOctaves = mapIntRange 2 8 (uiGenOctaves ui)
        , gcLacunarity = mapRange 1.5 3.5 (uiGenLacunarity ui)
        , gcGain = mapRange 0.3 0.8 (uiGenGain ui)
        , gcWarpScale = mapRange 0.005 0.08 (uiGenWarpScale ui)
        , gcWarpStrength = mapRange 2 20 (uiGenWarpStrength ui)
        , gcWorldExtent = worldExtentOrDefault extentX extentY
        , gcOceanEdgeDepth = edgeDepth
        }
      tectonics = terrainTectonics terrain
      tectonics' = tectonics
        { tcPlateSize = mapIntRange 16 128 (uiPlateSize ui)
        , tcPlateSpeed = mapRange 0.1 1.5 (uiPlateSpeed ui)
        , tcBoundarySharpness = mapRange 0.5 2.5 (uiBoundarySharpness ui)
        , tcBoundaryNoiseScale = mapRange 0.002 0.02 (uiBoundaryNoiseScale ui)
        , tcBoundaryNoiseStrength = mapRange 2 24 (uiBoundaryNoiseStrength ui)
        , tcBoundaryWarpOctaves = mapIntRange 1 5 (uiBoundaryWarpOctaves ui)
        , tcBoundaryWarpLacunarity = mapRange 1.5 3.5 (uiBoundaryWarpLacunarity ui)
        , tcBoundaryWarpGain = mapRange 0.3 0.8 (uiBoundaryWarpGain ui)
        , tcPlateMergeScale = mapRange 0.05 0.25 (uiPlateMergeScale ui)
        , tcPlateMergeBias = mapRange 0.3 0.8 (uiPlateMergeBias ui)
        , tcPlateDetailScale = mapRange 0.005 0.05 (uiPlateDetailScale ui)
        , tcPlateDetailStrength = mapRange 0 1 (uiPlateDetailStrength ui)
        , tcPlateRidgeStrength = mapRange 0 1 (uiPlateRidgeStrength ui)
        , tcPlateHeightBase = mapRange (-0.1) 0.35 (uiPlateHeightBase ui)
        , tcPlateHeightVariance = mapRange 0.2 1.2 (uiPlateHeightVariance ui)
        , tcPlateHardnessBase = mapRange 0.2 0.8 (uiPlateHardnessBase ui)
        , tcPlateHardnessVariance = mapRange 0.1 0.6 (uiPlateHardnessVariance ui)
        , tcPlateBiasStrength = mapRange 0 0.6 (uiPlateBiasStrength ui)
        , tcPlateBiasCenter = mapRange (-1) 1 (uiPlateBiasCenter ui)
        , tcPlateBiasEdge = mapRange (-1) 1 (uiPlateBiasEdge ui)
        , tcPlateBiasNorth = mapRange (-1) 1 (uiPlateBiasNorth ui)
        , tcPlateBiasSouth = mapRange (-1) 1 (uiPlateBiasSouth ui)
        , tcUplift = mapRange 0.05 0.4 (uiUplift ui)
        , tcRiftDepth = mapRange 0.05 0.6 (uiRiftDepth ui)
        , tcTrenchDepth = mapRange 0.1 0.5 (uiTrenchDepth ui)
        , tcRidgeHeight = mapRange 0.02 0.2 (uiRidgeHeight ui)
        }
      params = terrainParameters terrain
      params' = params
        { pcDetailScale = mapRange 0.5 2.5 (uiDetailScale ui)
        , pcRoughnessScale = mapRange 0.0 2.0 (uiRoughnessScale ui)
        , pcRockElevationThreshold = mapRange 0.2 0.9 (uiRockElevationThreshold ui)
        , pcRockHardnessThreshold = mapRange 0.2 0.9 (uiRockHardnessThreshold ui)
        , pcRockHardnessSecondary = mapRange 0.1 0.8 (uiRockHardnessSecondary ui)
        }
      formCfg = terrainFormConfig terrain
      formCfg' = formCfg
        { tfcCliffSlope = mapRange 0.1 0.8 (uiTfcCliffSlope ui)
        , tfcMountainSlope = mapRange 0.05 0.5 (uiTfcMountainSlope ui)
        , tfcMountainRelief = mapRange 0.05 0.5 (uiTfcMountainRelief ui)
        , tfcHillSlope = mapRange 0.02 0.2 (uiTfcHillSlope ui)
        , tfcRollingSlope = mapRange 0.005 0.1 (uiTfcRollingSlope ui)
        , tfcValleyCurvature = mapRange 0.05 0.4 (uiValleyCurvature ui)
        }
      hydro = terrainHydrology terrain
      hydro' = hydro
        { hcWaterLevel = uiWaterLevel ui
        , hcSinkBreachDepth = mapRange 0.0 0.1 (uiSinkBreachDepth ui)
        , hcStreamPowerMaxErosion = mapRange 0.0 0.2 (uiStreamPowerMaxErosion ui)
        , hcRiverCarveMaxDepth = mapRange 0.0 0.2 (uiRiverCarveMaxDepth ui)
        , hcCoastalErodeStrength = mapRange 0.0 0.1 (uiCoastalErodeStrength ui)
        , hcHardnessErodeWeight = mapRange 0.0 1.0 (uiHydroHardnessWeight ui)
        }
      terrain' = terrain
        { terrainGen = gen'
        , terrainHydrology = hydro'
        , terrainTectonics = tectonics'
        , terrainParameters = params'
        , terrainFormConfig = formCfg'
        }
      erosion = terrainErosion terrain'
      erosion' = erosion
        { ecHydraulicIterations = mapIntRange 1 12 (uiErosionHydraulic ui)
        , ecThermalIterations = mapIntRange 1 12 (uiErosionThermal ui)
        , ecRainRate = mapRange 0.05 0.5 (uiRainRate ui)
        , ecThermalTalus = mapRange 0.1 1.0 (uiErosionTalus ui)
        , ecMaxDrop = mapRange 0.1 1.0 (uiErosionMaxDrop ui)
        }
      terrain'' = terrain'
        { terrainErosion = erosion'
        , terrainVegetation = vegBoot'
        , terrainGlacier = glacier'
        , terrainVolcanism = volcanism'
        , terrainSoil = soil'
        , terrainWaterBody = waterBody'
        }
      glacier = terrainGlacier terrain
      glacier' = glacier
        { gcSnowTemp = mapRange 0.0 0.5 (uiGlacierSnowTemp ui)
        , gcSnowRange = mapRange 0.1 0.7 (uiGlacierSnowRange ui)
        , gcMeltTemp = mapRange 0.1 0.8 (uiGlacierMeltTemp ui)
        , gcMeltRate = mapRange 0.0 1.0 (uiGlacierMeltRate ui)
        , gcAccumScale = mapRange 0.0 3.0 (uiGlacierAccumScale ui)
        , gcFlowIterations = mapIntRange 0 10 (uiGlacierFlowIters ui)
        , gcFlowRate = mapRange 0.0 1.0 (uiGlacierFlowRate ui)
        , gcErosionScale = mapRange 0.0 1.0 (uiGlacierErosionScale ui)
        , gcCarveScale = mapRange 0.0 0.1 (uiGlacierCarveScale ui)
        , gcDepositScale = mapRange 0.0 1.0 (uiGlacierDepositScale ui)
        }
      volcanism = terrainVolcanism terrain
      volcanism' = volcanism
        { vcVentDensityBase = mapRange 0.0 0.2 (uiVentDensity ui)
        , vcVentThreshold = mapRange 0.2 0.9 (uiVentThreshold ui)
        , vcHotspotScale = mapRange 0.0 1.0 (uiHotspotScale ui)
        , vcHotspotThreshold = mapRange 0.3 0.95 (uiHotspotThreshold ui)
        , vcMagmaRecharge = mapRange 0.0 3.0 (uiMagmaRecharge ui)
        , vcLavaScale = mapRange 0.0 1.0 (uiLavaScale ui)
        , vcAshScale = mapRange 0.0 1.0 (uiAshScale ui)
        , vcDepositScale = mapRange 0.0 1.0 (uiVolcanicDepositScale ui)
        }
      soil = terrainSoil terrain
      soil' = soil
        { scMoistureThreshold = mapRange 0.0 1.0 (uiSoilMoistureThreshold ui)
        , scHardnessThreshold = mapRange 0.0 1.0 (uiSoilHardnessThreshold ui)
        , scFertilityMoistWeight = mapRange 0.0 1.0 (uiSoilFertilityMoistWeight ui)
        , scFertilityDepthWeight = mapRange 0.0 1.0 (uiSoilFertilityDepthWeight ui)
        }
      waterBody = terrainWaterBody terrain
      waterBody' = waterBody
        { wbcMinLakeSize = mapIntRange 1 50 (uiMinLakeSize ui)
        , wbcInlandSeaMinSize = mapIntRange 50 500 (uiInlandSeaMinSize ui)
        }
      vegBoot = terrainVegetation terrain
      vegBoot' = vegBoot
        { vbcTempMin = mapRange 0 0.3 (uiVbcTempMin ui)
        , vbcTempRange = mapRange 0.1 1.0 (uiVbcTempRange ui)
        , vbcFertilityBoost = mapRange 0 1 (uiVbcFertilityBoost ui)
        , vbcAlbedoBase = mapRange 0 0.3 (uiVbcAlbedoBase ui)
        , vbcAlbedoBare = mapRange 0.1 0.5 (uiVbcAlbedoBare ui)
        , vbcAlbedoVeg = mapRange 0 0.3 (uiVbcAlbedoVeg ui)
        , vbcOceanAlbedo = mapRange 0 0.2 (uiVbcOceanAlbedo ui)
        , vbcIceAlbedo = mapRange 0.3 1.0 (uiVbcIceAlbedo ui)
        }
      climate = worldClimate cfg
      climateTmp = ccTemperature climate
      climateWnd = ccWind climate
      climateMst = ccMoisture climate
      climatePrc = ccPrecipitation climate
      climateBnd = ccBoundary climate
      climate' = climate
        { ccTemperature = climateTmp
            { tmpEquatorTemp = uiEquatorTemp ui
            , tmpPoleTemp = uiPoleTemp ui
            , tmpLapseRate = uiLapseRate ui
            , tmpLatitudeExponent = mapRange 0.2 1.5 (uiLatitudeExponent ui)
            , tmpPlateHeightCooling = mapRange 0 0.2 (uiPlateHeightCooling ui)
            , tmpNoiseScale = mapRange 0 0.3 (uiTempNoiseScale ui)
            , tmpOceanModeration = uiOceanModeration ui
            , tmpOceanModerateTemp = mapRange (-0.1) 0.1 (uiOceanModerateTemp ui)
            , tmpAlbedoSensitivity = uiAlbedoSensitivity ui
            , tmpAlbedoReference = mapRange 0 0.5 (uiAlbedoReference ui)
            }
        , ccWind = climateWnd
            { windDiffuse = uiWindDiffuse ui
            , windIterations = mapIntRange 1 8 (uiWindIterations ui)
            , windBeltStrength = uiWindBeltStrength ui
            , windBeltHarmonics = mapRange 1 6 (uiWindBeltHarmonics ui)
            , windBeltBase = uiWindBeltBase ui
            , windBeltRange = uiWindBeltRange ui
            , windBeltSpeedScale = uiWindBeltSpeedScale ui
            }
        , ccMoisture = climateMst
            { moistEvapCoeff = uiEvaporation ui
            , moistIterations = mapIntRange 2 72 (uiMoistureIterations ui)
            , moistAdvect = uiMoistAdvect ui
            , moistLocal = uiMoistLocal ui
            , moistWindEvapScale = uiMoistWindEvapScale ui
            , moistEvapNoiseScale = mapRange 0 0.2 (uiMoistEvapNoiseScale ui)
            , moistLandETCoeff = uiMoistLandETCoeff ui
            , moistBareEvapFrac = uiMoistBareEvapFrac ui
            , moistVegTranspFrac = uiMoistVegTranspFrac ui
            , moistWindETScale = uiMoistWindETScale ui
            , moistCondensationRate = uiMoistCondensationRate ui
            , moistRecycleRate = uiMoistRecycleRate ui
            , moistITCZStrength = mapRange 0 0.5 (uiMoistITCZStrength ui)
            , moistITCZWidth = mapRange 2 20 (uiMoistITCZWidth ui)
            }
        , ccPrecipitation = climatePrc
            { precRainShadow = uiRainShadow ui
            , precOrographicScale = mapRange 0 2 (uiOrographicScale ui)
            , precOrographicStep = mapRange 0.5 3 (uiOrographicStep ui)
            , precCoastalIterations = mapIntRange 0 16 (uiCoastalIterations ui)
            , precCoastalDiffuse = uiCoastalDiffuse ui
            , precCoastalMoistureBoost = mapRange 0 0.5 (uiCoastalMoistureBoost ui)
            }
        , ccBoundary = climateBnd
            { bndMotionTemp = mapRange 0 2 (uiBoundaryMotionTemp ui)
            , bndMotionPrecip = mapRange 0 2 (uiBoundaryMotionPrecip ui)
            , bndLandRange = mapRange 0.1 1.5 (uiBndLandRange ui)
            , bndTempConvergent = mapRange (-0.2) 0.1 (uiBndTempConvergent ui)
            , bndTempDivergent = mapRange (-0.1) 0.2 (uiBndTempDivergent ui)
            , bndTempTransform = mapRange (-0.1) 0.1 (uiBndTempTransform ui)
            , bndPrecipConvergent = mapRange (-0.1) 0.2 (uiBndPrecipConvergent ui)
            , bndPrecipDivergent = mapRange (-0.2) 0.1 (uiBndPrecipDivergent ui)
            , bndPrecipTransform = mapRange (-0.1) 0.1 (uiBndPrecipTransform ui)
            }
        }
      weather = worldWeather cfg
      weather' = weather
        { wcTickSeconds = mapRange 0.1 5 (uiWeatherTick ui)
        , wcSeasonPhase = mapRange 0 1 (uiWeatherPhase ui)
        , wcSeasonAmplitude = mapRange 0 0.5 (uiWeatherAmplitude ui)
        , wcSeasonCycleLength = mapRange 30 730 (uiSeasonCycleLength ui)
        , wcJitterAmplitude = mapRange 0 0.5 (uiJitterAmplitude ui)
        , wcPressureBase = mapRange 0.3 1.0 (uiPressureBase ui)
        , wcPressureTempScale = mapRange 0 1 (uiPressureTempScale ui)
        , wcPressureCoriolisScale = mapRange 0 0.5 (uiPressureCoriolisScale ui)
        , wcSeasonalBase = mapRange 0 1 (uiSeasonalBase ui)
        , wcSeasonalRange = mapRange 0 2 (uiSeasonalRange ui)
        , wcHumidityNoiseScale = mapRange 0 0.3 (uiHumidityNoiseScale ui)
        , wcPrecipNoiseScale = mapRange 0 0.5 (uiPrecipNoiseScale ui)
        , wcITCZWidth = mapRange 2 20 (uiWeatherITCZWidth ui)
        , wcITCZPrecipBoost = mapRange 0 1 (uiWeatherITCZPrecipBoost ui)
        , wcPressureHumidityScale = mapRange 0 0.5 (uiPressureHumidityScale ui)
        , wcPressureGradientWindScale = mapRange 0 1 (uiPressureGradientWindScale ui)
        , wcWindNoiseScale = mapRange 0 0.3 (uiWindNoiseScale ui)
        , wcITCZMigrationScale = mapRange 0 1.5 (uiITCZMigrationScale ui)
        , wcCloudRHExponent = mapRange 0.5 3.0 (uiCloudRHExponent ui)
        , wcCloudAlbedoEffect = mapRange 0 0.3 (uiCloudAlbedoEffect ui)
        , wcCloudPrecipBoost = mapRange 0 0.5 (uiCloudPrecipBoost ui)
        }
      biome = worldBiome cfg
      veg = bcVegetation biome
      veg' = veg
        { vcBaseDensity = mapRange 0.02 0.6 (uiVegBase ui)
        , vcBiomeBoost = mapRange 0.1 1.0 (uiVegBoost ui)
        , vcTempWeight = mapRange 0 1 (uiVegTempWeight ui)
        , vcPrecipWeight = mapRange 0 1 (uiVegPrecipWeight ui)
        }
      biome' = biome
        { bcVegetation = veg'
        , bcThresholds = thresh'
        , bcSmoothingIterations = round (mapRange 0 5 (uiBiomeSmoothing ui))
        , bcVolcanicAshBoost = mapRange 0 0.5 (uiVolcanicAshBoost ui)
        , bcVolcanicLavaPenalty = mapRange 0 0.8 (uiVolcanicLavaPenalty ui)
        }
      thresh = bcThresholds biome
      thresh' = thresh
        { btCoastalBand = mapRange 0 0.1 (uiBtCoastalBand ui)
        , btSnowElevation = mapRange 0.5 1.0 (uiBtSnowElevation ui)
        , btAlpineElevation = mapRange 0.4 0.9 (uiBtAlpineElevation ui)
        , btIceCapTemp = mapRange 0 0.2 (uiBtIceCapTemp ui)
        , btMontaneLow = mapRange 0.3 0.8 (uiBtMontaneLow ui)
        , btMontanePrecip = mapRange 0.1 0.6 (uiBtMontanePrecip ui)
        , btCliffSlope = mapRange 0.2 0.8 (uiBtCliffSlope ui)
        , btValleyMoisture = mapRange 0.3 1.0 (uiBtValleyMoisture ui)
        , btDepressionMoisture = mapRange 0.2 1.0 (uiBtDepressionMoisture ui)
        , btPrecipWeight = mapRange 0.5 5.0 (uiBtPrecipWeight ui)
        }
      biomeFeedback = worldBiomeFeedback cfg
      biomeFeedback' = biomeFeedback
        { bfcBlendWeight = mapRange 0 1 (uiBiomeFeedbackBlend ui)
        }
      -- Planet / slice
      planetRadius = mapRange 4778 9557 (uiPlanetRadius ui)
      axialTilt = mapRange 0 45 (uiAxialTilt ui)
      insolation = mapRange 0.7 1.3 (uiInsolation ui)
      planet = defaultPlanetConfig
        { pcRadius = planetRadius
        , pcAxialTilt = axialTilt
        , pcInsolation = insolation
        }
      oceanCurrent = defaultOceanCurrentConfig
        { occWarmScale = mapRange 0 0.2 (uiOccWarmScale ui)
        , occColdScale = mapRange 0 0.2 (uiOccColdScale ui)
        , occLatPeakDeg = mapRange 0 60 (uiOccLatPeakDeg ui)
        , occLatWidthDeg = mapRange 5 45 (uiOccLatWidthDeg ui)
        }
      sliceLatCenter = mapRange (-90) 90 (uiSliceLatCenter ui)
      sliceLonCenter = mapRange (-180) 180 (uiSliceLonCenter ui)
      -- Derive lat/lon extent from chunk radii + planet radius
      hpdLat = hexesPerDegreeLatitude planet
      hpdLon = hexesPerDegreeLongitude planet sliceLatCenter
      cs = max 1 (uiChunkSize ui)
      sliceLatExtent = max 0.1 (fromIntegral (extentY * 2 * cs) / hpdLat)
      sliceLonExtent = max 0.1 (fromIntegral (extentX * 2 * cs) / hpdLon)
      slice = defaultWorldSlice
        { wsLatCenter = sliceLatCenter
        , wsLatExtent = sliceLatExtent
        , wsLonCenter = sliceLonCenter
        , wsLonExtent = sliceLonExtent
        }
  in cfg { worldTerrain = terrain'', worldClimate = climate', worldWeather = weather', worldBiome = biome', worldBiomeFeedback = biomeFeedback', worldPlanet = planet, worldOceanCurrent = oceanCurrent, worldSlice = slice }

-- | Produce a diagnostic summary of all slider fields for logging.
configSummary :: UiState -> Text
configSummary ui =
  Text.intercalate " | "
    [ "Config seed=" <> Text.pack (show (uiSeed ui)) <> " chunk=" <> Text.pack (show (uiChunkSize ui))
    , kv "water" uiWaterLevel <> " " <> kv "evap" uiEvaporation <> " " <> kv "rainShadow" uiRainShadow
    ]
  where
    kv :: Show a => Text -> (UiState -> a) -> Text
    kv label f = label <> "=" <> Text.pack (show (f ui))

mapRange :: Float -> Float -> Float -> Float
mapRange lo hi t =
  let t' = max 0 (min 1 t)
  in lo + (hi - lo) * t'

mapIntRange :: Int -> Int -> Float -> Int
mapIntRange lo hi t =
  round (mapRange (fromIntegral lo) (fromIntegral hi) t)
