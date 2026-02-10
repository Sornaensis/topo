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
import Topo.Biome (VegetationConfig(..))
import Topo.BiomeConfig (BiomeConfig(..))
import Topo.Climate (ClimateConfig(..))
import Topo.Erosion (ErosionConfig(..))
import Topo.Hydrology (HydroConfig(..))
import Topo.Parameters (ParameterConfig(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Tectonics (TectonicsConfig(..))
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
      hydro = terrainHydrology terrain
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
        }
      terrain' = terrain
        { terrainGen = gen'
        , terrainHydrology = hydro { hcWaterLevel = uiWaterLevel ui }
        , terrainTectonics = tectonics'
        , terrainParameters = params'
        }
      erosion = terrainErosion terrain'
      erosion' = erosion
        { ecHydraulicIterations = mapIntRange 1 12 (uiErosionHydraulic ui)
        , ecThermalIterations = mapIntRange 1 12 (uiErosionThermal ui)
        , ecRainRate = mapRange 0.05 0.5 (uiRainRate ui)
        , ecThermalTalus = mapRange 0.1 1.0 (uiErosionTalus ui)
        , ecMaxDrop = mapRange 0.1 1.0 (uiErosionMaxDrop ui)
        }
      terrain'' = terrain' { terrainErosion = erosion' }
      climate = worldClimate cfg
      climate' = climate
        { ccEvaporation = uiEvaporation ui
        , ccRainShadow = uiRainShadow ui
        , ccWindDiffuse = uiWindDiffuse ui
        , ccEquatorTemp = uiEquatorTemp ui
        , ccPoleTemp = uiPoleTemp ui
        , ccLapseRate = uiLapseRate ui
        , ccLatitudeBias = mapRange (-1) 1 (uiLatitudeBias ui)
        , ccWindIterations = mapIntRange 1 8 (uiWindIterations ui)
        , ccMoistureIterations = mapIntRange 2 12 (uiMoistureIterations ui)
        , ccBoundaryMotionTemp = mapRange 0 2 (uiBoundaryMotionTemp ui)
        , ccBoundaryMotionPrecip = mapRange 0 2 (uiBoundaryMotionPrecip ui)
        }
      weather = worldWeather cfg
      weather' = weather
        { wcTickSeconds = mapRange 0.1 5 (uiWeatherTick ui)
        , wcSeasonPhase = mapRange 0 1 (uiWeatherPhase ui)
        , wcSeasonAmplitude = mapRange 0 0.5 (uiWeatherAmplitude ui)
        }
      biome = worldBiome cfg
      veg = bcVegetation biome
      veg' = veg
        { vcBaseDensity = mapRange 0.02 0.6 (uiVegBase ui)
        , vcBiomeBoost = mapRange 0.1 1.0 (uiVegBoost ui)
        , vcTempWeight = mapRange 0 1 (uiVegTempWeight ui)
        , vcPrecipWeight = mapRange 0 1 (uiVegPrecipWeight ui)
        }
      biome' = biome { bcVegetation = veg' }
      -- Planet / slice
      planetRadius = mapRange 4778 9557 (uiPlanetRadius ui)
      axialTilt = mapRange 0 45 (uiAxialTilt ui)
      insolation = mapRange 0.7 1.3 (uiInsolation ui)
      planet = defaultPlanetConfig
        { pcRadius = planetRadius
        , pcAxialTilt = axialTilt
        , pcInsolation = insolation
        }
      sliceLatCenter = mapRange (-90) 90 (uiSliceLatCenter ui)
      sliceLatExtent = mapRange 0.1 180 (uiSliceLatExtent ui)
      sliceLonCenter = mapRange (-180) 180 (uiSliceLonCenter ui)
      sliceLonExtent = mapRange 0.1 360 (uiSliceLonExtent ui)
      slice = defaultWorldSlice
        { wsLatCenter = sliceLatCenter
        , wsLatExtent = sliceLatExtent
        , wsLonCenter = sliceLonCenter
        , wsLonExtent = sliceLonExtent
        }
  in cfg { worldTerrain = terrain'', worldClimate = climate', worldWeather = weather', worldBiome = biome', worldPlanet = planet, worldSlice = slice }

configSummary :: UiState -> Text
configSummary ui =
  Text.unwords
    [ "Config"
    , "seed=" <> Text.pack (show (uiSeed ui))
    , "chunk=" <> Text.pack (show (uiChunkSize ui))
    , "water=" <> Text.pack (show (uiWaterLevel ui))
    , "evap=" <> Text.pack (show (uiEvaporation ui))
    , "rainShadow=" <> Text.pack (show (uiRainShadow ui))
    , "windDiffuse=" <> Text.pack (show (uiWindDiffuse ui))
    , "erosionRain=" <> Text.pack (show (uiRainRate ui))
    , "erosionHyd=" <> Text.pack (show (uiErosionHydraulic ui))
    , "erosionTherm=" <> Text.pack (show (uiErosionThermal ui))
    , "erosionTalus=" <> Text.pack (show (uiErosionTalus ui))
    , "erosionDrop=" <> Text.pack (show (uiErosionMaxDrop ui))
    , "eqTemp=" <> Text.pack (show (uiEquatorTemp ui))
    , "poleTemp=" <> Text.pack (show (uiPoleTemp ui))
    , "lapse=" <> Text.pack (show (uiLapseRate ui))
    , "genScale=" <> Text.pack (show (uiGenScale ui))
    , "genCoordScale=" <> Text.pack (show (uiGenCoordScale ui))
    , "genOffsetX=" <> Text.pack (show (uiGenOffsetX ui))
    , "genOffsetY=" <> Text.pack (show (uiGenOffsetY ui))
    , "genFreq=" <> Text.pack (show (uiGenFrequency ui))
    , "genOct=" <> Text.pack (show (uiGenOctaves ui))
    , "genLac=" <> Text.pack (show (uiGenLacunarity ui))
    , "genGain=" <> Text.pack (show (uiGenGain ui))
    , "genWarpScale=" <> Text.pack (show (uiGenWarpScale ui))
    , "genWarpStrength=" <> Text.pack (show (uiGenWarpStrength ui))
    , "extentX=" <> Text.pack (show (mapIntRange 0 16 (uiWorldExtentX ui)))
    , "extentY=" <> Text.pack (show (mapIntRange 0 16 (uiWorldExtentY ui)))
    , "edgeNorth=" <> Text.pack (show (mapRange 0 2 (uiEdgeDepthNorth ui)))
    , "edgeSouth=" <> Text.pack (show (mapRange 0 2 (uiEdgeDepthSouth ui)))
    , "edgeEast=" <> Text.pack (show (mapRange 0 2 (uiEdgeDepthEast ui)))
    , "edgeWest=" <> Text.pack (show (mapRange 0 2 (uiEdgeDepthWest ui)))
    , "edgeFalloff=" <> Text.pack (show (mapRange 0 512 (uiEdgeDepthFalloff ui)))
    , "plateSize=" <> Text.pack (show (uiPlateSize ui))
    , "plateSpeed=" <> Text.pack (show (uiPlateSpeed ui))
    , "boundarySharp=" <> Text.pack (show (uiBoundarySharpness ui))
    , "boundaryScale=" <> Text.pack (show (uiBoundaryNoiseScale ui))
    , "boundaryStrength=" <> Text.pack (show (uiBoundaryNoiseStrength ui))
    , "warpOctaves=" <> Text.pack (show (uiBoundaryWarpOctaves ui))
    , "warpLac=" <> Text.pack (show (uiBoundaryWarpLacunarity ui))
    , "warpGain=" <> Text.pack (show (uiBoundaryWarpGain ui))
    , "mergeScale=" <> Text.pack (show (uiPlateMergeScale ui))
    , "mergeBias=" <> Text.pack (show (uiPlateMergeBias ui))
    , "plateDetailScale=" <> Text.pack (show (uiPlateDetailScale ui))
    , "plateDetailStrength=" <> Text.pack (show (uiPlateDetailStrength ui))
    , "plateRidgeStrength=" <> Text.pack (show (uiPlateRidgeStrength ui))
    , "plateHeightBase=" <> Text.pack (show (uiPlateHeightBase ui))
    , "plateHeightVar=" <> Text.pack (show (uiPlateHeightVariance ui))
    , "plateHardBase=" <> Text.pack (show (uiPlateHardnessBase ui))
    , "plateHardVar=" <> Text.pack (show (uiPlateHardnessVariance ui))
    , "plateBiasStr=" <> Text.pack (show (uiPlateBiasStrength ui))
    , "plateBiasCenter=" <> Text.pack (show (uiPlateBiasCenter ui))
    , "plateBiasEdge=" <> Text.pack (show (uiPlateBiasEdge ui))
    , "plateBiasNorth=" <> Text.pack (show (uiPlateBiasNorth ui))
    , "plateBiasSouth=" <> Text.pack (show (uiPlateBiasSouth ui))
    , "uplift=" <> Text.pack (show (uiUplift ui))
    , "riftDepth=" <> Text.pack (show (uiRiftDepth ui))
    , "trenchDepth=" <> Text.pack (show (uiTrenchDepth ui))
    , "ridgeHeight=" <> Text.pack (show (uiRidgeHeight ui))
    , "detailScale=" <> Text.pack (show (uiDetailScale ui))
    , "latBias=" <> Text.pack (show (uiLatitudeBias ui))
    , "windIter=" <> Text.pack (show (uiWindIterations ui))
    , "moistIter=" <> Text.pack (show (uiMoistureIterations ui))
    , "motionTemp=" <> Text.pack (show (uiBoundaryMotionTemp ui))
    , "motionPrecip=" <> Text.pack (show (uiBoundaryMotionPrecip ui))
    , "weatherTick=" <> Text.pack (show (uiWeatherTick ui))
    , "weatherPhase=" <> Text.pack (show (uiWeatherPhase ui))
    , "weatherAmp=" <> Text.pack (show (uiWeatherAmplitude ui))
    , "vegBase=" <> Text.pack (show (uiVegBase ui))
    , "vegBoost=" <> Text.pack (show (uiVegBoost ui))
    , "vegTempW=" <> Text.pack (show (uiVegTempWeight ui))
    , "vegPrecipW=" <> Text.pack (show (uiVegPrecipWeight ui))
    , "planetR=" <> Text.pack (show (mapRange 4778 9557 (uiPlanetRadius ui)))
    , "tilt=" <> Text.pack (show (mapRange 0 45 (uiAxialTilt ui)))
    , "insol=" <> Text.pack (show (mapRange 0.7 1.3 (uiInsolation ui)))
    , "sliceLatC=" <> Text.pack (show (mapRange (-90) 90 (uiSliceLatCenter ui)))
    , "sliceLatE=" <> Text.pack (show (mapRange 0.1 180 (uiSliceLatExtent ui)))
    , "sliceLonC=" <> Text.pack (show (mapRange (-180) 180 (uiSliceLonCenter ui)))
    , "sliceLonE=" <> Text.pack (show (mapRange 0.1 360 (uiSliceLonExtent ui)))
    ]

mapRange :: Float -> Float -> Float -> Float
mapRange lo hi t =
  let t' = max 0 (min 1 t)
  in lo + (hi - lo) * t'

mapIntRange :: Int -> Int -> Float -> Int
mapIntRange lo hi t =
  round (mapRange (fromIntegral lo) (fromIntegral hi) t)
