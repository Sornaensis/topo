module Main (main) where

import System.IO (hSetEncoding, stdout, stderr, utf8)
import Test.Hspec
import qualified Spec.World
import qualified Spec.Hex
import qualified Spec.HexDirection
import qualified Spec.Metadata
import qualified Spec.Pipeline
import qualified Spec.Calendar
import qualified Spec.Overlay
import qualified Spec.OverlayCache
import qualified Spec.Simulation
import qualified Spec.Storage
import qualified Spec.Mesh
import qualified Spec.Export
import qualified Spec.Provenance
import qualified Spec.Determinism
import qualified Spec.Tectonics
import qualified Spec.Erosion
import qualified Spec.Glacier
import qualified Spec.Volcanism
import qualified Spec.Climate
import qualified Spec.Diffusion
import qualified Spec.Biome
import qualified Spec.BiomeRefine
import qualified Spec.Vegetation
import qualified Spec.Weather
import qualified Spec.Noise
import qualified Spec.Hydrology
import qualified Spec.River
import qualified Spec.BaseHeight
import qualified Spec.Planet
import qualified Spec.Parameters
import qualified Spec.Soil
import qualified Spec.Evaporation
import qualified Spec.WaterBody
import qualified Spec.WaterTable
import qualified Spec.Integration
import qualified Spec.OceanCurrent
import qualified Spec.ConfigJSON
import qualified Spec.ConfigProperty
import qualified Spec.Units
import qualified Spec.UnitsConfig
import qualified Spec.Precipitation
import qualified Spec.MoistureFixture
import qualified Spec.ClimateAlignment
import qualified Spec.TerrainRelief
import qualified Spec.TerrainFormMetrics
import qualified Spec.TerrainFormCalibration
import qualified Spec.DirectionalSlope
import qualified Spec.TerrainFormModifiers
import qualified Spec.PluginRPC
import qualified Spec.PluginIntegration
import qualified Spec.DataResource
import qualified Spec.DataService
import qualified Spec.WorldBundle
import qualified Spec.CommandTypes
import qualified WorldProperty
import qualified HexProperty

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec $ do
    Spec.World.spec
    Spec.Hex.spec
    Spec.HexDirection.spec
    Spec.Metadata.spec
    Spec.Pipeline.spec
    Spec.Calendar.spec
    Spec.Overlay.spec
    Spec.OverlayCache.spec
    Spec.Simulation.spec
    Spec.Storage.spec
    Spec.Mesh.spec
    Spec.Export.spec
    Spec.Provenance.spec
    Spec.Determinism.spec
    Spec.Tectonics.spec
    Spec.Erosion.spec
    Spec.Glacier.spec
    Spec.Volcanism.spec
    Spec.Climate.spec
    Spec.Diffusion.spec
    Spec.Biome.spec
    Spec.BiomeRefine.spec
    Spec.Vegetation.spec
    Spec.Weather.spec
    Spec.Noise.spec
    Spec.Hydrology.spec
    Spec.River.spec
    Spec.BaseHeight.spec
    Spec.Planet.spec
    Spec.Parameters.spec
    Spec.Soil.spec
    Spec.Evaporation.spec
    Spec.WaterBody.spec
    Spec.WaterTable.spec
    Spec.Integration.spec
    Spec.OceanCurrent.spec
    Spec.ConfigJSON.spec
    Spec.ConfigProperty.spec
    Spec.Units.spec
    Spec.UnitsConfig.spec
    Spec.Precipitation.spec
    Spec.MoistureFixture.spec
    Spec.ClimateAlignment.spec
    Spec.TerrainRelief.spec
    Spec.TerrainFormMetrics.spec
    Spec.TerrainFormCalibration.spec
    Spec.DirectionalSlope.spec
    Spec.TerrainFormModifiers.spec
    Spec.PluginRPC.spec
    Spec.PluginIntegration.spec
    Spec.DataResource.spec
    Spec.DataService.spec
    Spec.WorldBundle.spec
    Spec.CommandTypes.spec
    WorldProperty.spec
    HexProperty.spec
