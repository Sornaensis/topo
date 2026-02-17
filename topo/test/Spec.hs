module Main (main) where

import Test.Hspec
import qualified Spec.World
import qualified Spec.Hex
import qualified Spec.Metadata
import qualified Spec.Pipeline
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
import qualified Spec.Integration
import qualified Spec.OceanCurrent
import qualified Spec.ConfigJSON
import qualified Spec.ConfigProperty
import qualified Spec.Units
import qualified Spec.Precipitation
import qualified Spec.MoistureFixture
import qualified Spec.ClimateAlignment
import qualified WorldProperty
import qualified HexProperty

main :: IO ()
main = hspec $ do
  Spec.World.spec
  Spec.Hex.spec
  Spec.Metadata.spec
  Spec.Pipeline.spec
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
  Spec.Integration.spec
  Spec.OceanCurrent.spec
  Spec.ConfigJSON.spec
  Spec.ConfigProperty.spec
  Spec.Units.spec
  Spec.Precipitation.spec
  Spec.MoistureFixture.spec
  Spec.ClimateAlignment.spec
  WorldProperty.spec
  HexProperty.spec
