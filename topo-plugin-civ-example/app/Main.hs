{-# LANGUAGE OverloadedStrings #-}

-- | Example civilization overlay plugin for topo.
--
-- Demonstrates the full overlay + generator + simulation workflow:
--
-- * __Generator stage__: seeds initial population on habitable hexes
--   (inserted after biomes, depends on biomes and rivers).
-- * __Simulation node__: ticks population growth and infrastructure
--   each simulation step, reading weather data as a dependency.
-- * __Overlay schema__: defines per-hex fields (population, culture,
--   infrastructure, food supply, trade value, city flag).
--
-- To use: build and install the executable, then place it in
-- @~\/.topo\/plugins\/civilization\/@ alongside @civilization.toposchema@.
module Main (main) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Aeson (Value(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Topo.Overlay.Schema (OverlaySchema, parseOverlaySchema)
import Topo.Plugin.SDK

------------------------------------------------------------------------
-- Configuration constants (exposed as plugin parameters)
------------------------------------------------------------------------

-- | Default growth rate: fraction of population increase per tick.
defaultGrowthRate :: Double
defaultGrowthRate = 0.02

-- | Default infrastructure cost per tick per population unit.
defaultInfraCost :: Double
defaultInfraCost = 0.1

-- | Population threshold for a hex to become a city.
defaultCityThreshold :: Double
defaultCityThreshold = 1000.0

-- | Minimum biome habitability for initial population seeding.
defaultHabitabilityThreshold :: Double
defaultHabitabilityThreshold = 0.3

------------------------------------------------------------------------
-- Plugin definition
------------------------------------------------------------------------

-- | Civilization plugin: overlay + generator + simulation.
civPlugin :: PluginDef
civPlugin = defaultPluginDef
  { pdName       = "civilization"
  , pdVersion    = "1.0.0"
  , pdSchemaFile = Just "civilization.toposchema"
  , pdParams     =
      [ ParamDef
          { paramName    = "growth_rate"
          , paramLabel   = "Growth Rate"
          , paramType    = PFloat
          , paramDefault = Number (realToFrac defaultGrowthRate)
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 0.5)
          , paramTooltip = "Population growth fraction per tick"
          }
      , ParamDef
          { paramName    = "infra_cost"
          , paramLabel   = "Infrastructure Cost"
          , paramType    = PFloat
          , paramDefault = Number (realToFrac defaultInfraCost)
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 1.0)
          , paramTooltip = "Infrastructure construction cost per population"
          }
      , ParamDef
          { paramName    = "city_threshold"
          , paramLabel   = "City Threshold"
          , paramType    = PFloat
          , paramDefault = Number (realToFrac defaultCityThreshold)
          , paramMin     = Just (Number 100)
          , paramMax     = Just (Number 50000)
          , paramTooltip = "Minimum population for city classification"
          }
      , ParamDef
          { paramName    = "habitability_threshold"
          , paramLabel   = "Habitability Threshold"
          , paramType    = PFloat
          , paramDefault = Number (realToFrac defaultHabitabilityThreshold)
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 1.0)
          , paramTooltip = "Minimum biome habitability for initial seeding"
          }
      , ParamDef
          { paramName    = "enable_trade"
          , paramLabel   = "Trade Routes"
          , paramType    = PBool
          , paramDefault = Bool True
          , paramMin     = Nothing
          , paramMax     = Nothing
          , paramTooltip = "Enable trade value accumulation between cities"
          }
      ]

  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "biomes"
      , gdRequires    = ["biomes", "rivers"]
      , gdRun         = runCivGenerator
      }

  , pdSimulation = Just SimulationDef
      { sdDependencies = ["weather"]
      , sdTick         = runCivSimTick
      }
  }

------------------------------------------------------------------------
-- Generator: seed initial population
------------------------------------------------------------------------

-- | Seed initial population on habitable hexes.
--
-- Currently a stub that logs the invocation and parameters.
-- A full implementation would:
--
-- 1. Read terrain biome and elevation data from 'pcWorld'
-- 2. Identify hexes above the habitability threshold
-- 3. Seed an initial population scaled by biome richness
-- 4. Return the seeded overlay data to the host
runCivGenerator :: PluginContext -> IO (Either Text GeneratorTickResult)
runCivGenerator ctx = do
  pcLog ctx "civilization: generator — seeding initial population"
  pcLog ctx ("civilization: habitability threshold = "
              <> showParam (pcParams ctx) "habitability_threshold")
  pcLog ctx ("civilization: seed = " <> pack (show (pcSeed ctx)))
  case decodeTerrainPayload (pcTerrain ctx) of
    Left decodeErr ->
      pure (Left ("civilization: failed to decode terrain payload: " <> decodeErr))
    Right terrainWorld ->
      case generatorResultFromTerrain terrainWorld of
        Left encodeErr ->
          pure (Left ("civilization: failed to encode generator terrain payload: " <> encodeErr))
        Right result -> do
          pcLog ctx "civilization: generator complete"
          pure (Right result)

------------------------------------------------------------------------
-- Simulation: tick population growth
------------------------------------------------------------------------

-- | Tick civilization simulation.
--
-- Currently a stub that logs the invocation and parameters.
-- A full implementation would:
--
-- 1. Read the current civilization overlay
-- 2. Read the weather overlay (dependency) for climate effects
-- 3. Compute population growth based on food supply and climate
-- 4. Accumulate infrastructure based on population
-- 5. Promote hexes to cities above the threshold
-- 6. Optionally compute trade value if trade is enabled
-- 7. Return the updated overlay to the host
runCivSimTick :: PluginContext -> IO (Either Text SimulationTickResult)
runCivSimTick ctx = do
  pcLog ctx "civilization: simulation tick"
  let params = pcParams ctx
  pcLog ctx ("civilization: growth rate = " <> showParam params "growth_rate")
  pcLog ctx ("civilization: trade enabled = " <> showParam params "enable_trade")
  schemaResult <- loadCivilizationSchema
  case schemaResult of
    Left schemaErr ->
      pure (Left ("civilization: failed to load schema: " <> schemaErr))
    Right schema ->
      case decodeOwnOverlay schema ctx of
        Left decodeErr ->
          pure (Left ("civilization: failed to decode own overlay payload: " <> decodeErr))
        Right overlay -> do
          -- TODO: decode dependency overlays (e.g. weather), update
          -- overlay values, and optionally emit terrain writes.
          pcLog ctx "civilization: own overlay payload decoded"
          pcLog ctx "civilization: tick complete"
          pure (Right (simulationResultFromOverlay overlay))

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Look up a parameter value and show it, with a fallback.
showParam :: Map Text Value -> Text -> Text
showParam params key = case Map.lookup key params of
  Just (Number n) -> pack (show n)
  Just (String s) -> s
  Just (Bool b)   -> pack (show b)
  Just _          -> "<complex>"
  Nothing         -> "<not set>"

-- | Load and parse the civilization overlay schema from disk.
loadCivilizationSchema :: IO (Either Text OverlaySchema)
loadCivilizationSchema = do
  readResult <- (try (BS.readFile "civilization.toposchema") :: IO (Either IOException BS.ByteString))
  case readResult of
    Left err -> pure (Left ("could not read civilization.toposchema: " <> pack (show err)))
    Right schemaBytes -> pure (parseOverlaySchema schemaBytes)

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

main :: IO ()
main = runPlugin civPlugin
