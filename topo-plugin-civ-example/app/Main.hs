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
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
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

-- | Civilization plugin: overlay + generator + simulation + data service.
civPlugin :: PluginDef
civPlugin = defaultPluginDef
  { pdName       = "civilization"
  , pdVersion    = "1.0.0"
  , pdSchemaFile = Just "civilization.toposchema"
  , pdDataResources = [settlementsResource, culturesResource]
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
-- Data resources
------------------------------------------------------------------------

-- | Settlements data resource — read-only listing of city/village hexes.
settlementsResource :: DataResourceDef
settlementsResource = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsName       = "settlements"
      , drsLabel      = "Settlements"
      , drsHexBound   = True
      , drsFields     =
          [ DataFieldDef "name"       DFText  "Name"       False Nothing
          , DataFieldDef "population" DFInt   "Population" False Nothing
          , DataFieldDef "is_city"    DFBool  "City?"      False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True, doQueryByHex = True }
      , drsKeyField   = "name"
      , drsOverlay    = Nothing
      }
  , drdHandler = noDataHandler
      { dhQuery = Just querySettlements
      }
  }

-- | Cultures data resource — read-only cultural group listing.
culturesResource :: DataResourceDef
culturesResource = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsName       = "cultures"
      , drsLabel      = "Cultures"
      , drsHexBound   = False
      , drsFields     =
          [ DataFieldDef "culture_id" DFText  "Culture ID"  False Nothing
          , DataFieldDef "name"       DFText  "Name"        False Nothing
          , DataFieldDef "population" DFInt   "Population"  False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True }
      , drsKeyField   = "culture_id"
      , drsOverlay    = Nothing
      }
  , drdHandler = noDataHandler
      { dhQuery = Just queryCultures
      }
  }

-- | Query handler for settlements — returns sample settlement data.
--
-- Supports 'QueryAll' (list all) and 'QueryByKey' (by name).
-- Returns hardcoded sample data representing the initial world state.
querySettlements :: PluginContext -> DataQuery -> IO (Either Text QueryResult)
querySettlements _ctx query = do
  let allSettlements =
        [ DataRecord (Map.fromList
            [ ("name", String "Ironhollow")
            , ("population", Number 2500)
            , ("is_city", Bool True)
            ])
        , DataRecord (Map.fromList
            [ ("name", String "Millbrook")
            , ("population", Number 450)
            , ("is_city", Bool False)
            ])
        , DataRecord (Map.fromList
            [ ("name", String "Stonewatch")
            , ("population", Number 1200)
            , ("is_city", Bool True)
            ])
        ]
  case query of
    QueryAll ->
      pure (Right (QueryResult "settlements" allSettlements (Just (length allSettlements))))
    QueryByKey (String keyName) ->
      let matching = filter (\(DataRecord m) -> Map.lookup "name" m == Just (String keyName)) allSettlements
      in pure (Right (QueryResult "settlements" matching (Just (length matching))))
    _ ->
      pure (Right (QueryResult "settlements" [] (Just 0)))

-- | Query handler for cultures — returns sample cultural group data.
--
-- Supports 'QueryAll' (list all) and 'QueryByKey' (by culture_id).
queryCultures :: PluginContext -> DataQuery -> IO (Either Text QueryResult)
queryCultures _ctx query = do
  let allCultures =
        [ DataRecord (Map.fromList
            [ ("culture_id", String "dwarven")
            , ("name", String "Dwarves of Ironhollow")
            , ("population", Number 3100)
            ])
        , DataRecord (Map.fromList
            [ ("culture_id", String "human")
            , ("name", String "Millbrook Settlers")
            , ("population", Number 1650)
            ])
        ]
  case query of
    QueryAll ->
      pure (Right (QueryResult "cultures" allCultures (Just (length allCultures))))
    QueryByKey (String keyId) ->
      let matching = filter (\(DataRecord m) -> Map.lookup "culture_id" m == Just (String keyId)) allCultures
      in pure (Right (QueryResult "cultures" matching (Just (length matching))))
    _ ->
      pure (Right (QueryResult "cultures" [] (Just 0)))

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
-- For each populated hex in the civilization overlay:
--
-- 1. Grow population by the configured growth rate
-- 2. Accumulate infrastructure proportional to population
-- 3. Promote hexes above the city threshold to cities
-- 4. Return the updated overlay
runCivSimTick :: PluginContext -> IO (Either Text SimulationTickResult)
runCivSimTick ctx = do
  pcLog ctx "civilization: simulation tick"
  let params = pcParams ctx
      growthRate = paramFloat params "growth_rate" defaultGrowthRate
      infraCost  = paramFloat params "infra_cost"  defaultInfraCost
      cityThresh = paramFloat params "city_threshold" defaultCityThreshold
  pcLog ctx ("civilization: growth rate = " <> pack (show growthRate))
  schemaResult <- loadCivilizationSchema
  case schemaResult of
    Left schemaErr ->
      pure (Left ("civilization: failed to load schema: " <> schemaErr))
    Right schema ->
      case decodeOwnOverlay schema ctx of
        Left decodeErr ->
          pure (Left ("civilization: failed to decode own overlay payload: " <> decodeErr))
        Right overlay -> do
          let overlay' = tickOverlay growthRate infraCost cityThresh schema overlay
          pcLog ctx "civilization: tick complete"
          pure (Right (simulationResultFromOverlay overlay'))

-- | Apply one simulation tick to the civilization overlay.
--
-- Iterates over all populated tiles in all chunks, updating
-- population, infrastructure, and city flag.
tickOverlay :: Double -> Double -> Double -> OverlaySchema -> Overlay -> Overlay
tickOverlay growthRate infraCost cityThresh schema overlay =
  case ovData overlay of
    SparseData sm ->
      let sm' = IntMap.map (tickChunk growthRate infraCost cityThresh schema) sm
      in overlay { ovData = SparseData sm' }
    DenseData _ ->
      -- Dense storage not used by this overlay; pass through unchanged.
      overlay

-- | Apply growth to every populated tile in a sparse chunk.
tickChunk :: Double -> Double -> Double -> OverlaySchema -> OverlayChunk -> OverlayChunk
tickChunk growthRate infraCost cityThresh schema (OverlayChunk tileMap) =
  OverlayChunk (IntMap.map (tickRecord growthRate infraCost cityThresh schema) tileMap)

-- | Update a single tile record:
--
-- * population += population * growthRate
-- * infrastructure += population * infraCost (capped at 1.0)
-- * is_city = population >= cityThreshold
tickRecord :: Double -> Double -> Double -> OverlaySchema -> OverlayRecord -> OverlayRecord
tickRecord growthRate infraCost cityThresh schema record =
  let popIdx   = fieldIndexOr schema "population" 0
      infraIdx = fieldIndexOr schema "infrastructure" 2
      cityIdx  = fieldIndexOr schema "is_city" 5
      pop      = readFloat record popIdx
      infra    = readFloat record infraIdx
      pop'     = pop + pop * realToFrac growthRate
      infra'   = min 1.0 (infra + pop * realToFrac infraCost * 0.001)
      isCity   = pop' >= realToFrac cityThresh
      r1 = setRecordField popIdx   (OVFloat pop')  record
      r2 = setRecordField infraIdx (OVFloat infra') r1
      r3 = setRecordField cityIdx  (OVBool isCity)  r2
  in r3

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Extract a numeric parameter with a default fallback.
paramFloat :: Map Text Value -> Text -> Double -> Double
paramFloat params key def = case Map.lookup key params of
  Just (Number n) -> realToFrac n
  _               -> def

-- | Read a 'Float' from a record at a given field index, defaulting to 0.
readFloat :: OverlayRecord -> Int -> Float
readFloat record idx = case recordField idx record of
  Just (OVFloat f) -> f
  _                -> 0

-- | Look up a field index by name, returning a fallback when the field
-- is not declared in the schema.
fieldIndexOr :: OverlaySchema -> Text -> Int -> Int
fieldIndexOr schema name def = case fieldIndex schema name of
  Just i  -> i
  Nothing -> def

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
