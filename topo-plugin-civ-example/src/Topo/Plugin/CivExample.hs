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
module Topo.Plugin.CivExample
  ( civPlugin
  , main
  ) where

import Control.Exception (IOException, try)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as BS
import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
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
  , pdVersion    = "1.1.0"
  , pdDescription = Just "Generated civilization overlay with an immutable sample catalogue"
  , pdRuntimeTopoMin = Just "1.0"
  , pdSchemaFile = Just "civilization.toposchema"
  , pdDataDirectory = Just "civilization-data"
  , pdDataResources = [settlementsResource, culturesResource]
  , pdExternalDataSources = [settlementLedgerSource]
  , pdUiHints = defaultRPCUIHints
      { ruiDisplayName = Just "Civilization"
      , ruiCategory = Just "Simulation"
      , ruiTags = ["settlements", "culture"]
      }
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
          , paramTooltip = "Enable deterministic trade and food bonuses for simulated settlements"
          }
      ]

  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "biomes"
      , gdRequires    = ["biomes", "rivers"]
      , gdRun         = runCivGenerator
      }

  , pdSimulation = Just SimulationDef
      { sdDependencies = ["weather"]
      , sdSchedule     = Just hourlyScheduleDecl
      , sdTick         = runCivSimTick
      }
  , pdGeneratorScope = Just GeneratorScopeDef
      { gsdInsertAfter = "biomes"
      , gsdRequires = ["biomes", "rivers"]
      , gsdScope = civGeneratorScope
      , gsdRun = runScopedCivGenerator
      }
  , pdSimulationScope = Just SimulationScopeDef
      { ssdDependencies = ["weather"]
      , ssdSchedule = Just hourlyScheduleDecl
      , ssdScope = civSimulationScope
      , ssdTick = runScopedCivSimTick
      }
  }

civGeneratorScope :: RPCInvocationScopeDecl
civGeneratorScope = RPCInvocationScopeDecl
  { risdInput = RPCScopeInput allSections SelectAllInvocationChunks [] False
  , risdOutput = RPCScopeOutput allSections SelectAllInvocationChunks True False
  , risdBudgets = broadScopeBudgets
  }
  where allSections = [TerrainElevation, TerrainClimate, TerrainVegetation]

-- | No terrain is requested during simulation. Only the named weather
-- dependency and this plugin's own overlay are visible to the callback.
civSimulationScope :: RPCInvocationScopeDecl
civSimulationScope = RPCInvocationScopeDecl
  { risdInput = RPCScopeInput [] SelectAllInvocationChunks ["weather"] True
  , risdOutput = RPCScopeOutput [] SelectAllInvocationChunks True False
  , risdBudgets = broadScopeBudgets
  }

broadScopeBudgets :: RPCScopeBudgets
broadScopeBudgets = RPCScopeBudgets maxBound maxBound maxBound

------------------------------------------------------------------------
-- Data resources and external data source declarations
------------------------------------------------------------------------

-- | Immutable sample settlements, independent of the generated overlay.
settlementsResource :: DataResourceDef
settlementsResource = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsSchemaVersion = currentDataResourceSchemaVersion
      , drsResourceVersion = defaultDataResourceVersion
      , drsName       = "settlements"
      , drsLabel      = "Sample Settlements"
      , drsHexBound   = True
      , drsFields     =
          [ DataFieldDef "name"       DFText  "Name"       False Nothing
          , DataFieldDef "population" DFInt   "Population" False Nothing
          , DataFieldDef "is_city"    DFBool  "City?"      False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True, doQueryByHex = True }
      , drsKeyField   = "name"
      , drsOverlay    = Nothing
      , drsPagination = defaultDataPagination
      }
  , drdHandler = noDataHandler
      { dhQuery = Just querySettlements
      }
  }

-- | Immutable sample cultures, independent of the generated overlay.
culturesResource :: DataResourceDef
culturesResource = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsSchemaVersion = currentDataResourceSchemaVersion
      , drsResourceVersion = defaultDataResourceVersion
      , drsName       = "cultures"
      , drsLabel      = "Sample Cultures"
      , drsHexBound   = False
      , drsFields     =
          [ DataFieldDef "culture_id" DFText  "Culture ID"  False Nothing
          , DataFieldDef "name"       DFText  "Name"        False Nothing
          , DataFieldDef "population" DFInt   "Population"  False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True }
      , drsKeyField   = "culture_id"
      , drsOverlay    = Nothing
      , drsPagination = defaultDataPagination
      }
  , drdHandler = noDataHandler
      { dhQuery = Just queryCultures
      }
  }

-- | Backend-neutral provider-owned source declaration for dependent plugins.
settlementLedgerSource :: RPCExternalDataSourceDecl
settlementLedgerSource = RPCExternalDataSourceDecl
  { redsdName = "settlement-ledger"
  , redsdLabel = "Sample Civilization Catalogue"
  , redsdDescription = "Provider-owned immutable demonstration records, not generated world state"
  , redsdKind = "catalog"
  , redsdCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
  , redsdResources = ["settlements", "cultures"]
  , redsdStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusReady
      , redssMessage = Just "Immutable sample records are available through the civilization plugin"
      , redssProviderId = Just "civilization"
      , redssAvailability = Just ExternalAvailabilityAvailable
      , redssHealth = Just ExternalHealthHealthy
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
      , redssVersion = Just "settlement-ledger.v1"
      , redssCompatibility = Just "manifest-v3"
      }
  , redsdConnection = Nothing
  , redsdConfigRefs =
      [ RPCExternalDataSourceConfigRef
          { redscrName = "settlement-ledger-binding"
          , redscrOrigin = ExternalConfigProvider
          , redscrKey = "civilization.settlement-ledger"
          , redscrRequired = True
          , redscrCompatibility = Just "manifest-v3"
          , redscrMetadata = Nothing
          }
      ]
  , redsdGrants =
      [ RPCExternalDataSourceGrant
          { redsgName = "settlement-read"
          , redsgAccess = [ExternalAccessRead]
          , redsgCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
          , redsgResources = ["settlements", "cultures"]
          , redsgStatus = defaultRPCExternalDataSourceStatus
              { redssState = ExternalStatusReady
              , redssMessage = Just "Read grant is brokered by topo without owning the source"
              , redssProviderId = Just "civilization"
              , redssAvailability = Just ExternalAvailabilityAvailable
              , redssHealth = Just ExternalHealthHealthy
              , redssAccessMode = Just ExternalAccessModeReadOnly
              , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
              , redssVersion = Just "settlement-read.v1"
              , redssCompatibility = Just "manifest-v3"
              }
          , redsgReference = Nothing
          , redsgConfigRefs =
              [ RPCExternalDataSourceConfigRef
                  { redscrName = "settlement-read-binding"
                  , redscrOrigin = ExternalConfigProvider
                  , redscrKey = "civilization.settlement-read"
                  , redscrRequired = True
                  , redscrCompatibility = Just "manifest-v3"
                  , redscrMetadata = Nothing
                  }
              ]
          }
      ]
  , redsdUiHints = defaultRPCUIHints
      { ruiDisplayName = Just "Sample Civilization Catalogue"
      , ruiCategory = Just "External data"
      }
  }

-- | Query immutable demonstration settlements.
--
-- Supports 'QueryAll' (list all) and 'QueryByKey' (by name). These records
-- deliberately do not reflect or track the generated civilization overlay.
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

-- | Query immutable demonstration cultures.
--
-- Supports 'QueryAll' (list all) and 'QueryByKey' (by culture_id). These
-- records deliberately do not reflect or track generated world state.
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

-- | Seed a deterministic sparse population from elevation, climate, and
-- vegetation. Each habitable chunk receives at least one settlement, while a
-- stable hash selects a small additional set of tiles.
runCivGenerator :: PluginContext -> IO (Either Text GeneratorTickResult)
runCivGenerator ctx = do
  pcLog ctx "civilization: generator — seeding initial population"
  reportPluginProgress ctx "civilization: preparing settlement seed data" 0.2
  let threshold = paramFloat (pcParams ctx) "habitability_threshold"
        defaultHabitabilityThreshold
  pcLog ctx ("civilization: habitability threshold = "
              <> showParam (pcParams ctx) "habitability_threshold")
  pcLog ctx ("civilization: seed = " <> pack (show (pcSeed ctx)))
  schemaResult <- loadCivilizationSchema
  case (decodeTerrainPayload (pcTerrain ctx), schemaResult) of
    (Left decodeErr, _) ->
      pure (Left ("civilization: failed to decode terrain payload: " <> decodeErr))
    (_, Left schemaErr) ->
      pure (Left ("civilization: failed to load schema: " <> schemaErr))
    (Right terrainWorld, Right schema) ->
      case generatorResultFromTerrainAndOverlay terrainWorld
          (seedCivilizationOverlay (pcSeed ctx) threshold schema terrainWorld) of
        Left encodeErr ->
          pure (Left ("civilization: failed to encode generator payload: " <> encodeErr))
        Right result -> do
          pcProgress ctx "civilization: generator complete" 0.9
          pcLog ctx "civilization: generator complete"
          pure (Right result)

-- | Build the initial sparse overlay. A tile is habitable only when all three
-- granted terrain sections are present for its chunk.
seedCivilizationOverlay
  :: Word64 -> Double -> OverlaySchema -> TerrainWorld -> Overlay
seedCivilizationOverlay seed threshold schema world =
  (emptyOverlay schema) { ovData = SparseData seededChunks }
  where
    seededChunks = IntMap.mapMaybeWithKey seedChunk (twTerrain world)
    seedChunk chunkId terrain = do
      climate <- IntMap.lookup chunkId (twClimate world)
      vegetation <- IntMap.lookup chunkId (twVegetation world)
      let tileCount = minimum
            [ U.length (tcElevation terrain)
            , U.length (ccTempAvg climate)
            , U.length (ccPrecipAvg climate)
            , U.length (vegCover vegetation)
            , U.length (vegDensity vegetation)
            ]
          candidates =
            [ (tileIdx, score)
            | tileIdx <- [0 .. tileCount - 1]
            , let score = habitabilityAt terrain climate vegetation tileIdx
            , score > 0
            , score >= realToFrac (clamp01 threshold)
            ]
      case candidates of
        [] -> Nothing
        _ ->
          let anchor = fst (foldl1 (lowerHash chunkId) candidates)
              selected = filter (isSelected chunkId anchor . fst) candidates
              records = IntMap.fromList
                [ (tileIdx, seedRecord seed chunkId tileIdx score schema)
                | (tileIdx, score) <- selected
                ]
          in Just (OverlayChunk records)
    lowerHash chunkId left@(leftIdx, _) right@(rightIdx, _)
      | tileHash seed chunkId rightIdx < tileHash seed chunkId leftIdx = right
      | otherwise = left
    isSelected chunkId anchor tileIdx =
      tileIdx == anchor || tileHash seed chunkId tileIdx `mod` 29 == 0

habitabilityAt :: TerrainChunk -> ClimateChunk -> VegetationChunk -> Int -> Float
habitabilityAt terrain climate vegetation tileIdx
  | elevation <= 0.25 = 0
  | otherwise = clamp01Float
      (0.25 * elevationSuitability
        + 0.25 * temperatureSuitability
        + 0.2 * precipitationSuitability
        + 0.3 * vegetationSuitability)
  where
    elevation = tcElevation terrain U.! tileIdx
    temperature = ccTempAvg climate U.! tileIdx
    precipitation = ccPrecipAvg climate U.! tileIdx
    cover = vegCover vegetation U.! tileIdx
    density = vegDensity vegetation U.! tileIdx
    elevationSuitability = clamp01Float ((elevation - 0.25) / 0.35)
    temperatureSuitability = clamp01Float (1 - abs (temperature - 0.55) * 2)
    precipitationSuitability = clamp01Float (0.25 + precipitation * 0.75)
    vegetationSuitability = clamp01Float ((cover + density) / 2)

seedRecord :: Word64 -> Int -> Int -> Float -> OverlaySchema -> OverlayRecord
seedRecord seed chunkId tileIdx score schema =
  setNamed "is_city" (OVBool False)
    . setNamed "trade_value" (OVFloat 0)
    . setNamed "food_supply" (OVFloat (clamp01Float score))
    . setNamed "infrastructure" (OVFloat (0.05 + 0.15 * clamp01Float score))
    . setNamed "culture_id" (OVInt cultureId)
    . setNamed "population" (OVFloat population)
    $ defaultRecord schema
  where
    hash = tileHash seed chunkId tileIdx
    randomUnit = fromIntegral (hash `mod` 10000) / 9999
    population = min 1500 (120 + 880 * clamp01Float score + 300 * randomUnit)
    cultureId = 1 + fromIntegral (hash `mod` 7)
    setNamed name value record = case fieldIndex schema name of
      Nothing -> record
      Just idx -> setRecordField idx value record

-- | Stable, platform-independent mixing for seed/chunk/tile selection.
tileHash :: Word64 -> Int -> Int -> Word64
tileHash seed chunkId tileIdx = mix64
  (seed `xor` (fromIntegral chunkId * 0x9e3779b97f4a7c15)
    `xor` (fromIntegral tileIdx * 0xbf58476d1ce4e5b9))

mix64 :: Word64 -> Word64
mix64 input =
  let z1 = (input `xor` (input `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in z2 `xor` (z2 `shiftR` 31)

------------------------------------------------------------------------
-- Simulation: tick population growth
------------------------------------------------------------------------

runScopedCivGenerator :: GeneratorContext -> IO (Either Text GeneratorTickResult)
runScopedCivGenerator ctx = case gcTerrain ctx of
  Nothing -> pure (Left "civilization: scoped generator terrain was unavailable")
  Just world -> do
    schemaResult <- loadCivilizationSchema
    pure $ case schemaResult of
      Left schemaErr -> Left schemaErr
      Right schema -> generatorResultFromScopedTerrainAndOverlay ctx world
        (seedCivilizationOverlay (gcSeed ctx)
          (paramFloat (gcParams ctx) "habitability_threshold" defaultHabitabilityThreshold)
          schema world)

runScopedCivSimTick :: SimulationContext -> IO (Either Text SimulationTickResult)
runScopedCivSimTick ctx = do
  schemaResult <- loadCivilizationSchema
  case schemaResult of
    Left schemaErr -> pure (Left schemaErr)
    Right schema -> case decodeScopedOwnOverlay schema ctx of
      Left decodeErr -> pure (Left decodeErr)
      Right overlay -> do
        let params = scParams ctx
            overlay' = tickOverlay
              (paramFloat params "growth_rate" defaultGrowthRate)
              (paramFloat params "infra_cost" defaultInfraCost)
              (paramFloat params "city_threshold" defaultCityThreshold)
              (paramBool params "enable_trade" True)
              schema overlay
        pure (simulationResultFromScopedOverlay ctx overlay')

-- | Tick civilization simulation.
--
-- For each populated hex in the civilization overlay:
--
-- 1. Grow population by the configured growth rate
-- 2. Accumulate infrastructure proportional to population
-- 3. Evolve bounded food supply and optional trade value
-- 4. Promote hexes above the city threshold to cities
-- 5. Return only the updated overlay (never terrain writes)
runCivSimTick :: PluginContext -> IO (Either Text SimulationTickResult)
runCivSimTick ctx = do
  pcLog ctx "civilization: simulation tick"
  pcProgress ctx "civilization: loading civilization overlay" 0.1
  let params = pcParams ctx
      growthRate = paramFloat params "growth_rate" defaultGrowthRate
      infraCost  = paramFloat params "infra_cost"  defaultInfraCost
      cityThresh = paramFloat params "city_threshold" defaultCityThreshold
      tradeEnabled = paramBool params "enable_trade" True
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
          let overlay' = tickOverlay growthRate infraCost cityThresh tradeEnabled schema overlay
          reportPluginProgress ctx "civilization: tick complete" 0.85
          pcLog ctx "civilization: tick complete"
          pure (Right (simulationResultFromOverlay overlay'))

-- | Apply one bounded deterministic simulation tick to every settlement.
tickOverlay :: Double -> Double -> Double -> Bool -> OverlaySchema -> Overlay -> Overlay
tickOverlay growthRate infraCost cityThresh tradeEnabled schema overlay =
  case ovData overlay of
    SparseData sm ->
      let sm' = IntMap.map
            (tickChunk growthRate infraCost cityThresh tradeEnabled schema) sm
      in overlay { ovData = SparseData sm' }
    DenseData _ -> overlay

-- | Apply growth, provisioning, and trade to every populated sparse tile.
tickChunk
  :: Double -> Double -> Double -> Bool -> OverlaySchema -> OverlayChunk -> OverlayChunk
tickChunk growthRate infraCost cityThresh tradeEnabled schema (OverlayChunk tileMap) =
  OverlayChunk (IntMap.map
    (tickRecord growthRate infraCost cityThresh tradeEnabled schema) tileMap)

-- | Update one settlement. Normalised infrastructure, food, and trade values
-- remain in @[0,1]@; population is capped to avoid runaway numeric growth.
tickRecord
  :: Double -> Double -> Double -> Bool -> OverlaySchema -> OverlayRecord -> OverlayRecord
tickRecord growthRate infraCost cityThresh tradeEnabled schema record =
  let popIdx   = fieldIndexOr schema "population" 0
      infraIdx = fieldIndexOr schema "infrastructure" 2
      foodIdx  = fieldIndexOr schema "food_supply" 3
      tradeIdx = fieldIndexOr schema "trade_value" 4
      cityIdx  = fieldIndexOr schema "is_city" 5
      pop      = max 0 (readFloat record popIdx)
      infra    = clamp01Float (readFloat record infraIdx)
      food     = clamp01Float (readFloat record foodIdx)
      trade    = clamp01Float (readFloat record tradeIdx)
      growth   = realToFrac (max 0 (min 0.5 growthRate))
      cost     = realToFrac (max 0 (min 1 infraCost))
      pop'     = min 1.0e9 (pop * (1 + growth))
      infra'   = clamp01Float (infra + pop * cost * 0.001)
      isCity   = pop' >= realToFrac (max 0 cityThresh)
      trade'   = clamp01Float $ if tradeEnabled
        then trade + 0.01 + 0.03 * infra' + (if isCity then 0.02 else 0)
        else trade - 0.03
      consumption = min 0.04 (pop' / 100000)
      food' = clamp01Float
        (food + 0.025 * infra' + (if tradeEnabled then 0.015 * trade' else 0)
          - consumption)
      r1 = setRecordField popIdx   (OVFloat pop')    record
      r2 = setRecordField infraIdx (OVFloat infra')  r1
      r3 = setRecordField foodIdx  (OVFloat food')   r2
      r4 = setRecordField tradeIdx (OVFloat trade')  r3
      r5 = setRecordField cityIdx  (OVBool isCity)   r4
  in r5

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Extract a numeric parameter with a default fallback.
paramFloat :: Map Text Value -> Text -> Double -> Double
paramFloat params key def = case Map.lookup key params of
  Just (Number n) -> realToFrac n
  _               -> def

-- | Extract a boolean parameter with a default fallback.
paramBool :: Map Text Value -> Text -> Bool -> Bool
paramBool params key def = case Map.lookup key params of
  Just (Bool value) -> value
  _ -> def

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

clamp01Float :: Float -> Float
clamp01Float = max 0 . min 1

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
main = runPluginWithManifestCommand civPlugin
