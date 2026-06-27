{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Executable plugin fixtures used by SDK and host integration tests.
--
-- The fixtures are intentionally backend-neutral and communicate through the
-- public plugin RPC protocol over stdio only when the test harness explicitly
-- enables compatibility mode. Tests can run this module through the dedicated
-- @topo-plugin-fixture@ executable, or by re-entering the test executable with
-- @--topo-plugin-fixture NAME@.
module Topo.Plugin.SDK.Test.Fixtures
  ( fixtureNames
  , runFixtureCli
  , runNamedFixture
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (stdin, stdout)

import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataResourceSchema(..)
  , allOperations
  , noOperations
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , MutateResult(..)
  , QueryResult(..)
  )
import Topo.Plugin.RPC.Transport
  ( closeTransport
  , connectPlugin
  , recvMessage
  , sendMessage
  )
import Topo.Plugin.SDK

fixtureNames :: [String]
fixtureNames =
  [ "echo"
  , "generator"
  , "simulation"
  , "crud"
  , "external-provider"
  , "external-consumer"
  , "bad-handshake"
  , "slow"
  , "crashy"
  ]

runFixtureCli :: IO ()
runFixtureCli = do
  args <- getArgs
  case args of
    ["--topo-plugin-fixture", name] -> runNamedFixture name
    [name] -> runNamedFixture name
    _ -> die ("usage: topo-plugin-fixture <" <> unwords fixtureNames <> ">")

runNamedFixture :: String -> IO ()
runNamedFixture = \case
  "echo" -> runPlugin echoPlugin
  "generator" -> runPlugin generatorPlugin
  "simulation" -> runPlugin simulationPlugin
  "crud" -> mkCrudPlugin >>= runPlugin
  "external-provider" -> mkExternalProviderPlugin >>= runPlugin
  "external-consumer" -> mkExternalConsumerPlugin >>= runPlugin
  "bad-handshake" -> runBadHandshakeFixture
  "slow" -> runSlowFixture
  "crashy" -> exitFailure
  unknown -> die ("unknown topo plugin fixture: " <> unknown)

echoPlugin :: PluginDef
echoPlugin = defaultPluginDef
  { pdName = "fixture-echo"
  , pdVersion = "1.0.0"
  }

generatorPlugin :: PluginDef
generatorPlugin = defaultPluginDef
  { pdName = "fixture-generator"
  , pdVersion = "1.0.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = ["erosion"]
      , gdRun = \ctx -> do
          pcLog ctx "fixture-generator: invoked"
          pure (Right defaultGeneratorTickResult
            { gtrTerrain = object ["fixture" .= ("generator" :: Text), "seed" .= pcSeed ctx]
            , gtrOverlay = Just (object ["generated" .= True])
            , gtrMetadata = Just (object ["stage" .= ("fixture-generator" :: Text)])
            })
      }
  }

simulationPlugin :: PluginDef
simulationPlugin = defaultPluginDef
  { pdName = "fixture-simulation"
  , pdVersion = "1.0.0"
  , pdSchemaFile = Just "fixture-simulation.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = ["weather"]
      , sdTick = \ctx -> do
          pcLog ctx "fixture-simulation: tick"
          pure (Right defaultSimulationTickResult
            { strOverlay = object
                [ "fixture" .= ("simulation" :: Text)
                , "seed" .= pcSeed ctx
                , "dependencies" .= Map.keys (pcOverlays ctx)
                ]
            , strTerrainWrites = Just (object ["writes" .= ([] :: [Value])])
            })
      }
  }

mkCrudPlugin :: IO PluginDef
mkCrudPlugin = do
  recordsRef <- newIORef
    [ record [ ("id", String "alpha"), ("name", String "Alpha"), ("chunk", Number 0), ("tile", Number 1) ]
    , record [ ("id", String "beta"), ("name", String "Beta"), ("chunk", Number 1), ("tile", Number 2) ]
    ]
  pure defaultPluginDef
    { pdName = "fixture-crud"
    , pdVersion = "1.0.0"
    , pdDataResources = [crudResource recordsRef]
    }

crudResource :: IORef [DataRecord] -> DataResourceDef
crudResource recordsRef = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsName = "items"
      , drsLabel = "Fixture Items"
      , drsHexBound = True
      , drsFields =
          [ DataFieldDef "id" DFText "ID" False Nothing
          , DataFieldDef "name" DFText "Name" False Nothing
          , DataFieldDef "chunk" DFInt "Chunk" False Nothing
          , DataFieldDef "tile" DFInt "Tile" False Nothing
          ]
      , drsOperations = allOperations
      , drsKeyField = "id"
      , drsOverlay = Nothing
      }
  , drdHandler = DataHandler
      { dhQuery = Just (\_ query -> Right <$> queryRecords recordsRef "items" query)
      , dhMutate = Just (\_ mutation -> mutateRecords recordsRef mutation)
      }
  }

mkExternalProviderPlugin :: IO PluginDef
mkExternalProviderPlugin = do
  sourcesRef <- newIORef
    [ record
        [ ("source_id", String "terrain.catalog")
        , ("endpoint", String "fixture://provider/terrain.catalog")
        , ("capability", String "read")
        , ("status", String "available")
        ]
    ]
  pure defaultPluginDef
    { pdName = "fixture-external-provider"
    , pdVersion = "1.0.0"
    , pdDataDirectory = Just "external-provider-data"
    , pdDataResources = [externalProviderResource sourcesRef]
    , pdExternalDataSources =
        [ RPCExternalDataSourceDecl
            { redsdName = "terrain.catalog"
            , redsdLabel = "Terrain Catalog"
            , redsdDescription = "Provider-owned terrain catalogue fixture"
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
            , redsdResources = ["shared_sources"]
            , redsdStatus = RPCExternalDataSourceStatus ExternalStatusReady (Just "fixture ready")
            , redsdUiHints = defaultRPCUIHints { ruiDisplayName = Just "Terrain Catalog" }
            }
        ]
    }

externalProviderResource :: IORef [DataRecord] -> DataResourceDef
externalProviderResource sourcesRef = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsName = "shared_sources"
      , drsLabel = "Shared External Sources"
      , drsHexBound = False
      , drsFields =
          [ DataFieldDef "source_id" DFText "Source ID" False Nothing
          , DataFieldDef "endpoint" DFText "Endpoint" False Nothing
          , DataFieldDef "capability" DFText "Capability" False Nothing
          , DataFieldDef "status" DFText "Status" False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True }
      , drsKeyField = "source_id"
      , drsOverlay = Nothing
      }
  , drdHandler = noDataHandler
      { dhQuery = Just (\_ query -> Right <$> queryRecords sourcesRef "shared_sources" query)
      }
  }

mkExternalConsumerPlugin :: IO PluginDef
mkExternalConsumerPlugin = do
  bindingsRef <- newIORef
    [ record
        [ ("binding_id", String "consumer.terrain.catalog")
        , ("source_id", String "terrain.catalog")
        , ("grant", String "read")
        , ("status", String "linked")
        ]
    ]
  pure defaultPluginDef
    { pdName = "fixture-external-consumer"
    , pdVersion = "1.0.0"
    , pdDataDirectory = Just "external-consumer-data"
    , pdDataResources = [externalConsumerResource bindingsRef]
    , pdExternalDataSourceRefs =
        [ RPCExternalDataSourceRef
            { redsrName = "terrain.catalog"
            , redsrProvider = Just "fixture-external-provider"
            , redsrSource = "terrain.catalog"
            , redsrRequired = True
            , redsrAccess = [ExternalAccessRead]
            , redsrResources = ["shared_sources"]
            , redsrStatus = RPCExternalDataSourceStatus ExternalStatusUnknown (Just "resolved by fixture startup")
            , redsrUiHints = defaultRPCUIHints { ruiDisplayName = Just "Terrain Catalog" }
            }
        ]
    }

externalConsumerResource :: IORef [DataRecord] -> DataResourceDef
externalConsumerResource bindingsRef = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsName = "source_bindings"
      , drsLabel = "External Source Bindings"
      , drsHexBound = False
      , drsFields =
          [ DataFieldDef "binding_id" DFText "Binding ID" False Nothing
          , DataFieldDef "source_id" DFText "Source ID" False Nothing
          , DataFieldDef "grant" DFText "Grant" False Nothing
          , DataFieldDef "status" DFText "Status" False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True }
      , drsKeyField = "binding_id"
      , drsOverlay = Nothing
      }
  , drdHandler = noDataHandler
      { dhQuery = Just (\_ query -> Right <$> queryRecords bindingsRef "source_bindings" query)
      }
  }

queryRecords :: IORef [DataRecord] -> Text -> DataQuery -> IO QueryResult
queryRecords recordsRef resourceName query = do
  records <- readIORef recordsRef
  let matching = filter (matchesQuery query) records
  pure (QueryResult resourceName matching (Just (length matching)))

mutateRecords :: IORef [DataRecord] -> DataMutation -> IO (Either Text MutateResult)
mutateRecords recordsRef mutation = do
  result <- atomicModifyIORef' recordsRef $ \records ->
    case mutation of
      MutCreate newRecord ->
        (records <> [newRecord], ok (Just newRecord))
      MutUpdate key newRecord ->
        let records' = replaceByKey key newRecord records
        in (records', ok (Just newRecord))
      MutDelete key ->
        (filter (not . recordHasField "id" key) records, ok Nothing)
      MutSetHex chunk tile newRecord ->
        let stamped = addHexFields chunk tile newRecord
            records' = stamped : filter (not . recordAtHex chunk tile) records
        in (records', ok (Just stamped))
  pure (Right result)
  where
    ok = MutateResult True Nothing

replaceByKey :: Value -> DataRecord -> [DataRecord] -> [DataRecord]
replaceByKey key newRecord records =
  let (matching, rest) = span (not . recordHasField "id" key) records
  in case rest of
    [] -> records <> [newRecord]
    (_old:tailRecords) -> matching <> [newRecord] <> tailRecords

matchesQuery :: DataQuery -> DataRecord -> Bool
matchesQuery QueryAll _ = True
matchesQuery (QueryByKey key) row = recordHasAnyKey key row
matchesQuery (QueryByHex chunk tile) row = recordAtHex chunk tile row
matchesQuery (QueryByField field value) row = recordHasField field value row

recordHasAnyKey :: Value -> DataRecord -> Bool
recordHasAnyKey key row = recordHasField "id" key row || recordHasField "source_id" key row || recordHasField "binding_id" key row

recordHasField :: Text -> Value -> DataRecord -> Bool
recordHasField field value (DataRecord fields) = Map.lookup field fields == Just value

recordAtHex :: Int -> Int -> DataRecord -> Bool
recordAtHex chunk tile row = recordHasField "chunk" (Number (fromIntegral chunk)) row && recordHasField "tile" (Number (fromIntegral tile)) row

addHexFields :: Int -> Int -> DataRecord -> DataRecord
addHexFields chunk tile (DataRecord fields) = DataRecord
  (Map.insert "chunk" (Number (fromIntegral chunk)) (Map.insert "tile" (Number (fromIntegral tile)) fields))

record :: [(Text, Value)] -> DataRecord
record = DataRecord . Map.fromList

runBadHandshakeFixture :: IO ()
runBadHandshakeFixture = do
  connectPlugin "fixture-bad-handshake" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      _ <- sendMessage transport (BSC.pack "{not valid json")
      closeTransport transport

runSlowFixture :: IO ()
runSlowFixture = do
  threadDelay 2000000
  runPlugin echoPlugin { pdName = "fixture-slow" }
