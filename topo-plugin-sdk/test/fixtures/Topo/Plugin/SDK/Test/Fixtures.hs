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
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  , defaultDataPagination
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

externalProviderName :: Text
externalProviderName = "fixture-external-provider"

externalConsumerName :: Text
externalConsumerName = "fixture-external-consumer"

externalSourceName :: Text
externalSourceName = "terrain.catalog"

externalGrantName :: Text
externalGrantName = "terrain-catalog-read"

externalBindingId :: Text
externalBindingId = "consumer.terrain.catalog"

externalSharedResources :: [Text]
externalSharedResources = ["shared_sources"]

externalCapabilities :: [RPCExternalDataSourceCapability]
externalCapabilities = [ExternalSourceQuery, ExternalSourceHealth]

externalReadAccess :: [RPCExternalDataSourceAccess]
externalReadAccess = [ExternalAccessRead]

externalProviderEndpoint :: Text
externalProviderEndpoint = "fixture://provider/terrain.catalog"

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
  , pdCapabilities = [CapWriteTerrain]
  , pdSimulation = Just SimulationDef
      { sdDependencies = ["weather"]
      , sdSchedule = Just hourlyScheduleDecl
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
      { drsSchemaVersion = currentDataResourceSchemaVersion
      , drsResourceVersion = defaultDataResourceVersion
      , drsName = "items"
      , drsLabel = "Fixture Items"
      , drsHexBound = True
      , drsFields =
          [ DataFieldDef "id" DFText "ID" False Nothing
          , DataFieldDef "name" DFText "Name" False Nothing
          , DataFieldDef "chunk" DFInt "Chunk" False Nothing
          , DataFieldDef "tile" DFInt "Tile" False Nothing
          ]
      , drsOperations = noOperations
          { doList = True
          , doGet = True
          , doCreate = True
          , doUpdate = True
          , doDelete = True
          , doQueryByHex = True
          , doQueryByField = True
          }
      , drsKeyField = "id"
      , drsOverlay = Nothing
      , drsPagination = defaultDataPagination
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
        [ ("source_id", String externalSourceName)
        , ("endpoint", String externalProviderEndpoint)
        , ("capability", String "read")
        , ("status", String "available")
        ]
    ]
  pure defaultPluginDef
    { pdName = externalProviderName
    , pdVersion = "1.0.0"
    , pdDataDirectory = Just "external-provider-data"
    , pdDataResources = [externalProviderResource sourcesRef]
    , pdExternalDataSources =
        [ RPCExternalDataSourceDecl
            { redsdName = externalSourceName
            , redsdLabel = "Terrain Catalog"
            , redsdDescription = "Provider-owned terrain catalogue fixture"
            , redsdKind = "catalog"
            , redsdCapabilities = externalCapabilities
            , redsdResources = externalSharedResources
            , redsdStatus = defaultRPCExternalDataSourceStatus
                { redssState = ExternalStatusReady
                , redssMessage = Just "fixture ready"
                , redssProviderId = Just externalProviderName
                , redssAvailability = Just ExternalAvailabilityAvailable
                , redssHealth = Just ExternalHealthHealthy
                , redssAccessMode = Just ExternalAccessModeReadOnly
                , redssCapabilityScope = externalCapabilities
                , redssVersion = Just "terrain.catalog.v1"
                , redssCompatibility = Just "manifest-v3"
                , redssDiagnostics = Just (object ["fixture" .= ("provider" :: Text)])
                }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants =
                [ RPCExternalDataSourceGrant
                    { redsgName = externalGrantName
                    , redsgAccess = externalReadAccess
                    , redsgCapabilities = externalCapabilities
                    , redsgResources = externalSharedResources
                    , redsgStatus = defaultRPCExternalDataSourceStatus
                        { redssState = ExternalStatusReady
                        , redssMessage = Just "fixture read grant"
                        , redssProviderId = Just externalProviderName
                        , redssAvailability = Just ExternalAvailabilityAvailable
                        , redssHealth = Just ExternalHealthHealthy
                        , redssAccessMode = Just ExternalAccessModeReadOnly
                        , redssCapabilityScope = externalCapabilities
                        , redssVersion = Just "terrain-catalog-read.v1"
                        , redssCompatibility = Just "manifest-v3"
                        , redssDiagnostics = Just (object ["fixture" .= ("grant" :: Text)])
                        }
                    , redsgReference = Nothing
                    , redsgConfigRefs = []
                    }
                ]
            , redsdUiHints = defaultRPCUIHints { ruiDisplayName = Just "Terrain Catalog" }
            }
        ]
    }

externalProviderResource :: IORef [DataRecord] -> DataResourceDef
externalProviderResource sourcesRef = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsSchemaVersion = currentDataResourceSchemaVersion
      , drsResourceVersion = defaultDataResourceVersion
      , drsName = "shared_sources"
      , drsLabel = "Shared External Sources"
      , drsHexBound = False
      , drsFields =
          [ DataFieldDef "source_id" DFText "Source ID" False Nothing
          , DataFieldDef "endpoint" DFText "Endpoint" False Nothing
          , DataFieldDef "capability" DFText "Capability" False Nothing
          , DataFieldDef "status" DFText "Status" False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True, doQueryByField = True }
      , drsKeyField = "source_id"
      , drsOverlay = Nothing
      , drsPagination = defaultDataPagination
      }
  , drdHandler = noDataHandler
      { dhQuery = Just (\_ query -> Right <$> queryRecords sourcesRef "shared_sources" query)
      }
  }

mkExternalConsumerPlugin :: IO PluginDef
mkExternalConsumerPlugin = do
  bindingsRef <- newIORef [consumerBindingRecord "declared"]
  pure defaultPluginDef
    { pdName = externalConsumerName
    , pdVersion = "1.0.0"
    , pdDataDirectory = Just "external-consumer-data"
    , pdDataResources = [externalConsumerResource bindingsRef]
    , pdExternalDataSourceRefs =
        [ RPCExternalDataSourceRef
            { redsrName = externalSourceName
            , redsrProvider = Just externalProviderName
            , redsrSource = externalSourceName
            , redsrRequired = True
            , redsrAccess = externalReadAccess
            , redsrResources = externalSharedResources
            , redsrGrant = Just externalGrantName
            , redsrStatus = defaultRPCExternalDataSourceStatus
                { redssState = ExternalStatusReady
                , redssMessage = Just "fixture read grant is available through provider contract"
                , redssProviderId = Just externalProviderName
                , redssAvailability = Just ExternalAvailabilityAvailable
                , redssHealth = Just ExternalHealthHealthy
                , redssAccessMode = Just ExternalAccessModeReadOnly
                , redssCapabilityScope = externalCapabilities
                , redssVersion = Just "terrain.catalog.v1"
                , redssCompatibility = Just "manifest-v3"
                , redssDiagnostics = Just (object ["fixture" .= ("consumer" :: Text)])
                }
            , redsrReference = Nothing
            , redsrConfigRefs = []
            , redsrUiHints = defaultRPCUIHints { ruiDisplayName = Just "Terrain Catalog" }
            }
        ]
    , pdOnExternalDataSourceGrant = Just (recordExternalGrant bindingsRef)
    , pdOnExternalDataSourceRevocation = Just (recordExternalRevocation bindingsRef)
    }

consumerBindingRecord :: Text -> DataRecord
consumerBindingRecord status = record
  [ ("binding_id", String externalBindingId)
  , ("provider", String externalProviderName)
  , ("source_id", String externalSourceName)
  , ("grant", String externalGrantName)
  , ("status", String status)
  ]

recordExternalGrant :: IORef [DataRecord] -> RPCExternalDataSourceGrantMessage -> IO ()
recordExternalGrant bindingsRef grant
  | redsgmProviderId grant == externalProviderName
      && grantTargetsConsumer (redsgmConsumerId grant)
      && redsgmSource grant == externalSourceName
      && redsgmGrant grant == externalGrantName =
      updateConsumerBinding bindingsRef "granted"
  | otherwise = pure ()

recordExternalRevocation :: IORef [DataRecord] -> RPCExternalDataSourceGrantRevocation -> IO ()
recordExternalRevocation bindingsRef revocation
  | redsrvProviderId revocation == externalProviderName
      && grantTargetsConsumer (redsrvConsumerId revocation)
      && redsrvSource revocation == externalSourceName
      && redsrvGrant revocation == externalGrantName =
      updateConsumerBinding bindingsRef "revoked"
  | otherwise = pure ()

grantTargetsConsumer :: Maybe Text -> Bool
grantTargetsConsumer Nothing = True
grantTargetsConsumer (Just consumerId) = consumerId == externalConsumerName

updateConsumerBinding :: IORef [DataRecord] -> Text -> IO ()
updateConsumerBinding bindingsRef status =
  atomicModifyIORef' bindingsRef $ \records ->
    (consumerBindingRecord status : filter (not . recordHasField "binding_id" (String externalBindingId)) records, ())

externalConsumerResource :: IORef [DataRecord] -> DataResourceDef
externalConsumerResource bindingsRef = DataResourceDef
  { drdSchema = DataResourceSchema
      { drsSchemaVersion = currentDataResourceSchemaVersion
      , drsResourceVersion = defaultDataResourceVersion
      , drsName = "source_bindings"
      , drsLabel = "External Source Bindings"
      , drsHexBound = False
      , drsFields =
          [ DataFieldDef "binding_id" DFText "Binding ID" False Nothing
          , DataFieldDef "provider" DFText "Provider" False Nothing
          , DataFieldDef "source_id" DFText "Source ID" False Nothing
          , DataFieldDef "grant" DFText "Grant" False Nothing
          , DataFieldDef "status" DFText "Status" False Nothing
          ]
      , drsOperations = noOperations { doList = True, doGet = True, doQueryByField = True }
      , drsKeyField = "binding_id"
      , drsOverlay = Nothing
      , drsPagination = defaultDataPagination
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
    ok record = MutateResult True Nothing record Nothing

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
