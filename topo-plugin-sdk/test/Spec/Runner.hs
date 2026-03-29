{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Runner (spec) where

import Control.Concurrent (MVar, forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import System.IO (BufferMode(NoBuffering), Handle, hSetBinaryMode, hSetBuffering)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec

import Topo.Plugin.DataResource
  ( DataFieldDef(..), DataFieldType(..)
  , DataOperations(..), DataResourceSchema(..)
  , noOperations, allOperations
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..), DataQuery(..), DataRecord(..)
  , MutateResource(..), MutateResult(..)
  , QueryResource(..), QueryResult(..)
  )
import Topo.Plugin.RPC (terrainWorldToPayload)
import Topo.Plugin.RPC.Protocol
  ( GeneratorResult(..)
  , Handshake(..), HandshakeAck(..)
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , PluginError(..)
  , PluginLog(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , SimulationResult(..)
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , closeTransport
  , recvMessage
  , sendMessage
  )
import Topo.Plugin.SDK.Runner (runPluginSession)
import Topo.Plugin.SDK.Types
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorldWithPlanet)

spec :: Spec
spec = describe "SDK runner pipe integration" $ do
  it "handles invoke_generator and returns generator_result payload" $
    withTransportPair $ \host plugin -> do
      done <- startSession generatorPlugin plugin
      sendEnvelope host (generatorInvoke 77)
      env <- recvEnvelope host
      envType env `shouldBe` MsgGeneratorResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          grTerrain result `shouldBe` object ["ok" .= True]
          grOverlay result `shouldBe` Just (object ["seeded" .= (1 :: Int)])
          grMetadata result `shouldBe` Just (object ["source" .= ("test" :: Text)])
      shutdownAndWait host done

  it "handles invoke_simulation with log interleaving before simulation_result" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationPlugin plugin
      sendEnvelope host simulationInvoke
      env1 <- recvEnvelope host
      env2 <- recvEnvelope host
      env3 <- recvEnvelope host
      map envType [env1, env2, env3] `shouldBe` [MsgLog, MsgLog, MsgSimulationResult]
      logMessages <- traverse decodeLog [env1, env2]
      logMessages `shouldBe` ["sim:start", "sim:end"]
      case Aeson.fromJSON (envPayload env3) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          srOverlay result `shouldBe` object ["population" .= (42 :: Int)]
          srTerrainWrites result `shouldBe` Just (object [])
      shutdownAndWait host done

  it "hydrates generator pcWorld from terrain payload metadata" $
    withTransportPair $ \host plugin -> do
      done <- startSession generatorMetadataPlugin plugin
      payload <- terrainPayloadFromWorld hydratedWorld
      sendEnvelope host (generatorInvokeWithTerrain 91 payload)
      env <- recvEnvelope host
      envType env `shouldBe` MsgGeneratorResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result ->
          grMetadata result `shouldBe` Just hydratedWorldMetadata
      shutdownAndWait host done

  it "hydrates simulation pcWorld from terrain payload metadata" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationMetadataPlugin plugin
      payload <- terrainPayloadFromWorld hydratedWorld
      sendEnvelope host (simulationInvokeWithTerrain payload)
      env <- recvEnvelope host
      envType env `shouldBe` MsgSimulationResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result ->
          srOverlay result `shouldBe` hydratedWorldMetadata
      shutdownAndWait host done

  it "returns plugin error for invoke_generator when generator capability is missing" $
    withTransportPair $ \host plugin -> do
      done <- startSession simOnlyPlugin plugin
      sendEnvelope host (generatorInvoke 0)
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success pluginErr -> do
          peCode pluginErr `shouldBe` 2
          peMessage pluginErr `shouldBe` "Plugin has no generator"
      shutdownAndWait host done

  it "shuts down cleanly on shutdown message" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationPlugin plugin
      shutdownAndWait host done

  it "handles handshake and returns handshake_ack with data resources" $
    withTransportPair $ \host plugin -> do
      done <- startSession dataPlugin plugin
      let hs = RPCEnvelope
            { envType = MsgHandshake
            , envPayload = Aeson.toJSON Handshake
                { hsProtocolVersion = 1
                , hsWorldPath = Just "/world/save"
            , hsHostCapabilities = []
                }
            }
      sendEnvelope host hs
      env <- recvEnvelope host
      envType env `shouldBe` MsgHandshakeAck
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (ack :: HandshakeAck) -> do
          length (haResources ack) `shouldBe` 1
          case haResources ack of
            [resource] -> drsName resource `shouldBe` "items"
            _ -> expectationFailure "expected exactly one data resource in handshake ack"
      shutdownAndWait host done

  it "dispatches query_resource to the correct data handler" $
    withTransportPair $ \host plugin -> do
      done <- startSession dataPlugin plugin
      let qr = RPCEnvelope
            { envType = MsgQueryResource
            , envPayload = Aeson.toJSON QueryResource
                { qrResource = "items"
                , qrQuery = QueryAll
                , qrPageSize = Nothing
                , qrPageOffset = Nothing
                }
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgQueryResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (result :: QueryResult) -> do
          qrsResource result `shouldBe` "items"
          length (qrsRecords result) `shouldBe` 2
          qrsTotalCount result `shouldBe` Just 2
      shutdownAndWait host done

  it "returns error for query on unknown resource" $
    withTransportPair $ \host plugin -> do
      done <- startSession dataPlugin plugin
      let qr = RPCEnvelope
            { envType = MsgQueryResource
            , envPayload = Aeson.toJSON QueryResource
                { qrResource = "nonexistent"
                , qrQuery = QueryAll
                , qrPageSize = Nothing
                , qrPageOffset = Nothing
                }
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) ->
          peMessage pluginErr `shouldBe` "Unknown resource: nonexistent"
      shutdownAndWait host done

  it "dispatches mutate_resource to the correct data handler" $
    withTransportPair $ \host plugin -> do
      done <- startSession dataPlugin plugin
      let newRecord = DataRecord (Map.fromList [("name", String "Sword"), ("count", Number 5)])
          mr = RPCEnvelope
            { envType = MsgMutateResource
            , envPayload = Aeson.toJSON MutateResource
                { mrResource = "items"
                , mrMutation = MutCreate newRecord
                }
            }
      sendEnvelope host mr
      env <- recvEnvelope host
      envType env `shouldBe` MsgMutateResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (result :: MutateResult) -> do
          mrsSuccess result `shouldBe` True
          mrsRecord result `shouldBe` Just newRecord
      shutdownAndWait host done

  it "returns error for mutate on resource with no mutate handler" $
    withTransportPair $ \host plugin -> do
      done <- startSession readOnlyDataPlugin plugin
      let mr = RPCEnvelope
            { envType = MsgMutateResource
            , envPayload = Aeson.toJSON MutateResource
                { mrResource = "readonly"
                , mrMutation = MutCreate (DataRecord Map.empty)
                }
            }
      sendEnvelope host mr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) ->
          peMessage pluginErr `shouldBe` "Resource 'readonly' does not support mutations"
      shutdownAndWait host done

  it "returns error when handler returns Left" $
    withTransportPair $ \host plugin -> do
      done <- startSession failingDataPlugin plugin
      let qr = RPCEnvelope
            { envType = MsgQueryResource
            , envPayload = Aeson.toJSON QueryResource
                { qrResource = "broken"
                , qrQuery = QueryAll
                , qrPageSize = Nothing
                , qrPageOffset = Nothing
                }
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) ->
          peMessage pluginErr `shouldBe` "database connection failed"
      shutdownAndWait host done

withTransportPair :: (Transport -> Transport -> IO a) -> IO a
withTransportPair action = do
  (host, plugin) <- mkTransportPair
  action host plugin `finally` do
    closeTransport host
    closeTransport plugin

mkTransportPair :: IO (Transport, Transport)
mkTransportPair = do
  (hostToPluginRead, hostToPluginWrite) <- createPipe
  (pluginToHostRead, pluginToHostWrite) <- createPipe
  configureHandle hostToPluginRead
  configureHandle hostToPluginWrite
  configureHandle pluginToHostRead
  configureHandle pluginToHostWrite
  now <- getPOSIXTime
  let timestamp = Text.pack (show (round (now * 1000000) :: Integer))
  let host = Transport
        { tReadHandle = pluginToHostRead
        , tWriteHandle = hostToPluginWrite
        , tPluginName = "test-host-" <> timestamp
        }
      plugin = Transport
        { tReadHandle = hostToPluginRead
        , tWriteHandle = pluginToHostWrite
        , tPluginName = "test-plugin-" <> timestamp
        }
  pure (host, plugin)

configureHandle :: Handle -> IO ()
configureHandle handle = do
  hSetBinaryMode handle True
  hSetBuffering handle NoBuffering

startSession :: PluginDef -> Transport -> IO (MVar ())
startSession plugin transport = do
  done <- newEmptyMVar
  _ <- forkFinally (runPluginSession plugin transport Map.empty) (\_ -> putMVar done ())
  pure done

sendEnvelope :: Transport -> RPCEnvelope -> IO ()
sendEnvelope transport envelope = do
  sendResult <- sendMessage transport (encodeMessage envelope)
  case sendResult of
    Left err -> expectationFailure ("send failed: " <> show err)
    Right () -> pure ()

recvEnvelope :: Transport -> IO RPCEnvelope
recvEnvelope transport = do
  recvResult <- timeout 2000000 (recvMessage transport)
  case recvResult of
    Nothing -> expectationFailure "recv timeout" >> fail "timeout"
    Just (Left err) -> expectationFailure ("recv failed: " <> show err) >> fail "recv"
    Just (Right bytes) ->
      case decodeMessage bytes of
        Left err -> expectationFailure ("decode failed: " <> Text.unpack err) >> fail "decode"
        Right env -> pure env

decodeLog :: RPCEnvelope -> IO Text
decodeLog envelope =
  case Aeson.fromJSON (envPayload envelope) of
    Aeson.Error err -> expectationFailure err >> fail "log decode"
    Aeson.Success (pluginLog :: PluginLog) -> pure (plMessage pluginLog)

generatorInvoke :: Word64 -> RPCEnvelope
generatorInvoke seed = generatorInvokeWithTerrain seed minimalTerrainPayload

generatorInvokeWithTerrain :: Word64 -> Value -> RPCEnvelope
generatorInvokeWithTerrain seed terrainPayload = RPCEnvelope
  { envType = MsgInvokeGenerator
  , envPayload = Aeson.toJSON InvokeGenerator
      { igPayloadVersion = 1
      , igStageId = "plugin:test"
      , igSeed = seed
      , igConfig = Map.empty
      , igTerrain = terrainPayload
      }
  }

simulationInvoke :: RPCEnvelope
simulationInvoke = simulationInvokeWithTerrain minimalTerrainPayload

simulationInvokeWithTerrain :: Value -> RPCEnvelope
simulationInvokeWithTerrain terrainPayload = RPCEnvelope
  { envType = MsgInvokeSimulation
  , envPayload = Aeson.toJSON InvokeSimulation
      { isPayloadVersion = 1
      , isNodeId = "simulation"
      , isWorldTime = 5
      , isDeltaTicks = 1
      , isCalendar = object ["year" .= (0 :: Int)]
      , isConfig = Map.empty
      , isTerrain = terrainPayload
      , isOverlays = object ["weather" .= object ["storage" .= ("sparse" :: Text), "chunks" .= ([] :: [Aeson.Value])]]
      , isOwnOverlay = object ["storage" .= ("sparse" :: Text), "chunks" .= ([] :: [Aeson.Value])]
      }
  }

minimalTerrainPayload :: Value
minimalTerrainPayload = object
  [ "chunk_count" .= (0 :: Int)
  , "climate_count" .= (0 :: Int)
  , "river_count" .= (0 :: Int)
  , "vegetation_count" .= (0 :: Int)
  , "chunk_size" .= (64 :: Int)
  , "hex_grid" .= defaultHexGridMeta
  , "planet" .= defaultPlanetConfig
  , "slice" .= defaultWorldSlice
  , "encoding" .= ("base64" :: Text)
  , "terrain" .= object []
  , "climate" .= object []
  , "vegetation" .= object []
  ]

terrainPayloadFromWorld :: TerrainWorld -> IO Value
terrainPayloadFromWorld world =
  case terrainWorldToPayload world of
    Left err -> expectationFailure (Text.unpack err) >> fail "terrain encode"
    Right payload -> pure payload

hydratedWorld :: TerrainWorld
hydratedWorld = emptyWorldWithPlanet
  (WorldConfig { wcChunkSize = 32 })
  (HexGridMeta { hexSizeKm = 11.0 })
  (defaultPlanetConfig
    { pcRadius = 7000.0
    , pcAxialTilt = 15.0
    , pcInsolation = 0.9
    })
  (defaultWorldSlice
    { wsLatCenter = 12.5
    , wsLatExtent = 24.0
    , wsLonCenter = -45.0
    , wsLonExtent = 80.0
    })

hydratedWorldMetadata :: Value
hydratedWorldMetadata = object
  [ "chunk_size" .= (32 :: Int)
  , "hex_size_km" .= (11.0 :: Float)
  , "planet_radius" .= (7000.0 :: Float)
  , "slice_lat_center" .= (12.5 :: Float)
  , "slice_lon_center" .= (-45.0 :: Float)
  ]

shutdownAndWait :: Transport -> MVar () -> IO ()
shutdownAndWait host done = do
  sendEnvelope host (RPCEnvelope MsgShutdown (object []))
  waitResult <- timeout 2000000 (takeMVar done)
  case waitResult of
    Nothing -> expectationFailure "plugin session did not shut down in time"
    Just () -> pure ()

generatorPlugin :: PluginDef
generatorPlugin = defaultPluginDef
  { pdName = "generator"
  , pdVersion = "1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = []
      , gdRun = \_ctx -> pure (Right GeneratorTickResult
          { gtrTerrain = object ["ok" .= True]
          , gtrOverlay = Just (object ["seeded" .= (1 :: Int)])
          , gtrMetadata = Just (object ["source" .= ("test" :: Text)])
          })
      }
  }

simulationPlugin :: PluginDef
simulationPlugin = defaultPluginDef
  { pdName = "simulation"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdTick = \ctx -> do
          pcLog ctx "sim:start"
          pcLog ctx "sim:end"
          pure (Right SimulationTickResult
            { strOverlay = object ["population" .= (42 :: Int)]
            , strTerrainWrites = Just (object [])
            })
      }
  }

generatorMetadataPlugin :: PluginDef
generatorMetadataPlugin = defaultPluginDef
  { pdName = "generator-world"
  , pdVersion = "1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = []
      , gdRun = \ctx ->
          pure (Right defaultGeneratorTickResult
            { gtrMetadata = Just (worldMetadataValue (pcWorld ctx))
            })
      }
  }

simulationMetadataPlugin :: PluginDef
simulationMetadataPlugin = defaultPluginDef
  { pdName = "simulation-world"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdTick = \ctx ->
          pure (Right defaultSimulationTickResult
            { strOverlay = worldMetadataValue (pcWorld ctx)
            })
      }
  }

worldMetadataValue :: TerrainWorld -> Value
worldMetadataValue world = object
  [ "chunk_size" .= wcChunkSize (twConfig world)
  , "hex_size_km" .= hexSizeKm (twHexGrid world)
  , "planet_radius" .= pcRadius (twPlanet world)
  , "slice_lat_center" .= wsLatCenter (twSlice world)
  , "slice_lon_center" .= wsLonCenter (twSlice world)
  ]

simOnlyPlugin :: PluginDef
simOnlyPlugin = defaultPluginDef
  { pdName = "sim-only"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdTick = \_ -> pure (Right defaultSimulationTickResult)
      }
  }

-- | Plugin with a queryable and mutable data resource.
dataPlugin :: PluginDef
dataPlugin = defaultPluginDef
  { pdName = "data-test"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsName = "items"
              , drsLabel = "Items"
              , drsHexBound = False
              , drsFields =
                  [ DataFieldDef "name" DFText "Name" False Nothing
                  , DataFieldDef "count" DFInt "Count" False Nothing
                  ]
              , drsOperations = allOperations
              , drsKeyField = "name"
              , drsOverlay = Nothing
              }
          , drdHandler = DataHandler
              { dhQuery = Just $ \_ctx _query ->
                  let records =
                        [ DataRecord (Map.fromList [("name", String "Shield"), ("count", Number 3)])
                        , DataRecord (Map.fromList [("name", String "Potion"), ("count", Number 10)])
                        ]
                  in pure (Right (QueryResult "items" records (Just 2)))
              , dhMutate = Just $ \_ctx mutation ->
                  case mutation of
                    MutCreate record ->
                      pure (Right MutateResult
                        { mrsSuccess = True
                        , mrsError = Nothing
                        , mrsRecord = Just record
                        })
                    _ ->
                      pure (Right MutateResult
                        { mrsSuccess = True
                        , mrsError = Nothing
                        , mrsRecord = Nothing
                        })
              }
          }
      ]
  }

-- | Plugin with a read-only data resource (no mutate handler).
readOnlyDataPlugin :: PluginDef
readOnlyDataPlugin = defaultPluginDef
  { pdName = "readonly-test"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsName = "readonly"
              , drsLabel = "Read Only"
              , drsHexBound = False
              , drsFields = [DataFieldDef "id" DFInt "ID" False Nothing]
              , drsOperations = noOperations { doList = True }
              , drsKeyField = "id"
              , drsOverlay = Nothing
              }
          , drdHandler = noDataHandler
              { dhQuery = Just $ \_ctx _query ->
                  pure (Right (QueryResult "readonly" [] Nothing))
              }
          }
      ]
  }

-- | Plugin whose query handler always returns an error.
failingDataPlugin :: PluginDef
failingDataPlugin = defaultPluginDef
  { pdName = "failing-test"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsName = "broken"
              , drsLabel = "Broken"
              , drsHexBound = False
              , drsFields = [DataFieldDef "x" DFInt "X" False Nothing]
              , drsOperations = noOperations { doList = True }
              , drsKeyField = "x"
              , drsOverlay = Nothing
              }
          , drdHandler = noDataHandler
              { dhQuery = Just $ \_ctx _query ->
                  pure (Left "database connection failed")
              }
          }
      ]
  }
