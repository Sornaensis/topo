{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Runner (spec) where

import Control.Concurrent (MVar, forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Data.Aeson (Value(..), (.:), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes (parseMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import System.Directory
  ( createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  , withCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv, withArgs)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), Handle, hClose, hSetBinaryMode, hSetBuffering, openTempFile)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec

import Topo.Plugin.DataResource
  ( DataFieldDef(..), DataFieldType(..)
  , DataOperations(..), DataResourceSchema(..)
  , currentDataResourceSchemaVersion, defaultDataResourceVersion, defaultDataPagination
  , noOperations, allOperations
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..), DataQuery(..), DataRecord(..), DataResourceErrorCode(..)
  , MutateResource(..), MutateResult(..)
  , QueryResource(..), QueryResult(..)
  , dataResourceErrorCodeText
  )
import Topo.Plugin.RPC (terrainWorldToPayload)
import Topo.Plugin.RPC.Protocol
  ( GeneratorResult(..)
  , Handshake(..), HandshakeAck(..)
  , Heartbeat(..), HealthStatus(..)
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , PluginError(..)
  , PluginLog(..)
  , PluginProgress(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , SimulationResult(..)
  , currentProtocolVersion
  , handshakeAuthProof
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..), RPCManifestRuntime(..), RPCSimulationDecl(..)
  , RPCRestartMode(..), RPCStartPolicy(..), RPCUIHints(..)
  , defaultRPCExternalDataSourceStatus
  , defaultRPCStartPolicy, defaultRPCUIHints
  , manifestV3, parseManifestFile, validateManifest
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , revokedExternalDataSourceStatus
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , TransportConfig(..)
  , TransportEndpoint(..)
  , TransportServer(..)
  , closeTransport
  , defaultTransportConfig
  , endpointKindText
  , openPluginServer
  , pluginAuthTokenEnv
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginSessionEnv
  , recvMessage
  , sendMessage
  )
import Topo.Plugin.SDK.Runner
  ( generateManifest
  , pluginManifestFileName
  , runPlugin
  , runPluginSession
  , runPluginWithManifestCommand
  , writePluginManifestToDirectory
  )
import Topo.Plugin.SDK.Types
import Topo.Simulation.Schedule (SimulationCatchUpPolicy(..), SimulationScheduleDecl(..), hourlyScheduleDecl)
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorldWithPlanet)

spec :: Spec
spec = describe "SDK runner pipe integration" $ do
  it "generates manifest v3 metadata, runtime bounds, and UI hints" $ do
    let manifest = generateManifest generatorPlugin
    rmManifestVersion manifest `shouldBe` manifestV3
    rmrProtocolMin (rmRuntime manifest) `shouldBe` currentProtocolVersion
    rmrProtocolMax (rmRuntime manifest) `shouldBe` currentProtocolVersion
    ruiDisplayName (rmUiHints manifest) `shouldBe` Just (pdName generatorPlugin)
    rmExternalDataSources manifest `shouldBe` []
    rmExternalDataSourceRefs manifest `shouldBe` []
    validateManifest manifest `shouldBe` []

  it "maps PluginDef v3 manifest fields without hand-edited compatibility JSON" $ do
    let customPolicy = defaultRPCStartPolicy
          { rspAutoStart = False
          , rspRestartMode = RestartNever
          }
        manifest = generateManifest (generatorPlugin
          { pdDescription = Just "SDK generated generator manifest"
          , pdRuntimeTopoMin = Just "1.0"
          , pdRuntimeTopoMax = Just "1.x"
          , pdUiHints = defaultRPCUIHints
              { ruiDisplayName = Just "Generator Fixture"
              , ruiCategory = Just "Generation"
              }
          , pdStartPolicy = customPolicy
          })
    rmDescription manifest `shouldBe` "SDK generated generator manifest"
    rmrTopoMin (rmRuntime manifest) `shouldBe` Just "1.0"
    rmrTopoMax (rmRuntime manifest) `shouldBe` Just "1.x"
    ruiDisplayName (rmUiHints manifest) `shouldBe` Just "Generator Fixture"
    ruiCategory (rmUiHints manifest) `shouldBe` Just "Generation"
    rmStartPolicy manifest `shouldBe` customPolicy
    validateManifest manifest `shouldBe` []

  it "writes a PluginDef manifest without starting the RPC transport loop" $
    withTempDir "topo-sdk-manifest-cwd" $ \cwd ->
      withTempDir "topo-sdk-manifest-install" $ \pluginDir -> do
        withCurrentDirectory cwd $
          withArgs ["--topo-write-manifest", pluginDir] $
            runPluginWithManifestCommand generatorPlugin
        let manifestPath = pluginDir </> pluginManifestFileName
            cwdManifestPath = cwd </> pluginManifestFileName
        doesFileExist manifestPath `shouldReturn` True
        doesFileExist cwdManifestPath `shouldReturn` False
        readGeneratedManifest manifestPath $ \manifest -> do
          rmName manifest `shouldBe` pdName generatorPlugin
          validateManifest manifest `shouldBe` []

  it "writes manifest.json directly to a plugin install directory" $
    withTempDir "topo-sdk-manifest-directory" $ \pluginDir -> do
      manifestPath <- writePluginManifestToDirectory pluginDir simulationPlugin
      manifestPath `shouldBe` pluginDir </> pluginManifestFileName
      doesFileExist manifestPath `shouldReturn` True
      readGeneratedManifest manifestPath $ \manifest -> do
        rmName manifest `shouldBe` pdName simulationPlugin
        rmOverlay manifest `shouldSatisfy` maybe False (const True)
        validateManifest manifest `shouldBe` []

  it "infers overlay simulation capabilities without implicit terrain writes" $ do
    let manifest = generateManifest simulationPlugin
    rmCapabilities manifest `shouldSatisfy` elem CapReadTerrain
    rmCapabilities manifest `shouldSatisfy` elem CapReadOverlay
    rmCapabilities manifest `shouldSatisfy` elem CapWriteOverlay
    rmCapabilities manifest `shouldSatisfy` notElem CapWriteTerrain
    (rsdSchedule <$> rmSimulation manifest) `shouldBe` Just hourlyScheduleDecl
    validateManifest manifest `shouldBe` []

  it "emits explicit terrain write capability when a simulation requests it" $ do
    let manifest = generateManifest (simulationPlugin
          { pdName = "terrain-writer"
          , pdCapabilities = [CapWriteTerrain]
          })
    rmCapabilities manifest `shouldSatisfy` elem CapWriteTerrain
    validateManifest manifest `shouldBe` []

  it "emits custom simulation schedule declarations" $ do
    let schedule = SimulationScheduleDecl
          { schedDeclIntervalTicks = 6
          , schedDeclPhaseTicks = 2
          , schedDeclCatchUpPolicy = SkipMissed
          }
        plugin = simulationPlugin
          { pdSimulation = fmap (\sim -> sim { sdSchedule = Just schedule }) (pdSimulation simulationPlugin)
          }
        manifest = generateManifest plugin
    (rsdSchedule <$> rmSimulation manifest) `shouldBe` Just schedule
    (Aeson.toJSON <$> rmSimulation manifest) `shouldBe` Just (object
      [ "dependencies" .= ([] :: [Text])
      , "interval_ticks" .= (6 :: Word64)
      , "phase_ticks" .= (2 :: Word64)
      , "catch_up" .= ("skip_missed" :: Text)
      ])
    validateManifest manifest `shouldBe` []

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

  it "emits generator progress before generator_result with the request id" $
    withTransportPair $ \host plugin -> do
      done <- startSession generatorProgressPlugin plugin
      sendEnvelope host (generatorInvoke 78)
      progressEnv <- recvEnvelope host
      resultEnv <- recvEnvelope host
      envType progressEnv `shouldBe` MsgProgress
      envRequestId progressEnv `shouldBe` Just 78
      progress <- decodeProgress progressEnv
      ppMessage progress `shouldBe` "generator:halfway"
      ppFraction progress `shouldBe` 0.5
      envType resultEnv `shouldBe` MsgGeneratorResult
      envRequestId resultEnv `shouldBe` Just 78
      shutdownAndWait host done

  it "sanitizes progress fractions before JSON encoding" $
    withTransportPair $ \host plugin -> do
      done <- startSession generatorSanitizingProgressPlugin plugin
      sendEnvelope host (generatorInvoke 79)
      progressEnvs <- traverse (\_ -> recvEnvelope host) [1 .. 5 :: Int]
      resultEnv <- recvEnvelope host
      mapM_ (\env -> envType env `shouldBe` MsgProgress) progressEnvs
      decoded <- traverse decodeProgress progressEnvs
      map (\p -> (ppMessage p, ppFraction p)) decoded `shouldBe`
        [ ("below", 0)
        , ("above", 1)
        , ("nan", 0)
        , ("positive-infinity", 1)
        , ("negative-infinity", 0)
        ]
      envType resultEnv `shouldBe` MsgGeneratorResult
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
          srTerrainWrites result `shouldBe` Nothing
      shutdownAndWait host done

  it "emits simulation progress before simulation_result with the request id" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationProgressPlugin plugin
      sendEnvelope host simulationInvoke
      progressEnv <- recvEnvelope host
      resultEnv <- recvEnvelope host
      envType progressEnv `shouldBe` MsgProgress
      envRequestId progressEnv `shouldBe` Just 100
      progress <- decodeProgress progressEnv
      ppMessage progress `shouldBe` "simulation:tick"
      ppFraction progress `shouldBe` 0.25
      envType resultEnv `shouldBe` MsgSimulationResult
      envRequestId resultEnv `shouldBe` Just 100
      shutdownAndWait host done

  it "emits query_resource progress before query_result with the request id" $
    withTransportPair $ \host plugin -> do
      done <- startSession progressDataPlugin plugin
      let qr = RPCEnvelope
            { envType = MsgQueryResource
            , envPayload = Aeson.toJSON QueryResource
                { qrResource = "items"
                , qrQuery = QueryAll
                , qrPageSize = Nothing
                , qrPageOffset = Nothing
                }
            , envRequestId = Just 142
            }
      sendEnvelope host qr
      progressEnv <- recvEnvelope host
      resultEnv <- recvEnvelope host
      envType progressEnv `shouldBe` MsgProgress
      envRequestId progressEnv `shouldBe` Just 142
      progress <- decodeProgress progressEnv
      ppMessage progress `shouldBe` "query:loading"
      ppFraction progress `shouldBe` 0.4
      envType resultEnv `shouldBe` MsgQueryResult
      envRequestId resultEnv `shouldBe` Just 142
      shutdownAndWait host done

  it "emits mutate_resource progress before mutate_result with the request id" $
    withTransportPair $ \host plugin -> do
      done <- startSession progressDataPlugin plugin
      let record = DataRecord (Map.fromList [("name", String "Lantern")])
          mr = RPCEnvelope
            { envType = MsgMutateResource
            , envPayload = Aeson.toJSON MutateResource
                { mrResource = "items"
                , mrMutation = MutCreate record
                }
            , envRequestId = Just 143
            }
      sendEnvelope host mr
      progressEnv <- recvEnvelope host
      resultEnv <- recvEnvelope host
      envType progressEnv `shouldBe` MsgProgress
      envRequestId progressEnv `shouldBe` Just 143
      progress <- decodeProgress progressEnv
      ppMessage progress `shouldBe` "mutate:saving"
      ppFraction progress `shouldBe` 0.6
      envType resultEnv `shouldBe` MsgMutateResult
      envRequestId resultEnv `shouldBe` Just 143
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
                , hsAuthChallenge = Nothing
                }
            , envRequestId = Just 41
            }
      sendEnvelope host hs
      env <- recvEnvelope host
      envType env `shouldBe` MsgHandshakeAck
      envRequestId env `shouldBe` Just 41
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (ack :: HandshakeAck) -> do
          length (haResources ack) `shouldBe` 1
          case haResources ack of
            [resource] -> drsName resource `shouldBe` "items"
            _ -> expectationFailure "expected exactly one data resource in handshake ack"
      shutdownAndWait host done

  it "runPlugin reads launch session/auth environment and returns a challenge proof" $
    withTempDir "topo-sdk-launch-auth" $ \cwd ->
      withTransportServer "sdk-launch-auth" $ \server ->
        withPluginLaunchEnvironment (tsEndpoint server) "session-sdk" "token-sdk" $
          withCurrentDirectory cwd $ do
            done <- newEmptyMVar
            _ <- forkFinally (runPlugin dataPlugin) (\_ -> putMVar done ())
            host <- requireServerAccept server
            let challenge = "nonce-sdk"
                hs = RPCEnvelope
                  { envType = MsgHandshake
                  , envPayload = Aeson.toJSON Handshake
                      { hsProtocolVersion = currentProtocolVersion
                      , hsWorldPath = Nothing
                      , hsHostCapabilities = ["query", "mutate", "launch_auth"]
                      , hsAuthChallenge = Just challenge
                      }
                  , envRequestId = Just 411
                  }
            (do
              sendEnvelope host hs
              env <- recvEnvelope host
              envType env `shouldBe` MsgHandshakeAck
              envRequestId env `shouldBe` Just 411
              case Aeson.fromJSON (envPayload env) of
                Aeson.Error err -> expectationFailure err
                Aeson.Success (ack :: HandshakeAck) -> do
                  haSessionId ack `shouldBe` Just "session-sdk"
                  haAuthProof ack `shouldBe` Just (handshakeAuthProof "session-sdk" "token-sdk" challenge)
              shutdownAndWait host done)
              `finally` closeTransport host

  it "echoes request ids on heartbeat and health responses" $
    withTransportPair $ \host plugin -> do
      done <- startSession dataPlugin plugin
      sendEnvelope host (RPCEnvelope
        { envType = MsgHeartbeat
        , envPayload = Aeson.toJSON (Heartbeat { hbStatus = "ping" })
        , envRequestId = Just 501
        })
      heartbeat <- recvEnvelope host
      envType heartbeat `shouldBe` MsgHeartbeat
      envRequestId heartbeat `shouldBe` Just 501
      case Aeson.fromJSON (envPayload heartbeat) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (hb :: Heartbeat) -> hbStatus hb `shouldBe` "ok"
      sendEnvelope host (RPCEnvelope
        { envType = MsgHealthCheck
        , envPayload = object []
        , envRequestId = Just 502
        })
      health <- recvEnvelope host
      envType health `shouldBe` MsgHealthStatus
      envRequestId health `shouldBe` Just 502
      case Aeson.fromJSON (envPayload health) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (status :: HealthStatus) -> do
          hstHealthy status `shouldBe` True
          hstMessage status `shouldBe` "ok"
      shutdownAndWait host done

  it "returns external data-source status reports with provider, grant, scope, reference, and diagnostics" $
    withTransportPair $ \host plugin -> do
      done <- startSession externalStatusPlugin plugin
      sendEnvelope host (RPCEnvelope
        { envType = MsgExternalDataSourceStatusRequest
        , envPayload = Aeson.toJSON RPCExternalDataSourceStatusRequest
            { redssrProviderId = Just "external-status"
            , redssrConsumerId = Nothing
            , redssrSources = ["terrain.catalog"]
            , redssrGrants = ["terrain-catalog-read"]
            , redssrIncludeDiagnostics = True
            , redssrReference = Nothing
            }
        , envRequestId = Just 503
        })
      statusEnv <- recvEnvelope host
      envType statusEnv `shouldBe` MsgExternalDataSourceStatus
      envRequestId statusEnv `shouldBe` Just 503
      case Aeson.fromJSON (envPayload statusEnv) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (report :: RPCExternalDataSourceStatusReport) ->
          case redssReportStatuses report of
            [entry] -> do
              redsstProviderId entry `shouldBe` "external-status"
              redsstSource entry `shouldBe` "terrain.catalog"
              redsstGrant entry `shouldBe` Just "terrain-catalog-read"
              redsstCapabilityScope entry `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
              redsstReference entry `shouldBe` Just (object ["grant" .= ("terrain-catalog-read" :: Text)])
              redsstDiagnostics entry `shouldBe` Just (object ["reportedBy" .= ("external-status" :: Text)])
            other -> expectationFailure ("expected one status entry, got " <> show other)
      shutdownAndWait host done

  it "dispatches external data-source grants and revocations to SDK callbacks" $
    withTransportPair $ \host plugin -> do
      grantSeen <- newEmptyMVar
      revocationSeen <- newEmptyMVar
      done <- startSession (externalCallbackPlugin grantSeen revocationSeen) plugin
      sendEnvelope host (RPCEnvelope
        { envType = MsgExternalDataSourceGrant
        , envPayload = Aeson.toJSON externalGrantMessage
        , envRequestId = Nothing
        })
      receivedGrant <- timeout 1000000 (takeMVar grantSeen)
      receivedGrant `shouldBe` Just externalGrantMessage
      sendEnvelope host (RPCEnvelope
        { envType = MsgExternalDataSourceRevoke
        , envPayload = Aeson.toJSON externalGrantRevocation
        , envRequestId = Nothing
        })
      receivedRevocation <- timeout 1000000 (takeMVar revocationSeen)
      receivedRevocation `shouldBe` Just externalGrantRevocation
      shutdownAndWait host done

  it "correlates external grant callback failures when a request id is supplied" $
    withTransportPair $ \host plugin -> do
      done <- startSession externalFailingCallbackPlugin plugin
      sendEnvelope host (RPCEnvelope
        { envType = MsgExternalDataSourceGrant
        , envPayload = Aeson.toJSON externalGrantMessage
        , envRequestId = Just 504
        })
      errEnv <- recvEnvelope host
      envType errEnv `shouldBe` MsgError
      envRequestId errEnv `shouldBe` Just 504
      case Aeson.fromJSON (envPayload errEnv) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) ->
          peMessage pluginErr `shouldSatisfy` Text.isInfixOf "grant handler failed"
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
            , envRequestId = Just 42
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
            , envRequestId = Just 43
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) -> do
          peCode pluginErr `shouldBe` 1001
          peMessage pluginErr `shouldBe` "Unknown resource: nonexistent"
          expectDataResourceError env ResourceNotFound
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
            , envRequestId = Just 44
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
            , envRequestId = Just 45
            }
      sendEnvelope host mr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) -> do
          peCode pluginErr `shouldBe` 1002
          peMessage pluginErr `shouldBe` "Resource 'readonly' does not support create mutations"
          expectDataResourceError env OperationNotSupported
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
            , envRequestId = Just 46
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) -> do
          peCode pluginErr `shouldBe` 1099
          peMessage pluginErr `shouldBe` "database connection failed"
          expectDataResourceError env DataResourceInternalError
      shutdownAndWait host done

  it "preserves standardized data-resource failures returned by handlers" $
    withTransportPair $ \host plugin -> do
      done <- startSession standardizedFailingDataPlugin plugin
      let qr = RPCEnvelope
            { envType = MsgQueryResource
            , envPayload = Aeson.toJSON QueryResource
                { qrResource = "missing"
                , qrQuery = QueryAll
                , qrPageSize = Nothing
                , qrPageOffset = Nothing
                }
            , envRequestId = Just 47
            }
      sendEnvelope host qr
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success (pluginErr :: PluginError) -> do
          peCode pluginErr `shouldBe` 1003
          peMessage pluginErr `shouldBe` "missing"
          expectDataResourceError env RecordNotFound
      shutdownAndWait host done

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = bracket setup cleanup action
  where
    setup = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp label
      hClose handle
      removeFile path
      createDirectory path
      pure path
    cleanup dir = removePathForcibly dir

readGeneratedManifest :: FilePath -> (RPCManifest -> Expectation) -> Expectation
readGeneratedManifest path check = do
  result <- parseManifestFile path
  case result of
    Left err -> expectationFailure ("manifest parse failed: " <> Text.unpack err)
    Right manifest -> check manifest

withTransportPair :: (Transport -> Transport -> IO a) -> IO a
withTransportPair action = do
  (host, plugin) <- mkTransportPair
  action host plugin `finally` do
    closeTransport host
    closeTransport plugin

withTransportServer :: Text -> (TransportServer -> IO a) -> IO a
withTransportServer name = bracket acquire tsClose
  where
    acquire = do
      serverResult <- openPluginServer defaultTransportConfig { tcTimeout = 1000 } name
      case serverResult of
        Left err -> expectationFailure ("openPluginServer failed: " <> show err) >> fail "openPluginServer"
        Right server -> pure server

requireServerAccept :: TransportServer -> IO Transport
requireServerAccept server = do
  acceptResult <- tsAccept server
  case acceptResult of
    Left err -> expectationFailure ("accept failed: " <> show err) >> fail "accept"
    Right transport -> pure transport

withPluginLaunchEnvironment :: TransportEndpoint -> String -> String -> IO a -> IO a
withPluginLaunchEnvironment endpoint sessionId authToken = bracket setup restore . const
  where
    setup = do
      oldEndpoint <- lookupEnv pluginEndpointEnv
      oldKind <- lookupEnv pluginEndpointKindEnv
      oldSession <- lookupEnv pluginSessionEnv
      oldAuthToken <- lookupEnv pluginAuthTokenEnv
      setEnv pluginEndpointEnv (teAddress endpoint)
      setEnv pluginEndpointKindEnv (Text.unpack (endpointKindText (teKind endpoint)))
      setEnv pluginSessionEnv sessionId
      setEnv pluginAuthTokenEnv authToken
      pure (oldEndpoint, oldKind, oldSession, oldAuthToken)
    restore (oldEndpoint, oldKind, oldSession, oldAuthToken) = do
      restoreEnv pluginEndpointEnv oldEndpoint
      restoreEnv pluginEndpointKindEnv oldKind
      restoreEnv pluginSessionEnv oldSession
      restoreEnv pluginAuthTokenEnv oldAuthToken
    restoreEnv key = maybe (unsetEnv key) (setEnv key)

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

expectDataResourceError :: RPCEnvelope -> DataResourceErrorCode -> IO ()
expectDataResourceError envelope expected =
  AesonTypes.parseMaybe (Aeson.withObject "PluginError" (.: "data_resource_error")) (envPayload envelope)
    `shouldBe` Just (dataResourceErrorCodeText expected)

decodeLog :: RPCEnvelope -> IO Text
decodeLog envelope =
  case Aeson.fromJSON (envPayload envelope) of
    Aeson.Error err -> expectationFailure err >> fail "log decode"
    Aeson.Success (pluginLog :: PluginLog) -> pure (plMessage pluginLog)

decodeProgress :: RPCEnvelope -> IO PluginProgress
decodeProgress envelope =
  case Aeson.fromJSON (envPayload envelope) of
    Aeson.Error err -> expectationFailure err >> fail "progress decode"
    Aeson.Success progress -> pure progress

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
  , envRequestId = Just seed
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
  , envRequestId = Just 100
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
  sendEnvelope host (RPCEnvelope MsgShutdown (object []) Nothing)
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

generatorProgressPlugin :: PluginDef
generatorProgressPlugin = defaultPluginDef
  { pdName = "generator-progress"
  , pdVersion = "1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = []
      , gdRun = \ctx -> do
          reportPluginProgress ctx "generator:halfway" 0.5
          pure (Right defaultGeneratorTickResult)
      }
  }

generatorSanitizingProgressPlugin :: PluginDef
generatorSanitizingProgressPlugin = defaultPluginDef
  { pdName = "generator-progress-sanitize"
  , pdVersion = "1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = []
      , gdRun = \ctx -> do
          let nan = 0 / 0 :: Double
              positiveInfinity = 1 / 0 :: Double
              negativeInfinity = -1 / 0 :: Double
          reportPluginProgress ctx "below" (-0.25)
          pcProgress ctx "above" 1.25
          reportPluginProgress ctx "nan" nan
          reportPluginProgress ctx "positive-infinity" positiveInfinity
          reportPluginProgress ctx "negative-infinity" negativeInfinity
          pure (Right defaultGeneratorTickResult)
      }
  }

simulationPlugin :: PluginDef
simulationPlugin = defaultPluginDef
  { pdName = "simulation"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdSchedule = Nothing
      , sdTick = \ctx -> do
          pcLog ctx "sim:start"
          pcLog ctx "sim:end"
          pure (Right SimulationTickResult
            { strOverlay = object ["population" .= (42 :: Int)]
            , strTerrainWrites = Nothing
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
      , sdSchedule = Just hourlyScheduleDecl
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

simulationProgressPlugin :: PluginDef
simulationProgressPlugin = defaultPluginDef
  { pdName = "simulation-progress"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdSchedule = Just hourlyScheduleDecl
      , sdTick = \ctx -> do
          pcProgress ctx "simulation:tick" 0.25
          pure (Right defaultSimulationTickResult)
      }
  }

simOnlyPlugin :: PluginDef
simOnlyPlugin = defaultPluginDef
  { pdName = "sim-only"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdSchedule = Just hourlyScheduleDecl
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
              { drsSchemaVersion = currentDataResourceSchemaVersion
              , drsResourceVersion = defaultDataResourceVersion
              , drsName = "items"
              , drsLabel = "Items"
              , drsHexBound = False
              , drsFields =
                  [ DataFieldDef "name" DFText "Name" False Nothing
                  , DataFieldDef "count" DFInt "Count" False Nothing
                  ]
              , drsOperations = allOperations
              , drsKeyField = "name"
              , drsOverlay = Nothing
              , drsPagination = defaultDataPagination
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
                        , mrsErrorCode = Nothing
                        })
                    _ ->
                      pure (Right MutateResult
                        { mrsSuccess = True
                        , mrsError = Nothing
                        , mrsRecord = Nothing
                        , mrsErrorCode = Nothing
                        })
              }
          }
      ]
  }

progressDataPlugin :: PluginDef
progressDataPlugin = defaultPluginDef
  { pdName = "data-progress"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsSchemaVersion = currentDataResourceSchemaVersion
              , drsResourceVersion = defaultDataResourceVersion
              , drsName = "items"
              , drsLabel = "Items"
              , drsHexBound = False
              , drsFields = [DataFieldDef "name" DFText "Name" False Nothing]
              , drsOperations = allOperations
              , drsKeyField = "name"
              , drsOverlay = Nothing
              , drsPagination = defaultDataPagination
              }
          , drdHandler = DataHandler
              { dhQuery = Just $ \ctx _query -> do
                  reportPluginProgress ctx "query:loading" 0.4
                  pure (Right (QueryResult "items" [] (Just 0)))
              , dhMutate = Just $ \ctx _mutation -> do
                  pcProgress ctx "mutate:saving" 0.6
                  pure (Right MutateResult
                    { mrsSuccess = True
                    , mrsError = Nothing
                    , mrsRecord = Nothing
                    , mrsErrorCode = Nothing
                    })
              }
          }
      ]
  }

externalStatusPlugin :: PluginDef
externalStatusPlugin = defaultPluginDef
  { pdName = "external-status"
  , pdVersion = "1.0"
  , pdExternalDataSources =
      [ RPCExternalDataSourceDecl
          { redsdName = "terrain.catalog"
          , redsdLabel = "Terrain Catalog"
          , redsdDescription = "Backend-neutral status fixture"
          , redsdKind = "catalog"
          , redsdCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
          , redsdResources = ["terrain_sources"]
          , redsdStatus = externalReadyStatus
          , redsdConnection = Just (object ["handle" .= ("provider-owned:terrain.catalog" :: Text)])
          , redsdConfigRefs = []
          , redsdGrants =
              [ RPCExternalDataSourceGrant
                  { redsgName = "terrain-catalog-read"
                  , redsgAccess = [ExternalAccessRead]
                  , redsgCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
                  , redsgResources = ["terrain_sources"]
                  , redsgStatus = externalReadyStatus
                  , redsgReference = Just (object ["grant" .= ("terrain-catalog-read" :: Text)])
                  , redsgConfigRefs = []
                  }
              ]
          , redsdUiHints = defaultRPCUIHints
          }
      ]
  }

externalCallbackPlugin
  :: MVar RPCExternalDataSourceGrantMessage
  -> MVar RPCExternalDataSourceGrantRevocation
  -> PluginDef
externalCallbackPlugin grantSeen revocationSeen = externalStatusPlugin
  { pdName = "external-callback"
  , pdOnExternalDataSourceGrant = Just (putMVar grantSeen)
  , pdOnExternalDataSourceRevocation = Just (putMVar revocationSeen)
  }

externalFailingCallbackPlugin :: PluginDef
externalFailingCallbackPlugin = externalStatusPlugin
  { pdName = "external-callback-failing"
  , pdOnExternalDataSourceGrant = Just (\_ -> fail "grant callback failed")
  }

externalReadyStatus :: RPCExternalDataSourceStatus
externalReadyStatus = defaultRPCExternalDataSourceStatus
  { redssState = ExternalStatusReady
  , redssMessage = Just "fixture provider ready"
  , redssProviderId = Just "external-status"
  , redssAvailability = Just ExternalAvailabilityAvailable
  , redssHealth = Just ExternalHealthHealthy
  , redssAccessMode = Just ExternalAccessModeReadOnly
  , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
  , redssDiagnostics = Just (object ["reportedBy" .= ("external-status" :: Text)])
  }

externalGrantMessage :: RPCExternalDataSourceGrantMessage
externalGrantMessage = RPCExternalDataSourceGrantMessage
  { redsgmOperationId = Just "sdk-runner-grant-op"
  , redsgmOperationEpoch = Just 1
  , redsgmProviderId = "external-status"
  , redsgmConsumerId = Just "external-callback"
  , redsgmSource = "terrain.catalog"
  , redsgmGrant = "terrain-catalog-read"
  , redsgmAccess = [ExternalAccessRead]
  , redsgmResources = ["terrain_sources"]
  , redsgmCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
  , redsgmStatus = externalReadyStatus
  , redsgmReference = Just (object ["grant" .= ("terrain-catalog-read" :: Text)])
  , redsgmConfigRefs = []
  , redsgmDiagnostics = Just (object ["reportedBy" .= ("host" :: Text)])
  }

externalGrantRevocation :: RPCExternalDataSourceGrantRevocation
externalGrantRevocation = RPCExternalDataSourceGrantRevocation
  { redsrvOperationId = Just "sdk-runner-revoke-op"
  , redsrvOperationEpoch = Just 2
  , redsrvProviderId = "external-status"
  , redsrvConsumerId = Just "external-callback"
  , redsrvSource = "terrain.catalog"
  , redsrvGrant = "terrain-catalog-read"
  , redsrvReason = Just "provider unavailable"
  , redsrvStatus = revokedExternalDataSourceStatus "external-status" (Just "provider unavailable")
  , redsrvReference = Just (object ["grant" .= ("terrain-catalog-read" :: Text)])
  , redsrvDiagnostics = Just (object ["reportedBy" .= ("host" :: Text)])
  }

-- | Plugin with a read-only data resource (no mutate handler).
readOnlyDataPlugin :: PluginDef
readOnlyDataPlugin = defaultPluginDef
  { pdName = "readonly-test"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsSchemaVersion = currentDataResourceSchemaVersion
              , drsResourceVersion = defaultDataResourceVersion
              , drsName = "readonly"
              , drsLabel = "Read Only"
              , drsHexBound = False
              , drsFields = [DataFieldDef "id" DFInt "ID" False Nothing]
              , drsOperations = noOperations { doList = True }
              , drsKeyField = "id"
              , drsOverlay = Nothing
              , drsPagination = defaultDataPagination
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
              { drsSchemaVersion = currentDataResourceSchemaVersion
              , drsResourceVersion = defaultDataResourceVersion
              , drsName = "broken"
              , drsLabel = "Broken"
              , drsHexBound = False
              , drsFields = [DataFieldDef "x" DFInt "X" False Nothing]
              , drsOperations = noOperations { doList = True }
              , drsKeyField = "x"
              , drsOverlay = Nothing
              , drsPagination = defaultDataPagination
              }
          , drdHandler = noDataHandler
              { dhQuery = Just $ \_ctx _query ->
                  pure (Left "database connection failed")
              }
          }
      ]
  }

standardizedFailingDataPlugin :: PluginDef
standardizedFailingDataPlugin = defaultPluginDef
  { pdName = "standardized-failing-test"
  , pdVersion = "1.0"
  , pdDataResources =
      [ DataResourceDef
          { drdSchema = DataResourceSchema
              { drsSchemaVersion = currentDataResourceSchemaVersion
              , drsResourceVersion = defaultDataResourceVersion
              , drsName = "missing"
              , drsLabel = "Missing"
              , drsHexBound = False
              , drsFields = [DataFieldDef "id" DFInt "ID" False Nothing]
              , drsOperations = noOperations { doList = True }
              , drsKeyField = "id"
              , drsOverlay = Nothing
              , drsPagination = defaultDataPagination
              }
          , drdHandler = noDataHandler
              { dhQuery = Just $ \_ctx _query ->
                  pure (Left "record_not_found: missing")
              }
          }
      ]
  }
