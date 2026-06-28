{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.PluginManager (spec, runFixtureCli) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally, throwIO)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, get, newActorSystem, shutdownActorSystem)
import System.Directory
  ( Permissions(..)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , getPermissions
  , getTemporaryDirectory
  , removePathForcibly
  , setPermissions
  )
import System.Environment (getArgs, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (stdin, stdout)
import System.Process (ProcessHandle, getProcessExitCode)
import System.Timeout (timeout)
import Test.Hspec

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManager
  , PluginStatus(..)
  , discoverPlugins
  , getDisabledPlugins
  , getLoadedPlugins
  , getPluginOverlaySchemas
  , getPluginStages
  , mutatePluginResource
  , queryPluginResource
  , refreshManifests
  , setDisabledPlugins
  , shutdownPlugins
  )
import Topo.Calendar (CalendarDate(..), defaultWorldTime)
import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay, emptyOverlay)
import Topo.Overlay.Schema (OverlayDeps(..), OverlaySchema(..), OverlayStorage(..))
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataResourceSchema(..)
  , currentDataResourceSchemaVersion
  , defaultDataPagination
  , defaultDataResourceVersion
  , noOperations
  )
import Topo.Plugin.RPC (RPCConnection, invokeGenerator, invokeSimulation)
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  )
import Topo.Plugin.RPC.Protocol
  ( HandshakeAck(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , currentProtocolVersion
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( closeTransport
  , connectPluginFromEnvironment
  , pluginAuthTokenEnv
  , pluginDataRootEnv
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginIdEnv
  , pluginProtocolEnv
  , pluginSessionEnv
  , pluginStdioCompatibilityEnv
  , pluginWorldIdEnv
  , recvMessage
  , sendMessage
  )
import Topo.Simulation (SimContext(..))
import Topo.Types (WorldConfig(..))
import Topo.World (emptyWorld)

getPluginManager :: ActorSystem -> IO (ActorHandle PluginManager (Protocol PluginManager))
getPluginManager system = get @PluginManager system

spec :: Spec
spec = describe "PluginManager" $ do
  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "launches plugin subprocesses and exposes generator stages cross-platform" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        stages <- getPluginStages pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        map stageName stages `shouldSatisfy` elem (Text.pack testLaunchPluginName)
        pluginStatuses testLaunchPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates testLaunchPluginName loaded `shouldSatisfy` elem LifecycleReady
        pluginLifecycleProtocols testLaunchPluginName loaded `shouldSatisfy` elem (Just currentProtocolVersion)
        let handles = pluginProcessHandles testLaunchPluginName loaded
        null handles `shouldBe` False
        shutdownPlugins pluginManagerHandle
        threadDelay 200000
        loadedAfterShutdown <- getLoadedPlugins pluginManagerHandle
        pluginStatuses testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem PluginDisconnected
        pluginLifecycleStates testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem LifecycleStopped
        mapM_ (assertProcessExited testLaunchPluginName) handles

  it "launches plugins with the complete TOPO_PLUGIN environment over endpoint transport" $ do
    withParentStdioCompatibilityFlag $ do
      withExecutablePluginDir envContractPluginName envContractManifestJSON "env-contract" $ do
        bracket newActorSystem shutdownActorSystem $ \system -> do
          pluginManagerHandle <- getPluginManager system
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses envContractPluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates envContractPluginName loaded `shouldSatisfy` elem LifecycleReady
          pluginLifecycleProtocols envContractPluginName loaded `shouldSatisfy` elem (Just currentProtocolVersion)
          shutdownPlugins pluginManagerHandle
          threadDelay 200000

  it "exposes Starting while public refreshManifests performs supervisor work" $ do
    withExecutablePluginDir refreshTransientPluginName refreshTransientManifestJSON "slow" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        done <- newEmptyMVar
        _ <- forkIO (refreshManifests pluginManagerHandle >> putMVar done ())
        threadDelay 50000
        starting <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName starting `shouldSatisfy` elem LifecycleStarting
        takeMVar done
        failed <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName failed `shouldSatisfy` elem LifecycleFailed

  it "exposes Stopping while public shutdownPlugins performs supervisor work" $ do
    withExecutablePluginDir shutdownTransientPluginName shutdownTransientManifestJSON "slow-shutdown" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName ready `shouldSatisfy` elem LifecycleReady
        done <- newEmptyMVar
        _ <- forkIO (shutdownPlugins pluginManagerHandle >> putMVar done ())
        threadDelay 50000
        stopping <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName stopping `shouldSatisfy` elem LifecycleStopping
        takeMVar done
        stopped <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName stopped `shouldSatisfy` elem LifecycleStopped

  it "reports a protocol-version mismatch as a plugin error" $ do
    withExecutablePluginDir mismatchPluginName mismatchManifestJSON "protocol-mismatch" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses mismatchPluginName loaded `shouldSatisfy` anyPluginErrorContaining "protocol version mismatch"
        pluginLifecycleStates mismatchPluginName loaded `shouldSatisfy` elem LifecycleFailed

  it "surfaces manifest parse diagnostics for missing required fields" $ do
    let pluginName = "copilot-test-plugin-missing-runtime"
    withExecutablePluginDir pluginName (missingRuntimeManifestFor pluginName) "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "runtime"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_parse_failed")

  it "blocks startup when manifest runtime protocol bounds exclude the host" $ do
    let pluginName = "copilot-test-plugin-invalid-protocol"
    withExecutablePluginDir pluginName (invalidProtocolManifestFor pluginName) "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "runtime.protocol"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_validation_failed")
        length (pluginProcessHandles pluginName loaded) `shouldBe` 0

  it "blocks startup when a declared overlay schema cannot be loaded" $ do
    let pluginName = "copilot-test-plugin-missing-overlay-schema"
    withExecutablePluginDir pluginName (missingOverlaySchemaManifestFor pluginName) "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "overlay.schemaFile"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_schema_failed")
        length (pluginProcessHandles pluginName loaded) `shouldBe` 0

  it "validates backend-neutral external data-source declarations with actionable diagnostics" $ do
    let pluginName = "copilot-test-plugin-invalid-external-source"
    withExecutablePluginDir pluginName (invalidExternalSourceManifestFor pluginName) "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "externalDataSources"
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "backend-neutral"
        pluginStatuses pluginName loaded `shouldSatisfy` (not . anyPluginErrorContaining "SQLite")
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_validation_failed")

  it "reports malformed handshake JSON as a plugin error" $ do
    withExecutablePluginDir malformedPluginName malformedManifestJSON "malformed-json" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses malformedPluginName loaded `shouldSatisfy` anyPluginErrorContaining "RPCProtocolError"

  it "reports unexpected handshake responses as plugin errors" $ do
    withExecutablePluginDir badHandshakePluginName badHandshakeManifestJSON "bad-handshake" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses badHandshakePluginName loaded `shouldSatisfy` anyPluginErrorContaining "unexpected response to handshake"
        pluginLifecycleStates badHandshakePluginName loaded `shouldSatisfy` elem LifecycleFailed

  it "reports early plugin exit during startup as a plugin error" $ do
    withExecutablePluginDir crashPluginName crashManifestJSON "early-exit" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses crashPluginName loaded `shouldSatisfy` anyPluginError

  it "reports handshake timeouts as plugin errors" $ do
    withExecutablePluginDir slowPluginName slowManifestJSON "slow" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses slowPluginName loaded `shouldSatisfy` anyPluginErrorContaining "timed out"

  it "honors auto_start=false by leaving runtime capabilities unavailable" $ do
    withExecutablePluginDir autoStartDisabledPluginName autoStartDisabledManifestJSON "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        stages <- getPluginStages pluginManagerHandle
        map stageName stages `shouldNotSatisfy` elem (Text.pack autoStartDisabledPluginName)
        pluginStatuses autoStartDisabledPluginName loaded `shouldSatisfy` elem PluginIdle
        pluginLifecycleStates autoStartDisabledPluginName loaded `shouldSatisfy` elem LifecycleStopped
        timed <- timeout 1000000 $ queryPluginResource pluginManagerHandle (Text.pack autoStartDisabledPluginName) testQuery
        case timed of
          Nothing -> expectationFailure "unavailable data query hung"
          Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "plugin unavailable"
          Just (Right _) -> expectationFailure "unavailable data query unexpectedly succeeded"

  it "omits disabled plugin dependencies from pipeline views" $ do
    withExecutablePluginDir disabledDependencyPluginName disabledDependencyManifestJSON "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses disabledDependencyPluginName loaded `shouldSatisfy` elem PluginConnected
        let disabledName = Text.pack disabledDependencyPluginName
        setDisabledPlugins pluginManagerHandle (Set.singleton disabledName)
        disabled <- getDisabledPlugins pluginManagerHandle
        disabled `shouldSatisfy` Set.member disabledName
        stages <- getPluginStages pluginManagerHandle
        map stageName stages `shouldNotSatisfy` elem disabledName
        setDisabledPlugins pluginManagerHandle Set.empty
        shutdownPlugins pluginManagerHandle

  it "surfaces external data-source provider failures without backend-specific storage assumptions" $ do
    withExecutablePluginDir providerFailedPluginName providerFailedManifestJSON "provider-failed" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses providerFailedPluginName loaded `shouldSatisfy` elem PluginConnected
        timed <- timeout 1000000 $ queryPluginResource pluginManagerHandle (Text.pack providerFailedPluginName) testQuery
        case timed of
          Nothing -> expectationFailure "provider failure query hung"
          Just (Left err) -> do
            err `shouldSatisfy` Text.isInfixOf "external data-source provider failed"
            err `shouldNotSatisfy` Text.isInfixOf "SQLite"
          Just (Right _) -> expectationFailure "provider failure query unexpectedly succeeded"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses providerFailedPluginName observed `shouldSatisfy` anyPluginErrorContaining "external data-source provider failed"
        pluginLifecycleErrorCodes providerFailedPluginName observed `shouldSatisfy` elem (Just "data_query_failed")
        shutdownPlugins pluginManagerHandle

  it "restarts startup failures with policy backoff and then connects" $ do
    withExecutablePluginDir flakyStartPluginName flakyStartManifestJSON "flaky-start" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses flakyStartPluginName loaded `shouldSatisfy` elem PluginConnected
        shutdownPlugins pluginManagerHandle
        count <- readFixtureCount flakyStartPluginName "flaky-start"
        count `shouldBe` 2

  it "stops restarting startup failures after the max restart window is exhausted" $ do
    withExecutablePluginDir restartLimitPluginName restartLimitManifestJSON "counted-early-exit" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses restartLimitPluginName loaded `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes restartLimitPluginName loaded `shouldSatisfy` elem (Just "restart_limit_exceeded")
        count <- readFixtureCount restartLimitPluginName "counted-early-exit"
        count `shouldBe` 2

  it "times out data-resource requests instead of hanging" $ do
    withExecutablePluginDir hangQueryPluginName hangQueryManifestJSON "hang-query" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses hangQueryPluginName loaded `shouldSatisfy` elem PluginConnected
        timed <- timeout 1000000 $ queryPluginResource pluginManagerHandle (Text.pack hangQueryPluginName) testQuery
        case timed of
          Nothing -> expectationFailure "data query hung"
          Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "timed out"
          Just (Right _) -> expectationFailure "hung data query unexpectedly succeeded"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses hangQueryPluginName observed `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes hangQueryPluginName observed `shouldSatisfy` elem (Just "restart_limit_exceeded")
        shutdownPlugins pluginManagerHandle

  it "rejects unsupported data-resource mutations before plugin calls" $ do
    withExecutablePluginDir unsupportedMutationPluginName unsupportedMutationManifestJSON "validation-ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        (do
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses unsupportedMutationPluginName loaded `shouldSatisfy` elem PluginConnected
          timed <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack unsupportedMutationPluginName)
            (MutateResource "records" (MutCreate (record [("id", String "alpha")])))
          case timed of
            Nothing -> expectationFailure "unsupported mutation hung"
            Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "operation_not_supported"
            Just (Right _) -> expectationFailure "unsupported mutation unexpectedly reached plugin"
          observed <- getLoadedPlugins pluginManagerHandle
          pluginStatuses unsupportedMutationPluginName observed `shouldSatisfy` elem PluginConnected)
          `finally` shutdownPlugins pluginManagerHandle

  it "validates inbound data-resource records before plugins can accept them" $ do
    withExecutablePluginDir validationPluginName validationManifestJSON "validation-ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        (do
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses validationPluginName loaded `shouldSatisfy` elem PluginConnected
          timed <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack validationPluginName)
            (MutateResource "records" (MutCreate (record [("id", Number 1), ("name", String "Alpha")])))
          case timed of
            Nothing -> expectationFailure "invalid mutation hung"
            Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "schema_validation_failed"
            Just (Right _) -> expectationFailure "invalid mutation unexpectedly reached plugin"
          observed <- getLoadedPlugins pluginManagerHandle
          pluginStatuses validationPluginName observed `shouldSatisfy` elem PluginConnected)
          `finally` shutdownPlugins pluginManagerHandle

  it "validates plugin-returned data-resource records before exposing them" $ do
    withExecutablePluginDir invalidReturnPluginName invalidReturnManifestJSON "invalid-mutate" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        (do
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses invalidReturnPluginName loaded `shouldSatisfy` elem PluginConnected
          timed <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack invalidReturnPluginName)
            (MutateResource "records" (MutCreate (record [("id", String "alpha"), ("name", String "Alpha")])))
          case timed of
            Nothing -> expectationFailure "invalid plugin record mutation hung"
            Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "schema_validation_failed"
            Just (Right _) -> expectationFailure "invalid plugin record unexpectedly passed validation"
          observed <- getLoadedPlugins pluginManagerHandle
          pluginStatuses invalidReturnPluginName observed `shouldSatisfy` elem PluginConnected)
          `finally` shutdownPlugins pluginManagerHandle

  it "validates data-resource calls against negotiated handshake schemas" $ do
    withExecutablePluginDir negotiatedValidationPluginName negotiatedValidationManifestJSON "negotiated-validation" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        (do
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses negotiatedValidationPluginName loaded `shouldSatisfy` elem PluginConnected
          timed <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack negotiatedValidationPluginName)
            (MutateResource "records" (MutCreate (record [("id", String "alpha"), ("name", String "Alpha")])))
          case timed of
            Nothing -> expectationFailure "negotiated-schema mutation hung"
            Just (Left err) -> expectationFailure ("negotiated schema was not used: " <> Text.unpack err)
            Just (Right mrs) -> mrsSuccess mrs `shouldBe` True
          observed <- getLoadedPlugins pluginManagerHandle
          pluginStatuses negotiatedValidationPluginName observed `shouldSatisfy` elem PluginConnected)
          `finally` shutdownPlugins pluginManagerHandle

  it "surfaces generator crashes without hanging" $ do
    withExecutablePluginDir generatorCrashPluginName generatorCrashManifestJSON "exit-on-generator" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        conn <- case pluginConnections generatorCrashPluginName loaded of
          Just c -> pure c
          Nothing -> expectationFailure "plugin did not connect" >> fail "missing plugin connection"
        timed <- timeout 1000000 $ invokeGenerator conn Aeson.Null
        case timed of
          Nothing -> expectationFailure "generator request hung"
          Just (Left _) -> pure ()
          Just (Right _) -> expectationFailure "crashed generator unexpectedly succeeded"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses generatorCrashPluginName observed `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes generatorCrashPluginName observed `shouldSatisfy` elem (Just "restart_limit_exceeded")
        shutdownPlugins pluginManagerHandle

  it "surfaces simulation crashes without hanging" $ do
    withExecutablePluginDir simulationCrashPluginName simulationCrashManifestJSON "exit-on-simulation" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        conn <- case pluginConnections simulationCrashPluginName loaded of
          Just c -> pure c
          Nothing -> expectationFailure "plugin did not connect" >> fail "missing plugin connection"
        timed <- timeout 1000000 $
          invokeSimulation conn sampleSimContext sampleOverlay (\_ -> pure ()) (\_ -> pure ())
        case timed of
          Nothing -> expectationFailure "simulation request hung"
          Just (Left _) -> pure ()
          Just (Right _) -> expectationFailure "crashed simulation unexpectedly succeeded"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses simulationCrashPluginName observed `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes simulationCrashPluginName observed `shouldSatisfy` elem (Just "restart_limit_exceeded")
        shutdownPlugins pluginManagerHandle

pluginStatuses :: String -> [LoadedPlugin] -> [PluginStatus]
pluginStatuses name loaded =
  [ lpStatus plugin
  | plugin <- loaded
  , lpName plugin == Text.pack name
  ]

pluginLifecycles :: String -> [LoadedPlugin] -> [PluginLifecycleSnapshot]
pluginLifecycles name loaded =
  [ lpLifecycle plugin
  | plugin <- loaded
  , lpName plugin == Text.pack name
  ]

pluginLifecycleStates :: String -> [LoadedPlugin] -> [PluginLifecycleState]
pluginLifecycleStates name = map plsState . pluginLifecycles name

pluginLifecycleProtocols :: String -> [LoadedPlugin] -> [Maybe Int]
pluginLifecycleProtocols name = map plsProtocolVersion . pluginLifecycles name

pluginLifecycleErrorCodes :: String -> [LoadedPlugin] -> [Maybe Text]
pluginLifecycleErrorCodes name = map plsErrorCode . pluginLifecycles name

pluginConnections :: String -> [LoadedPlugin] -> Maybe RPCConnection
pluginConnections _ [] = Nothing
pluginConnections name (plugin:rest)
  | lpName plugin == Text.pack name = case lpConnection plugin of
      Just conn -> Just conn
      Nothing -> Nothing
  | otherwise = pluginConnections name rest

pluginProcessHandles :: String -> [LoadedPlugin] -> [ProcessHandle]
pluginProcessHandles name loaded =
  [ processHandle
  | plugin <- loaded
  , lpName plugin == Text.pack name
  , Just processHandle <- [lpProcessHandle plugin]
  ]

assertProcessExited :: String -> ProcessHandle -> IO ()
assertProcessExited label processHandle = do
  result <- timeout 1000000 waitForExitCode
  case result of
    Nothing -> expectationFailure (label <> " fixture process did not exit after shutdown")
    Just _ -> pure ()
  where
    waitForExitCode = do
      mExitCode <- getProcessExitCode processHandle
      case mExitCode of
        Just exitCode -> pure exitCode
        Nothing -> threadDelay 10000 >> waitForExitCode

anyPluginError :: [PluginStatus] -> Bool
anyPluginError = any $ \case
  PluginError _ -> True
  _ -> False

anyPluginErrorContaining :: Text -> [PluginStatus] -> Bool
anyPluginErrorContaining needle = any $ \case
  PluginError msg -> needle `Text.isInfixOf` msg
  _ -> False

withTestPluginDir :: String -> BS.ByteString -> BS.ByteString -> IO a -> IO a
withTestPluginDir pluginName manifestJSON schemaJSON action =
  withIsolatedPluginHome pluginName $
    bracket setup teardown (const action)
  where
    setup = do
      baseDir <- currentPluginBaseDir
      let pluginDir = baseDir </> pluginName
      resetPluginDir pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> "test.toposchema") schemaJSON
      pure pluginDir

    teardown = removePathForciblyEventually

withParentStdioCompatibilityFlag :: IO a -> IO a
withParentStdioCompatibilityFlag = bracket setup restore . const
  where
    setup = do
      old <- lookupEnv pluginStdioCompatibilityEnv
      setEnv pluginStdioCompatibilityEnv "1"
      pure old
    restore = maybe (unsetEnv pluginStdioCompatibilityEnv) (setEnv pluginStdioCompatibilityEnv)

withExecutablePluginDir :: String -> BS.ByteString -> String -> IO a -> IO a
withExecutablePluginDir pluginName manifestJSON fixtureMode action =
  withIsolatedPluginHome pluginName $
    bracket setup teardown (const action)
  where
    setup = do
      baseDir <- currentPluginBaseDir
      let pluginDir = baseDir </> pluginName
      resetPluginDir pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> "test.toposchema") testSchemaJSON
      writePluginWrapper pluginDir pluginName fixtureMode
      pure pluginDir

    teardown = removePathForciblyEventually

withIsolatedPluginHome :: String -> IO a -> IO a
withIsolatedPluginHome label action =
  bracket setup teardown (const action)
  where
    setup = do
      oldPluginDir <- lookupEnv testPluginDirEnv
      tmp <- getTemporaryDirectory
      now <- getPOSIXTime
      let uniqueSuffix = show (round (now * 1000000) :: Integer)
          root = tmp </> ("topo-plugin-manager-" <> label <> "-" <> uniqueSuffix)
          pluginBase = root </> "plugins"
      removePathForciblyEventually root
      createDirectoryIfMissing True pluginBase
      setEnv testPluginDirEnv pluginBase
      pure (root, oldPluginDir)

    teardown (root, oldPluginDir) = do
      restoreEnv testPluginDirEnv oldPluginDir
      removePathForciblyEventually root

    restoreEnv key = maybe (unsetEnv key) (setEnv key)

removePathForciblyEventually :: FilePath -> IO ()
removePathForciblyEventually path = go (20 :: Int)
  where
    go attemptsLeft =
      removePathForcibly path `catch` \(err :: SomeException) ->
        if attemptsLeft <= 0
          then throwIO err
          else threadDelay 50000 >> go (attemptsLeft - 1)

currentPluginBaseDir :: IO FilePath
currentPluginBaseDir = do
  mOverride <- lookupEnv testPluginDirEnv
  case mOverride of
    Just dir | not (null dir) -> pure dir
    _ -> do
      home <- getHomeDirectory
      pure (home </> ".topo" </> "plugins")

testPluginDirEnv :: String
testPluginDirEnv = "TOPO_PLUGIN_DIR"

resetPluginDir :: FilePath -> IO ()
resetPluginDir pluginDir = do
  exists <- doesDirectoryExist pluginDir
  if exists
    then removePathForcibly pluginDir
    else pure ()
  createDirectoryIfMissing True pluginDir

readFixtureCount :: String -> String -> IO Int
readFixtureCount pluginName label = do
  baseDir <- currentPluginBaseDir
  let path = baseDir </> pluginName </> "data" </> (label <> ".count")
  readCountFile path

incrementFixtureCount :: String -> IO Int
incrementFixtureCount label = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  let path = dataRoot </> (label <> ".count")
  current <- readCountFile path
  let next = current + 1
  writeFile path (show next)
  pure next

readCountFile :: FilePath -> IO Int
readCountFile path = do
  exists <- doesFileExist path
  if not exists
    then pure 0
    else do
      raw <- BSC.unpack <$> BS.readFile path
      pure $ case reads raw of
        [(value, "")] -> value
        _ -> 0

writePluginWrapper :: FilePath -> String -> String -> IO ()
writePluginWrapper pluginDir pluginName fixtureMode = do
  testExe <- getExecutablePath
  if os == "mingw32"
    then do
      let wrapperPath = pluginDir </> (pluginName <> ".cmd")
      writeFile wrapperPath $ unlines
        [ "@echo off"
        , "\"" <> testExe <> "\" --plugin-manager-fixture " <> fixtureMode
        ]
    else do
      let wrapperPath = pluginDir </> pluginName
      writeFile wrapperPath $ unlines
        [ "#!/bin/sh"
        , "exec " <> shellQuote testExe <> " --plugin-manager-fixture " <> shellQuote fixtureMode
        ]
      permissions <- getPermissions wrapperPath
      setPermissions wrapperPath permissions { executable = True }

shellQuote :: String -> String
shellQuote value = "'" <> concatMap quoteChar value <> "'"
  where
    quoteChar '\'' = "'\\''"
    quoteChar c = [c]

runFixtureCli :: IO ()
runFixtureCli = do
  args <- getArgs
  case args of
    ["--plugin-manager-fixture", mode] -> runFixtureMode mode
    _ -> die "usage: topo-seer-test --plugin-manager-fixture <ok|env-contract|protocol-mismatch|malformed-json|bad-handshake|early-exit|slow|slow-shutdown|flaky-start|counted-early-exit|hang-query|provider-failed|validation-ok|invalid-mutate|negotiated-validation|exit-on-generator|exit-on-simulation>"

runFixtureMode :: String -> IO ()
runFixtureMode = \case
  "ok" -> runOkFixture
  "env-contract" -> runEnvContractFixture
  "protocol-mismatch" -> runOneShotAckFixture (currentProtocolVersion + 1)
  "malformed-json" -> runMalformedJsonFixture
  "bad-handshake" -> runBadHandshakeFixture
  "early-exit" -> exitFailure
  "slow" -> threadDelay 2000000 >> runOkFixture
  "slow-shutdown" -> runSlowShutdownFixture
  "flaky-start" -> runFlakyStartFixture
  "counted-early-exit" -> incrementFixtureCount "counted-early-exit" >> exitFailure
  "hang-query" -> runHangQueryFixture
  "provider-failed" -> runProviderFailedFixture
  "validation-ok" -> runValidationOkFixture
  "invalid-mutate" -> runInvalidMutateFixture
  "negotiated-validation" -> runNegotiatedValidationFixture
  "exit-on-generator" -> runExitOnGeneratorFixture
  "exit-on-simulation" -> runExitOnSimulationFixture
  unknown -> die ("unknown plugin-manager fixture: " <> unknown)

runEnvContractFixture :: IO ()
runEnvContractFixture = do
  verifyLaunchEnvironment
  runOkFixture

runOkFixture :: IO ()
runOkFixture = do
  connectPluginFromEnvironment "plugin-manager-test-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runSlowShutdownFixture :: IO ()
runSlowShutdownFixture = do
  connectPluginFromEnvironment "plugin-manager-slow-shutdown-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgShutdown -> threadDelay 1000000 >> closeTransport transport
              _ -> loop transport

runFlakyStartFixture :: IO ()
runFlakyStartFixture = do
  count <- incrementFixtureCount "flaky-start"
  if count <= 1
    then exitFailure
    else runOkFixture

runHangQueryFixture :: IO ()
runHangQueryFixture = do
  connectPluginFromEnvironment "plugin-manager-hang-query-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgQueryResource -> threadDelay 2000000 >> closeTransport transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runProviderFailedFixture :: IO ()
runProviderFailedFixture = do
  connectPluginFromEnvironment "plugin-manager-provider-failed-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgQueryResource -> do
                _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope
                  (envRequestId envelope)
                  503
                  "external data-source provider failed: status unavailable"))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runValidationOkFixture :: IO ()
runValidationOkFixture = do
  connectPluginFromEnvironment "plugin-manager-validation-ok-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgQueryResource -> do
                _ <- sendMessage transport (encodeMessage (queryResultEnvelope
                  (envRequestId envelope)
                  (QueryResult "records" [record [("id", String "alpha")]] (Just 1))))
                loop transport
              MsgMutateResource -> do
                _ <- sendMessage transport (encodeMessage (mutateResultEnvelope
                  (envRequestId envelope)
                  (MutateResult True Nothing (Just (record [("id", String "accepted"), ("name", String "Accepted")])) Nothing)))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runInvalidMutateFixture :: IO ()
runInvalidMutateFixture = do
  connectPluginFromEnvironment "plugin-manager-invalid-mutate-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgMutateResource -> do
                _ <- sendMessage transport (encodeMessage (mutateResultEnvelope
                  (envRequestId envelope)
                  (MutateResult True Nothing (Just (record [("id", Number 1), ("name", String "Alpha")])) Nothing)))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runNegotiatedValidationFixture :: IO ()
runNegotiatedValidationFixture = do
  connectPluginFromEnvironment "plugin-manager-negotiated-validation-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckWithResourcesEnvelope
                  (envRequestId envelope)
                  currentProtocolVersion
                  [negotiatedValidationSchema]))
                loop transport
              MsgMutateResource -> do
                _ <- sendMessage transport (encodeMessage (mutateResultEnvelope
                  (envRequestId envelope)
                  (MutateResult True Nothing (Just (record [("id", String "alpha"), ("name", String "Alpha")])) Nothing)))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runExitOnGeneratorFixture :: IO ()
runExitOnGeneratorFixture = do
  connectPluginFromEnvironment "plugin-manager-generator-crash-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgInvokeGenerator -> exitFailure
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runExitOnSimulationFixture :: IO ()
runExitOnSimulationFixture = do
  connectPluginFromEnvironment "plugin-manager-simulation-crash-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                loop transport
              MsgInvokeSimulation -> exitFailure
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runOneShotAckFixture :: Int -> IO ()
runOneShotAckFixture protocolVersion = do
  connectPluginFromEnvironment "plugin-manager-protocol-mismatch-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes -> do
          let requestId = either (const Nothing) envRequestId (decodeMessage bytes)
          _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope requestId protocolVersion))
          closeTransport transport

verifyLaunchEnvironment :: IO ()
verifyLaunchEnvironment = do
  pluginId <- requireEnv pluginIdEnv
  protocol <- requireEnv pluginProtocolEnv
  endpoint <- requireEnv pluginEndpointEnv
  endpointKind <- requireEnv pluginEndpointKindEnv
  session <- requireEnv pluginSessionEnv
  authToken <- requireEnv pluginAuthTokenEnv
  worldId <- requireEnv pluginWorldIdEnv
  dataRoot <- requireEnv pluginDataRootEnv
  stdioCompat <- lookupEnv pluginStdioCompatibilityEnv
  pluginId `shouldEqualOrDie` envContractPluginName
  protocol `shouldEqualOrDie` show currentProtocolVersion
  endpoint `shouldNotBeEmptyOrDie` pluginEndpointEnv
  endpointKind `shouldEqualOrDie` expectedEndpointKind
  session `shouldNotBeEmptyOrDie` pluginSessionEnv
  authToken `shouldNotBeEmptyOrDie` pluginAuthTokenEnv
  worldId `shouldEqualOrDie` "unsaved"
  dataRoot `shouldNotBeEmptyOrDie` pluginDataRootEnv
  stdioCompat `shouldBeUnsetOrDie` pluginStdioCompatibilityEnv
  dataRootExists <- doesDirectoryExist dataRoot
  unless dataRootExists (die (pluginDataRootEnv <> " does not name an existing directory"))

requireEnv :: String -> IO String
requireEnv key = lookupEnv key >>= maybe (die ("missing " <> key)) pure

shouldEqualOrDie :: (Eq a, Show a) => a -> a -> IO ()
shouldEqualOrDie actual expected =
  unless (actual == expected) (die ("expected " <> show expected <> ", got " <> show actual))

shouldNotBeEmptyOrDie :: String -> String -> IO ()
shouldNotBeEmptyOrDie value label =
  unless (not (null value)) (die (label <> " must not be empty"))

shouldBeUnsetOrDie :: Maybe String -> String -> IO ()
shouldBeUnsetOrDie value label =
  case value of
    Nothing -> pure ()
    Just _ -> die (label <> " must not be set for production plugin launches")

expectedEndpointKind :: String
expectedEndpointKind =
  if os == "mingw32"
    then "named-pipe"
    else "unix"

runMalformedJsonFixture :: IO ()
runMalformedJsonFixture = do
  connectPluginFromEnvironment "plugin-manager-malformed-json-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      _ <- sendMessage transport (BSC.pack "{not valid json")
      closeTransport transport

runBadHandshakeFixture :: IO ()
runBadHandshakeFixture = do
  connectPluginFromEnvironment "plugin-manager-bad-handshake-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      handshakeRequest <- recvMessage transport
      let requestId = case handshakeRequest of
            Left _ -> Nothing
            Right bytes -> either (const Nothing) envRequestId (decodeMessage bytes)
      _ <- sendMessage transport (encodeMessage (RPCEnvelope
        { envType = MsgHealthStatus
        , envPayload = object
            [ "healthy" .= True
            , "message" .= ("not a handshake acknowledgement" :: Text)
            ]
        , envRequestId = requestId
        }))
      closeTransport transport

handshakeAckEnvelope :: Maybe Word64 -> Int -> RPCEnvelope
handshakeAckEnvelope requestId protocolVersion =
  handshakeAckWithResourcesEnvelope requestId protocolVersion []

handshakeAckWithResourcesEnvelope :: Maybe Word64 -> Int -> [DataResourceSchema] -> RPCEnvelope
handshakeAckWithResourcesEnvelope requestId protocolVersion resources = RPCEnvelope
  { envType = MsgHandshakeAck
  , envPayload = Aeson.toJSON (HandshakeAck
      { haProtocolVersion = protocolVersion
      , haDataDirectory = Nothing
      , haResources = resources
      })
  , envRequestId = requestId
  }

pluginErrorEnvelope :: Maybe Word64 -> Int -> Text -> RPCEnvelope
pluginErrorEnvelope requestId code message = RPCEnvelope
  { envType = MsgError
  , envPayload = object
      [ "code" .= code
      , "message" .= message
      ]
  , envRequestId = requestId
  }

queryResultEnvelope :: Maybe Word64 -> QueryResult -> RPCEnvelope
queryResultEnvelope requestId result = RPCEnvelope
  { envType = MsgQueryResult
  , envPayload = Aeson.toJSON result
  , envRequestId = requestId
  }

mutateResultEnvelope :: Maybe Word64 -> MutateResult -> RPCEnvelope
mutateResultEnvelope requestId result = RPCEnvelope
  { envType = MsgMutateResult
  , envPayload = Aeson.toJSON result
  , envRequestId = requestId
  }

testPluginName :: String
testPluginName = "copilot-test-plugin-manager-schema"

testManifestJSON :: BS.ByteString
testManifestJSON =
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"copilot-test-plugin-manager-schema\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"overlay\": { \"schemaFile\": \"test.toposchema\" }\n"
    <> "}\n"

testSchemaJSON :: BS.ByteString
testSchemaJSON =
  "{\n"
    <> "  \"name\": \"copilot_test_overlay\",\n"
    <> "  \"version\": \"1.0.0\",\n"
    <> "  \"storage\": \"sparse\",\n"
    <> "  \"fields\": [\n"
    <> "    {\n"
    <> "      \"name\": \"value\",\n"
    <> "      \"type\": \"float\",\n"
    <> "      \"default\": 0.0,\n"
    <> "      \"indexed\": false\n"
    <> "    }\n"
    <> "  ]\n"
    <> "}\n"

testLaunchPluginName :: String
testLaunchPluginName = "copilot-test-plugin-launch"

testLaunchManifestJSON :: BS.ByteString
testLaunchManifestJSON = manifestFor testLaunchPluginName

envContractPluginName :: String
envContractPluginName = "copilot-test-plugin-env-contract"

envContractManifestJSON :: BS.ByteString
envContractManifestJSON = manifestFor envContractPluginName

mismatchPluginName :: String
mismatchPluginName = "copilot-test-plugin-protocol-mismatch"

mismatchManifestJSON :: BS.ByteString
mismatchManifestJSON = manifestFor mismatchPluginName

malformedPluginName :: String
malformedPluginName = "copilot-test-plugin-malformed-json"

malformedManifestJSON :: BS.ByteString
malformedManifestJSON = manifestFor malformedPluginName

badHandshakePluginName :: String
badHandshakePluginName = "copilot-test-plugin-bad-handshake"

badHandshakeManifestJSON :: BS.ByteString
badHandshakeManifestJSON = manifestFor badHandshakePluginName

crashPluginName :: String
crashPluginName = "copilot-test-plugin-crashy"

crashManifestJSON :: BS.ByteString
crashManifestJSON = manifestFor crashPluginName

refreshTransientPluginName :: String
refreshTransientPluginName = "copilot-test-plugin-refresh-transient"

refreshTransientManifestJSON :: BS.ByteString
refreshTransientManifestJSON = manifestFor refreshTransientPluginName

shutdownTransientPluginName :: String
shutdownTransientPluginName = "copilot-test-plugin-shutdown-transient"

shutdownTransientManifestJSON :: BS.ByteString
shutdownTransientManifestJSON = manifestFor shutdownTransientPluginName

slowPluginName :: String
slowPluginName = "copilot-test-plugin-slow"

slowManifestJSON :: BS.ByteString
slowManifestJSON = manifestFor slowPluginName

autoStartDisabledPluginName :: String
autoStartDisabledPluginName = "copilot-test-plugin-auto-start-disabled"

autoStartDisabledManifestJSON :: BS.ByteString
autoStartDisabledManifestJSON = dataResourceManifestFor autoStartDisabledPluginName
  [ "    \"auto_start\": false,"
  , "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 100,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

disabledDependencyPluginName :: String
disabledDependencyPluginName = "copilot-test-plugin-disabled-dependency"

disabledDependencyManifestJSON :: BS.ByteString
disabledDependencyManifestJSON = manifestFor disabledDependencyPluginName

providerFailedPluginName :: String
providerFailedPluginName = "copilot-test-plugin-provider-failed"

providerFailedManifestJSON :: BS.ByteString
providerFailedManifestJSON = dataResourceManifestFor providerFailedPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"max_restarts\": 0,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 1"
  ]

flakyStartPluginName :: String
flakyStartPluginName = "copilot-test-plugin-flaky-start-policy"

flakyStartManifestJSON :: BS.ByteString
flakyStartManifestJSON = manifestWithStartPolicyFor flakyStartPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 2,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 5"
  ]

restartLimitPluginName :: String
restartLimitPluginName = "copilot-test-plugin-restart-limit"

restartLimitManifestJSON :: BS.ByteString
restartLimitManifestJSON = manifestWithStartPolicyFor restartLimitPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 1,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 50,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100,"
  , "    \"backoff_ms\": 1"
  ]

hangQueryPluginName :: String
hangQueryPluginName = "copilot-test-plugin-hang-query"

hangQueryManifestJSON :: BS.ByteString
hangQueryManifestJSON = dataResourceManifestFor hangQueryPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 0,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 1"
  ]

unsupportedMutationPluginName :: String
unsupportedMutationPluginName = "copilot-test-plugin-unsupported-mutation"

unsupportedMutationManifestJSON :: BS.ByteString
unsupportedMutationManifestJSON = dataResourceManifestFor unsupportedMutationPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

validationPluginName :: String
validationPluginName = "copilot-test-plugin-record-validation"

validationManifestJSON :: BS.ByteString
validationManifestJSON = writableDataResourceManifestFor validationPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

invalidReturnPluginName :: String
invalidReturnPluginName = "copilot-test-plugin-invalid-return"

invalidReturnManifestJSON :: BS.ByteString
invalidReturnManifestJSON = writableDataResourceManifestFor invalidReturnPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

negotiatedValidationPluginName :: String
negotiatedValidationPluginName = "copilot-test-plugin-negotiated-validation"

negotiatedValidationManifestJSON :: BS.ByteString
negotiatedValidationManifestJSON = negotiatedValidationManifestFor negotiatedValidationPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

negotiatedValidationSchema :: DataResourceSchema
negotiatedValidationSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "records"
  , drsLabel = "Records"
  , drsHexBound = False
  , drsFields =
      [ DataFieldDef "id" DFText "ID" False Nothing
      , DataFieldDef "name" DFText "Name" False Nothing
      ]
  , drsOperations = noOperations
      { doList = True
      , doCreate = True
      , doPage = True
      }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = defaultDataPagination
  }

generatorCrashPluginName :: String
generatorCrashPluginName = "copilot-test-plugin-generator-crash"

generatorCrashManifestJSON :: BS.ByteString
generatorCrashManifestJSON = manifestWithStartPolicyFor generatorCrashPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 0,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 1"
  ]

simulationCrashPluginName :: String
simulationCrashPluginName = "copilot-test-plugin-simulation-crash"

simulationCrashManifestJSON :: BS.ByteString
simulationCrashManifestJSON = simulationManifestWithStartPolicyFor simulationCrashPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 0,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 1"
  ]

testQuery :: QueryResource
testQuery = QueryResource
  { qrResource = "records"
  , qrQuery = QueryAll
  , qrPageSize = Just 20
  , qrPageOffset = Just 0
  }

record :: [(Text, Value)] -> DataRecord
record = DataRecord . Map.fromList

manifestFor :: String -> BS.ByteString
manifestFor name = manifestWithStartPolicyFor name []

missingRuntimeManifestFor :: String -> BS.ByteString
missingRuntimeManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }\n"
    <> "}\n"

invalidProtocolManifestFor :: String -> BS.ByteString
invalidProtocolManifestFor name = BSC.pack $
  let unsupported = show (currentProtocolVersion + 1)
  in "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> unsupported <> ", \"max\": " <> unsupported <> " } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }\n"
    <> "}\n"

missingOverlaySchemaManifestFor :: String -> BS.ByteString
missingOverlaySchemaManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"overlay\": { \"schemaFile\": \"missing.toposchema\" }\n"
    <> "}\n"

invalidExternalSourceManifestFor :: String -> BS.ByteString
invalidExternalSourceManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"externalDataSources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"records\",\n"
    <> "      \"label\": \"Records\",\n"
    <> "      \"kind\": \"\",\n"
    <> "      \"capabilities\": [],\n"
    <> "      \"status\": { \"state\": \"ready\" }\n"
    <> "    }\n"
    <> "  ]\n"
    <> "}\n"

manifestWithStartPolicyFor :: String -> [String] -> BS.ByteString
manifestWithStartPolicyFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }"
    <> renderStartPolicy policyLines
    <> "\n}\n"

simulationManifestWithStartPolicyFor :: String -> [String] -> BS.ByteString
simulationManifestWithStartPolicyFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"simulation\": { \"dependencies\": [] },\n"
    <> "  \"overlay\": { \"schemaFile\": \"test.toposchema\" }"
    <> renderStartPolicy policyLines
    <> "\n}\n"

dataResourceManifestFor :: String -> [String] -> BS.ByteString
dataResourceManifestFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"records\",\n"
    <> "      \"label\": \"Records\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [ { \"name\": \"id\", \"type\": \"text\", \"label\": \"ID\" } ],\n"
    <> "      \"operations\": { \"list\": true, \"page\": true },\n"
    <> "      \"keyField\": \"id\"\n"
    <> "    }\n"
    <> "  ]"
    <> renderStartPolicy policyLines
    <> "\n}\n"

negotiatedValidationManifestFor :: String -> [String] -> BS.ByteString
negotiatedValidationManifestFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"capabilities\": [\"dataRead\", \"dataWrite\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"records\",\n"
    <> "      \"label\": \"Manifest Records\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [ { \"name\": \"id\", \"type\": \"text\", \"label\": \"ID\" } ],\n"
    <> "      \"operations\": { \"list\": true, \"page\": true },\n"
    <> "      \"keyField\": \"id\"\n"
    <> "    }\n"
    <> "  ]"
    <> renderStartPolicy policyLines
    <> "\n}\n"

writableDataResourceManifestFor :: String -> [String] -> BS.ByteString
writableDataResourceManifestFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"capabilities\": [\"dataRead\", \"dataWrite\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"records\",\n"
    <> "      \"label\": \"Records\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [\n"
    <> "        { \"name\": \"id\", \"type\": \"text\", \"label\": \"ID\" },\n"
    <> "        { \"name\": \"name\", \"type\": \"text\", \"label\": \"Name\" }\n"
    <> "      ],\n"
    <> "      \"operations\": { \"list\": true, \"get\": true, \"create\": true, \"update\": true, \"delete\": true, \"page\": true },\n"
    <> "      \"keyField\": \"id\"\n"
    <> "    }\n"
    <> "  ]"
    <> renderStartPolicy policyLines
    <> "\n}\n"

sampleSimContext :: SimContext
sampleSimContext = SimContext
  { scTerrain = emptyWorld (WorldConfig 8) defaultHexGridMeta
  , scCalendar = CalendarDate 0 0 0
  , scWorldTime = defaultWorldTime
  , scDeltaTicks = 1
  , scOverlays = mempty
  }

sampleOverlay :: Overlay
sampleOverlay = emptyOverlay sampleOverlaySchema

sampleOverlaySchema :: OverlaySchema
sampleOverlaySchema = OverlaySchema
  { osName = "simulation-crash"
  , osVersion = "1.0.0"
  , osDescription = ""
  , osFields = []
  , osStorage = StorageSparse
  , osDependencies = OverlayDeps False []
  , osFieldIndex = mempty
  }

renderStartPolicy :: [String] -> String
renderStartPolicy [] = ""
renderStartPolicy policyLines =
  ",\n  \"startPolicy\": {\n"
    <> unlines policyLines
    <> "  }"
