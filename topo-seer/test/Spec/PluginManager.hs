{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.PluginManager (spec, runFixtureCli, runFixtureCliIfRequested) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception
  ( Exception
  , SomeAsyncException
  , SomeException
  , bracket
  , catch
  , finally
  , fromException
  , onException
  , throwIO
  , try
  )
import Control.Monad (unless)
import Data.List (elemIndex)
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
  , copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
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
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , getProcessExitCode
  , proc
  )
import System.Timeout (timeout)
import Test.Hspec

import Actor.UI (UiState(..), emptyUiState)
import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManager
  , PluginStatus(..)
  , discoverPlugins
  , getDisabledPlugins
  , getLoadedPlugins
  , getPluginExternalDataSources
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
import Seer.World.Persist
  ( WorldExternalDataSourceSnapshot(..)
  , WorldSaveManifest(..)
  , deleteNamedWorld
  , loadNamedWorld
  , saveNamedWorldWithPluginsAndExternalData
  )
import Topo.Plugin.RPC
  ( RPCConnection
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusState(..)
  , defaultRPCExternalDataSourceStatus
  , invokeGenerator
  , invokeSimulation
  , requestExternalDataSourceStatus
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  )
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

withPluginManager :: (ActorHandle PluginManager (Protocol PluginManager) -> IO a) -> IO a
withPluginManager action =
  bracket acquire release (action . snd)
  where
    acquire = do
      system <- newActorSystem
      pluginManagerHandle <-
        getPluginManager system `onException` ignoreCleanupExceptions (shutdownActorSystem system)
      pure (system, pluginManagerHandle)

    release (system, pluginManagerHandle) =
      ignoreCleanupExceptions (shutdownPlugins pluginManagerHandle)
        `finally` ignoreCleanupExceptions (shutdownActorSystem system)

ignoreCleanupExceptions :: IO () -> IO ()
ignoreCleanupExceptions action =
  action `catch` \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure ()

data FixtureCleanupProbe = FixtureCleanupProbe
  deriving (Eq, Show)

instance Exception FixtureCleanupProbe

spec :: Spec
spec = describe "PluginManager" $ do
  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "launches plugin subprocesses and exposes generator stages cross-platform" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
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
        shutdownPlugins pluginManagerHandle
        threadDelay 200000
        loadedAfterShutdown <- getLoadedPlugins pluginManagerHandle
        pluginStatuses testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem PluginDisconnected
        pluginLifecycleStates testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem LifecycleStopped
        mapM_ (assertProcessExited testLaunchPluginName) handles

  it "launches plugins with the complete TOPO_PLUGIN environment over endpoint transport" $ do
    withParentStdioCompatibilityFlag $ do
      withExecutablePluginDir envContractPluginName envContractManifestJSON "env-contract" $ do
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses envContractPluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates envContractPluginName loaded `shouldSatisfy` elem LifecycleReady
          pluginLifecycleProtocols envContractPluginName loaded `shouldSatisfy` elem (Just currentProtocolVersion)

  it "cleans up connected fixture subprocesses when the manager scope aborts" $ do
    withExecutablePluginDir cleanupAbortPluginName cleanupAbortManifestJSON "ok" $ do
      handleVar <- newEmptyMVar
      result <- try @FixtureCleanupProbe $
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses cleanupAbortPluginName loaded `shouldSatisfy` elem PluginConnected
          handles <- expectPluginProcessHandles cleanupAbortPluginName loaded
          case handles of
            processHandle:_ -> putMVar handleVar processHandle
            [] -> expectationFailure "expected a non-empty fixture process handle list"
          throwIO FixtureCleanupProbe :: IO ()
      result `shouldBe` Left FixtureCleanupProbe
      captured <- timeout 1000000 (takeMVar handleVar)
      case captured of
        Nothing -> expectationFailure "did not capture a connected fixture process handle"
        Just processHandle -> assertProcessExited cleanupAbortPluginName processHandle

  it "cleans up Windows fixture child processes when the parent hangs during shutdown" $ do
    if os /= "mingw32"
      then pure ()
      else withExecutablePluginDir windowsProcessTreePluginName windowsProcessTreeManifestJSON "windows-process-tree" $ do
        heartbeatPath <- fixtureDataFile windowsProcessTreePluginName windowsHeartbeatFileName
        handles <- withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses windowsProcessTreePluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates windowsProcessTreePluginName loaded `shouldSatisfy` elem LifecycleReady
          processHandles <- expectPluginProcessHandles windowsProcessTreePluginName loaded
          expectHeartbeatAdvances heartbeatPath
          pure processHandles
        mapM_ (assertProcessExited windowsProcessTreePluginName) handles
        assertHeartbeatStops heartbeatPath

  it "exposes Starting while public refreshManifests performs supervisor work" $ do
    withExecutablePluginDir refreshTransientPluginName refreshTransientManifestJSON "slow" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        done <- newEmptyMVar
        _ <- forkIO (refreshManifests pluginManagerHandle >> putMVar done ())
        threadDelay 50000
        starting <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName starting `shouldSatisfy` elem LifecycleStarting
        takeMVar done
        failed <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName failed `shouldSatisfy` elem LifecycleFailed

  it "cleans up unpublished subprocesses when refreshManifests is interrupted" $ do
    withExecutablePluginDir interruptedRefreshPluginName interruptedRefreshManifestJSON "slow" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        result <- timeout 300000 (refreshManifests pluginManagerHandle)
        result `shouldBe` Nothing
        interrupted <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates interruptedRefreshPluginName interrupted `shouldSatisfy` elem LifecycleDiscovered
        pluginLifecycleStates interruptedRefreshPluginName interrupted `shouldNotSatisfy` elem LifecycleStarting
        length (pluginProcessHandles interruptedRefreshPluginName interrupted) `shouldBe` 0

  it "finalizes connected runtime state when refreshManifests is interrupted" $ do
    withExecutablePluginDir connectedRefreshInterruptedPluginName connectedRefreshInterruptedManifestJSON "slow-shutdown" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        handles <- expectPluginProcessHandles connectedRefreshInterruptedPluginName ready
        baseDir <- currentPluginBaseDir
        BS.writeFile
          (baseDir </> connectedRefreshInterruptedPluginName </> "manifest.json")
          connectedRefreshInterruptedDisabledManifestJSON
        result <- timeout 300000 (refreshManifests pluginManagerHandle)
        result `shouldBe` Nothing
        interrupted <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates connectedRefreshInterruptedPluginName interrupted `shouldSatisfy` elem LifecycleStopped
        pluginLifecycleStates connectedRefreshInterruptedPluginName interrupted `shouldNotSatisfy` elem LifecycleStarting
        length (pluginProcessHandles connectedRefreshInterruptedPluginName interrupted) `shouldBe` 0
        mapM_ (assertProcessExited connectedRefreshInterruptedPluginName) handles

  it "exposes Stopping while public shutdownPlugins performs supervisor work" $ do
    withExecutablePluginDir shutdownTransientPluginName shutdownTransientManifestJSON "slow-shutdown" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "finalizes shutdown state when shutdownPlugins is interrupted" $ do
    withExecutablePluginDir interruptedShutdownPluginName interruptedShutdownManifestJSON "slow-shutdown" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        handles <- expectPluginProcessHandles interruptedShutdownPluginName ready
        result <- timeout 300000 (shutdownPlugins pluginManagerHandle)
        result `shouldBe` Nothing
        interrupted <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates interruptedShutdownPluginName interrupted `shouldSatisfy` elem LifecycleStopped
        pluginLifecycleStates interruptedShutdownPluginName interrupted `shouldNotSatisfy` elem LifecycleStopping
        length (pluginProcessHandles interruptedShutdownPluginName interrupted) `shouldBe` 0
        mapM_ (assertProcessExited interruptedShutdownPluginName) handles

  it "reports a protocol-version mismatch as a plugin error" $ do
    withExecutablePluginDir mismatchPluginName mismatchManifestJSON "protocol-mismatch" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses mismatchPluginName loaded `shouldSatisfy` anyPluginErrorContaining "protocol version mismatch"
        pluginLifecycleStates mismatchPluginName loaded `shouldSatisfy` elem LifecycleFailed

  it "surfaces manifest parse diagnostics for missing required fields" $ do
    let pluginName = "copilot-test-plugin-missing-runtime"
    withExecutablePluginDir pluginName (missingRuntimeManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "runtime"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_parse_failed")

  it "blocks startup when manifest runtime protocol bounds exclude the host" $ do
    let pluginName = "copilot-test-plugin-invalid-protocol"
    withExecutablePluginDir pluginName (invalidProtocolManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
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
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "overlay.schemaFile"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_schema_failed")
        length (pluginProcessHandles pluginName loaded) `shouldBe` 0

  it "validates backend-neutral external data-source declarations with actionable diagnostics" $ do
    let pluginName = "copilot-test-plugin-invalid-external-source"
    withExecutablePluginDir pluginName (invalidExternalSourceManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "externalDataSources"
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "backend-neutral"
        pluginStatuses pluginName loaded `shouldSatisfy` (not . anyPluginErrorContaining "SQLite")
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_validation_failed")

  it "blocks startup for required external data-source refs declared unavailable" $ do
    let pluginName = "copilot-test-plugin-external-source-blocked"
    withExecutablePluginDir pluginName (blockedExternalRefManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "external-ledger:settlements:settlement-read"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "external_data_source_blocked")
        length (pluginProcessHandles pluginName loaded) `shouldBe` 0

  it "launches external data-source-only plugins for status protocol handling" $ do
    let pluginName = "copilot-test-plugin-external-source-only"
    withExecutablePluginDir pluginName (externalOnlyProviderManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleReady
        length (pluginProcessHandles pluginName loaded) `shouldSatisfy` (> 0)

  it "integrates shared external data-source provider and consumer fixtures without backend assumptions" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ do
        bracket
          (deleteNamedWorld externalIntegrationWorldName >> pure ())
          (\_ -> deleteNamedWorld externalIntegrationWorldName >> pure ())
          (\_ -> withPluginManager $ \pluginManagerHandle -> do
              discoverPlugins pluginManagerHandle
              refreshManifests pluginManagerHandle
              (do
                loaded <- getLoadedPlugins pluginManagerHandle
                pluginStatuses externalProviderPluginName loaded `shouldSatisfy` elem PluginConnected
                pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
                pluginLifecycleStates externalProviderPluginName loaded `shouldSatisfy` elem LifecycleReady
                pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleReady
                startupOrder <- readExternalStartupOrder
                assertStartedBefore externalProviderPluginName externalConsumerPluginName startupOrder

                providerConn <- expectPluginConnection externalProviderPluginName loaded
                consumerConn <- expectPluginConnection externalConsumerPluginName loaded
                providerStatus <- expectRight "provider external status" =<<
                  expectWithin "provider external status request" (requestExternalDataSourceStatus providerConn externalProviderStatusRequest)
                expectProviderStatusReport providerStatus
                consumerStatus <- expectRight "consumer external status" =<<
                  expectWithin "consumer external status request" (requestExternalDataSourceStatus consumerConn externalConsumerStatusRequest)
                expectConsumerStatusReport consumerStatus

                initialBinding <- expectRight "initial consumer binding query" =<<
                  expectWithin "initial consumer binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
                expectBindingStatus "declared" initialBinding
                shouldNotMentionSQLite initialBinding
                expectRight "send external grant" =<< expectWithin "send external grant" (sendExternalDataSourceGrant consumerConn externalGrantMessage)
                grantedBinding <- expectRight "granted consumer binding query" =<<
                  expectWithin "granted consumer binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
                expectBindingStatus "granted" grantedBinding
                expectRight "send external grant revocation" =<< expectWithin "send external grant revocation" (sendExternalDataSourceGrantRevocation consumerConn externalGrantRevocation)
                revokedBinding <- expectRight "revoked consumer binding query" =<<
                  expectWithin "revoked consumer binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
                expectBindingStatus "revoked" revokedBinding

                snapshots <- getPluginExternalDataSources pluginManagerHandle
                expectExternalSnapshots snapshots
                let world = emptyWorld (WorldConfig 8) defaultHexGridMeta
                    ui = emptyUiState { uiSeed = 77, uiChunkSize = 8 }
                saveResult <- expectWithin "save external integration world" (saveNamedWorldWithPluginsAndExternalData externalIntegrationWorldName ui world [] snapshots)
                saveResult `shouldBe` Right ()
                loadResult <- expectWithin "load external integration world" (loadNamedWorld externalIntegrationWorldName)
                case loadResult of
                  Left err -> expectationFailure (Text.unpack err)
                  Right (manifest, _snapshot, _loadedWorld) -> do
                    wsmExternalDataSources manifest `shouldBe` snapshots
                    shouldNotMentionSQLite (wsmExternalDataSources manifest)

                setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
                unavailableSnapshots <- getPluginExternalDataSources pluginManagerHandle
                expectProviderUnavailableSnapshots unavailableSnapshots))

  it "reports malformed handshake JSON as a plugin error" $ do
    withExecutablePluginDir malformedPluginName malformedManifestJSON "malformed-json" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses malformedPluginName loaded `shouldSatisfy` anyPluginErrorContaining "RPCProtocolError"

  it "reports unexpected handshake responses as plugin errors" $ do
    withExecutablePluginDir badHandshakePluginName badHandshakeManifestJSON "bad-handshake" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses badHandshakePluginName loaded `shouldSatisfy` anyPluginErrorContaining "unexpected response to handshake"
        pluginLifecycleStates badHandshakePluginName loaded `shouldSatisfy` elem LifecycleFailed
        length (pluginProcessHandles badHandshakePluginName loaded) `shouldBe` 0

  it "reports early plugin exit during startup as a plugin error" $ do
    withExecutablePluginDir crashPluginName crashManifestJSON "early-exit" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses crashPluginName loaded `shouldSatisfy` anyPluginError
        length (pluginProcessHandles crashPluginName loaded) `shouldBe` 0

  it "reports handshake timeouts as plugin errors" $ do
    withExecutablePluginDir slowPluginName slowManifestJSON "slow" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses slowPluginName loaded `shouldSatisfy` anyPluginErrorContaining "timed out"
        length (pluginProcessHandles slowPluginName loaded) `shouldBe` 0

  it "honors auto_start=false by leaving runtime capabilities unavailable" $ do
    withExecutablePluginDir autoStartDisabledPluginName autoStartDisabledManifestJSON "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
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
      withPluginManager $ \pluginManagerHandle -> do
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

  it "surfaces external data-source provider failures without backend-specific storage assumptions" $ do
    withExecutablePluginDir providerFailedPluginName providerFailedManifestJSON "provider-failed" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "restarts startup failures with policy backoff and then connects" $ do
    withExecutablePluginDir flakyStartPluginName flakyStartManifestJSON "flaky-start" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses flakyStartPluginName loaded `shouldSatisfy` elem PluginConnected
        count <- readFixtureCount flakyStartPluginName "flaky-start"
        count `shouldBe` 2

  it "stops restarting startup failures after the max restart window is exhausted" $ do
    withExecutablePluginDir restartLimitPluginName restartLimitManifestJSON "counted-early-exit" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses restartLimitPluginName loaded `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes restartLimitPluginName loaded `shouldSatisfy` elem (Just "restart_limit_exceeded")
        count <- readFixtureCount restartLimitPluginName "counted-early-exit"
        count `shouldBe` 2

  it "times out data-resource requests instead of hanging" $ do
    withExecutablePluginDir hangQueryPluginName hangQueryManifestJSON "hang-query" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses hangQueryPluginName loaded `shouldSatisfy` elem PluginConnected
        handles <- expectPluginProcessHandles hangQueryPluginName loaded
        timed <- timeout 5000000 $ queryPluginResource pluginManagerHandle (Text.pack hangQueryPluginName) testQuery
        case timed of
          Nothing -> expectationFailure "data query hung"
          Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "timed out"
          Just (Right _) -> expectationFailure "hung data query unexpectedly succeeded"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses hangQueryPluginName observed `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes hangQueryPluginName observed `shouldSatisfy` elem (Just "restart_limit_exceeded")
        length (pluginProcessHandles hangQueryPluginName observed) `shouldBe` 0
        mapM_ (assertProcessExited hangQueryPluginName) handles

  it "rejects unsupported data-resource mutations before plugin calls" $ do
    withExecutablePluginDir unsupportedMutationPluginName unsupportedMutationManifestJSON "validation-ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "validates inbound data-resource records before plugins can accept them" $ do
    withExecutablePluginDir validationPluginName validationManifestJSON "validation-ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "validates plugin-returned data-resource records before exposing them" $ do
    withExecutablePluginDir invalidReturnPluginName invalidReturnManifestJSON "invalid-mutate" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "validates data-resource calls against negotiated handshake schemas" $ do
    withExecutablePluginDir negotiatedValidationPluginName negotiatedValidationManifestJSON "negotiated-validation" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "surfaces generator crashes without hanging" $ do
    withExecutablePluginDir generatorCrashPluginName generatorCrashManifestJSON "exit-on-generator" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

  it "surfaces simulation crashes without hanging" $ do
    withExecutablePluginDir simulationCrashPluginName simulationCrashManifestJSON "exit-on-simulation" $ do
      withPluginManager $ \pluginManagerHandle -> do
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

expectPluginProcessHandles :: String -> [LoadedPlugin] -> IO [ProcessHandle]
expectPluginProcessHandles name loaded =
  case pluginProcessHandles name loaded of
    [] -> expectationFailure (name <> " did not expose a fixture process handle") >> fail "missing process handle"
    handles -> pure handles

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

expectHeartbeatAdvances :: FilePath -> IO ()
expectHeartbeatAdvances path = do
  first <- waitForHeartbeatChange path Nothing "start"
  _ <- waitForHeartbeatChange path (Just first) "advance"
  pure ()

waitForHeartbeatChange :: FilePath -> Maybe String -> String -> IO String
waitForHeartbeatChange path previous label = do
  result <- timeout 2000000 go
  case result of
    Nothing -> expectationFailure ("windows fixture heartbeat did not " <> label) >> fail "heartbeat did not change"
    Just value -> pure value
  where
    go = do
      current <- readHeartbeat path
      case current of
        Just value | Just value /= previous -> pure value
        _ -> threadDelay 50000 >> go

assertHeartbeatStops :: FilePath -> IO ()
assertHeartbeatStops path = do
  current <- readHeartbeat path
  result <- timeout 3000000 (waitForStableHeartbeat current (0 :: Int))
  case result of
    Nothing -> expectationFailure "windows fixture child heartbeat kept changing after plugin teardown"
    Just _ -> pure ()
  where
    waitForStableHeartbeat previous stableSamples
      | stableSamples >= 4 = pure ()
      | otherwise = do
          threadDelay 100000
          current <- readHeartbeat path
          let stableSamples' = case (previous, current) of
                (Just old, Just new) | old == new -> stableSamples + 1
                _ -> 0
          waitForStableHeartbeat current stableSamples'

readHeartbeat :: FilePath -> IO (Maybe String)
readHeartbeat path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else (Just . BSC.unpack <$> BS.readFile path) `catch` \(_ :: SomeException) -> pure Nothing

expectPluginConnection :: String -> [LoadedPlugin] -> IO RPCConnection
expectPluginConnection name loaded =
  case pluginConnections name loaded of
    Just conn -> pure conn
    Nothing -> expectationFailure (name <> " did not expose an RPC connection") >> fail "missing plugin connection"

expectRight :: Show e => String -> Either e a -> IO a
expectRight label result = case result of
  Left err -> expectationFailure (label <> " failed: " <> show err) >> fail label
  Right value -> pure value

expectWithin :: String -> IO a -> IO a
expectWithin label action = do
  result <- timeout 10000000 action
  case result of
    Nothing -> expectationFailure (label <> " timed out") >> fail label
    Just value -> pure value

assertStartedBefore :: String -> String -> [String] -> Expectation
assertStartedBefore first second order =
  case (elemIndex first order, elemIndex second order) of
    (Just firstIndex, Just secondIndex) -> firstIndex `shouldSatisfy` (< secondIndex)
    _ -> expectationFailure ("startup order did not contain expected plugins: " <> show order)

expectProviderStatusReport :: RPCExternalDataSourceStatusReport -> Expectation
expectProviderStatusReport report =
  case redssReportStatuses report of
    [entry] -> do
      redsstProviderId entry `shouldBe` externalProviderPluginNameText
      redsstConsumerId entry `shouldBe` Nothing
      redsstSource entry `shouldBe` externalSourceName
      redsstGrant entry `shouldBe` Just externalGrantName
      redsstAccess entry `shouldBe` externalReadAccess
      redsstResources entry `shouldBe` externalSharedResources
      redsstCapabilityScope entry `shouldBe` externalCapabilities
      redssState (redsstStatus entry) `shouldBe` ExternalStatusReady
      redssAvailability (redsstStatus entry) `shouldBe` Just ExternalAvailabilityAvailable
      redssHealth (redsstStatus entry) `shouldBe` Just ExternalHealthHealthy
      redssAccessMode (redsstStatus entry) `shouldBe` Just ExternalAccessModeReadOnly
      redsstDiagnostics entry `shouldBe` Just externalDiagnostics
      shouldNotMentionSQLite entry
    other -> expectationFailure ("expected one provider status entry, got " <> show other)

expectConsumerStatusReport :: RPCExternalDataSourceStatusReport -> Expectation
expectConsumerStatusReport report =
  case redssReportStatuses report of
    [entry] -> do
      redsstProviderId entry `shouldBe` externalProviderPluginNameText
      redsstConsumerId entry `shouldBe` Just externalSourceName
      redsstSource entry `shouldBe` externalSourceName
      redsstGrant entry `shouldBe` Just externalGrantName
      redsstAccess entry `shouldBe` externalReadAccess
      redsstResources entry `shouldBe` externalSharedResources
      redsstCapabilityScope entry `shouldBe` externalCapabilities
      redssState (redsstStatus entry) `shouldBe` ExternalStatusReady
      redssAvailability (redsstStatus entry) `shouldBe` Just ExternalAvailabilityAvailable
      redssHealth (redsstStatus entry) `shouldBe` Just ExternalHealthHealthy
      redssAccessMode (redsstStatus entry) `shouldBe` Just ExternalAccessModeReadOnly
      redsstDiagnostics entry `shouldBe` Just externalDiagnostics
      shouldNotMentionSQLite entry
    other -> expectationFailure ("expected one consumer status entry, got " <> show other)

expectBindingStatus :: Text -> QueryResult -> Expectation
expectBindingStatus expected result = do
  qrsResource result `shouldBe` externalBindingResource
  case qrsRecords result of
    [binding] -> do
      recordField "source_id" binding `shouldBe` Just (String externalSourceName)
      recordField "provider" binding `shouldBe` Just (String externalProviderPluginNameText)
      recordField "grant" binding `shouldBe` Just (String externalGrantName)
      recordField "status" binding `shouldBe` Just (String expected)
    other -> expectationFailure ("expected one binding row, got " <> show other)

recordField :: Text -> DataRecord -> Maybe Value
recordField key (DataRecord fields) = Map.lookup key fields

expectExternalSnapshots :: [WorldExternalDataSourceSnapshot] -> Expectation
expectExternalSnapshots snapshots = do
  case findExternalSnapshot externalProviderPluginNameText snapshots of
    Nothing -> expectationFailure "missing provider external data-source snapshot"
    Just snapshot -> case wedssProvidedSources snapshot of
      [source] -> do
        redsdName source `shouldBe` externalSourceName
        redsdKind source `shouldBe` "catalog"
        redsdCapabilities source `shouldBe` externalCapabilities
        redsdResources source `shouldBe` externalSharedResources
        redsdConnection source `shouldBe` Just externalSourceReference
        redsdConfigRefs source `shouldBe` [externalProviderConfigRef]
        redssState (redsdStatus source) `shouldBe` ExternalStatusReady
        case redsdGrants source of
          [grant] -> do
            redsgName grant `shouldBe` externalGrantName
            redsgAccess grant `shouldBe` externalReadAccess
            redsgCapabilities grant `shouldBe` externalCapabilities
            redsgReference grant `shouldBe` Just externalGrantReference
            redsgConfigRefs grant `shouldBe` [externalGrantConfigRef]
          other -> expectationFailure ("expected one provider grant, got " <> show other)
      other -> expectationFailure ("expected one provided source, got " <> show other)
  case findExternalSnapshot externalConsumerPluginNameText snapshots of
    Nothing -> expectationFailure "missing consumer external data-source snapshot"
    Just snapshot -> case wedssConsumedRefs snapshot of
      [ref] -> do
        redsrName ref `shouldBe` externalSourceName
        redsrProvider ref `shouldBe` Just externalProviderPluginNameText
        redsrSource ref `shouldBe` externalSourceName
        redsrRequired ref `shouldBe` True
        redsrAccess ref `shouldBe` externalReadAccess
        redsrResources ref `shouldBe` externalSharedResources
        redsrGrant ref `shouldBe` Just externalGrantName
        redsrReference ref `shouldBe` Just externalConsumerReference
        redsrConfigRefs ref `shouldBe` [externalConsumerConfigRef]
        redssState (redsrStatus ref) `shouldBe` ExternalStatusReady
      other -> expectationFailure ("expected one consumed ref, got " <> show other)
  shouldNotMentionSQLite snapshots

expectProviderUnavailableSnapshots :: [WorldExternalDataSourceSnapshot] -> Expectation
expectProviderUnavailableSnapshots snapshots = do
  case findExternalSnapshot externalProviderPluginNameText snapshots of
    Nothing -> expectationFailure "missing unavailable provider snapshot"
    Just snapshot -> case wedssProvidedSources snapshot of
      [source] -> do
        expectUnavailableStatus externalProviderPluginNameText (redsdStatus source)
        mapM_ (expectUnavailableStatus externalProviderPluginNameText . redsgStatus) (redsdGrants source)
      other -> expectationFailure ("expected one unavailable source, got " <> show other)
  case findExternalSnapshot externalConsumerPluginNameText snapshots of
    Nothing -> expectationFailure "missing consumer snapshot after provider disable"
    Just snapshot -> case wedssConsumedRefs snapshot of
      [ref] -> expectUnavailableStatus externalProviderPluginNameText (redsrStatus ref)
      other -> expectationFailure ("expected one unavailable consumed ref, got " <> show other)
  shouldNotMentionSQLite snapshots

expectUnavailableStatus :: Text -> RPCExternalDataSourceStatus -> Expectation
expectUnavailableStatus providerName status = do
  redssState status `shouldBe` ExternalStatusUnavailable
  redssProviderId status `shouldBe` Just providerName
  redssAvailability status `shouldBe` Just ExternalAvailabilityUnavailable
  redssHealth status `shouldBe` Just ExternalHealthUnhealthy
  redssAccessMode status `shouldBe` Just ExternalAccessModeDisabled
  redssMessage status `shouldBe` Just "provider plugin is unavailable"
  shouldNotMentionSQLite status

findExternalSnapshot :: Text -> [WorldExternalDataSourceSnapshot] -> Maybe WorldExternalDataSourceSnapshot
findExternalSnapshot pluginName snapshots = case filter ((== pluginName) . wedssPlugin) snapshots of
  snapshot:_ -> Just snapshot
  [] -> Nothing

shouldNotMentionSQLite :: Show a => a -> Expectation
shouldNotMentionSQLite value = do
  let rendered = show value
  rendered `shouldNotSatisfy` contains "SQLite"
  rendered `shouldNotSatisfy` contains "sqlite"
  where
    contains needle haystack = needle `isInfixOfString` haystack

isInfixOfString :: String -> String -> Bool
isInfixOfString needle haystack = Text.pack needle `Text.isInfixOf` Text.pack haystack

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

withExecutablePluginDirs :: [(String, BS.ByteString, String)] -> IO a -> IO a
withExecutablePluginDirs pluginSpecs action =
  withIsolatedPluginHome "external-provider-consumer" $
    bracket setup teardown (const action)
  where
    setup = do
      baseDir <- currentPluginBaseDir
      traverse (writeSpec baseDir) pluginSpecs
    teardown = mapM_ removePathForciblyEventually
    writeSpec baseDir (pluginName, manifestJSON, fixtureMode) = do
      let pluginDir = baseDir </> pluginName
      resetPluginDir pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> "test.toposchema") testSchemaJSON
      writePluginWrapper pluginDir pluginName fixtureMode
      pure pluginDir

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
  path <- fixtureDataFile pluginName (label <> ".count")
  readCountFile path

fixtureDataFile :: String -> String -> IO FilePath
fixtureDataFile pluginName fileName = do
  baseDir <- currentPluginBaseDir
  pure (baseDir </> pluginName </> "data" </> fileName)

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
      let wrapperPath = pluginDir </> (pluginName <> ".exe")
      copyFile testExe wrapperPath
      writeFile (windowsFixtureModePath pluginDir) fixtureMode
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

runFixtureCliIfRequested :: IO Bool
runFixtureCliIfRequested = do
  modeResult <- fixtureCliMode
  case modeResult of
    Left shouldFail
      | shouldFail -> fixtureUsage
      | otherwise -> pure False
    Right mode -> runFixtureMode mode >> pure True

runFixtureCli :: IO ()
runFixtureCli = do
  modeResult <- fixtureCliMode
  case modeResult of
    Right mode -> runFixtureMode mode
    Left _ -> fixtureUsage

fixtureCliMode :: IO (Either Bool String)
fixtureCliMode = do
  args <- getArgs
  case args of
    ["--plugin-manager-fixture", mode] -> pure (Right mode)
    "--plugin-manager-fixture":_ -> pure (Left True)
    [] | os == "mingw32" -> do
      cwd <- getCurrentDirectory
      exists <- doesFileExist (windowsFixtureModePath cwd)
      if exists
        then Right . normalizeFixtureMode <$> readFile (windowsFixtureModePath cwd)
        else pure (Left False)
    _ -> pure (Left False)

windowsFixtureModePath :: FilePath -> FilePath
windowsFixtureModePath dir = dir </> ".topo-plugin-manager-fixture-mode"

normalizeFixtureMode :: String -> String
normalizeFixtureMode = takeWhile (`notElem` ['\r', '\n'])

fixtureUsage :: IO a
fixtureUsage = die "usage: topo-seer-test --plugin-manager-fixture <ok|env-contract|protocol-mismatch|malformed-json|bad-handshake|early-exit|slow|slow-shutdown|flaky-start|counted-early-exit|hang-query|provider-failed|validation-ok|invalid-mutate|negotiated-validation|exit-on-generator|exit-on-simulation|external-provider|external-consumer|windows-process-tree|windows-heartbeat-child>"

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
  "external-provider" -> runExternalProviderFixture
  "external-consumer" -> runExternalConsumerFixture
  "windows-process-tree" -> runWindowsProcessTreeFixture
  "windows-heartbeat-child" -> runWindowsHeartbeatChild
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

runWindowsProcessTreeFixture :: IO ()
runWindowsProcessTreeFixture
  | os /= "mingw32" = runOkFixture
  | otherwise = do
      connectPluginFromEnvironment "plugin-manager-windows-process-tree-fixture" stdin stdout >>= \case
        Left _ -> exitFailure
        Right transport -> loop transport False
  where
    loop transport childStarted = do
      recvMessage transport >>= \case
        Left _ -> threadDelay 1000000 >> loop transport childStarted
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport childStarted
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
                unless childStarted startWindowsHeartbeatChild
                loop transport True
              MsgShutdown -> loop transport childStarted
              _ -> loop transport childStarted

startWindowsHeartbeatChild :: IO ()
startWindowsHeartbeatChild = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  testExe <- getExecutablePath
  _ <- createProcess (proc testExe ["--plugin-manager-fixture", "windows-heartbeat-child"])
    { cwd = Just dataRoot
    , std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  pure ()

runWindowsHeartbeatChild :: IO ()
runWindowsHeartbeatChild = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  let heartbeatPath = dataRoot </> windowsHeartbeatFileName
      loop n = do
        BS.writeFile heartbeatPath (BSC.pack (show n))
        threadDelay 50000
        loop (n + 1 :: Int)
  loop (0 :: Int)

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

runExternalProviderFixture :: IO ()
runExternalProviderFixture = do
  recordExternalStartup externalProviderPluginName
  connectPluginFromEnvironment "plugin-manager-external-provider-fixture" stdin stdout >>= \case
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
                _ <- sendMessage transport (encodeMessage
                  (handshakeAckWithDataDirectoryEnvelope (envRequestId envelope) currentProtocolVersion (Just "external-provider-data") []))
                loop transport
              MsgExternalDataSourceStatusRequest -> do
                let includeDiagnostics = requestIncludesDiagnostics envelope
                _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
                  (envRequestId envelope)
                  (externalProviderStatusReport includeDiagnostics)))
                loop transport
              MsgQueryResource -> do
                _ <- sendMessage transport (encodeMessage (queryResultEnvelope
                  (envRequestId envelope)
                  (QueryResult externalProviderResource [externalProviderRecord] (Just 1))))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runExternalConsumerFixture :: IO ()
runExternalConsumerFixture = do
  recordExternalStartup externalConsumerPluginName
  connectPluginFromEnvironment "plugin-manager-external-consumer-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport "declared"
  where
    loop transport bindingStatus = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport bindingStatus
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage
                  (handshakeAckWithDataDirectoryEnvelope (envRequestId envelope) currentProtocolVersion (Just "external-consumer-data") []))
                loop transport bindingStatus
              MsgExternalDataSourceStatusRequest -> do
                let includeDiagnostics = requestIncludesDiagnostics envelope
                _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
                  (envRequestId envelope)
                  (externalConsumerStatusReport includeDiagnostics)))
                loop transport bindingStatus
              MsgExternalDataSourceGrant -> loop transport "granted"
              MsgExternalDataSourceRevoke -> loop transport "revoked"
              MsgQueryResource -> do
                _ <- sendMessage transport (encodeMessage (queryResultEnvelope
                  (envRequestId envelope)
                  (QueryResult externalBindingResource [externalBindingRecord bindingStatus] (Just 1))))
                loop transport bindingStatus
              MsgShutdown -> closeTransport transport
              _ -> loop transport bindingStatus

requestIncludesDiagnostics :: RPCEnvelope -> Bool
requestIncludesDiagnostics envelope = case Aeson.fromJSON (envPayload envelope) of
  Aeson.Success (request :: RPCExternalDataSourceStatusRequest) -> redssrIncludeDiagnostics request
  Aeson.Error _ -> False

externalStatusEnvelope :: Maybe Word64 -> RPCExternalDataSourceStatusReport -> RPCEnvelope
externalStatusEnvelope requestId report = RPCEnvelope
  { envType = MsgExternalDataSourceStatus
  , envPayload = Aeson.toJSON report
  , envRequestId = requestId
  }

externalProviderStatusReport :: Bool -> RPCExternalDataSourceStatusReport
externalProviderStatusReport includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses = [externalProviderStatusEntry includeDiagnostics]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalConsumerStatusReport :: Bool -> RPCExternalDataSourceStatusReport
externalConsumerStatusReport includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses = [externalConsumerStatusEntry includeDiagnostics]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderStatusEntry :: Bool -> RPCExternalDataSourceStatusEntry
externalProviderStatusEntry includeDiagnostics = RPCExternalDataSourceStatusEntry
  { redsstProviderId = externalProviderPluginNameText
  , redsstConsumerId = Nothing
  , redsstSource = externalSourceName
  , redsstGrant = Just externalGrantName
  , redsstAccess = externalReadAccess
  , redsstResources = externalSharedResources
  , redsstCapabilityScope = externalCapabilities
  , redsstStatus = statusWithOptionalDiagnostics includeDiagnostics
  , redsstReference = Just externalGrantReference
  , redsstConfigRefs = [externalGrantConfigRef]
  , redsstDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalConsumerStatusEntry :: Bool -> RPCExternalDataSourceStatusEntry
externalConsumerStatusEntry includeDiagnostics = (externalProviderStatusEntry includeDiagnostics)
  { redsstConsumerId = Just externalSourceName
  , redsstReference = Just externalConsumerReference
  , redsstConfigRefs = [externalConsumerConfigRef]
  }

statusWithOptionalDiagnostics :: Bool -> RPCExternalDataSourceStatus
statusWithOptionalDiagnostics includeDiagnostics
  | includeDiagnostics = externalReadyStatus
  | otherwise = externalReadyStatus { redssDiagnostics = Nothing }

externalProviderRecord :: DataRecord
externalProviderRecord = record
  [ ("source_id", String externalSourceName)
  , ("endpoint", String "fixture://provider/terrain.catalog")
  ]

externalBindingRecord :: Text -> DataRecord
externalBindingRecord bindingStatus = record
  [ ("source_id", String externalSourceName)
  , ("provider", String externalProviderPluginNameText)
  , ("grant", String externalGrantName)
  , ("status", String bindingStatus)
  ]

recordExternalStartup :: String -> IO ()
recordExternalStartup pluginName = do
  baseDir <- requireEnv testPluginDirEnv
  appendFile (baseDir </> "external-startup.log") (pluginName <> "\n")

readExternalStartupOrder :: IO [String]
readExternalStartupOrder = do
  baseDir <- currentPluginBaseDir
  let path = baseDir </> "external-startup.log"
  exists <- doesFileExist path
  if exists
    then lines <$> readFile path
    else pure []

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
handshakeAckWithResourcesEnvelope requestId protocolVersion =
  handshakeAckWithDataDirectoryEnvelope requestId protocolVersion Nothing

handshakeAckWithDataDirectoryEnvelope :: Maybe Word64 -> Int -> Maybe Text -> [DataResourceSchema] -> RPCEnvelope
handshakeAckWithDataDirectoryEnvelope requestId protocolVersion dataDirectory resources = RPCEnvelope
  { envType = MsgHandshakeAck
  , envPayload = Aeson.toJSON (HandshakeAck
      { haProtocolVersion = protocolVersion
      , haDataDirectory = dataDirectory
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

cleanupAbortPluginName :: String
cleanupAbortPluginName = "copilot-test-plugin-cleanup-abort"

cleanupAbortManifestJSON :: BS.ByteString
cleanupAbortManifestJSON = manifestFor cleanupAbortPluginName

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

interruptedRefreshPluginName :: String
interruptedRefreshPluginName = "copilot-test-plugin-refresh-interrupted"

interruptedRefreshManifestJSON :: BS.ByteString
interruptedRefreshManifestJSON = manifestFor interruptedRefreshPluginName

connectedRefreshInterruptedPluginName :: String
connectedRefreshInterruptedPluginName = "copilot-test-plugin-refresh-connected-interrupted"

connectedRefreshInterruptedManifestJSON :: BS.ByteString
connectedRefreshInterruptedManifestJSON = manifestFor connectedRefreshInterruptedPluginName

connectedRefreshInterruptedDisabledManifestJSON :: BS.ByteString
connectedRefreshInterruptedDisabledManifestJSON = manifestWithStartPolicyFor connectedRefreshInterruptedPluginName
  [ "    \"auto_start\": false,"
  , "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 1000"
  ]

shutdownTransientPluginName :: String
shutdownTransientPluginName = "copilot-test-plugin-shutdown-transient"

shutdownTransientManifestJSON :: BS.ByteString
shutdownTransientManifestJSON = manifestFor shutdownTransientPluginName

interruptedShutdownPluginName :: String
interruptedShutdownPluginName = "copilot-test-plugin-shutdown-interrupted"

interruptedShutdownManifestJSON :: BS.ByteString
interruptedShutdownManifestJSON = manifestFor interruptedShutdownPluginName

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

windowsProcessTreePluginName :: String
windowsProcessTreePluginName = "copilot-test-plugin-windows-process-tree"

windowsProcessTreeManifestJSON :: BS.ByteString
windowsProcessTreeManifestJSON = manifestWithStartPolicyFor windowsProcessTreePluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

windowsHeartbeatFileName :: String
windowsHeartbeatFileName = "windows-child-heartbeat.txt"

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

blockedExternalRefManifestFor :: String -> BS.ByteString
blockedExternalRefManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"externalDataSourceRefs\": [\n"
    <> "    {\n"
    <> "      \"name\": \"settlements\",\n"
    <> "      \"provider\": \"external-ledger\",\n"
    <> "      \"source\": \"settlements\",\n"
    <> "      \"required\": true,\n"
    <> "      \"access\": [\"read\"],\n"
    <> "      \"grant\": \"settlement-read\",\n"
    <> "      \"status\": {\n"
    <> "        \"state\": \"unavailable\",\n"
    <> "        \"providerId\": \"external-ledger\",\n"
    <> "        \"availability\": \"unavailable\",\n"
    <> "        \"health\": \"unhealthy\",\n"
    <> "        \"accessMode\": \"disabled\"\n"
    <> "      }\n"
    <> "    }\n"
    <> "  ],\n"
    <> "  \"startPolicy\": {\n"
    <> "    \"restart_mode\": \"never\",\n"
    <> "    \"startup_timeout_ms\": 100,\n"
    <> "    \"request_timeout_ms\": 100,\n"
    <> "    \"shutdown_timeout_ms\": 100\n"
    <> "  }\n"
    <> "}\n"

externalOnlyProviderManifestFor :: String -> BS.ByteString
externalOnlyProviderManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": 3, \"max\": 3 } },\n"
    <> "  \"externalDataSources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"settlements\",\n"
    <> "      \"label\": \"Settlements\",\n"
    <> "      \"kind\": \"catalog\",\n"
    <> "      \"capabilities\": [\"query\", \"health\"],\n"
    <> "      \"status\": {\n"
    <> "        \"state\": \"ready\",\n"
    <> "        \"providerId\": \"" <> name <> "\",\n"
    <> "        \"availability\": \"available\",\n"
    <> "        \"health\": \"healthy\",\n"
    <> "        \"accessMode\": \"read_only\",\n"
    <> "        \"capabilityScope\": [\"query\", \"health\"]\n"
    <> "      }\n"
    <> "    }\n"
    <> "  ],\n"
    <> "  \"startPolicy\": {\n"
    <> "    \"restart_mode\": \"never\",\n"
    <> "    \"startup_timeout_ms\": 1000,\n"
    <> "    \"request_timeout_ms\": 100,\n"
    <> "    \"shutdown_timeout_ms\": 100\n"
    <> "  }\n"
    <> "}\n"

externalProviderPluginName :: String
externalProviderPluginName = "z-copilot-test-plugin-external-provider"

externalConsumerPluginName :: String
externalConsumerPluginName = "a-copilot-test-plugin-external-consumer"

externalProviderPluginNameText :: Text
externalProviderPluginNameText = Text.pack externalProviderPluginName

externalConsumerPluginNameText :: Text
externalConsumerPluginNameText = Text.pack externalConsumerPluginName

externalSourceName :: Text
externalSourceName = "terrain.catalog"

externalGrantName :: Text
externalGrantName = "terrain-catalog-read"

externalBindingResource :: Text
externalBindingResource = "source_bindings"

externalProviderResource :: Text
externalProviderResource = "shared_sources"

externalSharedResources :: [Text]
externalSharedResources = [externalProviderResource]

externalCapabilities :: [RPCExternalDataSourceCapability]
externalCapabilities = [ExternalSourceQuery, ExternalSourceHealth]

externalReadAccess :: [RPCExternalDataSourceAccess]
externalReadAccess = [ExternalAccessRead]

externalIntegrationWorldName :: Text
externalIntegrationWorldName = "__topo_test_external_provider_consumer__"

externalDiagnostics :: Value
externalDiagnostics = object
  [ "backend" .= ("fixture-service" :: Text)
  , "owner" .= ("provider-managed" :: Text)
  ]

externalSourceReference :: Value
externalSourceReference = object ["handle" .= ("fixture://provider/terrain.catalog" :: Text)]

externalGrantReference :: Value
externalGrantReference = object ["grant" .= externalGrantName]

externalConsumerReference :: Value
externalConsumerReference = object ["binding" .= ("fixture://consumer/terrain.catalog" :: Text)]

externalProviderConfigRef :: RPCExternalDataSourceConfigRef
externalProviderConfigRef = RPCExternalDataSourceConfigRef
  { redscrName = "terrain-catalog-binding"
  , redscrOrigin = ExternalConfigProvider
  , redscrKey = "fixture.provider.terrain.catalog"
  , redscrRequired = True
  , redscrCompatibility = Just "manifest-v3"
  , redscrMetadata = Just externalSourceReference
  }

externalGrantConfigRef :: RPCExternalDataSourceConfigRef
externalGrantConfigRef = RPCExternalDataSourceConfigRef
  { redscrName = "terrain-catalog-read-binding"
  , redscrOrigin = ExternalConfigProvider
  , redscrKey = "fixture.provider.terrain.catalog.read"
  , redscrRequired = True
  , redscrCompatibility = Just "manifest-v3"
  , redscrMetadata = Just externalGrantReference
  }

externalConsumerConfigRef :: RPCExternalDataSourceConfigRef
externalConsumerConfigRef = RPCExternalDataSourceConfigRef
  { redscrName = "terrain-catalog-consumer-binding"
  , redscrOrigin = ExternalConfigDeployment
  , redscrKey = "fixture.consumer.terrain.catalog"
  , redscrRequired = True
  , redscrCompatibility = Just "manifest-v3"
  , redscrMetadata = Just externalConsumerReference
  }

externalReadyStatus :: RPCExternalDataSourceStatus
externalReadyStatus = defaultRPCExternalDataSourceStatus
  { redssState = ExternalStatusReady
  , redssProviderId = Just externalProviderPluginNameText
  , redssAvailability = Just ExternalAvailabilityAvailable
  , redssHealth = Just ExternalHealthHealthy
  , redssAccessMode = Just ExternalAccessModeReadOnly
  , redssCapabilityScope = externalCapabilities
  , redssCompatibility = Just "manifest-v3"
  , redssDiagnostics = Just externalDiagnostics
  }

externalProviderStatusRequest :: RPCExternalDataSourceStatusRequest
externalProviderStatusRequest = RPCExternalDataSourceStatusRequest
  { redssrProviderId = Just externalProviderPluginNameText
  , redssrConsumerId = Nothing
  , redssrSources = [externalSourceName]
  , redssrGrants = [externalGrantName]
  , redssrIncludeDiagnostics = True
  , redssrReference = Nothing
  }

externalConsumerStatusRequest :: RPCExternalDataSourceStatusRequest
externalConsumerStatusRequest = externalProviderStatusRequest
  { redssrConsumerId = Just externalSourceName
  }

externalBindingQuery :: QueryResource
externalBindingQuery = QueryResource
  { qrResource = externalBindingResource
  , qrQuery = QueryAll
  , qrPageSize = Just 20
  , qrPageOffset = Just 0
  }

externalGrantMessage :: RPCExternalDataSourceGrantMessage
externalGrantMessage = RPCExternalDataSourceGrantMessage
  { redsgmProviderId = externalProviderPluginNameText
  , redsgmConsumerId = Just externalConsumerPluginNameText
  , redsgmSource = externalSourceName
  , redsgmGrant = externalGrantName
  , redsgmAccess = externalReadAccess
  , redsgmResources = externalSharedResources
  , redsgmCapabilityScope = externalCapabilities
  , redsgmStatus = externalReadyStatus
  , redsgmReference = Just externalGrantReference
  , redsgmConfigRefs = [externalGrantConfigRef]
  , redsgmDiagnostics = Just externalDiagnostics
  }

externalGrantRevocation :: RPCExternalDataSourceGrantRevocation
externalGrantRevocation = RPCExternalDataSourceGrantRevocation
  { redsrvProviderId = externalProviderPluginNameText
  , redsrvConsumerId = Just externalConsumerPluginNameText
  , redsrvSource = externalSourceName
  , redsrvGrant = externalGrantName
  , redsrvReason = Just "provider disabled for integration test"
  , redsrvStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusUnavailable
      , redssMessage = Just "provider disabled for integration test"
      , redssProviderId = Just externalProviderPluginNameText
      , redssAvailability = Just ExternalAvailabilityUnavailable
      , redssHealth = Just ExternalHealthUnhealthy
      , redssAccessMode = Just ExternalAccessModeDisabled
      , redssCompatibility = Just "manifest-v3"
      }
  , redsrvReference = Just externalGrantReference
  , redsrvDiagnostics = Just externalDiagnostics
  }

externalProviderManifestJSON :: BS.ByteString
externalProviderManifestJSON = externalProviderManifestFor externalProviderPluginName

externalConsumerManifestJSON :: BS.ByteString
externalConsumerManifestJSON = externalConsumerManifestFor externalConsumerPluginName externalProviderPluginName

externalProviderManifestFor :: String -> BS.ByteString
externalProviderManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"shared_sources\",\n"
    <> "      \"label\": \"Shared Sources\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [\n"
    <> "        { \"name\": \"source_id\", \"type\": \"text\", \"label\": \"Source ID\" },\n"
    <> "        { \"name\": \"endpoint\", \"type\": \"text\", \"label\": \"Endpoint\" }\n"
    <> "      ],\n"
    <> "      \"operations\": { \"list\": true, \"queryByField\": true, \"page\": true },\n"
    <> "      \"keyField\": \"source_id\"\n"
    <> "    }\n"
    <> "  ],\n"
    <> "  \"externalDataSources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"terrain.catalog\",\n"
    <> "      \"label\": \"Terrain Catalog\",\n"
    <> "      \"description\": \"Provider-owned terrain catalog fixture\",\n"
    <> "      \"kind\": \"catalog\",\n"
    <> "      \"capabilities\": [\"query\", \"health\"],\n"
    <> "      \"resources\": [\"shared_sources\"],\n"
    <> "      \"status\": " <> externalReadyStatusJSON name <> ",\n"
    <> "      \"connection\": { \"handle\": \"fixture://provider/terrain.catalog\" },\n"
    <> "      \"configRefs\": [\n"
    <> "        { \"name\": \"terrain-catalog-binding\", \"origin\": \"provider\", \"key\": \"fixture.provider.terrain.catalog\", \"required\": true, \"compatibility\": \"manifest-v3\", \"metadata\": { \"handle\": \"fixture://provider/terrain.catalog\" } }\n"
    <> "      ],\n"
    <> "      \"grants\": [\n"
    <> "        {\n"
    <> "          \"name\": \"terrain-catalog-read\",\n"
    <> "          \"access\": [\"read\"],\n"
    <> "          \"capabilities\": [\"query\", \"health\"],\n"
    <> "          \"resources\": [\"shared_sources\"],\n"
    <> "          \"status\": " <> externalReadyStatusJSON name <> ",\n"
    <> "          \"reference\": { \"grant\": \"terrain-catalog-read\" },\n"
    <> "          \"configRefs\": [\n"
    <> "            { \"name\": \"terrain-catalog-read-binding\", \"origin\": \"provider\", \"key\": \"fixture.provider.terrain.catalog.read\", \"required\": true, \"compatibility\": \"manifest-v3\", \"metadata\": { \"grant\": \"terrain-catalog-read\" } }\n"
    <> "          ]\n"
    <> "        }\n"
    <> "      ]\n"
    <> "    }\n"
    <> "  ]"
    <> externalIntegrationStartPolicy
    <> "\n}\n"

externalConsumerManifestFor :: String -> String -> BS.ByteString
externalConsumerManifestFor name providerName = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"source_bindings\",\n"
    <> "      \"label\": \"Source Bindings\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [\n"
    <> "        { \"name\": \"source_id\", \"type\": \"text\", \"label\": \"Source ID\" },\n"
    <> "        { \"name\": \"provider\", \"type\": \"text\", \"label\": \"Provider\" },\n"
    <> "        { \"name\": \"grant\", \"type\": \"text\", \"label\": \"Grant\" },\n"
    <> "        { \"name\": \"status\", \"type\": \"text\", \"label\": \"Status\" }\n"
    <> "      ],\n"
    <> "      \"operations\": { \"list\": true, \"queryByField\": true, \"page\": true },\n"
    <> "      \"keyField\": \"source_id\"\n"
    <> "    }\n"
    <> "  ],\n"
    <> "  \"externalDataSourceRefs\": [\n"
    <> "    {\n"
    <> "      \"name\": \"terrain.catalog\",\n"
    <> "      \"provider\": \"" <> providerName <> "\",\n"
    <> "      \"source\": \"terrain.catalog\",\n"
    <> "      \"required\": true,\n"
    <> "      \"access\": [\"read\"],\n"
    <> "      \"resources\": [\"shared_sources\"],\n"
    <> "      \"grant\": \"terrain-catalog-read\",\n"
    <> "      \"status\": " <> externalReadyStatusJSON providerName <> ",\n"
    <> "      \"reference\": { \"binding\": \"fixture://consumer/terrain.catalog\" },\n"
    <> "      \"configRefs\": [\n"
    <> "        { \"name\": \"terrain-catalog-consumer-binding\", \"origin\": \"deployment\", \"key\": \"fixture.consumer.terrain.catalog\", \"required\": true, \"compatibility\": \"manifest-v3\", \"metadata\": { \"binding\": \"fixture://consumer/terrain.catalog\" } }\n"
    <> "      ]\n"
    <> "    }\n"
    <> "  ]"
    <> externalIntegrationStartPolicy
    <> "\n}\n"

externalReadyStatusJSON :: String -> String
externalReadyStatusJSON providerName =
  "{ \"state\": \"ready\", \"providerId\": \"" <> providerName <> "\", \"availability\": \"available\", \"health\": \"healthy\", \"accessMode\": \"read_only\", \"capabilityScope\": [\"query\", \"health\"], \"compatibility\": \"manifest-v3\", \"diagnostics\": { \"backend\": \"fixture-service\", \"owner\": \"provider-managed\" } }"

externalIntegrationStartPolicy :: String
externalIntegrationStartPolicy =
  ",\n  \"startPolicy\": {\n"
    <> "    \"restart_mode\": \"never\",\n"
    <> "    \"startup_timeout_ms\": 1000,\n"
    <> "    \"request_timeout_ms\": 500,\n"
    <> "    \"shutdown_timeout_ms\": 300\n"
    <> "  }"

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
