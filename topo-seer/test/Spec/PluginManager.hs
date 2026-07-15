{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.PluginManager (spec, runFixtureCli, runFixtureCliIfRequested) where

import Control.Concurrent
  ( MVar
  , ThreadId
  , forkIO
  , killThread
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , threadDelay
  )
import Control.Exception
  ( Exception
  , IOException
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
import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt(..))
#if !defined(mingw32_HOST_OS)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (createSession, getProcessID)
import System.Posix.Signals (nullSignal, sigKILL, signalProcess, signalProcessGroup)
#else
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
#endif
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, call, get, newActorSystem, shutdownActorSystem)
import System.Directory
  ( Permissions(..)
  , copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , getHomeDirectory
  , getPermissions
  , getTemporaryDirectory
  , removePathForcibly
  , renameFile
  , setPermissions
  )
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..), die, exitFailure)
import System.FilePath (isAbsolute, (</>), takeDirectory, takeFileName)
import System.Info (os)
import System.IO (stdin, stdout)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , getPid
  , getProcessExitCode
  , proc
  , terminateProcess
  )
import System.Timeout (timeout)
import Test.Hspec

import Actor.UI (UiState(..), emptyUiState)
import Actor.PluginManager
  ( LoadedPlugin(..)
  , OwnedPluginCleanupResult(..)
  , OwnedPluginProcess
  , OwnedPluginRuntime
  , OwnedPluginRuntimeCleanupResult(..)
  , PluginDiagnosticState(..)
  , PluginExternalDataSourceDiagnostic(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManager
  , PluginParamUpdateError(..)
  , PluginSimulationNodeDiagnostic(..)
  , PluginSimulationPlan(..)
  , PluginStatus(..)
  , buildPluginSimulationPlanForPlugins
  , canRestartPlugin
  , cleanupOwnedPluginProcess
  , cleanupOwnedPluginRuntime
  , discoverPlugins
  , getDisabledPlugins
  , getLoadedPlugins
  , getPluginDataDirectories
  , getPluginDataResources
  , getPluginExternalDataSources
  , getPluginOverlaySchemas
  , getPluginStages
  , lpConnection
  , lpProcessHandle
  , mutatePluginResource
  , newConnectionOnlyPluginRuntime
  , ownedPluginProcessHandle
  , ownedPluginProcessId
  , ownedPluginRuntimeGeneration
  , ownedPluginRuntimeProcess
  , pluginDependencyDiagnostics
  , pluginDiagnosticState
  , pluginExternalDataSourceDiagnosticsFor
  , pluginLifecycleSnapshot
  , pruneRestartHistory
  , queryPluginResource
  , recordPluginRestart
  , refreshManifests
  , runtimeProcessExitNotice
  , setDisabledPlugins
  , setPluginParam
  , shutdownPlugins
  )
import Topo.Calendar (CalendarDate(..), defaultWorldTime)
import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay, emptyOverlay)
import Topo.Overlay.Schema (OverlayDeps(..), OverlaySchema(..), OverlayStorage(..))
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (Capability(..))
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
  ( WorldPluginDataDirectory(..)
  , WorldExternalDataSourceSnapshot(..)
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
  , RPCExternalDataSourceOperation(..)
  , RPCExternalDataSourceOperationResult(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , RPCManifestRuntime(..)
  , RPCParamSpec(..)
  , RPCParamType(..)
  , RPCParamValidationError(..)
  , RPCRestartMode(..)
  , RPCStartPolicy(..)
  , RPCOverlayDecl(..)
  , RPCSimulationDecl(..)
  , defaultRPCExternalDataSourceStatus
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , dataResourceFailureText
  , invokeGenerator
  , invokeSimulation
  , newRPCConnection
  , queryResource
  , requestExternalDataSourceStatus
  , rpcErrorDataResourceFailure
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , DataResourceErrorCode(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  , dataResourceErrorRPCCode
  )
import Topo.Plugin.RPC.Protocol
  ( Handshake(..)
  , HandshakeAck(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , currentProtocolVersion
  , decodeMessage
  , encodeMessage
  , handshakeAuthProof
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , TransportConfig(..)
  , TransportEndpoint(..)
  , TransportEndpointKind(..)
  , TransportError(..)
  , TransportPeerPolicy(..)
  , TransportServer(..)
  , closeTransport
  , connectPluginEndpoint
  , connectPluginFromEnvironment
  , defaultTransportConfig
  , pluginAuthTokenEnv
  , pluginDataRootEnv
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginIdEnv
  , pluginMaxFrameSizeEnv
  , pluginProtocolEnv
  , pluginSessionEnv
  , pluginStdioCompatibilityEnv
  , pluginWorldIdEnv
  , openPluginServer
  , recvMessage
  , sendMessage
  )
import Topo.Simulation (SimContext(..), defaultScheduleDecl)
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
  it "rejects stale refresh completion and cancellation tokens" $
    withPluginManager $ \pluginManagerHandle -> do
      Just (oldToken, _, oldPlugins) <- call @"refresh" pluginManagerHandle #refresh ()
      overlapping <- call @"refresh" pluginManagerHandle #refresh ()
      case overlapping of
        Nothing -> pure ()
        Just _ -> expectationFailure "overlapping refresh was not rejected"
      (cancelledOld, oldDirectives) <- call @"cancelRefresh" pluginManagerHandle #cancelRefresh (oldToken, oldPlugins)
      cancelledOld `shouldBe` False
      length oldDirectives `shouldBe` 0
      Just (currentToken, _, currentPlugins) <- call @"refresh" pluginManagerHandle #refresh ()
      call @"finishRefresh" pluginManagerHandle #finishRefresh (oldToken, []) `shouldReturn` False
      (cancelledStale, staleDirectives) <- call @"cancelRefresh" pluginManagerHandle #cancelRefresh (oldToken, [])
      cancelledStale `shouldBe` False
      length staleDirectives `shouldBe` 0
      call @"finishRefresh" pluginManagerHandle #finishRefresh (currentToken, currentPlugins) `shouldReturn` True

  it "lifecycle-matrix enforces zero and one restart limits across rolling windows without sleeping" $ do
    let epoch = posixSecondsToUTCTime 0
        insideWindow = posixSecondsToUTCTime 0.05
        outsideWindow = posixSecondsToUTCTime 0.2
        oneRestart = defaultRPCStartPolicy
          { rspRestartMode = RestartOnFailure
          , rspMaxRestarts = 1
          , rspRestartWindowMs = 100
          }
        zeroRestarts = oneRestart { rspMaxRestarts = 0 }
        oneAttempt = recordPluginRestart oneRestart epoch []
    canRestartPlugin zeroRestarts epoch [] `shouldBe` False
    canRestartPlugin oneRestart epoch [] `shouldBe` True
    canRestartPlugin oneRestart insideWindow oneAttempt `shouldBe` False
    pruneRestartHistory oneRestart outsideWindow oneAttempt `shouldBe` []
    canRestartPlugin oneRestart outsideWindow oneAttempt `shouldBe` True

  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "recovers plugin config by sanitizing saved values against parameter specs" $ do
    withIsolatedPluginHome "config-recovery" $ do
      baseDir <- currentPluginBaseDir
      let pluginDir = baseDir </> "config-recovery"
      resetPluginDir pluginDir
      BS.writeFile (pluginDir </> "manifest.json") (paramValidationManifestJSON 1 0.5)
      BL.writeFile (pluginDir </> "config.json") $ Aeson.encode $ object
        [ "density" .= String "dense"
        , "iterations" .= Number 99
        , "unknown" .= Bool False
        ]
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        fmap lpParams (findLoadedPlugin "validation-plugin" loaded) `shouldBe` Just (Map.fromList
          [ ("enabled", Bool True)
          , ("density", Number 0.5)
          , ("iterations", Number 3)
          ])

  it "uses manifest defaults when persisted plugin config is invalid JSON" $ do
    withIsolatedPluginHome "config-invalid-json" $ do
      baseDir <- currentPluginBaseDir
      let pluginDir = baseDir </> "config-invalid-json"
      resetPluginDir pluginDir
      BS.writeFile (pluginDir </> "manifest.json") (paramValidationManifestJSON 1 0.5)
      BS.writeFile (pluginDir </> "config.json") "{not-json"
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        fmap lpParams (findLoadedPlugin "validation-plugin" loaded) `shouldBe` Just (Map.fromList
          [ ("enabled", Bool True)
          , ("density", Number 0.5)
          , ("iterations", Number 3)
          ])

  it "validates parameter updates synchronously in the plugin manager facade" $ do
    withTestPluginDir paramValidationPluginName (paramValidationManifestJSON 1 0.5) testSchemaJSON $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        wrongType <- setPluginParam pluginManagerHandle "validation-plugin" "enabled" (String "yes")
        expectParamValidation ["value"] wrongType
        unknownParam <- setPluginParam pluginManagerHandle "validation-plugin" "missing" (Bool True)
        expectParamValidation ["param"] unknownParam
        unknownPlugin <- setPluginParam pluginManagerHandle "missing-plugin" "enabled" (Bool True)
        unknownPlugin `shouldBe` Left (PluginParamUnknownPlugin "missing-plugin")
        loaded <- getLoadedPlugins pluginManagerHandle
        (lookupPluginParam "validation-plugin" "enabled" loaded) `shouldBe` Just (Bool True)
        configPath <- (</> "validation-plugin" </> "config.json") <$> currentPluginBaseDir
        doesFileExist configPath `shouldReturn` False

        success <- setPluginParam pluginManagerHandle "validation-plugin" "density" (Number 0.7)
        success `shouldBe` Right (Number 0.7)
        loadedAfterSuccess <- getLoadedPlugins pluginManagerHandle
        lookupPluginParam "validation-plugin" "density" loadedAfterSuccess `shouldBe` Just (Number 0.7)
        savedBytes <- BL.readFile configPath
        case (Aeson.decode savedBytes :: Maybe (Map.Map Text Value)) of
          Just saved -> saved `shouldBe` Map.fromList
            [ ("enabled", Bool True)
            , ("density", Number 0.7)
            , ("iterations", Number 3)
            ]
          Nothing -> expectationFailure "expected saved plugin config JSON object"

  it "sanitizes preserved params when manifests are refreshed" $ do
    withTestPluginDir paramValidationPluginName (paramValidationManifestJSON 1 0.5) testSchemaJSON $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        setResult <- setPluginParam pluginManagerHandle "validation-plugin" "density" (Number 0.8)
        setResult `shouldBe` Right (Number 0.8)
        baseDir <- currentPluginBaseDir
        let manifestPath = baseDir </> paramValidationPluginName </> "manifest.json"
        BS.writeFile manifestPath (paramValidationManifestJSON 0.5 0.25)
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        lookupPluginParam "validation-plugin" "density" loaded `shouldBe` Just (Number 0.25)

  it "keeps simulation node dependencies out of plugin startup diagnostics" $ do
    let now = posixSecondsToUTCTime 0
        manifest = RPCManifest
          { rmManifestVersion = 3
          , rmName = "weather-consumer"
          , rmVersion = "1.0.0"
          , rmRuntime = RPCManifestRuntime currentProtocolVersion currentProtocolVersion Nothing Nothing
          , rmDescription = ""
          , rmUiHints = defaultRPCUIHints
          , rmGenerator = Nothing
          , rmSimulation = Just RPCSimulationDecl
              { rsdDependencies = ["weather"]
              , rsdSchedule = defaultScheduleDecl
              }
          , rmOverlay = Just (RPCOverlayDecl "weather-consumer.toposchema")
          , rmCapabilities = []
          , rmParameters = []
          , rmDataResources = []
          , rmDataDirectory = Nothing
          , rmExternalDataSources = []
          , rmExternalDataSourceRefs = []
          , rmStartPolicy = defaultRPCStartPolicy
          }
        loaded = LoadedPlugin
          { lpName = "weather-consumer"
          , lpManifest = manifest
          , lpParams = Map.empty
          , lpStatus = PluginConnected
          , lpLifecycle = pluginLifecycleSnapshot now LifecycleReady Nothing Nothing Nothing Nothing Nothing (Just currentProtocolVersion) []
          , lpRuntime = Nothing
          , lpStartPolicy = defaultRPCStartPolicy
          , lpRestartHistory = []
          , lpDirectory = ""
          , lpOverlaySchema = Nothing
          }
    pluginDependencyDiagnostics Set.empty loaded `shouldBe` []
    pluginDiagnosticState Set.empty Set.empty loaded `shouldBe` DiagnosticReady

  it "treats weather dependencies as host built-in simulation node dependencies" $ do
    let plugin = simulationPlanPlugin "weather-consumer" ["weather"] [CapWriteOverlay]
        plan = buildPluginSimulationPlanForPlugins (Just ["weather-consumer", "weather"]) [plugin]
    length (pspExecutableNodes plan) `shouldBe` 1
    case pspDiagnostics plan of
      [diagnostic] -> do
        psndDependencies diagnostic `shouldBe` ["weather"]
        psndExecutable diagnostic `shouldBe` True
        psndStatus diagnostic `shouldBe` "Ready"
        psndStatusDetail diagnostic `shouldSatisfy` maybe False (Text.isInfixOf "eligible for binding")
      _ -> expectationFailure "expected one plugin simulation declaration diagnostic"

  it "diagnoses plugin simulation declarations named weather as host built-in collisions" $ do
    let plugin = simulationPlanPlugin "weather" [] [CapWriteOverlay]
        plan = buildPluginSimulationPlanForPlugins (Just ["weather"]) [plugin]
    length (pspExecutableNodes plan) `shouldBe` 0
    case pspDiagnostics plan of
      [diagnostic] -> do
        psndExecutable diagnostic `shouldBe` False
        psndStatus diagnostic `shouldBe` "WaitingForDependencies"
        psndStatusDetail diagnostic `shouldSatisfy` maybe False (Text.isInfixOf "weather is a host built-in simulation node")
      _ -> expectationFailure "expected one builtin-collision plugin simulation diagnostic"

  it "marks simulation declarations without writeOverlay/writeWorld non-executable" $ do
    let plugin = simulationPlanPlugin "overlay-updater" [] []
        plan = buildPluginSimulationPlanForPlugins (Just ["overlay-updater"]) [plugin]
    length (pspExecutableNodes plan) `shouldBe` 0
    case pspDiagnostics plan of
      [diagnostic] -> do
        psndExecutable diagnostic `shouldBe` False
        psndStatus diagnostic `shouldBe` "WaitingForDependencies"
        psndStatusDetail diagnostic `shouldSatisfy` maybe False (Text.isInfixOf "writeOverlay/writeWorld")
      _ -> expectationFailure "expected one missing-writeOverlay plugin simulation diagnostic"

  it "treats writeWorld as sufficient for executable plugin simulation plans" $ do
    let plugin = simulationPlanPlugin "world-writer" [] [CapWriteWorld]
        plan = buildPluginSimulationPlanForPlugins (Just ["world-writer"]) [plugin]
    length (pspExecutableNodes plan) `shouldBe` 1
    case pspDiagnostics plan of
      [diagnostic] -> do
        psndExecutable diagnostic `shouldBe` True
        psndWritesTerrain diagnostic `shouldBe` True
        psndStatus diagnostic `shouldBe` "Ready"
      _ -> expectationFailure "expected one executable writeWorld plugin simulation diagnostic"

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
        let owners = pluginOwnedProcesses testLaunchPluginName loaded
            handles = map ownedPluginProcessHandle owners
        length owners `shouldBe` 1
        shutdownPlugins pluginManagerHandle
        shutdownPlugins pluginManagerHandle
        loadedAfterShutdown <- waitForLoadedPlugins
          (testLaunchPluginName <> " repeated shutdown cleanup")
          pluginManagerHandle
          (\loaded ->
            PluginDisconnected `elem` pluginStatuses testLaunchPluginName loaded
              && LifecycleStopped `elem` pluginLifecycleStates testLaunchPluginName loaded
              && null (pluginProcessHandles testLaunchPluginName loaded))
        pluginStatuses testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem PluginDisconnected
        pluginLifecycleStates testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem LifecycleStopped
        mapM_ (assertProcessExited testLaunchPluginName) handles
        forM_ owners $ \owner -> do
          assertOwnedCleanupComplete owner
          assertOwnedCleanupComplete owner

  it "lifecycle-matrix startup handoffs report exact lifecycle and ownership for every injected phase" $
    withExecutablePluginDir testLaunchPluginName startupHandoffManifestJSON "ok" $
      withPluginManager $ \pluginManagerHandle -> do
        forM_
          [ ("listener", False)
          , ("process", False)
          , ("accept", True)
          , ("handshake", True)
          , ("prepublication", True)
          , ("publication", True)
          ] $ \(phase, rollsBack) -> do
            discoverPlugins pluginManagerHandle
            result <- withEnvironmentValue "TOPO_TEST_PLUGIN_STARTUP_FAILURE" phase $
              try @SomeException (refreshManifests pluginManagerHandle)
            loaded <- getLoadedPlugins pluginManagerHandle
            assertStartupHandoffOutcome phase rollsBack result loaded

        discoverPlugins pluginManagerHandle
        containmentResult <- withEnvironmentValue "TOPO_TEST_PLUGIN_CONTAINMENT_FAILURE" "1" $
          try @SomeException (refreshManifests pluginManagerHandle)
        containment <- getLoadedPlugins pluginManagerHandle
        assertStartupHandoffOutcome "containment" False containmentResult containment

  forM_
    [ "listener"
    , "process"
    , "accept"
    , "handshake"
    , "prepublication"
    , "publication"
    ] $ \phase ->
      it ("lifecycle-cancellation-" <> phase <> " leaves no residual resources") $
        withExecutablePluginDir testLaunchPluginName startupHandoffManifestJSON "startup-pause-probe" $
          withPluginManager $ \pluginManagerHandle -> do
            heartbeatPath <- fixtureDataFile testLaunchPluginName processTreeHeartbeatFileName
            baseDir <- currentPluginBaseDir
            let markerPath = baseDir </> ("startup-pause-" <> phase <> ".marker")
                processExpected = phase `elem` ["accept", "handshake", "prepublication", "publication"]
                endpointExpected = phase `elem` ["listener", "process", "accept"]
            discoverPlugins pluginManagerHandle
            (cancelled, diagnostic, mProcessPid) <- withEnvironmentValue startupPauseTestEnv phase $
              withEnvironmentValue startupPauseMarkerTestEnv markerPath $ do
                (worker, done) <- forkCancellableLifecycleAction
                  (refreshManifests pluginManagerHandle)
                expectWithin (phase <> " pause marker") (waitForFixtureSignal markerPath)
                diagnostic <- Text.strip . Text.pack <$> readFile markerPath
                mProcessPid <- if processExpected
                  then expectHeartbeatAdvances heartbeatPath >> Just <$> readProcessTreeChildPid testLaunchPluginName
                  else pure Nothing
                killThread worker
                cancelled <- waitForLifecycleActionResult (phase <> " cancellation") done
                expectAsyncLifecycleCancellation phase cancelled
                pure (cancelled, diagnostic, mProcessPid)
            loaded <- getLoadedPlugins pluginManagerHandle
            assertStartupHandoffOutcome phase True cancelled loaded
            when processExpected $ do
              assertHeartbeatStops heartbeatPath
              forM_ mProcessPid (assertProcessTreeChildGone ("cancelled startup " <> phase) [])
            when endpointExpected $ assertCancelledEndpointGone phase diagnostic

  it "retains failed runtime aggregates at destructive startup handoffs" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $
      forM_ ["accept", "handshake", "prepublication", "publication"] $ \phase ->
        withEnvironmentValue "TOPO_TEST_PLUGIN_CLEANUP_FAILURE" "1" $
          withEnvironmentValue "TOPO_TEST_PLUGIN_STARTUP_FAILURE" phase $
            withPluginManager $ \pluginManagerHandle -> do
              discoverPlugins pluginManagerHandle
              _ <- try @SomeException (refreshManifests pluginManagerHandle)
              loaded <- getLoadedPlugins pluginManagerHandle
              let matching = filter ((== Text.pack testLaunchPluginName) . lpName) loaded
              map (isJust . lpRuntime) matching `shouldBe` [True]
              map (plsState . lpLifecycle) matching `shouldBe` [LifecycleFailed]
              map lpStatus matching `shouldSatisfy` anyPluginError
              unsetEnv "TOPO_TEST_PLUGIN_CLEANUP_FAILURE"
              shutdownPlugins pluginManagerHandle

  it "makes aggregate runtime cleanup idempotent" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        runtime <- case [owned | plugin <- loaded, lpName plugin == Text.pack testLaunchPluginName, Just owned <- [lpRuntime plugin]] of
          [owned] -> pure owned
          runtimes -> expectationFailure
            ("expected one owned runtime, got " <> show (length runtimes))
            >> fail "missing owned runtime"
        firstCleanup <- cleanupOwnedPluginRuntime runtime
        secondCleanup <- cleanupOwnedPluginRuntime runtime
        case (firstCleanup, secondCleanup) of
          (OwnedPluginRuntimeCleanupComplete, OwnedPluginRuntimeCleanupComplete) -> pure ()
          _ -> expectationFailure "repeated aggregate runtime cleanup did not complete"
        shutdownPlugins pluginManagerHandle
        stopped <- getLoadedPlugins pluginManagerHandle
        map (isNothing . lpRuntime) (filter ((== Text.pack testLaunchPluginName) . lpName) stopped)
          `shouldBe` [True]

  it "retains the same owner when containment setup cleanup fails" $ do
    withEnvironmentValue "TOPO_TEST_PLUGIN_CONTAINMENT_FAILURE" "1" $
      withEnvironmentValue "TOPO_TEST_PLUGIN_CLEANUP_FAILURE" "1" $
        withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $
          withPluginManager $ \pluginManagerHandle -> do
            discoverPlugins pluginManagerHandle
            refreshManifests pluginManagerHandle
            loaded <- getLoadedPlugins pluginManagerHandle
            pluginStatuses testLaunchPluginName loaded `shouldSatisfy`
              anyPluginErrorContaining "termination failed"
            owner <- case pluginOwnedProcesses testLaunchPluginName loaded of
              [ownedProcess] -> pure ownedProcess
              owners -> expectationFailure
                ("expected one retained process owner, got " <> show (length owners))
                >> fail "missing retained process owner"
            runtime <- case [owned | plugin <- loaded, lpName plugin == Text.pack testLaunchPluginName, Just owned <- [lpRuntime plugin]] of
              [owned] -> pure owned
              runtimes -> expectationFailure
                ("expected one retained runtime aggregate, got " <> show (length runtimes))
                >> fail "missing retained runtime"
            let stableIdentity = ownedPluginProcessId owner
                stableGeneration = ownedPluginRuntimeGeneration runtime
                processHandle = ownedPluginProcessHandle owner
            forM_ [1 :: Int, 2] $ \_ -> do
              cleanupResult <- cleanupOwnedPluginRuntime runtime
              case cleanupResult of
                OwnedPluginRuntimeCleanupComplete ->
                  expectationFailure "forced aggregate cleanup unexpectedly completed"
                OwnedPluginRuntimeCleanupFailed retainedRuntime ->
                  ownedPluginRuntimeGeneration retainedRuntime == stableGeneration `shouldBe` True
            retained <- getLoadedPlugins pluginManagerHandle
            map ownedPluginProcessId (pluginOwnedProcesses testLaunchPluginName retained)
              `shouldBe` [stableIdentity]
            unsetEnv "TOPO_TEST_PLUGIN_CLEANUP_FAILURE"
            shutdownPlugins pluginManagerHandle
            stopped <- getLoadedPlugins pluginManagerHandle
            null (pluginOwnedProcesses testLaunchPluginName stopped) `shouldBe` True
            assertProcessExited testLaunchPluginName processHandle

  it "fails Windows launch before plugin code when Job creation or configuration fails" $ do
    if os /= "mingw32"
      then pure ()
      else forM_ ["create", "configure"] $ \failureMode ->
        withEnvironmentValue "TOPO_TEST_PLUGIN_JOB_PREPARATION_FAILURE" failureMode $
          withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "entry-marker" $
            withPluginManager $ \pluginManagerHandle -> do
              discoverPlugins pluginManagerHandle
              refreshManifests pluginManagerHandle
              loaded <- getLoadedPlugins pluginManagerHandle
              pluginStatuses testLaunchPluginName loaded `shouldSatisfy`
                anyPluginErrorContaining "before launch"
              null (pluginOwnedProcesses testLaunchPluginName loaded) `shouldBe` True
              markerCount <- readFixtureCount testLaunchPluginName "entry-marker"
              markerCount `shouldBe` 0

  it "fails Windows launch before plugin code when Job assignment fails" $ do
    if os /= "mingw32"
      then pure ()
      else withEnvironmentValue "TOPO_TEST_PLUGIN_JOB_ASSIGNMENT_FAILURE" "1" $
        withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "entry-marker" $
          withPluginManager $ \pluginManagerHandle -> do
            discoverPlugins pluginManagerHandle
            refreshManifests pluginManagerHandle
            loaded <- getLoadedPlugins pluginManagerHandle
            pluginStatuses testLaunchPluginName loaded `shouldSatisfy`
              anyPluginErrorContaining "Job assignment failed"
            null (pluginOwnedProcesses testLaunchPluginName loaded) `shouldBe` True
            markerCount <- readFixtureCount testLaunchPluginName "entry-marker"
            markerCount `shouldBe` 0

  it "launches plugins with explicit TOPO_PLUGIN env and no parent secrets" $ do
    withParentStdioCompatibilityFlag $ do
      withSensitiveParentEnvironment $ do
        withExecutablePluginDir envContractPluginName envContractManifestJSON "env-contract" $ do
          withPluginManager $ \pluginManagerHandle -> do
            discoverPlugins pluginManagerHandle
            refreshManifests pluginManagerHandle
            loaded <- getLoadedPlugins pluginManagerHandle
            pluginStatuses envContractPluginName loaded `shouldSatisfy` elem PluginConnected
            pluginLifecycleStates envContractPluginName loaded `shouldSatisfy` elem LifecycleReady
            pluginLifecycleProtocols envContractPluginName loaded `shouldSatisfy` elem (Just currentProtocolVersion)

  it "Windows kill-on-close Job contains descendants after actual-host-death" $ do
    if os /= "mingw32"
      then pure ()
      else withActualHostDeathFixture $ \hostProcess diagnostics cleanupState -> do
        assertHostDeathDiagnostics hostProcess diagnostics
        expectHeartbeatAdvances (hddOrdinaryHeartbeat diagnostics)
        expectHeartbeatAdvances (hddEscapedHeartbeat diagnostics)
        terminateFixtureHost hostProcess
        assertFixturePidGone cleanupState "Windows plugin leader after host death" diagnostics (hddLeaderPid diagnostics)
        assertFixturePidGone cleanupState "Windows ordinary descendant after host death" diagnostics (hddOrdinaryPid diagnostics)
        assertFixturePidGone cleanupState "Windows new-process-group descendant after host death" diagnostics (hddEscapedPid diagnostics)
        assertHeartbeatStops (hddOrdinaryHeartbeat diagnostics)
        assertHeartbeatStops (hddEscapedHeartbeat diagnostics)

  it "POSIX actual-host-death exposes launch-group and new-session containment limits" $ do
    if os == "mingw32"
      then pure ()
      else withActualHostDeathFixture $ \hostProcess diagnostics cleanupState -> do
        assertHostDeathDiagnostics hostProcess diagnostics
        expectHeartbeatAdvances (hddOrdinaryHeartbeat diagnostics)
        expectHeartbeatAdvances (hddEscapedHeartbeat diagnostics)
        terminateFixtureHost hostProcess
        -- Closing the transport lets the cooperative leader exit, but portable
        -- POSIX has no host-death primitive that signals its launch group.
        assertFixturePidGone cleanupState "POSIX cooperative plugin leader after host death" diagnostics (hddLeaderPid diagnostics)
        expectHeartbeatAdvances (hddOrdinaryHeartbeat diagnostics)
        expectHeartbeatAdvances (hddEscapedHeartbeat diagnostics)
        terminateFixtureLaunchGroup (hddLeaderPid diagnostics)
        assertFixturePidGone cleanupState "POSIX launch-group descendant after group cleanup" diagnostics (hddOrdinaryPid diagnostics)
        assertHeartbeatStops (hddOrdinaryHeartbeat diagnostics)
        -- A deliberately daemonized/new-session descendant is outside that
        -- group and therefore remains alive until separately terminated.
        expectHeartbeatAdvances (hddEscapedHeartbeat diagnostics)
        terminateFixtureLaunchGroup (hddEscapedPid diagnostics)
        assertFixturePidGone cleanupState "POSIX escaped descendant after explicit cleanup" diagnostics (hddEscapedPid diagnostics)
        assertHeartbeatStops (hddEscapedHeartbeat diagnostics)

  it "cleans up the connected process tree when the manager scope aborts" $ do
    withExecutablePluginDir cleanupAbortPluginName cleanupAbortManifestJSON "process-tree" $ do
      heartbeatPath <- fixtureDataFile cleanupAbortPluginName processTreeHeartbeatFileName
      handleVar <- newEmptyMVar
      result <- try @FixtureCleanupProbe $
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses cleanupAbortPluginName loaded `shouldSatisfy` elem PluginConnected
          handles <- expectPluginProcessHandles cleanupAbortPluginName loaded
          expectHeartbeatAdvances heartbeatPath
          case handles of
            processHandle:_ -> putMVar handleVar processHandle
            [] -> expectationFailure "expected a non-empty fixture process handle list"
          throwIO FixtureCleanupProbe :: IO ()
      result `shouldBe` Left FixtureCleanupProbe
      captured <- timeout 1000000 (takeMVar handleVar)
      case captured of
        Nothing -> expectationFailure "did not capture a connected fixture process handle"
        Just processHandle -> assertProcessExited cleanupAbortPluginName processHandle
      childPid <- readProcessTreeChildPid cleanupAbortPluginName
      assertHeartbeatStops heartbeatPath
      assertProcessTreeChildGone cleanupAbortPluginName [] childPid

  it "waits for descendant teardown after the plugin leader exits" $ do
    withExecutablePluginDir processTreePluginName processTreeManifestJSON "process-tree" $ do
      heartbeatPath <- fixtureDataFile processTreePluginName processTreeHeartbeatFileName
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses processTreePluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates processTreePluginName loaded `shouldSatisfy` elem LifecycleReady
        handles <- expectPluginProcessHandles processTreePluginName loaded
        owners <- pure (pluginOwnedProcesses processTreePluginName loaded)
        expectHeartbeatAdvances heartbeatPath
        childPid <- readProcessTreeChildPid processTreePluginName
        shutdownPlugins pluginManagerHandle
        stopped <- waitForLoadedPlugins
          (processTreePluginName <> " process-tree cleanup")
          pluginManagerHandle
          (null . pluginOwnedProcesses processTreePluginName)
        pluginLifecycleStates processTreePluginName stopped `shouldSatisfy` elem LifecycleStopped
        mapM_ (assertProcessExited processTreePluginName) handles
        mapM_ assertOwnedCleanupComplete owners
        assertProcessTreeChildGone processTreePluginName owners childPid
      assertHeartbeatStops heartbeatPath

  it "lifecycle-matrix never publishes Stopped for a retained descendant and converges on repeated shutdown" $ do
    withExecutablePluginDir processTreePluginName processTreeManifestJSON "process-tree" $
      withPluginManager $ \pluginManagerHandle -> do
        heartbeatPath <- fixtureDataFile processTreePluginName processTreeHeartbeatFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        owners <- pure (pluginOwnedProcesses processTreePluginName ready)
        expectHeartbeatAdvances heartbeatPath
        childPid <- readProcessTreeChildPid processTreePluginName
        withEnvironmentValue "TOPO_TEST_PLUGIN_CLEANUP_FAILURE" "1" $ do
          expectWithin "forced retained shutdown" (shutdownPlugins pluginManagerHandle)
          retained <- getLoadedPlugins pluginManagerHandle
          pluginLifecycleStates processTreePluginName retained `shouldSatisfy` elem LifecycleFailed
          pluginLifecycleStates processTreePluginName retained `shouldNotSatisfy` elem LifecycleStopped
          pluginLifecycleErrorCodes processTreePluginName retained `shouldBe` [Just "termination_failed"]
          length (pluginOwnedProcesses processTreePluginName retained) `shouldBe` 1
          expectHeartbeatAdvances heartbeatPath

        expectWithin "repeated retained shutdown" (shutdownPlugins pluginManagerHandle)
        stopped <- waitForLoadedPlugins
          (processTreePluginName <> " repeated retained shutdown")
          pluginManagerHandle
          (\loaded -> LifecycleStopped `elem` pluginLifecycleStates processTreePluginName loaded
            && null (pluginOwnedProcesses processTreePluginName loaded))
        pluginLifecycleStates processTreePluginName stopped `shouldBe` [LifecycleStopped]
        null [runtime | plugin <- stopped, lpName plugin == Text.pack processTreePluginName, runtime <- maybe [] pure (lpRuntime plugin)]
          `shouldBe` True
        mapM_ assertOwnedCleanupComplete owners
        assertProcessTreeChildGone processTreePluginName owners childPid
        assertHeartbeatStops heartbeatPath

  it "rejects split Windows named-pipe endpoint clients from different processes" $ do
    if os /= "mingw32"
      then pure ()
      else withPluginTransportServer "windows-split-pipe-mismatch" $ \server -> do
        (hostReadPipeName, hostWritePipeName) <- expectWindowsNamedPipePair (teAddress (tsEndpoint server))
        readClient <- launchSinglePipeFixtureClient hostReadPipeName
        writeClient <- launchSinglePipeFixtureClient hostWritePipeName
        acceptResult <- tsAcceptWithPeerPolicy server TransportPeerPolicy
          { tppExpectedProcessId = Nothing
          , tppExpectedUserId = Nothing
          }
        case acceptResult of
          Left (TransportConnectionFailed msg) -> msg `shouldSatisfy` Text.isInfixOf "between pipe handles"
          Left err -> expectationFailure ("expected split-pipe peer mismatch, got " <> show err)
          Right transport -> closeTransport transport >> expectationFailure "split-pipe clients unexpectedly shared one peer identity"
        assertProcessExited "windows split-pipe read client" readClient
        assertProcessExited "windows split-pipe write client" writeClient

  it "exposes Starting while public refreshManifests performs supervisor work" $ do
    withExecutablePluginDir refreshTransientPluginName refreshTransientManifestJSON "wait-for-start-signal" $ do
      withPluginManager $ \pluginManagerHandle -> do
        releasePath <- fixtureDataFile refreshTransientPluginName refreshTransientStartSignalFileName
        discoverPlugins pluginManagerHandle
        done <- forkLifecycleAction (refreshManifests pluginManagerHandle)
        starting <- waitForPluginLifecycleState
          (refreshTransientPluginName <> " refresh transient")
          refreshTransientPluginName
          LifecycleStarting
          pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName starting `shouldSatisfy` elem LifecycleStarting
        createDirectoryIfMissing True (takeDirectory releasePath)
        writeFile releasePath "go\n"
        waitForLifecycleAction (refreshTransientPluginName <> " refresh") done
        ready <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName ready `shouldSatisfy` elem LifecycleReady

  it "cleans up unpublished subprocesses when refreshManifests is interrupted" $ do
    withExecutablePluginDir interruptedRefreshPluginName interruptedRefreshManifestJSON "slow" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        result <- timeout 300000 (refreshManifests pluginManagerHandle)
        result `shouldBe` Nothing
        interrupted <- waitForLoadedPlugins
          (interruptedRefreshPluginName <> " interrupted refresh cleanup")
          pluginManagerHandle
          (\loaded ->
            LifecycleDiscovered `elem` pluginLifecycleStates interruptedRefreshPluginName loaded
              && not (LifecycleStarting `elem` pluginLifecycleStates interruptedRefreshPluginName loaded)
              && null (pluginProcessHandles interruptedRefreshPluginName loaded))
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
        interrupted <- waitForLoadedPlugins
          (connectedRefreshInterruptedPluginName <> " interrupted connected refresh cleanup")
          pluginManagerHandle
          (\loaded ->
            LifecycleStopped `elem` pluginLifecycleStates connectedRefreshInterruptedPluginName loaded
              && not (LifecycleStarting `elem` pluginLifecycleStates connectedRefreshInterruptedPluginName loaded)
              && null (pluginProcessHandles connectedRefreshInterruptedPluginName loaded))
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
        done <- forkLifecycleAction (shutdownPlugins pluginManagerHandle)
        stopping <- waitForPluginLifecycleState
          (shutdownTransientPluginName <> " shutdown transient")
          shutdownTransientPluginName
          LifecycleStopping
          pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName stopping `shouldSatisfy` elem LifecycleStopping
        waitForLifecycleAction (shutdownTransientPluginName <> " shutdown") done
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
        interrupted <- waitForLoadedPlugins
          (interruptedShutdownPluginName <> " interrupted shutdown cleanup")
          pluginManagerHandle
          (\loaded ->
            LifecycleStopped `elem` pluginLifecycleStates interruptedShutdownPluginName loaded
              && not (LifecycleStopping `elem` pluginLifecycleStates interruptedShutdownPluginName loaded)
              && null (pluginProcessHandles interruptedShutdownPluginName loaded))
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

  it "does not mark a plugin ready when a fake endpoint client races without auth" $ do
    withExecutablePluginDir endpointRacePluginName endpointRaceManifestJSON "endpoint-race-parent" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses endpointRacePluginName loaded `shouldSatisfy` (not . elem PluginConnected)
        pluginLifecycleStates endpointRacePluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleStates endpointRacePluginName loaded `shouldNotSatisfy` elem LifecycleReady
        pluginLifecycleErrorCodes endpointRacePluginName loaded `shouldSatisfy`
          any (`elem` [Just "launch_failed", Just "protocol_error"])
        pluginStatuses endpointRacePluginName loaded `shouldSatisfy` \statuses ->
          anyPluginErrorContaining "peer identity" statuses
            || anyPluginErrorContaining "launch session" statuses
        length (pluginProcessHandles endpointRacePluginName loaded) `shouldBe` 0
        endpointRaceConnected <- doesFileExist =<< fixtureDataFile endpointRacePluginName endpointRaceConnectedFileName
        endpointRaceConnected `shouldBe` True
        token <- readFixtureToken endpointRacePluginName endpointRaceTokenFileName
        assertSecretAbsentFromPluginDiagnostics endpointRacePluginName token loaded

  it "rejects missing launch auth proof before marking a plugin ready" $ do
    withExecutablePluginDir authMissingPluginName authMissingManifestJSON "auth-missing" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses authMissingPluginName loaded `shouldSatisfy` anyPluginErrorContaining "launch session"
        pluginLifecycleStates authMissingPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes authMissingPluginName loaded `shouldSatisfy` elem (Just "protocol_error")
        length (pluginProcessHandles authMissingPluginName loaded) `shouldBe` 0
        token <- readFixtureToken authMissingPluginName authMissingTokenFileName
        assertSecretAbsentFromPluginDiagnostics authMissingPluginName token loaded

  it "rejects mismatched launch auth proof and cleans up the launched process" $ do
    withExecutablePluginDir authMismatchPluginName authMismatchManifestJSON "auth-mismatch" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses authMismatchPluginName loaded `shouldSatisfy` anyPluginErrorContaining "auth proof"
        pluginLifecycleStates authMismatchPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes authMismatchPluginName loaded `shouldSatisfy` elem (Just "protocol_error")
        length (pluginProcessHandles authMismatchPluginName loaded) `shouldBe` 0
        token <- readFixtureToken authMismatchPluginName authMismatchTokenFileName
        assertSecretAbsentFromPluginDiagnostics authMismatchPluginName token loaded

  it "surfaces manifest parse diagnostics for missing required fields" $ do
    let pluginName = "copilot-test-plugin-missing-runtime"
    withExecutablePluginDir pluginName (missingRuntimeManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "runtime"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_parse_failed")

  it "surfaces missing manifest diagnostics without launching unmanifested directories" $ do
    let pluginName = "copilot-test-plugin-missing-manifest"
    withUnmanifestedExecutablePluginDir pluginName "counted-early-exit" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` anyPluginErrorContaining "manifest.json is missing"
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "manifest_missing")
        length (pluginProcessHandles pluginName loaded) `shouldBe` 0
        countAfterDiscover <- readFixtureCount pluginName "counted-early-exit"
        countAfterDiscover `shouldBe` 0
        refreshManifests pluginManagerHandle
        refreshed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName refreshed `shouldSatisfy` anyPluginErrorContaining "manifest.json is missing"
        pluginLifecycleErrorCodes pluginName refreshed `shouldSatisfy` elem (Just "manifest_missing")
        length (pluginProcessHandles pluginName refreshed) `shouldBe` 0
        countAfterRefresh <- readFixtureCount pluginName "counted-early-exit"
        countAfterRefresh `shouldBe` 0

  it "prefers manifest-present plugins over missing-manifest fallback name collisions" $ do
    let pluginName = "copilot-test-plugin-collision"
        manifestDirName = pluginName <> "-manifested"
    withMissingManifestNameCollision pluginName manifestDirName $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldBe` [PluginIdle]
        pluginLifecycleStates pluginName loaded `shouldBe` [LifecycleDiscovered]
        pluginLifecycleErrorCodes pluginName loaded `shouldBe` [Nothing]

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

  it "starts degraded for optional unavailable external data-source refs and diagnostics" $ do
    let pluginName = "copilot-test-plugin-external-source-optional"
    withExecutablePluginDir pluginName (optionalExternalRefManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        case findExternalSnapshot (Text.pack pluginName) snapshots of
          Nothing -> expectationFailure "missing optional consumer external data-source snapshot"
          Just snapshot -> case wedssConsumedRefs snapshot of
            [ref] -> expectUnavailableStatus "external-ledger" (redsrStatus ref)
            other -> expectationFailure ("expected one optional consumed ref, got " <> show other)

  it "starts degraded for optional degraded external data-source refs" $ do
    let pluginName = "copilot-test-plugin-external-source-optional-degraded"
    withExecutablePluginDir pluginName (optionalDegradedExternalRefManifestFor pluginName) "ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses pluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates pluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes pluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")

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

  it "starts degraded for required external data-source consumers when providers are degraded" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderDegradedManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_blocked")

  it "blocks connected required consumers when providers refresh to hard unavailable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        baseDir <- currentPluginBaseDir
        BS.writeFile
          (baseDir </> externalProviderPluginName </> "manifest.json")
          externalProviderUnavailableManifestJSON
        refreshManifests pluginManagerHandle
        blocked <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName blocked `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginLifecycleStates externalConsumerPluginName blocked `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName blocked `shouldSatisfy` elem (Just "external_data_source_blocked")

  forM_ externalProviderStatusFailureModes $ \(modeName, expectedClass) ->
    it ("blocks required consumers when provider status refresh reports " <> modeName) $
      expectRequiredStatusRefreshFailure modeName expectedClass

  it "degrades optional consumers when provider status refresh reports plugin errors" $
    expectOptionalStatusRefreshFailure "plugin-error" "plugin error"

  it "does not broker stale ready grants when a provider status report omits a grant" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        grantedBinding <- expectRight "initial granted binding query" =<<
          expectWithin "initial granted binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
        expectBindingStatus "granted" grantedBinding
        writeExternalProviderStatusMode "omit-grant"
        blocked <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName blocked `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginLifecycleStates externalConsumerPluginName blocked `shouldSatisfy` elem LifecycleFailed
        providerSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectProviderStatusMessage externalProviderPluginNameText "omitted grant" providerSnapshots

  it "normalizes brokered external grant messages to the runtime capability scope" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
      ] $ do
        writeExternalProviderStatusMode "grant-scope-query-only"
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleReady
          grantScopes <- readExternalConsumerGrantScopeLog
          grantScopes `shouldBe` ["top=query;status=query"]

  it "blocks required consumers when runtime grant scope omits requested access capabilities" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
      ] $ do
        writeExternalProviderStatusMode "grant-scope-health-only"
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` anyPluginErrorContaining "capability scope mismatch"
          pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleFailed
          pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_blocked")
          expectNoExternalGrantCallback
          snapshots <- getPluginExternalDataSources pluginManagerHandle
          expectProviderGrantStatusCapabilityScope [] snapshots
          expectUnavailableConsumerBrokerSnapshot "unavailable" "capability scope mismatch" snapshots

  it "degrades optional consumers when runtime grant scope widens to migrate beyond read grant capabilities" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
      ] $ do
        writeExternalProviderStatusMode "grant-scope-query-migrate"
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleDegraded
          pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")
          expectNoExternalGrantCallback
          snapshots <- getPluginExternalDataSources pluginManagerHandle
          expectProviderGrantStatusCapabilityScope [] snapshots
          expectUnavailableConsumerBrokerSnapshot "unavailable" "capability scope mismatch" snapshots

  it "blocks required consumers when provider status report widens grant access" $
    expectRequiredStatusReportScopeMismatch "grant-access-widen" "reported grant access"

  it "blocks required consumers when provider status report widens grant resources" $
    expectRequiredStatusReportScopeMismatch "grant-resources-widen" "reported grant resources"

  it "brokers status reports for grants inheriting source resources" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderInheritedGrantResourcesManifestJSON, "external-provider-controlled-status")
      ] $ do
        writeExternalProviderStatusMode "grant-resources-inherited"
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
          pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleReady
          grantedBinding <- expectRight "inherited resource binding query" =<<
            expectWithin "inherited resource binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
          expectBindingStatus "granted" grantedBinding

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
                expectConsumerDiagnosticBrokerState Set.empty "grant_acked" (Just True) (Just True) (Just "applied") Nothing loaded
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

                grantedBinding <- expectRight "automatically granted consumer binding query" =<<
                  expectWithin "automatically granted consumer binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
                expectBindingStatus "granted" grantedBinding
                shouldNotMentionSQLite grantedBinding

                snapshots <- getPluginExternalDataSources pluginManagerHandle
                expectExternalSnapshots snapshots
                pluginDataDirs <- getPluginDataDirectories pluginManagerHandle
                Set.fromList (map wpddPlugin pluginDataDirs) `shouldBe`
                  Set.fromList [externalProviderPluginNameText, externalConsumerPluginNameText]
                Set.fromList (map wpddArchiveDirectory pluginDataDirs) `shouldBe`
                  Set.fromList ["external-provider-data", "external-consumer-data"]
                pluginDataDirs `shouldSatisfy` all
                  (\entry -> isAbsolute (wpddSourceDirectory entry)
                    && takeFileName (wpddSourceDirectory entry) == "data")
                let world = emptyWorld (WorldConfig 8) defaultHexGridMeta
                    ui = emptyUiState { uiSeed = 77, uiChunkSize = 8 }
                saveResult <- expectWithin "save external integration world"
                  (saveNamedWorldWithPluginsAndExternalData externalIntegrationWorldName ui world pluginDataDirs snapshots)
                saveResult `shouldBe` Right ()
                loadResult <- expectWithin "load external integration world" (loadNamedWorld externalIntegrationWorldName)
                case loadResult of
                  Left err -> expectationFailure (Text.unpack err)
                  Right (manifest, _snapshot, _loadedWorld) -> do
                    Set.fromList (wsmPluginData manifest) `shouldBe` Set.fromList
                      [ (externalProviderPluginNameText, "external-provider-data")
                      , (externalConsumerPluginNameText, "external-consumer-data")
                      ]
                    wsmExternalDataSources manifest `shouldBe` snapshots
                    shouldNotMentionSQLite (wsmExternalDataSources manifest)

                setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
                disabled <- getLoadedPlugins pluginManagerHandle
                pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
                pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleFailed
                pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_blocked")
                expectConsumerDiagnosticBrokerState (Set.singleton externalProviderPluginNameText) "revoke_acked" (Just True) (Just True) (Just "applied") Nothing disabled
                unavailableSnapshots <- getPluginExternalDataSources pluginManagerHandle
                expectProviderUnavailableSnapshots unavailableSnapshots

                setDisabledPlugins pluginManagerHandle Set.empty
                refreshManifests pluginManagerHandle
                recoveredBinding <- expectRight "provider-recovered granted consumer binding query" =<<
                  expectWithin "provider-recovered granted consumer binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
                expectBindingStatus "granted" recoveredBinding))

  it "surfaces external data-source grant ACK rejections to required consumers" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-reject-grant")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalProviderPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` anyPluginErrorContaining "consumer rejected grant"
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_blocked")
        expectConsumerDiagnosticBrokerState Set.empty "grant_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected grant") loaded
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        case findExternalSnapshot externalConsumerPluginNameText snapshots of
          Nothing -> expectationFailure "missing required consumer snapshot after rejected grant"
          Just snapshot -> case wedssConsumedRefs snapshot of
            [ref] -> do
              redssState (redsrStatus ref) `shouldBe` ExternalStatusUnavailable
              redssMessage (redsrStatus ref) `shouldSatisfy` maybe False (Text.isInfixOf "consumer rejected grant")
              expectBrokerOperationDiagnostics "grant_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected grant") (redssDiagnostics (redsrStatus ref))
            other -> expectationFailure ("expected one required consumed ref, got " <> show other)

  it "degrades optional consumers when external data-source grant ACKs are rejected" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer-reject-grant")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalProviderPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")
        expectConsumerDiagnosticBrokerState Set.empty "grant_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected grant") loaded
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        case findExternalSnapshot externalConsumerPluginNameText snapshots of
          Nothing -> expectationFailure "missing optional consumer snapshot after rejected grant"
          Just snapshot -> case wedssConsumedRefs snapshot of
            [ref] -> do
              redssState (redsrStatus ref) `shouldBe` ExternalStatusUnavailable
              redssMessage (redsrStatus ref) `shouldSatisfy` maybe False (Text.isInfixOf "consumer rejected grant")
              expectBrokerOperationDiagnostics "grant_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected grant") (redssDiagnostics (redsrStatus ref))
            other -> expectationFailure ("expected one optional consumed ref, got " <> show other)

  it "blocks required consumers when data queries report external data-source unavailable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-query-external-unavailable")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        queryFailure <- expectWithin "required external unavailable query" $
          queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery
        expectExternalDataSourceUnavailableError "required external unavailable query" queryFailure
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName observed `shouldSatisfy` anyPluginErrorContaining "external data-source data operation failed"
        pluginLifecycleStates externalConsumerPluginName observed `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName observed `shouldSatisfy` elem (Just "external_data_source_blocked")
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        expectConsumerRefUnavailable snapshots

  it "degrades optional consumers when data queries report external data-source unavailable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer-query-external-unavailable")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        queryFailure <- expectWithin "optional external unavailable query" $
          queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery
        expectExternalDataSourceUnavailableError "optional external unavailable query" queryFailure
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName observed `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName observed `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName observed `shouldSatisfy` elem (Just "external_data_source_degraded")
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        expectConsumerRefUnavailable snapshots

  it "keeps mixed required and optional query unavailability degraded instead of blocked" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalMixedConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName ready `shouldSatisfy` elem (Just "external_data_source_degraded")
        queryFailure <- expectWithin "mixed required optional unavailable query" $
          queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery
        expectExternalDataSourceUnavailableError "mixed required optional unavailable query" queryFailure
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName observed `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName observed `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName observed `shouldSatisfy` elem (Just "external_data_source_degraded")

  it "degrades optional consumers when providers are disabled after startup" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_degraded")

  it "blocks required consumers when revoke ACKs are rejected after provider disable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-reject-revoke")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        grantedBinding <- expectRight "initial granted binding query" =<<
          expectWithin "initial granted binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
        expectBindingStatus "granted" grantedBinding
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_blocked")
        expectConsumerDiagnosticBrokerState (Set.singleton externalProviderPluginNameText) "revoke_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected revoke") disabled
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        expectRejectedRevokeSnapshot snapshots

  it "degrades optional consumers when revoke ACKs are rejected after provider disable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer-reject-revoke")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        grantedBinding <- expectRight "initial optional granted binding query" =<<
          expectWithin "initial optional granted binding query" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
        expectBindingStatus "granted" grantedBinding
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_degraded")
        expectConsumerDiagnosticBrokerState (Set.singleton externalProviderPluginNameText) "revoke_failed" (Just False) (Just False) (Just "failed") (Just "consumer rejected revoke") disabled
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        expectRejectedRevokeSnapshot snapshots

  it "retries external data-source grants with the same operation id after ACK timeouts" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-timeout-grant-once")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        timedOut <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName timedOut `shouldSatisfy` anyPluginErrorContaining "plugin external data-source grant timed out"
        pluginLifecycleStates externalConsumerPluginName timedOut `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName timedOut `shouldSatisfy` elem (Just "external_data_source_blocked")
        expectConsumerDiagnosticBrokerState Set.empty "grant_failed" Nothing Nothing Nothing Nothing timedOut
        operationsAfterTimeout <- readExternalConsumerOperationLog
        operationsAfterTimeout `shouldBe` [externalBrokerOperationLogLine "grant"]
        refreshManifests pluginManagerHandle
        recovered <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalProviderPluginName recovered `shouldSatisfy` elem PluginConnected
        pluginStatuses externalConsumerPluginName recovered `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName recovered `shouldSatisfy` elem LifecycleReady
        expectConsumerDiagnosticBrokerState Set.empty "grant_acked" (Just True) (Just True) (Just "applied") Nothing recovered
        operationsAfterRetry <- readExternalConsumerOperationLog
        operationsAfterRetry `shouldBe` replicate 2 (externalBrokerOperationLogLine "grant")

  it "degrades optional consumers when external data-source grant ACKs time out" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer-timeout-grant-once")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        timedOut <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalProviderPluginName timedOut `shouldSatisfy` elem PluginConnected
        pluginStatuses externalConsumerPluginName timedOut `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName timedOut `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName timedOut `shouldSatisfy` elem (Just "external_data_source_degraded")
        expectConsumerDiagnosticBrokerState Set.empty "grant_failed" Nothing Nothing Nothing Nothing timedOut
        operationsAfterTimeout <- readExternalConsumerOperationLog
        operationsAfterTimeout `shouldBe` [externalBrokerOperationLogLine "grant"]
        timeoutSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectUnavailableConsumerBrokerSnapshot "grant_failed" "timed out" timeoutSnapshots
        refreshManifests pluginManagerHandle
        recovered <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalProviderPluginName recovered `shouldSatisfy` elem PluginConnected
        pluginStatuses externalConsumerPluginName recovered `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName recovered `shouldSatisfy` elem LifecycleReady
        expectConsumerDiagnosticBrokerState Set.empty "grant_acked" (Just True) (Just True) (Just "applied") Nothing recovered
        operationsAfterRetry <- readExternalConsumerOperationLog
        operationsAfterRetry `shouldSatisfy` all (== externalBrokerOperationLogLine "grant")
        length operationsAfterRetry `shouldSatisfy` (>= (2 :: Int))

  it "retries timed-out revokes before regranting stale external data-source bindings" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-timeout-revoke-once")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` anyPluginError
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleFailed
        expectConsumerDiagnosticBrokerState (Set.singleton externalProviderPluginNameText) "revoke_failed" Nothing Nothing Nothing Nothing disabled
        operationsAfterTimeout <- readExternalConsumerOperationLog
        operationsAfterTimeout `shouldBe`
          [ externalBrokerOperationLogLine "grant"
          , externalBrokerOperationLogLine "revoke"
          ]
        timeoutSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectUnavailableConsumerBrokerSnapshot "revoke_failed" "timed out" timeoutSnapshots
        setDisabledPlugins pluginManagerHandle Set.empty
        refreshManifests pluginManagerHandle
        recovered <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName recovered `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName recovered `shouldSatisfy` elem LifecycleReady
        expectConsumerDiagnosticBrokerState Set.empty "grant_acked" (Just True) (Just True) (Just "applied") Nothing recovered
        operationsAfterRetry <- readExternalConsumerOperationLog
        operationsAfterRetry `shouldBe`
          [ externalBrokerOperationLogLine "grant"
          , externalBrokerOperationLogLine "revoke"
          , externalBrokerOperationLogLine "revoke"
          , externalBrokerOperationLogLine "grant"
          ]

  it "keeps optional timed-out revokes degraded and prevents stale bindings from reading usable" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer-timeout-revoke-once")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName ready `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName ready `shouldSatisfy` elem LifecycleReady
        grantedBinding <- expectRight "initial optional granted binding query before revoke timeout" =<<
          expectWithin "initial optional granted binding query before revoke timeout" (queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery)
        expectBindingStatus "granted" grantedBinding
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        operationsAfterTimeout <- waitForExternalConsumerOperationLogLength 2
        operationsAfterTimeout `shouldBe`
          [ externalBrokerOperationLogLine "grant"
          , externalBrokerOperationLogLine "revoke"
          ]
        staleQuery <- expectWithin "optional stale binding query after revoke timeout" $
          queryPluginResource pluginManagerHandle externalConsumerPluginNameText externalBindingQuery
        expectExternalDataSourceUnavailableError "optional stale binding query after revoke timeout" staleQuery
        disabled <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName disabled `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName disabled `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName disabled `shouldSatisfy` elem (Just "external_data_source_degraded")
        timeoutSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectConsumerRefUnavailable timeoutSnapshots

  it "orders revokes before regrants across provider disable/re-enable and status replacement" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        setDisabledPlugins pluginManagerHandle (Set.singleton externalProviderPluginNameText)
        disabled <- getLoadedPlugins pluginManagerHandle
        expectConsumerDiagnosticBrokerState (Set.singleton externalProviderPluginNameText) "revoke_acked" (Just True) (Just True) (Just "applied") Nothing disabled
        setDisabledPlugins pluginManagerHandle Set.empty
        refreshManifests pluginManagerHandle
        recovered <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates externalConsumerPluginName recovered `shouldSatisfy` elem LifecycleReady
        writeExternalProviderStatusMode "replacement"
        replaced <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates externalConsumerPluginName replaced `shouldSatisfy` elem LifecycleReady
        expectConsumerDiagnosticBrokerState Set.empty "grant_acked" (Just True) (Just True) (Just "applied") Nothing replaced
        operations <- readExternalConsumerOperationLog
        operations `shouldBe`
          [ externalBrokerOperationLogLine "grant"
          , externalBrokerOperationLogLine "revoke"
          , externalBrokerOperationLogLine "grant"
          , externalBrokerOperationLogLine "revoke"
          , externalBrokerOperationLogLine "grant"
          ]

  it "observes consumer crashes after external grant ACK transport writes" $
    withExecutablePluginDirs
      [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer-crash-after-grant-ack")
      , (externalProviderPluginName, externalProviderManifestJSON, "external-provider")
      ] $ withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        crashed <- waitForLoadedPlugins
          (externalConsumerPluginName <> " crash after grant ACK")
          pluginManagerHandle
          (\loaded -> anyPluginErrorContaining "process exited" (pluginStatuses externalConsumerPluginName loaded)
            || anyPluginErrorContaining "EOF reading message length" (pluginStatuses externalConsumerPluginName loaded)
            || anyPluginErrorContaining "not brokerable" (pluginStatuses externalConsumerPluginName loaded))
        pluginLifecycleStates externalConsumerPluginName crashed `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName crashed `shouldSatisfy`
          any (`elem` [Just "process_exited", Just "transport_error", Just "external_data_source_blocked"])
        operations <- readExternalConsumerOperationLog
        operations `shouldBe` [externalBrokerOperationLogLine "grant"]

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

  it "reports endpoint accept timeouts as launch failures" $ do
    withExecutablePluginDir endpointAcceptTimeoutPluginName endpointAcceptTimeoutManifestJSON "slow" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses endpointAcceptTimeoutPluginName loaded `shouldSatisfy` anyPluginErrorContaining "timed out waiting for plugin connection"
        pluginLifecycleStates endpointAcceptTimeoutPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes endpointAcceptTimeoutPluginName loaded `shouldSatisfy` elem (Just "launch_failed")
        length (pluginProcessHandles endpointAcceptTimeoutPluginName loaded) `shouldBe` 0

  it "reports handshake stalls as handshake timeouts" $ do
    withExecutablePluginDir handshakeStallPluginName handshakeStallManifestJSON "handshake-stall" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses handshakeStallPluginName loaded `shouldSatisfy` anyPluginErrorContaining "plugin handshake timed out"
        pluginLifecycleStates handshakeStallPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes handshakeStallPluginName loaded `shouldSatisfy` elem (Just "handshake_timeout")
        length (pluginProcessHandles handshakeStallPluginName loaded) `shouldBe` 0

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

  it "observes autonomous process exit and exposes restart backoff before replacement" $ do
    withExecutablePluginDir runtimeRestartPluginName runtimeRestartManifestJSON "flaky-runtime-exit" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        threadDelay 250000
        backingOff <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates runtimeRestartPluginName backingOff `shouldSatisfy` elem LifecycleStarting
        restarted <- waitForLoadedPlugins
          (runtimeRestartPluginName <> " autonomous restart")
          pluginManagerHandle
          (elem PluginConnected . pluginStatuses runtimeRestartPluginName)
        pluginLifecycleStates runtimeRestartPluginName restarted `shouldSatisfy` elem LifecycleReady
        count <- readFixtureCount runtimeRestartPluginName "flaky-runtime-exit"
        count `shouldBe` 2

  it "lifecycle-matrix tears down the old process tree before replacement and rejects its late generation" $ do
    withExecutablePluginDir controlledRuntimePluginName controlledRuntimeManifestJSON "controlled-runtime-tree-restart" $
      withPluginManager $ \pluginManagerHandle -> do
        heartbeatPath <- fixtureDataFile controlledRuntimePluginName processTreeHeartbeatFileName
        crashRelease <- fixtureDataFile controlledRuntimePluginName controlledRuntimeCrashReleaseFileName
        replacementStarted <- fixtureDataFile controlledRuntimePluginName controlledRuntimeReplacementStartedFileName
        overlapProbe <- fixtureDataFile controlledRuntimePluginName controlledRuntimeOverlapProbeFileName
        replacementRelease <- fixtureDataFile controlledRuntimePluginName controlledRuntimeReplacementReleaseFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        firstReady <- getLoadedPlugins pluginManagerHandle
        oldRuntime <- expectPluginRuntime controlledRuntimePluginName firstReady
        oldOwner <- case pluginOwnedProcesses controlledRuntimePluginName firstReady of
          [owner] -> pure owner
          owners -> expectationFailure
            ("expected one old runtime owner, got " <> show (length owners)) >> fail "old owner"
        let oldGeneration = ownedPluginRuntimeGeneration oldRuntime
            oldProcess = ownedPluginProcessHandle oldOwner
        expectHeartbeatAdvances heartbeatPath
        childPid <- readProcessTreeChildPid controlledRuntimePluginName

        writeFile crashRelease "crash\n"
        expectWithin "replacement process start marker" (waitForFixtureSignal replacementStarted)
        overlapState <- Text.strip . Text.pack <$> readFile overlapProbe
        overlapState `shouldBe` "old-tree-gone"
        assertProcessExited (controlledRuntimePluginName <> " old generation") oldProcess
        assertProcessTreeChildGone controlledRuntimePluginName [oldOwner] childPid
        assertHeartbeatStops heartbeatPath

        writeFile replacementRelease "connect\n"
        restarted <- waitForLoadedPlugins
          (controlledRuntimePluginName <> " replacement ready")
          pluginManagerHandle
          (\loaded -> PluginConnected `elem` pluginStatuses controlledRuntimePluginName loaded
            && LifecycleReady `elem` pluginLifecycleStates controlledRuntimePluginName loaded)
        newRuntime <- expectPluginRuntime controlledRuntimePluginName restarted
        let newGeneration = ownedPluginRuntimeGeneration newRuntime
        newGeneration `shouldNotBe` oldGeneration
        length (pluginOwnedProcesses controlledRuntimePluginName restarted) `shouldBe` 1
        readFixtureCount controlledRuntimePluginName controlledRuntimeCountKey `shouldReturn` 2

        (accepted, directive) <- call @"runtimeFailure" pluginManagerHandle #runtimeFailure
          (runtimeProcessExitNotice (Text.pack controlledRuntimePluginName) oldGeneration (ExitFailure 77))
        accepted `shouldBe` False
        isNothing directive `shouldBe` True
        afterLateNotice <- getLoadedPlugins pluginManagerHandle
        currentRuntime <- expectPluginRuntime controlledRuntimePluginName afterLateNotice
        ownedPluginRuntimeGeneration currentRuntime `shouldBe` newGeneration
        pluginLifecycleStates controlledRuntimePluginName afterLateNotice `shouldBe` [LifecycleReady]

  it "lets refresh supersede a queued autonomous restart without a stale launch" $
    withExecutablePluginDir runtimeRestartPluginName runtimeRestartManifestJSON "flaky-runtime-exit" $
      withRuntimeRestartBarrier runtimeRestartPluginName $ \barrier ->
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          expectWithin "refresh-race queued restart" (waitForFixtureSignal (rrbMarker barrier))
          backingOff <- getLoadedPlugins pluginManagerHandle
          pluginLifecycleStates runtimeRestartPluginName backingOff `shouldSatisfy` elem LifecycleStarting
          refreshManifests pluginManagerHandle
          writeFile (rrbRelease barrier) "release\n"
          expectWithin "refresh-race restart cancellation" (waitForFixtureSignal (rrbComplete barrier))
          assertRestartBarrierDiagnostic barrier
          loaded <- getLoadedPlugins pluginManagerHandle
          pluginStatuses runtimeRestartPluginName loaded `shouldSatisfy` elem PluginConnected
          count <- readFixtureCount runtimeRestartPluginName "flaky-runtime-exit"
          count `shouldBe` 2

  it "lets shutdown suppress a queued autonomous restart" $
    withExecutablePluginDir runtimeRestartPluginName runtimeRestartManifestJSON "flaky-runtime-exit" $
      withRuntimeRestartBarrier runtimeRestartPluginName $ \barrier ->
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          expectWithin "shutdown-race queued restart" (waitForFixtureSignal (rrbMarker barrier))
          backingOff <- getLoadedPlugins pluginManagerHandle
          pluginLifecycleStates runtimeRestartPluginName backingOff `shouldSatisfy` elem LifecycleStarting
          shutdownPlugins pluginManagerHandle
          writeFile (rrbRelease barrier) "release\n"
          expectWithin "shutdown-race restart cancellation" (waitForFixtureSignal (rrbComplete barrier))
          assertRestartBarrierDiagnostic barrier
          stopped <- getLoadedPlugins pluginManagerHandle
          pluginLifecycleStates runtimeRestartPluginName stopped `shouldSatisfy` elem LifecycleStopped
          count <- readFixtureCount runtimeRestartPluginName "flaky-runtime-exit"
          count `shouldBe` 1

  it "honors restart_mode=never for autonomous runtime failure" $
    withExecutablePluginDir runtimeNeverPluginName runtimeNeverManifestJSON "flaky-runtime-exit" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        threadDelay 350000
        failed <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates runtimeNeverPluginName failed `shouldSatisfy` elem LifecycleFailed
        count <- readFixtureCount runtimeNeverPluginName "flaky-runtime-exit"
        count `shouldBe` 1

  it "honors restart_mode=always after a clean autonomous exit" $
    withExecutablePluginDir runtimeAlwaysPluginName runtimeAlwaysManifestJSON "flaky-runtime-exit-clean" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        threadDelay 300000
        restarted <- waitForLoadedPlugins
          (runtimeAlwaysPluginName <> " clean always restart")
          pluginManagerHandle
          (\loaded -> elem PluginConnected (pluginStatuses runtimeAlwaysPluginName loaded)
            && length (pluginProcessHandles runtimeAlwaysPluginName loaded) == 1)
        pluginLifecycleStates runtimeAlwaysPluginName restarted `shouldSatisfy` elem LifecycleReady
        count <- readFixtureCount runtimeAlwaysPluginName "flaky-runtime-exit-clean"
        count `shouldBe` 2

  it "recovers a retained termination failure after delayed cleanup succeeds" $
    withExecutablePluginDir runtimeRestartPluginName runtimeRestartManifestJSON "flaky-runtime-exit" $
      withEnvironmentValue "TOPO_TEST_PLUGIN_CLEANUP_FAILURE" "1" $
        withPluginManager $ \pluginManagerHandle -> do
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          retained <- waitForLoadedPlugins
            (runtimeRestartPluginName <> " retained termination failure")
            pluginManagerHandle
            (elem (Just "termination_failed") . pluginLifecycleErrorCodes runtimeRestartPluginName)
          length (pluginProcessHandles runtimeRestartPluginName retained) `shouldSatisfy` (> 0)
          unsetEnv "TOPO_TEST_PLUGIN_CLEANUP_FAILURE"
          recovered <- waitForLoadedPlugins
            (runtimeRestartPluginName <> " delayed cleanup recovery")
            pluginManagerHandle
            (elem PluginConnected . pluginStatuses runtimeRestartPluginName)
          pluginLifecycleStates runtimeRestartPluginName recovered `shouldSatisfy` elem LifecycleReady
          count <- readFixtureCount runtimeRestartPluginName "flaky-runtime-exit"
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

  it "lifecycle-matrix preserves the originating failure after exhausting multiple restart attempts" $ do
    withExecutablePluginDir restartLimitMultiplePluginName restartLimitMultipleManifestJSON "counted-early-exit" $
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        exhausted <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates restartLimitMultiplePluginName exhausted `shouldBe` [LifecycleFailed]
        pluginLifecycleErrorCodes restartLimitMultiplePluginName exhausted `shouldBe` [Just "restart_limit_exceeded"]
        pluginStatuses restartLimitMultiplePluginName exhausted `shouldSatisfy`
          anyPluginErrorContaining "timed out waiting for plugin connection"
        pluginStatuses restartLimitMultiplePluginName exhausted `shouldSatisfy`
          anyPluginErrorContaining "restart limit exceeded"
        null (pluginOwnedProcesses restartLimitMultiplePluginName exhausted) `shouldBe` True
        count <- readFixtureCount restartLimitMultiplePluginName "counted-early-exit"
        count `shouldBe` 3

  it "lease-mailbox-query keeps the pinned mailbox responsive while RPC waits" $ do
    withExecutablePluginDir gatedDataPluginName gatedDataManifestJSON "gated-data" $
      withPluginManager $ \pluginManagerHandle -> do
        queryReceived <- fixtureDataFile gatedDataPluginName gatedQueryReceivedFileName
        queryRelease <- fixtureDataFile gatedDataPluginName gatedQueryReleaseFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        runtime <- expectPluginRuntime gatedDataPluginName loaded
        done <- newEmptyMVar
        _ <- forkIO (queryPluginResource pluginManagerHandle (Text.pack gatedDataPluginName) testQuery >>= putMVar done)
        waitForFixtureSignal queryReceived
        resources <- expectPromptly "data resources during leased query" (getPluginDataResources pluginManagerHandle)
        Map.member (Text.pack gatedDataPluginName) resources `shouldBe` True
        promptLoaded <- expectPromptly "loaded plugins during leased query" (getLoadedPlugins pluginManagerHandle)
        pluginStatuses gatedDataPluginName promptLoaded `shouldSatisfy` elem PluginConnected
        (_, directive) <- expectPromptly "generation notice during leased query" $
          call @"runtimeFailure" pluginManagerHandle #runtimeFailure
            (runtimeProcessExitNotice (Text.pack gatedDataPluginName) (ownedPluginRuntimeGeneration runtime) ExitSuccess)
        case directive of
          Nothing -> pure ()
          Just _ -> expectationFailure "live generation notice unexpectedly scheduled a restart"
        writeFile queryRelease "release\n"
        result <- expectWithin "gated query completion" (takeMVar done)
        result `shouldSatisfy` either (const False) (const True)

  it "lease-mailbox-mutation keeps the pinned mailbox responsive while RPC waits" $ do
    withExecutablePluginDir gatedDataPluginName gatedDataManifestJSON "gated-data" $
      withPluginManager $ \pluginManagerHandle -> do
        mutationReceived <- fixtureDataFile gatedDataPluginName gatedMutationReceivedFileName
        mutationRelease <- fixtureDataFile gatedDataPluginName gatedMutationReleaseFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        done <- newEmptyMVar
        _ <- forkIO (mutatePluginResource pluginManagerHandle (Text.pack gatedDataPluginName)
          (MutateResource "records" (MutCreate (record [("id", String "alpha"), ("name", String "Alpha")]))) >>= putMVar done)
        waitForFixtureSignal mutationReceived
        _ <- expectPromptly "loaded plugins during leased mutation" (getLoadedPlugins pluginManagerHandle)
        writeFile mutationRelease "release\n"
        result <- expectWithin "gated mutation completion" (takeMVar done)
        result `shouldSatisfy` either (const False) (const True)

  it "lease-cancellation cleans the pending slot without stranding the actor" $ do
    withExecutablePluginDir gatedDataPluginName gatedDataManifestJSON "gated-data" $
      withPluginManager $ \pluginManagerHandle -> do
        queryReceived <- fixtureDataFile gatedDataPluginName gatedQueryReceivedFileName
        queryRelease <- fixtureDataFile gatedDataPluginName gatedQueryReleaseFileName
        queryReplied <- fixtureDataFile gatedDataPluginName gatedQueryRepliedFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        worker <- forkIO $ do
          _ <- queryPluginResource pluginManagerHandle (Text.pack gatedDataPluginName) testQuery
          pure ()
        waitForFixtureSignal queryReceived
        killThread worker
        -- The fixture deliberately omits correlation on this second reply.
        -- It can complete only if cancellation already removed the first slot.
        second <- expectPromptly "query after caller cancellation" $
          queryPluginResource pluginManagerHandle (Text.pack gatedDataPluginName) testQuery
        second `shouldSatisfy` either (const False) (const True)
        writeFile queryRelease "release\n"
        waitForFixtureSignal queryReplied
        loaded <- expectPromptly "actor after caller cancellation" (getLoadedPlugins pluginManagerHandle)
        pluginStatuses gatedDataPluginName loaded `shouldSatisfy` elem PluginConnected

  it "lease-stale rejects old replies and timeouts without changing the replacement lifecycle" $ do
    withExecutablePluginDir gatedDataPluginName gatedDataManifestJSON "gated-data" $
      withPluginManager $ \pluginManagerHandle -> do
        queryReceived <- fixtureDataFile gatedDataPluginName gatedQueryReceivedFileName
        queryRelease <- fixtureDataFile gatedDataPluginName gatedQueryReleaseFileName
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        oldPlugin <- case filter ((== Text.pack gatedDataPluginName) . lpName) loaded of
          [plugin] -> pure plugin
          plugins -> expectationFailure ("expected one gated plugin, got " <> show (length plugins)) >> fail "missing gated plugin"
        oldRuntime <- case lpRuntime oldPlugin of
          Just runtime -> pure runtime
          Nothing -> expectationFailure "gated plugin had no owned runtime" >> fail "missing gated runtime"
        preparedFailure <- call @"prepareQueryData" pluginManagerHandle #prepareQueryData
          (Text.pack gatedDataPluginName, testQuery)
        failureLease <- case preparedFailure of
          Right lease -> pure lease
          Left err -> expectationFailure ("failed to prepare stale-failure lease: " <> Text.unpack err) >> fail "missing route lease"
        done <- newEmptyMVar
        _ <- forkIO (queryPluginResource pluginManagerHandle (Text.pack gatedDataPluginName) testQuery >>= putMVar done)
        waitForFixtureSignal queryReceived

        Just (token, _, _) <- call @"refresh" pluginManagerHandle #refresh ()
        duringTransition <- call @"prepareQueryData" pluginManagerHandle #prepareQueryData
          (Text.pack gatedDataPluginName, testQuery)
        case duringTransition of
          Left err -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Right _ -> expectationFailure "prepared a data lease while refresh was starting"
        transitionResult <- call @"finalizeQueryData" pluginManagerHandle #finalizeQueryData
          ( failureLease
          , Right (QueryResult "records" [record [("id", String "transition"), ("name", String "Transition")]] (Just 1))
          )
        case transitionResult of
          Left err -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Right _ -> expectationFailure "accepted old data while refresh was starting"
        let replacementConnection = newRPCConnection
              (lpManifest oldPlugin)
              (Transport stdin stdout "stale-data-route-replacement")
              (lpParams oldPlugin)
            replacement = oldPlugin
              { lpRuntime = Just (newConnectionOnlyPluginRuntime replacementConnection) }
        call @"finishRefresh" pluginManagerHandle #finishRefresh (token, [replacement]) `shouldReturn` True
        writeFile queryRelease "release\n"
        stale <- expectWithin "stale leased query completion" (takeMVar done)
        case stale of
          Left err -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Right _ -> expectationFailure "old runtime result escaped the generation guard"
        staleFailure <- call @"finalizeQueryData" pluginManagerHandle #finalizeQueryData
          (failureLease, Left "external_data_source_unavailable: obsolete runtime" :: Either Text QueryResult)
        case staleFailure of
          Left err -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Right _ -> expectationFailure "old runtime failure escaped the generation guard"
        staleTimeout <- call @"finalizeQueryData" pluginManagerHandle #finalizeQueryData
          (failureLease, Left "timeout: obsolete runtime timed out" :: Either Text QueryResult)
        case staleTimeout of
          Left err -> do
            err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
            err `shouldNotSatisfy` Text.isInfixOf "timeout"
          Right _ -> expectationFailure "old runtime timeout escaped the generation guard"
        observed <- getLoadedPlugins pluginManagerHandle
        pluginStatuses gatedDataPluginName observed `shouldBe` [lpStatus replacement]
        pluginLifecycleStates gatedDataPluginName observed `shouldBe` [plsState (lpLifecycle replacement)]
        pluginLifecycleErrorCodes gatedDataPluginName observed `shouldBe` [plsErrorCode (lpLifecycle replacement)]

        Just (stopToken, _, replacementPlugins) <- call @"refresh" pluginManagerHandle #refresh ()
        let detached = map (\plugin -> plugin { lpStatus = PluginIdle, lpRuntime = Nothing }) replacementPlugins
        call @"finishRefresh" pluginManagerHandle #finishRefresh (stopToken, detached) `shouldReturn` True
        cleanupOwnedPluginRuntime oldRuntime >>= \case
          OwnedPluginRuntimeCleanupComplete -> pure ()
          OwnedPluginRuntimeCleanupFailed _ -> expectationFailure "old gated runtime cleanup failed"

  it "times out data-resource requests instead of hanging" $ do
    withExecutablePluginDir hangQueryPluginName hangQueryManifestJSON "hang-query" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses hangQueryPluginName loaded `shouldSatisfy` elem PluginConnected
        handles <- expectPluginProcessHandles hangQueryPluginName loaded
        conn <- case mapMaybe lpConnection (filter ((== Text.pack hangQueryPluginName) . lpName) loaded) of
          [connection] -> pure connection
          connections -> expectationFailure ("expected one hang-query connection, got " <> show (length connections))
            >> fail "missing hang-query connection"
        prepared <- call @"prepareQueryData" pluginManagerHandle #prepareQueryData
          (Text.pack hangQueryPluginName, testQuery)
        lease <- case prepared of
          Right routeLease -> pure routeLease
          Left err -> expectationFailure ("failed to prepare timeout lease: " <> Text.unpack err)
            >> fail "missing timeout lease"
        rawTimed <- timeout 5000000 (queryResource conn testQuery)
        routedResult <- case rawTimed of
          Nothing -> expectationFailure "data query hung" >> fail "data query hung"
          Just (Left err) -> pure (Left (dataResourceFailureText (rpcErrorDataResourceFailure err)))
          Just (Right _) -> expectationFailure "hung data query unexpectedly succeeded" >> fail "unexpected data query result"
        -- Force the monitor-before-finalize ordering that used to make timeout
        -- service mapping scheduler-dependent.
        observed <- waitForLoadedPlugins
          (hangQueryPluginName <> " timeout cleanup")
          pluginManagerHandle
          (\loaded ->
            anyPluginErrorContaining "restart limit exceeded" (pluginStatuses hangQueryPluginName loaded)
              && Just "restart_limit_exceeded" `elem` pluginLifecycleErrorCodes hangQueryPluginName loaded
              && null (pluginProcessHandles hangQueryPluginName loaded))
        timed <- call @"finalizeQueryData" pluginManagerHandle #finalizeQueryData
          (lease, routedResult)
        case timed of
          Left err -> err `shouldSatisfy` Text.isInfixOf "timed out"
          Right _ -> expectationFailure "monitor-first timeout finalize unexpectedly succeeded"
        pluginStatuses hangQueryPluginName observed `shouldSatisfy` anyPluginErrorContaining "restart limit exceeded"
        pluginLifecycleErrorCodes hangQueryPluginName observed `shouldSatisfy` elem (Just "restart_limit_exceeded")
        length (pluginProcessHandles hangQueryPluginName observed) `shouldBe` 0
        unavailable <- timeout 1000000 $ queryPluginResource pluginManagerHandle (Text.pack hangQueryPluginName) testQuery
        case unavailable of
          Nothing -> expectationFailure "unavailable data query after timeout hung"
          Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Just (Right _) -> expectationFailure "unavailable data query after timeout unexpectedly succeeded"
        mapM_ (assertProcessExited hangQueryPluginName) handles

  it "denies data-resource queries when the plugin lacks dataRead" $ do
    withExecutablePluginDir noDataReadQueryPluginName noDataReadQueryManifestJSON "validation-ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses noDataReadQueryPluginName loaded `shouldSatisfy` elem PluginConnected
        timed <- timeout 1000000 $ queryPluginResource pluginManagerHandle (Text.pack noDataReadQueryPluginName) testQuery
        case timed of
          Nothing -> expectationFailure "missing dataRead query hung"
          Just (Left err) -> do
            err `shouldSatisfy` Text.isInfixOf "permission_denied"
            err `shouldSatisfy` Text.isInfixOf "dataRead"
          Just (Right _) -> expectationFailure "query without dataRead unexpectedly succeeded"

  it "denies data-resource mutations when the plugin lacks dataWrite" $ do
    withExecutablePluginDir noDataWriteMutationPluginName noDataWriteMutationManifestJSON "validation-ok" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses noDataWriteMutationPluginName loaded `shouldSatisfy` elem PluginConnected
        timed <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack noDataWriteMutationPluginName)
          (MutateResource "records" (MutCreate (record [("id", String "alpha")])))
        case timed of
          Nothing -> expectationFailure "missing dataWrite mutation hung"
          Just (Left err) -> do
            err `shouldSatisfy` Text.isInfixOf "permission_denied"
            err `shouldSatisfy` Text.isInfixOf "dataWrite"
          Just (Right _) -> expectationFailure "mutation without dataWrite unexpectedly succeeded"

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

  it "rejects widening/write-capable negotiated handshake data-resource schemas during startup" $ do
    withExecutablePluginDir wideningHandshakePluginName wideningHandshakeManifestJSON "widening-handshake" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses wideningHandshakePluginName loaded `shouldSatisfy` anyPluginErrorContaining "invalid handshake data resources"
        pluginStatuses wideningHandshakePluginName loaded `shouldSatisfy` anyPluginErrorContaining "dataWrite"
        pluginStatuses wideningHandshakePluginName loaded `shouldSatisfy` anyPluginErrorContaining "cannot add fields"
        pluginLifecycleStates wideningHandshakePluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes wideningHandshakePluginName loaded `shouldSatisfy` elem (Just "protocol_error")
        length (pluginProcessHandles wideningHandshakePluginName loaded) `shouldBe` 0
        mutation <- timeout 1000000 $ mutatePluginResource pluginManagerHandle (Text.pack wideningHandshakePluginName)
          (MutateResource "records" (MutCreate (record [("id", String "alpha"), ("name", String "Alpha")])))
        case mutation of
          Nothing -> expectationFailure "failed handshake mutation hung"
          Just (Left err) -> err `shouldSatisfy` Text.isInfixOf "plugin_unavailable"
          Just (Right _) -> expectationFailure "write-capable handshake after read-only manifest unexpectedly mutated"

  it "surfaces generator crashes without hanging" $ do
    withExecutablePluginDir generatorCrashPluginName generatorCrashManifestJSON "exit-on-generator" $ do
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        conn <- case pluginConnections generatorCrashPluginName loaded of
          Just c -> pure c
          Nothing -> expectationFailure "plugin did not connect" >> fail "missing plugin connection"
        timed <- timeout 1000000 $ invokeGenerator conn 0 Aeson.Null
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

assertStartupHandoffOutcome
  :: String
  -> Bool
  -> Either SomeException ()
  -> [LoadedPlugin]
  -> Expectation
assertStartupHandoffOutcome phase rollsBack result loaded = do
  case (rollsBack, result) of
    (True, Left _) -> pure ()
    (True, Right ()) -> expectationFailure (phase <> " startup handoff unexpectedly completed")
    (False, Left err) -> expectationFailure (phase <> " startup failure escaped supervisor: " <> show err)
    (False, Right ()) -> pure ()
  case filter ((== Text.pack testLaunchPluginName) . lpName) loaded of
    [plugin] -> do
      isNothing (lpRuntime plugin) `shouldBe` True
      isNothing (lpConnection plugin) `shouldBe` True
      isNothing (lpProcessHandle plugin) `shouldBe` True
      if rollsBack
        then do
          lpStatus plugin `shouldBe` PluginIdle
          plsState (lpLifecycle plugin) `shouldBe` LifecycleDiscovered
          plsErrorCode (lpLifecycle plugin) `shouldBe` Nothing
        else do
          lpStatus plugin `shouldSatisfy` \case
            PluginError message -> not (Text.null message)
            _ -> False
          plsState (lpLifecycle plugin) `shouldBe` LifecycleFailed
          plsErrorCode (lpLifecycle plugin) `shouldBe` Just "launch_failed"
    matching -> expectationFailure
      (phase <> " expected exactly one plugin, got " <> show (length matching)
        <> ". Full state:\n" <> summarizeLoadedPlugins loaded)

assertSecretAbsentFromPluginDiagnostics :: String -> Text -> [LoadedPlugin] -> Expectation
assertSecretAbsentFromPluginDiagnostics pluginName secret loaded = do
  secret `shouldSatisfy` (not . Text.null)
  mapM_ (`shouldNotSatisfy` Text.isInfixOf secret) diagnosticTexts
  where
    diagnosticTexts =
      [ text
      | plugin <- loaded
      , lpName plugin == Text.pack pluginName
      , text <- Text.pack (show (lpStatus plugin)) : lifecycleDiagnosticTexts (lpLifecycle plugin)
      ]

lifecycleDiagnosticTexts :: PluginLifecycleSnapshot -> [Text]
lifecycleDiagnosticTexts snapshot =
  [ text
  | mText <-
      [ plsReason snapshot
      , plsErrorCode snapshot
      , plsErrorMessage snapshot
      , plsBlockingDependency snapshot
      , plsProcessId snapshot
      ]
  , Just text <- [mText]
  ] <> maybe [] (pure . Text.pack . show) (plsProtocolVersion snapshot)
    <> plsResources snapshot

pluginConnections :: String -> [LoadedPlugin] -> Maybe RPCConnection
pluginConnections _ [] = Nothing
pluginConnections name (plugin:rest)
  | lpName plugin == Text.pack name = case lpConnection plugin of
      Just conn -> Just conn
      Nothing -> Nothing
  | otherwise = pluginConnections name rest

simulationPlanPlugin :: Text -> [Text] -> [Capability] -> LoadedPlugin
simulationPlanPlugin name deps capabilities = plugin
  where
    plugin = LoadedPlugin
      { lpName = name
      , lpManifest = manifest
      , lpParams = Map.empty
      , lpStatus = PluginConnected
      , lpLifecycle = pluginLifecycleSnapshot (posixSecondsToUTCTime 0) LifecycleReady Nothing Nothing Nothing Nothing Nothing (Just currentProtocolVersion) []
      , lpRuntime = Just (newConnectionOnlyPluginRuntime
          (newRPCConnection manifest (Transport stdin stdout name) Map.empty))
      , lpStartPolicy = defaultRPCStartPolicy
      , lpRestartHistory = []
      , lpDirectory = ""
      , lpOverlaySchema = Just (simulationPlanOverlaySchema name)
      }
    manifest = simulationPlanManifest name deps capabilities

simulationPlanManifest :: Text -> [Text] -> [Capability] -> RPCManifest
simulationPlanManifest name deps capabilities = RPCManifest
  { rmManifestVersion = 3
  , rmName = name
  , rmVersion = "1.0.0"
  , rmRuntime = RPCManifestRuntime currentProtocolVersion currentProtocolVersion Nothing Nothing
  , rmDescription = ""
  , rmUiHints = defaultRPCUIHints
  , rmGenerator = Nothing
  , rmSimulation = Just RPCSimulationDecl
      { rsdDependencies = deps
      , rsdSchedule = defaultScheduleDecl
      }
  , rmOverlay = Just (RPCOverlayDecl "test.toposchema")
  , rmCapabilities = capabilities
  , rmParameters = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy = defaultRPCStartPolicy
  }

simulationPlanOverlaySchema :: Text -> OverlaySchema
simulationPlanOverlaySchema name = OverlaySchema
  { osName = name
  , osVersion = "1.0.0"
  , osDescription = "simulation plan test overlay"
  , osFields = []
  , osStorage = StorageSparse
  , osDependencies = OverlayDeps { odTerrain = False, odOverlays = [] }
  , osFieldIndex = Map.empty
  }

pluginOwnedProcesses :: String -> [LoadedPlugin] -> [OwnedPluginProcess]
pluginOwnedProcesses name loaded =
  [ ownedProcess
  | plugin <- loaded
  , lpName plugin == Text.pack name
  , Just ownedProcess <- [lpProcessHandle plugin]
  ]

pluginProcessHandles :: String -> [LoadedPlugin] -> [ProcessHandle]
pluginProcessHandles name = map ownedPluginProcessHandle . pluginOwnedProcesses name

assertOwnedCleanupComplete :: OwnedPluginProcess -> IO ()
assertOwnedCleanupComplete ownedProcess = do
  cleanupResult <- cleanupOwnedPluginProcess ownedProcess
  case cleanupResult of
    OwnedPluginCleanupComplete -> pure ()
    OwnedPluginCleanupFailed _ -> expectationFailure "owned plugin process cleanup did not complete"

expectPluginProcessHandles :: String -> [LoadedPlugin] -> IO [ProcessHandle]
expectPluginProcessHandles name loaded =
  case pluginProcessHandles name loaded of
    [] -> expectationFailure (name <> " did not expose a fixture process handle") >> fail "missing process handle"
    handles -> pure handles

forkLifecycleAction :: IO () -> IO (MVar (Either SomeException ()))
forkLifecycleAction action = snd <$> forkCancellableLifecycleAction action

forkCancellableLifecycleAction :: IO () -> IO (ThreadId, MVar (Either SomeException ()))
forkCancellableLifecycleAction action = do
  done <- newEmptyMVar
  worker <- forkIO (try @SomeException action >>= putMVar done)
  pure (worker, done)

waitForLifecycleAction :: String -> MVar (Either SomeException ()) -> IO ()
waitForLifecycleAction label done = do
  result <- waitForLifecycleActionResult label done
  case result of
    Left err -> expectationFailure $ label <> " failed: " <> show err
    Right () -> pure ()

waitForLifecycleActionResult
  :: String
  -> MVar (Either SomeException ())
  -> IO (Either SomeException ())
waitForLifecycleActionResult label done = do
  result <- timeout lifecycleActionTimeoutMicros (takeMVar done)
  case result of
    Nothing -> expectationFailure (label <> " did not finish within bounded test timeout")
      >> fail label
    Just completed -> pure completed

expectAsyncLifecycleCancellation :: String -> Either SomeException () -> Expectation
expectAsyncLifecycleCancellation phase result =
  case result of
    Left err | isJust (fromException err :: Maybe SomeAsyncException) -> pure ()
    Left err -> expectationFailure (phase <> " returned a non-async cancellation error: " <> show err)
    Right () -> expectationFailure (phase <> " completed instead of observing cancellation")

assertCancelledEndpointGone :: String -> Text -> IO ()
assertCancelledEndpointGone phase diagnostic
  | Text.null diagnostic = expectationFailure (phase <> " pause marker omitted endpoint diagnostics")
  | os == "mingw32" = do
      connection <- connectPluginEndpoint "cancelled-startup-probe" TransportEndpoint
        { teKind = TransportEndpointNamedPipe
        , teAddress = Text.unpack diagnostic
        }
      case connection of
        Left _ -> pure ()
        Right transport -> closeTransport transport
          >> expectationFailure (phase <> " left named-pipe handles reachable")
  | otherwise = do
      let socketPath = Text.unpack diagnostic
      doesPathExist socketPath `shouldReturn` False
      doesPathExist (takeDirectory socketPath) `shouldReturn` False

waitForPluginLifecycleState
  :: String
  -> String
  -> PluginLifecycleState
  -> ActorHandle PluginManager (Protocol PluginManager)
  -> IO [LoadedPlugin]
waitForPluginLifecycleState label pluginName expected pluginManagerHandle =
  go transientLifecyclePollAttempts []
  where
    go attemptsLeft observed = do
      loaded <- getLoadedPlugins pluginManagerHandle
      let observed' = observed <> [summarizePluginObservation pluginName loaded]
      if expected `elem` pluginLifecycleStates pluginName loaded
        then pure loaded
        else if attemptsLeft <= 0
          then do
            expectationFailure $
              label <> " did not observe " <> show expected <> ". Observed states/statuses:\n"
                <> formatObservedLifecycleStates observed'
                <> "Last full plugin state:\n"
                <> summarizeLoadedPlugins loaded
            fail label
          else threadDelay transientLifecyclePollDelayMicros >> go (attemptsLeft - 1) observed'

summarizePluginObservation :: String -> [LoadedPlugin] -> String
summarizePluginObservation pluginName loaded =
  case filter ((== Text.pack pluginName) . lpName) loaded of
    [] -> pluginName <> " missing; loaded plugins=" <> show (map (Text.unpack . lpName) loaded)
    matching -> unlines (map summarizeLoadedPlugin matching)

formatObservedLifecycleStates :: [String] -> String
formatObservedLifecycleStates observed =
  unlines [show index <> ". " <> observation | (index, observation) <- zip [(1 :: Int)..] observed]

waitForLoadedPlugins
  :: String
  -> ActorHandle PluginManager (Protocol PluginManager)
  -> ([LoadedPlugin] -> Bool)
  -> IO [LoadedPlugin]
waitForLoadedPlugins label pluginManagerHandle predicate = go cleanupPollAttempts
  where
    go attemptsLeft = do
      loaded <- getLoadedPlugins pluginManagerHandle
      if predicate loaded
        then pure loaded
        else if attemptsLeft <= 0
          then do
            expectationFailure $
              label <> " did not reach expected cleanup state. Last plugin state:\n"
                <> summarizeLoadedPlugins loaded
            fail label
          else threadDelay cleanupPollDelayMicros >> go (attemptsLeft - 1)

summarizeLoadedPlugins :: [LoadedPlugin] -> String
summarizeLoadedPlugins [] = "<no plugins>"
summarizeLoadedPlugins plugins = unlines (map summarizeLoadedPlugin plugins)

summarizeLoadedPlugin :: LoadedPlugin -> String
summarizeLoadedPlugin plugin =
  Text.unpack (lpName plugin)
    <> " status=" <> show (lpStatus plugin)
    <> " lifecycle=" <> show (plsState (lpLifecycle plugin))
    <> " errorCode=" <> show (plsErrorCode (lpLifecycle plugin))
    <> " processId=" <> show (plsProcessId (lpLifecycle plugin))
    <> " runtimeGeneration=" <> show (ownedPluginRuntimeGeneration <$> lpRuntime plugin)
    <> " runtimeOwnerId=" <> show (lpRuntime plugin >>= ownedPluginRuntimeProcess >>= ownedPluginProcessId)
    <> " hasRuntime=" <> show (isJust (lpRuntime plugin))
    <> " hasConnection=" <> show (isJust (lpConnection plugin))
    <> " hasProcess=" <> show (isJust (lpProcessHandle plugin))

assertProcessExited :: String -> ProcessHandle -> IO ()
assertProcessExited label processHandle = do
  mPid <- getPid processHandle
  exited <- waitForProcessExitCode processExitAssertTimeoutMicros processHandle
  if exited
    then pure ()
    else do
      mExitCode <- getProcessExitCode processHandle
      expectationFailure $
        label <> " fixture process " <> maybe "<unknown pid>" show mPid
          <> " did not exit after shutdown; last observed exit code: " <> show mExitCode

waitForProcessExitCode :: Int -> ProcessHandle -> IO Bool
waitForProcessExitCode remainingMicros processHandle = do
  mExitCode <- getProcessExitCode processHandle
  case mExitCode of
    Just _ -> pure True
    Nothing
      | remainingMicros <= 0 -> pure False
      | otherwise -> do
          let delayMicros = min cleanupPollDelayMicros remainingMicros
          threadDelay delayMicros
          waitForProcessExitCode (remainingMicros - delayMicros) processHandle

lifecycleActionTimeoutMicros :: Int
lifecycleActionTimeoutMicros = 10000000

transientLifecyclePollAttempts :: Int
transientLifecyclePollAttempts = 200

transientLifecyclePollDelayMicros :: Int
transientLifecyclePollDelayMicros = 25000

processExitAssertTimeoutMicros :: Int
processExitAssertTimeoutMicros = 5000000

cleanupPollAttempts :: Int
cleanupPollAttempts = 100

cleanupPollDelayMicros :: Int
cleanupPollDelayMicros = 50000

data HostDeathDiagnostics = HostDeathDiagnostics
  { hddPlatform :: !String
  , hddHostPid :: !Word64
  , hddLeaderPid :: !Word64
  , hddOrdinaryPid :: !Word64
  , hddEscapedPid :: !Word64
  , hddOrdinaryHeartbeat :: !FilePath
  , hddEscapedHeartbeat :: !FilePath
  } deriving (Eq, Show)

type HostDeathCleanupState = MVar (Maybe (Set.Set Word64))

withActualHostDeathFixture
  :: (ProcessHandle -> HostDeathDiagnostics -> HostDeathCleanupState -> IO a)
  -> IO a
withActualHostDeathFixture action =
  withExecutablePluginDir
    hostDeathPluginName
    hostDeathManifestJSON
    "host-death-tree" $
      bracket launch cleanup $ \(hostProcess, cleanupState) -> do
        diagnostics <- waitForHostDeathDiagnostics
        let fixturePids = Set.fromList
              [ hddLeaderPid diagnostics
              , hddOrdinaryPid diagnostics
              , hddEscapedPid diagnostics
              ]
        modifyMVar_ cleanupState (const (pure (Just fixturePids)))
        action hostProcess diagnostics cleanupState
  where
    launch = do
      cleanupState <- newMVar Nothing
      hostProcess <- launchHostDeathFixture
      pure (hostProcess, cleanupState)
    cleanup (hostProcess, cleanupState) =
      cleanupHostDeathFixture hostProcess cleanupState

launchHostDeathFixture :: IO ProcessHandle
launchHostDeathFixture = do
  testExe <- getExecutablePath
  (_, _, _, processHandle) <- createProcess
    (proc testExe ["--plugin-manager-fixture", "host-death-host"])
      { std_in = NoStream
      , std_out = NoStream
      , std_err = Inherit
      }
  pure processHandle

cleanupHostDeathFixture :: ProcessHandle -> HostDeathCleanupState -> IO ()
cleanupHostDeathFixture hostProcess cleanupState = do
  stopFixtureHostIfRunning hostProcess
  tracked <- readMVar cleanupState
  leader <- retainedHostDeathPid tracked hostDeathLeaderPidFileName
#if defined(mingw32_HOST_OS)
  ordinary <- retainedHostDeathPid tracked hostDeathOrdinaryPidFileName
  escaped <- retainedHostDeathPid tracked hostDeathEscapedPidFileName
  let remaining = [pid | Just pid <- [leader, ordinary, escaped]]
  mapM_ terminateFixturePid remaining
  verifyFixturePidsGone remaining
#else
  -- Kill and observe the leader before discovering descendants, then signal
  -- the now-stable launch group again. This closes the fork race: every child
  -- was either present for a group signal or published an escape PID before a
  -- release that the dead leader can no longer issue.
  forM_ leader $ \pid -> do
    terminateFixtureLaunchGroup pid
    verifyFixturePidsGone [pid]
    terminateFixtureLaunchGroup pid
  ordinary <- retainedHostDeathPid tracked hostDeathOrdinaryPidFileName
  escaped <- retainedHostDeathPid tracked hostDeathEscapedPidFileName
  when (isNothing leader) (forM_ ordinary terminateFixturePid)
  forM_ escaped terminateFixtureLaunchGroup
  verifyFixturePidsGone [pid | Just pid <- [ordinary, escaped]]
#endif

retainedHostDeathPid
  :: Maybe (Set.Set Word64)
  -> String
  -> IO (Maybe Word64)
retainedHostDeathPid tracked fileName = do
  mPid <- readHostDeathPidMaybe fileName
  pure $ mPid >>= \pid ->
    if maybe True (Set.member pid) tracked then Just pid else Nothing

verifyFixturePidsGone :: [Word64] -> IO ()
verifyFixturePidsGone pids = forM_ pids $ \pid -> do
  gone <- waitForFixturePidGone pid
  unless gone $ throwIO (userError
    ("could not clean host-death fixture pid " <> show pid))

terminateFixtureHost :: ProcessHandle -> IO ()
terminateFixtureHost processHandle = do
  mExit <- getProcessExitCode processHandle
  case mExit of
    Just exitCode -> expectationFailure
      ("external plugin host exited before forced host death: " <> show exitCode)
    Nothing -> forceTerminateFixtureHost processHandle
  assertFixtureHostExited processHandle

stopFixtureHostIfRunning :: ProcessHandle -> IO ()
stopFixtureHostIfRunning processHandle = do
  mExit <- getProcessExitCode processHandle
  when (isNothing mExit) (forceTerminateFixtureHost processHandle)
  assertFixtureHostExited processHandle

assertFixtureHostExited :: ProcessHandle -> IO ()
assertFixtureHostExited processHandle = do
  exited <- waitForProcessExitCode processExitAssertTimeoutMicros processHandle
  unless exited $ do
    mPid <- getPid processHandle
    expectationFailure
      ("external plugin host " <> maybe "<unknown pid>" show mPid
        <> " did not exit after forced host termination")

forceTerminateFixtureHost :: ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
forceTerminateFixtureHost = terminateProcess
#else
forceTerminateFixtureHost processHandle = do
  mPid <- getPid processHandle
  case mPid of
    Nothing -> terminateProcess processHandle
    Just pid -> signalProcess sigKILL (fromIntegral pid)
#endif

waitForHostDeathDiagnostics :: IO HostDeathDiagnostics
waitForHostDeathDiagnostics = do
  readyPath <- fixtureDataFile hostDeathPluginName hostDeathReadyFileName
  result <- timeout hostDeathReadyTimeoutMicros (poll readyPath)
  case result of
    Just diagnostics -> pure diagnostics
    Nothing -> do
      lastMarker <- readTextFileMaybe readyPath
      expectationFailure
        ("external plugin host did not publish ready diagnostics at " <> readyPath
          <> "; last marker=" <> show lastMarker)
      fail "missing host-death diagnostics"
  where
    poll readyPath = do
      marker <- readTextFileMaybe readyPath
      case marker >>= either (const Nothing) Just . parseHostDeathDiagnostics readyPath of
        Just diagnostics -> pure diagnostics
        Nothing -> case marker of
          Just raw | "error=" `isPrefixOfString` raw ->
            expectationFailure ("external plugin host setup failed: " <> raw)
              >> fail "host-death fixture setup failed"
          _ -> threadDelay 25000 >> poll readyPath

parseHostDeathDiagnostics :: FilePath -> String -> Either String HostDeathDiagnostics
parseHostDeathDiagnostics readyPath raw = do
  platform <- requireDiagnostic "platform"
  hostPid <- requirePid "host_pid"
  leaderPid <- requirePid "leader_pid"
  ordinaryPid <- requirePid "ordinary_pid"
  escapedPid <- requirePid "escaped_pid"
  let allPids = Set.fromList [hostPid, leaderPid, ordinaryPid, escapedPid]
      dataRoot = takeDirectory readyPath
  if Set.size allPids /= 4
    then Left "host, leader, and descendant pids were not distinct"
    else pure HostDeathDiagnostics
      { hddPlatform = platform
      , hddHostPid = hostPid
      , hddLeaderPid = leaderPid
      , hddOrdinaryPid = ordinaryPid
      , hddEscapedPid = escapedPid
      , hddOrdinaryHeartbeat = dataRoot </> hostDeathOrdinaryHeartbeatFileName
      , hddEscapedHeartbeat = dataRoot </> hostDeathEscapedHeartbeatFileName
      }
  where
    fields = map splitDiagnosticLine (lines raw)
    requireDiagnostic key = case lookup key fields of
      Just value | not (null value) -> Right value
      _ -> Left ("missing " <> key)
    requirePid key = do
      rawPid <- requireDiagnostic key
      case parseSafeFixturePid rawPid of
        Just pid -> Right pid
        Nothing -> Left ("invalid " <> key <> ": " <> rawPid)

splitDiagnosticLine :: String -> (String, String)
splitDiagnosticLine line = case break (== '=') line of
  (key, '=':value) -> (key, value)
  _ -> (line, "")

isPrefixOfString :: String -> String -> Bool
isPrefixOfString prefix value = take (length prefix) value == prefix

readTextFileMaybe :: FilePath -> IO (Maybe String)
readTextFileMaybe path = do
  exists <- doesFileExist path
  if exists
    then (Just <$> readFile path) `catch` \(_ :: SomeException) -> pure Nothing
    else pure Nothing

writeAtomicTextFile :: FilePath -> String -> IO ()
writeAtomicTextFile path content = do
  writerPid <- currentProcessIdForTest
  let temporaryPath = path <> ".tmp-" <> show writerPid
  writeFile temporaryPath content
  renameFile temporaryPath path

readHostDeathPidMaybe :: String -> IO (Maybe Word64)
readHostDeathPidMaybe fileName = do
  path <- fixtureDataFile hostDeathPluginName fileName
  raw <- readTextFileMaybe path
  pure (raw >>= parseSafeFixturePid)

parseSafeFixturePid :: String -> Maybe Word64
parseSafeFixturePid raw = case reads raw of
  [(pid, "")] | pid > 1 -> Just pid
  _ -> Nothing

assertHostDeathDiagnostics :: ProcessHandle -> HostDeathDiagnostics -> Expectation
assertHostDeathDiagnostics hostProcess diagnostics = do
  hddPlatform diagnostics `shouldBe` os
  mHostPid <- getPid hostProcess
  (mHostPid >>= parseSafeFixturePid . show) `shouldBe` Just (hddHostPid diagnostics)
  hddLeaderPid diagnostics `shouldSatisfy` (> 0)
  hddOrdinaryPid diagnostics `shouldSatisfy` (> 0)
  hddEscapedPid diagnostics `shouldSatisfy` (> 0)

assertFixturePidGone
  :: HostDeathCleanupState
  -> String
  -> HostDeathDiagnostics
  -> Word64
  -> IO ()
assertFixturePidGone cleanupState label diagnostics pid = do
  gone <- waitForFixturePidGone pid
  if gone
    then modifyMVar_ cleanupState (pure . fmap (Set.delete pid))
    else expectationFailure
      (label <> ": pid " <> show pid <> " remained; diagnostics=" <> show diagnostics)

waitForFixturePidGone :: Word64 -> IO Bool
waitForFixturePidGone pid = isJust <$> timeout processExitAssertTimeoutMicros waitUntilGone
  where
    waitUntilGone = do
      exists <- processIdExists pid
      if exists
        then threadDelay cleanupPollDelayMicros >> waitUntilGone
        else pure ()

terminateFixtureLaunchGroup :: Word64 -> IO ()
#if defined(mingw32_HOST_OS)
terminateFixtureLaunchGroup = terminateFixturePid
#else
terminateFixtureLaunchGroup pid =
  ignoreMissingFixtureProcess (signalProcessGroup sigKILL (fromIntegral pid))
#endif

terminateFixturePid :: Word64 -> IO ()
#if defined(mingw32_HOST_OS)
terminateFixturePid pid = do
  processHandle <- c_OpenProcessForTest processTerminateRight 0 (fromIntegral pid)
  when (processHandle /= nullPtr) $ do
    _ <- c_TerminateProcessForTest processHandle 1
    _ <- c_CloseHandleForTest processHandle
    pure ()
#else
terminateFixturePid pid =
  ignoreMissingFixtureProcess (signalProcess sigKILL (fromIntegral pid))

ignoreMissingFixtureProcess :: IO () -> IO ()
ignoreMissingFixtureProcess action =
  action `catch` \(err :: IOException) ->
    unless (isDoesNotExistError err) (throwIO err)
#endif

hostDeathReadyTimeoutMicros :: Int
hostDeathReadyTimeoutMicros = 10000000

expectHeartbeatAdvances :: FilePath -> IO ()
expectHeartbeatAdvances path = do
  first <- waitForHeartbeatChange path Nothing "start"
  _ <- waitForHeartbeatChange path (Just first) "advance"
  pure ()

waitForHeartbeatChange :: FilePath -> Maybe String -> String -> IO String
waitForHeartbeatChange path previous label = do
  result <- timeout 2000000 go
  case result of
    Nothing -> expectationFailure ("process-tree fixture heartbeat did not " <> label) >> fail "heartbeat did not change"
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
    Nothing -> expectationFailure "process-tree fixture child heartbeat kept changing after plugin teardown"
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

readProcessTreeChildPid :: String -> IO Word64
readProcessTreeChildPid pluginName = do
  path <- fixtureDataFile pluginName processTreeChildPidFileName
  result <- timeout 2000000 (waitForPid path)
  case result of
    Nothing -> expectationFailure
      (pluginName <> " did not publish descendant pid at " <> path) >> fail "missing descendant pid"
    Just pid -> pure pid
  where
    waitForPid path = do
      exists <- doesFileExist path
      if not exists
        then threadDelay 25000 >> waitForPid path
        else do
          raw <- readFile path
          case reads raw of
            [(pid, _)] -> pure pid
            _ -> threadDelay 25000 >> waitForPid path

assertProcessTreeChildGone :: String -> [OwnedPluginProcess] -> Word64 -> IO ()
assertProcessTreeChildGone label owners childPid = do
  gone <- timeout 3000000 waitUntilGone
  case gone of
    Just () -> pure ()
    Nothing -> expectationFailure $
      label <> " descendant pid " <> show childPid
        <> " remained after owned cleanup; owner identities="
        <> show (map ownedPluginProcessId owners)
  where
    waitUntilGone = do
      exists <- processIdExists childPid
      if exists
        then threadDelay 25000 >> waitUntilGone
        else pure ()

processIdExists :: Word64 -> IO Bool
#if defined(mingw32_HOST_OS)
processIdExists pid = do
  processHandle <- c_OpenProcessForTest processQueryLimitedInformation 0 (fromIntegral pid)
  if processHandle == nullPtr
    then (/= invalidParameterError) <$> c_GetLastErrorForTest
    else do
      alloca $ \exitCodePtr -> do
        queried <- c_GetExitCodeProcessForTest processHandle exitCodePtr
        exitCode <- if queried == 0 then pure stillActiveExitCode else peek exitCodePtr
        _ <- c_CloseHandleForTest processHandle
        -- Query/access errors are conservative: only a successfully observed
        -- non-STILL_ACTIVE status proves this PID is gone.
        pure (queried == 0 || exitCode == stillActiveExitCode)

foreign import ccall unsafe "windows.h GetCurrentProcessId"
  c_GetCurrentProcessId :: IO Word32

foreign import ccall unsafe "windows.h GetLastError"
  c_GetLastErrorForTest :: IO Word32

foreign import ccall unsafe "windows.h OpenProcess"
  c_OpenProcessForTest :: Word32 -> CInt -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "windows.h GetExitCodeProcess"
  c_GetExitCodeProcessForTest :: Ptr () -> Ptr Word32 -> IO CInt

foreign import ccall unsafe "windows.h CloseHandle"
  c_CloseHandleForTest :: Ptr () -> IO CInt

foreign import ccall unsafe "windows.h TerminateProcess"
  c_TerminateProcessForTest :: Ptr () -> Word32 -> IO CInt

processQueryLimitedInformation :: Word32
processQueryLimitedInformation = 0x00001000

processTerminateRight :: Word32
processTerminateRight = 0x00000001

stillActiveExitCode :: Word32
stillActiveExitCode = 259

invalidParameterError :: Word32
invalidParameterError = 87
#else
processIdExists pid = do
  result <- try @IOException (signalProcess nullSignal (fromIntegral pid))
  case result of
    Right () ->
#if defined(linux_HOST_OS)
      linuxProcessIsRunning pid
#else
      pure True
#endif
    Left err -> pure (not (isDoesNotExistError err))

#if defined(linux_HOST_OS)
linuxProcessIsRunning :: Word64 -> IO Bool
linuxProcessIsRunning pid = do
  stat <- readTextFileMaybe ("/proc/" <> show pid <> "/stat")
  pure $ case stat >>= linuxProcessState of
    Just state -> state `notElem` ['Z', 'X']
    Nothing -> True

linuxProcessState :: String -> Maybe Char
linuxProcessState raw = case break (== ')') (reverse raw) of
  (reversedAfterCommand, ')':_) -> case words (reverse reversedAfterCommand) of
    state:_ -> case state of
      [value] -> Just value
      _ -> Nothing
    [] -> Nothing
  _ -> Nothing
#endif
#endif

expectPluginConnection :: String -> [LoadedPlugin] -> IO RPCConnection
expectPluginConnection name loaded =
  case pluginConnections name loaded of
    Just conn -> pure conn
    Nothing -> expectationFailure (name <> " did not expose an RPC connection") >> fail "missing plugin connection"

expectPluginRuntime :: String -> [LoadedPlugin] -> IO OwnedPluginRuntime
expectPluginRuntime name loaded =
  case [runtime | plugin <- loaded, lpName plugin == Text.pack name, Just runtime <- [lpRuntime plugin]] of
    [runtime] -> pure runtime
    runtimes -> expectationFailure
      (name <> " expected one runtime, got " <> show (length runtimes)
        <> ". Full state:\n" <> summarizeLoadedPlugins loaded) >> fail "missing plugin runtime"

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

expectPromptly :: String -> IO a -> IO a
expectPromptly label action = do
  result <- timeout 500000 action
  case result of
    Nothing -> expectationFailure (label <> " did not complete while RPC was gated") >> fail label
    Just value -> pure value

assertStartedBefore :: String -> String -> [String] -> Expectation
assertStartedBefore first second order =
  case (elemIndex first order, elemIndex second order) of
    (Just firstIndex, Just secondIndex) -> firstIndex `shouldSatisfy` (< secondIndex)
    _ -> expectationFailure ("startup order did not contain expected plugins: " <> show order)

externalProviderStatusFailureModes :: [(String, Text)]
externalProviderStatusFailureModes =
  [ ("timeout", "timeout")
  , ("transport", "transport")
  , ("malformed", "protocol/decode")
  , ("unexpected", "unexpected response")
  , ("plugin-error", "plugin error")
  ]

expectRequiredStatusRefreshFailure :: String -> Text -> IO ()
expectRequiredStatusRefreshFailure modeName expectedClass =
  withExecutablePluginDirs
    [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
    , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
    ] $ do
      writeExternalProviderStatusMode modeName
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_blocked")
        providerSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectProviderStatusMessage externalProviderPluginNameText expectedClass providerSnapshots

expectOptionalStatusRefreshFailure :: String -> Text -> IO ()
expectOptionalStatusRefreshFailure modeName expectedClass =
  withExecutablePluginDirs
    [ (externalConsumerPluginName, externalOptionalConsumerManifestJSON, "external-consumer")
    , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
    ] $ do
      writeExternalProviderStatusMode modeName
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleDegraded
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_degraded")
        providerSnapshots <- getPluginExternalDataSources pluginManagerHandle
        expectProviderStatusMessage externalProviderPluginNameText expectedClass providerSnapshots

expectRequiredStatusReportScopeMismatch :: String -> Text -> IO ()
expectRequiredStatusReportScopeMismatch modeName expectedNeedle =
  withExecutablePluginDirs
    [ (externalConsumerPluginName, externalConsumerManifestJSON, "external-consumer")
    , (externalProviderPluginName, externalProviderManifestJSON, "external-provider-controlled-status")
    ] $ do
      writeExternalProviderStatusMode modeName
      withPluginManager $ \pluginManagerHandle -> do
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` anyPluginErrorContaining "external data-source startup blocked"
        pluginStatuses externalConsumerPluginName loaded `shouldSatisfy` anyPluginErrorContaining expectedNeedle
        pluginLifecycleStates externalConsumerPluginName loaded `shouldSatisfy` elem LifecycleFailed
        pluginLifecycleErrorCodes externalConsumerPluginName loaded `shouldSatisfy` elem (Just "external_data_source_blocked")
        expectNoExternalGrantCallback
        snapshots <- getPluginExternalDataSources pluginManagerHandle
        expectProviderStatusMessage externalProviderPluginNameText expectedNeedle snapshots
        expectProviderGrantStatusCapabilityScope [] snapshots
        expectUnavailableConsumerBrokerSnapshot "unavailable" expectedNeedle snapshots

expectProviderStatusMessage :: Text -> Text -> [WorldExternalDataSourceSnapshot] -> Expectation
expectProviderStatusMessage providerName expectedNeedle snapshots =
  case findExternalSnapshot providerName snapshots of
    Nothing -> expectationFailure "missing provider external data-source snapshot"
    Just snapshot -> do
      let statuses = map redsdStatus (wedssProvidedSources snapshot)
            <> concatMap (map redsgStatus . redsdGrants) (wedssProvidedSources snapshot)
          messages = mapMaybe redssMessage statuses
      messages `shouldSatisfy` any (expectedNeedle `Text.isInfixOf`)

expectProviderGrantStatusCapabilityScope :: [RPCExternalDataSourceCapability] -> [WorldExternalDataSourceSnapshot] -> Expectation
expectProviderGrantStatusCapabilityScope expectedScope snapshots =
  case findExternalSnapshot externalProviderPluginNameText snapshots of
    Nothing -> expectationFailure "missing provider external data-source snapshot"
    Just snapshot -> case wedssProvidedSources snapshot of
      [source] -> case redsdGrants source of
        [grant] -> do
          redsgCapabilities grant `shouldBe` externalCapabilities
          redssCapabilityScope (redsgStatus grant) `shouldBe` expectedScope
        other -> expectationFailure ("expected one provider grant, got " <> show other)
      other -> expectationFailure ("expected one provider source, got " <> show other)

expectNoExternalGrantCallback :: IO ()
expectNoExternalGrantCallback = do
  operations <- readExternalConsumerOperationLog
  operations `shouldBe` []
  grantScopes <- readExternalConsumerGrantScopeLog
  grantScopes `shouldBe` []

writeExternalProviderStatusMode :: String -> IO ()
writeExternalProviderStatusMode modeName = do
  modePath <- fixtureDataFile externalProviderPluginName externalProviderStatusModeFileName
  createDirectoryIfMissing True (takeDirectory modePath)
  writeFile modePath modeName

expectProviderStatusReport :: RPCExternalDataSourceStatusReport -> Expectation
expectProviderStatusReport report =
  case redssReportStatuses report of
    [sourceEntry, grantEntry] -> do
      redsstProviderId sourceEntry `shouldBe` externalProviderPluginNameText
      redsstConsumerId sourceEntry `shouldBe` Nothing
      redsstSource sourceEntry `shouldBe` externalSourceName
      redsstGrant sourceEntry `shouldBe` Nothing
      redsstAccess sourceEntry `shouldBe` []
      redsstResources sourceEntry `shouldBe` externalSharedResources
      redsstCapabilityScope sourceEntry `shouldBe` externalCapabilities
      redssState (redsstStatus sourceEntry) `shouldBe` ExternalStatusReady
      redsstDiagnostics sourceEntry `shouldBe` Just externalDiagnostics
      redsstProviderId grantEntry `shouldBe` externalProviderPluginNameText
      redsstConsumerId grantEntry `shouldBe` Nothing
      redsstSource grantEntry `shouldBe` externalSourceName
      redsstGrant grantEntry `shouldBe` Just externalGrantName
      redsstAccess grantEntry `shouldBe` externalReadAccess
      redsstResources grantEntry `shouldBe` externalSharedResources
      redsstCapabilityScope grantEntry `shouldBe` externalCapabilities
      redssState (redsstStatus grantEntry) `shouldBe` ExternalStatusReady
      redssAvailability (redsstStatus grantEntry) `shouldBe` Just ExternalAvailabilityAvailable
      redssHealth (redsstStatus grantEntry) `shouldBe` Just ExternalHealthHealthy
      redssAccessMode (redsstStatus grantEntry) `shouldBe` Just ExternalAccessModeReadOnly
      redsstDiagnostics grantEntry `shouldBe` Just externalDiagnostics
      shouldNotMentionSQLite sourceEntry
      shouldNotMentionSQLite grantEntry
    other -> expectationFailure ("expected source and grant provider status entries, got " <> show other)

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
        redssObservedAt (redsdStatus source) `shouldSatisfy` maybe False (const True)
        redssFresh (redsdStatus source) `shouldBe` True
        case redsdGrants source of
          [grant] -> do
            redsgName grant `shouldBe` externalGrantName
            redsgAccess grant `shouldBe` externalReadAccess
            redsgCapabilities grant `shouldBe` externalCapabilities
            redsgReference grant `shouldBe` Just externalGrantReference
            redsgConfigRefs grant `shouldBe` [externalGrantConfigRef]
            redssObservedAt (redsgStatus grant) `shouldSatisfy` maybe False (const True)
            redssFresh (redsgStatus grant) `shouldBe` True
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
        redssMessage (redsrStatus ref) `shouldBe` Just "external data-source grant applied"
        expectBrokerOperationDiagnostics
          "grant_acked"
          (Just True)
          (Just True)
          (Just "applied")
          Nothing
          (redssDiagnostics (redsrStatus ref))
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
      [ref] -> expectRevokedConsumerStatus (redsrStatus ref)
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

expectRevokedConsumerStatus :: RPCExternalDataSourceStatus -> Expectation
expectRevokedConsumerStatus status = do
  redssState status `shouldBe` ExternalStatusUnavailable
  redssProviderId status `shouldBe` Just externalProviderPluginNameText
  redssAvailability status `shouldBe` Just ExternalAvailabilityUnavailable
  redssHealth status `shouldBe` Just ExternalHealthUnhealthy
  redssAccessMode status `shouldBe` Just ExternalAccessModeDisabled
  redssMessage status `shouldBe` Just "external data-source revocation applied"
  expectBrokerOperationDiagnostics
    "revoke_acked"
    (Just True)
    (Just True)
    (Just "applied")
    Nothing
    (redssDiagnostics status)
  shouldNotMentionSQLite status

expectRejectedRevokeSnapshot :: [WorldExternalDataSourceSnapshot] -> Expectation
expectRejectedRevokeSnapshot snapshots =
  case findExternalSnapshot externalConsumerPluginNameText snapshots of
    Nothing -> expectationFailure "missing consumer snapshot after rejected revoke"
    Just snapshot -> case wedssConsumedRefs snapshot of
      [ref] -> do
        redssState (redsrStatus ref) `shouldBe` ExternalStatusUnavailable
        redssMessage (redsrStatus ref) `shouldSatisfy` maybe False (Text.isInfixOf "consumer rejected revoke")
        expectBrokerOperationDiagnostics
          "revoke_failed"
          (Just False)
          (Just False)
          (Just "failed")
          (Just "consumer rejected revoke")
          (redssDiagnostics (redsrStatus ref))
      other -> expectationFailure ("expected one rejected revoke consumed ref, got " <> show other)

expectUnavailableConsumerBrokerSnapshot :: Text -> Text -> [WorldExternalDataSourceSnapshot] -> Expectation
expectUnavailableConsumerBrokerSnapshot expectedPhase expectedMessageNeedle snapshots =
  case findExternalSnapshot externalConsumerPluginNameText snapshots of
    Nothing -> expectationFailure "missing unavailable consumer snapshot"
    Just snapshot -> case wedssConsumedRefs snapshot of
      [ref] -> do
        redssState (redsrStatus ref) `shouldBe` ExternalStatusUnavailable
        redssMessage (redsrStatus ref) `shouldSatisfy` maybe False (Text.isInfixOf expectedMessageNeedle)
        redssCapabilityScope (redsrStatus ref) `shouldBe` []
        expectBrokerOperationDiagnostics expectedPhase Nothing Nothing Nothing Nothing (redssDiagnostics (redsrStatus ref))
      other -> expectationFailure ("expected one unavailable consumed ref, got " <> show other)

expectConsumerRefUnavailable :: [WorldExternalDataSourceSnapshot] -> Expectation
expectConsumerRefUnavailable snapshots =
  case findExternalSnapshot externalConsumerPluginNameText snapshots of
    Nothing -> expectationFailure "missing consumer snapshot"
    Just snapshot -> case wedssConsumedRefs snapshot of
      [ref] -> redssState (redsrStatus ref) `shouldBe` ExternalStatusUnavailable
      other -> expectationFailure ("expected one consumed ref, got " <> show other)

expectExternalDataSourceUnavailableError :: Show a => String -> Either Text a -> Expectation
expectExternalDataSourceUnavailableError label result =
  case result of
    Left err -> err `shouldSatisfy` Text.isInfixOf "external_data_source_unavailable"
    Right value -> expectationFailure (label <> " unexpectedly succeeded: " <> show value)

expectBrokerOperationDiagnostics
  :: Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Text
  -> Maybe Value
  -> Expectation
expectBrokerOperationDiagnostics expectedPhase expectedAccepted expectedApplied expectedStatus expectedError diagnostics =
  case diagnostics of
    Just (Object fields) -> do
      KM.lookup (Key.fromText "brokerPhase") fields `shouldBe` Just (String expectedPhase)
      forM_ expectedAccepted $ \accepted ->
        KM.lookup (Key.fromText "accepted") fields `shouldBe` Just (Bool accepted)
      forM_ expectedApplied $ \applied ->
        KM.lookup (Key.fromText "applied") fields `shouldBe` Just (Bool applied)
      forM_ expectedStatus $ \status ->
        KM.lookup (Key.fromText "status") fields `shouldBe` Just (String status)
      forM_ expectedError $ \err ->
        KM.lookup (Key.fromText "error") fields `shouldBe` Just (String err)
    other -> expectationFailure ("expected broker operation diagnostics object, got " <> show other)

expectConsumerDiagnosticBrokerState
  :: Set.Set Text
  -> Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Text
  -> [LoadedPlugin]
  -> Expectation
expectConsumerDiagnosticBrokerState disabled expectedPhase expectedAccepted expectedApplied expectedStatus expectedError plugins =
  case findLoadedPlugin externalConsumerPluginNameText plugins of
    Nothing -> expectationFailure "missing consumer plugin for external diagnostic check"
    Just consumer ->
      case filter ((== "consumer") . pedsRole) (pluginExternalDataSourceDiagnosticsFor disabled plugins consumer) of
        [diag] -> do
          pedsBrokerState diag `shouldBe` expectedPhase
          pedsBrokerAccepted diag `shouldBe` expectedAccepted
          pedsBrokerApplied diag `shouldBe` expectedApplied
          pedsBrokerOperationStatus diag `shouldBe` expectedStatus
          pedsBrokerOperationError diag `shouldBe` expectedError
        other -> expectationFailure ("expected one consumer external data-source diagnostic, got " <> show other)

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

expectParamValidation :: [Text] -> Either PluginParamUpdateError Value -> Expectation
expectParamValidation expectedPath result = case result of
  Left (PluginParamValidationFailed err) -> rpvPath err `shouldBe` expectedPath
  other -> expectationFailure ("expected PluginParamValidationFailed, got: " <> show other)

lookupPluginParam :: Text -> Text -> [LoadedPlugin] -> Maybe Value
lookupPluginParam pluginName paramName plugins = do
  plugin <- findLoadedPlugin pluginName plugins
  Map.lookup paramName (lpParams plugin)

findLoadedPlugin :: Text -> [LoadedPlugin] -> Maybe LoadedPlugin
findLoadedPlugin _ [] = Nothing
findLoadedPlugin pluginName (plugin:rest)
  | lpName plugin == pluginName = Just plugin
  | otherwise = findLoadedPlugin pluginName rest

validationParamSpecs :: [RPCParamSpec]
validationParamSpecs =
  [ RPCParamSpec
      { rpsName = "enabled"
      , rpsLabel = "Enabled"
      , rpsType = ParamBool
      , rpsRange = Nothing
      , rpsDefault = Bool True
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "density"
      , rpsLabel = "Density"
      , rpsType = ParamFloat
      , rpsRange = Just (Number 0, Number 1)
      , rpsDefault = Number 0.5
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "iterations"
      , rpsLabel = "Iterations"
      , rpsType = ParamInt
      , rpsRange = Just (Number 1, Number 10)
      , rpsDefault = Number 3
      , rpsTooltip = ""
      }
  ]

paramValidationPluginName :: String
paramValidationPluginName = "validation-plugin"

paramValidationManifestJSON :: Double -> Double -> BS.ByteString
paramValidationManifestJSON densityMax densityDefault = BL.toStrict $ Aeson.encode $ object
  [ "manifestVersion" .= (3 :: Int)
  , "name" .= ("validation-plugin" :: Text)
  , "version" .= ("1.0.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object
          [ "min" .= currentProtocolVersion
          , "max" .= currentProtocolVersion
          ]
      ]
  , "generator" .= object ["insertAfter" .= ("biomes" :: Text)]
  , "config" .= object ["parameters" .= refreshedSpecs]
  ]
  where
    refreshedSpecs =
      [ spec
      | spec <- validationParamSpecs
      , let specName = rpsName spec
      , specName /= "density"
      ] <>
      [ RPCParamSpec
          { rpsName = "density"
          , rpsLabel = "Density"
          , rpsType = ParamFloat
          , rpsRange = Just (Number 0, Aeson.toJSON densityMax)
          , rpsDefault = Aeson.toJSON densityDefault
          , rpsTooltip = ""
          }
      ]

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

withEnvironmentValue :: String -> String -> IO a -> IO a
withEnvironmentValue key value = bracket setup restore . const
  where
    setup = do
      old <- lookupEnv key
      setEnv key value
      pure old
    restore = maybe (unsetEnv key) (setEnv key)

data RuntimeRestartBarrier = RuntimeRestartBarrier
  { rrbMarker :: !FilePath
  , rrbRelease :: !FilePath
  , rrbComplete :: !FilePath
  }

withRuntimeRestartBarrier :: String -> (RuntimeRestartBarrier -> IO a) -> IO a
withRuntimeRestartBarrier pluginName action = do
  barrier <- RuntimeRestartBarrier
    <$> fixtureDataFile pluginName "restart-backoff.waiting"
    <*> fixtureDataFile pluginName "restart-backoff.release"
    <*> fixtureDataFile pluginName "restart-backoff.complete"
  withEnvironmentValue restartBackoffMarkerTestEnv (rrbMarker barrier) $
    withEnvironmentValue restartBackoffReleaseTestEnv (rrbRelease barrier) $
      withEnvironmentValue restartBackoffCompleteTestEnv (rrbComplete barrier) $
        action barrier

assertRestartBarrierDiagnostic :: RuntimeRestartBarrier -> Expectation
assertRestartBarrierDiagnostic barrier = do
  waiting <- Text.pack <$> readFile (rrbMarker barrier)
  completed <- Text.pack <$> readFile (rrbComplete barrier)
  forM_ [waiting, completed] $ \diagnostic -> do
    diagnostic `shouldSatisfy` Text.isInfixOf "generation=PluginRuntimeGeneration"
    diagnostic `shouldSatisfy` Text.isInfixOf "operation_token="

restartBackoffMarkerTestEnv :: String
restartBackoffMarkerTestEnv = "TOPO_TEST_PLUGIN_RESTART_BACKOFF_MARKER"

restartBackoffReleaseTestEnv :: String
restartBackoffReleaseTestEnv = "TOPO_TEST_PLUGIN_RESTART_BACKOFF_RELEASE"

restartBackoffCompleteTestEnv :: String
restartBackoffCompleteTestEnv = "TOPO_TEST_PLUGIN_RESTART_BACKOFF_COMPLETE"

withSensitiveParentEnvironment :: IO a -> IO a
withSensitiveParentEnvironment = bracket setup restore . const
  where
    setup = traverse stashAndSet sensitiveParentEnvironment
    stashAndSet (key, value) = do
      old <- lookupEnv key
      setEnv key value
      pure (key, old)
    restore = mapM_ (\(key, old) -> maybe (unsetEnv key) (setEnv key) old)

sensitiveParentEnvironment :: [(String, String)]
sensitiveParentEnvironment =
  [ ("AWS_SECRET_ACCESS_KEY", "aws-secret-access-key")
  , ("GH_TOKEN", "github-token")
  , ("DATABASE_URL", "postgres://topo:secret@example.invalid/topo")
  , ("TOPO_TEST_SECRET_TOKEN", "topo-test-secret-token")
  , ("SERVICE_TOKEN", "service-token")
  , ("APP_SECRET", "app-secret")
  , ("DB_PASSWORD", "db-password")
  , ("PRIVATE_KEY", "private-key")
  , ("LC_SECRET_TOKEN", "locale-prefixed-secret-token")
  ]

sensitiveParentEnvKeys :: [String]
sensitiveParentEnvKeys = map fst sensitiveParentEnvironment

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

withUnmanifestedExecutablePluginDir :: String -> String -> IO a -> IO a
withUnmanifestedExecutablePluginDir pluginName fixtureMode action =
  withIsolatedPluginHome pluginName $
    bracket setup teardown (const action)
  where
    setup = do
      baseDir <- currentPluginBaseDir
      let pluginDir = baseDir </> pluginName
      resetPluginDir pluginDir
      writePluginWrapper pluginDir pluginName fixtureMode
      pure pluginDir

    teardown = removePathForciblyEventually

withMissingManifestNameCollision :: String -> String -> IO a -> IO a
withMissingManifestNameCollision missingDirName manifestDirName action =
  withIsolatedPluginHome missingDirName $
    bracket setup teardown (const action)
  where
    setup = do
      baseDir <- currentPluginBaseDir
      let missingDir = baseDir </> missingDirName
          manifestDir = baseDir </> manifestDirName
      resetPluginDir missingDir
      writePluginWrapper missingDir missingDirName "counted-early-exit"
      resetPluginDir manifestDir
      BS.writeFile (manifestDir </> "manifest.json") (manifestFor missingDirName)
      pure [missingDir, manifestDir]

    teardown = mapM_ removePathForciblyEventually

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
removePathForciblyEventually path = go (100 :: Int)
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

readFixtureToken :: String -> String -> IO Text
readFixtureToken pluginName fileName = do
  path <- fixtureDataFile pluginName fileName
  Text.strip . Text.pack <$> readFile path

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

withPluginTransportServer :: Text -> (TransportServer -> IO a) -> IO a
withPluginTransportServer pluginName = bracket acquire tsClose
  where
    acquire = do
      serverResult <- openPluginServer defaultTransportConfig { tcTimeout = 1000 } pluginName
      case serverResult of
        Left err -> expectationFailure ("openPluginServer failed: " <> show err) >> fail "openPluginServer"
        Right server -> pure server

expectWindowsNamedPipePair :: FilePath -> IO (FilePath, FilePath)
expectWindowsNamedPipePair address =
  case break (== '|') address of
    (hostReadPipeName, '|':hostWritePipeName)
      | not (null hostReadPipeName) && not (null hostWritePipeName) ->
          pure (hostReadPipeName, hostWritePipeName)
    _ -> expectationFailure ("expected split named-pipe endpoint, got " <> address) >> fail "named-pipe pair"

launchSinglePipeFixtureClient :: FilePath -> IO ProcessHandle
launchSinglePipeFixtureClient pipeName = do
  testExe <- getExecutablePath
  inherited <- getEnvironment
  (_, _, _, processHandle) <- createProcess
    (proc testExe ["--plugin-manager-fixture", "split-pipe-client"])
      { env = Just (withEndpointEnv pipeName inherited)
      , std_in = NoStream
      , std_out = NoStream
      , std_err = Inherit
      }
  pure processHandle

withEndpointEnv :: FilePath -> [(String, String)] -> [(String, String)]
withEndpointEnv endpoint inherited = overrides <> filter (not . overridden . fst) inherited
  where
    overrides =
      [ (pluginEndpointEnv, endpoint)
      , (pluginEndpointKindEnv, "named-pipe")
      ]
    overridden key = any (envKeyEqualsForHost key . fst) overrides

envKeyEqualsForHost :: String -> String -> Bool
envKeyEqualsForHost left right
  | os == "mingw32" = map toLower left == map toLower right
  | otherwise = left == right

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
fixtureUsage = die "usage: topo-seer-test --plugin-manager-fixture <ok|startup-pause-probe|entry-marker|env-contract|protocol-mismatch|auth-missing|auth-mismatch|endpoint-race-parent|endpoint-race-fake|split-pipe-client|malformed-json|bad-handshake|early-exit|slow|wait-for-start-signal|handshake-stall|slow-shutdown|flaky-start|flaky-runtime-exit|flaky-runtime-exit-clean|controlled-runtime-tree-restart|counted-early-exit|hang-query|provider-failed|validation-ok|invalid-mutate|negotiated-validation|widening-handshake|exit-on-generator|exit-on-simulation|external-provider|external-provider-controlled-status|external-consumer|external-consumer-reject-grant|external-consumer-reject-revoke|external-consumer-timeout-grant-once|external-consumer-timeout-revoke-once|external-consumer-query-external-unavailable|external-consumer-crash-after-grant-ack|process-tree|heartbeat-child|host-death-host|host-death-tree|host-death-ordinary-child|host-death-escaped-child>"

runFixtureMode :: String -> IO ()
runFixtureMode = \case
  "ok" -> runOkFixture
  "startup-pause-probe" -> do
    recordStartupPauseProbePid
    _ <- forkIO runHeartbeatChild
    runOkFixture
  "entry-marker" -> incrementFixtureCount "entry-marker" >> runOkFixture
  "env-contract" -> runEnvContractFixture
  "protocol-mismatch" -> runOneShotAckFixture (currentProtocolVersion + 1)
  "auth-missing" -> runMissingAuthAckFixture
  "auth-mismatch" -> runMismatchedAuthAckFixture
  "endpoint-race-parent" -> runEndpointRaceParentFixture
  "endpoint-race-fake" -> runEndpointRaceFakeFixture
  "split-pipe-client" -> runSplitPipeClientFixture
  "malformed-json" -> runMalformedJsonFixture
  "bad-handshake" -> runBadHandshakeFixture
  "early-exit" -> exitFailure
  "slow" -> threadDelay 2000000 >> runOkFixture
  "wait-for-start-signal" -> runWaitForStartSignalFixture
  "handshake-stall" -> runHandshakeStallFixture
  "slow-shutdown" -> runSlowShutdownFixture
  "flaky-start" -> runFlakyStartFixture
  "flaky-runtime-exit" -> runFlakyRuntimeExitFixture
  "flaky-runtime-exit-clean" -> runFlakyRuntimeCleanExitFixture
  "controlled-runtime-tree-restart" -> runControlledRuntimeTreeRestartFixture
  "counted-early-exit" -> incrementFixtureCount "counted-early-exit" >> exitFailure
  "hang-query" -> runHangQueryFixture
  "gated-data" -> runGatedDataFixture
  "provider-failed" -> runProviderFailedFixture
  "validation-ok" -> runValidationOkFixture
  "invalid-mutate" -> runInvalidMutateFixture
  "negotiated-validation" -> runNegotiatedValidationFixture
  "widening-handshake" -> runWideningHandshakeFixture
  "exit-on-generator" -> runExitOnGeneratorFixture
  "exit-on-simulation" -> runExitOnSimulationFixture
  "external-provider" -> runExternalProviderFixture
  "external-provider-controlled-status" -> runExternalProviderControlledStatusFixture
  "external-consumer" -> runExternalConsumerFixture
  "external-consumer-reject-grant" -> runExternalConsumerRejectGrantFixture
  "external-consumer-reject-revoke" -> runExternalConsumerRejectRevokeFixture
  "external-consumer-timeout-grant-once" -> runExternalConsumerTimeoutGrantOnceFixture
  "external-consumer-timeout-revoke-once" -> runExternalConsumerTimeoutRevokeOnceFixture
  "external-consumer-query-external-unavailable" -> runExternalConsumerQueryExternalUnavailableFixture
  "external-consumer-crash-after-grant-ack" -> runExternalConsumerCrashAfterGrantAckFixture
  "process-tree" -> runProcessTreeFixture
  "heartbeat-child" -> runHeartbeatChild
  "host-death-host" -> runHostDeathHostFixture
  "host-death-tree" -> runHostDeathTreeFixture
  "host-death-ordinary-child" -> runHostDeathHeartbeatChild
    hostDeathOrdinaryPidFileName hostDeathOrdinaryHeartbeatFileName
  "host-death-escaped-child" -> runHostDeathEscapedChild
  unknown -> die ("unknown plugin-manager fixture: " <> unknown)

runEnvContractFixture :: IO ()
runEnvContractFixture = do
  verifyLaunchEnvironment
  runOkFixture

runWaitForStartSignalFixture :: IO ()
runWaitForStartSignalFixture = do
  dataRoot <- requireEnv pluginDataRootEnv
  waitForFixtureSignal (dataRoot </> refreshTransientStartSignalFileName)
  runOkFixture

waitForFixtureSignal :: FilePath -> IO ()
waitForFixtureSignal path = go (1000 :: Int)
  where
    go attemptsLeft = do
      exists <- doesFileExist path
      if exists
        then pure ()
        else if attemptsLeft <= 0
          then die ("timed out waiting for fixture signal " <> path)
          else threadDelay 10000 >> go (attemptsLeft - 1)

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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
                loop transport
              MsgShutdown -> threadDelay 1000000 >> closeTransport transport
              _ -> loop transport

runFlakyStartFixture :: IO ()
runFlakyStartFixture = do
  count <- incrementFixtureCount "flaky-start"
  if count <= 1
    then exitFailure
    else runOkFixture

runFlakyRuntimeExitFixture :: IO ()
runFlakyRuntimeExitFixture =
  runFlakyRuntimeExitFixtureWith "flaky-runtime-exit" exitFailure

runFlakyRuntimeCleanExitFixture :: IO ()
runFlakyRuntimeCleanExitFixture =
  runFlakyRuntimeExitFixtureWith "flaky-runtime-exit-clean" (pure ())

runFlakyRuntimeExitFixtureWith :: String -> IO () -> IO ()
runFlakyRuntimeExitFixtureWith countKey finishFirst = do
  count <- incrementFixtureCount countKey
  if count > 1
    then runOkFixture
    else do
      connectPluginFromEnvironment "plugin-manager-flaky-runtime-fixture" stdin stdout >>= \case
        Left _ -> exitFailure
        Right transport -> waitForHandshake transport
  where
    waitForHandshake transport = do
      recvMessage transport >>= \case
        Left _ -> exitFailure
        Right bytes -> case decodeMessage bytes of
          Right envelope | envType envelope == MsgHandshake -> do
            ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
            _ <- sendMessage transport (encodeMessage ack)
            threadDelay 150000
            closeTransport transport
            finishFirst
          _ -> waitForHandshake transport

runControlledRuntimeTreeRestartFixture :: IO ()
runControlledRuntimeTreeRestartFixture = do
  count <- incrementFixtureCount controlledRuntimeCountKey
  dataRoot <- requireEnv pluginDataRootEnv
  if count > 1
    then do
      oldTreeAlive <- controlledOldTreeAlive dataRoot
      writeFile
        (dataRoot </> controlledRuntimeOverlapProbeFileName)
        (if oldTreeAlive then "old-tree-alive\n" else "old-tree-gone\n")
      recordFixtureMarker controlledRuntimeReplacementStartedFileName
      waitForFixtureSignal (dataRoot </> controlledRuntimeReplacementReleaseFileName)
      runOkFixture
    else connectPluginFromEnvironment "plugin-manager-controlled-runtime-fixture" stdin stdout >>= \case
      Left _ -> exitFailure
      Right transport -> waitForHandshake dataRoot transport
  where
    waitForHandshake dataRoot transport = do
      recvMessage transport >>= \case
        Left _ -> exitFailure
        Right bytes -> case decodeMessage bytes of
          Right envelope | envType envelope == MsgHandshake -> do
            ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
            _ <- sendMessage transport (encodeMessage ack)
            startHeartbeatChild
            waitForFixtureSignal (dataRoot </> controlledRuntimeCrashReleaseFileName)
            closeTransport transport
            exitFailure
          _ -> waitForHandshake dataRoot transport

controlledOldTreeAlive :: FilePath -> IO Bool
controlledOldTreeAlive dataRoot = do
  raw <- readFile (dataRoot </> processTreeChildPidFileName)
  case reads raw of
    [(pid, _)] -> processIdExists pid
    _ -> pure True

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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
                loop transport
              MsgQueryResource -> threadDelay 2000000 >> closeTransport transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runGatedDataFixture :: IO ()
runGatedDataFixture = do
  dataRoot <- requireEnv pluginDataRootEnv
  connectPluginFromEnvironment "plugin-manager-gated-data-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop dataRoot transport
  where
    loop dataRoot transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop dataRoot transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
                loop dataRoot transport
              MsgQueryResource -> do
                _ <- forkIO $ gateDataReply
                  dataRoot gatedQueryReceivedFileName gatedQueryReleaseFileName gatedQueryRepliedFileName
                  transport
                  (queryResultEnvelope (envRequestId envelope)
                    (QueryResult "records" [record [("id", String "alpha"), ("name", String "Alpha")]] (Just 1)))
                loop dataRoot transport
              MsgMutateResource -> do
                _ <- forkIO $ gateDataReply
                  dataRoot gatedMutationReceivedFileName gatedMutationReleaseFileName gatedMutationRepliedFileName
                  transport
                  (mutateResultEnvelope (envRequestId envelope)
                    (MutateResult True Nothing (Just (record [("id", String "accepted"), ("name", String "Accepted")])) Nothing))
                loop dataRoot transport
              MsgShutdown -> closeTransport transport
              _ -> loop dataRoot transport

    gateDataReply dataRoot receivedName releaseName repliedName transport envelope = do
      let received = dataRoot </> receivedName
      alreadyReceived <- doesFileExist received
      unless alreadyReceived $ do
        writeFile received "received\n"
        waitForFixtureSignal (dataRoot </> releaseName)
      -- An uncorrelated follow-up reply is accepted only when the cancelled
      -- first request has already been removed from the pending map.
      let reply = if alreadyReceived then envelope { envRequestId = Nothing } else envelope
      _ <- sendMessage transport (encodeMessage reply)
      writeFile (dataRoot </> repliedName) "replied\n"

runHostDeathHostFixture :: IO ()
runHostDeathHostFixture = do
  readyPath <- fixtureDataFile hostDeathPluginName hostDeathReadyFileName
  createDirectoryIfMissing True (takeDirectory readyPath)
  runHost readyPath `catch` \(err :: SomeException) -> do
    writeAtomicTextFile readyPath ("error=" <> show err <> "\n")
    throwIO err
  where
    runHost readyPath = withPluginManager $ \pluginManagerHandle -> do
      discoverPlugins pluginManagerHandle
      refreshManifests pluginManagerHandle
      loaded <- getLoadedPlugins pluginManagerHandle
      unless (PluginConnected `elem` pluginStatuses hostDeathPluginName loaded) $
        fail ("host-death plugin did not connect: " <> summarizeLoadedPlugins loaded)
      leaderPid <- case mapMaybe ownedPluginProcessId (pluginOwnedProcesses hostDeathPluginName loaded) of
        [pid] -> pure pid
        pids -> fail ("host-death fixture expected one leader pid, got " <> show pids)
      ordinaryPid <- waitForPublishedFixturePid
        hostDeathPluginName hostDeathOrdinaryPidFileName
      escapedPid <- waitForPublishedFixturePid
        hostDeathPluginName hostDeathEscapedPidFileName
      ordinaryHeartbeat <- fixtureDataFile hostDeathPluginName hostDeathOrdinaryHeartbeatFileName
      escapedHeartbeat <- fixtureDataFile hostDeathPluginName hostDeathEscapedHeartbeatFileName
      expectHeartbeatAdvances ordinaryHeartbeat
      expectHeartbeatAdvances escapedHeartbeat
      hostPid <- currentProcessIdForTest
      writeAtomicTextFile readyPath $ unlines
        [ "platform=" <> os
        , "host_pid=" <> show hostPid
        , "leader_pid=" <> show leaderPid
        , "ordinary_pid=" <> show ordinaryPid
        , "escaped_pid=" <> show escapedPid
        , "containment=" <> if os == "mingw32"
            then "kill-on-close-job"
            else "launch-process-group-only;no-portable-host-death;new-session-escapes"
        ]
      waitForever

    waitForever = threadDelay 1000000 >> waitForever

runHostDeathTreeFixture :: IO ()
runHostDeathTreeFixture = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  leaderPid <- currentProcessIdForTest
  writeAtomicTextFile (dataRoot </> hostDeathLeaderPidFileName) (show leaderPid)
  connectPluginFromEnvironment "plugin-manager-host-death-tree-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport False
  where
    loop transport childrenStarted = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes -> case decodeMessage bytes of
          Left _ -> loop transport childrenStarted
          Right envelope -> case envType envelope of
            MsgHandshake -> do
              ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
              _ <- sendMessage transport (encodeMessage ack)
              unless childrenStarted startHostDeathDescendants
              loop transport True
            MsgShutdown -> closeTransport transport
            _ -> loop transport childrenStarted

startHostDeathDescendants :: IO ()
startHostDeathDescendants = do
  dataRoot <- requireEnv pluginDataRootEnv
  testExe <- getExecutablePath
  let childSpec mode = (proc testExe ["--plugin-manager-fixture", mode])
        { cwd = Just dataRoot
        , std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
  _ <- createProcess (childSpec "host-death-ordinary-child")
#if defined(mingw32_HOST_OS)
  _ <- createProcess
    ((childSpec "host-death-escaped-child") { new_session = True })
#else
  -- The child first publishes its PID while still in the retained launch group.
  -- Only then may it create the session that deliberately escapes that group.
  _ <- createProcess (childSpec "host-death-escaped-child")
  waitForFixtureSignal (dataRoot </> hostDeathPreEscapeReadyFileName)
  writeAtomicTextFile (dataRoot </> hostDeathEscapeReleaseFileName) "escape\n"
  waitForFixtureSignal (dataRoot </> hostDeathEscapeCompleteFileName)
#endif
  pure ()

runHostDeathHeartbeatChild :: String -> String -> IO ()
runHostDeathHeartbeatChild pidFileName heartbeatFileName = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  childPid <- currentProcessIdForTest
  writeAtomicTextFile (dataRoot </> pidFileName) (show childPid)
  runHostDeathHeartbeatLoop dataRoot heartbeatFileName

runHostDeathEscapedChild :: IO ()
#if defined(mingw32_HOST_OS)
runHostDeathEscapedChild = runHostDeathHeartbeatChild
  hostDeathEscapedPidFileName hostDeathEscapedHeartbeatFileName
#else
runHostDeathEscapedChild = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  childPid <- currentProcessIdForTest
  writeAtomicTextFile (dataRoot </> hostDeathEscapedPidFileName) (show childPid)
  writeAtomicTextFile (dataRoot </> hostDeathPreEscapeReadyFileName) "ready\n"
  waitForFixtureSignal (dataRoot </> hostDeathEscapeReleaseFileName)
  _ <- createSession
  writeAtomicTextFile (dataRoot </> hostDeathEscapeCompleteFileName) "escaped\n"
  runHostDeathHeartbeatLoop dataRoot hostDeathEscapedHeartbeatFileName
#endif

runHostDeathHeartbeatLoop :: FilePath -> String -> IO ()
runHostDeathHeartbeatLoop dataRoot heartbeatFileName = do
  let heartbeatPath = dataRoot </> heartbeatFileName
      loop n = do
        BS.writeFile heartbeatPath (BSC.pack (show n))
        threadDelay 50000
        loop (n + 1 :: Int)
  loop (0 :: Int)

waitForPublishedFixturePid :: String -> String -> IO Word64
waitForPublishedFixturePid pluginName fileName = do
  path <- fixtureDataFile pluginName fileName
  result <- timeout 5000000 (poll path)
  case result of
    Just pid -> pure pid
    Nothing -> fail ("fixture did not publish pid at " <> path)
  where
    poll path = do
      raw <- readTextFileMaybe path
      case raw >>= parsePublishedPid of
        Just pid -> pure pid
        Nothing -> threadDelay 25000 >> poll path
    parsePublishedPid = parseSafeFixturePid

runProcessTreeFixture :: IO ()
runProcessTreeFixture = do
  connectPluginFromEnvironment "plugin-manager-process-tree-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport False
  where
    loop transport childStarted = do
      recvMessage transport >>= \case
        Left _
          | os == "mingw32" -> threadDelay 1000000 >> loop transport childStarted
          | otherwise -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport childStarted
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
                unless childStarted startHeartbeatChild
                loop transport True
              MsgShutdown
                | os == "mingw32" -> loop transport childStarted
                | otherwise -> closeTransport transport
              _ -> loop transport childStarted

startHeartbeatChild :: IO ()
startHeartbeatChild = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  if os == "mingw32"
    then do
      testExe <- getExecutablePath
      _ <- createProcess (proc testExe ["--plugin-manager-fixture", "heartbeat-child"])
        { cwd = Just dataRoot
        , std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
      pure ()
    else do
      -- The child deliberately ignores TERM. It inherits the plugin launch
      -- group, while the fixture leader exits on Shutdown, exercising stable
      -- group identity and TERM-to-KILL escalation after leader exit.
      _ <- createProcess (proc "/bin/sh"
        [ "-c"
        , "trap '' TERM; echo $$ > \"$TOPO_PLUGIN_DATA_ROOT/" <> processTreeChildPidFileName
            <> "\"; n=0; while :; do n=$((n+1)); printf '%s' \"$n\" > \"$TOPO_PLUGIN_DATA_ROOT/"
            <> processTreeHeartbeatFileName <> "\"; sleep 0.05; done"
        ])
        { cwd = Just dataRoot
        , std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
      pure ()

recordStartupPauseProbePid :: IO ()
recordStartupPauseProbePid = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
  pid <- currentProcessIdForTest
  writeFile (dataRoot </> processTreeChildPidFileName) (show pid)

currentProcessIdForTest :: IO Word64
#if defined(mingw32_HOST_OS)
currentProcessIdForTest = fromIntegral <$> c_GetCurrentProcessId
#else
currentProcessIdForTest = fromIntegral <$> getProcessID
#endif

runHeartbeatChild :: IO ()
runHeartbeatChild = do
  dataRoot <- requireEnv pluginDataRootEnv
  createDirectoryIfMissing True dataRoot
#if defined(mingw32_HOST_OS)
  childPid <- c_GetCurrentProcessId
  writeFile (dataRoot </> processTreeChildPidFileName) (show childPid)
#endif
  let heartbeatPath = dataRoot </> processTreeHeartbeatFileName
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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
runExternalProviderFixture = runExternalProviderFixtureWith (pure "ok")

runExternalProviderControlledStatusFixture :: IO ()
runExternalProviderControlledStatusFixture = runExternalProviderFixtureWith readExternalProviderStatusMode

runExternalProviderFixtureWith :: IO Text -> IO ()
runExternalProviderFixtureWith readStatusMode = do
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
                ack <- handshakeAckWithDataDirectoryEnvelopeFor envelope currentProtocolVersion (Just "external-provider-data") []
                _ <- sendMessage transport (encodeMessage ack)
                loop transport
              MsgExternalDataSourceStatusRequest -> do
                modeName <- readStatusMode
                continue <- sendExternalProviderStatusResponse transport envelope modeName
                if continue then loop transport else pure ()
              MsgQueryResource -> do
                _ <- sendMessage transport (encodeMessage (queryResultEnvelope
                  (envRequestId envelope)
                  (QueryResult externalProviderResource [externalProviderRecord] (Just 1))))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

readExternalProviderStatusMode :: IO Text
readExternalProviderStatusMode = do
  dataRoot <- requireEnv pluginDataRootEnv
  let modePath = dataRoot </> externalProviderStatusModeFileName
  exists <- doesFileExist modePath
  if exists
    then Text.strip . Text.pack <$> readFile modePath
    else pure "ok"

sendExternalProviderStatusResponse :: Transport -> RPCEnvelope -> Text -> IO Bool
sendExternalProviderStatusResponse transport envelope modeName =
  case modeName of
    "timeout" -> threadDelay 2000000 >> pure True
    "transport" -> closeTransport transport >> pure False
    "malformed" -> do
      _ <- sendMessage transport (encodeMessage (malformedExternalStatusEnvelope (envRequestId envelope)))
      pure True
    "unexpected" -> do
      _ <- sendMessage transport (encodeMessage (queryResultEnvelope
        (envRequestId envelope)
        (QueryResult externalProviderResource [externalProviderRecord] (Just 1))))
      pure True
    "plugin-error" -> do
      _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope
        (envRequestId envelope)
        503
        "provider status unavailable"))
      pure True
    "omit-grant" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportOmittingGrant includeDiagnostics)))
      pure True
    "replacement" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderReplacementStatusReport includeDiagnostics)))
      pure True
    "grant-scope-query-only" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantScope [ExternalSourceQuery] includeDiagnostics)))
      pure True
    "grant-scope-health-only" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantScope [ExternalSourceHealth] includeDiagnostics)))
      pure True
    "grant-scope-query-migrate" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantScope [ExternalSourceQuery, ExternalSourceMigrate] includeDiagnostics)))
      pure True
    "grant-access-widen" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantAccess [ExternalAccessRead, ExternalAccessAdmin] includeDiagnostics)))
      pure True
    "grant-resources-widen" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantResources (externalSharedResources <> ["private_sources"]) includeDiagnostics)))
      pure True
    "grant-resources-inherited" -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReportWithGrantResources [] includeDiagnostics)))
      pure True
    _ -> do
      let includeDiagnostics = requestIncludesDiagnostics envelope
      _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
        (envRequestId envelope)
        (externalProviderStatusReport includeDiagnostics)))
      pure True

malformedExternalStatusEnvelope :: Maybe Word64 -> RPCEnvelope
malformedExternalStatusEnvelope requestId = RPCEnvelope
  { envType = MsgExternalDataSourceStatus
  , envPayload = object
      [ "statuses" .=
          [ object
              [ "providerId" .= externalProviderPluginNameText
              , "source" .= externalSourceName
              ]
          ]
      ]
  , envRequestId = requestId
  }

runExternalConsumerFixture :: IO ()
runExternalConsumerFixture = runExternalConsumerFixtureWith False False False False False False

runExternalConsumerRejectGrantFixture :: IO ()
runExternalConsumerRejectGrantFixture = runExternalConsumerFixtureWith True False False False False False

runExternalConsumerRejectRevokeFixture :: IO ()
runExternalConsumerRejectRevokeFixture = runExternalConsumerFixtureWith False True False False False False

runExternalConsumerTimeoutGrantOnceFixture :: IO ()
runExternalConsumerTimeoutGrantOnceFixture = runExternalConsumerFixtureWith False False True False False False

runExternalConsumerTimeoutRevokeOnceFixture :: IO ()
runExternalConsumerTimeoutRevokeOnceFixture = runExternalConsumerFixtureWith False False False True False False

runExternalConsumerQueryExternalUnavailableFixture :: IO ()
runExternalConsumerQueryExternalUnavailableFixture = runExternalConsumerFixtureWith False False False False False True

runExternalConsumerCrashAfterGrantAckFixture :: IO ()
runExternalConsumerCrashAfterGrantAckFixture = runExternalConsumerFixtureWith False False False False True False

runExternalConsumerFixtureWith :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
runExternalConsumerFixtureWith rejectGrant rejectRevoke timeoutGrantOnce timeoutRevokeOnce crashAfterGrantAck queryExternalUnavailable = do
  recordExternalStartup externalConsumerPluginName
  connectPluginFromEnvironment "plugin-manager-external-consumer-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport "declared"
  where
    shouldTimeoutExternalGrant
      | timeoutGrantOnce = (<= 1) <$> incrementFixtureCount "external-grant-timeout"
      | otherwise = pure False

    shouldTimeoutExternalRevoke
      | timeoutRevokeOnce = (<= 1) <$> incrementFixtureCount "external-revoke-timeout"
      | otherwise = pure False

    loop transport bindingStatus = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport bindingStatus
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                ack <- handshakeAckWithDataDirectoryEnvelopeFor envelope currentProtocolVersion (Just "external-consumer-data") []
                _ <- sendMessage transport (encodeMessage ack)
                loop transport bindingStatus
              MsgExternalDataSourceStatusRequest -> do
                let includeDiagnostics = requestIncludesDiagnostics envelope
                _ <- sendMessage transport (encodeMessage (externalStatusEnvelope
                  (envRequestId envelope)
                  (externalConsumerStatusReport includeDiagnostics)))
                loop transport bindingStatus
              MsgExternalDataSourceGrant ->
                case Aeson.fromJSON (envPayload envelope) of
                  Aeson.Error _ -> loop transport bindingStatus
                  Aeson.Success grant -> do
                    recordExternalConsumerOperation "grant" (redsgmOperationId grant)
                    recordExternalConsumerGrantScope grant
                    timedOut <- shouldTimeoutExternalGrant
                    if timedOut
                      then threadDelay 1500000 >> loop transport bindingStatus
                      else do
                        sendExternalGrantResult transport envelope grant (not rejectGrant)
                        if crashAfterGrantAck
                          then exitFailure
                          else loop transport (if rejectGrant then bindingStatus else "granted")
              MsgExternalDataSourceRevoke ->
                case Aeson.fromJSON (envPayload envelope) of
                  Aeson.Error _ -> loop transport bindingStatus
                  Aeson.Success revocation -> do
                    recordExternalConsumerOperation "revoke" (redsrvOperationId revocation)
                    timedOut <- shouldTimeoutExternalRevoke
                    if timedOut
                      then threadDelay 1500000 >> loop transport bindingStatus
                      else do
                        sendExternalRevokeResult transport envelope revocation (not rejectRevoke)
                        loop transport (if rejectRevoke then bindingStatus else "revoked")
              MsgQueryResource -> do
                _ <- if queryExternalUnavailable
                  then sendMessage transport (encodeMessage (pluginErrorEnvelope
                    (envRequestId envelope)
                    (dataResourceErrorRPCCode ExternalDataSourceUnavailable)
                    "external data-source unavailable during fixture query"))
                  else sendMessage transport (encodeMessage (queryResultEnvelope
                    (envRequestId envelope)
                    (QueryResult externalBindingResource [externalBindingRecord bindingStatus] (Just 1))))
                loop transport bindingStatus
              MsgShutdown -> closeTransport transport
              _ -> loop transport bindingStatus

sendExternalGrantResult :: Transport -> RPCEnvelope -> RPCExternalDataSourceGrantMessage -> Bool -> IO ()
sendExternalGrantResult transport envelope grant accepted =
  sendExternalOperationResult transport envelope RPCExternalDataSourceOperationResult
    { redsoOperationId = fromMaybe "missing-grant-operation-id" (redsgmOperationId grant)
    , redsoOperationEpoch = redsgmOperationEpoch grant
    , redsoOperation = ExternalDataSourceGrantOperation
    , redsoProviderId = redsgmProviderId grant
    , redsoConsumerId = fromMaybe externalConsumerPluginNameText (redsgmConsumerId grant)
    , redsoSource = redsgmSource grant
    , redsoGrant = redsgmGrant grant
    , redsoAccepted = accepted
    , redsoApplied = accepted
    , redsoStatus = if accepted then "applied" else "failed"
    , redsoMessage = if accepted then Just "external data-source grant applied" else Nothing
    , redsoError = if accepted then Nothing else Just "consumer rejected grant"
    , redsoDiagnostics = Nothing
    }

sendExternalRevokeResult :: Transport -> RPCEnvelope -> RPCExternalDataSourceGrantRevocation -> Bool -> IO ()
sendExternalRevokeResult transport envelope revocation accepted =
  sendExternalOperationResult transport envelope RPCExternalDataSourceOperationResult
    { redsoOperationId = fromMaybe "missing-revoke-operation-id" (redsrvOperationId revocation)
    , redsoOperationEpoch = redsrvOperationEpoch revocation
    , redsoOperation = ExternalDataSourceRevokeOperation
    , redsoProviderId = redsrvProviderId revocation
    , redsoConsumerId = fromMaybe externalConsumerPluginNameText (redsrvConsumerId revocation)
    , redsoSource = redsrvSource revocation
    , redsoGrant = redsrvGrant revocation
    , redsoAccepted = accepted
    , redsoApplied = accepted
    , redsoStatus = if accepted then "applied" else "failed"
    , redsoMessage = if accepted then Just "external data-source revocation applied" else Nothing
    , redsoError = if accepted then Nothing else Just "consumer rejected revoke"
    , redsoDiagnostics = Nothing
    }

sendExternalOperationResult :: Transport -> RPCEnvelope -> RPCExternalDataSourceOperationResult -> IO ()
sendExternalOperationResult transport envelope operationResult = do
  _ <- sendMessage transport (encodeMessage RPCEnvelope
    { envType = MsgExternalDataSourceOperationResult
    , envPayload = Aeson.toJSON operationResult
    , envRequestId = envRequestId envelope
    })
  pure ()

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
  { redssReportStatuses =
      [ externalProviderSourceStatusEntry includeDiagnostics
      , externalProviderGrantStatusEntry includeDiagnostics
      ]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderStatusReportOmittingGrant :: Bool -> RPCExternalDataSourceStatusReport
externalProviderStatusReportOmittingGrant includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses = [externalProviderSourceStatusEntry includeDiagnostics]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderReplacementStatusReport :: Bool -> RPCExternalDataSourceStatusReport
externalProviderReplacementStatusReport includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses =
      [ (externalProviderSourceStatusEntry includeDiagnostics)
          { redsstReference = Just (object ["handle" .= ("fixture://provider/terrain.catalog.replacement" :: Text)])
          }
      , (externalProviderGrantStatusEntry includeDiagnostics)
          { redsstReference = Just (object ["grant" .= ("terrain-catalog-read-replacement" :: Text)])
          }
      ]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderStatusReportWithGrantScope :: [RPCExternalDataSourceCapability] -> Bool -> RPCExternalDataSourceStatusReport
externalProviderStatusReportWithGrantScope scope includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses =
      [ externalProviderSourceStatusEntry includeDiagnostics
      , (externalProviderGrantStatusEntry includeDiagnostics)
          { redsstCapabilityScope = scope
          , redsstStatus = (statusWithOptionalDiagnostics includeDiagnostics) { redssCapabilityScope = scope }
          }
      ]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderStatusReportWithGrantAccess :: [RPCExternalDataSourceAccess] -> Bool -> RPCExternalDataSourceStatusReport
externalProviderStatusReportWithGrantAccess access includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses =
      [ externalProviderSourceStatusEntry includeDiagnostics
      , (externalProviderGrantStatusEntry includeDiagnostics) { redsstAccess = access }
      ]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderStatusReportWithGrantResources :: [Text] -> Bool -> RPCExternalDataSourceStatusReport
externalProviderStatusReportWithGrantResources resources includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses =
      [ externalProviderSourceStatusEntry includeDiagnostics
      , (externalProviderGrantStatusEntry includeDiagnostics) { redsstResources = resources }
      ]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalConsumerStatusReport :: Bool -> RPCExternalDataSourceStatusReport
externalConsumerStatusReport includeDiagnostics = RPCExternalDataSourceStatusReport
  { redssReportStatuses = [externalConsumerStatusEntry includeDiagnostics]
  , redssReportDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderSourceStatusEntry :: Bool -> RPCExternalDataSourceStatusEntry
externalProviderSourceStatusEntry includeDiagnostics = RPCExternalDataSourceStatusEntry
  { redsstProviderId = externalProviderPluginNameText
  , redsstConsumerId = Nothing
  , redsstSource = externalSourceName
  , redsstGrant = Nothing
  , redsstAccess = []
  , redsstResources = externalSharedResources
  , redsstCapabilityScope = externalCapabilities
  , redsstStatus = statusWithOptionalDiagnostics includeDiagnostics
  , redsstReference = Just externalSourceReference
  , redsstConfigRefs = [externalProviderConfigRef]
  , redsstDiagnostics = if includeDiagnostics then Just externalDiagnostics else Nothing
  }

externalProviderGrantStatusEntry :: Bool -> RPCExternalDataSourceStatusEntry
externalProviderGrantStatusEntry includeDiagnostics = RPCExternalDataSourceStatusEntry
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
externalConsumerStatusEntry includeDiagnostics = (externalProviderGrantStatusEntry includeDiagnostics)
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
  baseDir <- launchedFixturePluginBaseDir
  appendFile (baseDir </> "external-startup.log") (pluginName <> "\n")

readExternalStartupOrder :: IO [String]
readExternalStartupOrder = do
  baseDir <- currentPluginBaseDir
  let path = baseDir </> "external-startup.log"
  exists <- doesFileExist path
  if exists
    then lines <$> readFile path
    else pure []

recordExternalConsumerOperation :: Text -> Maybe Text -> IO ()
recordExternalConsumerOperation operation operationId = do
  baseDir <- launchedFixturePluginBaseDir
  appendFile (baseDir </> externalConsumerOperationLogFileName) $
    Text.unpack operation <> ":" <> maybe "" Text.unpack operationId <> "\n"

recordExternalConsumerGrantScope :: RPCExternalDataSourceGrantMessage -> IO ()
recordExternalConsumerGrantScope grant = do
  baseDir <- launchedFixturePluginBaseDir
  appendFile (baseDir </> externalConsumerGrantScopeLogFileName) $
    Text.unpack (externalGrantScopeLogLine grant) <> "\n"

launchedFixturePluginBaseDir :: IO FilePath
launchedFixturePluginBaseDir = do
  dataRoot <- requireEnv pluginDataRootEnv
  pure (takeDirectory (takeDirectory dataRoot))

readExternalConsumerOperationLog :: IO [String]
readExternalConsumerOperationLog = do
  baseDir <- currentPluginBaseDir
  let path = baseDir </> externalConsumerOperationLogFileName
  exists <- doesFileExist path
  if exists
    then lines <$> readFile path
    else pure []

readExternalConsumerGrantScopeLog :: IO [String]
readExternalConsumerGrantScopeLog = do
  baseDir <- currentPluginBaseDir
  let path = baseDir </> externalConsumerGrantScopeLogFileName
  exists <- doesFileExist path
  if exists
    then lines <$> readFile path
    else pure []

waitForExternalConsumerOperationLogLength :: Int -> IO [String]
waitForExternalConsumerOperationLogLength expected = go (200 :: Int)
  where
    go attemptsLeft = do
      operations <- readExternalConsumerOperationLog
      if length operations >= expected || attemptsLeft <= 0
        then pure operations
        else threadDelay 10000 >> go (attemptsLeft - 1)

externalBrokerOperationLogLine :: Text -> String
externalBrokerOperationLogLine operation =
  Text.unpack operation <> ":" <> Text.unpack (externalBrokerOperationId operation)

externalBrokerOperationId :: Text -> Text
externalBrokerOperationId operation = Text.intercalate ":"
  [ "external-data-source"
  , operation
  , externalConsumerPluginNameText
  , externalSourceName
  , externalProviderPluginNameText
  , externalSourceName
  , externalGrantName
  ]

externalGrantScopeLogLine :: RPCExternalDataSourceGrantMessage -> Text
externalGrantScopeLogLine grant = Text.intercalate ";"
  [ "top=" <> externalCapabilityScopeLogText (redsgmCapabilityScope grant)
  , "status=" <> externalCapabilityScopeLogText (redssCapabilityScope (redsgmStatus grant))
  ]

externalCapabilityScopeLogText :: [RPCExternalDataSourceCapability] -> Text
externalCapabilityScopeLogText [] = "none"
externalCapabilityScopeLogText capabilities = Text.intercalate "," (map externalCapabilityLogText capabilities)

externalCapabilityLogText :: RPCExternalDataSourceCapability -> Text
externalCapabilityLogText ExternalSourceQuery = "query"
externalCapabilityLogText ExternalSourceMutate = "mutate"
externalCapabilityLogText ExternalSourceSubscribe = "subscribe"
externalCapabilityLogText ExternalSourceMigrate = "migrate"
externalCapabilityLogText ExternalSourceHealth = "health"

externalConsumerOperationLogFileName :: String
externalConsumerOperationLogFileName = "external-consumer-operations.log"

externalConsumerGrantScopeLogFileName :: String
externalConsumerGrantScopeLogFileName = "external-consumer-grant-scopes.log"

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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
                ack <- handshakeAckWithResourcesEnvelopeFor envelope currentProtocolVersion [negotiatedValidationSchema]
                _ <- sendMessage transport (encodeMessage ack)
                loop transport
              MsgMutateResource -> do
                _ <- sendMessage transport (encodeMessage (mutateResultEnvelope
                  (envRequestId envelope)
                  (MutateResult True Nothing (Just (record [("id", String "alpha"), ("name", String "Alpha")])) Nothing)))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runWideningHandshakeFixture :: IO ()
runWideningHandshakeFixture = do
  connectPluginFromEnvironment "plugin-manager-widening-handshake-fixture" stdin stdout >>= \case
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
                ack <- handshakeAckWithResourcesEnvelopeFor envelope currentProtocolVersion [negotiatedValidationSchema]
                _ <- sendMessage transport (encodeMessage ack)
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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
                ack <- handshakeAckEnvelopeFor envelope currentProtocolVersion
                _ <- sendMessage transport (encodeMessage ack)
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
          ack <- case decodeMessage bytes of
            Left _ -> pure (handshakeAckEnvelope Nothing protocolVersion)
            Right envelope -> handshakeAckEnvelopeFor envelope protocolVersion
          _ <- sendMessage transport (encodeMessage ack)
          closeTransport transport

runMissingAuthAckFixture :: IO ()
runMissingAuthAckFixture = do
  recordLaunchAuthTokenProbe authMissingTokenFileName
  runMissingAuthAckClient "plugin-manager-auth-missing-fixture" True

runMismatchedAuthAckFixture :: IO ()
runMismatchedAuthAckFixture = do
  recordLaunchAuthTokenProbe authMismatchTokenFileName
  connectPluginFromEnvironment "plugin-manager-auth-mismatch-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes -> do
          let requestId = either (const Nothing) envRequestId (decodeMessage bytes)
          sessionId <- Text.pack <$> requireEnv pluginSessionEnv
          authToken <- Text.pack <$> requireEnv pluginAuthTokenEnv
          let ack = handshakeAckWithDataDirectoryAndAuthEnvelope
                requestId
                currentProtocolVersion
                Nothing
                []
                (Just (sessionId, authToken))
          _ <- sendMessage transport (encodeMessage ack)
          threadDelay 2000000
          closeTransport transport

runEndpointRaceParentFixture :: IO ()
runEndpointRaceParentFixture = do
  launchEndpointRaceFakeClient
  waitForever

runEndpointRaceFakeFixture :: IO ()
runEndpointRaceFakeFixture = do
  recordLaunchAuthTokenProbe endpointRaceTokenFileName
  runMissingAuthAckClientWith "plugin-manager-endpoint-race-fake" False (Just endpointRaceConnectedFileName)

runMissingAuthAckClient :: Text -> Bool -> IO ()
runMissingAuthAckClient clientName lingerAfterAck =
  runMissingAuthAckClientWith clientName lingerAfterAck Nothing

runMissingAuthAckClientWith :: Text -> Bool -> Maybe String -> IO ()
runMissingAuthAckClientWith clientName lingerAfterAck mConnectedMarker = do
  connectPluginFromEnvironment clientName stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      maybe (pure ()) recordFixtureMarker mConnectedMarker
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes -> do
          let requestId = either (const Nothing) envRequestId (decodeMessage bytes)
          _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope requestId currentProtocolVersion))
          if lingerAfterAck then threadDelay 2000000 else pure ()
          closeTransport transport

launchEndpointRaceFakeClient :: IO ()
launchEndpointRaceFakeClient = do
  testExe <- getExecutablePath
  _ <- createProcess (proc testExe ["--plugin-manager-fixture", "endpoint-race-fake"])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = Inherit
    }
  pure ()

runSplitPipeClientFixture :: IO ()
runSplitPipeClientFixture = do
  connectPluginFromEnvironment "plugin-manager-split-pipe-client" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      received <- recvMessage transport
      closeTransport transport
      case received of
        Left _ -> pure ()
        Right _ -> exitFailure

recordLaunchAuthTokenProbe :: String -> IO ()
recordLaunchAuthTokenProbe fileName = do
  mDataRoot <- lookupEnv pluginDataRootEnv
  mAuthToken <- lookupEnv pluginAuthTokenEnv
  case (mDataRoot, mAuthToken) of
    (Just dataRoot, Just authToken) -> do
      createDirectoryIfMissing True dataRoot
      writeFile (dataRoot </> fileName) authToken
    _ -> pure ()

recordFixtureMarker :: String -> IO ()
recordFixtureMarker fileName = do
  mDataRoot <- lookupEnv pluginDataRootEnv
  case mDataRoot of
    Just dataRoot -> do
      createDirectoryIfMissing True dataRoot
      writeFile (dataRoot </> fileName) "connected\n"
    Nothing -> pure ()

waitForever :: IO ()
waitForever = threadDelay 1000000 >> waitForever

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
  maxFrameSize <- requireEnv pluginMaxFrameSizeEnv
  stdioCompat <- lookupEnv pluginStdioCompatibilityEnv
  pluginId `shouldEqualOrDie` envContractPluginName
  protocol `shouldEqualOrDie` show currentProtocolVersion
  endpoint `shouldNotBeEmptyOrDie` pluginEndpointEnv
  endpointKind `shouldEqualOrDie` expectedEndpointKind
  session `shouldNotBeEmptyOrDie` pluginSessionEnv
  authToken `shouldNotBeEmptyOrDie` pluginAuthTokenEnv
  worldId `shouldEqualOrDie` "unsaved"
  dataRoot `shouldNotBeEmptyOrDie` pluginDataRootEnv
  maxFrameSize `shouldEqualOrDie` show (64 * 1024 * 1024 :: Int)
  stdioCompat `shouldBeUnsetOrDie` pluginStdioCompatibilityEnv
  mapM_ verifyParentSecretUnset sensitiveParentEnvKeys
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

verifyParentSecretUnset :: String -> IO ()
verifyParentSecretUnset key = do
  value <- lookupEnv key
  case value of
    Nothing -> pure ()
    Just _ -> die (key <> " leaked into production plugin launch environment")

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

runHandshakeStallFixture :: IO ()
runHandshakeStallFixture = do
  connectPluginFromEnvironment "plugin-manager-handshake-stall-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      threadDelay 2000000
      closeTransport transport

handshakeAckEnvelope :: Maybe Word64 -> Int -> RPCEnvelope
handshakeAckEnvelope requestId protocolVersion =
  handshakeAckWithDataDirectoryAndAuthEnvelope requestId protocolVersion Nothing [] Nothing

handshakeAckEnvelopeFor :: RPCEnvelope -> Int -> IO RPCEnvelope
handshakeAckEnvelopeFor envelope protocolVersion =
  handshakeAckWithDataDirectoryEnvelopeFor envelope protocolVersion Nothing []

handshakeAckWithResourcesEnvelopeFor :: RPCEnvelope -> Int -> [DataResourceSchema] -> IO RPCEnvelope
handshakeAckWithResourcesEnvelopeFor envelope protocolVersion =
  handshakeAckWithDataDirectoryEnvelopeFor envelope protocolVersion Nothing

handshakeAckWithDataDirectoryEnvelopeFor :: RPCEnvelope -> Int -> Maybe Text -> [DataResourceSchema] -> IO RPCEnvelope
handshakeAckWithDataDirectoryEnvelopeFor envelope protocolVersion dataDirectory resources = do
  mAuth <- handshakeAuthFromEnvelope envelope
  pure (handshakeAckWithDataDirectoryAndAuthEnvelope
    (envRequestId envelope)
    protocolVersion
    dataDirectory
    resources
    mAuth)

handshakeAckWithDataDirectoryAndAuthEnvelope :: Maybe Word64 -> Int -> Maybe Text -> [DataResourceSchema] -> Maybe (Text, Text) -> RPCEnvelope
handshakeAckWithDataDirectoryAndAuthEnvelope requestId protocolVersion dataDirectory resources mAuth = RPCEnvelope
  { envType = MsgHandshakeAck
  , envPayload = Aeson.toJSON (HandshakeAck
      { haProtocolVersion = protocolVersion
      , haDataDirectory = dataDirectory
      , haResources = resources
      , haSessionId = fst <$> mAuth
      , haAuthProof = snd <$> mAuth
      })
  , envRequestId = requestId
  }

handshakeAuthFromEnvelope :: RPCEnvelope -> IO (Maybe (Text, Text))
handshakeAuthFromEnvelope envelope = case Aeson.fromJSON (envPayload envelope) of
  Aeson.Error _ -> pure Nothing
  Aeson.Success (hs :: Handshake) -> case hsAuthChallenge hs of
    Nothing -> pure Nothing
    Just challenge -> do
      sessionId <- Text.pack <$> requireEnv pluginSessionEnv
      authToken <- Text.pack <$> requireEnv pluginAuthTokenEnv
      pure (Just (sessionId, handshakeAuthProof sessionId authToken challenge))

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

runtimeProtocolLine :: String
runtimeProtocolLine =
  "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion
    <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"

testPluginName :: String
testPluginName = "copilot-test-plugin-manager-schema"

testManifestJSON :: BS.ByteString
testManifestJSON =
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"copilot-test-plugin-manager-schema\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> BSC.pack runtimeProtocolLine
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

startupPauseTestEnv :: String
startupPauseTestEnv = "TOPO_TEST_PLUGIN_STARTUP_PAUSE"

startupPauseMarkerTestEnv :: String
startupPauseMarkerTestEnv = "TOPO_TEST_PLUGIN_STARTUP_PAUSE_MARKER"

testLaunchPluginName :: String
testLaunchPluginName = "copilot-test-plugin-launch"

testLaunchManifestJSON :: BS.ByteString
testLaunchManifestJSON = manifestFor testLaunchPluginName

startupHandoffManifestJSON :: BS.ByteString
startupHandoffManifestJSON = manifestWithStartPolicyFor testLaunchPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

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

endpointRacePluginName :: String
endpointRacePluginName = "copilot-test-plugin-endpoint-race"

endpointRaceManifestJSON :: BS.ByteString
endpointRaceManifestJSON = manifestWithStartPolicyFor endpointRacePluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

endpointRaceTokenFileName :: String
endpointRaceTokenFileName = "endpoint-race-auth-token.txt"

endpointRaceConnectedFileName :: String
endpointRaceConnectedFileName = "endpoint-race-client-connected.txt"

authMissingPluginName :: String
authMissingPluginName = "copilot-test-plugin-auth-missing"

authMissingManifestJSON :: BS.ByteString
authMissingManifestJSON = manifestWithStartPolicyFor authMissingPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

authMissingTokenFileName :: String
authMissingTokenFileName = "auth-missing-token.txt"

authMismatchPluginName :: String
authMismatchPluginName = "copilot-test-plugin-auth-mismatch"

authMismatchManifestJSON :: BS.ByteString
authMismatchManifestJSON = manifestWithStartPolicyFor authMismatchPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

authMismatchTokenFileName :: String
authMismatchTokenFileName = "auth-mismatch-token.txt"

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
refreshTransientManifestJSON = manifestWithStartPolicyFor refreshTransientPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 8000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

refreshTransientStartSignalFileName :: String
refreshTransientStartSignalFileName = "refresh-start.release"

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

endpointAcceptTimeoutPluginName :: String
endpointAcceptTimeoutPluginName = "copilot-test-plugin-endpoint-accept-timeout"

endpointAcceptTimeoutManifestJSON :: BS.ByteString
endpointAcceptTimeoutManifestJSON = manifestWithStartPolicyFor endpointAcceptTimeoutPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 100,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

handshakeStallPluginName :: String
handshakeStallPluginName = "copilot-test-plugin-handshake-stall"

handshakeStallManifestJSON :: BS.ByteString
handshakeStallManifestJSON = manifestWithStartPolicyFor handshakeStallPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 500,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

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

runtimeRestartPluginName :: String
runtimeRestartPluginName = "copilot-test-plugin-runtime-restart"

runtimeRestartManifestJSON :: BS.ByteString
runtimeRestartManifestJSON = manifestWithStartPolicyFor runtimeRestartPluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 1,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 500"
  ]

runtimeNeverPluginName :: String
runtimeNeverPluginName = "copilot-test-plugin-runtime-never"

runtimeNeverManifestJSON :: BS.ByteString
runtimeNeverManifestJSON = manifestWithStartPolicyFor runtimeNeverPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"max_restarts\": 3,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 10"
  ]

runtimeAlwaysPluginName :: String
runtimeAlwaysPluginName = "copilot-test-plugin-runtime-always"

runtimeAlwaysManifestJSON :: BS.ByteString
runtimeAlwaysManifestJSON = manifestWithStartPolicyFor runtimeAlwaysPluginName
  [ "    \"restart_mode\": \"always\","
  , "    \"max_restarts\": 1,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300,"
  , "    \"backoff_ms\": 10"
  ]

controlledRuntimePluginName :: String
controlledRuntimePluginName = "copilot-test-plugin-controlled-runtime-tree"

controlledRuntimeManifestJSON :: BS.ByteString
controlledRuntimeManifestJSON = manifestWithStartPolicyFor controlledRuntimePluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 1,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 10000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 100,"
  , "    \"backoff_ms\": 10"
  ]

controlledRuntimeCountKey :: String
controlledRuntimeCountKey = "controlled-runtime-tree-restart"

controlledRuntimeCrashReleaseFileName :: String
controlledRuntimeCrashReleaseFileName = "controlled-runtime-crash.release"

controlledRuntimeReplacementStartedFileName :: String
controlledRuntimeReplacementStartedFileName = "controlled-runtime-replacement.started"

controlledRuntimeOverlapProbeFileName :: String
controlledRuntimeOverlapProbeFileName = "controlled-runtime-overlap.probe"

controlledRuntimeReplacementReleaseFileName :: String
controlledRuntimeReplacementReleaseFileName = "controlled-runtime-replacement.release"

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

restartLimitMultiplePluginName :: String
restartLimitMultiplePluginName = "copilot-test-plugin-restart-limit-multiple"

restartLimitMultipleManifestJSON :: BS.ByteString
restartLimitMultipleManifestJSON = manifestWithStartPolicyFor restartLimitMultiplePluginName
  [ "    \"restart_mode\": \"on_failure\","
  , "    \"max_restarts\": 2,"
  , "    \"restart_window_ms\": 10000,"
  , "    \"startup_timeout_ms\": 50,"
  , "    \"request_timeout_ms\": 100,"
  , "    \"shutdown_timeout_ms\": 100,"
  , "    \"backoff_ms\": 1"
  ]

gatedDataPluginName :: String
gatedDataPluginName = "copilot-test-plugin-gated-data"

gatedDataManifestJSON :: BS.ByteString
gatedDataManifestJSON = writableDataResourceManifestFor gatedDataPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 5000,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

gatedQueryReceivedFileName, gatedQueryReleaseFileName, gatedQueryRepliedFileName :: String
gatedQueryReceivedFileName = "gated-query.received"
gatedQueryReleaseFileName = "gated-query.release"
gatedQueryRepliedFileName = "gated-query.replied"

gatedMutationReceivedFileName, gatedMutationReleaseFileName, gatedMutationRepliedFileName :: String
gatedMutationReceivedFileName = "gated-mutation.received"
gatedMutationReleaseFileName = "gated-mutation.release"
gatedMutationRepliedFileName = "gated-mutation.replied"

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

processTreePluginName :: String
processTreePluginName = "copilot-test-plugin-process-tree"

processTreeManifestJSON :: BS.ByteString
processTreeManifestJSON = manifestWithStartPolicyFor processTreePluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

processTreeHeartbeatFileName :: String
processTreeHeartbeatFileName = "child-heartbeat.txt"

processTreeChildPidFileName :: String
processTreeChildPidFileName = "child.pid"

hostDeathPluginName :: String
hostDeathPluginName = "copilot-test-plugin-host-death"

hostDeathManifestJSON :: BS.ByteString
hostDeathManifestJSON = manifestWithStartPolicyFor hostDeathPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 2000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 100"
  ]

hostDeathReadyFileName :: String
hostDeathReadyFileName = "host-death-ready.txt"

hostDeathLeaderPidFileName :: String
hostDeathLeaderPidFileName = "host-death-leader.pid"

hostDeathOrdinaryPidFileName :: String
hostDeathOrdinaryPidFileName = "host-death-ordinary.pid"

hostDeathEscapedPidFileName :: String
hostDeathEscapedPidFileName = "host-death-escaped.pid"

hostDeathOrdinaryHeartbeatFileName :: String
hostDeathOrdinaryHeartbeatFileName = "host-death-ordinary-heartbeat.txt"

hostDeathEscapedHeartbeatFileName :: String
hostDeathEscapedHeartbeatFileName = "host-death-escaped-heartbeat.txt"

hostDeathPreEscapeReadyFileName :: String
hostDeathPreEscapeReadyFileName = "host-death-pre-escape.ready"

hostDeathEscapeReleaseFileName :: String
hostDeathEscapeReleaseFileName = "host-death-escape.release"

hostDeathEscapeCompleteFileName :: String
hostDeathEscapeCompleteFileName = "host-death-escape.complete"

noDataReadQueryPluginName :: String
noDataReadQueryPluginName = "copilot-test-plugin-no-data-read-query"

noDataReadQueryManifestJSON :: BS.ByteString
noDataReadQueryManifestJSON = manifestWithStartPolicyFor noDataReadQueryPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

noDataWriteMutationPluginName :: String
noDataWriteMutationPluginName = "copilot-test-plugin-no-data-write-mutation"

noDataWriteMutationManifestJSON :: BS.ByteString
noDataWriteMutationManifestJSON = dataResourceManifestFor noDataWriteMutationPluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

unsupportedMutationPluginName :: String
unsupportedMutationPluginName = "copilot-test-plugin-unsupported-mutation"

unsupportedMutationManifestJSON :: BS.ByteString
unsupportedMutationManifestJSON = readWriteDataResourceManifestFor unsupportedMutationPluginName
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

wideningHandshakePluginName :: String
wideningHandshakePluginName = "copilot-test-plugin-widening-handshake"

wideningHandshakeManifestJSON :: BS.ByteString
wideningHandshakeManifestJSON = dataResourceManifestFor wideningHandshakePluginName
  [ "    \"restart_mode\": \"never\","
  , "    \"startup_timeout_ms\": 1000,"
  , "    \"request_timeout_ms\": 300,"
  , "    \"shutdown_timeout_ms\": 300"
  ]

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
    <> runtimeProtocolLine
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"overlay\": { \"schemaFile\": \"missing.toposchema\" }\n"
    <> "}\n"

invalidExternalSourceManifestFor :: String -> BS.ByteString
invalidExternalSourceManifestFor name = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> runtimeProtocolLine
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
blockedExternalRefManifestFor name = externalRefManifestFor name True (externalUnavailableStatusJSON "external-ledger")

optionalExternalRefManifestFor :: String -> BS.ByteString
optionalExternalRefManifestFor name = externalRefManifestFor name False (externalUnavailableStatusJSON "external-ledger")

optionalDegradedExternalRefManifestFor :: String -> BS.ByteString
optionalDegradedExternalRefManifestFor name = externalRefManifestFor name False (externalDegradedStatusJSON "external-ledger")

externalRefManifestFor :: String -> Bool -> String -> BS.ByteString
externalRefManifestFor name required statusJSON = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> runtimeProtocolLine
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"externalDataSourceRefs\": [\n"
    <> "    {\n"
    <> "      \"name\": \"settlements\",\n"
    <> "      \"provider\": \"external-ledger\",\n"
    <> "      \"source\": \"settlements\",\n"
    <> "      \"required\": " <> (if required then "true" else "false") <> ",\n"
    <> "      \"access\": [\"read\"],\n"
    <> "      \"grant\": \"settlement-read\",\n"
    <> "      \"status\": " <> statusJSON <> "\n"
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
    <> runtimeProtocolLine
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

externalProviderStatusModeFileName :: String
externalProviderStatusModeFileName = "external-status-mode.txt"

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
  { redsgmOperationId = Just "seer-plugin-manager-grant-op"
  , redsgmOperationEpoch = Just 1
  , redsgmProviderId = externalProviderPluginNameText
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
  { redsrvOperationId = Just "seer-plugin-manager-revoke-op"
  , redsrvOperationEpoch = Just 2
  , redsrvProviderId = externalProviderPluginNameText
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

externalProviderDegradedManifestJSON :: BS.ByteString
externalProviderDegradedManifestJSON =
  externalProviderManifestWithStatusesFor
    externalProviderPluginName
    (externalDegradedStatusJSON externalProviderPluginName)
    (externalDegradedStatusJSON externalProviderPluginName)

externalProviderUnavailableManifestJSON :: BS.ByteString
externalProviderUnavailableManifestJSON =
  externalProviderManifestWithStatusesFor
    externalProviderPluginName
    (externalUnavailableStatusJSON externalProviderPluginName)
    (externalUnavailableStatusJSON externalProviderPluginName)

externalProviderInheritedGrantResourcesManifestJSON :: BS.ByteString
externalProviderInheritedGrantResourcesManifestJSON =
  externalProviderManifestWithStatusesAndGrantResourcesFor
    externalProviderPluginName
    (externalReadyStatusJSON externalProviderPluginName)
    (externalReadyStatusJSON externalProviderPluginName)
    ""

externalConsumerManifestJSON :: BS.ByteString
externalConsumerManifestJSON = externalConsumerManifestFor externalConsumerPluginName externalProviderPluginName

externalOptionalConsumerManifestJSON :: BS.ByteString
externalOptionalConsumerManifestJSON = externalConsumerManifestWithRequiredFor externalConsumerPluginName externalProviderPluginName False

externalMixedConsumerManifestJSON :: BS.ByteString
externalMixedConsumerManifestJSON = externalMixedConsumerManifestFor externalConsumerPluginName externalProviderPluginName

externalProviderManifestFor :: String -> BS.ByteString
externalProviderManifestFor name =
  externalProviderManifestWithStatusesFor name (externalReadyStatusJSON name) (externalReadyStatusJSON name)

externalProviderManifestWithStatusesFor :: String -> String -> String -> BS.ByteString
externalProviderManifestWithStatusesFor name sourceStatusJSON grantStatusJSON =
  externalProviderManifestWithStatusesAndGrantResourcesFor
    name
    sourceStatusJSON
    grantStatusJSON
    "          \"resources\": [\"shared_sources\"],\n"

externalProviderManifestWithStatusesAndGrantResourcesFor :: String -> String -> String -> String -> BS.ByteString
externalProviderManifestWithStatusesAndGrantResourcesFor name sourceStatusJSON grantStatusJSON grantResourcesJSON = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataDirectory\": \"external-provider-data\",\n"
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
    <> "      \"status\": " <> sourceStatusJSON <> ",\n"
    <> "      \"connection\": { \"handle\": \"fixture://provider/terrain.catalog\" },\n"
    <> "      \"configRefs\": [\n"
    <> "        { \"name\": \"terrain-catalog-binding\", \"origin\": \"provider\", \"key\": \"fixture.provider.terrain.catalog\", \"required\": true, \"compatibility\": \"manifest-v3\", \"metadata\": { \"handle\": \"fixture://provider/terrain.catalog\" } }\n"
    <> "      ],\n"
    <> "      \"grants\": [\n"
    <> "        {\n"
    <> "          \"name\": \"terrain-catalog-read\",\n"
    <> "          \"access\": [\"read\"],\n"
    <> "          \"capabilities\": [\"query\", \"health\"],\n"
    <> grantResourcesJSON
    <> "          \"status\": " <> grantStatusJSON <> ",\n"
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
externalConsumerManifestFor name providerName = externalConsumerManifestWithRequiredFor name providerName True

externalConsumerManifestWithRequiredFor :: String -> String -> Bool -> BS.ByteString
externalConsumerManifestWithRequiredFor name providerName required = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataDirectory\": \"external-consumer-data\",\n"
    <> externalConsumerDataResourcesJSON
    <> "  \"externalDataSourceRefs\": [\n"
    <> externalConsumerRefJSON "terrain.catalog" providerName "terrain.catalog" required (externalUnknownStatusJSON providerName) "fixture://consumer/terrain.catalog"
    <> "  ]"
    <> externalIntegrationStartPolicy
    <> "\n}\n"

externalMixedConsumerManifestFor :: String -> String -> BS.ByteString
externalMixedConsumerManifestFor name providerName = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"runtime\": { \"protocol\": { \"min\": " <> show currentProtocolVersion <> ", \"max\": " <> show currentProtocolVersion <> " } },\n"
    <> "  \"capabilities\": [\"dataRead\"],\n"
    <> "  \"dataDirectory\": \"external-consumer-data\",\n"
    <> externalConsumerDataResourcesJSON
    <> "  \"externalDataSourceRefs\": [\n"
    <> externalConsumerRefJSON "terrain.catalog" providerName "terrain.catalog" True (externalUnknownStatusJSON providerName) "fixture://consumer/terrain.catalog"
    <> "    ,\n"
    <> externalConsumerRefJSON "terrain.catalog.optional" "missing-external-provider" "missing.catalog" False (externalUnavailableStatusJSON "missing-external-provider") "fixture://consumer/missing.catalog"
    <> "  ]"
    <> externalIntegrationStartPolicy
    <> "\n}\n"

externalConsumerDataResourcesJSON :: String
externalConsumerDataResourcesJSON =
  "  \"dataResources\": [\n"
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

externalConsumerRefJSON :: String -> String -> String -> Bool -> String -> String -> String
externalConsumerRefJSON refName providerName sourceName required statusJSON binding =
  "    {\n"
    <> "      \"name\": \"" <> refName <> "\",\n"
    <> "      \"provider\": \"" <> providerName <> "\",\n"
    <> "      \"source\": \"" <> sourceName <> "\",\n"
    <> "      \"required\": " <> (if required then "true" else "false") <> ",\n"
    <> "      \"access\": [\"read\"],\n"
    <> "      \"resources\": [\"shared_sources\"],\n"
    <> "      \"grant\": \"terrain-catalog-read\",\n"
    <> "      \"status\": " <> statusJSON <> ",\n"
    <> "      \"reference\": { \"binding\": \"" <> binding <> "\" },\n"
    <> "      \"configRefs\": [\n"
    <> "        { \"name\": \"terrain-catalog-consumer-binding\", \"origin\": \"deployment\", \"key\": \"fixture.consumer.terrain.catalog\", \"required\": true, \"compatibility\": \"manifest-v3\", \"metadata\": { \"binding\": \"" <> binding <> "\" } }\n"
    <> "      ]\n"
    <> "    }\n"

externalReadyStatusJSON :: String -> String
externalReadyStatusJSON providerName =
  "{ \"state\": \"ready\", \"providerId\": \"" <> providerName <> "\", \"availability\": \"available\", \"health\": \"healthy\", \"accessMode\": \"read_only\", \"capabilityScope\": [\"query\", \"health\"], \"compatibility\": \"manifest-v3\", \"diagnostics\": { \"backend\": \"fixture-service\", \"owner\": \"provider-managed\" } }"

externalUnknownStatusJSON :: String -> String
externalUnknownStatusJSON providerName =
  "{ \"state\": \"unknown\", \"providerId\": \"" <> providerName <> "\", \"availability\": \"unknown\", \"health\": \"unknown\", \"accessMode\": \"provider_managed\", \"compatibility\": \"manifest-v3\" }"

externalDegradedStatusJSON :: String -> String
externalDegradedStatusJSON providerName =
  "{ \"state\": \"ready\", \"providerId\": \"" <> providerName <> "\", \"availability\": \"degraded\", \"health\": \"degraded\", \"accessMode\": \"read_only\", \"capabilityScope\": [\"query\", \"health\"], \"compatibility\": \"manifest-v3\" }"

externalUnavailableStatusJSON :: String -> String
externalUnavailableStatusJSON providerName =
  "{ \"state\": \"unavailable\", \"providerId\": \"" <> providerName <> "\", \"availability\": \"unavailable\", \"health\": \"unhealthy\", \"accessMode\": \"disabled\", \"compatibility\": \"manifest-v3\" }"

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
    <> runtimeProtocolLine
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }"
    <> renderStartPolicy policyLines
    <> "\n}\n"

simulationManifestWithStartPolicyFor :: String -> [String] -> BS.ByteString
simulationManifestWithStartPolicyFor name policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> runtimeProtocolLine
    <> "  \"simulation\": { \"dependencies\": [] },\n"
    <> "  \"overlay\": { \"schemaFile\": \"test.toposchema\" }"
    <> renderStartPolicy policyLines
    <> "\n}\n"

dataResourceManifestFor :: String -> [String] -> BS.ByteString
dataResourceManifestFor name = dataResourceManifestWithCapabilitiesFor name "[\"dataRead\"]"

readWriteDataResourceManifestFor :: String -> [String] -> BS.ByteString
readWriteDataResourceManifestFor name = dataResourceManifestWithCapabilitiesFor name "[\"dataRead\", \"dataWrite\"]"

dataResourceManifestWithCapabilitiesFor :: String -> String -> [String] -> BS.ByteString
dataResourceManifestWithCapabilitiesFor name capabilities policyLines = BSC.pack $
  "{\n"
    <> "  \"manifestVersion\": 3,\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> runtimeProtocolLine
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"capabilities\": " <> capabilities <> ",\n"
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
    <> runtimeProtocolLine
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"capabilities\": [\"dataRead\", \"dataWrite\"],\n"
    <> "  \"dataResources\": [\n"
    <> "    {\n"
    <> "      \"name\": \"records\",\n"
    <> "      \"label\": \"Manifest Records\",\n"
    <> "      \"hexBound\": false,\n"
    <> "      \"fields\": [\n"
    <> "        { \"name\": \"id\", \"type\": \"text\", \"label\": \"ID\" },\n"
    <> "        { \"name\": \"name\", \"type\": \"text\", \"label\": \"Name\" }\n"
    <> "      ],\n"
    <> "      \"operations\": { \"list\": true, \"create\": true, \"page\": true },\n"
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
    <> runtimeProtocolLine
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
  , scReportProgress = \_ -> pure ()
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
