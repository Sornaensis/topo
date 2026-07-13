{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Public facade for the plugin manager actor.
--
-- The actor protocol in this module is intentionally stable; the
-- implementation is split across @Actor.PluginManager.*@ supervisor
-- components.
module Actor.PluginManager
  ( -- * Actor
    PluginManager
  , pluginManagerActorDef
    -- * Loaded plugin info
  , LoadedPlugin(..)
  , OwnedPluginProcess
  , OwnedPluginCleanupResult(..)
  , OwnedPluginRuntime
  , OwnedPluginRuntimeCleanupResult(..)
  , PluginRuntimeGeneration
  , cleanupOwnedPluginRuntime
  , newConnectionOnlyPluginRuntime
  , ownedPluginRuntimeConnection
  , ownedPluginRuntimeProcess
  , ownedPluginRuntimeGeneration
  , ownedPluginProcessHandle
  , ownedPluginProcessId
  , ownedPluginProcessExitCode
  , cleanupOwnedPluginProcess
  , RefreshRuntimeCleanupFailed
  , refreshRuntimeCleanupOwners
  , lpConnection
  , lpProcessHandle
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStateLease(..)
  , PluginDiagnosticState(..)
  , PluginDependencyDiagnostic(..)
  , PluginExternalDataSourceResourceDiagnostic(..)
  , PluginExternalDataSourceConfigRefDiagnostic(..)
  , PluginExternalDataSourceStatusDiagnostic(..)
  , PluginExternalDataSourceGrantDiagnostic(..)
  , PluginExternalDataSourceDiagnostic(..)
  , ExternalDataSourceGrantBrokerPhase(..)
  , PluginParamUpdateError(..)
  , PluginStatus(..)
  , pluginLifecycleSnapshot
  , pluginLifecycleStateText
  , pluginStatusText
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginDiagnosticDetail
  , pluginAvailableDependencyKeys
  , pluginDependencyDiagnostics
  , pluginExternalDataSourceDiagnostics
  , pluginExternalDataSourceDiagnosticsFor
  , externalDataSourceGrantBrokerPhaseText
  , pluginPanelDiagnosticLines
  , pluginResourceNames
  , pluginCapabilitiesText
  , pluginEndpointKind
  , pluginLastError
  , pluginUptimeSeconds
    -- * Pipeline generator integration
  , PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , PluginGeneratorResolution(..)
  , PluginPipelineDiagnostic(..)
  , buildPluginPipelinePlan
    -- * Commands
  , discoverPlugins
  , getLoadedPlugins
  , setPluginParam
  , getPluginStages
  , getPluginOverlaySchemas
  , refreshManifests
  , shutdownPlugins
  , setPluginOrder
  , getPluginOrder
  , setDisabledPlugins
  , getDisabledPlugins
    -- * Data service
  , getPluginDataResources
  , queryPluginResource
  , mutatePluginResource
    -- * World lifecycle
  , WorldPluginDataDirectory(..)
  , notifyWorldChanged
  , getPluginDataDirectories
  , getPluginExternalDataSources
    -- * Simulation DAG integration
  , PluginSimulationPlan(..)
  , PluginSimulationNodeDiagnostic(..)
  , buildPluginSimulationPlanForPlugins
  , getPluginSimulationPlan
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, fromException, mask, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor

import Actor.PluginManager.ProcessLauncher
  ( OwnedPluginCleanupResult(..)
  , OwnedPluginProcess
  , OwnedPluginRuntime
  , OwnedPluginRuntimeCleanupResult(..)
  , PluginRuntimeGeneration
  , cleanupOwnedPluginProcess
  , cleanupOwnedPluginRuntime
  , claimOwnedPluginRuntimeRPCMonitor
  , releaseOwnedPluginRuntimeRPCMonitor
  , claimOwnedPluginRuntimeProcessMonitor
  , releaseOwnedPluginRuntimeProcessMonitor
  , newConnectionOnlyPluginRuntime
  , ownedPluginRuntimeConnection
  , ownedPluginRuntimeGeneration
  , ownedPluginRuntimeProcess
  , ownedPluginProcessExitCode
  , ownedPluginProcessHandle
  , ownedPluginProcessId
  )
import Actor.PluginManager.PipelineIntegrator
  ( PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , PluginGeneratorResolution(..)
  , PluginPipelineDiagnostic(..)
  , buildPluginPipelinePlan
  )
import Actor.PluginManager.PluginSupervisor
  ( RefreshRuntimeCleanupFailed
  , RefreshRuntimeIdentity
  , finalizeInterruptedPluginCleanup
  , failPluginRuntime
  , refreshRuntimeCleanupOwners
  , refreshRuntimeIdentity
  , restartLoadedPluginOnce
  , shutdownPlugin
  , withRefreshedManifestsHandlingPublishException
  )
import Actor.PluginManager.RootSupervisor
  ( PluginManager
  , RuntimeFailureNotice(..)
  , RuntimeRestartDirective(..)
  , RuntimePublicationIdentity(..)
  , pluginManagerActorDef
  )
import Actor.PluginManager.SimulationIntegrator
  ( PluginSimulationPlan(..)
  , PluginSimulationNodeDiagnostic(..)
  , buildPluginSimulationPlan
  )
import Seer.World.Persist.Types (WorldExternalDataSourceSnapshot, WorldPluginDataDirectory)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , lpConnection
  , lpProcessHandle
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStateLease(..)
  , PluginDiagnosticState(..)
  , PluginDependencyDiagnostic(..)
  , PluginExternalDataSourceResourceDiagnostic(..)
  , PluginExternalDataSourceConfigRefDiagnostic(..)
  , PluginExternalDataSourceStatusDiagnostic(..)
  , PluginExternalDataSourceGrantDiagnostic(..)
  , PluginExternalDataSourceDiagnostic(..)
  , ExternalDataSourceGrantBrokerPhase(..)
  , PluginParamUpdateError(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , emptyPluginManagerState
  , pluginLifecycleSnapshot
  , pluginLifecycleStateText
  , pluginStatusText
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginDiagnosticDetail
  , pluginAvailableDependencyKeys
  , pluginDependencyDiagnostics
  , pluginExternalDataSourceDiagnostics
  , pluginExternalDataSourceDiagnosticsFor
  , externalDataSourceGrantBrokerPhaseText
  , pluginPanelDiagnosticLines
  , pluginResourceNames
  , pluginCapabilitiesText
  , pluginEndpointKind
  , pluginLastError
  , pluginUptimeSeconds
  )
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Pipeline (PipelineStage)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC
  ( awaitRPCFailureEvent
  , MutateResource
  , MutateResult
  , QueryResource
  , QueryResult
  )

lifecycleObservationLeaseMicros :: Int
lifecycleObservationLeaseMicros = 100000

waitForLifecycleObservationLease :: [LoadedPlugin] -> IO ()
waitForLifecycleObservationLease plugins =
  if null plugins
    then pure ()
    else threadDelay lifecycleObservationLeaseMicros

commitLifecycleTransition :: IO a -> IO a
commitLifecycleTransition = mask_

trackStoppedPlugin :: IORef (Map Text LoadedPlugin) -> LoadedPlugin -> IO ()
trackStoppedPlugin stoppedRef plugin =
  modifyIORef' stoppedRef (Map.insert (lpName plugin) plugin)

cleanupInterruptedShutdown :: (LoadedPlugin -> IO ()) -> LoadedPlugin -> IO ()
cleanupInterruptedShutdown trackStopped plugin =
  finalizeInterruptedPluginCleanup plugin >>= trackStopped

-- | Discover all plugins in the standard directory.
-- This is asynchronous — use 'getLoadedPlugins' afterwards to
-- retrieve the discovered plugins.
discoverPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
discoverPlugins handle =
  cast @"discover" handle #discover ()

-- | Get the list of currently loaded plugins.
getLoadedPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [LoadedPlugin]
getLoadedPlugins handle =
  call @"getPlugins" handle #getPlugins ()

-- | Set a parameter value on a named plugin.
setPluginParam
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Text -> Text -> Value -> IO (Either PluginParamUpdateError Value)
setPluginParam handle pluginName paramName value =
  call @"setParam" handle #setParam (pluginName, paramName, value)

-- | Get pipeline stages for all loaded generator plugins.
getPluginStages
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [PipelineStage]
getPluginStages handle =
  call @"getStages" handle #getStages ()

-- | Get parsed overlay schemas from loaded plugins in effective order.
getPluginOverlaySchemas
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [OverlaySchema]
getPluginOverlaySchemas handle =
  call @"getOverlaySchemas" handle #getOverlaySchemas ()

-- | Attach generation-scoped monitors after a runtime has been accepted by the
-- actor. Each monitor only reports facts; the actor exclusively claims failure
-- events and restart operations.
monitorPublishedPluginRuntimes
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> [LoadedPlugin]
  -> IO ()
monitorPublishedPluginRuntimes handle = mask_ . mapM_ monitorOne
  where
    monitorOne lp = case lpRuntime lp of
      Nothing -> pure ()
      Just runtime -> do
        let generation = ownedPluginRuntimeGeneration runtime
            pluginName = lpName lp
        rpcClaimed <- claimOwnedPluginRuntimeRPCMonitor runtime
        when rpcClaimed $
          installRPCMonitor pluginName generation runtime
            `onException` releaseOwnedPluginRuntimeRPCMonitor runtime
        processClaimed <- claimOwnedPluginRuntimeProcessMonitor runtime
        when processClaimed $
          installProcessMonitor pluginName generation runtime
            `onException` releaseOwnedPluginRuntimeProcessMonitor runtime

    installRPCMonitor pluginName generation runtime =
      case ownedPluginRuntimeConnection runtime of
        Nothing -> pure ()
        Just conn -> do
          _ <- forkIO $ do
            event <- awaitRPCFailureEvent conn
            superviseRuntimeNotice handle (RuntimeRPCFailure pluginName generation conn event)
          pure ()

    installProcessMonitor pluginName generation runtime =
      case ownedPluginRuntimeProcess runtime of
        Nothing -> pure ()
        Just process -> do
          _ <- forkIO $ do
            exitCode <- waitForExit process
            superviseProcessExit handle (RuntimeProcessExit pluginName generation exitCode)
          pure ()

    waitForExit process = do
      mExit <- ownedPluginProcessExitCode process
      case mExit of
        Just exitCode -> pure exitCode
        Nothing -> threadDelay 10000 >> waitForExit process

superviseRuntimeNotice
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> RuntimeFailureNotice
  -> IO ()
superviseRuntimeNotice handle notice = do
  result <- try @SomeException (call @"runtimeFailure" handle #runtimeFailure notice)
  case result of
    Right (_, Just directive) -> runRuntimeRestartDirective handle directive
    _ -> pure ()

-- A retained aggregate may outlive its root process while descendants or
-- containment are still draining. Re-submit the same confirmed exit until the
-- actor discharges that generation or schedules policy recovery.
superviseProcessExit
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> RuntimeFailureNotice
  -> IO ()
superviseProcessExit handle notice = do
  result <- try @SomeException (call @"runtimeFailure" handle #runtimeFailure notice)
  case result of
    Right (True, Just directive) -> runRuntimeRestartDirective handle directive
    Right (True, Nothing) -> threadDelay 50000 >> superviseProcessExit handle notice
    _ -> pure ()

runRuntimeRestartDirective
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> RuntimeRestartDirective
  -> IO ()
runRuntimeRestartDirective handle directive = do
  backoffResult <- try @SomeException
    (call @"beginRuntimeRestartBackoff" handle #beginRuntimeRestartBackoff directive)
  case backoffResult of
    Left _ -> cancelMatchingRestart handle directive
    Right False -> pure ()
    Right True -> do
      delayResult <- try @SomeException $
        when (rrdBackoffMicros directive > 0) (threadDelay (rrdBackoffMicros directive))
      case delayResult of
        Left _ -> cancelMatchingRestart handle directive
        Right () -> launchRestart
  where
    launchRestart = do
      seedResult <- try @SomeException
        (call @"beginRuntimeRestart" handle #beginRuntimeRestart directive)
      case seedResult of
        Left _ -> cancelMatchingRestart handle directive
        Right Nothing -> pure ()
        Right (Just seed) -> do
          attempt <- try @SomeException (restartLoadedPluginOnce seed)
          restarted <- case attempt of
            Right lp -> pure lp
            Left err -> failPluginRuntime (Text.pack "restart_exception") (Text.pack (show err)) seed
          completion <- try @SomeException $ uninterruptibleMask_
            (call @"finishRuntimeRestart" handle #finishRuntimeRestart (directive, restarted))
          case completion of
            Right (True, nextDirective) -> do
              monitorPublishedPluginRuntimes handle [restarted]
              maybe (pure ()) (runRuntimeRestartDirective handle) nextDirective
            _ -> cleanupUnpublishedRuntime restarted

cancelMatchingRestart
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> RuntimeRestartDirective
  -> IO ()
cancelMatchingRestart handle directive = do
  _ <- try @SomeException $ uninterruptibleMask_
    (call @"cancelRuntimeRestart" handle #cancelRuntimeRestart directive)
  pure ()

-- Keep rejected replacement ownership in this worker until cleanup succeeds;
-- a stale completion must never leak or publish its transport.
cleanupUnpublishedRuntime :: LoadedPlugin -> IO ()
cleanupUnpublishedRuntime lp = do
  stopped <- shutdownPlugin lp
  case lpRuntime stopped of
    Nothing -> pure ()
    Just _ -> threadDelay 10000 >> cleanupUnpublishedRuntime stopped

publishedRuntimeEntries :: [LoadedPlugin] -> [(LoadedPlugin, RuntimePublicationIdentity)]
publishedRuntimeEntries plugins =
  [ ( plugin
    , RuntimePublicationIdentity
        { rpiPluginName = lpName plugin
        , rpiGeneration = ownedPluginRuntimeGeneration runtime
        , rpiConnection = ownedPluginRuntimeConnection runtime
        , rpiProcessId = ownedPluginRuntimeProcess runtime >>= ownedPluginProcessId
        }
    )
  | plugin <- plugins
  , Just runtime <- [lpRuntime plugin]
  ]

resolvePublishedRuntimeOwnership
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> [LoadedPlugin]
  -> IO ([LoadedPlugin], [RefreshRuntimeIdentity])
resolvePublishedRuntimeOwnership handle plugins = uninterruptibleMask_ $ do
  let entries = publishedRuntimeEntries plugins
  ownership <- call @"ownsPublishedRuntimes" handle #ownsPublishedRuntimes (map snd entries)
  let ownedPlugins = [plugin | ((plugin, _), True) <- zip entries ownership]
      ownedIdentities =
        [ identity
        | plugin <- ownedPlugins
        , Just identity <- [refreshRuntimeIdentity plugin]
        ]
  pure (ownedPlugins, ownedIdentities)

-- | Re-read manifests for all loaded plugins (hot-reload).
refreshManifests
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
refreshManifests handle = mask $ \restore -> do
  mOperation <- commitLifecycleTransition (call @"refresh" handle #refresh ())
  (token, baseDir, plugins) <- case mOperation of
    Nothing -> throwIO (userError "plugin lifecycle operation already in progress")
    Just operation -> pure operation
  let cancelRefresh = do
        (_, directives) <- commitLifecycleTransition $
          call @"cancelRefresh" handle #cancelRefresh (token, plugins)
        mapM_ (runRuntimeRestartDirective handle) directives
  refreshResult <- try @SomeException $ restore $ do
    waitForLifecycleObservationLease plugins
    withRefreshedManifestsHandlingPublishException
      baseDir
      (Map.fromList [(lpName p, p) | p <- plugins])
      (\refreshed -> do
        let published = Map.elems refreshed
        accepted <- commitLifecycleTransition $
          call @"finishRefresh" handle #finishRefresh (token, published)
        unless accepted (throwIO (userError "stale plugin refresh completion"))
        monitorPublishedPluginRuntimes handle published)
      (\connected _ -> do
        (ownedPlugins, ownedGenerations) <-
          resolvePublishedRuntimeOwnership handle (Map.elems connected)
        monitorPublishedPluginRuntimes handle ownedPlugins
        pure ownedGenerations)
  case refreshResult of
    Right () -> pure ()
    Left err -> do
      adopted <- case fromException err :: Maybe RefreshRuntimeCleanupFailed of
        Nothing -> pure False
        Just cleanupFailure -> do
          let retained = refreshRuntimeCleanupOwners cleanupFailure
          accepted <- commitLifecycleTransition $
            call @"adoptRefreshCleanup" handle #adoptRefreshCleanup (token, retained)
          if accepted
            then monitorPublishedPluginRuntimes handle retained >> pure True
            else do
              (owned, _) <- resolvePublishedRuntimeOwnership handle retained
              monitorPublishedPluginRuntimes handle owned
              pure (length owned == length retained)
      unless adopted $ do
        -- Rollback only after unpublished cleanup has finished. A retained
        -- cleanup exception is adopted by the actor instead of clearing its
        -- overlap gate.
        _ <- try @SomeException cancelRefresh
        pure ()
      throwIO err

-- | Shut down all connected plugins.
shutdownPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
shutdownPlugins handle = mask $ \restore -> do
  mOperation <- commitLifecycleTransition (call @"shutdown" handle #shutdown ())
  (token, plugins) <- case mOperation of
    Nothing -> throwIO (userError "plugin lifecycle operation already in progress")
    Just operation -> pure operation
  stoppedRef <- newIORef Map.empty
  let trackStopped = trackStoppedPlugin stoppedRef
      cancelShutdown = do
        stopped <- Map.elems <$> readIORef stoppedRef
        _ <- commitLifecycleTransition $
          call @"cancelShutdown" handle #cancelShutdown (token, plugins, stopped)
        pure ()
      shutdownAndTrack plugin = mask $ \restoreOne -> do
        stopped <- restoreOne (shutdownPlugin plugin)
          `onException` cleanupInterruptedShutdown trackStopped plugin
        trackStopped stopped
        pure stopped
  stopped <- restore
    ( do
        waitForLifecycleObservationLease plugins
        traverse shutdownAndTrack plugins
    ) `onException` cancelShutdown
  accepted <- commitLifecycleTransition
    (call @"finishShutdown" handle #finishShutdown (token, stopped))
    `onException` cancelShutdown
  unless accepted (throwIO (userError "stale plugin shutdown completion"))

-- | Set the user-defined plugin order.
setPluginOrder
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> [Text] -> IO ()
setPluginOrder handle order =
  cast @"setOrder" handle #setOrder order

-- | Get the current plugin order.
getPluginOrder
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [Text]
getPluginOrder handle =
  call @"getOrder" handle #getOrder ()

-- | Get data resource schemas from all plugins that declare them.
-- Returns a map from plugin name to its declared data resource schemas.
getPluginDataResources
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO (Map Text [DataResourceSchema])
getPluginDataResources handle =
  call @"getDataResources" handle #getDataResources ()

-- | Forward a data query to the named plugin.
queryPluginResource
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Text -> QueryResource -> IO (Either Text QueryResult)
queryPluginResource handle pluginName qr =
  call @"queryData" handle #queryData (pluginName, qr)

-- | Forward a data mutation to the named plugin.
mutatePluginResource
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Text -> MutateResource -> IO (Either Text MutateResult)
mutatePluginResource handle pluginName mr =
  call @"mutateData" handle #mutateData (pluginName, mr)

-- | Notify all connected plugins that the world path has changed.
--
-- Called after world save\/load so plugins can locate their data
-- directories relative to the new world path.
notifyWorldChanged
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Maybe Text  -- ^ New world path, or 'Nothing' if no world is loaded.
  -> IO ()
notifyWorldChanged handle mWorldPath =
  cast @"notifyWorld" handle #notifyWorld mWorldPath

-- | Get host-derived data roots for connected plugins with validated data
-- archive destinations from the handshake/manifest contract.
getPluginDataDirectories
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [WorldPluginDataDirectory]
getPluginDataDirectories handle =
  call @"getDataDirs" handle #getDataDirs ()

-- | Get backend-neutral external data-source declarations/references from
-- loaded plugin manifests for world-save metadata.
getPluginExternalDataSources
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [WorldExternalDataSourceSnapshot]
getPluginExternalDataSources handle =
  call @"getExternalDataSources" handle #getExternalDataSources ()

-- | Build plugin simulation diagnostics from an explicit plugin list.
-- This keeps tests and service previews on the public facade without exposing
-- the actor's full mutable state.
buildPluginSimulationPlanForPlugins :: Maybe [Text] -> [LoadedPlugin] -> PluginSimulationPlan
buildPluginSimulationPlanForPlugins mOverlayNames plugins =
  buildPluginSimulationPlan mOverlayNames emptyPluginManagerState
    { pmsPlugins = Map.fromList [(lpName plugin, plugin) | plugin <- plugins]
    , pmsPluginOrder = map lpName plugins
    }

-- | Build executable plugin simulation nodes and declaration diagnostics for
-- the current plugin-manager snapshot.  Pass bound-world overlay names when a
-- world is available; otherwise declarations are reported as non-executable.
getPluginSimulationPlan
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Maybe [Text]
  -> IO PluginSimulationPlan
getPluginSimulationPlan handle mOverlayNames =
  call @"getSimulationPlan" handle #getSimulationPlan mOverlayNames

-- | Update the set of disabled plugin names.
--
-- Disabled plugins are excluded from 'getPluginStages' and
-- 'getPluginOverlaySchemas'.
setDisabledPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Set Text -> IO ()
setDisabledPlugins handle disabled =
  cast @"setDisabled" handle #setDisabled disabled

-- | Query the current set of disabled plugin names.
getDisabledPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO (Set Text)
getDisabledPlugins handle =
  call @"getDisabled" handle #getDisabled ()
