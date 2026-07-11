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

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, mask, mask_, onException, try)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Hyperspace.Actor

import Actor.PluginManager.PipelineIntegrator
  ( PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , PluginGeneratorResolution(..)
  , PluginPipelineDiagnostic(..)
  , buildPluginPipelinePlan
  )
import Actor.PluginManager.PluginSupervisor
  ( shutdownPlugin
  , withRefreshedManifestsHandlingPublishException
  )
import Actor.PluginManager.RootSupervisor (PluginManager, pluginManagerActorDef)
import Actor.PluginManager.SimulationIntegrator
  ( PluginSimulationPlan(..)
  , PluginSimulationNodeDiagnostic(..)
  , buildPluginSimulationPlan
  )
import Seer.World.Persist.Types (WorldExternalDataSourceSnapshot, WorldPluginDataDirectory)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
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
  ( MutateResource
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
cleanupInterruptedShutdown trackStopped plugin = do
  result <- try @SomeException (shutdownPlugin plugin)
  case result of
    Left _ -> pure ()
    Right stopped -> trackStopped stopped

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

-- | Re-read manifests for all loaded plugins (hot-reload).
refreshManifests
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
refreshManifests handle = mask $ \restore -> do
  (baseDir, plugins) <- commitLifecycleTransition (call @"refresh" handle #refresh ())
    `onException` commitLifecycleTransition (call @"cancelRefresh" handle #cancelRefresh [])
  let cancelRefresh = commitLifecycleTransition $
        call @"cancelRefresh" handle #cancelRefresh plugins
  restore
    ( do
        waitForLifecycleObservationLease plugins
        withRefreshedManifestsHandlingPublishException
          baseDir
          (Map.fromList [(lpName p, p) | p <- plugins])
          (\refreshed ->
            commitLifecycleTransition $
              call @"finishRefresh" handle #finishRefresh (Map.elems refreshed))
          (\_ _ ->
            commitLifecycleTransition $
              call @"cancelRefresh" handle #cancelRefresh plugins)
    ) `onException` cancelRefresh

-- | Shut down all connected plugins.
shutdownPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
shutdownPlugins handle = mask $ \restore -> do
  plugins <- commitLifecycleTransition (call @"shutdown" handle #shutdown ())
    `onException` commitLifecycleTransition (call @"cancelShutdown" handle #cancelShutdown ([], []))
  stoppedRef <- newIORef Map.empty
  let trackStopped = trackStoppedPlugin stoppedRef
      cancelShutdown = do
        stopped <- Map.elems <$> readIORef stoppedRef
        commitLifecycleTransition $
          call @"cancelShutdown" handle #cancelShutdown (plugins, stopped)
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
  commitLifecycleTransition (call @"finishShutdown" handle #finishShutdown stopped)
    `onException` cancelShutdown

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
