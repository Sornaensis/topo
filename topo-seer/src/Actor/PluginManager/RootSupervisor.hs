{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The public PluginManager actor protocol and root supervision loop.
-- The actor keeps external messages stable while delegating scanner,
-- lifecycle, routing, and integration work to focused modules.
module Actor.PluginManager.RootSupervisor
  ( PluginManager
  , pluginManagerActorDef
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Traversable (mapAccumM)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import System.Directory (createDirectoryIfMissing)

import Actor.PluginManager.Config
  ( loadDisabledPlugins
  , loadPluginOrder
  , saveDisabledPlugins
  , savePluginConfig
  , savePluginOrder
  )
import Actor.PluginManager.DataResourceRouter
  ( mutatePluginDataResource
  , queryPluginDataResource
  )
import Actor.PluginManager.ExternalDataSourceBroker
  ( reconcileExternalDataSourceBrokering
  , revokeExternalDataSourceBrokeredGrants
  )
import Actor.PluginManager.PipelineIntegrator
  ( buildPluginOverlaySchemas
  , buildPluginStages
  )
import Actor.PluginManager.PluginSupervisor
  ( disconnectPlugin
  , handlePluginRuntimeFailure
  , markExternalDataSourceBlocked
  , markExternalDataSourceDegraded
  , markPluginStarting
  , markPluginStopping
  , observePluginRuntime
  , shutdownPlugin
  )
import Actor.PluginManager.ResourceRegistry
  ( buildPluginDataResources
  , collectPluginDataDirs
  , collectPluginExternalDataSources
  )
import Actor.PluginManager.Scanner
  ( pluginsBaseDir
  , scanPluginDirs
  )
import Actor.PluginManager.SimulationIntegrator
  ( PluginSimulationPlan
  , buildPluginSimulationPlan
  , notifyPluginsWorldChanged
  )
import Actor.PluginManager.Types
  ( ExternalDataSourceGrantBrokerPhase(..)
  , ExternalDataSourceGrantBrokerState(..)
  , ExternalDataSourceGrantKey(..)
  , LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginParamUpdateError(..)
  , PluginStatus(..)
  , emptyPluginManagerState
  , setParamOnPlugin
  )
import Seer.World.Persist.Types (WorldExternalDataSourceSnapshot, WorldPluginDataDirectory)
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Pipeline (PipelineStage)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC
  ( DataResourceErrorCode(..)
  , MutateResource
  , MutateResult
  , QueryResource
  , QueryResult
  , RPCConnection(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , dataResourceFailureFromText
  , drfCode
  , externalDataSourceStatusBlocksStartup
  )
import Topo.Plugin.RPC.Manifest
  ( rmExternalDataSourceRefs
  , rmParameters
  , validateRPCParamUpdate
  )

[hyperspace|
actor PluginManager
  state PluginManagerState
  lifetime Singleton
  schedule pinned 1
  noDeps
  mailbox Unbounded

  cast discover :: ()
  call getPlugins :: () -> [LoadedPlugin]
  call getStages :: () -> [PipelineStage]
  call getOverlaySchemas :: () -> [OverlaySchema]
  call getOrder :: () -> [Text]
  call getDisabled :: () -> Set Text
  call setParam :: (Text, Text, Value) -> Either PluginParamUpdateError Value
  cast setOrder :: [Text]
  cast setDisabled :: Set Text
  call refresh :: () -> (FilePath, [LoadedPlugin])
  call finishRefresh :: [LoadedPlugin] -> ()
  call cancelRefresh :: [LoadedPlugin] -> Bool
  call shutdown :: () -> [LoadedPlugin]
  call finishShutdown :: [LoadedPlugin] -> ()
  call cancelShutdown :: ([LoadedPlugin], [LoadedPlugin]) -> ()
  call getDataResources :: () -> Map Text [DataResourceSchema]
  call queryData :: (Text, QueryResource) -> Either Text QueryResult
  call mutateData :: (Text, MutateResource) -> Either Text MutateResult
  cast notifyWorld :: Maybe Text
  call getDataDirs :: () -> [WorldPluginDataDirectory]
  call getExternalDataSources :: () -> [WorldExternalDataSourceSnapshot]
  call getSimulationPlan :: Maybe [Text] -> PluginSimulationPlan

  initial emptyPluginManagerState
  on_ discover = \() st -> do
    baseDir <- pluginsBaseDir
    createDirectoryIfMissing True baseDir
    plugins <- scanPluginDirs baseDir
    orderTxt <- loadPluginOrder baseDir
    disabled <- loadDisabledPlugins baseDir
    pure st
      { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins]
      , pmsPluginOrder = orderTxt
      , pmsBaseDir = baseDir
      , pmsDisabledPlugins = disabled
      , pmsPendingRefresh = Nothing
      , pmsPendingShutdown = Nothing
      , pmsExternalDataSourceGrants = Map.empty
      }
  on getPlugins = \() st -> do
    st' <- observePluginRuntimes st
    pure (st', Map.elems (pmsPlugins st'))
  on getStages = \() st -> do
    st' <- observePluginRuntimes st
    pure (st', buildPluginStages st')
  onPure getOverlaySchemas = \() st ->
    (st, buildPluginOverlaySchemas st)
  onPure getOrder = \() st ->
    (st, pmsPluginOrder st)
  onPure getDisabled = \() st ->
    (st, pmsDisabledPlugins st)
  on setParam = \(pluginName, paramName, value) st -> do
    case Map.lookup pluginName (pmsPlugins st) of
      Nothing -> pure (st, Left (PluginParamUnknownPlugin pluginName))
      Just lp -> case validateRPCParamUpdate (rmParameters (lpManifest lp)) paramName value of
        Left err -> pure (st, Left (PluginParamValidationFailed err))
        Right sanitized -> do
          let lp' = setParamOnPlugin paramName sanitized lp
              st' = st { pmsPlugins = Map.insert pluginName lp' (pmsPlugins st) }
          savePluginConfig (lpDirectory lp') (lpParams lp')
          pure (st', Right sanitized)
  on_ setOrder = \order st -> do
    savePluginOrder (pmsBaseDir st) order
    pure st { pmsPluginOrder = order }
  on_ setDisabled = \disabled st -> do
    saveDisabledPlugins (pmsBaseDir st) disabled
    let st' = st { pmsDisabledPlugins = disabled }
    reconcileExternalDataSourceBrokering st st'
  on refresh = \() st -> do
    startingAt <- getCurrentTime
    let plugins = Map.elems (pmsPlugins st)
    pure
      ( st
          { pmsPlugins = Map.map (markPluginStarting startingAt) (pmsPlugins st)
          , pmsPendingRefresh = Just (pmsPlugins st)
          }
      , (pmsBaseDir st, plugins)
      )
  on finishRefresh = \plugins st -> do
    let st' = st
          { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins]
          , pmsPendingRefresh = Nothing
          }
    st'' <- reconcileExternalDataSourceBrokering st st'
    pure (st'', ())
  on cancelRefresh = \plugins st -> do
    cancelledAt <- getCurrentTime
    (st', cancelled) <- cancelStartingPlugins cancelledAt plugins st
    pure (st', cancelled)
  on shutdown = \() st -> do
    stoppingAt <- getCurrentTime
    stRevoked <- revokeExternalDataSourceBrokeredGrants st "plugin manager shutdown requested"
    let plugins = Map.elems (pmsPlugins stRevoked)
    pure
      ( stRevoked
          { pmsPlugins = Map.map (markPluginStopping stoppingAt) (pmsPlugins stRevoked)
          , pmsPendingShutdown = Just (pmsPlugins stRevoked)
          }
      , plugins
      )
  on finishShutdown = \plugins st -> do
    stoppedAt <- getCurrentTime
    pure
      ( st
          { pmsPlugins = Map.fromList [(lpName p, disconnectPlugin stoppedAt p) | p <- plugins]
          , pmsPendingShutdown = Nothing
          , pmsExternalDataSourceGrants = Map.empty
          }
      , ()
      )
  on cancelShutdown = \(plugins, stoppedPlugins) st -> do
    stoppedAt <- getCurrentTime
    pure (cancelStoppingPlugins stoppedAt plugins stoppedPlugins st, ())
  onPure getDataResources = \() st ->
    (st, buildPluginDataResources st)
  on queryData = \(pluginName, qr) st -> do
    result <- queryPluginDataResource pluginName qr st
    st' <- markRuntimeFailureOnConnectedDataError pluginName "data_query_failed" result st
    st'' <- reconcileExternalDataSourceBrokering st st'
    pure (st'', result)
  on mutateData = \(pluginName, mr) st -> do
    result <- mutatePluginDataResource pluginName mr st
    st' <- markRuntimeFailureOnConnectedDataError pluginName "data_mutation_failed" result st
    st'' <- reconcileExternalDataSourceBrokering st st'
    pure (st'', result)
  on_ notifyWorld = \mWorldPath st -> do
    notifyPluginsWorldChanged mWorldPath (Map.elems (pmsPlugins st))
    pure st
  on getDataDirs = \() st -> do
    dirs <- collectPluginDataDirs st
    pure (st, dirs)
  on getExternalDataSources = \() st -> do
    st' <- observePluginRuntimes st
    pure (st', collectPluginExternalDataSources st')
  on getSimulationPlan = \mOverlayNames st -> do
    st' <- observePluginRuntimes st
    pure (st', buildPluginSimulationPlan mOverlayNames st')|]

cancelStartingPlugins :: UTCTime -> [LoadedPlugin] -> PluginManagerState -> IO (PluginManagerState, Bool)
cancelStartingPlugins cancelledAt plugins st = do
  (cancelledAny, plugins') <- mapAccumM rollbackStartingPlugin False (pmsPlugins st)
  pure
    ( st
        { pmsPlugins = plugins'
        , pmsPendingRefresh = Nothing
        }
    , cancelledAny
    )
  where
    previous = maybe (loadedPluginsByName plugins) id (pmsPendingRefresh st)
    rollbackStartingPlugin cancelled current
      | plsState (lpLifecycle current) /= LifecycleStarting = pure (cancelled, current)
      | Just previousPlugin <- Map.lookup (lpName current) previous = do
          plugin' <- cancelRefreshPlugin cancelledAt previousPlugin
          pure (True, plugin')
      | otherwise = pure (cancelled, current)

cancelRefreshPlugin :: UTCTime -> LoadedPlugin -> IO LoadedPlugin
cancelRefreshPlugin cancelledAt previousPlugin
  | isNothing (lpConnection previousPlugin) && isNothing (lpProcessHandle previousPlugin) =
      pure previousPlugin
  | otherwise = do
      stopped <- shutdownPlugin previousPlugin
      pure (disconnectPlugin cancelledAt stopped)

cancelStoppingPlugins :: UTCTime -> [LoadedPlugin] -> [LoadedPlugin] -> PluginManagerState -> PluginManagerState
cancelStoppingPlugins stoppedAt plugins stoppedPlugins st =
  st
    { pmsPlugins = Map.map rollbackOrFinalizeStoppingPlugin (pmsPlugins st)
    , pmsPendingShutdown = Nothing
    }
  where
    previous = maybe (loadedPluginsByName plugins) id (pmsPendingShutdown st)
    stopped = loadedPluginsByName stoppedPlugins
    rollbackOrFinalizeStoppingPlugin current
      | plsState (lpLifecycle current) /= LifecycleStopping = current
      | Just stoppedPlugin <- Map.lookup (lpName current) stopped =
          disconnectPlugin stoppedAt stoppedPlugin
      | otherwise = Map.findWithDefault current (lpName current) previous

loadedPluginsByName :: [LoadedPlugin] -> Map Text LoadedPlugin
loadedPluginsByName plugins = Map.fromList [(lpName p, p) | p <- plugins]

observePluginRuntimes :: PluginManagerState -> IO PluginManagerState
observePluginRuntimes st
  | isNothing (pmsPendingRefresh st) && isNothing (pmsPendingShutdown st) = do
      plugins' <- traverse observePluginRuntime (pmsPlugins st)
      let observed = st { pmsPlugins = plugins' }
      reconcileExternalDataSourceBrokering st observed
  | otherwise = pure st

markRuntimeFailureOnConnectedDataError
  :: Text
  -> Text
  -> Either Text a
  -> PluginManagerState
  -> IO PluginManagerState
markRuntimeFailureOnConnectedDataError pluginName errorCode result st =
  case result of
    Left err ->
      case drfCode (dataResourceFailureFromText err) of
        ExternalDataSourceUnavailable ->
          markExternalDataSourceUnavailableOnConnectedDataError pluginName errorCode err st
        code
          | dataResourceErrorCodeMarksRuntimeFailure code ->
              markRuntimeFailureOnConnectedError pluginName errorCode result st
        _ -> pure st
    _ -> pure st

markExternalDataSourceUnavailableOnConnectedDataError
  :: Text
  -> Text
  -> Text
  -> PluginManagerState
  -> IO PluginManagerState
markExternalDataSourceUnavailableOnConnectedDataError pluginName errorCode err st =
  case Map.lookup pluginName (pmsPlugins st) of
    Just lp@LoadedPlugin { lpStatus = PluginConnected, lpConnection = Just _ } ->
      case externalDataSourceFailureRef lp of
        Just (ref, True) -> do
          let reason = externalDataSourceDataErrorReason err
              lpWithUnavailableRef = markLoadedPluginExternalRefUnavailable ref reason lp
          lp' <- markExternalDataSourceBlocked (externalDataSourceRefDependency ref) reason lpWithUnavailableRef
          let stWithUnavailableGrant = markExternalDataSourceGrantUnavailableForRef
                (lpName lp')
                ref
                reason
                (plsUpdatedAt (lpLifecycle lp'))
                st
          pure stWithUnavailableGrant { pmsPlugins = Map.insert pluginName lp' (pmsPlugins stWithUnavailableGrant) }
        Just (ref, False) -> do
          let reason = externalDataSourceDataErrorReason err
              lpWithUnavailableRef = markLoadedPluginExternalRefUnavailable ref reason lp
          lp' <- markExternalDataSourceDegraded (externalDataSourceRefDependency ref) reason lpWithUnavailableRef
          let stWithUnavailableGrant = markExternalDataSourceGrantUnavailableForRef
                (lpName lp')
                ref
                reason
                (plsUpdatedAt (lpLifecycle lp'))
                st
          pure stWithUnavailableGrant { pmsPlugins = Map.insert pluginName lp' (pmsPlugins stWithUnavailableGrant) }
        Nothing -> markRuntimeFailureOnConnectedError pluginName errorCode (Left err) st
    _ -> pure st

externalDataSourceFailureRef :: LoadedPlugin -> Maybe (RPCExternalDataSourceRef, Bool)
externalDataSourceFailureRef lp =
  case (hardRequiredRefs, hardRefs, requiredRefs, refs) of
    (ref:_, _, _, _) -> Just (ref, True)
    ([], ref:_, _, _) -> Just (ref, redsrRequired ref)
    ([], [], ref:_, _) -> Just (ref, True)
    ([], [], [], ref:_) -> Just (ref, False)
    _ -> Nothing
  where
    refs = rmExternalDataSourceRefs (lpManifest lp)
    hardRefs = filter (externalDataSourceStatusBlocksStartup . redsrStatus) refs
    hardRequiredRefs = filter redsrRequired hardRefs
    requiredRefs = filter redsrRequired refs

markLoadedPluginExternalRefUnavailable :: RPCExternalDataSourceRef -> Text -> LoadedPlugin -> LoadedPlugin
markLoadedPluginExternalRefUnavailable targetRef reason lp = lp { lpManifest = manifest', lpConnection = fmap syncConn (lpConnection lp) }
  where
    manifest = lpManifest lp
    refs = map markRef (rmExternalDataSourceRefs manifest)
    manifest' = manifest { rmExternalDataSourceRefs = refs }
    syncConn conn = conn { rpcManifest = manifest' }
    markRef ref
      | externalDataSourceRefMatches targetRef ref = ref { redsrStatus = externalDataSourceQueryFailureStatus ref reason }
      | otherwise = ref

markExternalDataSourceGrantUnavailableForRef
  :: Text
  -> RPCExternalDataSourceRef
  -> Text
  -> UTCTime
  -> PluginManagerState
  -> PluginManagerState
markExternalDataSourceGrantUnavailableForRef consumerName ref reason consumerReadyAt st =
  st { pmsExternalDataSourceGrants = Map.mapWithKey markGrant grants }
  where
    grants = pmsExternalDataSourceGrants st
    hasMatchingGrant = any (externalDataSourceGrantKeyMatchesRef consumerName ref) (Map.keys grants)
    fallbackToConsumerGrants = not hasMatchingGrant && not (externalDataSourceStatusBlocksStartup (redsrStatus ref))
    markGrant key grantState
      | externalDataSourceGrantKeyMatchesRef consumerName ref key || fallbackMatchesConsumer key = grantState
          { edsgbsState = ExternalDataSourceGrantFailed
          , edsgbsReason = Just reason
          , edsgbsConsumerReadyAt = consumerReadyAt
          }
      | otherwise = grantState
    fallbackMatchesConsumer key = fallbackToConsumerGrants && edsgkConsumer key == consumerName

externalDataSourceQueryFailureStatus :: RPCExternalDataSourceRef -> Text -> RPCExternalDataSourceStatus
externalDataSourceQueryFailureStatus ref reason = (redsrStatus ref)
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = redsrProvider ref
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }

externalDataSourceRefMatches :: RPCExternalDataSourceRef -> RPCExternalDataSourceRef -> Bool
externalDataSourceRefMatches expected actual =
  redsrName expected == redsrName actual
    && redsrSource expected == redsrSource actual
    && redsrProvider expected == redsrProvider actual
    && redsrGrant expected == redsrGrant actual

externalDataSourceGrantKeyMatchesRef :: Text -> RPCExternalDataSourceRef -> ExternalDataSourceGrantKey -> Bool
externalDataSourceGrantKeyMatchesRef consumerName ref key =
  edsgkConsumer key == consumerName
    && edsgkRef key == redsrName ref
    && edsgkSource key == redsrSource ref
    && maybe True (== edsgkProvider key) (redsrProvider ref)
    && maybe True (== edsgkGrant key) (redsrGrant ref)

externalDataSourceRefDependency :: RPCExternalDataSourceRef -> Text
externalDataSourceRefDependency ref =
  maybe "unresolved" id (redsrProvider ref)
    <> ":" <> redsrSource ref
    <> maybe "" (":" <>) (redsrGrant ref)

externalDataSourceDataErrorReason :: Text -> Text
externalDataSourceDataErrorReason err =
  "external data-source data operation failed: " <> err

markRuntimeFailureOnConnectedError
  :: Text
  -> Text
  -> Either Text a
  -> PluginManagerState
  -> IO PluginManagerState
markRuntimeFailureOnConnectedError pluginName errorCode result st =
  case (result, Map.lookup pluginName (pmsPlugins st)) of
    (Left err, Just lp@LoadedPlugin { lpStatus = PluginConnected, lpConnection = Just _ }) -> do
      lp' <- handlePluginRuntimeFailure errorCode err lp
      pure st { pmsPlugins = Map.insert pluginName lp' (pmsPlugins st) }
    _ -> pure st

dataResourceErrorCodeMarksRuntimeFailure :: DataResourceErrorCode -> Bool
dataResourceErrorCodeMarksRuntimeFailure code =
  case code of
    PluginUnavailable -> True
    DataResourceTimeout -> True
    DataResourceInternalError -> True
    _ -> False
