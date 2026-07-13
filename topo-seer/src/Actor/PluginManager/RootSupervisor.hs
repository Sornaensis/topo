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
  , RuntimeFailureNotice(..)
  , RuntimeRestartDirective(..)
  , RuntimePublicationIdentity(..)
  , pluginManagerActorDef
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Word (Word64)
import System.Exit (ExitCode(..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
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
import Actor.PluginManager.ProcessLauncher
  ( PluginRuntimeGeneration
  , ownedPluginProcessExitCode
  , ownedPluginProcessId
  , ownedPluginRuntimeGeneration
  , ownedPluginRuntimeProcess
  )
import Actor.PluginManager.PipelineIntegrator
  ( buildPluginOverlaySchemas
  , buildPluginStages
  )
import Actor.PluginManager.PluginSupervisor
  ( disconnectPlugin
  , failPluginProcessExit
  , failPluginRuntime
  , handlePluginRuntimeFailure
  , markExternalDataSourceBlocked
  , markExternalDataSourceDegraded
  , markPluginStarting
  , markPluginStopping
  , preparePluginRuntimeRestart
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
  , PluginManagerOperation(..)
  , PluginManagerState(..)
  , PluginParamUpdateError(..)
  , PluginRefreshFailure(..)
  , PluginRestartOperation(..)
  , PluginRestartPhase(..)
  , PluginStatus(..)
  , emptyPluginManagerState
  , lpConnection
  , lpProcessHandle
  , mapLoadedPluginConnection
  , pluginLifecycleSnapshot
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
  , RPCError(..)
  , RPCFailureEvent(..)
  , RPCRestartMode(..)
  , RPCStartPolicy(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , claimRPCFailureEvent
  , dataResourceFailureFromText
  , drfCode
  , externalDataSourceStatusBlocksStartup
  , sameRPCConnection
  )
import Topo.Plugin.RPC.Manifest
  ( rmExternalDataSourceRefs
  , rmParameters
  , validateRPCParamUpdate
  )

-- | Generation-tagged notification delivered by a runtime monitor. RPC
-- failures include their event identity so only the actor can claim the slot.
data RuntimeFailureNotice
  = RuntimeRPCFailure !Text !PluginRuntimeGeneration !RPCConnection !RPCFailureEvent
  | RuntimeProcessExit !Text !PluginRuntimeGeneration !ExitCode

-- | Actor-owned permission to perform one delayed replacement attempt.
data RuntimeRestartDirective = RuntimeRestartDirective
  { rrdPluginName :: !Text
  , rrdGeneration :: !PluginRuntimeGeneration
  , rrdOperationToken :: !Word64
  , rrdBackoffMicros :: !Int
  }

-- | Physical identities used to resolve an interrupted publication handoff.
data RuntimePublicationIdentity = RuntimePublicationIdentity
  { rpiPluginName :: !Text
  , rpiGeneration :: !PluginRuntimeGeneration
  , rpiConnection :: !(Maybe RPCConnection)
  , rpiProcessId :: !(Maybe Word64)
  }

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
  call refresh :: () -> Maybe (Word64, FilePath, [LoadedPlugin])
  call finishRefresh :: (Word64, [LoadedPlugin]) -> Bool
  call ownsPublishedRuntimes :: [RuntimePublicationIdentity] -> [Bool]
  call adoptRefreshCleanup :: (Word64, [LoadedPlugin]) -> Bool
  call cancelRefresh :: (Word64, [LoadedPlugin]) -> (Bool, [RuntimeRestartDirective])
  call shutdown :: () -> Maybe (Word64, [LoadedPlugin])
  call finishShutdown :: (Word64, [LoadedPlugin]) -> Bool
  call cancelShutdown :: (Word64, [LoadedPlugin], [LoadedPlugin]) -> Bool
  call runtimeFailure :: RuntimeFailureNotice -> (Bool, Maybe RuntimeRestartDirective)
  call beginRuntimeRestartBackoff :: RuntimeRestartDirective -> Bool
  call beginRuntimeRestart :: RuntimeRestartDirective -> Maybe LoadedPlugin
  call cancelRuntimeRestart :: RuntimeRestartDirective -> Bool
  call finishRuntimeRestart :: (RuntimeRestartDirective, LoadedPlugin) -> (Bool, Maybe RuntimeRestartDirective)
  call getDataResources :: () -> Map Text [DataResourceSchema]
  call queryData :: (Text, QueryResource) -> Either Text QueryResult
  call mutateData :: (Text, MutateResource) -> Either Text MutateResult
  cast notifyWorld :: Maybe Text
  call getDataDirs :: () -> [WorldPluginDataDirectory]
  call getExternalDataSources :: () -> [WorldExternalDataSourceSnapshot]
  call getSimulationPlan :: Maybe [Text] -> PluginSimulationPlan

  initial emptyPluginManagerState
  on_ discover = \() st ->
    if isJust (pmsPendingRefresh st) || isJust (pmsPendingShutdown st)
        || not (Map.null (pmsPendingRestarts st))
        || any (isJust . lpRuntime) (Map.elems (pmsPlugins st))
      then pure st
      else do
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
          , pmsPendingRestarts = Map.empty
          , pmsNextOperationToken = 1
          , pmsExternalDataSourceGrants = Map.empty
          }
  on getPlugins = \() st -> do
    st' <- reconcileExternalDataSourceBrokering st st
    pure (st', Map.elems (pmsPlugins st'))
  on getStages = \() st -> do
    st' <- reconcileExternalDataSourceBrokering st st
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
  on refresh = \() st ->
    if lifecycleOperationPending st
      then pure (st, Nothing)
      else do
        startingAt <- getCurrentTime
        let token = pmsNextOperationToken st
            plugins = Map.elems (pmsPlugins st)
            operation = PluginManagerOperation token (pmsPlugins st) False Map.empty
        pure
          ( st
              { pmsPlugins = Map.map (markPluginStarting startingAt) (pmsPlugins st)
              , pmsPendingRefresh = Just operation
              , pmsPendingRestarts = Map.empty
              , pmsNextOperationToken = nextOperationToken token
              }
          , Just (token, pmsBaseDir st, plugins)
          )
  on finishRefresh = \(token, plugins) st ->
    case pmsPendingRefresh st of
      Just operation
        | pmoToken operation == token
        , not (pmoInvalidated operation)
        , isNothing (pmsPendingShutdown st) -> do
            let st' = st
                  { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins]
                  , pmsPendingRefresh = Nothing
                  }
            st'' <- reconcileExternalDataSourceBrokering st st'
            pure (st'', True)
      _ -> pure (st, False)
  onPure ownsPublishedRuntimes = \identities st ->
    (st, map (runtimePublicationIdentityMatches st) identities)
  onPure adoptRefreshCleanup = \(token, retained) st ->
    case pmsPendingRefresh st of
      Just operation | pmoToken operation == token ->
        ( st
            { pmsPlugins = foldr (\plugin -> Map.insert (lpName plugin) plugin)
                (pmsPlugins st)
                retained
            , pmsPendingRefresh = Nothing
            }
        , True
        )
      _ -> (st, False)
  on cancelRefresh = \(token, plugins) st -> do
    cancelledAt <- getCurrentTime
    cancelStartingPlugins token cancelledAt plugins st
  on shutdown = \() st ->
    if lifecycleOperationPending st
      then pure (st, Nothing)
      else do
        stoppingAt <- getCurrentTime
        stRevoked <- revokeExternalDataSourceBrokeredGrants st "plugin manager shutdown requested"
        let token = pmsNextOperationToken stRevoked
            plugins = Map.elems (pmsPlugins stRevoked)
            operation = PluginManagerOperation token (pmsPlugins stRevoked) False Map.empty
        pure
          ( stRevoked
              { pmsPlugins = Map.map (markPluginStopping stoppingAt) (pmsPlugins stRevoked)
              , pmsPendingShutdown = Just operation
              , pmsPendingRestarts = Map.empty
              , pmsNextOperationToken = nextOperationToken token
              }
          , Just (token, plugins)
          )
  on finishShutdown = \(token, plugins) st ->
    case pmsPendingShutdown st of
      Just operation
        | pmoToken operation == token
        , isNothing (pmsPendingRefresh st) -> do
            stoppedAt <- getCurrentTime
            pure
              ( st
                  { pmsPlugins = Map.fromList [(lpName p, disconnectPlugin stoppedAt p) | p <- plugins]
                  , pmsPendingShutdown = Nothing
                  , pmsExternalDataSourceGrants = Map.empty
                  }
              , True
              )
      _ -> pure (st, False)
  on cancelShutdown = \(token, plugins, stoppedPlugins) st -> do
    stoppedAt <- getCurrentTime
    pure (cancelStoppingPlugins token stoppedAt plugins stoppedPlugins st)
  on runtimeFailure = \notice st -> handleRuntimeFailureNotice notice st
  on beginRuntimeRestartBackoff = \directive st ->
    beginRuntimeRestartBackoffState directive st
  onPure beginRuntimeRestart = \directive st ->
    beginRuntimeRestartLaunchState directive st
  on cancelRuntimeRestart = \directive st ->
    cancelRuntimeRestartState directive st
  on finishRuntimeRestart = \(directive, restarted) st ->
    finishRuntimeRestartAttempt directive restarted st
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
    st' <- reconcileExternalDataSourceBrokering st st
    pure (st', collectPluginExternalDataSources st')
  on getSimulationPlan = \mOverlayNames st -> do
    st' <- reconcileExternalDataSourceBrokering st st
    pure (st', buildPluginSimulationPlan mOverlayNames st')|]

runtimePublicationIdentityMatches :: PluginManagerState -> RuntimePublicationIdentity -> Bool
runtimePublicationIdentityMatches st identity =
  case Map.lookup (rpiPluginName identity) (pmsPlugins st) >>= lpRuntime of
    Nothing -> False
    Just runtime
      | ownedPluginRuntimeGeneration runtime /= rpiGeneration identity -> False
      | (ownedPluginRuntimeProcess runtime >>= ownedPluginProcessId) /= rpiProcessId identity -> False
      | otherwise -> case (rpiConnection identity, lpConnection =<< Map.lookup (rpiPluginName identity) (pmsPlugins st)) of
          (Just expected, Just current) -> sameRPCConnection expected current
          (Nothing, Nothing) -> True
          _ -> False

lifecycleOperationPending :: PluginManagerState -> Bool
lifecycleOperationPending st =
  isJust (pmsPendingRefresh st)
    || isJust (pmsPendingShutdown st)
    || any ((== PluginRestartLaunching) . proPhase) (Map.elems (pmsPendingRestarts st))

nextOperationToken :: Word64 -> Word64
nextOperationToken token
  | token == maxBound = 1
  | otherwise = token + 1

handleRuntimeFailureNotice
  :: RuntimeFailureNotice
  -> PluginManagerState
  -> IO (PluginManagerState, (Bool, Maybe RuntimeRestartDirective))
handleRuntimeFailureNotice notice st =
  case Map.lookup pluginName (pmsPlugins st) of
    Nothing -> rejectStale
    Just lp
      | not (noticeMatchesGeneration generation lp) -> rejectStale
      | Map.member pluginName (pmsPendingRestarts st) -> pure (st, (False, Nothing))
      | otherwise -> do
          claimed <- claimNotice notice lp
          if not claimed
            then do
              consumeStaleNotice notice
              pure (st, (False, Nothing))
            else do
              restartAllowed <- noticeAllowsRestart notice lp
              failed <- failRuntimeNotice notice lp
              if isJust (pmsPendingRefresh st)
                then pure
                  ( st
                      { pmsPlugins = Map.insert pluginName failed (pmsPlugins st)
                      , pmsPendingRefresh = fmap
                          (\operation -> operation
                            { pmoInvalidated = True
                            , pmoFailures = Map.insertWith
                                (\_ existing -> existing)
                                pluginName
                                (PluginRefreshFailure generation restartAllowed)
                                (pmoFailures operation)
                            })
                          (pmsPendingRefresh st)
                      }
                  , (True, Nothing)
                  )
                else if isJust (pmsPendingShutdown st)
                  then pure
                    ( st { pmsPlugins = Map.insert pluginName failed (pmsPlugins st) }
                    , (True, Nothing)
                    )
                  else do
                    if restartAllowed
                      then do
                        (scheduled, directive) <- scheduleRuntimeRestart generation failed st
                        pure (scheduled, (True, directive))
                      else pure
                        ( st
                            { pmsPlugins = Map.insert pluginName failed (pmsPlugins st)
                            , pmsPendingRestarts = Map.delete pluginName (pmsPendingRestarts st)
                            }
                        , (True, Nothing)
                        )
  where
    (pluginName, generation) = case notice of
      RuntimeRPCFailure name gen _ _ -> (name, gen)
      RuntimeProcessExit name gen _ -> (name, gen)
    rejectStale = do
      consumeStaleNotice notice
      pure (st, (False, Nothing))

failRuntimeNotice :: RuntimeFailureNotice -> LoadedPlugin -> IO LoadedPlugin
failRuntimeNotice notice lp = case notice of
  RuntimeRPCFailure _ _ _ event ->
    failPluginRuntime
      (runtimeRPCErrorCode (rpfeError event))
      (runtimeRPCErrorMessage (rpfeError event))
      lp
  RuntimeProcessExit{} -> failPluginProcessExit lp

noticeMatchesGeneration :: PluginRuntimeGeneration -> LoadedPlugin -> Bool
noticeMatchesGeneration generation lp =
  maybe False ((== generation) . ownedPluginRuntimeGeneration) (lpRuntime lp)

claimNotice :: RuntimeFailureNotice -> LoadedPlugin -> IO Bool
claimNotice notice lp = case notice of
  RuntimeRPCFailure _ _ conn event ->
    case lpConnection lp of
      Just current | sameRPCConnection current conn -> claimRPCFailureEvent conn event
      _ -> pure False
  RuntimeProcessExit _ _ expectedExit -> case lpProcessHandle lp of
    Nothing -> pure False
    Just process -> (== Just expectedExit) <$> ownedPluginProcessExitCode process

consumeStaleNotice :: RuntimeFailureNotice -> IO ()
consumeStaleNotice notice = case notice of
  RuntimeRPCFailure _ _ conn event -> do
    _ <- claimRPCFailureEvent conn event
    pure ()
  RuntimeProcessExit{} -> pure ()

noticeAllowsRestart :: RuntimeFailureNotice -> LoadedPlugin -> IO Bool
noticeAllowsRestart notice lp = case notice of
  RuntimeProcessExit _ _ ExitSuccess -> pure (always || retainedFailure)
  RuntimeProcessExit _ _ (ExitFailure _) -> pure True
  RuntimeRPCFailure{} -> case lpProcessHandle lp of
    Nothing -> pure True
    Just process -> do
      mExit <- ownedPluginProcessExitCode process
      pure $ case mExit of
        Just ExitSuccess -> always
        _ -> True
  where
    always = rspRestartMode (lpStartPolicy lp) == RestartAlways
    retainedFailure = plsErrorCode (lpLifecycle lp) == Just "termination_failed"

scheduleRuntimeRestart
  :: PluginRuntimeGeneration
  -> LoadedPlugin
  -> PluginManagerState
  -> IO (PluginManagerState, Maybe RuntimeRestartDirective)
scheduleRuntimeRestart generation failed st = do
  now <- getCurrentTime
  let (planned, mBackoff) = preparePluginRuntimeRestart now failed
      pluginName = lpName failed
  case mBackoff of
    Nothing -> pure
      ( st
          { pmsPlugins = Map.insert pluginName planned (pmsPlugins st)
          , pmsPendingRestarts = Map.delete pluginName (pmsPendingRestarts st)
          }
      , Nothing
      )
    Just backoff -> do
      let token = pmsNextOperationToken st
          lifecycle = lpLifecycle failed
          operation = PluginRestartOperation
            { proToken = token
            , proGeneration = generation
            , proPhase = PluginRestartBackoff
            , proErrorCode = maybe "runtime_failure" id (plsErrorCode lifecycle)
            , proErrorMessage = maybe "plugin runtime failed" id (plsErrorMessage lifecycle)
            }
          directive = RuntimeRestartDirective pluginName generation token backoff
      pure
        ( st
            { pmsPlugins = Map.insert pluginName failed (pmsPlugins st)
            , pmsPendingRestarts = Map.insert pluginName operation (pmsPendingRestarts st)
            , pmsNextOperationToken = nextOperationToken token
            }
        , Just directive
        )

beginRuntimeRestartBackoffState
  :: RuntimeRestartDirective
  -> PluginManagerState
  -> IO (PluginManagerState, Bool)
beginRuntimeRestartBackoffState directive st
  | isJust (pmsPendingRefresh st) || isJust (pmsPendingShutdown st) = pure (st, False)
  | otherwise = case Map.lookup pluginName (pmsPendingRestarts st) of
      Just operation
        | proToken operation == rrdOperationToken directive
        , proGeneration operation == rrdGeneration directive
        , proPhase operation == PluginRestartBackoff
        , Just lp <- Map.lookup pluginName (pmsPlugins st)
        , isNothing (lpRuntime lp) -> do
            now <- getCurrentTime
            let (starting, mBackoff) = preparePluginRuntimeRestart now lp
            case mBackoff of
              Just _ -> pure
                (st { pmsPlugins = Map.insert pluginName starting (pmsPlugins st) }, True)
              Nothing -> pure
                ( st
                    { pmsPlugins = Map.insert pluginName starting (pmsPlugins st)
                    , pmsPendingRestarts = Map.delete pluginName (pmsPendingRestarts st)
                    }
                , False
                )
      _ -> pure (st, False)
  where
    pluginName = rrdPluginName directive

beginRuntimeRestartLaunchState
  :: RuntimeRestartDirective
  -> PluginManagerState
  -> (PluginManagerState, Maybe LoadedPlugin)
beginRuntimeRestartLaunchState directive st
  | isJust (pmsPendingRefresh st) || isJust (pmsPendingShutdown st) = (st, Nothing)
  | otherwise = case Map.lookup pluginName (pmsPendingRestarts st) of
      Just operation
        | proToken operation == rrdOperationToken directive
        , proGeneration operation == rrdGeneration directive
        , proPhase operation == PluginRestartBackoff
        , Just lp <- Map.lookup pluginName (pmsPlugins st)
        , isNothing (lpRuntime lp)
        , plsState (lpLifecycle lp) == LifecycleStarting ->
            let launching = operation { proPhase = PluginRestartLaunching }
            in (st { pmsPendingRestarts = Map.insert pluginName launching (pmsPendingRestarts st) }, Just lp)
      _ -> (st, Nothing)
  where
    pluginName = rrdPluginName directive

cancelRuntimeRestartState
  :: RuntimeRestartDirective
  -> PluginManagerState
  -> IO (PluginManagerState, Bool)
cancelRuntimeRestartState directive st =
  case Map.lookup pluginName (pmsPendingRestarts st) of
    Just operation
      | proToken operation == rrdOperationToken directive
      , proGeneration operation == rrdGeneration directive -> do
          now <- getCurrentTime
          let plugins' = Map.adjust (markCancelled now operation) pluginName (pmsPlugins st)
          pure
            ( st
                { pmsPlugins = plugins'
                , pmsPendingRestarts = Map.delete pluginName (pmsPendingRestarts st)
                }
            , True
            )
    _ -> pure (st, False)
  where
    pluginName = rrdPluginName directive
    markCancelled now operation lp = lp
      { lpStatus = PluginError (proErrorMessage operation)
      , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
          (Just "plugin restart cancelled")
          (Just (proErrorCode operation))
          (Just (proErrorMessage operation))
          (plsBlockingDependency (lpLifecycle lp))
          Nothing
          (plsProtocolVersion (lpLifecycle lp))
          (plsResources (lpLifecycle lp))
      }

finishRuntimeRestartAttempt
  :: RuntimeRestartDirective
  -> LoadedPlugin
  -> PluginManagerState
  -> IO (PluginManagerState, (Bool, Maybe RuntimeRestartDirective))
finishRuntimeRestartAttempt directive restarted st =
  case Map.lookup pluginName (pmsPendingRestarts st) of
    Just operation
      | proToken operation == rrdOperationToken directive
      , proGeneration operation == rrdGeneration directive
      , proPhase operation == PluginRestartLaunching
      , isNothing (pmsPendingRefresh st)
      , isNothing (pmsPendingShutdown st) -> do
          let resultPlugin = preserveRestartOrigin operation restarted
              accepted = st
                { pmsPlugins = Map.insert pluginName resultPlugin (pmsPlugins st)
                , pmsPendingRestarts = Map.delete pluginName (pmsPendingRestarts st)
                }
          if lpStatus resultPlugin == PluginConnected && isJust (lpConnection resultPlugin)
            then pure (accepted, (True, Nothing))
            else do
              let nextGeneration = maybe (rrdGeneration directive) ownedPluginRuntimeGeneration (lpRuntime resultPlugin)
              (scheduled, nextDirective) <- scheduleRuntimeRestart nextGeneration resultPlugin accepted
              pure (scheduled, (True, nextDirective))
    _ -> pure (st, (False, Nothing))
  where
    pluginName = rrdPluginName directive

preserveRestartOrigin :: PluginRestartOperation -> LoadedPlugin -> LoadedPlugin
preserveRestartOrigin operation lp
  | lpStatus lp == PluginConnected = lp
  | origin `Text.isInfixOf` current = lp
  | otherwise = lp
      { lpStatus = PluginError combined
      , lpLifecycle = (lpLifecycle lp) { plsErrorMessage = Just combined }
      }
  where
    origin = proErrorMessage operation
    current = case lpStatus lp of
      PluginError message -> message
      _ -> maybe "plugin restart failed" id (plsErrorMessage (lpLifecycle lp))
    combined = current <> "; originating failure: " <> origin

runtimeRPCErrorCode :: RPCError -> Text
runtimeRPCErrorCode rpcError = case rpcError of
  RPCTransportError _ -> "transport_error"
  RPCProtocolError _ -> "protocol_error"
  RPCPluginError code _ -> "plugin_error_" <> Text.pack (show code)
  RPCDataResourceError code _ -> Text.pack (show code)
  RPCTimeout _ -> "timeout"

runtimeRPCErrorMessage :: RPCError -> Text
runtimeRPCErrorMessage rpcError = case rpcError of
  RPCTransportError err -> Text.pack (show err)
  RPCProtocolError message -> "RPCProtocolError " <> message
  RPCPluginError _ message -> message
  RPCDataResourceError _ message -> message
  RPCTimeout message -> message

cancelStartingPlugins
  :: Word64
  -> UTCTime
  -> [LoadedPlugin]
  -> PluginManagerState
  -> IO (PluginManagerState, (Bool, [RuntimeRestartDirective]))
cancelStartingPlugins token cancelledAt _plugins st =
  case pmsPendingRefresh st of
    Just operation | pmoToken operation == token -> do
      (cancelledAny, plugins') <- mapAccumM rollbackStartingPlugin False (pmsPlugins st)
      let rolledBack = st
            { pmsPlugins = plugins'
            , pmsPendingRefresh = Nothing
            }
      if pmoInvalidated operation
        then do
          (recovered, directives) <- foldM scheduleFailed (rolledBack, [])
            (Map.toList (pmoFailures operation))
          pure (recovered, (cancelledAny, directives))
        else pure (rolledBack, (cancelledAny, []))
      where
        previous = pmoPlugins operation
        rollbackStartingPlugin cancelled current
          | plsState (lpLifecycle current) /= LifecycleStarting = pure (cancelled, current)
          | Just previousPlugin <- Map.lookup (lpName current) previous = do
              plugin' <- cancelRefreshPlugin cancelledAt previousPlugin
              pure (True, plugin')
          | otherwise = pure (cancelled, current)
        scheduleFailed (currentState, directives) (pluginName, failure)
          | not (prfRestartAllowed failure) = pure (currentState, directives)
          | otherwise = case Map.lookup pluginName (pmsPlugins currentState) of
              Just plugin | plsState (lpLifecycle plugin) == LifecycleFailed -> do
                (nextState, mDirective) <- scheduleRuntimeRestart
                  (prfGeneration failure)
                  plugin
                  currentState
                pure (nextState, directives <> maybe [] pure mDirective)
              _ -> pure (currentState, directives)
    _ -> pure (st, (False, []))

cancelRefreshPlugin :: UTCTime -> LoadedPlugin -> IO LoadedPlugin
cancelRefreshPlugin cancelledAt previousPlugin
  | isNothing (lpConnection previousPlugin) && isNothing (lpProcessHandle previousPlugin) =
      pure previousPlugin
  | otherwise = do
      stopped <- shutdownPlugin previousPlugin
      pure (disconnectPlugin cancelledAt stopped)

cancelStoppingPlugins :: Word64 -> UTCTime -> [LoadedPlugin] -> [LoadedPlugin] -> PluginManagerState -> (PluginManagerState, Bool)
cancelStoppingPlugins token stoppedAt _plugins stoppedPlugins st =
  case pmsPendingShutdown st of
    Just operation | pmoToken operation == token ->
      ( st
          { pmsPlugins = Map.map rollbackOrFinalizeStoppingPlugin (pmsPlugins st)
          , pmsPendingShutdown = Nothing
          }
      , True
      )
      where
        previous = pmoPlugins operation
        stopped = loadedPluginsByName stoppedPlugins
        rollbackOrFinalizeStoppingPlugin current
          | plsState (lpLifecycle current) /= LifecycleStopping = current
          | Just stoppedPlugin <- Map.lookup (lpName current) stopped =
              disconnectPlugin stoppedAt stoppedPlugin
          | otherwise = Map.findWithDefault current (lpName current) previous
    _ -> (st, False)

loadedPluginsByName :: [LoadedPlugin] -> Map Text LoadedPlugin
loadedPluginsByName plugins = Map.fromList [(lpName p, p) | p <- plugins]

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
    Just lp
      | lpStatus lp == PluginConnected && isJust (lpConnection lp) ->
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
markLoadedPluginExternalRefUnavailable targetRef reason lp =
  mapLoadedPluginConnection syncConn lp { lpManifest = manifest' }
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
    (Left err, Just lp)
      | lpStatus lp == PluginConnected && isJust (lpConnection lp) -> do
      lp' <- handlePluginRuntimeFailure errorCode err lp
      pure st { pmsPlugins = Map.insert pluginName lp' (pmsPlugins st) }
    _ -> pure st

