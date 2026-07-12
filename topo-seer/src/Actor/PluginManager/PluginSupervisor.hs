{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-plugin supervision: manifest refresh, launch, handshake, and
-- shutdown of live runtime sessions.
module Actor.PluginManager.PluginSupervisor
  ( refreshAllManifests
  , withRefreshedManifests
  , withRefreshedManifestsHandlingPublishException
  , refreshOneManifest
  , ensurePluginConnection
  , loadedPluginDependencyProvider
  , allHostCapabilities
  , markExternalDataSourceBlocked
  , markExternalDataSourceDegraded
  , connectLoadedPlugin
  , observePluginRuntime
  , handlePluginRuntimeFailure
  , shutdownPlugin
  , finalizeInterruptedPluginCleanup
  , markPluginStarting
  , markPluginStopping
  , disconnectPlugin
  , RefreshRuntimeCleanupFailed
  , refreshRuntimeCleanupOwners
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, mask, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import System.Info (os)

import Actor.PluginManager.HandshakeSession
  ( ExpectedHandshakeCredentials(..)
  , PluginHandshakeError(..)
  , performPluginHandshakeWithTimeout
  )
import Actor.PluginManager.ProcessLauncher
  ( LaunchPluginResult(..)
  , OwnedPluginProcess
  , OwnedPluginRuntime
  , OwnedPluginRuntimeCleanupResult(..)
  , PluginRuntimeGeneration
  , cleanupOwnedPluginRuntime
  , freshPluginRuntimeGeneration
  , launchPluginTransport
  , mapOwnedPluginRuntimeConnection
  , newOwnedPluginRuntime
  , ownedPluginRuntimeConnection
  , ownedPluginRuntimeGeneration
  , ownedPluginRuntimeProcess
  , ownedPluginProcessExitCode
  , ownedPluginProcessId
  , resolvePluginExecutable
  )
import Actor.PluginManager.Scanner (ManifestLoadFailure(..), loadManifestForHost)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , canRestartPlugin
  , lpConnection
  , lpProcessHandle
  , manifestLifecycleResources
  , mapLoadedPluginConnection
  , pluginLifecycleSnapshot
  , policyTimeoutMicros
  , pruneRestartHistory
  , recordPluginRestart
  , requiresRuntimeConnection
  , restartModeAllowsFailure
  )
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource (DataOperations(..), DataResourceSchema(..))
import Topo.Plugin.Dependency
  ( DependencyExternalDataSourceBindingDiagnostic(..)
  , DependencyExternalDataSourceBindingResolution(..)
  , DependencyExternalDataSourceGrant(..)
  , DependencyExternalDataSourceProvider(..)
  , DependencyProvider(..)
  , DependencyResourceProvider(..)
  , ResourceOperation(..)
  , defaultDependencyResolverInput
  , dependencyStartupOrder
  , driAvailableCapabilities
  , driRequireExternalStatusCurrent
  , manifestDependencyDecls
  , resolveExternalDataSourceBindings
  )
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCError(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceStartupDecision(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , RPCStartPolicy(..)
  , dataResourceErrorCodeText
  , externalDataSourceStatusBlocksStartup
  , externalDataSourceStatusDegradesStartup
  , newRPCConnection
  , rpcShutdown
  )
import Topo.Plugin.RPC.Manifest (sanitizeRPCManifestParams)

-- | Re-read manifests for all known plugins, preserving params.
refreshAllManifests :: FilePath -> Map Text LoadedPlugin -> IO (Map Text LoadedPlugin)
refreshAllManifests baseDir plugins =
  withRefreshedManifests baseDir plugins pure

-- | Run a manifest refresh and hand the refreshed state to a publisher.
-- Runtime handles launched during the refresh are owned here until the
-- publisher succeeds, so interrupted refreshes do not strand unpublished
-- subprocesses outside the actor state.
withRefreshedManifests
  :: FilePath
  -> Map Text LoadedPlugin
  -> (Map Text LoadedPlugin -> IO a)
  -> IO a
withRefreshedManifests baseDir plugins publish =
  withRefreshedManifestsHandlingPublishException baseDir plugins publish (\_ _ -> pure True)

-- | Variant of 'withRefreshedManifests' that lets the publisher decide whether
-- newly launched runtimes should still be cleaned up when publish is interrupted.
withRefreshedManifestsHandlingPublishException
  :: FilePath
  -> Map Text LoadedPlugin
  -> (Map Text LoadedPlugin -> IO a)
  -> (Map Text LoadedPlugin -> SomeException -> IO Bool)
     -- ^ Return 'True' when unpublished runtimes are still owned here and must
     -- be cleaned up; return 'False' when the actor has already accepted them.
  -> IO a
withRefreshedManifestsHandlingPublishException baseDir plugins publish handlePublishException = mask $ \restore -> do
  ownedRef <- newIORef []
  let cleanupOwned = do
        retained <- cleanupTrackedRefreshRuntimes ownedRef
        unless (null retained) $ do
          let retainedState = foldr (\lp -> Map.insert (lpName lp) lp) plugins retained
          _ <- try @SomeException (publish retainedState)
          throwIO (RefreshRuntimeCleanupFailed retained)
  refreshed <- restore (Map.traverseWithKey (\_ lp -> refreshOneManifest baseDir lp) plugins)
    `onException` cleanupOwned
  connected <- restore (connectAndTrackRefreshRuntimes ownedRef refreshed)
    `onException` cleanupOwned
  publishResult <- try @SomeException (restore (do
    injectStartupFailure "publication"
    publish connected))
  case publishResult of
    Right value -> pure value
    Left err -> do
      cleanupStillOwned <- publishExceptionCleanupDecision connected err handlePublishException
      when cleanupStillOwned cleanupOwned
      throwIO err

publishExceptionCleanupDecision
  :: Map Text LoadedPlugin
  -> SomeException
  -> (Map Text LoadedPlugin -> SomeException -> IO Bool)
  -> IO Bool
publishExceptionCleanupDecision connected err handlePublishException = do
  decision <- try @SomeException (handlePublishException connected err)
  case decision of
    Right cleanupStillOwned -> pure cleanupStillOwned
    Left _ -> pure True

connectAndTrackRefreshRuntimes :: IORef [LoadedPlugin] -> Map Text LoadedPlugin -> IO (Map Text LoadedPlugin)
connectAndTrackRefreshRuntimes ownedRef refreshed = do
  let startupDecisions = externalDataSourceStartupDecisions refreshed
  connected <- traverse (connectAndTrackRefreshRuntime ownedRef startupDecisions) (orderLoadedPluginsByDependencies refreshed)
  pure (Map.fromList [(lpName p, p) | p <- connected])

connectAndTrackRefreshRuntime :: IORef [LoadedPlugin] -> Map Text RPCExternalDataSourceStartupDecision -> LoadedPlugin -> IO LoadedPlugin
connectAndTrackRefreshRuntime ownedRef startupDecisions lp = mask $ \_ -> do
  let before = runtimeIdentity lp
  let startupDecision = Map.findWithDefault ExternalDataSourceStartupReady (lpName lp) startupDecisions
  lp' <- applyExternalDataSourceStartupDecision startupDecision lp
  let after = runtimeIdentity lp'
  when (refreshOwnsRuntime before after) (modifyIORef' ownedRef (lp':))
  pure lp'

applyExternalDataSourceStartupDecision :: RPCExternalDataSourceStartupDecision -> LoadedPlugin -> IO LoadedPlugin
applyExternalDataSourceStartupDecision decision lp = case decision of
  ExternalDataSourceStartupReady -> ensurePluginConnection lp
  ExternalDataSourceStartupBlocked dependency reason ->
    markExternalDataSourceBlocked dependency reason lp
  ExternalDataSourceStartupDegraded dependency reason -> do
    connected <- ensurePluginConnection lp
    if canMarkExternalDataSourceDegraded connected
      then markExternalDataSourceDegraded dependency reason connected
      else pure connected

canMarkExternalDataSourceDegraded :: LoadedPlugin -> Bool
canMarkExternalDataSourceDegraded lp =
  plsState (lpLifecycle lp) == LifecycleReady || lpStatus lp == PluginConnected

externalDataSourceStartupDecisions :: Map Text LoadedPlugin -> Map Text RPCExternalDataSourceStartupDecision
externalDataSourceStartupDecisions plugins = Map.fromListWith selectExternalDataSourceStartupDecision $
  providerDecisions <> diagnosticDecisions
  where
    providers = map loadedPluginDependencyProvider (Map.elems plugins)
    resolverInput = (defaultDependencyResolverInput providers)
      { driAvailableCapabilities = Set.fromList allHostCapabilities
      , driRequireExternalStatusCurrent = False
      }
    resolution = resolveExternalDataSourceBindings resolverInput
    providerDecisions =
      [ (lpName lp, decision)
      | lp <- Map.elems plugins
      , decision <- externalDataSourceProviderStartupDecisions (lpManifest lp)
      ]
    diagnosticDecisions =
      [ (desbdConsumer diag, externalDataSourceDiagnosticStartupDecision plugins diag)
      | diag <- desbrDiagnostics resolution
      ]

selectExternalDataSourceStartupDecision
  :: RPCExternalDataSourceStartupDecision
  -> RPCExternalDataSourceStartupDecision
  -> RPCExternalDataSourceStartupDecision
selectExternalDataSourceStartupDecision a b
  | externalDataSourceStartupDecisionPriority a >= externalDataSourceStartupDecisionPriority b = a
  | otherwise = b

externalDataSourceStartupDecisionPriority :: RPCExternalDataSourceStartupDecision -> Int
externalDataSourceStartupDecisionPriority ExternalDataSourceStartupReady = 0
externalDataSourceStartupDecisionPriority ExternalDataSourceStartupDegraded{} = 1
externalDataSourceStartupDecisionPriority ExternalDataSourceStartupBlocked{} = 2

externalDataSourceProviderStartupDecisions :: RPCManifest -> [RPCExternalDataSourceStartupDecision]
externalDataSourceProviderStartupDecisions manifest = sourceDecisions <> grantDecisions
  where
    sourceDecisions =
      [ ExternalDataSourceStartupDegraded
          (redsdName source)
          (externalDataSourceStatusReason "external data-source provider declaration is degraded" (redsdName source) (redsdStatus source))
      | source <- rmExternalDataSources manifest
      , externalDataSourceStatusUnavailableOrDegraded (redsdStatus source)
      ]
    grantDecisions =
      [ ExternalDataSourceStartupDegraded
          (redsdName source <> ":" <> redsgName grant)
          (externalDataSourceStatusReason "external data-source grant is degraded" (redsdName source <> ":" <> redsgName grant) (redsgStatus grant))
      | source <- rmExternalDataSources manifest
      , grant <- redsdGrants source
      , externalDataSourceStatusUnavailableOrDegraded (redsgStatus grant)
      ]

externalDataSourceDiagnosticStartupDecision
  :: Map Text LoadedPlugin
  -> DependencyExternalDataSourceBindingDiagnostic
  -> RPCExternalDataSourceStartupDecision
externalDataSourceDiagnosticStartupDecision plugins diag =
  case (desbdRequired diag, externalDataSourceDiagnosticStatusClass plugins diag) of
    (True, Just (ExternalStatusHard reason)) -> ExternalDataSourceStartupBlocked dependency reason
    (True, Just (ExternalStatusSoft reason)) -> ExternalDataSourceStartupDegraded dependency reason
    (True, Nothing) -> ExternalDataSourceStartupBlocked dependency fallbackReason
    (False, Just (ExternalStatusHard reason)) -> ExternalDataSourceStartupDegraded dependency reason
    (False, Just (ExternalStatusSoft reason)) -> ExternalDataSourceStartupDegraded dependency reason
    (False, Nothing) -> ExternalDataSourceStartupDegraded dependency optionalReason
  where
    dependency = externalBindingDiagnosticDependency diag
    fallbackReason = dependency <> ": " <> desbdMessage diag
    optionalReason = "optional external data-source unavailable: " <> fallbackReason

data ExternalStatusClass
  = ExternalStatusHard !Text
  | ExternalStatusSoft !Text

externalDataSourceDiagnosticStatusClass
  :: Map Text LoadedPlugin
  -> DependencyExternalDataSourceBindingDiagnostic
  -> Maybe ExternalStatusClass
externalDataSourceDiagnosticStatusClass plugins diag =
  case hardReasons of
    reason:_ -> Just (ExternalStatusHard reason)
    [] -> case softReasons of
      reason:_ -> Just (ExternalStatusSoft reason)
      [] -> Nothing
  where
    classes = diagnosticCandidateStatusClasses plugins diag
    hardReasons = [reason | ExternalStatusHard reason <- classes]
    softReasons = [reason | ExternalStatusSoft reason <- classes]

externalDataSourceStatusUnavailableOrDegraded :: RPCExternalDataSourceStatus -> Bool
externalDataSourceStatusUnavailableOrDegraded status =
  externalDataSourceStatusBlocksStartup status || externalDataSourceStatusDegradesStartup status

diagnosticCandidateStatusClasses
  :: Map Text LoadedPlugin
  -> DependencyExternalDataSourceBindingDiagnostic
  -> [ExternalStatusClass]
diagnosticCandidateStatusClasses plugins diag = sourceClasses <> grantClasses
  where
    sourceClasses =
      [ statusClass
      | (providerName, provider) <- Map.toList plugins
      , diagnosticProviderMatches providerName
      , source <- rmExternalDataSources (lpManifest provider)
      , redsdName source == desbdSource diag
      , Just statusClass <- [externalDataSourceStatusClass (providerName <> ":" <> redsdName source) (redsdStatus source)]
      ]
    grantClasses =
      [ statusClass
      | (providerName, provider) <- Map.toList plugins
      , diagnosticProviderMatches providerName
      , source <- rmExternalDataSources (lpManifest provider)
      , redsdName source == desbdSource diag
      , grant <- redsdGrants source
      , maybe True (== redsgName grant) (desbdGrant diag)
      , Just statusClass <- [externalDataSourceStatusClass (providerName <> ":" <> redsdName source <> ":" <> redsgName grant) (redsgStatus grant)]
      ]
    diagnosticProviderMatches providerName = maybe True (== providerName) (desbdProvider diag)

externalDataSourceStatusClass :: Text -> RPCExternalDataSourceStatus -> Maybe ExternalStatusClass
externalDataSourceStatusClass dependency status
  | externalDataSourceStatusBlocksStartup status =
      Just (ExternalStatusHard (externalDataSourceStatusReason "external data-source unavailable" dependency status))
  | externalDataSourceStatusDegradesStartup status =
      Just (ExternalStatusSoft (externalDataSourceStatusReason "external data-source degraded" dependency status))
  | otherwise = Nothing

externalDataSourceStatusReason :: Text -> Text -> RPCExternalDataSourceStatus -> Text
externalDataSourceStatusReason prefix dependency status =
  prefix <> ": " <> dependency <> " (" <> externalDataSourceStatusSummary status <> ")"

externalDataSourceStatusSummary :: RPCExternalDataSourceStatus -> Text
externalDataSourceStatusSummary status = Text.intercalate ", " $ filter (not . Text.null)
  [ "state=" <> externalStatusStateLabel (redssState status)
  , maybe "" (("availability=" <>) . externalAvailabilityLabel) (redssAvailability status)
  , maybe "" (("health=" <>) . externalHealthLabel) (redssHealth status)
  , maybe "" (("access_mode=" <>) . externalAccessModeLabel) (redssAccessMode status)
  , maybe "" ("message=" <>) (redssMessage status)
  ]

externalStatusStateLabel :: RPCExternalDataSourceStatusState -> Text
externalStatusStateLabel ExternalStatusUnknown = "unknown"
externalStatusStateLabel ExternalStatusUnconfigured = "unconfigured"
externalStatusStateLabel ExternalStatusReady = "ready"
externalStatusStateLabel ExternalStatusDegraded = "degraded"
externalStatusStateLabel ExternalStatusUnavailable = "unavailable"

externalAvailabilityLabel :: RPCExternalDataSourceAvailability -> Text
externalAvailabilityLabel ExternalAvailabilityUnknown = "unknown"
externalAvailabilityLabel ExternalAvailabilityAvailable = "available"
externalAvailabilityLabel ExternalAvailabilityDegraded = "degraded"
externalAvailabilityLabel ExternalAvailabilityUnavailable = "unavailable"
externalAvailabilityLabel ExternalAvailabilityUnconfigured = "unconfigured"

externalHealthLabel :: RPCExternalDataSourceHealth -> Text
externalHealthLabel ExternalHealthUnknown = "unknown"
externalHealthLabel ExternalHealthHealthy = "healthy"
externalHealthLabel ExternalHealthDegraded = "degraded"
externalHealthLabel ExternalHealthUnhealthy = "unhealthy"

externalAccessModeLabel :: RPCExternalDataSourceAccessMode -> Text
externalAccessModeLabel ExternalAccessModeReadOnly = "read_only"
externalAccessModeLabel ExternalAccessModeReadWrite = "read_write"
externalAccessModeLabel ExternalAccessModeAdmin = "admin"
externalAccessModeLabel ExternalAccessModeDisabled = "disabled"
externalAccessModeLabel ExternalAccessModeProviderManaged = "provider_managed"

externalBindingDiagnosticDependency :: DependencyExternalDataSourceBindingDiagnostic -> Text
externalBindingDiagnosticDependency diag =
  maybe "" (<> ":") (desbdProvider diag)
    <> desbdSource diag
    <> maybe "" (":" <>) (desbdGrant diag)

runtimeIdentity :: LoadedPlugin -> Maybe PluginRuntimeGeneration
runtimeIdentity = fmap ownedPluginRuntimeGeneration . lpRuntime

refreshOwnsRuntime
  :: Maybe PluginRuntimeGeneration
  -> Maybe PluginRuntimeGeneration
  -> Bool
refreshOwnsRuntime before after = isJust after && before /= after

-- A failed unpublished-runtime cleanup remains reachable in the exception
-- rather than being discarded with the refresh-local ownership list.
data RefreshRuntimeCleanupFailed = RefreshRuntimeCleanupFailed ![LoadedPlugin]

refreshRuntimeCleanupOwners :: RefreshRuntimeCleanupFailed -> [LoadedPlugin]
refreshRuntimeCleanupOwners (RefreshRuntimeCleanupFailed plugins) = plugins

instance Show RefreshRuntimeCleanupFailed where
  show (RefreshRuntimeCleanupFailed plugins) =
    "refresh runtime cleanup failed for: " <> show (map lpName plugins)

instance Exception RefreshRuntimeCleanupFailed

cleanupTrackedRefreshRuntimes :: IORef [LoadedPlugin] -> IO [LoadedPlugin]
cleanupTrackedRefreshRuntimes ownedRef = do
  owned <- readIORef ownedRef
  retained <- filter runtimeHandlesPresent <$> mapM cleanup owned
  writeIORef ownedRef retained
  pure retained
  where
    cleanup lp = do
      result <- try @SomeException (shutdownPlugin lp)
      case result of
        Left _ -> pure lp
        Right stopped -> retryTrackedCleanup 1 stopped

retryTrackedCleanup :: Int -> LoadedPlugin -> IO LoadedPlugin
retryTrackedCleanup attemptsLeft lp
  | not (runtimeHandlesPresent lp) = pure lp
  | attemptsLeft <= 0 = pure lp
  | otherwise = do
      result <- try @SomeException (shutdownPlugin lp)
      case result of
        Left _ -> pure lp
        Right stopped -> retryTrackedCleanup (attemptsLeft - 1) stopped

runtimeHandlesPresent :: LoadedPlugin -> Bool
runtimeHandlesPresent = isJust . lpRuntime

orderLoadedPluginsByDependencies :: Map Text LoadedPlugin -> [LoadedPlugin]
orderLoadedPluginsByDependencies plugins =
  let loaded = Map.elems plugins
      byName = Map.fromList [(lpName p, p) | p <- loaded]
      providers = map loadedPluginDependencyProvider loaded
      resolverInput = (defaultDependencyResolverInput providers)
        { driAvailableCapabilities = Set.fromList allHostCapabilities
        , driRequireExternalStatusCurrent = False
        }
      dependencyOrder = dependencyStartupOrder resolverInput
      ordered = [p | name <- dependencyOrder, Just p <- [Map.lookup name byName]]
      remaining = [p | p <- loaded, lpName p `notElem` dependencyOrder]
  in ordered <> remaining

loadedPluginDependencyProvider :: LoadedPlugin -> DependencyProvider
loadedPluginDependencyProvider lp = DependencyProvider
  { dpName = rmName manifest
  , dpVersion = rmVersion manifest
  , dpDependencies = manifestDependencyDecls manifest
  , dpCapabilities = rmCapabilities manifest
  , dpOverlays = maybe [] ((:[]) . osName) (lpOverlaySchema lp)
  , dpResources = map dependencyResourceProvider (rmDataResources manifest)
  , dpExternalDataSources = map dependencyExternalDataSourceProvider (rmExternalDataSources manifest)
  }
  where
    manifest = lpManifest lp

allHostCapabilities :: [Capability]
allHostCapabilities =
  [ CapLog
  , CapNoise
  , CapReadTerrain
  , CapWriteTerrain
  , CapReadOverlay
  , CapWriteOverlay
  , CapReadWorld
  , CapWriteWorld
  , CapDataRead
  , CapDataWrite
  ]

dependencyResourceProvider :: DataResourceSchema -> DependencyResourceProvider
dependencyResourceProvider resource = DependencyResourceProvider
  { drpName = drsName resource
  , drpOperations = dependencyResourceOperations (drsOperations resource)
  , drpOverlay = drsOverlay resource
  }

dependencyResourceOperations :: DataOperations -> [ResourceOperation]
dependencyResourceOperations ops = concat
  [ [ResourceList | doList ops]
  , [ResourceGet | doGet ops]
  , [ResourceCreate | doCreate ops]
  , [ResourceUpdate | doUpdate ops]
  , [ResourceDelete | doDelete ops]
  , [ResourceQueryByHex | doQueryByHex ops]
  , [ResourceQueryByField | doQueryByField ops]
  , [ResourceSort | doSort ops]
  , [ResourceFilter | doFilter ops]
  , [ResourcePage | doPage ops]
  ]

dependencyExternalDataSourceProvider :: RPCExternalDataSourceDecl -> DependencyExternalDataSourceProvider
dependencyExternalDataSourceProvider source = DependencyExternalDataSourceProvider
  { despName = redsdName source
  , despCapabilities = redsdCapabilities source
  , despResources = redsdResources source
  , despStatus = redssState status
  , despObservedAt = redssObservedAt status
  , despFresh = redssFresh status
  , despBrokerable = externalDataSourceStatusReady status
  , despGrants = map dependencyExternalDataSourceGrant (redsdGrants source)
  }
  where
    status = redsdStatus source

dependencyExternalDataSourceGrant :: RPCExternalDataSourceGrant -> DependencyExternalDataSourceGrant
dependencyExternalDataSourceGrant grant = DependencyExternalDataSourceGrant
  { desgName = redsgName grant
  , desgAccess = redsgAccess grant
  , desgCapabilities = redsgCapabilities grant
  , desgResources = redsgResources grant
  , desgStatus = redssState status
  , desgObservedAt = redssObservedAt status
  , desgFresh = redssFresh status
  , desgBrokerable = externalDataSourceStatusReady status
  }
  where
    status = redsgStatus grant

externalDataSourceStatusReady :: RPCExternalDataSourceStatus -> Bool
externalDataSourceStatusReady status =
  redssState status == ExternalStatusReady
    && redssAvailability status `notElem`
      [ Just ExternalAvailabilityUnknown
      , Just ExternalAvailabilityUnconfigured
      , Just ExternalAvailabilityDegraded
      , Just ExternalAvailabilityUnavailable
      ]
    && redssHealth status `notElem` [Just ExternalHealthDegraded, Just ExternalHealthUnhealthy]
    && redssAccessMode status /= Just ExternalAccessModeDisabled

-- | Re-read a single plugin's manifest, preserving current params.
refreshOneManifest :: FilePath -> LoadedPlugin -> IO LoadedPlugin
refreshOneManifest _baseDir lp = do
  result <- loadManifestForHost (lpDirectory lp) (lpName lp)
  now <- getCurrentTime
  case result of
    Left failure -> do
      stopped <- shutdownPlugin lp
      pure $ preserveRuntimeHandles stopped (markPluginManifestLoadFailure now failure lp)
    Right (manifest, overlaySchema) -> do
      let sanitizedParams = sanitizeRPCManifestParams manifest (lpParams lp)
      pure $ mapLoadedPluginConnection
        (\conn -> conn { rpcManifest = manifest, rpcParams = sanitizedParams }) lp
        { lpName = rmName manifest
        , lpManifest = manifest
        , lpParams = sanitizedParams
        , lpStartPolicy = rmStartPolicy manifest
        , lpOverlaySchema = overlaySchema
        , lpRestartHistory = pruneRestartHistory (rmStartPolicy manifest) now (lpRestartHistory lp)
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleDiscovered
            (Just "manifest refreshed") Nothing Nothing Nothing Nothing Nothing
            (manifestLifecycleResources manifest)
        }

ensurePluginConnection :: LoadedPlugin -> IO LoadedPlugin
ensurePluginConnection lp
  | plsState (lpLifecycle lp) == LifecycleDegraded = pure lp
  | otherwise = ensurePluginConnectionAfterExternalGate lp

ensurePluginConnectionAfterExternalGate :: LoadedPlugin -> IO LoadedPlugin
ensurePluginConnectionAfterExternalGate lp
  | not (requiresRuntimeConnection (lpManifest lp)) = do
      stopped <- shutdownPlugin lp
      now <- getCurrentTime
      pure $ preserveRuntimeHandles stopped lp
        { lpStatus = PluginIdle
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleReady
            (Just "manifest loaded; no runtime connection required") Nothing Nothing Nothing Nothing Nothing
            (manifestLifecycleResources (lpManifest lp))
        , lpRuntime = Nothing
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  | not (rspAutoStart (lpStartPolicy lp)) = do
      stopped <- shutdownPlugin lp
      disabled <- markAutoStartDisabled lp
      pure (preserveRuntimeHandles stopped disabled)
  | otherwise =
      case (lpConnection lp, lpProcessHandle lp, lpStatus lp) of
        (_, Just processHandle, PluginError _) -> do
          alive <- isNothing <$> ownedPluginProcessExitCode processHandle
          if alive
            then pure lp
            else restartCrashedPlugin lp
        (Just conn, _, _) -> do
          alive <- pluginProcessAlive lp
          if alive
            then do
              now <- getCurrentTime
              mPid <- maybe (pure Nothing) processHandleIdText (lpProcessHandle lp)
              let conn' = syncConnectionForPlugin lp conn
              pure $ mapLoadedPluginConnection (const conn') lp
                { lpStatus = PluginConnected
                , lpLifecycle = readyLifecycle now (Just "connection already active") mPid conn'
                , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
                }
            else restartCrashedPlugin lp
        (Nothing, Nothing, _) -> connectLoadedPlugin lp
        (Nothing, Just processHandle, _) -> do
          alive <- isNothing <$> ownedPluginProcessExitCode processHandle
          if alive
            then pure lp
            else restartCrashedPlugin lp

syncConnectionForPlugin :: LoadedPlugin -> RPCConnection -> RPCConnection
syncConnectionForPlugin lp conn = conn
  { rpcManifest = lpManifest lp
  , rpcParams = sanitizeRPCManifestParams (lpManifest lp) (lpParams lp)
  , rpcRequestTimeoutMicros = Just (policyTimeoutMicros (rspRequestTimeoutMs (lpStartPolicy lp)))
  }

connectLoadedPlugin :: LoadedPlugin -> IO LoadedPlugin
connectLoadedPlugin lp
  | not (rspAutoStart (lpStartPolicy lp)) = markAutoStartDisabled lp
  | otherwise = do
      mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
      case mExecutable of
        Nothing -> do
          now <- getCurrentTime
          let message = "plugin executable not found; expected an executable named "
                <> quoteText (lpName lp) <> " in " <> Text.pack (lpDirectory lp)
                <> executableHint
          pure lp
            { lpStatus = PluginError message
            , lpLifecycle = failedLifecycle now "executable_not_found" message
                (Just (lpName lp)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
            , lpRuntime = Nothing
            , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
            }
        Just executablePath -> connectWithRestartPolicy executablePath lp

connectWithRestartPolicy :: FilePath -> LoadedPlugin -> IO LoadedPlugin
connectWithRestartPolicy executablePath lp = do
  attempted <- connectLoadedPluginOnce executablePath lp
  case (lpStatus attempted, lpConnection attempted) of
    (PluginConnected, Just _) -> pure attempted
    _ -> maybeRestartAfterFailure executablePath attempted

connectLoadedPluginOnce :: FilePath -> LoadedPlugin -> IO LoadedPlugin
connectLoadedPluginOnce executablePath lp = mask $ \restore -> do
  generation <- freshPluginRuntimeGeneration
  let policy = lpStartPolicy lp
      startupTimeoutMs = rspStartupTimeoutMs policy
      startupTimeoutMicros = policyTimeoutMicros startupTimeoutMs
  launchResult <- launchPluginTransport executablePath (lpDirectory lp) (lpName lp) startupTimeoutMs
  case launchResult of
    Left (err, mProcessHandle) -> do
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginError err
        , lpLifecycle = failedLifecycle now "launch_failed" err
            (Just (Text.pack executablePath)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
        , lpRuntime = newOwnedPluginRuntime generation Nothing <$> mProcessHandle
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Right launch -> do
      let initialConnection = newRPCConnection (lpManifest lp) (lprTransport launch) (lpParams lp)
          runtime = newOwnedPluginRuntime generation (Just initialConnection) (lprOwnedProcess launch)
      connectResult <- try @SomeException
        (restore (connectLaunchedPlugin lp policy startupTimeoutMicros runtime launch))
      case connectResult of
        Right connected -> pure connected
        Left err -> do
          cleanupResult <- cleanupOwnedPluginRuntime runtime
          case cleanupResult of
            OwnedPluginRuntimeCleanupComplete -> throwIO err
            OwnedPluginRuntimeCleanupFailed retained -> do
              now <- getCurrentTime
              let message = "plugin cleanup failed after interrupted launch: " <> Text.pack (show err)
              pure lp
                { lpStatus = PluginError message
                , lpLifecycle = failedLifecycle now "termination_failed" message
                    (Just "process")
                    (Text.pack . show <$> (ownedPluginRuntimeProcess retained >>= ownedPluginProcessId))
                    Nothing
                    (manifestLifecycleResources (lpManifest lp))
                , lpRuntime = Just retained
                , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
                }

connectLaunchedPlugin
  :: LoadedPlugin
  -> RPCStartPolicy
  -> Int
  -> OwnedPluginRuntime
  -> LaunchPluginResult
  -> IO LoadedPlugin
connectLaunchedPlugin lp policy startupTimeoutMicros runtime launch = do
  let processHandle = lprOwnedProcess launch
      expectedCredentials = ExpectedHandshakeCredentials
        { ehcSessionId = lprSessionId launch
        , ehcAuthToken = lprAuthToken launch
        }
      conn = case ownedPluginRuntimeConnection runtime of
        Just connection -> connection
        Nothing -> error "accepted plugin runtime is missing its RPC connection"
  mPid <- processHandleIdText processHandle
  injectStartupFailure "handshake"
  hsResult <- performPluginHandshakeWithTimeout startupTimeoutMicros (Just expectedCredentials) conn
  case hsResult of
    Nothing -> startupHandshakeFailure policy runtime lp
      "handshake_timeout" "plugin handshake timed out" mPid
    Just (Right conn') -> do
      injectStartupFailure "prepublication"
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginConnected
        , lpLifecycle = readyLifecycle now (Just "handshake complete") mPid conn'
        , lpRuntime = Just (mapOwnedPluginRuntimeConnection (const conn') runtime)
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Just (Left err) -> startupHandshakeFailure policy runtime lp
      (handshakeErrorCode err) (handshakeErrorMessage err) mPid

startupHandshakeFailure
  :: RPCStartPolicy
  -> OwnedPluginRuntime
  -> LoadedPlugin
  -> Text
  -> Text
  -> Maybe Text
  -> IO LoadedPlugin
startupHandshakeFailure policy runtime lp errorCode message mPid = do
  cleanupResult <- cleanupOwnedPluginRuntime runtime
  now <- getCurrentTime
  pure lp
    { lpStatus = PluginError message
    , lpLifecycle = failedLifecycle now errorCode message
        (Just "handshake") mPid Nothing (manifestLifecycleResources (lpManifest lp))
    , lpRuntime = case cleanupResult of
        OwnedPluginRuntimeCleanupComplete -> Nothing
        OwnedPluginRuntimeCleanupFailed retained -> Just retained
    , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
    }

maybeRestartAfterFailure :: FilePath -> LoadedPlugin -> IO LoadedPlugin
maybeRestartAfterFailure executablePath failedLp = do
  failedLp' <- clearExitedProcessHandle failedLp
  case lpProcessHandle failedLp' of
    Just _ -> do
      now <- getCurrentTime
      pure (markTerminationFailed now "plugin termination failed after startup failure" failedLp')
    Nothing -> do
      let policy = lpStartPolicy failedLp'
          failedAt = plsUpdatedAt (lpLifecycle failedLp')
          history = lpRestartHistory failedLp'
      if canRestartPlugin policy failedAt history
        then do
          let history' = recordPluginRestart policy failedAt history
              backoffMicros = rspBackoffMs policy * 1000
          when (backoffMicros > 0) (threadDelay backoffMicros)
          connectWithRestartPolicy executablePath failedLp' { lpRestartHistory = history' }
        else pure (markRestartLimitIfApplicable failedAt failedLp')

clearExitedProcessHandle :: LoadedPlugin -> IO LoadedPlugin
clearExitedProcessHandle lp =
  case lpRuntime lp of
    Nothing -> pure lp
    Just runtime -> case ownedPluginRuntimeProcess runtime of
      Nothing -> pure lp
      Just ownedProcess -> do
        alive <- isNothing <$> ownedPluginProcessExitCode ownedProcess
        if alive
          then pure lp
          else do
            retained <- cleanupRuntimeRetained runtime
            pure lp { lpRuntime = retained }

markTerminationFailed :: UTCTime -> Text -> LoadedPlugin -> LoadedPlugin
markTerminationFailed now context lp
  | plsErrorCode (lpLifecycle lp) == Just "termination_failed" = lp
  | otherwise = lp
      { lpStatus = PluginError message
      , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
          (Just "plugin termination failed") (Just "termination_failed") (Just message)
          (plsBlockingDependency (lpLifecycle lp))
          (plsProcessId (lpLifecycle lp))
          (plsProtocolVersion (lpLifecycle lp))
          (plsResources (lpLifecycle lp))
      }
  where
    message = case previousErrorMessage lp of
      Nothing -> context
      Just previous -> context <> "; previous error: " <> previous

previousErrorMessage :: LoadedPlugin -> Maybe Text
previousErrorMessage lp = case lpStatus lp of
  PluginError message -> Just message
  _ -> plsErrorMessage (lpLifecycle lp)

markRestartLimitIfApplicable :: UTCTime -> LoadedPlugin -> LoadedPlugin
markRestartLimitIfApplicable now lp
  | restartModeAllowsFailure (rspRestartMode (lpStartPolicy lp))
      && length (pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)) >= rspMaxRestarts (lpStartPolicy lp) =
      let previousMessage = case lpStatus lp of
            PluginError msg -> msg
            _ -> maybe "plugin failed" id (plsErrorMessage (lpLifecycle lp))
          message = "plugin restart limit exceeded: " <> previousMessage
      in lp
        { lpStatus = PluginError message
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
            (Just "plugin restart limit exceeded") (Just "restart_limit_exceeded") (Just message)
            (plsBlockingDependency (lpLifecycle lp))
            (plsProcessId (lpLifecycle lp))
            (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  | otherwise = lp

markAutoStartDisabled :: LoadedPlugin -> IO LoadedPlugin
markAutoStartDisabled lp = do
  now <- getCurrentTime
  pure lp
    { lpStatus = PluginIdle
    , lpLifecycle = pluginLifecycleSnapshot now LifecycleStopped
        (Just "auto-start disabled by plugin policy") Nothing Nothing Nothing Nothing Nothing
        (manifestLifecycleResources (lpManifest lp))
    , lpRuntime = Nothing
    , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
    }

markExternalDataSourceBlocked :: Text -> Text -> LoadedPlugin -> IO LoadedPlugin
markExternalDataSourceBlocked dependency reason lp = do
  stopped <- shutdownPlugin lp
  now <- getCurrentTime
  let message = "external data-source startup blocked: " <> reason
  pure $ preserveRuntimeHandles stopped lp
    { lpStatus = PluginError message
    , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
        (Just "required external data source unavailable")
        (Just "external_data_source_blocked")
        (Just message)
        (Just dependency)
        Nothing
        Nothing
        (manifestLifecycleResources (lpManifest lp))
    , lpRuntime = Nothing
    , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
    }

markExternalDataSourceDegraded :: Text -> Text -> LoadedPlugin -> IO LoadedPlugin
markExternalDataSourceDegraded dependency reason lp = do
  now <- getCurrentTime
  let message = "external data-source degraded: " <> reason
      status
        | isJust (lpConnection lp) = PluginConnected
        | otherwise = PluginError message
  pure lp
    { lpStatus = status
    , lpLifecycle = pluginLifecycleSnapshot now LifecycleDegraded
        (Just "external data-source degraded")
        (Just "external_data_source_degraded")
        (Just message)
        (Just dependency)
        (plsProcessId (lpLifecycle lp))
        (plsProtocolVersion (lpLifecycle lp))
        (manifestLifecycleResources (lpManifest lp))
    , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
    }

restartCrashedPlugin :: LoadedPlugin -> IO LoadedPlugin
restartCrashedPlugin lp = do
  retained <- cleanupLoadedPluginRuntime lp
  now <- getCurrentTime
  let failed = lp
        { lpStatus = PluginError "plugin process exited"
        , lpLifecycle = failedLifecycle now "process_exited" "plugin process exited"
            (Just "process") (plsProcessId (lpLifecycle lp)) (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpRuntime = retained
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

observePluginRuntime :: LoadedPlugin -> IO LoadedPlugin
observePluginRuntime lp
  | plsState (lpLifecycle lp) == LifecycleStopping = pure lp
  | plsState (lpLifecycle lp) == LifecycleDegraded && lpStatus lp /= PluginConnected = pure lp
  | lpStatus lp /= PluginConnected = pure lp
  | otherwise = case lpConnection lp of
      Nothing -> pure lp
      Just conn -> do
        mRuntimeFailure <- readIORef (rpcRuntimeFailure conn)
        case mRuntimeFailure of
          Just rpcErr -> do
            writeIORef (rpcRuntimeFailure conn) Nothing
            handlePluginRuntimeFailure (rpcErrorCode rpcErr) (rpcErrorMessage rpcErr) lp
          Nothing -> do
            alive <- pluginProcessAlive lp
            if alive
              then pure lp
              else restartCrashedPlugin lp

handlePluginRuntimeFailure :: Text -> Text -> LoadedPlugin -> IO LoadedPlugin
handlePluginRuntimeFailure errorCode message lp = do
  retained <- cleanupLoadedPluginRuntime lp
  now <- getCurrentTime
  let failed = lp
        { lpStatus = PluginError message
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
            (Just "plugin runtime failed") (Just errorCode) (Just message)
            Nothing
            (plsProcessId (lpLifecycle lp))
            (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpRuntime = retained
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

cleanupRuntimeRetained :: OwnedPluginRuntime -> IO (Maybe OwnedPluginRuntime)
cleanupRuntimeRetained runtime = do
  result <- cleanupOwnedPluginRuntime runtime
  pure $ case result of
    OwnedPluginRuntimeCleanupComplete -> Nothing
    OwnedPluginRuntimeCleanupFailed retained -> Just retained

cleanupLoadedPluginRuntime :: LoadedPlugin -> IO (Maybe OwnedPluginRuntime)
cleanupLoadedPluginRuntime lp = case lpRuntime lp of
  Nothing -> pure Nothing
  Just runtime -> cleanupRuntimeRetained runtime

-- | Finish destructive cleanup after cancellation without allowing the same
-- pending asynchronous exception to interrupt the bounded teardown again.
finalizeInterruptedPluginCleanup :: LoadedPlugin -> IO LoadedPlugin
finalizeInterruptedPluginCleanup lp = uninterruptibleMask_ $ do
  retained <- cleanupLoadedPluginRuntime lp
  let finalized = lp { lpRuntime = retained }
  case retained of
    Nothing -> pure finalized
    Just _ -> do
      now <- getCurrentTime
      pure (markTerminationFailed now "plugin termination failed after interrupted shutdown" finalized)

preserveRuntimeHandles :: LoadedPlugin -> LoadedPlugin -> LoadedPlugin
preserveRuntimeHandles stopped updated = case lpRuntime stopped of
  Just runtime -> updated
    { lpStatus = lpStatus stopped
    , lpLifecycle = lpLifecycle stopped
    , lpRuntime = Just runtime
    }
  Nothing -> updated { lpRuntime = Nothing }

pluginProcessAlive :: LoadedPlugin -> IO Bool
pluginProcessAlive lp =
  case lpProcessHandle lp of
    Nothing -> pure True
    Just ownedProcess -> isNothing <$> ownedPluginProcessExitCode ownedProcess

markPluginStarting :: UTCTime -> LoadedPlugin -> LoadedPlugin
markPluginStarting now lp = lp
  { lpLifecycle = pluginLifecycleSnapshot now LifecycleStarting
      (Just "refresh requested") Nothing Nothing Nothing
      (plsProcessId (lpLifecycle lp))
      (plsProtocolVersion (lpLifecycle lp))
      (plsResources (lpLifecycle lp))
  }

-- | Shut down a single plugin's RPC connection.
shutdownPlugin :: LoadedPlugin -> IO LoadedPlugin
shutdownPlugin lp = mask $ \restore -> do
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- try @SomeException (restore (rpcShutdown conn))
      pure ()
  retained <- case lpRuntime lp of
    Nothing -> pure Nothing
    Just runtime -> do
      case ownedPluginRuntimeProcess runtime of
        Nothing -> pure ()
        Just ownedProcess -> do
          _ <- restore (waitForProcessExitPoll
            (policyTimeoutMicros (rspShutdownTimeoutMs (lpStartPolicy lp)))
            ownedProcess)
          pure ()
      cleanupRuntimeRetained runtime
  let stopped = lp { lpRuntime = retained }
  case retained of
    Nothing -> pure stopped
    Just _ -> do
      now <- getCurrentTime
      pure (markTerminationFailed now "plugin termination failed during shutdown" stopped)

waitForProcessExitPoll :: Int -> OwnedPluginProcess -> IO Bool
waitForProcessExitPoll remainingMicros ownedProcess = do
  mExit <- ownedPluginProcessExitCode ownedProcess
  case mExit of
    Just _ -> pure True
    Nothing
      | remainingMicros <= 0 -> pure False
      | otherwise -> do
          let delayMicros = min processPollDelayMicros remainingMicros
          threadDelay delayMicros
          waitForProcessExitPoll (remainingMicros - delayMicros) ownedProcess

processPollDelayMicros :: Int
processPollDelayMicros = 10000

markPluginStopping :: UTCTime -> LoadedPlugin -> LoadedPlugin
markPluginStopping now lp = lp
  { lpLifecycle = pluginLifecycleSnapshot now LifecycleStopping
      (Just "shutdown requested") Nothing Nothing Nothing
      (plsProcessId (lpLifecycle lp))
      (plsProtocolVersion (lpLifecycle lp))
      (plsResources (lpLifecycle lp))
  }

-- | Mark a plugin as disconnected.
disconnectPlugin :: UTCTime -> LoadedPlugin -> LoadedPlugin
disconnectPlugin now lp
  | isJust (lpRuntime lp) =
      markTerminationFailed now "plugin termination failed during shutdown" lp
  | otherwise = lp
      { lpStatus = PluginDisconnected
      , lpLifecycle = pluginLifecycleSnapshot now LifecycleStopped
          (Just "plugin stopped") Nothing Nothing Nothing
          (plsProcessId (lpLifecycle lp))
          (plsProtocolVersion (lpLifecycle lp))
          (plsResources (lpLifecycle lp))
      , lpRuntime = Nothing
      }

markPluginManifestLoadFailure :: UTCTime -> ManifestLoadFailure -> LoadedPlugin -> LoadedPlugin
markPluginManifestLoadFailure now failure lp =
  let manifest = mlfDiagnosticManifest failure
      message = mlfErrorMessage failure
  in lp
    { lpName = rmName manifest
    , lpManifest = manifest
    , lpStatus = PluginError message
    , lpLifecycle = pluginLifecycleSnapshot now LifecycleDegraded
        (Just "manifest refresh failed") (Just (mlfErrorCode failure)) (Just message) Nothing
        Nothing Nothing (manifestLifecycleResources manifest)
    , lpRuntime = Nothing
    , lpStartPolicy = rmStartPolicy manifest
    , lpRestartHistory = pruneRestartHistory (rmStartPolicy manifest) now (lpRestartHistory lp)
    , lpOverlaySchema = Nothing
    }

readyLifecycle :: UTCTime -> Maybe Text -> Maybe Text -> RPCConnection -> PluginLifecycleSnapshot
readyLifecycle now reason mPid conn =
  pluginLifecycleSnapshot now LifecycleReady reason Nothing Nothing Nothing mPid
    (Just (rpcProtocolVersion conn))
    (connectionLifecycleResources conn)

failedLifecycle
  :: UTCTime
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> [Text]
  -> PluginLifecycleSnapshot
failedLifecycle now errorCode message blockingDependency mPid mProtocol resources =
  pluginLifecycleSnapshot now LifecycleFailed
    (Just "plugin startup failed") (Just errorCode) (Just message) blockingDependency mPid mProtocol resources

connectionLifecycleResources :: RPCConnection -> [Text]
connectionLifecycleResources conn =
  let negotiated = map drsName (rpcResources conn)
  in if null negotiated
       then manifestLifecycleResources (rpcManifest conn)
       else negotiated

processHandleIdText :: OwnedPluginProcess -> IO (Maybe Text)
processHandleIdText ownedProcess =
  pure (Text.pack . show <$> ownedPluginProcessId ownedProcess)

quoteText :: Text -> Text
quoteText value = "'" <> value <> "'"

injectStartupFailure :: String -> IO ()
injectStartupFailure phase = do
  injected <- (== Just phase) <$> lookupEnv "TOPO_TEST_PLUGIN_STARTUP_FAILURE"
  when injected (throwIO (userError ("forced plugin startup failure at " <> phase)))

executableHint :: Text
executableHint
  | os == "mingw32" = " (.exe plugins are accepted on Windows)."
  | otherwise = " and mark it executable."

handshakeErrorCode :: PluginHandshakeError -> Text
handshakeErrorCode err = case err of
  PluginHandshakeException _ -> "handshake_exception"
  PluginHandshakeRPC (RPCTimeout _) -> "handshake_timeout"
  PluginHandshakeRPC rpcErr -> rpcErrorCode rpcErr

handshakeErrorMessage :: PluginHandshakeError -> Text
handshakeErrorMessage err = case err of
  PluginHandshakeException msg -> msg
  PluginHandshakeRPC rpcErr -> rpcErrorMessage rpcErr

rpcErrorCode :: RPCError -> Text
rpcErrorCode rpcErr = case rpcErr of
  RPCTransportError _ -> "transport_error"
  RPCProtocolError _ -> "protocol_error"
  RPCPluginError code _ -> "plugin_error_" <> Text.pack (show code)
  RPCDataResourceError code _ -> dataResourceErrorCodeText code
  RPCTimeout _ -> "timeout"

rpcErrorMessage :: RPCError -> Text
rpcErrorMessage rpcErr = case rpcErr of
  RPCTransportError err -> Text.pack (show err)
  RPCProtocolError msg -> "RPCProtocolError " <> msg
  RPCPluginError _ msg -> msg
  RPCDataResourceError _ msg -> msg
  RPCTimeout msg -> msg
