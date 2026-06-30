{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-plugin supervision: manifest refresh, launch, handshake, and
-- shutdown of live runtime sessions.
module Actor.PluginManager.PluginSupervisor
  ( refreshAllManifests
  , withRefreshedManifests
  , refreshOneManifest
  , ensurePluginConnection
  , connectLoadedPlugin
  , observePluginRuntime
  , handlePluginRuntimeFailure
  , shutdownPlugin
  , markPluginStarting
  , markPluginStopping
  , disconnectPlugin
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, mask, onException, try)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.Info (os)
import System.Process (ProcessHandle, getPid, getProcessExitCode)

import Actor.PluginManager.HandshakeSession
  ( PluginHandshakeError(..)
  , performPluginHandshakeWithTimeout
  )
import Actor.PluginManager.ProcessLauncher
  ( launchPluginTransport
  , resolvePluginExecutable
  , safeTerminateProcess
  )
import Actor.PluginManager.Scanner (ManifestLoadFailure(..), loadManifestForHost)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , canRestartPlugin
  , manifestLifecycleResources
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
  ( DependencyExternalDataSourceGrant(..)
  , DependencyExternalDataSourceProvider(..)
  , DependencyProvider(..)
  , DependencyResourceProvider(..)
  , ResourceOperation(..)
  , defaultDependencyResolverInput
  , dependencyStartupOrder
  , driAvailableCapabilities
  , manifestDependencyDecls
  )
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCError(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceStartupDecision(..)
  , RPCExternalDataSourceStatus(..)
  , RPCManifest(..)
  , RPCStartPolicy(..)
  , dataResourceErrorCodeText
  , externalDataSourceManifestStartupDecision
  , newRPCConnection
  , rpcShutdown
  )
import Topo.Plugin.RPC.Transport (Transport, closeTransport)

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
withRefreshedManifests baseDir plugins publish = mask $ \restore -> do
  ownedRef <- newIORef []
  let cleanupOwned = cleanupTrackedRefreshRuntimes ownedRef
  refreshed <- restore (Map.traverseWithKey (\_ lp -> refreshOneManifest baseDir lp) plugins)
    `onException` cleanupOwned
  connected <- restore (connectAndTrackRefreshRuntimes ownedRef refreshed)
    `onException` cleanupOwned
  restore (publish connected) `onException` cleanupOwned

connectAndTrackRefreshRuntimes :: IORef [LoadedPlugin] -> Map Text LoadedPlugin -> IO (Map Text LoadedPlugin)
connectAndTrackRefreshRuntimes ownedRef refreshed = do
  connected <- traverse (connectAndTrackRefreshRuntime ownedRef) (orderLoadedPluginsByDependencies refreshed)
  pure (Map.fromList [(lpName p, p) | p <- connected])

connectAndTrackRefreshRuntime :: IORef [LoadedPlugin] -> LoadedPlugin -> IO LoadedPlugin
connectAndTrackRefreshRuntime ownedRef lp = mask $ \restore -> do
  before <- runtimeIdentity lp
  lp' <- restore (ensurePluginConnection lp)
  after <- runtimeIdentity lp'
  when (refreshOwnsRuntime before after) (modifyIORef' ownedRef (lp':))
  pure lp'

data RuntimeIdentity = RuntimeIdentity
  { riHasConnection :: !Bool
  , riHasProcess :: !Bool
  , riProcessId :: !(Maybe Text)
  } deriving (Eq)

runtimeIdentity :: LoadedPlugin -> IO RuntimeIdentity
runtimeIdentity lp = do
  mPid <- maybe (pure Nothing) processHandleIdText (lpProcessHandle lp)
  pure RuntimeIdentity
    { riHasConnection = isJust (lpConnection lp)
    , riHasProcess = isJust (lpProcessHandle lp)
    , riProcessId = mPid
    }

refreshOwnsRuntime :: RuntimeIdentity -> RuntimeIdentity -> Bool
refreshOwnsRuntime before after = runtimePresent after && before /= after
  where
    runtimePresent identity = riHasConnection identity || riHasProcess identity

cleanupTrackedRefreshRuntimes :: IORef [LoadedPlugin] -> IO ()
cleanupTrackedRefreshRuntimes ownedRef = do
  owned <- readIORef ownedRef
  writeIORef ownedRef []
  mapM_ cleanup owned
  where
    cleanup lp = do
      result <- try @SomeException (shutdownPlugin lp)
      case result of
        Left _ -> pure ()
        Right stopped -> retryTrackedCleanup 1 stopped

retryTrackedCleanup :: Int -> LoadedPlugin -> IO ()
retryTrackedCleanup attemptsLeft lp
  | not (runtimeHandlesPresent lp) = pure ()
  | attemptsLeft <= 0 = pure ()
  | otherwise = do
      result <- try @SomeException (shutdownPlugin lp)
      case result of
        Left _ -> pure ()
        Right stopped -> retryTrackedCleanup (attemptsLeft - 1) stopped

runtimeHandlesPresent :: LoadedPlugin -> Bool
runtimeHandlesPresent lp = isJust (lpConnection lp) || isJust (lpProcessHandle lp)

orderLoadedPluginsByDependencies :: Map Text LoadedPlugin -> [LoadedPlugin]
orderLoadedPluginsByDependencies plugins =
  let loaded = Map.elems plugins
      byName = Map.fromList [(lpName p, p) | p <- loaded]
      providers = map loadedPluginDependencyProvider loaded
      resolverInput = (defaultDependencyResolverInput providers)
        { driAvailableCapabilities = Set.fromList allHostCapabilities
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
  , despStatus = redssState (redsdStatus source)
  , despGrants = map dependencyExternalDataSourceGrant (redsdGrants source)
  }

dependencyExternalDataSourceGrant :: RPCExternalDataSourceGrant -> DependencyExternalDataSourceGrant
dependencyExternalDataSourceGrant grant = DependencyExternalDataSourceGrant
  { desgName = redsgName grant
  , desgAccess = redsgAccess grant
  , desgCapabilities = redsgCapabilities grant
  , desgResources = redsgResources grant
  , desgStatus = redssState (redsgStatus grant)
  }

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
      pure lp
        { lpName = rmName manifest
        , lpManifest = manifest
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
  | otherwise =
      case externalDataSourceManifestStartupDecision (lpManifest lp) of
        ExternalDataSourceStartupBlocked dependency reason ->
          markExternalDataSourceBlocked dependency reason lp
        ExternalDataSourceStartupDegraded dependency reason ->
          markExternalDataSourceDegraded dependency reason lp
        ExternalDataSourceStartupReady -> ensurePluginConnectionAfterExternalGate lp

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
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  | not (rspAutoStart (lpStartPolicy lp)) = do
      stopped <- shutdownPlugin lp
      disabled <- markAutoStartDisabled lp
      pure (preserveRuntimeHandles stopped disabled)
  | otherwise =
      case (lpConnection lp, lpProcessHandle lp, lpStatus lp) of
        (_, Just processHandle, PluginError _) -> do
          alive <- isNothing <$> getProcessExitCode processHandle
          if alive
            then pure lp
            else restartCrashedPlugin lp
        (Just conn, _, _) -> do
          alive <- pluginProcessAlive lp
          if alive
            then do
              now <- getCurrentTime
              mPid <- maybe (pure Nothing) processHandleIdText (lpProcessHandle lp)
              pure lp
                { lpStatus = PluginConnected
                , lpLifecycle = readyLifecycle now (Just "connection already active") mPid conn
                , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
                }
            else restartCrashedPlugin lp
        (Nothing, Nothing, _) -> connectLoadedPlugin lp
        (Nothing, Just processHandle, _) -> do
          alive <- isNothing <$> getProcessExitCode processHandle
          if alive
            then pure lp
            else restartCrashedPlugin lp

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
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
            , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
            }
        Just executablePath -> connectWithRestartPolicy executablePath lp

connectWithRestartPolicy :: FilePath -> LoadedPlugin -> IO LoadedPlugin
connectWithRestartPolicy executablePath lp = do
  attempted <- connectLoadedPluginOnce executablePath lp
  case lpConnection attempted of
    Just _ -> pure attempted
    Nothing -> maybeRestartAfterFailure executablePath attempted

connectLoadedPluginOnce :: FilePath -> LoadedPlugin -> IO LoadedPlugin
connectLoadedPluginOnce executablePath lp = mask $ \restore -> do
  let policy = lpStartPolicy lp
      startupTimeoutMs = rspStartupTimeoutMs policy
      startupTimeoutMicros = policyTimeoutMicros startupTimeoutMs
  launchResult <- restore (launchPluginTransport executablePath (lpDirectory lp) (lpName lp) startupTimeoutMs)
  case launchResult of
    Left (err, mProcessHandle) -> do
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginError err
        , lpLifecycle = failedLifecycle now "launch_failed" err
            (Just (Text.pack executablePath)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = mProcessHandle
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Right (transport, processHandle) ->
      restore (connectLaunchedPlugin lp policy startupTimeoutMicros transport processHandle)
        `onException` cleanupLaunchedPlugin policy transport processHandle

connectLaunchedPlugin :: LoadedPlugin -> RPCStartPolicy -> Int -> Transport -> ProcessHandle -> IO LoadedPlugin
connectLaunchedPlugin lp policy startupTimeoutMicros transport processHandle = do
  mPid <- processHandleIdText processHandle
  let conn = newRPCConnection (lpManifest lp) transport (lpParams lp)
  hsResult <- performPluginHandshakeWithTimeout startupTimeoutMicros conn
  case hsResult of
    Nothing -> do
      terminated <- stopLaunchedPlugin policy transport processHandle
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginError "plugin handshake timed out"
        , lpLifecycle = failedLifecycle now "handshake_timeout" "plugin handshake timed out"
            (Just "handshake") mPid Nothing (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = if terminated then Nothing else Just processHandle
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Just (Right conn') -> do
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginConnected
        , lpLifecycle = readyLifecycle now (Just "handshake complete") mPid conn'
        , lpConnection = Just conn'
        , lpProcessHandle = Just processHandle
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Just (Left err) -> do
      terminated <- stopLaunchedPlugin policy transport processHandle
      now <- getCurrentTime
      let message = handshakeErrorMessage err
      pure lp
        { lpStatus = PluginError message
        , lpLifecycle = failedLifecycle now (handshakeErrorCode err) message
            (Just "handshake") mPid Nothing (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = if terminated then Nothing else Just processHandle
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }

cleanupLaunchedPlugin :: RPCStartPolicy -> Transport -> ProcessHandle -> IO ()
cleanupLaunchedPlugin policy transport processHandle = do
  result <- try @SomeException (stopLaunchedPlugin policy transport processHandle)
  case result of
    Right False -> do
      _ <- try @SomeException (safeTerminateProcess processHandle)
      pure ()
    _ -> pure ()

maybeRestartAfterFailure :: FilePath -> LoadedPlugin -> IO LoadedPlugin
maybeRestartAfterFailure executablePath failedLp =
  case lpProcessHandle failedLp of
    Just _ -> pure failedLp
    Nothing -> do
      let policy = lpStartPolicy failedLp
          failedAt = plsUpdatedAt (lpLifecycle failedLp)
          history = lpRestartHistory failedLp
      if canRestartPlugin policy failedAt history
        then do
          let history' = recordPluginRestart policy failedAt history
              backoffMicros = rspBackoffMs policy * 1000
          when (backoffMicros > 0) (threadDelay backoffMicros)
          connectWithRestartPolicy executablePath failedLp { lpRestartHistory = history' }
        else pure (markRestartLimitIfApplicable failedAt failedLp)

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
    , lpConnection = Nothing
    , lpProcessHandle = Nothing
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
    , lpConnection = Nothing
    , lpProcessHandle = Nothing
    , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
    }

markExternalDataSourceDegraded :: Text -> Text -> LoadedPlugin -> IO LoadedPlugin
markExternalDataSourceDegraded dependency reason lp = do
  stopped <- shutdownPlugin lp
  now <- getCurrentTime
  let message = "external data-source degraded: " <> reason
  pure $ preserveRuntimeHandles stopped lp
    { lpStatus = PluginError message
    , lpLifecycle = pluginLifecycleSnapshot now LifecycleDegraded
        (Just "external data-source degraded")
        (Just "external_data_source_degraded")
        (Just message)
        (Just dependency)
        Nothing
        Nothing
        (manifestLifecycleResources (lpManifest lp))
    , lpConnection = Nothing
    , lpProcessHandle = Nothing
    , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
    }

restartCrashedPlugin :: LoadedPlugin -> IO LoadedPlugin
restartCrashedPlugin lp = do
  terminated <- stopLoadedPluginRuntime lp
  now <- getCurrentTime
  let failed = lp
        { lpStatus = PluginError "plugin process exited"
        , lpLifecycle = failedLifecycle now "process_exited" "plugin process exited"
            (Just "process") (plsProcessId (lpLifecycle lp)) (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpConnection = Nothing
        , lpProcessHandle = if terminated then Nothing else lpProcessHandle lp
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

observePluginRuntime :: LoadedPlugin -> IO LoadedPlugin
observePluginRuntime lp
  | plsState (lpLifecycle lp) `elem` [LifecycleDegraded, LifecycleStopping] = pure lp
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
  terminated <- stopLoadedPluginRuntime lp
  now <- getCurrentTime
  let failed = lp
        { lpStatus = PluginError message
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleFailed
            (Just "plugin runtime failed") (Just errorCode) (Just message)
            Nothing
            (plsProcessId (lpLifecycle lp))
            (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpConnection = Nothing
        , lpProcessHandle = if terminated then Nothing else lpProcessHandle lp
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

stopLaunchedPlugin :: RPCStartPolicy -> Transport -> ProcessHandle -> IO Bool
stopLaunchedPlugin _policy transport processHandle = do
  closeTransportIgnoring transport
  safeTerminateProcess processHandle

stopLoadedPluginRuntime :: LoadedPlugin -> IO Bool
stopLoadedPluginRuntime lp = do
  terminated <- case lpProcessHandle lp of
    Nothing -> pure True
    Just processHandle -> safeTerminateProcess processHandle
  closePluginTransport lp
  pure terminated

closePluginTransport :: LoadedPlugin -> IO ()
closePluginTransport lp =
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> closeTransportIgnoring (rpcTransport conn)

closeTransportIgnoring :: Transport -> IO ()
closeTransportIgnoring transport = do
  _ <- try @SomeException (closeTransport transport)
  pure ()

preserveRuntimeHandles :: LoadedPlugin -> LoadedPlugin -> LoadedPlugin
preserveRuntimeHandles stopped updated = updated
  { lpConnection = lpConnection stopped
  , lpProcessHandle = lpProcessHandle stopped
  }

pluginProcessAlive :: LoadedPlugin -> IO Bool
pluginProcessAlive lp =
  case lpProcessHandle lp of
    Nothing -> pure True
    Just processHandle -> isNothing <$> getProcessExitCode processHandle

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
shutdownPlugin lp = do
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- try @SomeException (rpcShutdown conn)
      pure ()
  terminated <- case lpProcessHandle lp of
    Nothing -> pure True
    Just processHandle -> waitForProcessExitOrTerminate (rspShutdownTimeoutMs (lpStartPolicy lp)) processHandle
  closePluginTransport lp
  pure lp
    { lpConnection = Nothing
    , lpProcessHandle = if terminated then Nothing else lpProcessHandle lp
    }

waitForProcessExitOrTerminate :: Int -> ProcessHandle -> IO Bool
waitForProcessExitOrTerminate timeoutMillis processHandle = do
  exited <- waitForProcessExitPoll (policyTimeoutMicros timeoutMillis) processHandle
  if exited
    then pure True
    else safeTerminateProcess processHandle

waitForProcessExitPoll :: Int -> ProcessHandle -> IO Bool
waitForProcessExitPoll remainingMicros processHandle = do
  mExit <- getProcessExitCode processHandle
  case mExit of
    Just _ -> pure True
    Nothing
      | remainingMicros <= 0 -> pure False
      | otherwise -> do
          let delayMicros = min processPollDelayMicros remainingMicros
          threadDelay delayMicros
          waitForProcessExitPoll (remainingMicros - delayMicros) processHandle

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
disconnectPlugin now lp = lp
  { lpStatus = PluginDisconnected
  , lpLifecycle = pluginLifecycleSnapshot now LifecycleStopped
      (Just "plugin stopped") Nothing Nothing Nothing
      (plsProcessId (lpLifecycle lp))
      (plsProtocolVersion (lpLifecycle lp))
      (plsResources (lpLifecycle lp))
  , lpConnection = lpConnection lp
  , lpProcessHandle = lpProcessHandle lp
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
    , lpConnection = Nothing
    , lpProcessHandle = Nothing
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

processHandleIdText :: ProcessHandle -> IO (Maybe Text)
processHandleIdText processHandle = fmap (Text.pack . show) <$> getPid processHandle

quoteText :: Text -> Text
quoteText value = "'" <> value <> "'"

executableHint :: Text
executableHint
  | os == "mingw32" = " (.exe, .cmd, and .bat wrappers are accepted on Windows)."
  | otherwise = " and mark it executable."

handshakeErrorCode :: PluginHandshakeError -> Text
handshakeErrorCode err = case err of
  PluginHandshakeException _ -> "handshake_exception"
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
