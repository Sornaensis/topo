{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-plugin supervision: manifest refresh, launch, handshake, and
-- shutdown of live runtime sessions.
module Actor.PluginManager.PluginSupervisor
  ( refreshAllManifests
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

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.Info (os)
import System.Process (ProcessHandle, getPid, getProcessExitCode, waitForProcess)
import System.Timeout (timeout)

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
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCError(..)
  , RPCManifest(..)
  , RPCStartPolicy(..)
  , newRPCConnection
  , rpcShutdown
  )
import Topo.Plugin.RPC.Transport (closeTransport)

-- | Re-read manifests for all known plugins, preserving params.
refreshAllManifests :: FilePath -> Map Text LoadedPlugin -> IO (Map Text LoadedPlugin)
refreshAllManifests baseDir plugins = do
  Map.traverseWithKey
    (\_ lp -> do
      refreshed <- refreshOneManifest baseDir lp
      ensurePluginConnection refreshed)
    plugins

-- | Re-read a single plugin's manifest, preserving current params.
refreshOneManifest :: FilePath -> LoadedPlugin -> IO LoadedPlugin
refreshOneManifest _baseDir lp = do
  result <- loadManifestForHost (lpDirectory lp) (lpName lp)
  now <- getCurrentTime
  case result of
    Left failure -> do
      shutdownPlugin lp
      pure $ markPluginManifestLoadFailure now failure lp
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
  | not (requiresRuntimeConnection (lpManifest lp)) = do
      shutdownPlugin lp
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginIdle
        , lpLifecycle = pluginLifecycleSnapshot now LifecycleReady
            (Just "manifest loaded; no runtime connection required") Nothing Nothing Nothing Nothing Nothing
            (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  | not (rspAutoStart (lpStartPolicy lp)) = do
      shutdownPlugin lp
      markAutoStartDisabled lp
  | otherwise =
      case lpConnection lp of
        Just conn -> do
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
        Nothing -> connectLoadedPlugin lp

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
connectLoadedPluginOnce executablePath lp = do
  let policy = lpStartPolicy lp
      startupTimeoutMs = rspStartupTimeoutMs policy
      startupTimeoutMicros = policyTimeoutMicros startupTimeoutMs
  launchResult <- launchPluginTransport executablePath (lpDirectory lp) (lpName lp) startupTimeoutMs
  case launchResult of
    Left err -> do
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginError err
        , lpLifecycle = failedLifecycle now "launch_failed" err
            (Just (Text.pack executablePath)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
        }
    Right (transport, processHandle) -> do
      mPid <- processHandleIdText processHandle
      let conn = newRPCConnection (lpManifest lp) transport (lpParams lp)
      hsResult <- performPluginHandshakeWithTimeout startupTimeoutMicros conn
      case hsResult of
        Nothing -> do
          closeTransport transport
          safeTerminateProcess processHandle
          now <- getCurrentTime
          pure lp
            { lpStatus = PluginError "plugin handshake timed out"
            , lpLifecycle = failedLifecycle now "handshake_timeout" "plugin handshake timed out"
                (Just "handshake") mPid Nothing (manifestLifecycleResources (lpManifest lp))
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
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
          closeTransport transport
          safeTerminateProcess processHandle
          now <- getCurrentTime
          let message = handshakeErrorMessage err
          pure lp
            { lpStatus = PluginError message
            , lpLifecycle = failedLifecycle now (handshakeErrorCode err) message
                (Just "handshake") mPid Nothing (manifestLifecycleResources (lpManifest lp))
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
            , lpRestartHistory = pruneRestartHistory policy now (lpRestartHistory lp)
            }

maybeRestartAfterFailure :: FilePath -> LoadedPlugin -> IO LoadedPlugin
maybeRestartAfterFailure executablePath failedLp = do
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

restartCrashedPlugin :: LoadedPlugin -> IO LoadedPlugin
restartCrashedPlugin lp = do
  closePluginTransportAsync lp
  case lpProcessHandle lp of
    Nothing -> pure ()
    Just processHandle -> waitForProcessExitOrTerminate 1 processHandle
  now <- getCurrentTime
  let failed = lp
        { lpStatus = PluginError "plugin process exited"
        , lpLifecycle = failedLifecycle now "process_exited" "plugin process exited"
            (Just "process") (plsProcessId (lpLifecycle lp)) (plsProtocolVersion (lpLifecycle lp))
            (plsResources (lpLifecycle lp))
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

observePluginRuntime :: LoadedPlugin -> IO LoadedPlugin
observePluginRuntime lp
  | plsState (lpLifecycle lp) `elem` [LifecycleDegraded, LifecycleStopping] = pure lp
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
  closePluginTransportAsync lp
  case lpProcessHandle lp of
    Nothing -> pure ()
    Just processHandle -> safeTerminateProcess processHandle
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
        , lpProcessHandle = Nothing
        , lpRestartHistory = pruneRestartHistory (lpStartPolicy lp) now (lpRestartHistory lp)
        }
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> pure failed
    Just executablePath -> maybeRestartAfterFailure executablePath failed

closePluginTransportAsync :: LoadedPlugin -> IO ()
closePluginTransportAsync lp =
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- forkIO $ do
        _ <- try @SomeException (closeTransport (rpcTransport conn))
        pure ()
      pure ()

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
shutdownPlugin :: LoadedPlugin -> IO ()
shutdownPlugin lp = do
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- try @SomeException (rpcShutdown conn)
      _ <- try @SomeException (closeTransport (rpcTransport conn))
      pure ()
  case lpProcessHandle lp of
    Nothing -> pure ()
    Just processHandle -> waitForProcessExitOrTerminate (rspShutdownTimeoutMs (lpStartPolicy lp)) processHandle

waitForProcessExitOrTerminate :: Int -> ProcessHandle -> IO ()
waitForProcessExitOrTerminate timeoutMillis processHandle = do
  exited <- timeout (policyTimeoutMicros timeoutMillis) (try @SomeException (waitForProcess processHandle))
  case exited of
    Just _ -> pure ()
    Nothing -> safeTerminateProcess processHandle

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
  , lpConnection = Nothing
  , lpProcessHandle = Nothing
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
  RPCTimeout _ -> "timeout"

rpcErrorMessage :: RPCError -> Text
rpcErrorMessage rpcErr = case rpcErr of
  RPCTransportError err -> Text.pack (show err)
  RPCProtocolError msg -> "RPCProtocolError " <> msg
  RPCPluginError _ msg -> msg
  RPCTimeout msg -> msg
