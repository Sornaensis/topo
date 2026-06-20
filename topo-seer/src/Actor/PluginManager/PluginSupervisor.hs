{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-plugin supervision: manifest refresh, launch, handshake, and
-- shutdown of live runtime sessions.
module Actor.PluginManager.PluginSupervisor
  ( refreshAllManifests
  , refreshOneManifest
  , ensurePluginConnection
  , connectLoadedPlugin
  , shutdownPlugin
  , markPluginStarting
  , markPluginStopping
  , disconnectPlugin
  ) where

import Control.Exception (SomeException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.FilePath ((</>))
import System.Process (ProcessHandle, getPid)

import Actor.PluginManager.HandshakeSession
  ( PluginHandshakeError(..)
  , performPluginHandshakeWithTimeout
  )
import Actor.PluginManager.ProcessLauncher
  ( launchPluginTransport
  , resolvePluginExecutable
  , safeTerminateProcess
  )
import Actor.PluginManager.Scanner (loadOverlaySchema)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , manifestLifecycleResources
  , pluginLifecycleSnapshot
  , requiresRuntimeConnection
  )
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCError(..)
  , newRPCConnection
  , parseManifestFile
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
  let manifestPath = lpDirectory lp </> "manifest.json"
  result <- try @SomeException (parseManifestFile manifestPath)
  now <- getCurrentTime
  case result of
    Left _ -> pure $ markPluginDegraded now "manifest_read_failed" "manifest read failed" lp
    Right (Left err) -> pure $ markPluginDegraded now "manifest_parse_failed" (Text.pack (show err)) lp
    Right (Right manifest) -> do
      overlaySchema <- loadOverlaySchema (lpDirectory lp) manifest
      pure lp
        { lpManifest = manifest
        , lpOverlaySchema = overlaySchema
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
        }
  | otherwise =
      case lpConnection lp of
        Just conn -> do
          now <- getCurrentTime
          mPid <- maybe (pure Nothing) processHandleIdText (lpProcessHandle lp)
          pure lp
            { lpStatus = PluginConnected
            , lpLifecycle = readyLifecycle now (Just "connection already active") mPid conn
            }
        Nothing -> connectLoadedPlugin lp

connectLoadedPlugin :: LoadedPlugin -> IO LoadedPlugin
connectLoadedPlugin lp = do
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing -> do
      now <- getCurrentTime
      pure lp
        { lpStatus = PluginError "plugin executable not found"
        , lpLifecycle = failedLifecycle now "executable_not_found" "plugin executable not found"
            (Just (lpName lp)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        }
    Just executablePath -> do
      launchResult <- launchPluginTransport executablePath (lpDirectory lp) (lpName lp)
      case launchResult of
        Left err -> do
          now <- getCurrentTime
          pure lp
            { lpStatus = PluginError err
            , lpLifecycle = failedLifecycle now "launch_failed" err
                (Just (Text.pack executablePath)) Nothing Nothing (manifestLifecycleResources (lpManifest lp))
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
            }
        Right (transport, processHandle) -> do
          mPid <- processHandleIdText processHandle
          let conn = newRPCConnection (lpManifest lp) transport (lpParams lp)
          hsResult <- performPluginHandshakeWithTimeout conn
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
                }
            Just (Right conn') -> do
              now <- getCurrentTime
              pure lp
                { lpStatus = PluginConnected
                , lpLifecycle = readyLifecycle now (Just "handshake complete") mPid conn'
                , lpConnection = Just conn'
                , lpProcessHandle = Just processHandle
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
                }

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
    Just processHandle -> safeTerminateProcess processHandle

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

markPluginDegraded :: UTCTime -> Text -> Text -> LoadedPlugin -> LoadedPlugin
markPluginDegraded now errorCode message lp = lp
  { lpStatus = PluginError message
  , lpLifecycle = pluginLifecycleSnapshot now LifecycleDegraded
      (Just "manifest refresh failed") (Just errorCode) (Just message) Nothing
      (plsProcessId (lpLifecycle lp))
      (plsProtocolVersion (lpLifecycle lp))
      (manifestLifecycleResources (lpManifest lp))
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
