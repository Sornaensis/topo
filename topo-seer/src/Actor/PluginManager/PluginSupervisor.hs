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
  , disconnectPlugin
  ) where

import Control.Exception (SomeException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

import Actor.PluginManager.HandshakeSession (performPluginHandshakeWithTimeout)
import Actor.PluginManager.ProcessLauncher
  ( launchPluginTransport
  , resolvePluginExecutable
  , safeTerminateProcess
  )
import Actor.PluginManager.Scanner (loadOverlaySchema)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginStatus(..)
  , requiresRuntimeConnection
  )
import Topo.Plugin.RPC
  ( RPCConnection(..)
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
  case result of
    Left _ -> pure lp { lpStatus = PluginError "manifest read failed" }
    Right (Left err) -> pure lp { lpStatus = PluginError (Text.pack (show err)) }
    Right (Right manifest) -> do
      overlaySchema <- loadOverlaySchema (lpDirectory lp) manifest
      pure lp { lpManifest = manifest, lpOverlaySchema = overlaySchema }

ensurePluginConnection :: LoadedPlugin -> IO LoadedPlugin
ensurePluginConnection lp
  | not (requiresRuntimeConnection (lpManifest lp)) = do
      shutdownPlugin lp
      pure lp
        { lpStatus = PluginIdle
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        }
  | otherwise =
      case lpConnection lp of
        Just _ -> pure lp
        Nothing -> connectLoadedPlugin lp

connectLoadedPlugin :: LoadedPlugin -> IO LoadedPlugin
connectLoadedPlugin lp = do
  mExecutable <- resolvePluginExecutable (lpDirectory lp) (lpName lp)
  case mExecutable of
    Nothing ->
      pure lp
        { lpStatus = PluginError "plugin executable not found"
        , lpConnection = Nothing
        , lpProcessHandle = Nothing
        }
    Just executablePath -> do
      launchResult <- launchPluginTransport executablePath (lpDirectory lp) (lpName lp)
      case launchResult of
        Left err ->
          pure lp
            { lpStatus = PluginError err
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
            }
        Right (transport, processHandle) -> do
          let conn = newRPCConnection (lpManifest lp) transport (lpParams lp)
          hsResult <- performPluginHandshakeWithTimeout conn
          case hsResult of
            Nothing -> do
              closeTransport transport
              safeTerminateProcess processHandle
              pure lp
                { lpStatus = PluginError "plugin handshake timed out"
                , lpConnection = Nothing
                , lpProcessHandle = Nothing
                }
            Just (Right conn') ->
              pure lp
                { lpStatus = PluginConnected
                , lpConnection = Just conn'
                , lpProcessHandle = Just processHandle
                }
            Just (Left err) -> do
              closeTransport transport
              safeTerminateProcess processHandle
              pure lp
                { lpStatus = PluginError err
                , lpConnection = Nothing
                , lpProcessHandle = Nothing
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

-- | Mark a plugin as disconnected.
disconnectPlugin :: LoadedPlugin -> LoadedPlugin
disconnectPlugin lp = lp
  { lpStatus = PluginDisconnected
  , lpConnection = Nothing
  , lpProcessHandle = Nothing
  }
