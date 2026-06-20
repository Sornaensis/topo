{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin directory scanning and manifest loading.
module Actor.PluginManager.Scanner
  ( pluginsBaseDir
  , scanPluginDirs
  , tryLoadPlugin
  , loadOverlaySchema
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  )
import System.FilePath ((</>))
import Data.Time (getCurrentTime)

import Actor.PluginManager.Config (loadPluginConfig)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , manifestLifecycleResources
  , pluginLifecycleSnapshot
  )
import Topo.Overlay.Schema (OverlaySchema, parseOverlaySchema)
import Topo.Plugin.RPC
  ( RPCManifest(..)
  , parseManifestFile
  )
import qualified Topo.Plugin.RPC.Manifest as RPCManifest

-- | The standard plugin directory.
pluginsBaseDir :: IO FilePath
pluginsBaseDir = do
  home <- getHomeDirectory
  pure (home </> ".topo" </> "plugins")

-- | Scan the plugins directory for subdirectories with manifest.json.
scanPluginDirs :: FilePath -> IO [LoadedPlugin]
scanPluginDirs baseDir = do
  exists <- doesDirectoryExist baseDir
  if not exists
    then pure []
    else do
      entries <- listDirectory baseDir
      plugins <- mapM (tryLoadPlugin baseDir) entries
      pure (concat plugins)

-- | Try to load a single plugin from a subdirectory.
tryLoadPlugin :: FilePath -> String -> IO [LoadedPlugin]
tryLoadPlugin baseDir entry = do
  let pluginDir = baseDir </> entry
      manifestPath = pluginDir </> "manifest.json"
  isDir <- doesDirectoryExist pluginDir
  hasManifest <- doesFileExist manifestPath
  if isDir && hasManifest
    then do
      result <- try @SomeException (parseManifestFile manifestPath)
      case result of
        Left _err -> pure []
        Right (Left _parseErr) -> pure []
        Right (Right manifest) -> do
          params <- loadPluginConfig pluginDir (rmParameters manifest)
          overlaySchema <- loadOverlaySchema pluginDir manifest
          now <- getCurrentTime
          pure [LoadedPlugin
            { lpName       = rmName manifest
            , lpManifest   = manifest
            , lpParams     = params
            , lpStatus     = PluginIdle
            , lpLifecycle  = pluginLifecycleSnapshot now LifecycleDiscovered
                (Just "manifest discovered") Nothing Nothing Nothing Nothing Nothing
                (manifestLifecycleResources manifest)
            , lpConnection = Nothing
            , lpProcessHandle = Nothing
            , lpDirectory  = pluginDir
            , lpOverlaySchema = overlaySchema
            }]
    else pure []

loadOverlaySchema :: FilePath -> RPCManifest -> IO (Maybe OverlaySchema)
loadOverlaySchema pluginDir manifest =
  case rmOverlay manifest of
    Nothing -> pure Nothing
    Just overlayDecl -> do
      let schemaPath = pluginDir </> Text.unpack (RPCManifest.rodSchemaFile overlayDecl)
      schemaResult <- try @SomeException (BS.readFile schemaPath)
      case schemaResult of
        Left _ -> pure Nothing
        Right schemaBytes ->
          case parseOverlaySchema schemaBytes of
            Left _ -> pure Nothing
            Right schema -> pure (Just schema)
