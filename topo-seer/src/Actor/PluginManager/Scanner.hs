{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin directory scanning and manifest loading.
module Actor.PluginManager.Scanner
  ( ManifestLoadFailure(..)
  , pluginsBaseDir
  , scanPluginDirs
  , tryLoadPlugin
  , loadManifestForHost
  , loadOverlaySchema
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Time (UTCTime, getCurrentTime)

import Actor.PluginManager.Config (loadPluginConfig)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , manifestLifecycleResources
  , pluginLifecycleSnapshot
  )
import Topo.Overlay.Schema (OverlaySchema, parseOverlaySchema)
import Topo.Plugin.RPC
  ( RPCManifest(..)
  , defaultRPCManifestRuntime
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , manifestV3
  , parseManifestFile
  )
import qualified Topo.Plugin.RPC.Manifest as RPCManifest

-- | Actionable diagnostics produced while loading a plugin manifest for the
-- host.  A diagnostic manifest is always provided so existing UI/API plugin
-- status surfaces can display the failure instead of silently omitting the
-- plugin directory.
data ManifestLoadFailure = ManifestLoadFailure
  { mlfErrorCode :: !Text
  , mlfErrorMessage :: !Text
  , mlfDiagnosticManifest :: !RPCManifest
  } deriving (Eq, Show)

-- | The standard plugin directory.
pluginsBaseDir :: IO FilePath
pluginsBaseDir = do
  mOverride <- lookupEnv "TOPO_PLUGIN_DIR"
  case mOverride of
    Just dir | not (null dir) -> pure dir
    _ -> do
      home <- getHomeDirectory
      pure (home </> ".topo" </> "plugins")

-- | Scan the plugins directory for plugin subdirectories and diagnostics.
scanPluginDirs :: FilePath -> IO [LoadedPlugin]
scanPluginDirs baseDir = do
  exists <- doesDirectoryExist baseDir
  if not exists
    then pure []
    else do
      entries <- listDirectory baseDir
      plugins <- mapM (tryLoadPlugin baseDir) entries
      pure (dedupeDiscoveredPlugins (concat plugins))

dedupeDiscoveredPlugins :: [LoadedPlugin] -> [LoadedPlugin]
dedupeDiscoveredPlugins plugins =
  Map.elems $ Map.fromListWith preferDiscoveredPlugin
    [(lpName plugin, plugin) | plugin <- plugins]

preferDiscoveredPlugin :: LoadedPlugin -> LoadedPlugin -> LoadedPlugin
preferDiscoveredPlugin new old
  | isMissingManifestDiagnostic new && not (isMissingManifestDiagnostic old) = old
  | otherwise = new

isMissingManifestDiagnostic :: LoadedPlugin -> Bool
isMissingManifestDiagnostic plugin = plsErrorCode (lpLifecycle plugin) == Just "manifest_missing"

-- | Try to load a single plugin from a subdirectory.
tryLoadPlugin :: FilePath -> String -> IO [LoadedPlugin]
tryLoadPlugin baseDir entry = do
  let pluginDir = baseDir </> entry
      manifestPath = pluginDir </> "manifest.json"
      fallbackName = Text.pack entry
  isDir <- doesDirectoryExist pluginDir
  hasManifest <- doesFileExist manifestPath
  case (isDir, hasManifest) of
    (True, True) -> do
      loadResult <- loadManifestForHost pluginDir fallbackName
      now <- getCurrentTime
      case loadResult of
        Left failure -> pure [loadedPluginFromFailure now pluginDir failure]
        Right (manifest, overlaySchema) -> do
          params <- loadPluginConfig pluginDir (rmParameters manifest)
          pure [LoadedPlugin
            { lpName       = rmName manifest
            , lpManifest   = manifest
            , lpParams     = params
            , lpStatus     = PluginIdle
            , lpLifecycle  = pluginLifecycleSnapshot now LifecycleDiscovered
                (Just "manifest discovered") Nothing Nothing Nothing Nothing Nothing
                (manifestLifecycleResources manifest)
            , lpRuntime = Nothing
            , lpStartPolicy = rmStartPolicy manifest
            , lpRestartHistory = []
            , lpDirectory  = pluginDir
            , lpOverlaySchema = overlaySchema
            }]
    (True, False) -> do
      now <- getCurrentTime
      pure [loadedPluginFromFailure now pluginDir (missingManifestFailure fallbackName)]
    (False, _) -> pure []

-- | Parse, validate, and load schema files for a plugin manifest before the
-- supervisor is allowed to launch the runtime process.
loadManifestForHost :: FilePath -> Text -> IO (Either ManifestLoadFailure (RPCManifest, Maybe OverlaySchema))
loadManifestForHost pluginDir fallbackName = do
  let manifestPath = pluginDir </> "manifest.json"
  hasManifest <- doesFileExist manifestPath
  if not hasManifest
    then pure $ Left $ missingManifestFailure fallbackName
    else do
      result <- try @SomeException (parseManifestFile manifestPath)
      case result of
        Left err -> pure $ Left $ manifestFailure fallbackName Nothing
          "manifest_read_failed"
          ("manifest.json could not be read: " <> Text.pack (show err))
        Right (Left parseErr) -> pure $ Left $ manifestFailure fallbackName Nothing
          "manifest_parse_failed"
          ("manifest.json could not be parsed as manifest v3: " <> parseErr
            <> ". Check required fields manifestVersion, name, version, runtime.protocol.min, and runtime.protocol.max.")
        Right (Right manifest) -> do
          let validationErrors = RPCManifest.validateManifest manifest
          if not (null validationErrors)
            then pure $ Left $ manifestFailure fallbackName (Just manifest)
              "manifest_validation_failed"
              ("manifest v3 validation failed: " <> RPCManifest.renderManifestErrors validationErrors)
            else do
              schemaResult <- loadOverlaySchemaStrict pluginDir manifest
              case schemaResult of
                Left schemaErr -> pure $ Left $ manifestFailure fallbackName (Just manifest)
                  "manifest_schema_failed"
                  schemaErr
                Right overlaySchema -> pure (Right (manifest, overlaySchema))

loadOverlaySchema :: FilePath -> RPCManifest -> IO (Maybe OverlaySchema)
loadOverlaySchema pluginDir manifest = do
  result <- loadOverlaySchemaStrict pluginDir manifest
  case result of
    Left _ -> pure Nothing
    Right overlaySchema -> pure overlaySchema

loadOverlaySchemaStrict :: FilePath -> RPCManifest -> IO (Either Text (Maybe OverlaySchema))
loadOverlaySchemaStrict pluginDir manifest =
  case rmOverlay manifest of
    Nothing -> pure (Right Nothing)
    Just overlayDecl -> do
      let schemaFile = RPCManifest.rodSchemaFile overlayDecl
          schemaPath = pluginDir </> Text.unpack schemaFile
      schemaResult <- try @SomeException (BS.readFile schemaPath)
      case schemaResult of
        Left err -> pure $ Left $
          "overlay.schemaFile " <> quote schemaFile <> " could not be read relative to the plugin directory: " <> Text.pack (show err)
        Right schemaBytes ->
          case parseOverlaySchema schemaBytes of
            Left parseErr -> pure $ Left $
              "overlay.schemaFile " <> quote schemaFile <> " could not be parsed as a .toposchema file: " <> parseErr
            Right schema -> pure (Right (Just schema))

loadedPluginFromFailure :: UTCTime -> FilePath -> ManifestLoadFailure -> LoadedPlugin
loadedPluginFromFailure now pluginDir failure =
  let manifest = mlfDiagnosticManifest failure
  in LoadedPlugin
    { lpName       = rmName manifest
    , lpManifest   = manifest
    , lpParams     = Map.empty
    , lpStatus     = PluginError (mlfErrorMessage failure)
    , lpLifecycle  = pluginLifecycleSnapshot now LifecycleDegraded
        (Just "manifest load failed") (Just (mlfErrorCode failure)) (Just (mlfErrorMessage failure))
        Nothing Nothing Nothing (manifestLifecycleResources manifest)
    , lpRuntime = Nothing
    , lpStartPolicy = rmStartPolicy manifest
    , lpRestartHistory = []
    , lpDirectory  = pluginDir
    , lpOverlaySchema = Nothing
    }

missingManifestFailure :: Text -> ManifestLoadFailure
missingManifestFailure fallbackName = manifestFailure fallbackName Nothing
  "manifest_missing"
  ( "manifest.json is missing from the plugin directory; generate or package "
      <> "manifest.json before discovery. topo-seer will not launch unmanifested "
      <> "plugin directories."
  )

manifestFailure :: Text -> Maybe RPCManifest -> Text -> Text -> ManifestLoadFailure
manifestFailure fallbackName mManifest code message = ManifestLoadFailure
  { mlfErrorCode = code
  , mlfErrorMessage = message
  , mlfDiagnosticManifest = diagnosticManifest fallbackName mManifest
  }

diagnosticManifest :: Text -> Maybe RPCManifest -> RPCManifest
diagnosticManifest fallbackName Nothing = emptyDiagnosticManifest fallbackName
diagnosticManifest fallbackName (Just manifest)
  | Text.null (rmName manifest) = manifest { rmName = fallbackName }
  | otherwise = manifest

emptyDiagnosticManifest :: Text -> RPCManifest
emptyDiagnosticManifest fallbackName = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName = fallbackName
  , rmVersion = ""
  , rmRuntime = defaultRPCManifestRuntime
  , rmDescription = ""
  , rmUiHints = defaultRPCUIHints
  , rmGenerator = Nothing
  , rmSimulation = Nothing
  , rmInvocationScopes = Nothing
  , rmOverlay = Nothing
  , rmCapabilities = []
  , rmParameters = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy = defaultRPCStartPolicy
  }

quote :: Text -> Text
quote value = "'" <> value <> "'"
