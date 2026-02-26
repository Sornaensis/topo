{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Plugin lifecycle manager actor.
--
-- Handles plugin discovery, process lifecycle, manifest parsing,
-- parameter persistence, and RPC connection management.
--
-- = Lifecycle
--
-- 1. On startup, 'discoverPlugins' scans @~\/.topo\/plugins\/@ for
--    directories containing a @manifest.json@.
-- 2. For each discovered plugin, the manifest is parsed and
--    persisted parameter values are loaded from
--    @~\/.topo\/plugins\/\<name\>\/config.json@.
-- 3. On generation, manifests are re-read (hot-reload) before
--    building the pipeline.
-- 4. On shutdown, all connected plugins receive a @shutdown@ message.
module Actor.PluginManager
  ( -- * Actor
    PluginManager
  , pluginManagerActorDef
    -- * Loaded plugin info
  , LoadedPlugin(..)
  , PluginStatus(..)
    -- * Commands
  , discoverPlugins
  , getLoadedPlugins
  , setPluginParam
  , getPluginStages
  , refreshManifests
  , shutdownPlugins
  , setPluginOrder
  , getPluginOrder
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  , createDirectoryIfMissing
  )
import System.FilePath ((</>))

import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCManifest(..)
  , RPCGeneratorDecl(..)
  , RPCSimulationDecl(..)
  , RPCParamSpec(..)
  , parseManifestFile
  , newRPCConnection
  , rpcGeneratorStage
  , rpcShutdown
  )
import Topo.Plugin.RPC.Transport (Transport, closeTransport)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Runtime status of a plugin.
data PluginStatus
  = PluginIdle
    -- ^ Discovered but not connected.
  | PluginConnected
    -- ^ Transport established and ready.
  | PluginError !Text
    -- ^ Last operation failed.
  | PluginDisconnected
    -- ^ Was connected but transport closed.
  deriving (Eq, Show)

-- | A discovered plugin with its manifest and current state.
data LoadedPlugin = LoadedPlugin
  { lpName       :: !Text
    -- ^ Plugin name from manifest.
  , lpManifest   :: !RPCManifest
    -- ^ Parsed manifest.
  , lpParams     :: !(Map Text Value)
    -- ^ Current parameter values (user-modified or defaults).
  , lpStatus     :: !PluginStatus
    -- ^ Current lifecycle status.
  , lpConnection :: !(Maybe RPCConnection)
    -- ^ Active RPC connection, if connected.
  , lpDirectory  :: !FilePath
    -- ^ Filesystem path to the plugin directory.
  }

-- | Plugin manager actor state.
data PluginManagerState = PluginManagerState
  { pmsPlugins    :: !(Map Text LoadedPlugin)
    -- ^ Loaded plugins keyed by name.
  , pmsPluginOrder :: ![Text]
    -- ^ User-defined plugin ordering for pipeline insertion.
  , pmsBaseDir    :: !FilePath
    -- ^ Base directory for plugin discovery (@~\/.topo\/plugins\/@).
  }

emptyPluginManagerState :: PluginManagerState
emptyPluginManagerState = PluginManagerState
  { pmsPlugins     = Map.empty
  , pmsPluginOrder = []
  , pmsBaseDir     = ""
  }

------------------------------------------------------------------------
-- Actor definition
------------------------------------------------------------------------

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
  call getOrder :: () -> [Text]
  cast setParam :: (Text, Text, Value)
  cast setOrder :: [Text]
  cast refresh :: ()
  cast shutdown :: ()

  initial emptyPluginManagerState
  on_ discover = \() st -> do
    baseDir <- pluginsBaseDir
    createDirectoryIfMissing True baseDir
    plugins <- scanPluginDirs baseDir
    orderTxt <- loadPluginOrder baseDir
    pure st
      { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins]
      , pmsPluginOrder = orderTxt
      , pmsBaseDir = baseDir
      }
  onPure getPlugins = \() st ->
    (st, Map.elems (pmsPlugins st))
  onPure getStages = \() st ->
    (st, buildPluginStages st)
  onPure getOrder = \() st ->
    (st, pmsPluginOrder st)
  onPure_ setParam = \(pluginName, paramName, value) st ->
    st { pmsPlugins = Map.adjust (setParamOnPlugin paramName value) pluginName (pmsPlugins st) }
  onPure_ setOrder = \order st ->
    st { pmsPluginOrder = order }
  on_ refresh = \() st -> do
    plugins' <- refreshAllManifests (pmsBaseDir st) (pmsPlugins st)
    pure st { pmsPlugins = plugins' }
  on_ shutdown = \() st -> do
    mapM_ shutdownPlugin (Map.elems (pmsPlugins st))
    pure st { pmsPlugins = Map.map disconnectPlugin (pmsPlugins st) }
|]

------------------------------------------------------------------------
-- Discovery
------------------------------------------------------------------------

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
          pure [LoadedPlugin
            { lpName       = rmName manifest
            , lpManifest   = manifest
            , lpParams     = params
            , lpStatus     = PluginIdle
            , lpConnection = Nothing
            , lpDirectory  = pluginDir
            }]
    else pure []

------------------------------------------------------------------------
-- Configuration persistence
------------------------------------------------------------------------

-- | Load saved parameter values, falling back to manifest defaults.
loadPluginConfig :: FilePath -> [RPCParamSpec] -> IO (Map Text Value)
loadPluginConfig pluginDir paramSpecs = do
  let configPath = pluginDir </> "config.json"
      defaults = Map.fromList
        [(rpsName spec, rpsDefault spec) | spec <- paramSpecs]
  exists <- doesFileExist configPath
  if not exists
    then pure defaults
    else do
      result <- try @SomeException (BL.readFile configPath)
      case result of
        Left _ -> pure defaults
        Right bs -> case Aeson.decode bs of
          Nothing -> pure defaults
          Just (Object km) ->
            let saved = Map.fromList
                  [(Key.toText k, v) | (k, v) <- KM.toList km]
            in pure (Map.union saved defaults)
          Just _ -> pure defaults

-- | Save current parameter values to config.json.
savePluginConfig :: FilePath -> Map Text Value -> IO ()
savePluginConfig pluginDir params = do
  let configPath = pluginDir </> "config.json"
  createDirectoryIfMissing True pluginDir
  BL.writeFile configPath (Aeson.encode (Aeson.toJSON params))

-- | Load the global plugin ordering from plugin-order.json.
loadPluginOrder :: FilePath -> IO [Text]
loadPluginOrder baseDir = do
  let orderPath = baseDir </> "plugin-order.json"
  exists <- doesFileExist orderPath
  if not exists
    then pure []
    else do
      result <- try @SomeException (BL.readFile orderPath)
      case result of
        Left _ -> pure []
        Right bs -> case Aeson.decode bs of
          Just names -> pure names
          Nothing -> pure []

-- | Persist the plugin ordering.
savePluginOrder :: FilePath -> [Text] -> IO ()
savePluginOrder baseDir order = do
  let orderPath = baseDir </> "plugin-order.json"
  createDirectoryIfMissing True baseDir
  BL.writeFile orderPath (Aeson.encode order)

------------------------------------------------------------------------
-- Manifest hot-reload
------------------------------------------------------------------------

-- | Re-read manifests for all known plugins, preserving params.
refreshAllManifests :: FilePath -> Map Text LoadedPlugin -> IO (Map Text LoadedPlugin)
refreshAllManifests baseDir plugins = do
  Map.traverseWithKey (\_ lp -> refreshOneManifest baseDir lp) plugins

-- | Re-read a single plugin's manifest, preserving current params.
refreshOneManifest :: FilePath -> LoadedPlugin -> IO LoadedPlugin
refreshOneManifest _baseDir lp = do
  let manifestPath = lpDirectory lp </> "manifest.json"
  result <- try @SomeException (parseManifestFile manifestPath)
  case result of
    Left _ -> pure lp { lpStatus = PluginError "manifest read failed" }
    Right (Left err) -> pure lp { lpStatus = PluginError (Text.pack (show err)) }
    Right (Right manifest) -> pure lp { lpManifest = manifest }

------------------------------------------------------------------------
-- Pipeline integration
------------------------------------------------------------------------

-- | Build pipeline stages from all loaded plugins that declare
-- a generator section, ordered by the user-defined plugin order.
buildPluginStages :: PluginManagerState -> [PipelineStage]
buildPluginStages st =
  let plugins = pmsPlugins st
      ordered = orderPlugins (pmsPluginOrder st) (Map.elems plugins)
  in concatMap pluginToStage ordered

-- | Reorder plugins according to the user's saved ordering.
-- Plugins not in the ordering list appear at the end.
orderPlugins :: [Text] -> [LoadedPlugin] -> [LoadedPlugin]
orderPlugins order plugins =
  let byName = Map.fromList [(lpName p, p) | p <- plugins]
      ordered = [p | name <- order, Just p <- [Map.lookup name byName]]
      remaining = [p | p <- plugins, lpName p `notElem` order]
  in ordered ++ remaining

-- | Convert a loaded plugin to a pipeline stage if it has a generator
-- declaration.
pluginToStage :: LoadedPlugin -> [PipelineStage]
pluginToStage lp =
  case rmGenerator (lpManifest lp) of
    Nothing -> []
    Just _genDecl ->
      case lpConnection lp of
        Nothing ->
          -- Not connected yet: produce a no-op stage that logs a warning.
          -- Phase 7 TODO: auto-connect before generation.
          []
        Just conn ->
          [rpcGeneratorStage conn]

------------------------------------------------------------------------
-- Shutdown
------------------------------------------------------------------------

-- | Shut down a single plugin's RPC connection.
shutdownPlugin :: LoadedPlugin -> IO ()
shutdownPlugin lp =
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- try @SomeException (rpcShutdown conn)
      _ <- try @SomeException (closeTransport (rpcTransport conn))
      pure ()

-- | Mark a plugin as disconnected.
disconnectPlugin :: LoadedPlugin -> LoadedPlugin
disconnectPlugin lp = lp
  { lpStatus = PluginDisconnected
  , lpConnection = Nothing
  }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Update a single parameter in a loaded plugin.
setParamOnPlugin :: Text -> Value -> LoadedPlugin -> LoadedPlugin
setParamOnPlugin paramName value lp =
  lp { lpParams = Map.insert paramName value (lpParams lp) }

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

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
  -> Text -> Text -> Value -> IO ()
setPluginParam handle pluginName paramName value =
  cast @"setParam" handle #setParam (pluginName, paramName, value)

-- | Get pipeline stages for all loaded generator plugins.
getPluginStages
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [PipelineStage]
getPluginStages handle =
  call @"getStages" handle #getStages ()

-- | Re-read manifests for all loaded plugins (hot-reload).
refreshManifests
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
refreshManifests handle =
  cast @"refresh" handle #refresh ()

-- | Shut down all connected plugins.
shutdownPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO ()
shutdownPlugins handle =
  cast @"shutdown" handle #shutdown ()

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
