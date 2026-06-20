{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Public facade for the plugin manager actor.
--
-- The actor protocol in this module is intentionally stable; the
-- implementation is split across @Actor.PluginManager.*@ supervisor
-- components.
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
  , getPluginOverlaySchemas
  , refreshManifests
  , shutdownPlugins
  , setPluginOrder
  , getPluginOrder
  , setDisabledPlugins
  , getDisabledPlugins
    -- * Data service
  , getPluginDataResources
  , queryPluginResource
  , mutatePluginResource
    -- * World lifecycle
  , notifyWorldChanged
  , getPluginDataDirectories
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Hyperspace.Actor

import Actor.PluginManager.RootSupervisor (PluginManager, pluginManagerActorDef)
import Actor.PluginManager.Types (LoadedPlugin(..), PluginStatus(..))
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Pipeline (PipelineStage)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC
  ( MutateResource
  , MutateResult
  , QueryResource
  , QueryResult
  )

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

-- | Get parsed overlay schemas from loaded plugins in effective order.
getPluginOverlaySchemas
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [OverlaySchema]
getPluginOverlaySchemas handle =
  call @"getOverlaySchemas" handle #getOverlaySchemas ()

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

-- | Get data resource schemas from all plugins that declare them.
-- Returns a map from plugin name to its declared data resource schemas.
getPluginDataResources
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO (Map Text [DataResourceSchema])
getPluginDataResources handle =
  call @"getDataResources" handle #getDataResources ()

-- | Forward a data query to the named plugin.
queryPluginResource
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Text -> QueryResource -> IO (Either Text QueryResult)
queryPluginResource handle pluginName qr =
  call @"queryData" handle #queryData (pluginName, qr)

-- | Forward a data mutation to the named plugin.
mutatePluginResource
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Text -> MutateResource -> IO (Either Text MutateResult)
mutatePluginResource handle pluginName mr =
  call @"mutateData" handle #mutateData (pluginName, mr)

-- | Notify all connected plugins that the world path has changed.
--
-- Called after world save\/load so plugins can locate their data
-- directories relative to the new world path.
notifyWorldChanged
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Maybe Text  -- ^ New world path, or 'Nothing' if no world is loaded.
  -> IO ()
notifyWorldChanged handle mWorldPath =
  cast @"notifyWorld" handle #notifyWorld mWorldPath

-- | Get the data directories declared by connected plugins.
--
-- Returns @(pluginName, absoluteDataDirPath)@ pairs for all plugins
-- whose handshake declared a data directory.
getPluginDataDirectories
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO [(Text, FilePath)]
getPluginDataDirectories handle =
  call @"getDataDirs" handle #getDataDirs ()

-- | Update the set of disabled plugin names.
--
-- Disabled plugins are excluded from 'getPluginStages' and
-- 'getPluginOverlaySchemas'.
setDisabledPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> Set Text -> IO ()
setDisabledPlugins handle disabled =
  cast @"setDisabled" handle #setDisabled disabled

-- | Query the current set of disabled plugin names.
getDisabledPlugins
  :: ActorHandle PluginManager (Protocol PluginManager)
  -> IO (Set Text)
getDisabledPlugins handle =
  call @"getDisabled" handle #getDisabled ()
