-- | Backend-neutral plugin data-resource registry views.
module Actor.PluginManager.ResourceRegistry
  ( buildPluginDataResources
  , collectPluginDataDirs
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Actor.PluginManager.Types (LoadedPlugin(..), PluginManagerState(..))
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC (RPCConnection(..), RPCManifest(..))

-- | Collect data resource schemas from all loaded plugins.
-- Returns a map from plugin name to its declared data resources.
buildPluginDataResources :: PluginManagerState -> Map Text [DataResourceSchema]
buildPluginDataResources st =
  Map.mapMaybe extractResources (pmsPlugins st)
  where
    extractResources lp =
      case rmDataResources (lpManifest lp) of
        [] -> Nothing
        rs -> Just rs

-- | Collect @(pluginName, dataDir)@ pairs from all plugins that
-- declared a data directory via the handshake.
collectPluginDataDirs :: PluginManagerState -> [(Text, FilePath)]
collectPluginDataDirs st =
  [ (lpName lp, dir)
  | lp <- Map.elems (pmsPlugins st)
  , Just conn <- [lpConnection lp]
  , Just dir <- [rpcDataDirectory conn]
  ]
