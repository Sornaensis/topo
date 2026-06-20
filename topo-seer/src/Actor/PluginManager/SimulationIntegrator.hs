-- | World/simulation notifications for connected plugin sessions.
module Actor.PluginManager.SimulationIntegrator
  ( notifyPluginWorldChanged
  , notifyPluginsWorldChanged
  ) where

import Data.Text (Text)

import Actor.PluginManager.Types (LoadedPlugin(..))
import Topo.Plugin.RPC (sendWorldChanged)

-- | Send a world-change notification to all connected plugins.
notifyPluginsWorldChanged :: Maybe Text -> [LoadedPlugin] -> IO ()
notifyPluginsWorldChanged mWorldPath plugins =
  mapM_ (notifyPluginWorldChanged mWorldPath) plugins

-- | Send a 'MsgWorldChanged' notification to a single plugin.
-- No-op if the plugin is not connected.
notifyPluginWorldChanged :: Maybe Text -> LoadedPlugin -> IO ()
notifyPluginWorldChanged mWorldPath lp =
  case lpConnection lp of
    Nothing -> pure ()
    Just conn -> do
      _ <- sendWorldChanged conn mWorldPath
      pure ()
