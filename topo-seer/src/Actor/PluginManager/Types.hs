{-# LANGUAGE StrictData #-}

-- | Shared runtime types for the plugin manager actor and its helper
-- components.
module Actor.PluginManager.Types
  ( PluginStatus(..)
  , LoadedPlugin(..)
  , PluginManagerState(..)
  , emptyPluginManagerState
  , requiresRuntimeConnection
  , setParamOnPlugin
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.Process (ProcessHandle)

import Topo.Overlay.Schema (OverlaySchema)
import Topo.Plugin.RPC (RPCConnection, RPCManifest(..))

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
  , lpProcessHandle :: !(Maybe ProcessHandle)
    -- ^ Active plugin subprocess handle, if launched.
  , lpDirectory  :: !FilePath
    -- ^ Filesystem path to the plugin directory.
  , lpOverlaySchema :: !(Maybe OverlaySchema)
    -- ^ Parsed overlay schema when the plugin declares one.
  }

-- | Plugin manager actor state.
data PluginManagerState = PluginManagerState
  { pmsPlugins    :: !(Map Text LoadedPlugin)
    -- ^ Loaded plugins keyed by name.
  , pmsPluginOrder :: ![Text]
    -- ^ User-defined plugin ordering for pipeline insertion.
  , pmsBaseDir    :: !FilePath
    -- ^ Base directory for plugin discovery (@~\/.topo\/plugins\/@).
  , pmsDisabledPlugins :: !(Set Text)
    -- ^ Plugins the user has disabled in the pipeline.
  }

emptyPluginManagerState :: PluginManagerState
emptyPluginManagerState = PluginManagerState
  { pmsPlugins     = Map.empty
  , pmsPluginOrder = []
  , pmsBaseDir     = ""
  , pmsDisabledPlugins = Set.empty
  }

-- | Whether a manifest needs a live RPC session to serve its declared
-- capabilities.
requiresRuntimeConnection :: RPCManifest -> Bool
requiresRuntimeConnection manifest =
  isJust (rmGenerator manifest)
  || isJust (rmSimulation manifest)
  || not (null (rmDataResources manifest))

-- | Update a single parameter in a loaded plugin.
setParamOnPlugin :: Text -> Value -> LoadedPlugin -> LoadedPlugin
setParamOnPlugin paramName value lp =
  lp { lpParams = Map.insert paramName value (lpParams lp) }
