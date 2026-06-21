{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared runtime types for the plugin manager actor and its helper
-- components.
module Actor.PluginManager.Types
  ( PluginStatus(..)
  , PluginLifecycleState(..)
  , PluginStateLease(..)
  , PluginLifecycleSnapshot(..)
  , LoadedPlugin(..)
  , PluginManagerState(..)
  , emptyPluginManagerState
  , pluginLifecycleStateText
  , pluginLifecycleSnapshot
  , manifestLifecycleResources
  , requiresRuntimeConnection
  , restartModeAllowsFailure
  , pruneRestartHistory
  , canRestartPlugin
  , recordPluginRestart
  , policyTimeoutMicros
  , setParamOnPlugin
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import System.Process (ProcessHandle)

import Topo.Overlay.Schema (OverlaySchema)
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC (RPCConnection, RPCManifest(..), RPCStartPolicy(..), RPCRestartMode(..))

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

-- | Observable lifecycle state for plugin supervision diagnostics.
data PluginLifecycleState
  = LifecycleDiscovered
  | LifecycleStarting
  | LifecycleReady
  | LifecycleDegraded
  | LifecycleStopping
  | LifecycleStopped
  | LifecycleFailed
  deriving (Eq, Ord, Show)

pluginLifecycleStateText :: PluginLifecycleState -> Text
pluginLifecycleStateText LifecycleDiscovered = "discovered"
pluginLifecycleStateText LifecycleStarting = "starting"
pluginLifecycleStateText LifecycleReady = "ready"
pluginLifecycleStateText LifecycleDegraded = "degraded"
pluginLifecycleStateText LifecycleStopping = "stopping"
pluginLifecycleStateText LifecycleStopped = "stopped"
pluginLifecycleStateText LifecycleFailed = "failed"

instance ToJSON PluginLifecycleState where
  toJSON = toJSON . pluginLifecycleStateText

-- | Lease held by the supervisor for a plugin's current lifecycle state.
data PluginStateLease = PluginStateLease
  { pslHolder    :: !Text
  , pslState     :: !PluginLifecycleState
  , pslAcquiredAt :: !UTCTime
  , pslExpiresAt :: !(Maybe UTCTime)
  } deriving (Eq, Show)

instance ToJSON PluginStateLease where
  toJSON lease = object
    [ "holder"      .= pslHolder lease
    , "state"       .= pslState lease
    , "acquired_at" .= pslAcquiredAt lease
    , "expires_at"  .= pslExpiresAt lease
    ]

-- | Diagnostic snapshot for the plugin lifecycle state machine.
data PluginLifecycleSnapshot = PluginLifecycleSnapshot
  { plsState              :: !PluginLifecycleState
  , plsUpdatedAt          :: !UTCTime
  , plsReason             :: !(Maybe Text)
  , plsErrorCode          :: !(Maybe Text)
  , plsErrorMessage       :: !(Maybe Text)
  , plsBlockingDependency :: !(Maybe Text)
  , plsProcessId          :: !(Maybe Text)
  , plsProtocolVersion    :: !(Maybe Int)
  , plsResources          :: ![Text]
  , plsStateLeases        :: ![PluginStateLease]
  } deriving (Eq, Show)

instance ToJSON PluginLifecycleSnapshot where
  toJSON snapshot = object
    [ "state"               .= plsState snapshot
    , "updated_at"          .= plsUpdatedAt snapshot
    , "reason"              .= plsReason snapshot
    , "error_code"          .= plsErrorCode snapshot
    , "error_message"       .= plsErrorMessage snapshot
    , "blocking_dependency" .= plsBlockingDependency snapshot
    , "process_id"          .= plsProcessId snapshot
    , "protocol_version"    .= plsProtocolVersion snapshot
    , "resources"           .= plsResources snapshot
    , "state_leases"        .= plsStateLeases snapshot
    ]

pluginLifecycleSnapshot
  :: UTCTime
  -> PluginLifecycleState
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> [Text]
  -> PluginLifecycleSnapshot
pluginLifecycleSnapshot now lifecycleState reason errorCode errorMessage blockingDependency processId protocolVersion resources =
  PluginLifecycleSnapshot
    { plsState = lifecycleState
    , plsUpdatedAt = now
    , plsReason = reason
    , plsErrorCode = errorCode
    , plsErrorMessage = errorMessage
    , plsBlockingDependency = blockingDependency
    , plsProcessId = processId
    , plsProtocolVersion = protocolVersion
    , plsResources = resources
    , plsStateLeases =
        [ PluginStateLease
            { pslHolder = "plugin-supervisor"
            , pslState = lifecycleState
            , pslAcquiredAt = now
            , pslExpiresAt = Nothing
            }
        ]
    }

manifestLifecycleResources :: RPCManifest -> [Text]
manifestLifecycleResources manifest = map drsName (rmDataResources manifest)

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
  , lpLifecycle  :: !PluginLifecycleSnapshot
    -- ^ Observable lifecycle diagnostics.
  , lpConnection :: !(Maybe RPCConnection)
    -- ^ Active RPC connection, if connected.
  , lpProcessHandle :: !(Maybe ProcessHandle)
    -- ^ Active plugin subprocess handle, if launched.
  , lpStartPolicy :: !RPCStartPolicy
    -- ^ Host-side startup, restart, and timeout policy.
  , lpRestartHistory :: ![UTCTime]
    -- ^ Restart attempt timestamps kept within the policy window.
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

restartModeAllowsFailure :: RPCRestartMode -> Bool
restartModeAllowsFailure RestartNever = False
restartModeAllowsFailure RestartOnFailure = True
restartModeAllowsFailure RestartAlways = True

pruneRestartHistory :: RPCStartPolicy -> UTCTime -> [UTCTime] -> [UTCTime]
pruneRestartHistory policy now =
  filter (withinRestartWindow now (restartWindowSeconds policy))

canRestartPlugin :: RPCStartPolicy -> UTCTime -> [UTCTime] -> Bool
canRestartPlugin policy now history =
  rspAutoStart policy
  && restartModeAllowsFailure (rspRestartMode policy)
  && length (pruneRestartHistory policy now history) < rspMaxRestarts policy

recordPluginRestart :: RPCStartPolicy -> UTCTime -> [UTCTime] -> [UTCTime]
recordPluginRestart policy now history =
  pruneRestartHistory policy now history <> [now]

policyTimeoutMicros :: Int -> Int
policyTimeoutMicros millis = max 1 (millis * 1000)

restartWindowSeconds :: RPCStartPolicy -> NominalDiffTime
restartWindowSeconds policy = realToFrac (rspRestartWindowMs policy) / 1000

withinRestartWindow :: UTCTime -> NominalDiffTime -> UTCTime -> Bool
withinRestartWindow now window startedAt =
  diffUTCTime now startedAt <= window

-- | Update a single parameter in a loaded plugin.
setParamOnPlugin :: Text -> Value -> LoadedPlugin -> LoadedPlugin
setParamOnPlugin paramName value lp =
  lp { lpParams = Map.insert paramName value (lpParams lp) }
