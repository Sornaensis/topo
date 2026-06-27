{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared runtime types for the plugin manager actor and its helper
-- components.
module Actor.PluginManager.Types
  ( PluginStatus(..)
  , PluginLifecycleState(..)
  , PluginStateLease(..)
  , PluginLifecycleSnapshot(..)
  , PluginDiagnosticState(..)
  , PluginDependencyDiagnostic(..)
  , PluginExternalDataSourceDiagnostic(..)
  , LoadedPlugin(..)
  , PluginManagerState(..)
  , emptyPluginManagerState
  , pluginStatusText
  , pluginLifecycleStateText
  , pluginLifecycleSnapshot
  , manifestLifecycleResources
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginDiagnosticDetail
  , pluginAvailableDependencyKeys
  , pluginDependencyDiagnostics
  , pluginExternalDataSourceDiagnostics
  , pluginPanelDiagnosticLines
  , pluginResourceNames
  , pluginCapabilitiesText
  , pluginEndpointKind
  , pluginLastError
  , pluginUptimeSeconds
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
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import System.Info (os)
import System.Process (ProcessHandle)

import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Pipeline.Stage (allBuiltinStageIds, stageCanonicalName)
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource (DataOperations(..), DataResourceSchema(..))
import Topo.Plugin.RPC
  ( RPCConnection
  , RPCGeneratorDecl(..)
  , RPCManifest(..)
  , RPCSimulationDecl(..)
  , RPCStartPolicy(..)
  , RPCRestartMode(..)
  )

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

pluginStatusText :: PluginStatus -> Text
pluginStatusText PluginIdle = "idle"
pluginStatusText PluginConnected = "connected"
pluginStatusText (PluginError err) = "error: " <> err
pluginStatusText PluginDisconnected = "disconnected"

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

-- | User-facing readiness buckets shown by the plugin panel and \/plugins
-- diagnostics.  The buckets are intentionally backend-neutral: external data
-- resources are reported as plugin/provider-owned capabilities, not as topo
-- storage that topo manages.
data PluginDiagnosticState
  = DiagnosticReady
  | DiagnosticWaitingForDependencies
  | DiagnosticDegraded
  | DiagnosticDisabled
  | DiagnosticFailed
  deriving (Eq, Ord, Show)

pluginDiagnosticStateText :: PluginDiagnosticState -> Text
pluginDiagnosticStateText DiagnosticReady = "Ready"
pluginDiagnosticStateText DiagnosticWaitingForDependencies = "WaitingForDependencies"
pluginDiagnosticStateText DiagnosticDegraded = "Degraded"
pluginDiagnosticStateText DiagnosticDisabled = "Disabled"
pluginDiagnosticStateText DiagnosticFailed = "Failed"

instance ToJSON PluginDiagnosticState where
  toJSON = toJSON . pluginDiagnosticStateText

-- | One dependency edge declared by a plugin manifest and its current
-- availability from the host's perspective.
data PluginDependencyDiagnostic = PluginDependencyDiagnostic
  { pddKind :: !Text
  , pddName :: !Text
  , pddStatus :: !Text
  , pddRequired :: !Bool
  , pddDetail :: !(Maybe Text)
  } deriving (Eq, Show)

instance ToJSON PluginDependencyDiagnostic where
  toJSON dep = object
    [ "kind" .= pddKind dep
    , "name" .= pddName dep
    , "status" .= pddStatus dep
    , "required" .= pddRequired dep
    , "detail" .= pddDetail dep
    ]

-- | Backend-neutral data-source diagnostics.  The provider is the plugin; topo
-- is only the consumer/router for the declared grant and does not own provider
-- storage or lifecycle.
data PluginExternalDataSourceDiagnostic = PluginExternalDataSourceDiagnostic
  { pedsProvider :: !Text
  , pedsConsumer :: !Text
  , pedsResource :: !Text
  , pedsLabel :: !Text
  , pedsStatus :: !Text
  , pedsOwnership :: !Text
  , pedsHostRole :: !Text
  , pedsLifecycleBoundary :: !Text
  , pedsOperations :: !DataOperations
  , pedsOverlay :: !(Maybe Text)
  , pedsDataReadGrant :: !Bool
  , pedsDataWriteGrant :: !Bool
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceDiagnostic where
  toJSON source = object
    [ "provider" .= pedsProvider source
    , "consumer" .= pedsConsumer source
    , "resource" .= pedsResource source
    , "label" .= pedsLabel source
    , "status" .= pedsStatus source
    , "ownership" .= pedsOwnership source
    , "host_role" .= pedsHostRole source
    , "lifecycle_boundary" .= pedsLifecycleBoundary source
    , "operations" .= dataOperationsDiagnosticJSON (pedsOperations source)
    , "overlay" .= pedsOverlay source
    , "grants" .= object
        [ "data_read" .= pedsDataReadGrant source
        , "data_write" .= pedsDataWriteGrant source
        ]
    ]

-- | Dependencies that are currently satisfiable in this plugin-manager view.
pluginAvailableDependencyKeys :: Set Text -> [LoadedPlugin] -> Set Text
pluginAvailableDependencyKeys disabled plugins = Set.fromList $
  map stageCanonicalName allBuiltinStageIds
  <> pluginNames
  <> map ("plugin:" <>) pluginNames
  <> overlayNames
  where
    availablePlugins = filter (dependencyProviderAvailable disabled) plugins
    pluginNames = map lpName availablePlugins
    overlayNames = [osName schema | plugin <- availablePlugins, Just schema <- [lpOverlaySchema plugin]]

dependencyProviderAvailable :: Set Text -> LoadedPlugin -> Bool
dependencyProviderAvailable disabled lp =
  not (pluginDisabledForDiagnostics disabled lp)
  && plsState (lpLifecycle lp) == LifecycleReady

pluginDiagnosticState :: Set Text -> Set Text -> LoadedPlugin -> PluginDiagnosticState
pluginDiagnosticState disabled availableDeps lp
  | pluginDisabledForDiagnostics disabled lp = DiagnosticDisabled
  | plsState lifecycle == LifecycleFailed = DiagnosticFailed
  | plsState lifecycle == LifecycleDegraded = DiagnosticDegraded
  | any ((== "waiting") . pddStatus) (pluginDependencyDiagnostics availableDeps lp) =
      DiagnosticWaitingForDependencies
  | plsState lifecycle == LifecycleReady = DiagnosticReady
  | otherwise = DiagnosticWaitingForDependencies
  where
    lifecycle = lpLifecycle lp

pluginDiagnosticDetail :: Set Text -> Set Text -> LoadedPlugin -> Text
pluginDiagnosticDetail disabled availableDeps lp = case pluginDiagnosticState disabled availableDeps lp of
  DiagnosticDisabled
    | Set.member (lpName lp) disabled ->
        "Disabled by user; excluded from pipeline execution but manifest diagnostics remain available."
    | otherwise ->
        "Disabled by plugin start policy; topo will not auto-start this runtime."
  DiagnosticReady
    | requiresRuntimeConnection (lpManifest lp) -> "Ready; RPC connection is active."
    | otherwise -> "Ready; manifest does not require a runtime connection."
  DiagnosticDegraded ->
    "Degraded: " <> fromMaybe "manifest or lifecycle diagnostics are incomplete" (pluginLastError lp)
  DiagnosticFailed ->
    "Failed: " <> fromMaybe "plugin startup or runtime failed" (pluginLastError lp)
  DiagnosticWaitingForDependencies ->
    case waitingDeps of
      [] -> fromMaybe "Waiting for runtime startup, handshake, or dependency observation." (plsReason (lpLifecycle lp))
      deps -> "Waiting for dependencies: " <> Text.intercalate ", " deps
  where
    waitingDeps = [pddName dep | dep <- pluginDependencyDiagnostics availableDeps lp, pddStatus dep == "waiting"]

pluginDependencyDiagnostics :: Set Text -> LoadedPlugin -> [PluginDependencyDiagnostic]
pluginDependencyDiagnostics availableDeps lp =
  generatorInsertAfter <> generatorRequires <> simulationDeps
  where
    manifest = lpManifest lp
    mkDep kind name required = PluginDependencyDiagnostic
      { pddKind = kind
      , pddName = name
      , pddStatus = if Set.member name availableDeps then "available" else "waiting"
      , pddRequired = required
      , pddDetail = if Set.member name availableDeps
          then Nothing
          else Just "No loaded plugin, overlay, or built-in stage currently satisfies this declaration."
      }
    generatorInsertAfter = case rmGenerator manifest of
      Nothing -> []
      Just gen -> [mkDep "generator_insert_after" (rgdInsertAfter gen) True]
    generatorRequires = case rmGenerator manifest of
      Nothing -> []
      Just gen -> map (\dep -> mkDep "generator_requires" dep True) (rgdRequires gen)
    simulationDeps = case rmSimulation manifest of
      Nothing -> []
      Just sim -> map (\dep -> mkDep "simulation_dependency" dep True) (rsdDependencies sim)

pluginExternalDataSourceDiagnostics :: LoadedPlugin -> [PluginExternalDataSourceDiagnostic]
pluginExternalDataSourceDiagnostics lp =
  [ PluginExternalDataSourceDiagnostic
      { pedsProvider = lpName lp
      , pedsConsumer = "topo"
      , pedsResource = drsName resource
      , pedsLabel = drsLabel resource
      , pedsStatus = dataSourceStatus resource
      , pedsOwnership = "plugin-owned"
      , pedsHostRole = "consumer-router"
      , pedsLifecycleBoundary = "external-provider-managed"
      , pedsOperations = drsOperations resource
      , pedsOverlay = drsOverlay resource
      , pedsDataReadGrant = hasCapability CapDataRead
      , pedsDataWriteGrant = hasCapability CapDataWrite
      }
  | resource <- rmDataResources manifest
  ]
  where
    manifest = lpManifest lp
    hasCapability cap = cap `elem` rmCapabilities manifest
    dataSourceStatus resource
      | not (hasCapability CapDataRead) = "missing-grant:dataRead"
      | requiresDataWriteGrant (drsOperations resource) && not (hasCapability CapDataWrite) = "missing-grant:dataWrite"
      | plsState (lpLifecycle lp) == LifecycleReady = "available"
      | otherwise = "declared"

dataOperationsDiagnosticJSON :: DataOperations -> Value
dataOperationsDiagnosticJSON ops = object
  [ "list" .= doList ops
  , "get" .= doGet ops
  , "create" .= doCreate ops
  , "update" .= doUpdate ops
  , "delete" .= doDelete ops
  , "query_by_hex" .= doQueryByHex ops
  ]

requiresDataWriteGrant :: DataOperations -> Bool
requiresDataWriteGrant ops = doCreate ops || doUpdate ops || doDelete ops

pluginDisabledForDiagnostics :: Set Text -> LoadedPlugin -> Bool
pluginDisabledForDiagnostics disabled lp =
  Set.member (lpName lp) disabled
  || (requiresRuntimeConnection (lpManifest lp) && not (rspAutoStart (lpStartPolicy lp)))

pluginPanelDiagnosticLines :: Set Text -> LoadedPlugin -> [Text]
pluginPanelDiagnosticLines availableDeps lp =
  [ "Lifecycle: state=" <> pluginLifecycleStateText (plsState lifecycle)
      <> " reason=" <> fromMaybe "n/a" (plsReason lifecycle)
  , "Process: pid=" <> fromMaybe "n/a" (plsProcessId lifecycle)
      <> " endpoint=" <> fromMaybe "n/a" (pluginEndpointKind lp)
      <> " protocol=" <> maybe "n/a" (Text.pack . show) (plsProtocolVersion lifecycle)
      <> " updated_at=" <> Text.pack (show (plsUpdatedAt lifecycle))
  , "Last error: " <> fromMaybe "none" (pluginLastError lp)
  , "Restarts: " <> Text.pack (show (length (lpRestartHistory lp)))
  , "Dependencies: " <> dependencySummary
  , "Resources: " <> summaryOrNone (pluginResourceNames lp)
  , "External data: " <> externalDataSummary
  , "Capabilities: " <> summaryOrNone (pluginCapabilitiesText (lpManifest lp))
  ]
  where
    lifecycle = lpLifecycle lp
    dependencySummary = case pluginDependencyDiagnostics availableDeps lp of
      [] -> "none declared"
      deps -> Text.intercalate "; "
        [ pddKind dep <> ":" <> pddName dep <> "=" <> pddStatus dep
        | dep <- deps
        ]
    externalDataSummary = case pluginExternalDataSourceDiagnostics lp of
      [] -> "none declared"
      sources -> Text.intercalate "; "
        [ pedsResource source <> "=" <> pedsStatus source
            <> " (plugin-owned; topo routes only)"
        | source <- sources
        ]

pluginResourceNames :: LoadedPlugin -> [Text]
pluginResourceNames = map drsName . rmDataResources . lpManifest

pluginCapabilitiesText :: RPCManifest -> [Text]
pluginCapabilitiesText = map capabilityText . rmCapabilities

pluginEndpointKind :: LoadedPlugin -> Maybe Text
pluginEndpointKind lp
  | isJust (lpConnection lp) || isJust (plsProcessId (lpLifecycle lp)) = Just productionEndpointKindText
  | otherwise = Nothing

pluginLastError :: LoadedPlugin -> Maybe Text
pluginLastError lp = case lpStatus lp of
  PluginError err -> Just err
  _ -> plsErrorMessage (lpLifecycle lp)

pluginUptimeSeconds :: UTCTime -> LoadedPlugin -> Maybe Double
pluginUptimeSeconds now lp
  | plsState (lpLifecycle lp) == LifecycleReady =
      Just (realToFrac (diffUTCTime now (plsUpdatedAt (lpLifecycle lp))) :: Double)
  | otherwise = Nothing

productionEndpointKindText :: Text
productionEndpointKindText
  | os == "mingw32" = "named-pipe"
  | otherwise = "unix"

summaryOrNone :: [Text] -> Text
summaryOrNone [] = "none"
summaryOrNone xs = Text.intercalate ", " xs

capabilityText :: Capability -> Text
capabilityText CapLog = "log"
capabilityText CapNoise = "noise"
capabilityText CapReadTerrain = "readTerrain"
capabilityText CapWriteTerrain = "writeTerrain"
capabilityText CapReadOverlay = "readOverlay"
capabilityText CapWriteOverlay = "writeOverlay"
capabilityText CapReadWorld = "readWorld"
capabilityText CapWriteWorld = "writeWorld"
capabilityText CapDataRead = "dataRead"
capabilityText CapDataWrite = "dataWrite"

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
