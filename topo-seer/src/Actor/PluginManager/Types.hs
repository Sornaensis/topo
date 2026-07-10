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
  , PluginExternalDataSourceResourceDiagnostic(..)
  , PluginExternalDataSourceConfigRefDiagnostic(..)
  , PluginExternalDataSourceStatusDiagnostic(..)
  , PluginExternalDataSourceGrantDiagnostic(..)
  , PluginExternalDataSourceDiagnostic(..)
  , PluginParamUpdateError(..)
  , ExternalDataSourceGrantKey(..)
  , ExternalDataSourceGrantBrokerPhase(..)
  , ExternalDataSourceGrantBrokerState(..)
  , externalDataSourceGrantBrokerPhaseText
  , externalDataSourceGrantBrokerPhaseApplied
  , externalDataSourceGrantBrokerPhaseRevocable
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
  , pluginExternalDataSourceDiagnosticsFor
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
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCGeneratorDecl(..)
  , RPCManifest(..)
  , RPCStartPolicy(..)
  , RPCRestartMode(..)
  , RPCUIHints(..)
  )
import Topo.Plugin.RPC.Manifest
  ( RPCParamValidationError(..)
  , sanitizeRPCManifestParams
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

-- | Result errors for synchronous, manifest-backed parameter updates.
data PluginParamUpdateError
  = PluginParamUnknownPlugin !Text
  | PluginParamValidationFailed !RPCParamValidationError
  deriving (Eq, Show)

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

-- | Backend-neutral resource availability for an external data-source status.
-- These rows intentionally describe provider-owned resources without exposing
-- storage, schema, migration, connection, or locking internals as topo state.
data PluginExternalDataSourceResourceDiagnostic = PluginExternalDataSourceResourceDiagnostic
  { pedsrResource :: !Text
  , pedsrAvailable :: !Bool
  , pedsrStatus :: !Text
  , pedsrDetail :: !(Maybe Text)
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceResourceDiagnostic where
  toJSON resource = object $
    [ "resource" .= pedsrResource resource
    , "available" .= pedsrAvailable resource
    , "status" .= pedsrStatus resource
    ] <>
    [ "detail" .= detail | Just detail <- [pedsrDetail resource] ]

-- | Sanitised config-reference diagnostics. The opaque key is deliberately not
-- surfaced; callers get the binding name/origin and whether a key exists.
data PluginExternalDataSourceConfigRefDiagnostic = PluginExternalDataSourceConfigRefDiagnostic
  { pedscName :: !Text
  , pedscOrigin :: !Text
  , pedscRequired :: !Bool
  , pedscCompatibility :: !(Maybe Text)
  , pedscKeyPresent :: !Bool
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceConfigRefDiagnostic where
  toJSON configRef = object $
    [ "name" .= pedscName configRef
    , "origin" .= pedscOrigin configRef
    , "required" .= pedscRequired configRef
    , "key_present" .= pedscKeyPresent configRef
    ] <>
    [ "compatibility" .= compatibility | Just compatibility <- [pedscCompatibility configRef] ]

-- | Sanitised status detail. Opaque provider diagnostics are not copied here;
-- providers own those internals and topo only reports backend-neutral status.
data PluginExternalDataSourceStatusDiagnostic = PluginExternalDataSourceStatusDiagnostic
  { pedssState :: !Text
  , pedssMessage :: !(Maybe Text)
  , pedssProviderId :: !(Maybe Text)
  , pedssAvailability :: !(Maybe Text)
  , pedssHealth :: !(Maybe Text)
  , pedssAccessMode :: !(Maybe Text)
  , pedssCapabilityScope :: ![RPCExternalDataSourceCapability]
  , pedssVersion :: !(Maybe Text)
  , pedssCompatibility :: !(Maybe Text)
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceStatusDiagnostic where
  toJSON status = object $
    [ "state" .= pedssState status
    , "capabilityScope" .= pedssCapabilityScope status
    ] <>
    [ "message" .= message | Just message <- [pedssMessage status] ] <>
    [ "providerId" .= providerId | Just providerId <- [pedssProviderId status] ] <>
    [ "availability" .= availability | Just availability <- [pedssAvailability status] ] <>
    [ "health" .= health | Just health <- [pedssHealth status] ] <>
    [ "accessMode" .= accessMode | Just accessMode <- [pedssAccessMode status] ] <>
    [ "version" .= version | Just version <- [pedssVersion status] ] <>
    [ "compatibility" .= compatibility | Just compatibility <- [pedssCompatibility status] ]

-- | One grant offered by a provider-owned external data source.
data PluginExternalDataSourceGrantDiagnostic = PluginExternalDataSourceGrantDiagnostic
  { pedsgName :: !Text
  , pedsgAccess :: ![RPCExternalDataSourceAccess]
  , pedsgCapabilities :: ![RPCExternalDataSourceCapability]
  , pedsgResources :: ![Text]
  , pedsgStatus :: !Text
  , pedsgStatusSummary :: !Text
  , pedsgStatusDetail :: !PluginExternalDataSourceStatusDiagnostic
  , pedsgAvailability :: !Text
  , pedsgHealth :: !(Maybe Text)
  , pedsgAccessMode :: !(Maybe Text)
  , pedsgFailureReason :: !(Maybe Text)
  , pedsgResourceAvailability :: ![PluginExternalDataSourceResourceDiagnostic]
  , pedsgConfigRefs :: ![PluginExternalDataSourceConfigRefDiagnostic]
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceGrantDiagnostic where
  toJSON grant = object $
    [ "name" .= pedsgName grant
    , "access" .= pedsgAccess grant
    , "capabilities" .= pedsgCapabilities grant
    , "resources" .= pedsgResources grant
    , "status" .= pedsgStatus grant
    , "status_summary" .= pedsgStatusSummary grant
    , "status_detail" .= pedsgStatusDetail grant
    , "availability" .= pedsgAvailability grant
    , "resource_availability" .= pedsgResourceAvailability grant
    , "config_refs" .= pedsgConfigRefs grant
    ] <>
    [ "health" .= health | Just health <- [pedsgHealth grant] ] <>
    [ "access_mode" .= accessMode | Just accessMode <- [pedsgAccessMode grant] ] <>
    [ "failure_reason" .= reason | Just reason <- [pedsgFailureReason grant] ]

-- | Backend-neutral external data-source diagnostics. Provider plugins,
-- adapters, or external systems own the backing data source; topo only reports
-- declarations, grants, consumer references, opaque config-reference presence,
-- status/health, failure reasons, and availability.
data PluginExternalDataSourceDiagnostic = PluginExternalDataSourceDiagnostic
  { pedsRole :: !Text
  , pedsPlugin :: !Text
  , pedsProvider :: !Text
  , pedsConsumer :: !Text
  , pedsSource :: !Text
  , pedsGrant :: !(Maybe Text)
  , pedsResource :: !Text
  , pedsLabel :: !Text
  , pedsKind :: !(Maybe Text)
  , pedsDescription :: !(Maybe Text)
  , pedsRequired :: !(Maybe Bool)
  , pedsResolvedProvider :: !(Maybe Text)
  , pedsBrokerState :: !Text
  , pedsBrokerReason :: !(Maybe Text)
  , pedsAccess :: ![RPCExternalDataSourceAccess]
  , pedsCapabilities :: ![RPCExternalDataSourceCapability]
  , pedsResources :: ![Text]
  , pedsStatus :: !Text
  , pedsStatusSummary :: !Text
  , pedsStatusDetail :: !PluginExternalDataSourceStatusDiagnostic
  , pedsAvailability :: !Text
  , pedsHealth :: !(Maybe Text)
  , pedsAccessMode :: !(Maybe Text)
  , pedsFailureReason :: !(Maybe Text)
  , pedsResourceAvailability :: ![PluginExternalDataSourceResourceDiagnostic]
  , pedsConfigRefs :: ![PluginExternalDataSourceConfigRefDiagnostic]
  , pedsGrants :: ![PluginExternalDataSourceGrantDiagnostic]
  , pedsOwnership :: !Text
  , pedsHostRole :: !Text
  , pedsLifecycleBoundary :: !Text
  , pedsDataReadGrant :: !Bool
  , pedsDataWriteGrant :: !Bool
  } deriving (Eq, Show)

instance ToJSON PluginExternalDataSourceDiagnostic where
  toJSON source = object $
    [ "role" .= pedsRole source
    , "plugin" .= pedsPlugin source
    , "provider" .= pedsProvider source
    , "consumer" .= pedsConsumer source
    , "source" .= pedsSource source
    , "resource" .= pedsResource source
    , "label" .= pedsLabel source
    , "access" .= pedsAccess source
    , "capabilities" .= pedsCapabilities source
    , "resources" .= pedsResources source
    , "broker_state" .= pedsBrokerState source
    , "status" .= pedsStatus source
    , "status_summary" .= pedsStatusSummary source
    , "status_detail" .= pedsStatusDetail source
    , "availability" .= pedsAvailability source
    , "resource_availability" .= pedsResourceAvailability source
    , "config_refs" .= pedsConfigRefs source
    , "grants" .= pedsGrants source
    , "grant_summary" .= object
        [ "data_read" .= pedsDataReadGrant source
        , "data_write" .= pedsDataWriteGrant source
        ]
    , "ownership" .= pedsOwnership source
    , "host_role" .= pedsHostRole source
    , "lifecycle_boundary" .= pedsLifecycleBoundary source
    , "relationship" .= object
        [ "role" .= pedsRole source
        , "plugin" .= pedsPlugin source
        , "provider" .= pedsProvider source
        , "consumer" .= pedsConsumer source
        , "source" .= pedsSource source
        , "grant" .= pedsGrant source
        ]
    ] <>
    [ "grant" .= grant | Just grant <- [pedsGrant source] ] <>
    [ "kind" .= kind | Just kind <- [pedsKind source] ] <>
    [ "description" .= description | Just description <- [pedsDescription source] ] <>
    [ "required" .= required | Just required <- [pedsRequired source] ] <>
    [ "resolved_provider" .= provider | Just provider <- [pedsResolvedProvider source] ] <>
    [ "broker_reason" .= reason | Just reason <- [pedsBrokerReason source] ] <>
    [ "health" .= health | Just health <- [pedsHealth source] ] <>
    [ "access_mode" .= accessMode | Just accessMode <- [pedsAccessMode source] ] <>
    [ "failure_reason" .= reason | Just reason <- [pedsFailureReason source] ]

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
  -- Simulation dependencies name simulation DAG nodes, not plugin/startup providers.
  generatorInsertAfter <> generatorRequires
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

pluginExternalDataSourceDiagnostics :: LoadedPlugin -> [PluginExternalDataSourceDiagnostic]
pluginExternalDataSourceDiagnostics lp =
  pluginExternalDataSourceDiagnosticsFor Set.empty [lp] lp

pluginExternalDataSourceDiagnosticsFor :: Set Text -> [LoadedPlugin] -> LoadedPlugin -> [PluginExternalDataSourceDiagnostic]
pluginExternalDataSourceDiagnosticsFor disabled plugins lp =
  pluginExternalDataSourceDiagnosticsWithProviderReadiness disabled providerReady lp
  where
    providerReady = Map.fromList [(lpName plugin, externalPluginReady disabled plugin) | plugin <- plugins]

pluginExternalDataSourceDiagnosticsWithProviderReadiness :: Set Text -> Map Text Bool -> LoadedPlugin -> [PluginExternalDataSourceDiagnostic]
pluginExternalDataSourceDiagnosticsWithProviderReadiness disabled providerReady lp =
  map (providerExternalDataSourceDiagnostic (lpName lp) ownOverride) (rmExternalDataSources manifest)
  <> map (consumerExternalDataSourceDiagnostic (lpName lp) (consumerStatusFor providerReady ownOverride)) (rmExternalDataSourceRefs manifest)
  where
    manifest = lpManifest lp
    ownOverride = pluginStatusOverride disabled lp

providerExternalDataSourceDiagnostic :: Text -> Maybe (RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus) -> RPCExternalDataSourceDecl -> PluginExternalDataSourceDiagnostic
providerExternalDataSourceDiagnostic pluginName statusOverride source =
  let status = applyStatusOverride statusOverride (redsdStatus source)
      grants = map (grantDiagnostic statusOverride) (redsdGrants source)
  in PluginExternalDataSourceDiagnostic
    { pedsRole = "provider"
    , pedsPlugin = pluginName
    , pedsProvider = pluginName
    , pedsConsumer = "brokered-by-topo"
    , pedsSource = redsdName source
    , pedsGrant = Nothing
    , pedsResource = redsdName source
    , pedsLabel = redsdLabel source
    , pedsKind = Just (redsdKind source)
    , pedsDescription = nonEmptyText (redsdDescription source)
    , pedsRequired = Nothing
    , pedsResolvedProvider = Just pluginName
    , pedsBrokerState = if statusAvailable status then "available" else externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantUnavailable
    , pedsBrokerReason = statusFailureReason status
    , pedsAccess = []
    , pedsCapabilities = redsdCapabilities source
    , pedsResources = redsdResources source
    , pedsStatus = externalStatusStateText (redssState status)
    , pedsStatusSummary = statusSummary status
    , pedsStatusDetail = statusDiagnostic status
    , pedsAvailability = statusAvailabilityText status
    , pedsHealth = externalHealthText <$> redssHealth status
    , pedsAccessMode = externalAccessModeText <$> redssAccessMode status
    , pedsFailureReason = statusFailureReason status
    , pedsResourceAvailability = resourceAvailabilityDiagnostics (redsdResources source) status
    , pedsConfigRefs = map configRefDiagnostic (redsdConfigRefs source)
    , pedsGrants = grants
    , pedsOwnership = "provider-owned"
    , pedsHostRole = "broker"
    , pedsLifecycleBoundary = "external-provider-managed"
    , pedsDataReadGrant = any (grantOffersAccess ExternalAccessRead) (redsdGrants source)
    , pedsDataWriteGrant = anyGrantWriteAccess (redsdGrants source)
    }

consumerExternalDataSourceDiagnostic :: Text -> (RPCExternalDataSourceRef -> RPCExternalDataSourceStatus) -> RPCExternalDataSourceRef -> PluginExternalDataSourceDiagnostic
consumerExternalDataSourceDiagnostic pluginName statusForRef ref =
  let status = statusForRef ref
      providerName = fromMaybe (fromMaybe "unresolved" (redssProviderId status)) (redsrProvider ref)
  in PluginExternalDataSourceDiagnostic
    { pedsRole = "consumer"
    , pedsPlugin = pluginName
    , pedsProvider = providerName
    , pedsConsumer = pluginName
    , pedsSource = redsrSource ref
    , pedsGrant = redsrGrant ref
    , pedsResource = redsrSource ref
    , pedsLabel = externalRefLabel ref
    , pedsKind = Nothing
    , pedsDescription = Nothing
    , pedsRequired = Just (redsrRequired ref)
    , pedsResolvedProvider = Just providerName
    , pedsBrokerState = consumerBrokerState providerName status ref
    , pedsBrokerReason = statusFailureReason status
    , pedsAccess = redsrAccess ref
    , pedsCapabilities = redssCapabilityScope status
    , pedsResources = redsrResources ref
    , pedsStatus = externalStatusStateText (redssState status)
    , pedsStatusSummary = statusSummary status
    , pedsStatusDetail = statusDiagnostic status
    , pedsAvailability = statusAvailabilityText status
    , pedsHealth = externalHealthText <$> redssHealth status
    , pedsAccessMode = externalAccessModeText <$> redssAccessMode status
    , pedsFailureReason = statusFailureReason status
    , pedsResourceAvailability = resourceAvailabilityDiagnostics (redsrResources ref) status
    , pedsConfigRefs = map configRefDiagnostic (redsrConfigRefs ref)
    , pedsGrants = []
    , pedsOwnership = "provider-owned"
    , pedsHostRole = "consumer-router"
    , pedsLifecycleBoundary = "external-provider-managed"
    , pedsDataReadGrant = ExternalAccessRead `elem` redsrAccess ref
    , pedsDataWriteGrant = any (`elem` redsrAccess ref) [ExternalAccessWrite, ExternalAccessAdmin]
    }

consumerBrokerState :: Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceRef -> Text
consumerBrokerState providerName status _ref
  | providerName == "unresolved" = "unresolved"
  | statusAvailable status = externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantAcked
  | redssState status == ExternalStatusUnavailable = externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantUnavailable
  | otherwise = "unresolved"

grantDiagnostic :: Maybe (RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus) -> RPCExternalDataSourceGrant -> PluginExternalDataSourceGrantDiagnostic
grantDiagnostic statusOverride grant =
  let status = applyStatusOverride statusOverride (redsgStatus grant)
  in PluginExternalDataSourceGrantDiagnostic
    { pedsgName = redsgName grant
    , pedsgAccess = redsgAccess grant
    , pedsgCapabilities = redsgCapabilities grant
    , pedsgResources = redsgResources grant
    , pedsgStatus = externalStatusStateText (redssState status)
    , pedsgStatusSummary = statusSummary status
    , pedsgStatusDetail = statusDiagnostic status
    , pedsgAvailability = statusAvailabilityText status
    , pedsgHealth = externalHealthText <$> redssHealth status
    , pedsgAccessMode = externalAccessModeText <$> redssAccessMode status
    , pedsgFailureReason = statusFailureReason status
    , pedsgResourceAvailability = resourceAvailabilityDiagnostics (redsgResources grant) status
    , pedsgConfigRefs = map configRefDiagnostic (redsgConfigRefs grant)
    }

configRefDiagnostic :: RPCExternalDataSourceConfigRef -> PluginExternalDataSourceConfigRefDiagnostic
configRefDiagnostic configRef = PluginExternalDataSourceConfigRefDiagnostic
  { pedscName = redscrName configRef
  , pedscOrigin = externalConfigOriginText (redscrOrigin configRef)
  , pedscRequired = redscrRequired configRef
  , pedscCompatibility = redscrCompatibility configRef
  , pedscKeyPresent = not (Text.null (redscrKey configRef))
  }

resourceAvailabilityDiagnostics :: [Text] -> RPCExternalDataSourceStatus -> [PluginExternalDataSourceResourceDiagnostic]
resourceAvailabilityDiagnostics resources status =
  [ PluginExternalDataSourceResourceDiagnostic
      { pedsrResource = resource
      , pedsrAvailable = statusAvailable status
      , pedsrStatus = statusAvailabilityText status
      , pedsrDetail = statusFailureReason status
      }
  | resource <- resources
  ]

externalRefLabel :: RPCExternalDataSourceRef -> Text
externalRefLabel ref = fromMaybe (redsrName ref) (ruiDisplayName (redsrUiHints ref))

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
  | Text.null value = Nothing
  | otherwise = Just value

grantOffersAccess :: RPCExternalDataSourceAccess -> RPCExternalDataSourceGrant -> Bool
grantOffersAccess access grant = access `elem` redsgAccess grant

anyGrantWriteAccess :: [RPCExternalDataSourceGrant] -> Bool
anyGrantWriteAccess grants = any (grantOffersAccess ExternalAccessWrite) grants
  || any (grantOffersAccess ExternalAccessAdmin) grants

applyStatusOverride :: Maybe (RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus) -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
applyStatusOverride Nothing status = status
applyStatusOverride (Just override) status = override status

consumerStatusFor :: Map Text Bool -> Maybe (RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus) -> RPCExternalDataSourceRef -> RPCExternalDataSourceStatus
consumerStatusFor providerReady ownOverride ref =
  case ownOverride of
    Just override -> override (redsrStatus ref)
    Nothing -> case redsrProvider ref of
      Just providerName
        | not (Map.findWithDefault False providerName providerReady) ->
            unavailableExternalStatus providerName "provider plugin is unavailable" (redsrStatus ref)
      _ -> redsrStatus ref

externalPluginReady :: Set Text -> LoadedPlugin -> Bool
externalPluginReady disabled lp =
  not (pluginDisabledForDiagnostics disabled lp)
    && plsState (lpLifecycle lp) == LifecycleReady

pluginStatusOverride :: Set Text -> LoadedPlugin -> Maybe (RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus)
pluginStatusOverride disabled lp
  | pluginDisabledForDiagnostics disabled lp =
      Just (unavailableExternalStatus (lpName lp) "plugin is disabled")
  | plsState lifecycle == LifecycleReady = Nothing
  | plsState lifecycle == LifecycleDegraded =
      Just (degradedExternalStatus (lpName lp) (pluginRuntimeReason "plugin is degraded" lp))
  | otherwise =
      Just (unavailableExternalStatus (lpName lp) (pluginRuntimeReason ("plugin lifecycle is " <> pluginLifecycleStateText (plsState lifecycle)) lp))
  where
    lifecycle = lpLifecycle lp

pluginRuntimeReason :: Text -> LoadedPlugin -> Text
pluginRuntimeReason prefix lp = prefix <> maybe "" (": " <>) (pluginLastError lp)

unavailableExternalStatus :: Text -> Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
unavailableExternalStatus providerName reason status = status
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }

degradedExternalStatus :: Text -> Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
degradedExternalStatus providerName reason status = status
  { redssState = ExternalStatusDegraded
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityDegraded
  , redssHealth = Just ExternalHealthDegraded
  }

statusDiagnostic :: RPCExternalDataSourceStatus -> PluginExternalDataSourceStatusDiagnostic
statusDiagnostic status = PluginExternalDataSourceStatusDiagnostic
  { pedssState = externalStatusStateText (redssState status)
  , pedssMessage = redssMessage status
  , pedssProviderId = redssProviderId status
  , pedssAvailability = externalAvailabilityText <$> redssAvailability status
  , pedssHealth = externalHealthText <$> redssHealth status
  , pedssAccessMode = externalAccessModeText <$> redssAccessMode status
  , pedssCapabilityScope = redssCapabilityScope status
  , pedssVersion = redssVersion status
  , pedssCompatibility = redssCompatibility status
  }

statusAvailable :: RPCExternalDataSourceStatus -> Bool
statusAvailable status =
  redssState status == ExternalStatusReady
  && redssAvailability status `notElem`
      [ Just ExternalAvailabilityUnknown
      , Just ExternalAvailabilityUnconfigured
      , Just ExternalAvailabilityDegraded
      , Just ExternalAvailabilityUnavailable
      ]
  && redssHealth status `notElem` [Just ExternalHealthDegraded, Just ExternalHealthUnhealthy]
  && redssAccessMode status /= Just ExternalAccessModeDisabled

statusFailureReason :: RPCExternalDataSourceStatus -> Maybe Text
statusFailureReason status
  | statusAvailable status = Nothing
  | Just message <- redssMessage status = Just message
  | otherwise = Just (statusSummary status)

statusSummary :: RPCExternalDataSourceStatus -> Text
statusSummary status = Text.intercalate ", " $ filter (not . Text.null)
  [ "state=" <> externalStatusStateText (redssState status)
  , "availability=" <> statusAvailabilityText status
  , maybe "" (("health=" <>) . externalHealthText) (redssHealth status)
  , maybe "" (("access_mode=" <>) . externalAccessModeText) (redssAccessMode status)
  , capabilitiesSummary (redssCapabilityScope status)
  , maybe "" ("message=" <>) (redssMessage status)
  ]

capabilitiesSummary :: [RPCExternalDataSourceCapability] -> Text
capabilitiesSummary [] = ""
capabilitiesSummary capabilities =
  "capabilities=" <> Text.intercalate "," (map externalCapabilityText capabilities)

statusAvailabilityText :: RPCExternalDataSourceStatus -> Text
statusAvailabilityText status = case redssAvailability status of
  Just availability -> externalAvailabilityText availability
  Nothing -> case redssState status of
    ExternalStatusUnknown -> "unknown"
    ExternalStatusUnconfigured -> "unconfigured"
    ExternalStatusReady -> "available"
    ExternalStatusDegraded -> "degraded"
    ExternalStatusUnavailable -> "unavailable"

externalStatusStateText :: RPCExternalDataSourceStatusState -> Text
externalStatusStateText ExternalStatusUnknown = "unknown"
externalStatusStateText ExternalStatusUnconfigured = "unconfigured"
externalStatusStateText ExternalStatusReady = "ready"
externalStatusStateText ExternalStatusDegraded = "degraded"
externalStatusStateText ExternalStatusUnavailable = "unavailable"

externalAvailabilityText :: RPCExternalDataSourceAvailability -> Text
externalAvailabilityText ExternalAvailabilityUnknown = "unknown"
externalAvailabilityText ExternalAvailabilityAvailable = "available"
externalAvailabilityText ExternalAvailabilityDegraded = "degraded"
externalAvailabilityText ExternalAvailabilityUnavailable = "unavailable"
externalAvailabilityText ExternalAvailabilityUnconfigured = "unconfigured"

externalHealthText :: RPCExternalDataSourceHealth -> Text
externalHealthText ExternalHealthUnknown = "unknown"
externalHealthText ExternalHealthHealthy = "healthy"
externalHealthText ExternalHealthDegraded = "degraded"
externalHealthText ExternalHealthUnhealthy = "unhealthy"

externalAccessModeText :: RPCExternalDataSourceAccessMode -> Text
externalAccessModeText ExternalAccessModeReadOnly = "read_only"
externalAccessModeText ExternalAccessModeReadWrite = "read_write"
externalAccessModeText ExternalAccessModeAdmin = "admin"
externalAccessModeText ExternalAccessModeDisabled = "disabled"
externalAccessModeText ExternalAccessModeProviderManaged = "provider_managed"

externalAccessText :: RPCExternalDataSourceAccess -> Text
externalAccessText ExternalAccessRead = "read"
externalAccessText ExternalAccessWrite = "write"
externalAccessText ExternalAccessAdmin = "admin"

externalCapabilityText :: RPCExternalDataSourceCapability -> Text
externalCapabilityText ExternalSourceQuery = "query"
externalCapabilityText ExternalSourceMutate = "mutate"
externalCapabilityText ExternalSourceSubscribe = "subscribe"
externalCapabilityText ExternalSourceMigrate = "migrate"
externalCapabilityText ExternalSourceHealth = "health"

externalConfigOriginText :: RPCExternalDataSourceConfigOrigin -> Text
externalConfigOriginText ExternalConfigUser = "user"
externalConfigOriginText ExternalConfigProvider = "provider"
externalConfigOriginText ExternalConfigEnvironment = "environment"
externalConfigOriginText ExternalConfigDeployment = "deployment"

pluginDisabledForDiagnostics :: Set Text -> LoadedPlugin -> Bool
pluginDisabledForDiagnostics disabled lp =
  Set.member (lpName lp) disabled
  || (requiresRuntimeConnection (lpManifest lp) && not (rspAutoStart (lpStartPolicy lp)))

pluginPanelDiagnosticLines :: Set Text -> Set Text -> LoadedPlugin -> [Text]
pluginPanelDiagnosticLines disabled availableDeps lp =
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
  ] <> externalDataDetailLines
  where
    lifecycle = lpLifecycle lp
    dependencySummary = case pluginDependencyDiagnostics availableDeps lp of
      [] -> "none declared"
      deps -> Text.intercalate "; "
        [ pddKind dep <> ":" <> pddName dep <> "=" <> pddStatus dep
        | dep <- deps
        ]
    externalDiagnostics = pluginExternalDataSourceDiagnosticsWithProviderReadiness disabled providerReady lp
    providerReady = Map.fromList
      [ (providerName, providerNameAvailable providerName)
      | providerName <- lpName lp : [provider | ref <- rmExternalDataSourceRefs (lpManifest lp), Just provider <- [redsrProvider ref]]
      ]
    providerNameAvailable providerName =
      Set.member ("plugin:" <> providerName) availableDeps
    externalDataSummary = case externalDiagnostics of
      [] -> "none declared"
      sources -> Text.intercalate "; "
        [ pedsRole source <> ":" <> pedsSource source <> "=" <> pedsAvailability source
            <> failureSuffix (pedsFailureReason source)
        | source <- sources
        ]
    externalDataDetailLines = concatMap externalDataSourcePanelLines externalDiagnostics

externalDataSourcePanelLines :: PluginExternalDataSourceDiagnostic -> [Text]
externalDataSourcePanelLines source =
  [ "External " <> pedsRole source <> " " <> pedsSource source
      <> relationshipText source
      <> ": broker=" <> pedsBrokerState source
      <> "; " <> pedsStatusSummary source
      <> "; resources=" <> summaryOrNone (pedsResources source)
      <> failureSuffix (pedsFailureReason source)
  ] <> map grantPanelLine (pedsGrants source)
  where
    relationshipText diag
      | pedsRole diag == "consumer" =
          " <- " <> pedsProvider diag <> maybe "" (":" <>) (pedsGrant diag)
          <> maybe "" (\required -> if required then " required" else " optional") (pedsRequired diag)
      | otherwise = " -> brokered grants"
    grantPanelLine grant =
      "External grant " <> pedsgName grant
        <> ": access=" <> summaryOrNone (map externalAccessText (pedsgAccess grant))
        <> "; " <> pedsgStatusSummary grant
        <> "; resources=" <> summaryOrNone (pedsgResources grant)
        <> failureSuffix (pedsgFailureReason grant)

failureSuffix :: Maybe Text -> Text
failureSuffix Nothing = ""
failureSuffix (Just reason) = "; action=" <> reason

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

-- | Stable identity for a grant that the host has attempted to broker to a
-- consumer. The provider owns the referenced data; this key is only a host
-- lifecycle/revocation handle.
data ExternalDataSourceGrantKey = ExternalDataSourceGrantKey
  { edsgkConsumer :: !Text
  , edsgkRef :: !Text
  , edsgkProvider :: !Text
  , edsgkSource :: !Text
  , edsgkGrant :: !Text
  } deriving (Eq, Ord, Show)

-- | Host-side phase for a brokered external data-source grant.  Only
-- 'ExternalDataSourceGrantAcked' represents a grant that has been accepted and
-- applied by the consumer.
data ExternalDataSourceGrantBrokerPhase
  = ExternalDataSourceGrantPending
  | ExternalDataSourceGrantAcked
  | ExternalDataSourceGrantFailed
  | ExternalDataSourceRevokePending
  | ExternalDataSourceRevokeFailed
  | ExternalDataSourceGrantUnavailable
  deriving (Eq, Ord, Show)

externalDataSourceGrantBrokerPhaseText :: ExternalDataSourceGrantBrokerPhase -> Text
externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantPending = "grant_pending"
externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantAcked = "grant_acked"
externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantFailed = "grant_failed"
externalDataSourceGrantBrokerPhaseText ExternalDataSourceRevokePending = "revoke_pending"
externalDataSourceGrantBrokerPhaseText ExternalDataSourceRevokeFailed = "revoke_failed"
externalDataSourceGrantBrokerPhaseText ExternalDataSourceGrantUnavailable = "unavailable"

externalDataSourceGrantBrokerPhaseApplied :: ExternalDataSourceGrantBrokerPhase -> Bool
externalDataSourceGrantBrokerPhaseApplied ExternalDataSourceGrantAcked = True
externalDataSourceGrantBrokerPhaseApplied _ = False

externalDataSourceGrantBrokerPhaseRevocable :: ExternalDataSourceGrantBrokerPhase -> Bool
-- Acked grants and unresolved revokes must not be dropped without a revoke ACK.
externalDataSourceGrantBrokerPhaseRevocable ExternalDataSourceGrantAcked = True
externalDataSourceGrantBrokerPhaseRevocable ExternalDataSourceRevokePending = True
externalDataSourceGrantBrokerPhaseRevocable ExternalDataSourceRevokeFailed = True
externalDataSourceGrantBrokerPhaseRevocable _ = False

-- | Host-side send/revocation state for a brokered external data-source grant.
data ExternalDataSourceGrantBrokerState = ExternalDataSourceGrantBrokerState
  { edsgbsKey :: !ExternalDataSourceGrantKey
  , edsgbsRequired :: !Bool
  , edsgbsMessage :: !RPCExternalDataSourceGrantMessage
  , edsgbsConsumerReadyAt :: !UTCTime
  , edsgbsProviderReadyAt :: !UTCTime
  , edsgbsState :: !ExternalDataSourceGrantBrokerPhase
  , edsgbsReason :: !(Maybe Text)
  } deriving (Eq, Show)

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
  , pmsPendingRefresh :: !(Maybe (Map Text LoadedPlugin))
    -- ^ Pre-refresh snapshot used to roll back interrupted refreshes.
  , pmsPendingShutdown :: !(Maybe (Map Text LoadedPlugin))
    -- ^ Pre-shutdown snapshot used to roll back interrupted shutdowns.
  , pmsExternalDataSourceGrants :: !(Map ExternalDataSourceGrantKey ExternalDataSourceGrantBrokerState)
    -- ^ Grants the host has sent and must revoke deterministically.
  }

emptyPluginManagerState :: PluginManagerState
emptyPluginManagerState = PluginManagerState
  { pmsPlugins     = Map.empty
  , pmsPluginOrder = []
  , pmsBaseDir     = ""
  , pmsDisabledPlugins = Set.empty
  , pmsPendingRefresh = Nothing
  , pmsPendingShutdown = Nothing
  , pmsExternalDataSourceGrants = Map.empty
  }

-- | Whether a manifest needs a live RPC session to serve its declared
-- capabilities.
requiresRuntimeConnection :: RPCManifest -> Bool
requiresRuntimeConnection manifest =
  isJust (rmGenerator manifest)
  || isJust (rmSimulation manifest)
  || not (null (rmDataResources manifest))
  || not (null (rmExternalDataSources manifest))
  || not (null (rmExternalDataSourceRefs manifest))

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
  lp
    { lpParams = params'
    , lpConnection = fmap (\conn -> conn { rpcParams = params' }) (lpConnection lp)
    }
  where
    params' = sanitizeRPCManifestParams (lpManifest lp) (Map.insert paramName value (lpParams lp))
