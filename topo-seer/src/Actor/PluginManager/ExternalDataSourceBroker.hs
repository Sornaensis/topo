{-# LANGUAGE OverloadedStrings #-}

-- | Host-side brokering of manifest-declared external data-source grants.
module Actor.PluginManager.ExternalDataSourceBroker
  ( reconcileExternalDataSourceBrokering
  , revokeExternalDataSourceBrokeredGrants
  ) where

import Control.Monad (foldM)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)

import Actor.PluginManager.PluginSupervisor
  ( allHostCapabilities
  , loadedPluginDependencyProvider
  , markExternalDataSourceBlocked
  , markExternalDataSourceDegraded
  )
import Actor.PluginManager.Types
  ( ExternalDataSourceGrantBrokerPhase(..)
  , ExternalDataSourceGrantBrokerState(..)
  , ExternalDataSourceGrantKey(..)
  , LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , externalDataSourceGrantBrokerPhaseApplied
  , externalDataSourceGrantBrokerPhaseRevocable
  , externalDataSourceGrantBrokerPhaseText
  )
import Topo.Plugin.Dependency
  ( DependencyExternalDataSourceBinding(..)
  , DependencyExternalDataSourceBindingDiagnostic(..)
  , DependencyExternalDataSourceBindingResolution(..)
  , defaultDependencyResolverInput
  , desbrBindings
  , desbrDiagnostics
  , driAvailableCapabilities
  , driDisabledPlugins
  , resolveExternalDataSourceBindings
  )
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCError(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , externalDataSourceStatusBlocksStartup
  , externalDataSourceStatusCurrent
  , externalDataSourceStatusDegradesStartup
  , applyExternalDataSourceStatusReport
  , requestExternalDataSourceStatus
  , revokedExternalDataSourceStatus
  , rpcErrorText
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  )

-- | Reconcile old brokered grants with the current plugin snapshot: revoke
-- stale grants, send new grants, and annotate consumer refs with brokered
-- status.
reconcileExternalDataSourceBrokering :: PluginManagerState -> PluginManagerState -> IO PluginManagerState
reconcileExternalDataSourceBrokering oldSt newSt = do
  statusSt <- refreshProviderStatuses newSt
  let (desired, bindingDiagnostics) = desiredBrokeredGrants statusSt
      desiredMap = Map.fromList [(edsgbsKey grant, grant) | grant <- desired]
      oldActive = pmsExternalDataSourceGrants oldSt
      oldRevocable = Map.filter (externalDataSourceGrantBrokerPhaseRevocable . edsgbsState) oldActive
      removedKeys = Map.keysSet oldActive `Set.difference` Map.keysSet desiredMap
      changedKeys = Set.fromList
        [ key
        | (key, oldGrant) <- Map.toList oldActive
        , Just newGrant <- [Map.lookup key desiredMap]
        , grantNeedsRefresh oldGrant newGrant
        ]
      revokedKeys = Set.union removedKeys changedKeys
  stAfterRevokes <- foldM revokeOne statusSt (mapMaybe (`Map.lookup` oldRevocable) (Set.toList revokedKeys))
  let dropWithoutRevokeKeys = Set.toList (revokedKeys `Set.difference` Map.keysSet oldRevocable)
      stAfterDrops = stAfterRevokes
        { pmsExternalDataSourceGrants = foldr Map.delete (pmsExternalDataSourceGrants stAfterRevokes) dropWithoutRevokeKeys
        }
  stAfterGrants <- foldM sendOneGrant stAfterDrops (Map.elems desiredMap)
  let active = pmsExternalDataSourceGrants stAfterGrants
      annotated = annotateConsumerRefs bindingDiagnostics stAfterGrants { pmsExternalDataSourceGrants = active }
      requiredDiagnostics =
        [ diag
        | diag <- bindingDiagnostics
        , desbdRequired diag
        ]
      optionalDiagnostics =
        [ diag
        | diag <- bindingDiagnostics
        , not (desbdRequired diag)
        ]
  requiredSt <- foldM blockRequiredBindingDiagnostic annotated requiredDiagnostics
  foldM degradeOptionalBindingDiagnostic requiredSt optionalDiagnostics

grantNeedsRefresh :: ExternalDataSourceGrantBrokerState -> ExternalDataSourceGrantBrokerState -> Bool
grantNeedsRefresh oldGrant newGrant =
  not terminalFailureMadeConsumerUnbrokerable
    && ( edsgbsRequired oldGrant /= edsgbsRequired newGrant
      || grantMessageNeedsRefresh (edsgbsMessage oldGrant) (edsgbsMessage newGrant)
      || edsgbsConsumerReadyAt oldGrant /= edsgbsConsumerReadyAt newGrant
      || edsgbsProviderReadyAt oldGrant /= edsgbsProviderReadyAt newGrant
      || brokerPhaseNeedsRefresh (edsgbsState oldGrant) (edsgbsState newGrant)
      || unavailableReasonChanged
      )
  where
    -- Desired sendable grants are represented as pending.  Previously acked or
    -- failed grants do not need to be resent until their binding/lifecycle
    -- inputs change; broader retry policy is handled by a follow-on task.
    brokerPhaseNeedsRefresh ExternalDataSourceGrantAcked ExternalDataSourceGrantPending = False
    brokerPhaseNeedsRefresh ExternalDataSourceGrantFailed ExternalDataSourceGrantPending = False
    brokerPhaseNeedsRefresh oldPhase newPhase = oldPhase /= newPhase

    unavailableReasonChanged =
      edsgbsState newGrant == ExternalDataSourceGrantUnavailable
        && edsgbsReason oldGrant /= edsgbsReason newGrant

    terminalFailureMadeConsumerUnbrokerable =
      edsgbsState oldGrant `elem` [ExternalDataSourceGrantFailed, ExternalDataSourceRevokeFailed]
        && edsgbsState newGrant == ExternalDataSourceGrantUnavailable
        && maybe False (consumerUnbrokerableReasonForKey (edsgbsKey oldGrant)) (edsgbsReason newGrant)

grantMessageNeedsRefresh :: RPCExternalDataSourceGrantMessage -> RPCExternalDataSourceGrantMessage -> Bool
grantMessageNeedsRefresh oldMessage newMessage =
  normalizeGrantMessageFreshness oldMessage /= normalizeGrantMessageFreshness newMessage

normalizeGrantMessageFreshness :: RPCExternalDataSourceGrantMessage -> RPCExternalDataSourceGrantMessage
normalizeGrantMessageFreshness message = message
  { redsgmStatus = (redsgmStatus message)
      { redssObservedAt = Nothing
      , redssFresh = True
      }
  }

-- | Revoke every active grant in a state. Used before shutdown starts closing
-- consumer transports.
revokeExternalDataSourceBrokeredGrants :: PluginManagerState -> Text -> IO PluginManagerState
revokeExternalDataSourceBrokeredGrants st reason = do
  let revocableGrants = filter (externalDataSourceGrantBrokerPhaseRevocable . edsgbsState) (Map.elems (pmsExternalDataSourceGrants st))
  foldM (revokeOneWithReason reason) st revocableGrants

refreshProviderStatuses :: PluginManagerState -> IO PluginManagerState
refreshProviderStatuses st = do
  plugins' <- traverse refreshPluginStatus (pmsPlugins st)
  pure st { pmsPlugins = plugins' }
  where
    refreshPluginStatus lp
      | not (providerQueryable st lp) = pure lp
      | otherwise = case lpConnection lp of
          Nothing -> pure lp
          Just conn -> do
            result <- requestExternalDataSourceStatus conn statusRequest
            case result of
              Right report -> do
                observedAt <- getCurrentTime
                pure (applyStatusReport observedAt report lp)
              Left err -> do
                observedAt <- getCurrentTime
                pure (markProviderStatusRefreshFailure observedAt err lp)

    statusRequest = RPCExternalDataSourceStatusRequest
      { redssrProviderId = Nothing
      , redssrConsumerId = Nothing
      , redssrSources = []
      , redssrGrants = []
      , redssrIncludeDiagnostics = True
      , redssrReference = Nothing
      }

providerQueryable :: PluginManagerState -> LoadedPlugin -> Bool
providerQueryable st lp =
  not (Set.member (lpName lp) (pmsDisabledPlugins st))
    && pluginConnectionBrokerable lp
    && not (null (rmExternalDataSources (lpManifest lp)))
    && providerHasConsumerRef st lp

pluginConnectionBrokerable :: LoadedPlugin -> Bool
pluginConnectionBrokerable lp =
  lpStatus lp == PluginConnected
    && plsState (lpLifecycle lp) `elem` [LifecycleReady, LifecycleDegraded]

providerHasConsumerRef :: PluginManagerState -> LoadedPlugin -> Bool
providerHasConsumerRef st provider = any referencesProvider (Map.elems (pmsPlugins st))
  where
    providerSources = Set.fromList (map redsdName (rmExternalDataSources (lpManifest provider)))
    referencesProvider consumer = any refMatches (rmExternalDataSourceRefs (lpManifest consumer))
    refMatches ref =
      redsrSource ref `Set.member` providerSources
        && maybe True (== lpName provider) (redsrProvider ref)

applyStatusReport :: UTCTime -> RPCExternalDataSourceStatusReport -> LoadedPlugin -> LoadedPlugin
applyStatusReport observedAt report lp = lp { lpManifest = manifest' , lpConnection = fmap syncConn (lpConnection lp) }
  where
    manifest' = applyExternalDataSourceStatusReport observedAt (lpName lp) report (lpManifest lp)
    syncConn conn = conn { rpcManifest = manifest' }

markProviderStatusRefreshFailure :: UTCTime -> RPCError -> LoadedPlugin -> LoadedPlugin
markProviderStatusRefreshFailure observedAt err lp = lp { lpManifest = manifest' , lpConnection = fmap syncConn (lpConnection lp) }
  where
    manifest = lpManifest lp
    manifest' = manifest { rmExternalDataSources = map markSource (rmExternalDataSources manifest) }
    syncConn conn = conn { rpcManifest = manifest' }

    markSource source = source
      { redsdStatus = failureStatus (redsdStatus source)
      , redsdGrants = map markGrant (redsdGrants source)
      }

    markGrant grant = grant { redsgStatus = failureStatus (redsgStatus grant) }

    failureStatus status = status
      { redssState = ExternalStatusUnavailable
      , redssMessage = Just failureMessage
      , redssProviderId = Just (lpName lp)
      , redssAvailability = Just ExternalAvailabilityUnavailable
      , redssHealth = Just ExternalHealthUnhealthy
      , redssAccessMode = Just ExternalAccessModeDisabled
      , redssCapabilityScope = []
      , redssObservedAt = Just observedAt
      , redssFresh = False
      }

    failureMessage = "external data-source status refresh failed ("
      <> providerStatusRefreshFailureClass err <> "): " <> rpcErrorText err

providerStatusRefreshFailureClass :: RPCError -> Text
providerStatusRefreshFailureClass err = case err of
  RPCTimeout _ -> "timeout"
  RPCTransportError _ -> "transport"
  RPCProtocolError message
    | "unexpected external data-source status response" `Text.isPrefixOf` message -> "unexpected response"
    | otherwise -> "protocol/decode"
  RPCPluginError _ _ -> "plugin error"
  RPCDataResourceError _ _ -> "plugin error"

desiredBrokeredGrants
  :: PluginManagerState
  -> ([ExternalDataSourceGrantBrokerState], [DependencyExternalDataSourceBindingDiagnostic])
desiredBrokeredGrants st = (mapMaybe (bindingToGrant st) (desbrBindings resolution), desbrDiagnostics resolution)
  where
    providers = map loadedPluginDependencyProvider (Map.elems (pmsPlugins st))
    resolverInput = (defaultDependencyResolverInput providers)
      { driAvailableCapabilities = Set.fromList allHostCapabilities
      , driDisabledPlugins = pmsDisabledPlugins st
      }
    resolution = resolveExternalDataSourceBindings resolverInput

bindingToGrant :: PluginManagerState -> DependencyExternalDataSourceBinding -> Maybe ExternalDataSourceGrantBrokerState
bindingToGrant st binding = do
  consumer <- Map.lookup (desbConsumer binding) (pmsPlugins st)
  provider <- Map.lookup (desbProvider binding) (pmsPlugins st)
  ref <- findRef binding consumer
  source <- find ((== desbSource binding) . redsdName) (rmExternalDataSources (lpManifest provider))
  grant <- find ((== desbGrant binding) . redsgName) (redsdGrants source)
  let sendable = consumerGrantSendable st consumer provider
  Just ExternalDataSourceGrantBrokerState
    { edsgbsKey = grantKey binding
    , edsgbsRequired = desbRequired binding
    , edsgbsMessage = grantMessage binding ref grant
    , edsgbsConsumerReadyAt = plsUpdatedAt (lpLifecycle consumer)
    , edsgbsProviderReadyAt = plsUpdatedAt (lpLifecycle provider)
    , edsgbsState = if sendable then ExternalDataSourceGrantPending else ExternalDataSourceGrantUnavailable
    , edsgbsReason = if sendable then Nothing else Just (unsendableGrantReason st consumer provider)
    }

consumerGrantSendable :: PluginManagerState -> LoadedPlugin -> LoadedPlugin -> Bool
consumerGrantSendable st consumer provider =
  not (Set.member (lpName consumer) (pmsDisabledPlugins st))
    && not (Set.member (lpName provider) (pmsDisabledPlugins st))
    && pluginConnectionBrokerable consumer
    && pluginConnectionBrokerable provider
    && maybe False (const True) (lpConnection consumer)

unsendableGrantReason :: PluginManagerState -> LoadedPlugin -> LoadedPlugin -> Text
unsendableGrantReason st consumer provider
  | Set.member (lpName provider) (pmsDisabledPlugins st) =
      "provider plugin '" <> lpName provider <> "' is disabled"
  | Set.member (lpName consumer) (pmsDisabledPlugins st) =
      "consumer plugin '" <> lpName consumer <> "' is disabled"
  | not (pluginConnectionBrokerable provider) =
      "provider plugin '" <> lpName provider <> "' is not brokerable"
  | not (pluginConnectionBrokerable consumer) =
      "consumer plugin '" <> lpName consumer <> "' is not brokerable"
  | otherwise = "consumer connection is unavailable"

consumerUnbrokerableReasonForKey :: ExternalDataSourceGrantKey -> Text -> Bool
consumerUnbrokerableReasonForKey key reason =
  ("consumer plugin '" <> edsgkConsumer key <> "' is not brokerable") `Text.isInfixOf` reason

findRef :: DependencyExternalDataSourceBinding -> LoadedPlugin -> Maybe RPCExternalDataSourceRef
findRef binding consumer = find ((== desbRef binding) . redsrName) (rmExternalDataSourceRefs (lpManifest consumer))

grantKey :: DependencyExternalDataSourceBinding -> ExternalDataSourceGrantKey
grantKey binding = ExternalDataSourceGrantKey
  { edsgkConsumer = desbConsumer binding
  , edsgkRef = desbRef binding
  , edsgkProvider = desbProvider binding
  , edsgkSource = desbSource binding
  , edsgkGrant = desbGrant binding
  }

brokerOperationId :: Text -> ExternalDataSourceGrantKey -> Text
brokerOperationId operation key = Text.intercalate ":"
  [ "external-data-source"
  , operation
  , edsgkConsumer key
  , edsgkRef key
  , edsgkProvider key
  , edsgkSource key
  , edsgkGrant key
  ]

grantMessage
  :: DependencyExternalDataSourceBinding
  -> RPCExternalDataSourceRef
  -> RPCExternalDataSourceGrant
  -> RPCExternalDataSourceGrantMessage
grantMessage binding ref grant = RPCExternalDataSourceGrantMessage
  { redsgmOperationId = Just (brokerOperationId "grant" (grantKey binding))
  , redsgmOperationEpoch = Nothing
  , redsgmProviderId = desbProvider binding
  , redsgmConsumerId = Just (desbConsumer binding)
  , redsgmSource = desbSource binding
  , redsgmGrant = desbGrant binding
  , redsgmAccess = desbAccess binding
  , redsgmResources = desbResources binding
  , redsgmCapabilityScope = desbCapabilityScope binding
  , redsgmStatus = (redsgStatus grant) { redssProviderId = Just (desbProvider binding) }
  , redsgmReference = redsgReference grant
  , redsgmConfigRefs = redsgConfigRefs grant
  , redsgmDiagnostics = redssDiagnostics (redsgStatus grant)
  }

sendOneGrant :: PluginManagerState -> ExternalDataSourceGrantBrokerState -> IO PluginManagerState
sendOneGrant st grantState
  | Map.member key (pmsExternalDataSourceGrants st) = pure st
  | Just staleRevoke <- unresolvedRevokeForRef key st =
      let reason = "external data-source revoke is unresolved for previous grant: "
            <> grantKeyDependency (edsgbsKey staleRevoke)
            <> maybe "" (": " <>) (edsgbsReason staleRevoke)
      in if edsgbsRequired grantState
        then blockConsumer key reason st
        else degradeConsumer key reason st
  | edsgbsState grantState == ExternalDataSourceGrantUnavailable && edsgbsRequired grantState =
      let reason = fromMaybe "required external data-source grant is unavailable" (edsgbsReason grantState)
          stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantUnavailable (Just reason) grantState st
      in blockConsumer key reason stWithDiagnostic
  | edsgbsState grantState == ExternalDataSourceGrantUnavailable =
      let reason = fromMaybe "optional external data-source grant is unavailable" (edsgbsReason grantState)
          stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantUnavailable (Just reason) grantState st
      in degradeConsumer key reason stWithDiagnostic
  | otherwise = case consumerConnection st key of
      Nothing ->
        let reason = if edsgbsRequired grantState
              then "external data-source consumer is not connected for grant send"
              else "external data-source consumer is not connected for optional grant send"
            stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantFailed (Just reason) grantState st
        in if edsgbsRequired grantState
          then blockConsumer key reason stWithDiagnostic
          else degradeConsumer key reason stWithDiagnostic
      Just conn -> do
        let pendingSt = recordGrantDiagnostic ExternalDataSourceGrantPending Nothing grantState st
        result <- sendExternalDataSourceGrant conn (edsgbsMessage grantState)
        case result of
          Right () -> pure pendingSt
            { pmsExternalDataSourceGrants = Map.insert key grantState
                { edsgbsState = ExternalDataSourceGrantAcked, edsgbsReason = Nothing }
                (pmsExternalDataSourceGrants pendingSt)
            }
          Left err ->
            let reasonPrefix = if edsgbsRequired grantState
                  then "failed to send required external data-source grant: "
                  else "failed to send optional external data-source grant: "
                reason = reasonPrefix <> rpcErrorText err
                stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantFailed (Just reason) grantState pendingSt
            in if edsgbsRequired grantState
              then blockConsumer key reason stWithDiagnostic
              else degradeConsumer key reason stWithDiagnostic
  where
    key = edsgbsKey grantState

recordGrantDiagnostic :: ExternalDataSourceGrantBrokerPhase -> Maybe Text -> ExternalDataSourceGrantBrokerState -> PluginManagerState -> PluginManagerState
recordGrantDiagnostic state reason grantState st = st
  { pmsExternalDataSourceGrants = Map.insert (edsgbsKey grantState)
      grantState { edsgbsState = state, edsgbsReason = reason }
      (pmsExternalDataSourceGrants st)
  }

unresolvedRevokeForRef :: ExternalDataSourceGrantKey -> PluginManagerState -> Maybe ExternalDataSourceGrantBrokerState
unresolvedRevokeForRef key st = find matches (Map.elems (pmsExternalDataSourceGrants st))
  where
    matches grantState =
      let staleKey = edsgbsKey grantState
      in edsgkConsumer staleKey == edsgkConsumer key
        && edsgkRef staleKey == edsgkRef key
        && staleKey /= key
        && edsgbsState grantState `elem` [ExternalDataSourceRevokePending, ExternalDataSourceRevokeFailed]

consumerConnection :: PluginManagerState -> ExternalDataSourceGrantKey -> Maybe RPCConnection
consumerConnection st key = do
  consumer <- Map.lookup (edsgkConsumer key) (pmsPlugins st)
  lpConnection consumer

blockConsumer :: ExternalDataSourceGrantKey -> Text -> PluginManagerState -> IO PluginManagerState
blockConsumer key reason st = case Map.lookup (edsgkConsumer key) (pmsPlugins st) of
  Nothing -> pure st
  Just consumer
    | lifecycleHasExternalDataSourceError "external_data_source_blocked" "external data-source startup blocked: " key reason consumer -> pure st
    | plsErrorCode (lpLifecycle consumer) == Just "external_data_source_blocked"
      && consumerUnbrokerableReasonForKey key reason -> pure st
    | otherwise -> do
        blocked <- markExternalDataSourceBlocked (grantKeyDependency key) reason consumer
        pure st { pmsPlugins = Map.insert (lpName blocked) blocked (pmsPlugins st) }

degradeConsumer :: ExternalDataSourceGrantKey -> Text -> PluginManagerState -> IO PluginManagerState
degradeConsumer key reason st = case Map.lookup (edsgkConsumer key) (pmsPlugins st) of
  Nothing -> pure st
  Just consumer
    | plsState (lpLifecycle consumer) == LifecycleFailed -> pure st
    | lifecycleHasExternalDataSourceError "external_data_source_degraded" "external data-source degraded: " key reason consumer -> pure st
    | otherwise -> do
        degraded <- markExternalDataSourceDegraded (grantKeyDependency key) reason consumer
        pure st { pmsPlugins = Map.insert (lpName degraded) degraded (pmsPlugins st) }

lifecycleHasExternalDataSourceError :: Text -> Text -> ExternalDataSourceGrantKey -> Text -> LoadedPlugin -> Bool
lifecycleHasExternalDataSourceError code messagePrefix key reason consumer =
  plsErrorCode lifecycle == Just code
    && plsBlockingDependency lifecycle == Just (grantKeyDependency key)
    && plsErrorMessage lifecycle == Just (messagePrefix <> reason)
  where
    lifecycle = lpLifecycle consumer

blockRequiredBindingDiagnostic :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> IO PluginManagerState
blockRequiredBindingDiagnostic st diag =
  case bindingDiagnosticStatusClass st diag of
    Just BindingDiagnosticSoft -> degradeConsumer key degradedReason st
    _ -> blockConsumer key blockedReason st
  where
    key = diagnosticGrantKey diag
    blockedReason = grantKeyDependency key <> ": " <> desbdMessage diag
    degradedReason = "required external data-source binding degraded: " <> blockedReason

degradeOptionalBindingDiagnostic :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> IO PluginManagerState
degradeOptionalBindingDiagnostic st diag =
  degradeConsumer key reason st
  where
    key = diagnosticGrantKey diag
    reason = "optional external data-source unavailable: " <> grantKeyDependency key <> ": " <> desbdMessage diag

data BindingDiagnosticStatusClass
  = BindingDiagnosticHard
  | BindingDiagnosticSoft

bindingDiagnosticStatusClass :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> Maybe BindingDiagnosticStatusClass
bindingDiagnosticStatusClass st diag
  | bindingDiagnosticProviderHardUnavailable st diag = Just BindingDiagnosticHard
  | any (not . externalDataSourceStatusCurrent) candidateStatuses = Just BindingDiagnosticHard
  | any externalDataSourceStatusBlocksStartup candidateStatuses = Just BindingDiagnosticHard
  | any externalDataSourceStatusDegradesStartup candidateStatuses = Just BindingDiagnosticSoft
  | otherwise = Nothing
  where
    candidateStatuses = bindingDiagnosticCandidateStatuses st diag

bindingDiagnosticProviderHardUnavailable :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> Bool
bindingDiagnosticProviderHardUnavailable st diag = any providerHardUnavailable (Map.toList (pmsPlugins st))
  where
    providerHardUnavailable (providerName, provider) =
      diagnosticProviderMatches providerName
        && providerDeclaresDiagnosticSource provider
        && (Set.member providerName (pmsDisabledPlugins st) || not (pluginConnectionBrokerable provider))
    diagnosticProviderMatches providerName = maybe True (== providerName) (desbdProvider diag)
    providerDeclaresDiagnosticSource provider =
      any ((== desbdSource diag) . redsdName) (rmExternalDataSources (lpManifest provider))

bindingDiagnosticCandidateStatuses :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> [RPCExternalDataSourceStatus]
bindingDiagnosticCandidateStatuses st diag = sourceStatuses <> grantStatuses
  where
    sourceStatuses =
      [ redsdStatus source
      | (providerName, provider) <- Map.toList (pmsPlugins st)
      , diagnosticProviderMatches providerName
      , source <- rmExternalDataSources (lpManifest provider)
      , redsdName source == desbdSource diag
      ]
    grantStatuses =
      [ redsgStatus grant
      | (providerName, provider) <- Map.toList (pmsPlugins st)
      , diagnosticProviderMatches providerName
      , source <- rmExternalDataSources (lpManifest provider)
      , redsdName source == desbdSource diag
      , grant <- redsdGrants source
      , maybe True (== redsgName grant) (desbdGrant diag)
      ]
    diagnosticProviderMatches providerName = maybe True (== providerName) (desbdProvider diag)

diagnosticGrantKey :: DependencyExternalDataSourceBindingDiagnostic -> ExternalDataSourceGrantKey
diagnosticGrantKey diag = ExternalDataSourceGrantKey
  { edsgkConsumer = desbdConsumer diag
  , edsgkRef = desbdRef diag
  , edsgkProvider = fromMaybe "unresolved" (desbdProvider diag)
  , edsgkSource = desbdSource diag
  , edsgkGrant = fromMaybe "unresolved" (desbdGrant diag)
  }

revokeOne :: PluginManagerState -> ExternalDataSourceGrantBrokerState -> IO PluginManagerState
revokeOne = revokeOneWithReason "external data-source binding is no longer brokerable"

revokeOneWithReason :: Text -> PluginManagerState -> ExternalDataSourceGrantBrokerState -> IO PluginManagerState
revokeOneWithReason reason st grantState = do
  let key = edsgbsKey grantState
      pendingSt = recordGrantDiagnostic ExternalDataSourceRevokePending (Just reason) grantState st
      removeActive = pendingSt
        { pmsExternalDataSourceGrants = Map.delete key (pmsExternalDataSourceGrants pendingSt)
        }
      -- If there is no live consumer transport, there is no path to obtain a
      -- revoke ACK; the host explicitly abandons the active grant handle.
      abandonActive = removeActive
  case consumerConnection pendingSt key of
    Nothing
      | edsgbsState grantState `elem` [ExternalDataSourceRevokePending, ExternalDataSourceRevokeFailed] -> pure st
      | otherwise -> pure abandonActive
    Just conn -> do
      result <- sendExternalDataSourceGrantRevocation conn (revocationMessage reason grantState)
      case result of
        Right () -> pure removeActive
        Left err ->
          let failureReason = if edsgbsRequired grantState
                then "failed to revoke required external data-source grant: " <> rpcErrorText err
                else "failed to revoke optional external data-source grant: " <> rpcErrorText err
              failedSt = recordGrantDiagnostic ExternalDataSourceRevokeFailed (Just failureReason) grantState pendingSt
          in if edsgbsRequired grantState
            then blockConsumer key failureReason failedSt
            else degradeConsumer key failureReason failedSt

revocationMessage :: Text -> ExternalDataSourceGrantBrokerState -> RPCExternalDataSourceGrantRevocation
revocationMessage reason grantState = RPCExternalDataSourceGrantRevocation
  { redsrvOperationId = Just (brokerOperationId "revoke" (edsgbsKey grantState))
  , redsrvOperationEpoch = redsgmOperationEpoch message
  , redsrvProviderId = redsgmProviderId message
  , redsrvConsumerId = redsgmConsumerId message
  , redsrvSource = redsgmSource message
  , redsrvGrant = redsgmGrant message
  , redsrvReason = Just reason
  , redsrvStatus = (revokedExternalDataSourceStatus (redsgmProviderId message) (Just reason))
      { redssCompatibility = redssCompatibility (redsgmStatus message) }
  , redsrvReference = redsgmReference message
  , redsrvDiagnostics = redsgmDiagnostics message
  }
  where
    message = edsgbsMessage grantState

annotateConsumerRefs
  :: [DependencyExternalDataSourceBindingDiagnostic]
  -> PluginManagerState
  -> PluginManagerState
annotateConsumerRefs bindingDiagnostics st = st { pmsPlugins = Map.map annotatePlugin (pmsPlugins st) }
  where
    active = pmsExternalDataSourceGrants st
    unresolvedByConsumer = Map.fromListWith (<>)
      [ (desbdConsumer diag, [diag])
      | diag <- bindingDiagnostics
      ]

    annotatePlugin lp = lp { lpManifest = manifest', lpConnection = fmap syncConn (lpConnection lp) }
      where
        manifest = lpManifest lp
        refs = map (annotateRef lp) (rmExternalDataSourceRefs manifest)
        manifest' = manifest { rmExternalDataSourceRefs = refs }
        syncConn conn = conn { rpcManifest = manifest' }

    annotateRef lp ref = case findGrantState (lpName lp) ref of
      Just grantState
        | externalDataSourceGrantBrokerPhaseApplied (edsgbsState grantState) -> ref
            { redsrStatus = (redsgmStatus (edsgbsMessage grantState))
                { redssProviderId = Just (edsgkProvider (edsgbsKey grantState)) }
            }
        | otherwise -> ref
            { redsrStatus = unavailableRefStatus
                (edsgkProvider (edsgbsKey grantState))
                (fromMaybe ("external data-source broker state is " <> externalDataSourceGrantBrokerPhaseText (edsgbsState grantState)) (edsgbsReason grantState))
                (redsrStatus ref)
            }
      Nothing -> case findDiagnostic (lpName lp) ref of
        Just diag -> ref
          { redsrStatus = unavailableRefStatus (fromMaybe "unresolved" (desbdProvider diag)) (desbdMessage diag) (redsrStatus ref)
          }
        Nothing -> ref

    findGrantState consumerName ref =
      let matches = filter
            (\grantState -> let key = edsgbsKey grantState in
              edsgkConsumer key == consumerName
                && edsgkRef key == redsrName ref)
            (Map.elems active)
      in case find (externalDataSourceGrantBrokerPhaseApplied . edsgbsState) matches of
        Just applied -> Just applied
        Nothing -> case matches of
          grantState:_ -> Just grantState
          [] -> Nothing

    findDiagnostic consumerName ref = find
      (\diag -> desbdRef diag == redsrName ref
        && desbdSource diag == redsrSource ref
        && desbdConsumer diag == consumerName
        && maybe True (\providerName -> redsrProvider ref == Just providerName) (desbdProvider diag)
        && maybe True (\grantName -> redsrGrant ref == Just grantName) (desbdGrant diag))
      (Map.findWithDefault [] consumerName unresolvedByConsumer)

unavailableRefStatus :: Text -> Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
unavailableRefStatus providerName reason status = status
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }

grantKeyDependency :: ExternalDataSourceGrantKey -> Text
grantKeyDependency key =
  edsgkProvider key <> ":" <> edsgkSource key <> ":" <> edsgkGrant key
