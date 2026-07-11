{-# LANGUAGE OverloadedStrings #-}

-- | Host-side brokering of manifest-declared external data-source grants.
module Actor.PluginManager.ExternalDataSourceBroker
  ( reconcileExternalDataSourceBrokering
  , revokeExternalDataSourceBrokeredGrants
  ) where

import Control.Monad (foldM)
import Data.IORef (writeIORef)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
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
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceOperationResult(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , externalDataSourceStatusBlocksStartup
  , externalDataSourceStatusCurrent
  , externalDataSourceStatusDegradesStartup
  , externalAccessRequiredCapabilities
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
                pure (applyStatusReport st observedAt report lp)
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

applyStatusReport :: PluginManagerState -> UTCTime -> RPCExternalDataSourceStatusReport -> LoadedPlugin -> LoadedPlugin
applyStatusReport st observedAt report lp = lp { lpManifest = manifest' , lpConnection = fmap syncConn (lpConnection lp) }
  where
    validatedReport = validateProviderStatusReport st lp report
    manifest' = applyExternalDataSourceStatusReport observedAt (lpName lp) validatedReport (lpManifest lp)
    syncConn conn = conn { rpcManifest = manifest' }

validateProviderStatusReport
  :: PluginManagerState
  -> LoadedPlugin
  -> RPCExternalDataSourceStatusReport
  -> RPCExternalDataSourceStatusReport
validateProviderStatusReport st provider report = report
  { redssReportStatuses = map validateEntry (redssReportStatuses report)
  }
  where
    manifest = lpManifest provider
    providerName = lpName provider

    validateEntry entry
      | redsstProviderId entry /= providerName = entry
      | redsstConsumerId entry /= Nothing = entry
      | otherwise = case find ((== redsstSource entry) . redsdName) (rmExternalDataSources manifest) of
          Nothing -> entry
          Just source -> case redsstGrant entry of
            Nothing -> maybe entry (statusReportScopeFailure entry) (sourceEntryScopeMismatch source entry)
            Just grantName -> case find ((== grantName) . redsgName) (redsdGrants source) of
              Nothing -> entry
              Just grant -> maybe entry (statusReportScopeFailure entry) (grantEntryScopeMismatch source grant entry)

    sourceEntryScopeMismatch source entry = firstJust
      [ unlessValid (reportedWithinDeclared (redsstResources entry) (redsdResources source)) $
          statusReportScopeMessage entry $ "reported source resources " <> textListText (redsstResources entry)
            <> " are not a subset of declared source resources " <> textListText (redsdResources source)
      , unlessValid (reportedWithinDeclared (redsstCapabilityScope entry) (redsdCapabilities source)) $
          statusReportScopeMessage entry $ "capability scope mismatch: reported source capability scope " <> capabilityListText (redsstCapabilityScope entry)
            <> " is not a subset of declared source capabilities " <> capabilityListText (redsdCapabilities source)
      ]

    grantEntryScopeMismatch source grant entry = firstJust
      [ unlessValid (reportedWithinDeclared (redsstAccess entry) (redsgAccess grant)) $
          statusReportScopeMessage entry $ "reported grant access " <> accessListText (redsstAccess entry)
            <> " is not a subset of declared grant access " <> accessListText (redsgAccess grant)
      , unlessValid (reportedWithinDeclared reportedResources (grantResourceScope source grant)) $
          statusReportScopeMessage entry $ "reported grant resources " <> textListText (redsstResources entry)
            <> " are not a subset of declared grant resources " <> textListText (grantResourceScope source grant)
      , unlessValid (reportedWithinDeclared (redsstCapabilityScope entry) (redsgCapabilities grant)) $
          statusReportScopeMessage entry $ "capability scope mismatch: reported grant capability scope " <> capabilityListText (redsstCapabilityScope entry)
            <> " is not a subset of declared grant capabilities " <> capabilityListText (redsgCapabilities grant)
      , firstJust (map (requestedScopeMismatch source grant entry) (statusReportConsumerRefs source grant))
      ]
      where
        reportedResources = reportedGrantResources source grant entry

    requestedScopeMismatch source grant entry (consumerName, ref) = firstJust
      [ unlessValid (all (`elem` redsstAccess entry) (redsrAccess ref)) $
          statusReportScopeMessage entry $ "reported grant access " <> accessListText (redsstAccess entry)
            <> " does not include requested access " <> accessListText (redsrAccess ref)
            <> " for consumer ref '" <> consumerName <> ":" <> redsrName ref <> "'"
      , unlessValid (all (`elem` reportedResources) requestedResources) $
          statusReportScopeMessage entry $ "reported grant resources " <> textListText (redsstResources entry)
            <> " do not include requested resources " <> textListText requestedResources
            <> " for consumer ref '" <> consumerName <> ":" <> redsrName ref <> "'"
      , unlessValid (null (redsstCapabilityScope entry) || all (`elem` redsstCapabilityScope entry) requiredCapabilities) $
          statusReportScopeMessage entry $ "capability scope mismatch: reported grant capability scope "
            <> capabilityListText (redsstCapabilityScope entry)
            <> " does not include required capabilities " <> capabilityListText requiredCapabilities
            <> " for requested access " <> accessListText (redsrAccess ref)
            <> " on consumer ref '" <> consumerName <> ":" <> redsrName ref <> "'"
      ]
      where
        requestedResources = effectiveGrantResources source grant (redsrResources ref)
        reportedResources = reportedGrantResources source grant entry
        requiredCapabilities = Set.toList (Set.fromList (concatMap externalAccessRequiredCapabilities (redsrAccess ref)))

    statusReportConsumerRefs source grant =
      [ (lpName consumer, ref)
      | consumer <- Map.elems (pmsPlugins st)
      , ref <- rmExternalDataSourceRefs (lpManifest consumer)
      , redsrSource ref == redsdName source
      , maybe True (== providerName) (redsrProvider ref)
      , maybe True (== redsgName grant) (redsrGrant ref)
      , grantStaticallySatisfiesRef source grant ref
      ]

    grantStaticallySatisfiesRef source grant ref =
      all (`elem` redsgAccess grant) (redsrAccess ref)
        && all (`elem` redsdResources source) (redsrResources ref)
        && all (`elem` grantResourceScope source grant) (redsrResources ref)
        && all (`elem` redsgCapabilities grant) requiredCapabilities
      where
        requiredCapabilities = concatMap externalAccessRequiredCapabilities (redsrAccess ref)

reportedGrantResources :: RPCExternalDataSourceDecl -> RPCExternalDataSourceGrant -> RPCExternalDataSourceStatusEntry -> [Text]
reportedGrantResources source grant entry
  | null (redsstResources entry) = grantResourceScope source grant
  | otherwise = redsstResources entry

reportedWithinDeclared :: Eq a => [a] -> [a] -> Bool
reportedWithinDeclared reported declared = null reported || all (`elem` declared) reported

statusReportScopeMessage :: RPCExternalDataSourceStatusEntry -> Text -> Text
statusReportScopeMessage entry detail =
  "external data-source status report scope mismatch for " <> statusReportEntryName entry <> ": " <> detail

statusReportEntryName :: RPCExternalDataSourceStatusEntry -> Text
statusReportEntryName entry =
  redsstProviderId entry <> ":" <> redsstSource entry <> maybe "" (":" <>) (redsstGrant entry)

statusReportScopeFailure :: RPCExternalDataSourceStatusEntry -> Text -> RPCExternalDataSourceStatusEntry
statusReportScopeFailure entry reason = entry
  { redsstCapabilityScope = []
  , redsstStatus = status
      { redssState = ExternalStatusUnavailable
      , redssMessage = Just reason
      , redssProviderId = Just (redsstProviderId entry)
      , redssAvailability = Just ExternalAvailabilityUnavailable
      , redssHealth = Just ExternalHealthUnhealthy
      , redssAccessMode = Just ExternalAccessModeDisabled
      , redssCapabilityScope = []
      , redssDiagnostics = Just (statusReportScopeDiagnostics entry reason (redssDiagnostics status))
      }
  }
  where
    status = redsstStatus entry

statusReportScopeDiagnostics :: RPCExternalDataSourceStatusEntry -> Text -> Maybe Value -> Value
statusReportScopeDiagnostics entry reason providerDiagnostics = object $
  [ "statusReportValidation" .= object
      [ "result" .= ("rejected" :: Text)
      , "reason" .= reason
      , "providerId" .= redsstProviderId entry
      , "source" .= redsstSource entry
      ]
  ] <>
  [ "grant" .= grant | Just grant <- [redsstGrant entry] ] <>
  [ "providerDiagnostics" .= diagnostics | Just diagnostics <- [providerDiagnostics >>= originalProviderDiagnostics] ]

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
  let key = grantKey binding
      finalized = finalizeGrantMessage binding ref source grant
      sendable = consumerGrantSendable st consumer provider key
        && fgmReason finalized == Nothing
      unavailableReason = fromMaybe (unsendableGrantReason st consumer provider key) (fgmReason finalized)
  Just ExternalDataSourceGrantBrokerState
    { edsgbsKey = key
    , edsgbsRequired = desbRequired binding
    , edsgbsMessage = fgmMessage finalized
    , edsgbsConsumerReadyAt = plsUpdatedAt (lpLifecycle consumer)
    , edsgbsProviderReadyAt = plsUpdatedAt (lpLifecycle provider)
    , edsgbsState = if sendable then ExternalDataSourceGrantPending else ExternalDataSourceGrantUnavailable
    , edsgbsReason = if sendable then Nothing else Just unavailableReason
    , edsgbsGrantResult = Nothing
    , edsgbsRevokeResult = Nothing
    }

consumerGrantSendable :: PluginManagerState -> LoadedPlugin -> LoadedPlugin -> ExternalDataSourceGrantKey -> Bool
consumerGrantSendable st consumer provider key =
  not (Set.member (lpName consumer) (pmsDisabledPlugins st))
    && not (Set.member (lpName provider) (pmsDisabledPlugins st))
    && pluginConnectionBrokerable consumer
    && pluginConnectionBrokerable provider
    && maybe False (const True) (lpConnection consumer)
    && not (consumerExternalDataSourceDataFailureBlocksGrant key consumer)

unsendableGrantReason :: PluginManagerState -> LoadedPlugin -> LoadedPlugin -> ExternalDataSourceGrantKey -> Text
unsendableGrantReason st consumer provider key
  | Set.member (lpName provider) (pmsDisabledPlugins st) =
      "provider plugin '" <> lpName provider <> "' is disabled"
  | Set.member (lpName consumer) (pmsDisabledPlugins st) =
      "consumer plugin '" <> lpName consumer <> "' is disabled"
  | not (pluginConnectionBrokerable provider) =
      "provider plugin '" <> lpName provider <> "' is not brokerable"
  | not (pluginConnectionBrokerable consumer) =
      "consumer plugin '" <> lpName consumer <> "' is not brokerable"
  | consumerExternalDataSourceDataFailureBlocksGrant key consumer =
      fromMaybe "consumer external data-source data operation failed" (plsErrorMessage (lpLifecycle consumer))
  | otherwise = "consumer connection is unavailable"

consumerExternalDataSourceDataFailureBlocksGrant :: ExternalDataSourceGrantKey -> LoadedPlugin -> Bool
consumerExternalDataSourceDataFailureBlocksGrant key consumer =
  plsErrorCode lifecycle == Just "external_data_source_degraded"
    && plsBlockingDependency lifecycle == Just (grantKeyDependency key)
    && maybe False ("external data-source data operation failed" `Text.isInfixOf`) (plsErrorMessage lifecycle)
  where
    lifecycle = lpLifecycle consumer

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

data FinalizedGrantMessage = FinalizedGrantMessage
  { fgmMessage :: RPCExternalDataSourceGrantMessage
  , fgmReason :: Maybe Text
  }

finalizeGrantMessage
  :: DependencyExternalDataSourceBinding
  -> RPCExternalDataSourceRef
  -> RPCExternalDataSourceDecl
  -> RPCExternalDataSourceGrant
  -> FinalizedGrantMessage
finalizeGrantMessage binding ref source grant = FinalizedGrantMessage
  { fgmMessage = grantMessage binding ref grant effectiveScope
  , fgmReason = finalGrantValidationReason binding ref source grant effectiveScope
  }
  where
    statusScope = redssCapabilityScope (redsgStatus grant)
    effectiveScope
      | null statusScope = redsgCapabilities grant
      | otherwise = statusScope

grantMessage
  :: DependencyExternalDataSourceBinding
  -> RPCExternalDataSourceRef
  -> RPCExternalDataSourceGrant
  -> [RPCExternalDataSourceCapability]
  -> RPCExternalDataSourceGrantMessage
grantMessage binding _ref grant effectiveScope = RPCExternalDataSourceGrantMessage
  { redsgmOperationId = Just (brokerOperationId "grant" (grantKey binding))
  , redsgmOperationEpoch = Nothing
  , redsgmProviderId = desbProvider binding
  , redsgmConsumerId = Just (desbConsumer binding)
  , redsgmSource = desbSource binding
  , redsgmGrant = desbGrant binding
  , redsgmAccess = desbAccess binding
  , redsgmResources = desbResources binding
  , redsgmCapabilityScope = effectiveScope
  , redsgmStatus = (redsgStatus grant)
      { redssProviderId = Just (desbProvider binding)
      , redssCapabilityScope = effectiveScope
      }
  , redsgmReference = redsgReference grant
  , redsgmConfigRefs = redsgConfigRefs grant
  , redsgmDiagnostics = redssDiagnostics (redsgStatus grant)
  }

finalGrantValidationReason
  :: DependencyExternalDataSourceBinding
  -> RPCExternalDataSourceRef
  -> RPCExternalDataSourceDecl
  -> RPCExternalDataSourceGrant
  -> [RPCExternalDataSourceCapability]
  -> Maybe Text
finalGrantValidationReason binding ref source grant effectiveScope = firstJust
  [ unlessValid (redsrSource ref == desbSource binding) $
      validationFailed "consumer ref source no longer matches resolved source"
  , unlessValid (maybe True (== desbProvider binding) (redsrProvider ref)) $
      validationFailed "consumer ref provider no longer matches resolved provider"
  , unlessValid (maybe True (== desbGrant binding) (redsrGrant ref)) $
      validationFailed "consumer ref grant no longer matches resolved grant"
  , unlessValid (redsrRequired ref == desbRequired binding) $
      validationFailed "consumer ref required flag no longer matches resolved binding"
  , unlessValid (sameSet (redsrAccess ref) (desbAccess binding)) $
      validationFailed "consumer ref access no longer matches resolved binding access"
  , unlessValid (sameSet requestedResources (desbResources binding)) $
      validationFailed "consumer ref resources no longer match resolved binding resources"
  , unlessValid (all (`elem` redsgAccess grant) (desbAccess binding)) $
      validationFailed $ "requested access " <> accessListText (desbAccess binding)
        <> " is not offered by provider grant access " <> accessListText (redsgAccess grant)
  , unlessValid (all (`elem` redsdResources source) requestedResources) $
      validationFailed $ "requested resources " <> textListText requestedResources
        <> " are not all declared by provider source resources " <> textListText (redsdResources source)
  , unlessValid (all (`elem` providerGrantResources) requestedResources) $
      validationFailed $ "requested resources " <> textListText requestedResources
        <> " are not all offered by provider grant resources " <> textListText providerGrantResources
  , unlessValid (all (`elem` redsdCapabilities source) (redsgCapabilities grant)) $
      scopeMismatch $ "provider grant capabilities " <> capabilityListText (redsgCapabilities grant)
        <> " are not a subset of source capabilities " <> capabilityListText (redsdCapabilities source)
  , unlessValid (sameSet (desbCapabilityScope binding) (redsgCapabilities grant)) $
      scopeMismatch $ "resolved capability scope " <> capabilityListText (desbCapabilityScope binding)
        <> " no longer matches provider grant capabilities " <> capabilityListText (redsgCapabilities grant)
  , unlessValid (all (`elem` redsgCapabilities grant) effectiveScope) $
      scopeMismatch $ "runtime scope " <> capabilityListText effectiveScope
        <> " is not a subset of provider grant capabilities " <> capabilityListText (redsgCapabilities grant)
  , unlessValid (all (`elem` effectiveScope) requiredCapabilities) $
      scopeMismatch $ "runtime scope " <> capabilityListText effectiveScope
        <> " does not include required capabilities " <> capabilityListText requiredCapabilities
        <> " for requested access " <> accessListText (desbAccess binding)
  ]
  where
    requestedResources = effectiveGrantResources source grant (redsrResources ref)
    providerGrantResources = grantResourceScope source grant
    requiredCapabilities = Set.toList (Set.fromList (concatMap externalAccessRequiredCapabilities (desbAccess binding)))
    validationFailed detail = validationPrefix <> detail
    scopeMismatch detail = validationPrefix <> "capability scope mismatch: " <> detail
    validationPrefix = "external data-source grant validation failed for " <> grantKeyDependency (grantKey binding) <> ": "

unlessValid :: Bool -> Text -> Maybe Text
unlessValid True _ = Nothing
unlessValid False reason = Just reason

sameSet :: Ord a => [a] -> [a] -> Bool
sameSet a b = Set.fromList a == Set.fromList b

effectiveGrantResources :: RPCExternalDataSourceDecl -> RPCExternalDataSourceGrant -> [Text] -> [Text]
effectiveGrantResources source grant requested
  | not (null requested) = requested
  | otherwise = grantResourceScope source grant

grantResourceScope :: RPCExternalDataSourceDecl -> RPCExternalDataSourceGrant -> [Text]
grantResourceScope source grant
  | null (redsgResources grant) = redsdResources source
  | otherwise = redsgResources grant

capabilityListText :: [RPCExternalDataSourceCapability] -> Text
capabilityListText capabilities = "[" <> Text.intercalate "," (map externalCapabilityText capabilities) <> "]"

accessListText :: [RPCExternalDataSourceAccess] -> Text
accessListText access = "[" <> Text.intercalate "," (map externalAccessText access) <> "]"

textListText :: [Text] -> Text
textListText values = "[" <> Text.intercalate "," values <> "]"

externalCapabilityText :: RPCExternalDataSourceCapability -> Text
externalCapabilityText ExternalSourceQuery = "query"
externalCapabilityText ExternalSourceMutate = "mutate"
externalCapabilityText ExternalSourceSubscribe = "subscribe"
externalCapabilityText ExternalSourceMigrate = "migrate"
externalCapabilityText ExternalSourceHealth = "health"

externalAccessText :: RPCExternalDataSourceAccess -> Text
externalAccessText ExternalAccessRead = "read"
externalAccessText ExternalAccessWrite = "write"
externalAccessText ExternalAccessAdmin = "admin"

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
  | edsgbsState grantState == ExternalDataSourceGrantUnavailable
      && consumerRefHasBrokerPhase key ExternalDataSourceRevokeAcked st =
      let reason = fromMaybe "external data-source grant is unavailable after revoke" (edsgbsReason grantState)
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
          Right operationResult
            | operationResultApplied operationResult ->
                let appliedGrant = grantState
                      { edsgbsState = ExternalDataSourceGrantAcked
                      , edsgbsReason = Nothing
                      , edsgbsGrantResult = Just operationResult
                      }
                in pure pendingSt
                  { pmsExternalDataSourceGrants = Map.insert key appliedGrant
                      (pmsExternalDataSourceGrants pendingSt)
                  }
            | otherwise -> do
                let reasonPrefix = if edsgbsRequired grantState
                      then "required external data-source grant was not applied by consumer: "
                      else "optional external data-source grant was not applied by consumer: "
                    reason = reasonPrefix <> operationResultReasonText operationResult
                    failedGrant = grantState { edsgbsGrantResult = Just operationResult }
                    stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantFailed (Just reason) failedGrant pendingSt
                if edsgbsRequired grantState
                  then blockConsumer key reason stWithDiagnostic
                  else degradeConsumer key reason stWithDiagnostic
          Left err -> do
            clearBrokerTimeoutRuntimeFailure conn err
            let reasonPrefix = if edsgbsRequired grantState
                  then "failed to send required external data-source grant: "
                  else "failed to send optional external data-source grant: "
                reason = reasonPrefix <> rpcErrorText err
                stWithDiagnostic = recordGrantDiagnostic ExternalDataSourceGrantFailed (Just reason) grantState pendingSt
            if edsgbsRequired grantState
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

consumerRefHasBrokerPhase :: ExternalDataSourceGrantKey -> ExternalDataSourceGrantBrokerPhase -> PluginManagerState -> Bool
consumerRefHasBrokerPhase key phase st = case Map.lookup (edsgkConsumer key) (pmsPlugins st) of
  Nothing -> False
  Just consumer -> any refMatches (rmExternalDataSourceRefs (lpManifest consumer))
  where
    expectedPhase = externalDataSourceGrantBrokerPhaseText phase
    refMatches ref = redsrName ref == edsgkRef key
      && redsrSource ref == edsgkSource key
      && maybe True (== edsgkProvider key) (redsrProvider ref)
      && maybe True (== edsgkGrant key) (redsrGrant ref)
      && brokerStatusPhaseText (redsrStatus ref) == Just expectedPhase

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
    detail = bindingDiagnosticDetailMessage st diag
    blockedReason = grantKeyDependency key <> ": " <> detail
    degradedReason = "required external data-source binding degraded: " <> blockedReason

degradeOptionalBindingDiagnostic :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> IO PluginManagerState
degradeOptionalBindingDiagnostic st diag =
  degradeConsumer key reason st
  where
    key = diagnosticGrantKey diag
    reason = "optional external data-source unavailable: " <> grantKeyDependency key <> ": " <> bindingDiagnosticDetailMessage st diag

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

bindingDiagnosticDetailMessage :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> Text
bindingDiagnosticDetailMessage st diag =
  fromMaybe (desbdMessage diag) (bindingDiagnosticCandidateStatusMessage st diag)

bindingDiagnosticCandidateStatusMessage :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> Maybe Text
bindingDiagnosticCandidateStatusMessage st diag = firstJust
  [ redssMessage status
  | status <- bindingDiagnosticCandidateStatuses st diag
  , bindingDiagnosticStatusActionable status
  ]

bindingDiagnosticStatusActionable :: RPCExternalDataSourceStatus -> Bool
bindingDiagnosticStatusActionable status =
  not (externalDataSourceStatusCurrent status)
    || externalDataSourceStatusBlocksStartup status
    || externalDataSourceStatusDegradesStartup status

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
        Right operationResult
          | operationResultApplied operationResult ->
              pure (recordRevocationResultOnConsumerRef reason operationResult grantState removeActive)
          | otherwise -> do
              let failureReason = if edsgbsRequired grantState
                    then "required external data-source revoke was not applied by consumer: " <> operationResultReasonText operationResult
                    else "optional external data-source revoke was not applied by consumer: " <> operationResultReasonText operationResult
                  failedGrant = grantState { edsgbsRevokeResult = Just operationResult }
                  failedSt = recordGrantDiagnostic ExternalDataSourceRevokeFailed (Just failureReason) failedGrant pendingSt
              if edsgbsRequired grantState
                then blockConsumer key failureReason failedSt
                else degradeConsumer key failureReason failedSt
        Left err -> do
          clearBrokerTimeoutRuntimeFailure conn err
          let failureReason = if edsgbsRequired grantState
                then "failed to revoke required external data-source grant: " <> rpcErrorText err
                else "failed to revoke optional external data-source grant: " <> rpcErrorText err
              failedSt = recordGrantDiagnostic ExternalDataSourceRevokeFailed (Just failureReason) grantState pendingSt
          if edsgbsRequired grantState
            then blockConsumer key failureReason failedSt
            else degradeConsumer key failureReason failedSt

clearBrokerTimeoutRuntimeFailure :: RPCConnection -> RPCError -> IO ()
clearBrokerTimeoutRuntimeFailure conn err =
  case err of
    RPCTimeout _ -> writeIORef (rpcRuntimeFailure conn) Nothing
    _ -> pure ()

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
            { redsrProvider = Just (edsgkProvider (edsgbsKey grantState))
            , redsrStatus = appliedGrantRefStatus grantState (redsrStatus ref)
            }
        | otherwise -> ref
            { redsrProvider = Just (edsgkProvider (edsgbsKey grantState))
            , redsrStatus = unavailableBrokerRefStatus
                (edsgbsState grantState)
                (grantStateOperationResult grantState)
                (edsgkProvider (edsgbsKey grantState))
                (fromMaybe ("external data-source broker state is " <> externalDataSourceGrantBrokerPhaseText (edsgbsState grantState)) (edsgbsReason grantState))
                (redsrStatus ref)
            }
      Nothing -> case findDiagnostic (lpName lp) ref of
        Just diag
          | brokerStatusPhaseText (redsrStatus ref) == Just (externalDataSourceGrantBrokerPhaseText ExternalDataSourceRevokeAcked) -> ref
          | otherwise -> ref
              { redsrProvider = desbdProvider diag
              , redsrStatus = unavailableRefStatus
                  (fromMaybe "unresolved" (desbdProvider diag))
                  (bindingDiagnosticDetailMessage st diag)
                  (redsrStatus ref)
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

recordRevocationResultOnConsumerRef
  :: Text
  -> RPCExternalDataSourceOperationResult
  -> ExternalDataSourceGrantBrokerState
  -> PluginManagerState
  -> PluginManagerState
recordRevocationResultOnConsumerRef reason operationResult grantState st = st
  { pmsPlugins = Map.adjust annotatePlugin (edsgkConsumer key) (pmsPlugins st)
  }
  where
    key = edsgbsKey grantState
    providerName = edsgkProvider key
    message = edsgbsMessage grantState
    baseStatus = (revokedExternalDataSourceStatus providerName (Just (operationResultReasonText operationResult)))
      { redssCompatibility = redssCompatibility (redsgmStatus message)
      , redssDiagnostics = redssDiagnostics (redsgmStatus message)
      }
    revokedStatus = unavailableOperationStatus
      ExternalDataSourceRevokeAcked
      (Just operationResult)
      providerName
      (fromMaybe reason (operationResultReason operationResult))
      baseStatus

    annotatePlugin lp = lp { lpManifest = manifest', lpConnection = fmap syncConn (lpConnection lp) }
      where
        manifest = lpManifest lp
        refs = map annotateRef (rmExternalDataSourceRefs manifest)
        manifest' = manifest { rmExternalDataSourceRefs = refs }
        syncConn conn = conn { rpcManifest = manifest' }

    annotateRef ref
      | redsrName ref == edsgkRef key = ref
          { redsrProvider = Just providerName
          , redsrStatus = revokedStatus
          }
      | otherwise = ref

appliedGrantRefStatus :: ExternalDataSourceGrantBrokerState -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
appliedGrantRefStatus grantState _currentStatus =
  readyOperationStatus
    ExternalDataSourceGrantAcked
    (edsgbsGrantResult grantState)
    (edsgkProvider key)
    (redsgmCapabilityScope message)
    (redsgmStatus message)
  where
    key = edsgbsKey grantState
    message = edsgbsMessage grantState

unavailableBrokerRefStatus
  :: ExternalDataSourceGrantBrokerPhase
  -> Maybe RPCExternalDataSourceOperationResult
  -> Text
  -> Text
  -> RPCExternalDataSourceStatus
  -> RPCExternalDataSourceStatus
unavailableBrokerRefStatus phase operationResult providerName reason =
  unavailableOperationStatus phase operationResult providerName reason

readyOperationStatus
  :: ExternalDataSourceGrantBrokerPhase
  -> Maybe RPCExternalDataSourceOperationResult
  -> Text
  -> [RPCExternalDataSourceCapability]
  -> RPCExternalDataSourceStatus
  -> RPCExternalDataSourceStatus
readyOperationStatus phase operationResult providerName capabilityScope status = status
  { redssState = ExternalStatusReady
  , redssMessage = operationResult >>= operationResultReason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityAvailable
  , redssHealth = Just ExternalHealthHealthy
  , redssAccessMode = readyAccessMode (redssAccessMode status)
  , redssCapabilityScope = if null (redssCapabilityScope status) then capabilityScope else redssCapabilityScope status
  , redssDiagnostics = Just (brokerOperationDiagnostics phase operationResult (redssDiagnostics status))
  }

unavailableOperationStatus
  :: ExternalDataSourceGrantBrokerPhase
  -> Maybe RPCExternalDataSourceOperationResult
  -> Text
  -> Text
  -> RPCExternalDataSourceStatus
  -> RPCExternalDataSourceStatus
unavailableOperationStatus phase operationResult providerName reason status = status
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  , redssDiagnostics = Just (brokerOperationDiagnostics phase operationResult (redssDiagnostics status))
  }

readyAccessMode :: Maybe RPCExternalDataSourceAccessMode -> Maybe RPCExternalDataSourceAccessMode
readyAccessMode (Just ExternalAccessModeDisabled) = Just ExternalAccessModeProviderManaged
readyAccessMode (Just accessMode) = Just accessMode
readyAccessMode Nothing = Just ExternalAccessModeProviderManaged

grantStateOperationResult :: ExternalDataSourceGrantBrokerState -> Maybe RPCExternalDataSourceOperationResult
grantStateOperationResult grantState = case edsgbsState grantState of
  ExternalDataSourceGrantFailed -> edsgbsGrantResult grantState
  ExternalDataSourceRevokeFailed -> edsgbsRevokeResult grantState
  ExternalDataSourceRevokeAcked -> edsgbsRevokeResult grantState
  _ -> Nothing

operationResultApplied :: RPCExternalDataSourceOperationResult -> Bool
operationResultApplied operationResult = redsoAccepted operationResult && redsoApplied operationResult

operationResultReasonText :: RPCExternalDataSourceOperationResult -> Text
operationResultReasonText operationResult = fromMaybe (redsoStatus operationResult) (operationResultReason operationResult)

operationResultReason :: RPCExternalDataSourceOperationResult -> Maybe Text
operationResultReason operationResult = firstJust
  [ redsoError operationResult
  , redsoMessage operationResult
  , nonEmptyText (redsoStatus operationResult)
  ]

brokerOperationDiagnostics
  :: ExternalDataSourceGrantBrokerPhase
  -> Maybe RPCExternalDataSourceOperationResult
  -> Maybe Value
  -> Value
brokerOperationDiagnostics phase operationResult providerDiagnostics = object $
  [ "brokerPhase" .= externalDataSourceGrantBrokerPhaseText phase ] <>
  [ "providerDiagnostics" .= diagnostics | Just diagnostics <- [providerDiagnostics >>= originalProviderDiagnostics] ] <>
  case operationResult of
    Nothing -> []
    Just result ->
      [ "operationId" .= redsoOperationId result
      , "operation" .= redsoOperation result
      , "providerId" .= redsoProviderId result
      , "consumerId" .= redsoConsumerId result
      , "source" .= redsoSource result
      , "grant" .= redsoGrant result
      , "accepted" .= redsoAccepted result
      , "applied" .= redsoApplied result
      , "status" .= redsoStatus result
      ] <>
      [ "operationEpoch" .= epoch | Just epoch <- [redsoOperationEpoch result] ] <>
      [ "message" .= message | Just message <- [redsoMessage result] ] <>
      [ "error" .= err | Just err <- [redsoError result] ] <>
      [ "diagnostics" .= diagnostics | Just diagnostics <- [redsoDiagnostics result] ]

originalProviderDiagnostics :: Value -> Maybe Value
originalProviderDiagnostics diagnostics@(Object fields)
  | KM.member (Key.fromText "brokerPhase") fields = KM.lookup (Key.fromText "providerDiagnostics") fields
  | otherwise = Just diagnostics
originalProviderDiagnostics diagnostics = Just diagnostics

brokerStatusPhaseText :: RPCExternalDataSourceStatus -> Maybe Text
brokerStatusPhaseText status = do
  Object fields <- redssDiagnostics status
  String phase <- KM.lookup (Key.fromText "brokerPhase") fields
  pure phase

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
  | Text.null value = Nothing
  | otherwise = Just value

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just value:_) = Just value

unavailableRefStatus :: Text -> Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
unavailableRefStatus providerName reason status = status
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  , redssDiagnostics = Just (brokerOperationDiagnostics ExternalDataSourceGrantUnavailable Nothing (redssDiagnostics status))
  }

grantKeyDependency :: ExternalDataSourceGrantKey -> Text
grantKeyDependency key =
  edsgkProvider key <> ":" <> edsgkSource key <> ":" <> edsgkGrant key
