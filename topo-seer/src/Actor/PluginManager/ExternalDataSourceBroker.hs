{-# LANGUAGE OverloadedStrings #-}

-- | Host-side brokering of manifest-declared external data-source grants.
module Actor.PluginManager.ExternalDataSourceBroker
  ( reconcileExternalDataSourceBrokering
  , revokeExternalDataSourceBrokeredGrants
  ) where

import Control.Monad (foldM)
import Data.IORef (writeIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

import Actor.PluginManager.PluginSupervisor
  ( allHostCapabilities
  , loadedPluginDependencyProvider
  , markExternalDataSourceBlocked
  , markExternalDataSourceDegraded
  )
import Actor.PluginManager.Types
  ( ExternalDataSourceGrantBrokerState(..)
  , ExternalDataSourceGrantKey(..)
  , LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , manifestLifecycleResources
  , pluginLifecycleSnapshot
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
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , defaultRPCExternalDataSourceStatus
  , externalDataSourceStatusBlocksStartup
  , externalDataSourceStatusDegradesStartup
  , requestExternalDataSourceStatus
  , revokedExternalDataSourceStatus
  , rpcErrorText
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  )

-- | Reconcile old sent grants with the current plugin snapshot: revoke stale
-- grants, send new grants, and annotate consumer refs with brokered status.
reconcileExternalDataSourceBrokering :: PluginManagerState -> PluginManagerState -> IO PluginManagerState
reconcileExternalDataSourceBrokering oldSt newSt = do
  statusSt <- refreshProviderStatuses newSt
  let (desired, bindingDiagnostics) = desiredBrokeredGrants statusSt
      desiredMap = Map.fromList [(edsgbsKey grant, grant) | grant <- desired]
      oldActive = pmsExternalDataSourceGrants oldSt
      oldSent = Map.filter ((== "sent") . edsgbsState) oldActive
      removedKeys = Map.keysSet oldActive `Set.difference` Map.keysSet desiredMap
      changedKeys = Set.fromList
        [ key
        | (key, oldGrant) <- Map.toList oldActive
        , Just newGrant <- [Map.lookup key desiredMap]
        , grantNeedsRefresh oldGrant newGrant
        ]
      revokedKeys = Set.union removedKeys changedKeys
  stAfterRevokes <- foldM revokeOne statusSt (mapMaybe (`Map.lookup` oldSent) (Set.toList revokedKeys))
  let dropWithoutRevokeKeys = Set.toList (revokedKeys `Set.difference` Map.keysSet oldSent)
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
  edsgbsRequired oldGrant /= edsgbsRequired newGrant
    || edsgbsMessage oldGrant /= edsgbsMessage newGrant
    || edsgbsConsumerReadyAt oldGrant /= edsgbsConsumerReadyAt newGrant
    || edsgbsProviderReadyAt oldGrant /= edsgbsProviderReadyAt newGrant
    || normalizedBrokerState (edsgbsState oldGrant) /= normalizedBrokerState (edsgbsState newGrant)
  where
    normalizedBrokerState "sent" = "resolved"
    normalizedBrokerState state = state

-- | Revoke every active grant in a state. Used before shutdown starts closing
-- consumer transports.
revokeExternalDataSourceBrokeredGrants :: PluginManagerState -> Text -> IO PluginManagerState
revokeExternalDataSourceBrokeredGrants st reason = do
  let sentGrants = filter ((== "sent") . edsgbsState) (Map.elems (pmsExternalDataSourceGrants st))
  foldM (revokeOneWithReason reason) st sentGrants

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
              Right report -> pure (applyStatusReport report lp)
              Left _ -> do
                writeIORef (rpcRuntimeFailure conn) Nothing
                pure lp

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

applyStatusReport :: RPCExternalDataSourceStatusReport -> LoadedPlugin -> LoadedPlugin
applyStatusReport report lp = lp { lpManifest = manifest' , lpConnection = fmap syncConn (lpConnection lp) }
  where
    manifest = lpManifest lp
    entries = redssReportStatuses report
    manifest' = manifest { rmExternalDataSources = map updateSource (rmExternalDataSources manifest) }
    syncConn conn = conn { rpcManifest = manifest' }

    updateSource source = source
      { redsdStatus = fromMaybe (redsdStatus source) (entryStatus Nothing (redsdName source))
      , redsdConnection = fromMaybe (redsdConnection source) (entryReference Nothing (redsdName source))
      , redsdConfigRefs = fromMaybe (redsdConfigRefs source) (nonEmptyConfigRefs Nothing (redsdName source))
      , redsdGrants = map (updateGrant (redsdName source)) (redsdGrants source)
      }

    updateGrant sourceName grant = grant
      { redsgStatus = fromMaybe (redsgStatus grant) (entryStatus (Just (redsgName grant)) sourceName)
      , redsgReference = fromMaybe (redsgReference grant) (entryReference (Just (redsgName grant)) sourceName)
      , redsgConfigRefs = fromMaybe (redsgConfigRefs grant) (nonEmptyConfigRefs (Just (redsgName grant)) sourceName)
      }

    matchingEntry mGrant sourceName = find
      (\entry -> redsstProviderId entry == lpName lp
        && redsstSource entry == sourceName
        && redsstGrant entry == mGrant)
      entries
    entryStatus mGrant sourceName = redsstStatus <$> matchingEntry mGrant sourceName
    entryReference mGrant sourceName = redsstReference <$> matchingEntry mGrant sourceName
    nonEmptyConfigRefs mGrant sourceName = do
      refs <- redsstConfigRefs <$> matchingEntry mGrant sourceName
      if null refs then Nothing else Just refs

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
    , edsgbsState = if sendable then "resolved" else "unavailable"
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
  | edsgbsState grantState == "unavailable" && edsgbsRequired grantState =
      let reason = fromMaybe "required external data-source grant is unavailable" (edsgbsReason grantState)
          stWithDiagnostic = recordGrantDiagnostic "unavailable" (Just reason) grantState st
      in blockConsumer key reason stWithDiagnostic
  | edsgbsState grantState == "unavailable" =
      let reason = fromMaybe "optional external data-source grant is unavailable" (edsgbsReason grantState)
          stWithDiagnostic = recordGrantDiagnostic "unavailable" (Just reason) grantState st
      in degradeConsumer key reason stWithDiagnostic
  | Map.member key (pmsExternalDataSourceGrants st) = pure st
  | otherwise = case consumerConnection st key of
      Nothing -> if edsgbsRequired grantState
        then blockConsumer key "external data-source consumer is not connected for grant send" st
        else
          let reason = "external data-source consumer is not connected for optional grant send"
              stWithDiagnostic = recordGrantDiagnostic "unavailable" (Just reason) grantState st
          in degradeConsumer key reason stWithDiagnostic
      Just conn -> do
        result <- sendExternalDataSourceGrant conn (edsgbsMessage grantState)
        case result of
          Right () -> pure st
            { pmsExternalDataSourceGrants = Map.insert key grantState
                { edsgbsState = "sent", edsgbsReason = Nothing }
                (pmsExternalDataSourceGrants st)
            }
          Left err
            | edsgbsRequired grantState -> blockConsumer key ("failed to send required external data-source grant: " <> rpcErrorText err) st
            | otherwise ->
                let reason = "failed to send optional external data-source grant: " <> rpcErrorText err
                    stWithDiagnostic = recordGrantDiagnostic "unavailable" (Just reason) grantState st
                in degradeConsumer key reason stWithDiagnostic
  where
    key = edsgbsKey grantState

recordGrantDiagnostic :: Text -> Maybe Text -> ExternalDataSourceGrantBrokerState -> PluginManagerState -> PluginManagerState
recordGrantDiagnostic state reason grantState st = st
  { pmsExternalDataSourceGrants = Map.insert (edsgbsKey grantState)
      grantState { edsgbsState = state, edsgbsReason = reason }
      (pmsExternalDataSourceGrants st)
  }

consumerConnection :: PluginManagerState -> ExternalDataSourceGrantKey -> Maybe RPCConnection
consumerConnection st key = do
  consumer <- Map.lookup (edsgkConsumer key) (pmsPlugins st)
  lpConnection consumer

blockConsumer :: ExternalDataSourceGrantKey -> Text -> PluginManagerState -> IO PluginManagerState
blockConsumer key reason st = case Map.lookup (edsgkConsumer key) (pmsPlugins st) of
  Nothing -> pure st
  Just consumer
    | plsErrorCode (lpLifecycle consumer) == Just "external_data_source_blocked" -> pure st
    | otherwise -> do
        blocked <- markExternalDataSourceBlocked (grantKeyDependency key) reason consumer
        pure st { pmsPlugins = Map.insert (lpName blocked) blocked (pmsPlugins st) }

degradeConsumer :: ExternalDataSourceGrantKey -> Text -> PluginManagerState -> IO PluginManagerState
degradeConsumer key reason st = case Map.lookup (edsgkConsumer key) (pmsPlugins st) of
  Nothing -> pure st
  Just consumer
    | plsState (lpLifecycle consumer) == LifecycleFailed -> pure st
    | plsErrorCode (lpLifecycle consumer) == Just "external_data_source_degraded" -> pure st
    | otherwise -> do
        degraded <- markExternalDataSourceDegraded (grantKeyDependency key) reason consumer
        pure st { pmsPlugins = Map.insert (lpName degraded) degraded (pmsPlugins st) }

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
  case consumerConnection st key of
    Nothing -> pure abandonActive
    Just conn -> do
      result <- sendExternalDataSourceGrantRevocation conn (revocationMessage reason grantState)
      case result of
        Right () -> pure removeActive
        Left err -> pure (recordGrantDiagnostic "sent" (Just ("failed to revoke external data-source grant: " <> rpcErrorText err)) grantState st)
  where
    removeActive = st
      { pmsExternalDataSourceGrants = Map.delete (edsgbsKey grantState) (pmsExternalDataSourceGrants st)
      }
    -- If there is no live consumer transport, there is no path to obtain a
    -- revoke ACK; the host explicitly abandons the active grant handle.
    abandonActive = removeActive

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
        | edsgbsState grantState == "sent" -> ref
            { redsrStatus = (redsgmStatus (edsgbsMessage grantState))
                { redssProviderId = Just (edsgkProvider (edsgbsKey grantState)) }
            }
        | otherwise -> ref
            { redsrStatus = unavailableRefStatus
                (edsgkProvider (edsgbsKey grantState))
                (fromMaybe "external data-source grant was not sent" (edsgbsReason grantState))
                (redsrStatus ref)
            }
      Nothing -> case findDiagnostic (lpName lp) ref of
        Just diag -> ref
          { redsrStatus = unavailableRefStatus (fromMaybe "unresolved" (desbdProvider diag)) (desbdMessage diag) (redsrStatus ref)
          }
        Nothing -> ref

    findGrantState consumerName ref = find
      (\grantState -> let key = edsgbsKey grantState in
        edsgkConsumer key == consumerName
          && edsgkRef key == redsrName ref)
      (Map.elems active)

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
