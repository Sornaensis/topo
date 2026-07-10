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
  let stAfterDrops = stAfterRevokes
        { pmsExternalDataSourceGrants = foldr Map.delete (pmsExternalDataSourceGrants stAfterRevokes) (Set.toList revokedKeys)
        }
  stAfterGrants <- foldM sendOneGrant stAfterDrops (Map.elems desiredMap)
  let active = pmsExternalDataSourceGrants stAfterGrants
      annotated = annotateConsumerRefs bindingDiagnostics stAfterGrants { pmsExternalDataSourceGrants = active }
      blockingDiagnostics =
        [ diag
        | diag <- bindingDiagnostics
        , desbdRequired diag
        , not (diagnosticHadActiveGrant oldActive diag)
        ]
  foldM blockRequiredBindingDiagnostic annotated blockingDiagnostics

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
  st' <- foldM (revokeOneWithReason reason) st sentGrants
  pure st' { pmsExternalDataSourceGrants = Map.empty }

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
    && lpStatus lp == PluginConnected
    && plsState (lpLifecycle lp) == LifecycleReady
    && not (null (rmExternalDataSources (lpManifest lp)))
    && providerHasConsumerRef st lp

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
    && lpStatus consumer == PluginConnected
    && lpStatus provider == PluginConnected
    && plsState (lpLifecycle consumer) == LifecycleReady
    && plsState (lpLifecycle provider) == LifecycleReady
    && maybe False (const True) (lpConnection consumer)

unsendableGrantReason :: PluginManagerState -> LoadedPlugin -> LoadedPlugin -> Text
unsendableGrantReason st consumer provider
  | Set.member (lpName provider) (pmsDisabledPlugins st) =
      "provider plugin '" <> lpName provider <> "' is disabled"
  | Set.member (lpName consumer) (pmsDisabledPlugins st) =
      "consumer plugin '" <> lpName consumer <> "' is disabled"
  | lpStatus provider /= PluginConnected || plsState (lpLifecycle provider) /= LifecycleReady =
      "provider plugin '" <> lpName provider <> "' is not ready"
  | lpStatus consumer /= PluginConnected || plsState (lpLifecycle consumer) /= LifecycleReady =
      "consumer plugin '" <> lpName consumer <> "' is not ready"
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
      pure (recordGrantDiagnostic "unavailable" (edsgbsReason grantState) grantState st)
  | Map.member key (pmsExternalDataSourceGrants st) = pure st
  | otherwise = case consumerConnection st key of
      Nothing -> if edsgbsRequired grantState
        then blockConsumer key "external data-source consumer is not connected for grant send" st
        else pure (recordGrantDiagnostic "unavailable" (Just "consumer is not connected") grantState st)
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
            | otherwise -> pure (recordGrantDiagnostic "unavailable" (Just (rpcErrorText err)) grantState st)
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
  Just consumer -> do
    blocked <- markExternalDataSourceBlocked (grantKeyDependency key) reason consumer
    pure st { pmsPlugins = Map.insert (lpName blocked) blocked (pmsPlugins st) }

blockRequiredBindingDiagnostic :: PluginManagerState -> DependencyExternalDataSourceBindingDiagnostic -> IO PluginManagerState
blockRequiredBindingDiagnostic st diag =
  blockConsumer key (grantKeyDependency key <> ": " <> desbdMessage diag) st
  where
    key = diagnosticGrantKey diag

diagnosticHadActiveGrant :: Map ExternalDataSourceGrantKey ExternalDataSourceGrantBrokerState -> DependencyExternalDataSourceBindingDiagnostic -> Bool
diagnosticHadActiveGrant active diag = any matches (Map.toList active)
  where
    diagnosticKey = diagnosticGrantKey diag
    matches (key, grantState) = edsgbsState grantState == "sent"
      && edsgkConsumer key == edsgkConsumer diagnosticKey
      && edsgkRef key == edsgkRef diagnosticKey
      && edsgkSource key == edsgkSource diagnosticKey
      && (edsgkProvider diagnosticKey == "unresolved" || edsgkProvider key == edsgkProvider diagnosticKey)
      && (edsgkGrant diagnosticKey == "unresolved" || edsgkGrant key == edsgkGrant diagnosticKey)

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
    Nothing -> pure removeActive
    Just conn -> do
      _ <- sendExternalDataSourceGrantRevocation conn (revocationMessage reason grantState)
      pure removeActive
  where
    removeActive = st
      { pmsExternalDataSourceGrants = Map.delete (edsgbsKey grantState) (pmsExternalDataSourceGrants st)
      }

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
