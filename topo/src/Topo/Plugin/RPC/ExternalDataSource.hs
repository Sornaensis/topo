{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Backend-neutral external data-source protocol payloads.
--
-- These messages let the host broker provider-owned external data-source
-- grants, grant/revoke ACK results, and status snapshots without learning
-- backend-specific connection, migration, schema, locking, or consistency
-- details. Opaque references and diagnostics are preserved as JSON values for
-- providers, consumers, and UI diagnostics to interpret outside topo core.
module Topo.Plugin.RPC.ExternalDataSource
  ( -- * Grant and revocation protocol payloads
    RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceOperation(..)
  , RPCExternalDataSourceOperationResult(..)
    -- * Status protocol payloads
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , applyExternalDataSourceStatusReport
  , externalDataSourceStatusReportFromManifest
  , externalDataSourceGrantStatusEntry
  , externalDataSourceRefStatusEntry
  , externalDataSourceSourceStatusEntry
  , revokedExternalDataSourceStatus
    -- * Startup availability decisions
  , RPCExternalDataSourceStartupDecision(..)
  , externalDataSourceStatusBlocksStartup
  , externalDataSourceStatusDegradesStartup
  , externalDataSourceManifestStartupDecision
  , externalDataSourceStartupDecisionReason
  , externalDataSourceStartupDecisionBlockingDependency
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.=)
  , object
  , withObject
  , withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceConfigRef
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , defaultRPCExternalDataSourceStatus
  , observeExternalDataSourceStatus
  , staleExternalDataSourceStatus
  )

------------------------------------------------------------------------
-- Grant and revocation payloads
------------------------------------------------------------------------

-- | Host-brokered grant for a consumer plugin.
--
-- The payload duplicates the wire-important pieces of the manifest grant so a
-- consumer can receive a concrete binding at startup without requiring topo to
-- interpret the provider's opaque reference metadata.
data RPCExternalDataSourceGrantMessage = RPCExternalDataSourceGrantMessage
  { redsgmOperationId    :: !(Maybe Text)
    -- ^ Stable host/broker operation identifier used by ACK/result payloads.
    -- Older grant messages may omit this and decode as 'Nothing'.
  , redsgmOperationEpoch :: !(Maybe Word64)
    -- ^ Optional broker epoch for hosts that version replayed grant sends.
  , redsgmProviderId      :: !Text
  , redsgmConsumerId      :: !(Maybe Text)
  , redsgmSource          :: !Text
  , redsgmGrant           :: !Text
  , redsgmAccess          :: ![RPCExternalDataSourceAccess]
  , redsgmResources       :: ![Text]
  , redsgmCapabilityScope :: ![RPCExternalDataSourceCapability]
  , redsgmStatus          :: !RPCExternalDataSourceStatus
  , redsgmReference       :: !(Maybe Value)
  , redsgmConfigRefs      :: ![RPCExternalDataSourceConfigRef]
  , redsgmDiagnostics     :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceGrantMessage where
  parseJSON = withObject "RPCExternalDataSourceGrantMessage" $ \o -> do
    operationId <- optionalNonNullField o ["operationId", "operation_id", "brokerOperationId", "broker_operation_id"]
    operationEpoch <- optionalNonNullField o ["operationEpoch", "operation_epoch", "epoch"]
    providerId <- requiredAliasedField o ["providerId", "provider_id", "provider"]
    consumerId <- optionalNonNullField o ["consumerId", "consumer_id", "consumer"]
    source <- requiredAliasedField o ["source", "sourceName", "source_name"]
    grant <- requiredAliasedField o ["grant", "grantName", "grant_name"]
    access <- optionalNonNullListField o ["access"]
    resources <- optionalNonNullListField o ["resources"]
    capabilityScope <- optionalNonNullListField o ["capabilityScope", "capability_scope", "capabilities"]
    status <- optionalAliasedField o ["status"] defaultRPCExternalDataSourceStatus
    reference <- optionalNonNullField o ["reference"]
    configRefs <- optionalNonNullListField o ["configRefs", "config_refs"]
    diagnostics <- optionalNonNullField o ["diagnostics"]
    pure RPCExternalDataSourceGrantMessage
      { redsgmOperationId = operationId
      , redsgmOperationEpoch = operationEpoch
      , redsgmProviderId = providerId
      , redsgmConsumerId = consumerId
      , redsgmSource = source
      , redsgmGrant = grant
      , redsgmAccess = access
      , redsgmResources = resources
      , redsgmCapabilityScope = effectiveCapabilityScope status capabilityScope
      , redsgmStatus = status
      , redsgmReference = reference
      , redsgmConfigRefs = configRefs
      , redsgmDiagnostics = diagnostics
      }

instance ToJSON RPCExternalDataSourceGrantMessage where
  toJSON grant = object $
    [ "operationId" .= operationId | Just operationId <- [redsgmOperationId grant] ] <>
    [ "operationEpoch" .= operationEpoch | Just operationEpoch <- [redsgmOperationEpoch grant] ] <>
    [ "providerId" .= redsgmProviderId grant
    , "source" .= redsgmSource grant
    , "grant" .= redsgmGrant grant
    , "access" .= redsgmAccess grant
    , "resources" .= redsgmResources grant
    , "capabilityScope" .= redsgmCapabilityScope grant
    , "status" .= redsgmStatus grant
    ] <>
    [ "consumerId" .= consumerId | Just consumerId <- [redsgmConsumerId grant] ] <>
    [ "reference" .= reference | Just reference <- [redsgmReference grant] ] <>
    [ "configRefs" .= redsgmConfigRefs grant | not (null (redsgmConfigRefs grant)) ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redsgmDiagnostics grant] ]

-- | Host notification that a previously brokered grant is no longer usable.
data RPCExternalDataSourceGrantRevocation = RPCExternalDataSourceGrantRevocation
  { redsrvOperationId    :: !(Maybe Text)
    -- ^ Stable host/broker operation identifier used by ACK/result payloads.
    -- Older revocation messages may omit this and decode as 'Nothing'.
  , redsrvOperationEpoch :: !(Maybe Word64)
    -- ^ Optional broker epoch for hosts that version replayed revocations.
  , redsrvProviderId  :: !Text
  , redsrvConsumerId  :: !(Maybe Text)
  , redsrvSource      :: !Text
  , redsrvGrant       :: !Text
  , redsrvReason      :: !(Maybe Text)
  , redsrvStatus      :: !RPCExternalDataSourceStatus
  , redsrvReference   :: !(Maybe Value)
  , redsrvDiagnostics :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceGrantRevocation where
  parseJSON = withObject "RPCExternalDataSourceGrantRevocation" $ \o -> do
    operationId <- optionalNonNullField o ["operationId", "operation_id", "brokerOperationId", "broker_operation_id"]
    operationEpoch <- optionalNonNullField o ["operationEpoch", "operation_epoch", "epoch"]
    providerId <- requiredAliasedField o ["providerId", "provider_id", "provider"]
    consumerId <- optionalNonNullField o ["consumerId", "consumer_id", "consumer"]
    source <- requiredAliasedField o ["source", "sourceName", "source_name"]
    grant <- requiredAliasedField o ["grant", "grantName", "grant_name"]
    reason <- optionalNonNullField o ["reason", "message"]
    status <- optionalAliasedField o ["status"] (revokedExternalDataSourceStatus providerId reason)
    reference <- optionalNonNullField o ["reference"]
    diagnostics <- optionalNonNullField o ["diagnostics"]
    pure RPCExternalDataSourceGrantRevocation
      { redsrvOperationId = operationId
      , redsrvOperationEpoch = operationEpoch
      , redsrvProviderId = providerId
      , redsrvConsumerId = consumerId
      , redsrvSource = source
      , redsrvGrant = grant
      , redsrvReason = reason
      , redsrvStatus = status
      , redsrvReference = reference
      , redsrvDiagnostics = diagnostics
      }

instance ToJSON RPCExternalDataSourceGrantRevocation where
  toJSON revocation = object $
    [ "operationId" .= operationId | Just operationId <- [redsrvOperationId revocation] ] <>
    [ "operationEpoch" .= operationEpoch | Just operationEpoch <- [redsrvOperationEpoch revocation] ] <>
    [ "providerId" .= redsrvProviderId revocation
    , "source" .= redsrvSource revocation
    , "grant" .= redsrvGrant revocation
    , "status" .= redsrvStatus revocation
    ] <>
    [ "consumerId" .= consumerId | Just consumerId <- [redsrvConsumerId revocation] ] <>
    [ "reason" .= reason | Just reason <- [redsrvReason revocation] ] <>
    [ "reference" .= reference | Just reference <- [redsrvReference revocation] ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redsrvDiagnostics revocation] ]

-- | External data-source operation kind acknowledged by a plugin result.
data RPCExternalDataSourceOperation
  = ExternalDataSourceGrantOperation
  | ExternalDataSourceRevokeOperation
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceOperation where
  parseJSON = withText "RPCExternalDataSourceOperation" $ \t -> case t of
    "grant"      -> pure ExternalDataSourceGrantOperation
    "granted"    -> pure ExternalDataSourceGrantOperation
    "revoke"     -> pure ExternalDataSourceRevokeOperation
    "revocation" -> pure ExternalDataSourceRevokeOperation
    "revoked"    -> pure ExternalDataSourceRevokeOperation
    _            -> fail ("unknown external data-source operation: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceOperation where
  toJSON ExternalDataSourceGrantOperation = "grant"
  toJSON ExternalDataSourceRevokeOperation = "revoke"

-- | Plugin ACK/result for a host-brokered external data-source grant or revoke.
--
-- This payload is intentionally only a protocol shape. Host state-machine
-- handling of pending/applied/failed operations is implemented separately.
data RPCExternalDataSourceOperationResult = RPCExternalDataSourceOperationResult
  { redsoOperationId    :: !Text
    -- ^ Stable operation identifier from the grant/revoke payload.
  , redsoOperationEpoch :: !(Maybe Word64)
    -- ^ Optional broker epoch echoed from the grant/revoke payload.
  , redsoOperation      :: !RPCExternalDataSourceOperation
  , redsoProviderId     :: !Text
  , redsoConsumerId     :: !Text
  , redsoSource         :: !Text
  , redsoGrant          :: !Text
  , redsoAccepted       :: !Bool
    -- ^ The consumer accepted the operation for processing.
  , redsoApplied        :: !Bool
    -- ^ The consumer applied the grant/revoke to its runtime state.
  , redsoStatus         :: !Text
    -- ^ Stable status text such as @accepted@, @applied@, @rejected@, or @failed@.
  , redsoMessage        :: !(Maybe Text)
  , redsoError          :: !(Maybe Text)
  , redsoDiagnostics    :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceOperationResult where
  parseJSON = withObject "RPCExternalDataSourceOperationResult" $ \o -> do
    operationId <- requiredAliasedField o ["operationId", "operation_id", "brokerOperationId", "broker_operation_id"]
    operationEpoch <- optionalNonNullField o ["operationEpoch", "operation_epoch", "epoch"]
    operation <- requiredAliasedField o ["operation", "operationType", "operation_type", "action"]
    providerId <- requiredAliasedField o ["providerId", "provider_id", "provider"]
    consumerId <- requiredAliasedField o ["consumerId", "consumer_id", "consumer"]
    source <- requiredAliasedField o ["source", "sourceName", "source_name"]
    grant <- requiredAliasedField o ["grant", "grantName", "grant_name"]
    accepted <- requiredAliasedField o ["accepted"]
    applied <- requiredAliasedField o ["applied"]
    status <- requiredAliasedField o ["status"]
    message <- optionalNullableField o ["message", "reason"]
    err <- optionalNullableField o ["error", "errorMessage", "error_message"]
    diagnostics <- optionalNonNullField o ["diagnostics"]
    pure RPCExternalDataSourceOperationResult
      { redsoOperationId = operationId
      , redsoOperationEpoch = operationEpoch
      , redsoOperation = operation
      , redsoProviderId = providerId
      , redsoConsumerId = consumerId
      , redsoSource = source
      , redsoGrant = grant
      , redsoAccepted = accepted
      , redsoApplied = applied
      , redsoStatus = status
      , redsoMessage = message
      , redsoError = err
      , redsoDiagnostics = diagnostics
      }

instance ToJSON RPCExternalDataSourceOperationResult where
  toJSON result = object $
    [ "operationId" .= redsoOperationId result
    , "operation" .= redsoOperation result
    , "providerId" .= redsoProviderId result
    , "consumerId" .= redsoConsumerId result
    , "source" .= redsoSource result
    , "grant" .= redsoGrant result
    , "accepted" .= redsoAccepted result
    , "applied" .= redsoApplied result
    , "status" .= redsoStatus result
    , "message" .= redsoMessage result
    , "error" .= redsoError result
    ] <>
    [ "operationEpoch" .= operationEpoch | Just operationEpoch <- [redsoOperationEpoch result] ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redsoDiagnostics result] ]

-- | Canonical unavailable status used when the host revokes a grant.
revokedExternalDataSourceStatus :: Text -> Maybe Text -> RPCExternalDataSourceStatus
revokedExternalDataSourceStatus providerId reason = defaultRPCExternalDataSourceStatus
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just (fromMaybe "external data-source grant revoked" reason)
  , redssProviderId = Just providerId
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }

------------------------------------------------------------------------
-- Status payloads
------------------------------------------------------------------------

-- | Host request for external data-source status. Empty filters ask the plugin
-- to report every provider source, grant, and consumer reference it declared.
data RPCExternalDataSourceStatusRequest = RPCExternalDataSourceStatusRequest
  { redssrProviderId         :: !(Maybe Text)
  , redssrConsumerId         :: !(Maybe Text)
  , redssrSources            :: ![Text]
  , redssrGrants             :: ![Text]
  , redssrIncludeDiagnostics :: !Bool
  , redssrReference          :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceStatusRequest where
  parseJSON = withObject "RPCExternalDataSourceStatusRequest" $ \o -> do
    providerId <- optionalNonNullField o ["providerId", "provider_id", "provider"]
    consumerId <- optionalNonNullField o ["consumerId", "consumer_id", "consumer"]
    source <- optionalNonNullField o ["source", "sourceName", "source_name"]
    sources <- optionalNonNullListField o ["sources"]
    grant <- optionalNonNullField o ["grant", "grantName", "grant_name"]
    grants <- optionalNonNullListField o ["grants"]
    includeDiagnostics <- optionalAliasedField o ["includeDiagnostics", "include_diagnostics"] False
    reference <- optionalNonNullField o ["reference"]
    pure RPCExternalDataSourceStatusRequest
      { redssrProviderId = providerId
      , redssrConsumerId = consumerId
      , redssrSources = nub (maybeToList source <> sources)
      , redssrGrants = nub (maybeToList grant <> grants)
      , redssrIncludeDiagnostics = includeDiagnostics
      , redssrReference = reference
      }

instance ToJSON RPCExternalDataSourceStatusRequest where
  toJSON request = object $
    [ "sources" .= redssrSources request
    , "grants" .= redssrGrants request
    , "includeDiagnostics" .= redssrIncludeDiagnostics request
    ] <>
    [ "providerId" .= providerId | Just providerId <- [redssrProviderId request] ] <>
    [ "consumerId" .= consumerId | Just consumerId <- [redssrConsumerId request] ] <>
    [ "reference" .= reference | Just reference <- [redssrReference request] ]

-- | A single backend-neutral status entry for a source, grant, or consumer ref.
data RPCExternalDataSourceStatusEntry = RPCExternalDataSourceStatusEntry
  { redsstProviderId      :: !Text
  , redsstConsumerId      :: !(Maybe Text)
  , redsstSource          :: !Text
  , redsstGrant           :: !(Maybe Text)
  , redsstAccess          :: ![RPCExternalDataSourceAccess]
  , redsstResources       :: ![Text]
  , redsstCapabilityScope :: ![RPCExternalDataSourceCapability]
  , redsstStatus          :: !RPCExternalDataSourceStatus
  , redsstReference       :: !(Maybe Value)
  , redsstConfigRefs      :: ![RPCExternalDataSourceConfigRef]
  , redsstDiagnostics     :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceStatusEntry where
  parseJSON = withObject "RPCExternalDataSourceStatusEntry" $ \o -> do
    providerId <- requiredAliasedField o ["providerId", "provider_id", "provider"]
    consumerId <- optionalNonNullField o ["consumerId", "consumer_id", "consumer"]
    source <- requiredAliasedField o ["source", "sourceName", "source_name"]
    grant <- optionalNonNullField o ["grant", "grantName", "grant_name"]
    access <- optionalNonNullListField o ["access"]
    resources <- optionalNonNullListField o ["resources"]
    capabilityScope <- optionalNonNullListField o ["capabilityScope", "capability_scope", "capabilities"]
    status <- requiredAliasedField o ["status"]
    reference <- optionalNonNullField o ["reference"]
    configRefs <- optionalNonNullListField o ["configRefs", "config_refs"]
    diagnostics <- optionalNonNullField o ["diagnostics"]
    pure RPCExternalDataSourceStatusEntry
      { redsstProviderId = providerId
      , redsstConsumerId = consumerId
      , redsstSource = source
      , redsstGrant = grant
      , redsstAccess = access
      , redsstResources = resources
      , redsstCapabilityScope = effectiveCapabilityScope status capabilityScope
      , redsstStatus = status
      , redsstReference = reference
      , redsstConfigRefs = configRefs
      , redsstDiagnostics = diagnostics
      }

instance ToJSON RPCExternalDataSourceStatusEntry where
  toJSON entry = object $
    [ "providerId" .= redsstProviderId entry
    , "source" .= redsstSource entry
    , "access" .= redsstAccess entry
    , "resources" .= redsstResources entry
    , "capabilityScope" .= redsstCapabilityScope entry
    , "status" .= redsstStatus entry
    ] <>
    [ "consumerId" .= consumerId | Just consumerId <- [redsstConsumerId entry] ] <>
    [ "grant" .= grant | Just grant <- [redsstGrant entry] ] <>
    [ "reference" .= reference | Just reference <- [redsstReference entry] ] <>
    [ "configRefs" .= redsstConfigRefs entry | not (null (redsstConfigRefs entry)) ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redsstDiagnostics entry] ]

-- | Status response. The list is intentionally generic so providers can report
-- source-level, grant-level, or consumer-reference statuses in one response.
data RPCExternalDataSourceStatusReport = RPCExternalDataSourceStatusReport
  { redssReportStatuses    :: ![RPCExternalDataSourceStatusEntry]
  , redssReportDiagnostics :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceStatusReport where
  parseJSON = withObject "RPCExternalDataSourceStatusReport" $ \o ->
    RPCExternalDataSourceStatusReport
      <$> optionalNonNullListField o ["statuses", "sources"]
      <*> optionalNonNullField o ["diagnostics"]

instance ToJSON RPCExternalDataSourceStatusReport where
  toJSON report = object $
    [ "statuses" .= redssReportStatuses report ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redssReportDiagnostics report] ]

-- | Apply a successful provider status report to a manifest snapshot.
-- Reported source/grant entries are stamped with the host observation time.
-- Declared source/grant entries missing from the report are retained only as
-- stale diagnostic history and must not be considered brokerable.
applyExternalDataSourceStatusReport
  :: UTCTime
  -> Text
  -> RPCExternalDataSourceStatusReport
  -> RPCManifest
  -> RPCManifest
applyExternalDataSourceStatusReport observedAt providerId report manifest =
  manifest { rmExternalDataSources = map updateSource (rmExternalDataSources manifest) }
  where
    entries = redssReportStatuses report

    updateSource source = source
      { redsdStatus = entryStatus Nothing (redsdName source) (redsdStatus source)
      , redsdConnection = fromMaybe (redsdConnection source) (entryReference Nothing (redsdName source))
      , redsdConfigRefs = fromMaybe (redsdConfigRefs source) (nonEmptyConfigRefs Nothing (redsdName source))
      , redsdGrants = map (updateGrant (redsdName source)) (redsdGrants source)
      }

    updateGrant sourceName grant = grant
      { redsgStatus = entryStatus (Just (redsgName grant)) sourceName (redsgStatus grant)
      , redsgReference = fromMaybe (redsgReference grant) (entryReference (Just (redsgName grant)) sourceName)
      , redsgConfigRefs = fromMaybe (redsgConfigRefs grant) (nonEmptyConfigRefs (Just (redsgName grant)) sourceName)
      }

    matchingEntry mGrant sourceName = find
      (\entry -> redsstProviderId entry == providerId
        && redsstConsumerId entry == Nothing
        && redsstSource entry == sourceName
        && redsstGrant entry == mGrant)
      entries

    entryStatus mGrant sourceName currentStatus =
      case matchingEntry mGrant sourceName of
        Just entry -> observeExternalDataSourceStatus observedAt (redsstStatus entry)
        Nothing -> staleExternalDataSourceStatus currentStatus

    entryReference mGrant sourceName = redsstReference <$> matchingEntry mGrant sourceName

    nonEmptyConfigRefs mGrant sourceName = do
      refs <- redsstConfigRefs <$> matchingEntry mGrant sourceName
      if null refs then Nothing else Just refs

-- | Produce a status report from a manifest snapshot and request filters.
-- This is used by the SDK's default status handler and keeps status reporting
-- limited to backend-neutral manifest data.
externalDataSourceStatusReportFromManifest
  :: RPCManifest
  -> RPCExternalDataSourceStatusRequest
  -> RPCExternalDataSourceStatusReport
externalDataSourceStatusReportFromManifest manifest request =
  RPCExternalDataSourceStatusReport
    { redssReportStatuses = map (stripDiagnosticsIfNeeded request) filteredEntries
    , redssReportDiagnostics = Nothing
    }
  where
    providerId = rmName manifest
    entries = concatMap (sourceEntries providerId) (rmExternalDataSources manifest)
      <> map externalDataSourceRefStatusEntry (rmExternalDataSourceRefs manifest)
    filteredEntries = filter (matchesStatusRequest request) entries

sourceEntries :: Text -> RPCExternalDataSourceDecl -> [RPCExternalDataSourceStatusEntry]
sourceEntries providerId source =
  externalDataSourceSourceStatusEntry providerId source
    : map (externalDataSourceGrantStatusEntry providerId (redsdName source)) (redsdGrants source)

-- | Build a source-level status entry for a provider declaration.
externalDataSourceSourceStatusEntry
  :: Text
  -> RPCExternalDataSourceDecl
  -> RPCExternalDataSourceStatusEntry
externalDataSourceSourceStatusEntry providerId source =
  let status = redsdStatus source
  in RPCExternalDataSourceStatusEntry
    { redsstProviderId = providerId
    , redsstConsumerId = Nothing
    , redsstSource = redsdName source
    , redsstGrant = Nothing
    , redsstAccess = []
    , redsstResources = redsdResources source
    , redsstCapabilityScope = effectiveCapabilityScope status (redsdCapabilities source)
    , redsstStatus = status
    , redsstReference = redsdConnection source
    , redsstConfigRefs = redsdConfigRefs source
    , redsstDiagnostics = redssDiagnostics status
    }

-- | Build a grant-level status entry for a provider grant.
externalDataSourceGrantStatusEntry
  :: Text
  -> Text
  -> RPCExternalDataSourceGrant
  -> RPCExternalDataSourceStatusEntry
externalDataSourceGrantStatusEntry providerId sourceName grant =
  let status = redsgStatus grant
  in RPCExternalDataSourceStatusEntry
    { redsstProviderId = providerId
    , redsstConsumerId = Nothing
    , redsstSource = sourceName
    , redsstGrant = Just (redsgName grant)
    , redsstAccess = redsgAccess grant
    , redsstResources = redsgResources grant
    , redsstCapabilityScope = effectiveCapabilityScope status (redsgCapabilities grant)
    , redsstStatus = status
    , redsstReference = redsgReference grant
    , redsstConfigRefs = redsgConfigRefs grant
    , redsstDiagnostics = redssDiagnostics status
    }

-- | Build a status entry for a consumer reference.
externalDataSourceRefStatusEntry :: RPCExternalDataSourceRef -> RPCExternalDataSourceStatusEntry
externalDataSourceRefStatusEntry ref =
  let status = redsrStatus ref
      providerId = fromMaybe (fromMaybe "" (redssProviderId status)) (redsrProvider ref)
  in RPCExternalDataSourceStatusEntry
    { redsstProviderId = providerId
    , redsstConsumerId = Just (redsrName ref)
    , redsstSource = redsrSource ref
    , redsstGrant = redsrGrant ref
    , redsstAccess = redsrAccess ref
    , redsstResources = redsrResources ref
    , redsstCapabilityScope = redssCapabilityScope status
    , redsstStatus = status
    , redsstReference = redsrReference ref
    , redsstConfigRefs = redsrConfigRefs ref
    , redsstDiagnostics = redssDiagnostics status
    }

matchesStatusRequest :: RPCExternalDataSourceStatusRequest -> RPCExternalDataSourceStatusEntry -> Bool
matchesStatusRequest request entry =
  providerMatches
    && consumerMatches
    && sourceMatches
    && grantMatches
  where
    providerMatches = maybe True (== redsstProviderId entry) (redssrProviderId request)
    consumerMatches = maybe True (== redsstConsumerId entry) (Just <$> redssrConsumerId request)
    sourceMatches = null (redssrSources request) || redsstSource entry `elem` redssrSources request
    grantMatches = null (redssrGrants request) || maybe False (`elem` redssrGrants request) (redsstGrant entry)

stripDiagnosticsIfNeeded :: RPCExternalDataSourceStatusRequest -> RPCExternalDataSourceStatusEntry -> RPCExternalDataSourceStatusEntry
stripDiagnosticsIfNeeded request entry
  | redssrIncludeDiagnostics request = entry
  | otherwise = entry
      { redsstDiagnostics = Nothing
      , redsstStatus = (redsstStatus entry) { redssDiagnostics = Nothing }
      }

------------------------------------------------------------------------
-- Startup availability decisions
------------------------------------------------------------------------

data RPCExternalDataSourceStartupDecision
  = ExternalDataSourceStartupReady
  | ExternalDataSourceStartupDegraded
      { edssdDependency :: !Text
      , edssdReason     :: !Text
      }
  | ExternalDataSourceStartupBlocked
      { edssdDependency :: !Text
      , edssdReason     :: !Text
      }
  deriving (Eq, Show, Generic)

-- | A status that should prevent startup for a required consumer reference.
externalDataSourceStatusBlocksStartup :: RPCExternalDataSourceStatus -> Bool
externalDataSourceStatusBlocksStartup status =
  redssState status `elem`
    [ ExternalStatusUnknown
    , ExternalStatusUnconfigured
    , ExternalStatusUnavailable
    ]
  || redssAvailability status `elem`
    [ Just ExternalAvailabilityUnknown
    , Just ExternalAvailabilityUnconfigured
    , Just ExternalAvailabilityUnavailable
    ]
  || redssHealth status == Just ExternalHealthUnhealthy
  || redssAccessMode status == Just ExternalAccessModeDisabled

-- | A status that allows diagnostics to mark a plugin degraded without knowing
-- backend-specific internals.
externalDataSourceStatusDegradesStartup :: RPCExternalDataSourceStatus -> Bool
externalDataSourceStatusDegradesStartup status =
  redssState status == ExternalStatusDegraded
    || redssAvailability status == Just ExternalAvailabilityDegraded
    || redssHealth status == Just ExternalHealthDegraded

-- | Evaluate manifest-declared source/ref availability before runtime startup.
-- Required consumer references can block startup; optional references and
-- provider-owned source/grant degradations degrade the plugin.
externalDataSourceManifestStartupDecision :: RPCManifest -> RPCExternalDataSourceStartupDecision
externalDataSourceManifestStartupDecision manifest =
  case requiredBlocks of
    (dep, reason):_ -> ExternalDataSourceStartupBlocked dep reason
    [] -> case degradations of
      (dep, reason):_ -> ExternalDataSourceStartupDegraded dep reason
      [] -> ExternalDataSourceStartupReady
  where
    requiredBlocks =
      [ (externalRefDependencyName ref, externalRefReason "required external data source is not available" ref)
      | ref <- rmExternalDataSourceRefs manifest
      , redsrRequired ref
      , externalDataSourceStatusBlocksStartup (redsrStatus ref)
      ]
    degradations = providerDegradations <> requiredRefDegradations <> optionalRefDegradations
    providerDegradations =
      [ (redsdName source, providerReason "external data-source provider declaration is degraded" (redsdStatus source))
      | source <- rmExternalDataSources manifest
      , providerStatusUnavailableOrDegraded (redsdStatus source)
      ] <>
      [ (redsdName source <> ":" <> redsgName grant, providerReason "external data-source grant is degraded" (redsgStatus grant))
      | source <- rmExternalDataSources manifest
      , grant <- redsdGrants source
      , providerStatusUnavailableOrDegraded (redsgStatus grant)
      ]
    requiredRefDegradations =
      [ (externalRefDependencyName ref, externalRefReason "required external data source is degraded" ref)
      | ref <- rmExternalDataSourceRefs manifest
      , redsrRequired ref
      , externalDataSourceStatusDegradesStartup (redsrStatus ref)
      ]
    optionalRefDegradations =
      [ (externalRefDependencyName ref, externalRefReason "optional external data source is degraded" ref)
      | ref <- rmExternalDataSourceRefs manifest
      , not (redsrRequired ref)
      , providerStatusUnavailableOrDegraded (redsrStatus ref)
      ]

externalDataSourceStartupDecisionReason :: RPCExternalDataSourceStartupDecision -> Maybe Text
externalDataSourceStartupDecisionReason ExternalDataSourceStartupReady = Nothing
externalDataSourceStartupDecisionReason decision = Just (edssdReason decision)

externalDataSourceStartupDecisionBlockingDependency :: RPCExternalDataSourceStartupDecision -> Maybe Text
externalDataSourceStartupDecisionBlockingDependency ExternalDataSourceStartupReady = Nothing
externalDataSourceStartupDecisionBlockingDependency decision = Just (edssdDependency decision)

providerStatusUnavailableOrDegraded :: RPCExternalDataSourceStatus -> Bool
providerStatusUnavailableOrDegraded status =
  externalDataSourceStatusBlocksStartup status
    || externalDataSourceStatusDegradesStartup status

externalRefDependencyName :: RPCExternalDataSourceRef -> Text
externalRefDependencyName ref =
  maybe "" (<> ":") (redsrProvider ref)
    <> redsrSource ref
    <> maybe "" (":" <>) (redsrGrant ref)

externalRefReason :: Text -> RPCExternalDataSourceRef -> Text
externalRefReason prefix ref =
  prefix <> ": " <> externalRefDependencyName ref <> " (" <> statusSummary (redsrStatus ref) <> ")"

providerReason :: Text -> RPCExternalDataSourceStatus -> Text
providerReason prefix status = prefix <> " (" <> statusSummary status <> ")"

statusSummary :: RPCExternalDataSourceStatus -> Text
statusSummary status = Text.intercalate ", " $ filter (not . Text.null)
  [ "state=" <> externalStatusStateText (redssState status)
  , maybe "" (("availability=" <>) . externalAvailabilityText) (redssAvailability status)
  , maybe "" (("health=" <>) . externalHealthText) (redssHealth status)
  , maybe "" (("access_mode=" <>) . externalAccessModeText) (redssAccessMode status)
  , maybe "" ("message=" <>) (redssMessage status)
  ]

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

------------------------------------------------------------------------
-- JSON helpers
------------------------------------------------------------------------

optionalAliasedField :: FromJSON a => Aeson.Object -> [Text] -> a -> Parser a
optionalAliasedField o aliases fallback =
  case lookupAliasedField o aliases of
    Nothing -> pure fallback
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value

optionalNonNullField :: FromJSON a => Aeson.Object -> [Text] -> Parser (Maybe a)
optionalNonNullField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> pure Nothing
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> Just <$> parseJSON value

optionalNonNullListField :: FromJSON a => Aeson.Object -> [Text] -> Parser [a]
optionalNonNullListField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> pure []
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value

optionalNullableField :: FromJSON a => Aeson.Object -> [Text] -> Parser (Maybe a)
optionalNullableField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> pure Nothing
    Just (_, Null) -> pure Nothing
    Just (_, value) -> Just <$> parseJSON value

requiredAliasedField :: FromJSON a => Aeson.Object -> [Text] -> Parser a
requiredAliasedField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> fail ("missing required field: " <> Text.unpack (fromMaybe "<unknown>" (listToMaybe aliases)))
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value

lookupAliasedField :: Aeson.Object -> [Text] -> Maybe (Text, Value)
lookupAliasedField o aliases =
  listToMaybe [(alias, value) | alias <- aliases, Just value <- [KM.lookup (Key.fromText alias) o]]

effectiveCapabilityScope
  :: RPCExternalDataSourceStatus
  -> [RPCExternalDataSourceCapability]
  -> [RPCExternalDataSourceCapability]
effectiveCapabilityScope status fallback
  | null (redssCapabilityScope status) = fallback
  | otherwise = redssCapabilityScope status
