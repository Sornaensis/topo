{-# LANGUAGE OverloadedStrings #-}

-- | Data-resource RPC routing for plugin-owned resources.
module Actor.PluginManager.DataResourceRouter
  ( DataResourceRouteLease(..)
  , DataResourceRouteOperation(..)
  , prepareQueryPluginDataResource
  , prepareMutatePluginDataResource
  , executeQueryPluginDataResource
  , executeMutatePluginDataResource
  , dataResourceRouteLeaseCurrent
  , staleDataResourceRouteFailure
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Actor.PluginManager.ProcessLauncher
  ( PluginRuntimeGeneration
  , ownedPluginRuntimeGeneration
  )
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , lpConnection
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , pluginLifecycleStateText
  )
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource
  ( DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  )
import Topo.Plugin.DataResource.Validation
  ( validateMutateResourceRequest
  , validateMutateResult
  , validateQueryResourceRequest
  , validateQueryResult
  )
import Topo.Plugin.RPC
  ( DataResourceErrorCode(..)
  , DataResourceFailure(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  , RPCConnection(..)
  , RPCError
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCManifest(..)
  , dataResourceFailureText
  , externalDataSourceStatusBlocksStartup
  , mutateResource
  , queryResource
  , rpcErrorDataResourceFailure
  , sameRPCConnection
  )

-- | The operation authorized by a route lease. Keeping this in the snapshot
-- prevents a query lease from being finalized through the mutation path.
data DataResourceRouteOperation
  = DataResourceRouteQuery
  | DataResourceRouteMutation
  deriving (Eq, Show)

-- | Immutable actor-prepared snapshot for one data-resource RPC. It grants no
-- ownership: the actor remains free to replace or stop the runtime while the
-- caller waits on the exact connection captured here.
data DataResourceRouteLease = DataResourceRouteLease
  { drrlPluginName :: !Text
  , drrlRuntimeGeneration :: !PluginRuntimeGeneration
  , drrlConnection :: !RPCConnection
  , drrlSchema :: !DataResourceSchema
  , drrlOperation :: !DataResourceRouteOperation
  , drrlAuthorizedQuery :: !(Maybe QueryResource)
  , drrlEffectiveQuery :: !(Maybe QueryResource)
  , drrlAuthorizedMutation :: !(Maybe MutateResource)
  }

prepareQueryPluginDataResource
  :: Text
  -> QueryResource
  -> PluginManagerState
  -> Either Text DataResourceRouteLease
prepareQueryPluginDataResource pluginName qr st = do
  lease <- preparePluginDataResource
    pluginName
    (qrResource qr)
    CapDataRead
    DataResourceRouteQuery
    (\schema -> validateQueryResourceRequest schema qr)
    st
  let effective = applyQueryPaginationDefaults (drrlSchema lease) qr
  Right lease
    { drrlAuthorizedQuery = Just qr
    , drrlEffectiveQuery = Just effective
    }

prepareMutatePluginDataResource
  :: Text
  -> MutateResource
  -> PluginManagerState
  -> Either Text DataResourceRouteLease
prepareMutatePluginDataResource pluginName mr st = do
  lease <- preparePluginDataResource
    pluginName
    (mrResource mr)
    CapDataWrite
    DataResourceRouteMutation
    (\schema -> validateMutateResourceRequest schema mr)
    st
  Right lease { drrlAuthorizedMutation = Just mr }

preparePluginDataResource
  :: Text
  -> Text
  -> Capability
  -> DataResourceRouteOperation
  -> (DataResourceSchema -> Maybe DataResourceFailure)
  -> PluginManagerState
  -> Either Text DataResourceRouteLease
preparePluginDataResource pluginName resourceName capability operation validateRequest st =
  case Map.lookup pluginName (pmsPlugins st) of
    Nothing -> failureLeft (DataResourceFailure PluginUnavailable ("unknown plugin: " <> pluginName))
    Just lp -> case (lpStatus lp, lpConnection lp, lpRuntime lp) of
      (PluginConnected, Just conn, Just runtime)
        | dataResourceLifecycleAvailable lp ->
            case requiredCapabilityFailure capability lp of
              Just failure -> failureLeft failure
              Nothing -> case findResourceSchema resourceName lp conn of
                Nothing -> failureLeft (DataResourceFailure ResourceNotFound ("unknown resource: " <> resourceName))
                Just schema -> case validateRequest schema of
                  Just failure -> failureLeft failure
                  Nothing -> case unavailableExternalDataSourceFailure lp of
                    Just failure -> failureLeft failure
                    Nothing -> Right DataResourceRouteLease
                      { drrlPluginName = pluginName
                      , drrlRuntimeGeneration = ownedPluginRuntimeGeneration runtime
                      , drrlConnection = conn
                      , drrlSchema = schema
                      , drrlOperation = operation
                      , drrlAuthorizedQuery = Nothing
                      , drrlEffectiveQuery = Nothing
                      , drrlAuthorizedMutation = Nothing
                      }
      _ -> failureLeft (DataResourceFailure PluginUnavailable (pluginUnavailableMessage lp))

-- | Perform query IO and result validation on the caller thread.
executeQueryPluginDataResource
  :: DataResourceRouteLease
  -> QueryResource
  -> IO (Either Text QueryResult)
executeQueryPluginDataResource lease qr
  | drrlOperation lease /= DataResourceRouteQuery = pure staleDataResourceRouteFailure
  | drrlAuthorizedQuery lease /= Just qr = pure (requestMismatchFailure "query")
  | Just effective <- drrlEffectiveQuery lease = do
      result <- queryResource (drrlConnection lease) effective
      pure $ case result of
        Left err -> Left (renderRPCDataResourceError err)
        Right qResult -> case validateQueryResult (drrlSchema lease) effective qResult of
          Just failure -> failureLeft failure
          Nothing -> Right qResult
  | otherwise = pure (requestMismatchFailure "query")

-- | Perform mutation IO and result validation on the caller thread.
executeMutatePluginDataResource
  :: DataResourceRouteLease
  -> MutateResource
  -> IO (Either Text MutateResult)
executeMutatePluginDataResource lease mr
  | drrlOperation lease /= DataResourceRouteMutation = pure staleDataResourceRouteFailure
  | drrlAuthorizedMutation lease /= Just mr = pure (requestMismatchFailure "mutation")
  | otherwise = do
      result <- mutateResource (drrlConnection lease) mr
      pure $ case result of
        Left err -> Left (renderRPCDataResourceError err)
        Right mResult -> case validateMutateResult (drrlSchema lease) mr mResult of
          Just failure -> failureLeft failure
          Nothing -> Right mResult

-- | Check both logical generation and physical connection identity. Embedded
-- runtimes can share generation zero, so neither guard is sufficient alone.
dataResourceRouteLeaseCurrent :: DataResourceRouteLease -> PluginManagerState -> Bool
dataResourceRouteLeaseCurrent lease st =
  case Map.lookup (drrlPluginName lease) (pmsPlugins st) of
    Just lp
      | dataResourceLifecycleAvailable lp
      , Just currentConnection <- lpConnection lp
      , Just currentRuntime <- lpRuntime lp ->
          ownedPluginRuntimeGeneration currentRuntime == drrlRuntimeGeneration lease
            && sameRPCConnection currentConnection (drrlConnection lease)
    _ -> False

dataResourceLifecycleAvailable :: LoadedPlugin -> Bool
dataResourceLifecycleAvailable lp =
  plsState (lpLifecycle lp) `elem` [LifecycleReady, LifecycleDegraded]

staleDataResourceRouteFailure :: Either Text a
staleDataResourceRouteFailure =
  failureLeft (DataResourceFailure PluginUnavailable "plugin runtime changed while data-resource request was in flight")

requestMismatchFailure :: Text -> Either Text a
requestMismatchFailure operation = failureLeft
  (DataResourceFailure PermissionDenied
    ("data-resource " <> operation <> " does not match the exact actor-authorized request"))

applyQueryPaginationDefaults :: DataResourceSchema -> QueryResource -> QueryResource
applyQueryPaginationDefaults schema request
  | doPage (drsOperations schema) = request
      { qrPageSize = Just (fromMaybe (dpDefaultPageSize pagination) (qrPageSize request))
      , qrPageOffset = Just (fromMaybe (dpDefaultPageOffset pagination) (qrPageOffset request))
      }
  | otherwise = request
  where
    pagination = drsPagination schema

failureLeft :: DataResourceFailure -> Either Text a
failureLeft = Left . dataResourceFailureText

requiredCapabilityFailure :: Capability -> LoadedPlugin -> Maybe DataResourceFailure
requiredCapabilityFailure capability lp
  | capability `elem` rmCapabilities (lpManifest lp) = Nothing
  | otherwise = Just (DataResourceFailure PermissionDenied message)
  where
    message = "plugin '" <> lpName lp <> "' missing required " <> dataCapabilityName capability <> " capability"

dataCapabilityName :: Capability -> Text
dataCapabilityName CapDataRead = "dataRead"
dataCapabilityName CapDataWrite = "dataWrite"
dataCapabilityName _ = "data-resource"

unavailableExternalDataSourceFailure :: LoadedPlugin -> Maybe DataResourceFailure
unavailableExternalDataSourceFailure lp =
  case filter (externalDataSourceStatusBlocksStartup . redsrStatus) (rmExternalDataSourceRefs (lpManifest lp)) of
    ref:_ -> Just (DataResourceFailure ExternalDataSourceUnavailable (externalDataSourceUnavailableMessage ref))
    [] -> Nothing

externalDataSourceUnavailableMessage :: RPCExternalDataSourceRef -> Text
externalDataSourceUnavailableMessage ref =
  "external data-source unavailable: " <> externalDataSourceRefDependency ref <> messageSuffix
  where
    messageSuffix = maybe "" (": " <>) (redssMessage (redsrStatus ref))

externalDataSourceRefDependency :: RPCExternalDataSourceRef -> Text
externalDataSourceRefDependency ref =
  fromMaybe "unresolved" (redsrProvider ref)
    <> ":" <> redsrSource ref
    <> maybe "" (":" <>) (redsrGrant ref)

renderRPCDataResourceError :: RPCError -> Text
renderRPCDataResourceError err =
  dataResourceFailureText (rpcErrorDataResourceFailure err)

findResourceSchema :: Text -> LoadedPlugin -> RPCConnection -> Maybe DataResourceSchema
findResourceSchema resourceName lp conn = go negotiatedResources
  where
    negotiatedResources
      | rpcResourcesNegotiated conn = rpcResources conn
      | otherwise = case rpcResources conn of
          [] -> rmDataResources (lpManifest lp)
          resources -> resources
    go [] = Nothing
    go (schema:rest)
      | drsName schema == resourceName = Just schema
      | otherwise = go rest

pluginUnavailableMessage :: LoadedPlugin -> Text
pluginUnavailableMessage lp =
  "plugin unavailable: " <> lpName lp
    <> " (state=" <> pluginLifecycleStateText (plsState lifecycle)
    <> ", detail=" <> detail <> ")"
  where
    lifecycle = lpLifecycle lp
    detail = fromMaybe
      (fromMaybe "not connected" (plsReason lifecycle))
      (plsErrorMessage lifecycle)
