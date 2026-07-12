{-# LANGUAGE OverloadedStrings #-}

-- | Data-resource RPC routing for plugin-owned resources.
module Actor.PluginManager.DataResourceRouter
  ( queryPluginDataResource
  , mutatePluginDataResource
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , lpConnection
  , PluginLifecycleSnapshot(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , pluginLifecycleStateText
  )
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource (DataResourceSchema(..))
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
  )

-- | Forward a data query to the named plugin without taking ownership of
-- plugin storage.
queryPluginDataResource
  :: Text
  -> QueryResource
  -> PluginManagerState
  -> IO (Either Text QueryResult)
queryPluginDataResource pluginName qr st =
  case Map.lookup pluginName (pmsPlugins st) of
    Nothing -> pure (failureLeft (DataResourceFailure PluginUnavailable ("unknown plugin: " <> pluginName)))
    Just lp -> case (lpStatus lp, lpConnection lp) of
      (PluginConnected, Just conn) -> case requiredCapabilityFailure CapDataRead lp of
        Just failure -> pure (failureLeft failure)
        Nothing -> case findResourceSchema (qrResource qr) lp conn of
          Nothing -> pure (failureLeft (DataResourceFailure ResourceNotFound ("unknown resource: " <> qrResource qr)))
          Just schema -> case validateQueryResourceRequest schema qr of
            Just failure -> pure (failureLeft failure)
            Nothing -> case unavailableExternalDataSourceFailure lp of
              Just failure -> pure (failureLeft failure)
              Nothing -> do
                result <- queryResource conn qr
                case result of
                  Left err -> pure (Left (renderRPCDataResourceError err))
                  Right qResult -> case validateQueryResult schema qr qResult of
                    Just failure -> pure (failureLeft failure)
                    Nothing -> pure (Right qResult)
      _ -> pure (failureLeft (DataResourceFailure PluginUnavailable (pluginUnavailableMessage lp)))

-- | Forward a data mutation to the named plugin without taking ownership
-- of plugin storage.
mutatePluginDataResource
  :: Text
  -> MutateResource
  -> PluginManagerState
  -> IO (Either Text MutateResult)
mutatePluginDataResource pluginName mr st =
  case Map.lookup pluginName (pmsPlugins st) of
    Nothing -> pure (failureLeft (DataResourceFailure PluginUnavailable ("unknown plugin: " <> pluginName)))
    Just lp -> case (lpStatus lp, lpConnection lp) of
      (PluginConnected, Just conn) -> case requiredCapabilityFailure CapDataWrite lp of
        Just failure -> pure (failureLeft failure)
        Nothing -> case findResourceSchema (mrResource mr) lp conn of
          Nothing -> pure (failureLeft (DataResourceFailure ResourceNotFound ("unknown resource: " <> mrResource mr)))
          Just schema -> case validateMutateResourceRequest schema mr of
            Just failure -> pure (failureLeft failure)
            Nothing -> case unavailableExternalDataSourceFailure lp of
              Just failure -> pure (failureLeft failure)
              Nothing -> do
                result <- mutateResource conn mr
                case result of
                  Left err -> pure (Left (renderRPCDataResourceError err))
                  Right mResult -> case validateMutateResult schema mr mResult of
                    Just failure -> pure (failureLeft failure)
                    Nothing -> pure (Right mResult)
      _ -> pure (failureLeft (DataResourceFailure PluginUnavailable (pluginUnavailableMessage lp)))

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
    negotiatedResources = case rpcResources conn of
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
