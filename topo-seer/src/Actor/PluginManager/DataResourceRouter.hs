{-# LANGUAGE OverloadedStrings #-}

-- | Data-resource RPC routing for plugin-owned resources.
module Actor.PluginManager.DataResourceRouter
  ( queryPluginDataResource
  , mutatePluginDataResource
  ) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, throwIO)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Timeout (timeout)

import Actor.PluginManager.ProcessLauncher (safeTerminateProcess)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , pluginLifecycleStateText
  )
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
  , RPCManifest(..)
  , RPCStartPolicy(..)
  , dataResourceFailureText
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
      (PluginConnected, Just conn) -> case findResourceSchema (qrResource qr) lp conn of
        Nothing -> pure (failureLeft (DataResourceFailure ResourceNotFound ("unknown resource: " <> qrResource qr)))
        Just schema -> case validateQueryResourceRequest schema qr of
          Just failure -> pure (failureLeft failure)
          Nothing -> do
            mResult <- withPluginRequestTimeout lp conn (queryResource conn qr)
            case mResult of
              Nothing -> pure (failureLeft (DataResourceFailure DataResourceTimeout "plugin data query timed out"))
              Just (Left err) -> pure (Left (renderRPCDataResourceError err))
              Just (Right qResult) -> case validateQueryResult schema qr qResult of
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
      (PluginConnected, Just conn) -> case findResourceSchema (mrResource mr) lp conn of
        Nothing -> pure (failureLeft (DataResourceFailure ResourceNotFound ("unknown resource: " <> mrResource mr)))
        Just schema -> case validateMutateResourceRequest schema mr of
          Just failure -> pure (failureLeft failure)
          Nothing -> do
            mResult <- withPluginRequestTimeout lp conn (mutateResource conn mr)
            case mResult of
              Nothing -> pure (failureLeft (DataResourceFailure DataResourceTimeout "plugin data mutation timed out"))
              Just (Left err) -> pure (Left (renderRPCDataResourceError err))
              Just (Right mResult) -> case validateMutateResult schema mr mResult of
                Just failure -> pure (failureLeft failure)
                Nothing -> pure (Right mResult)
      _ -> pure (failureLeft (DataResourceFailure PluginUnavailable (pluginUnavailableMessage lp)))

withPluginRequestTimeout :: LoadedPlugin -> RPCConnection -> IO a -> IO (Maybe a)
withPluginRequestTimeout lp conn action = do
  done <- newEmptyMVar
  worker <- forkFinally action $ \result -> do
    _ <- tryPutMVar done result
    pure ()
  result <- timeout (max 1 (rspRequestTimeoutMs (lpStartPolicy lp) * 1000)) (takeMVar done)
  case result of
    Just (Right value) -> pure (Just value)
    Just (Left err) -> throwIO (err :: SomeException)
    Nothing -> do
      abortPluginRequest lp conn
      killThread worker
      _ <- takeMVar done
      pure Nothing

abortPluginRequest :: LoadedPlugin -> RPCConnection -> IO ()
abortPluginRequest lp _conn = do
  case lpProcessHandle lp of
    Nothing -> pure ()
    Just processHandle -> do
      _ <- safeTerminateProcess processHandle
      pure ()

failureLeft :: DataResourceFailure -> Either Text a
failureLeft = Left . dataResourceFailureText

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
