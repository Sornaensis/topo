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
  , PluginLifecycleSnapshot(..)
  , PluginManagerState(..)
  , pluginLifecycleStateText
  )
import Topo.Plugin.RPC
  ( MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  , mutateResource
  , queryResource
  , rpcErrorText
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
    Nothing -> pure (Left ("unknown plugin: " <> pluginName))
    Just lp -> case lpConnection lp of
      Nothing -> pure (Left (pluginUnavailableMessage lp))
      Just conn -> do
        result <- queryResource conn qr
        case result of
          Left err -> pure (Left (rpcErrorText err))
          Right qResult -> pure (Right qResult)

-- | Forward a data mutation to the named plugin without taking ownership
-- of plugin storage.
mutatePluginDataResource
  :: Text
  -> MutateResource
  -> PluginManagerState
  -> IO (Either Text MutateResult)
mutatePluginDataResource pluginName mr st =
  case Map.lookup pluginName (pmsPlugins st) of
    Nothing -> pure (Left ("unknown plugin: " <> pluginName))
    Just lp -> case lpConnection lp of
      Nothing -> pure (Left (pluginUnavailableMessage lp))
      Just conn -> do
        result <- mutateResource conn mr
        case result of
          Left err -> pure (Left (rpcErrorText err))
          Right mResult -> pure (Right mResult)

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
