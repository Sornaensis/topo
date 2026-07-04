{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin configuration and user preference persistence.
module Actor.PluginManager.Config
  ( loadPluginConfig
  , savePluginConfig
  , loadPluginOrder
  , savePluginOrder
  , loadDisabledPlugins
  , saveDisabledPlugins
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))

import Topo.Plugin.RPC.Manifest
  ( RPCParamSpec
  , rpcParamDefaults
  , sanitizeRPCParamMap
  )

-- | Load saved parameter values, falling back to manifest defaults.
loadPluginConfig :: FilePath -> [RPCParamSpec] -> IO (Map Text Value)
loadPluginConfig pluginDir paramSpecs = do
  let configPath = pluginDir </> "config.json"
      defaults = rpcParamDefaults paramSpecs
  exists <- doesFileExist configPath
  if not exists
    then pure defaults
    else do
      result <- try @SomeException (BL.readFile configPath)
      case result of
        Left _ -> pure defaults
        Right bs -> case Aeson.decode bs of
          Nothing -> pure defaults
          Just (Object km) ->
            let saved = Map.fromList
                  [(Key.toText k, v) | (k, v) <- KM.toList km]
            in pure (sanitizeRPCParamMap paramSpecs saved)
          Just _ -> pure defaults

-- | Save current parameter values to config.json.
savePluginConfig :: FilePath -> Map Text Value -> IO ()
savePluginConfig pluginDir params = do
  let configPath = pluginDir </> "config.json"
  createDirectoryIfMissing True pluginDir
  BL.writeFile configPath (Aeson.encode (Aeson.toJSON params))

-- | Load the global plugin ordering from plugin-order.json.
loadPluginOrder :: FilePath -> IO [Text]
loadPluginOrder baseDir = do
  let orderPath = baseDir </> "plugin-order.json"
  exists <- doesFileExist orderPath
  if not exists
    then pure []
    else do
      result <- try @SomeException (BL.readFile orderPath)
      case result of
        Left _ -> pure []
        Right bs -> case (Aeson.decode bs :: Maybe [Text]) of
          Just names -> pure names
          Nothing -> pure []

-- | Persist the plugin ordering.
savePluginOrder :: FilePath -> [Text] -> IO ()
savePluginOrder baseDir order = do
  let orderPath = baseDir </> "plugin-order.json"
  createDirectoryIfMissing True baseDir
  BL.writeFile orderPath (Aeson.encode order)

-- | Load the set of disabled plugins from @disabled-plugins.json@.
loadDisabledPlugins :: FilePath -> IO (Set Text)
loadDisabledPlugins baseDir = do
  let path = baseDir </> "disabled-plugins.json"
  exists <- doesFileExist path
  if not exists
    then pure Set.empty
    else do
      result <- try @SomeException (BL.readFile path)
      case result of
        Left _ -> pure Set.empty
        Right bs -> case (Aeson.decode bs :: Maybe [Text]) of
          Just names -> pure (Set.fromList names)
          Nothing -> pure Set.empty

-- | Persist the set of disabled plugins.
saveDisabledPlugins :: FilePath -> Set Text -> IO ()
saveDisabledPlugins baseDir disabled = do
  let path = baseDir </> "disabled-plugins.json"
  createDirectoryIfMissing True baseDir
  BL.writeFile path (Aeson.encode (Set.toList disabled))
