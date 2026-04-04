{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC handlers for the data browser: listing plugins, resources,
-- records, and performing CRUD operations on plugin data.
--
-- @data_list_plugins@, @data_list_resources@, @data_list_records@,
-- @data_get_record@, @data_create_record@, @data_update_record@,
-- @data_delete_record@, @data_get_state@.
module Seer.Command.Handlers.Data
  ( handleDataListPlugins
  , handleDataListResources
  , handleDataListRecords
  , handleDataGetRecord
  , handleDataCreateRecord
  , handleDataUpdateRecord
  , handleDataDeleteRecord
  , handleDataGetState
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Actor.PluginManager
  ( getPluginDataResources
  , queryPluginResource
  , mutatePluginResource
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (UiState(..), DataBrowserState(..), readUiSnapshotRef)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin.DataResource
  ( DataResourceSchema(..)
  , DataFieldDef(..)
  , DataOperations(..)
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  )

-- | Handle @data_list_plugins@ — return all plugins that have data resources.
handleDataListPlugins :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataListPlugins ctx reqId _params = do
  let pmH = ahPluginManagerHandle (ccActorHandles ctx)
  resources <- getPluginDataResources pmH
  let plugins = Map.toList resources
      pluginObjs = map (\(pname, schemas) -> object
        [ "plugin"    .= pname
        , "resources" .= map drsName schemas
        ]) plugins
  pure $ okResponse reqId $ object
    [ "plugins" .= pluginObjs
    , "count"   .= length plugins
    ]

-- | Handle @data_list_resources@ — return resource schemas for a plugin.
--
-- Params: @{ "plugin": "name" }@
handleDataListResources :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataListResources ctx reqId params = do
  case Aeson.parseMaybe parsePlugin params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin' parameter"
    Just pluginName -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
      resources <- getPluginDataResources pmH
      case Map.lookup pluginName resources of
        Nothing ->
          pure $ errResponse reqId ("unknown plugin: " <> pluginName)
        Just schemas ->
          pure $ okResponse reqId $ object
            [ "plugin"    .= pluginName
            , "resources" .= map schemaToJSON schemas
            ]

-- | Handle @data_list_records@ — list records for a plugin resource with
-- pagination.
--
-- Params: @{ "plugin": "name", "resource": "name",
--            "page_size": int?, "page_offset": int? }@
handleDataListRecords :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataListRecords ctx reqId params = do
  case Aeson.parseMaybe parsePluginResource params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin' and/or 'resource' parameters"
    Just (pluginName, resourceName) -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
          mPageSize   = Aeson.parseMaybe parsePageSize params
          mPageOffset = Aeson.parseMaybe parsePageOffset params
          qr = QueryResource
            { qrResource   = resourceName
            , qrQuery      = QueryAll
            , qrPageSize   = mPageSize
            , qrPageOffset = mPageOffset
            }
      result <- queryPluginResource pmH pluginName qr
      case result of
        Left err ->
          pure $ errResponse reqId ("query failed: " <> err)
        Right qrs ->
          pure $ okResponse reqId $ object
            [ "plugin"      .= pluginName
            , "resource"    .= qrsResource qrs
            , "records"     .= map recordToJSON (qrsRecords qrs)
            , "total_count" .= qrsTotalCount qrs
            , "count"       .= length (qrsRecords qrs)
            ]

-- | Handle @data_get_record@ — get a single record by primary key.
--
-- Params: @{ "plugin": "name", "resource": "name", "key": value }@
handleDataGetRecord :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataGetRecord ctx reqId params = do
  case Aeson.parseMaybe parsePluginResourceKey params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin', 'resource', and/or 'key' parameters"
    Just (pluginName, resourceName, keyVal) -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
          qr = QueryResource
            { qrResource   = resourceName
            , qrQuery      = QueryByKey keyVal
            , qrPageSize   = Nothing
            , qrPageOffset = Nothing
            }
      result <- queryPluginResource pmH pluginName qr
      case result of
        Left err ->
          pure $ errResponse reqId ("query failed: " <> err)
        Right qrs ->
          case qrsRecords qrs of
            [r] -> pure $ okResponse reqId $ object
              [ "plugin"   .= pluginName
              , "resource" .= resourceName
              , "record"   .= recordToJSON r
              ]
            [] -> pure $ errResponse reqId "record not found"
            rs -> pure $ okResponse reqId $ object
              [ "plugin"   .= pluginName
              , "resource" .= resourceName
              , "records"  .= map recordToJSON rs
              , "count"    .= length rs
              ]

-- | Handle @data_create_record@ — create a new record.
--
-- Params: @{ "plugin": "name", "resource": "name", "fields": { ... } }@
handleDataCreateRecord :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataCreateRecord ctx reqId params = do
  case Aeson.parseMaybe parsePluginResourceFields params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin', 'resource', and/or 'fields' parameters"
    Just (pluginName, resourceName, fields) -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
          record = DataRecord fields
          mr = MutateResource
            { mrResource = resourceName
            , mrMutation = MutCreate record
            }
      result <- mutatePluginResource pmH pluginName mr
      case result of
        Left err ->
          pure $ errResponse reqId ("create failed: " <> err)
        Right mrs
          | mrsSuccess mrs ->
              pure $ okResponse reqId $ object
                [ "plugin"   .= pluginName
                , "resource" .= resourceName
                , "created"  .= True
                , "record"   .= fmap recordToJSON (mrsRecord mrs)
                ]
          | otherwise ->
              pure $ errResponse reqId (maybe "create failed" id (mrsError mrs))

-- | Handle @data_update_record@ — update an existing record.
--
-- Params: @{ "plugin": "name", "resource": "name", "key": value,
--            "fields": { ... } }@
handleDataUpdateRecord :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataUpdateRecord ctx reqId params = do
  case Aeson.parseMaybe parseUpdateParams params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin', 'resource', 'key', and/or 'fields' parameters"
    Just (pluginName, resourceName, keyVal, fields) -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
          record = DataRecord fields
          mr = MutateResource
            { mrResource = resourceName
            , mrMutation = MutUpdate keyVal record
            }
      result <- mutatePluginResource pmH pluginName mr
      case result of
        Left err ->
          pure $ errResponse reqId ("update failed: " <> err)
        Right mrs
          | mrsSuccess mrs ->
              pure $ okResponse reqId $ object
                [ "plugin"   .= pluginName
                , "resource" .= resourceName
                , "updated"  .= True
                , "record"   .= fmap recordToJSON (mrsRecord mrs)
                ]
          | otherwise ->
              pure $ errResponse reqId (maybe "update failed" id (mrsError mrs))

-- | Handle @data_delete_record@ — delete a record by primary key.
--
-- Params: @{ "plugin": "name", "resource": "name", "key": value }@
handleDataDeleteRecord :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataDeleteRecord ctx reqId params = do
  case Aeson.parseMaybe parsePluginResourceKey params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin', 'resource', and/or 'key' parameters"
    Just (pluginName, resourceName, keyVal) -> do
      let pmH = ahPluginManagerHandle (ccActorHandles ctx)
          mr = MutateResource
            { mrResource = resourceName
            , mrMutation = MutDelete keyVal
            }
      result <- mutatePluginResource pmH pluginName mr
      case result of
        Left err ->
          pure $ errResponse reqId ("delete failed: " <> err)
        Right mrs
          | mrsSuccess mrs ->
              pure $ okResponse reqId $ object
                [ "plugin"   .= pluginName
                , "resource" .= resourceName
                , "deleted"  .= True
                ]
          | otherwise ->
              pure $ errResponse reqId (maybe "delete failed" id (mrsError mrs))

-- | Handle @data_get_state@ — return the current data browser UI state.
handleDataGetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleDataGetState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let dbs = uiDataBrowser ui
  pure $ okResponse reqId $ object
    [ "selected_plugin"   .= dbsSelectedPlugin dbs
    , "selected_resource" .= dbsSelectedResource dbs
    , "record_count"      .= length (dbsRecords dbs)
    , "total_count"       .= dbsTotalCount dbs
    , "page_offset"       .= dbsPageOffset dbs
    , "loading"           .= dbsLoading dbs
    , "edit_mode"         .= dbsEditMode dbs
    , "create_mode"       .= dbsCreateMode dbs
    , "has_selection"     .= case dbsSelectedRecord dbs of
        Just _  -> True
        Nothing -> False
    , "selected_key"      .= dbsSelectedRecordKey dbs
    ]

-- --------------------------------------------------------------------------
-- Parsers
-- --------------------------------------------------------------------------

parsePlugin :: Value -> Aeson.Parser Text
parsePlugin = Aeson.withObject "params" (.: "plugin")

parsePluginResource :: Value -> Aeson.Parser (Text, Text)
parsePluginResource = Aeson.withObject "params" $ \o ->
  (,) <$> o .: "plugin" <*> o .: "resource"

parsePluginResourceKey :: Value -> Aeson.Parser (Text, Text, Value)
parsePluginResourceKey = Aeson.withObject "params" $ \o ->
  (,,) <$> o .: "plugin" <*> o .: "resource" <*> o .: "key"

parsePluginResourceFields :: Value -> Aeson.Parser (Text, Text, Map Text Value)
parsePluginResourceFields = Aeson.withObject "params" $ \o ->
  (,,) <$> o .: "plugin" <*> o .: "resource" <*> o .: "fields"

parseUpdateParams :: Value -> Aeson.Parser (Text, Text, Value, Map Text Value)
parseUpdateParams = Aeson.withObject "params" $ \o ->
  (,,,) <$> o .: "plugin" <*> o .: "resource" <*> o .: "key" <*> o .: "fields"

parsePageSize :: Value -> Aeson.Parser Int
parsePageSize = Aeson.withObject "params" (.: "page_size")

parsePageOffset :: Value -> Aeson.Parser Int
parsePageOffset = Aeson.withObject "params" (.: "page_offset")

-- --------------------------------------------------------------------------
-- JSON helpers
-- --------------------------------------------------------------------------

recordToJSON :: DataRecord -> Value
recordToJSON (DataRecord m) =
  object [ Key.fromText k .= v | (k, v) <- Map.toList m ]

schemaToJSON :: DataResourceSchema -> Value
schemaToJSON drs = object
  [ "name"       .= drsName drs
  , "label"      .= drsLabel drs
  , "hex_bound"  .= drsHexBound drs
  , "key_field"  .= drsKeyField drs
  , "overlay"    .= drsOverlay drs
  , "fields"     .= map fieldToJSON (drsFields drs)
  , "operations" .= opsToJSON (drsOperations drs)
  ]

fieldToJSON :: DataFieldDef -> Value
fieldToJSON fd = object
  [ "name"     .= dfName fd
  , "type"     .= dfType fd
  , "label"    .= dfLabel fd
  , "editable" .= dfEditable fd
  , "default"  .= dfDefault fd
  ]

opsToJSON :: DataOperations -> Value
opsToJSON ops = object
  [ "list"         .= doList ops
  , "get"          .= doGet ops
  , "create"       .= doCreate ops
  , "update"       .= doUpdate ops
  , "delete"       .= doDelete ops
  , "query_by_hex" .= doQueryByHex ops
  ]
