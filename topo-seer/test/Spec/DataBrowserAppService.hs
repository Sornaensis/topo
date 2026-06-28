{-# LANGUAGE OverloadedStrings #-}

module Spec.DataBrowserAppService (spec) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Seer.DataBrowser.AppService
  ( DataBrowserUi(..)
  , dbarError
  , dbarUi
  , emptyDataBrowserUi
  , runDataBrowserAppAction
  )
import qualified Seer.DataBrowser.AppService as App
import Seer.DataBrowser.Model
import Seer.Service.Types (ServiceResponse(..), ServiceResult)
import Test.Hspec
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , allOperations
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..))

spec :: Spec
spec = describe "DataBrowser AppService integration" $ do
  it "loads plugins, selects resources, and queries records through AppService" $ do
    calls <- newIORef []
    loaded <- runDataBrowserAppAction (fakeDataService calls) emptyDataBrowserUi App.DataBrowserLoadPlugins
    dbarError loaded `shouldBe` Nothing
    Map.keys (dbuResources (dbarUi loaded)) `shouldBe` ["atlas"]

    selected <- runDataBrowserAppAction
      (fakeDataService calls)
      (dbarUi loaded)
      (App.DataBrowserSelectResource "atlas" "cities")
    dbarError selected `shouldBe` Nothing
    dbModelRecords (dbuModel (dbarUi selected)) `shouldBe` [record1, record2]
    dbModelLoading (dbuModel (dbarUi selected)) `shouldBe` False

    methods <- map fst <$> readIORef calls
    methods `shouldBe` ["data_list_plugins", "data_list_resources", "data_list_records"]

  it "surfaces reducer validation errors without a service call" $ do
    calls <- newIORef []
    result <- runDataBrowserAppAction (fakeDataService calls) emptyDataBrowserUi (App.DataBrowserSelectRecord 99)
    dbarError result `shouldBe` Just "record index out of range"
    readIORef calls `shouldReturn` []

  it "creates, updates, deletes, and cancels edits through the reducer/AppService boundary" $ do
    calls <- newIORef []
    let service = fakeDataService calls
        initialUi = DataBrowserUi (Map.singleton "atlas" [pagedSchema]) loadedModel

    creating <- runDataBrowserAppAction service initialUi App.DataBrowserStartCreate
    created <- runDataBrowserAppAction service (dbarUi creating) App.DataBrowserCreateRecord
    dbModelRecords (dbuModel (dbarUi created)) `shouldContain` [createdRecord]

    selected <- runDataBrowserAppAction service (dbarUi created) (App.DataBrowserSelectRecord 0)
    editing <- runDataBrowserAppAction service (dbarUi selected) App.DataBrowserStartEdit
    changed <- runDataBrowserAppAction service (dbarUi editing) (App.DataBrowserSetFieldValue "population" (Number 11))
    updated <- runDataBrowserAppAction service (dbarUi changed) App.DataBrowserUpdateRecord
    dbSelectionRecord (dbModelSelection (dbuModel (dbarUi updated))) `shouldBe` Just updatedRecord

    confirming <- runDataBrowserAppAction service (dbarUi updated) App.DataBrowserRequestDelete
    deleted <- runDataBrowserAppAction service (dbarUi confirming) App.DataBrowserDeleteRecord
    dbModelRecords (dbuModel (dbarUi deleted)) `shouldNotContain` [updatedRecord]

    recancel <- runDataBrowserAppAction service (dbarUi editing) App.DataBrowserCancelEdit
    dbModelMode (dbuModel (dbarUi recancel)) `shouldBe` DataBrowserViewMode

    methods <- map fst <$> readIORef calls
    filter (`elem` ["data_create_record", "data_update_record", "data_delete_record"]) methods
      `shouldBe` ["data_create_record", "data_update_record", "data_delete_record"]

fakeDataService :: IORef [(Text, Value)] -> Text -> Value -> IO ServiceResult
fakeDataService calls method params = do
  modifyIORef' calls (<> [(method, params)])
  pure . Right . ServiceResponse $ case method of
    "data_list_plugins" -> object
      [ "plugins" .= [object ["plugin" .= ("atlas" :: Text), "resources" .= ["cities" :: Text]]]
      , "count" .= (1 :: Int)
      ]
    "data_list_resources" -> object
      [ "plugin" .= pluginParam params
      , "resources" .= [snakeSchemaValue pagedSchema]
      , "external_data_sources" .= ([] :: [Value])
      , "external_data_source_count" .= (0 :: Int)
      , "external_data_source_failures" .= (0 :: Int)
      ]
    "data_list_records" -> object
      [ "plugin" .= pluginParam params
      , "resource" .= resourceParam params
      , "records" .= [record1, record2]
      , "total_count" .= Just (2 :: Int)
      , "count" .= (2 :: Int)
      ]
    "data_create_record" -> object
      [ "plugin" .= pluginParam params
      , "resource" .= resourceParam params
      , "created" .= True
      , "record" .= Just createdRecord
      ]
    "data_update_record" -> object
      [ "plugin" .= pluginParam params
      , "resource" .= resourceParam params
      , "updated" .= True
      , "record" .= Just (recordFromFields params)
      ]
    "data_delete_record" -> object
      [ "plugin" .= pluginParam params
      , "resource" .= resourceParam params
      , "deleted" .= True
      ]
    _ -> object []

pluginParam :: Value -> Text
pluginParam = fromMaybe "" . Aeson.parseMaybe (Aeson.withObject "params" (.: "plugin"))

resourceParam :: Value -> Text
resourceParam = fromMaybe "" . Aeson.parseMaybe (Aeson.withObject "params" (.: "resource"))

recordFromFields :: Value -> DataRecord
recordFromFields value = DataRecord $ Map.insert "id" key fields
  where
    key = fromMaybe Null $ Aeson.parseMaybe (Aeson.withObject "params" (.: "key")) value
    fields = fromMaybe Map.empty $ Aeson.parseMaybe (Aeson.withObject "params" (.: "fields")) value

snakeSchemaValue :: DataResourceSchema -> Value
snakeSchemaValue schema = object
  [ "schema_version" .= drsSchemaVersion schema
  , "resource_version" .= drsResourceVersion schema
  , "name" .= drsName schema
  , "label" .= drsLabel schema
  , "hex_bound" .= drsHexBound schema
  , "key_field" .= drsKeyField schema
  , "overlay" .= drsOverlay schema
  , "fields" .= drsFields schema
  , "operations" .= snakeOperationsValue (drsOperations schema)
  , "pagination" .= snakePaginationValue (drsPagination schema)
  ]

snakeOperationsValue :: DataOperations -> Value
snakeOperationsValue ops = object
  [ "list" .= doList ops
  , "get" .= doGet ops
  , "create" .= doCreate ops
  , "update" .= doUpdate ops
  , "delete" .= doDelete ops
  , "query_by_hex" .= doQueryByHex ops
  , "query_by_field" .= doQueryByField ops
  , "sort" .= doSort ops
  , "filter" .= doFilter ops
  , "page" .= doPage ops
  ]

snakePaginationValue :: DataPagination -> Value
snakePaginationValue pagination = object
  [ "default_page_size" .= dpDefaultPageSize pagination
  , "max_page_size" .= dpMaxPageSize pagination
  , "default_page_offset" .= dpDefaultPageOffset pagination
  ]

loadedModel :: DataBrowserModel
loadedModel =
  dataBrowserReducer selectedResourceModel $
    DataBrowserListSucceeded
      (DataBrowserListRecordsRequest "atlas" "cities" (Just 2) (Just 0))
      (QueryResult "cities" [record1, record2] (Just 2))

selectedResourceModel :: DataBrowserModel
selectedResourceModel =
  dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" pagedSchema)

record1 :: DataRecord
record1 = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("name", String "Alpha")
  , ("population", Number 10)
  ]

record2 :: DataRecord
record2 = DataRecord $ Map.fromList
  [ ("id", Number 2)
  , ("name", String "Beta")
  , ("population", Number 20)
  ]

createdRecord :: DataRecord
createdRecord = DataRecord $ Map.fromList
  [ ("id", Number 3)
  , ("name", String "Unnamed")
  , ("population", Number 0)
  ]

updatedRecord :: DataRecord
updatedRecord = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("name", String "Alpha")
  , ("population", Number 11)
  ]

pagedSchema :: DataResourceSchema
pagedSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "cities"
  , drsLabel = "Cities"
  , drsHexBound = False
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsFields =
      [ DataFieldDef "id" DFInt "ID" False Nothing
      , DataFieldDef "name" DFText "Name" True (Just (String "Unnamed"))
      , DataFieldDef "population" DFInt "Population" True (Just (Number 0))
      ]
  , drsOperations = allOperations { doPage = True }
  , drsPagination = DataPagination 2 10 0
  }
