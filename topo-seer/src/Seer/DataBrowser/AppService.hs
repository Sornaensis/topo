{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | AppService-backed integration for the pure Data Browser reducer.
module Seer.DataBrowser.AppService
  ( DataBrowserAppAction(..)
  , DataBrowserActionResult(..)
  , DataBrowserUi(..)
  , DataBrowserRunService
  , emptyDataBrowserUi
  , dataBrowserUiFromState
  , dataBrowserUiToState
  , dataBrowserStateToModel
  , dataBrowserStateFromModel
  , runDataBrowserAppAction
  , runDataBrowserPendingRequest
  ) where

import Actor.UI.State (DataBrowserState(..), emptyDataBrowserState)
import Control.Monad (foldM)
import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.DataBrowser.Model hiding (DataBrowserAction(..))
import qualified Seer.DataBrowser.Model as Model
import Seer.Service.Types (ServiceResponse(..), ServiceResult, serviceErrorText)
import Topo.Plugin.DataResource
  ( DataResourceSchema(..)
  , currentDataResourceSchemaVersion
  , defaultDataPagination
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..))

-- | AppService invocation used by Data Browser integration.
type DataBrowserRunService = Text -> Value -> IO ServiceResult

-- | UI-facing state used while interpreting Data Browser actions.
data DataBrowserUi = DataBrowserUi
  { dbuResources :: !(Map Text [DataResourceSchema])
  , dbuModel :: !DataBrowserModel
  } deriving (Eq, Show)

-- | Result of interpreting an action. The model is always updated; service
-- failures are also returned for command/test callers that need to surface them.
data DataBrowserActionResult = DataBrowserActionResult
  { dbarUi :: !DataBrowserUi
  , dbarError :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Integration actions. Pure-only actions are interpreted by the reducer;
-- data loading and mutations are performed through AppService.
data DataBrowserAppAction
  = DataBrowserLoadPlugins
  | DataBrowserSelectPlugin !Text
  | DataBrowserSelectResource !Text !Text
  | DataBrowserQueryRecords !DataBrowserPageAction
  | DataBrowserSelectRecord !Int
  | DataBrowserDismissRecord
  | DataBrowserToggleField !Text
  | DataBrowserStartEdit
  | DataBrowserStartCreate
  | DataBrowserCancelEdit
  | DataBrowserFocusField !Text
  | DataBrowserBlurField
  | DataBrowserSetFieldValue !Text !Value
  | DataBrowserStepNumberField !Text !Int
  | DataBrowserToggleBoolField !Text
  | DataBrowserCycleEnumField !Text !Int
  | DataBrowserCreateRecord
  | DataBrowserUpdateRecord
  | DataBrowserRequestDelete
  | DataBrowserCancelDelete
  | DataBrowserDeleteRecord
  deriving (Eq, Show)

emptyDataBrowserUi :: DataBrowserUi
emptyDataBrowserUi = DataBrowserUi Map.empty emptyDataBrowserModel

dataBrowserUiFromState :: Map Text [DataResourceSchema] -> DataBrowserState -> DataBrowserUi
dataBrowserUiFromState resources state =
  DataBrowserUi resources (dataBrowserStateToModel resources state)

dataBrowserUiToState :: DataBrowserUi -> (Map Text [DataResourceSchema], DataBrowserState)
dataBrowserUiToState ui =
  (dbuResources ui, dataBrowserStateFromModel (dbuModel ui))

-- | Convert the legacy UI actor DataBrowserState into the reducer model.
dataBrowserStateToModel :: Map Text [DataResourceSchema] -> DataBrowserState -> DataBrowserModel
dataBrowserStateToModel resources state = DataBrowserModel
  { dbModelMode = mode
  , dbModelSelection = DataBrowserSelection
      { dbSelectionPlugin = dbsSelectedPlugin state
      , dbSelectionResource = dbsSelectedResource state
      , dbSelectionSchema = schema
      , dbSelectionRecord = dbsSelectedRecord state
      , dbSelectionRecordKey = dbsSelectedRecordKey state
      , dbSelectionRowIndex = dbsSelectedRowIndex state
      }
  , dbModelRecords = dbsRecords state
  , dbModelPagination = DataBrowserPagination
      { dbPaginationOffset = dbsPageOffset state
      , dbPaginationPageSize = schema >>= fst . dataBrowserPageRequestFor
      , dbPaginationTotalCount = dbsTotalCount state
      }
  , dbModelExpandedFields = dbsExpandedFields state
  , dbModelEditBuffer = DataBrowserEditBuffer (dbsEditValues state)
  , dbModelFocusedField = dbsFocusedField state
  , dbModelTextCursor = dbsTextCursor state
  , dbModelValidationErrors = []
  , dbModelPendingRequest = Nothing
  , dbModelLoading = dbsLoading state
  }
  where
    schema = do
      pluginName <- dbsSelectedPlugin state
      resourceName <- dbsSelectedResource state
      resourceSchemaFor resources pluginName resourceName
    mode
      | dbsDeleteConfirm state = DataBrowserDeleteConfirmMode
      | dbsCreateMode state = DataBrowserCreateMode
      | dbsEditMode state = DataBrowserEditMode
      | dbsSelectedRecord state /= Nothing = DataBrowserViewMode
      | otherwise = DataBrowserBrowseMode

-- | Convert the reducer model back to the legacy UI actor DataBrowserState.
dataBrowserStateFromModel :: DataBrowserModel -> DataBrowserState
dataBrowserStateFromModel model = emptyDataBrowserState
  { dbsSelectedPlugin = dbSelectionPlugin selection
  , dbsSelectedResource = dbSelectionResource selection
  , dbsRecords = dbModelRecords model
  , dbsPageOffset = dbPaginationOffset (dbModelPagination model)
  , dbsTotalCount = dbPaginationTotalCount (dbModelPagination model)
  , dbsLoading = dbModelLoading model
  , dbsSelectedRecord = dbSelectionRecord selection
  , dbsSelectedRecordKey = dbSelectionRecordKey selection
  , dbsSelectedRowIndex = dbSelectionRowIndex selection
  , dbsExpandedFields = dbModelExpandedFields model
  , dbsEditMode = dbModelMode model == DataBrowserEditMode
  , dbsCreateMode = dbModelMode model == DataBrowserCreateMode
  , dbsEditValues = dbEditBufferValues (dbModelEditBuffer model)
  , dbsFocusedField = dbModelFocusedField model
  , dbsTextCursor = dbModelTextCursor model
  , dbsDeleteConfirm = dbModelMode model == DataBrowserDeleteConfirmMode
  }
  where
    selection = dbModelSelection model

runDataBrowserAppAction
  :: DataBrowserRunService
  -> DataBrowserUi
  -> DataBrowserAppAction
  -> IO DataBrowserActionResult
runDataBrowserAppAction runService ui action = case action of
  DataBrowserLoadPlugins -> do
    loaded <- loadAllResources runService
    pure $ case loaded of
      Left err -> failure ui err
      Right resources -> success ui
        { dbuResources = resources
        , dbuModel = attachSelectionSchema resources (dbuModel ui)
        }

  DataBrowserSelectPlugin pluginName -> do
    loaded <- loadPluginResources runService pluginName
    let model' = dataBrowserReducer (dbuModel ui) (Model.DataBrowserSelectPlugin pluginName)
    pure $ case loaded of
      Left err -> failure ui { dbuModel = model' } err
      Right schemas -> success ui
        { dbuResources = Map.insert pluginName schemas (dbuResources ui)
        , dbuModel = model'
        }

  DataBrowserSelectResource pluginName resourceName -> do
    (resources', mErr) <- ensurePluginResources runService pluginName (dbuResources ui)
    case resourceSchemaFor resources' pluginName resourceName of
      Nothing ->
        pure $ failure ui { dbuResources = resources' } $
          fromMaybe ("unknown resource: " <> resourceName) mErr
      Just schema -> do
        let model' = dataBrowserReducer (dbuModel ui) (Model.DataBrowserSelectResource pluginName schema)
        result <- runPendingIfAny runService ui { dbuResources = resources', dbuModel = model' }
        pure $ addPriorError mErr result

  DataBrowserQueryRecords pageAction ->
    runReducerThenPending runService ui (Model.DataBrowserPage pageAction)

  DataBrowserSelectRecord rowIndex ->
    pureAction ui (Model.DataBrowserSelectRecord rowIndex)

  DataBrowserDismissRecord ->
    pureAction ui Model.DataBrowserDismissRecord

  DataBrowserToggleField path ->
    pureAction ui (Model.DataBrowserToggleField path)

  DataBrowserStartEdit ->
    pureAction ui Model.DataBrowserStartEdit

  DataBrowserStartCreate ->
    pureAction ui Model.DataBrowserStartCreate

  DataBrowserCancelEdit ->
    pureAction ui Model.DataBrowserCancelEdit

  DataBrowserFocusField path ->
    pureAction ui (Model.DataBrowserFocusField path)

  DataBrowserBlurField ->
    pureAction ui Model.DataBrowserBlurField

  DataBrowserSetFieldValue path value ->
    pureAction ui (Model.DataBrowserSetFieldValue path value)

  DataBrowserStepNumberField path delta ->
    pureAction ui (Model.DataBrowserStepNumberField path delta)

  DataBrowserToggleBoolField path ->
    pureAction ui (Model.DataBrowserToggleBoolField path)

  DataBrowserCycleEnumField path direction ->
    pureAction ui (Model.DataBrowserCycleEnumField path direction)

  DataBrowserCreateRecord ->
    runSavedMutation runService ui isCreateRequest

  DataBrowserUpdateRecord ->
    runSavedMutation runService ui isUpdateRequest

  DataBrowserRequestDelete ->
    pureAction ui Model.DataBrowserRequestDelete

  DataBrowserCancelDelete ->
    pureAction ui Model.DataBrowserCancelDelete

  DataBrowserDeleteRecord ->
    runReducerThenPending runService ui Model.DataBrowserConfirmDelete

-- | Run one pending reducer request through AppService and feed completion back
-- into the reducer.
runDataBrowserPendingRequest
  :: DataBrowserRunService
  -> DataBrowserModel
  -> DataBrowserPendingRequest
  -> IO DataBrowserActionResult
runDataBrowserPendingRequest runService model request = do
  result <- performPendingRequest runService request
  pure $ case result of
    Left err -> failure (DataBrowserUi Map.empty (applyFailure err)) err
    Right completion -> success (DataBrowserUi Map.empty (completion model))
  where
    applyFailure err = case request of
      DataBrowserListRecordsRequest {} ->
        dataBrowserReducer model (Model.DataBrowserListFailed request err)
      _ ->
        dataBrowserReducer model (Model.DataBrowserMutationFailed request err)

runReducerThenPending
  :: DataBrowserRunService
  -> DataBrowserUi
  -> Model.DataBrowserAction
  -> IO DataBrowserActionResult
runReducerThenPending runService ui action =
  runPendingIfAny runService ui { dbuModel = dataBrowserReducer (dbuModel ui) action }

runSavedMutation
  :: DataBrowserRunService
  -> DataBrowserUi
  -> (DataBrowserPendingRequest -> Bool)
  -> IO DataBrowserActionResult
runSavedMutation runService ui expected = do
  let model' = dataBrowserReducer (dbuModel ui) Model.DataBrowserSave
  case dbModelPendingRequest model' of
    Just request | expected request ->
      runPendingIfAny runService ui { dbuModel = model' }
    _ ->
      pure (resultFromUi ui { dbuModel = model' })

runPendingIfAny :: DataBrowserRunService -> DataBrowserUi -> IO DataBrowserActionResult
runPendingIfAny runService ui = case dbModelPendingRequest (dbuModel ui) of
  Nothing -> pure (resultFromUi ui)
  Just request -> do
    result <- runDataBrowserPendingRequest runService (dbuModel ui) request
    pure result
      { dbarUi = (dbarUi result)
          { dbuResources = dbuResources ui
          }
      }

pureAction :: DataBrowserUi -> Model.DataBrowserAction -> IO DataBrowserActionResult
pureAction ui action =
  pure . resultFromUi $ ui { dbuModel = dataBrowserReducer (dbuModel ui) action }

performPendingRequest
  :: DataBrowserRunService
  -> DataBrowserPendingRequest
  -> IO (Either Text (DataBrowserModel -> DataBrowserModel))
performPendingRequest runService request = case request of
  DataBrowserListRecordsRequest pluginName resourceName pageSize pageOffset -> do
    body <- callService runService "data_list_records" $
      listRecordsParams pluginName resourceName pageSize pageOffset
    pure $ do
      result <- body >>= parseBody parseQueryResult
      pure $ \model -> dataBrowserReducer model (Model.DataBrowserListSucceeded request result)

  DataBrowserCreateRecordRequest pluginName resourceName record -> do
    body <- callService runService "data_create_record" $
      object [ "plugin" .= pluginName, "resource" .= resourceName, "fields" .= unDataRecord record ]
    pure $ do
      returnedRecord <- body >>= parseBody parseMutationRecord
      pure $ \model -> dataBrowserReducer model (Model.DataBrowserMutationSucceeded request returnedRecord)

  DataBrowserUpdateRecordRequest pluginName resourceName keyValue record -> do
    body <- callService runService "data_update_record" $
      object
        [ "plugin" .= pluginName
        , "resource" .= resourceName
        , "key" .= keyValue
        , "fields" .= unDataRecord record
        ]
    pure $ do
      returnedRecord <- body >>= parseBody parseMutationRecord
      pure $ \model -> dataBrowserReducer model (Model.DataBrowserMutationSucceeded request returnedRecord)

  DataBrowserDeleteRecordRequest pluginName resourceName keyValue -> do
    body <- callService runService "data_delete_record" $
      object [ "plugin" .= pluginName, "resource" .= resourceName, "key" .= keyValue ]
    pure $ do
      _ <- body
      pure $ \model -> dataBrowserReducer model (Model.DataBrowserMutationSucceeded request Nothing)

loadAllResources :: DataBrowserRunService -> IO (Either Text (Map Text [DataResourceSchema]))
loadAllResources runService = do
  body <- callService runService "data_list_plugins" Null
  case body >>= parseBody parsePluginNames of
    Left err -> pure (Left err)
    Right pluginNames -> foldM loadOne (Right Map.empty) pluginNames
  where
    loadOne (Left err) _ = pure (Left err)
    loadOne (Right resources) pluginName = do
      loaded <- loadPluginResources runService pluginName
      pure $ Map.insert pluginName <$> loaded <*> pure resources

loadPluginResources :: DataBrowserRunService -> Text -> IO (Either Text [DataResourceSchema])
loadPluginResources runService pluginName = do
  body <- callService runService "data_list_resources" (object ["plugin" .= pluginName])
  pure (body >>= parseBody parseResources)

ensurePluginResources
  :: DataBrowserRunService
  -> Text
  -> Map Text [DataResourceSchema]
  -> IO (Map Text [DataResourceSchema], Maybe Text)
ensurePluginResources runService pluginName resources
  | Map.member pluginName resources = pure (resources, Nothing)
  | otherwise = do
      loaded <- loadPluginResources runService pluginName
      pure $ case loaded of
        Left err -> (resources, Just err)
        Right schemas -> (Map.insert pluginName schemas resources, Nothing)

callService :: DataBrowserRunService -> Text -> Value -> IO (Either Text Value)
callService runService method params = do
  result <- runService method params
  pure $ case result of
    Left err -> Left (serviceErrorText err)
    Right (ServiceResponse body) -> Right body

parseBody :: (Value -> Aeson.Parser a) -> Value -> Either Text a
parseBody parser value = case Aeson.parseMaybe parser value of
  Just parsed -> Right parsed
  Nothing -> Left "invalid data-resource service response"

parsePluginNames :: Value -> Aeson.Parser [Text]
parsePluginNames = Aeson.withObject "DataResourceListPluginsResponse" $ \o -> do
  plugins <- o .: "plugins"
  mapM parsePluginName plugins
  where
    parsePluginName = Aeson.withObject "DataResourcePluginSummary" (.: "plugin")

parseResources :: Value -> Aeson.Parser [DataResourceSchema]
parseResources = Aeson.withObject "DataResourceListResourcesResponse" $ \o -> do
  resources <- o .: "resources"
  mapM parseResourceSchema resources

parseResourceSchema :: Value -> Aeson.Parser DataResourceSchema
parseResourceSchema = Aeson.withObject "DataResourceSchema" $ \o -> do
  schemaVersion <- do
    mCamel <- o .:? "schemaVersion"
    case mCamel of
      Just v -> pure v
      Nothing -> o .:? "schema_version" >>= pure . maybe currentDataResourceSchemaVersion id
  resourceVersion <- do
    mCamel <- o .:? "resourceVersion"
    case mCamel of
      Just v -> pure v
      Nothing -> o .:? "resource_version" >>= pure . maybe defaultDataResourceVersion id
  hexBound <- do
    mCamel <- o .:? "hexBound"
    case mCamel of
      Just v -> pure v
      Nothing -> o .:? "hex_bound" >>= pure . maybe False id
  keyField <- do
    mCamel <- o .:? "keyField"
    case mCamel of
      Just v -> pure v
      Nothing -> o .: "key_field"
  DataResourceSchema
    <$> pure schemaVersion
    <*> pure resourceVersion
    <*> o .: "name"
    <*> o .: "label"
    <*> pure hexBound
    <*> o .: "fields"
    <*> o .: "operations"
    <*> pure keyField
    <*> o .:? "overlay"
    <*> (o .:? "pagination" >>= pure . maybe defaultDataPagination id)

parseQueryResult :: Value -> Aeson.Parser QueryResult
parseQueryResult = Aeson.withObject "DataResourceListRecordsResponse" $ \o ->
  QueryResult
    <$> o .: "resource"
    <*> o .: "records"
    <*> o .:? "total_count"

parseMutationRecord :: Value -> Aeson.Parser (Maybe DataRecord)
parseMutationRecord = Aeson.withObject "DataResourceMutationResponse" (.:? "record")

listRecordsParams :: Text -> Text -> Maybe Int -> Maybe Int -> Value
listRecordsParams pluginName resourceName pageSize pageOffset = object $
  [ "plugin" .= pluginName
  , "resource" .= resourceName
  , "query" .= ("all" :: Text)
  ]
  <> [ "page_size" .= size | Just size <- [pageSize] ]
  <> [ "page_offset" .= offset | Just offset <- [pageOffset] ]

resourceSchemaFor :: Map Text [DataResourceSchema] -> Text -> Text -> Maybe DataResourceSchema
resourceSchemaFor resources pluginName resourceName = do
  schemas <- Map.lookup pluginName resources
  findSchema resourceName schemas

findSchema :: Text -> [DataResourceSchema] -> Maybe DataResourceSchema
findSchema _ [] = Nothing
findSchema resourceName (schema:rest)
  | drsName schema == resourceName = Just schema
  | otherwise = findSchema resourceName rest

attachSelectionSchema :: Map Text [DataResourceSchema] -> DataBrowserModel -> DataBrowserModel
attachSelectionSchema resources model = model
  { dbModelSelection = selection
      { dbSelectionSchema = do
          pluginName <- dbSelectionPlugin selection
          resourceName <- dbSelectionResource selection
          resourceSchemaFor resources pluginName resourceName
      }
  }
  where
    selection = dbModelSelection model

isCreateRequest :: DataBrowserPendingRequest -> Bool
isCreateRequest DataBrowserCreateRecordRequest {} = True
isCreateRequest _ = False

isUpdateRequest :: DataBrowserPendingRequest -> Bool
isUpdateRequest DataBrowserUpdateRecordRequest {} = True
isUpdateRequest _ = False

resultFromUi :: DataBrowserUi -> DataBrowserActionResult
resultFromUi nextUi = case validationMessage (dbuModel nextUi) of
  Nothing -> success nextUi
  Just err -> failure nextUi err

validationMessage :: DataBrowserModel -> Maybe Text
validationMessage model = case dbModelValidationErrors model of
  [] -> Nothing
  errors -> Just . Text.intercalate "; " $ map formatValidation errors
  where
    formatValidation err = case dbValidationField err of
      Nothing -> dbValidationMessage err
      Just field -> field <> ": " <> dbValidationMessage err

success :: DataBrowserUi -> DataBrowserActionResult
success nextUi = DataBrowserActionResult nextUi Nothing

failure :: DataBrowserUi -> Text -> DataBrowserActionResult
failure nextUi err = DataBrowserActionResult nextUi (Just err)

addPriorError :: Maybe Text -> DataBrowserActionResult -> DataBrowserActionResult
addPriorError Nothing result = result
addPriorError (Just err) result = result { dbarError = dbarError result <|> Just err }

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just a <|> _ = Just a
Nothing <|> b = b
