{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure Data Browser state, actions, and reducer.
--
-- This module intentionally has no SDL, actor, service, or plugin-manager
-- dependencies.  Integration layers can interpret 'DataBrowserPendingRequest'
-- values and feed completion actions back into 'dataBrowserReducer'.
module Seer.DataBrowser.Model
  ( DataBrowserModel(..)
  , DataBrowserMode(..)
  , DataBrowserSelection(..)
  , DataBrowserPagination(..)
  , DataBrowserEditBuffer(..)
  , DataBrowserValidationError(..)
  , DataBrowserRequestId(..)
  , DataBrowserOperation(..)
  , dataBrowserOperationText
  , DataBrowserPendingRequest(..)
  , DataBrowserWorkerRequest(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserAsyncError(..)
  , DataBrowserWorkerOutcome(..)
  , DataBrowserCompletion(..)
  , DataBrowserBeginResult(..)
  , DataBrowserPageAction(..)
  , DataBrowserAction(..)
  , DataBrowserUi(..)
  , emptyDataBrowserModel
  , emptyDataBrowserSelection
  , emptyDataBrowserPagination
  , emptyDataBrowserEditBuffer
  , dataBrowserReducer
  , completeDataBrowserRequest
  , dataBrowserRequestOperation
  , dataBrowserRequestIsMutation
  , dataBrowserPendingDescriptor
  , dataBrowserPendingEnvelopeValue
  , dataBrowserAsyncErrorValue
  , validateEditBuffer
  , dataBrowserCanList
  , dataBrowserCanCreate
  , dataBrowserCanUpdate
  , dataBrowserCanDelete
  , dataBrowserCanPage
  , dataBrowserPageRequestFor
  , dataBrowserPageStep
  , lookupDataBrowserFieldType
  , clearAdtSiblingEditValues
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import Data.List (find, findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, isInteger)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Data.Vector as Vector
import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , dataFieldTypeName
  , doCreate
  , doList
  , doDelete
  , doPage
  , doUpdate
  )
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..))

-- | Coarse UI mode for the data browser panel/detail popover.
data DataBrowserMode
  = DataBrowserBrowseMode
  | DataBrowserViewMode
  | DataBrowserEditMode
  | DataBrowserCreateMode
  | DataBrowserDeleteConfirmMode
  deriving (Eq, Show)

-- | Current plugin/resource/record selection.
data DataBrowserSelection = DataBrowserSelection
  { dbSelectionPlugin :: !(Maybe Text)
  , dbSelectionResource :: !(Maybe Text)
  , dbSelectionSchema :: !(Maybe DataResourceSchema)
  , dbSelectionRecord :: !(Maybe DataRecord)
  , dbSelectionRecordKey :: !(Maybe Value)
  , dbSelectionRowIndex :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Paging state for the selected resource.
data DataBrowserPagination = DataBrowserPagination
  { dbPaginationOffset :: !Int
  , dbPaginationPageSize :: !(Maybe Int)
  , dbPaginationTotalCount :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Flat dot-path to JSON-value edit buffer.
newtype DataBrowserEditBuffer = DataBrowserEditBuffer
  { dbEditBufferValues :: Map Text Value
  } deriving (Eq, Show)

-- | Validation or reducer guard error surfaced by pure actions.
data DataBrowserValidationError = DataBrowserValidationError
  { dbValidationField :: !(Maybe Text)
  , dbValidationMessage :: !Text
  } deriving (Eq, Show)

-- | Monotonically allocated by the Ui actor for accepted asynchronous work.
newtype DataBrowserRequestId = DataBrowserRequestId
  { unDataBrowserRequestId :: Word64
  } deriving (Eq, Ord, Show)

-- | Stable operation kind exposed to click and state APIs.
data DataBrowserOperation
  = DataBrowserLoadCatalogOperation
  | DataBrowserLoadPluginOperation
  | DataBrowserSelectResourceOperation
  | DataBrowserListOperation
  | DataBrowserCreateOperation
  | DataBrowserUpdateOperation
  | DataBrowserDeleteOperation
  deriving (Eq, Ord, Show)

dataBrowserOperationText :: DataBrowserOperation -> Text
dataBrowserOperationText operation = case operation of
  DataBrowserLoadCatalogOperation -> "load_catalog"
  DataBrowserLoadPluginOperation -> "load_plugin_resources"
  DataBrowserSelectResourceOperation -> "select_resource"
  DataBrowserListOperation -> "list_records"
  DataBrowserCreateOperation -> "create_record"
  DataBrowserUpdateOperation -> "update_record"
  DataBrowserDeleteOperation -> "delete_record"

-- | Effect descriptor produced by the pure reducer for integration layers.
data DataBrowserPendingRequest
  = DataBrowserListRecordsRequest !Text !Text !(Maybe Int) !(Maybe Int)
  | DataBrowserCreateRecordRequest !Text !Text !DataRecord
  | DataBrowserUpdateRecordRequest !Text !Text !Value !DataRecord
  | DataBrowserDeleteRecordRequest !Text !Text !Value
  deriving (Eq, Show)

-- | Concrete worker IO request. The envelope containing this value is the
-- authoritative target captured when the Ui actor accepts an action.
data DataBrowserWorkerRequest
  = DataBrowserLoadCatalogRequest
  | DataBrowserLoadPluginRequest !Text
  | DataBrowserSelectResourceRequest !Text !Text
  | DataBrowserRecordRequest !DataBrowserPendingRequest
  deriving (Eq, Show)

data DataBrowserPendingEnvelope = DataBrowserPendingEnvelope
  { dbpeRequestId :: !DataBrowserRequestId
  , dbpeOperation :: !DataBrowserOperation
  , dbpeRequest :: !DataBrowserWorkerRequest
  } deriving (Eq, Show)

data DataBrowserAsyncError = DataBrowserAsyncError
  { dbaeRequestId :: !DataBrowserRequestId
  , dbaeOperation :: !DataBrowserOperation
  , dbaeRequest :: !DataBrowserWorkerRequest
  , dbaeMessage :: !Text
  } deriving (Eq, Show)

-- | Typed worker result. Resource/schema and record changes are deliberately
-- returned together so the Ui owner can publish one atomic snapshot.
data DataBrowserWorkerOutcome
  = DataBrowserCatalogLoaded !(Map Text [DataResourceSchema])
  | DataBrowserPluginLoaded !Text ![DataResourceSchema]
  | DataBrowserResourceLoaded !Text ![DataResourceSchema] !DataResourceSchema !(Maybe QueryResult)
  | DataBrowserRecordsLoaded !QueryResult
  | DataBrowserMutationCompleted !(Maybe DataRecord)
  | DataBrowserWorkerFailed !Text
  deriving (Eq, Show)

data DataBrowserCompletion = DataBrowserCompletion
  { dbcRequestId :: !DataBrowserRequestId
  , dbcRequest :: !DataBrowserWorkerRequest
  , dbcOutcome :: !DataBrowserWorkerOutcome
  } deriving (Eq, Show)

data DataBrowserBeginResult
  = DataBrowserBeginRejected !Text
  | DataBrowserBeginPure
  | DataBrowserBeginAccepted !DataBrowserPendingEnvelope !(Maybe DataBrowserRequestId)
  deriving (Eq, Show)

-- | Pagination intent.
data DataBrowserPageAction
  = DataBrowserPagePrevious
  | DataBrowserPageNext
  | DataBrowserPageOffset !Int
  deriving (Eq, Show)

-- | Pure events handled by 'dataBrowserReducer'.
data DataBrowserAction
  = DataBrowserSelectPlugin !Text
  | DataBrowserSelectResource !Text !DataResourceSchema
  | DataBrowserListSucceeded !DataBrowserPendingRequest !QueryResult
  | DataBrowserListFailed !DataBrowserPendingRequest !Text
  | DataBrowserSelectRecord !Int
  | DataBrowserDismissRecord
  | DataBrowserToggleField !Text
  | DataBrowserStartEdit
  | DataBrowserStartCreate
  | DataBrowserCancelEdit
  | DataBrowserFocusField !Text
  | DataBrowserBlurField
  | DataBrowserSetFieldValue !Text !Value
  | DataBrowserInsertText !Text
  | DataBrowserReplaceText !Text
  | DataBrowserBackspace
  | DataBrowserDeleteText
  | DataBrowserSetTextCursor !Int
  | DataBrowserStepNumberField !Text !Int
  | DataBrowserToggleBoolField !Text
  | DataBrowserCycleEnumField !Text !Int
  | DataBrowserSave
  | DataBrowserPage !DataBrowserPageAction
  | DataBrowserRequestDelete
  | DataBrowserCancelDelete
  | DataBrowserConfirmDelete
  | DataBrowserMutationSucceeded !DataBrowserPendingRequest !(Maybe DataRecord)
  | DataBrowserMutationFailed !DataBrowserPendingRequest !Text
  deriving (Eq, Show)

-- | Complete pure model for the Data Browser.
data DataBrowserModel = DataBrowserModel
  { dbModelMode :: !DataBrowserMode
  , dbModelSelection :: !DataBrowserSelection
  , dbModelRecords :: ![DataRecord]
  , dbModelPagination :: !DataBrowserPagination
  , dbModelExpandedFields :: !(Set Text)
  , dbModelEditBuffer :: !DataBrowserEditBuffer
  , dbModelFocusedField :: !(Maybe Text)
  , dbModelTextCursor :: !Int
  , dbModelValidationErrors :: ![DataBrowserValidationError]
  , dbModelPendingRequest :: !(Maybe DataBrowserPendingRequest)
  , dbModelPendingEnvelope :: !(Maybe DataBrowserPendingEnvelope)
  , dbModelAsyncError :: !(Maybe DataBrowserAsyncError)
  , dbModelLoading :: !Bool
  } deriving (Eq, Show)

-- | Resource catalog plus browser model, atomically owned by the Ui actor.
data DataBrowserUi = DataBrowserUi
  { dbuResources :: !(Map Text [DataResourceSchema])
  , dbuModel :: !DataBrowserModel
  } deriving (Eq, Show)

emptyDataBrowserSelection :: DataBrowserSelection
emptyDataBrowserSelection = DataBrowserSelection
  { dbSelectionPlugin = Nothing
  , dbSelectionResource = Nothing
  , dbSelectionSchema = Nothing
  , dbSelectionRecord = Nothing
  , dbSelectionRecordKey = Nothing
  , dbSelectionRowIndex = Nothing
  }

emptyDataBrowserPagination :: DataBrowserPagination
emptyDataBrowserPagination = DataBrowserPagination
  { dbPaginationOffset = 0
  , dbPaginationPageSize = Nothing
  , dbPaginationTotalCount = Nothing
  }

emptyDataBrowserEditBuffer :: DataBrowserEditBuffer
emptyDataBrowserEditBuffer = DataBrowserEditBuffer Map.empty

emptyDataBrowserModel :: DataBrowserModel
emptyDataBrowserModel = DataBrowserModel
  { dbModelMode = DataBrowserBrowseMode
  , dbModelSelection = emptyDataBrowserSelection
  , dbModelRecords = []
  , dbModelPagination = emptyDataBrowserPagination
  , dbModelExpandedFields = Set.empty
  , dbModelEditBuffer = emptyDataBrowserEditBuffer
  , dbModelFocusedField = Nothing
  , dbModelTextCursor = 0
  , dbModelValidationErrors = []
  , dbModelPendingRequest = Nothing
  , dbModelPendingEnvelope = Nothing
  , dbModelAsyncError = Nothing
  , dbModelLoading = False
  }

-- | Apply one pure action to a model.
dataBrowserReducer :: DataBrowserModel -> DataBrowserAction -> DataBrowserModel
dataBrowserReducer model action = case action of
  DataBrowserSelectPlugin pluginName ->
    emptyDataBrowserModel
      { dbModelSelection = emptyDataBrowserSelection
          { dbSelectionPlugin = Just pluginName }
      }

  DataBrowserSelectResource pluginName schema ->
    let (pageSize, pageOffset) = dataBrowserPageRequestFor schema
        offset = maybe 0 id pageOffset
        resourceName = drsName schema
        request = DataBrowserListRecordsRequest pluginName resourceName pageSize pageOffset
        selectedModel = emptyDataBrowserModel
          { dbModelSelection = emptyDataBrowserSelection
              { dbSelectionPlugin = Just pluginName
              , dbSelectionResource = Just resourceName
              , dbSelectionSchema = Just schema
              }
          , dbModelPagination = DataBrowserPagination offset pageSize Nothing
          }
    in if doList (drsOperations schema)
       then selectedModel
          { dbModelPendingRequest = Just request
          , dbModelLoading = True
          }
       else selectedModel
          { dbModelValidationErrors = [validationError Nothing "list not supported"] }

  DataBrowserListSucceeded request result
    | dbModelPendingRequest model == Just request ->
        let pagination = dbModelPagination model
        in model
          { dbModelRecords = qrsRecords result
          , dbModelPagination = pagination
              { dbPaginationTotalCount = qrsTotalCount result }
          , dbModelPendingRequest = Nothing
          , dbModelLoading = False
          , dbModelValidationErrors = []
          }
    | otherwise -> model

  DataBrowserListFailed request message
    | dbModelPendingRequest model == Just request ->
        model
          { dbModelPendingRequest = Nothing
          , dbModelLoading = False
          , dbModelValidationErrors = [validationError Nothing message]
          }
    | otherwise -> model

  DataBrowserSelectRecord rowIndex -> selectRecord rowIndex model

  DataBrowserDismissRecord -> clearDetail model

  DataBrowserToggleField path ->
    let expanded = dbModelExpandedFields model
        expanded'
          | Set.member path expanded = Set.delete path expanded
          | otherwise = Set.insert path expanded
    in model { dbModelExpandedFields = expanded' }

  DataBrowserStartEdit -> startEdit model

  DataBrowserStartCreate -> startCreate model

  DataBrowserCancelEdit -> cancelEdit model

  DataBrowserFocusField path ->
    let cursor = fieldTextLength path (dbModelEditBuffer model)
    in model { dbModelFocusedField = Just path, dbModelTextCursor = cursor }

  DataBrowserBlurField ->
    model { dbModelFocusedField = Nothing, dbModelTextCursor = 0 }

  DataBrowserSetFieldValue path value ->
    updateEditValue path value model

  DataBrowserInsertText text ->
    editFocusedText (insertAtCursor text) model

  DataBrowserReplaceText text -> case dbModelFocusedField model of
    Nothing -> model
    Just path -> (updateEditValue path (String text) model)
      { dbModelTextCursor = Text.length text }

  DataBrowserBackspace ->
    editFocusedText deleteBeforeCursor model

  DataBrowserDeleteText ->
    editFocusedText deleteAtCursor model

  DataBrowserSetTextCursor cursor ->
    model { dbModelTextCursor = clampTextCursor cursor model }

  DataBrowserStepNumberField path delta ->
    let values = dbEditBufferValues (dbModelEditBuffer model)
        current = Map.lookup path values
        value = Number (steppedNumber current delta)
    in updateEditValue path value model

  DataBrowserToggleBoolField path ->
    let values = dbEditBufferValues (dbModelEditBuffer model)
        current = case Map.lookup path values of
          Just (Bool b) -> b
          _ -> False
    in updateEditValue path (Bool (not current)) model

  DataBrowserCycleEnumField path direction ->
    cycleEnumField path direction model

  DataBrowserSave -> saveEditBuffer model

  DataBrowserPage pageAction -> requestPage pageAction model

  DataBrowserRequestDelete -> requestDelete model

  DataBrowserCancelDelete ->
    let mode = if hasSelectedRecord model then DataBrowserViewMode else DataBrowserBrowseMode
    in model { dbModelMode = mode, dbModelValidationErrors = [] }

  DataBrowserConfirmDelete -> confirmDelete model

  DataBrowserMutationSucceeded request record
    | dbModelPendingRequest model == Just request ->
        completeMutationSuccess request record model
    | otherwise -> model

  DataBrowserMutationFailed request message
    | dbModelPendingRequest model == Just request ->
        model
          { dbModelPendingRequest = Nothing
          , dbModelLoading = False
          , dbModelValidationErrors = [validationError Nothing message]
          }
    | otherwise -> model

-- | Apply one terminal worker result. ID and current target context are the
-- correctness guards; the descriptor echoed by the worker is diagnostic only.
-- A stale completion is a total no-op.
completeDataBrowserRequest
  :: DataBrowserCompletion
  -> DataBrowserUi
  -> (DataBrowserUi, Bool)
completeDataBrowserRequest completion ui = case dbModelPendingEnvelope model of
  Nothing -> (ui, False)
  Just pending
    | dbpeRequestId pending /= dbcRequestId completion -> (ui, False)
    | not (requestMatchesCurrent (dbpeRequest pending) model) -> (ui, False)
    | otherwise ->
        let ui' = applyOutcome pending (dbcOutcome completion) ui
        in (ui', True)
  where
    model = dbuModel ui

applyOutcome :: DataBrowserPendingEnvelope -> DataBrowserWorkerOutcome -> DataBrowserUi -> DataBrowserUi
applyOutcome pending outcome ui = case outcome of
  DataBrowserWorkerFailed message ->
    ui { dbuModel = failedModel message }
  DataBrowserCatalogLoaded resources ->
    ui
      { dbuResources = resources
      , dbuModel = clearPending (attachSelectionSchema resources model)
      }
  DataBrowserPluginLoaded pluginName schemas ->
    let resources = Map.insert pluginName schemas (dbuResources ui)
    in ui
      { dbuResources = resources
      , dbuModel = clearPending (attachSelectionSchema resources model)
      }
  DataBrowserResourceLoaded pluginName schemas schema mResult ->
    let resources = Map.insert pluginName schemas (dbuResources ui)
        selected = dataBrowserReducer model (DataBrowserSelectResource pluginName schema)
        completed = case (dbModelPendingRequest selected, mResult) of
          (Just descriptor, Just result) ->
            dataBrowserReducer selected (DataBrowserListSucceeded descriptor result)
          _ -> selected
    in ui { dbuResources = resources, dbuModel = clearPending completed }
  DataBrowserRecordsLoaded result ->
    ui { dbuModel = clearPending (completeRecords result) }
  DataBrowserMutationCompleted returned ->
    ui { dbuModel = clearPending (completeMutation returned) }
  where
    model = dbuModel ui
    request = dbpeRequest pending

    failedModel message =
      let next = case dataBrowserPendingDescriptor request of
            Just descriptor@DataBrowserListRecordsRequest {} ->
              dataBrowserReducer (withDescriptor descriptor model) (DataBrowserListFailed descriptor message)
            Just descriptor ->
              dataBrowserReducer (withDescriptor descriptor model) (DataBrowserMutationFailed descriptor message)
            Nothing -> model
              { dbModelValidationErrors = [DataBrowserValidationError Nothing message] }
      in (clearPending next)
        { dbModelAsyncError = Just DataBrowserAsyncError
            { dbaeRequestId = dbpeRequestId pending
            , dbaeOperation = dbpeOperation pending
            , dbaeRequest = request
            , dbaeMessage = message
            }
        }

    completeRecords result = case dataBrowserPendingDescriptor request of
      Just descriptor@DataBrowserListRecordsRequest {} ->
        dataBrowserReducer (withDescriptor descriptor model) (DataBrowserListSucceeded descriptor result)
      _ -> model

    completeMutation returned = case dataBrowserPendingDescriptor request of
      Just descriptor ->
        dataBrowserReducer (withDescriptor descriptor model) (DataBrowserMutationSucceeded descriptor returned)
      Nothing -> model

clearPending :: DataBrowserModel -> DataBrowserModel
clearPending model = model
  { dbModelPendingRequest = Nothing
  , dbModelPendingEnvelope = Nothing
  , dbModelLoading = False
  }

withDescriptor :: DataBrowserPendingRequest -> DataBrowserModel -> DataBrowserModel
withDescriptor descriptor model = model
  { dbModelPendingRequest = Just descriptor
  , dbModelLoading = True
  }

dataBrowserRequestOperation :: DataBrowserWorkerRequest -> DataBrowserOperation
dataBrowserRequestOperation request = case request of
  DataBrowserLoadCatalogRequest -> DataBrowserLoadCatalogOperation
  DataBrowserLoadPluginRequest _ -> DataBrowserLoadPluginOperation
  DataBrowserSelectResourceRequest _ _ -> DataBrowserSelectResourceOperation
  DataBrowserRecordRequest descriptor -> case descriptor of
    DataBrowserListRecordsRequest {} -> DataBrowserListOperation
    DataBrowserCreateRecordRequest {} -> DataBrowserCreateOperation
    DataBrowserUpdateRecordRequest {} -> DataBrowserUpdateOperation
    DataBrowserDeleteRecordRequest {} -> DataBrowserDeleteOperation

dataBrowserRequestIsMutation :: DataBrowserWorkerRequest -> Bool
dataBrowserRequestIsMutation request = case dataBrowserRequestOperation request of
  DataBrowserCreateOperation -> True
  DataBrowserUpdateOperation -> True
  DataBrowserDeleteOperation -> True
  _ -> False

dataBrowserPendingDescriptor :: DataBrowserWorkerRequest -> Maybe DataBrowserPendingRequest
dataBrowserPendingDescriptor (DataBrowserRecordRequest descriptor) = Just descriptor
dataBrowserPendingDescriptor _ = Nothing

dataBrowserPendingEnvelopeValue :: DataBrowserPendingEnvelope -> Value
dataBrowserPendingEnvelopeValue envelope = object
  [ "request_id" .= unDataBrowserRequestId (dbpeRequestId envelope)
  , "operation" .= dataBrowserOperationText (dbpeOperation envelope)
  , "target" .= dataBrowserWorkerTargetValue (dbpeRequest envelope)
  ]

dataBrowserAsyncErrorValue :: DataBrowserAsyncError -> Value
dataBrowserAsyncErrorValue asyncError = object
  [ "request_id" .= unDataBrowserRequestId (dbaeRequestId asyncError)
  , "operation" .= dataBrowserOperationText (dbaeOperation asyncError)
  , "target" .= dataBrowserWorkerTargetValue (dbaeRequest asyncError)
  , "message" .= dbaeMessage asyncError
  ]

dataBrowserWorkerTargetValue :: DataBrowserWorkerRequest -> Value
dataBrowserWorkerTargetValue request = case request of
  DataBrowserLoadCatalogRequest -> object []
  DataBrowserLoadPluginRequest pluginName -> object ["plugin" .= pluginName]
  DataBrowserSelectResourceRequest pluginName resourceName -> object
    [ "plugin" .= pluginName, "resource" .= resourceName ]
  DataBrowserRecordRequest descriptor -> case descriptor of
    DataBrowserListRecordsRequest pluginName resourceName pageSize pageOffset -> object
      [ "plugin" .= pluginName
      , "resource" .= resourceName
      , "page_size" .= pageSize
      , "page_offset" .= pageOffset
      ]
    DataBrowserCreateRecordRequest pluginName resourceName _ -> object
      [ "plugin" .= pluginName, "resource" .= resourceName ]
    DataBrowserUpdateRecordRequest pluginName resourceName key _ -> object
      [ "plugin" .= pluginName, "resource" .= resourceName, "key" .= key ]
    DataBrowserDeleteRecordRequest pluginName resourceName key -> object
      [ "plugin" .= pluginName, "resource" .= resourceName, "key" .= key ]

requestMatchesCurrent :: DataBrowserWorkerRequest -> DataBrowserModel -> Bool
requestMatchesCurrent request model = case request of
  DataBrowserLoadCatalogRequest -> True
  DataBrowserLoadPluginRequest pluginName -> selectedPlugin == Just pluginName
  DataBrowserSelectResourceRequest pluginName resourceName ->
    selectedPlugin == Just pluginName && selectedResource == Just resourceName
  DataBrowserRecordRequest descriptor -> case descriptor of
    DataBrowserListRecordsRequest pluginName resourceName _ _ -> sameResource pluginName resourceName
    DataBrowserCreateRecordRequest pluginName resourceName _ -> sameResource pluginName resourceName
    DataBrowserUpdateRecordRequest pluginName resourceName key _ ->
      sameResource pluginName resourceName && dbSelectionRecordKey selection == Just key
    DataBrowserDeleteRecordRequest pluginName resourceName key ->
      sameResource pluginName resourceName && dbSelectionRecordKey selection == Just key
  where
    selection = dbModelSelection model
    selectedPlugin = dbSelectionPlugin selection
    selectedResource = dbSelectionResource selection
    sameResource pluginName resourceName =
      selectedPlugin == Just pluginName && selectedResource == Just resourceName

resourceSchemaFor :: Map Text [DataResourceSchema] -> Text -> Text -> Maybe DataResourceSchema
resourceSchemaFor resources pluginName resourceName = do
  schemas <- Map.lookup pluginName resources
  find ((== resourceName) . drsName) schemas

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

-- | Validate every buffered dot-path value against the selected resource schema.
validateEditBuffer :: DataResourceSchema -> DataBrowserEditBuffer -> [DataBrowserValidationError]
validateEditBuffer schema (DataBrowserEditBuffer values) =
  concatMap validateOne (Map.toList values)
  where
    validateOne (path, value) = case lookupDataBrowserField path (drsFields schema) of
      Nothing -> [validationError (Just path) "unknown field"]
      Just field
        | not (dfEditable field) -> [validationError (Just path) "field is read-only"]
        | valueMatchesField (dfType field) value -> []
        | otherwise ->
            [ validationError (Just path)
                ("expected " <> dataFieldTypeName (dfType field))
            ]

-- | Whether the selected resource advertises list support.
dataBrowserCanList :: DataBrowserModel -> Bool
dataBrowserCanList = maybe False (doList . drsOperations) . dbSelectionSchema . dbModelSelection

-- | Whether the selected resource advertises create support.
dataBrowserCanCreate :: DataBrowserModel -> Bool
dataBrowserCanCreate = maybe False (doCreate . drsOperations) . dbSelectionSchema . dbModelSelection

-- | Whether the selected resource advertises update support.
dataBrowserCanUpdate :: DataBrowserModel -> Bool
dataBrowserCanUpdate = maybe False (doUpdate . drsOperations) . dbSelectionSchema . dbModelSelection

-- | Whether the selected resource advertises delete support.
dataBrowserCanDelete :: DataBrowserModel -> Bool
dataBrowserCanDelete = maybe False (doDelete . drsOperations) . dbSelectionSchema . dbModelSelection

-- | Whether the selected resource advertises pagination support.
dataBrowserCanPage :: DataBrowserModel -> Bool
dataBrowserCanPage = maybe False (doPage . drsOperations) . dbSelectionSchema . dbModelSelection

-- | Compute the request page size and offset for an initial list query.
dataBrowserPageRequestFor :: DataResourceSchema -> (Maybe Int, Maybe Int)
dataBrowserPageRequestFor schema
  | doPage (drsOperations schema) =
      let pagination = drsPagination schema
      in ( Just (pageSizeFrom pagination)
         , Just (max 0 (dpDefaultPageOffset pagination))
         )
  | otherwise = (Nothing, Nothing)

-- | Effective page step for pagination controls.
dataBrowserPageStep :: DataBrowserModel -> Int
dataBrowserPageStep model = maybe 20 id (dbPaginationPageSize (dbModelPagination model))

-- | Resolve a dot-path field type in a resource schema field list.
lookupDataBrowserFieldType :: Text -> [DataFieldDef] -> Maybe DataFieldType
lookupDataBrowserFieldType path fields = dfType <$> lookupDataBrowserField path fields

selectRecord :: Int -> DataBrowserModel -> DataBrowserModel
selectRecord rowIndex model = case recordAt rowIndex (dbModelRecords model) of
  Nothing -> model
    { dbModelValidationErrors = [validationError Nothing "record index out of range"] }
  Just record ->
    let selection = dbModelSelection model
        keyField = maybe "id" drsKeyField (dbSelectionSchema selection)
        keyValue = Map.lookup keyField (unDataRecord record)
    in model
      { dbModelMode = DataBrowserViewMode
      , dbModelSelection = selection
          { dbSelectionRecord = Just record
          , dbSelectionRecordKey = keyValue
          , dbSelectionRowIndex = Just rowIndex
          }
      , dbModelExpandedFields = Set.empty
      , dbModelEditBuffer = emptyDataBrowserEditBuffer
      , dbModelFocusedField = Nothing
      , dbModelTextCursor = 0
      , dbModelValidationErrors = []
      }

startEdit :: DataBrowserModel -> DataBrowserModel
startEdit model = case dbSelectionRecord (dbModelSelection model) of
  Nothing -> model
    { dbModelValidationErrors = [validationError Nothing "no record selected"] }
  Just record ->
    let values = case dbSelectionSchema (dbModelSelection model) of
          Just schema -> editableRecordValues schema record
          Nothing -> unDataRecord record
    in model
      { dbModelMode = DataBrowserEditMode
      , dbModelEditBuffer = DataBrowserEditBuffer values
      , dbModelFocusedField = Nothing
      , dbModelTextCursor = 0
      , dbModelValidationErrors = []
      }

startCreate :: DataBrowserModel -> DataBrowserModel
startCreate model = case dbSelectionSchema (dbModelSelection model) of
  Nothing -> model
    { dbModelValidationErrors = [validationError Nothing "no resource selected"] }
  Just schema ->
    let defaults = defaultValues schema
        dummyRecord = editBufferRecord schema Nothing (DataBrowserEditBuffer defaults)
        selection = dbModelSelection model
    in model
      { dbModelMode = DataBrowserCreateMode
      , dbModelSelection = selection
          { dbSelectionRecord = Just dummyRecord
          , dbSelectionRecordKey = Nothing
          , dbSelectionRowIndex = Just 0
          }
      , dbModelExpandedFields = Set.empty
      , dbModelEditBuffer = DataBrowserEditBuffer defaults
      , dbModelFocusedField = Nothing
      , dbModelTextCursor = 0
      , dbModelValidationErrors = []
      }

cancelEdit :: DataBrowserModel -> DataBrowserModel
cancelEdit model = case dbModelMode model of
  DataBrowserCreateMode -> clearDetail model
  DataBrowserDeleteConfirmMode ->
    model
      { dbModelMode = if hasSelectedRecord model then DataBrowserViewMode else DataBrowserBrowseMode
      , dbModelValidationErrors = []
      }
  _ ->
    model
      { dbModelMode = if hasSelectedRecord model then DataBrowserViewMode else DataBrowserBrowseMode
      , dbModelEditBuffer = emptyDataBrowserEditBuffer
      , dbModelFocusedField = Nothing
      , dbModelTextCursor = 0
      , dbModelValidationErrors = []
      }

saveEditBuffer :: DataBrowserModel -> DataBrowserModel
saveEditBuffer model = case dbSelectionSchema selection of
  Nothing -> model { dbModelValidationErrors = [validationError Nothing "no resource selected"] }
  Just schema ->
    let errors = validateEditBuffer schema (dbModelEditBuffer model)
    in if not (null errors)
       then model { dbModelValidationErrors = errors, dbModelPendingRequest = Nothing, dbModelLoading = False }
       else case dbModelMode model of
         DataBrowserCreateMode
           | dataBrowserCanCreate model -> saveCreate selection model
           | otherwise -> model { dbModelValidationErrors = [validationError Nothing "create not supported"] }
         DataBrowserEditMode
           | dataBrowserCanUpdate model -> saveUpdate selection model
           | otherwise -> model { dbModelValidationErrors = [validationError Nothing "update not supported"] }
         _ -> model { dbModelValidationErrors = [validationError Nothing "not editing"] }
  where
    selection = dbModelSelection model

saveCreate :: DataBrowserSelection -> DataBrowserModel -> DataBrowserModel
saveCreate selection model = case (dbSelectionPlugin selection, dbSelectionResource selection, dbSelectionSchema selection) of
  (Just pluginName, Just resourceName, Just schema) ->
    let record = editBufferRecord schema Nothing (dbModelEditBuffer model)
        request = DataBrowserCreateRecordRequest pluginName resourceName record
    in model
      { dbModelPendingRequest = Just request
      , dbModelLoading = True
      , dbModelValidationErrors = []
      }
  _ -> model { dbModelValidationErrors = [validationError Nothing "no resource selected"] }

saveUpdate :: DataBrowserSelection -> DataBrowserModel -> DataBrowserModel
saveUpdate selection model =
  case (dbSelectionPlugin selection, dbSelectionResource selection, dbSelectionRecordKey selection, dbSelectionSchema selection) of
    (Just pluginName, Just resourceName, Just keyValue, Just schema) ->
      let record = editBufferRecord schema (dbSelectionRecord selection) (dbModelEditBuffer model)
          request = DataBrowserUpdateRecordRequest pluginName resourceName keyValue record
      in model
        { dbModelValidationErrors = []
        , dbModelPendingRequest = Just request
        , dbModelLoading = True
        }
    _ -> model { dbModelValidationErrors = [validationError Nothing "no selected record key"] }

requestPage :: DataBrowserPageAction -> DataBrowserModel -> DataBrowserModel
requestPage pageAction model
  | not (dataBrowserCanPage model) = model
  | not (dataBrowserCanList model) =
      model { dbModelValidationErrors = [validationError Nothing "list not supported"] }
  | otherwise = case (dbSelectionPlugin selection, dbSelectionResource selection) of
      (Just pluginName, Just resourceName) ->
        let step = dataBrowserPageStep model
            currentOffset = dbPaginationOffset pagination
            offset = case pageAction of
              DataBrowserPagePrevious -> max 0 (currentOffset - step)
              DataBrowserPageNext -> currentOffset + step
              DataBrowserPageOffset requested -> max 0 requested
            pageSize = dbPaginationPageSize pagination
            pageOffset = fmap (const offset) pageSize
            request = DataBrowserListRecordsRequest pluginName resourceName pageSize pageOffset
        in model
          { dbModelRecords = []
          , dbModelPagination = pagination
              { dbPaginationOffset = offset
              , dbPaginationTotalCount = Nothing
              }
          , dbModelSelection = clearSelectedRecord selection
          , dbModelExpandedFields = Set.empty
          , dbModelEditBuffer = emptyDataBrowserEditBuffer
          , dbModelFocusedField = Nothing
          , dbModelTextCursor = 0
          , dbModelValidationErrors = []
          , dbModelPendingRequest = Just request
          , dbModelLoading = True
          , dbModelMode = DataBrowserBrowseMode
          }
      _ -> model { dbModelValidationErrors = [validationError Nothing "no resource selected"] }
  where
    selection = dbModelSelection model
    pagination = dbModelPagination model

requestDelete :: DataBrowserModel -> DataBrowserModel
requestDelete model
  | not (dataBrowserCanDelete model) =
      model { dbModelValidationErrors = [validationError Nothing "delete not supported"] }
  | otherwise = case dbSelectionRecordKey (dbModelSelection model) of
      Just _ -> model
        { dbModelMode = DataBrowserDeleteConfirmMode
        , dbModelValidationErrors = []
        }
      Nothing -> model
        { dbModelValidationErrors = [validationError Nothing "no selected record key"] }

confirmDelete :: DataBrowserModel -> DataBrowserModel
confirmDelete model
  | dbModelMode model /= DataBrowserDeleteConfirmMode =
      model { dbModelValidationErrors = [validationError Nothing "delete not confirmed"] }
  | otherwise = case ( dbSelectionPlugin selection
                     , dbSelectionResource selection
                     , dbSelectionRecordKey selection
                     ) of
      (Just pluginName, Just resourceName, Just keyValue) ->
        let request = DataBrowserDeleteRecordRequest pluginName resourceName keyValue
        in model
          { dbModelValidationErrors = []
          , dbModelPendingRequest = Just request
          , dbModelLoading = True
          }
      _ -> model { dbModelValidationErrors = [validationError Nothing "no selected record key"] }
  where
    selection = dbModelSelection model

completeMutationSuccess :: DataBrowserPendingRequest -> Maybe DataRecord -> DataBrowserModel -> DataBrowserModel
completeMutationSuccess request returnedRecord model = case request of
  DataBrowserListRecordsRequest _ _ _ _ ->
    model
      { dbModelPendingRequest = Nothing
      , dbModelLoading = False
      , dbModelValidationErrors = []
      }
  DataBrowserCreateRecordRequest _ _ submittedRecord ->
    let createdRecord = maybe submittedRecord id returnedRecord
        model' = clearDetail model
    in model'
      { dbModelRecords = dbModelRecords model <> [createdRecord]
      , dbModelPagination = incrementTotal (dbModelPagination model)
      , dbModelPendingRequest = Nothing
      , dbModelLoading = False
      , dbModelValidationErrors = []
      }
  DataBrowserUpdateRecordRequest _ _ keyValue submittedRecord ->
    let selection = dbModelSelection model
        keyField = maybe "id" drsKeyField (dbSelectionSchema selection)
        updatedRecord = case returnedRecord of
          Just record -> record
          Nothing -> mergeSubmittedRecord keyField keyValue selection submittedRecord (dbModelRecords model)
        records' = replaceRecord keyField keyValue (dbSelectionRowIndex selection) updatedRecord (dbModelRecords model)
        keyValue' = case Map.lookup keyField (unDataRecord updatedRecord) of
          Just value -> Just value
          Nothing -> Just keyValue
    in model
      { dbModelMode = DataBrowserViewMode
      , dbModelSelection = selection
          { dbSelectionRecord = Just updatedRecord
          , dbSelectionRecordKey = keyValue'
          }
      , dbModelRecords = records'
      , dbModelEditBuffer = emptyDataBrowserEditBuffer
      , dbModelFocusedField = Nothing
      , dbModelTextCursor = 0
      , dbModelValidationErrors = []
      , dbModelPendingRequest = Nothing
      , dbModelLoading = False
      }
  DataBrowserDeleteRecordRequest _ _ keyValue ->
    let selection = dbModelSelection model
        keyField = maybe "id" drsKeyField (dbSelectionSchema selection)
        records' = deleteRecord keyField keyValue (dbSelectionRowIndex selection) (dbModelRecords model)
        model' = clearDetail model
    in model'
      { dbModelRecords = records'
      , dbModelPagination = decrementTotal (dbModelPagination model)
      , dbModelPendingRequest = Nothing
      , dbModelLoading = False
      , dbModelValidationErrors = []
      }

clearDetail :: DataBrowserModel -> DataBrowserModel
clearDetail model = model
  { dbModelMode = DataBrowserBrowseMode
  , dbModelSelection = clearSelectedRecord (dbModelSelection model)
  , dbModelExpandedFields = Set.empty
  , dbModelEditBuffer = emptyDataBrowserEditBuffer
  , dbModelFocusedField = Nothing
  , dbModelTextCursor = 0
  , dbModelValidationErrors = []
  }

clearSelectedRecord :: DataBrowserSelection -> DataBrowserSelection
clearSelectedRecord selection = selection
  { dbSelectionRecord = Nothing
  , dbSelectionRecordKey = Nothing
  , dbSelectionRowIndex = Nothing
  }

hasSelectedRecord :: DataBrowserModel -> Bool
hasSelectedRecord model = case dbSelectionRecord (dbModelSelection model) of
  Just _ -> True
  Nothing -> False

updateEditValue :: Text -> Value -> DataBrowserModel -> DataBrowserModel
updateEditValue path value model =
  let DataBrowserEditBuffer values = dbModelEditBuffer model
      valuesForPath = clearAdtSiblingValues path model values
      errors = filter ((/= Just path) . dbValidationField) (dbModelValidationErrors model)
  in model
    { dbModelEditBuffer = DataBrowserEditBuffer (Map.insert path value valuesForPath)
    , dbModelValidationErrors = errors
    }

clearAdtSiblingValues :: Text -> DataBrowserModel -> Map Text Value -> Map Text Value
clearAdtSiblingValues path model values =
  case dbSelectionSchema (dbModelSelection model) of
    Nothing -> values
    Just schema -> clearAdtSiblingEditValues (drsFields schema) path values

clearAdtSiblingEditValues :: [DataFieldDef] -> Text -> Map Text Value -> Map Text Value
clearAdtSiblingEditValues fields path values =
  foldl clearOne values (adtEditSelections path fields)
  where
    clearOne acc (adtPath, constructorName) =
      let adtPrefix = adtPath <> "."
          constructorPrefix = adtPath <> "." <> constructorName <> "."
          keep key _ = not (Text.isPrefixOf adtPrefix key) || Text.isPrefixOf constructorPrefix key
      in Map.filterWithKey keep acc

adtEditSelections :: Text -> [DataFieldDef] -> [(Text, Text)]
adtEditSelections path fields = resolveFields "" (Text.splitOn "." path) fields
  where
    resolveFields _ [] _ = []
    resolveFields prefix (segment:rest) defs =
      case find ((== segment) . dfName) defs of
        Nothing -> []
        Just field -> resolveFieldType (qualifyPath prefix segment) rest (dfType field)

    resolveFieldType _ [] _ = []
    resolveFieldType pathPrefix segments fieldType = case fieldType of
      DFRecord nestedFields -> resolveFields pathPrefix segments nestedFields
      DFAdt constructors -> case segments of
        constructorName:indexText:rest -> case find ((== constructorName) . dcdName) constructors of
          Nothing -> []
          Just constructorDef ->
            let here = [(pathPrefix, constructorName)]
                nested = case readInt indexText >>= (`nth` dcdFields constructorDef) of
                  Nothing -> []
                  Just positionalType ->
                    resolveFieldType (pathPrefix <> "." <> constructorName <> "." <> indexText) rest positionalType
            in here <> nested
        constructorName:_
          | any ((== constructorName) . dcdName) constructors -> [(pathPrefix, constructorName)]
        _ -> []
      _ -> []

editFocusedText :: (Text -> Int -> (Text, Int)) -> DataBrowserModel -> DataBrowserModel
editFocusedText f model = case dbModelFocusedField model of
  Nothing -> model
  Just path ->
    let DataBrowserEditBuffer values = dbModelEditBuffer model
        current = case Map.lookup path values of
          Just (String text) -> text
          _ -> ""
        (text', cursor') = f current (dbModelTextCursor model)
    in (updateEditValue path (String text') model)
      { dbModelTextCursor = cursor' }

insertAtCursor :: Text -> Text -> Int -> (Text, Int)
insertAtCursor inserted current cursor =
  let cursor' = clamp 0 (Text.length current) cursor
      (before, after) = Text.splitAt cursor' current
      next = before <> inserted <> after
  in (next, cursor' + Text.length inserted)

deleteBeforeCursor :: Text -> Int -> (Text, Int)
deleteBeforeCursor current cursor
  | cursor' <= 0 = (current, 0)
  | otherwise =
      let before = Text.take (cursor' - 1) current
          after = Text.drop cursor' current
      in (before <> after, cursor' - 1)
  where
    cursor' = clamp 0 (Text.length current) cursor

deleteAtCursor :: Text -> Int -> (Text, Int)
deleteAtCursor current cursor
  | cursor' >= Text.length current = (current, cursor')
  | otherwise = (Text.take cursor' current <> Text.drop (cursor' + 1) current, cursor')
  where
    cursor' = clamp 0 (Text.length current) cursor

clampTextCursor :: Int -> DataBrowserModel -> Int
clampTextCursor cursor model = case dbModelFocusedField model of
  Nothing -> 0
  Just path -> clamp 0 (fieldTextLength path (dbModelEditBuffer model)) cursor

fieldTextLength :: Text -> DataBrowserEditBuffer -> Int
fieldTextLength path (DataBrowserEditBuffer values) = case Map.lookup path values of
  Just (String text) -> Text.length text
  _ -> 0

steppedNumber :: Maybe Value -> Int -> Scientific
steppedNumber current delta = case current of
  Just (Number n) -> n + fromIntegral delta
  Just (String text) -> case readDouble text of
    Just value -> fromFloatDigits (value + fromIntegral delta)
    Nothing -> fromIntegral delta
  _ -> fromIntegral delta

cycleEnumField :: Text -> Int -> DataBrowserModel -> DataBrowserModel
cycleEnumField path direction model = case dbSelectionSchema (dbModelSelection model) of
  Nothing -> model
  Just schema -> case lookupDataBrowserFieldType path (drsFields schema) of
    Just (DFEnum options) | not (null options) ->
      let DataBrowserEditBuffer values = dbModelEditBuffer model
          currentText = case Map.lookup path values of
            Just (String text) -> Just text
            _ -> Nothing
          currentIndex = currentText >>= \text -> findIndex (== text) options
          optionCount = length options
          nextIndex = case currentIndex of
            Just idx -> (idx + direction) `mod` optionCount
            Nothing
              | direction < 0 -> optionCount - 1
              | otherwise -> 0
      in updateEditValue path (String (options !! nextIndex)) model
    _ -> model

valueMatchesField :: DataFieldType -> Value -> Bool
valueMatchesField fieldType value = case fieldType of
  DFText -> case value of
    String _ -> True
    _ -> False
  DFInt -> case value of
    Number n -> isInteger n
    _ -> False
  DFFloat -> isNumber value
  DFDouble -> isNumber value
  DFFixed2 -> isNumber value
  DFFixed3 -> isNumber value
  DFFixed4 -> isNumber value
  DFBool -> case value of
    Bool _ -> True
    _ -> False
  DFEnum options -> case value of
    String text -> text `elem` options
    _ -> False
  DFRecord _ -> case value of
    Object _ -> True
    _ -> False
  DFAdt _ -> case value of
    Object _ -> True
    _ -> False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

lookupDataBrowserField :: Text -> [DataFieldDef] -> Maybe DataFieldDef
lookupDataBrowserField path fields = case Text.splitOn "." path of
  [] -> Nothing
  (segment:rest) -> findField segment fields >>= resolve rest
  where
    findField name = find ((== name) . dfName)

    resolve [] field = Just field
    resolve (segment:rest) field = case dfType field of
      DFRecord nestedFields -> findField segment nestedFields >>= resolve rest
      DFAdt constructors -> do
        constructor <- find ((== segment) . dcdName) constructors
        resolveConstructor field rest constructor
      _ -> Nothing

    resolveConstructor _ [] _ = Nothing
    resolveConstructor parent (indexText:rest) constructor = do
      index <- readInt indexText
      fieldType <- nth index (dcdFields constructor)
      let field = DataFieldDef
            { dfName = indexText
            , dfType = fieldType
            , dfLabel = indexText
            , dfEditable = dfEditable parent
            , dfDefault = Nothing
            }
      resolve rest field

defaultValues :: DataResourceSchema -> Map Text Value
defaultValues schema = Map.fromList (concatMap (defaultField "") (drsFields schema))

editableRecordValues :: DataResourceSchema -> DataRecord -> Map Text Value
editableRecordValues schema (DataRecord values) =
  Map.fromList (concatMap (flattenRecordField "" values) (drsFields schema))

-- | Build the mutation payload from dot-path edit values.
editBufferRecord :: DataResourceSchema -> Maybe DataRecord -> DataBrowserEditBuffer -> DataRecord
editBufferRecord schema mBaseRecord (DataBrowserEditBuffer values) =
  let baseValues = maybe Map.empty unDataRecord mBaseRecord
  in DataRecord (Map.fromList (concatMap (hydrateTopField values baseValues "") (drsFields schema)))

defaultField :: Text -> DataFieldDef -> [(Text, Value)]
defaultField prefix field =
  let path = qualifyPath prefix (dfName field)
  in case dfDefault field of
    Just value -> flattenFieldValue path (dfEditable field) (dfType field) value
    Nothing -> case dfType field of
      DFRecord nestedFields -> concatMap (defaultField path) nestedFields
      _ -> []

flattenRecordField :: Text -> Map Text Value -> DataFieldDef -> [(Text, Value)]
flattenRecordField prefix values field =
  let path = qualifyPath prefix (dfName field)
  in case Map.lookup (dfName field) values of
    Nothing -> []
    Just value -> flattenFieldValue path (dfEditable field) (dfType field) value

flattenFieldValue :: Text -> Bool -> DataFieldType -> Value -> [(Text, Value)]
flattenFieldValue path editable fieldType value = case fieldType of
  DFRecord nestedFields -> case value of
    Object objectValue -> concatMap (flattenObjectField path objectValue) nestedFields
    _ -> []
  DFAdt constructors -> flattenAdtValue path editable constructors value
  _
    | editable -> [(path, value)]
    | otherwise -> []

flattenObjectField :: Text -> AesonKM.KeyMap Value -> DataFieldDef -> [(Text, Value)]
flattenObjectField prefix objectValue field =
  let path = qualifyPath prefix (dfName field)
  in case AesonKM.lookup (AesonKey.fromText (dfName field)) objectValue of
    Nothing -> []
    Just value -> flattenFieldValue path (dfEditable field) (dfType field) value

flattenAdtValue :: Text -> Bool -> [DataConstructorDef] -> Value -> [(Text, Value)]
flattenAdtValue path editable constructors (Object objectValue) =
  case (AesonKM.lookup "constructor" objectValue, AesonKM.lookup "fields" objectValue) of
    (Just (String constructorName), Just (Array fieldValues)) ->
      case find ((== constructorName) . dcdName) constructors of
        Nothing -> []
        Just constructorDef -> concat
          [ flattenFieldValue
              (path <> "." <> constructorName <> "." <> Text.pack (show index))
              editable
              fieldType
              fieldValue
          | (index, fieldType, fieldValue) <- zip3 [(0 :: Int)..] (dcdFields constructorDef) (Vector.toList fieldValues)
          ]
    _ -> []
flattenAdtValue _ _ _ _ = []

hydrateTopField :: Map Text Value -> Map Text Value -> Text -> DataFieldDef -> [(Text, Value)]
hydrateTopField values baseValues prefix field =
  let path = qualifyPath prefix (dfName field)
      baseValue = Map.lookup (dfName field) baseValues
  in case hydrateFieldValue values baseValue path (dfType field) of
    Nothing -> []
    Just value -> [(dfName field, value)]

hydrateFieldValue :: Map Text Value -> Maybe Value -> Text -> DataFieldType -> Maybe Value
hydrateFieldValue values baseValue path fieldType = case fieldType of
  DFRecord nestedFields
    | hasEditAt path values ->
        let objectValue = case baseValue of
              Just (Object km) -> Just km
              _ -> Nothing
            objectPairs = concatMap (hydrateObjectField values objectValue path) nestedFields
        in if null objectPairs
           then Nothing
           else Just (Object (AesonKM.fromList objectPairs))
    | otherwise -> Nothing
  DFAdt constructors
    | hasEditAt path values -> hydrateAdtField values baseValue path constructors
    | otherwise -> Nothing
  _ -> Map.lookup path values

hydrateObjectField :: Map Text Value -> Maybe (AesonKM.KeyMap Value) -> Text -> DataFieldDef -> [(AesonKey.Key, Value)]
hydrateObjectField values mBaseObject prefix field =
  let path = qualifyPath prefix (dfName field)
      baseValue = mBaseObject >>= AesonKM.lookup (AesonKey.fromText (dfName field))
  in case hydrateFieldValue values baseValue path (dfType field) of
    Just value -> [(AesonKey.fromText (dfName field), value)]
    Nothing -> case baseValue of
      Just value
        | hasEditAt prefix values -> [(AesonKey.fromText (dfName field), value)]
      _ -> []

hydrateAdtField :: Map Text Value -> Maybe Value -> Text -> [DataConstructorDef] -> Maybe Value
hydrateAdtField values baseValue path constructors = do
  constructorDef <- find (constructorHasValues values path) constructors
  let constructorName = dcdName constructorDef
      baseFields = baseAdtFields constructorName baseValue
      fieldValues =
        [ hydrateAdtPosition values baseFields (path <> "." <> constructorName <> "." <> Text.pack (show index)) index fieldType
        | (index, fieldType) <- zip [(0 :: Int)..] (dcdFields constructorDef)
        ]
  pure $ Object $ AesonKM.fromList
    [ (AesonKey.fromText "constructor", String constructorName)
    , (AesonKey.fromText "fields", Array (Vector.fromList fieldValues))
    ]

hydrateAdtPosition :: Map Text Value -> Maybe [Value] -> Text -> Int -> DataFieldType -> Value
hydrateAdtPosition values mBaseFields path index fieldType =
  let baseValue = mBaseFields >>= nth index
  in case hydrateFieldValue values baseValue path fieldType of
    Just value -> value
    Nothing -> maybe Null id baseValue

baseAdtFields :: Text -> Maybe Value -> Maybe [Value]
baseAdtFields constructorName (Just (Object objectValue)) =
  case (AesonKM.lookup "constructor" objectValue, AesonKM.lookup "fields" objectValue) of
    (Just (String baseConstructor), Just (Array fieldValues))
      | baseConstructor == constructorName -> Just (Vector.toList fieldValues)
    _ -> Nothing
baseAdtFields _ _ = Nothing

constructorHasValues :: Map Text Value -> Text -> DataConstructorDef -> Bool
constructorHasValues values path constructorDef =
  let prefix = path <> "." <> dcdName constructorDef <> "."
  in any (Text.isPrefixOf prefix) (Map.keys values)

hasEditAt :: Text -> Map Text Value -> Bool
hasEditAt path values =
  let prefix = path <> "."
  in Map.member path values || any (Text.isPrefixOf prefix) (Map.keys values)

qualifyPath :: Text -> Text -> Text
qualifyPath prefix name
  | Text.null prefix = name
  | otherwise = prefix <> "." <> name

replaceRecord :: Text -> Value -> Maybe Int -> DataRecord -> [DataRecord] -> [DataRecord]
replaceRecord keyField keyValue fallbackIndex updatedRecord records =
  let byKey = map (\record -> if recordKeyMatches keyField keyValue record then updatedRecord else record) records
  in if byKey /= records
     then byKey
     else maybe records (\index -> replaceAt index updatedRecord records) fallbackIndex

deleteRecord :: Text -> Value -> Maybe Int -> [DataRecord] -> [DataRecord]
deleteRecord keyField keyValue fallbackIndex records =
  let byKey = filter (not . recordKeyMatches keyField keyValue) records
  in if length byKey /= length records
     then byKey
     else maybe records (`removeAt` records) fallbackIndex

mergeSubmittedRecord :: Text -> Value -> DataBrowserSelection -> DataRecord -> [DataRecord] -> DataRecord
mergeSubmittedRecord keyField keyValue selection (DataRecord submittedValues) records =
  let DataRecord baseValues = baseRecord
  in DataRecord (Map.union submittedValues baseValues)
  where
    baseRecord = case find (recordKeyMatches keyField keyValue) records of
      Just record -> record
      Nothing -> case dbSelectionRecord selection of
        Just record -> record
        Nothing -> DataRecord Map.empty

recordKeyMatches :: Text -> Value -> DataRecord -> Bool
recordKeyMatches keyField keyValue (DataRecord values) =
  Map.lookup keyField values == Just keyValue

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value values
  | index < 0 = values
  | otherwise = go index values
  where
    go 0 (_:rest) = value : rest
    go n (x:rest) = x : go (n - 1) rest
    go _ [] = []

removeAt :: Int -> [a] -> [a]
removeAt index values
  | index < 0 = values
  | otherwise = go index values
  where
    go 0 (_:rest) = rest
    go n (x:rest) = x : go (n - 1) rest
    go _ [] = []

incrementTotal :: DataBrowserPagination -> DataBrowserPagination
incrementTotal pagination = pagination
  { dbPaginationTotalCount = fmap (+ 1) (dbPaginationTotalCount pagination) }

decrementTotal :: DataBrowserPagination -> DataBrowserPagination
decrementTotal pagination = pagination
  { dbPaginationTotalCount = fmap (max 0 . subtract 1) (dbPaginationTotalCount pagination) }

recordAt :: Int -> [DataRecord] -> Maybe DataRecord
recordAt index records
  | index < 0 = Nothing
  | otherwise = case drop index records of
      record:_ -> Just record
      [] -> Nothing

pageSizeFrom :: DataPagination -> Int
pageSizeFrom pagination =
  max 1 (min (max 1 (dpMaxPageSize pagination)) (dpDefaultPageSize pagination))

validationError :: Maybe Text -> Text -> DataBrowserValidationError
validationError = DataBrowserValidationError

readDouble :: Text -> Maybe Double
readDouble text = case reads (Text.unpack text) of
  [(value, "")] -> Just value
  _ -> Nothing

readInt :: Text -> Maybe Int
readInt text = case reads (Text.unpack text) of
  [(value, "")] | value >= 0 -> Just value
  _ -> Nothing

nth :: Int -> [a] -> Maybe a
nth index values
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value:_ -> Just value
      [] -> Nothing

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
