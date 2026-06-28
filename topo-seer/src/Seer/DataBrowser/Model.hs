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
  , DataBrowserPendingRequest(..)
  , DataBrowserPageAction(..)
  , DataBrowserAction(..)
  , emptyDataBrowserModel
  , emptyDataBrowserSelection
  , emptyDataBrowserPagination
  , emptyDataBrowserEditBuffer
  , dataBrowserReducer
  , validateEditBuffer
  , dataBrowserCanList
  , dataBrowserCanCreate
  , dataBrowserCanUpdate
  , dataBrowserCanDelete
  , dataBrowserCanPage
  , dataBrowserPageRequestFor
  , dataBrowserPageStep
  , lookupDataBrowserFieldType
  ) where

import Data.Aeson (Value(..))
import Data.List (find, findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, isInteger)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
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

-- | Effect descriptor produced by the pure reducer for integration layers.
data DataBrowserPendingRequest
  = DataBrowserListRecordsRequest !Text !Text !(Maybe Int) !(Maybe Int)
  | DataBrowserCreateRecordRequest !Text !Text !DataRecord
  | DataBrowserUpdateRecordRequest !Text !Text !Value !DataRecord
  | DataBrowserDeleteRecordRequest !Text !Text !Value
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
  | DataBrowserBackspace
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
  , dbModelLoading :: !Bool
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

  DataBrowserBackspace ->
    editFocusedText deleteBeforeCursor model

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
        dummyRecord = DataRecord defaults
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
saveCreate selection model = case (dbSelectionPlugin selection, dbSelectionResource selection) of
  (Just pluginName, Just resourceName) ->
    let record = DataRecord (dbEditBufferValues (dbModelEditBuffer model))
        request = DataBrowserCreateRecordRequest pluginName resourceName record
    in model
      { dbModelPendingRequest = Just request
      , dbModelLoading = True
      , dbModelValidationErrors = []
      }
  _ -> model { dbModelValidationErrors = [validationError Nothing "no resource selected"] }

saveUpdate :: DataBrowserSelection -> DataBrowserModel -> DataBrowserModel
saveUpdate selection model =
  case (dbSelectionPlugin selection, dbSelectionResource selection, dbSelectionRecordKey selection) of
    (Just pluginName, Just resourceName, Just keyValue) ->
      let record = DataRecord (dbEditBufferValues (dbModelEditBuffer model))
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
      errors = filter ((/= Just path) . dbValidationField) (dbModelValidationErrors model)
  in model
    { dbModelEditBuffer = DataBrowserEditBuffer (Map.insert path value values)
    , dbModelValidationErrors = errors
    }

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
defaultValues schema = Map.fromList
  [ (dfName field, value)
  | field <- drsFields schema
  , dfEditable field
  , Just value <- [dfDefault field]
  ]

editableRecordValues :: DataResourceSchema -> DataRecord -> Map Text Value
editableRecordValues schema (DataRecord values) =
  Map.filterWithKey (\path _ -> maybe False dfEditable (lookupDataBrowserField path (drsFields schema))) values

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
