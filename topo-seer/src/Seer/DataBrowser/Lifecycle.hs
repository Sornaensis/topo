{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Ui-owner side Data Browser action lifecycle. This module is pure: service
-- IO belongs in 'Seer.DataBrowser.AppService' and worker supervision belongs in
-- 'Seer.DataBrowser.Executor'.
module Seer.DataBrowser.Lifecycle
  ( DataBrowserAppAction(..)
  , beginDataBrowserAction
  ) where

import Data.Aeson (Value)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.DataBrowser.Model hiding (DataBrowserAction(..))
import qualified Seer.DataBrowser.Model as Model
import Topo.Plugin.DataResource (DataResourceSchema(..))

-- | UI-level intent accepted atomically by the Ui actor.
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
  | DataBrowserInsertText !Text
  | DataBrowserReplaceText !Text
  | DataBrowserBackspace
  | DataBrowserDeleteText
  | DataBrowserSetTextCursor !Int
  | DataBrowserStepNumberField !Text !Int
  | DataBrowserToggleBoolField !Text
  | DataBrowserCycleEnumField !Text !Int
  | DataBrowserCreateRecord
  | DataBrowserUpdateRecord
  | DataBrowserRequestDelete
  | DataBrowserCancelDelete
  | DataBrowserDeleteRecord
  deriving (Eq, Show)

-- | Begin an action against the latest owner state. The supplied ID is only
-- consumed when the result is 'DataBrowserBeginAccepted'.
beginDataBrowserAction
  :: DataBrowserRequestId
  -> DataBrowserUi
  -> DataBrowserAppAction
  -> (DataBrowserUi, DataBrowserBeginResult)
beginDataBrowserAction requestId ui action =
  case dbModelPendingEnvelope model of
    Just pending
      | dataBrowserRequestIsMutation (dbpeRequest pending) -> reject "data browser mutation in progress" model
      | not (replacementAction action) -> reject "data browser request in progress" model
    _ -> beginAllowed
  where
    model = dbuModel ui
    superseded = dbpeRequestId <$> dbModelPendingEnvelope model

    beginAllowed = case action of
      DataBrowserLoadPlugins -> accept DataBrowserLoadCatalogRequest model
      DataBrowserSelectPlugin pluginName ->
        accept (DataBrowserLoadPluginRequest pluginName) $
          dataBrowserReducer model (Model.DataBrowserSelectPlugin pluginName)
      DataBrowserSelectResource pluginName resourceName ->
        case resourceSchemaFor (dbuResources ui) pluginName resourceName of
          Nothing ->
            let selected = emptyDataBrowserModel
                  { dbModelSelection = emptyDataBrowserSelection
                      { dbSelectionPlugin = Just pluginName
                      , dbSelectionResource = Just resourceName
                      }
                  }
            in accept (DataBrowserSelectResourceRequest pluginName resourceName) selected
          Just schema ->
            fromReducer model (dataBrowserReducer model (Model.DataBrowserSelectResource pluginName schema))
      DataBrowserQueryRecords pageAction ->
        fromReducer model (dataBrowserReducer model (Model.DataBrowserPage pageAction))
      DataBrowserCreateRecord -> savedMutation DataBrowserCreateOperation isCreateRequest
      DataBrowserUpdateRecord -> savedMutation DataBrowserUpdateOperation isUpdateRequest
      DataBrowserDeleteRecord -> fromReducer model (dataBrowserReducer model Model.DataBrowserConfirmDelete)
      _ -> pureResult (applyPureAction model action)

    savedMutation operation expected =
      let next = dataBrowserReducer model Model.DataBrowserSave
      in case dbModelPendingRequest next of
        Just descriptor | expected descriptor -> acceptWith operation (DataBrowserRecordRequest descriptor) next
        _ -> pureResult next

    fromReducer previous next = case dbModelPendingRequest next of
      Just descriptor -> accept (DataBrowserRecordRequest descriptor) next
      Nothing
        | Just _ <- dbModelPendingEnvelope previous ->
            reject (validationMessage next "data browser replacement unavailable") previous
        | next == previous -> reject (validationMessage next "data browser action unavailable") next
        | otherwise -> pureResult next

    accept request next = acceptWith (dataBrowserRequestOperation request) request next
    acceptWith operation request next =
      let envelope = DataBrowserPendingEnvelope requestId operation request
          next' = next
            { dbModelPendingRequest = Nothing
            , dbModelPendingEnvelope = Just envelope
            , dbModelAsyncError = Nothing
            , dbModelLoading = True
            }
      in (ui { dbuModel = next' }, DataBrowserBeginAccepted envelope superseded)

    pureResult next =
      let next' = next
            { dbModelPendingEnvelope = Nothing
            , dbModelAsyncError = Nothing
            , dbModelLoading = False
            }
      in case dbModelValidationErrors next' of
        [] -> (ui { dbuModel = next' }, DataBrowserBeginPure)
        _ -> reject (validationMessage next' "data browser action rejected") next'

    reject message next = (ui { dbuModel = next }, DataBrowserBeginRejected message)

replacementAction :: DataBrowserAppAction -> Bool
replacementAction DataBrowserSelectPlugin {} = True
replacementAction DataBrowserSelectResource {} = True
replacementAction _ = False

applyPureAction :: DataBrowserModel -> DataBrowserAppAction -> DataBrowserModel
applyPureAction model action = case action of
  DataBrowserSelectRecord row -> dataBrowserReducer model (Model.DataBrowserSelectRecord row)
  DataBrowserDismissRecord -> dataBrowserReducer model Model.DataBrowserDismissRecord
  DataBrowserToggleField path -> dataBrowserReducer model (Model.DataBrowserToggleField path)
  DataBrowserStartEdit -> dataBrowserReducer model Model.DataBrowserStartEdit
  DataBrowserStartCreate -> dataBrowserReducer model Model.DataBrowserStartCreate
  DataBrowserCancelEdit -> dataBrowserReducer model Model.DataBrowserCancelEdit
  DataBrowserFocusField path -> dataBrowserReducer model (Model.DataBrowserFocusField path)
  DataBrowserBlurField -> dataBrowserReducer model Model.DataBrowserBlurField
  DataBrowserSetFieldValue path value -> dataBrowserReducer model (Model.DataBrowserSetFieldValue path value)
  DataBrowserInsertText text -> dataBrowserReducer model (Model.DataBrowserInsertText text)
  DataBrowserReplaceText text -> dataBrowserReducer model (Model.DataBrowserReplaceText text)
  DataBrowserBackspace -> dataBrowserReducer model Model.DataBrowserBackspace
  DataBrowserDeleteText -> dataBrowserReducer model Model.DataBrowserDeleteText
  DataBrowserSetTextCursor cursor -> dataBrowserReducer model (Model.DataBrowserSetTextCursor cursor)
  DataBrowserStepNumberField path delta -> dataBrowserReducer model (Model.DataBrowserStepNumberField path delta)
  DataBrowserToggleBoolField path -> dataBrowserReducer model (Model.DataBrowserToggleBoolField path)
  DataBrowserCycleEnumField path direction -> dataBrowserReducer model (Model.DataBrowserCycleEnumField path direction)
  DataBrowserRequestDelete -> dataBrowserReducer model Model.DataBrowserRequestDelete
  DataBrowserCancelDelete -> dataBrowserReducer model Model.DataBrowserCancelDelete
  _ -> model

resourceSchemaFor :: Map.Map Text [DataResourceSchema] -> Text -> Text -> Maybe DataResourceSchema
resourceSchemaFor resources pluginName resourceName = do
  schemas <- Map.lookup pluginName resources
  find ((== resourceName) . drsName) schemas

isCreateRequest :: DataBrowserPendingRequest -> Bool
isCreateRequest DataBrowserCreateRecordRequest {} = True
isCreateRequest _ = False

isUpdateRequest :: DataBrowserPendingRequest -> Bool
isUpdateRequest DataBrowserUpdateRecordRequest {} = True
isUpdateRequest _ = False

validationMessage :: DataBrowserModel -> Text -> Text
validationMessage model fallback = case dbModelValidationErrors model of
  [] -> fallback
  errors -> Text.intercalate "; " (map format errors)
  where
    format err = maybe "" (<> ": ") (dbValidationField err) <> dbValidationMessage err
