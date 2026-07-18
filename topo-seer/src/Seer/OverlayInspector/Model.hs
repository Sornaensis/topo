{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure state and guarded completion logic for the overlay inspector.
module Seer.OverlayInspector.Model
  ( OverlayInspectorOperation(..)
  , overlayInspectorOperationText
  , OverlayInspectorAction(..)
  , OverlayInspectorRequestId(..)
  , OverlayInspectorRequest(..)
  , OverlayInspectorPending(..)
  , OverlayInspectorAsyncError(..)
  , OverlayInspectorWorkerOutcome(..)
  , OverlayInspectorCompletion(..)
  , OverlayInspectorBeginResult(..)
  , OverlayInspectorModel(..)
  , emptyOverlayInspectorModel
  , beginOverlayInspectorAction
  , completeOverlayInspectorRequest
  , selectOverlayInspectorOverlay
  , setOverlayInspectorImportDraft
  , overlayInspectorLoading
  , overlayInspectorPendingValue
  , overlayInspectorAsyncErrorValue
  , overlayInspectorModelValue
  ) where

import Data.Aeson (Value(..), object, (.=), (.:?))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Data.Word (Word64)

-- | Stable action names shared by widget responses and inspector state.
data OverlayInspectorOperation
  = OverlayInspectorLoadManager
  | OverlayInspectorLoadSchema
  | OverlayInspectorLoadProvenance
  | OverlayInspectorExport
  | OverlayInspectorValidateImport
  deriving (Eq, Ord, Show)

overlayInspectorOperationText :: OverlayInspectorOperation -> Text
overlayInspectorOperationText operation = case operation of
  OverlayInspectorLoadManager -> "get_overlays"
  OverlayInspectorLoadSchema -> "get_overlay_schema"
  OverlayInspectorLoadProvenance -> "get_overlay_provenance"
  OverlayInspectorExport -> "export_overlay_data"
  OverlayInspectorValidateImport -> "validate_overlay_import"

data OverlayInspectorAction
  = OverlayInspectorRefreshManager
  | OverlayInspectorInspectSchema
  | OverlayInspectorInspectProvenance
  | OverlayInspectorExportSelected
  | OverlayInspectorValidateDraft
  deriving (Eq, Show)

newtype OverlayInspectorRequestId = OverlayInspectorRequestId
  { unOverlayInspectorRequestId :: Word64
  } deriving (Eq, Ord, Show)

-- | Exact service request captured when the Ui actor accepts an action.
data OverlayInspectorRequest
  = OverlayInspectorManagerRequest
  | OverlayInspectorSchemaRequest !Text
  | OverlayInspectorProvenanceRequest !Text
  | OverlayInspectorExportRequest !Text
  | OverlayInspectorImportValidationRequest !Value
  deriving (Eq, Show)

data OverlayInspectorPending = OverlayInspectorPending
  { oipRequestId :: !OverlayInspectorRequestId
  , oipOperation :: !OverlayInspectorOperation
  , oipRequest :: !OverlayInspectorRequest
  } deriving (Eq, Show)

-- | Structured AppService error payload retained without flattening it to text.
data OverlayInspectorAsyncError = OverlayInspectorAsyncError
  { oiaeRequestId :: !OverlayInspectorRequestId
  , oiaeOperation :: !OverlayInspectorOperation
  , oiaeRequest :: !OverlayInspectorRequest
  , oiaeError :: !Value
  } deriving (Eq, Show)

data OverlayInspectorWorkerOutcome
  = OverlayInspectorWorkerSucceeded !Value
  | OverlayInspectorWorkerFailed !Value
  deriving (Eq, Show)

data OverlayInspectorCompletion = OverlayInspectorCompletion
  { oicRequestId :: !OverlayInspectorRequestId
  , oicRequest :: !OverlayInspectorRequest
  , oicOutcome :: !OverlayInspectorWorkerOutcome
  } deriving (Eq, Show)

data OverlayInspectorBeginResult
  = OverlayInspectorBeginRejected !Text
  | OverlayInspectorBeginAccepted !OverlayInspectorPending
  deriving (Eq, Show)

-- | Renderer-readable overlay inspector model. Service responses are retained
-- verbatim so SDL and HTTP expose the same payload semantics.
data OverlayInspectorModel = OverlayInspectorModel
  { oimOverlayNames :: ![Text]
  , oimSelectedOverlay :: !(Maybe Text)
  , oimManagerPayload :: !(Maybe Value)
  , oimSchemaPayload :: !(Maybe Value)
  , oimProvenancePayload :: !(Maybe Value)
  , oimExportPayload :: !(Maybe Value)
  , oimImportDraft :: !Value
  , oimImportValidation :: !(Maybe Value)
  , oimValidationDiagnostics :: ![Value]
  , oimPending :: !(Maybe OverlayInspectorPending)
  , oimAsyncError :: !(Maybe OverlayInspectorAsyncError)
  } deriving (Eq, Show)

emptyOverlayInspectorModel :: OverlayInspectorModel
emptyOverlayInspectorModel = OverlayInspectorModel
  { oimOverlayNames = []
  , oimSelectedOverlay = Nothing
  , oimManagerPayload = Nothing
  , oimSchemaPayload = Nothing
  , oimProvenancePayload = Nothing
  , oimExportPayload = Nothing
  , oimImportDraft = object
      [ "schema" .= object []
      , "payload" .= object []
      ]
  , oimImportValidation = Nothing
  , oimValidationDiagnostics = []
  , oimPending = Nothing
  , oimAsyncError = Nothing
  }

beginOverlayInspectorAction
  :: OverlayInspectorRequestId
  -> OverlayInspectorAction
  -> OverlayInspectorModel
  -> (OverlayInspectorModel, OverlayInspectorBeginResult)
beginOverlayInspectorAction requestId action model = case oimPending model of
  Just _ -> (model, OverlayInspectorBeginRejected "an overlay inspector request is already pending")
  Nothing -> case requestForAction action model of
    Left message -> (model, OverlayInspectorBeginRejected message)
    Right request ->
      let operation = requestOperation request
          pending = OverlayInspectorPending requestId operation request
          next = model
            { oimPending = Just pending
            , oimAsyncError = Nothing
            }
      in (next, OverlayInspectorBeginAccepted pending)

completeOverlayInspectorRequest
  :: OverlayInspectorCompletion
  -> OverlayInspectorModel
  -> (OverlayInspectorModel, Bool)
completeOverlayInspectorRequest completion model = case oimPending model of
  Nothing -> (model, False)
  Just pending
    | oipRequestId pending /= oicRequestId completion -> (model, False)
    | oipRequest pending /= oicRequest completion -> (model, False)
    | not (requestMatchesCurrent (oipRequest pending) model) -> (model, False)
    | otherwise -> (applyOutcome pending (oicOutcome completion) model, True)

selectOverlayInspectorOverlay :: Maybe Text -> OverlayInspectorModel -> OverlayInspectorModel
selectOverlayInspectorOverlay requested model =
  let selected = case requested of
        Just name | name `elem` oimOverlayNames model -> Just name
        _ -> Nothing
  in if selected == oimSelectedOverlay model
       then model
       else clearSelectedPayloads model
          { oimSelectedOverlay = selected
          , oimPending = Nothing
          , oimAsyncError = Nothing
          }

setOverlayInspectorImportDraft :: Value -> OverlayInspectorModel -> OverlayInspectorModel
setOverlayInspectorImportDraft draft model = model
  { oimImportDraft = draft
  , oimImportValidation = Nothing
  , oimValidationDiagnostics = []
  , oimPending = case oimPending model of
      Just pending@OverlayInspectorPending{ oipRequest = OverlayInspectorImportValidationRequest _ } -> Nothing
      other -> other
  , oimAsyncError = Nothing
  }

overlayInspectorLoading :: OverlayInspectorModel -> Bool
overlayInspectorLoading = maybe False (const True) . oimPending

overlayInspectorPendingValue :: OverlayInspectorPending -> Value
overlayInspectorPendingValue pending = object
  [ "request_id" .= unOverlayInspectorRequestId (oipRequestId pending)
  , "operation" .= overlayInspectorOperationText (oipOperation pending)
  , "target" .= requestTargetValue (oipRequest pending)
  ]

overlayInspectorAsyncErrorValue :: OverlayInspectorAsyncError -> Value
overlayInspectorAsyncErrorValue asyncError = object
  [ "request_id" .= unOverlayInspectorRequestId (oiaeRequestId asyncError)
  , "operation" .= overlayInspectorOperationText (oiaeOperation asyncError)
  , "target" .= requestTargetValue (oiaeRequest asyncError)
  , "error" .= oiaeError asyncError
  ]

overlayInspectorModelValue :: OverlayInspectorModel -> Value
overlayInspectorModelValue model = object
  [ "overlay_names" .= oimOverlayNames model
  , "selected_overlay" .= oimSelectedOverlay model
  , "loading" .= overlayInspectorLoading model
  , "pending" .= fmap overlayInspectorPendingValue (oimPending model)
  , "async_error" .= fmap overlayInspectorAsyncErrorValue (oimAsyncError model)
  , "manager" .= oimManagerPayload model
  , "schema" .= oimSchemaPayload model
  , "provenance" .= oimProvenancePayload model
  , "export" .= oimExportPayload model
  , "import_draft" .= oimImportDraft model
  , "import_validation" .= oimImportValidation model
  , "validation_diagnostics" .= oimValidationDiagnostics model
  ]

requestForAction :: OverlayInspectorAction -> OverlayInspectorModel -> Either Text OverlayInspectorRequest
requestForAction action model = case action of
  OverlayInspectorRefreshManager -> Right OverlayInspectorManagerRequest
  OverlayInspectorInspectSchema -> OverlayInspectorSchemaRequest <$> requireSelection
  OverlayInspectorInspectProvenance -> OverlayInspectorProvenanceRequest <$> requireSelection
  OverlayInspectorExportSelected -> OverlayInspectorExportRequest <$> requireSelection
  OverlayInspectorValidateDraft -> Right (OverlayInspectorImportValidationRequest (oimImportDraft model))
  where
    requireSelection = maybe (Left "no overlay selected") Right (oimSelectedOverlay model)

requestOperation :: OverlayInspectorRequest -> OverlayInspectorOperation
requestOperation request = case request of
  OverlayInspectorManagerRequest -> OverlayInspectorLoadManager
  OverlayInspectorSchemaRequest _ -> OverlayInspectorLoadSchema
  OverlayInspectorProvenanceRequest _ -> OverlayInspectorLoadProvenance
  OverlayInspectorExportRequest _ -> OverlayInspectorExport
  OverlayInspectorImportValidationRequest _ -> OverlayInspectorValidateImport

requestMatchesCurrent :: OverlayInspectorRequest -> OverlayInspectorModel -> Bool
requestMatchesCurrent request model = case request of
  OverlayInspectorManagerRequest -> True
  OverlayInspectorSchemaRequest name -> oimSelectedOverlay model == Just name
  OverlayInspectorProvenanceRequest name -> oimSelectedOverlay model == Just name
  OverlayInspectorExportRequest name -> oimSelectedOverlay model == Just name
  OverlayInspectorImportValidationRequest draft -> oimImportDraft model == draft

applyOutcome
  :: OverlayInspectorPending
  -> OverlayInspectorWorkerOutcome
  -> OverlayInspectorModel
  -> OverlayInspectorModel
applyOutcome pending outcome model = case outcome of
  OverlayInspectorWorkerFailed errorValue -> model
    { oimPending = Nothing
    , oimAsyncError = Just OverlayInspectorAsyncError
        { oiaeRequestId = oipRequestId pending
        , oiaeOperation = oipOperation pending
        , oiaeRequest = oipRequest pending
        , oiaeError = errorValue
        }
    }
  OverlayInspectorWorkerSucceeded payload ->
    (applySuccess (oipRequest pending) payload model)
      { oimPending = Nothing
      , oimAsyncError = Nothing
      }

applySuccess :: OverlayInspectorRequest -> Value -> OverlayInspectorModel -> OverlayInspectorModel
applySuccess request payload model = case request of
  OverlayInspectorManagerRequest ->
    let (names, active) = managerSelection payload
        selected
          | Just activeName <- active, activeName `elem` names = Just activeName
          | Just current <- oimSelectedOverlay model, current `elem` names = Just current
          | firstName:_ <- names = Just firstName
          | otherwise = Nothing
        reconciled = if selected == oimSelectedOverlay model
          then model
          else clearSelectedPayloads model
    in reconciled
      { oimOverlayNames = names
      , oimSelectedOverlay = selected
      , oimManagerPayload = Just payload
      }
  OverlayInspectorSchemaRequest _ -> model { oimSchemaPayload = Just payload }
  OverlayInspectorProvenanceRequest _ -> model { oimProvenancePayload = Just payload }
  OverlayInspectorExportRequest _ -> model { oimExportPayload = Just payload }
  OverlayInspectorImportValidationRequest _ -> model
    { oimImportValidation = Just payload
    , oimValidationDiagnostics = validationDiagnostics payload
    }

clearSelectedPayloads :: OverlayInspectorModel -> OverlayInspectorModel
clearSelectedPayloads model = model
  { oimSchemaPayload = Nothing
  , oimProvenancePayload = Nothing
  , oimExportPayload = Nothing
  }

managerSelection :: Value -> ([Text], Maybe Text)
managerSelection value = case Aeson.parseMaybe parser value of
  Just parsed -> parsed
  Nothing -> ([], Nothing)
  where
    parser = Aeson.withObject "OverlayManagerResponse" $ \o ->
      (,) <$> (o .:? "overlay_names" >>= pure . maybe [] id)
          <*> o .:? "active_overlay"

validationDiagnostics :: Value -> [Value]
validationDiagnostics value = case Aeson.parseMaybe parser value of
  Just diagnostics -> diagnostics
  Nothing -> []
  where
    parser = Aeson.withObject "OverlayImportValidationResponse" $ \o ->
      o .:? "diagnostics" >>= pure . maybe [] id

requestTargetValue :: OverlayInspectorRequest -> Value
requestTargetValue request = case request of
  OverlayInspectorManagerRequest -> object []
  OverlayInspectorSchemaRequest name -> object ["overlay" .= name]
  OverlayInspectorProvenanceRequest name -> object ["overlay" .= name]
  OverlayInspectorExportRequest name -> object ["overlay" .= name]
  OverlayInspectorImportValidationRequest draft -> object ["draft" .= draft]
