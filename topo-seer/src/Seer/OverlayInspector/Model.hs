{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure state and guarded completion logic for the overlay inspector.
module Seer.OverlayInspector.Model
  ( OverlayInspectorOperation(..)
  , overlayInspectorOperationText
  , OverlayInspectorAction(..)
  , OverlayInspectorView(..)
  , OverlayInspectorFocus(..)
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
  , openOverlayInspectorView
  , closeOverlayInspectorView
  , setOverlayInspectorFocus
  , setOverlayInspectorScroll
  , moveOverlayInspectorSelection
  , setOverlayInspectorImportText
  , prepareOverlayInspectorValidation
  , applyOverlayInspectorValidationPreparation
  , setOverlayInspectorNotice
  , overlayInspectorPayloadText
  , overlayInspectorLoading
  , overlayInspectorPendingValue
  , overlayInspectorAsyncErrorValue
  , overlayInspectorModelValue
  ) where

import Data.Aeson (Value(..), eitherDecodeStrict', encode, object, (.=), (.:?))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
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

-- | Observable presentation selected by one of the retained overlay widgets.
data OverlayInspectorView
  = OverlayInspectorManagerView
  | OverlayInspectorSchemaView
  | OverlayInspectorProvenanceView
  | OverlayInspectorExportView
  | OverlayInspectorImportView
  deriving (Eq, Show)

-- | Keyboard focus inside the modal. Manager rows carry their stable index.
data OverlayInspectorFocus
  = OverlayInspectorCloseFocus
  | OverlayInspectorManagerFocus !Int
  | OverlayInspectorCopyFocus
  | OverlayInspectorSaveFocus
  | OverlayInspectorImportInputFocus
  | OverlayInspectorValidateFocus
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
  , oimView :: !(Maybe OverlayInspectorView)
  , oimFocus :: !OverlayInspectorFocus
  , oimScroll :: !Int
  , oimImportText :: !Text
  , oimImportCursor :: !Int
  , oimLocalDiagnostics :: ![Text]
  , oimNotice :: !(Maybe Text)
  } deriving (Eq, Show)

emptyOverlayInspectorModel :: OverlayInspectorModel
emptyOverlayInspectorModel = OverlayInspectorModel
  { oimOverlayNames = []
  , oimSelectedOverlay = Nothing
  , oimManagerPayload = Nothing
  , oimSchemaPayload = Nothing
  , oimProvenancePayload = Nothing
  , oimExportPayload = Nothing
  , oimImportDraft = defaultDraft
  , oimImportValidation = Nothing
  , oimValidationDiagnostics = []
  , oimPending = Nothing
  , oimAsyncError = Nothing
  , oimView = Nothing
  , oimFocus = OverlayInspectorCloseFocus
  , oimScroll = 0
  , oimImportText = overlayInspectorPayloadText defaultDraft
  , oimImportCursor = Text.length (overlayInspectorPayloadText defaultDraft)
  , oimLocalDiagnostics = []
  , oimNotice = Nothing
  }
  where
    defaultDraft = object
      [ "schema" .= object []
      , "payload" .= object []
      ]

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
selectOverlayInspectorOverlay requested model
  | managerRefreshPending model = model
  | otherwise =
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

openOverlayInspectorView :: OverlayInspectorView -> OverlayInspectorModel -> OverlayInspectorModel
openOverlayInspectorView view model = (clearStalePayload view model)
  { oimView = Just view
  , oimFocus = initialFocus view
  , oimScroll = 0
  , oimLocalDiagnostics = []
  , oimNotice = Nothing
  }
  where
    initialFocus OverlayInspectorManagerView = OverlayInspectorManagerFocus 0
    initialFocus OverlayInspectorExportView = OverlayInspectorCopyFocus
    initialFocus OverlayInspectorImportView = OverlayInspectorImportInputFocus
    initialFocus _ = OverlayInspectorCloseFocus
    clearStalePayload OverlayInspectorManagerView current = current { oimManagerPayload = Nothing }
    clearStalePayload OverlayInspectorSchemaView current = current { oimSchemaPayload = Nothing }
    clearStalePayload OverlayInspectorProvenanceView current = current { oimProvenancePayload = Nothing }
    clearStalePayload OverlayInspectorExportView current = current { oimExportPayload = Nothing }
    clearStalePayload OverlayInspectorImportView current = current

closeOverlayInspectorView :: OverlayInspectorModel -> OverlayInspectorModel
closeOverlayInspectorView model = model
  { oimView = Nothing
  , oimFocus = OverlayInspectorCloseFocus
  , oimScroll = 0
  , oimNotice = Nothing
  }

setOverlayInspectorFocus :: OverlayInspectorFocus -> OverlayInspectorModel -> OverlayInspectorModel
setOverlayInspectorFocus focus model = model { oimFocus = focus }

setOverlayInspectorScroll :: Int -> OverlayInspectorModel -> OverlayInspectorModel
setOverlayInspectorScroll scroll model = model { oimScroll = max 0 scroll }

moveOverlayInspectorSelection :: Int -> OverlayInspectorModel -> OverlayInspectorModel
moveOverlayInspectorSelection delta model
  | managerRefreshPending model = model
  | otherwise =
      case oimOverlayNames model of
        [] -> model { oimFocus = OverlayInspectorManagerFocus 0 }
        names ->
          let current = case oimSelectedOverlay model of
                Just selected -> maybe 0 id (indexOf selected names)
                Nothing -> 0
              nextIndex = max 0 (min (length names - 1) (current + delta))
              next = selectOverlayInspectorOverlay (Just (names !! nextIndex)) model
          in next
            { oimFocus = OverlayInspectorManagerFocus nextIndex
            , oimScroll = nextIndex
            }
  where
    indexOf target = go 0
      where
        go _ [] = Nothing
        go index (value:rest)
          | value == target = Just index
          | otherwise = go (index + 1) rest

managerRefreshPending :: OverlayInspectorModel -> Bool
managerRefreshPending model = case oimPending model of
  Just OverlayInspectorPending{ oipRequest = OverlayInspectorManagerRequest } -> True
  _ -> False

setOverlayInspectorImportText :: Text -> Int -> OverlayInspectorModel -> OverlayInspectorModel
setOverlayInspectorImportText input cursor model = model
  { oimImportText = input
  , oimImportCursor = max 0 (min (Text.length input) cursor)
  , oimImportValidation = Nothing
  , oimValidationDiagnostics = []
  , oimLocalDiagnostics = []
  , oimPending = case oimPending model of
      Just OverlayInspectorPending{ oipRequest = OverlayInspectorImportValidationRequest _ } -> Nothing
      other -> other
  , oimAsyncError = Nothing
  , oimNotice = Nothing
  }

-- | Parse the raw editor text without adopting data. A successful parse only
-- replaces the draft that the validation AppService operation will inspect.
prepareOverlayInspectorValidation
  :: OverlayInspectorModel
  -> (OverlayInspectorModel, Either Text Value)
prepareOverlayInspectorValidation model =
  let parsed = case eitherDecodeStrict' (TextEncoding.encodeUtf8 (oimImportText model)) of
        Left parseError -> Left ("Invalid JSON: " <> Text.pack parseError)
        Right draft -> Right draft
  in (applyOverlayInspectorValidationPreparation parsed model, parsed)

applyOverlayInspectorValidationPreparation
  :: Either Text Value
  -> OverlayInspectorModel
  -> OverlayInspectorModel
applyOverlayInspectorValidationPreparation parsed model = case parsed of
  Left message -> model
    { oimLocalDiagnostics = [message]
    , oimImportValidation = Nothing
    , oimValidationDiagnostics = []
    , oimNotice = Nothing
    }
  Right draft -> (setOverlayInspectorImportDraft draft model)
    { oimLocalDiagnostics = []
    , oimNotice = Just "JSON parsed; validating only — no data will be adopted."
    }

setOverlayInspectorNotice :: Maybe Text -> OverlayInspectorModel -> OverlayInspectorModel
setOverlayInspectorNotice notice model = model { oimNotice = notice }

overlayInspectorPayloadText :: Value -> Text
overlayInspectorPayloadText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . encode

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
  , "view" .= fmap overlayInspectorViewText (oimView model)
  , "focus" .= overlayInspectorFocusText (oimFocus model)
  , "scroll" .= oimScroll model
  , "import_text" .= oimImportText model
  , "import_cursor" .= oimImportCursor model
  , "local_diagnostics" .= oimLocalDiagnostics model
  , "notice" .= oimNotice model
  ]


overlayInspectorViewText :: OverlayInspectorView -> Text
overlayInspectorViewText view = case view of
  OverlayInspectorManagerView -> "manager"
  OverlayInspectorSchemaView -> "schema"
  OverlayInspectorProvenanceView -> "provenance"
  OverlayInspectorExportView -> "export"
  OverlayInspectorImportView -> "import_validation"

overlayInspectorFocusText :: OverlayInspectorFocus -> Text
overlayInspectorFocusText focus = case focus of
  OverlayInspectorCloseFocus -> "close"
  OverlayInspectorManagerFocus index -> "manager:" <> Text.pack (show index)
  OverlayInspectorCopyFocus -> "copy"
  OverlayInspectorSaveFocus -> "save"
  OverlayInspectorImportInputFocus -> "import_input"
  OverlayInspectorValidateFocus -> "validate"

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
        selectedIndex = selected >>= (`indexIn` names)
        focus = maybe OverlayInspectorCloseFocus OverlayInspectorManagerFocus selectedIndex
        scroll = maybe 0 id selectedIndex
    in reconciled
      { oimOverlayNames = names
      , oimSelectedOverlay = selected
      , oimManagerPayload = Just payload
      , oimFocus = if oimView model == Just OverlayInspectorManagerView then focus else oimFocus reconciled
      , oimScroll = if oimView model == Just OverlayInspectorManagerView then scroll else oimScroll reconciled
      }
  OverlayInspectorSchemaRequest _ -> model { oimSchemaPayload = Just payload }
  OverlayInspectorProvenanceRequest _ -> model { oimProvenancePayload = Just payload }
  OverlayInspectorExportRequest _ -> model { oimExportPayload = Just payload }
  OverlayInspectorImportValidationRequest _ -> model
    { oimImportValidation = Just payload
    , oimValidationDiagnostics = validationDiagnostics payload
    }

indexIn :: Eq a => a -> [a] -> Maybe Int
indexIn target = go 0
  where
    go _ [] = Nothing
    go index (value:rest)
      | target == value = Just index
      | otherwise = go (index + 1) rest

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
