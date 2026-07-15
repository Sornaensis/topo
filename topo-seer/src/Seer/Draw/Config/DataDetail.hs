{-# LANGUAGE OverloadedStrings #-}

-- | Focused rendering and view-model helpers for the data-browser record
-- detail popover and its delete confirmation dialog.
module Seer.Draw.Config.DataDetail
  ( DataDetailPopoverView(..)
  , DataDetailFieldRow(..)
  , DataDetailFieldControl(..)
  , DataDetailValidationRow(..)
  , DataDetailDeleteConfirmView(..)
  , dataDetailPopoverView
  , drawDataDetailPopover
  , enumerateVisibleFields
  , isNestable
  , lastSegment
  , lookupNestedValue
  , lookupFieldType
  , renderValue
  ) where

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , UiState(..)
  , dataBrowserMutationError
  , dataBrowserMutationPending
  , dataBrowserReadPending
  )
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Linear (V2(..))
import qualified SDL
import Seer.DataBrowser.Model
  ( DataBrowserAsyncError(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserValidationError(..)
  , dataBrowserOperationFailureText
  , dataBrowserOperationProgressText
  , dataBrowserRequestIsMutation
  )
import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataResourceSchema(..)
  )
import Topo.Plugin.RPC.DataService (DataRecord(..))
import UI.Font (FontCache, drawText)
import UI.Layout
  ( Layout
  , dataDetailCancelRect
  , dataDetailDeleteRect
  , dataDetailEditToggleRect
  , dataDetailFieldInputRect
  , dataDetailFieldRect
  , dataDetailFieldStepMinusRect
  , dataDetailFieldStepPlusRect
  , dataDetailPopoverRect
  , dataDetailSaveRect
  , deleteConfirmCancelRect
  , deleteConfirmDialogRect
  , deleteConfirmOkRect
  )
import UI.Theme
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Pure view model for a visible record detail popover.
data DataDetailPopoverView = DataDetailPopoverView
  { ddvSelectedRowIndex :: !Int
  , ddvHeaderText :: !Text
  , ddvEditing :: !Bool
  , ddvShowEditToggle :: !Bool
  , ddvEditToggleActive :: !Bool
  , ddvShowDeleteButton :: !Bool
  , ddvInteractionLocked :: !Bool
  , ddvOperationStatus :: !(Maybe Text)
  , ddvOperationError :: !(Maybe Text)
  , ddvFields :: ![DataDetailFieldRow]
  , ddvValidationRows :: ![DataDetailValidationRow]
  , ddvDeleteConfirm :: !(Maybe DataDetailDeleteConfirmView)
  } deriving (Eq, Show)

-- | One display row inside the detail popover.
data DataDetailFieldRow = DataDetailFieldRow
  { ddfPath :: !Text
  , ddfDepth :: !Int
  , ddfName :: !Text
  , ddfNestable :: !Bool
  , ddfExpanded :: !Bool
  , ddfValue :: !(Maybe Value)
  , ddfValueText :: !Text
  , ddfControl :: !DataDetailFieldControl
  } deriving (Eq, Show)

-- | Display or edit affordance used for a field row.
data DataDetailFieldControl
  = DataDetailNoValue
  | DataDetailValueDisplay
  | DataDetailTextInput !Bool !Text
  | DataDetailBoolToggle !Bool !Text
  | DataDetailEnumStepper !Bool !Text
  | DataDetailNumericStepper !Bool !Text
  deriving (Eq, Show)

-- | Validation text shown inside the popover.
data DataDetailValidationRow = DataDetailValidationRow
  { ddvrField :: !(Maybe Text)
  , ddvrMessage :: !Text
  } deriving (Eq, Show)

-- | Copy for the delete confirmation dialog.
data DataDetailDeleteConfirmView = DataDetailDeleteConfirmView
  { ddcTitle :: !Text
  , ddcConfirmLabel :: !Text
  , ddcCancelLabel :: !Text
  } deriving (Eq, Show)

-- | Build the pure view model for the currently selected record, if any.
dataDetailPopoverView :: UiState -> Maybe DataDetailPopoverView
dataDetailPopoverView ui
  | uiConfigTab ui /= ConfigData = Nothing
  | dataBrowserReadPending dbs = Nothing
  | otherwise = do
      record <- dbsSelectedRecord dbs
      rowIdx <- dbsSelectedRowIndex dbs
      schema <- selectedSchema dbs (uiDataResources ui)
      let fields = drsFields schema
          ops = drsOperations schema
          editing = dbsEditMode dbs || dbsCreateMode dbs
          locked = dataBrowserMutationPending dbs
          fieldRows = dataDetailFieldRows locked dbs fields record
      pure DataDetailPopoverView
        { ddvSelectedRowIndex = rowIdx
        , ddvHeaderText = dataDetailHeaderText dbs
        , ddvEditing = editing
        , ddvShowEditToggle = not locked && doUpdate ops && not (dbsCreateMode dbs)
        , ddvEditToggleActive = dbsEditMode dbs
        , ddvShowDeleteButton = not locked && doDelete ops && not editing
        , ddvInteractionLocked = locked
        , ddvOperationStatus = pendingMutationText dbs
        , ddvOperationError = mutationErrorText dbs
        , ddvFields = fieldRows
        , ddvValidationRows = map validationRow (dbsValidationErrors dbs)
        , ddvDeleteConfirm =
            if dbsDeleteConfirm dbs
            then Just (DataDetailDeleteConfirmView "Delete record?" "Delete" "Cancel")
            else Nothing
        }
  where
    dbs = uiDataBrowser ui

pendingMutationText :: DataBrowserState -> Maybe Text
pendingMutationText dbs = do
  pending <- dbsPendingRequest dbs
  if dataBrowserRequestIsMutation (dbpeRequest pending)
    then Just (dataBrowserOperationProgressText (dbpeOperation pending))
    else Nothing

mutationErrorText :: DataBrowserState -> Maybe Text
mutationErrorText dbs = do
  asyncError <- dataBrowserMutationError dbs
  Just (dataBrowserOperationFailureText
    (dbaeOperation asyncError) (dbaeMessage asyncError))

selectedSchema :: DataBrowserState -> Map.Map Text [DataResourceSchema] -> Maybe DataResourceSchema
selectedSchema dbs resources = do
  pName <- dbsSelectedPlugin dbs
  rName <- dbsSelectedResource dbs
  schemas <- Map.lookup pName resources
  findSchema rName schemas

findSchema :: Text -> [DataResourceSchema] -> Maybe DataResourceSchema
findSchema rName = foldr (\schema acc -> if drsName schema == rName then Just schema else acc) Nothing

dataDetailHeaderText :: DataBrowserState -> Text
dataDetailHeaderText dbs
  | dbsCreateMode dbs = "New Record"
  | otherwise = case dbsSelectedRecordKey dbs of
      Just (String t) -> t
      Just (Number n) -> T.pack (show n)
      Just v          -> TE.decodeUtf8 (BSL.toStrict (Aeson.encode v))
      Nothing         -> "Record"

dataDetailFieldRows :: Bool -> DataBrowserState -> [DataFieldDef] -> DataRecord -> [DataDetailFieldRow]
dataDetailFieldRows locked dbs fields record =
  map toRow (enumerateVisibleFields "" fields expanded)
  where
    expanded = dbsExpandedFields dbs
    editing = dbsEditMode dbs || dbsCreateMode dbs
    editValues = dbsEditValues dbs
    focusedField = dbsFocusedField dbs
    textCursor = dbsTextCursor dbs
    toRow (path, nestable) =
      let depth = length (T.splitOn "." path) - 1
          expandedRow = Set.member path expanded
          recordVal = lookupNestedValue path (unDataRecord record)
          editVal = Map.lookup path editValues
          displayVal = if editing then editVal <|> recordVal else recordVal
          valText = renderValue displayVal
      in DataDetailFieldRow
        { ddfPath = path
        , ddfDepth = depth
        , ddfName = lastSegment path
        , ddfNestable = nestable
        , ddfExpanded = expandedRow
        , ddfValue = displayVal
        , ddfValueText = valText
        , ddfControl = fieldControl (editing && not locked) nestable expandedRow focusedField textCursor fields path displayVal valText
        }

fieldControl
  :: Bool
  -> Bool
  -> Bool
  -> Maybe Text
  -> Int
  -> [DataFieldDef]
  -> Text
  -> Maybe Value
  -> Text
  -> DataDetailFieldControl
fieldControl editing nestable expandedRow focusedField textCursor fields path displayVal valText
  | nestable && expandedRow = DataDetailNoValue
  | editing && not nestable = editableFieldControl (focusedField == Just path) textCursor displayVal valText (lookupFieldType path fields)
  | otherwise = DataDetailValueDisplay

editableFieldControl :: Bool -> Int -> Maybe Value -> Text -> Maybe DataFieldType -> DataDetailFieldControl
editableFieldControl focused textCursor displayVal valText fieldType =
  let prefix = cursorPrefix textCursor displayVal valText
  in case fieldType of
    Just DFBool -> DataDetailBoolToggle focused prefix
    Just (DFEnum _) -> DataDetailEnumStepper focused prefix
    Just DFText -> DataDetailTextInput focused prefix
    _ -> DataDetailNumericStepper focused prefix

cursorPrefix :: Int -> Maybe Value -> Text -> Text
cursorPrefix textCursor displayVal valText =
  case displayVal of
    Just (String t) -> T.take textCursor t
    _               -> T.take textCursor valText

validationRow :: DataBrowserValidationError -> DataDetailValidationRow
validationRow err = DataDetailValidationRow (dbValidationField err) message
  where
    message = case dbValidationField err of
      Nothing -> dbValidationMessage err
      Just field -> field <> ": " <> dbValidationMessage err

-- | Draw the record detail popover when a data browser record is selected.
--
-- Shows field names and values; nested DFRecord\/DFAdt fields can be
-- expanded.  In edit\/create mode, fields become editable controls and
-- Save\/Cancel buttons appear.
drawDataDetailPopover :: SDL.Renderer -> FontCache -> UiState -> Layout -> IO ()
drawDataDetailPopover renderer fontCache ui layout =
  case dataDetailPopoverView ui of
    Nothing -> pure ()
    Just view -> drawDataDetailPopoverView renderer fontCache layout view

drawDataDetailPopoverView :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> IO ()
drawDataDetailPopoverView renderer fontCache layout view = do
  let rowIdx = ddvSelectedRowIndex view
      totalRows = dataDetailTotalRows view
      popRect@(Rect (V2 px py, V2 pw _ph)) = dataDetailPopoverRect rowIdx totalRows layout
      headerH = 28

  SDL.rendererDrawColor renderer SDL.$= colDataDetailBg
  SDL.fillRect renderer (Just (rectToSDL popRect))
  SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
  SDL.drawRect renderer (Just (rectToSDL popRect))
  SDL.rendererDrawColor renderer SDL.$= colDataDetailHeader
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 px py, V2 pw headerH))))
  drawText fontCache colDataDetailFieldValue (V2 (px + 6) (py + 5)) (ddvHeaderText view)

  drawDataDetailActions renderer fontCache layout view totalRows
  forM_ (zip [0..] (ddvFields view)) $ uncurry (drawDataDetailFieldRow renderer fontCache layout view totalRows)
  let operationRows = maybeToList (ddvOperationStatus view) ++ maybeToList (ddvOperationError view)
  forM_ (zip [0..] operationRows) $ uncurry (drawDataDetailOperationRow renderer fontCache layout view totalRows)
  forM_ (zip [0..] (ddvValidationRows view)) $ uncurry (drawDataDetailValidationRow renderer fontCache layout view totalRows)
  forM_ (ddvDeleteConfirm view) $ \dialog ->
    drawDeleteConfirmDialog renderer fontCache layout dialog (ddvInteractionLocked view)


dataDetailTotalRows :: DataDetailPopoverView -> Int
dataDetailTotalRows view =
  length (ddvFields view)
    + maybeCount (ddvOperationStatus view)
    + maybeCount (ddvOperationError view)
    + length (ddvValidationRows view)

maybeCount :: Maybe a -> Int
maybeCount Nothing = 0
maybeCount (Just _) = 1

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

drawDataDetailActions :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> Int -> IO ()
drawDataDetailActions renderer fontCache layout view totalRows = do
  let rowIdx = ddvSelectedRowIndex view
  when (ddvShowEditToggle view) $ do
    let editRect = dataDetailEditToggleRect rowIdx totalRows layout
        editColor = if ddvEditToggleActive view then colDataEditBtnActive else colDataEditBtn
    SDL.rendererDrawColor renderer SDL.$= editColor
    SDL.fillRect renderer (Just (rectToSDL editRect))
    let Rect (V2 ex ey, V2 _ew eh) = editRect
    drawText fontCache colDataDetailFieldValue (V2 (ex + 4) (ey + (eh - 12) `div` 2)) "✎"

  when (ddvShowDeleteButton view) $ do
    let delRect = dataDetailDeleteRect rowIdx totalRows layout
    SDL.rendererDrawColor renderer SDL.$= colDataDeleteBtn
    SDL.fillRect renderer (Just (rectToSDL delRect))
    let Rect (V2 dx dy, V2 _dw dh) = delRect
    drawText fontCache colDataDetailFieldValue (V2 (dx + 4) (dy + (dh - 12) `div` 2)) "✕"

  when (ddvEditing view && not (ddvInteractionLocked view)) $ do
    let saveRect = dataDetailSaveRect rowIdx totalRows layout
    SDL.rendererDrawColor renderer SDL.$= colDataSaveBtn
    SDL.fillRect renderer (Just (rectToSDL saveRect))
    let Rect (V2 sx sy, V2 _sw sh) = saveRect
    drawText fontCache colDataDetailFieldValue (V2 (sx + 4) (sy + (sh - 12) `div` 2)) "Save"

    let cancelRect = dataDetailCancelRect rowIdx totalRows layout
    SDL.rendererDrawColor renderer SDL.$= colDataCancelBtn
    SDL.fillRect renderer (Just (rectToSDL cancelRect))
    let Rect (V2 cx cy, V2 _cw ch) = cancelRect
    drawText fontCache colDataDetailFieldValue (V2 (cx + 4) (cy + (ch - 12) `div` 2)) "Cancel"

drawDataDetailFieldRow :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> Int -> Int -> DataDetailFieldRow -> IO ()
drawDataDetailFieldRow renderer fontCache layout view totalRows fIdx row = do
  let Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect (ddvSelectedRowIndex view) totalRows fIdx layout
      indent = ddfDepth row * 12
  when (ddfNestable row) $ do
    let indicator = if ddfExpanded row then "▼" else "▶"
    drawText fontCache colDataDetailToggle (V2 (fx + indent) (fy + 2)) indicator

  let nameX = fx + indent + (if ddfNestable row then 14 else 0)
  drawText fontCache colDataDetailFieldName (V2 nameX (fy + 2)) (ddfName row <> ":")
  drawDataDetailFieldControl renderer fontCache layout view totalRows fIdx row (Rect (V2 fx fy, V2 fw fh))

  SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
  SDL.drawLine renderer
    (SDL.P (V2 (fromIntegral fx) (fromIntegral (fy + fh - 1))))
    (SDL.P (V2 (fromIntegral (fx + fw)) (fromIntegral (fy + fh - 1))))

drawDataDetailFieldControl :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> Int -> Int -> DataDetailFieldRow -> Rect -> IO ()
drawDataDetailFieldControl renderer fontCache layout view totalRows fIdx row (Rect (V2 fx fy, V2 fw fh)) =
  case ddfControl row of
    DataDetailNoValue -> pure ()
    DataDetailValueDisplay -> do
      let valX = fx + fw `div` 2
      drawText fontCache colDataDetailFieldValue (V2 valX (fy + 2)) (ddfValueText row)
    DataDetailTextInput focused prefix ->
      drawInputControl focused prefix Nothing
    DataDetailBoolToggle focused prefix ->
      drawInputControl focused prefix Nothing
    DataDetailEnumStepper focused prefix ->
      drawInputControl focused prefix (Just enumStepper)
    DataDetailNumericStepper focused prefix ->
      drawInputControl focused prefix (Just numericStepper)
  where
    rowIdx = ddvSelectedRowIndex view
    drawInputControl focused prefix mStepper = do
      let inputRect = dataDetailFieldInputRect rowIdx totalRows fIdx layout
          borderCol = if focused then colDataFieldInputFocused else colDataFieldInputBorder
      SDL.rendererDrawColor renderer SDL.$= colDataFieldInputBg
      SDL.fillRect renderer (Just (rectToSDL inputRect))
      SDL.rendererDrawColor renderer SDL.$= borderCol
      SDL.drawRect renderer (Just (rectToSDL inputRect))
      let Rect (V2 ix iy, V2 _iw _ih) = inputRect
      drawText fontCache colDataDetailFieldValue (V2 (ix + 3) (iy + 2)) (ddfValueText row)
      when focused $ do
        let cursorX = ix + 3 + T.length prefix * 7
        SDL.rendererDrawColor renderer SDL.$= colDataFieldInputFocused
        SDL.drawLine renderer
          (SDL.P (V2 (fromIntegral cursorX) (fromIntegral (iy + 2))))
          (SDL.P (V2 (fromIntegral cursorX) (fromIntegral (iy + fh - 4))))
      forM_ mStepper ($ ())

    enumStepper () = drawStepperButtons "◀" "▶" 3 3
    numericStepper () = drawStepperButtons "-" "+" 5 5
    drawStepperButtons minusLabel plusLabel minusPad plusPad = do
      let minusRect = dataDetailFieldStepMinusRect rowIdx totalRows fIdx layout
          plusRect = dataDetailFieldStepPlusRect rowIdx totalRows fIdx layout
      SDL.rendererDrawColor renderer SDL.$= colSliderBtn
      SDL.fillRect renderer (Just (rectToSDL minusRect))
      SDL.fillRect renderer (Just (rectToSDL plusRect))
      let Rect (V2 mx my, V2 _mw mh) = minusRect
          Rect (V2 plx ply, V2 _plw plh) = plusRect
      drawText fontCache colDataDetailFieldValue (V2 (mx + minusPad) (my + (mh - 12) `div` 2)) minusLabel
      drawText fontCache colDataDetailFieldValue (V2 (plx + plusPad) (ply + (plh - 12) `div` 2)) plusLabel

drawDataDetailOperationRow :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> Int -> Int -> Text -> IO ()
drawDataDetailOperationRow renderer fontCache layout view totalRows operationIdx message = do
  let fIdx = length (ddvFields view) + operationIdx
      Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect (ddvSelectedRowIndex view) totalRows fIdx layout
  SDL.rendererDrawColor renderer SDL.$= colDataDeleteConfirmBg
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 fx fy, V2 fw fh))))
  drawText fontCache colLogErrorText (V2 (fx + 4) (fy + 2)) message

drawDataDetailValidationRow :: SDL.Renderer -> FontCache -> Layout -> DataDetailPopoverView -> Int -> Int -> DataDetailValidationRow -> IO ()
drawDataDetailValidationRow renderer fontCache layout view totalRows vIdx row = do
  let operationCount = maybeCount (ddvOperationStatus view) + maybeCount (ddvOperationError view)
      fIdx = length (ddvFields view) + operationCount + vIdx
      Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect (ddvSelectedRowIndex view) totalRows fIdx layout
  SDL.rendererDrawColor renderer SDL.$= colDataDeleteConfirmBg
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 fx fy, V2 fw fh))))
  drawText fontCache colLogErrorText (V2 (fx + 4) (fy + 2)) (ddvrMessage row)
  SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
  SDL.drawLine renderer
    (SDL.P (V2 (fromIntegral fx) (fromIntegral (fy + fh - 1))))
    (SDL.P (V2 (fromIntegral (fx + fw)) (fromIntegral (fy + fh - 1))))

drawDeleteConfirmDialog :: SDL.Renderer -> FontCache -> Layout -> DataDetailDeleteConfirmView -> Bool -> IO ()
drawDeleteConfirmDialog renderer fontCache layout dialog locked = do
  let dlgRect = deleteConfirmDialogRect layout
      okRect = deleteConfirmOkRect layout
      cancelRect = deleteConfirmCancelRect layout
  SDL.rendererDrawColor renderer SDL.$= colDataDeleteConfirmBg
  SDL.fillRect renderer (Just (rectToSDL dlgRect))
  SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
  SDL.drawRect renderer (Just (rectToSDL dlgRect))
  let Rect (V2 dlx dly, V2 dlw _dlh) = dlgRect
  drawText fontCache colDataDetailFieldValue (V2 (dlx + dlw `div` 2 - 50) (dly + 16)) (ddcTitle dialog)
  when (not locked) $ do
    SDL.rendererDrawColor renderer SDL.$= colDataDeleteBtn
    SDL.fillRect renderer (Just (rectToSDL okRect))
    SDL.rendererDrawColor renderer SDL.$= colDataCancelBtn
    SDL.fillRect renderer (Just (rectToSDL cancelRect))
    let Rect (V2 ox oy, V2 _ow oh) = okRect
        Rect (V2 ccx ccy, V2 _ccw cch) = cancelRect
    drawText fontCache colDataDetailFieldValue (V2 (ox + 8) (oy + (oh - 12) `div` 2)) (ddcConfirmLabel dialog)
    drawText fontCache colDataDetailFieldValue (V2 (ccx + 8) (ccy + (cch - 12) `div` 2)) (ddcCancelLabel dialog)

-- | Enumerate the visible field rows, returning @(dotPath, isExpandable)@.
enumerateVisibleFields :: Text -> [DataFieldDef] -> Set.Set Text -> [(Text, Bool)]
enumerateVisibleFields prefix defs expanded = concatMap go defs
  where
    qualify name
      | T.null prefix = name
      | otherwise     = prefix <> "." <> name
    go fdef =
      let path = qualify (dfName fdef)
          expandable = isNestable (dfType fdef)
          thisRow = [(path, expandable)]
      in if expandable && Set.member path expanded
         then thisRow ++ childRows path (dfType fdef)
         else thisRow
    childRows path (DFRecord subFields) =
      enumerateVisibleFields path subFields expanded
    childRows path (DFAdt ctors) =
      concatMap constructorRows ctors
      where
        constructorRows ctor =
          childRows
            (path <> "." <> dcdName ctor)
            (DFRecord (zipWith positionalField [(0 :: Int)..] (dcdFields ctor)))
        positionalField i fieldType =
          let name = T.pack (show i)
          in DataFieldDef name fieldType name False Nothing
    childRows _ _ = []

-- | Whether a field type can be expanded.
isNestable :: DataFieldType -> Bool
isNestable (DFRecord _) = True
isNestable (DFAdt _)    = True
isNestable _            = False

-- | Get the last segment of a dot-separated path.
lastSegment :: Text -> Text
lastSegment t = case T.splitOn "." t of
  [] -> t
  xs -> last xs

-- | Look up a potentially nested value in a record map using a dot-path.
lookupNestedValue :: Text -> Map.Map Text Value -> Maybe Value
lookupNestedValue path m =
  case T.splitOn "." path of
    []     -> Nothing
    (k:ks) -> case Map.lookup k m of
      Nothing -> Nothing
      Just v  -> goNested ks v
  where
    goNested [] v = Just v
    goNested (s:ss) (Object km) = case AesonKM.lookup (AesonKey.fromText s) km of
      Just v  -> goNested ss v
      Nothing -> case (AesonKM.lookup "constructor" km, AesonKM.lookup "fields" km) of
        (Just (String ctor), Just fieldsValue) | ctor == s -> goNested ss fieldsValue
        _ -> Nothing
    goNested (s:ss) (Array values) = do
      index <- parseIndex s
      value <- values Vector.!? index
      goNested ss value
    goNested _ _ = Nothing

-- | Render a JSON 'Value' as a concise 'Text' for display.
renderValue :: Maybe Value -> Text
renderValue Nothing           = "—"
renderValue (Just Null)       = "null"
renderValue (Just (Bool b))   = if b then "true" else "false"
renderValue (Just (String t)) = t
renderValue (Just (Number n)) = T.pack (show n)
renderValue (Just (Array _))  = "[…]"
renderValue (Just (Object _)) = "{…}"

-- | Look up the 'DataFieldType' for a dot-separated path within field defs.
lookupFieldType :: Text -> [DataFieldDef] -> Maybe DataFieldType
lookupFieldType path defs = case T.splitOn "." path of
  []     -> Nothing
  (k:ks) -> case filter (\d -> dfName d == k) defs of
    (d:_) -> resolveRest ks (dfType d)
    []    -> Nothing
  where
    resolveRest [] ft = Just ft
    resolveRest (s:ss) (DFRecord subDefs) =
      case filter (\d -> dfName d == s) subDefs of
        (d:_) -> resolveRest ss (dfType d)
        []    -> Nothing
    resolveRest (ctorName:indexText:ss) (DFAdt ctors) = do
      ctor <- findConstructor ctorName ctors
      index <- parseIndex indexText
      fieldType <- safeIndex (dcdFields ctor) index
      resolveRest ss fieldType
    resolveRest _ _ = Nothing

findConstructor :: Text -> [DataConstructorDef] -> Maybe DataConstructorDef
findConstructor ctorName = foldr (\ctor acc -> if dcdName ctor == ctorName then Just ctor else acc) Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs index
  | index < 0 = Nothing
  | otherwise = case drop index xs of
      value:_ -> Just value
      []      -> Nothing

parseIndex :: Text -> Maybe Int
parseIndex text = case reads (T.unpack text) of
  [(n, "")] | n >= 0 -> Just n
  _                  -> Nothing
