{-# LANGUAGE OverloadedStrings #-}

-- | SDL presentation for the AppService-backed overlay inspector modal.
module Seer.Draw.OverlayInspector
  ( drawOverlayInspector
  , overlayInspectorTitle
  , overlayInspectorBodyLines
  , overlayInspectorVisibleLines
  , overlayInspectorScrollLimit
  , overlayInspectorViewScrollLimit
  ) where

import Actor.UI (UiMenuMode(..), UiState(..))
import Data.Aeson (Value)
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import Seer.Draw.Dialog
  ( drawDialogButton
  , drawDialogPanel
  , drawTextInputField
  )
import Seer.OverlayInspector.Model
import UI.Font (FontCache)
import UI.Layout
import UI.Theme
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawLeftTruncated, drawTextLine, rectToSDL)

lineHeight :: Int
lineHeight = 18

-- | Drawn last by 'Seer.Render.Ui', making the inspector a true input and
-- presentation barrier over all terrain and panel content.
drawOverlayInspector :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawOverlayInspector renderer fontCache ui layout
  | uiMenuMode ui /= MenuOverlayInspector = pure ()
  | otherwise = do
      let inspector = uiOverlayInspector ui
          dialog = overlayInspectorDialogRect layout
          body = overlayInspectorBodyRect layout
      drawDialogPanel renderer dialog
      drawTextLine fontCache (titlePoint (overlayInspectorTitleRect layout)) textDialogTitle
        (overlayInspectorTitle inspector)
      drawDialogButton renderer fontCache (overlayInspectorCloseRect layout) "×" True
      case oimView inspector of
        Just OverlayInspectorManagerView -> drawManager renderer fontCache inspector layout
        Just OverlayInspectorImportView -> drawImport renderer fontCache inspector layout
        Just OverlayInspectorExportView -> do
          drawLines renderer fontCache body (overlayInspectorVisibleLines body inspector)
          drawDialogButton renderer fontCache (overlayInspectorCopyRect layout) "Copy JSON" exportReady
          drawDialogButton renderer fontCache (overlayInspectorSaveRect layout) "Save JSON" exportReady
        _ -> drawLines renderer fontCache body (overlayInspectorVisibleLines body inspector)
      drawFocus renderer inspector layout
  where
    exportReady = maybe False (const True) (oimExportPayload (uiOverlayInspector ui))
    titlePoint (Rect (V2 x y, _)) = V2 x (y + 6)

drawManager :: SDL.Renderer -> Maybe FontCache -> OverlayInspectorModel -> Layout -> IO ()
drawManager renderer fontCache inspector layout = do
  let names = oimOverlayNames inspector
      selectedIndex = oimSelectedOverlay inspector >>= \selected -> findIndex (== selected) names
      status = statusPrefix inspector
      statusSlots = if null status then 0 else 1
      rowCount = managerVisibleRowCount layout
      detailRows = min managerDetailRows (max 0 (rowCount - statusSlots - 1))
      visibleCount = max 1 (rowCount - statusSlots - detailRows)
      maxStart = max 0 (length names - visibleCount)
      start = min maxStart (oimScroll inspector)
      visible = take visibleCount (drop start (zip [0..] names))
      body = overlayInspectorBodyRect layout
      emptySlots = if null names && null status then 1 else 0
      detailStart = statusSlots + emptySlots + length visible
      details = take (max 0 (rowCount - detailStart)) (managerDetailLines body inspector)
  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL body)
  case status of
    line:_ -> drawTextLine fontCache (bodyPoint layout 0) textDialogContent line
    [] -> pure ()
  mapM_ (drawRow selectedIndex statusSlots) (zip [0..] visible)
  case names of
    [] | null status -> drawTextLine fontCache (bodyPoint layout 0) textDialogContent
      "No overlays are loaded."
    _ -> pure ()
  mapM_ (drawDetail detailStart) (zip [0..] details)
  SDL.rendererClipRect renderer SDL.$= Nothing
  where
    drawRow selectedIndex statusSlots (slot, (index, name)) = do
      let rect = overlayInspectorRowRect (slot + statusSlots) layout
          selected = selectedIndex == Just index
      SDL.rendererDrawColor renderer SDL.$= if selected then colDialogButtonActive else colListBg
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawLeftTruncated fontCache (if selected then textDialogContentSel else textDialogContent) rect
        ((if selected then "● " else "  ") <> name)
    drawDetail detailStart (slot, line) =
      drawLeftTruncated fontCache textDialogContent
        (overlayInspectorRowRect (detailStart + slot) layout) line

-- This layout intentionally exposes raw JSON as text while preserving the
-- exact parsed Value used by validation. It never labels the operation import.
drawImport :: SDL.Renderer -> Maybe FontCache -> OverlayInspectorModel -> Layout -> IO ()
drawImport renderer fontCache inspector layout = do
  drawTextLine fontCache (bodyPoint layout 0) textDialogContent
    "VALIDATION ONLY — this does not import or adopt overlay data."
  let inputRect = overlayInspectorImportInputRect layout
      visibleCount = max 1 ((rectHeight inputRect - 8) `div` lineHeight)
      displayText = if oimFocus inspector == OverlayInspectorImportInputFocus
        then insertCursor (oimImportCursor inspector) (oimImportText inspector)
        else oimImportText inspector
      allInputLines = textLinesAtWidth (rectTextWidth inputRect) displayText
      inputScroll = min (max 0 (length allInputLines - visibleCount)) (oimScroll inspector)
      inputLines = take visibleCount (drop inputScroll allInputLines)
  SDL.rendererDrawColor renderer SDL.$= colInputFieldBg
  SDL.fillRect renderer (Just (rectToSDL inputRect))
  SDL.rendererDrawColor renderer SDL.$= textInputFieldBorder
  SDL.drawRect renderer (Just (rectToSDL inputRect))
  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL inputRect)
  mapM_ (drawInputLine inputRect) (zip [0..] inputLines)
  SDL.rendererClipRect renderer SDL.$= Nothing
  drawDialogButton renderer fontCache (overlayInspectorValidateRect layout) "Validate JSON"
    (not (overlayInspectorLoading inspector))
  mapM_ drawDiagnostic (zip [0..1] (importDiagnosticLines inspector))
  where
    drawInputLine (Rect (V2 x y, _)) (index, line) =
      drawTextLine fontCache (V2 (x + 6) (y + 5 + index * lineHeight)) textDialogContent line
    drawDiagnostic (index, line) =
      drawLeftTruncated fontCache textDialogContent (diagnosticRect index layout) line

drawLines :: SDL.Renderer -> Maybe FontCache -> Rect -> [Text] -> IO ()
drawLines renderer fontCache body lines = do
  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL body)
  mapM_ drawOne (zip [0..] lines)
  SDL.rendererClipRect renderer SDL.$= Nothing
  where
    Rect (V2 x y, _) = body
    drawOne (index, line) =
      drawTextLine fontCache (V2 (x + 4) (y + 4 + index * lineHeight)) textDialogContent line

drawFocus :: SDL.Renderer -> OverlayInspectorModel -> Layout -> IO ()
drawFocus renderer inspector layout = case focusRect of
  Nothing -> pure ()
  Just rect -> do
    SDL.rendererDrawColor renderer SDL.$= colHoverHex
    SDL.drawRect renderer (Just (rectToSDL rect))
  where
    focusRect = case oimFocus inspector of
      OverlayInspectorCloseFocus -> Just (overlayInspectorCloseRect layout)
      OverlayInspectorManagerFocus index
        | index >= managerStart && index < managerStart + visibleRows ->
            Just (overlayInspectorRowRect (index - managerStart + statusSlots) layout)
        | otherwise -> Nothing
      OverlayInspectorCopyFocus -> Just (overlayInspectorCopyRect layout)
      OverlayInspectorSaveFocus -> Just (overlayInspectorSaveRect layout)
      OverlayInspectorImportInputFocus -> Just (overlayInspectorImportInputRect layout)
      OverlayInspectorValidateFocus -> Just (overlayInspectorValidateRect layout)
    statusSlots = if null (statusPrefix inspector) then 0 else 1
    visibleRows = max 1 (managerVisibleRowCount layout - statusSlots - managerDetailRows)
    managerStart = min (max 0 (length (oimOverlayNames inspector) - visibleRows))
      (oimScroll inspector)

overlayInspectorTitle :: OverlayInspectorModel -> Text
overlayInspectorTitle inspector = case oimView inspector of
  Just OverlayInspectorManagerView -> "Overlay Manager"
  Just OverlayInspectorSchemaView -> selected "Overlay Schema"
  Just OverlayInspectorProvenanceView -> selected "Overlay Provenance"
  Just OverlayInspectorExportView -> selected "Overlay JSON Export"
  Just OverlayInspectorImportView -> "Overlay JSON Import Validation"
  Nothing -> "Overlay Inspector"
  where
    selected label = label <> maybe "" (" — " <>) (oimSelectedOverlay inspector)

overlayInspectorBodyLines :: OverlayInspectorModel -> [Text]
overlayInspectorBodyLines inspector =
  statusPrefix inspector <> case oimView inspector of
    Just OverlayInspectorManagerView -> managerStatusLines inspector
    Just OverlayInspectorSchemaView -> payloadLines "Schema response" (oimSchemaPayload inspector)
    Just OverlayInspectorProvenanceView -> payloadLines "Provenance response" (oimProvenancePayload inspector)
    Just OverlayInspectorExportView ->
      [ "Exact AppService/HTTP JSON payload. Use Copy JSON or Save JSON."
      , "Saved as a unique <overlay>-*.json under ~/.topo/exports/."
      ] <> payloadLines "Export response" (oimExportPayload inspector)
    Just OverlayInspectorImportView ->
      ["VALIDATION ONLY — this does not import or adopt overlay data."]
        <> textLines (oimImportText inspector)
        <> map ("Diagnostic: " <>) (oimLocalDiagnostics inspector)
        <> concatMap (payloadLines "Validation diagnostic" . Just) (oimValidationDiagnostics inspector)
        <> payloadLines "Validation response" (oimImportValidation inspector)
    Nothing -> []

overlayInspectorVisibleLines :: Rect -> OverlayInspectorModel -> [Text]
overlayInspectorVisibleLines body@(Rect (_, V2 _ height)) inspector =
  take visibleCount (drop scroll wrapped)
  where
    visibleCount = max 1 ((height - 8) `div` lineHeight)
    wrapped = concatMap (chunks (rectTextWidth body)) (overlayInspectorBodyLines inspector)
    scroll = min (max 0 (length wrapped - visibleCount)) (oimScroll inspector)

-- | Highest scroll offset that still leaves a full useful page visible.
overlayInspectorScrollLimit :: Rect -> OverlayInspectorModel -> Int
overlayInspectorScrollLimit body@(Rect (_, V2 _ height)) inspector =
  max 0 (length wrapped - visibleCount)
  where
    visibleCount = max 1 ((height - 8) `div` lineHeight)
    wrapped = concatMap (chunks (rectTextWidth body)) (overlayInspectorBodyLines inspector)

-- | Geometry-aware limit shared by keyboard and mouse input.
overlayInspectorViewScrollLimit :: Layout -> OverlayInspectorModel -> Int
overlayInspectorViewScrollLimit layout inspector = case oimView inspector of
  Just OverlayInspectorManagerView ->
    let Rect (_, V2 _ bodyHeight) = overlayInspectorBodyRect layout
        statusSlots = if null (statusPrefix inspector) then 0 else 1
        visible = max 1 (bodyHeight `div` 30 - statusSlots - managerDetailRows)
    in max 0 (length (oimOverlayNames inspector) - visible)
  Just OverlayInspectorImportView ->
    let inputRect@(Rect (_, V2 _ height)) = overlayInspectorImportInputRect layout
        visible = max 1 ((height - 8) `div` lineHeight)
        wrapped = textLinesAtWidth (rectTextWidth inputRect) (oimImportText inspector)
    in max 0 (length wrapped - visible)
  _ -> overlayInspectorScrollLimit (overlayInspectorBodyRect layout) inspector

statusPrefix :: OverlayInspectorModel -> [Text]
statusPrefix inspector =
  [ "Loading " <> overlayInspectorOperationText (oipOperation pending) <> "…"
  | pending <- maybe [] pure (oimPending inspector)
  ]
  <> [ "Service error: " <> overlayInspectorPayloadText (oiaeError asyncError)
     | asyncError <- maybe [] pure (oimAsyncError inspector)
     ]
  <> maybe [] (pure . ("Status: " <>)) (oimNotice inspector)

managerStatusLines :: OverlayInspectorModel -> [Text]
managerStatusLines inspector =
  [ "Selected: " <> maybe "none" id (oimSelectedOverlay inspector)
  , "Use ↑/↓ or click to select; Schema opens full details."
  ] <> payloadLines "Manager response" (oimManagerPayload inspector)

payloadLines :: Text -> Maybe Value -> [Text]
payloadLines label payload =
  label : case payload of
    Nothing -> ["(not loaded)"]
    Just value -> textLines (overlayInspectorPayloadText value)

textLines :: Text -> [Text]
textLines = textLinesAtWidth 100

textLinesAtWidth :: Int -> Text -> [Text]
textLinesAtWidth width text
  | Text.null text = [""]
  | otherwise = concatMap (chunks width) (Text.splitOn "\n" text)

chunks :: Int -> Text -> [Text]
chunks requestedWidth value
  | Text.null value = [""]
  | otherwise =
      let width = max 1 requestedWidth
          (line, rest) = Text.splitAt width value
      in line : if Text.null rest then [] else chunks width rest

managerDetailLines :: Rect -> OverlayInspectorModel -> [Text]
managerDetailLines body inspector =
  ["Selected detail: " <> maybe "none" id (oimSelectedOverlay inspector)]
    <> case oimManagerPayload inspector of
      Nothing -> ["Manager response: (not loaded)"]
      Just payload -> "Manager response:" : textLinesAtWidth
        (rectTextWidth body) (overlayInspectorPayloadText payload)

importDiagnosticLines :: OverlayInspectorModel -> [Text]
importDiagnosticLines inspector =
  [ "Validation result: " <> overlayInspectorPayloadText value
  | value <- maybe [] pure (oimImportValidation inspector)
  ]
    <> map ("Diagnostic: " <>) (oimLocalDiagnostics inspector)
    <> [ "Diagnostic: " <> overlayInspectorPayloadText value
       | value <- oimValidationDiagnostics inspector
       ]
    <> maybe [] (pure . ("Status: " <>)) (oimNotice inspector)
    <> [ "Service error: " <> overlayInspectorPayloadText (oiaeError err)
       | err <- maybe [] pure (oimAsyncError inspector)
       ]

insertCursor :: Int -> Text -> Text
insertCursor requested text =
  let cursor = max 0 (min (Text.length text) requested)
  in Text.take cursor text <> "│" <> Text.drop cursor text

managerVisibleRowCount :: Layout -> Int
managerVisibleRowCount layout =
  let Rect (_, V2 _ height) = overlayInspectorBodyRect layout
  in max 1 (height `div` 30)

bodyPoint :: Layout -> Int -> V2 Int
bodyPoint layout index =
  let Rect (V2 x y, _) = overlayInspectorBodyRect layout
  in V2 (x + 4) (y + 4 + index * 30)

diagnosticRect :: Int -> Layout -> Rect
diagnosticRect index layout =
  let Rect (V2 x y, V2 width height) = overlayInspectorImportInputRect layout
  in Rect (V2 x (y + height + 3 + index * lineHeight), V2 width lineHeight)

rectHeight :: Rect -> Int
rectHeight (Rect (_, V2 _ height)) = height

rectTextWidth :: Rect -> Int
rectTextWidth (Rect (_, V2 width _)) = max 1 ((width - 12) `div` 8)

managerDetailRows :: Int
managerDetailRows = 4
