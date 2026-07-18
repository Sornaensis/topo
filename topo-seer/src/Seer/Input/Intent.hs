{-# LANGUAGE OverloadedStrings #-}

-- | Renderer-neutral keyboard and dialog intent execution.
--
-- SDL and automation both enter here after normalizing a key press.  Keeping
-- precedence and side effects in one place prevents remote input from merely
-- imitating the visible state changes of a real UI action.
module Seer.Input.Intent
  ( InputKey(..)
  , KeyModifiers(..)
  , noKeyModifiers
  , InputIntentEnv(..)
  , InputIntentResult(..)
  , parseInputKey
  , parseKeyModifiers
  , executeKeyIntent
  , executeTextIntent
  , executeDialogConfirm
  , executeDialogCancel
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=))
import Data.Char (toLower)
import Data.List (findIndex)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.Log (LogSnapshot(..), setLogCollapsed)
import Actor.UI.Setters
  ( setUiContextHex
  , setUiContextPos
  , setUiMenuMode
  , setUiPresetFilter
  , setUiPresetInput
  , setUiPresetSelected
  , setUiSeedEditing
  , setUiSeedInput
  , setUiShowConfig
  , setUiWorldFilter
  , setUiWorldSaveInput
  , setUiWorldSelected
  , setUiWorldDeleteConfirm
  , setUiWorldDeleteTarget
  , setUiWorldDeleteError
  , closeUiOverlayInspector
  , setUiOverlayInspectorFocus
  , setUiOverlayInspectorScroll
  , moveUiOverlayInspectorSelection
  , setUiOverlayInspectorImportText
  )
import Actor.UI.State
  ( BaseViewMode(..)
  , ConfigTab(..)
  , DataBrowserState(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , UiMenuMode(..)
  , UiState(..)
  , WeatherBasis(..)
  , effectiveViewSelection
  )
import Actor.UiActions.Handles (ActorHandles(..), publishUiMutation)
import Seer.Config.PresetCatalogue (presetCatalogueMatches)
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction(..))
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..))
import Seer.Input.Seed (parseSeedText)
import Seer.Input.ViewControls (nextBuiltinOverlay, nextWeatherBasis)
import Seer.OverlayInspector.Model
  ( OverlayInspectorFocus(..)
  , OverlayInspectorModel(..)
  , OverlayInspectorView(..)
  )
import Seer.Service.Types (ServiceResponse(..), ServiceResult, serviceErrorText)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import UI.WidgetId (WidgetId(..), widgetIdToText)

-- | Normalized keys understood by both SDL and automation.
data InputKey
  = KeyEscape
  | KeyEnter
  | KeyBackspace
  | KeyDelete
  | KeyTab
  | KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyHome
  | KeyEnd
  | KeySpace
  | KeyLeftBracket
  | KeyRightBracket
  | KeyCharacter !Char
  deriving (Eq, Show)

-- | Modifier state carried by a key press.
data KeyModifiers = KeyModifiers
  { kmCtrl :: !Bool
  , kmShift :: !Bool
  , kmAlt :: !Bool
  , kmMeta :: !Bool
  } deriving (Eq, Show)

noKeyModifiers :: KeyModifiers
noKeyModifiers = KeyModifiers False False False False

-- | Capabilities required by the shared executor.  The service runner is
-- injected so this module does not depend on the AppService adapter.
data InputIntentEnv = InputIntentEnv
  { iieActorHandles :: !ActorHandles
  , iieGetUi :: !(IO UiState)
  , iieGetLog :: !(IO LogSnapshot)
  , iieRunService :: !(Text -> Value -> IO ServiceResult)
  , iieApplyDataBrowser :: !(DataBrowserAppAction -> IO (Either Text ()))
  , iieDeferBlockingWidgets :: !Bool
  , iieOverlayInspectorScrollLimit :: !(OverlayInspectorModel -> IO Int)
  }

-- | Structured execution outcome used by API responses and SDL runtime hooks.
data InputIntentResult = InputIntentResult
  { iirAction :: !Text
  , iirApplied :: !Bool
  , iirStopTextInput :: !Bool
  , iirSelected :: !(Maybe Int)
  , iirText :: !(Maybe Text)
  , iirFilter :: !(Maybe Text)
  , iirCursor :: !(Maybe Int)
  , iirField :: !(Maybe Text)
  , iirName :: !(Maybe Text)
  , iirMenuMode :: !(Maybe Text)
  , iirClipboard :: !(Maybe Text)
  , iirDeferredWidget :: !(Maybe WidgetId)
  , iirStartTextInput :: !Bool
  } deriving (Eq, Show)

emptyResult :: Text -> Bool -> InputIntentResult
emptyResult action applied = InputIntentResult
  { iirAction = action
  , iirApplied = applied
  , iirStopTextInput = False
  , iirSelected = Nothing
  , iirText = Nothing
  , iirFilter = Nothing
  , iirCursor = Nothing
  , iirField = Nothing
  , iirName = Nothing
  , iirMenuMode = Nothing
  , iirClipboard = Nothing
  , iirDeferredWidget = Nothing
  , iirStartTextInput = False
  }

applied :: Text -> InputIntentResult
applied action = emptyResult action True

noEffect :: InputIntentResult
noEffect = emptyResult "no_effect" False

parseInputKey :: Text -> Either Text InputKey
parseInputKey raw =
  case Text.toLower raw of
    "escape" -> Right KeyEscape
    "esc" -> Right KeyEscape
    "return" -> Right KeyEnter
    "enter" -> Right KeyEnter
    "backspace" -> Right KeyBackspace
    "delete" -> Right KeyDelete
    "tab" -> Right KeyTab
    "up" -> Right KeyUp
    "down" -> Right KeyDown
    "left" -> Right KeyLeft
    "right" -> Right KeyRight
    "home" -> Right KeyHome
    "end" -> Right KeyEnd
    "space" -> Right KeySpace
    "left_bracket" -> Right KeyLeftBracket
    "right_bracket" -> Right KeyRightBracket
    _ -> case Text.unpack raw of
      [ch] -> Right (KeyCharacter ch)
      _ -> Left ("unrecognized key: " <> raw)

parseKeyModifiers :: [Text] -> Either Text KeyModifiers
parseKeyModifiers = foldl step (Right noKeyModifiers)
  where
    step acc raw = do
      mods <- acc
      case Text.toLower raw of
        "ctrl" -> Right mods { kmCtrl = True }
        "control" -> Right mods { kmCtrl = True }
        "shift" -> Right mods { kmShift = True }
        "alt" -> Right mods { kmAlt = True }
        "meta" -> Right mods { kmMeta = True }
        "gui" -> Right mods { kmMeta = True }
        _ -> Left ("unrecognized modifier: " <> raw)

executeTextIntent :: InputIntentEnv -> Text -> IO (Either Text InputIntentResult)
executeTextIntent env input = do
  ui <- iieGetUi env
  let uiH = ahUiHandle (iieActorHandles env)
  if uiMenuMode ui == MenuOverlayInspector
    then case focusedOverlayImport ui of
      Just inspector -> insertOverlay inspector
      Nothing -> pure (Right noEffect)
    else if dataDeleteConfirmationVisible ui
      then pure (Right noEffect)
    else if uiSeedEditing ui
      then do
        let accepted = Text.filter (`elem` ['0'..'9']) input
            current = uiSeedInput ui
            next | Text.null accepted = current
                 | current == "0" = accepted
                 | otherwise = current <> accepted
        setUiSeedInput uiH next
        pure (Right (applied "seed_text") { iirText = Just next })
      else case focusedDataField ui of
        Just field -> do
          let accepted = Text.filter (>= ' ') input
          if Text.null accepted
            then pure (Right noEffect)
            else dataAction env (DataBrowserInsertText accepted)
              (applied "data_field_text") { iirField = Just field, iirText = Just accepted }
        Nothing -> case uiMenuMode ui of
          MenuPresetSave -> appendText (uiPresetInput ui) input (setUiPresetInput uiH) "preset_text"
          MenuWorldSave -> appendText (uiWorldSaveInput ui) input (setUiWorldSaveInput uiH) "world_text"
          MenuPresetLoad -> appendFilter (uiPresetFilter ui) input (setUiPresetFilter uiH) (setUiPresetSelected uiH) "preset_filter"
          MenuWorldLoad
            | uiWorldDeleteConfirm ui -> pure (Right noEffect)
            | otherwise -> appendFilter (uiWorldFilter ui) input (setUiWorldFilter uiH) (setUiWorldSelected uiH) "world_filter"
          _ -> pure (Right noEffect)
  where
    insertOverlay inspector = do
      let accepted = Text.filter (>= ' ') input
          cursor = oimImportCursor inspector
          current = oimImportText inspector
          next = Text.take cursor current <> accepted <> Text.drop cursor current
          nextCursor = cursor + Text.length accepted
      if Text.null accepted
        then pure (Right noEffect)
        else do
          setUiOverlayInspectorImportText
            (ahUiHandle (iieActorHandles env)) next nextCursor
          pure (Right (applied "overlay_import_text")
            { iirText = Just next, iirCursor = Just nextCursor })
    appendText current raw setter action = do
      let next = current <> Text.filter (>= ' ') raw
      setter next
      pure (Right (applied action) { iirText = Just next })
    appendFilter current raw setter setSelection action = do
      let next = current <> Text.filter (>= ' ') raw
      setter next
      setSelection 0
      pure (Right (applied action) { iirFilter = Just next, iirSelected = Just 0 })

executeDialogConfirm :: InputIntentEnv -> IO (Either Text InputIntentResult)
executeDialogConfirm env = do
  ui <- iieGetUi env
  case focusedDataField ui of
    Just field -> dataAction env DataBrowserBlurField
      (applied "data_field_confirm") { iirField = Just field, iirStopTextInput = True }
    Nothing -> case uiMenuMode ui of
      MenuPresetSave -> runAndClose "save_preset" (object ["name" .= uiPresetInput ui])
        (applied "preset_save_confirm") { iirName = Just (uiPresetInput ui), iirStopTextInput = True }
      MenuPresetLoad ->
        let items = filteredPresets ui
        in runSelected "load_preset" (uiPresetSelected ui) items id "preset_load_confirm"
      MenuWorldSave ->
        if Text.null (uiWorldSaveInput ui)
          then pure (Left "world name must not be empty")
          else runAndClose "save_world" (object ["name" .= uiWorldSaveInput ui])
            (applied "world_save_confirm") { iirName = Just (uiWorldSaveInput ui), iirStopTextInput = True }
      MenuWorldLoad
        | uiWorldDeleteConfirm ui -> confirmWorldDelete env ui
        | otherwise ->
            let items = filteredWorlds ui
            in runSelected "load_world" (uiWorldSelected ui) items wsmName "world_load_confirm"
      _ | uiSeedEditing ui -> commitSeed env ui
        | otherwise -> pure (Right noEffect)
  where
    uiH = ahUiHandle (iieActorHandles env)
    runAndClose method params result = do
      serviceResult <- iieRunService env method params
      case serviceResult of
        Left err -> pure (Left (serviceErrorText err))
        Right _ -> setUiMenuMode uiH MenuNone >> pure (Right result)
    runSelected method index items itemName action =
      case atIndex index items of
        Nothing -> pure (Left ("no filtered selection for " <> action))
        Just item -> runAndClose method (object ["name" .= itemName item])
          (applied action)
            { iirSelected = Just index
            , iirName = Just (itemName item)
            , iirStopTextInput = True
            }

executeDialogCancel :: InputIntentEnv -> IO (Either Text InputIntentResult)
executeDialogCancel env = do
  ui <- iieGetUi env
  if uiSeedEditing ui
    then cancelSeed env ui
    else case focusedDataField ui of
      Just field -> executeDataFieldKey env ui field KeyEscape
      Nothing -> case uiMenuMode ui of
        MenuOverlayInspector -> do
          closeUiOverlayInspector (ahUiHandle (iieActorHandles env))
          pure (Right (applied "overlay_inspector_close") { iirStopTextInput = True })
        MenuPresetSave -> executeModalKey env ui KeyEscape
        MenuPresetLoad -> executeModalKey env ui KeyEscape
        MenuWorldSave -> executeModalKey env ui KeyEscape
        MenuWorldLoad -> executeModalKey env ui KeyEscape
        _ -> escapeCascade env ui

executeKeyIntent :: InputIntentEnv -> KeyModifiers -> InputKey -> IO (Either Text InputIntentResult)
executeKeyIntent env mods key = do
  ui <- iieGetUi env
  if uiMenuMode ui == MenuOverlayInspector
    then executeOverlayInspectorKey env ui mods key
    else if uiMenuMode ui == MenuEscape
      then case key of
        KeyEscape -> do
          setUiMenuMode (ahUiHandle (iieActorHandles env)) MenuNone
          pure (Right (applied "close_menu") { iirMenuMode = Just "none" })
        _ -> pure (Right noEffect)
    else if dataDeleteConfirmationVisible ui
      then case key of
        KeyEscape -> escapeCascade env ui
        _ -> pure (Right noEffect)
    else if uiSeedEditing ui
      then executeSeedKey env ui key
      else case focusedDataField ui of
        Just field -> executeDataFieldKey env ui field key
        Nothing -> case uiMenuMode ui of
          MenuPresetSave -> executeModalKey env ui key
          MenuPresetLoad -> executeModalKey env ui key
          MenuWorldSave -> executeModalKey env ui key
          MenuWorldLoad -> executeModalKey env ui key
          _ | editorActive (uiEditor ui) -> executeEditorKey env ui mods key
            | otherwise -> executeGlobalKey env ui key

executeOverlayInspectorKey
  :: InputIntentEnv
  -> UiState
  -> KeyModifiers
  -> InputKey
  -> IO (Either Text InputIntentResult)
executeOverlayInspectorKey env ui mods key =
  case key of
    KeyEscape -> do
      closeUiOverlayInspector uiH
      pure (Right (applied "overlay_inspector_close") { iirStopTextInput = True })
    KeyTab -> cycleFocus (if kmShift mods then (-1) else 1)
    KeyUp -> vertical (-1)
    KeyDown -> vertical 1
    KeyHome | importFocused -> setCursor 0
    KeyEnd | importFocused -> setCursor (Text.length (oimImportText inspector))
    KeyHome -> setScroll 0
    KeyEnd -> setScroll maxBound
    KeyEnter -> activateFocus
    KeyBackspace | importFocused -> editImportBackspace
    KeyDelete | importFocused -> editImportDelete
    KeyLeft | importFocused -> moveCursor (-1)
    KeyRight | importFocused -> moveCursor 1
    KeySpace | importFocused -> executeTextIntent env " "
    KeyCharacter ch | importFocused && ch >= ' ' -> executeTextIntent env (Text.singleton ch)
    KeyCharacter ch | kmCtrl mods && toLower ch == 'c' && exportView -> copyResult
    _ -> pure (Right noEffect)
  where
    uiH = ahUiHandle (iieActorHandles env)
    inspector = uiOverlayInspector ui
    importFocused = oimView inspector == Just OverlayInspectorImportView
      && oimFocus inspector == OverlayInspectorImportInputFocus
    exportView = oimView inspector == Just OverlayInspectorExportView
    setScroll requested = do
      scrollLimit <- iieOverlayInspectorScrollLimit env inspector
      let next = max 0 (min scrollLimit requested)
      setUiOverlayInspectorScroll uiH next
      pure (Right (applied "overlay_inspector_scroll") { iirSelected = Just next })
    vertical delta = case oimView inspector of
      Just OverlayInspectorManagerView -> do
        moveUiOverlayInspectorSelection uiH delta
        pure (Right (applied "overlay_inspector_selection"))
      _ -> setScroll (oimScroll inspector + delta * 3)
    cycleFocus delta = do
      let focuses = case oimView inspector of
            Just OverlayInspectorManagerView ->
              [ OverlayInspectorCloseFocus
              , OverlayInspectorManagerFocus (selectedManagerIndex inspector)
              ]
            Just OverlayInspectorExportView ->
              [OverlayInspectorCloseFocus, OverlayInspectorCopyFocus, OverlayInspectorSaveFocus]
            Just OverlayInspectorImportView ->
              [OverlayInspectorCloseFocus, OverlayInspectorImportInputFocus, OverlayInspectorValidateFocus]
            _ -> [OverlayInspectorCloseFocus]
          currentIndex = maybe 0 id (findIndex (sameFocus (oimFocus inspector)) focuses)
          nextIndex = (currentIndex + delta + length focuses) `mod` length focuses
          next = focuses !! nextIndex
      setUiOverlayInspectorFocus uiH next
      pure (Right (applied "overlay_inspector_focus")
        { iirSelected = Just nextIndex
        , iirStartTextInput = next == OverlayInspectorImportInputFocus
        , iirStopTextInput = oimFocus inspector == OverlayInspectorImportInputFocus
            && next /= OverlayInspectorImportInputFocus
        })
    sameFocus current candidate = case (current, candidate) of
      (OverlayInspectorManagerFocus _, OverlayInspectorManagerFocus _) -> True
      _ -> current == candidate
    activateFocus = case oimFocus inspector of
      OverlayInspectorCloseFocus -> do
        closeUiOverlayInspector uiH
        pure (Right (applied "overlay_inspector_close") { iirStopTextInput = True })
      OverlayInspectorCopyFocus -> copyResult
      OverlayInspectorSaveFocus -> invokeOrDefer "overlay_inspector_save" WidgetOverlayInspectorSave
      OverlayInspectorValidateFocus -> invokeOrDefer
        "overlay_inspector_validate" WidgetOverlayInspectorValidate
      OverlayInspectorImportInputFocus ->
        pure (Right (applied "overlay_import_focus") { iirStartTextInput = True })
      OverlayInspectorManagerFocus _ -> pure (Right noEffect)
    invoke widget = do
      result <- iieRunService env "click_widget"
        (object ["widget_id" .= widgetIdToText widget])
      pure $ case result of
        Left err -> Left (serviceErrorText err)
        Right _ -> Right (applied "overlay_inspector_activate")
    invokeOrDefer action widget
      | iieDeferBlockingWidgets env = pure (Right (applied action)
          { iirDeferredWidget = Just widget })
      | otherwise = invoke widget
    invokeClipboard = do
      result <- iieRunService env "click_widget"
        (object ["widget_id" .= widgetIdToText WidgetOverlayInspectorCopy])
      pure $ case result of
        Left err -> Left (serviceErrorText err)
        Right (ServiceResponse (Object fields)) -> case KeyMap.lookup "clipboard" fields of
          Just (String clipboard) -> Right (applied "overlay_inspector_copy")
            { iirClipboard = Just clipboard }
          _ -> Left "overlay copy response omitted clipboard payload"
        Right _ -> Left "overlay copy response was not an object"
    copyResult = case oimExportPayload inspector of
      Nothing -> pure (Left "export payload is not loaded")
      Just _
        | iieDeferBlockingWidgets env -> pure (Right (applied "overlay_inspector_copy")
            { iirDeferredWidget = Just WidgetOverlayInspectorCopy })
        | otherwise -> invokeClipboard
    editImportBackspace =
      let cursor = oimImportCursor inspector
          current = oimImportText inspector
          next = if cursor <= 0 then current else Text.take (cursor - 1) current <> Text.drop cursor current
          nextCursor = max 0 (cursor - 1)
      in updateImport next nextCursor "overlay_import_backspace"
    editImportDelete =
      let cursor = oimImportCursor inspector
          current = oimImportText inspector
          next = Text.take cursor current <> Text.drop (cursor + 1) current
      in updateImport next cursor "overlay_import_delete"
    moveCursor delta = setCursor (oimImportCursor inspector + delta)
    setCursor requested =
      let next = max 0 (min (Text.length (oimImportText inspector)) requested)
      in do
        setUiOverlayInspectorImportText uiH (oimImportText inspector) next
        pure (Right (applied "overlay_import_cursor") { iirCursor = Just next })
    updateImport next cursor action = do
      setUiOverlayInspectorImportText uiH next cursor
      pure (Right (applied action) { iirText = Just next, iirCursor = Just cursor })

executeSeedKey :: InputIntentEnv -> UiState -> InputKey -> IO (Either Text InputIntentResult)
executeSeedKey env ui key =
  let uiH = ahUiHandle (iieActorHandles env)
  in case key of
    KeyEscape -> cancelSeed env ui
    KeyEnter -> commitSeed env ui
    KeyBackspace -> do
      let next = Text.dropEnd 1 (uiSeedInput ui)
      setUiSeedInput uiH next
      pure (Right (applied "seed_backspace") { iirText = Just next })
    KeyCharacter ch | ch >= '0' && ch <= '9' -> executeTextIntent env (Text.singleton ch)
    _ -> pure (Right noEffect)

commitSeed :: InputIntentEnv -> UiState -> IO (Either Text InputIntentResult)
commitSeed env ui = do
  let current = uiSeedInput ui
      seedValue :: Word64
      seedValue = case parseSeedText current of
        Just n | n >= 0 -> fromIntegral n
        Just n -> fromIntegral (abs n)
        Nothing -> uiSeed ui
  result <- iieRunService env "set_seed" (object ["seed" .= seedValue])
  setUiSeedEditing (ahUiHandle (iieActorHandles env)) False
  pure $ case result of
    Left err -> Left (serviceErrorText err)
    Right _ -> Right (applied "seed_confirm")
      { iirText = Just (Text.pack (show seedValue)), iirStopTextInput = True }

cancelSeed :: InputIntentEnv -> UiState -> IO (Either Text InputIntentResult)
cancelSeed env ui = do
  let uiH = ahUiHandle (iieActorHandles env)
      restored = Text.pack (show (uiSeed ui))
      nextMenu = case uiMenuMode ui of
        MenuNone -> MenuEscape
        _ -> MenuNone
  setUiSeedInput uiH restored
  setUiSeedEditing uiH False
  setUiMenuMode uiH nextMenu
  pure (Right (applied "seed_cancel")
    { iirText = Just restored
    , iirMenuMode = Just (menuModeText nextMenu)
    , iirStopTextInput = True
    })

executeDataFieldKey :: InputIntentEnv -> UiState -> Text -> InputKey -> IO (Either Text InputIntentResult)
executeDataFieldKey env ui field key =
  let dbs = uiDataBrowser ui
      cursor = dbsTextCursor dbs
      currentText = case Map.lookup field (dbsEditValues dbs) of
        Just (String t) -> t
        _ -> ""
      cursorAction pos = dataAction env (DataBrowserSetTextCursor pos)
        (applied "data_field_cursor") { iirCursor = Just (clampCursor currentText pos), iirField = Just field }
  in case key of
    KeyEscape -> blur
    KeyEnter -> blur
    KeyTab -> blur
    KeyBackspace -> dataAction env DataBrowserBackspace (applied "data_field_backspace") { iirField = Just field }
    KeyDelete -> dataAction env DataBrowserDeleteText (applied "data_field_delete") { iirField = Just field }
    KeyLeft -> cursorAction (cursor - 1)
    KeyRight -> cursorAction (cursor + 1)
    KeyHome -> cursorAction 0
    KeyEnd -> cursorAction (Text.length currentText)
    KeySpace -> insert " "
    KeyCharacter ch | ch >= ' ' -> insert (Text.singleton ch)
    _ -> pure (Right noEffect)
  where
    blur = dataAction env DataBrowserBlurField
      (applied "data_field_blur") { iirField = Just field, iirStopTextInput = True }
    insert txt = dataAction env (DataBrowserInsertText txt)
      (applied "data_field_text") { iirField = Just field, iirText = Just txt }

executeModalKey :: InputIntentEnv -> UiState -> InputKey -> IO (Either Text InputIntentResult)
executeModalKey env ui key =
  let uiH = ahUiHandle (iieActorHandles env)
  in if uiMenuMode ui == MenuWorldLoad && uiWorldDeleteConfirm ui
    then case key of
      KeyEscape -> cancelWorldDelete env
      KeyEnter -> confirmWorldDelete env ui
      _ -> pure (Right noEffect)
    else case key of
      KeyEscape -> do
        when (uiMenuMode ui == MenuWorldLoad) $ do
          setUiWorldDeleteConfirm uiH False
          setUiWorldDeleteTarget uiH Nothing
          setUiWorldDeleteError uiH Nothing
        setUiMenuMode uiH MenuNone
        pure (Right (applied "cancel_dialog") { iirStopTextInput = True })
      KeyEnter -> executeDialogConfirm env
      KeyDelete
        | uiMenuMode ui == MenuWorldLoad -> requestWorldDelete env ui
        | otherwise -> pure (Right noEffect)
      KeyBackspace -> case uiMenuMode ui of
        MenuPresetSave -> textBackspace (uiPresetInput ui) (setUiPresetInput uiH)
        MenuWorldSave -> textBackspace (uiWorldSaveInput ui) (setUiWorldSaveInput uiH)
        MenuPresetLoad -> filterBackspace (uiPresetFilter ui) (setUiPresetFilter uiH) (setUiPresetSelected uiH)
        MenuWorldLoad -> filterBackspace (uiWorldFilter ui) (setUiWorldFilter uiH) (setUiWorldSelected uiH)
        _ -> pure (Right noEffect)
      KeyUp -> moveSelection (-1)
      KeyDown -> moveSelection 1
      KeySpace -> executeTextIntent env " "
      KeyCharacter ch | ch >= ' ' -> executeTextIntent env (Text.singleton ch)
      _ -> pure (Right noEffect)
  where
    textBackspace current setter = do
      let next = Text.dropEnd 1 current
      setter next
      pure (Right (applied "modal_backspace") { iirText = Just next })
    filterBackspace current setter setSelection = do
      let next = Text.dropEnd 1 current
      setter next
      setSelection 0
      pure (Right (applied "modal_filter_backspace") { iirFilter = Just next, iirSelected = Just 0 })
    moveSelection delta =
      let uiH = ahUiHandle (iieActorHandles env)
          move current count setter = do
            let maxIdx = max 0 (count - 1)
                next = max 0 (min maxIdx (current + delta))
            setter next
            pure (Right (applied "modal_selection") { iirSelected = Just next })
      in case uiMenuMode ui of
        MenuPresetLoad -> move (uiPresetSelected ui) (length (filteredPresets ui)) (setUiPresetSelected uiH)
        MenuWorldLoad -> move (uiWorldSelected ui) (length (filteredWorlds ui)) (setUiWorldSelected uiH)
        _ -> pure (Right noEffect)

executeEditorKey :: InputIntentEnv -> UiState -> KeyModifiers -> InputKey -> IO (Either Text InputIntentResult)
executeEditorKey env ui mods key
  | kmCtrl mods && shortcutChar key == Just 'z' = service "editor_undo" Null "editor_undo"
  | kmCtrl mods && shortcutChar key == Just 'y' = service "editor_redo" Null "editor_redo"
  | otherwise = case key of
      KeyEscape -> disable
      KeyCharacter ch | toLower ch == 'e' -> disable
      KeyCharacter ch | Just tool <- toolForDigit (toLower ch) ->
        service "editor_set_tool" (object ["tool" .= tool]) "editor_tool"
      KeyLeftBracket -> radius (-1)
      KeyRightBracket -> radius 1
      KeyCharacter '[' -> radius (-1)
      KeyCharacter ']' -> radius 1
      _ -> pure (Right noEffect)
  where
    editor = uiEditor ui
    service method params action = serviceAction env method params (applied action)
    disable = service "editor_toggle" (object ["active" .= False]) "editor_close"
    radius delta =
      let next = max 0 (min 6 (brushRadius (editorBrush editor) + delta))
      in serviceAction env "editor_set_brush" (object ["radius" .= next])
           (applied "editor_brush_radius") { iirSelected = Just next }

executeGlobalKey :: InputIntentEnv -> UiState -> InputKey -> IO (Either Text InputIntentResult)
executeGlobalKey env ui key =
  let handles = iieActorHandles env
      uiH = ahUiHandle handles
  in case key of
    KeyEscape -> escapeCascade env ui
    KeyCharacter ch | toLower ch == 'g' -> serviceAction env "generate" Null (applied "generate")
    KeyCharacter ch | toLower ch == 'c' -> do
      setUiShowConfig uiH (not (uiShowConfig ui))
      pure (Right (applied "toggle_config"))
    KeyCharacter ch | toLower ch == 'e' -> serviceAction env "editor_toggle" (object ["active" .= True]) (applied "editor_open")
    KeyCharacter ch | toLower ch == 'l' -> do
      logSnap <- iieGetLog env
      setLogCollapsed (ahLogHandle handles) (not (lsCollapsed logSnap))
      pure (Right (applied "toggle_log"))
    KeyUp -> bump 1
    KeyDown -> bump (-1)
    KeyCharacter ch | Just hotkey <- viewHotkeyForChar (toLower ch) -> applyViewHotkey env ui hotkey
    _ -> pure (Right noEffect)
  where
    bump delta =
      let next = uiSeed ui + fromIntegral delta
      in serviceAction env "set_seed" (object ["seed" .= next])
           (applied "bump_seed") { iirText = Just (Text.pack (show next)) }

escapeCascade :: InputIntentEnv -> UiState -> IO (Either Text InputIntentResult)
escapeCascade env ui =
  let dbs = uiDataBrowser ui
      uiH = ahUiHandle (iieActorHandles env)
  in if dataDeleteConfirmationVisible ui
    then dataAction env DataBrowserCancelDelete (applied "cancel_delete_confirm")
    else if dbsEditMode dbs || dbsCreateMode dbs
      then dataAction env DataBrowserCancelEdit
        (applied (if dbsCreateMode dbs then "cancel_create" else "cancel_edit")) { iirStopTextInput = True }
      else case dbsSelectedRecord dbs of
        Just _ -> dataAction env DataBrowserDismissRecord (applied "dismiss_record")
        Nothing -> case uiContextHex ui of
          Just _ -> do
            setUiContextHex uiH Nothing
            setUiContextPos uiH Nothing
            setUiMenuMode uiH MenuEscape
            pure (Right (applied "close_context") { iirMenuMode = Just "escape_menu" })
          Nothing -> case uiMenuMode ui of
            MenuNone -> do
              setUiMenuMode uiH MenuEscape
              pure (Right (applied "open_escape_menu") { iirMenuMode = Just "escape_menu" })
            _ -> do
              setUiMenuMode uiH MenuNone
              pure (Right (applied "close_menu") { iirMenuMode = Just "none" })

serviceAction :: InputIntentEnv -> Text -> Value -> InputIntentResult -> IO (Either Text InputIntentResult)
serviceAction env method params result = do
  response <- iieRunService env method params
  pure $ case response of
    Left err -> Left (serviceErrorText err)
    Right _ -> Right result

dataAction :: InputIntentEnv -> DataBrowserAppAction -> InputIntentResult -> IO (Either Text InputIntentResult)
dataAction env action result = do
  response <- iieApplyDataBrowser env action
  pure (result <$ response)

selectedManagerIndex :: OverlayInspectorModel -> Int
selectedManagerIndex inspector =
  case oimSelectedOverlay inspector >>= \selected ->
      findIndex (== selected) (oimOverlayNames inspector) of
    Just index -> index
    Nothing -> 0

focusedOverlayImport :: UiState -> Maybe OverlayInspectorModel
focusedOverlayImport ui =
  let inspector = uiOverlayInspector ui
  in if uiMenuMode ui == MenuOverlayInspector
        && oimView inspector == Just OverlayInspectorImportView
        && oimFocus inspector == OverlayInspectorImportInputFocus
       then Just inspector
       else Nothing

dataDeleteConfirmationVisible :: UiState -> Bool
dataDeleteConfirmationVisible ui =
  uiMenuMode ui == MenuNone
    && uiShowConfig ui
    && uiConfigTab ui == ConfigData
    && dbsDeleteConfirm (uiDataBrowser ui)

focusedDataField :: UiState -> Maybe Text
focusedDataField ui =
  let dbs = uiDataBrowser ui
  in case dbsFocusedField dbs of
    Just field | dbsEditMode dbs || dbsCreateMode dbs -> Just field
    _ -> Nothing

filteredPresets :: UiState -> [Text]
filteredPresets ui =
  filter (presetCatalogueMatches (uiPresetFilter ui)) (uiPresetList ui)

filteredWorlds :: UiState -> [WorldSaveManifest]
filteredWorlds ui =
  let needle = Text.toLower (uiWorldFilter ui)
  in filter (Text.isInfixOf needle . Text.toLower . wsmName) (uiWorldList ui)

selectedWorld :: UiState -> Maybe WorldSaveManifest
selectedWorld ui = atIndex (uiWorldSelected ui) (filteredWorlds ui)

requestWorldDelete :: InputIntentEnv -> UiState -> IO (Either Text InputIntentResult)
requestWorldDelete env ui = case selectedWorld ui of
  Nothing -> pure (Left "select a saved world before deleting")
  Just manifest -> do
    let uiH = ahUiHandle (iieActorHandles env)
    setUiWorldDeleteConfirm uiH True
    setUiWorldDeleteTarget uiH (Just (wsmName manifest))
    setUiWorldDeleteError uiH Nothing
    _ <- publishUiMutation (iieActorHandles env)
    pure (Right (applied "world_delete_request") { iirName = Just (wsmName manifest) })

cancelWorldDelete :: InputIntentEnv -> IO (Either Text InputIntentResult)
cancelWorldDelete env = do
  let uiH = ahUiHandle (iieActorHandles env)
  setUiWorldDeleteConfirm uiH False
  setUiWorldDeleteTarget uiH Nothing
  setUiWorldDeleteError uiH Nothing
  _ <- publishUiMutation (iieActorHandles env)
  pure (Right (applied "world_delete_cancel"))

confirmWorldDelete :: InputIntentEnv -> UiState -> IO (Either Text InputIntentResult)
confirmWorldDelete env ui = case uiWorldDeleteTarget ui of
  Nothing -> do
    let message = "Delete failed: the confirmed saved world is no longer available"
    setUiWorldDeleteError (ahUiHandle (iieActorHandles env)) (Just message)
    _ <- publishUiMutation (iieActorHandles env)
    pure (Left message)
  Just name -> do
    let uiH = ahUiHandle (iieActorHandles env)
    result <- iieRunService env "delete_world" (object ["name" .= name])
    case result of
      Right _ -> do
        setUiWorldDeleteConfirm uiH False
        setUiWorldDeleteTarget uiH Nothing
        setUiWorldDeleteError uiH Nothing
        _ <- publishUiMutation (iieActorHandles env)
        pure (Right (applied "world_delete_confirm")
          { iirName = Just name, iirSelected = Just 0 })
      Left err -> do
        let message = "Delete failed: " <> serviceErrorText err <> " (world: " <> name <> ")"
        setUiWorldDeleteError uiH (Just message)
        _ <- publishUiMutation (iieActorHandles env)
        pure (Left message)

atIndex :: Int -> [a] -> Maybe a
atIndex index values
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value : _ -> Just value
      [] -> Nothing

clampCursor :: Text -> Int -> Int
clampCursor txt = max 0 . min (Text.length txt)

shortcutChar :: InputKey -> Maybe Char
shortcutChar (KeyCharacter ch) = Just (toLower ch)
shortcutChar _ = Nothing

toolForDigit :: Char -> Maybe Text
toolForDigit ch = case ch of
  '1' -> Just "raise"
  '2' -> Just "lower"
  '3' -> Just "smooth"
  '4' -> Just "flatten"
  '5' -> Just "noise"
  '6' -> Just "paint_biome"
  '7' -> Just "paint_form"
  '8' -> Just "set_hardness"
  '9' -> Just "erode"
  _ -> Nothing

data ViewHotkey
  = SetBase BaseViewMode
  | SetOverlay (Maybe SkyOverlayMode)
  | CycleOverlay
  | CycleBasis

viewHotkeyForChar :: Char -> Maybe ViewHotkey
viewHotkeyForChar ch = case ch of
  '1' -> Just (SetBase BaseViewElevation)
  '2' -> Just (SetBase BaseViewBiome)
  '3' -> Just (SetBase BaseViewMoisture)
  '4' -> Just (SetBase BaseViewVegetation)
  '5' -> Just (SetBase BaseViewTerrainForm)
  '6' -> Just (SetBase BaseViewPlateId)
  '7' -> Just (SetBase BaseViewPlateBoundary)
  '8' -> Just (SetBase BaseViewPlateHardness)
  '9' -> Just (SetBase BaseViewPlateCrust)
  '0' -> Just (SetBase BaseViewPlateAge)
  'h' -> Just (SetBase BaseViewPlateHeight)
  'v' -> Just (SetBase BaseViewPlateVelocity)
  'n' -> Just (SetOverlay Nothing)
  't' -> Just (SetOverlay (Just SkyOverlayWeatherTemperature))
  'p' -> Just (SetOverlay (Just SkyOverlayPrecipitation))
  'k' -> Just (SetOverlay (Just SkyOverlayCloud))
  'o' -> Just CycleOverlay
  'b' -> Just CycleBasis
  _ -> Nothing

applyViewHotkey :: InputIntentEnv -> UiState -> ViewHotkey -> IO (Either Text InputIntentResult)
applyViewHotkey env ui hotkey =
  let selection = effectiveViewSelection ui
      setView params action = serviceAction env "set_view" params (applied action)
  in case hotkey of
    SetBase base -> setView (object ["base_mode" .= baseModeText base]) "view_base"
    SetOverlay overlay -> setView (object ["overlay_mode" .= overlayText overlay]) "view_overlay"
    CycleOverlay -> setView
      (object ["overlay_mode" .= overlayText (nextBuiltinOverlay selection)])
      "view_overlay_cycle"
    CycleBasis -> case lvsSkyOverlay selection of
      Just (SkyOverlayPlugin _ _) -> pure (Right noEffect)
      Just _ -> setView
        (object ["weather_basis" .= weatherBasisText (nextWeatherBasis (lvsWeatherBasis selection))])
        "view_weather_basis"
      Nothing -> pure (Right noEffect)

baseModeText :: BaseViewMode -> Text
baseModeText base = case base of
  BaseViewElevation -> "elevation"
  BaseViewBiome -> "biome"
  BaseViewMoisture -> "moisture"
  BaseViewVegetation -> "vegetation"
  BaseViewTerrainForm -> "terrain_form"
  BaseViewPlateId -> "plate_id"
  BaseViewPlateBoundary -> "plate_boundary"
  BaseViewPlateHardness -> "plate_hardness"
  BaseViewPlateCrust -> "plate_crust"
  BaseViewPlateAge -> "plate_age"
  BaseViewPlateHeight -> "plate_height"
  BaseViewPlateVelocity -> "plate_velocity"

overlayText :: Maybe SkyOverlayMode -> Maybe Text
overlayText overlay = case overlay of
  Nothing -> Nothing
  Just SkyOverlayWeatherTemperature -> Just "weather"
  Just SkyOverlayPrecipitation -> Just "precipitation"
  Just SkyOverlayCloud -> Just "cloud"
  Just (SkyOverlayPlugin _ _) -> Nothing

weatherBasisText :: WeatherBasis -> Text
weatherBasisText WeatherBasisAverage = "average"
weatherBasisText WeatherBasisCurrent = "current"

menuModeText :: UiMenuMode -> Text
menuModeText mode = case mode of
  MenuNone -> "none"
  MenuEscape -> "escape_menu"
  MenuPresetSave -> "preset_save"
  MenuPresetLoad -> "preset_load"
  MenuWorldSave -> "world_save"
  MenuWorldLoad -> "world_load"
  MenuOverlayInspector -> "overlay_inspector"
