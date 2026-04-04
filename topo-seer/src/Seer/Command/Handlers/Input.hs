{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for dialog and text input interaction:
-- @get_dialog_state@, @set_dialog_text@, @dialog_confirm@,
-- @dialog_cancel@, @send_key@.
--
-- These let an LLM interact with modal dialogs (preset save\/load,
-- world save\/load) and text-input fields (seed editing, data browser
-- field editing) in the same way a user would with keyboard input.
module Seer.Command.Handlers.Input
  ( handleGetDialogState
  , handleSetDialogText
  , handleDialogConfirm
  , handleDialogCancel
  , handleSendKey
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import Data.Char (isPrint)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Actor.UI.Setters
  ( setUiMenuMode
  , setUiPresetInput
  , setUiPresetFilter
  , setUiPresetSelected
  , setUiWorldSaveInput
  , setUiWorldFilter
  , setUiWorldSelected
  , setUiSeedEditing
  , setUiSeedInput
  , setUiDataBrowser
  )
import Actor.UI.State
  ( DataBrowserState(..)
  , UiMenuMode(..)
  , UiState(..)
  , readUiSnapshotRef
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- --------------------------------------------------------------------------
-- get_dialog_state
-- --------------------------------------------------------------------------

-- | Handle @get_dialog_state@ — return the currently active dialog and its content.
--
-- Returns the menu mode, any active text input fields, and relevant state.
handleGetDialogState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetDialogState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let dbs = uiDataBrowser ui
      menuStr = case uiMenuMode ui of
        MenuNone       -> "none" :: Text
        MenuEscape     -> "escape_menu"
        MenuPresetSave -> "preset_save"
        MenuPresetLoad -> "preset_load"
        MenuWorldSave  -> "world_save"
        MenuWorldLoad  -> "world_load"
      dbsField = case dbsFocusedField dbs of
        Just f | dbsEditMode dbs || dbsCreateMode dbs -> Just f
        _ -> Nothing
  pure $ okResponse reqId $ object
    [ "menu_mode"       .= menuStr
    , "seed_editing"    .= uiSeedEditing ui
    , "preset_input"    .= uiPresetInput ui
    , "preset_filter"   .= uiPresetFilter ui
    , "preset_selected" .= uiPresetSelected ui
    , "preset_count"    .= length (uiPresetList ui)
    , "world_save_input" .= uiWorldSaveInput ui
    , "world_filter"    .= uiWorldFilter ui
    , "world_selected"  .= uiWorldSelected ui
    , "world_count"     .= length (uiWorldList ui)
    , "data_focused_field" .= dbsField
    , "data_edit_mode"  .= dbsEditMode dbs
    , "data_create_mode" .= dbsCreateMode dbs
    , "data_text_cursor" .= dbsTextCursor dbs
    ]

-- --------------------------------------------------------------------------
-- set_dialog_text
-- --------------------------------------------------------------------------

-- | Handle @set_dialog_text@ — set the text content of the active input field.
--
-- Params: @{ "text": string, "target"?: string }@
--
-- If @target@ is omitted, auto-detects based on current dialog state:
-- * MenuPresetSave → preset name input
-- * MenuWorldSave → world name input
-- * MenuPresetLoad → preset filter
-- * MenuWorldLoad → world filter
-- * seed_editing → seed value (parsed as integer)
-- * data browser focused field → field text
--
-- Explicit @target@ values: @"preset_input"@, @"preset_filter"@,
-- @"world_input"@, @"world_filter"@, @"seed"@, @"data_field"@.
handleSetDialogText :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetDialogText ctx reqId params = do
  case Aeson.parseMaybe parseSetText params of
    Nothing ->
      pure $ errResponse reqId "missing 'text' parameter"
    Just (txt, mTarget) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let target = case mTarget of
            Just t  -> t
            Nothing -> autoDetectTarget ui
      case target of
        "preset_input" -> do
          setUiPresetInput uiH txt
          pure $ okResponse reqId $ object
            [ "target" .= ("preset_input" :: Text)
            , "text"   .= txt
            ]
        "preset_filter" -> do
          setUiPresetFilter uiH txt
          setUiPresetSelected uiH 0
          pure $ okResponse reqId $ object
            [ "target" .= ("preset_filter" :: Text)
            , "text"   .= txt
            ]
        "world_input" -> do
          setUiWorldSaveInput uiH txt
          pure $ okResponse reqId $ object
            [ "target" .= ("world_input" :: Text)
            , "text"   .= txt
            ]
        "world_filter" -> do
          setUiWorldFilter uiH txt
          setUiWorldSelected uiH 0
          pure $ okResponse reqId $ object
            [ "target" .= ("world_filter" :: Text)
            , "text"   .= txt
            ]
        "seed" -> do
          setUiSeedInput uiH txt
          pure $ okResponse reqId $ object
            [ "target" .= ("seed" :: Text)
            , "text"   .= txt
            ]
        "data_field" -> do
          let dbs = uiDataBrowser ui
          case dbsFocusedField dbs of
            Just path -> do
              let filtered = Text.filter isPrint txt
                  newDbs = dbs
                    { dbsEditValues = Map.insert path (String filtered) (dbsEditValues dbs)
                    , dbsTextCursor = Text.length filtered
                    }
              setUiDataBrowser uiH newDbs
              pure $ okResponse reqId $ object
                [ "target" .= ("data_field" :: Text)
                , "field"  .= path
                , "text"   .= filtered
                ]
            Nothing ->
              pure $ errResponse reqId "no data browser field is currently focused"
        _ ->
          pure $ errResponse reqId ("unknown target: " <> target)

-- --------------------------------------------------------------------------
-- dialog_confirm
-- --------------------------------------------------------------------------

-- | Handle @dialog_confirm@ — confirm / submit the active dialog (Enter).
--
-- Behaviour depends on the active dialog mode:
-- * MenuPresetSave → saves preset with current input text
-- * MenuWorldSave → saves world with current input text
-- * MenuPresetLoad → loads selected preset
-- * MenuWorldLoad → loads selected world
-- * data browser field → unfocuses the field (confirms edit)
-- * seed editing → stops seed editing
-- * all others → no-op
--
-- Note: For preset/world save/load, the actual file operations are
-- performed by the dedicated @save_preset@, @load_preset@, etc. IPC
-- commands.  This handler only manipulates the dialog UI state
-- (closes the dialog, clears input), matching what Enter does in the UI.
-- For complex operations, use the dedicated commands instead.
handleDialogConfirm :: CommandContext -> Int -> Value -> IO SeerResponse
handleDialogConfirm ctx reqId _params = do
  let handles = ccActorHandles ctx
      uiH = ahUiHandle handles
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let dbs = uiDataBrowser ui
  case dbsFocusedField dbs of
    Just _path | dbsEditMode dbs || dbsCreateMode dbs -> do
      -- Unfocus the field (accept text)
      let newDbs = dbs { dbsFocusedField = Nothing, dbsTextCursor = 0 }
      setUiDataBrowser uiH newDbs
      pure $ okResponse reqId $ object
        [ "action" .= ("data_field_confirm" :: Text) ]
    _ ->
      case uiMenuMode ui of
        MenuPresetSave -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("preset_save_confirm" :: Text)
            , "name"   .= uiPresetInput ui
            ]
        MenuPresetLoad -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("preset_load_confirm" :: Text)
            , "selected" .= uiPresetSelected ui
            ]
        MenuWorldSave -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("world_save_confirm" :: Text)
            , "name"   .= uiWorldSaveInput ui
            ]
        MenuWorldLoad -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("world_load_confirm" :: Text)
            , "selected" .= uiWorldSelected ui
            ]
        _ -> do
          when (uiSeedEditing ui) $ do
            setUiSeedEditing uiH False
          pure $ okResponse reqId $ object
            [ "action" .= ("confirm" :: Text)
            , "menu_mode" .= show (uiMenuMode ui)
            ]

-- --------------------------------------------------------------------------
-- dialog_cancel
-- --------------------------------------------------------------------------

-- | Handle @dialog_cancel@ — cancel the active dialog (Escape).
--
-- Dismisses the current modal, data browser popover, or edit mode,
-- following the same cascade as Escape in the UI.
handleDialogCancel :: CommandContext -> Int -> Value -> IO SeerResponse
handleDialogCancel ctx reqId _params = do
  let handles = ccActorHandles ctx
      uiH = ahUiHandle handles
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let dbs = uiDataBrowser ui
  -- Priority cascade matching Events.hs closeContextOrMenu
  if dbsDeleteConfirm dbs
    then do
      let newDbs = dbs { dbsDeleteConfirm = False }
      setUiDataBrowser uiH newDbs
      pure $ okResponse reqId $ object
        [ "action" .= ("cancel_delete_confirm" :: Text) ]
    else if dbsEditMode dbs || dbsCreateMode dbs
      then do
        let newDbs = if dbsCreateMode dbs
              then dbs
                { dbsEditMode         = False
                , dbsCreateMode       = False
                , dbsEditValues       = Map.empty
                , dbsFocusedField     = Nothing
                , dbsTextCursor       = 0
                , dbsSelectedRecord   = Nothing
                , dbsSelectedRecordKey = Nothing
                , dbsSelectedRowIndex = Nothing
                }
              else dbs
                { dbsEditMode     = False
                , dbsEditValues   = Map.empty
                , dbsFocusedField = Nothing
                , dbsTextCursor   = 0
                }
        setUiDataBrowser uiH newDbs
        pure $ okResponse reqId $ object
          [ "action" .= (if dbsCreateMode dbs
                          then "cancel_create" :: Text
                          else "cancel_edit") ]
      else case uiMenuMode ui of
        MenuNone -> do
          setUiMenuMode uiH MenuEscape
          pure $ okResponse reqId $ object
            [ "action" .= ("open_escape_menu" :: Text) ]
        MenuEscape -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("close_escape_menu" :: Text) ]
        _ -> do
          setUiMenuMode uiH MenuNone
          pure $ okResponse reqId $ object
            [ "action" .= ("cancel_dialog" :: Text) ]

-- --------------------------------------------------------------------------
-- send_key
-- --------------------------------------------------------------------------

-- | Handle @send_key@ — simulate a keyboard key press.
--
-- Params: @{ "key": string }@
--
-- Supported key names: @"escape"@, @"return"@/@"enter"@, @"backspace"@,
-- @"delete"@, @"tab"@, @"up"@, @"down"@, @"left"@, @"right"@,
-- @"home"@, @"end"@, @"space"@, and single printable characters.
--
-- For text input, prefer @set_dialog_text@ which directly sets the
-- full text.  @send_key@ is useful for navigation (up/down in lists)
-- and triggering specific key-driven actions.
handleSendKey :: CommandContext -> Int -> Value -> IO SeerResponse
handleSendKey ctx reqId params = do
  case Aeson.parseMaybe parseKey params of
    Nothing ->
      pure $ errResponse reqId "missing 'key' parameter"
    Just keyName -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let dbs = uiDataBrowser ui
          keyLower = Text.toLower keyName
      case keyLower of
        -- Navigation keys for list dialogs
        "up" -> do
          case uiMenuMode ui of
            MenuPresetLoad -> do
              let sel = max 0 (uiPresetSelected ui - 1)
              setUiPresetSelected uiH sel
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "selected" .= sel ]
            MenuWorldLoad -> do
              let sel = max 0 (uiWorldSelected ui - 1)
              setUiWorldSelected uiH sel
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "selected" .= sel ]
            _ -> do
              -- Data field cursor: move cursor left
              handleDataFieldCursorKey ui dbs uiH "left" reqId keyName

        "down" -> do
          case uiMenuMode ui of
            MenuPresetLoad -> do
              let maxIdx = max 0 (length (uiPresetList ui) - 1)
                  sel = min maxIdx (uiPresetSelected ui + 1)
              setUiPresetSelected uiH sel
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "selected" .= sel ]
            MenuWorldLoad -> do
              let maxIdx = max 0 (length (uiWorldList ui) - 1)
                  sel = min maxIdx (uiWorldSelected ui + 1)
              setUiWorldSelected uiH sel
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "selected" .= sel ]
            _ ->
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

        "backspace" -> do
          -- Backspace in active text input
          case uiMenuMode ui of
            MenuPresetSave -> do
              setUiPresetInput uiH (Text.dropEnd 1 (uiPresetInput ui))
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "text" .= Text.dropEnd 1 (uiPresetInput ui) ]
            MenuWorldSave -> do
              setUiWorldSaveInput uiH (Text.dropEnd 1 (uiWorldSaveInput ui))
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "text" .= Text.dropEnd 1 (uiWorldSaveInput ui) ]
            MenuPresetLoad -> do
              let newFilter = Text.dropEnd 1 (uiPresetFilter ui)
              setUiPresetFilter uiH newFilter
              setUiPresetSelected uiH 0
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "filter" .= newFilter ]
            MenuWorldLoad -> do
              let newFilter = Text.dropEnd 1 (uiWorldFilter ui)
              setUiWorldFilter uiH newFilter
              setUiWorldSelected uiH 0
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "filter" .= newFilter ]
            _ ->
              handleDataFieldBackspace ui dbs uiH reqId keyName

        "delete" ->
          handleDataFieldDeleteKey ui dbs uiH reqId keyName

        "left" ->
          handleDataFieldCursorKey ui dbs uiH "left" reqId keyName

        "right" ->
          handleDataFieldCursorKey ui dbs uiH "right" reqId keyName

        "home" ->
          handleDataFieldCursorKey ui dbs uiH "home" reqId keyName

        "end" ->
          handleDataFieldCursorKey ui dbs uiH "end" reqId keyName

        "escape" -> do
          -- Delegate to dialog_cancel
          handleDialogCancel ctx reqId (object [])

        "return" -> handleDialogConfirm ctx reqId (object [])
        "enter"  -> handleDialogConfirm ctx reqId (object [])

        "tab" -> do
          -- Tab unfocuses data browser field
          case dbsFocusedField dbs of
            Just _path | dbsEditMode dbs || dbsCreateMode dbs -> do
              let newDbs = dbs { dbsFocusedField = Nothing, dbsTextCursor = 0 }
              setUiDataBrowser uiH newDbs
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "action" .= ("unfocus_field" :: Text) ]
            _ ->
              pure $ okResponse reqId $ object
                [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

        _ ->
          -- Single character → type into active text field
          let ch = Text.filter isPrint keyName
          in if Text.length ch == 1
            then do
              case uiMenuMode ui of
                MenuPresetSave -> do
                  let newText = uiPresetInput ui <> ch
                  setUiPresetInput uiH newText
                  pure $ okResponse reqId $ object
                    [ "key" .= keyName, "text" .= newText ]
                MenuWorldSave -> do
                  let newText = uiWorldSaveInput ui <> ch
                  setUiWorldSaveInput uiH newText
                  pure $ okResponse reqId $ object
                    [ "key" .= keyName, "text" .= newText ]
                MenuPresetLoad -> do
                  let newFilter = uiPresetFilter ui <> ch
                  setUiPresetFilter uiH newFilter
                  setUiPresetSelected uiH 0
                  pure $ okResponse reqId $ object
                    [ "key" .= keyName, "filter" .= newFilter ]
                MenuWorldLoad -> do
                  let newFilter = uiWorldFilter ui <> ch
                  setUiWorldFilter uiH newFilter
                  setUiWorldSelected uiH 0
                  pure $ okResponse reqId $ object
                    [ "key" .= keyName, "filter" .= newFilter ]
                _ ->
                  handleDataFieldChar ui dbs uiH ch reqId keyName
            else
              pure $ errResponse reqId ("unrecognized key: " <> keyName)

-- --------------------------------------------------------------------------
-- Data field helpers (no type signatures — UiHandle is module-internal)
-- --------------------------------------------------------------------------

handleDataFieldBackspace _ui dbs uiH reqId keyName =
  case dbsFocusedField dbs of
    Just path | dbsEditMode dbs || dbsCreateMode dbs -> do
      let cursor = dbsTextCursor dbs
          editVals = dbsEditValues dbs
          currentText = case Map.lookup path editVals of
            Just (String t) -> t
            _ -> ""
      if cursor > 0
        then do
          let (before, after) = Text.splitAt cursor currentText
              newText = Text.dropEnd 1 before <> after
              newDbs = dbs
                { dbsEditValues = Map.insert path (String newText) editVals
                , dbsTextCursor = cursor - 1
                }
          setUiDataBrowser uiH newDbs
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "field" .= path, "text" .= newText ]
        else
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]
    _ ->
      pure $ okResponse reqId $ object
        [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

handleDataFieldDeleteKey _ui dbs uiH reqId keyName =
  case dbsFocusedField dbs of
    Just path | dbsEditMode dbs || dbsCreateMode dbs -> do
      let cursor = dbsTextCursor dbs
          editVals = dbsEditValues dbs
          currentText = case Map.lookup path editVals of
            Just (String t) -> t
            _ -> ""
      if cursor < Text.length currentText
        then do
          let (before, after) = Text.splitAt cursor currentText
              newText = before <> Text.drop 1 after
              newDbs = dbs
                { dbsEditValues = Map.insert path (String newText) editVals
                }
          setUiDataBrowser uiH newDbs
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "field" .= path, "text" .= newText ]
        else
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]
    _ ->
      pure $ okResponse reqId $ object
        [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

handleDataFieldCursorKey _ui dbs uiH direction reqId keyName =
  case dbsFocusedField dbs of
    Just path | dbsEditMode dbs || dbsCreateMode dbs -> do
      let cursor = dbsTextCursor dbs
          editVals = dbsEditValues dbs
          currentText = case Map.lookup path editVals of
            Just (String t) -> t
            _ -> ""
          newCursor = case direction of
            "left"  -> max 0 (cursor - 1)
            "right" -> min (Text.length currentText) (cursor + 1)
            "home"  -> 0
            "end"   -> Text.length currentText
            _       -> cursor
      if newCursor /= cursor
        then do
          let newDbs = dbs { dbsTextCursor = newCursor }
          setUiDataBrowser uiH newDbs
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "cursor" .= newCursor ]
        else
          pure $ okResponse reqId $ object
            [ "key" .= keyName, "cursor" .= cursor ]
    _ ->
      pure $ okResponse reqId $ object
        [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

handleDataFieldChar _ui dbs uiH ch reqId keyName =
  case dbsFocusedField dbs of
    Just path | dbsEditMode dbs || dbsCreateMode dbs -> do
      let cursor = dbsTextCursor dbs
          editVals = dbsEditValues dbs
          currentText = case Map.lookup path editVals of
            Just (String t) -> t
            _ -> ""
          (before, after) = Text.splitAt cursor currentText
          newText = before <> ch <> after
          newCursor = cursor + Text.length ch
          newDbs = dbs
            { dbsEditValues = Map.insert path (String newText) editVals
            , dbsTextCursor = newCursor
            }
      setUiDataBrowser uiH newDbs
      pure $ okResponse reqId $ object
        [ "key" .= keyName, "field" .= path, "text" .= newText ]
    _ ->
      pure $ okResponse reqId $ object
        [ "key" .= keyName, "action" .= ("no_effect" :: Text) ]

-- --------------------------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------------------------

autoDetectTarget :: UiState -> Text
autoDetectTarget ui = case uiMenuMode ui of
  MenuPresetSave -> "preset_input"
  MenuWorldSave  -> "world_input"
  MenuPresetLoad -> "preset_filter"
  MenuWorldLoad  -> "world_filter"
  _ | uiSeedEditing ui -> "seed"
    | otherwise ->
        let dbs = uiDataBrowser ui
        in case dbsFocusedField dbs of
          Just _ | dbsEditMode dbs || dbsCreateMode dbs -> "data_field"
          _ -> "none"

-- --------------------------------------------------------------------------
-- Parsing helpers
-- --------------------------------------------------------------------------

parseSetText :: Value -> Aeson.Parser (Text, Maybe Text)
parseSetText = Aeson.withObject "set_dialog_text" $ \o ->
  (,) <$> o .: "text" <*> o .:? "target"

parseKey :: Value -> Aeson.Parser Text
parseKey = Aeson.withObject "send_key" $ \o ->
  o .: "key"
