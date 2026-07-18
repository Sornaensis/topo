{-# LANGUAGE OverloadedStrings #-}

-- | Command handlers for dialog and text input interaction:
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
  , handleDialogConfirmWithRunner
  , handleDialogCancel
  , handleDialogCancelWithRunner
  , handleSendKey
  , handleSendKeyWithRunner
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as Aeson
import Data.Char (isPrint)
import Data.Text (Text)
import qualified Data.Text as Text
import Linear (V2(..))

import Actor.UI.Setters
  ( setUiPresetInput
  , setUiPresetFilter
  , setUiPresetSelected
  , setUiWorldSaveInput
  , setUiWorldFilter
  , setUiWorldSelected
  , setUiSeedInput
  , setUiOverlayInspectorImportText
  )
import Actor.Log (getLogSnapshot)
import Actor.UI.State
  ( DataBrowserState(..)
  , UiMenuMode(..)
  , UiState(..)
  , getUiSnapshot
  , readUiSnapshotRef
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.Config.PresetCatalogue (presetCatalogueMatches)
import Seer.DataBrowser.Executor (submitDataBrowserAction)
import qualified Seer.DataBrowser.Lifecycle as DataBrowser
import Seer.DataBrowser.Model (DataBrowserBeginResult(..))
import Seer.Draw.OverlayInspector (overlayInspectorViewScrollLimit)
import Seer.Input.Intent
  ( InputIntentEnv(..)
  , InputIntentResult(..)
  , KeyModifiers(..)
  , executeDialogCancel
  , executeDialogConfirm
  , executeKeyIntent
  , parseInputKey
  , parseKeyModifiers
  )
import Seer.OverlayInspector.Model (overlayInspectorModelValue)
import Seer.Service.Types (ServiceError(..), ServiceResult)
import Seer.Command.Types (SeerResponse, okResponse, errResponse)
import UI.Layout (layoutForSeed)

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
        MenuOverlayInspector -> "overlay_inspector"
      dbsField = case dbsFocusedField dbs of
        Just f | dbsEditMode dbs || dbsCreateMode dbs -> Just f
        _ -> Nothing
  pure $ okResponse reqId $ object
    [ "menu_mode"       .= menuStr
    , "seed_editing"    .= uiSeedEditing ui
    , "preset_input"    .= uiPresetInput ui
    , "preset_filter"   .= uiPresetFilter ui
    , "preset_selected" .= uiPresetSelected ui
    , "preset_count"    .= length (filter (presetCatalogueMatches (uiPresetFilter ui)) (uiPresetList ui))
    , "world_save_input" .= uiWorldSaveInput ui
    , "world_filter"    .= uiWorldFilter ui
    , "world_selected"  .= uiWorldSelected ui
    , "world_count"     .= length (uiWorldList ui)
    , "world_delete_confirm" .= uiWorldDeleteConfirm ui
    , "world_delete_target" .= uiWorldDeleteTarget ui
    , "world_delete_error" .= uiWorldDeleteError ui
    , "data_focused_field" .= dbsField
    , "data_edit_mode"  .= dbsEditMode dbs
    , "data_create_mode" .= dbsCreateMode dbs
    , "data_text_cursor" .= dbsTextCursor dbs
    , "overlay_inspector" .= overlayInspectorModelValue (uiOverlayInspector ui)
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
        "overlay_import" -> do
          let filtered = Text.filter isPrint txt
          setUiOverlayInspectorImportText uiH filtered (Text.length filtered)
          pure $ okResponse reqId $ object
            [ "target" .= ("overlay_import" :: Text)
            , "text" .= filtered
            ]
        "data_field" -> do
          let dbs = uiDataBrowser ui
          case dbsFocusedField dbs of
            Just path -> do
              let filtered = Text.filter isPrint txt
              applied <- runDataBrowserOwnerAction ctx (DataBrowser.DataBrowserReplaceText filtered)
              pure $ case applied of
                Left err -> errResponse reqId err
                Right () -> okResponse reqId $ object
                  [ "target" .= ("data_field" :: Text)
                  , "field"  .= path
                  , "text"   .= filtered
                  ]
            Nothing ->
              pure $ errResponse reqId "no data browser field is currently focused"
        _ ->
          pure $ errResponse reqId ("unknown target: " <> target)

-- --------------------------------------------------------------------------
-- Shared dialog and keyboard execution
-- --------------------------------------------------------------------------

type InputServiceRunner = CommandContext -> Text -> Value -> IO ServiceResult

-- | Compatibility entry point.  AppService binds the runner-aware variant so
-- dialog confirmation can invoke real persistence operations.
handleDialogConfirm :: CommandContext -> Int -> Value -> IO SeerResponse
handleDialogConfirm = handleDialogConfirmWithRunner unavailableRunner

handleDialogConfirmWithRunner :: InputServiceRunner -> CommandContext -> Int -> Value -> IO SeerResponse
handleDialogConfirmWithRunner runner ctx reqId _params =
  executeDialogConfirm (inputIntentEnv runner ctx) >>= pure . intentResponse reqId Nothing []

handleDialogCancel :: CommandContext -> Int -> Value -> IO SeerResponse
handleDialogCancel = handleDialogCancelWithRunner unavailableRunner

handleDialogCancelWithRunner :: InputServiceRunner -> CommandContext -> Int -> Value -> IO SeerResponse
handleDialogCancelWithRunner runner ctx reqId _params =
  executeDialogCancel (inputIntentEnv runner ctx) >>= pure . intentResponse reqId Nothing []

-- | Execute the same normalized key intent as the SDL path.  Optional
-- modifiers make editor undo/redo explicit instead of silently advertising
-- unsupported keyboard behavior.
handleSendKey :: CommandContext -> Int -> Value -> IO SeerResponse
handleSendKey = handleSendKeyWithRunner unavailableRunner

handleSendKeyWithRunner :: InputServiceRunner -> CommandContext -> Int -> Value -> IO SeerResponse
handleSendKeyWithRunner runner ctx reqId params =
  case Aeson.parseMaybe parseKey params of
    Nothing -> pure $ errResponse reqId "missing or invalid 'key' parameter"
    Just (keyName, modifierNames) ->
      case (parseInputKey keyName, parseKeyModifiers modifierNames) of
        (Left err, _) -> pure $ errResponse reqId err
        (_, Left err) -> pure $ errResponse reqId err
        (Right key, Right modifiers) ->
          executeKeyIntent (inputIntentEnv runner ctx) modifiers key
            >>= pure . intentResponse reqId (Just keyName) (modifierTexts modifiers)

inputIntentEnv :: InputServiceRunner -> CommandContext -> InputIntentEnv
inputIntentEnv runner ctx = InputIntentEnv
  { iieActorHandles = handles
  , iieGetUi = getUiSnapshot (ahUiHandle handles)
  , iieGetLog = getLogSnapshot (ahLogHandle handles)
  , iieRunService = runner ctx
  , iieApplyDataBrowser = runDataBrowserOwnerActionWith (runner ctx) ctx
  , iieDeferBlockingWidgets = False
  , iieOverlayInspectorScrollLimit = \inspector -> pure $
      overlayInspectorViewScrollLimit (layoutForSeed (V2 1200 800) 160 120) inspector
  }
  where
    handles = ccActorHandles ctx

intentResponse :: Int -> Maybe Text -> [Text] -> Either Text InputIntentResult -> SeerResponse
intentResponse reqId _ _ (Left err) = errResponse reqId err
intentResponse reqId keyName modifiers (Right result) = okResponse reqId $ object
  [ "key" .= keyName
  , "modifiers" .= modifiers
  , "outcome" .= if iirApplied result then ("applied" :: Text) else "no_effect"
  , "action" .= iirAction result
  , "selected" .= iirSelected result
  , "text" .= iirText result
  , "filter" .= iirFilter result
  , "cursor" .= iirCursor result
  , "field" .= iirField result
  , "name" .= iirName result
  , "menu_mode" .= iirMenuMode result
  , "clipboard" .= iirClipboard result
  ]

modifierTexts :: KeyModifiers -> [Text]
modifierTexts modifiers =
  [ "ctrl" | kmCtrl modifiers ]
    <> [ "shift" | kmShift modifiers ]
    <> [ "alt" | kmAlt modifiers ]
    <> [ "meta" | kmMeta modifiers ]

unavailableRunner :: InputServiceRunner
unavailableRunner _ method _ = pure (Left (ServiceInternalError ("input service runner unavailable for " <> method)))

-- --------------------------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------------------------

runDataBrowserOwnerAction ctx =
  runDataBrowserOwnerActionWith
    (\_ _ -> pure (Left (ServiceInternalError "pure Data Browser action attempted service IO")))
    ctx

runDataBrowserOwnerActionWith runner ctx action = do
  result <- submitDataBrowserAction (ccDataBrowserExecutor ctx) runner action
  pure $ case result of
    DataBrowserBeginRejected err -> Left err
    _ -> Right ()

autoDetectTarget :: UiState -> Text
autoDetectTarget ui = case uiMenuMode ui of
  MenuPresetSave -> "preset_input"
  MenuWorldSave  -> "world_input"
  MenuPresetLoad -> "preset_filter"
  MenuWorldLoad  -> "world_filter"
  MenuOverlayInspector -> "overlay_import"
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

parseKey :: Value -> Aeson.Parser (Text, [Text])
parseKey = Aeson.withObject "send_key" $ \o ->
  (,) <$> o .: "key" <*> (o .:? "modifiers" .!= [])
