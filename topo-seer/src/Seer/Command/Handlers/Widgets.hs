{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC handlers for generic widget interaction:
-- @click_widget@, @list_widgets@, @get_widget_state@.
module Seer.Command.Handlers.Widgets
  ( handleClickWidget
  , handleListWidgets
  , handleGetWidgetState
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Actor.Log (LogLevel(..), setLogMinLevel)
import Actor.PluginManager (setPluginOrder, setDisabledPlugins, queryPluginResource, mutatePluginResource)
import Actor.Simulation (requestSimTick)
import Actor.Terrain (TerrainReplyOps)
import Actor.UiActions (UiAction(..), UiActionRequest(..), submitUiAction)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( ConfigTab(..)
  , DataBrowserState(..)
  , LeftTab(..)
  , UiState(..)
  , ViewMode(..)
  , readUiSnapshotRef
  )
import Actor.UI.Setters
  ( setUiShowLeftPanel
  , setUiLeftTab
  , setUiShowConfig
  , setUiConfigTab
  , setUiConfigScroll
  , setUiChunkSize
  , setUiDisabledStages
  , setUiDisabledPlugins
  , setUiPluginNames
  , setUiPluginExpanded
  , setUiPluginParam
  , setUiDataBrowser
  , setUiSimAutoTick
  )
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.Config.SliderId (SliderId(..))
import Seer.Config.SliderRegistry (SliderDef(..), sliderWidgetPart, SliderPart(..), sliderDefsForTab, SliderTab(..))
import Seer.Config.SliderState (bumpSliderValue)
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Pipeline.Dep (builtinDependencies, disabledClosure)
import Topo.Pipeline.Stage (StageId, parseStageId, stageCanonicalName, allBuiltinStageIds)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataFieldDef(..), DataOperations(..))
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..), QueryResource(..), DataQuery(..), MutateResource(..), DataMutation(..))
import Seer.Editor.Types (EditorState(..))
import UI.WidgetTree (WidgetId(..))

-- ---------------------------------------------------------------------------
-- Widget ID serialisation
-- ---------------------------------------------------------------------------

-- | Convert a 'WidgetId' to a colon-separated text representation.
widgetIdToText :: WidgetId -> Text
widgetIdToText wid = case wid of
  -- Nullary constructors
  WidgetGenerate               -> "WidgetGenerate"
  WidgetLeftToggle             -> "WidgetLeftToggle"
  WidgetLeftTabTopo            -> "WidgetLeftTabTopo"
  WidgetLeftTabView            -> "WidgetLeftTabView"
  WidgetSeedValue              -> "WidgetSeedValue"
  WidgetSeedRandom             -> "WidgetSeedRandom"
  WidgetChunkMinus             -> "WidgetChunkMinus"
  WidgetChunkPlus              -> "WidgetChunkPlus"
  WidgetConfigToggle           -> "WidgetConfigToggle"
  WidgetConfigTabTerrain       -> "WidgetConfigTabTerrain"
  WidgetConfigTabPlanet        -> "WidgetConfigTabPlanet"
  WidgetConfigTabClimate       -> "WidgetConfigTabClimate"
  WidgetConfigTabWeather       -> "WidgetConfigTabWeather"
  WidgetConfigTabBiome         -> "WidgetConfigTabBiome"
  WidgetConfigTabErosion       -> "WidgetConfigTabErosion"
  WidgetConfigTabPipeline      -> "WidgetConfigTabPipeline"
  WidgetConfigPresetSave       -> "WidgetConfigPresetSave"
  WidgetConfigPresetLoad       -> "WidgetConfigPresetLoad"
  WidgetConfigReset            -> "WidgetConfigReset"
  WidgetConfigRevert           -> "WidgetConfigRevert"
  WidgetViewElevation          -> "WidgetViewElevation"
  WidgetViewBiome              -> "WidgetViewBiome"
  WidgetViewClimate            -> "WidgetViewClimate"
  WidgetViewWeather            -> "WidgetViewWeather"
  WidgetViewMoisture           -> "WidgetViewMoisture"
  WidgetViewPrecip             -> "WidgetViewPrecip"
  WidgetViewVegetation         -> "WidgetViewVegetation"
  WidgetViewTerrainForm        -> "WidgetViewTerrainForm"
  WidgetViewPlateId            -> "WidgetViewPlateId"
  WidgetViewPlateBoundary      -> "WidgetViewPlateBoundary"
  WidgetViewPlateHardness      -> "WidgetViewPlateHardness"
  WidgetViewPlateCrust         -> "WidgetViewPlateCrust"
  WidgetViewPlateAge           -> "WidgetViewPlateAge"
  WidgetViewPlateHeight        -> "WidgetViewPlateHeight"
  WidgetViewPlateVelocity      -> "WidgetViewPlateVelocity"
  WidgetViewCloud               -> "WidgetViewCloud"
  WidgetViewOverlayPrev        -> "WidgetViewOverlayPrev"
  WidgetViewOverlayNext        -> "WidgetViewOverlayNext"
  WidgetViewFieldPrev          -> "WidgetViewFieldPrev"
  WidgetViewFieldNext          -> "WidgetViewFieldNext"
  WidgetLogDebug               -> "WidgetLogDebug"
  WidgetLogInfo                -> "WidgetLogInfo"
  WidgetLogWarn                -> "WidgetLogWarn"
  WidgetLogError               -> "WidgetLogError"
  WidgetLogHeader              -> "WidgetLogHeader"
  WidgetMenuSave               -> "WidgetMenuSave"
  WidgetMenuLoad               -> "WidgetMenuLoad"
  WidgetMenuExit               -> "WidgetMenuExit"
  WidgetPresetSaveOk           -> "WidgetPresetSaveOk"
  WidgetPresetSaveCancel       -> "WidgetPresetSaveCancel"
  WidgetPresetLoadOk           -> "WidgetPresetLoadOk"
  WidgetPresetLoadCancel       -> "WidgetPresetLoadCancel"
  WidgetPresetLoadItem         -> "WidgetPresetLoadItem"
  WidgetWorldSaveOk            -> "WidgetWorldSaveOk"
  WidgetWorldSaveCancel        -> "WidgetWorldSaveCancel"
  WidgetWorldLoadOk            -> "WidgetWorldLoadOk"
  WidgetWorldLoadCancel        -> "WidgetWorldLoadCancel"
  WidgetWorldLoadItem          -> "WidgetWorldLoadItem"
  WidgetSimTick                -> "WidgetSimTick"
  WidgetSimAutoTick            -> "WidgetSimAutoTick"
  WidgetConfigTabData          -> "WidgetConfigTabData"
  WidgetDataDetailDismiss      -> "WidgetDataDetailDismiss"
  WidgetDataEditToggle         -> "WidgetDataEditToggle"
  WidgetDataEditSave           -> "WidgetDataEditSave"
  WidgetDataEditCancel         -> "WidgetDataEditCancel"
  WidgetDataCreateNew          -> "WidgetDataCreateNew"
  WidgetDataDeleteBtn          -> "WidgetDataDeleteBtn"
  WidgetDataDeleteConfirm      -> "WidgetDataDeleteConfirm"
  WidgetDataDeleteCancel       -> "WidgetDataDeleteCancel"
  WidgetEditorRadiusMinus      -> "WidgetEditorRadiusMinus"
  WidgetEditorRadiusPlus       -> "WidgetEditorRadiusPlus"
  WidgetEditorClose            -> "WidgetEditorClose"
  WidgetEditorReopen           -> "WidgetEditorReopen"
  WidgetEditorFalloffPrev      -> "WidgetEditorFalloffPrev"
  WidgetEditorFalloffNext      -> "WidgetEditorFalloffNext"
  -- Parameterized constructors: colon-separated
  WidgetSliderMinus sid        -> "WidgetSliderMinus:" <> Text.pack (show sid)
  WidgetSliderPlus sid         -> "WidgetSliderPlus:" <> Text.pack (show sid)
  WidgetPipelineToggle name    -> "WidgetPipelineToggle:" <> name
  WidgetPluginMoveUp name      -> "WidgetPluginMoveUp:" <> name
  WidgetPluginMoveDown name    -> "WidgetPluginMoveDown:" <> name
  WidgetPluginToggle name      -> "WidgetPluginToggle:" <> name
  WidgetPluginExpand name      -> "WidgetPluginExpand:" <> name
  WidgetPluginParamSlider a b  -> "WidgetPluginParamSlider:" <> a <> ":" <> b
  WidgetPluginParamCheck a b   -> "WidgetPluginParamCheck:" <> a <> ":" <> b
  WidgetDataPluginSelect n     -> "WidgetDataPluginSelect:" <> n
  WidgetDataResourceSelect a b -> "WidgetDataResourceSelect:" <> a <> ":" <> b
  WidgetDataPagePrev a b       -> "WidgetDataPagePrev:" <> a <> ":" <> b
  WidgetDataPageNext a b       -> "WidgetDataPageNext:" <> a <> ":" <> b
  WidgetDataRecordSelect i     -> "WidgetDataRecordSelect:" <> Text.pack (show i)
  WidgetDataFieldToggle p      -> "WidgetDataFieldToggle:" <> p
  WidgetDataFieldTextClick p   -> "WidgetDataFieldTextClick:" <> p
  WidgetDataFieldStepMinus p   -> "WidgetDataFieldStepMinus:" <> p
  WidgetDataFieldStepPlus p    -> "WidgetDataFieldStepPlus:" <> p
  WidgetDataFieldBoolToggle p  -> "WidgetDataFieldBoolToggle:" <> p
  WidgetDataFieldEnumPrev p    -> "WidgetDataFieldEnumPrev:" <> p
  WidgetDataFieldEnumNext p    -> "WidgetDataFieldEnumNext:" <> p
  WidgetEditorTool i           -> "WidgetEditorTool:" <> Text.pack (show i)
  WidgetEditorParamMinus i     -> "WidgetEditorParamMinus:" <> Text.pack (show i)
  WidgetEditorParamPlus i      -> "WidgetEditorParamPlus:" <> Text.pack (show i)
  WidgetEditorCyclePrev i      -> "WidgetEditorCyclePrev:" <> Text.pack (show i)
  WidgetEditorCycleNext i      -> "WidgetEditorCycleNext:" <> Text.pack (show i)

-- | Parse a colon-separated text representation back to 'WidgetId'.
parseWidgetId :: Text -> Maybe WidgetId
parseWidgetId t
  | Just wid <- Map.lookup t nullaryWidgetMap = Just wid
  -- SliderId params
  | Just rest <- Text.stripPrefix "WidgetSliderMinus:" t
  , Just sid <- lookupSliderId rest = Just (WidgetSliderMinus sid)
  | Just rest <- Text.stripPrefix "WidgetSliderPlus:" t
  , Just sid <- lookupSliderId rest = Just (WidgetSliderPlus sid)
  -- Single Text params
  | Just rest <- Text.stripPrefix "WidgetPipelineToggle:" t = Just (WidgetPipelineToggle rest)
  | Just rest <- Text.stripPrefix "WidgetPluginMoveUp:" t = Just (WidgetPluginMoveUp rest)
  | Just rest <- Text.stripPrefix "WidgetPluginMoveDown:" t = Just (WidgetPluginMoveDown rest)
  | Just rest <- Text.stripPrefix "WidgetPluginToggle:" t = Just (WidgetPluginToggle rest)
  | Just rest <- Text.stripPrefix "WidgetPluginExpand:" t = Just (WidgetPluginExpand rest)
  | Just rest <- Text.stripPrefix "WidgetDataPluginSelect:" t = Just (WidgetDataPluginSelect rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldToggle:" t = Just (WidgetDataFieldToggle rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldTextClick:" t = Just (WidgetDataFieldTextClick rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldStepMinus:" t = Just (WidgetDataFieldStepMinus rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldStepPlus:" t = Just (WidgetDataFieldStepPlus rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldBoolToggle:" t = Just (WidgetDataFieldBoolToggle rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldEnumPrev:" t = Just (WidgetDataFieldEnumPrev rest)
  | Just rest <- Text.stripPrefix "WidgetDataFieldEnumNext:" t = Just (WidgetDataFieldEnumNext rest)
  -- Two Text params (split on first colon in the rest)
  | Just rest <- Text.stripPrefix "WidgetPluginParamSlider:" t
  , Just (a, b) <- splitColon rest = Just (WidgetPluginParamSlider a b)
  | Just rest <- Text.stripPrefix "WidgetPluginParamCheck:" t
  , Just (a, b) <- splitColon rest = Just (WidgetPluginParamCheck a b)
  | Just rest <- Text.stripPrefix "WidgetDataResourceSelect:" t
  , Just (a, b) <- splitColon rest = Just (WidgetDataResourceSelect a b)
  | Just rest <- Text.stripPrefix "WidgetDataPagePrev:" t
  , Just (a, b) <- splitColon rest = Just (WidgetDataPagePrev a b)
  | Just rest <- Text.stripPrefix "WidgetDataPageNext:" t
  , Just (a, b) <- splitColon rest = Just (WidgetDataPageNext a b)
  -- Int params
  | Just rest <- Text.stripPrefix "WidgetDataRecordSelect:" t
  , Just i <- readInt rest = Just (WidgetDataRecordSelect i)
  | Just rest <- Text.stripPrefix "WidgetEditorTool:" t
  , Just i <- readInt rest = Just (WidgetEditorTool i)
  | Just rest <- Text.stripPrefix "WidgetEditorParamMinus:" t
  , Just i <- readInt rest = Just (WidgetEditorParamMinus i)
  | Just rest <- Text.stripPrefix "WidgetEditorParamPlus:" t
  , Just i <- readInt rest = Just (WidgetEditorParamPlus i)
  | Just rest <- Text.stripPrefix "WidgetEditorCyclePrev:" t
  , Just i <- readInt rest = Just (WidgetEditorCyclePrev i)
  | Just rest <- Text.stripPrefix "WidgetEditorCycleNext:" t
  , Just i <- readInt rest = Just (WidgetEditorCycleNext i)
  | otherwise = Nothing

-- | Split on the first colon.
splitColon :: Text -> Maybe (Text, Text)
splitColon t = case Text.breakOn ":" t of
  (a, rest)
    | Text.null rest -> Nothing
    | otherwise      -> Just (a, Text.drop 1 rest)

readInt :: Text -> Maybe Int
readInt t = case Text.decimal t of
  Right (i, remainder) | Text.null remainder -> Just i
  _ -> Nothing

-- | Lookup map for nullary WidgetId constructors.
nullaryWidgetMap :: Map Text WidgetId
nullaryWidgetMap = Map.fromList
  [ ("WidgetGenerate",          WidgetGenerate)
  , ("WidgetLeftToggle",        WidgetLeftToggle)
  , ("WidgetLeftTabTopo",       WidgetLeftTabTopo)
  , ("WidgetLeftTabView",       WidgetLeftTabView)
  , ("WidgetSeedValue",         WidgetSeedValue)
  , ("WidgetSeedRandom",        WidgetSeedRandom)
  , ("WidgetChunkMinus",        WidgetChunkMinus)
  , ("WidgetChunkPlus",         WidgetChunkPlus)
  , ("WidgetConfigToggle",      WidgetConfigToggle)
  , ("WidgetConfigTabTerrain",  WidgetConfigTabTerrain)
  , ("WidgetConfigTabPlanet",   WidgetConfigTabPlanet)
  , ("WidgetConfigTabClimate",  WidgetConfigTabClimate)
  , ("WidgetConfigTabWeather",  WidgetConfigTabWeather)
  , ("WidgetConfigTabBiome",    WidgetConfigTabBiome)
  , ("WidgetConfigTabErosion",  WidgetConfigTabErosion)
  , ("WidgetConfigTabPipeline", WidgetConfigTabPipeline)
  , ("WidgetConfigPresetSave",  WidgetConfigPresetSave)
  , ("WidgetConfigPresetLoad",  WidgetConfigPresetLoad)
  , ("WidgetConfigReset",       WidgetConfigReset)
  , ("WidgetConfigRevert",      WidgetConfigRevert)
  , ("WidgetViewElevation",     WidgetViewElevation)
  , ("WidgetViewBiome",         WidgetViewBiome)
  , ("WidgetViewClimate",       WidgetViewClimate)
  , ("WidgetViewWeather",       WidgetViewWeather)
  , ("WidgetViewMoisture",      WidgetViewMoisture)
  , ("WidgetViewPrecip",        WidgetViewPrecip)
  , ("WidgetViewVegetation",    WidgetViewVegetation)
  , ("WidgetViewTerrainForm",   WidgetViewTerrainForm)
  , ("WidgetViewPlateId",       WidgetViewPlateId)
  , ("WidgetViewPlateBoundary", WidgetViewPlateBoundary)
  , ("WidgetViewPlateHardness", WidgetViewPlateHardness)
  , ("WidgetViewPlateCrust",    WidgetViewPlateCrust)
  , ("WidgetViewPlateAge",      WidgetViewPlateAge)
  , ("WidgetViewPlateHeight",   WidgetViewPlateHeight)
  , ("WidgetViewPlateVelocity", WidgetViewPlateVelocity)
  , ("WidgetViewCloud",         WidgetViewCloud)
  , ("WidgetViewOverlayPrev",   WidgetViewOverlayPrev)
  , ("WidgetViewOverlayNext",   WidgetViewOverlayNext)
  , ("WidgetViewFieldPrev",     WidgetViewFieldPrev)
  , ("WidgetViewFieldNext",     WidgetViewFieldNext)
  , ("WidgetLogDebug",          WidgetLogDebug)
  , ("WidgetLogInfo",           WidgetLogInfo)
  , ("WidgetLogWarn",           WidgetLogWarn)
  , ("WidgetLogError",          WidgetLogError)
  , ("WidgetLogHeader",         WidgetLogHeader)
  , ("WidgetMenuSave",          WidgetMenuSave)
  , ("WidgetMenuLoad",          WidgetMenuLoad)
  , ("WidgetMenuExit",          WidgetMenuExit)
  , ("WidgetPresetSaveOk",      WidgetPresetSaveOk)
  , ("WidgetPresetSaveCancel",  WidgetPresetSaveCancel)
  , ("WidgetPresetLoadOk",      WidgetPresetLoadOk)
  , ("WidgetPresetLoadCancel",  WidgetPresetLoadCancel)
  , ("WidgetPresetLoadItem",    WidgetPresetLoadItem)
  , ("WidgetWorldSaveOk",       WidgetWorldSaveOk)
  , ("WidgetWorldSaveCancel",   WidgetWorldSaveCancel)
  , ("WidgetWorldLoadOk",       WidgetWorldLoadOk)
  , ("WidgetWorldLoadCancel",   WidgetWorldLoadCancel)
  , ("WidgetWorldLoadItem",     WidgetWorldLoadItem)
  , ("WidgetSimTick",           WidgetSimTick)
  , ("WidgetSimAutoTick",       WidgetSimAutoTick)
  , ("WidgetConfigTabData",     WidgetConfigTabData)
  , ("WidgetDataDetailDismiss", WidgetDataDetailDismiss)
  , ("WidgetDataEditToggle",    WidgetDataEditToggle)
  , ("WidgetDataEditSave",      WidgetDataEditSave)
  , ("WidgetDataEditCancel",    WidgetDataEditCancel)
  , ("WidgetDataCreateNew",     WidgetDataCreateNew)
  , ("WidgetDataDeleteBtn",     WidgetDataDeleteBtn)
  , ("WidgetDataDeleteConfirm", WidgetDataDeleteConfirm)
  , ("WidgetDataDeleteCancel",  WidgetDataDeleteCancel)
  , ("WidgetEditorRadiusMinus", WidgetEditorRadiusMinus)
  , ("WidgetEditorRadiusPlus",  WidgetEditorRadiusPlus)
  , ("WidgetEditorClose",       WidgetEditorClose)
  , ("WidgetEditorReopen",      WidgetEditorReopen)
  , ("WidgetEditorFalloffPrev", WidgetEditorFalloffPrev)
  , ("WidgetEditorFalloffNext", WidgetEditorFalloffNext)
  ]

-- | SliderId lookup map.
sliderIdMap :: Map Text SliderId
sliderIdMap = Map.fromList
  [ (Text.pack (show sid), sid) | sid <- [minBound..maxBound] ]

lookupSliderId :: Text -> Maybe SliderId
lookupSliderId = flip Map.lookup sliderIdMap

-- ---------------------------------------------------------------------------
-- click_widget
-- ---------------------------------------------------------------------------

-- | Handle @click_widget@ — simulate clicking a widget by its ID.
--
-- Params: @{ "widget_id": "<WidgetId text>" }@
handleClickWidget :: CommandContext -> Int -> Value -> IO SeerResponse
handleClickWidget ctx reqId params = do
  case Aeson.parseMaybe parseWidgetParam params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'widget_id' parameter"
    Just widText ->
      case parseWidgetId widText of
        Nothing ->
          pure $ errResponse reqId ("unknown widget_id: " <> widText)
        Just wid -> do
          result <- executeWidgetClick ctx wid
          case result of
            Right msg -> pure $ okResponse reqId $ object
              [ "widget_id" .= widgetIdToText wid
              , "status" .= ("clicked" :: Text)
              , "info" .= msg
              ]
            Left err -> pure $ errResponse reqId err
  where
    parseWidgetParam = Aeson.withObject "params" $ \o -> o .: "widget_id"

-- | Execute the action associated with clicking a widget.
executeWidgetClick :: CommandContext -> WidgetId -> IO (Either Text Text)
executeWidgetClick ctx wid = do
  let handles = ccActorHandles ctx
      uiH = ahUiHandle handles
      logH = ahLogHandle handles
      pluginH = ahPluginManagerHandle handles
      simH = ahSimulationHandle handles
  uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  case wid of
    -- ----- Left panel & tabs -----
    WidgetLeftToggle -> do
      setUiShowLeftPanel uiH (not (uiShowLeftPanel uiSnap))
      pure $ Right "toggled left panel"
    WidgetLeftTabTopo -> do
      setUiLeftTab uiH LeftTopo
      pure $ Right "switched to topo tab"
    WidgetLeftTabView -> do
      setUiLeftTab uiH LeftView
      pure $ Right "switched to view tab"

    -- ----- Generation / reset -----
    WidgetGenerate -> do
      submitAction ctx UiActionGenerate
      pure $ Right "generation started"
    WidgetConfigReset -> do
      submitAction ctx UiActionReset
      pure $ Right "config reset to defaults"
    WidgetConfigRevert -> do
      submitAction ctx UiActionRevert
      pure $ Right "config reverted"

    -- ----- Seed -----
    WidgetSeedRandom -> do
      -- Cannot generate random seed without System.Random in scope;
      -- use set_seed IPC command instead.
      pure $ Left "use 'set_seed' IPC command for seed changes"
    WidgetSeedValue -> do
      pure $ Left "use 'set_seed' IPC command for seed editing"

    -- ----- Chunk buttons -----
    WidgetChunkMinus -> do
      let cur = uiChunkSize uiSnap
          new = max 8 (cur - 8)
      setUiChunkSize uiH new
      pure $ Right ("chunk size: " <> Text.pack (show new))
    WidgetChunkPlus -> do
      let cur = uiChunkSize uiSnap
          new = cur + 8
      setUiChunkSize uiH new
      pure $ Right ("chunk size: " <> Text.pack (show new))

    -- ----- Config panel & tabs -----
    WidgetConfigToggle -> do
      setUiShowConfig uiH (not (uiShowConfig uiSnap))
      pure $ Right "toggled config panel"
    WidgetConfigTabTerrain -> setTab uiH ConfigTerrain
    WidgetConfigTabPlanet  -> setTab uiH ConfigPlanet
    WidgetConfigTabClimate -> setTab uiH ConfigClimate
    WidgetConfigTabWeather -> setTab uiH ConfigWeather
    WidgetConfigTabBiome   -> setTab uiH ConfigBiome
    WidgetConfigTabErosion -> setTab uiH ConfigErosion
    WidgetConfigTabPipeline -> setTab uiH ConfigPipeline
    WidgetConfigTabData    -> setTab uiH ConfigData

    -- ----- View modes -----
    WidgetViewElevation     -> setView ctx ViewElevation
    WidgetViewBiome         -> setView ctx ViewBiome
    WidgetViewClimate       -> setView ctx ViewClimate
    WidgetViewWeather       -> setView ctx ViewWeather
    WidgetViewMoisture      -> setView ctx ViewMoisture
    WidgetViewPrecip        -> setView ctx ViewPrecip
    WidgetViewVegetation    -> setView ctx ViewVegetation
    WidgetViewTerrainForm   -> setView ctx ViewTerrainForm
    WidgetViewPlateId       -> setView ctx ViewPlateId
    WidgetViewPlateBoundary -> setView ctx ViewPlateBoundary
    WidgetViewPlateHardness -> setView ctx ViewPlateHardness
    WidgetViewPlateCrust    -> setView ctx ViewPlateCrust
    WidgetViewPlateAge      -> setView ctx ViewPlateAge
    WidgetViewPlateHeight   -> setView ctx ViewPlateHeight
    WidgetViewPlateVelocity -> setView ctx ViewPlateVelocity
    WidgetViewCloud          -> setView ctx ViewCloud

    -- ----- Overlay cycling -----
    WidgetViewOverlayPrev -> pure $ Right "use 'cycle_overlay' IPC with direction -1"
    WidgetViewOverlayNext -> pure $ Right "use 'cycle_overlay' IPC with direction 1"
    WidgetViewFieldPrev   -> pure $ Right "use 'cycle_overlay_field' IPC with direction -1"
    WidgetViewFieldNext   -> pure $ Right "use 'cycle_overlay_field' IPC with direction 1"

    -- ----- Slider +/- buttons -----
    WidgetSliderMinus sid -> bumpSlider uiH sid SliderPartMinus
    WidgetSliderPlus sid  -> bumpSlider uiH sid SliderPartPlus

    -- ----- Log controls -----
    WidgetLogDebug  -> setLogMinLevel logH LogDebug  >> pure (Right "log level: debug")
    WidgetLogInfo   -> setLogMinLevel logH LogInfo   >> pure (Right "log level: info")
    WidgetLogWarn   -> setLogMinLevel logH LogWarn   >> pure (Right "log level: warn")
    WidgetLogError  -> setLogMinLevel logH LogError  >> pure (Right "log level: error")
    WidgetLogHeader -> pure $ Right "use 'set_log_collapsed' IPC command"

    -- ----- Menu -----
    WidgetMenuSave   -> pure $ Right "use 'save_world' IPC command"
    WidgetMenuLoad   -> pure $ Right "use 'load_world' IPC command"
    WidgetMenuExit   -> pure $ Left "exit not available via IPC"

    -- ----- Preset dialog -----
    WidgetConfigPresetSave -> pure $ Right "use 'save_preset' IPC command"
    WidgetConfigPresetLoad -> pure $ Right "use 'load_preset' IPC command"
    WidgetPresetSaveOk     -> pure $ Right "use 'save_preset' IPC command"
    WidgetPresetSaveCancel -> pure $ Right "use 'save_preset' IPC command"
    WidgetPresetLoadOk     -> pure $ Right "use 'load_preset' IPC command"
    WidgetPresetLoadCancel -> pure $ Right "use 'load_preset' IPC command"
    WidgetPresetLoadItem   -> pure $ Right "use 'load_preset' IPC command"

    -- ----- World dialog -----
    WidgetWorldSaveOk     -> pure $ Right "use 'save_world' IPC command"
    WidgetWorldSaveCancel -> pure $ Right "use 'save_world' IPC command"
    WidgetWorldLoadOk     -> pure $ Right "use 'load_world' IPC command"
    WidgetWorldLoadCancel -> pure $ Right "use 'load_world' IPC command"
    WidgetWorldLoadItem   -> pure $ Right "use 'load_world' IPC command"

    -- ----- Pipeline stage toggles -----
    WidgetPipelineToggle name ->
      case parseStageId name of
        Nothing -> pure $ Left ("unknown pipeline stage: " <> name)
        Just sid -> do
          let current = uiDisabledStages uiSnap
              toggled :: Set StageId
              toggled
                | Set.member sid current = Set.delete sid current
                | otherwise              = Set.insert sid current
              closed = disabledClosure builtinDependencies toggled
          setUiDisabledStages uiH closed
          let enabled = not (Set.member sid closed)
          pure $ Right ("stage " <> name <> if enabled then " enabled" else " disabled")

    -- ----- Simulation -----
    WidgetSimTick -> do
      let count = uiSimTickCount uiSnap
      requestSimTick simH (count + 1)
      pure $ Right ("sim tick " <> Text.pack (show (count + 1)))
    WidgetSimAutoTick -> do
      setUiSimAutoTick uiH (not (uiSimAutoTick uiSnap))
      pure $ Right ("auto tick " <> if uiSimAutoTick uiSnap then "off" else "on")

    -- ----- Plugin management -----
    WidgetPluginMoveUp name -> do
      let names = uiPluginNames uiSnap
          swapped = swapWithPrev name names
      setUiPluginNames uiH swapped
      setPluginOrder pluginH swapped
      pure $ Right ("moved plugin " <> name <> " up")
    WidgetPluginMoveDown name -> do
      let names = uiPluginNames uiSnap
          swapped = swapWithNext name names
      setUiPluginNames uiH swapped
      setPluginOrder pluginH swapped
      pure $ Right ("moved plugin " <> name <> " down")
    WidgetPluginToggle name -> do
      let current = uiDisabledPlugins uiSnap
          toggled
            | Set.member name current = Set.delete name current
            | otherwise               = Set.insert name current
      setUiDisabledPlugins uiH toggled
      setDisabledPlugins pluginH toggled
      let enabled = not (Set.member name toggled)
      pure $ Right ("plugin " <> name <> if enabled then " enabled" else " disabled")
    WidgetPluginExpand name -> do
      let current = Map.findWithDefault False name (uiPluginExpanded uiSnap)
      setUiPluginExpanded uiH name (not current)
      pure $ Right ("plugin " <> name <> if current then " collapsed" else " expanded")
    WidgetPluginParamSlider _pluginName _paramName ->
      pure $ Left "use 'set_plugin_param' IPC command for positional slider"
    WidgetPluginParamCheck pluginName paramName -> do
      let params' = Map.findWithDefault Map.empty pluginName (uiPluginParams uiSnap)
          current = case Map.lookup paramName params' of
                      Just (Bool b) -> b
                      _ -> False
      setUiPluginParam uiH pluginName paramName (Bool (not current))
      pure $ Right ("plugin param " <> paramName <> " toggled")

    -- ----- Data browser -----
    WidgetDataPluginSelect pluginName -> do
      let dbs = uiDataBrowser uiSnap
          newDbs = dbs
            { dbsSelectedPlugin = Just pluginName
            , dbsSelectedResource = Nothing
            , dbsRecords = []
            , dbsPageOffset = 0
            , dbsTotalCount = Nothing
            , dbsLoading = False
            , dbsSelectedRecord = Nothing
            , dbsSelectedRecordKey = Nothing
            , dbsSelectedRowIndex = Nothing
            , dbsExpandedFields = Set.empty
            }
      setUiDataBrowser uiH newDbs
      pure $ Right ("selected plugin: " <> pluginName)
    WidgetDataResourceSelect pluginName resourceName -> do
      let dbs = uiDataBrowser uiSnap
          newDbs = dbs
            { dbsSelectedResource = Just resourceName
            , dbsRecords = []
            , dbsPageOffset = 0
            , dbsTotalCount = Nothing
            , dbsLoading = True
            , dbsSelectedRecord = Nothing
            , dbsSelectedRecordKey = Nothing
            , dbsSelectedRowIndex = Nothing
            , dbsExpandedFields = Set.empty
            }
      setUiDataBrowser uiH newDbs
      let qr = QueryResource resourceName QueryAll (Just 20) (Just 0)
      result <- queryPluginResource pluginH pluginName qr
      case result of
        Right qResult -> do
          setUiDataBrowser uiH newDbs
            { dbsRecords = qrsRecords qResult
            , dbsTotalCount = qrsTotalCount qResult
            , dbsLoading = False
            }
          pure $ Right ("selected resource: " <> resourceName)
        Left err -> do
          setUiDataBrowser uiH newDbs { dbsLoading = False }
          pure $ Left ("query failed: " <> Text.pack (show err))
    WidgetDataPagePrev pluginName resourceName -> do
      let dbs = uiDataBrowser uiSnap
          newOffset = max 0 (dbsPageOffset dbs - 20)
      paginate uiH pluginH dbs pluginName resourceName newOffset
    WidgetDataPageNext pluginName resourceName -> do
      let dbs = uiDataBrowser uiSnap
          newOffset = dbsPageOffset dbs + 20
      paginate uiH pluginH dbs pluginName resourceName newOffset
    WidgetDataRecordSelect idx -> do
      let dbs = uiDataBrowser uiSnap
          records = dbsRecords dbs
      case drop idx records of
        (rec : _) -> do
          let keyField = case (dbsSelectedResource dbs, dbsSelectedPlugin dbs) of
                (Just rName, Just pName) ->
                  case Map.lookup pName (uiDataResources uiSnap) >>= find (\s -> drsName s == rName) of
                    Just schema -> drsKeyField schema
                    Nothing -> "id"
                _ -> "id"
              keyVal = Map.lookup keyField (unDataRecord rec)
              newDbs = dbs
                { dbsSelectedRecord   = Just rec
                , dbsSelectedRecordKey = keyVal
                , dbsSelectedRowIndex  = Just idx
                , dbsExpandedFields   = Set.empty
                }
          setUiDataBrowser uiH newDbs
          pure $ Right ("selected record at index " <> Text.pack (show idx))
        [] -> pure $ Left "record index out of range"
    WidgetDataDetailDismiss -> do
      let dbs = uiDataBrowser uiSnap
          newDbs = dbs
            { dbsSelectedRecord    = Nothing
            , dbsSelectedRecordKey = Nothing
            , dbsSelectedRowIndex  = Nothing
            , dbsExpandedFields    = Set.empty
            , dbsEditMode          = False
            , dbsCreateMode        = False
            , dbsEditValues        = Map.empty
            , dbsFocusedField      = Nothing
            , dbsTextCursor        = 0
            , dbsDeleteConfirm     = False
            }
      setUiDataBrowser uiH newDbs
      pure $ Right "detail popover dismissed"
    WidgetDataFieldToggle path -> do
      let dbs = uiDataBrowser uiSnap
          expanded = dbsExpandedFields dbs
          newExpanded
            | Set.member path expanded = Set.delete path expanded
            | otherwise                = Set.insert path expanded
      setUiDataBrowser uiH dbs { dbsExpandedFields = newExpanded }
      pure $ Right ("toggled field: " <> path)
    WidgetDataEditToggle -> do
      let dbs = uiDataBrowser uiSnap
      if dbsEditMode dbs
        then do
          setUiDataBrowser uiH dbs
            { dbsEditMode = False, dbsEditValues = Map.empty
            , dbsFocusedField = Nothing, dbsTextCursor = 0 }
          pure $ Right "edit mode off"
        else do
          let vals = case dbsSelectedRecord dbs of
                Just rec -> unDataRecord rec
                Nothing  -> Map.empty
          setUiDataBrowser uiH dbs { dbsEditMode = True, dbsEditValues = vals }
          pure $ Right "edit mode on"
    WidgetDataEditSave -> do
      pure $ Right "use 'data_update_record' or 'data_create_record' IPC command"
    WidgetDataEditCancel -> do
      let dbs = uiDataBrowser uiSnap
      if dbsCreateMode dbs
        then do
          setUiDataBrowser uiH dbs
            { dbsCreateMode = False, dbsEditMode = False, dbsEditValues = Map.empty
            , dbsFocusedField = Nothing, dbsTextCursor = 0
            , dbsSelectedRecord = Nothing, dbsSelectedRecordKey = Nothing
            , dbsSelectedRowIndex = Nothing }
          pure $ Right "create cancelled"
        else do
          setUiDataBrowser uiH dbs
            { dbsEditMode = False, dbsEditValues = Map.empty
            , dbsFocusedField = Nothing, dbsTextCursor = 0 }
          pure $ Right "edit cancelled"
    WidgetDataCreateNew -> do
      let dbs = uiDataBrowser uiSnap
          mSchema = do
            pName <- dbsSelectedPlugin dbs
            rName <- dbsSelectedResource dbs
            schemas <- Map.lookup pName (uiDataResources uiSnap)
            find (\s -> drsName s == rName) schemas
          defaults = case mSchema of
            Just schema -> Map.fromList
              [ (dfName f, v) | f <- drsFields schema, Just v <- [dfDefault f] ]
            Nothing -> Map.empty
          dummyRecord = DataRecord defaults
          newDbs = dbs
            { dbsCreateMode = True, dbsEditMode = True, dbsEditValues = defaults
            , dbsSelectedRecord = Just dummyRecord, dbsSelectedRecordKey = Nothing
            , dbsSelectedRowIndex = Just 0, dbsExpandedFields = Set.empty
            , dbsFocusedField = Nothing, dbsTextCursor = 0 }
      setUiDataBrowser uiH newDbs
      pure $ Right "create mode opened"
    WidgetDataDeleteBtn -> do
      let dbs = uiDataBrowser uiSnap
      setUiDataBrowser uiH dbs { dbsDeleteConfirm = True }
      pure $ Right "delete confirmation shown"
    WidgetDataDeleteConfirm -> do
      let dbs = uiDataBrowser uiSnap
      case (dbsSelectedPlugin dbs, dbsSelectedResource dbs, dbsSelectedRecordKey dbs) of
        (Just pName, Just rName, Just keyVal) -> do
          let mr = MutateResource rName (MutDelete keyVal)
          result <- mutatePluginResource pluginH pName mr
          case result of
            Right _mResult -> do
              setUiDataBrowser uiH dbs
                { dbsDeleteConfirm = False, dbsSelectedRecord = Nothing
                , dbsSelectedRecordKey = Nothing, dbsSelectedRowIndex = Nothing
                , dbsExpandedFields = Set.empty }
              pure $ Right "record deleted"
            Left err -> pure $ Left ("delete failed: " <> Text.pack (show err))
        _ -> pure $ Left "no record selected for deletion"
    WidgetDataDeleteCancel -> do
      let dbs = uiDataBrowser uiSnap
      setUiDataBrowser uiH dbs { dbsDeleteConfirm = False }
      pure $ Right "delete cancelled"

    -- Data field editing
    WidgetDataFieldTextClick path -> do
      let dbs = uiDataBrowser uiSnap
          curLen = case Map.lookup path (dbsEditValues dbs) of
            Just (String t) -> Text.length t
            _ -> 0
      setUiDataBrowser uiH dbs { dbsFocusedField = Just path, dbsTextCursor = curLen }
      pure $ Right ("focused field: " <> path)
    WidgetDataFieldStepMinus path -> do
      bumpFieldNum ctx path (-1)
    WidgetDataFieldStepPlus path -> do
      bumpFieldNum ctx path 1
    WidgetDataFieldBoolToggle path -> do
      let dbs = uiDataBrowser uiSnap
          current = case Map.lookup path (dbsEditValues dbs) of
            Just (Bool b) -> b
            _ -> False
      setUiDataBrowser uiH dbs
        { dbsEditValues = Map.insert path (Bool (not current)) (dbsEditValues dbs) }
      pure $ Right ("toggled bool field: " <> path)
    WidgetDataFieldEnumPrev _path ->
      pure $ Right "use 'data_update_record' IPC command for enum changes"
    WidgetDataFieldEnumNext _path ->
      pure $ Right "use 'data_update_record' IPC command for enum changes"

    -- ----- Editor -----
    WidgetEditorTool _i ->
      pure $ Right "use 'editor_set_tool' IPC command"
    WidgetEditorRadiusMinus ->
      pure $ Right "use 'editor_set_brush' IPC command (radius)"
    WidgetEditorRadiusPlus ->
      pure $ Right "use 'editor_set_brush' IPC command (radius)"
    WidgetEditorClose ->
      pure $ Right "use 'editor_toggle' IPC command"
    WidgetEditorReopen ->
      pure $ Right "use 'editor_toggle' IPC command"
    WidgetEditorFalloffPrev ->
      pure $ Right "use 'editor_set_brush' IPC command (falloff)"
    WidgetEditorFalloffNext ->
      pure $ Right "use 'editor_set_brush' IPC command (falloff)"
    WidgetEditorParamMinus _i ->
      pure $ Right "use 'editor_set_brush' IPC command"
    WidgetEditorParamPlus _i ->
      pure $ Right "use 'editor_set_brush' IPC command"
    WidgetEditorCyclePrev _i ->
      pure $ Right "use 'editor_set_brush' IPC command"
    WidgetEditorCycleNext _i ->
      pure $ Right "use 'editor_set_brush' IPC command"

-- helpers

setTab uiH tab = do
  setUiConfigTab uiH tab
  setUiConfigScroll uiH 0
  pure $ Right ("config tab: " <> tabToText tab)
  where
    tabToText ConfigTerrain  = "terrain"
    tabToText ConfigPlanet   = "planet"
    tabToText ConfigClimate  = "climate"
    tabToText ConfigWeather  = "weather"
    tabToText ConfigBiome    = "biome"
    tabToText ConfigErosion  = "erosion"
    tabToText ConfigPipeline = "pipeline"
    tabToText ConfigData     = "data"

setView :: CommandContext -> ViewMode -> IO (Either Text Text)
setView ctx vm = do
  submitAction ctx (UiActionSetViewMode vm)
  pure $ Right "view mode set"

submitAction :: CommandContext -> UiAction -> IO ()
submitAction ctx action = do
  let uiActionsH = ccUiActionsHandle ctx
      handles    = ccActorHandles ctx
      request = UiActionRequest
        { uarAction = action
        , uarActorHandles = handles
        , uarTerrainReplyTo = replyTo @TerrainReplyOps uiActionsH
        }
  submitUiAction uiActionsH request

bumpSlider uiH sid part = do
  let style = sliderStyleForId sid
      delta = case part of
        SliderPartMinus -> negate (sliderStyleStep style)
        SliderPartPlus  -> sliderStyleStep style
  bumpSliderValue uiH sid delta
  pure $ Right ("slider " <> Text.pack (show sid) <> (if delta < 0 then " -" else " +"))

paginate uiH pluginH dbs pluginName resourceName newOffset = do
  let newDbs = dbs
        { dbsPageOffset = newOffset, dbsLoading = True, dbsRecords = []
        , dbsSelectedRecord = Nothing, dbsSelectedRecordKey = Nothing
        , dbsSelectedRowIndex = Nothing, dbsExpandedFields = Set.empty }
  setUiDataBrowser uiH newDbs
  let qr = QueryResource resourceName QueryAll (Just 20) (Just newOffset)
  result <- queryPluginResource pluginH pluginName qr
  case result of
    Right qResult -> do
      setUiDataBrowser uiH newDbs
        { dbsRecords = qrsRecords qResult
        , dbsTotalCount = qrsTotalCount qResult
        , dbsLoading = False }
      pure $ Right ("page offset " <> Text.pack (show newOffset))
    Left err -> do
      setUiDataBrowser uiH newDbs { dbsLoading = False }
      pure $ Left ("page query failed: " <> Text.pack (show err))

bumpFieldNum :: CommandContext -> Text -> Int -> IO (Either Text Text)
bumpFieldNum ctx path dir = do
  uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let uiH = ahUiHandle (ccActorHandles ctx)
      dbs = uiDataBrowser uiSnap
      editVals = dbsEditValues dbs
  case Map.lookup path editVals of
    Just (Number n) -> do
      let newN = n + fromIntegral dir
      setUiDataBrowser uiH dbs
        { dbsEditValues = Map.insert path (Number newN) editVals }
      pure $ Right ("stepped " <> path <> (if dir > 0 then " +" else " -"))
    _ -> pure $ Left ("field " <> path <> " is not numeric")

swapWithPrev :: Eq a => a -> [a] -> [a]
swapWithPrev _ [] = []
swapWithPrev _ [x] = [x]
swapWithPrev target (a:b:rest)
  | b == target = b : a : rest
  | otherwise   = a : swapWithPrev target (b:rest)

swapWithNext :: Eq a => a -> [a] -> [a]
swapWithNext _ [] = []
swapWithNext _ [x] = [x]
swapWithNext target (a:b:rest)
  | a == target = b : a : rest
  | otherwise   = a : swapWithNext target (b:rest)

-- ---------------------------------------------------------------------------
-- list_widgets
-- ---------------------------------------------------------------------------

-- | Handle @list_widgets@ — return all widget IDs grouped by category.
--
-- Returns widgets logically available given current UI state.
handleListWidgets :: CommandContext -> Int -> Value -> IO SeerResponse
handleListWidgets ctx reqId _params = do
  uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let leftOpen   = uiShowLeftPanel uiSnap
      leftTopo   = leftOpen && uiLeftTab uiSnap == LeftTopo
      leftView   = leftOpen && uiLeftTab uiSnap == LeftView
      configOpen = uiShowConfig uiSnap
      configTab  = uiConfigTab uiSnap
      dbs        = uiDataBrowser uiSnap
      editorOn   = editorActive (uiEditor uiSnap)

      -- Static categories
      navigation = filter snd
        [ ("WidgetLeftToggle",  True)
        , ("WidgetLeftTabTopo", leftOpen)
        , ("WidgetLeftTabView", leftOpen)
        ]
      generation = filter snd
        [ ("WidgetGenerate",   leftTopo)
        , ("WidgetSeedValue",  leftTopo)
        , ("WidgetSeedRandom", leftTopo)
        , ("WidgetChunkMinus", leftTopo)
        , ("WidgetChunkPlus",  leftTopo)
        ]
      config = filter snd
        [ ("WidgetConfigToggle",      True)
        , ("WidgetConfigTabTerrain",  configOpen)
        , ("WidgetConfigTabPlanet",   configOpen)
        , ("WidgetConfigTabClimate",  configOpen)
        , ("WidgetConfigTabWeather",  configOpen)
        , ("WidgetConfigTabBiome",    configOpen)
        , ("WidgetConfigTabErosion",  configOpen)
        , ("WidgetConfigTabPipeline", configOpen)
        , ("WidgetConfigTabData",     configOpen)
        , ("WidgetConfigPresetSave",  configOpen)
        , ("WidgetConfigPresetLoad",  configOpen)
        , ("WidgetConfigReset",       configOpen)
        , ("WidgetConfigRevert",      configOpen)
        ]
      viewModes = filter snd
        [ ("WidgetViewElevation",     leftView)
        , ("WidgetViewBiome",         leftView)
        , ("WidgetViewClimate",       leftView)
        , ("WidgetViewWeather",       leftView)
        , ("WidgetViewMoisture",      leftView)
        , ("WidgetViewPrecip",        leftView)
        , ("WidgetViewVegetation",    leftView)
        , ("WidgetViewTerrainForm",   leftView)
        , ("WidgetViewPlateId",       leftView)
        , ("WidgetViewPlateBoundary", leftView)
        , ("WidgetViewPlateHardness", leftView)
        , ("WidgetViewPlateCrust",    leftView)
        , ("WidgetViewPlateAge",      leftView)
        , ("WidgetViewPlateHeight",   leftView)
        , ("WidgetViewPlateVelocity", leftView)
        , ("WidgetViewCloud",         leftView)
        , ("WidgetViewOverlayPrev",   leftView)
        , ("WidgetViewOverlayNext",   leftView)
        , ("WidgetViewFieldPrev",     leftView)
        , ("WidgetViewFieldNext",     leftView)
        ]
      logWidgets =
        [ ("WidgetLogDebug",  True)
        , ("WidgetLogInfo",   True)
        , ("WidgetLogWarn",   True)
        , ("WidgetLogError",  True)
        , ("WidgetLogHeader", True)
        ]
      simulation = filter snd
        [ ("WidgetSimTick",     configOpen && configTab == ConfigPipeline)
        , ("WidgetSimAutoTick", configOpen && configTab == ConfigPipeline)
        ]
      -- Dynamic: sliders for current tab
      sliderTab' = case configTab of
        ConfigTerrain  -> Just SliderTabTerrain
        ConfigPlanet   -> Just SliderTabPlanet
        ConfigClimate  -> Just SliderTabClimate
        ConfigWeather  -> Just SliderTabWeather
        ConfigBiome    -> Just SliderTabBiome
        ConfigErosion  -> Just SliderTabErosion
        _              -> Nothing
      sliderWids = case sliderTab' of
        Just st | configOpen ->
          concatMap (\sd ->
            [ widgetIdToText (WidgetSliderMinus (sliderId sd))
            , widgetIdToText (WidgetSliderPlus (sliderId sd))
            ]) (sliderDefsForTab st)
        _ -> []
      -- Dynamic: pipeline stages
      pipelineWids
        | configOpen && configTab == ConfigPipeline =
            map (\sid -> widgetIdToText (WidgetPipelineToggle (stageCanonicalName sid)))
                allBuiltinStageIds
        | otherwise = []
      -- Dynamic: plugins
      pluginWids
        | configOpen && configTab == ConfigPipeline =
            concatMap (\name ->
              [ widgetIdToText (WidgetPluginMoveUp name)
              , widgetIdToText (WidgetPluginMoveDown name)
              , widgetIdToText (WidgetPluginToggle name)
              , widgetIdToText (WidgetPluginExpand name)
              ]) (uiPluginNames uiSnap)
        | otherwise = []
      -- Dynamic: data browser
      dataBrowserWids
        | configOpen && configTab == ConfigData =
            let pluginNames = Map.keys (uiDataResources uiSnap)
                pluginSelectWids = map (widgetIdToText . WidgetDataPluginSelect) pluginNames
                resourceWids = case dbsSelectedPlugin dbs of
                  Just pName ->
                    let schemas = Map.findWithDefault [] pName (uiDataResources uiSnap)
                    in map (\s -> widgetIdToText (WidgetDataResourceSelect pName (drsName s))) schemas
                  Nothing -> []
                detailWids
                  | dbsSelectedRowIndex dbs /= Nothing =
                      [ "WidgetDataDetailDismiss", "WidgetDataEditToggle"
                      , "WidgetDataEditSave", "WidgetDataEditCancel"
                      , "WidgetDataCreateNew", "WidgetDataDeleteBtn"
                      , "WidgetDataDeleteConfirm", "WidgetDataDeleteCancel" ]
                  | otherwise = []
            in pluginSelectWids ++ resourceWids ++ detailWids
        | otherwise = []
      -- Dynamic: editor
      editorWids
        | editorOn =
            [ "WidgetEditorClose" ] ++
            [ widgetIdToText (WidgetEditorTool i) | i <- [0..5] ] ++
            [ "WidgetEditorRadiusMinus", "WidgetEditorRadiusPlus"
            , "WidgetEditorFalloffPrev", "WidgetEditorFalloffNext" ]
        | otherwise = [ "WidgetEditorReopen" ]

      allWidgets = map fst navigation
                ++ map fst generation
                ++ map fst config
                ++ map fst viewModes
                ++ map fst logWidgets
                ++ map fst simulation
                ++ sliderWids ++ pipelineWids ++ pluginWids ++ dataBrowserWids ++ editorWids

  pure $ okResponse reqId $ object
    [ "widgets" .= allWidgets
    , "widget_count" .= length allWidgets
    , "categories" .= object
        [ "navigation" .= map fst navigation
        , "generation" .= map fst generation
        , "config" .= map fst config
        , "view_modes" .= map fst viewModes
        , "log" .= map fst logWidgets
        , "simulation" .= map fst simulation
        , "sliders" .= sliderWids
        , "pipeline" .= pipelineWids
        , "plugins" .= pluginWids
        , "data_browser" .= dataBrowserWids
        , "editor" .= editorWids
        ]
    ]

-- ---------------------------------------------------------------------------
-- get_widget_state
-- ---------------------------------------------------------------------------

-- | Handle @get_widget_state@ — return state/info for a specific widget.
--
-- Params: @{ "widget_id": "<WidgetId text>" }@
handleGetWidgetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetWidgetState ctx reqId params = do
  case Aeson.parseMaybe parseWidgetParam params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'widget_id' parameter"
    Just widText ->
      case parseWidgetId widText of
        Nothing ->
          pure $ errResponse reqId ("unknown widget_id: " <> widText)
        Just wid -> do
          uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          pure $ okResponse reqId $ widgetState uiSnap wid
  where
    parseWidgetParam = Aeson.withObject "params" $ \o -> o .: "widget_id"

-- | Compute state JSON for a widget.
widgetState :: UiState -> WidgetId -> Value
widgetState uiSnap wid = object $ base ++ specific
  where
    base = [ "widget_id" .= widgetIdToText wid ]
    specific = case wid of
      WidgetLeftToggle ->
        [ "active" .= uiShowLeftPanel uiSnap ]
      WidgetLeftTabTopo ->
        [ "active" .= (uiLeftTab uiSnap == LeftTopo) ]
      WidgetLeftTabView ->
        [ "active" .= (uiLeftTab uiSnap == LeftView) ]
      WidgetConfigToggle ->
        [ "active" .= uiShowConfig uiSnap ]
      WidgetConfigTabTerrain -> [ "active" .= (uiConfigTab uiSnap == ConfigTerrain) ]
      WidgetConfigTabPlanet  -> [ "active" .= (uiConfigTab uiSnap == ConfigPlanet) ]
      WidgetConfigTabClimate -> [ "active" .= (uiConfigTab uiSnap == ConfigClimate) ]
      WidgetConfigTabWeather -> [ "active" .= (uiConfigTab uiSnap == ConfigWeather) ]
      WidgetConfigTabBiome   -> [ "active" .= (uiConfigTab uiSnap == ConfigBiome) ]
      WidgetConfigTabErosion -> [ "active" .= (uiConfigTab uiSnap == ConfigErosion) ]
      WidgetConfigTabPipeline -> [ "active" .= (uiConfigTab uiSnap == ConfigPipeline) ]
      WidgetConfigTabData    -> [ "active" .= (uiConfigTab uiSnap == ConfigData) ]
      WidgetSimAutoTick ->
        [ "active" .= uiSimAutoTick uiSnap ]
      WidgetPipelineToggle name ->
        case parseStageId name of
          Just sid -> [ "enabled" .= not (Set.member sid (uiDisabledStages uiSnap)) ]
          Nothing  -> []
      WidgetPluginToggle name ->
        [ "enabled" .= not (Set.member name (uiDisabledPlugins uiSnap)) ]
      WidgetPluginExpand name ->
        [ "expanded" .= Map.findWithDefault False name (uiPluginExpanded uiSnap) ]
      WidgetDataEditToggle ->
        [ "edit_mode" .= dbsEditMode (uiDataBrowser uiSnap) ]
      WidgetDataDeleteBtn ->
        [ "confirm_shown" .= dbsDeleteConfirm (uiDataBrowser uiSnap) ]
      _ -> []
