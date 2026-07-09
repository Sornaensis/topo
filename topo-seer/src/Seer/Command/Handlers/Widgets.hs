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

import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Actor.Log (LogLevel(..), setLogMinLevel)
import Actor.PluginManager
  ( PluginSimulationPlan(..)
  , getPluginSimulationPlan
  , setPluginOrder
  )
import Actor.Simulation (SimulationDagSnapshot(..), getSimDagSnapshot, rebindSimNodes)
import Actor.Terrain (TerrainReplyOps)
import Actor.UiActions (UiAction(..), UiActionRequest(..), submitUiAction)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( BaseViewMode(..)
  , ConfigTab(..)
  , DataBrowserState(..)
  , LayeredViewState(..)
  , LeftTab(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , effectiveViewSelection
  , getUiSnapshot
  , readUiSnapshotRef
  )
import Actor.UI.Setters
  ( setUiShowLeftPanel
  , setUiLeftTab
  , setUiShowConfig
  , setUiConfigTab
  , setUiConfigScroll
  , setUiChunkSize
  , setUiPluginNames
  , setUiPluginExpanded
  , setUiDataBrowser
  , setUiDataResources
  )
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import qualified Seer.Command.Handlers.Data as HData
import qualified Seer.Command.Handlers.Pipeline as HPipeline
import qualified Seer.Command.Handlers.Plugin as HPlugin
import qualified Seer.Command.Handlers.Simulation as HSimulation
import qualified Seer.Command.Handlers.Sliders as HSliders
import Seer.Config.SliderId (SliderId(..))
import Seer.Config.SliderRegistry (SliderDef(..), SliderPart(..), sliderDefsForTab, SliderTab(..))
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (sliderValueForId)
import Seer.Service.Types (ServiceError(..), ServiceResponse(..), ServiceResult)
import Topo.Command.Types (SeerResponse(..), okResponse, errResponse)
import Topo.Pipeline.Stage (parseStageId, stageCanonicalName, allBuiltinStageIds)
import qualified Seer.DataBrowser.AppService as DataBrowser
import Seer.DataBrowser.Model (DataBrowserPageAction(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
import Seer.Editor.Types (EditorState(..))
import UI.Components.PipelineControls (pipelineParamToggleValue)
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
  WidgetViewBaseElevation      -> "WidgetViewBaseElevation"
  WidgetViewBaseBiome          -> "WidgetViewBaseBiome"
  WidgetViewBaseMoisture       -> "WidgetViewBaseMoisture"
  WidgetViewBaseVegetation     -> "WidgetViewBaseVegetation"
  WidgetViewBaseTerrainForm    -> "WidgetViewBaseTerrainForm"
  WidgetViewBasePlateId        -> "WidgetViewBasePlateId"
  WidgetViewBasePlateBoundary  -> "WidgetViewBasePlateBoundary"
  WidgetViewBasePlateHardness  -> "WidgetViewBasePlateHardness"
  WidgetViewBasePlateCrust     -> "WidgetViewBasePlateCrust"
  WidgetViewBasePlateAge       -> "WidgetViewBasePlateAge"
  WidgetViewBasePlateHeight    -> "WidgetViewBasePlateHeight"
  WidgetViewBasePlateVelocity  -> "WidgetViewBasePlateVelocity"
  WidgetViewOverlayNone        -> "WidgetViewOverlayNone"
  WidgetViewOverlayTemperature -> "WidgetViewOverlayTemperature"
  WidgetViewOverlayPrecipitation -> "WidgetViewOverlayPrecipitation"
  WidgetViewOverlayCloud       -> "WidgetViewOverlayCloud"
  WidgetViewBasisAverage       -> "WidgetViewBasisAverage"
  WidgetViewBasisCurrent       -> "WidgetViewBasisCurrent"
  WidgetViewElevation          -> "WidgetViewElevation"
  WidgetViewBiome              -> "WidgetViewBiome"
  WidgetViewClimate            -> "WidgetViewClimate"
  WidgetViewWeather            -> "WidgetViewWeather"
  WidgetViewMoisture           -> "WidgetViewMoisture"
  WidgetViewPrecip             -> "WidgetViewPrecip"
  WidgetViewPrecipCurrent      -> "WidgetViewPrecipCurrent"
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
  WidgetViewCloudTypical        -> "WidgetViewCloudTypical"
  WidgetDayNightToggle           -> "WidgetDayNightToggle"
  WidgetViewOverlayPrev        -> "WidgetViewOverlayPrev"
  WidgetViewOverlayNext        -> "WidgetViewOverlayNext"
  WidgetViewFieldPrev          -> "WidgetViewFieldPrev"
  WidgetViewFieldNext          -> "WidgetViewFieldNext"
  WidgetOverlayManager         -> "WidgetOverlayManager"
  WidgetOverlaySchema          -> "WidgetOverlaySchema"
  WidgetOverlayProvenance      -> "WidgetOverlayProvenance"
  WidgetOverlayExport          -> "WidgetOverlayExport"
  WidgetOverlayImportValidate  -> "WidgetOverlayImportValidate"
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
  , ("WidgetViewBaseElevation", WidgetViewBaseElevation)
  , ("WidgetViewBaseBiome", WidgetViewBaseBiome)
  , ("WidgetViewBaseMoisture", WidgetViewBaseMoisture)
  , ("WidgetViewBaseVegetation", WidgetViewBaseVegetation)
  , ("WidgetViewBaseTerrainForm", WidgetViewBaseTerrainForm)
  , ("WidgetViewBasePlateId", WidgetViewBasePlateId)
  , ("WidgetViewBasePlateBoundary", WidgetViewBasePlateBoundary)
  , ("WidgetViewBasePlateHardness", WidgetViewBasePlateHardness)
  , ("WidgetViewBasePlateCrust", WidgetViewBasePlateCrust)
  , ("WidgetViewBasePlateAge", WidgetViewBasePlateAge)
  , ("WidgetViewBasePlateHeight", WidgetViewBasePlateHeight)
  , ("WidgetViewBasePlateVelocity", WidgetViewBasePlateVelocity)
  , ("WidgetViewOverlayNone", WidgetViewOverlayNone)
  , ("WidgetViewOverlayTemperature", WidgetViewOverlayTemperature)
  , ("WidgetViewOverlayPrecipitation", WidgetViewOverlayPrecipitation)
  , ("WidgetViewOverlayCloud", WidgetViewOverlayCloud)
  , ("WidgetViewBasisAverage", WidgetViewBasisAverage)
  , ("WidgetViewBasisCurrent", WidgetViewBasisCurrent)
  , ("WidgetViewElevation",     WidgetViewElevation)
  , ("WidgetViewBiome",         WidgetViewBiome)
  , ("WidgetViewClimate",       WidgetViewClimate)
  , ("WidgetViewWeather",       WidgetViewWeather)
  , ("WidgetViewMoisture",      WidgetViewMoisture)
  , ("WidgetViewPrecip",        WidgetViewPrecip)
  , ("WidgetViewPrecipCurrent", WidgetViewPrecipCurrent)
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
  , ("WidgetViewCloudTypical",  WidgetViewCloudTypical)
  , ("WidgetDayNightToggle",    WidgetDayNightToggle)
  , ("WidgetViewOverlayPrev",   WidgetViewOverlayPrev)
  , ("WidgetViewOverlayNext",   WidgetViewOverlayNext)
  , ("WidgetViewFieldPrev",     WidgetViewFieldPrev)
  , ("WidgetViewFieldNext",     WidgetViewFieldNext)
  , ("WidgetOverlayManager",    WidgetOverlayManager)
  , ("WidgetOverlaySchema",     WidgetOverlaySchema)
  , ("WidgetOverlayProvenance", WidgetOverlayProvenance)
  , ("WidgetOverlayExport",     WidgetOverlayExport)
  , ("WidgetOverlayImportValidate", WidgetOverlayImportValidate)
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
  -- Use an actor call for command-triggered widget clicks so a rapid sequence
  -- of clicks observes UI updates enqueued by prior clicks before it.
  uiSnap <- getUiSnapshot uiH
  let dataBrowserResult message action = do
        result <- applyDataBrowserClick ctx uiSnap action
        pure (result *> Right message)
      setBase baseMode = do
        submitAction ctx (UiActionSetBaseViewMode baseMode)
        pure $ Right "base view set"
      setOverlay overlayMode = do
        submitAction ctx (UiActionSetSkyOverlayMode overlayMode)
        pure $ Right "sky/weather overlay set"
      setBasis basis =
        if weatherBasisEnabled uiSnap
          then do
            submitAction ctx (UiActionSetWeatherBasis basis)
            pure $ Right "weather basis set"
          else pure $ Left "weather basis requires an active builtin weather overlay"
      setLegacyView mode = do
        submitAction ctx (UiActionSetViewMode mode)
        pure $ Right "view mode set"
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
    WidgetConfigTabData -> do
      tabResult <- setTab uiH ConfigData
      dataResult <- applyDataBrowserClick ctx uiSnap DataBrowser.DataBrowserLoadPlugins
      pure (dataResult *> tabResult)

    -- ----- Layered View tab controls -----
    WidgetViewBaseElevation -> setBase BaseViewElevation
    WidgetViewBaseBiome -> setBase BaseViewBiome
    WidgetViewBaseMoisture -> setBase BaseViewMoisture
    WidgetViewBaseVegetation -> setBase BaseViewVegetation
    WidgetViewBaseTerrainForm -> setBase BaseViewTerrainForm
    WidgetViewBasePlateId -> setBase BaseViewPlateId
    WidgetViewBasePlateBoundary -> setBase BaseViewPlateBoundary
    WidgetViewBasePlateHardness -> setBase BaseViewPlateHardness
    WidgetViewBasePlateCrust -> setBase BaseViewPlateCrust
    WidgetViewBasePlateAge -> setBase BaseViewPlateAge
    WidgetViewBasePlateHeight -> setBase BaseViewPlateHeight
    WidgetViewBasePlateVelocity -> setBase BaseViewPlateVelocity
    WidgetViewOverlayNone -> setOverlay Nothing
    WidgetViewOverlayTemperature -> setOverlay (Just SkyOverlayWeatherTemperature)
    WidgetViewOverlayPrecipitation -> setOverlay (Just SkyOverlayPrecipitation)
    WidgetViewOverlayCloud -> setOverlay (Just SkyOverlayCloud)
    WidgetViewBasisAverage -> setBasis WeatherBasisAverage
    WidgetViewBasisCurrent -> setBasis WeatherBasisCurrent

    -- Legacy single-mode widget IDs are still accepted by click_widget.
    WidgetViewElevation     -> setLegacyView ViewElevation
    WidgetViewBiome         -> setLegacyView ViewBiome
    WidgetViewClimate       -> setLegacyView ViewClimate
    WidgetViewWeather       -> setLegacyView ViewWeather
    WidgetViewMoisture      -> setLegacyView ViewMoisture
    WidgetViewPrecip        -> setLegacyView ViewPrecip
    WidgetViewPrecipCurrent -> setLegacyView ViewPrecipCurrent
    WidgetViewVegetation    -> setLegacyView ViewVegetation
    WidgetViewTerrainForm   -> setLegacyView ViewTerrainForm
    WidgetViewPlateId       -> setLegacyView ViewPlateId
    WidgetViewPlateBoundary -> setLegacyView ViewPlateBoundary
    WidgetViewPlateHardness -> setLegacyView ViewPlateHardness
    WidgetViewPlateCrust    -> setLegacyView ViewPlateCrust
    WidgetViewPlateAge      -> setLegacyView ViewPlateAge
    WidgetViewPlateHeight   -> setLegacyView ViewPlateHeight
    WidgetViewPlateVelocity -> setLegacyView ViewPlateVelocity
    WidgetViewCloud          -> setLegacyView ViewCloud
    WidgetViewCloudTypical   -> setLegacyView ViewCloudTypical

    -- ----- Day/night toggle -----
    WidgetDayNightToggle -> do
      submitAction ctx UiActionToggleDayNight
      pure $ Right "day/night toggle queued"

    -- ----- Overlay cycling -----
    WidgetViewOverlayPrev -> pure $ Right "use 'cycle_overlay' IPC with direction -1"
    WidgetViewOverlayNext -> pure $ Right "use 'cycle_overlay' IPC with direction 1"
    WidgetViewFieldPrev   -> pure $ Right "use 'cycle_overlay_field' IPC with direction -1"
    WidgetViewFieldNext   -> pure $ Right "use 'cycle_overlay_field' IPC with direction 1"
    WidgetOverlayManager  -> pure $ Right "use 'get_overlays' or GET /overlays for overlay manager metadata"
    WidgetOverlaySchema   -> pure $ Right "use 'get_overlay_schema' or GET /overlays/schema for schema inspection"
    WidgetOverlayProvenance -> pure $ Right "use 'get_overlay_provenance' or GET /overlays/provenance for provenance inspection"
    WidgetOverlayExport   -> pure $ Right "use 'export_overlay_data' or POST /overlays/export for overlay export"
    WidgetOverlayImportValidate -> pure $ Right "use 'validate_overlay_import' or POST /overlays/import/validate for import diagnostics"

    -- ----- Slider +/- buttons -----
    WidgetSliderMinus sid -> bumpSlider ctx uiSnap sid SliderPartMinus
    WidgetSliderPlus sid  -> bumpSlider ctx uiSnap sid SliderPartPlus

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
          let enabled = Set.member sid (uiDisabledStages uiSnap)
          commandResult ("stage " <> name <> if enabled then " enabled" else " disabled") $
            HPipeline.handleSetStageEnabled ctx 0 (object ["stage" .= name, "enabled" .= enabled])

    -- ----- Simulation -----
    WidgetSimTick ->
      commandResult "sim tick requested" $
        HSimulation.handleSimTick ctx 0 (object ["count" .= (1 :: Int)])
    WidgetSimAutoTick ->
      commandResult ("auto tick " <> if uiSimAutoTick uiSnap then "off" else "on") $
        HSimulation.handleSetSimAutoTick ctx 0 (object ["enabled" .= not (uiSimAutoTick uiSnap)])

    -- ----- Plugin management -----
    WidgetPluginMoveUp name -> do
      let names = uiPluginNames uiSnap
          swapped = swapWithPrev name names
      setUiPluginNames uiH swapped
      setPluginOrder pluginH swapped
      rebindSimulationForCurrentWorld handles
      pure $ Right ("moved plugin " <> name <> " up")
    WidgetPluginMoveDown name -> do
      let names = uiPluginNames uiSnap
          swapped = swapWithNext name names
      setUiPluginNames uiH swapped
      setPluginOrder pluginH swapped
      rebindSimulationForCurrentWorld handles
      pure $ Right ("moved plugin " <> name <> " down")
    WidgetPluginToggle name -> do
      let enabled = Set.member name (uiDisabledPlugins uiSnap)
      commandResult ("plugin " <> name <> if enabled then " enabled" else " disabled") $
        HPlugin.handleSetPluginEnabled ctx 0 (object ["name" .= name, "enabled" .= enabled])
    WidgetPluginExpand name -> do
      let current = Map.findWithDefault False name (uiPluginExpanded uiSnap)
      setUiPluginExpanded uiH name (not current)
      pure $ Right ("plugin " <> name <> if current then " collapsed" else " expanded")
    WidgetPluginParamSlider _pluginName _paramName ->
      pure $ Left "use 'set_plugin_param' IPC command for positional slider"
    WidgetPluginParamCheck pluginName paramName ->
      commandResult ("plugin param " <> paramName <> " toggled") $
        HPlugin.handleSetPluginParam ctx 0 (object
          [ "plugin" .= pluginName
          , "param" .= paramName
          , "value" .= pipelineParamToggleValue uiSnap pluginName paramName
          ])

    -- ----- Data browser -----
    WidgetDataPluginSelect pluginName ->
      dataBrowserResult ("selected plugin: " <> pluginName) $
        DataBrowser.DataBrowserSelectPlugin pluginName
    WidgetDataResourceSelect pluginName resourceName ->
      dataBrowserResult ("selected resource: " <> resourceName) $
        DataBrowser.DataBrowserSelectResource pluginName resourceName
    WidgetDataPagePrev _pluginName _resourceName ->
      dataBrowserResult "page previous" $
        DataBrowser.DataBrowserQueryRecords DataBrowserPagePrevious
    WidgetDataPageNext _pluginName _resourceName ->
      dataBrowserResult "page next" $
        DataBrowser.DataBrowserQueryRecords DataBrowserPageNext
    WidgetDataRecordSelect idx ->
      dataBrowserResult ("selected record at index " <> Text.pack (show idx)) $
        DataBrowser.DataBrowserSelectRecord idx
    WidgetDataDetailDismiss ->
      dataBrowserResult "detail popover dismissed" DataBrowser.DataBrowserDismissRecord
    WidgetDataFieldToggle path ->
      dataBrowserResult ("toggled field: " <> path) $
        DataBrowser.DataBrowserToggleField path
    WidgetDataEditToggle -> do
      let action = if dbsEditMode (uiDataBrowser uiSnap)
            then DataBrowser.DataBrowserCancelEdit
            else DataBrowser.DataBrowserStartEdit
          message = if action == DataBrowser.DataBrowserCancelEdit then "edit mode off" else "edit mode on"
      dataBrowserResult message action
    WidgetDataEditSave -> do
      let action = if dbsCreateMode (uiDataBrowser uiSnap)
            then DataBrowser.DataBrowserCreateRecord
            else DataBrowser.DataBrowserUpdateRecord
      dataBrowserResult "record saved" action
    WidgetDataEditCancel ->
      dataBrowserResult "edit cancelled" DataBrowser.DataBrowserCancelEdit
    WidgetDataCreateNew ->
      dataBrowserResult "create mode opened" DataBrowser.DataBrowserStartCreate
    WidgetDataDeleteBtn ->
      dataBrowserResult "delete confirmation shown" DataBrowser.DataBrowserRequestDelete
    WidgetDataDeleteConfirm ->
      dataBrowserResult "record deleted" DataBrowser.DataBrowserDeleteRecord
    WidgetDataDeleteCancel ->
      dataBrowserResult "delete cancelled" DataBrowser.DataBrowserCancelDelete

    -- Data field editing
    WidgetDataFieldTextClick path ->
      dataBrowserResult ("focused field: " <> path) $
        DataBrowser.DataBrowserFocusField path
    WidgetDataFieldStepMinus path ->
      dataBrowserResult ("stepped " <> path <> " -") $
        DataBrowser.DataBrowserStepNumberField path (-1)
    WidgetDataFieldStepPlus path ->
      dataBrowserResult ("stepped " <> path <> " +") $
        DataBrowser.DataBrowserStepNumberField path 1
    WidgetDataFieldBoolToggle path ->
      dataBrowserResult ("toggled bool field: " <> path) $
        DataBrowser.DataBrowserToggleBoolField path
    WidgetDataFieldEnumPrev path ->
      dataBrowserResult ("cycled enum field: " <> path) $
        DataBrowser.DataBrowserCycleEnumField path (-1)
    WidgetDataFieldEnumNext path ->
      dataBrowserResult ("cycled enum field: " <> path) $
        DataBrowser.DataBrowserCycleEnumField path 1

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

weatherBasisEnabled :: UiState -> Bool
weatherBasisEnabled uiSnap = case lvsSkyOverlay (effectiveViewSelection uiSnap) of
  Just (SkyOverlayPlugin _ _) -> False
  Just _ -> True
  Nothing -> False

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

rebindSimulationForCurrentWorld :: ActorHandles -> IO ()
rebindSimulationForCurrentWorld handles = do
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  when (sdsWorldBound dag) $ do
    simPlan <- getPluginSimulationPlan (ahPluginManagerHandle handles) (Just (sdsOverlayNames dag))
    _ <- rebindSimNodes (ahSimulationHandle handles) (pspExecutableNodes simPlan)
    pure ()

commandResult :: Text -> IO SeerResponse -> IO (Either Text Text)
commandResult successInfo action = do
  response <- action
  if srSuccess response
    then pure (Right successInfo)
    else pure (Left (maybe "command failed" id (srError response)))

runWidgetDataService :: CommandContext -> Text -> Value -> IO ServiceResult
runWidgetDataService ctx method params = do
  response <- case method of
    "data_list_plugins" -> HData.handleDataListPlugins ctx 0 params
    "data_list_resources" -> HData.handleDataListResources ctx 0 params
    "data_list_records" -> HData.handleDataListRecords ctx 0 params
    "data_create_record" -> HData.handleDataCreateRecord ctx 0 params
    "data_update_record" -> HData.handleDataUpdateRecord ctx 0 params
    "data_delete_record" -> HData.handleDataDeleteRecord ctx 0 params
    _ -> pure $ errResponse 0 ("unknown data browser service method: " <> method)
  pure $ if srSuccess response
    then Right (ServiceResponse (srResult response))
    else Left (ServiceInternalError (maybe "data browser service failed" id (srError response)))

applyDataBrowserClick :: CommandContext -> UiState -> DataBrowser.DataBrowserAppAction -> IO (Either Text ())
applyDataBrowserClick ctx uiSnap action = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  result <- DataBrowser.runDataBrowserAppAction
    (runWidgetDataService ctx)
    (DataBrowser.dataBrowserUiFromState (uiDataResources uiSnap) (uiDataBrowser uiSnap))
    action
  let (resources, dbs) = DataBrowser.dataBrowserUiToState (DataBrowser.dbarUi result)
  setUiDataResources uiH resources
  setUiDataBrowser uiH dbs
  pure $ case DataBrowser.dbarError result of
    Nothing -> Right ()
    Just err -> Left err

bumpSlider ctx uiSnap sid part = do
  let style = sliderStyleForId sid
      delta = case part of
        SliderPartMinus -> negate (sliderStyleStep style)
        SliderPartPlus  -> sliderStyleStep style
      newValue = sliderValueForId uiSnap sid + delta
  commandResult ("slider " <> Text.pack (show sid) <> (if delta < 0 then " -" else " +")) $
    HSliders.handleSetSlider ctx 0 (object ["name" .= Text.pack (show sid), "value" .= newValue])

resourceSchemaFor :: UiState -> Text -> Text -> Maybe DataResourceSchema
resourceSchemaFor uiSnap pluginName resourceName = do
  schemas <- Map.lookup pluginName (uiDataResources uiSnap)
  find (\schema -> drsName schema == resourceName) schemas

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
        [ ("WidgetViewBaseElevation", leftView)
        , ("WidgetViewBaseBiome", leftView)
        , ("WidgetViewBaseMoisture", leftView)
        , ("WidgetViewBaseVegetation", leftView)
        , ("WidgetViewBaseTerrainForm", leftView)
        , ("WidgetViewBasePlateId", leftView)
        , ("WidgetViewBasePlateBoundary", leftView)
        , ("WidgetViewBasePlateHardness", leftView)
        , ("WidgetViewBasePlateCrust", leftView)
        , ("WidgetViewBasePlateAge", leftView)
        , ("WidgetViewBasePlateHeight", leftView)
        , ("WidgetViewBasePlateVelocity", leftView)
        , ("WidgetViewOverlayNone", leftView)
        , ("WidgetViewOverlayTemperature", leftView)
        , ("WidgetViewOverlayPrecipitation", leftView)
        , ("WidgetViewOverlayCloud", leftView)
        , ("WidgetViewBasisAverage", leftView)
        , ("WidgetViewBasisCurrent", leftView)
        , ("WidgetDayNightToggle",    leftView)
        , ("WidgetViewOverlayPrev",   leftView)
        , ("WidgetViewOverlayNext",   leftView)
        , ("WidgetViewFieldPrev",     leftView)
        , ("WidgetViewFieldNext",     leftView)
        , ("WidgetOverlayManager",    leftView)
        , ("WidgetOverlaySchema",     leftView)
        , ("WidgetOverlayProvenance", leftView)
        , ("WidgetOverlayExport",     leftView)
        , ("WidgetOverlayImportValidate", leftView)
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
                selectedSchema = do
                  pName <- dbsSelectedPlugin dbs
                  rName <- dbsSelectedResource dbs
                  resourceSchemaFor uiSnap pName rName
                recordWids =
                  [ widgetIdToText (WidgetDataRecordSelect idx)
                  | idx <- [0 .. length (dbsRecords dbs) - 1]
                  ]
                pageWids = case (dbsSelectedPlugin dbs, dbsSelectedResource dbs, selectedSchema) of
                  (Just pName, Just rName, Just schema)
                    | doPage (drsOperations schema) && not (null (dbsRecords dbs)) ->
                        [ widgetIdToText (WidgetDataPagePrev pName rName)
                        , widgetIdToText (WidgetDataPageNext pName rName)
                        ]
                  _ -> []
                detailWids
                  | dbsSelectedRowIndex dbs /= Nothing =
                      [ "WidgetDataDetailDismiss", "WidgetDataEditToggle"
                      , "WidgetDataEditSave", "WidgetDataEditCancel"
                      , "WidgetDataCreateNew", "WidgetDataDeleteBtn"
                      , "WidgetDataDeleteConfirm", "WidgetDataDeleteCancel" ]
                  | otherwise = []
            in pluginSelectWids ++ resourceWids ++ recordWids ++ pageWids ++ detailWids
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
      WidgetDayNightToggle ->
        [ "active" .= uiDayNightEnabled uiSnap ]
      WidgetViewBaseElevation -> baseActive BaseViewElevation
      WidgetViewBaseBiome -> baseActive BaseViewBiome
      WidgetViewBaseMoisture -> baseActive BaseViewMoisture
      WidgetViewBaseVegetation -> baseActive BaseViewVegetation
      WidgetViewBaseTerrainForm -> baseActive BaseViewTerrainForm
      WidgetViewBasePlateId -> baseActive BaseViewPlateId
      WidgetViewBasePlateBoundary -> baseActive BaseViewPlateBoundary
      WidgetViewBasePlateHardness -> baseActive BaseViewPlateHardness
      WidgetViewBasePlateCrust -> baseActive BaseViewPlateCrust
      WidgetViewBasePlateAge -> baseActive BaseViewPlateAge
      WidgetViewBasePlateHeight -> baseActive BaseViewPlateHeight
      WidgetViewBasePlateVelocity -> baseActive BaseViewPlateVelocity
      WidgetViewOverlayNone -> overlayActive Nothing
      WidgetViewOverlayTemperature -> overlayActive (Just SkyOverlayWeatherTemperature)
      WidgetViewOverlayPrecipitation -> overlayActive (Just SkyOverlayPrecipitation)
      WidgetViewOverlayCloud -> overlayActive (Just SkyOverlayCloud)
      WidgetViewBasisAverage -> basisState WeatherBasisAverage
      WidgetViewBasisCurrent -> basisState WeatherBasisCurrent
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
    selection = effectiveViewSelection uiSnap
    baseActive baseMode = [ "active" .= (lvsBaseView selection == baseMode) ]
    overlayActive overlayMode = [ "active" .= (lvsSkyOverlay selection == overlayMode) ]
    basisState basis =
      [ "active" .= (lvsWeatherBasis selection == basis && weatherBasisEnabled uiSnap)
      , "enabled" .= weatherBasisEnabled uiSnap
      ]
