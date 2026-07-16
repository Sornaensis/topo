{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC handlers for generic widget interaction:
-- @click_widget@, @list_widgets@, @get_widget_state@.
module Seer.Command.Handlers.Widgets
  ( handleClickWidget
  , WidgetInvocation(..)
  , WidgetActionResult(..)
  , executeWidgetInvocation
  , executeLocalWidgetInvocation
  , handleListWidgets
  , handleGetWidgetState
  , WidgetClickSupport(..)
  , WidgetCapability(..)
  , widgetCapabilities
  , widgetCapability
  , dataBrowserWidgetIds
  , widgetState
  ) where

import Control.Concurrent.MVar (withMVar)
import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as Aeson
import Data.List (find, findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.String (IsString(..))
import Linear (V2(..))
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Actor.Data (TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..), getLogSnapshot, setLogCollapsed, setLogMinLevel)
import Actor.SnapshotReceiver (publishChangedUiAndLog, readTerrainSnapshot)
import Actor.PluginManager
  ( PluginSimulationPlan(..)
  , getPluginSimulationPlan
  , setPluginOrder
  )
import Actor.Simulation (SimulationDagSnapshot(..), getSimDagSnapshot, rebindSimNodes)
import Actor.Terrain (TerrainReplyOps)
import Actor.UiActions (UiAction(..), UiActionRequest(..), submitUiActionSync)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( BaseViewMode(..)
  , ConfigTab(..)
  , DataBrowserState(..)
  , LayeredViewState(..)
  , LeftTab(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , UiMenuMode(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , dataBrowserScopedError
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
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiPresetFilter
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiWorldFilter
  , setUiOverlayFields
  )
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.DataBrowser.Executor (submitDataBrowserAction)
import qualified Seer.Command.Handlers.Data as HData
import qualified Seer.Command.Handlers.Editor as HEditor
import qualified Seer.Command.Handlers.Presets as HPresets
import qualified Seer.Command.Handlers.View as HView
import qualified Seer.Command.Handlers.World as HWorld
import qualified Seer.Command.Handlers.Pipeline as HPipeline
import qualified Seer.Command.Handlers.Plugin as HPlugin
import qualified Seer.Command.Handlers.Simulation as HSimulation
import qualified Seer.Command.Handlers.Sliders as HSliders
import Seer.Config.SliderId (SliderId(..))
import Seer.Config.SliderRegistry (SliderDef(..), SliderPart(..), sliderDefsForTab, SliderTab(..))
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.Snapshot (listSnapshots)
import Seer.Config.SliderUi (sliderValueForId)
import Seer.Service.Types (ServiceError(..), ServiceResponse(..), ServiceResult)
import Seer.World.Persist (listWorlds)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Command.Types (SeerResponse(..), okResponse, errResponse)
import Topo.Pipeline.Stage (parseStageId, stageCanonicalName, allBuiltinStageIds)
import qualified Seer.DataBrowser.AppService as DataBrowser
import Seer.DataBrowser.Model
  ( DataBrowserBeginResult(..)
  , DataBrowserPageAction(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserRequestId(..)
  , dataBrowserAsyncErrorValue
  , dataBrowserOperationText
  , dataBrowserPendingEnvelopeValue
  )
import Topo.Overlay (Overlay(..), lookupOverlay, overlayNames)
import Topo.Overlay.Schema (OverlayFieldDef(..), OverlaySchema(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
import Seer.Editor.Types
  ( BrushSettings(..)
  , EditorState(..)
  , EditorTool(..)
  , Falloff(..)
  , allTerrainForms
  , paintableBiomes
  )
import UI.Component (ComponentId(..), componentForWidget)
import UI.Components.PipelineControls (pipelineParamToggleValue)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..))
import Topo.Types (biomeIdToCode, terrainFormToCode)
import UI.Layout (layoutFor)
import UI.WidgetId (widgetIdFromText, widgetIdToText)
import UI.WidgetTree
  ( Widget(..)
  , WidgetId(..)
  , buildActiveWidgets
  , buildDataBrowserWidgets
  , buildDataDetailWidgetsForState
  )

-- ---------------------------------------------------------------------------
-- Widget ID serialisation
-- ---------------------------------------------------------------------------

-- | Convert a 'WidgetId' to a colon-separated text representation.
legacyWidgetIdToText :: WidgetId -> Text
legacyWidgetIdToText wid = case wid of
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
legacyParseWidgetId :: Text -> Maybe WidgetId
legacyParseWidgetId t
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

data WidgetInvocation = WidgetInvocation
  { wiWidgetId :: !WidgetId
  , wiNormalizedPosition :: !(Maybe Double)
  , wiItemIndex :: !(Maybe Int)
  } deriving (Eq, Show)

data WidgetActionResult
  = WidgetActionCompleted !Text !Bool
  | WidgetActionAccepted !DataBrowserPendingEnvelope
  deriving (Eq, Show)

instance IsString WidgetActionResult where
  -- String-form results describe UI/log transitions; the coordinator derives
  -- their changed flag from fresh before/after snapshots.
  fromString value = WidgetActionCompleted (Text.pack value) False

-- | Handle @click_widget@ through the shared serialized semantic interpreter.
handleClickWidget :: CommandContext -> Int -> Value -> IO SeerResponse
handleClickWidget ctx reqId params =
  case Aeson.parseMaybe parseWidgetParam params of
    Nothing -> pure $ errResponse reqId "missing or invalid widget invocation"
    Just (widText, normalizedPosition, itemIndex) ->
      case widgetIdFromText widText of
        Nothing -> pure $ errResponse reqId ("unknown widget_id: " <> widText)
        Just wid -> do
          result <- executeWidgetInvocation ctx WidgetInvocation
            { wiWidgetId = wid
            , wiNormalizedPosition = normalizedPosition
            , wiItemIndex = itemIndex
            }
          pure $ case result of
            Left err -> errResponse reqId err
            Right (WidgetActionCompleted info changed) -> okResponse reqId $ object
              [ "widget_id" .= widgetIdToText wid
              , "status" .= ("completed" :: Text)
              , "changed" .= changed
              , "info" .= info
              ]
            Right (WidgetActionAccepted envelope) -> okResponse reqId $ object
              [ "widget_id" .= widgetIdToText wid
              , "status" .= ("accepted" :: Text)
              , "request_id" .= unDataBrowserRequestId (dbpeRequestId envelope)
              , "operation" .= dataBrowserOperationText (dbpeOperation envelope)
              ]
  where
    parseWidgetParam = Aeson.withObject "widget invocation" $ \o ->
      (,,) <$> o .: "widget_id" <*> o .:? "normalized_position" <*> o .:? "item_index"

-- | Single per-application owner for all SDL and remote widget semantics.
executeWidgetInvocation :: CommandContext -> WidgetInvocation -> IO (Either Text WidgetActionResult)
executeWidgetInvocation ctx invocation =
  withMVar (ahWidgetActionLock handles) $ \_ -> do
    uiBefore <- getUiSnapshot (ahUiHandle handles)
    logBefore <- getLogSnapshot (ahLogHandle handles)
    let capability = widgetCapability uiBefore (wiWidgetId invocation)
    case invocationGate uiBefore capability invocation of
      Left err -> pure (Left err)
      Right () -> do
        result <- executeWidgetClick ctx uiBefore invocation
        case result of
          Left err -> pure (Left err)
          Right success -> do
            uiAfter <- getUiSnapshot (ahUiHandle handles)
            logAfter <- getLogSnapshot (ahLogHandle handles)
            -- Publish direct UI/log mutations before releasing the widget
            -- coordinator. Domain owners that already published are detected
            -- by the conditional helper and do not gain a duplicate epoch.
            publishChangedUiAndLog
              (ahSnapshotVersionRef handles)
              uiBefore
              logBefore
              (getUiSnapshot (ahUiHandle handles))
              (getLogSnapshot (ahLogHandle handles))
            pure $ Right $ case success of
              accepted@WidgetActionAccepted{} -> accepted
              WidgetActionCompleted info domainChanged -> WidgetActionCompleted info
                (domainChanged || uiAfter /= uiBefore || logAfter /= logBefore)
  where
    handles = ccActorHandles ctx

-- | Run one SDL-only semantic action under the same fresh-state coordinator.
-- The callback receives the mailbox-current UI state and is invoked only when
-- the local-only widget is visible and enabled.
executeLocalWidgetInvocation
  :: ActorHandles
  -> WidgetId
  -> (UiState -> IO (Either Text a))
  -> IO (Either Text a)
executeLocalWidgetInvocation handles wid action =
  withMVar (ahWidgetActionLock handles) $ \_ -> do
    uiSnap <- getUiSnapshot (ahUiHandle handles)
    let capability = widgetCapability uiSnap wid
    case localInvocationGate capability wid of
      Left err -> pure (Left err)
      Right () -> action uiSnap

localInvocationGate :: WidgetCapability -> WidgetId -> Either Text ()
localInvocationGate capability wid
  | not (wcVisible capability) = Left ("widget is not visible: " <> widgetIdToText wid)
  | not (wcEnabled capability) =
      Left ("widget rejected: " <> Text.intercalate "; " (wcPreconditions capability))
  | wcSupport capability /= WidgetLocalOnly =
      Left ("widget is not a local-only action: " <> widgetIdToText wid)
  | otherwise = Right ()

invocationGate :: UiState -> WidgetCapability -> WidgetInvocation -> Either Text ()
invocationGate uiSnap capability invocation
  | wcSupport capability == WidgetCompatibilityOnly
  , uiMenuMode uiSnap == MenuNone = Right ()
  | wcSupport capability == WidgetCompatibilityOnly =
      Left "widget is not available while a modal dialog is active"
  | not (wcVisible capability) = Left ("widget is not visible: " <> widgetIdToText (wiWidgetId invocation))
  | not (wcEnabled capability) = Left ("widget rejected: " <> Text.intercalate "; " (wcPreconditions capability))
  | wcSupport capability == WidgetLocalOnly = unsupported "local-only"
  | wcSupport capability == WidgetNonClickable = unsupported "non-clickable"
  | wcSupport capability == WidgetArgumentRequired = requireArgument
  | otherwise = Right ()
  where
    unsupported support = Left $ "unsupported " <> support <> " widget: "
      <> widgetIdToText (wiWidgetId invocation) <> alternativeSuffix
    alternativeSuffix = maybe "" ("; use " <>) (wcAlternative capability)
    requireArgument = case wiWidgetId invocation of
      WidgetPluginParamSlider _ _ -> case wiNormalizedPosition invocation of
        Just position | position >= 0 && position <= 1 -> Right ()
        _ -> Left "invalid normalized_position: expected a number in [0,1]"
      WidgetPresetLoadItem -> requireIndex
      WidgetWorldLoadItem -> requireIndex
      _ -> Left "missing required widget argument"
    requireIndex = case wiItemIndex invocation of
      Just index | index >= 0 -> Right ()
      _ -> Left "invalid item_index: expected a non-negative integer"

-- | Execute the action associated with a capability-checked invocation.
executeWidgetClick :: CommandContext -> UiState -> WidgetInvocation -> IO (Either Text WidgetActionResult)
executeWidgetClick ctx uiSnap invocation = do
  let wid = wiWidgetId invocation
  let handles = ccActorHandles ctx
      uiH = ahUiHandle handles
      logH = ahLogHandle handles
      pluginH = ahPluginManagerHandle handles
  let dataBrowserResult message action = do
        result <- applyDataBrowserClick ctx action
        pure $ case result of
          Left err -> Left err
          Right Nothing -> Right (WidgetActionCompleted message False)
          Right (Just envelope) -> Right (WidgetActionAccepted envelope)
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
      pure $ completed ("chunk size: " <> Text.pack (show new))
    WidgetChunkPlus -> do
      let cur = uiChunkSize uiSnap
          new = cur + 8
      setUiChunkSize uiH new
      pure $ completed ("chunk size: " <> Text.pack (show new))

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
      dataResult <- applyDataBrowserClick ctx DataBrowser.DataBrowserLoadPlugins
      pure $ case dataResult of
        Left err -> Left err
        Right Nothing -> tabResult
        Right (Just envelope) -> Right (WidgetActionAccepted envelope)

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
    WidgetViewOverlayPrev -> cycleLayeredOverlay ctx uiSnap (-1)
    WidgetViewOverlayNext -> cycleLayeredOverlay ctx uiSnap 1
    WidgetViewFieldPrev   -> cycleLayeredOverlayField ctx uiSnap (-1)
    WidgetViewFieldNext   -> cycleLayeredOverlayField ctx uiSnap 1
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
    WidgetLogHeader -> do
      logSnapshot <- getLogSnapshot logH
      setLogCollapsed logH (not (lsCollapsed logSnapshot))
      pure $ Right "log panel toggled"

    -- ----- Menu and dialog intents -----
    WidgetMenuSave -> do
      setUiWorldSaveInput uiH (uiWorldName uiSnap)
      setUiMenuMode uiH MenuWorldSave
      pure $ Right "world save dialog opened"
    WidgetMenuLoad -> do
      worlds <- listWorlds
      setUiWorldList uiH worlds
      setUiWorldSelected uiH 0
      setUiWorldFilter uiH Text.empty
      setUiMenuMode uiH MenuWorldLoad
      pure $ Right "world load dialog opened"
    WidgetMenuExit -> pure $ Left "unsupported local-only widget: WidgetMenuExit"

    WidgetConfigPresetSave -> do
      setUiPresetInput uiH ("preset-" <> Text.pack (show (uiSeed uiSnap)))
      setUiMenuMode uiH MenuPresetSave
      pure $ Right "preset save dialog opened"
    WidgetConfigPresetLoad -> do
      names <- listSnapshots
      setUiPresetList uiH names
      setUiPresetSelected uiH 0
      setUiPresetFilter uiH Text.empty
      setUiMenuMode uiH MenuPresetLoad
      pure $ Right "preset load dialog opened"
    WidgetPresetSaveOk -> confirmPresetDialog ctx uiSnap
    WidgetPresetSaveCancel -> closeDialog uiH "preset save cancelled"
    WidgetPresetLoadOk -> confirmPresetDialog ctx uiSnap
    WidgetPresetLoadCancel -> closeDialog uiH "preset load cancelled"
    WidgetPresetLoadItem -> selectPresetItem uiH uiSnap (wiItemIndex invocation)

    WidgetWorldSaveOk -> confirmWorldDialog ctx uiSnap
    WidgetWorldSaveCancel -> closeDialog uiH "world save cancelled"
    WidgetWorldLoadOk -> confirmWorldDialog ctx uiSnap
    WidgetWorldLoadCancel -> closeDialog uiH "world load cancelled"
    WidgetWorldLoadItem -> selectWorldItem uiH uiSnap (wiItemIndex invocation)

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
      commandEffectResult "sim tick requested" $
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
      pure $ completed ("moved plugin " <> name <> " up")
    WidgetPluginMoveDown name -> do
      let names = uiPluginNames uiSnap
          swapped = swapWithNext name names
      setUiPluginNames uiH swapped
      setPluginOrder pluginH swapped
      rebindSimulationForCurrentWorld handles
      pure $ completed ("moved plugin " <> name <> " down")
    WidgetPluginToggle name -> do
      let enabled = Set.member name (uiDisabledPlugins uiSnap)
      commandResult ("plugin " <> name <> if enabled then " enabled" else " disabled") $
        HPlugin.handleSetPluginEnabled ctx 0 (object ["name" .= name, "enabled" .= enabled])
    WidgetPluginExpand name -> do
      let current = Map.findWithDefault False name (uiPluginExpanded uiSnap)
      setUiPluginExpanded uiH name (not current)
      pure $ completed ("plugin " <> name <> if current then " collapsed" else " expanded")
    WidgetPluginParamSlider pluginName paramName ->
      case wiNormalizedPosition invocation of
        Nothing -> pure $ Left "missing normalized_position for plugin slider"
        Just position ->
          case pluginParamValue uiSnap pluginName paramName position of
            Left err -> pure (Left err)
            Right value -> commandResult ("plugin param " <> paramName <> " set") $
              HPlugin.handleSetPluginParam ctx 0 (object
                [ "plugin" .= pluginName
                , "param" .= paramName
                , "value" .= value
                ])
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
    WidgetEditorTool index ->
      case drop index ([minBound .. maxBound] :: [EditorTool]) of
        tool:_ -> editorCommand ctx "editor tool set" HEditor.handleEditorSetTool
          (object ["tool" .= editorToolText tool])
        [] -> pure $ Left "invalid editor tool index"
    WidgetEditorRadiusMinus -> editorRadiusCommand ctx uiSnap (-1)
    WidgetEditorRadiusPlus -> editorRadiusCommand ctx uiSnap 1
    WidgetEditorClose -> editorCommand ctx "editor closed" HEditor.handleEditorToggle
      (object ["active" .= False])
    WidgetEditorReopen -> editorCommand ctx "editor opened" HEditor.handleEditorToggle
      (object ["active" .= True])
    WidgetEditorFalloffPrev -> editorFalloffCommand ctx uiSnap (-1)
    WidgetEditorFalloffNext -> editorFalloffCommand ctx uiSnap 1
    WidgetEditorParamMinus slot -> editorNumericCommand ctx uiSnap slot (-1)
    WidgetEditorParamPlus slot -> editorNumericCommand ctx uiSnap slot 1
    WidgetEditorCyclePrev slot -> editorCycleCommand ctx uiSnap slot (-1)
    WidgetEditorCycleNext slot -> editorCycleCommand ctx uiSnap slot 1

-- helpers

completed :: Text -> Either Text WidgetActionResult
completed info = Right (WidgetActionCompleted info False)

closeDialog uiH info = do
  setUiMenuMode uiH MenuNone
  pure (completed info)

confirmPresetDialog ctx uiSnap = case uiMenuMode uiSnap of
  MenuPresetSave -> runDialogCommand ctx "preset saved"
    (HPresets.handleSavePreset ctx 0 (object ["name" .= uiPresetInput uiSnap]))
  MenuPresetLoad ->
    let names = filteredPresets uiSnap
        index = uiPresetSelected uiSnap
    in case atIndex index names of
      Nothing -> pure $ Left "widget rejected: select a filtered preset"
      Just name -> runDialogCommand ctx "preset loaded"
        (HPresets.handleLoadPreset ctx 0 (object ["name" .= name]))
  _ -> pure $ Left "preset dialog is not active"

confirmWorldDialog ctx uiSnap = case uiMenuMode uiSnap of
  MenuWorldSave -> runDialogCommand ctx "world saved"
    (HWorld.handleSaveWorld ctx 0 (object ["name" .= uiWorldSaveInput uiSnap]))
  MenuWorldLoad ->
    let worlds = filteredWorlds uiSnap
        index = uiWorldSelected uiSnap
    in case atIndex index worlds of
      Nothing -> pure $ Left "widget rejected: select a filtered world"
      Just manifest -> runDialogCommand ctx "world loaded"
        (HWorld.handleLoadWorld ctx 0 (object ["name" .= wsmName manifest]))
  _ -> pure $ Left "world dialog is not active"

runDialogCommand ctx info action = do
  response <- action
  if srSuccess response
    then do
      setUiMenuMode (ahUiHandle (ccActorHandles ctx)) MenuNone
      pure (completed info)
    else pure $ Left (maybe "dialog action failed" id (srError response))

selectPresetItem uiH uiSnap maybeIndex = case maybeIndex >>= (`atIndex` filteredPresets uiSnap) of
  Nothing -> pure $ Left "invalid item_index for filtered preset list"
  Just _ -> do
    let Just index = maybeIndex
    setUiPresetSelected uiH index
    pure $ completed "preset selected"

selectWorldItem uiH uiSnap maybeIndex = case maybeIndex >>= (`atIndex` filteredWorlds uiSnap) of
  Nothing -> pure $ Left "invalid item_index for filtered world list"
  Just _ -> do
    let Just index = maybeIndex
    setUiWorldSelected uiH index
    pure $ completed "world selected"

filteredPresets uiSnap = filter (matches (uiPresetFilter uiSnap)) (uiPresetList uiSnap)
filteredWorlds uiSnap = filter (matches (uiWorldFilter uiSnap) . wsmName) (uiWorldList uiSnap)
matches query candidate = Text.toLower query `Text.isInfixOf` Text.toLower candidate

atIndex index values
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value:_ -> Just value
      [] -> Nothing

pluginParamValue uiSnap pluginName paramName normalized = do
  specs <- maybe (Left "plugin parameter metadata is unavailable") Right
    (Map.lookup pluginName (uiPluginParamSpecs uiSnap))
  spec <- maybe (Left "unknown plugin parameter") Right
    (find ((== paramName) . rpsName) specs)
  pure $ case rpsRange spec of
    Just (Number low, Number high) | high > low ->
      let lowD = realToFrac low :: Double
          highD = realToFrac high :: Double
          value = lowD + normalized * (highD - lowD)
      in Number (realToFrac value)
    _ -> Number (realToFrac normalized)

cycleLayeredOverlay ctx uiSnap direction = do
  terrainSnap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let names = availableOverlayNames uiSnap terrainSnap
      selection = effectiveViewSelection uiSnap
  if null names
    then pure $ Left "widget rejected: no overlays available"
    else do
      let currentIndex = case lvsSkyOverlay selection of
            Just (SkyOverlayPlugin name _) -> maybe 0 (+ 1) (findIndex (== name) names)
            _ -> 0
          nextIndex = (currentIndex + direction) `mod` (length names + 1)
      if nextIndex == 0
        then submitAction ctx (UiActionSetViewSelection selection { lvsSkyOverlay = Nothing })
        else do
          let name = names !! (nextIndex - 1)
              fields = fieldsForOverlayName uiSnap terrainSnap name
          setUiOverlayFields (ahUiHandle (ccActorHandles ctx)) fields
          submitAction ctx (UiActionSetViewSelection selection
            { lvsSkyOverlay = Just (SkyOverlayPlugin name 0) })
      pure $ completed "overlay cycled"

cycleLayeredOverlayField ctx uiSnap direction =
  case lvsSkyOverlay selection of
    Just (SkyOverlayPlugin name fieldIndex) -> do
      terrainSnap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let fields = fieldsForOverlayName uiSnap terrainSnap name
      if null fields
        then pure $ Left "widget rejected: overlay has no fields"
        else do
          let nextIndex = (fieldIndex + direction) `mod` length fields
          setUiOverlayFields (ahUiHandle (ccActorHandles ctx)) fields
          submitAction ctx (UiActionSetViewSelection selection
            { lvsSkyOverlay = Just (SkyOverlayPlugin name nextIndex) })
          pure $ completed "overlay field cycled"
    _ -> pure $ Left "widget rejected: plugin overlay is not active"
  where
    selection = effectiveViewSelection uiSnap

availableOverlayNames uiSnap terrainSnap =
  uiOverlayNames uiSnap
    ++ [name | name <- overlayNames (tsOverlayStore terrainSnap), name `notElem` uiOverlayNames uiSnap]

fieldsForOverlayName uiSnap terrainSnap name = case lookupOverlay name (tsOverlayStore terrainSnap) of
  Just overlay -> map (\field -> (ofdName field, ofdType field)) (osFields (ovSchema overlay))
  Nothing -> uiOverlayFields uiSnap

editorCommand ctx info handler params = commandResult info (handler ctx 0 params)

editorRadiusCommand ctx uiSnap direction =
  let radius = brushRadius (editorBrush (uiEditor uiSnap))
      next = max 0 (min 6 (radius + direction))
  in editorCommand ctx "editor radius changed" HEditor.handleEditorSetBrush
       (object ["radius" .= next])

editorNumericCommand ctx uiSnap slot direction =
  let editor = uiEditor uiSnap
      brush = editorBrush editor
      sign = fromIntegral direction :: Float
      brushParam name value = editorCommand ctx "editor parameter changed" HEditor.handleEditorSetBrush
        (object [AesonKey.fromText name .= value])
  in case editorTool editor of
    ToolRaise | slot == 0 -> brushParam "strength" (clampF 0.005 0.2 (brushStrength brush + sign * 0.005))
    ToolLower | slot == 0 -> brushParam "strength" (clampF 0.005 0.2 (brushStrength brush + sign * 0.005))
    ToolSmooth | slot == 0 -> brushParam "smooth_passes" (clampI 1 5 (editorSmoothPasses editor + direction))
    ToolFlatten | slot == 0 -> brushParam "strength" (clampF 0.01 0.5 (brushStrength brush + sign * 0.01))
    ToolNoise | slot == 0 -> brushParam "noise_frequency" (clampF 0.5 4 (editorNoiseFrequency editor + sign * 0.1))
    ToolNoise | slot == 1 -> brushParam "strength" (clampF 0.005 0.2 (brushStrength brush + sign * 0.005))
    ToolSetHardness | slot == 0 -> editorCommand ctx "editor hardness changed" HEditor.handleEditorSetHardness
      (object ["hardness" .= clampF 0 1 (editorHardnessTarget editor + sign * 0.05)])
    ToolErode | slot == 0 -> brushParam "erode_passes" (clampI 1 20 (editorErodePasses editor + direction))
    _ -> pure $ Left "widget rejected: numeric parameter is unavailable for the current editor tool"

editorCycleCommand ctx uiSnap slot direction
  | slot /= 0 = pure $ Left "widget rejected: invalid editor cycle slot"
  | otherwise = case editorTool editor of
      ToolPaintBiome ->
        let value = cycleValue paintableBiomes (editorBiomeId editor) direction
        in editorCommand ctx "editor biome changed" HEditor.handleEditorSetBiome
             (object ["biome" .= biomeIdToCode value])
      ToolPaintForm ->
        let value = cycleValue allTerrainForms (editorFormOverride editor) direction
        in editorCommand ctx "editor terrain form changed" HEditor.handleEditorSetForm
             (object ["form" .= terrainFormToCode value])
      _ -> pure $ Left "widget rejected: cycle parameter is unavailable for the current editor tool"
  where
    editor = uiEditor uiSnap

editorFalloffCommand ctx uiSnap direction =
  let current = brushFalloff (editorBrush (uiEditor uiSnap))
      next = cycleValue [FalloffLinear, FalloffSmooth, FalloffConstant] current direction
  in editorCommand ctx "editor falloff changed" HEditor.handleEditorSetBrush
       (object ["falloff" .= falloffText next])

editorToolText :: EditorTool -> Text
editorToolText tool = case tool of
  ToolRaise -> "raise"
  ToolLower -> "lower"
  ToolSmooth -> "smooth"
  ToolFlatten -> "flatten"
  ToolNoise -> "noise"
  ToolPaintBiome -> "paint_biome"
  ToolPaintForm -> "paint_form"
  ToolSetHardness -> "set_hardness"
  ToolErode -> "erode"

falloffText :: Falloff -> Text
falloffText falloff = case falloff of
  FalloffLinear -> "linear"
  FalloffSmooth -> "smooth"
  FalloffConstant -> "constant"

cycleValue values current direction =
  let index = maybe 0 id (findIndex (== current) values)
  in values !! ((index + direction) `mod` length values)

clampF low high value = max low (min high value)
clampI low high value = max low (min high value)

setTab uiH tab = do
  setUiConfigTab uiH tab
  setUiConfigScroll uiH 0
  pure $ completed ("config tab: " <> tabToText tab)
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
  submitUiActionSync uiActionsH request

rebindSimulationForCurrentWorld :: ActorHandles -> IO ()
rebindSimulationForCurrentWorld handles = do
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  when (sdsWorldBound dag) $ do
    simPlan <- getPluginSimulationPlan (ahPluginManagerHandle handles) (Just (sdsOverlayNames dag))
    _ <- rebindSimNodes (ahSimulationHandle handles) (pspExecutableNodes simPlan)
    pure ()

commandResult :: Text -> IO SeerResponse -> IO (Either Text WidgetActionResult)
commandResult = commandResultWithDomainChange False

commandEffectResult :: Text -> IO SeerResponse -> IO (Either Text WidgetActionResult)
commandEffectResult = commandResultWithDomainChange True

commandResultWithDomainChange
  :: Bool
  -> Text
  -> IO SeerResponse
  -> IO (Either Text WidgetActionResult)
commandResultWithDomainChange domainChanged successInfo action = do
  response <- action
  if srSuccess response
    then pure (Right (WidgetActionCompleted successInfo domainChanged))
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

applyDataBrowserClick
  :: CommandContext
  -> DataBrowser.DataBrowserAppAction
  -> IO (Either Text (Maybe DataBrowserPendingEnvelope))
applyDataBrowserClick ctx action = do
  result <- submitDataBrowserAction
    (ccDataBrowserExecutor ctx)
    (runWidgetDataService ctx)
    action
  pure $ case result of
    DataBrowserBeginRejected err -> Left err
    DataBrowserBeginPure -> Right Nothing
    DataBrowserBeginAccepted envelope _ -> Right (Just envelope)

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

-- | How a live widget can be driven by remote automation.
data WidgetClickSupport
  = WidgetClickable
  | WidgetArgumentRequired
  | WidgetLocalOnly
  | WidgetNonClickable
  | WidgetCompatibilityOnly
  deriving (Eq, Show)

data WidgetCapability = WidgetCapability
  { wcWidgetId :: !WidgetId
  , wcComponent :: !Text
  , wcCategory :: !Text
  , wcActive :: !(Maybe Bool)
  , wcVisible :: !Bool
  , wcEnabled :: !Bool
  , wcPreconditions :: ![Text]
  , wcSupport :: !WidgetClickSupport
  , wcRequiredArgument :: !(Maybe Value)
  , wcAlternative :: !(Maybe Text)
  } deriving (Eq, Show)

widgetCapabilities :: UiState -> [WidgetCapability]
widgetCapabilities uiSnap =
  map (widgetCapability uiSnap . widgetId) activeWidgets
  where
    commandLayout = layoutFor (V2 1200 900) 0
    activeWidgets = buildActiveWidgets uiSnap commandLayout

widgetCapability :: UiState -> WidgetId -> WidgetCapability
widgetCapability uiSnap wid = WidgetCapability
  { wcWidgetId = wid
  , wcComponent = unComponentId (componentForWidget wid)
  , wcCategory = widgetCategory wid
  , wcActive = widgetActive uiSnap wid
  , wcVisible = visible
  , wcEnabled = visible && null unmet
  , wcPreconditions = unmet
  , wcSupport = support
  , wcRequiredArgument = requiredArgument
  , wcAlternative = alternative
  }
  where
    visible = wid `elem` map widgetId (buildActiveWidgets uiSnap commandLayout)
    commandLayout = layoutFor (V2 1200 900) 0
    stateConditions = widgetPreconditions uiSnap wid
    unmet
      | visible = stateConditions
      | otherwise = "widget is not visible in the current UI state" : stateConditions
    (support, requiredArgument, alternative) = widgetSupport wid

widgetSupport :: WidgetId -> (WidgetClickSupport, Maybe Value, Maybe Text)
widgetSupport wid = case wid of
  WidgetViewElevation -> compatibility
  WidgetViewBiome -> compatibility
  WidgetViewClimate -> compatibility
  WidgetViewWeather -> compatibility
  WidgetViewMoisture -> compatibility
  WidgetViewPrecip -> compatibility
  WidgetViewPrecipCurrent -> compatibility
  WidgetViewVegetation -> compatibility
  WidgetViewTerrainForm -> compatibility
  WidgetViewPlateId -> compatibility
  WidgetViewPlateBoundary -> compatibility
  WidgetViewPlateHardness -> compatibility
  WidgetViewPlateCrust -> compatibility
  WidgetViewPlateAge -> compatibility
  WidgetViewPlateHeight -> compatibility
  WidgetViewPlateVelocity -> compatibility
  WidgetViewCloud -> compatibility
  WidgetViewCloudTypical -> compatibility
  WidgetPresetLoadItem -> required "item_index" "zero-based filtered preset index"
  WidgetWorldLoadItem -> required "item_index" "zero-based filtered world index"
  WidgetPluginParamSlider _ _ -> normalizedPosition
  WidgetMenuExit -> (WidgetLocalOnly, Nothing, Nothing)
  WidgetSeedValue -> localOnly "set_seed"
  WidgetSeedRandom -> localOnly "set_seed"
  WidgetOverlayManager -> nonClickable "get_overlays"
  WidgetOverlaySchema -> nonClickable "get_overlay_schema"
  WidgetOverlayProvenance -> nonClickable "get_overlay_provenance"
  WidgetOverlayExport -> nonClickable "export_overlay_data"
  WidgetOverlayImportValidate -> nonClickable "validate_overlay_import"
  _ -> (WidgetClickable, Nothing, Nothing)
  where
    compatibility = (WidgetCompatibilityOnly, Nothing, Just "set_view_mode")
    localOnly operation = (WidgetLocalOnly, Nothing, Just operation)
    nonClickable operation = (WidgetNonClickable, Nothing, Just operation)
    normalizedPosition =
      ( WidgetArgumentRequired
      , Just (object
          [ "name" .= ("normalized_position" :: Text)
          , "type" .= ("number" :: Text)
          , "minimum" .= (0 :: Int)
          , "maximum" .= (1 :: Int)
          , "description" .= ("normalized slider position" :: Text)
          ])
      , Just "set_plugin_param"
      )
    required :: Text -> Text -> (WidgetClickSupport, Maybe Value, Maybe Text)
    required name description =
      ( WidgetArgumentRequired
      , Just (object
          [ "name" .= name
          , "type" .= ("integer" :: Text)
          , "minimum" .= (0 :: Int)
          , "description" .= description
          ])
      , Nothing
      )

widgetPreconditions :: UiState -> WidgetId -> [Text]
widgetPreconditions uiSnap wid = case wid of
  WidgetGenerate | uiGenerating uiSnap -> ["world generation is already in progress"]
  WidgetConfigReset | uiGenerating uiSnap -> ["world generation is in progress"]
  WidgetConfigRevert | uiGenerating uiSnap -> ["world generation is in progress"]
  WidgetConfigTabData
    | dbsPendingRequest dbs /= Nothing -> ["a Data Browser request is already pending"]
  WidgetViewBasisAverage | not (weatherBasisEnabled uiSnap) -> [weatherBasisRequirement]
  WidgetViewBasisCurrent | not (weatherBasisEnabled uiSnap) -> [weatherBasisRequirement]
  WidgetSimTick -> simulationConditions
  WidgetSimAutoTick -> simulationConditions
  WidgetPluginMoveUp name
    | name `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
    | take 1 (uiPluginNames uiSnap) == [name] -> ["plugin is already first"]
  WidgetPluginMoveDown name
    | name `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
    | take 1 (reverse (uiPluginNames uiSnap)) == [name] -> ["plugin is already last"]
  WidgetPluginToggle name
    | name `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
  WidgetPluginExpand name
    | name `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
  WidgetPluginParamSlider pluginName _
    | pluginName `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
  WidgetPluginParamCheck pluginName _
    | pluginName `notElem` uiPluginNames uiSnap -> ["plugin is not loaded"]
  WidgetPresetSaveOk
    | Text.null (Text.strip (uiPresetInput uiSnap)) -> ["preset name is required"]
  WidgetPresetLoadOk
    | not presetSelectionValid -> ["select a filtered preset"]
  WidgetPresetLoadItem
    | null filteredPresets -> ["no presets match the current filter"]
  WidgetWorldSaveOk
    | Text.null (Text.strip (uiWorldSaveInput uiSnap)) -> ["world name is required"]
  WidgetWorldLoadOk
    | not worldSelectionValid -> ["select a filtered world"]
  WidgetWorldLoadItem
    | null filteredWorlds -> ["no worlds match the current filter"]
  WidgetDataPagePrev _ _
    | dbsPageOffset dbs <= 0 -> ["already at the first page"]
  WidgetDataPageNext _ _
    | Just total <- dbsTotalCount dbs
    , dbsPageOffset dbs + length (dbsRecords dbs) >= total -> ["already at the last page"]
  WidgetDataEditSave
    | not (null (dbsValidationErrors dbs)) -> ["data browser validation errors must be resolved"]
  WidgetEditorRadiusMinus
    | brushRadius (editorBrush editor) <= 0 -> ["editor radius is at its minimum"]
  WidgetEditorRadiusPlus
    | brushRadius (editorBrush editor) >= 6 -> ["editor radius is at its maximum"]
  WidgetEditorParamMinus slot
    | editorParamAtMinimum editor slot -> ["editor parameter is at its minimum"]
  WidgetEditorParamPlus slot
    | editorParamAtMaximum editor slot -> ["editor parameter is at its maximum"]
  WidgetSliderMinus sliderId
    | sliderValueForId uiSnap sliderId <= 0 -> ["slider is at its minimum"]
  WidgetSliderPlus sliderId
    | sliderValueForId uiSnap sliderId >= 1 -> ["slider is at its maximum"]
  _ -> []
  where
    dbs = uiDataBrowser uiSnap
    editor = uiEditor uiSnap
    filteredPresets = filter (matches (uiPresetFilter uiSnap)) (uiPresetList uiSnap)
    presetSelectionValid =
      uiPresetSelected uiSnap >= 0 && uiPresetSelected uiSnap < length filteredPresets
    filteredWorlds = filter (matches (uiWorldFilter uiSnap) . wsmName) (uiWorldList uiSnap)
    worldSelectionValid =
      uiWorldSelected uiSnap >= 0 && uiWorldSelected uiSnap < length filteredWorlds
    matches query candidate =
      Text.toLower query `Text.isInfixOf` Text.toLower candidate
    editorParamAtMinimum currentEditor slot = case editorTool currentEditor of
      ToolRaise -> slot == 0 && strength <= 0.005
      ToolLower -> slot == 0 && strength <= 0.005
      ToolSmooth -> slot == 0 && editorSmoothPasses currentEditor <= 1
      ToolFlatten -> slot == 0 && strength <= 0.01
      ToolNoise
        | slot == 0 -> editorNoiseFrequency currentEditor <= 0.5
        | slot == 1 -> strength <= 0.005
      ToolSetHardness -> slot == 0 && editorHardnessTarget currentEditor <= 0
      ToolErode -> slot == 0 && editorErodePasses currentEditor <= 1
      _ -> False
      where
        strength = brushStrength (editorBrush currentEditor)
    editorParamAtMaximum currentEditor slot = case editorTool currentEditor of
      ToolRaise -> slot == 0 && strength >= 0.2
      ToolLower -> slot == 0 && strength >= 0.2
      ToolSmooth -> slot == 0 && editorSmoothPasses currentEditor >= 5
      ToolFlatten -> slot == 0 && strength >= 0.5
      ToolNoise
        | slot == 0 -> editorNoiseFrequency currentEditor >= 4
        | slot == 1 -> strength >= 0.2
      ToolSetHardness -> slot == 0 && editorHardnessTarget currentEditor >= 1
      ToolErode -> slot == 0 && editorErodePasses currentEditor >= 20
      _ -> False
      where
        strength = brushStrength (editorBrush currentEditor)
    weatherBasisRequirement = "weather basis requires an active built-in weather overlay"
    simulationConditions =
      [ "world generation is in progress" | uiGenerating uiSnap ]
      ++ [ "simulation requires a generated or loaded world" | uiWorldConfig uiSnap == Nothing ]

widgetActive :: UiState -> WidgetId -> Maybe Bool
widgetActive uiSnap wid = case wid of
  WidgetLeftToggle -> Just (uiShowLeftPanel uiSnap)
  WidgetLeftTabTopo -> Just (uiLeftTab uiSnap == LeftTopo)
  WidgetLeftTabView -> Just (uiLeftTab uiSnap == LeftView)
  WidgetConfigToggle -> Just (uiShowConfig uiSnap)
  WidgetConfigTabTerrain -> configTabActive ConfigTerrain
  WidgetConfigTabPlanet -> configTabActive ConfigPlanet
  WidgetConfigTabClimate -> configTabActive ConfigClimate
  WidgetConfigTabWeather -> configTabActive ConfigWeather
  WidgetConfigTabBiome -> configTabActive ConfigBiome
  WidgetConfigTabErosion -> configTabActive ConfigErosion
  WidgetConfigTabPipeline -> configTabActive ConfigPipeline
  WidgetConfigTabData -> configTabActive ConfigData
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
  WidgetViewBasisAverage -> basisActive WeatherBasisAverage
  WidgetViewBasisCurrent -> basisActive WeatherBasisCurrent
  WidgetDayNightToggle -> Just (uiDayNightEnabled uiSnap)
  WidgetPipelineToggle name -> case parseStageId name of
    Just stageId -> Just (not (Set.member stageId (uiDisabledStages uiSnap)))
    Nothing -> Nothing
  WidgetPluginToggle name -> Just (not (Set.member name (uiDisabledPlugins uiSnap)))
  WidgetPluginParamCheck pluginName paramName ->
    Just (pluginBoolValue pluginName paramName)
  WidgetSimAutoTick -> Just (uiSimAutoTick uiSnap)
  WidgetDataPluginSelect pluginName ->
    Just (dbsSelectedPlugin dbs == Just pluginName)
  WidgetDataResourceSelect pluginName resourceName ->
    Just (dbsSelectedPlugin dbs == Just pluginName
      && dbsSelectedResource dbs == Just resourceName)
  WidgetDataRecordSelect index -> Just (dbsSelectedRowIndex dbs == Just index)
  WidgetDataFieldToggle path -> Just (Set.member path (dbsExpandedFields dbs))
  WidgetDataEditToggle -> Just (dbsEditMode dbs)
  WidgetEditorTool index -> Just (index == fromEnum (editorTool (uiEditor uiSnap)))
  WidgetViewElevation -> legacyActive ViewElevation
  WidgetViewBiome -> legacyActive ViewBiome
  WidgetViewClimate -> legacyActive ViewClimate
  WidgetViewWeather -> legacyActive ViewWeather
  WidgetViewMoisture -> legacyActive ViewMoisture
  WidgetViewPrecip -> legacyActive ViewPrecip
  WidgetViewPrecipCurrent -> legacyActive ViewPrecipCurrent
  WidgetViewVegetation -> legacyActive ViewVegetation
  WidgetViewTerrainForm -> legacyActive ViewTerrainForm
  WidgetViewPlateId -> legacyActive ViewPlateId
  WidgetViewPlateBoundary -> legacyActive ViewPlateBoundary
  WidgetViewPlateHardness -> legacyActive ViewPlateHardness
  WidgetViewPlateCrust -> legacyActive ViewPlateCrust
  WidgetViewPlateAge -> legacyActive ViewPlateAge
  WidgetViewPlateHeight -> legacyActive ViewPlateHeight
  WidgetViewPlateVelocity -> legacyActive ViewPlateVelocity
  WidgetViewCloud -> legacyActive ViewCloud
  WidgetViewCloudTypical -> legacyActive ViewCloudTypical
  _ -> Nothing
  where
    dbs = uiDataBrowser uiSnap
    selection = effectiveViewSelection uiSnap
    configTabActive tab = Just (uiConfigTab uiSnap == tab)
    baseActive mode = Just (lvsBaseView selection == mode)
    overlayActive mode = Just (lvsSkyOverlay selection == mode)
    basisActive basis = Just
      (weatherBasisEnabled uiSnap && lvsWeatherBasis selection == basis)
    legacyActive mode = Just (uiViewMode uiSnap == mode)
    pluginBoolValue pluginName paramName = case
        Map.lookup pluginName (uiPluginParams uiSnap) >>= Map.lookup paramName of
      Just (Bool current) -> current
      _ -> False

widgetCategory :: WidgetId -> Text
widgetCategory wid = case wid of
  WidgetLeftToggle -> "navigation"
  WidgetLeftTabTopo -> "navigation"
  WidgetLeftTabView -> "navigation"
  WidgetGenerate -> "generation"
  WidgetSeedValue -> "generation"
  WidgetSeedRandom -> "generation"
  WidgetChunkMinus -> "generation"
  WidgetChunkPlus -> "generation"
  WidgetSliderMinus _ -> "sliders"
  WidgetSliderPlus _ -> "sliders"
  WidgetPipelineToggle _ -> "pipeline"
  WidgetSimTick -> "simulation"
  WidgetSimAutoTick -> "simulation"
  WidgetPluginMoveUp _ -> "plugins"
  WidgetPluginMoveDown _ -> "plugins"
  WidgetPluginToggle _ -> "plugins"
  WidgetPluginExpand _ -> "plugins"
  WidgetPluginParamSlider _ _ -> "plugins"
  WidgetPluginParamCheck _ _ -> "plugins"
  WidgetDataPluginSelect _ -> "data_browser"
  WidgetDataResourceSelect _ _ -> "data_browser"
  WidgetDataPagePrev _ _ -> "data_browser"
  WidgetDataPageNext _ _ -> "data_browser"
  WidgetDataRecordSelect _ -> "data_browser"
  WidgetDataDetailDismiss -> "data_browser"
  WidgetDataFieldToggle _ -> "data_browser"
  WidgetDataEditToggle -> "data_browser"
  WidgetDataEditSave -> "data_browser"
  WidgetDataEditCancel -> "data_browser"
  WidgetDataCreateNew -> "data_browser"
  WidgetDataDeleteBtn -> "data_browser"
  WidgetDataDeleteConfirm -> "data_browser"
  WidgetDataDeleteCancel -> "data_browser"
  WidgetDataFieldTextClick _ -> "data_browser"
  WidgetDataFieldStepMinus _ -> "data_browser"
  WidgetDataFieldStepPlus _ -> "data_browser"
  WidgetDataFieldBoolToggle _ -> "data_browser"
  WidgetDataFieldEnumPrev _ -> "data_browser"
  WidgetDataFieldEnumNext _ -> "data_browser"
  WidgetEditorTool _ -> "editor"
  WidgetEditorRadiusMinus -> "editor"
  WidgetEditorRadiusPlus -> "editor"
  WidgetEditorClose -> "editor"
  WidgetEditorReopen -> "editor"
  WidgetEditorParamMinus _ -> "editor"
  WidgetEditorParamPlus _ -> "editor"
  WidgetEditorCyclePrev _ -> "editor"
  WidgetEditorCycleNext _ -> "editor"
  WidgetEditorFalloffPrev -> "editor"
  WidgetEditorFalloffNext -> "editor"
  WidgetMenuSave -> "menu"
  WidgetMenuLoad -> "menu"
  WidgetMenuExit -> "menu"
  WidgetPresetSaveOk -> "menu"
  WidgetPresetSaveCancel -> "menu"
  WidgetPresetLoadOk -> "menu"
  WidgetPresetLoadCancel -> "menu"
  WidgetPresetLoadItem -> "menu"
  WidgetWorldSaveOk -> "menu"
  WidgetWorldSaveCancel -> "menu"
  WidgetWorldLoadOk -> "menu"
  WidgetWorldLoadCancel -> "menu"
  WidgetWorldLoadItem -> "menu"
  WidgetLogDebug -> "log"
  WidgetLogInfo -> "log"
  WidgetLogWarn -> "log"
  WidgetLogError -> "log"
  WidgetLogHeader -> "log"
  wid' | isViewWidget wid' -> "view_modes"
  _ -> "config"
  where
    isViewWidget candidate = case candidate of
      WidgetViewBaseElevation -> True
      WidgetViewBaseBiome -> True
      WidgetViewBaseMoisture -> True
      WidgetViewBaseVegetation -> True
      WidgetViewBaseTerrainForm -> True
      WidgetViewBasePlateId -> True
      WidgetViewBasePlateBoundary -> True
      WidgetViewBasePlateHardness -> True
      WidgetViewBasePlateCrust -> True
      WidgetViewBasePlateAge -> True
      WidgetViewBasePlateHeight -> True
      WidgetViewBasePlateVelocity -> True
      WidgetViewOverlayNone -> True
      WidgetViewOverlayTemperature -> True
      WidgetViewOverlayPrecipitation -> True
      WidgetViewOverlayCloud -> True
      WidgetViewBasisAverage -> True
      WidgetViewBasisCurrent -> True
      WidgetDayNightToggle -> True
      WidgetViewOverlayPrev -> True
      WidgetViewOverlayNext -> True
      WidgetViewFieldPrev -> True
      WidgetViewFieldNext -> True
      WidgetOverlayManager -> True
      WidgetOverlaySchema -> True
      WidgetOverlayProvenance -> True
      WidgetOverlayExport -> True
      WidgetOverlayImportValidate -> True
      _ -> False

widgetSupportText :: WidgetClickSupport -> Text
widgetSupportText support = case support of
  WidgetClickable -> "clickable"
  WidgetArgumentRequired -> "argument_required"
  WidgetLocalOnly -> "local_only"
  WidgetNonClickable -> "non_clickable"
  WidgetCompatibilityOnly -> "compatibility_only"

widgetCapabilityValue :: WidgetCapability -> Value
widgetCapabilityValue capability = object $
  [ "widget_id" .= widgetIdToText (wcWidgetId capability)
  , "component" .= wcComponent capability
  , "category" .= wcCategory capability
  , "visible" .= wcVisible capability
  , "enabled" .= wcEnabled capability
  , "preconditions" .= wcPreconditions capability
  , "support" .= widgetSupportText (wcSupport capability)
  , "required_argument" .= wcRequiredArgument capability
  , "alternative" .= wcAlternative capability
  ] ++ maybe [] (\active -> ["active" .= active]) (wcActive capability)

-- | Handle @list_widgets@ from the canonical live widget tree.
handleListWidgets :: CommandContext -> Int -> Value -> IO SeerResponse
handleListWidgets ctx reqId _params = do
  uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let capabilities = widgetCapabilities uiSnap
      ids = map (widgetIdToText . wcWidgetId) capabilities
      categoryIds category =
        [ widgetIdToText (wcWidgetId capability)
        | capability <- capabilities
        , wcCategory capability == category
        ]
  pure $ okResponse reqId $ object
    [ "widgets" .= ids
    , "widget_count" .= length ids
    , "capabilities" .= map widgetCapabilityValue capabilities
    , "data_browser_state" .= object (dataBrowserAsyncStateFields (uiDataBrowser uiSnap))
    , "categories" .= object
        [ AesonKey.fromText category .= categoryIds category
        | category <- [ "navigation", "generation", "config", "view_modes"
                      , "log", "simulation", "sliders", "pipeline", "plugins"
                      , "data_browser", "editor", "menu"
                      ]
        ]
    ]

-- | Historical manual inventory retained temporarily as a compatibility
-- reference; the public handler above is exclusively live-tree-derived.
legacyHandleListWidgets :: CommandContext -> Int -> Value -> IO SeerResponse
-- | Handle @list_widgets@ — return all widget IDs grouped by category.
--
-- Returns widgets logically available given current UI state.
legacyHandleListWidgets ctx reqId _params = do
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
        | configOpen && configTab == ConfigData = dataBrowserWidgetIds uiSnap
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
    , "data_browser_state" .= object (dataBrowserAsyncStateFields dbs)
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

-- | Data Browser widget IDs exactly as exposed by the shared SDL widget
-- builders. Geometry is irrelevant to the command surface, so a stable
-- synthetic layout is sufficient.
dataBrowserWidgetIds :: UiState -> [Text]
dataBrowserWidgetIds uiSnap = map (widgetIdToText . widgetId) (browserWidgets ++ detailWidgets)
  where
    dbs = uiDataBrowser uiSnap
    commandLayout = layoutFor (V2 1200 900) 0
    browserWidgets = buildDataBrowserWidgets (uiDataResources uiSnap) dbs commandLayout
    detailWidgets =
      buildDataDetailWidgetsForState (uiDataResources uiSnap) dbs commandLayout

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
      case widgetIdFromText widText of
        Nothing ->
          pure $ errResponse reqId ("unknown widget_id: " <> widText)
        Just wid -> do
          uiSnap <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          pure $ okResponse reqId $ widgetState uiSnap wid
  where
    parseWidgetParam = Aeson.withObject "params" $ \o -> o .: "widget_id"

-- | Compute state JSON for a widget.
widgetState :: UiState -> WidgetId -> Value
widgetState uiSnap wid = object $ base ++ browserState ++ specific
  where
    dbs = uiDataBrowser uiSnap
    capability = widgetCapability uiSnap wid
    base =
      [ "widget_id" .= widgetIdToText wid
      , "component" .= wcComponent capability
      , "category" .= wcCategory capability
      , "visible" .= wcVisible capability
      , "enabled" .= wcEnabled capability
      , "preconditions" .= wcPreconditions capability
      , "support" .= widgetSupportText (wcSupport capability)
      , "required_argument" .= wcRequiredArgument capability
      , "alternative" .= wcAlternative capability
      ] ++ maybe [] (\active -> ["active" .= active]) (wcActive capability)
    browserState
      | isDataBrowserWidget wid = dataBrowserAsyncStateFields dbs
      | otherwise = []
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
          Just sid -> [ "active" .= not (Set.member sid (uiDisabledStages uiSnap)) ]
          Nothing  -> []
      WidgetPluginToggle name ->
        [ "active" .= not (Set.member name (uiDisabledPlugins uiSnap)) ]
      WidgetPluginExpand name ->
        [ "expanded" .= Map.findWithDefault False name (uiPluginExpanded uiSnap) ]
      WidgetDataEditToggle ->
        [ "edit_mode" .= dbsEditMode dbs ]
      WidgetDataDeleteBtn ->
        [ "confirm_shown" .= dbsDeleteConfirm dbs ]
      _ -> []
    selection = effectiveViewSelection uiSnap
    baseActive baseMode = [ "active" .= (lvsBaseView selection == baseMode) ]
    overlayActive overlayMode = [ "active" .= (lvsSkyOverlay selection == overlayMode) ]
    basisState basis =
      [ "active" .= (lvsWeatherBasis selection == basis && weatherBasisEnabled uiSnap) ]

dataBrowserAsyncStateFields :: DataBrowserState -> [Aeson.Pair]
dataBrowserAsyncStateFields dbs =
  [ "loading" .= dbsLoading dbs
  , "pending" .= fmap dataBrowserPendingEnvelopeValue (dbsPendingRequest dbs)
  , "async_error" .= fmap dataBrowserAsyncErrorValue (dataBrowserScopedError dbs)
  ]

isDataBrowserWidget :: WidgetId -> Bool
isDataBrowserWidget wid = case wid of
  WidgetConfigTabData -> True
  WidgetDataPluginSelect _ -> True
  WidgetDataResourceSelect _ _ -> True
  WidgetDataPagePrev _ _ -> True
  WidgetDataPageNext _ _ -> True
  WidgetDataRecordSelect _ -> True
  WidgetDataDetailDismiss -> True
  WidgetDataFieldToggle _ -> True
  WidgetDataEditToggle -> True
  WidgetDataEditSave -> True
  WidgetDataEditCancel -> True
  WidgetDataCreateNew -> True
  WidgetDataDeleteBtn -> True
  WidgetDataDeleteConfirm -> True
  WidgetDataDeleteCancel -> True
  WidgetDataFieldTextClick _ -> True
  WidgetDataFieldStepMinus _ -> True
  WidgetDataFieldStepPlus _ -> True
  WidgetDataFieldBoolToggle _ -> True
  WidgetDataFieldEnumPrev _ -> True
  WidgetDataFieldEnumNext _ -> True
  _ -> False
