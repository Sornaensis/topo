{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Widgets
  ( handleClick
  ) where

import Actor.Data (DataSnapshot(..), getDataSnapshot, getTerrainSnapshot, replaceTerrainData)
import Actor.SnapshotReceiver (writeDataSnapshot, writeTerrainSnapshot, bumpSnapshotVersion)
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..), appendLog, getLogSnapshot, setLogCollapsed, setLogMinLevel, setLogScroll)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , ViewMode(..)
  , configRowCount
  , getUiSnapshot
  , setUiChunkSize
  , setUiConfigTab
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiDataBrowser
  , setUiDisabledStages
  , setUiLeftTab
  , setUiSeed
  , setUiSeedEditing
  , setUiSeedInput
  , setUiShowConfig
  , setUiShowLeftPanel
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiPresetFilter
  , setUiWorldName
  , setUiWorldConfig
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiWorldFilter
  , setUiZoom
  , setUiSimAutoTick
  , setUiSimTickCount
  , setUiDayNightEnabled
  , setUiPluginNames
  , setUiPluginExpanded
  , setUiPluginParam
  , setUiOverlayNames
  , setUiDisabledPlugins
  )
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Int (Int32)
import Data.List (find, findIndex, partition)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Seer.Config.SliderRegistry
  ( SliderPart(..)
  , SliderDef(..)
  , sliderDefForWidget
  , sliderWidgetPart
  )
import Seer.Config.SliderState (bumpSliderValue)
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (configTabForSliderTab)
import System.FilePath ((</>))
import Seer.Draw (seedMaxDigits)
import Seer.Config.Snapshot (listSnapshots, loadSnapshot, snapshotDir, snapshotFromUi, saveSnapshot, applySnapshotToUi)
import Seer.World.Persist (listWorlds, saveNamedWorld, saveNamedWorldWithPlugins, loadNamedWorld, snapshotToWorld, worldDir)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Seer.Input.ConfigScroll
  ( ScrollSettings
  , computeScrollUpdates
  , defaultScrollSettings
  )
import Seer.Input.Modal (handleModalTextKey, handleModalTextInput, handleModalListKey)
import Seer.Input.Router (isQuit)
import Seer.Input.Seed (bumpSeed, handleSeedKey, handleSeedTextInput)
import Seer.Input.ViewControls
  ()
import Topo.Overlay (overlayNames)
import Topo.Pipeline.Dep (builtinDependencies, disabledClosure)
import Topo.Pipeline.Stage (StageId, parseStageId)
import Topo.Plugin.RPC.DataService (DataMutation(..), DataQuery(..), DataRecord(..), MutateResource(..), MutateResult(..), QueryResource(..), QueryResult(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataFieldDef(..), DataFieldType(..), DataOperations(..), noOperations)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..))
import Topo.World (TerrainWorld(..))
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildPluginWidgets, buildDataBrowserWidgets, buildDataDetailWidgets, buildSliderRowWidgets, hitTest, isLeftViewWidget)
import UI.Widgets (Rect(..), containsPoint)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol)
import Actor.PluginManager (getPluginDataDirectories, notifyWorldChanged, queryPluginResource, mutatePluginResource, setDisabledPlugins, setPluginOrder)
import Control.Concurrent (forkIO)
import Actor.Simulation (requestSimTick, setSimWorld)
import Actor.UiActions (ActorHandles(..), UiAction(..))
import Seer.Input.Actions (InputEnv(..), submitAction)
import Seer.Input.Context (InputContext(..))
handleClick
  :: InputContext
  -> SDL.Point V2 Int32
  -> IO ()
handleClick inputContext (SDL.P (V2 x y)) = do
  let window = icWindow inputContext
      widgetEnv = icInputEnv inputContext
      actorHandles = ieActorHandles widgetEnv
      uiHandle = ahUiHandle actorHandles
      logHandle = ahLogHandle actorHandles
      dataHandle = ahDataHandle actorHandles
      pluginManagerHandle = ahPluginManagerHandle actorHandles
      simulationHandle = ahSimulationHandle actorHandles
      quitRef = icQuitRef inputContext
  (V2 winW winH) <- SDL.get (SDL.windowSize window)
  let logSnap = ieLogSnapshot widgetEnv
      uiSnap = ieUiSnapshot widgetEnv
      point = V2 (fromIntegral x) (fromIntegral y)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      simWorldReady = dsTerrainChunks (ieDataSnapshot widgetEnv) > 0
      seedValue = configSeedValueRect layout
      scrollArea = configScrollAreaRect layout
      scrollBar = configScrollBarRect layout
      inScrollBar = uiShowConfig uiSnap && containsPoint scrollBar point
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea point
      scrollPoint = if inConfigScroll
        then V2 (fromIntegral x) (fromIntegral y + uiConfigScroll uiSnap)
        else point
      dbs_ = uiDataBrowser uiSnap
      detailWidgets = case (dbsSelectedRowIndex dbs_, dbsSelectedPlugin dbs_, dbsSelectedResource dbs_) of
        (Just rowIdx, Just pName, Just rName) ->
          let schemas = Map.findWithDefault [] pName (uiDataResources uiSnap)
              mSchema = find (\s -> drsName s == rName) schemas
              fields = maybe [] drsFields mSchema
              ops = maybe noOperations drsOperations mSchema
              isEditing = dbsEditMode dbs_ || dbsCreateMode dbs_
          in buildDataDetailWidgets rowIdx fields (dbsExpandedFields dbs_) isEditing (doUpdate ops) (doDelete ops) layout
        _ -> []
      selectedSchema = do
        pName <- dbsSelectedPlugin (uiDataBrowser uiSnap)
        rName <- dbsSelectedResource (uiDataBrowser uiSnap)
        schemas <- Map.lookup pName (uiDataResources uiSnap)
        find (\s -> drsName s == rName) schemas
      canCreate = maybe False (doCreate . drsOperations) selectedSchema
      widgetsAll = buildWidgets layout
                ++ buildPluginWidgets (uiPluginNames uiSnap) (uiPluginExpanded uiSnap) (uiPluginParamSpecs uiSnap) layout
                ++ buildDataBrowserWidgets (uiDataResources uiSnap) (dbsSelectedPlugin (uiDataBrowser uiSnap)) (dbsSelectedResource (uiDataBrowser uiSnap)) (length (dbsRecords (uiDataBrowser uiSnap))) canCreate layout
                ++ detailWidgets
      widgets =
        if uiShowConfig uiSnap
          then filter (configWidgetAllowed simWorldReady (uiConfigTab uiSnap)) widgetsAll
          else widgetsAll
      isConfigSliderWidget = isJust . sliderDefForWidget
      (configSliderWidgets, nonSliderWidgets) = partition (isConfigSliderWidget . widgetId) widgets
      inLeftViewPanel = uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftView
                        && containsPoint (leftPanelRect layout) point
      leftViewAdjPoint = V2 (fromIntegral x) (fromIntegral y + uiLeftViewScroll uiSnap)
      (leftViewWidgets, otherWidgets) = partition (isLeftViewWidget . widgetId) nonSliderWidgets
      hitWidget =
        if inConfigScroll
          then case hitTest configSliderWidgets scrollPoint of
            Just wid -> Just wid
            Nothing ->
              let viewHit = if inLeftViewPanel then hitTest leftViewWidgets leftViewAdjPoint else Nothing
              in viewHit <|> hitTest otherWidgets point
          else if inLeftViewPanel
            then hitTest leftViewWidgets leftViewAdjPoint <|> hitTest otherWidgets point
            else hitTest widgets point
      configWidgetAllowed simReady tab widget =
        case sliderDefForWidget (widgetId widget) of
          Just sliderDef -> tab == configTabForSliderTab (sliderTab sliderDef)
          Nothing -> bespokeConfigWidgetAllowed simReady tab (widgetId widget)
  if inScrollBar
    then do
      let rowHeight = 24
          gap = 10
          rows = configRowCount (uiConfigTab uiSnap) uiSnap
          contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
          Rect (V2 _ sy, V2 _ sh) = scrollArea
          maxOffset = max 0 (contentHeight - sh)
          ratio
            | sh <= 1 = 0
            | otherwise = max 0 (min 1 (fromIntegral (fromIntegral y - sy) / fromIntegral sh))
          newScroll = round (ratio * fromIntegral maxOffset)
      setUiConfigScroll uiHandle newScroll
    else do
      case uiContextHex uiSnap of
        Just _ -> do
          setUiContextHex uiHandle Nothing
          setUiContextPos uiHandle Nothing
        Nothing -> pure ()
      let whenLeftPanel action = when (uiShowLeftPanel uiSnap) action
          whenLeftTopo action = when (uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftTopo) action
          whenLeftView action = when (uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftView) action
          whenConfigVisible action = when (uiShowConfig uiSnap) action
      if uiMenuMode uiSnap /= MenuNone
        then handleMenuClick layout widgets point
        else case hitWidget of
          { Just WidgetGenerate -> whenLeftTopo (submit UiActionGenerate)
          ; Just WidgetLeftToggle -> setUiShowLeftPanel uiHandle (not (uiShowLeftPanel uiSnap))
          ; Just WidgetLeftTabTopo -> whenLeftPanel (setUiLeftTab uiHandle LeftTopo)
          ; Just WidgetLeftTabView -> whenLeftPanel (setUiLeftTab uiHandle LeftView)
          ; Just WidgetSeedValue -> whenLeftTopo (startSeedEdit seedValue)
          ; Just WidgetSeedRandom -> whenLeftTopo randomSeed
          ; Just WidgetChunkMinus -> whenLeftTopo (bumpChunk (-8))
          ; Just WidgetChunkPlus -> whenLeftTopo (bumpChunk 8)
          ; Just WidgetConfigToggle -> toggleConfig
          ; Just WidgetConfigTabTerrain -> whenConfigVisible (setUiConfigTab uiHandle ConfigTerrain >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabPlanet -> whenConfigVisible (setUiConfigTab uiHandle ConfigPlanet >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabClimate -> whenConfigVisible (setUiConfigTab uiHandle ConfigClimate >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabWeather -> whenConfigVisible (setUiConfigTab uiHandle ConfigWeather >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabBiome -> whenConfigVisible (setUiConfigTab uiHandle ConfigBiome >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabErosion -> whenConfigVisible (setUiConfigTab uiHandle ConfigErosion >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabPipeline -> whenConfigVisible (setUiConfigTab uiHandle ConfigPipeline >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabData -> whenConfigVisible (setUiConfigTab uiHandle ConfigData >> setUiConfigScroll uiHandle 0)
          ; Just wid
          | Just (sliderDef, sliderPart) <- sliderWidgetPart wid
              -> whenConfigVisible
                (let sliderStyle = sliderStyleForId (sliderId sliderDef)
                 in bumpSliderValue uiHandle
                      (sliderId sliderDef)
                      (signedSliderDelta sliderPart (sliderStyleStep sliderStyle)))
          ; Just wid
          | isBespokeConfigWidget wid ->
              handleBespokeConfigWidget layout wid scrollPoint whenConfigVisible uiSnap
          ; Just WidgetViewElevation -> whenLeftView (submit (UiActionSetViewMode ViewElevation))
          ; Just WidgetViewBiome -> whenLeftView (submit (UiActionSetViewMode ViewBiome))
          ; Just WidgetViewClimate -> whenLeftView (submit (UiActionSetViewMode ViewClimate))
          ; Just WidgetViewWeather -> whenLeftView (submit (UiActionSetViewMode ViewWeather))
          ; Just WidgetViewMoisture -> whenLeftView (submit (UiActionSetViewMode ViewMoisture))
          ; Just WidgetViewPrecip -> whenLeftView (submit (UiActionSetViewMode ViewPrecip))
          ; Just WidgetViewVegetation -> whenLeftView (submit (UiActionSetViewMode ViewVegetation))
          ; Just WidgetViewTerrainForm -> whenLeftView (submit (UiActionSetViewMode ViewTerrainForm))
          ; Just WidgetViewPlateId -> whenLeftView (submit (UiActionSetViewMode ViewPlateId))
          ; Just WidgetViewPlateBoundary -> whenLeftView (submit (UiActionSetViewMode ViewPlateBoundary))
          ; Just WidgetViewPlateHardness -> whenLeftView (submit (UiActionSetViewMode ViewPlateHardness))
          ; Just WidgetViewPlateCrust -> whenLeftView (submit (UiActionSetViewMode ViewPlateCrust))
          ; Just WidgetViewPlateAge -> whenLeftView (submit (UiActionSetViewMode ViewPlateAge))
          ; Just WidgetViewPlateHeight -> whenLeftView (submit (UiActionSetViewMode ViewPlateHeight))
          ; Just WidgetViewPlateVelocity -> whenLeftView (submit (UiActionSetViewMode ViewPlateVelocity))
          ; Just WidgetViewCloud -> whenLeftView (submit (UiActionSetViewMode ViewCloud))
          ; Just WidgetDayNightToggle -> whenLeftView (setUiDayNightEnabled uiHandle (not (uiDayNightEnabled uiSnap)) >> submit (UiActionRebuildAtlas (uiViewMode uiSnap)))
          -- Overlay cycling: prev/next overlay name, prev/next field
          ; Just WidgetViewOverlayPrev -> whenLeftView $ cycleOverlay uiSnap uiHandle (-1) submit
          ; Just WidgetViewOverlayNext -> whenLeftView $ cycleOverlay uiSnap uiHandle 1 submit
          ; Just WidgetViewFieldPrev -> whenLeftView $ cycleOverlayField uiSnap uiHandle (-1) submit
          ; Just WidgetViewFieldNext -> whenLeftView $ cycleOverlayField uiSnap uiHandle 1 submit
          ; Just WidgetLogDebug -> setLogMinLevel logHandle LogDebug
          ; Just WidgetLogInfo -> setLogMinLevel logHandle LogInfo
          ; Just WidgetLogWarn -> setLogMinLevel logHandle LogWarn
          ; Just WidgetLogError -> setLogMinLevel logHandle LogError
          ; Just WidgetLogHeader -> toggleLog
          ; Just _ -> pure ()
          ; Nothing -> pure ()
          }
  where
    widgetEnv :: InputEnv
    widgetEnv = icInputEnv inputContext

    actorHandles :: ActorHandles
    actorHandles = ieActorHandles widgetEnv

    uiHandle :: ActorHandle Ui (Protocol Ui)
    uiHandle = ahUiHandle actorHandles

    logHandle = ahLogHandle actorHandles

    dataHandle = ahDataHandle actorHandles

    pluginManagerHandle = ahPluginManagerHandle actorHandles

    simulationHandle = ahSimulationHandle actorHandles

    quitRef = icQuitRef inputContext

    -- Controls like presets, pipeline toggles, and sim/plugin management do
    -- not fit the generic slider model and stay on an explicit bespoke path.
    isBespokeConfigWidget :: WidgetId -> Bool
    isBespokeConfigWidget wid = case wid of
      WidgetConfigPresetSave -> True
      WidgetConfigPresetLoad -> True
      WidgetConfigReset -> True
      WidgetConfigRevert -> True
      WidgetPipelineToggle _ -> True
      WidgetSimTick -> True
      WidgetSimAutoTick -> True
      WidgetPluginMoveUp _ -> True
      WidgetPluginMoveDown _ -> True
      WidgetPluginExpand _ -> True
      WidgetPluginParamSlider _ _ -> True
      WidgetPluginParamCheck _ _ -> True
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

    bespokeConfigWidgetAllowed :: Bool -> ConfigTab -> WidgetId -> Bool
    bespokeConfigWidgetAllowed simReady tab wid = case wid of
      WidgetSimTick -> tab == ConfigPipeline && simReady
      WidgetSimAutoTick -> tab == ConfigPipeline
      WidgetPluginMoveUp _ -> tab == ConfigPipeline
      WidgetPluginMoveDown _ -> tab == ConfigPipeline
      WidgetPluginExpand _ -> tab == ConfigPipeline
      WidgetPluginParamSlider _ _ -> tab == ConfigPipeline
      WidgetPluginParamCheck _ _ -> tab == ConfigPipeline
      WidgetDataPluginSelect _ -> tab == ConfigData
      WidgetDataResourceSelect _ _ -> tab == ConfigData
      WidgetDataPagePrev _ _ -> tab == ConfigData
      WidgetDataPageNext _ _ -> tab == ConfigData
      WidgetDataRecordSelect _ -> tab == ConfigData
      WidgetDataDetailDismiss -> tab == ConfigData
      WidgetDataFieldToggle _ -> tab == ConfigData
      WidgetDataEditToggle -> tab == ConfigData
      WidgetDataEditSave -> tab == ConfigData
      WidgetDataEditCancel -> tab == ConfigData
      WidgetDataCreateNew -> tab == ConfigData
      WidgetDataDeleteBtn -> tab == ConfigData
      WidgetDataDeleteConfirm -> tab == ConfigData
      WidgetDataDeleteCancel -> tab == ConfigData
      WidgetDataFieldTextClick _ -> tab == ConfigData
      WidgetDataFieldStepMinus _ -> tab == ConfigData
      WidgetDataFieldStepPlus _ -> tab == ConfigData
      WidgetDataFieldBoolToggle _ -> tab == ConfigData
      WidgetDataFieldEnumPrev _ -> tab == ConfigData
      WidgetDataFieldEnumNext _ -> tab == ConfigData
      _ -> True

    handleBespokeConfigWidget :: Layout -> WidgetId -> V2 Int -> (IO () -> IO ()) -> UiState -> IO ()
    handleBespokeConfigWidget currentLayout wid clickPoint whenConfigVisible uiState = case wid of
      WidgetConfigPresetSave -> whenConfigVisible (openPresetSaveDialog currentLayout uiState)
      WidgetConfigPresetLoad -> whenConfigVisible (openPresetLoadDialog currentLayout)
      WidgetConfigReset -> whenConfigVisible (SDL.stopTextInput >> submit UiActionReset)
      WidgetConfigRevert -> whenConfigVisible (submit UiActionRevert)
      WidgetPipelineToggle name -> whenConfigVisible $ do
        case parseStageId name of
          Nothing -> pure ()
          Just sid -> do
            let current = uiDisabledStages uiState
                toggled :: Set.Set StageId
                toggled
                  | Set.member sid current = Set.delete sid current
                  | otherwise              = Set.insert sid current
                closed = disabledClosure builtinDependencies toggled
            setUiDisabledStages uiHandle closed
      WidgetSimTick -> whenConfigVisible $ do
        let count = uiSimTickCount uiState
            hasTerrain = dsTerrainChunks (ieDataSnapshot widgetEnv) > 0
        if hasTerrain
          then do
            appendLog logHandle (LogEntry LogInfo (Text.pack ("ui: tick button pressed -> request tick " <> show (count + 1))))
            requestSimTick simulationHandle (count + 1)
          else
            appendLog logHandle (LogEntry LogWarn "ui: tick ignored (no world terrain loaded yet)")
      WidgetSimAutoTick -> whenConfigVisible $ do
        setUiSimAutoTick uiHandle (not (uiSimAutoTick uiState))
      WidgetPluginMoveUp name -> whenConfigVisible $ do
        let names = uiPluginNames uiState
            swapped = swapWithPrev name names
        setUiPluginNames uiHandle swapped
        setPluginOrder pluginManagerHandle swapped
      WidgetPluginMoveDown name -> whenConfigVisible $ do
        let names = uiPluginNames uiState
            swapped = swapWithNext name names
        setUiPluginNames uiHandle swapped
        setPluginOrder pluginManagerHandle swapped
      WidgetPluginToggle name -> whenConfigVisible $ do
        let current = uiDisabledPlugins uiState
            toggled
              | Set.member name current = Set.delete name current
              | otherwise               = Set.insert name current
        setUiDisabledPlugins uiHandle toggled
        setDisabledPlugins pluginManagerHandle toggled
      WidgetPluginExpand name -> whenConfigVisible $ do
        let current = Map.findWithDefault False name (uiPluginExpanded uiState)
        setUiPluginExpanded uiHandle name (not current)
      WidgetPluginParamSlider pluginName paramName -> whenConfigVisible $ do
        -- Positional click-to-set: use click X relative to the bar rect
        let Rect (V2 barX _, V2 barW _) = pipelineParamBarRect 0 currentLayout
            V2 cx _ = clickPoint
            normalized = max 0 (min 1 (fromIntegral (cx - barX) / max 1 (fromIntegral barW))) :: Float
            specs = Map.findWithDefault [] pluginName (uiPluginParamSpecs uiState)
            mSpec = find (\s -> rpsName s == paramName) specs
            value = case mSpec >>= rpsRange of
              Just (Number lo, Number hi)
                | hi > lo ->
                    let v = realToFrac lo + realToFrac normalized * (realToFrac hi - realToFrac lo) :: Double
                    in Number (realToFrac (max (realToFrac lo) (min (realToFrac hi) v)))
              _ -> Number (realToFrac normalized)
        setUiPluginParam uiHandle pluginName paramName value
      WidgetPluginParamCheck pluginName paramName -> whenConfigVisible $ do
        let params = Map.findWithDefault Map.empty pluginName (uiPluginParams uiState)
            current = case Map.lookup paramName params of
                        Just (Bool b) -> b
                        _ -> False
        setUiPluginParam uiHandle pluginName paramName (Bool (not current))
      WidgetDataPluginSelect pluginName -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
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
        setUiDataBrowser uiHandle newDbs
      WidgetDataResourceSelect pluginName resourceName -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
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
        setUiDataBrowser uiHandle newDbs
        _ <- forkIO $ do
          let qr = QueryResource resourceName QueryAll (Just 20) (Just 0)
          result <- queryPluginResource pluginManagerHandle pluginName qr
          case result of
            Right qResult -> setUiDataBrowser uiHandle newDbs
              { dbsRecords = qrsRecords qResult
              , dbsTotalCount = qrsTotalCount qResult
              , dbsLoading = False
              }
            Left _err -> setUiDataBrowser uiHandle newDbs { dbsLoading = False }
        pure ()
      WidgetDataPagePrev pluginName resourceName -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            newOffset = max 0 (dbsPageOffset dbs - 20)
            newDbs = dbs
              { dbsPageOffset = newOffset
              , dbsLoading = True
              , dbsRecords = []
              , dbsSelectedRecord = Nothing
              , dbsSelectedRecordKey = Nothing
              , dbsSelectedRowIndex = Nothing
              , dbsExpandedFields = Set.empty
              }
        setUiDataBrowser uiHandle newDbs
        _ <- forkIO $ do
          let qr = QueryResource resourceName QueryAll (Just 20) (Just newOffset)
          result <- queryPluginResource pluginManagerHandle pluginName qr
          case result of
            Right qResult -> setUiDataBrowser uiHandle newDbs
              { dbsRecords = qrsRecords qResult
              , dbsTotalCount = qrsTotalCount qResult
              , dbsLoading = False
              }
            Left _err -> setUiDataBrowser uiHandle newDbs { dbsLoading = False }
        pure ()
      WidgetDataPageNext pluginName resourceName -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            newOffset = dbsPageOffset dbs + 20
            newDbs = dbs
              { dbsPageOffset = newOffset
              , dbsLoading = True
              , dbsRecords = []
              , dbsSelectedRecord = Nothing
              , dbsSelectedRecordKey = Nothing
              , dbsSelectedRowIndex = Nothing
              , dbsExpandedFields = Set.empty
              }
        setUiDataBrowser uiHandle newDbs
        _ <- forkIO $ do
          let qr = QueryResource resourceName QueryAll (Just 20) (Just newOffset)
          result <- queryPluginResource pluginManagerHandle pluginName qr
          case result of
            Right qResult -> setUiDataBrowser uiHandle newDbs
              { dbsRecords = qrsRecords qResult
              , dbsTotalCount = qrsTotalCount qResult
              , dbsLoading = False
              }
            Left _err -> setUiDataBrowser uiHandle newDbs { dbsLoading = False }
        pure ()
      WidgetDataRecordSelect idx -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            records = dbsRecords dbs
        case drop idx records of
          (rec : _) ->
            let keyField = case dbsSelectedResource dbs >>= \rName ->
                             dbsSelectedPlugin dbs >>= \pName ->
                               Map.lookup pName (uiDataResources uiState) >>= find (\s -> drsName s == rName) of
                             Just schema -> drsKeyField schema
                             Nothing -> "id"
                keyVal = Map.lookup keyField (unDataRecord rec)
                newDbs = dbs
                  { dbsSelectedRecord   = Just rec
                  , dbsSelectedRecordKey = keyVal
                  , dbsSelectedRowIndex  = Just idx
                  , dbsExpandedFields   = Set.empty
                  }
            in setUiDataBrowser uiHandle newDbs
          [] -> pure ()
      WidgetDataDetailDismiss -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
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
        setUiDataBrowser uiHandle newDbs
        SDL.stopTextInput
      WidgetDataFieldToggle path -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            expanded = dbsExpandedFields dbs
            newExpanded
              | Set.member path expanded = Set.delete path expanded
              | otherwise                = Set.insert path expanded
            newDbs = dbs { dbsExpandedFields = newExpanded }
        setUiDataBrowser uiHandle newDbs
      WidgetDataEditToggle -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
        if dbsEditMode dbs
          then do
            -- Exit edit mode, discard changes
            let newDbs = dbs
                  { dbsEditMode     = False
                  , dbsEditValues   = Map.empty
                  , dbsFocusedField = Nothing
                  , dbsTextCursor   = 0
                  }
            setUiDataBrowser uiHandle newDbs
            SDL.stopTextInput
          else do
            -- Enter edit mode, populate editValues from current record
            let vals = case dbsSelectedRecord dbs of
                  Just rec -> unDataRecord rec
                  Nothing  -> Map.empty
                newDbs = dbs
                  { dbsEditMode   = True
                  , dbsEditValues = vals
                  }
            setUiDataBrowser uiHandle newDbs
      WidgetDataEditSave -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
        case (dbsSelectedPlugin dbs, dbsSelectedResource dbs) of
          (Just pName, Just rName) -> do
            let editVals = dbsEditValues dbs
                newRecord = DataRecord editVals
            SDL.stopTextInput
            if dbsCreateMode dbs
              then do
                -- MutCreate
                let mr = MutateResource rName (MutCreate newRecord)
                _ <- forkIO $ do
                  result <- mutatePluginResource pluginManagerHandle pName mr
                  case result of
                    Right _mResult -> refreshDataBrowser dbs pName rName
                    Left _err -> pure ()
                let newDbs = dbs
                      { dbsCreateMode   = False
                      , dbsEditMode     = False
                      , dbsEditValues   = Map.empty
                      , dbsFocusedField = Nothing
                      , dbsTextCursor   = 0
                      , dbsSelectedRecord = Nothing
                      , dbsSelectedRecordKey = Nothing
                      , dbsSelectedRowIndex = Nothing
                      }
                setUiDataBrowser uiHandle newDbs
              else do
                -- MutUpdate
                case dbsSelectedRecordKey dbs of
                  Just keyVal -> do
                    let mr = MutateResource rName (MutUpdate keyVal newRecord)
                    _ <- forkIO $ do
                      result <- mutatePluginResource pluginManagerHandle pName mr
                      case result of
                        Right _mResult -> refreshDataBrowser dbs pName rName
                        Left _err -> pure ()
                    let newDbs = dbs
                          { dbsEditMode     = False
                          , dbsEditValues   = Map.empty
                          , dbsFocusedField = Nothing
                          , dbsTextCursor   = 0
                          }
                    setUiDataBrowser uiHandle newDbs
                  Nothing -> pure ()
          _ -> pure ()
      WidgetDataEditCancel -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
        if dbsCreateMode dbs
          then do
            -- Cancel create: dismiss the popover entirely
            let newDbs = dbs
                  { dbsCreateMode       = False
                  , dbsEditMode         = False
                  , dbsEditValues       = Map.empty
                  , dbsFocusedField     = Nothing
                  , dbsTextCursor       = 0
                  , dbsSelectedRecord   = Nothing
                  , dbsSelectedRecordKey = Nothing
                  , dbsSelectedRowIndex = Nothing
                  }
            setUiDataBrowser uiHandle newDbs
            SDL.stopTextInput
          else do
            -- Cancel edit: revert to read-only
            let newDbs = dbs
                  { dbsEditMode     = False
                  , dbsEditValues   = Map.empty
                  , dbsFocusedField = Nothing
                  , dbsTextCursor   = 0
                  }
            setUiDataBrowser uiHandle newDbs
            SDL.stopTextInput
      WidgetDataCreateNew -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            -- Build default values from schema field definitions
            mSchema = do
              pName <- dbsSelectedPlugin dbs
              rName <- dbsSelectedResource dbs
              schemas <- Map.lookup pName (uiDataResources uiState)
              find (\s -> drsName s == rName) schemas
            defaults = case mSchema of
              Just schema -> Map.fromList
                [ (dfName f, v)
                | f <- drsFields schema
                , Just v <- [dfDefault f]
                ]
              Nothing -> Map.empty
            dummyRecord = DataRecord defaults
            newDbs = dbs
              { dbsCreateMode       = True
              , dbsEditMode         = True
              , dbsEditValues       = defaults
              , dbsSelectedRecord   = Just dummyRecord
              , dbsSelectedRecordKey = Nothing
              , dbsSelectedRowIndex = Just 0
              , dbsExpandedFields   = Set.empty
              , dbsFocusedField     = Nothing
              , dbsTextCursor       = 0
              }
        setUiDataBrowser uiHandle newDbs
      WidgetDataDeleteBtn -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            newDbs = dbs { dbsDeleteConfirm = True }
        setUiDataBrowser uiHandle newDbs
      WidgetDataDeleteConfirm -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
        case (dbsSelectedPlugin dbs, dbsSelectedResource dbs, dbsSelectedRecordKey dbs) of
          (Just pName, Just rName, Just keyVal) -> do
            let mr = MutateResource rName (MutDelete keyVal)
            _ <- forkIO $ do
              result <- mutatePluginResource pluginManagerHandle pName mr
              case result of
                Right _mResult -> refreshDataBrowser dbs pName rName
                Left _err -> pure ()
            let newDbs = dbs
                  { dbsDeleteConfirm    = False
                  , dbsSelectedRecord   = Nothing
                  , dbsSelectedRecordKey = Nothing
                  , dbsSelectedRowIndex = Nothing
                  , dbsExpandedFields   = Set.empty
                  }
            setUiDataBrowser uiHandle newDbs
          _ -> pure ()
      WidgetDataDeleteCancel -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            newDbs = dbs { dbsDeleteConfirm = False }
        setUiDataBrowser uiHandle newDbs
      WidgetDataFieldTextClick path -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            currentVal = case Map.lookup path (dbsEditValues dbs) of
              Just (String t) -> Text.length t
              _ -> 0
            newDbs = dbs
              { dbsFocusedField = Just path
              , dbsTextCursor   = currentVal
              }
        setUiDataBrowser uiHandle newDbs
        -- Start SDL text input
        let Rect (V2 rx ry, V2 rw rh) = configPanelRect currentLayout
            rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
        SDL.startTextInput rawRect
      WidgetDataFieldStepMinus path -> whenConfigVisible $ do
        bumpFieldValue uiState path (-1)
      WidgetDataFieldStepPlus path -> whenConfigVisible $ do
        bumpFieldValue uiState path 1
      WidgetDataFieldBoolToggle path -> whenConfigVisible $ do
        let dbs = uiDataBrowser uiState
            current = case Map.lookup path (dbsEditValues dbs) of
              Just (Bool b) -> b
              _ -> False
            newDbs = dbs { dbsEditValues = Map.insert path (Bool (not current)) (dbsEditValues dbs) }
        setUiDataBrowser uiHandle newDbs
      WidgetDataFieldEnumPrev path -> whenConfigVisible $ do
        cycleEnumValue uiState path (-1)
      WidgetDataFieldEnumNext path -> whenConfigVisible $ do
        cycleEnumValue uiState path 1
      _ -> pure ()

    handleMenuClick ly _widgets point = do
      uiSnap <- getUiSnapshot uiHandle
      case uiMenuMode uiSnap of
        MenuEscape -> handleEscapeMenuClick ly point
        MenuPresetSave -> handlePresetSaveClick ly point uiSnap
        MenuPresetLoad -> handlePresetLoadClick ly point uiSnap
        MenuWorldSave  -> handleWorldSaveClick ly point uiSnap
        MenuWorldLoad  -> handleWorldLoadClick ly point uiSnap
        _ -> pure ()

    handleEscapeMenuClick ly point
      | containsPoint (menuExitRect ly) point = do
          setUiMenuMode uiHandle MenuNone
          writeIORef quitRef True
      | containsPoint (menuSaveRect ly) point = do
          uiSnap <- getUiSnapshot uiHandle
          openWorldSaveDialog ly uiSnap
      | containsPoint (menuLoadRect ly) point =
          openWorldLoadDialog ly
      | otherwise = setUiMenuMode uiHandle MenuNone

    handlePresetSaveClick ly point uiSnap
      | containsPoint (presetSaveOkRect ly) point = confirmPresetSave uiSnap
      | containsPoint (presetSaveCancelRect ly) point = cancelPresetDialog
      | otherwise = pure ()

    handlePresetLoadClick ly point uiSnap
      | containsPoint (presetLoadOkRect ly) point = confirmPresetLoad uiSnap
      | containsPoint (presetLoadCancelRect ly) point = cancelPresetDialog
      | containsPoint (presetLoadListRect ly) point = do
          let V2 _mx my = point
              Rect (V2 _lx listY, _) = presetLoadListRect ly
              fText = Text.toLower (uiPresetFilter uiSnap)
              filteredCount = length (filter (\n -> Text.isInfixOf fText (Text.toLower n)) (uiPresetList uiSnap))
              idx = min (max 0 (filteredCount - 1)) ((my - listY) `div` 24)
          when (filteredCount > 0) $ setUiPresetSelected uiHandle idx
      | otherwise = pure ()

    -- Preset dialog helpers

    openPresetSaveDialog ly uiSnap = do
      let defaultName = "preset-" <> Text.pack (show (uiSeed uiSnap))
          Rect (V2 rx ry, V2 rw rh) = presetSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiPresetInput uiHandle defaultName
      setUiMenuMode uiHandle MenuPresetSave
      SDL.startTextInput rawRect

    openPresetLoadDialog ly = do
      names <- listSnapshots
      setUiPresetList uiHandle names
      setUiPresetSelected uiHandle 0
      setUiPresetFilter uiHandle Text.empty
      setUiMenuMode uiHandle MenuPresetLoad
      let Rect (V2 rx ry, V2 rw rh) = presetLoadFilterRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect

    cancelPresetDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetSave uiSnap = do
      let name = uiPresetInput uiSnap
      dir <- snapshotDir
      let path = dir </> Text.unpack name <> ".json"
      _result <- saveSnapshot path (snapshotFromUi uiSnap name)
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetLoad uiSnap = do
      let fText = Text.toLower (uiPresetFilter uiSnap)
          names = filter (\n -> Text.isInfixOf fText (Text.toLower n)) (uiPresetList uiSnap)
          sel = uiPresetSelected uiSnap
      when (sel >= 0 && sel < length names) $ do
        let name = names !! sel
        dir <- snapshotDir
        let path = dir </> Text.unpack name <> ".json"
        result <- loadSnapshot path
        case result of
          Right cp -> applySnapshotToUi cp uiHandle
          Left _err -> pure ()
      setUiMenuMode uiHandle MenuNone

    -- World save/load click handlers

    handleWorldSaveClick ly point uiSnap
      | containsPoint (worldSaveOkRect ly) point = confirmWorldSave uiSnap
      | containsPoint (worldSaveCancelRect ly) point = cancelWorldDialog
      | otherwise = pure ()

    handleWorldLoadClick ly point uiSnap
      | containsPoint (worldLoadOkRect ly) point = confirmWorldLoad uiSnap
      | containsPoint (worldLoadCancelRect ly) point = cancelWorldDialog
      | containsPoint (worldLoadListRect ly) point = do
          let V2 _mx my = point
              Rect (V2 _lx listY, _) = worldLoadListRect ly
              fText = Text.toLower (uiWorldFilter uiSnap)
              filteredCount = length (filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m))) (uiWorldList uiSnap))
              idx = min (max 0 (filteredCount - 1)) ((my - listY) `div` 28)
          when (filteredCount > 0) $ setUiWorldSelected uiHandle idx
      | otherwise = pure ()

    -- World dialog helpers

    openWorldSaveDialog ly uiSnap = do
      let defaultName = uiWorldName uiSnap
          Rect (V2 rx ry, V2 rw rh) = worldSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiWorldSaveInput uiHandle defaultName
      setUiMenuMode uiHandle MenuWorldSave
      SDL.startTextInput rawRect

    openWorldLoadDialog ly = do
      worlds <- listWorlds
      setUiWorldList uiHandle worlds
      setUiWorldSelected uiHandle 0
      setUiWorldFilter uiHandle Text.empty
      setUiMenuMode uiHandle MenuWorldLoad
      let Rect (V2 rx ry, V2 rw rh) = worldLoadFilterRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect

    cancelWorldDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldSave uiSnap = do
      let name = uiWorldSaveInput uiSnap
      when (not (Text.null name)) $ do
        terrainSnap <- getTerrainSnapshot dataHandle
        let world = snapshotToWorld uiSnap terrainSnap
        pluginDirs <- getPluginDataDirectories pluginManagerHandle
        _result <- saveNamedWorldWithPlugins name uiSnap world pluginDirs
        -- Notify plugins of the saved world path
        wDir <- worldDir
        notifyWorldChanged pluginManagerHandle (Just (Text.pack (wDir </> Text.unpack name)))
        setUiWorldName uiHandle name
        setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap name))
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldLoad uiSnap = do
      let fText = Text.toLower (uiWorldFilter uiSnap)
          manifests = filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m))) (uiWorldList uiSnap)
          sel = uiWorldSelected uiSnap
      when (sel >= 0 && sel < length manifests) $ do
        let manifest = manifests !! sel
            name = wsmName manifest
        result <- loadNamedWorld name
        case result of
          Right (_manifest, snapshot, world) -> do
            replaceTerrainData dataHandle world
            setSimWorld simulationHandle world
            setUiOverlayNames uiHandle (overlayNames (twOverlays world))
            dataSnap <- getDataSnapshot dataHandle
            terrainSnap' <- getTerrainSnapshot dataHandle
            writeDataSnapshot (ahDataSnapshotRef actorHandles) dataSnap
            writeTerrainSnapshot (ahTerrainSnapshotRef actorHandles) terrainSnap'
            bumpSnapshotVersion (ahSnapshotVersionRef actorHandles)
            applySnapshotToUi snapshot uiHandle
            setUiWorldName uiHandle name
            setUiWorldConfig uiHandle (Just snapshot)
            -- Notify plugins of the loaded world path
            wDir <- worldDir
            notifyWorldChanged pluginManagerHandle (Just (Text.pack (wDir </> Text.unpack name)))
            submit (UiActionRebuildAtlas (uiViewMode uiSnap))
          Left _err -> pure ()
      setUiMenuMode uiHandle MenuNone

    bumpSeed delta = do
      uiSnap <- getUiSnapshot uiHandle
      let newSeed = uiSeed uiSnap + fromIntegral delta
      setUiSeed uiHandle newSeed
      setUiSeedInput uiHandle (Text.pack (show newSeed))
    bumpChunk delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiChunkSize uiHandle (uiChunkSize uiSnap + delta)
    toggleLog = do
      logSnap <- getLogSnapshot logHandle
      setLogCollapsed logHandle (not (lsCollapsed logSnap))
    whenConfigVisible action = do
      let uiSnap = ieUiSnapshot widgetEnv
      when (uiShowConfig uiSnap) action
    toggleConfig = do
      uiSnap <- getUiSnapshot uiHandle
      setUiShowConfig uiHandle (not (uiShowConfig uiSnap))
    submit action =
      submitAction widgetEnv action
    startSeedEdit rect = do
      let uiSnap = ieUiSnapshot widgetEnv
          Rect (V2 rx ry, V2 rw rh) = rect
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect
      setUiSeedEditing uiHandle True
      setUiSeedInput uiHandle (Text.pack (show (uiSeed uiSnap)))
    randomSeed = do
      seed <- randomIO
      setUiSeed uiHandle seed
      setUiSeedInput uiHandle (Text.pack (show seed))
      setUiSeedEditing uiHandle False
      SDL.stopTextInput

    -- | Re-query the current resource and update the data browser records.
    refreshDataBrowser :: DataBrowserState -> Text.Text -> Text.Text -> IO ()
    refreshDataBrowser dbs pName rName = do
      let offset = dbsPageOffset dbs
          qr = QueryResource rName QueryAll (Just 20) (Just offset)
      result <- queryPluginResource pluginManagerHandle pName qr
      uiSnap' <- getUiSnapshot uiHandle
      let dbs' = uiDataBrowser uiSnap'
      case result of
        Right qResult -> setUiDataBrowser uiHandle dbs'
          { dbsRecords    = qrsRecords qResult
          , dbsTotalCount = qrsTotalCount qResult
          , dbsLoading    = False
          }
        Left _err -> setUiDataBrowser uiHandle dbs' { dbsLoading = False }

    -- | Bump a numeric field value by a delta direction.
    bumpFieldValue :: UiState -> Text.Text -> Int -> IO ()
    bumpFieldValue uiSt path dir = do
      let dbs = uiDataBrowser uiSt
          editVals = dbsEditValues dbs
          currentVal = Map.lookup path editVals
          delta = fromIntegral dir :: Double
          newVal = case currentVal of
            Just (Number n) -> Number (realToFrac (realToFrac n + delta :: Double))
            Just (String t)
              | Just d <- readDouble t -> Number (realToFrac (d + delta))
            _ -> Number (realToFrac delta)
          newDbs = dbs { dbsEditValues = Map.insert path newVal editVals }
      setUiDataBrowser uiHandle newDbs

    readDouble :: Text.Text -> Maybe Double
    readDouble t = case reads (Text.unpack t) of
      [(d, "")] -> Just d
      _         -> Nothing

    -- | Cycle a DFEnum field value by a delta direction.
    cycleEnumValue :: UiState -> Text.Text -> Int -> IO ()
    cycleEnumValue uiSt path dir = do
      let dbs = uiDataBrowser uiSt
          editVals = dbsEditValues dbs
          mSchema = do
            pName <- dbsSelectedPlugin dbs
            rName <- dbsSelectedResource dbs
            schemas <- Map.lookup pName (uiDataResources uiSt)
            find (\s -> drsName s == rName) schemas
          mFieldType = mSchema >>= \schema -> lookupFieldType path (drsFields schema)
      case mFieldType of
        Just (DFEnum options) | not (null options) -> do
          let currentText = case Map.lookup path editVals of
                Just (String t) -> t
                _ -> ""
              currentIdx = maybe 0 id (findIndex (== currentText) options)
              n = length options
              newIdx = (currentIdx + dir + n) `mod` n
              newVal = String (options !! newIdx)
              newDbs = dbs { dbsEditValues = Map.insert path newVal editVals }
          setUiDataBrowser uiHandle newDbs
        _ -> pure ()

    -- | Look up the field type for a dot-path.
    lookupFieldType :: Text.Text -> [DataFieldDef] -> Maybe DataFieldType
    lookupFieldType path' defs = case Text.splitOn "." path' of
      []     -> Nothing
      (k:ks) -> case filter (\d -> dfName d == k) defs of
        (d:_) -> resolveRest ks (dfType d)
        []    -> Nothing
      where
        resolveRest [] ft = Just ft
        resolveRest (s:ss) (DFRecord subDefs') =
          case filter (\d -> dfName d == s) subDefs' of
            (d:_) -> resolveRest ss (dfType d)
            []    -> Nothing
        resolveRest _ _ = Nothing

    signedSliderDelta sliderPart step = case sliderPart of
      SliderPartMinus -> negate step
      SliderPartPlus -> step
-- | Cycle through available overlay names by @dir@ (+1 or -1).
--
-- When no overlays are available, does nothing.  When cycling past the
-- end, wraps to ViewElevation (no overlay); from ViewElevation, wraps to
-- the first overlay.
cycleOverlay :: UiState -> ActorHandle Ui (Protocol Ui) -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlay uiSnap uiHandle dir submit = do
  let names = uiOverlayNames uiSnap
  if null names
    then pure ()
    else do
      let currentIdx = case uiViewMode uiSnap of
            ViewOverlay name _ ->
              case findIndex (== name) names of
                Just i  -> i + 1  -- +1 because index 0 = "no overlay"
                Nothing -> 0
            _ -> 0
          total = length names + 1  -- +1 for "no overlay" position
          newIdx = (currentIdx + dir) `mod` total
      if newIdx == 0
        then submit (UiActionSetViewMode ViewElevation)
        else do
          let overlayName = names !! (newIdx - 1)
          -- Reset to first field when switching overlay
          submit (UiActionSetViewMode (ViewOverlay overlayName 0))

-- | Cycle through fields within the currently-selected overlay.
--
-- Only effective when in 'ViewOverlay' mode with fields available.
cycleOverlayField :: UiState -> ActorHandle Ui (Protocol Ui) -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlayField uiSnap _uiHandle dir submit =
  case uiViewMode uiSnap of
    ViewOverlay name fieldIdx -> do
      let fields = uiOverlayFields uiSnap
          fieldCount = length fields
      if fieldCount <= 0
        then pure ()
        else do
          let newIdx = (fieldIdx + dir) `mod` fieldCount
          submit (UiActionSetViewMode (ViewOverlay name newIdx))
    _ -> pure ()

-- | Swap an element with the one before it in a list.
--
-- If the element is at index 0 or not found, the list is returned unchanged.
swapWithPrev :: Eq a => a -> [a] -> [a]
swapWithPrev _ [] = []
swapWithPrev target (x:xs)
  | x == target = x : xs  -- already first, no swap
  | otherwise   = go x xs
  where
    go prev []     = [prev]  -- target not found, reconstruct
    go prev (y:ys)
      | y == target = target : prev : ys
      | otherwise   = prev : go y ys

-- | Swap an element with the one after it in a list.
--
-- If the element is last or not found, the list is returned unchanged.
swapWithNext :: Eq a => a -> [a] -> [a]
swapWithNext _ [] = []
swapWithNext target xs = go xs
  where
    go []         = []
    go [y]        = [y]  -- last element, no swap
    go (y:z:ys)
      | y == target = z : y : ys
      | otherwise   = y : go (z:ys)
