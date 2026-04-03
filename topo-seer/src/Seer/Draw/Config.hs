{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Config
  ( drawConfigPanel
  , drawConfigTabs
  , drawDataDetailPopover
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.UI (ConfigTab(..), DataBrowserState(..), UiState(..), builtinStageRowCount, configRowCount, pluginRowIndex, pluginRowsWithParams)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderRegistry (SliderDef(..))
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (sliderDefsForConfigTab, sliderValueForId)
import Topo.Pipeline.Stage (allBuiltinStageIds)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataFieldDef(..), DataFieldType(..), DataConstructorDef(..), DataOperations(..), noOperations)
import Topo.Plugin.RPC.DataService (DataRecord(..))
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.Layout
  ( ConfigParamRowRects(..)
  , Layout
  , configPanelRect
  , configParamRects
  , configPresetLoadRect
  , configPresetSaveRect
  , configResetRect
  , configRevertRect
  , configRowTopPad
  , configScrollAreaRect
  , configScrollBarRect
  , configTabRects
  , pipelineCheckboxRect
  , pipelineMoveDownRect
  , pipelineMoveUpRect
  , pipelineTickButtonRect
  , pipelineTickRateBarRect
  , pipelineExpandRect
  , pipelineParamBarRect
  , pipelineParamCheckRect
  , dataBrowserItemRect
  , dataBrowserCreateButtonRect
  , dataDetailPopoverRect
  , dataDetailFieldRect
  , dataDetailEditToggleRect
  , dataDetailSaveRect
  , dataDetailCancelRect
  , dataDetailDeleteRect
  , dataDetailFieldInputRect
  , dataDetailFieldStepMinusRect
  , dataDetailFieldStepPlusRect
  , deleteConfirmDialogRect
  , deleteConfirmOkRect
  , deleteConfirmCancelRect
  )
import UI.Font (FontCache, drawText)
import UI.Widgets (Rect(..))
import UI.Theme
import UI.WidgetsDraw (rectToSDL)

-- | Normalize a numeric parameter value into [0,1] using the spec's range bounds.
--   Falls back to 0.5 when range is missing or invalid.
normalizeParam :: RPCParamSpec -> Double -> Float
normalizeParam spec val =
  case rpsRange spec of
    Just (Number lo, Number hi)
      | hi > lo   -> max 0 . min 1 $ realToFrac ((val - realToFrac lo) / (realToFrac hi - realToFrac lo))
    _             -> max 0 (min 1 (realToFrac val))

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData) = do
  drawTab tabTerrain (uiConfigTab ui == ConfigTerrain)
  drawTab tabPlanet (uiConfigTab ui == ConfigPlanet)
  drawTab tabClimate (uiConfigTab ui == ConfigClimate)
  drawTab tabWeather (uiConfigTab ui == ConfigWeather)
  drawTab tabBiome (uiConfigTab ui == ConfigBiome)
  drawTab tabErosion (uiConfigTab ui == ConfigErosion)
  drawTab tabPipeline (uiConfigTab ui == ConfigPipeline)
  drawTab tabData (uiConfigTab ui == ConfigData)
  where
    drawTab rect isActive = do
      let fill = if isActive then colTabActive else colTabInactive
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

drawConfigPanel :: SDL.Renderer -> Maybe FontCache -> UiState -> DataSnapshot -> Layout -> IO ()
drawConfigPanel renderer mFontCache ui dataSnap layout =
  let rect = configPanelRect layout
      tabs = configTabRects layout
      presetSaveRect = configPresetSaveRect layout
      presetLoadRect = configPresetLoadRect layout
      resetRect = configResetRect layout
      revertRect = configRevertRect layout
      scrollAreaRect = configScrollAreaRect layout
      scrollBarRect = configScrollBarRect layout
      rowHeight = 24
      gap = 10
      rows = configRowCount (uiConfigTab ui) ui
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _, V2 _ scrollH) = scrollAreaRect
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
      scrollRect (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      activeSliderDefs = sliderDefsForConfigTab (uiConfigTab ui)
      sliderRects sliderDef =
        let ConfigParamRowRects _ minusRect barRect plusRect =
              configParamRects (sliderRowIndex sliderDef) layout
        in (minusRect, barRect, plusRect)
      drawSliderDef sliderDef =
        let sid = sliderId sliderDef
            (minusRect, barRect, plusRect) = sliderRects sliderDef
            sliderStyle = sliderStyleForId sid
        in drawConfigSlider
            renderer
            (sliderValueForId ui sid)
            (scrollRect minusRect)
            (scrollRect barRect)
            (scrollRect plusRect)
            (sliderStyleFillColor sliderStyle)
  in if uiShowConfig ui
    then do
      SDL.rendererDrawColor renderer SDL.$= colConfigPanel
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawConfigTabs renderer ui tabs
      SDL.rendererDrawColor renderer SDL.$= colConfigScrollArea
      SDL.fillRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererDrawColor renderer SDL.$= colConfigBorder
      SDL.drawRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollAreaRect)
      case uiConfigTab ui of
        ConfigPipeline -> do
          let stages = allBuiltinStageIds
              disabled = uiDisabledStages ui
              plugins = uiPluginNames ui
              checkboxSize = 16
          forM_ (zip [0 ..] stages) $ \(idx, sid) -> do
            let isDisabled = Set.member sid disabled
                Rect (V2 checkX checkY, V2 _ _ ) = scrollRect (pipelineCheckboxRect idx layout)
                checkColor = if isDisabled then colPipelineCheckDisabled else colPipelineCheckEnabled
                borderColor = if isDisabled then colPipelineCheckDisabledBorder else colPipelineCheckEnabledBorder
            SDL.rendererDrawColor renderer SDL.$= checkColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= borderColor
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
          let pluginOffset = length stages
          forM_ (zip [0 ..] plugins) $ \(idx, pName) -> do
            let rowIndex = pluginRowIndex ui idx
                Rect (V2 checkX checkY, V2 _ _) = scrollRect (pipelineCheckboxRect rowIndex layout)
                isPluginDisabled = Set.member pName (uiDisabledPlugins ui)
                pluginColor = if isPluginDisabled then colPipelineCheckDisabled else colPipelinePlugin
                pluginBorder = if isPluginDisabled then colPipelineCheckDisabledBorder else colPipelinePluginBorder
                btnSize = 14
                Rect (V2 upX btnY, V2 _ _) = scrollRect (pipelineMoveUpRect rowIndex layout)
                Rect (V2 downX _downY, V2 _ _) = scrollRect (pipelineMoveDownRect rowIndex layout)
                arrowColor = colPipelineArrow
                arrowBorder = colPipelineArrowBorder
                -- Expand toggle
                isExpanded = Map.findWithDefault False pName (uiPluginExpanded ui)
                Rect (V2 expX expY, V2 expW expH) = scrollRect (pipelineExpandRect rowIndex layout)
                expColor = if isExpanded then colPipelineExpandActive else colPipelineExpandInactive
            SDL.rendererDrawColor renderer SDL.$= pluginColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= pluginBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= colPipelineArrowBg
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 upX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 upX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowColor
            let upMidX = upX + btnSize `div` 2
                upTop = btnY + 3
                upBot = btnY + btnSize - 3
            forM_ [upTop .. upBot] $ \row -> do
              let halfW = (row - upTop) * (btnSize `div` 2 - 2) `div` max 1 (upBot - upTop)
              SDL.drawLine renderer
                (SDL.P (V2 (fromIntegral (upMidX - halfW)) (fromIntegral row)))
                (SDL.P (V2 (fromIntegral (upMidX + halfW)) (fromIntegral row)))
            SDL.rendererDrawColor renderer SDL.$= colPipelineArrowBg
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 downX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 downX btnY, V2 btnSize btnSize))))
            SDL.rendererDrawColor renderer SDL.$= arrowColor
            let dnMidX = downX + btnSize `div` 2
                dnTop = btnY + 3
                dnBot = btnY + btnSize - 3
            forM_ [dnTop .. dnBot] $ \row -> do
              let halfW = (dnBot - row) * (btnSize `div` 2 - 2) `div` max 1 (dnBot - dnTop)
              SDL.drawLine renderer
                (SDL.P (V2 (fromIntegral (dnMidX - halfW)) (fromIntegral row)))
                (SDL.P (V2 (fromIntegral (dnMidX + halfW)) (fromIntegral row)))
            -- Expand toggle: small filled square
            SDL.rendererDrawColor renderer SDL.$= expColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 expX expY, V2 expW expH))))
            SDL.rendererDrawColor renderer SDL.$= colPipelineExpandBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 expX expY, V2 expW expH))))
            -- Draw parameter sub-rows when expanded
            when isExpanded $ do
              let specs = Map.findWithDefault [] pName (uiPluginParamSpecs ui)
                  params = Map.findWithDefault Map.empty pName (uiPluginParams ui)
              forM_ (zip [0..] specs) $ \(pIdx, spec) -> do
                let paramRowIdx = rowIndex + 1 + pIdx
                case rpsType spec of
                  ParamBool -> do
                    let Rect (V2 pcX pcY, V2 pcW pcH) = scrollRect (pipelineParamCheckRect paramRowIdx layout)
                        isChecked = case Map.lookup (rpsName spec) params of
                                      Just (Bool b) -> b
                                      _             -> False
                        boolColor = if isChecked then colPipelineCheckEnabled else colPipelineCheckDisabled
                        boolBorder = if isChecked then colPipelineCheckEnabledBorder else colPipelineCheckDisabledBorder
                    SDL.rendererDrawColor renderer SDL.$= boolColor
                    SDL.fillRect renderer (Just (rectToSDL (Rect (V2 pcX pcY, V2 pcW pcH))))
                    SDL.rendererDrawColor renderer SDL.$= boolBorder
                    SDL.drawRect renderer (Just (rectToSDL (Rect (V2 pcX pcY, V2 pcW pcH))))
                  _ -> do
                    let barRect = scrollRect (pipelineParamBarRect paramRowIdx layout)
                        paramVal = case Map.lookup (rpsName spec) params of
                                     Just (Number n) -> normalizeParam spec (realToFrac n)
                                     _               -> 0.5
                    SDL.rendererDrawColor renderer SDL.$= colSliderTrack
                    SDL.fillRect renderer (Just (rectToSDL barRect))
                    drawBarFill renderer paramVal barRect colPipelineParamBarFill
          let simOffset = builtinStageRowCount + pluginRowsWithParams ui
              simWorldReady = dsTerrainChunks dataSnap > 0
              tickBtnRect = scrollRect (pipelineTickButtonRect simOffset layout)
              tickBtnColor = if simWorldReady then colPipelineTickEnabled else colPipelineTickDisabled
              Rect (V2 autoTickCheckX autoTickCheckY, V2 _ _) = scrollRect (pipelineCheckboxRect (simOffset + 1) layout)
              autoTickColor = if uiSimAutoTick ui then colPipelineCheckEnabled else colPipelineCheckDisabled
              autoTickBorder = if uiSimAutoTick ui then colPipelineCheckEnabledBorder else colPipelineCheckDisabledBorder
              tickRateBarRect = scrollRect (pipelineTickRateBarRect (simOffset + 2) layout)
          SDL.rendererDrawColor renderer SDL.$= tickBtnColor
          SDL.fillRect renderer (Just (rectToSDL tickBtnRect))
          SDL.rendererDrawColor renderer SDL.$= autoTickColor
          SDL.fillRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= autoTickBorder
          SDL.drawRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= colSliderTrack
          SDL.fillRect renderer (Just (rectToSDL tickRateBarRect))
          drawBarFill renderer (uiSimTickRate ui) tickRateBarRect colPipelineTickRateBarFill
        ConfigData -> do
          -- Data browser: list plugins with data resources, selected resource's records
          let dbs = uiDataBrowser ui
              resources = uiDataResources ui
              pluginNames = Map.keys resources
              selectedPlugin = dbsSelectedPlugin dbs
              selectedResource = dbsSelectedResource dbs
          -- Draw plugin rows
          forM_ (zip [0..] pluginNames) $ \(idx, pName) -> do
            let Rect (V2 rx ry, V2 rw rh) = scrollRect (dataBrowserItemRect idx layout)
                isSelected = selectedPlugin == Just pName
                fillColor = if isSelected then colDataListSelActive else colDataListSelInactive
                borderColor = if isSelected then colDataListSelActiveBorder else colDataListSelInactiveBorder
            SDL.rendererDrawColor renderer SDL.$= fillColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
            SDL.rendererDrawColor renderer SDL.$= borderColor
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
          -- Draw resource rows beneath when a plugin is selected
          let resourceOffset = length pluginNames
          case selectedPlugin of
            Nothing -> pure ()
            Just pName -> do
              let schemas = Map.findWithDefault [] pName resources
              forM_ (zip [0..] schemas) $ \(rIdx, schema) -> do
                let rowIdx = resourceOffset + rIdx
                    Rect (V2 rx ry, V2 rw rh) = scrollRect (dataBrowserItemRect rowIdx layout)
                    isSelected = selectedResource == Just (drsName schema)
                    fillColor = if isSelected then colDataResourceActive else colDataResourceInactive
                    borderColor = if isSelected then colDataResourceActiveBorder else colDataResourceInactiveBorder
                SDL.rendererDrawColor renderer SDL.$= fillColor
                SDL.fillRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
                SDL.rendererDrawColor renderer SDL.$= borderColor
                SDL.drawRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
              -- Draw record rows when a resource is selected
              let recordOffset = resourceOffset + length schemas
                  records = dbsRecords dbs
              forM_ (zip [0..] records) $ \(recIdx, _record) -> do
                let rowIdx = recordOffset + recIdx
                    Rect (V2 rx ry, V2 rw rh) = scrollRect (dataBrowserItemRect rowIdx layout)
                    isSelected = dbsSelectedRowIndex dbs == Just recIdx
                    fillColor = if isSelected then colDataRecordSelected else colDataRecordBg
                SDL.rendererDrawColor renderer SDL.$= fillColor
                SDL.fillRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
                SDL.rendererDrawColor renderer SDL.$= colDataRecordBorder
                SDL.drawRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
              -- Loading indicator
              when (dbsLoading dbs) $ do
                let loadRow = recordOffset + length records
                    Rect (V2 lx ly, V2 _lw lh) = scrollRect (dataBrowserItemRect loadRow layout)
                SDL.rendererDrawColor renderer SDL.$= colDataLoadingIndicator
                SDL.fillRect renderer (Just (rectToSDL (Rect (V2 lx ly, V2 40 lh))))
              -- Create button (+ button after records, if resource supports it)
              case selectedResource of
                Just rName -> do
                  let mSchema = find (\s -> drsName s == rName) schemas
                      canCreate = maybe False (doCreate . drsOperations) mSchema
                      pageRow = recordOffset + length records
                      createRow = pageRow + (if null records then 0 else 1)
                  when canCreate $ do
                    let createRect = scrollRect (dataBrowserCreateButtonRect createRow layout)
                    SDL.rendererDrawColor renderer SDL.$= colDataCreateBtn
                    SDL.fillRect renderer (Just (rectToSDL createRect))
                    case mFontCache of
                      Just fc -> do
                        let Rect (V2 cx cy, V2 _cw ch) = createRect
                        drawText fc colDataDetailFieldValue (V2 (cx + 8) (cy + (ch - 12) `div` 2)) "+"
                      Nothing -> pure ()
                Nothing -> pure ()
              where
                find p = foldr (\x acc -> if p x then Just x else acc) Nothing
        _ -> forM_ activeSliderDefs drawSliderDef
      SDL.rendererClipRect renderer SDL.$= Nothing
      let Rect (V2 bx by, V2 bw bh) = scrollBarRect
          handleH = if maxOffset == 0 then bh else max 12 (bh * scrollH `div` max 1 contentHeight)
          handleY = if maxOffset == 0 then by else by + (bh - handleH) * scrollY `div` maxOffset
      SDL.rendererDrawColor renderer SDL.$= colScrollbarTrack
      SDL.fillRect renderer (Just (rectToSDL scrollBarRect))
      SDL.rendererDrawColor renderer SDL.$= colScrollbarHandle
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 bx handleY, V2 bw handleH))))
      SDL.rendererDrawColor renderer SDL.$= colConfigPresetSave
      SDL.fillRect renderer (Just (rectToSDL presetSaveRect))
      SDL.rendererDrawColor renderer SDL.$= colConfigPresetLoad
      SDL.fillRect renderer (Just (rectToSDL presetLoadRect))
      SDL.rendererDrawColor renderer SDL.$= colConfigReset
      SDL.fillRect renderer (Just (rectToSDL resetRect))
      let revertColor = case uiWorldConfig ui of
            Just _ -> colConfigRevertActive
            Nothing -> colConfigRevertDimmed
      SDL.rendererDrawColor renderer SDL.$= revertColor
      SDL.fillRect renderer (Just (rectToSDL revertRect))
    else pure ()

drawConfigSlider :: SDL.Renderer -> Float -> Rect -> Rect -> Rect -> V4 Word8 -> IO ()
drawConfigSlider renderer value minusRect barRect plusRect fillColor = do
  SDL.rendererDrawColor renderer SDL.$= colSliderBtn
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= colSliderTrack
  SDL.fillRect renderer (Just (rectToSDL barRect))
  drawBarFill renderer value barRect fillColor

drawBarFill :: SDL.Renderer -> Float -> Rect -> V4 Word8 -> IO ()
drawBarFill renderer value (Rect (V2 x y, V2 w h)) color = do
  let fillW = max 0 (min w (round (fromIntegral w * value)))
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 x y, V2 fillW h))))

------------------------------------------------------------------------
-- Record detail popover
------------------------------------------------------------------------

-- | Draw the record detail popover when a data browser record is selected.
--
-- Shows field names and values; nested DFRecord\/DFAdt fields can be
-- expanded.  In edit\/create mode, fields become editable controls and
-- Save\/Cancel buttons appear.
drawDataDetailPopover :: SDL.Renderer -> FontCache -> UiState -> Layout -> IO ()
drawDataDetailPopover renderer fontCache ui layout = do
  let dbs = uiDataBrowser ui
  case (dbsSelectedRecord dbs, dbsSelectedRowIndex dbs) of
    (Just record, Just rowIdx) -> do
      let resources = uiDataResources ui
          mSchema = do
            pName <- dbsSelectedPlugin dbs
            rName <- dbsSelectedResource dbs
            schemas <- Map.lookup pName resources
            find (\s -> drsName s == rName) schemas
          fields = maybe [] drsFields mSchema
          ops = maybe noOperations drsOperations mSchema
          expanded = dbsExpandedFields dbs
          visibleFields = enumerateVisibleFields "" fields expanded
          fieldCount = length visibleFields
          editing = dbsEditMode dbs || dbsCreateMode dbs
          editValues = dbsEditValues dbs
          focusedField = dbsFocusedField dbs
          textCursor = dbsTextCursor dbs

          popRect@(Rect (V2 px py, V2 pw _ph)) = dataDetailPopoverRect rowIdx fieldCount layout
          headerH = 28

      -- Background
      SDL.rendererDrawColor renderer SDL.$= colDataDetailBg
      SDL.fillRect renderer (Just (rectToSDL popRect))
      -- Border
      SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
      SDL.drawRect renderer (Just (rectToSDL popRect))
      -- Header
      SDL.rendererDrawColor renderer SDL.$= colDataDetailHeader
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 px py, V2 pw headerH))))

      -- Header text
      let headerText
            | dbsCreateMode dbs = "New Record"
            | otherwise = case dbsSelectedRecordKey dbs of
                Just (String t) -> t
                Just (Number n) -> T.pack (show n)
                Just v          -> TE.decodeUtf8 (BSL.toStrict (Aeson.encode v))
                Nothing         -> "Record"
      drawText fontCache colDataDetailFieldValue (V2 (px + 6) (py + 5)) headerText

      -- Edit toggle button (right side of header)
      when (doUpdate ops && not (dbsCreateMode dbs)) $ do
        let editRect = dataDetailEditToggleRect rowIdx fieldCount layout
            editColor = if dbsEditMode dbs then colDataEditBtnActive else colDataEditBtn
        SDL.rendererDrawColor renderer SDL.$= editColor
        SDL.fillRect renderer (Just (rectToSDL editRect))
        let Rect (V2 ex ey, V2 _ew eh) = editRect
        drawText fontCache colDataDetailFieldValue (V2 (ex + 4) (ey + (eh - 12) `div` 2)) "✎"

      -- Delete button (left side of header, only in view mode)
      when (doDelete ops && not editing) $ do
        let delRect = dataDetailDeleteRect rowIdx fieldCount layout
        SDL.rendererDrawColor renderer SDL.$= colDataDeleteBtn
        SDL.fillRect renderer (Just (rectToSDL delRect))
        let Rect (V2 dx dy, V2 _dw dh) = delRect
        drawText fontCache colDataDetailFieldValue (V2 (dx + 4) (dy + (dh - 12) `div` 2)) "✕"

      -- Save / Cancel buttons (only in edit/create mode)
      when editing $ do
        let saveRect = dataDetailSaveRect rowIdx fieldCount layout
        SDL.rendererDrawColor renderer SDL.$= colDataSaveBtn
        SDL.fillRect renderer (Just (rectToSDL saveRect))
        let Rect (V2 sx sy, V2 _sw sh) = saveRect
        drawText fontCache colDataDetailFieldValue (V2 (sx + 4) (sy + (sh - 12) `div` 2)) "Save"

        let cancelRect = dataDetailCancelRect rowIdx fieldCount layout
        SDL.rendererDrawColor renderer SDL.$= colDataCancelBtn
        SDL.fillRect renderer (Just (rectToSDL cancelRect))
        let Rect (V2 cx cy, V2 _cw ch) = cancelRect
        drawText fontCache colDataDetailFieldValue (V2 (cx + 4) (cy + (ch - 12) `div` 2)) "Cancel"

      -- Field rows
      forM_ (zip [0..] visibleFields) $ \(fIdx, (path, nestable)) -> do
        let Rect (V2 fx fy, V2 fw fh) = dataDetailFieldRect rowIdx fieldCount fIdx layout
            depth = length (T.splitOn "." path) - 1
            indent = depth * 12
            fieldName = lastSegment path
            recordVal = lookupNestedValue path (unDataRecord record)
            editVal = Map.lookup path editValues
            displayVal = if editing then editVal <|> recordVal else recordVal
            valText = renderValue displayVal

        -- Toggle indicator for nestable fields
        when nestable $ do
          let isExp = Set.member path expanded
              indicator = if isExp then "▼" else "▶"
          drawText fontCache colDataDetailToggle (V2 (fx + indent) (fy + 2)) indicator

        -- Field name
        let nameX = fx + indent + (if nestable then 14 else 0)
        drawText fontCache colDataDetailFieldName (V2 nameX (fy + 2)) (fieldName <> ":")

        -- Value display area
        when (not nestable || not (Set.member path expanded)) $ do
          if editing && not nestable
            then do
              -- Draw editable field control
              let inputRect = dataDetailFieldInputRect rowIdx fieldCount fIdx layout
                  isFocused = focusedField == Just path
                  borderCol = if isFocused then colDataFieldInputFocused else colDataFieldInputBorder
              SDL.rendererDrawColor renderer SDL.$= colDataFieldInputBg
              SDL.fillRect renderer (Just (rectToSDL inputRect))
              SDL.rendererDrawColor renderer SDL.$= borderCol
              SDL.drawRect renderer (Just (rectToSDL inputRect))
              -- Draw the value text inside the input area
              let Rect (V2 ix iy, V2 _iw _ih) = inputRect
              drawText fontCache colDataDetailFieldValue (V2 (ix + 3) (iy + 2)) valText
              -- Draw cursor for focused text fields
              when isFocused $ do
                let cursorText = case displayVal of
                      Just (String t) -> T.take textCursor t
                      _               -> T.take textCursor valText
                    cursorX = ix + 3 + T.length cursorText * 7  -- approximate char width
                SDL.rendererDrawColor renderer SDL.$= colDataFieldInputFocused
                SDL.drawLine renderer
                  (SDL.P (V2 (fromIntegral cursorX) (fromIntegral (iy + 2))))
                  (SDL.P (V2 (fromIntegral cursorX) (fromIntegral (iy + fh - 4))))
              -- Stepper buttons for numeric types
              let fType = lookupFieldType path fields
              case fType of
                Just DFBool -> pure () -- Toggle on click, no stepper
                Just (DFEnum _) -> do
                  -- Draw prev/next arrows
                  let minusRect = dataDetailFieldStepMinusRect rowIdx fieldCount fIdx layout
                      plusRect = dataDetailFieldStepPlusRect rowIdx fieldCount fIdx layout
                  SDL.rendererDrawColor renderer SDL.$= colSliderBtn
                  SDL.fillRect renderer (Just (rectToSDL minusRect))
                  SDL.fillRect renderer (Just (rectToSDL plusRect))
                  let Rect (V2 mx' my', V2 _mw mh) = minusRect
                      Rect (V2 plx ply, V2 _plw plh) = plusRect
                  drawText fontCache colDataDetailFieldValue (V2 (mx' + 3) (my' + (mh - 12) `div` 2)) "◀"
                  drawText fontCache colDataDetailFieldValue (V2 (plx + 3) (ply + (plh - 12) `div` 2)) "▶"
                Just DFText -> pure () -- Text input, no stepper
                _ -> do
                  -- Numeric stepper (DFInt, DFFloat, DFDouble, DFFixed*)
                  let minusRect = dataDetailFieldStepMinusRect rowIdx fieldCount fIdx layout
                      plusRect = dataDetailFieldStepPlusRect rowIdx fieldCount fIdx layout
                  SDL.rendererDrawColor renderer SDL.$= colSliderBtn
                  SDL.fillRect renderer (Just (rectToSDL minusRect))
                  SDL.fillRect renderer (Just (rectToSDL plusRect))
                  let Rect (V2 mx' my', V2 _mw mh) = minusRect
                      Rect (V2 plx ply, V2 _plw plh) = plusRect
                  drawText fontCache colDataDetailFieldValue (V2 (mx' + 5) (my' + (mh - 12) `div` 2)) "-"
                  drawText fontCache colDataDetailFieldValue (V2 (plx + 5) (ply + (plh - 12) `div` 2)) "+"
            else do
              let valX = fx + fw `div` 2
              drawText fontCache colDataDetailFieldValue (V2 valX (fy + 2)) valText

        -- Separator line
        SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
        SDL.drawLine renderer
          (SDL.P (V2 (fromIntegral fx) (fromIntegral (fy + fh - 1))))
          (SDL.P (V2 (fromIntegral (fx + fw)) (fromIntegral (fy + fh - 1))))

      -- Delete confirmation modal
      when (dbsDeleteConfirm dbs) $ do
        let dlgRect = deleteConfirmDialogRect layout
            okRect = deleteConfirmOkRect layout
            cancelRect = deleteConfirmCancelRect layout
        SDL.rendererDrawColor renderer SDL.$= colDataDeleteConfirmBg
        SDL.fillRect renderer (Just (rectToSDL dlgRect))
        SDL.rendererDrawColor renderer SDL.$= colDataDetailBorder
        SDL.drawRect renderer (Just (rectToSDL dlgRect))
        let Rect (V2 dlx dly, V2 dlw _dlh) = dlgRect
        drawText fontCache colDataDetailFieldValue (V2 (dlx + dlw `div` 2 - 50) (dly + 16)) "Delete record?"
        SDL.rendererDrawColor renderer SDL.$= colDataDeleteBtn
        SDL.fillRect renderer (Just (rectToSDL okRect))
        SDL.rendererDrawColor renderer SDL.$= colDataCancelBtn
        SDL.fillRect renderer (Just (rectToSDL cancelRect))
        let Rect (V2 ox oy, V2 _ow oh) = okRect
            Rect (V2 ccx ccy, V2 _ccw cch) = cancelRect
        drawText fontCache colDataDetailFieldValue (V2 (ox + 8) (oy + (oh - 12) `div` 2)) "Delete"
        drawText fontCache colDataDetailFieldValue (V2 (ccx + 8) (ccy + (cch - 12) `div` 2)) "Cancel"

    _ -> pure ()
  where
    find p = foldr (\x acc -> if p x then Just x else acc) Nothing

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
      concatMap (\c -> childRows (path <> "." <> dcdName c) (DFRecord (zipWith (\i t -> DataFieldDef (T.pack (show i)) t (T.pack (show i)) False Nothing) [(0::Int)..] (dcdFields c)))) ctors
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

-- | Look up a potentially nested value in a flat record map using a dot-path.
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
      Nothing -> Nothing
    goNested _ _ = Nothing

-- | Render a JSON 'Value' as a concise 'Text' for display.
renderValue :: Maybe Value -> Text
renderValue Nothing          = "—"
renderValue (Just Null)      = "null"
renderValue (Just (Bool b))  = if b then "true" else "false"
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
    resolveRest _ _ = Nothing