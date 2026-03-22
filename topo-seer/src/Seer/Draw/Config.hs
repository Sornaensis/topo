{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Config
  ( drawConfigPanel
  , drawConfigTabs
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.UI (ConfigTab(..), DataBrowserState(..), UiState(..), builtinStageRowCount, configRowCount, pluginRowIndex, pluginRowsWithParams)
import Control.Monad (forM_, when)
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderRegistry (SliderDef(..))
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (sliderDefsForConfigTab, sliderValueForId)
import Topo.Pipeline.Stage (allBuiltinStageIds)
import Topo.Plugin.DataResource (DataResourceSchema(..))
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
  )
import UI.Widgets (Rect(..))
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
      let fill = if isActive then V4 70 90 120 255 else V4 50 60 75 255
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

drawConfigPanel :: SDL.Renderer -> UiState -> DataSnapshot -> Layout -> IO ()
drawConfigPanel renderer ui dataSnap layout =
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
      SDL.rendererDrawColor renderer SDL.$= V4 35 45 60 230
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawConfigTabs renderer ui tabs
      SDL.rendererDrawColor renderer SDL.$= V4 30 38 52 230
      SDL.fillRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererDrawColor renderer SDL.$= V4 60 70 90 255
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
                checkColor = if isDisabled then V4 60 60 70 200 else V4 70 150 90 255
                borderColor = if isDisabled then V4 80 80 90 200 else V4 100 180 120 255
            SDL.rendererDrawColor renderer SDL.$= checkColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= borderColor
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
          let pluginOffset = length stages
          forM_ (zip [0 ..] plugins) $ \(idx, pName) -> do
            let rowIndex = pluginRowIndex ui idx
                Rect (V2 checkX checkY, V2 _ _) = scrollRect (pipelineCheckboxRect rowIndex layout)
                isPluginDisabled = Set.member pName (uiDisabledPlugins ui)
                pluginColor = if isPluginDisabled then V4 60 60 70 200 else V4 100 90 160 255
                pluginBorder = if isPluginDisabled then V4 80 80 90 200 else V4 130 120 190 255
                btnSize = 14
                Rect (V2 upX btnY, V2 _ _) = scrollRect (pipelineMoveUpRect rowIndex layout)
                Rect (V2 downX _downY, V2 _ _) = scrollRect (pipelineMoveDownRect rowIndex layout)
                arrowColor = V4 150 150 170 255
                arrowBorder = V4 100 100 120 255
                -- Expand toggle
                isExpanded = Map.findWithDefault False pName (uiPluginExpanded ui)
                Rect (V2 expX expY, V2 expW expH) = scrollRect (pipelineExpandRect rowIndex layout)
                expColor = if isExpanded then V4 100 160 100 255 else V4 100 100 120 255
            SDL.rendererDrawColor renderer SDL.$= pluginColor
            SDL.fillRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= pluginBorder
            SDL.drawRect renderer (Just (rectToSDL (Rect (V2 checkX checkY, V2 checkboxSize checkboxSize))))
            SDL.rendererDrawColor renderer SDL.$= V4 50 50 65 255
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
            SDL.rendererDrawColor renderer SDL.$= V4 50 50 65 255
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
            SDL.rendererDrawColor renderer SDL.$= V4 140 140 160 255
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
                        boolColor = if isChecked then V4 70 150 90 255 else V4 60 60 70 200
                        boolBorder = if isChecked then V4 100 180 120 255 else V4 80 80 90 200
                    SDL.rendererDrawColor renderer SDL.$= boolColor
                    SDL.fillRect renderer (Just (rectToSDL (Rect (V2 pcX pcY, V2 pcW pcH))))
                    SDL.rendererDrawColor renderer SDL.$= boolBorder
                    SDL.drawRect renderer (Just (rectToSDL (Rect (V2 pcX pcY, V2 pcW pcH))))
                  _ -> do
                    let barRect = scrollRect (pipelineParamBarRect paramRowIdx layout)
                        paramVal = case Map.lookup (rpsName spec) params of
                                     Just (Number n) -> normalizeParam spec (realToFrac n)
                                     _               -> 0.5
                    SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
                    SDL.fillRect renderer (Just (rectToSDL barRect))
                    drawBarFill renderer paramVal barRect (V4 120 100 180 255)
          let simOffset = builtinStageRowCount + pluginRowsWithParams ui
              simWorldReady = dsTerrainChunks dataSnap > 0
              tickBtnRect = scrollRect (pipelineTickButtonRect simOffset layout)
              tickBtnColor = if simWorldReady then V4 80 120 160 255 else V4 55 65 80 170
              Rect (V2 autoTickCheckX autoTickCheckY, V2 _ _) = scrollRect (pipelineCheckboxRect (simOffset + 1) layout)
              autoTickColor = if uiSimAutoTick ui then V4 70 150 90 255 else V4 60 60 70 200
              autoTickBorder = if uiSimAutoTick ui then V4 100 180 120 255 else V4 80 80 90 200
              tickRateBarRect = scrollRect (pipelineTickRateBarRect (simOffset + 2) layout)
          SDL.rendererDrawColor renderer SDL.$= tickBtnColor
          SDL.fillRect renderer (Just (rectToSDL tickBtnRect))
          SDL.rendererDrawColor renderer SDL.$= autoTickColor
          SDL.fillRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= autoTickBorder
          SDL.drawRect renderer (Just (rectToSDL (Rect (V2 autoTickCheckX autoTickCheckY, V2 checkboxSize checkboxSize))))
          SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
          SDL.fillRect renderer (Just (rectToSDL tickRateBarRect))
          drawBarFill renderer (uiSimTickRate ui) tickRateBarRect (V4 100 130 180 255)
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
                fillColor = if isSelected then V4 70 90 120 255 else V4 50 55 68 220
                borderColor = if isSelected then V4 100 130 180 255 else V4 70 75 90 200
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
                    fillColor = if isSelected then V4 80 100 130 255 else V4 45 50 62 220
                    borderColor = if isSelected then V4 110 140 190 255 else V4 65 70 85 200
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
                SDL.rendererDrawColor renderer SDL.$= V4 40 45 58 200
                SDL.fillRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
                SDL.rendererDrawColor renderer SDL.$= V4 60 65 78 200
                SDL.drawRect renderer (Just (rectToSDL (Rect (V2 rx ry, V2 rw rh))))
              -- Loading indicator
              when (dbsLoading dbs) $ do
                let loadRow = recordOffset + length records
                    Rect (V2 lx ly, V2 _lw lh) = scrollRect (dataBrowserItemRect loadRow layout)
                SDL.rendererDrawColor renderer SDL.$= V4 100 100 130 180
                SDL.fillRect renderer (Just (rectToSDL (Rect (V2 lx ly, V2 40 lh))))
        _ -> forM_ activeSliderDefs drawSliderDef
      SDL.rendererClipRect renderer SDL.$= Nothing
      let Rect (V2 bx by, V2 bw bh) = scrollBarRect
          handleH = if maxOffset == 0 then bh else max 12 (bh * scrollH `div` max 1 contentHeight)
          handleY = if maxOffset == 0 then by else by + (bh - handleH) * scrollY `div` maxOffset
      SDL.rendererDrawColor renderer SDL.$= V4 25 25 30 255
      SDL.fillRect renderer (Just (rectToSDL scrollBarRect))
      SDL.rendererDrawColor renderer SDL.$= V4 160 160 170 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 bx handleY, V2 bw handleH))))
      SDL.rendererDrawColor renderer SDL.$= V4 60 120 80 255
      SDL.fillRect renderer (Just (rectToSDL presetSaveRect))
      SDL.rendererDrawColor renderer SDL.$= V4 80 110 160 255
      SDL.fillRect renderer (Just (rectToSDL presetLoadRect))
      SDL.rendererDrawColor renderer SDL.$= V4 120 80 80 255
      SDL.fillRect renderer (Just (rectToSDL resetRect))
      let revertColor = case uiWorldConfig ui of
            Just _ -> V4 140 100 50 255
            Nothing -> V4 70 60 45 120
      SDL.rendererDrawColor renderer SDL.$= revertColor
      SDL.fillRect renderer (Just (rectToSDL revertRect))
    else pure ()

drawConfigSlider :: SDL.Renderer -> Float -> Rect -> Rect -> Rect -> V4 Word8 -> IO ()
drawConfigSlider renderer value minusRect barRect plusRect fillColor = do
  SDL.rendererDrawColor renderer SDL.$= V4 90 90 110 255
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
  SDL.fillRect renderer (Just (rectToSDL barRect))
  drawBarFill renderer value barRect fillColor

drawBarFill :: SDL.Renderer -> Float -> Rect -> V4 Word8 -> IO ()
drawBarFill renderer value (Rect (V2 x y, V2 w h)) color = do
  let fillW = max 0 (min w (round (fromIntegral w * value)))
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 x y, V2 fillW h))))
