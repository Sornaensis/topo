{-# LANGUAGE OverloadedStrings #-}

-- | Config-panel text labels: tab names, slider value labels,
-- pipeline stage names, and simulation controls.
module Seer.Draw.Config.Labels
  ( drawConfigLabels
  ) where

import Actor.UI (ConfigTab(..), DataBrowserState(..), UiState(..), configRowCount)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderRegistry (SliderDef(..))
import Seer.Config.SliderSpec
import Seer.Config.SliderStyle (SliderLabelMode(..), SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (sliderDefsForConfigTab, sliderValueForId)
import Topo.Pipeline.Stage (allBuiltinStageIds, stageCanonicalName)
import Topo.Plugin.DataResource (DataResourceSchema(..))
import UI.Font (FontCache)
import UI.Layout
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, drawLabelAbove, drawTextLine, rectToSDL)

-- | Render all config-panel text labels when the config panel is visible.
drawConfigLabels :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawConfigLabels renderer fontCache ui layout = when (uiShowConfig ui) $ do
  let lc = V4 235 235 235 255
      scrollArea = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      rows = configRowCount (uiConfigTab ui) ui
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _, V2 _ scrollH) = scrollArea
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
      sr (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      activeSliderDefs = sliderDefsForConfigTab (uiConfigTab ui)
      drawSliderLabels sliderDef = do
        let rowIndex = sliderRowIndex sliderDef
            sid = sliderId sliderDef
            val = sliderValueForId ui sid
            sliderStyle = sliderStyleForId sid
            rects = configParamRects rowIndex layout
        case sliderStyleLabelMode sliderStyle of
          SliderLabelFull -> do
            drawCentered fontCache lc (sr (configParamRowMinusRect rects)) "-"
            drawCentered fontCache lc (sr (configParamRowPlusRect rects)) "+"
          SliderLabelBarOnly -> pure ()
        drawLabelAbove fontCache lc (sr (configParamRowBarRect rects)) (sliderLabelForId sid val)
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData) = configTabRects layout

  -- Tab labels
  drawCentered fontCache lc tabTerrain "Terrain"
  drawCentered fontCache lc tabPlanet "Planet"
  drawCentered fontCache lc tabClimate "Climate"
  drawCentered fontCache lc tabWeather "Weather"
  drawCentered fontCache lc tabBiome "Biome"
  drawCentered fontCache lc tabErosion "Erosion"
  drawCentered fontCache lc tabPipeline "Pipe"
  drawCentered fontCache lc tabData "Data"

  -- Preset / reset / revert
  drawCentered fontCache lc (configPresetSaveRect layout) "Save"
  drawCentered fontCache lc (configPresetLoadRect layout) "Load"
  drawCentered fontCache lc (configResetRect layout) "Reset"
  let revertLabelColor = case uiWorldConfig ui of
        Just _  -> lc
        Nothing -> V4 120 120 130 140
  drawCentered fontCache revertLabelColor (configRevertRect layout) "Revert"

  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollArea)

  case uiConfigTab ui of
    ConfigPipeline -> do
      let stages = allBuiltinStageIds
          disabled = uiDisabledStages ui
          plugins = uiPluginNames ui
          checkboxSize = 16
          Rect (V2 sx _sy, V2 _sw _sh) = scrollArea
          pad = 12
      forM_ (zip [0..] stages) $ \(idx, sid) -> do
        let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect idx layout)
            isDisabled = Set.member sid disabled
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            name = stageCanonicalName sid
            textColor = if isDisabled
              then V4 120 120 130 180
              else V4 220 220 225 255
        drawTextLine fontCache (V2 labelX labelY) textColor name
      -- Plugin name labels after built-in stages
      let pluginOffset = length stages
      forM_ (zip [0..] plugins) $ \(idx, pName) -> do
        let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect (pluginOffset + idx) layout)
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            pluginTextColor = V4 190 180 220 255
        drawTextLine fontCache (V2 labelX labelY) pluginTextColor pName
      -- Simulation control labels
      let simOffset = length stages + length plugins
          simWorldReady = maybe False (const True) (uiWorldConfig ui)
          simLabelX = sx + pad + 68
          simAutoLabelX = sx + pad + checkboxSize + 8
          simRateLabelX = sx + pad + 128
          simLabelColor = V4 180 200 220 255
      -- Tick button label
      let Rect (V2 _ tickRowY, V2 _ _tickRowH) = sr (configScrollRowRect simOffset layout)
          tickLabelY = tickRowY + 4
          tickLabelColor = if simWorldReady then V4 220 230 240 255 else V4 140 150 165 180
      drawTextLine fontCache (V2 (sx + pad + 8) tickLabelY) tickLabelColor "Tick"
      -- Auto-tick label
      let Rect (V2 _ autoTickRowY, V2 _ _autoTickRowH) = sr (configScrollRowRect (simOffset + 1) layout)
          autoTickLabelY = autoTickRowY + 4
      drawTextLine fontCache (V2 simAutoLabelX autoTickLabelY) simLabelColor "Auto-tick"
      -- Tick rate label
      let Rect (V2 _ tickRateRowY, V2 _ _tickRateRowH) = sr (configScrollRowRect (simOffset + 2) layout)
          tickRateLabelY = tickRateRowY + 4
          rateText = "Rate: " <> Text.pack (show (round (uiSimTickRate ui * 10) :: Int)) <> "/s"
      drawTextLine fontCache (V2 simRateLabelX tickRateLabelY) simLabelColor rateText

    ConfigData -> do
      let dbs = uiDataBrowser ui
          resources = uiDataResources ui
          pluginNames = Map.keys resources
          selectedPlugin = dbsSelectedPlugin dbs
          Rect (V2 sx _sy, V2 _sw _sh) = scrollArea
          pad = 16
          pluginColor = V4 190 180 220 255
          resourceColor = V4 170 200 230 255
          recordColor = V4 200 200 210 255
      -- Plugin name labels
      forM_ (zip [0..] pluginNames) $ \(idx, pName) -> do
        let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect idx layout)
            labelY = ry + 4
        drawTextLine fontCache (V2 (sx + pad) labelY) pluginColor pName
      -- Resource labels when plugin selected
      let resourceOffset = length pluginNames
      case selectedPlugin of
        Nothing -> pure ()
        Just pName -> do
          let schemas = Map.findWithDefault [] pName resources
          forM_ (zip [0..] schemas) $ \(rIdx, schema) -> do
            let rowIdx = resourceOffset + rIdx
                Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIdx layout)
                labelY = ry + 4
            drawTextLine fontCache (V2 (sx + pad + 12) labelY) resourceColor (drsLabel schema)
          -- Record labels
          let recordOffset = resourceOffset + length schemas
              records = dbsRecords dbs
          forM_ (zip [0..] records) $ \(recIdx, _record) -> do
            let rowIdx = recordOffset + recIdx
                Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIdx layout)
                labelY = ry + 4
                -- Show the record's key value or index
                recordLabel = Text.pack ("#" <> show (recIdx + 1))
            drawTextLine fontCache (V2 (sx + pad + 24) labelY) recordColor recordLabel
          when (dbsLoading dbs) $ do
            let loadRow = recordOffset + length records
                Rect (V2 _ ly, V2 _ _lh) = sr (configScrollRowRect loadRow layout)
            drawTextLine fontCache (V2 (sx + pad) (ly + 4)) (V4 140 140 160 200) "Loading..."

    _ -> forM_ activeSliderDefs drawSliderLabels

  SDL.rendererClipRect renderer SDL.$= Nothing
