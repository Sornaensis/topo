{-# LANGUAGE OverloadedStrings #-}

-- | Config-panel text labels: tab names, slider value labels,
-- pipeline stage names, and simulation controls.
module Seer.Draw.Config.Labels
  ( drawConfigLabels
  ) where

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , UiState(..)
  , configRowCount
  , dataBrowserReadPending
  )
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Linear (V2(..))
import qualified SDL
import Topo.Plugin.DataResource (DataResourceSchema(..))
import UI.Components.ConfigSliders
  ( configSliderLabelCommands
  , configSliderRowsScrolled
  , configSliderValidationCommands
  , configSliderValidations
  , configTabLabelCommands
  , configTabViews
  )
import UI.Components.PipelineControls (PipelineLabelView(..), pipelineLabelViews)
import UI.DrawCommand.SDL (interpretDrawCommands)
import UI.Font (FontCache)
import UI.Layout
import UI.Theme
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, drawTextLine, drawTextLineTruncated, rectToSDL)

-- | Render all config-panel text labels when the config panel is visible.
drawConfigLabels :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawConfigLabels renderer fontCache ui layout = when (uiShowConfig ui) $ do
  let lc = textPrimary
      scrollArea = configScrollAreaRect layout
      rows = configRowCount (uiConfigTab ui) ui
      scrollY = configScrollOffsetForRows rows (uiConfigScroll ui) layout
      sr (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      activeSliderRows = configSliderRowsScrolled ui layout

  -- Tab labels
  interpretDrawCommands renderer fontCache $
    configTabLabelCommands (configTabViews (uiConfigTab ui) (configTabRects layout))

  -- Preset / reset / revert
  drawCentered fontCache lc (configPresetSaveRect layout) "Save"
  drawCentered fontCache lc (configPresetLoadRect layout) "Load"
  drawCentered fontCache lc (configResetRect layout) "Reset"
  let revertLabelColor = case uiWorldConfig ui of
        Just _  -> lc
        Nothing -> textConfigRevertDimmed
  drawCentered fontCache revertLabelColor (configRevertRect layout) "Revert"

  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollArea)

  case uiConfigTab ui of
    ConfigPipeline -> do
      now <- getCurrentTime
      forM_ (pipelineLabelViews now ui layout) $ \labelView ->
        case plvMaxWidth labelView of
          Just maxWidth -> drawTextLineTruncated fontCache (plvPosition labelView) (plvColor labelView) maxWidth (plvText labelView)
          Nothing -> drawTextLine fontCache (plvPosition labelView) (plvColor labelView) (plvText labelView)

    ConfigData -> do
      let dbs = uiDataBrowser ui
          resources = uiDataResources ui
          pluginNames = Map.keys resources
          selectedPlugin = dbsSelectedPlugin dbs
          Rect (V2 sx _sy, V2 sw _sh) = scrollArea
          pad = 16
          pluginLabelMaxW = sw - pad - 8
          resourceLabelMaxW = sw - (pad + 12) - 8
      -- Plugin name labels
      forM_ (zip [0..] pluginNames) $ \(idx, pName) -> do
        let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect idx layout)
            labelY = ry + 4
            statusSuffix = maybe "" (\status -> " [" <> status <> "]") (Map.lookup pName (uiPluginDiagnosticStatuses ui))
        drawTextLineTruncated fontCache (V2 (sx + pad) labelY) textPipelinePluginName pluginLabelMaxW (pName <> statusSuffix)
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
            drawTextLineTruncated fontCache (V2 (sx + pad + 12) labelY) textDataResourceLabel resourceLabelMaxW (drsLabel schema)
          -- Record labels
          -- Read requests hide record rows and their targets until the
          -- replacement result arrives. Progress/error copy is emitted by the
          -- shared Data Browser draw-command view.
          let recordOffset = resourceOffset + length schemas
              records
                | dataBrowserReadPending dbs = []
                | otherwise = dbsRecords dbs
          forM_ (zip [0..] records) $ \(recIdx, _record) -> do
            let rowIdx = recordOffset + recIdx
                Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIdx layout)
                labelY = ry + 4
                -- Show the record's key value or index
                recordLabel = Text.pack ("#" <> show (recIdx + 1))
            drawTextLine fontCache (V2 (sx + pad + 24) labelY) textMuted recordLabel

    _ -> interpretDrawCommands renderer fontCache $
      configSliderLabelCommands activeSliderRows
        ++ configSliderValidationCommands (configSliderValidations activeSliderRows)

  SDL.rendererClipRect renderer SDL.$= Nothing
