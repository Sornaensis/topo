{-# LANGUAGE OverloadedStrings #-}

-- | Config-panel text labels: tab names, slider value labels,
-- pipeline stage names, and simulation controls.
module Seer.Draw.Config.Labels
  ( drawConfigLabels
  ) where

import Actor.PluginManager.Types (PluginLifecycleSnapshot(..), PluginLifecycleState(..), pluginLifecycleStateText)
import Actor.UI (ConfigTab(..), DataBrowserState(..), UiState(..), builtinStageRowCount, configRowCount, pluginRowIndex, pluginRowsWithParams)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Linear (V2(..))
import qualified SDL
import Topo.Pipeline.Stage (allBuiltinStageIds, stageCanonicalName)
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..))
import UI.Components.ConfigSliders
  ( configSliderLabelCommands
  , configSliderRowsScrolled
  , configSliderValidationCommands
  , configSliderValidations
  , configTabLabelCommands
  , configTabViews
  )
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
      rowHeight = 24
      gap = 10
      rows = configRowCount (uiConfigTab ui) ui
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _, V2 _ scrollH) = scrollArea
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
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
      let stages = allBuiltinStageIds
          disabled = uiDisabledStages ui
          plugins = uiPluginNames ui
          checkboxSize = 16
          Rect (V2 sx _sy, V2 sw _sh) = scrollArea
          pad = 12
          labelMaxW = sw - (pad + checkboxSize + 8) - 8
      forM_ (zip [0..] stages) $ \(idx, sid) -> do
        let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect idx layout)
            isDisabled = Set.member sid disabled
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            name = stageCanonicalName sid
            textColor = if isDisabled
              then textPipelineStageDisabled
              else textPipelineStageName
        drawTextLine fontCache (V2 labelX labelY) textColor name
      -- Plugin name labels after built-in stages
      forM_ (zip [0..] plugins) $ \(idx, pName) -> do
        let rowIndex = pluginRowIndex ui idx
            Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIndex layout)
            labelX = sx + pad + checkboxSize + 8
            labelY = ry + 4
            pluginTextColor = textPipelinePluginName
            lifecycle = Map.lookup pName (uiPluginLifecycles ui)
            isDisabled = Set.member pName (uiDisabledPlugins ui)
            diagnosticStatus = Map.lookup pName (uiPluginDiagnosticStatuses ui)
            lifecycleSuffix = " [" <> diagnosticStatusLabel diagnosticStatus isDisabled lifecycle <> uptimeSuffix now lifecycle <> "]"
        drawTextLineTruncated fontCache (V2 labelX labelY) pluginTextColor labelMaxW (pName <> lifecycleSuffix)
        when (Map.findWithDefault False pName (uiPluginExpanded ui)) $ do
          let detailLines = Map.findWithDefault [] pName (uiPluginDiagnosticLines ui)
              detailX = sx + pad + 24
              detailMaxW = sw - (pad + 24) - 8
          forM_ (zip [0..] detailLines) $ \(dIdx, lineText) -> do
            let Rect (V2 _ dy, V2 _ _detailRowH) = sr (configScrollRowRect (rowIndex + 1 + dIdx) layout)
            drawTextLineTruncated fontCache (V2 detailX (dy + 4)) textMuted detailMaxW lineText
          let specs = Map.findWithDefault [] pName (uiPluginParamSpecs ui)
              paramStart = rowIndex + 1 + length detailLines
              paramLabelX = sx + pad + 40
              paramLabelMaxW = sw - (pad + 40) - 8
          forM_ (zip [0..] specs) $ \(pIdx, spec) -> do
            let Rect (V2 _ py, V2 _ _paramRowH) = sr (configScrollRowRect (paramStart + pIdx) layout)
            drawTextLineTruncated fontCache (V2 paramLabelX (py + 4)) textPipelineStageName paramLabelMaxW (rpsLabel spec)
      -- Simulation control labels
      let simOffset = builtinStageRowCount + pluginRowsWithParams ui
          simWorldReady = maybe False (const True) (uiWorldConfig ui)
          simLabelX = sx + pad + 68
          simAutoLabelX = sx + pad + checkboxSize + 8
          simRateLabelX = sx + pad + 128
          simLabelColor = textPipelineSimLabel
      -- Tick button label
      let Rect (V2 _ tickRowY, V2 _ _tickRowH) = sr (configScrollRowRect simOffset layout)
          tickLabelY = tickRowY + 4
          tickLabelColor = if simWorldReady then textPipelineTickActive else textPipelineTickInactive
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
          let recordOffset = resourceOffset + length schemas
              records = dbsRecords dbs
          forM_ (zip [0..] records) $ \(recIdx, _record) -> do
            let rowIdx = recordOffset + recIdx
                Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIdx layout)
                labelY = ry + 4
                -- Show the record's key value or index
                recordLabel = Text.pack ("#" <> show (recIdx + 1))
            drawTextLine fontCache (V2 (sx + pad + 24) labelY) textMuted recordLabel
          when (dbsLoading dbs) $ do
            let loadRow = recordOffset + length records
                Rect (V2 _ ly, V2 _ _lh) = sr (configScrollRowRect loadRow layout)
            drawTextLine fontCache (V2 (sx + pad) (ly + 4)) textDataLoading "Loading..."

    _ -> interpretDrawCommands renderer fontCache $
      configSliderLabelCommands activeSliderRows
        ++ configSliderValidationCommands (configSliderValidations activeSliderRows)

  SDL.rendererClipRect renderer SDL.$= Nothing

diagnosticStatusLabel :: Maybe Text.Text -> Bool -> Maybe PluginLifecycleSnapshot -> Text.Text
diagnosticStatusLabel (Just status) _ _ = status
diagnosticStatusLabel Nothing True _ = "Disabled"
diagnosticStatusLabel Nothing False Nothing = "WaitingForDependencies"
diagnosticStatusLabel Nothing False (Just snapshot) = case plsState snapshot of
  LifecycleReady -> "Ready"
  LifecycleDegraded -> "Degraded"
  LifecycleFailed -> "Failed"
  _ -> "WaitingForDependencies:" <> pluginLifecycleStateText (plsState snapshot)

uptimeSuffix :: UTCTime -> Maybe PluginLifecycleSnapshot -> Text.Text
uptimeSuffix now (Just snapshot)
  | plsState snapshot == LifecycleReady = " uptime=" <> formatUptime (diffUTCTime now (plsUpdatedAt snapshot))
uptimeSuffix _ _ = ""

formatUptime :: RealFrac a => a -> Text.Text
formatUptime seconds = Text.pack (show (max (0 :: Int) (floor seconds :: Int))) <> "s"
