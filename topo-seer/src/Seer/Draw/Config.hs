{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Config
  ( drawConfigPanel
  , drawConfigTabs
  , drawDataDetailPopover
  ) where

import Actor.Data (DataSnapshot)
import Actor.UI (ConfigTab(..), UiState(..), configRowCount)
import Control.Monad (when)
import Linear (V2(..))
import qualified SDL
import Seer.Draw.Config.DataDetail (drawDataDetailPopover)
import UI.Layout
  ( Layout
  , configPanelRect
  , configPresetLoadRect
  , configPresetSaveRect
  , configResetRect
  , configRevertRect
  , configRowTopPad
  , configScrollAreaRect
  , configScrollBarRect
  , configScrollOffsetForRows
  , configTabRects
  )
import UI.Components.ConfigSliders
  ( configSliderDrawCommands
  , configSliderRowsScrolled
  , configTabDrawCommands
  , configTabViews
  )
import UI.Components.DataBrowser
  ( dataBrowserDrawCommands
  , dataBrowserViewAtScroll
  )
import UI.Components.PipelineControls
  ( pipelineControlDrawCommands
  , pipelineControlsView
  )
import UI.DrawCommand.SDL (interpretDrawCommands)
import UI.Font (FontCache)
import UI.Widgets (Rect(..))
import UI.Theme
import UI.WidgetsDraw (rectToSDL)

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui tabs =
  interpretDrawCommands renderer Nothing $
    configTabDrawCommands (configTabViews (uiConfigTab ui) tabs)

drawConfigPanel :: SDL.Renderer -> Maybe FontCache -> UiState -> DataSnapshot -> Layout -> IO ()
drawConfigPanel renderer fontCache ui dataSnap layout =
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
      scrollY = configScrollOffsetForRows rows (uiConfigScroll ui) layout
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
        ConfigPipeline -> interpretDrawCommands renderer Nothing $
          pipelineControlDrawCommands (pipelineControlsView ui dataSnap layout)
        ConfigData -> interpretDrawCommands renderer fontCache $
          dataBrowserDrawCommands $
            dataBrowserViewAtScroll scrollY (uiDataResources ui) (uiDataBrowser ui) layout
        _ -> interpretDrawCommands renderer Nothing $
          configSliderDrawCommands (configSliderRowsScrolled ui layout)
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

