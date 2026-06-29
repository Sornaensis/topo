{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Config
  ( drawConfigPanel
  , drawConfigTabs
  , drawDataDetailPopover
  ) where

import Actor.Data (DataSnapshot)
import Actor.UI (ConfigTab(..), DataBrowserState(..), UiState(..), configRowCount)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Linear (V2(..))
import qualified SDL
import Seer.Draw.Config.DataDetail (drawDataDetailPopover)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
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
  , configTabRects
  , dataBrowserItemRect
  , dataBrowserCreateButtonRect
  )
import UI.Components.ConfigSliders
  ( configSliderDrawCommands
  , configSliderRowsScrolled
  , configTabDrawCommands
  , configTabViews
  )
import UI.Components.PipelineControls
  ( pipelineControlDrawCommands
  , pipelineControlsView
  )
import UI.DrawCommand.SDL (interpretDrawCommands)
import UI.Font (FontCache, drawText)
import UI.Widgets (Rect(..))
import UI.Theme
import UI.WidgetsDraw (rectToSDL)

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui tabs =
  interpretDrawCommands renderer Nothing $
    configTabDrawCommands (configTabViews (uiConfigTab ui) tabs)

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
                      canPage = maybe False (doPage . drsOperations) mSchema
                      pageRow = recordOffset + length records
                      createRow = pageRow + (if null records || not canPage then 0 else 1)
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

