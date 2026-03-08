{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.LeftPanel
  ( modeColor
  , drawLeftTabs
  , drawViewModeButtons
  , drawOverlayButtons
  , drawChunkControl
  , drawSeedControl
  , drawStatusBars
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.UI (LeftTab(..), UiState(..), ViewMode(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config (mapIntRange)
import UI.Font (FontCache)
import UI.Layout (Layout, leftPanelRect)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, drawTextLine, rectToSDL)

modeColor :: ViewMode -> ViewMode -> Word8
modeColor target current = if target == current then 200 else 110

drawLeftTabs :: SDL.Renderer -> UiState -> (Rect, Rect) -> IO ()
drawLeftTabs renderer ui (tabTopo, tabView) = do
  drawTab tabTopo (uiLeftTab ui == LeftTopo)
  drawTab tabView (uiLeftTab ui == LeftView)
  where
    drawTab rect isActive = do
      let fill = if isActive then V4 70 90 120 255 else V4 50 60 75 255
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

drawViewModeButtons :: SDL.Renderer -> ViewMode -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawViewModeButtons renderer mode (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12) = do
  SDL.rendererDrawColor renderer SDL.$= V4 (modeColor ViewElevation mode) 90 90 255
  SDL.fillRect renderer (Just (rectToSDL r1))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewBiome mode) 90 255
  SDL.fillRect renderer (Just (rectToSDL r2))
  SDL.rendererDrawColor renderer SDL.$= V4 90 90 (modeColor ViewClimate mode) 255
  SDL.fillRect renderer (Just (rectToSDL r3))
  SDL.rendererDrawColor renderer SDL.$= V4 (modeColor ViewMoisture mode) 90 140 255
  SDL.fillRect renderer (Just (rectToSDL r4))
  SDL.rendererDrawColor renderer SDL.$= V4 170 90 (modeColor ViewWeather mode) 255
  SDL.fillRect renderer (Just (rectToSDL r5))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateId mode) 170 255
  SDL.fillRect renderer (Just (rectToSDL r6))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateBoundary mode) 140 255
  SDL.fillRect renderer (Just (rectToSDL r7))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateHardness mode) 120 255
  SDL.fillRect renderer (Just (rectToSDL r8))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateCrust mode) 110 255
  SDL.fillRect renderer (Just (rectToSDL r9))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateAge mode) 100 255
  SDL.fillRect renderer (Just (rectToSDL r10))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateHeight mode) 120 255
  SDL.fillRect renderer (Just (rectToSDL r11))
  SDL.rendererDrawColor renderer SDL.$= V4 90 (modeColor ViewPlateVelocity mode) 140 255
  SDL.fillRect renderer (Just (rectToSDL r12))

drawOverlayButtons :: SDL.Renderer -> Maybe FontCache -> UiState -> (Rect, Rect, Rect, Rect) -> IO ()
drawOverlayButtons renderer fontCache ui (oPrev, oNext, fPrev, fNext) = do
  let isOverlay = case uiViewMode ui of
        ViewOverlay _ _ -> True
        _ -> False
      activeColor = V4 160 100 200 255
      passiveColor = V4 80 70 100 255
      labelColor = V4 230 230 240 255
      buttonColor = if isOverlay then activeColor else passiveColor
  SDL.rendererDrawColor renderer SDL.$= buttonColor
  SDL.fillRect renderer (Just (rectToSDL oPrev))
  SDL.fillRect renderer (Just (rectToSDL oNext))
  drawCentered fontCache labelColor oPrev "\9664 Ov"
  drawCentered fontCache labelColor oNext "Ov \9654"
  SDL.rendererDrawColor renderer SDL.$= buttonColor
  SDL.fillRect renderer (Just (rectToSDL fPrev))
  SDL.fillRect renderer (Just (rectToSDL fNext))
  drawCentered fontCache labelColor fPrev "\9664 Fld"
  drawCentered fontCache labelColor fNext "Fld \9654"

drawChunkControl :: SDL.Renderer -> UiState -> Rect -> Rect -> Rect -> IO ()
drawChunkControl renderer _ui minusRect valueRect plusRect = do
  SDL.rendererDrawColor renderer SDL.$= V4 90 90 110 255
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
  SDL.fillRect renderer (Just (rectToSDL valueRect))

drawSeedControl :: SDL.Renderer -> UiState -> Rect -> Rect -> IO ()
drawSeedControl renderer ui valueRect randomRect = do
  let valueColor = if uiSeedEditing ui then V4 70 90 120 255 else V4 45 55 70 255
  SDL.rendererDrawColor renderer SDL.$= valueColor
  SDL.fillRect renderer (Just (rectToSDL valueRect))
  SDL.rendererDrawColor renderer SDL.$= V4 90 90 110 255
  SDL.fillRect renderer (Just (rectToSDL randomRect))

drawStatusBars :: SDL.Renderer -> Maybe FontCache -> UiState -> DataSnapshot -> Layout -> IO ()
drawStatusBars renderer fontCache ui dataSnap layout =
  if uiShowLeftPanel ui && uiLeftTab ui == LeftTopo
    then do
      let Rect (V2 px py, V2 pw ph) = leftPanelRect layout
          pad = 12
          barH = 10
          barGap = 14
          labelGap = 18
          totalBlockH = barH * 2 + barGap + labelGap * 2
          baseY = py + ph - pad - totalBlockH
          barW = max 40 (pw - pad * 2)
          rx = mapIntRange 0 16 (uiWorldExtentX ui)
          ry = mapIntRange 0 16 (uiWorldExtentY ui)
          totalChunks = max 1 ((rx * 2 + 1) * (ry * 2 + 1))
          terrainCount = dsTerrainChunks dataSnap
          biomeCount = dsBiomeChunks dataSnap
          terrainFill = round (fromIntegral barW * min 1 (fromIntegral terrainCount / fromIntegral totalChunks))
          biomeFill = round (fromIntegral barW * min 1 (fromIntegral biomeCount / fromIntegral totalChunks))
          barX = px + pad
          terrainBarY = baseY
          biomeBarY = baseY + barH + barGap
          terrainLabel = "Terrain " <> Text.pack (show terrainCount) <> "/" <> Text.pack (show totalChunks)
          biomeLabel = "Biomes " <> Text.pack (show biomeCount) <> "/" <> Text.pack (show totalChunks)
      drawTextLine fontCache (V2 barX (terrainBarY - labelGap)) (V4 230 230 235 255) terrainLabel
      SDL.rendererDrawColor renderer SDL.$= V4 40 60 70 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX terrainBarY, V2 barW barH))))
      SDL.rendererDrawColor renderer SDL.$= V4 40 180 90 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX terrainBarY, V2 terrainFill barH))))
      drawTextLine fontCache (V2 barX (biomeBarY - labelGap)) (V4 230 230 235 255) biomeLabel
      SDL.rendererDrawColor renderer SDL.$= V4 40 60 70 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX biomeBarY, V2 barW barH))))
      SDL.rendererDrawColor renderer SDL.$= V4 180 120 40 255
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX biomeBarY, V2 biomeFill barH))))
    else pure ()