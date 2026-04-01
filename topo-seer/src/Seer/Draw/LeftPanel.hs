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
import UI.Theme
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
      let fill = if isActive then colTabActive else colTabInactive
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

drawViewModeButtons :: SDL.Renderer -> ViewMode -> [Rect] -> IO ()
drawViewModeButtons renderer currentMode rects =
  mapM_ drawOne (zip viewModeButtonStyles rects)
  where
    drawOne ((targetMode, colorFn), rect) = do
      SDL.rendererDrawColor renderer SDL.$= colorFn (modeColor targetMode currentMode)
      SDL.fillRect renderer (Just (rectToSDL rect))

-- | (ViewMode, Word8 -> V4 Word8) where the function builds the button
-- color from the active/inactive brightness.
viewModeButtonStyles :: [(ViewMode, Word8 -> V4 Word8)]
viewModeButtonStyles =
  [ (ViewElevation,     \mc -> V4 mc  90  90  255)
  , (ViewBiome,         \mc -> V4 90  mc  90  255)
  , (ViewClimate,       \mc -> V4 90  90  mc  255)
  , (ViewWeather,       \mc -> V4 170 90  mc  255)
  , (ViewMoisture,      \mc -> V4 mc  90  140 255)
  , (ViewPrecip,        \mc -> V4 90  140 mc  255)
  , (ViewVegetation,    \mc -> V4 90  mc  120 255)
  , (ViewTerrainForm,   \mc -> V4 mc  120 90  255)
  , (ViewPlateId,       \mc -> V4 90  mc  170 255)
  , (ViewPlateBoundary, \mc -> V4 90  mc  140 255)
  , (ViewPlateHardness, \mc -> V4 90  mc  120 255)
  , (ViewPlateCrust,    \mc -> V4 90  mc  110 255)
  , (ViewPlateAge,      \mc -> V4 90  mc  100 255)
  , (ViewPlateHeight,   \mc -> V4 90  mc  120 255)
  , (ViewPlateVelocity, \mc -> V4 90  mc  140 255)
  ]

drawOverlayButtons :: SDL.Renderer -> Maybe FontCache -> UiState -> (Rect, Rect, Rect, Rect) -> IO ()
drawOverlayButtons renderer fontCache ui (oPrev, oNext, fPrev, fNext) = do
  let isOverlay = case uiViewMode ui of
        ViewOverlay _ _ -> True
        _ -> False
      activeColor = colOverlayActive
      passiveColor = colOverlayInactive
      labelColor = textOverlayBtn
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
  SDL.rendererDrawColor renderer SDL.$= colCtrlBtn
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= colCtrlValue
  SDL.fillRect renderer (Just (rectToSDL valueRect))

drawSeedControl :: SDL.Renderer -> UiState -> Rect -> Rect -> IO ()
drawSeedControl renderer ui valueRect randomRect = do
  let valueColor = if uiSeedEditing ui then colCtrlValueActive else colCtrlValue
  SDL.rendererDrawColor renderer SDL.$= valueColor
  SDL.fillRect renderer (Just (rectToSDL valueRect))
  SDL.rendererDrawColor renderer SDL.$= colCtrlBtn
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
          terrainLabel = "Terrain chunks: " <> Text.pack (show terrainCount) <> "/" <> Text.pack (show totalChunks)
          biomeLabel = "Biome chunks: " <> Text.pack (show biomeCount) <> "/" <> Text.pack (show totalChunks)
      drawTextLine fontCache (V2 barX (terrainBarY - labelGap)) textStatusBar terrainLabel
      SDL.rendererDrawColor renderer SDL.$= colStatusBarTrack
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX terrainBarY, V2 barW barH))))
      SDL.rendererDrawColor renderer SDL.$= colStatusTerrainFill
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX terrainBarY, V2 terrainFill barH))))
      drawTextLine fontCache (V2 barX (biomeBarY - labelGap)) textStatusBar biomeLabel
      SDL.rendererDrawColor renderer SDL.$= colStatusBarTrack
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX biomeBarY, V2 barW barH))))
      SDL.rendererDrawColor renderer SDL.$= colStatusBiomeFill
      SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX biomeBarY, V2 biomeFill barH))))
    else pure ()