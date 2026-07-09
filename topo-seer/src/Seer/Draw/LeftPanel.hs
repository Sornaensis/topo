{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.LeftPanel
  ( modeColor
  , drawLeftTabs
  , drawViewModeButtons
  , drawDayNightToggle
  , drawOverlayButtons
  , drawOverlayActionButtons
  , drawChunkControl
  , drawSeedControl
  , drawStatusBars
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.UI (BaseViewMode(..), LayeredViewState(..), LeftTab(..), SkyOverlayMode(..), UiState(..), ViewMode(..), WeatherBasis(..), effectiveViewSelection)
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

drawViewModeButtons :: SDL.Renderer -> UiState -> [Rect] -> [Rect] -> [Rect] -> IO ()
drawViewModeButtons renderer ui baseRects overlayRects basisRects = do
  mapM_ (drawActiveButton (lvsBaseView selection)) (zip baseViewButtonStyles baseRects)
  mapM_ (drawActiveButton (lvsSkyOverlay selection)) (zip skyOverlayButtonStyles overlayRects)
  mapM_ drawBasisButton (zip basisButtonStyles basisRects)
  where
    selection = effectiveViewSelection ui
    basisEnabled = supportsWeatherBasis (lvsSkyOverlay selection)
    drawActiveButton current ((target, colorFn), rect) = do
      let brightness = if target == current then 200 else 110
      SDL.rendererDrawColor renderer SDL.$= colorFn brightness
      SDL.fillRect renderer (Just (rectToSDL rect))
    drawBasisButton ((target, colorFn), rect) = do
      let brightness
            | not basisEnabled = 70
            | target == lvsWeatherBasis selection = 200
            | otherwise = 110
      SDL.rendererDrawColor renderer SDL.$= colorFn brightness
      SDL.fillRect renderer (Just (rectToSDL rect))

supportsWeatherBasis :: Maybe SkyOverlayMode -> Bool
supportsWeatherBasis (Just (SkyOverlayPlugin _ _)) = False
supportsWeatherBasis (Just _) = True
supportsWeatherBasis Nothing = False

-- | (BaseViewMode, Word8 -> V4 Word8) where the function builds the button
-- color from the active/inactive brightness.
baseViewButtonStyles :: [(BaseViewMode, Word8 -> V4 Word8)]
baseViewButtonStyles =
  [ (BaseViewElevation,     \mc -> V4 mc  90  90  255)
  , (BaseViewBiome,         \mc -> V4 90  mc  90  255)
  , (BaseViewMoisture,      \mc -> V4 mc  90  140 255)
  , (BaseViewVegetation,    \mc -> V4 90  mc  120 255)
  , (BaseViewTerrainForm,   \mc -> V4 mc  120 90  255)
  , (BaseViewPlateId,       \mc -> V4 90  mc  170 255)
  , (BaseViewPlateBoundary, \mc -> V4 90  mc  140 255)
  , (BaseViewPlateHardness, \mc -> V4 90  mc  120 255)
  , (BaseViewPlateCrust,    \mc -> V4 90  mc  110 255)
  , (BaseViewPlateAge,      \mc -> V4 90  mc  100 255)
  , (BaseViewPlateHeight,   \mc -> V4 90  mc  120 255)
  , (BaseViewPlateVelocity, \mc -> V4 90  mc  140 255)
  ]

skyOverlayButtonStyles :: [(Maybe SkyOverlayMode, Word8 -> V4 Word8)]
skyOverlayButtonStyles =
  [ (Nothing,                         \mc -> V4 mc  mc  mc  255)
  , (Just SkyOverlayWeatherTemperature, \mc -> V4 170 90  mc  255)
  , (Just SkyOverlayPrecipitation,      \mc -> V4 90  140 mc  255)
  , (Just SkyOverlayCloud,              \mc -> V4 mc  mc  mc  255)
  ]

basisButtonStyles :: [(WeatherBasis, Word8 -> V4 Word8)]
basisButtonStyles =
  [ (WeatherBasisAverage, \mc -> V4 120 120 mc  255)
  , (WeatherBasisCurrent, \mc -> V4 90  mc  190 255)
  ]

drawDayNightToggle :: SDL.Renderer -> Bool -> Rect -> IO ()
drawDayNightToggle renderer enabled rect = do
  let fill = if enabled then V4 90 130 180 255 else V4 70 80 100 255
  SDL.rendererDrawColor renderer SDL.$= fill
  SDL.fillRect renderer (Just (rectToSDL rect))

drawOverlayButtons :: SDL.Renderer -> Maybe FontCache -> UiState -> (Rect, Rect, Rect, Rect) -> IO ()
drawOverlayButtons renderer fontCache ui (oPrev, oNext, fPrev, fNext) = do
  let isOverlay = case lvsSkyOverlay (effectiveViewSelection ui) of
        Just (SkyOverlayPlugin _ _) -> True
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

-- | Draw overlay manager/schema/provenance/export/import action buttons.
drawOverlayActionButtons :: SDL.Renderer -> Maybe FontCache -> [Rect] -> IO ()
drawOverlayActionButtons renderer fontCache rects =
  mapM_ drawOne (zip rects labels)
  where
    labels = ["Overlays", "Schema", "Prov", "Export", "Validate"] :: [Text]
    drawOne (rect, label) = do
      SDL.rendererDrawColor renderer SDL.$= colOverlayInactive
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawCentered fontCache textOverlayBtn rect label

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