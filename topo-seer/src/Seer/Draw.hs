{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw
  ( seedMaxDigits
  , viewColor
  , modeColor
  , logLineColor
  , logTextColor
  , logLineHeight
  , logTextYOffset
  , drawConfigTabs
  , drawLeftTabs
  , drawViewModeButtons
  , drawLogFilters
  , drawLogLines
  , drawLogScrollbar
  , drawEscapeMenu
  , drawPresetSaveDialog
  , drawPresetLoadDialog
  , drawWorldSaveDialog
  , drawWorldLoadDialog
  , drawTopBar
  , drawConfigPanel
  , drawChunkControl
  , drawSeedControl
  , drawStatusBars
  , drawHoverHex
  , drawHexContext
  , drawTooltip
  , drawUiLabels
  ) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..))
import Actor.UI (ConfigTab(..), LeftTab(..), UiMenuMode(..), UiState(..), ViewMode(..))
import Control.Monad (when)
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config (mapIntRange)
import Seer.Config.SliderSpec
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo (BiomeId, ChunkCoord(..), ChunkId(..), ClimateChunk(..), PlateBoundary(..), TerrainChunk(..), TileCoord(..), TileIndex(..), WeatherChunk(..), WorldConfig(..), biomeDisplayName, chunkCoordFromTile, chunkIdFromCoord, plateBoundaryToCode, tileIndex)
import UI.Font (FontCache, textSize)
import UI.HexPick (axialToScreen)
import UI.Layout
import UI.Widgets (Rect(..))
import Seer.Draw.Dialog (drawDialogButton, drawDialogPanel, drawDialogTitle, drawListSelection, drawTextInputField)
import UI.WidgetsDraw (drawCentered, drawLabelAbove, drawLabelLeft, drawLeft, drawTextLine, rectToSDL)
import qualified Data.Vector.Unboxed as U

seedMaxDigits :: Int
seedMaxDigits = 20

viewColor :: ViewMode -> Int -> Int -> (Word8, Word8, Word8)
viewColor mode terrainCount biomeCount =
  case mode of
    ViewElevation -> (40, 80 + scale terrainCount, 160)
    ViewBiome -> (120, 70 + scale biomeCount, 60)
    ViewClimate -> (70, 120, 160 + scale biomeCount)
    ViewMoisture -> (60, 140 + scale biomeCount, 120)
    ViewPrecip -> (60, 110 + scale biomeCount, 150)
    ViewPlateId -> (100, 120 + scale biomeCount, 160)
    ViewPlateBoundary -> (140, 110 + scale biomeCount, 90)
    ViewPlateHardness -> (120, 100 + scale biomeCount, 70)
    ViewPlateCrust -> (110, 100 + scale biomeCount, 90)
    ViewPlateAge -> (110, 90 + scale biomeCount, 130)
    ViewPlateHeight -> (90, 120 + scale biomeCount, 150)
    ViewPlateVelocity -> (90, 110 + scale biomeCount, 180)
  where
    scale n = fromIntegral (min 75 (n * 5))

modeColor :: ViewMode -> ViewMode -> Word8
modeColor target current = if target == current then 200 else 110

logLineColor :: LogLevel -> V4 Word8
logLineColor level =
  case level of
    LogDebug -> V4 70 70 90 255
    LogInfo -> V4 60 120 170 255
    LogWarn -> V4 180 120 40 255
    LogError -> V4 180 60 60 255

logTextColor :: LogLevel -> V4 Word8
logTextColor level =
  case level of
    LogDebug -> V4 200 200 220 255
    LogInfo -> V4 210 230 245 255
    LogWarn -> V4 250 230 170 255
    LogError -> V4 250 200 200 255

logLevelColor :: LogLevel -> Bool -> V4 Word8
logLevelColor level isActive =
  let boost = if isActive then 60 else 0
      clamp x = fromIntegral (min 255 (x + boost))
  in case level of
       LogDebug -> V4 (clamp 80) (clamp 80) (clamp 110) 255
       LogInfo -> V4 (clamp 60) (clamp 140) (clamp 200) 255
       LogWarn -> V4 (clamp 200) (clamp 140) (clamp 50) 255
       LogError -> V4 (clamp 200) (clamp 60) (clamp 60) 255

logLineHeight :: Maybe FontCache -> IO Int
logLineHeight Nothing = pure 12
logLineHeight (Just cache) = do
  V2 _ th <- textSize cache (V4 255 255 255 255) "Ag"
  pure (max 12 (fromIntegral th + 4))

logTextYOffset :: Maybe FontCache -> Int -> IO Int
logTextYOffset Nothing _ = pure 1
logTextYOffset (Just cache) lineHeight = do
  V2 _ th <- textSize cache (V4 255 255 255 255) "Ag"
  pure (max 1 ((lineHeight - fromIntegral th) `div` 2))

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect, Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) = do
  drawTab tabTerrain (uiConfigTab ui == ConfigTerrain)
  drawTab tabPlanet (uiConfigTab ui == ConfigPlanet)
  drawTab tabClimate (uiConfigTab ui == ConfigClimate)
  drawTab tabWeather (uiConfigTab ui == ConfigWeather)
  drawTab tabBiome (uiConfigTab ui == ConfigBiome)
  drawTab tabErosion (uiConfigTab ui == ConfigErosion)
  where
    drawTab rect isActive = do
      let fill = if isActive then V4 70 90 120 255 else V4 50 60 75 255
      SDL.rendererDrawColor renderer SDL.$= fill
      SDL.fillRect renderer (Just (rectToSDL rect))

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
  SDL.rendererDrawColor renderer SDL.$= V4 90 110 (modeColor ViewPrecip mode) 255
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

drawLogFilters :: SDL.Renderer -> Maybe FontCache -> LogLevel -> (Rect, Rect, Rect, Rect) -> IO ()
drawLogFilters renderer fontCache minLevel (r1, r2, r3, r4) = do
  drawLogFilter renderer fontCache LogDebug minLevel r1 "D"
  drawLogFilter renderer fontCache LogInfo minLevel r2 "I"
  drawLogFilter renderer fontCache LogWarn minLevel r3 "W"
  drawLogFilter renderer fontCache LogError minLevel r4 "E"

drawLogFilter :: SDL.Renderer -> Maybe FontCache -> LogLevel -> LogLevel -> Rect -> Text -> IO ()
drawLogFilter renderer fontCache level active rect label = do
  SDL.rendererDrawColor renderer SDL.$= logLevelColor level (level == active)
  SDL.fillRect renderer (Just (rectToSDL rect))
  drawCentered fontCache (V4 230 230 230 255) rect label

drawLogLines :: SDL.Renderer -> Maybe FontCache -> LogSnapshot -> Rect -> IO ()
drawLogLines renderer fontCache logSnap (Rect (V2 x y, V2 w h)) = do
  lineHeight <- logLineHeight fontCache
  textYOffset <- logTextYOffset fontCache lineHeight
  let padding = 2
      gutterW = 8
      contentX = x + gutterW
      contentW = max 0 (w - gutterW)
      visibleLines = max 0 (h `div` lineHeight)
      entries = lsEntries logSnap
      total = length entries
      maxOffset = max 0 (total - visibleLines)
      offset = min (lsScroll logSnap) maxOffset
      startIndex = max 0 (total - visibleLines - offset)
      visible = take visibleLines (drop startIndex entries)
      lineWidth = max 0 (contentW - 2 * padding)
  SDL.rendererClipRect renderer SDL.$= Just (rectToSDL (Rect (V2 contentX y, V2 contentW h)))
  sequence_ [ drawLogLine renderer fontCache (contentX + padding) (y + padding + idx * lineHeight) lineWidth (lineHeight - 2) textYOffset entry
            | (idx, entry) <- zip [0..] visible
            ]
  SDL.rendererClipRect renderer SDL.$= Nothing

drawLogLine :: SDL.Renderer -> Maybe FontCache -> Int -> Int -> Int -> Int -> Int -> LogEntry -> IO ()
drawLogLine renderer fontCache x y w h textYOffset entry = do
  SDL.rendererDrawColor renderer SDL.$= logLineColor (leLevel entry)
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 x y, V2 w h))))
  drawTextLine fontCache (V2 (x + 2) (y + textYOffset)) (logTextColor (leLevel entry)) (leMessage entry)

drawLogScrollbar :: SDL.Renderer -> LogSnapshot -> Rect -> IO ()
drawLogScrollbar renderer logSnap (Rect (V2 x y, V2 w h)) = do
  let lineHeight = 12
      padding = 2
      totalLines = length (lsEntries logSnap)
      visibleLines = max 1 (h `div` lineHeight)
      maxOffset = max 0 (totalLines - visibleLines)
      offset = min (lsScroll logSnap) maxOffset
      barX = x + padding
      barY = y + padding
      barW = 4
      barH = max 0 (h - padding * 2)
      handleH = max 10 (barH * visibleLines `div` max 1 totalLines)
      handleY =
        if maxOffset == 0
          then barY
          else barY + (barH - handleH) * offset `div` maxOffset
  SDL.rendererDrawColor renderer SDL.$= V4 25 25 30 255
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX barY, V2 barW barH))))
  SDL.rendererDrawColor renderer SDL.$= V4 160 160 170 255
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX handleY, V2 barW handleH))))

drawEscapeMenu :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawEscapeMenu renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuEscape -> do
      let panel = menuPanelRect layout
          saveRect = menuSaveRect layout
          loadRect = menuLoadRect layout
          exitRect = menuExitRect layout
      SDL.rendererDrawColor renderer SDL.$= V4 20 25 35 230
      SDL.fillRect renderer (Just (rectToSDL panel))
      drawDialogButton renderer fontCache saveRect "Save" True
      drawDialogButton renderer fontCache loadRect "Load" True
      drawDialogButton renderer fontCache exitRect "Exit" True
    _ -> pure ()

-- | Draw the preset save dialog (text input + Ok/Cancel).
drawPresetSaveDialog :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawPresetSaveDialog renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuPresetSave -> do
      let dialog  = presetSaveDialogRect layout
          inputR  = presetSaveInputRect layout
          okR     = presetSaveOkRect layout
          cancelR = presetSaveCancelRect layout
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Save Preset"
      drawTextInputField renderer fontCache inputR (uiPresetInput ui)
      drawDialogButton renderer fontCache okR "Ok" True
      drawDialogButton renderer fontCache cancelR "Cancel" True
    _ -> pure ()

-- | Draw the preset load dialog (list + Load/Cancel).
drawPresetLoadDialog :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawPresetLoadDialog renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuPresetLoad -> do
      let dialog  = presetLoadDialogRect layout
          listR   = presetLoadListRect layout
          okR     = presetLoadOkRect layout
          cancelR = presetLoadCancelRect layout
          items   = uiPresetList ui
          sel     = uiPresetSelected ui
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Load Preset"
      drawListSelection renderer fontCache listR 24 8 sel
        (presetLoadItemRect layout) (\_ name -> name) items
      drawDialogButton renderer fontCache okR "Load" True
      drawDialogButton renderer fontCache cancelR "Cancel" True
    _ -> pure ()

-- | Draw the world save dialog (text input + Save/Cancel).
drawWorldSaveDialog :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawWorldSaveDialog renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuWorldSave -> do
      let dialog  = worldSaveDialogRect layout
          inputR  = worldSaveInputRect layout
          okR     = worldSaveOkRect layout
          cancelR = worldSaveCancelRect layout
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Save World"
      drawTextInputField renderer fontCache inputR (uiWorldSaveInput ui)
      drawDialogButton renderer fontCache okR "Save" True
      drawDialogButton renderer fontCache cancelR "Cancel" True
    _ -> pure ()

-- | Draw the world load dialog (list + Load/Cancel).
drawWorldLoadDialog :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawWorldLoadDialog renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuWorldLoad -> do
      let dialog  = worldLoadDialogRect layout
          listR   = worldLoadListRect layout
          okR     = worldLoadOkRect layout
          cancelR = worldLoadCancelRect layout
          items   = uiWorldList ui
          sel     = uiWorldSelected ui
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Load World"
      drawListSelection renderer fontCache listR 28 8 sel
        (worldLoadItemRect layout) worldLoadLabel items
      let hasItems = not (null items)
      drawDialogButton renderer fontCache okR "Load" hasItems
      drawDialogButton renderer fontCache cancelR "Cancel" True
    _ -> pure ()

-- | Format a world manifest entry for display in the load list.
worldLoadLabel :: Int -> WorldSaveManifest -> Text
worldLoadLabel _i manifest =
  wsmName manifest
    <> " (seed " <> Text.pack (show (wsmSeed manifest))
    <> ", " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d" (wsmCreatedAt manifest))
    <> ")"

-- | Draw the top bar showing the current world name.
drawTopBar :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawTopBar renderer fontCache ui layout = do
  let bar = topBarRect layout
      Rect (V2 bx by, V2 bw bh) = bar
      textRect = Rect (V2 (bx + 16) by, V2 (bw - 32) bh)
  SDL.rendererDrawColor renderer SDL.$= V4 25 30 42 230
  SDL.fillRect renderer (Just (rectToSDL bar))
  drawLeft fontCache (V4 200 210 225 255) textRect (uiWorldName ui)

drawConfigPanel
  :: SDL.Renderer
  -> UiState
  -> Rect
  -> (Rect, Rect, Rect, Rect, Rect, Rect)
  -> Rect
  -> Rect
  -> Rect
  -> Rect
  -> Rect
  -> Rect
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> (Rect, Rect, Rect)
  -> IO ()
drawConfigPanel renderer ui rect (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) presetSaveRect presetLoadRect resetRect revertRect scrollAreaRect scrollBarRect
  (waterMinus, waterBar, waterPlus)
  (evapMinus, evapBar, evapPlus)
  (rainShadowMinus, rainShadowBar, rainShadowPlus)
  (windDiffuseMinus, windDiffuseBar, windDiffusePlus)
  (equatorTempMinus, equatorTempBar, equatorTempPlus)
  (poleTempMinus, poleTempBar, poleTempPlus)
  (lapseRateMinus, lapseRateBar, lapseRatePlus)
  (windIterationsMinus, windIterationsBar, windIterationsPlus)
  (moistureIterationsMinus, moistureIterationsBar, moistureIterationsPlus)
  (weatherTickMinus, weatherTickBar, weatherTickPlus)
  (weatherPhaseMinus, weatherPhaseBar, weatherPhasePlus)
  (weatherAmplitudeMinus, weatherAmplitudeBar, weatherAmplitudePlus)
  (seasonCycleLengthMinus, seasonCycleLengthBar, seasonCycleLengthPlus)
  (jitterAmplitudeMinus, jitterAmplitudeBar, jitterAmplitudePlus)
  (pressureBaseMinus, pressureBaseBar, pressureBasePlus)
  (pressureTempScaleMinus, pressureTempScaleBar, pressureTempScalePlus)
  (pressureCoriolisScaleMinus, pressureCoriolisScaleBar, pressureCoriolisScalePlus)
  (seasonalBaseMinus, seasonalBaseBar, seasonalBasePlus)
  (seasonalRangeMinus, seasonalRangeBar, seasonalRangePlus)
  (humidityNoiseScaleMinus, humidityNoiseScaleBar, humidityNoiseScalePlus)
  (precipNoiseScaleMinus, precipNoiseScaleBar, precipNoiseScalePlus)
  (weatherITCZWidthMinus, weatherITCZWidthBar, weatherITCZWidthPlus)
  (weatherITCZPrecipBoostMinus, weatherITCZPrecipBoostBar, weatherITCZPrecipBoostPlus)
  (pressureHumidityScaleMinus, pressureHumidityScaleBar, pressureHumidityScalePlus)
  (pressureGradientWindScaleMinus, pressureGradientWindScaleBar, pressureGradientWindScalePlus)
  (windNoiseScaleMinus, windNoiseScaleBar, windNoiseScalePlus)
  (itczMigrationScaleMinus, itczMigrationScaleBar, itczMigrationScalePlus)
  (cloudRHExponentMinus, cloudRHExponentBar, cloudRHExponentPlus)
  (cloudAlbedoEffectMinus, cloudAlbedoEffectBar, cloudAlbedoEffectPlus)
  (cloudPrecipBoostMinus, cloudPrecipBoostBar, cloudPrecipBoostPlus)
  (vegBaseMinus, vegBaseBar, vegBasePlus)
  (vegBoostMinus, vegBoostBar, vegBoostPlus)
  (vegTempWeightMinus, vegTempWeightBar, vegTempWeightPlus)
  (vegPrecipWeightMinus, vegPrecipWeightBar, vegPrecipWeightPlus)
  (btCoastalBandMinus, btCoastalBandBar, btCoastalBandPlus)
  (btSnowMaxTempMinus, btSnowMaxTempBar, btSnowMaxTempPlus)
  (btAlpineMaxTempMinus, btAlpineMaxTempBar, btAlpineMaxTempPlus)
  (btIceCapTempMinus, btIceCapTempBar, btIceCapTempPlus)
  (btMontaneMaxTempMinus, btMontaneMaxTempBar, btMontaneMaxTempPlus)
  (btMontanePrecipMinus, btMontanePrecipBar, btMontanePrecipPlus)
  (btCliffSlopeMinus, btCliffSlopeBar, btCliffSlopePlus)
  (btValleyMoistureMinus, btValleyMoistureBar, btValleyMoisturePlus)
  (btDepressionMoistureMinus, btDepressionMoistureBar, btDepressionMoisturePlus)
  (btPrecipWeightMinus, btPrecipWeightBar, btPrecipWeightPlus)
  (vbcTempMinMinus, vbcTempMinBar, vbcTempMinPlus)
  (vbcTempRangeMinus, vbcTempRangeBar, vbcTempRangePlus)
  (vbcFertilityBoostMinus, vbcFertilityBoostBar, vbcFertilityBoostPlus)
  (vbcAlbedoBaseMinus, vbcAlbedoBaseBar, vbcAlbedoBasePlus)
  (vbcAlbedoBareMinus, vbcAlbedoBareBar, vbcAlbedoBarePlus)
  (vbcAlbedoVegMinus, vbcAlbedoVegBar, vbcAlbedoVegPlus)
  (vbcOceanAlbedoMinus, vbcOceanAlbedoBar, vbcOceanAlbedoPlus)
  (vbcIceAlbedoMinus, vbcIceAlbedoBar, vbcIceAlbedoPlus)
  (biomeSmoothingMinus, biomeSmoothingBar, biomeSmoothingPlus)
  (volcanicAshBoostMinus, volcanicAshBoostBar, volcanicAshBoostPlus)
  (volcanicLavaPenaltyMinus, volcanicLavaPenaltyBar, volcanicLavaPenaltyPlus)
  (biomeFeedbackBlendMinus, biomeFeedbackBlendBar, biomeFeedbackBlendPlus)
  (boundaryMotionTempMinus, boundaryMotionTempBar, boundaryMotionTempPlus)
  (boundaryMotionPrecipMinus, boundaryMotionPrecipBar, boundaryMotionPrecipPlus)
  (planetRadiusMinus, planetRadiusBar, planetRadiusPlus)
  (axialTiltMinus, axialTiltBar, axialTiltPlus)
  (insolationMinus, insolationBar, insolationPlus)
  (occWarmScaleMinus, occWarmScaleBar, occWarmScalePlus)
  (occColdScaleMinus, occColdScaleBar, occColdScalePlus)
  (occLatPeakDegMinus, occLatPeakDegBar, occLatPeakDegPlus)
  (occLatWidthDegMinus, occLatWidthDegBar, occLatWidthDegPlus)
  (sliceLatCenterMinus, sliceLatCenterBar, sliceLatCenterPlus)
  (sliceLonCenterMinus, sliceLonCenterBar, sliceLonCenterPlus)
  (latitudeExponentMinus, latitudeExponentBar, latitudeExponentPlus)
  (plateHeightCoolingMinus, plateHeightCoolingBar, plateHeightCoolingPlus)
  (tempNoiseScaleMinus, tempNoiseScaleBar, tempNoiseScalePlus)
  (oceanModerationMinus, oceanModerationBar, oceanModerationPlus)
  (oceanModerateTempMinus, oceanModerateTempBar, oceanModerateTempPlus)
  (albedoSensitivityMinus, albedoSensitivityBar, albedoSensitivityPlus)
  (albedoReferenceMinus, albedoReferenceBar, albedoReferencePlus)
  (moistAdvectMinus, moistAdvectBar, moistAdvectPlus)
  (moistLocalMinus, moistLocalBar, moistLocalPlus)
  (moistWindEvapScaleMinus, moistWindEvapScaleBar, moistWindEvapScalePlus)
  (moistEvapNoiseScaleMinus, moistEvapNoiseScaleBar, moistEvapNoiseScalePlus)
  (moistLandETCoeffMinus, moistLandETCoeffBar, moistLandETCoeffPlus)
  (moistBareEvapFracMinus, moistBareEvapFracBar, moistBareEvapFracPlus)
  (moistVegTranspFracMinus, moistVegTranspFracBar, moistVegTranspFracPlus)
  (moistWindETScaleMinus, moistWindETScaleBar, moistWindETScalePlus)
  (moistCondensationRateMinus, moistCondensationRateBar, moistCondensationRatePlus)
  (moistRecycleRateMinus, moistRecycleRateBar, moistRecycleRatePlus)
  (moistITCZStrengthMinus, moistITCZStrengthBar, moistITCZStrengthPlus)
  (moistITCZWidthMinus, moistITCZWidthBar, moistITCZWidthPlus)
  (orographicScaleMinus, orographicScaleBar, orographicScalePlus)
  (orographicStepMinus, orographicStepBar, orographicStepPlus)
  (coastalIterationsMinus, coastalIterationsBar, coastalIterationsPlus)
  (coastalDiffuseMinus, coastalDiffuseBar, coastalDiffusePlus)
  (coastalMoistureBoostMinus, coastalMoistureBoostBar, coastalMoistureBoostPlus)
  (windBeltStrengthMinus, windBeltStrengthBar, windBeltStrengthPlus)
  (windBeltHarmonicsMinus, windBeltHarmonicsBar, windBeltHarmonicsPlus)
  (windBeltBaseMinus, windBeltBaseBar, windBeltBasePlus)
  (windBeltRangeMinus, windBeltRangeBar, windBeltRangePlus)
  (windBeltSpeedScaleMinus, windBeltSpeedScaleBar, windBeltSpeedScalePlus)
  (bndLandRangeMinus, bndLandRangeBar, bndLandRangePlus)
  (bndTempConvergentMinus, bndTempConvergentBar, bndTempConvergentPlus)
  (bndTempDivergentMinus, bndTempDivergentBar, bndTempDivergentPlus)
  (bndTempTransformMinus, bndTempTransformBar, bndTempTransformPlus)
  (bndPrecipConvergentMinus, bndPrecipConvergentBar, bndPrecipConvergentPlus)
  (bndPrecipDivergentMinus, bndPrecipDivergentBar, bndPrecipDivergentPlus)
  (bndPrecipTransformMinus, bndPrecipTransformBar, bndPrecipTransformPlus)
  (genScaleMinus, genScaleBar, genScalePlus)
  (genCoordScaleMinus, genCoordScaleBar, genCoordScalePlus)
  (genOffsetXMinus, genOffsetXBar, genOffsetXPlus)
  (genOffsetYMinus, genOffsetYBar, genOffsetYPlus)
  (genFrequencyMinus, genFrequencyBar, genFrequencyPlus)
  (genOctavesMinus, genOctavesBar, genOctavesPlus)
  (genLacunarityMinus, genLacunarityBar, genLacunarityPlus)
  (genGainMinus, genGainBar, genGainPlus)
  (genWarpScaleMinus, genWarpScaleBar, genWarpScalePlus)
  (genWarpStrengthMinus, genWarpStrengthBar, genWarpStrengthPlus)
  (extentXMinus, extentXBar, extentXPlus)
  (extentYMinus, extentYBar, extentYPlus)
  (edgeNorthMinus, edgeNorthBar, edgeNorthPlus)
  (edgeSouthMinus, edgeSouthBar, edgeSouthPlus)
  (edgeEastMinus, edgeEastBar, edgeEastPlus)
  (edgeWestMinus, edgeWestBar, edgeWestPlus)
  (edgeFalloffMinus, edgeFalloffBar, edgeFalloffPlus)
  (plateSizeMinus, plateSizeBar, plateSizePlus)
  (upliftMinus, upliftBar, upliftPlus)
  (riftDepthMinus, riftDepthBar, riftDepthPlus)
  (detailScaleMinus, detailScaleBar, detailScalePlus)
  (plateSpeedMinus, plateSpeedBar, plateSpeedPlus)
  (boundarySharpnessMinus, boundarySharpnessBar, boundarySharpnessPlus)
  (boundaryNoiseScaleMinus, boundaryNoiseScaleBar, boundaryNoiseScalePlus)
  (boundaryNoiseStrengthMinus, boundaryNoiseStrengthBar, boundaryNoiseStrengthPlus)
  (boundaryWarpOctavesMinus, boundaryWarpOctavesBar, boundaryWarpOctavesPlus)
  (boundaryWarpLacunarityMinus, boundaryWarpLacunarityBar, boundaryWarpLacunarityPlus)
  (boundaryWarpGainMinus, boundaryWarpGainBar, boundaryWarpGainPlus)
  (plateMergeScaleMinus, plateMergeScaleBar, plateMergeScalePlus)
  (plateMergeBiasMinus, plateMergeBiasBar, plateMergeBiasPlus)
  (plateDetailScaleMinus, plateDetailScaleBar, plateDetailScalePlus)
  (plateDetailStrengthMinus, plateDetailStrengthBar, plateDetailStrengthPlus)
  (plateRidgeStrengthMinus, plateRidgeStrengthBar, plateRidgeStrengthPlus)
  (plateHeightBaseMinus, plateHeightBaseBar, plateHeightBasePlus)
  (plateHeightVarianceMinus, plateHeightVarianceBar, plateHeightVariancePlus)
  (plateHardnessBaseMinus, plateHardnessBaseBar, plateHardnessBasePlus)
  (plateHardnessVarianceMinus, plateHardnessVarianceBar, plateHardnessVariancePlus)
  (trenchDepthMinus, trenchDepthBar, trenchDepthPlus)
  (ridgeHeightMinus, ridgeHeightBar, ridgeHeightPlus)
  (plateBiasStrengthMinus, plateBiasStrengthBar, plateBiasStrengthPlus)
  (plateBiasCenterMinus, plateBiasCenterBar, plateBiasCenterPlus)
  (plateBiasEdgeMinus, plateBiasEdgeBar, plateBiasEdgePlus)
  (plateBiasNorthMinus, plateBiasNorthBar, plateBiasNorthPlus)
  (plateBiasSouthMinus, plateBiasSouthBar, plateBiasSouthPlus)
  (tfcCliffSlopeMinus, tfcCliffSlopeBar, tfcCliffSlopePlus)
  (tfcMountainSlopeMinus, tfcMountainSlopeBar, tfcMountainSlopePlus)
  (tfcMountainReliefMinus, tfcMountainReliefBar, tfcMountainReliefPlus)
  (tfcHillSlopeMinus, tfcHillSlopeBar, tfcHillSlopePlus)
  (tfcRollingSlopeMinus, tfcRollingSlopeBar, tfcRollingSlopePlus)
  (valleyCurvatureMinus, valleyCurvatureBar, valleyCurvaturePlus)
  (rockElevationThresholdMinus, rockElevationThresholdBar, rockElevationThresholdPlus)
  (rockHardnessThresholdMinus, rockHardnessThresholdBar, rockHardnessThresholdPlus)
  (rockHardnessSecondaryMinus, rockHardnessSecondaryBar, rockHardnessSecondaryPlus)
  (erosionHydraulicMinus, erosionHydraulicBar, erosionHydraulicPlus)
  (erosionThermalMinus, erosionThermalBar, erosionThermalPlus)
  (erosionRainRateMinus, erosionRainRateBar, erosionRainRatePlus)
  (erosionTalusMinus, erosionTalusBar, erosionTalusPlus)
  (erosionMaxDropMinus, erosionMaxDropBar, erosionMaxDropPlus)
  (glacierSnowTempMinus, glacierSnowTempBar, glacierSnowTempPlus)
  (glacierSnowRangeMinus, glacierSnowRangeBar, glacierSnowRangePlus)
  (glacierMeltTempMinus, glacierMeltTempBar, glacierMeltTempPlus)
  (glacierMeltRateMinus, glacierMeltRateBar, glacierMeltRatePlus)
  (glacierAccumScaleMinus, glacierAccumScaleBar, glacierAccumScalePlus)
  (glacierFlowItersMinus, glacierFlowItersBar, glacierFlowItersPlus)
  (glacierFlowRateMinus, glacierFlowRateBar, glacierFlowRatePlus)
  (glacierErosionScaleMinus, glacierErosionScaleBar, glacierErosionScalePlus)
  (glacierCarveScaleMinus, glacierCarveScaleBar, glacierCarveScalePlus)
  (glacierDepositScaleMinus, glacierDepositScaleBar, glacierDepositScalePlus)
  (ventDensityMinus, ventDensityBar, ventDensityPlus)
  (ventThresholdMinus, ventThresholdBar, ventThresholdPlus)
  (hotspotScaleMinus, hotspotScaleBar, hotspotScalePlus)
  (hotspotThresholdMinus, hotspotThresholdBar, hotspotThresholdPlus)
  (magmaRechargeMinus, magmaRechargeBar, magmaRechargePlus)
  (lavaScaleMinus, lavaScaleBar, lavaScalePlus)
  (ashScaleMinus, ashScaleBar, ashScalePlus)
  (volcanicDepositScaleMinus, volcanicDepositScaleBar, volcanicDepositScalePlus)
  (soilMoistureThresholdMinus, soilMoistureThresholdBar, soilMoistureThresholdPlus)
  (soilHardnessThresholdMinus, soilHardnessThresholdBar, soilHardnessThresholdPlus)
  (soilFertilityMoistWeightMinus, soilFertilityMoistWeightBar, soilFertilityMoistWeightPlus)
  (soilFertilityDepthWeightMinus, soilFertilityDepthWeightBar, soilFertilityDepthWeightPlus)
  (sinkBreachDepthMinus, sinkBreachDepthBar, sinkBreachDepthPlus)
  (streamPowerMaxErosionMinus, streamPowerMaxErosionBar, streamPowerMaxErosionPlus)
  (riverCarveMaxDepthMinus, riverCarveMaxDepthBar, riverCarveMaxDepthPlus)
  (coastalErodeStrengthMinus, coastalErodeStrengthBar, coastalErodeStrengthPlus)
  (hydroHardnessWeightMinus, hydroHardnessWeightBar, hydroHardnessWeightPlus)
  (minLakeSizeMinus, minLakeSizeBar, minLakeSizePlus)
  (inlandSeaMinSizeMinus, inlandSeaMinSizeBar, inlandSeaMinSizePlus)
  (roughnessScaleMinus, roughnessScaleBar, roughnessScalePlus) =
  if uiShowConfig ui
    then do
      SDL.rendererDrawColor renderer SDL.$= V4 35 45 60 230
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawConfigTabs renderer ui (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion)
      SDL.rendererDrawColor renderer SDL.$= V4 30 38 52 230
      SDL.fillRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererDrawColor renderer SDL.$= V4 60 70 90 255
      SDL.drawRect renderer (Just (rectToSDL scrollAreaRect))
      let rowHeight = 24
          gap = 10
          rows = case uiConfigTab ui of
            ConfigTerrain -> 53
            ConfigPlanet -> 7
            ConfigClimate -> 49
            ConfigWeather -> 21
            ConfigBiome -> 26
            ConfigErosion -> 35
          contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
          Rect (V2 _ _ , V2 _ scrollH) = scrollAreaRect
          maxOffset = max 0 (contentHeight - scrollH)
          scrollY = min maxOffset (uiConfigScroll ui)
          scrollRect (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
      SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollAreaRect)
      case uiConfigTab ui of
        ConfigTerrain -> do
          drawConfigSlider renderer (uiGenScale ui) (scrollRect genScaleMinus) (scrollRect genScaleBar) (scrollRect genScalePlus) (V4 90 140 180 255)
          drawConfigSlider renderer (uiGenCoordScale ui) (scrollRect genCoordScaleMinus) (scrollRect genCoordScaleBar) (scrollRect genCoordScalePlus) (V4 80 120 170 255)
          drawConfigSlider renderer (uiGenOffsetX ui) (scrollRect genOffsetXMinus) (scrollRect genOffsetXBar) (scrollRect genOffsetXPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenOffsetY ui) (scrollRect genOffsetYMinus) (scrollRect genOffsetYBar) (scrollRect genOffsetYPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenFrequency ui) (scrollRect genFrequencyMinus) (scrollRect genFrequencyBar) (scrollRect genFrequencyPlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiGenOctaves ui) (scrollRect genOctavesMinus) (scrollRect genOctavesBar) (scrollRect genOctavesPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiGenLacunarity ui) (scrollRect genLacunarityMinus) (scrollRect genLacunarityBar) (scrollRect genLacunarityPlus) (V4 130 110 170 255)
          drawConfigSlider renderer (uiGenGain ui) (scrollRect genGainMinus) (scrollRect genGainBar) (scrollRect genGainPlus) (V4 140 120 160 255)
          drawConfigSlider renderer (uiGenWarpScale ui) (scrollRect genWarpScaleMinus) (scrollRect genWarpScaleBar) (scrollRect genWarpScalePlus) (V4 90 150 140 255)
          drawConfigSlider renderer (uiGenWarpStrength ui) (scrollRect genWarpStrengthMinus) (scrollRect genWarpStrengthBar) (scrollRect genWarpStrengthPlus) (V4 110 150 110 255)
          drawConfigSlider renderer (uiWorldExtentX ui) (scrollRect extentXMinus) (scrollRect extentXBar) (scrollRect extentXPlus) (V4 130 140 120 255)
          drawConfigSlider renderer (uiWorldExtentY ui) (scrollRect extentYMinus) (scrollRect extentYBar) (scrollRect extentYPlus) (V4 120 140 140 255)
          drawConfigSlider renderer (uiEdgeDepthNorth ui) (scrollRect edgeNorthMinus) (scrollRect edgeNorthBar) (scrollRect edgeNorthPlus) (V4 120 150 180 255)
          drawConfigSlider renderer (uiEdgeDepthSouth ui) (scrollRect edgeSouthMinus) (scrollRect edgeSouthBar) (scrollRect edgeSouthPlus) (V4 120 130 200 255)
          drawConfigSlider renderer (uiEdgeDepthEast ui) (scrollRect edgeEastMinus) (scrollRect edgeEastBar) (scrollRect edgeEastPlus) (V4 110 140 190 255)
          drawConfigSlider renderer (uiEdgeDepthWest ui) (scrollRect edgeWestMinus) (scrollRect edgeWestBar) (scrollRect edgeWestPlus) (V4 110 140 170 255)
          drawConfigSlider renderer (uiEdgeDepthFalloff ui) (scrollRect edgeFalloffMinus) (scrollRect edgeFalloffBar) (scrollRect edgeFalloffPlus) (V4 140 150 110 255)
          drawConfigSlider renderer (uiPlateSize ui) (scrollRect plateSizeMinus) (scrollRect plateSizeBar) (scrollRect plateSizePlus) (V4 120 120 170 255)
          drawConfigSlider renderer (uiUplift ui) (scrollRect upliftMinus) (scrollRect upliftBar) (scrollRect upliftPlus) (V4 150 120 120 255)
          drawConfigSlider renderer (uiRiftDepth ui) (scrollRect riftDepthMinus) (scrollRect riftDepthBar) (scrollRect riftDepthPlus) (V4 120 130 160 255)
          drawConfigSlider renderer (uiDetailScale ui) (scrollRect detailScaleMinus) (scrollRect detailScaleBar) (scrollRect detailScalePlus) (V4 90 140 160 255)
          drawConfigSlider renderer (uiPlateSpeed ui) (scrollRect plateSpeedMinus) (scrollRect plateSpeedBar) (scrollRect plateSpeedPlus) (V4 110 130 180 255)
          drawConfigSlider renderer (uiBoundarySharpness ui) (scrollRect boundarySharpnessMinus) (scrollRect boundarySharpnessBar) (scrollRect boundarySharpnessPlus) (V4 130 120 170 255)
          drawConfigSlider renderer (uiBoundaryNoiseScale ui) (scrollRect boundaryNoiseScaleMinus) (scrollRect boundaryNoiseScaleBar) (scrollRect boundaryNoiseScalePlus) (V4 100 140 150 255)
          drawConfigSlider renderer (uiBoundaryNoiseStrength ui) (scrollRect boundaryNoiseStrengthMinus) (scrollRect boundaryNoiseStrengthBar) (scrollRect boundaryNoiseStrengthPlus) (V4 140 130 140 255)
          drawConfigSlider renderer (uiBoundaryWarpOctaves ui) (scrollRect boundaryWarpOctavesMinus) (scrollRect boundaryWarpOctavesBar) (scrollRect boundaryWarpOctavesPlus) (V4 120 120 150 255)
          drawConfigSlider renderer (uiBoundaryWarpLacunarity ui) (scrollRect boundaryWarpLacunarityMinus) (scrollRect boundaryWarpLacunarityBar) (scrollRect boundaryWarpLacunarityPlus) (V4 120 120 170 255)
          drawConfigSlider renderer (uiBoundaryWarpGain ui) (scrollRect boundaryWarpGainMinus) (scrollRect boundaryWarpGainBar) (scrollRect boundaryWarpGainPlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiPlateMergeScale ui) (scrollRect plateMergeScaleMinus) (scrollRect plateMergeScaleBar) (scrollRect plateMergeScalePlus) (V4 120 140 120 255)
          drawConfigSlider renderer (uiPlateMergeBias ui) (scrollRect plateMergeBiasMinus) (scrollRect plateMergeBiasBar) (scrollRect plateMergeBiasPlus) (V4 140 130 120 255)
          drawConfigSlider renderer (uiPlateDetailScale ui) (scrollRect plateDetailScaleMinus) (scrollRect plateDetailScaleBar) (scrollRect plateDetailScalePlus) (V4 90 140 160 255)
          drawConfigSlider renderer (uiPlateDetailStrength ui) (scrollRect plateDetailStrengthMinus) (scrollRect plateDetailStrengthBar) (scrollRect plateDetailStrengthPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiPlateRidgeStrength ui) (scrollRect plateRidgeStrengthMinus) (scrollRect plateRidgeStrengthBar) (scrollRect plateRidgeStrengthPlus) (V4 150 130 120 255)
          drawConfigSlider renderer (uiPlateHeightBase ui) (scrollRect plateHeightBaseMinus) (scrollRect plateHeightBaseBar) (scrollRect plateHeightBasePlus) (V4 110 150 160 255)
          drawConfigSlider renderer (uiPlateHeightVariance ui) (scrollRect plateHeightVarianceMinus) (scrollRect plateHeightVarianceBar) (scrollRect plateHeightVariancePlus) (V4 140 140 160 255)
          drawConfigSlider renderer (uiPlateHardnessBase ui) (scrollRect plateHardnessBaseMinus) (scrollRect plateHardnessBaseBar) (scrollRect plateHardnessBasePlus) (V4 100 130 160 255)
          drawConfigSlider renderer (uiPlateHardnessVariance ui) (scrollRect plateHardnessVarianceMinus) (scrollRect plateHardnessVarianceBar) (scrollRect plateHardnessVariancePlus) (V4 120 140 160 255)
          drawConfigSlider renderer (uiTrenchDepth ui) (scrollRect trenchDepthMinus) (scrollRect trenchDepthBar) (scrollRect trenchDepthPlus) (V4 120 130 170 255)
          drawConfigSlider renderer (uiRidgeHeight ui) (scrollRect ridgeHeightMinus) (scrollRect ridgeHeightBar) (scrollRect ridgeHeightPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiPlateBiasStrength ui) (scrollRect plateBiasStrengthMinus) (scrollRect plateBiasStrengthBar) (scrollRect plateBiasStrengthPlus) (V4 140 120 160 255)
          drawConfigSlider renderer (uiPlateBiasCenter ui) (scrollRect plateBiasCenterMinus) (scrollRect plateBiasCenterBar) (scrollRect plateBiasCenterPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasEdge ui) (scrollRect plateBiasEdgeMinus) (scrollRect plateBiasEdgeBar) (scrollRect plateBiasEdgePlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasNorth ui) (scrollRect plateBiasNorthMinus) (scrollRect plateBiasNorthBar) (scrollRect plateBiasNorthPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiPlateBiasSouth ui) (scrollRect plateBiasSouthMinus) (scrollRect plateBiasSouthBar) (scrollRect plateBiasSouthPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiTfcCliffSlope ui) (scrollRect tfcCliffSlopeMinus) (scrollRect tfcCliffSlopeBar) (scrollRect tfcCliffSlopePlus) (V4 150 100 100 255)
          drawConfigSlider renderer (uiTfcMountainSlope ui) (scrollRect tfcMountainSlopeMinus) (scrollRect tfcMountainSlopeBar) (scrollRect tfcMountainSlopePlus) (V4 140 110 100 255)
          drawConfigSlider renderer (uiTfcMountainRelief ui) (scrollRect tfcMountainReliefMinus) (scrollRect tfcMountainReliefBar) (scrollRect tfcMountainReliefPlus) (V4 130 120 100 255)
          drawConfigSlider renderer (uiTfcHillSlope ui) (scrollRect tfcHillSlopeMinus) (scrollRect tfcHillSlopeBar) (scrollRect tfcHillSlopePlus) (V4 120 130 110 255)
          drawConfigSlider renderer (uiTfcRollingSlope ui) (scrollRect tfcRollingSlopeMinus) (scrollRect tfcRollingSlopeBar) (scrollRect tfcRollingSlopePlus) (V4 110 140 120 255)
          drawConfigSlider renderer (uiValleyCurvature ui) (scrollRect valleyCurvatureMinus) (scrollRect valleyCurvatureBar) (scrollRect valleyCurvaturePlus) (V4 100 140 140 255)
          drawConfigSlider renderer (uiRockElevationThreshold ui) (scrollRect rockElevationThresholdMinus) (scrollRect rockElevationThresholdBar) (scrollRect rockElevationThresholdPlus) (V4 140 130 120 255)
          drawConfigSlider renderer (uiRockHardnessThreshold ui) (scrollRect rockHardnessThresholdMinus) (scrollRect rockHardnessThresholdBar) (scrollRect rockHardnessThresholdPlus) (V4 130 130 130 255)
          drawConfigSlider renderer (uiRockHardnessSecondary ui) (scrollRect rockHardnessSecondaryMinus) (scrollRect rockHardnessSecondaryBar) (scrollRect rockHardnessSecondaryPlus) (V4 120 130 140 255)
        ConfigClimate -> do
          drawConfigSlider renderer (uiWaterLevel ui) (scrollRect waterMinus) (scrollRect waterBar) (scrollRect waterPlus) (V4 70 120 180 255)
          drawConfigSlider renderer (uiEvaporation ui) (scrollRect evapMinus) (scrollRect evapBar) (scrollRect evapPlus) (V4 140 110 80 255)
          drawConfigSlider renderer (uiRainShadow ui) (scrollRect rainShadowMinus) (scrollRect rainShadowBar) (scrollRect rainShadowPlus) (V4 110 140 190 255)
          drawConfigSlider renderer (uiWindDiffuse ui) (scrollRect windDiffuseMinus) (scrollRect windDiffuseBar) (scrollRect windDiffusePlus) (V4 90 140 120 255)
          drawConfigSlider renderer (uiEquatorTemp ui) (scrollRect equatorTempMinus) (scrollRect equatorTempBar) (scrollRect equatorTempPlus) (V4 180 120 90 255)
          drawConfigSlider renderer (uiPoleTemp ui) (scrollRect poleTempMinus) (scrollRect poleTempBar) (scrollRect poleTempPlus) (V4 90 150 200 255)
          drawConfigSlider renderer (uiLapseRate ui) (scrollRect lapseRateMinus) (scrollRect lapseRateBar) (scrollRect lapseRatePlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiWindIterations ui) (scrollRect windIterationsMinus) (scrollRect windIterationsBar) (scrollRect windIterationsPlus) (V4 120 140 200 255)
          drawConfigSlider renderer (uiMoistureIterations ui) (scrollRect moistureIterationsMinus) (scrollRect moistureIterationsBar) (scrollRect moistureIterationsPlus) (V4 120 150 160 255)
          drawConfigSlider renderer (uiBoundaryMotionTemp ui) (scrollRect boundaryMotionTempMinus) (scrollRect boundaryMotionTempBar) (scrollRect boundaryMotionTempPlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiBoundaryMotionPrecip ui) (scrollRect boundaryMotionPrecipMinus) (scrollRect boundaryMotionPrecipBar) (scrollRect boundaryMotionPrecipPlus) (V4 120 140 170 255)
          drawConfigSlider renderer (uiSliceLatCenter ui) (scrollRect sliceLatCenterMinus) (scrollRect sliceLatCenterBar) (scrollRect sliceLatCenterPlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiSliceLonCenter ui) (scrollRect sliceLonCenterMinus) (scrollRect sliceLonCenterBar) (scrollRect sliceLonCenterPlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiLatitudeExponent ui) (scrollRect latitudeExponentMinus) (scrollRect latitudeExponentBar) (scrollRect latitudeExponentPlus) (V4 150 110 130 255)
          drawConfigSlider renderer (uiPlateHeightCooling ui) (scrollRect plateHeightCoolingMinus) (scrollRect plateHeightCoolingBar) (scrollRect plateHeightCoolingPlus) (V4 130 130 150 255)
          drawConfigSlider renderer (uiTempNoiseScale ui) (scrollRect tempNoiseScaleMinus) (scrollRect tempNoiseScaleBar) (scrollRect tempNoiseScalePlus) (V4 140 120 150 255)
          drawConfigSlider renderer (uiOceanModeration ui) (scrollRect oceanModerationMinus) (scrollRect oceanModerationBar) (scrollRect oceanModerationPlus) (V4 80 140 170 255)
          drawConfigSlider renderer (uiOceanModerateTemp ui) (scrollRect oceanModerateTempMinus) (scrollRect oceanModerateTempBar) (scrollRect oceanModerateTempPlus) (V4 90 130 180 255)
          drawConfigSlider renderer (uiAlbedoSensitivity ui) (scrollRect albedoSensitivityMinus) (scrollRect albedoSensitivityBar) (scrollRect albedoSensitivityPlus) (V4 170 150 100 255)
          drawConfigSlider renderer (uiAlbedoReference ui) (scrollRect albedoReferenceMinus) (scrollRect albedoReferenceBar) (scrollRect albedoReferencePlus) (V4 160 140 110 255)
          drawConfigSlider renderer (uiMoistAdvect ui) (scrollRect moistAdvectMinus) (scrollRect moistAdvectBar) (scrollRect moistAdvectPlus) (V4 100 150 160 255)
          drawConfigSlider renderer (uiMoistLocal ui) (scrollRect moistLocalMinus) (scrollRect moistLocalBar) (scrollRect moistLocalPlus) (V4 110 140 170 255)
          drawConfigSlider renderer (uiMoistWindEvapScale ui) (scrollRect moistWindEvapScaleMinus) (scrollRect moistWindEvapScaleBar) (scrollRect moistWindEvapScalePlus) (V4 90 160 150 255)
          drawConfigSlider renderer (uiMoistEvapNoiseScale ui) (scrollRect moistEvapNoiseScaleMinus) (scrollRect moistEvapNoiseScaleBar) (scrollRect moistEvapNoiseScalePlus) (V4 120 140 160 255)
          drawConfigSlider renderer (uiMoistLandETCoeff ui) (scrollRect moistLandETCoeffMinus) (scrollRect moistLandETCoeffBar) (scrollRect moistLandETCoeffPlus) (V4 100 170 140 255)
          drawConfigSlider renderer (uiMoistBareEvapFrac ui) (scrollRect moistBareEvapFracMinus) (scrollRect moistBareEvapFracBar) (scrollRect moistBareEvapFracPlus) (V4 130 150 120 255)
          drawConfigSlider renderer (uiMoistVegTranspFrac ui) (scrollRect moistVegTranspFracMinus) (scrollRect moistVegTranspFracBar) (scrollRect moistVegTranspFracPlus) (V4 80 160 130 255)
          drawConfigSlider renderer (uiMoistWindETScale ui) (scrollRect moistWindETScaleMinus) (scrollRect moistWindETScaleBar) (scrollRect moistWindETScalePlus) (V4 110 150 150 255)
          drawConfigSlider renderer (uiMoistCondensationRate ui) (scrollRect moistCondensationRateMinus) (scrollRect moistCondensationRateBar) (scrollRect moistCondensationRatePlus) (V4 90 140 170 255)
          drawConfigSlider renderer (uiMoistRecycleRate ui) (scrollRect moistRecycleRateMinus) (scrollRect moistRecycleRateBar) (scrollRect moistRecycleRatePlus) (V4 100 130 160 255)
          drawConfigSlider renderer (uiMoistITCZStrength ui) (scrollRect moistITCZStrengthMinus) (scrollRect moistITCZStrengthBar) (scrollRect moistITCZStrengthPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiMoistITCZWidth ui) (scrollRect moistITCZWidthMinus) (scrollRect moistITCZWidthBar) (scrollRect moistITCZWidthPlus) (V4 110 160 130 255)
          drawConfigSlider renderer (uiOrographicScale ui) (scrollRect orographicScaleMinus) (scrollRect orographicScaleBar) (scrollRect orographicScalePlus) (V4 160 130 80 255)
          drawConfigSlider renderer (uiOrographicStep ui) (scrollRect orographicStepMinus) (scrollRect orographicStepBar) (scrollRect orographicStepPlus) (V4 150 140 90 255)
          drawConfigSlider renderer (uiCoastalIterations ui) (scrollRect coastalIterationsMinus) (scrollRect coastalIterationsBar) (scrollRect coastalIterationsPlus) (V4 100 160 160 255)
          drawConfigSlider renderer (uiCoastalDiffuse ui) (scrollRect coastalDiffuseMinus) (scrollRect coastalDiffuseBar) (scrollRect coastalDiffusePlus) (V4 90 150 170 255)
          drawConfigSlider renderer (uiCoastalMoistureBoost ui) (scrollRect coastalMoistureBoostMinus) (scrollRect coastalMoistureBoostBar) (scrollRect coastalMoistureBoostPlus) (V4 80 140 180 255)
          drawConfigSlider renderer (uiWindBeltStrength ui) (scrollRect windBeltStrengthMinus) (scrollRect windBeltStrengthBar) (scrollRect windBeltStrengthPlus) (V4 130 150 180 255)
          drawConfigSlider renderer (uiWindBeltHarmonics ui) (scrollRect windBeltHarmonicsMinus) (scrollRect windBeltHarmonicsBar) (scrollRect windBeltHarmonicsPlus) (V4 120 140 170 255)
          drawConfigSlider renderer (uiWindBeltBase ui) (scrollRect windBeltBaseMinus) (scrollRect windBeltBaseBar) (scrollRect windBeltBasePlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiWindBeltRange ui) (scrollRect windBeltRangeMinus) (scrollRect windBeltRangeBar) (scrollRect windBeltRangePlus) (V4 100 140 175 255)
          drawConfigSlider renderer (uiWindBeltSpeedScale ui) (scrollRect windBeltSpeedScaleMinus) (scrollRect windBeltSpeedScaleBar) (scrollRect windBeltSpeedScalePlus) (V4 90 135 185 255)
          drawConfigSlider renderer (uiBndLandRange ui) (scrollRect bndLandRangeMinus) (scrollRect bndLandRangeBar) (scrollRect bndLandRangePlus) (V4 140 130 100 255)
          drawConfigSlider renderer (uiBndTempConvergent ui) (scrollRect bndTempConvergentMinus) (scrollRect bndTempConvergentBar) (scrollRect bndTempConvergentPlus) (V4 130 120 110 255)
          drawConfigSlider renderer (uiBndTempDivergent ui) (scrollRect bndTempDivergentMinus) (scrollRect bndTempDivergentBar) (scrollRect bndTempDivergentPlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiBndTempTransform ui) (scrollRect bndTempTransformMinus) (scrollRect bndTempTransformBar) (scrollRect bndTempTransformPlus) (V4 140 140 110 255)
          drawConfigSlider renderer (uiBndPrecipConvergent ui) (scrollRect bndPrecipConvergentMinus) (scrollRect bndPrecipConvergentBar) (scrollRect bndPrecipConvergentPlus) (V4 100 140 170 255)
          drawConfigSlider renderer (uiBndPrecipDivergent ui) (scrollRect bndPrecipDivergentMinus) (scrollRect bndPrecipDivergentBar) (scrollRect bndPrecipDivergentPlus) (V4 90 130 160 255)
          drawConfigSlider renderer (uiBndPrecipTransform ui) (scrollRect bndPrecipTransformMinus) (scrollRect bndPrecipTransformBar) (scrollRect bndPrecipTransformPlus) (V4 110 140 150 255)
        ConfigPlanet -> do
          drawConfigSlider renderer (uiPlanetRadius ui) (scrollRect planetRadiusMinus) (scrollRect planetRadiusBar) (scrollRect planetRadiusPlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiAxialTilt ui) (scrollRect axialTiltMinus) (scrollRect axialTiltBar) (scrollRect axialTiltPlus) (V4 140 140 100 255)
          drawConfigSlider renderer (uiInsolation ui) (scrollRect insolationMinus) (scrollRect insolationBar) (scrollRect insolationPlus) (V4 180 150 80 255)
          drawConfigSlider renderer (uiOccWarmScale ui) (scrollRect occWarmScaleMinus) (scrollRect occWarmScaleBar) (scrollRect occWarmScalePlus) (V4 180 120 100 255)
          drawConfigSlider renderer (uiOccColdScale ui) (scrollRect occColdScaleMinus) (scrollRect occColdScaleBar) (scrollRect occColdScalePlus) (V4 100 140 180 255)
          drawConfigSlider renderer (uiOccLatPeakDeg ui) (scrollRect occLatPeakDegMinus) (scrollRect occLatPeakDegBar) (scrollRect occLatPeakDegPlus) (V4 140 130 160 255)
          drawConfigSlider renderer (uiOccLatWidthDeg ui) (scrollRect occLatWidthDegMinus) (scrollRect occLatWidthDegBar) (scrollRect occLatWidthDegPlus) (V4 130 150 140 255)
        ConfigWeather -> do
          drawConfigSlider renderer (uiWeatherTick ui) (scrollRect weatherTickMinus) (scrollRect weatherTickBar) (scrollRect weatherTickPlus) (V4 140 120 140 255)
          drawConfigSlider renderer (uiWeatherPhase ui) (scrollRect weatherPhaseMinus) (scrollRect weatherPhaseBar) (scrollRect weatherPhasePlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiWeatherAmplitude ui) (scrollRect weatherAmplitudeMinus) (scrollRect weatherAmplitudeBar) (scrollRect weatherAmplitudePlus) (V4 140 120 90 255)
          drawConfigSlider renderer (uiSeasonCycleLength ui) (scrollRect seasonCycleLengthMinus) (scrollRect seasonCycleLengthBar) (scrollRect seasonCycleLengthPlus) (V4 130 110 140 255)
          drawConfigSlider renderer (uiJitterAmplitude ui) (scrollRect jitterAmplitudeMinus) (scrollRect jitterAmplitudeBar) (scrollRect jitterAmplitudePlus) (V4 120 130 130 255)
          drawConfigSlider renderer (uiPressureBase ui) (scrollRect pressureBaseMinus) (scrollRect pressureBaseBar) (scrollRect pressureBasePlus) (V4 110 140 120 255)
          drawConfigSlider renderer (uiPressureTempScale ui) (scrollRect pressureTempScaleMinus) (scrollRect pressureTempScaleBar) (scrollRect pressureTempScalePlus) (V4 100 150 110 255)
          drawConfigSlider renderer (uiPressureCoriolisScale ui) (scrollRect pressureCoriolisScaleMinus) (scrollRect pressureCoriolisScaleBar) (scrollRect pressureCoriolisScalePlus) (V4 130 120 150 255)
          drawConfigSlider renderer (uiSeasonalBase ui) (scrollRect seasonalBaseMinus) (scrollRect seasonalBaseBar) (scrollRect seasonalBasePlus) (V4 140 110 130 255)
          drawConfigSlider renderer (uiSeasonalRange ui) (scrollRect seasonalRangeMinus) (scrollRect seasonalRangeBar) (scrollRect seasonalRangePlus) (V4 120 140 140 255)
          drawConfigSlider renderer (uiHumidityNoiseScale ui) (scrollRect humidityNoiseScaleMinus) (scrollRect humidityNoiseScaleBar) (scrollRect humidityNoiseScalePlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiPrecipNoiseScale ui) (scrollRect precipNoiseScaleMinus) (scrollRect precipNoiseScaleBar) (scrollRect precipNoiseScalePlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiWeatherITCZWidth ui) (scrollRect weatherITCZWidthMinus) (scrollRect weatherITCZWidthBar) (scrollRect weatherITCZWidthPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiWeatherITCZPrecipBoost ui) (scrollRect weatherITCZPrecipBoostMinus) (scrollRect weatherITCZPrecipBoostBar) (scrollRect weatherITCZPrecipBoostPlus) (V4 120 150 140 255)
          drawConfigSlider renderer (uiPressureHumidityScale ui) (scrollRect pressureHumidityScaleMinus) (scrollRect pressureHumidityScaleBar) (scrollRect pressureHumidityScalePlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiPressureGradientWindScale ui) (scrollRect pressureGradientWindScaleMinus) (scrollRect pressureGradientWindScaleBar) (scrollRect pressureGradientWindScalePlus) (V4 100 130 150 255)
          drawConfigSlider renderer (uiWindNoiseScale ui) (scrollRect windNoiseScaleMinus) (scrollRect windNoiseScaleBar) (scrollRect windNoiseScalePlus) (V4 130 120 160 255)
          drawConfigSlider renderer (uiITCZMigrationScale ui) (scrollRect itczMigrationScaleMinus) (scrollRect itczMigrationScaleBar) (scrollRect itczMigrationScalePlus) (V4 140 130 170 255)
          drawConfigSlider renderer (uiCloudRHExponent ui) (scrollRect cloudRHExponentMinus) (scrollRect cloudRHExponentBar) (scrollRect cloudRHExponentPlus) (V4 120 120 140 255)
          drawConfigSlider renderer (uiCloudAlbedoEffect ui) (scrollRect cloudAlbedoEffectMinus) (scrollRect cloudAlbedoEffectBar) (scrollRect cloudAlbedoEffectPlus) (V4 110 110 150 255)
          drawConfigSlider renderer (uiCloudPrecipBoost ui) (scrollRect cloudPrecipBoostMinus) (scrollRect cloudPrecipBoostBar) (scrollRect cloudPrecipBoostPlus) (V4 100 100 160 255)
        ConfigBiome -> do
          drawConfigSlider renderer (uiVegBase ui) (scrollRect vegBaseMinus) (scrollRect vegBaseBar) (scrollRect vegBasePlus) (V4 100 130 120 255)
          drawConfigSlider renderer (uiVegBoost ui) (scrollRect vegBoostMinus) (scrollRect vegBoostBar) (scrollRect vegBoostPlus) (V4 120 150 120 255)
          drawConfigSlider renderer (uiVegTempWeight ui) (scrollRect vegTempWeightMinus) (scrollRect vegTempWeightBar) (scrollRect vegTempWeightPlus) (V4 130 140 170 255)
          drawConfigSlider renderer (uiVegPrecipWeight ui) (scrollRect vegPrecipWeightMinus) (scrollRect vegPrecipWeightBar) (scrollRect vegPrecipWeightPlus) (V4 120 140 190 255)
          drawConfigSlider renderer (uiBtCoastalBand ui) (scrollRect btCoastalBandMinus) (scrollRect btCoastalBandBar) (scrollRect btCoastalBandPlus) (V4 100 160 180 255)
          drawConfigSlider renderer (uiBtSnowMaxTemp ui) (scrollRect btSnowMaxTempMinus) (scrollRect btSnowMaxTempBar) (scrollRect btSnowMaxTempPlus) (V4 180 180 200 255)
          drawConfigSlider renderer (uiBtAlpineMaxTemp ui) (scrollRect btAlpineMaxTempMinus) (scrollRect btAlpineMaxTempBar) (scrollRect btAlpineMaxTempPlus) (V4 160 160 180 255)
          drawConfigSlider renderer (uiBtIceCapTemp ui) (scrollRect btIceCapTempMinus) (scrollRect btIceCapTempBar) (scrollRect btIceCapTempPlus) (V4 180 200 220 255)
          drawConfigSlider renderer (uiBtMontaneMaxTemp ui) (scrollRect btMontaneMaxTempMinus) (scrollRect btMontaneMaxTempBar) (scrollRect btMontaneMaxTempPlus) (V4 140 130 110 255)
          drawConfigSlider renderer (uiBtMontanePrecip ui) (scrollRect btMontanePrecipMinus) (scrollRect btMontanePrecipBar) (scrollRect btMontanePrecipPlus) (V4 120 140 120 255)
          drawConfigSlider renderer (uiBtCliffSlope ui) (scrollRect btCliffSlopeMinus) (scrollRect btCliffSlopeBar) (scrollRect btCliffSlopePlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiBtValleyMoisture ui) (scrollRect btValleyMoistureMinus) (scrollRect btValleyMoistureBar) (scrollRect btValleyMoisturePlus) (V4 100 150 130 255)
          drawConfigSlider renderer (uiBtDepressionMoisture ui) (scrollRect btDepressionMoistureMinus) (scrollRect btDepressionMoistureBar) (scrollRect btDepressionMoisturePlus) (V4 110 140 140 255)
          drawConfigSlider renderer (uiBtPrecipWeight ui) (scrollRect btPrecipWeightMinus) (scrollRect btPrecipWeightBar) (scrollRect btPrecipWeightPlus) (V4 120 130 150 255)
          drawConfigSlider renderer (uiVbcTempMin ui) (scrollRect vbcTempMinMinus) (scrollRect vbcTempMinBar) (scrollRect vbcTempMinPlus) (V4 130 120 100 255)
          drawConfigSlider renderer (uiVbcTempRange ui) (scrollRect vbcTempRangeMinus) (scrollRect vbcTempRangeBar) (scrollRect vbcTempRangePlus) (V4 140 130 110 255)
          drawConfigSlider renderer (uiVbcFertilityBoost ui) (scrollRect vbcFertilityBoostMinus) (scrollRect vbcFertilityBoostBar) (scrollRect vbcFertilityBoostPlus) (V4 100 150 100 255)
          drawConfigSlider renderer (uiVbcAlbedoBase ui) (scrollRect vbcAlbedoBaseMinus) (scrollRect vbcAlbedoBaseBar) (scrollRect vbcAlbedoBasePlus) (V4 160 160 140 255)
          drawConfigSlider renderer (uiVbcAlbedoBare ui) (scrollRect vbcAlbedoBareMinus) (scrollRect vbcAlbedoBareBar) (scrollRect vbcAlbedoBarePlus) (V4 170 160 130 255)
          drawConfigSlider renderer (uiVbcAlbedoVeg ui) (scrollRect vbcAlbedoVegMinus) (scrollRect vbcAlbedoVegBar) (scrollRect vbcAlbedoVegPlus) (V4 100 140 100 255)
          drawConfigSlider renderer (uiVbcOceanAlbedo ui) (scrollRect vbcOceanAlbedoMinus) (scrollRect vbcOceanAlbedoBar) (scrollRect vbcOceanAlbedoPlus) (V4 100 120 170 255)
          drawConfigSlider renderer (uiVbcIceAlbedo ui) (scrollRect vbcIceAlbedoMinus) (scrollRect vbcIceAlbedoBar) (scrollRect vbcIceAlbedoPlus) (V4 190 200 210 255)
          drawConfigSlider renderer (uiBiomeSmoothing ui) (scrollRect biomeSmoothingMinus) (scrollRect biomeSmoothingBar) (scrollRect biomeSmoothingPlus) (V4 130 140 150 255)
          drawConfigSlider renderer (uiVolcanicAshBoost ui) (scrollRect volcanicAshBoostMinus) (scrollRect volcanicAshBoostBar) (scrollRect volcanicAshBoostPlus) (V4 160 120 80 255)
          drawConfigSlider renderer (uiVolcanicLavaPenalty ui) (scrollRect volcanicLavaPenaltyMinus) (scrollRect volcanicLavaPenaltyBar) (scrollRect volcanicLavaPenaltyPlus) (V4 180 100 60 255)
          drawConfigSlider renderer (uiBiomeFeedbackBlend ui) (scrollRect biomeFeedbackBlendMinus) (scrollRect biomeFeedbackBlendBar) (scrollRect biomeFeedbackBlendPlus) (V4 120 130 140 255)
        ConfigErosion -> do
          drawConfigSlider renderer (uiErosionHydraulic ui) (scrollRect erosionHydraulicMinus) (scrollRect erosionHydraulicBar) (scrollRect erosionHydraulicPlus) (V4 90 140 180 255)
          drawConfigSlider renderer (uiErosionThermal ui) (scrollRect erosionThermalMinus) (scrollRect erosionThermalBar) (scrollRect erosionThermalPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiRainRate ui) (scrollRect erosionRainRateMinus) (scrollRect erosionRainRateBar) (scrollRect erosionRainRatePlus) (V4 100 110 170 255)
          drawConfigSlider renderer (uiErosionTalus ui) (scrollRect erosionTalusMinus) (scrollRect erosionTalusBar) (scrollRect erosionTalusPlus) (V4 160 120 90 255)
          drawConfigSlider renderer (uiErosionMaxDrop ui) (scrollRect erosionMaxDropMinus) (scrollRect erosionMaxDropBar) (scrollRect erosionMaxDropPlus) (V4 140 120 120 255)
          drawConfigSlider renderer (uiGlacierSnowTemp ui) (scrollRect glacierSnowTempMinus) (scrollRect glacierSnowTempBar) (scrollRect glacierSnowTempPlus) (V4 180 210 240 255)
          drawConfigSlider renderer (uiGlacierSnowRange ui) (scrollRect glacierSnowRangeMinus) (scrollRect glacierSnowRangeBar) (scrollRect glacierSnowRangePlus) (V4 170 200 235 255)
          drawConfigSlider renderer (uiGlacierMeltTemp ui) (scrollRect glacierMeltTempMinus) (scrollRect glacierMeltTempBar) (scrollRect glacierMeltTempPlus) (V4 160 195 230 255)
          drawConfigSlider renderer (uiGlacierMeltRate ui) (scrollRect glacierMeltRateMinus) (scrollRect glacierMeltRateBar) (scrollRect glacierMeltRatePlus) (V4 150 190 225 255)
          drawConfigSlider renderer (uiGlacierAccumScale ui) (scrollRect glacierAccumScaleMinus) (scrollRect glacierAccumScaleBar) (scrollRect glacierAccumScalePlus) (V4 140 185 220 255)
          drawConfigSlider renderer (uiGlacierFlowIters ui) (scrollRect glacierFlowItersMinus) (scrollRect glacierFlowItersBar) (scrollRect glacierFlowItersPlus) (V4 130 180 215 255)
          drawConfigSlider renderer (uiGlacierFlowRate ui) (scrollRect glacierFlowRateMinus) (scrollRect glacierFlowRateBar) (scrollRect glacierFlowRatePlus) (V4 120 175 210 255)
          drawConfigSlider renderer (uiGlacierErosionScale ui) (scrollRect glacierErosionScaleMinus) (scrollRect glacierErosionScaleBar) (scrollRect glacierErosionScalePlus) (V4 110 170 210 255)
          drawConfigSlider renderer (uiGlacierCarveScale ui) (scrollRect glacierCarveScaleMinus) (scrollRect glacierCarveScaleBar) (scrollRect glacierCarveScalePlus) (V4 100 165 205 255)
          drawConfigSlider renderer (uiGlacierDepositScale ui) (scrollRect glacierDepositScaleMinus) (scrollRect glacierDepositScaleBar) (scrollRect glacierDepositScalePlus) (V4 90 160 200 255)
          drawConfigSlider renderer (uiVentDensity ui) (scrollRect ventDensityMinus) (scrollRect ventDensityBar) (scrollRect ventDensityPlus) (V4 200 80 60 255)
          drawConfigSlider renderer (uiVentThreshold ui) (scrollRect ventThresholdMinus) (scrollRect ventThresholdBar) (scrollRect ventThresholdPlus) (V4 190 90 70 255)
          drawConfigSlider renderer (uiHotspotScale ui) (scrollRect hotspotScaleMinus) (scrollRect hotspotScaleBar) (scrollRect hotspotScalePlus) (V4 210 100 50 255)
          drawConfigSlider renderer (uiHotspotThreshold ui) (scrollRect hotspotThresholdMinus) (scrollRect hotspotThresholdBar) (scrollRect hotspotThresholdPlus) (V4 200 110 60 255)
          drawConfigSlider renderer (uiMagmaRecharge ui) (scrollRect magmaRechargeMinus) (scrollRect magmaRechargeBar) (scrollRect magmaRechargePlus) (V4 220 90 40 255)
          drawConfigSlider renderer (uiLavaScale ui) (scrollRect lavaScaleMinus) (scrollRect lavaScaleBar) (scrollRect lavaScalePlus) (V4 230 80 30 255)
          drawConfigSlider renderer (uiAshScale ui) (scrollRect ashScaleMinus) (scrollRect ashScaleBar) (scrollRect ashScalePlus) (V4 170 130 100 255)
          drawConfigSlider renderer (uiVolcanicDepositScale ui) (scrollRect volcanicDepositScaleMinus) (scrollRect volcanicDepositScaleBar) (scrollRect volcanicDepositScalePlus) (V4 180 120 80 255)
          drawConfigSlider renderer (uiSoilMoistureThreshold ui) (scrollRect soilMoistureThresholdMinus) (scrollRect soilMoistureThresholdBar) (scrollRect soilMoistureThresholdPlus) (V4 140 120 80 255)
          drawConfigSlider renderer (uiSoilHardnessThreshold ui) (scrollRect soilHardnessThresholdMinus) (scrollRect soilHardnessThresholdBar) (scrollRect soilHardnessThresholdPlus) (V4 150 130 90 255)
          drawConfigSlider renderer (uiSoilFertilityMoistWeight ui) (scrollRect soilFertilityMoistWeightMinus) (scrollRect soilFertilityMoistWeightBar) (scrollRect soilFertilityMoistWeightPlus) (V4 120 140 80 255)
          drawConfigSlider renderer (uiSoilFertilityDepthWeight ui) (scrollRect soilFertilityDepthWeightMinus) (scrollRect soilFertilityDepthWeightBar) (scrollRect soilFertilityDepthWeightPlus) (V4 130 140 90 255)
          drawConfigSlider renderer (uiSinkBreachDepth ui) (scrollRect sinkBreachDepthMinus) (scrollRect sinkBreachDepthBar) (scrollRect sinkBreachDepthPlus) (V4 70 130 170 255)
          drawConfigSlider renderer (uiStreamPowerMaxErosion ui) (scrollRect streamPowerMaxErosionMinus) (scrollRect streamPowerMaxErosionBar) (scrollRect streamPowerMaxErosionPlus) (V4 80 140 170 255)
          drawConfigSlider renderer (uiRiverCarveMaxDepth ui) (scrollRect riverCarveMaxDepthMinus) (scrollRect riverCarveMaxDepthBar) (scrollRect riverCarveMaxDepthPlus) (V4 60 120 160 255)
          drawConfigSlider renderer (uiCoastalErodeStrength ui) (scrollRect coastalErodeStrengthMinus) (scrollRect coastalErodeStrengthBar) (scrollRect coastalErodeStrengthPlus) (V4 70 110 150 255)
          drawConfigSlider renderer (uiHydroHardnessWeight ui) (scrollRect hydroHardnessWeightMinus) (scrollRect hydroHardnessWeightBar) (scrollRect hydroHardnessWeightPlus) (V4 90 130 160 255)
          drawConfigSlider renderer (uiMinLakeSize ui) (scrollRect minLakeSizeMinus) (scrollRect minLakeSizeBar) (scrollRect minLakeSizePlus) (V4 60 140 180 255)
          drawConfigSlider renderer (uiInlandSeaMinSize ui) (scrollRect inlandSeaMinSizeMinus) (scrollRect inlandSeaMinSizeBar) (scrollRect inlandSeaMinSizePlus) (V4 50 130 170 255)
          drawConfigSlider renderer (uiRoughnessScale ui) (scrollRect roughnessScaleMinus) (scrollRect roughnessScaleBar) (scrollRect roughnessScalePlus) (V4 110 130 140 255)
      SDL.rendererClipRect renderer SDL.$= Nothing
      let
        { Rect (V2 bx by, V2 bw bh) = scrollBarRect
        ; handleH = if maxOffset == 0
            then bh
            else max 12 (bh * scrollH `div` max 1 contentHeight)
        ; handleY = if maxOffset == 0
            then by
            else by + (bh - handleH) * scrollY `div` maxOffset
        }
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
            Just _  -> V4 140 100 50 255
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

drawChunkControl :: SDL.Renderer -> UiState -> Rect -> Rect -> Rect -> IO ()
drawChunkControl renderer ui minusRect valueRect plusRect = do
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

drawUiLabels :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawUiLabels renderer fontCache ui layout = do
  let buttonRect = leftGenButtonRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      configToggle = configToggleRect layout
      configPresetSave = configPresetSaveRect layout
      configPresetLoad = configPresetLoadRect layout
      configReset = configResetRect layout
      configRevert = configRevertRect layout
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) = configTabRects layout
      configWaterMinus = configWaterMinusRect layout
      configWaterPlus = configWaterPlusRect layout
      configWaterBar = configWaterBarRect layout
      configEvapMinus = configEvapMinusRect layout
      configEvapPlus = configEvapPlusRect layout
      configEvapBar = configEvapBarRect layout
      configRainShadowMinus = configRainShadowMinusRect layout
      configRainShadowPlus = configRainShadowPlusRect layout
      configRainShadowBar = configRainShadowBarRect layout
      configWindDiffuseMinus = configWindDiffuseMinusRect layout
      configWindDiffusePlus = configWindDiffusePlusRect layout
      configWindDiffuseBar = configWindDiffuseBarRect layout
      configEquatorTempMinus = configEquatorTempMinusRect layout
      configEquatorTempPlus = configEquatorTempPlusRect layout
      configEquatorTempBar = configEquatorTempBarRect layout
      configPoleTempMinus = configPoleTempMinusRect layout
      configPoleTempPlus = configPoleTempPlusRect layout
      configPoleTempBar = configPoleTempBarRect layout
      configLapseRateMinus = configLapseRateMinusRect layout
      configLapseRatePlus = configLapseRatePlusRect layout
      configLapseRateBar = configLapseRateBarRect layout
      configWindIterationsMinus = configWindIterationsMinusRect layout
      configWindIterationsPlus = configWindIterationsPlusRect layout
      configWindIterationsBar = configWindIterationsBarRect layout
      configMoistureIterationsMinus = configMoistureIterationsMinusRect layout
      configMoistureIterationsPlus = configMoistureIterationsPlusRect layout
      configMoistureIterationsBar = configMoistureIterationsBarRect layout
      configWeatherTickMinus = configWeatherTickMinusRect layout
      configWeatherTickPlus = configWeatherTickPlusRect layout
      configWeatherTickBar = configWeatherTickBarRect layout
      configWeatherPhaseMinus = configWeatherPhaseMinusRect layout
      configWeatherPhasePlus = configWeatherPhasePlusRect layout
      configWeatherPhaseBar = configWeatherPhaseBarRect layout
      configWeatherAmplitudeMinus = configWeatherAmplitudeMinusRect layout
      configWeatherAmplitudePlus = configWeatherAmplitudePlusRect layout
      configWeatherAmplitudeBar = configWeatherAmplitudeBarRect layout
      configSeasonCycleLengthMinus = configSeasonCycleLengthMinusRect layout
      configSeasonCycleLengthPlus = configSeasonCycleLengthPlusRect layout
      configSeasonCycleLengthBar = configSeasonCycleLengthBarRect layout
      configJitterAmplitudeMinus = configJitterAmplitudeMinusRect layout
      configJitterAmplitudePlus = configJitterAmplitudePlusRect layout
      configJitterAmplitudeBar = configJitterAmplitudeBarRect layout
      configPressureBaseMinus = configPressureBaseMinusRect layout
      configPressureBasePlus = configPressureBasePlusRect layout
      configPressureBaseBar = configPressureBaseBarRect layout
      configPressureTempScaleMinus = configPressureTempScaleMinusRect layout
      configPressureTempScalePlus = configPressureTempScalePlusRect layout
      configPressureTempScaleBar = configPressureTempScaleBarRect layout
      configPressureCoriolisScaleMinus = configPressureCoriolisScaleMinusRect layout
      configPressureCoriolisScalePlus = configPressureCoriolisScalePlusRect layout
      configPressureCoriolisScaleBar = configPressureCoriolisScaleBarRect layout
      configSeasonalBaseMinus = configSeasonalBaseMinusRect layout
      configSeasonalBasePlus = configSeasonalBasePlusRect layout
      configSeasonalBaseBar = configSeasonalBaseBarRect layout
      configSeasonalRangeMinus = configSeasonalRangeMinusRect layout
      configSeasonalRangePlus = configSeasonalRangePlusRect layout
      configSeasonalRangeBar = configSeasonalRangeBarRect layout
      configHumidityNoiseScaleMinus = configHumidityNoiseScaleMinusRect layout
      configHumidityNoiseScalePlus = configHumidityNoiseScalePlusRect layout
      configHumidityNoiseScaleBar = configHumidityNoiseScaleBarRect layout
      configPrecipNoiseScaleMinus = configPrecipNoiseScaleMinusRect layout
      configPrecipNoiseScalePlus = configPrecipNoiseScalePlusRect layout
      configPrecipNoiseScaleBar = configPrecipNoiseScaleBarRect layout
      configWeatherITCZWidthMinus = configWeatherITCZWidthMinusRect layout
      configWeatherITCZWidthPlus = configWeatherITCZWidthPlusRect layout
      configWeatherITCZWidthBar = configWeatherITCZWidthBarRect layout
      configWeatherITCZPrecipBoostMinus = configWeatherITCZPrecipBoostMinusRect layout
      configWeatherITCZPrecipBoostPlus = configWeatherITCZPrecipBoostPlusRect layout
      configWeatherITCZPrecipBoostBar = configWeatherITCZPrecipBoostBarRect layout
      configPressureHumidityScaleMinus = configPressureHumidityScaleMinusRect layout
      configPressureHumidityScalePlus = configPressureHumidityScalePlusRect layout
      configPressureHumidityScaleBar = configPressureHumidityScaleBarRect layout
      configPressureGradientWindScaleMinus = configPressureGradientWindScaleMinusRect layout
      configPressureGradientWindScalePlus = configPressureGradientWindScalePlusRect layout
      configPressureGradientWindScaleBar = configPressureGradientWindScaleBarRect layout
      configWindNoiseScaleMinus = configWindNoiseScaleMinusRect layout
      configWindNoiseScalePlus = configWindNoiseScalePlusRect layout
      configWindNoiseScaleBar = configWindNoiseScaleBarRect layout
      configITCZMigrationScaleMinus = configITCZMigrationScaleMinusRect layout
      configITCZMigrationScalePlus = configITCZMigrationScalePlusRect layout
      configITCZMigrationScaleBar = configITCZMigrationScaleBarRect layout
      configCloudRHExponentMinus = configCloudRHExponentMinusRect layout
      configCloudRHExponentPlus = configCloudRHExponentPlusRect layout
      configCloudRHExponentBar = configCloudRHExponentBarRect layout
      configCloudAlbedoEffectMinus = configCloudAlbedoEffectMinusRect layout
      configCloudAlbedoEffectPlus = configCloudAlbedoEffectPlusRect layout
      configCloudAlbedoEffectBar = configCloudAlbedoEffectBarRect layout
      configCloudPrecipBoostMinus = configCloudPrecipBoostMinusRect layout
      configCloudPrecipBoostPlus = configCloudPrecipBoostPlusRect layout
      configCloudPrecipBoostBar = configCloudPrecipBoostBarRect layout
      configVegBaseMinus = configVegBaseMinusRect layout
      configVegBasePlus = configVegBasePlusRect layout
      configVegBaseBar = configVegBaseBarRect layout
      configVegBoostMinus = configVegBoostMinusRect layout
      configVegBoostPlus = configVegBoostPlusRect layout
      configVegBoostBar = configVegBoostBarRect layout
      configVegTempWeightMinus = configVegTempWeightMinusRect layout
      configVegTempWeightPlus = configVegTempWeightPlusRect layout
      configVegTempWeightBar = configVegTempWeightBarRect layout
      configVegPrecipWeightMinus = configVegPrecipWeightMinusRect layout
      configVegPrecipWeightPlus = configVegPrecipWeightPlusRect layout
      configVegPrecipWeightBar = configVegPrecipWeightBarRect layout
      configBtCoastalBandMinus = configBtCoastalBandMinusRect layout
      configBtCoastalBandPlus = configBtCoastalBandPlusRect layout
      configBtCoastalBandBar = configBtCoastalBandBarRect layout
      configBtSnowMaxTempMinus = configBtSnowMaxTempMinusRect layout
      configBtSnowMaxTempPlus = configBtSnowMaxTempPlusRect layout
      configBtSnowMaxTempBar = configBtSnowMaxTempBarRect layout
      configBtAlpineMaxTempMinus = configBtAlpineMaxTempMinusRect layout
      configBtAlpineMaxTempPlus = configBtAlpineMaxTempPlusRect layout
      configBtAlpineMaxTempBar = configBtAlpineMaxTempBarRect layout
      configBtIceCapTempMinus = configBtIceCapTempMinusRect layout
      configBtIceCapTempPlus = configBtIceCapTempPlusRect layout
      configBtIceCapTempBar = configBtIceCapTempBarRect layout
      configBtMontaneMaxTempMinus = configBtMontaneMaxTempMinusRect layout
      configBtMontaneMaxTempPlus = configBtMontaneMaxTempPlusRect layout
      configBtMontaneMaxTempBar = configBtMontaneMaxTempBarRect layout
      configBtMontanePrecipMinus = configBtMontanePrecipMinusRect layout
      configBtMontanePrecipPlus = configBtMontanePrecipPlusRect layout
      configBtMontanePrecipBar = configBtMontanePrecipBarRect layout
      configBtCliffSlopeMinus = configBtCliffSlopeMinusRect layout
      configBtCliffSlopePlus = configBtCliffSlopePlusRect layout
      configBtCliffSlopeBar = configBtCliffSlopeBarRect layout
      configBtValleyMoistureMinus = configBtValleyMoistureMinusRect layout
      configBtValleyMoisturePlus = configBtValleyMoisturePlusRect layout
      configBtValleyMoistureBar = configBtValleyMoistureBarRect layout
      configBtDepressionMoistureMinus = configBtDepressionMoistureMinusRect layout
      configBtDepressionMoisturePlus = configBtDepressionMoisturePlusRect layout
      configBtDepressionMoistureBar = configBtDepressionMoistureBarRect layout
      configBtPrecipWeightMinus = configBtPrecipWeightMinusRect layout
      configBtPrecipWeightPlus = configBtPrecipWeightPlusRect layout
      configBtPrecipWeightBar = configBtPrecipWeightBarRect layout
      configVbcTempMinMinus = configVbcTempMinMinusRect layout
      configVbcTempMinPlus = configVbcTempMinPlusRect layout
      configVbcTempMinBar = configVbcTempMinBarRect layout
      configVbcTempRangeMinus = configVbcTempRangeMinusRect layout
      configVbcTempRangePlus = configVbcTempRangePlusRect layout
      configVbcTempRangeBar = configVbcTempRangeBarRect layout
      configVbcFertilityBoostMinus = configVbcFertilityBoostMinusRect layout
      configVbcFertilityBoostPlus = configVbcFertilityBoostPlusRect layout
      configVbcFertilityBoostBar = configVbcFertilityBoostBarRect layout
      configVbcAlbedoBaseMinus = configVbcAlbedoBaseMinusRect layout
      configVbcAlbedoBasePlus = configVbcAlbedoBasePlusRect layout
      configVbcAlbedoBaseBar = configVbcAlbedoBaseBarRect layout
      configVbcAlbedoBareMinus = configVbcAlbedoBareMinusRect layout
      configVbcAlbedoBarePlus = configVbcAlbedoBarePlusRect layout
      configVbcAlbedoBareBar = configVbcAlbedoBareBarRect layout
      configVbcAlbedoVegMinus = configVbcAlbedoVegMinusRect layout
      configVbcAlbedoVegPlus = configVbcAlbedoVegPlusRect layout
      configVbcAlbedoVegBar = configVbcAlbedoVegBarRect layout
      configVbcOceanAlbedoMinus = configVbcOceanAlbedoMinusRect layout
      configVbcOceanAlbedoPlus = configVbcOceanAlbedoPlusRect layout
      configVbcOceanAlbedoBar = configVbcOceanAlbedoBarRect layout
      configVbcIceAlbedoMinus = configVbcIceAlbedoMinusRect layout
      configVbcIceAlbedoPlus = configVbcIceAlbedoPlusRect layout
      configVbcIceAlbedoBar = configVbcIceAlbedoBarRect layout
      configBiomeSmoothingMinus = configBiomeSmoothingMinusRect layout
      configBiomeSmoothingPlus = configBiomeSmoothingPlusRect layout
      configBiomeSmoothingBar = configBiomeSmoothingBarRect layout
      configVolcanicAshBoostMinus = configVolcanicAshBoostMinusRect layout
      configVolcanicAshBoostPlus = configVolcanicAshBoostPlusRect layout
      configVolcanicAshBoostBar = configVolcanicAshBoostBarRect layout
      configVolcanicLavaPenaltyMinus = configVolcanicLavaPenaltyMinusRect layout
      configVolcanicLavaPenaltyPlus = configVolcanicLavaPenaltyPlusRect layout
      configVolcanicLavaPenaltyBar = configVolcanicLavaPenaltyBarRect layout
      configBiomeFeedbackBlendMinus = configBiomeFeedbackBlendMinusRect layout
      configBiomeFeedbackBlendPlus = configBiomeFeedbackBlendPlusRect layout
      configBiomeFeedbackBlendBar = configBiomeFeedbackBlendBarRect layout
      configBoundaryMotionTempMinus = configBoundaryMotionTempMinusRect layout
      configBoundaryMotionTempPlus = configBoundaryMotionTempPlusRect layout
      configBoundaryMotionTempBar = configBoundaryMotionTempBarRect layout
      configBoundaryMotionPrecipMinus = configBoundaryMotionPrecipMinusRect layout
      configBoundaryMotionPrecipPlus = configBoundaryMotionPrecipPlusRect layout
      configBoundaryMotionPrecipBar = configBoundaryMotionPrecipBarRect layout
      configPlanetRadiusMinus = configPlanetRadiusMinusRect layout
      configPlanetRadiusPlus = configPlanetRadiusPlusRect layout
      configPlanetRadiusBar = configPlanetRadiusBarRect layout
      configAxialTiltMinus = configAxialTiltMinusRect layout
      configAxialTiltPlus = configAxialTiltPlusRect layout
      configAxialTiltBar = configAxialTiltBarRect layout
      configInsolationMinus = configInsolationMinusRect layout
      configInsolationPlus = configInsolationPlusRect layout
      configInsolationBar = configInsolationBarRect layout
      configOccWarmScaleMinus = configOccWarmScaleMinusRect layout
      configOccWarmScalePlus = configOccWarmScalePlusRect layout
      configOccWarmScaleBar = configOccWarmScaleBarRect layout
      configOccColdScaleMinus = configOccColdScaleMinusRect layout
      configOccColdScalePlus = configOccColdScalePlusRect layout
      configOccColdScaleBar = configOccColdScaleBarRect layout
      configOccLatPeakDegMinus = configOccLatPeakDegMinusRect layout
      configOccLatPeakDegPlus = configOccLatPeakDegPlusRect layout
      configOccLatPeakDegBar = configOccLatPeakDegBarRect layout
      configOccLatWidthDegMinus = configOccLatWidthDegMinusRect layout
      configOccLatWidthDegPlus = configOccLatWidthDegPlusRect layout
      configOccLatWidthDegBar = configOccLatWidthDegBarRect layout
      configSliceLatCenterMinus = configSliceLatCenterMinusRect layout
      configSliceLatCenterPlus = configSliceLatCenterPlusRect layout
      configSliceLatCenterBar = configSliceLatCenterBarRect layout
      configSliceLonCenterMinus = configSliceLonCenterMinusRect layout
      configSliceLonCenterPlus = configSliceLonCenterPlusRect layout
      configSliceLonCenterBar = configSliceLonCenterBarRect layout
      configLatitudeExponentMinus = configLatitudeExponentMinusRect layout
      configLatitudeExponentPlus = configLatitudeExponentPlusRect layout
      configLatitudeExponentBar = configLatitudeExponentBarRect layout
      configPlateHeightCoolingMinus = configPlateHeightCoolingMinusRect layout
      configPlateHeightCoolingPlus = configPlateHeightCoolingPlusRect layout
      configPlateHeightCoolingBar = configPlateHeightCoolingBarRect layout
      configTempNoiseScaleMinus = configTempNoiseScaleMinusRect layout
      configTempNoiseScalePlus = configTempNoiseScalePlusRect layout
      configTempNoiseScaleBar = configTempNoiseScaleBarRect layout
      configOceanModerationMinus = configOceanModerationMinusRect layout
      configOceanModerationPlus = configOceanModerationPlusRect layout
      configOceanModerationBar = configOceanModerationBarRect layout
      configOceanModerateTempMinus = configOceanModerateTempMinusRect layout
      configOceanModerateTempPlus = configOceanModerateTempPlusRect layout
      configOceanModerateTempBar = configOceanModerateTempBarRect layout
      configAlbedoSensitivityMinus = configAlbedoSensitivityMinusRect layout
      configAlbedoSensitivityPlus = configAlbedoSensitivityPlusRect layout
      configAlbedoSensitivityBar = configAlbedoSensitivityBarRect layout
      configAlbedoReferenceMinus = configAlbedoReferenceMinusRect layout
      configAlbedoReferencePlus = configAlbedoReferencePlusRect layout
      configAlbedoReferenceBar = configAlbedoReferenceBarRect layout
      configMoistAdvectMinus = configMoistAdvectMinusRect layout
      configMoistAdvectPlus = configMoistAdvectPlusRect layout
      configMoistAdvectBar = configMoistAdvectBarRect layout
      configMoistLocalMinus = configMoistLocalMinusRect layout
      configMoistLocalPlus = configMoistLocalPlusRect layout
      configMoistLocalBar = configMoistLocalBarRect layout
      configMoistWindEvapScaleMinus = configMoistWindEvapScaleMinusRect layout
      configMoistWindEvapScalePlus = configMoistWindEvapScalePlusRect layout
      configMoistWindEvapScaleBar = configMoistWindEvapScaleBarRect layout
      configMoistEvapNoiseScaleMinus = configMoistEvapNoiseScaleMinusRect layout
      configMoistEvapNoiseScalePlus = configMoistEvapNoiseScalePlusRect layout
      configMoistEvapNoiseScaleBar = configMoistEvapNoiseScaleBarRect layout
      configMoistLandETCoeffMinus = configMoistLandETCoeffMinusRect layout
      configMoistLandETCoeffPlus = configMoistLandETCoeffPlusRect layout
      configMoistLandETCoeffBar = configMoistLandETCoeffBarRect layout
      configMoistBareEvapFracMinus = configMoistBareEvapFracMinusRect layout
      configMoistBareEvapFracPlus = configMoistBareEvapFracPlusRect layout
      configMoistBareEvapFracBar = configMoistBareEvapFracBarRect layout
      configMoistVegTranspFracMinus = configMoistVegTranspFracMinusRect layout
      configMoistVegTranspFracPlus = configMoistVegTranspFracPlusRect layout
      configMoistVegTranspFracBar = configMoistVegTranspFracBarRect layout
      configMoistWindETScaleMinus = configMoistWindETScaleMinusRect layout
      configMoistWindETScalePlus = configMoistWindETScalePlusRect layout
      configMoistWindETScaleBar = configMoistWindETScaleBarRect layout
      configMoistCondensationRateMinus = configMoistCondensationRateMinusRect layout
      configMoistCondensationRatePlus = configMoistCondensationRatePlusRect layout
      configMoistCondensationRateBar = configMoistCondensationRateBarRect layout
      configMoistRecycleRateMinus = configMoistRecycleRateMinusRect layout
      configMoistRecycleRatePlus = configMoistRecycleRatePlusRect layout
      configMoistRecycleRateBar = configMoistRecycleRateBarRect layout
      configMoistITCZStrengthMinus = configMoistITCZStrengthMinusRect layout
      configMoistITCZStrengthPlus = configMoistITCZStrengthPlusRect layout
      configMoistITCZStrengthBar = configMoistITCZStrengthBarRect layout
      configMoistITCZWidthMinus = configMoistITCZWidthMinusRect layout
      configMoistITCZWidthPlus = configMoistITCZWidthPlusRect layout
      configMoistITCZWidthBar = configMoistITCZWidthBarRect layout
      configOrographicScaleMinus = configOrographicScaleMinusRect layout
      configOrographicScalePlus = configOrographicScalePlusRect layout
      configOrographicScaleBar = configOrographicScaleBarRect layout
      configOrographicStepMinus = configOrographicStepMinusRect layout
      configOrographicStepPlus = configOrographicStepPlusRect layout
      configOrographicStepBar = configOrographicStepBarRect layout
      configCoastalIterationsMinus = configCoastalIterationsMinusRect layout
      configCoastalIterationsPlus = configCoastalIterationsPlusRect layout
      configCoastalIterationsBar = configCoastalIterationsBarRect layout
      configCoastalDiffuseMinus = configCoastalDiffuseMinusRect layout
      configCoastalDiffusePlus = configCoastalDiffusePlusRect layout
      configCoastalDiffuseBar = configCoastalDiffuseBarRect layout
      configCoastalMoistureBoostMinus = configCoastalMoistureBoostMinusRect layout
      configCoastalMoistureBoostPlus = configCoastalMoistureBoostPlusRect layout
      configCoastalMoistureBoostBar = configCoastalMoistureBoostBarRect layout
      configWindBeltStrengthMinus = configWindBeltStrengthMinusRect layout
      configWindBeltStrengthPlus = configWindBeltStrengthPlusRect layout
      configWindBeltStrengthBar = configWindBeltStrengthBarRect layout
      configWindBeltHarmonicsMinus = configWindBeltHarmonicsMinusRect layout
      configWindBeltHarmonicsPlus = configWindBeltHarmonicsPlusRect layout
      configWindBeltHarmonicsBar = configWindBeltHarmonicsBarRect layout
      configWindBeltBaseMinus = configWindBeltBaseMinusRect layout
      configWindBeltBasePlus = configWindBeltBasePlusRect layout
      configWindBeltBaseBar = configWindBeltBaseBarRect layout
      configWindBeltRangeMinus = configWindBeltRangeMinusRect layout
      configWindBeltRangePlus = configWindBeltRangePlusRect layout
      configWindBeltRangeBar = configWindBeltRangeBarRect layout
      configWindBeltSpeedScaleMinus = configWindBeltSpeedScaleMinusRect layout
      configWindBeltSpeedScalePlus = configWindBeltSpeedScalePlusRect layout
      configWindBeltSpeedScaleBar = configWindBeltSpeedScaleBarRect layout
      configBndLandRangeMinus = configBndLandRangeMinusRect layout
      configBndLandRangePlus = configBndLandRangePlusRect layout
      configBndLandRangeBar = configBndLandRangeBarRect layout
      configBndTempConvergentMinus = configBndTempConvergentMinusRect layout
      configBndTempConvergentPlus = configBndTempConvergentPlusRect layout
      configBndTempConvergentBar = configBndTempConvergentBarRect layout
      configBndTempDivergentMinus = configBndTempDivergentMinusRect layout
      configBndTempDivergentPlus = configBndTempDivergentPlusRect layout
      configBndTempDivergentBar = configBndTempDivergentBarRect layout
      configBndTempTransformMinus = configBndTempTransformMinusRect layout
      configBndTempTransformPlus = configBndTempTransformPlusRect layout
      configBndTempTransformBar = configBndTempTransformBarRect layout
      configBndPrecipConvergentMinus = configBndPrecipConvergentMinusRect layout
      configBndPrecipConvergentPlus = configBndPrecipConvergentPlusRect layout
      configBndPrecipConvergentBar = configBndPrecipConvergentBarRect layout
      configBndPrecipDivergentMinus = configBndPrecipDivergentMinusRect layout
      configBndPrecipDivergentPlus = configBndPrecipDivergentPlusRect layout
      configBndPrecipDivergentBar = configBndPrecipDivergentBarRect layout
      configBndPrecipTransformMinus = configBndPrecipTransformMinusRect layout
      configBndPrecipTransformPlus = configBndPrecipTransformPlusRect layout
      configBndPrecipTransformBar = configBndPrecipTransformBarRect layout
      configErosionHydraulicMinus = configErosionHydraulicMinusRect layout
      configErosionHydraulicPlus = configErosionHydraulicPlusRect layout
      configErosionHydraulicBar = configErosionHydraulicBarRect layout
      configErosionThermalMinus = configErosionThermalMinusRect layout
      configErosionThermalPlus = configErosionThermalPlusRect layout
      configErosionThermalBar = configErosionThermalBarRect layout
      configErosionRainRateMinus = configErosionRainRateMinusRect layout
      configErosionRainRatePlus = configErosionRainRatePlusRect layout
      configErosionRainRateBar = configErosionRainRateBarRect layout
      configErosionTalusMinus = configErosionTalusMinusRect layout
      configErosionTalusPlus = configErosionTalusPlusRect layout
      configErosionTalusBar = configErosionTalusBarRect layout
      configErosionMaxDropMinus = configErosionMaxDropMinusRect layout
      configErosionMaxDropPlus = configErosionMaxDropPlusRect layout
      configErosionMaxDropBar = configErosionMaxDropBarRect layout
      configGlacierSnowTempMinus = configGlacierSnowTempMinusRect layout
      configGlacierSnowTempPlus = configGlacierSnowTempPlusRect layout
      configGlacierSnowTempBar = configGlacierSnowTempBarRect layout
      configGlacierSnowRangeMinus = configGlacierSnowRangeMinusRect layout
      configGlacierSnowRangePlus = configGlacierSnowRangePlusRect layout
      configGlacierSnowRangeBar = configGlacierSnowRangeBarRect layout
      configGlacierMeltTempMinus = configGlacierMeltTempMinusRect layout
      configGlacierMeltTempPlus = configGlacierMeltTempPlusRect layout
      configGlacierMeltTempBar = configGlacierMeltTempBarRect layout
      configGlacierMeltRateMinus = configGlacierMeltRateMinusRect layout
      configGlacierMeltRatePlus = configGlacierMeltRatePlusRect layout
      configGlacierMeltRateBar = configGlacierMeltRateBarRect layout
      configGlacierAccumScaleMinus = configGlacierAccumScaleMinusRect layout
      configGlacierAccumScalePlus = configGlacierAccumScalePlusRect layout
      configGlacierAccumScaleBar = configGlacierAccumScaleBarRect layout
      configGlacierFlowItersMinus = configGlacierFlowItersMinusRect layout
      configGlacierFlowItersPlus = configGlacierFlowItersPlusRect layout
      configGlacierFlowItersBar = configGlacierFlowItersBarRect layout
      configGlacierFlowRateMinus = configGlacierFlowRateMinusRect layout
      configGlacierFlowRatePlus = configGlacierFlowRatePlusRect layout
      configGlacierFlowRateBar = configGlacierFlowRateBarRect layout
      configGlacierErosionScaleMinus = configGlacierErosionScaleMinusRect layout
      configGlacierErosionScalePlus = configGlacierErosionScalePlusRect layout
      configGlacierErosionScaleBar = configGlacierErosionScaleBarRect layout
      configGlacierCarveScaleMinus = configGlacierCarveScaleMinusRect layout
      configGlacierCarveScalePlus = configGlacierCarveScalePlusRect layout
      configGlacierCarveScaleBar = configGlacierCarveScaleBarRect layout
      configGlacierDepositScaleMinus = configGlacierDepositScaleMinusRect layout
      configGlacierDepositScalePlus = configGlacierDepositScalePlusRect layout
      configGlacierDepositScaleBar = configGlacierDepositScaleBarRect layout
      configVentDensityMinus = configVentDensityMinusRect layout
      configVentDensityPlus = configVentDensityPlusRect layout
      configVentDensityBar = configVentDensityBarRect layout
      configVentThresholdMinus = configVentThresholdMinusRect layout
      configVentThresholdPlus = configVentThresholdPlusRect layout
      configVentThresholdBar = configVentThresholdBarRect layout
      configHotspotScaleMinus = configHotspotScaleMinusRect layout
      configHotspotScalePlus = configHotspotScalePlusRect layout
      configHotspotScaleBar = configHotspotScaleBarRect layout
      configHotspotThresholdMinus = configHotspotThresholdMinusRect layout
      configHotspotThresholdPlus = configHotspotThresholdPlusRect layout
      configHotspotThresholdBar = configHotspotThresholdBarRect layout
      configMagmaRechargeMinus = configMagmaRechargeMinusRect layout
      configMagmaRechargePlus = configMagmaRechargePlusRect layout
      configMagmaRechargeBar = configMagmaRechargeBarRect layout
      configLavaScaleMinus = configLavaScaleMinusRect layout
      configLavaScalePlus = configLavaScalePlusRect layout
      configLavaScaleBar = configLavaScaleBarRect layout
      configAshScaleMinus = configAshScaleMinusRect layout
      configAshScalePlus = configAshScalePlusRect layout
      configAshScaleBar = configAshScaleBarRect layout
      configVolcanicDepositScaleMinus = configVolcanicDepositScaleMinusRect layout
      configVolcanicDepositScalePlus = configVolcanicDepositScalePlusRect layout
      configVolcanicDepositScaleBar = configVolcanicDepositScaleBarRect layout
      configSoilMoistureThresholdMinus = configSoilMoistureThresholdMinusRect layout
      configSoilMoistureThresholdPlus = configSoilMoistureThresholdPlusRect layout
      configSoilMoistureThresholdBar = configSoilMoistureThresholdBarRect layout
      configSoilHardnessThresholdMinus = configSoilHardnessThresholdMinusRect layout
      configSoilHardnessThresholdPlus = configSoilHardnessThresholdPlusRect layout
      configSoilHardnessThresholdBar = configSoilHardnessThresholdBarRect layout
      configSoilFertilityMoistWeightMinus = configSoilFertilityMoistWeightMinusRect layout
      configSoilFertilityMoistWeightPlus = configSoilFertilityMoistWeightPlusRect layout
      configSoilFertilityMoistWeightBar = configSoilFertilityMoistWeightBarRect layout
      configSoilFertilityDepthWeightMinus = configSoilFertilityDepthWeightMinusRect layout
      configSoilFertilityDepthWeightPlus = configSoilFertilityDepthWeightPlusRect layout
      configSoilFertilityDepthWeightBar = configSoilFertilityDepthWeightBarRect layout
      configSinkBreachDepthMinus = configSinkBreachDepthMinusRect layout
      configSinkBreachDepthPlus = configSinkBreachDepthPlusRect layout
      configSinkBreachDepthBar = configSinkBreachDepthBarRect layout
      configStreamPowerMaxErosionMinus = configStreamPowerMaxErosionMinusRect layout
      configStreamPowerMaxErosionPlus = configStreamPowerMaxErosionPlusRect layout
      configStreamPowerMaxErosionBar = configStreamPowerMaxErosionBarRect layout
      configRiverCarveMaxDepthMinus = configRiverCarveMaxDepthMinusRect layout
      configRiverCarveMaxDepthPlus = configRiverCarveMaxDepthPlusRect layout
      configRiverCarveMaxDepthBar = configRiverCarveMaxDepthBarRect layout
      configCoastalErodeStrengthMinus = configCoastalErodeStrengthMinusRect layout
      configCoastalErodeStrengthPlus = configCoastalErodeStrengthPlusRect layout
      configCoastalErodeStrengthBar = configCoastalErodeStrengthBarRect layout
      configHydroHardnessWeightMinus = configHydroHardnessWeightMinusRect layout
      configHydroHardnessWeightPlus = configHydroHardnessWeightPlusRect layout
      configHydroHardnessWeightBar = configHydroHardnessWeightBarRect layout
      configMinLakeSizeMinus = configMinLakeSizeMinusRect layout
      configMinLakeSizePlus = configMinLakeSizePlusRect layout
      configMinLakeSizeBar = configMinLakeSizeBarRect layout
      configInlandSeaMinSizeMinus = configInlandSeaMinSizeMinusRect layout
      configInlandSeaMinSizePlus = configInlandSeaMinSizePlusRect layout
      configInlandSeaMinSizeBar = configInlandSeaMinSizeBarRect layout
      configRoughnessScaleMinus = configRoughnessScaleMinusRect layout
      configRoughnessScalePlus = configRoughnessScalePlusRect layout
      configRoughnessScaleBar = configRoughnessScaleBarRect layout
      configGenScaleMinus = configGenScaleMinusRect layout
      configGenScalePlus = configGenScalePlusRect layout
      configGenScaleBar = configGenScaleBarRect layout
      configGenCoordScaleMinus = configGenCoordScaleMinusRect layout
      configGenCoordScalePlus = configGenCoordScalePlusRect layout
      configGenCoordScaleBar = configGenCoordScaleBarRect layout
      configGenOffsetXMinus = configGenOffsetXMinusRect layout
      configGenOffsetXPlus = configGenOffsetXPlusRect layout
      configGenOffsetXBar = configGenOffsetXBarRect layout
      configGenOffsetYMinus = configGenOffsetYMinusRect layout
      configGenOffsetYPlus = configGenOffsetYPlusRect layout
      configGenOffsetYBar = configGenOffsetYBarRect layout
      configGenFrequencyMinus = configGenFrequencyMinusRect layout
      configGenFrequencyPlus = configGenFrequencyPlusRect layout
      configGenFrequencyBar = configGenFrequencyBarRect layout
      configGenOctavesMinus = configGenOctavesMinusRect layout
      configGenOctavesPlus = configGenOctavesPlusRect layout
      configGenOctavesBar = configGenOctavesBarRect layout
      configGenLacunarityMinus = configGenLacunarityMinusRect layout
      configGenLacunarityPlus = configGenLacunarityPlusRect layout
      configGenLacunarityBar = configGenLacunarityBarRect layout
      configGenGainMinus = configGenGainMinusRect layout
      configGenGainPlus = configGenGainPlusRect layout
      configGenGainBar = configGenGainBarRect layout
      configGenWarpScaleMinus = configGenWarpScaleMinusRect layout
      configGenWarpScalePlus = configGenWarpScalePlusRect layout
      configGenWarpScaleBar = configGenWarpScaleBarRect layout
      configGenWarpStrengthMinus = configGenWarpStrengthMinusRect layout
      configGenWarpStrengthPlus = configGenWarpStrengthPlusRect layout
      configGenWarpStrengthBar = configGenWarpStrengthBarRect layout
      configExtentXMinus = configExtentXMinusRect layout
      configExtentXPlus = configExtentXPlusRect layout
      configExtentXBar = configExtentXBarRect layout
      configExtentYMinus = configExtentYMinusRect layout
      configExtentYPlus = configExtentYPlusRect layout
      configExtentYBar = configExtentYBarRect layout
      configEdgeNorthMinus = configEdgeNorthMinusRect layout
      configEdgeNorthPlus = configEdgeNorthPlusRect layout
      configEdgeNorthBar = configEdgeNorthBarRect layout
      configEdgeSouthMinus = configEdgeSouthMinusRect layout
      configEdgeSouthPlus = configEdgeSouthPlusRect layout
      configEdgeSouthBar = configEdgeSouthBarRect layout
      configEdgeEastMinus = configEdgeEastMinusRect layout
      configEdgeEastPlus = configEdgeEastPlusRect layout
      configEdgeEastBar = configEdgeEastBarRect layout
      configEdgeWestMinus = configEdgeWestMinusRect layout
      configEdgeWestPlus = configEdgeWestPlusRect layout
      configEdgeWestBar = configEdgeWestBarRect layout
      configEdgeFalloffMinus = configEdgeFalloffMinusRect layout
      configEdgeFalloffPlus = configEdgeFalloffPlusRect layout
      configEdgeFalloffBar = configEdgeFalloffBarRect layout
      configPlateSizeMinus = configPlateSizeMinusRect layout
      configPlateSizePlus = configPlateSizePlusRect layout
      configPlateSizeBar = configPlateSizeBarRect layout
      configUpliftMinus = configUpliftMinusRect layout
      configUpliftPlus = configUpliftPlusRect layout
      configUpliftBar = configUpliftBarRect layout
      configRiftDepthMinus = configRiftDepthMinusRect layout
      configRiftDepthPlus = configRiftDepthPlusRect layout
      configRiftDepthBar = configRiftDepthBarRect layout
      configDetailScaleMinus = configDetailScaleMinusRect layout
      configDetailScalePlus = configDetailScalePlusRect layout
      configDetailScaleBar = configDetailScaleBarRect layout
      configPlateSpeedMinus = configPlateSpeedMinusRect layout
      configPlateSpeedPlus = configPlateSpeedPlusRect layout
      configPlateSpeedBar = configPlateSpeedBarRect layout
      configBoundarySharpnessMinus = configBoundarySharpnessMinusRect layout
      configBoundarySharpnessPlus = configBoundarySharpnessPlusRect layout
      configBoundarySharpnessBar = configBoundarySharpnessBarRect layout
      configBoundaryNoiseScaleMinus = configBoundaryNoiseScaleMinusRect layout
      configBoundaryNoiseScalePlus = configBoundaryNoiseScalePlusRect layout
      configBoundaryNoiseScaleBar = configBoundaryNoiseScaleBarRect layout
      configBoundaryNoiseStrengthMinus = configBoundaryNoiseStrengthMinusRect layout
      configBoundaryNoiseStrengthPlus = configBoundaryNoiseStrengthPlusRect layout
      configBoundaryNoiseStrengthBar = configBoundaryNoiseStrengthBarRect layout
      configBoundaryWarpOctavesMinus = configBoundaryWarpOctavesMinusRect layout
      configBoundaryWarpOctavesPlus = configBoundaryWarpOctavesPlusRect layout
      configBoundaryWarpOctavesBar = configBoundaryWarpOctavesBarRect layout
      configBoundaryWarpLacunarityMinus = configBoundaryWarpLacunarityMinusRect layout
      configBoundaryWarpLacunarityPlus = configBoundaryWarpLacunarityPlusRect layout
      configBoundaryWarpLacunarityBar = configBoundaryWarpLacunarityBarRect layout
      configBoundaryWarpGainMinus = configBoundaryWarpGainMinusRect layout
      configBoundaryWarpGainPlus = configBoundaryWarpGainPlusRect layout
      configBoundaryWarpGainBar = configBoundaryWarpGainBarRect layout
      configPlateMergeScaleMinus = configPlateMergeScaleMinusRect layout
      configPlateMergeScalePlus = configPlateMergeScalePlusRect layout
      configPlateMergeScaleBar = configPlateMergeScaleBarRect layout
      configPlateMergeBiasMinus = configPlateMergeBiasMinusRect layout
      configPlateMergeBiasPlus = configPlateMergeBiasPlusRect layout
      configPlateMergeBiasBar = configPlateMergeBiasBarRect layout
      configPlateDetailScaleMinus = configPlateDetailScaleMinusRect layout
      configPlateDetailScalePlus = configPlateDetailScalePlusRect layout
      configPlateDetailScaleBar = configPlateDetailScaleBarRect layout
      configPlateDetailStrengthMinus = configPlateDetailStrengthMinusRect layout
      configPlateDetailStrengthPlus = configPlateDetailStrengthPlusRect layout
      configPlateDetailStrengthBar = configPlateDetailStrengthBarRect layout
      configPlateRidgeStrengthMinus = configPlateRidgeStrengthMinusRect layout
      configPlateRidgeStrengthPlus = configPlateRidgeStrengthPlusRect layout
      configPlateRidgeStrengthBar = configPlateRidgeStrengthBarRect layout
      configPlateHeightBaseMinus = configPlateHeightBaseMinusRect layout
      configPlateHeightBasePlus = configPlateHeightBasePlusRect layout
      configPlateHeightBaseBar = configPlateHeightBaseBarRect layout
      configPlateHeightVarianceMinus = configPlateHeightVarianceMinusRect layout
      configPlateHeightVariancePlus = configPlateHeightVariancePlusRect layout
      configPlateHeightVarianceBar = configPlateHeightVarianceBarRect layout
      configPlateHardnessBaseMinus = configPlateHardnessBaseMinusRect layout
      configPlateHardnessBasePlus = configPlateHardnessBasePlusRect layout
      configPlateHardnessBaseBar = configPlateHardnessBaseBarRect layout
      configPlateHardnessVarianceMinus = configPlateHardnessVarianceMinusRect layout
      configPlateHardnessVariancePlus = configPlateHardnessVariancePlusRect layout
      configPlateHardnessVarianceBar = configPlateHardnessVarianceBarRect layout
      configTrenchDepthMinus = configTrenchDepthMinusRect layout
      configTrenchDepthPlus = configTrenchDepthPlusRect layout
      configTrenchDepthBar = configTrenchDepthBarRect layout
      configRidgeHeightMinus = configRidgeHeightMinusRect layout
      configRidgeHeightPlus = configRidgeHeightPlusRect layout
      configRidgeHeightBar = configRidgeHeightBarRect layout
      configPlateBiasStrengthMinus = configPlateBiasStrengthMinusRect layout
      configPlateBiasStrengthPlus = configPlateBiasStrengthPlusRect layout
      configPlateBiasStrengthBar = configPlateBiasStrengthBarRect layout
      configPlateBiasCenterMinus = configPlateBiasCenterMinusRect layout
      configPlateBiasCenterPlus = configPlateBiasCenterPlusRect layout
      configPlateBiasCenterBar = configPlateBiasCenterBarRect layout
      configPlateBiasEdgeMinus = configPlateBiasEdgeMinusRect layout
      configPlateBiasEdgePlus = configPlateBiasEdgePlusRect layout
      configPlateBiasEdgeBar = configPlateBiasEdgeBarRect layout
      configPlateBiasNorthMinus = configPlateBiasNorthMinusRect layout
      configPlateBiasNorthPlus = configPlateBiasNorthPlusRect layout
      configPlateBiasNorthBar = configPlateBiasNorthBarRect layout
      configPlateBiasSouthMinus = configPlateBiasSouthMinusRect layout
      configPlateBiasSouthPlus = configPlateBiasSouthPlusRect layout
      configPlateBiasSouthBar = configPlateBiasSouthBarRect layout
      configTfcCliffSlopeMinus = configTfcCliffSlopeMinusRect layout
      configTfcCliffSlopePlus = configTfcCliffSlopePlusRect layout
      configTfcCliffSlopeBar = configTfcCliffSlopeBarRect layout
      configTfcMountainSlopeMinus = configTfcMountainSlopeMinusRect layout
      configTfcMountainSlopePlus = configTfcMountainSlopePlusRect layout
      configTfcMountainSlopeBar = configTfcMountainSlopeBarRect layout
      configTfcMountainReliefMinus = configTfcMountainReliefMinusRect layout
      configTfcMountainReliefPlus = configTfcMountainReliefPlusRect layout
      configTfcMountainReliefBar = configTfcMountainReliefBarRect layout
      configTfcHillSlopeMinus = configTfcHillSlopeMinusRect layout
      configTfcHillSlopePlus = configTfcHillSlopePlusRect layout
      configTfcHillSlopeBar = configTfcHillSlopeBarRect layout
      configTfcRollingSlopeMinus = configTfcRollingSlopeMinusRect layout
      configTfcRollingSlopePlus = configTfcRollingSlopePlusRect layout
      configTfcRollingSlopeBar = configTfcRollingSlopeBarRect layout
      configValleyCurvatureMinus = configValleyCurvatureMinusRect layout
      configValleyCurvaturePlus = configValleyCurvaturePlusRect layout
      configValleyCurvatureBar = configValleyCurvatureBarRect layout
      configRockElevationThresholdMinus = configRockElevationThresholdMinusRect layout
      configRockElevationThresholdPlus = configRockElevationThresholdPlusRect layout
      configRockElevationThresholdBar = configRockElevationThresholdBarRect layout
      configRockHardnessThresholdMinus = configRockHardnessThresholdMinusRect layout
      configRockHardnessThresholdPlus = configRockHardnessThresholdPlusRect layout
      configRockHardnessThresholdBar = configRockHardnessThresholdBarRect layout
      configRockHardnessSecondaryMinus = configRockHardnessSecondaryMinusRect layout
      configRockHardnessSecondaryPlus = configRockHardnessSecondaryPlusRect layout
      configRockHardnessSecondaryBar = configRockHardnessSecondaryBarRect layout
      configChunkMinus = configChunkMinusRect layout
      configChunkPlus = configChunkPlusRect layout
      configChunkValue = configChunkValueRect layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12) = leftViewRects layout
      logHeader = logHeaderRect layout
      labelColor = V4 235 235 235 255
      scrollArea = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      rows = case uiConfigTab ui of
        ConfigTerrain -> 54
        ConfigPlanet -> 7
        ConfigClimate -> 49
        ConfigWeather -> 21
        ConfigBiome -> 26
        ConfigErosion -> 35
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _ , V2 _ scrollH) = scrollArea
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
      scrollRect (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
  let configLabel = if uiShowConfig ui then ">>" else "<<"
  drawCentered fontCache labelColor configToggle configLabel
  let leftLabel = if uiShowLeftPanel ui then "<<" else ">>"
  drawCentered fontCache labelColor leftToggle leftLabel
  drawCentered fontCache labelColor logHeader "Logs"
  when (uiShowLeftPanel ui) $ do
      drawCentered fontCache labelColor leftTabTopo "Topo"
      drawCentered fontCache labelColor leftTabView "View"
      case uiLeftTab ui of
        LeftTopo -> do
          drawCentered fontCache labelColor configChunkMinus "-"
          drawCentered fontCache labelColor configChunkPlus "+"
          drawLabelAbove fontCache labelColor configChunkValue "Chunk Size"
          drawCentered fontCache labelColor configChunkValue (Text.pack (show (uiChunkSize ui)))
          drawLeft fontCache labelColor seedLabel "Seed"
          drawCentered fontCache labelColor seedRandom "Random"
          let seedText = if uiSeedEditing ui then uiSeedInput ui else Text.pack (show (uiSeed ui))
          drawCentered fontCache labelColor seedValue seedText
          drawCentered fontCache labelColor buttonRect "Generate"
        LeftView -> do
          drawCentered fontCache labelColor viewRect1 "Elev"
          drawCentered fontCache labelColor viewRect2 "Biome"
          drawCentered fontCache labelColor viewRect3 "Climate"
          drawCentered fontCache labelColor viewRect4 "Moist"
          drawCentered fontCache labelColor viewRect5 "Precip"
          drawCentered fontCache labelColor viewRect6 "Plate"
          drawCentered fontCache labelColor viewRect7 "Bound"
          drawCentered fontCache labelColor viewRect8 "Hard"
          drawCentered fontCache labelColor viewRect9 "Crust"
          drawCentered fontCache labelColor viewRect10 "Age"
          drawCentered fontCache labelColor viewRect11 "PHeight"
          drawCentered fontCache labelColor viewRect12 "PVel"
  when (uiShowConfig ui) $ do
    drawCentered fontCache labelColor tabTerrain "Terrain"
    drawCentered fontCache labelColor tabPlanet "Planet"
    drawCentered fontCache labelColor tabClimate "Climate"
    drawCentered fontCache labelColor tabWeather "Weather"
    drawCentered fontCache labelColor tabBiome "Biome"
    drawCentered fontCache labelColor tabErosion "Erosion"
    drawCentered fontCache labelColor configPresetSave "Save"
    drawCentered fontCache labelColor configPresetLoad "Load"
    drawCentered fontCache labelColor configReset "Reset"
    let revertLabelColor = case uiWorldConfig ui of
          Just _  -> labelColor
          Nothing -> V4 120 120 130 140
    drawCentered fontCache revertLabelColor configRevert "Revert"
    SDL.rendererClipRect renderer SDL.$= Just (rectToSDL scrollArea)
    case uiConfigTab ui of
      ConfigTerrain -> do
        drawCentered fontCache labelColor (scrollRect configGenScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenCoordScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenCoordScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenOffsetXMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenOffsetXPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenOffsetYMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenOffsetYPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenFrequencyMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenFrequencyPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenOctavesMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenOctavesPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenLacunarityMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenLacunarityPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenGainMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenGainPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenWarpScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenWarpScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGenWarpStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGenWarpStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configExtentXMinus) "-"
        drawCentered fontCache labelColor (scrollRect configExtentXPlus) "+"
        drawCentered fontCache labelColor (scrollRect configExtentYMinus) "-"
        drawCentered fontCache labelColor (scrollRect configExtentYPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEdgeNorthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEdgeNorthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEdgeSouthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEdgeSouthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEdgeEastMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEdgeEastPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEdgeWestMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEdgeWestPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEdgeFalloffMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEdgeFalloffPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateSizeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateSizePlus) "+"
        drawCentered fontCache labelColor (scrollRect configUpliftMinus) "-"
        drawCentered fontCache labelColor (scrollRect configUpliftPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRiftDepthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRiftDepthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configDetailScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configDetailScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateSpeedMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateSpeedPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundarySharpnessMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundarySharpnessPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryNoiseStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryNoiseStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpOctavesMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpOctavesPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpLacunarityMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpLacunarityPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpGainMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryWarpGainPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateMergeScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateMergeScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateMergeBiasMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateMergeBiasPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateDetailScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateDetailScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateDetailStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateDetailStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateRidgeStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateRidgeStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateHeightBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateHeightBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateHeightVarianceMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateHeightVariancePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateHardnessBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateHardnessBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateHardnessVarianceMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateHardnessVariancePlus) "+"
        drawCentered fontCache labelColor (scrollRect configTrenchDepthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTrenchDepthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRidgeHeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRidgeHeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateBiasStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateBiasStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateBiasCenterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateBiasCenterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateBiasEdgeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateBiasEdgePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateBiasNorthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateBiasNorthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateBiasSouthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateBiasSouthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configTfcCliffSlopeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTfcCliffSlopePlus) "+"
        drawCentered fontCache labelColor (scrollRect configTfcMountainSlopeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTfcMountainSlopePlus) "+"
        drawCentered fontCache labelColor (scrollRect configTfcMountainReliefMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTfcMountainReliefPlus) "+"
        drawCentered fontCache labelColor (scrollRect configTfcHillSlopeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTfcHillSlopePlus) "+"
        drawCentered fontCache labelColor (scrollRect configTfcRollingSlopeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTfcRollingSlopePlus) "+"
        drawCentered fontCache labelColor (scrollRect configValleyCurvatureMinus) "-"
        drawCentered fontCache labelColor (scrollRect configValleyCurvaturePlus) "+"
        drawCentered fontCache labelColor (scrollRect configRockElevationThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRockElevationThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRockHardnessThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRockHardnessThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRockHardnessSecondaryMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRockHardnessSecondaryPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configGenScaleBar) (sliderLabel specGenScale (uiGenScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenCoordScaleBar) (sliderLabel specGenCoordScale (uiGenCoordScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenOffsetXBar) (sliderLabel specGenOffsetX (uiGenOffsetX ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenOffsetYBar) (sliderLabel specGenOffsetY (uiGenOffsetY ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenFrequencyBar) (sliderLabel specGenFrequency (uiGenFrequency ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenOctavesBar) (sliderLabel specGenOctaves (uiGenOctaves ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenLacunarityBar) (sliderLabel specGenLacunarity (uiGenLacunarity ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenGainBar) (sliderLabel specGenGain (uiGenGain ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenWarpScaleBar) (sliderLabel specGenWarpScale (uiGenWarpScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGenWarpStrengthBar) (sliderLabel specGenWarpStrength (uiGenWarpStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configExtentXBar) (sliderLabel specExtentX (uiWorldExtentX ui))
        drawLabelAbove fontCache labelColor (scrollRect configExtentYBar) (sliderLabel specExtentY (uiWorldExtentY ui))
        drawLabelAbove fontCache labelColor (scrollRect configEdgeNorthBar) (sliderLabel specEdgeNorth (uiEdgeDepthNorth ui))
        drawLabelAbove fontCache labelColor (scrollRect configEdgeSouthBar) (sliderLabel specEdgeSouth (uiEdgeDepthSouth ui))
        drawLabelAbove fontCache labelColor (scrollRect configEdgeEastBar) (sliderLabel specEdgeEast (uiEdgeDepthEast ui))
        drawLabelAbove fontCache labelColor (scrollRect configEdgeWestBar) (sliderLabel specEdgeWest (uiEdgeDepthWest ui))
        drawLabelAbove fontCache labelColor (scrollRect configEdgeFalloffBar) (sliderLabel specEdgeFalloff (uiEdgeDepthFalloff ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateSizeBar) (sliderLabel specPlateSize (uiPlateSize ui))
        drawLabelAbove fontCache labelColor (scrollRect configUpliftBar) (sliderLabel specUplift (uiUplift ui))
        drawLabelAbove fontCache labelColor (scrollRect configRiftDepthBar) (sliderLabel specRiftDepth (uiRiftDepth ui))
        drawLabelAbove fontCache labelColor (scrollRect configDetailScaleBar) (sliderLabel specDetailScale (uiDetailScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateSpeedBar) (sliderLabel specPlateSpeed (uiPlateSpeed ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundarySharpnessBar) (sliderLabel specBoundarySharpness (uiBoundarySharpness ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryNoiseScaleBar) (sliderLabel specBoundaryNoiseScale (uiBoundaryNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryNoiseStrengthBar) (sliderLabel specBoundaryNoiseStrength (uiBoundaryNoiseStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryWarpOctavesBar) (sliderLabel specBoundaryWarpOctaves (uiBoundaryWarpOctaves ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryWarpLacunarityBar) (sliderLabel specBoundaryWarpLacunarity (uiBoundaryWarpLacunarity ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryWarpGainBar) (sliderLabel specBoundaryWarpGain (uiBoundaryWarpGain ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateMergeScaleBar) (sliderLabel specPlateMergeScale (uiPlateMergeScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateMergeBiasBar) (sliderLabel specPlateMergeBias (uiPlateMergeBias ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateDetailScaleBar) (sliderLabel specPlateDetailScale (uiPlateDetailScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateDetailStrengthBar) (sliderLabel specPlateDetailStrength (uiPlateDetailStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateRidgeStrengthBar) (sliderLabel specPlateRidgeStrength (uiPlateRidgeStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateHeightBaseBar) (sliderLabel specPlateHeightBase (uiPlateHeightBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateHeightVarianceBar) (sliderLabel specPlateHeightVariance (uiPlateHeightVariance ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateHardnessBaseBar) (sliderLabel specPlateHardnessBase (uiPlateHardnessBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateHardnessVarianceBar) (sliderLabel specPlateHardnessVariance (uiPlateHardnessVariance ui))
        drawLabelAbove fontCache labelColor (scrollRect configTrenchDepthBar) (sliderLabel specTrenchDepth (uiTrenchDepth ui))
        drawLabelAbove fontCache labelColor (scrollRect configRidgeHeightBar) (sliderLabel specRidgeHeight (uiRidgeHeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateBiasStrengthBar) (sliderLabel specPlateBiasStrength (uiPlateBiasStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateBiasCenterBar) (sliderLabel specPlateBiasCenter (uiPlateBiasCenter ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateBiasEdgeBar) (sliderLabel specPlateBiasEdge (uiPlateBiasEdge ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateBiasNorthBar) (sliderLabel specPlateBiasNorth (uiPlateBiasNorth ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateBiasSouthBar) (sliderLabel specPlateBiasSouth (uiPlateBiasSouth ui))
        drawLabelAbove fontCache labelColor (scrollRect configTfcCliffSlopeBar) (sliderLabel specTfcCliffSlope (uiTfcCliffSlope ui))
        drawLabelAbove fontCache labelColor (scrollRect configTfcMountainSlopeBar) (sliderLabel specTfcMountainSlope (uiTfcMountainSlope ui))
        drawLabelAbove fontCache labelColor (scrollRect configTfcMountainReliefBar) (sliderLabel specTfcMountainRelief (uiTfcMountainRelief ui))
        drawLabelAbove fontCache labelColor (scrollRect configTfcHillSlopeBar) (sliderLabel specTfcHillSlope (uiTfcHillSlope ui))
        drawLabelAbove fontCache labelColor (scrollRect configTfcRollingSlopeBar) (sliderLabel specTfcRollingSlope (uiTfcRollingSlope ui))
        drawLabelAbove fontCache labelColor (scrollRect configValleyCurvatureBar) (sliderLabel specValleyCurvature (uiValleyCurvature ui))
        drawLabelAbove fontCache labelColor (scrollRect configRockElevationThresholdBar) (sliderLabel specRockElevationThreshold (uiRockElevationThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configRockHardnessThresholdBar) (sliderLabel specRockHardnessThreshold (uiRockHardnessThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configRockHardnessSecondaryBar) (sliderLabel specRockHardnessSecondary (uiRockHardnessSecondary ui))
      ConfigClimate -> do
        drawCentered fontCache labelColor (scrollRect configWaterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWaterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configEvapMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEvapPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRainShadowMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRainShadowPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindDiffuseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindDiffusePlus) "+"
        drawCentered fontCache labelColor (scrollRect configEquatorTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configEquatorTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPoleTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPoleTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configLapseRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configLapseRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindIterationsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindIterationsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistureIterationsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistureIterationsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionPrecipMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionPrecipPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLatCenterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLatCenterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLonCenterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLonCenterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configLatitudeExponentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configLatitudeExponentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlateHeightCoolingMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlateHeightCoolingPlus) "+"
        drawCentered fontCache labelColor (scrollRect configTempNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configTempNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configOceanModerationMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOceanModerationPlus) "+"
        drawCentered fontCache labelColor (scrollRect configOceanModerateTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOceanModerateTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configAlbedoSensitivityMinus) "-"
        drawCentered fontCache labelColor (scrollRect configAlbedoSensitivityPlus) "+"
        drawCentered fontCache labelColor (scrollRect configAlbedoReferenceMinus) "-"
        drawCentered fontCache labelColor (scrollRect configAlbedoReferencePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistAdvectMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistAdvectPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistLocalMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistLocalPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistWindEvapScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistWindEvapScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistEvapNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistEvapNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistLandETCoeffMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistLandETCoeffPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistBareEvapFracMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistBareEvapFracPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistVegTranspFracMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistVegTranspFracPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistWindETScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistWindETScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistCondensationRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistCondensationRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistRecycleRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistRecycleRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistITCZStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistITCZStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistITCZWidthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistITCZWidthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configOrographicScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOrographicScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configOrographicStepMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOrographicStepPlus) "+"
        drawCentered fontCache labelColor (scrollRect configCoastalIterationsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCoastalIterationsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configCoastalDiffuseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCoastalDiffusePlus) "+"
        drawCentered fontCache labelColor (scrollRect configCoastalMoistureBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCoastalMoistureBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindBeltStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindBeltStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindBeltHarmonicsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindBeltHarmonicsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindBeltBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindBeltBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindBeltRangeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindBeltRangePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindBeltSpeedScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindBeltSpeedScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndLandRangeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndLandRangePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndTempConvergentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndTempConvergentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndTempDivergentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndTempDivergentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndTempTransformMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndTempTransformPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndPrecipConvergentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndPrecipConvergentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndPrecipDivergentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndPrecipDivergentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBndPrecipTransformMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBndPrecipTransformPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configWaterBar) (sliderLabel specWaterLevel (uiWaterLevel ui))
        drawLabelAbove fontCache labelColor (scrollRect configEvapBar) (sliderLabel specEvaporation (uiEvaporation ui))
        drawLabelAbove fontCache labelColor (scrollRect configRainShadowBar) (sliderLabel specRainShadow (uiRainShadow ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindDiffuseBar) (sliderLabel specWindDiffuse (uiWindDiffuse ui))
        drawLabelAbove fontCache labelColor (scrollRect configEquatorTempBar) (sliderLabel specEquatorTemp (uiEquatorTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configPoleTempBar) (sliderLabel specPoleTemp (uiPoleTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configLapseRateBar) (sliderLabel specLapseRate (uiLapseRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindIterationsBar) (sliderLabel specWindIterations (uiWindIterations ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistureIterationsBar) (sliderLabel specMoistureIterations (uiMoistureIterations ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryMotionTempBar) (sliderLabel specBoundaryMotionTemp (uiBoundaryMotionTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryMotionPrecipBar) (sliderLabel specBoundaryMotionPrecip (uiBoundaryMotionPrecip ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLatCenterBar) (sliderLabel specSliceLatCenter (uiSliceLatCenter ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLonCenterBar) (sliderLabel specSliceLonCenter (uiSliceLonCenter ui))
        drawLabelAbove fontCache labelColor (scrollRect configLatitudeExponentBar) (sliderLabel specLatitudeExponent (uiLatitudeExponent ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlateHeightCoolingBar) (sliderLabel specPlateHeightCooling (uiPlateHeightCooling ui))
        drawLabelAbove fontCache labelColor (scrollRect configTempNoiseScaleBar) (sliderLabel specTempNoiseScale (uiTempNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configOceanModerationBar) (sliderLabel specOceanModeration (uiOceanModeration ui))
        drawLabelAbove fontCache labelColor (scrollRect configOceanModerateTempBar) (sliderLabel specOceanModerateTemp (uiOceanModerateTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configAlbedoSensitivityBar) (sliderLabel specAlbedoSensitivity (uiAlbedoSensitivity ui))
        drawLabelAbove fontCache labelColor (scrollRect configAlbedoReferenceBar) (sliderLabel specAlbedoReference (uiAlbedoReference ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistAdvectBar) (sliderLabel specMoistAdvect (uiMoistAdvect ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistLocalBar) (sliderLabel specMoistLocal (uiMoistLocal ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistWindEvapScaleBar) (sliderLabel specMoistWindEvapScale (uiMoistWindEvapScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistEvapNoiseScaleBar) (sliderLabel specMoistEvapNoiseScale (uiMoistEvapNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistLandETCoeffBar) (sliderLabel specMoistLandETCoeff (uiMoistLandETCoeff ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistBareEvapFracBar) (sliderLabel specMoistBareEvapFrac (uiMoistBareEvapFrac ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistVegTranspFracBar) (sliderLabel specMoistVegTranspFrac (uiMoistVegTranspFrac ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistWindETScaleBar) (sliderLabel specMoistWindETScale (uiMoistWindETScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistCondensationRateBar) (sliderLabel specMoistCondensationRate (uiMoistCondensationRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistRecycleRateBar) (sliderLabel specMoistRecycleRate (uiMoistRecycleRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistITCZStrengthBar) (sliderLabel specMoistITCZStrength (uiMoistITCZStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistITCZWidthBar) (sliderLabel specMoistITCZWidth (uiMoistITCZWidth ui))
        drawLabelAbove fontCache labelColor (scrollRect configOrographicScaleBar) (sliderLabel specOrographicScale (uiOrographicScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configOrographicStepBar) (sliderLabel specOrographicStep (uiOrographicStep ui))
        drawLabelAbove fontCache labelColor (scrollRect configCoastalIterationsBar) (sliderLabel specCoastalIterations (uiCoastalIterations ui))
        drawLabelAbove fontCache labelColor (scrollRect configCoastalDiffuseBar) (sliderLabel specCoastalDiffuse (uiCoastalDiffuse ui))
        drawLabelAbove fontCache labelColor (scrollRect configCoastalMoistureBoostBar) (sliderLabel specCoastalMoistureBoost (uiCoastalMoistureBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindBeltStrengthBar) (sliderLabel specWindBeltStrength (uiWindBeltStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindBeltHarmonicsBar) (sliderLabel specWindBeltHarmonics (uiWindBeltHarmonics ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindBeltBaseBar) (sliderLabel specWindBeltBase (uiWindBeltBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindBeltRangeBar) (sliderLabel specWindBeltRange (uiWindBeltRange ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindBeltSpeedScaleBar) (sliderLabel specWindBeltSpeedScale (uiWindBeltSpeedScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndLandRangeBar) (sliderLabel specBndLandRange (uiBndLandRange ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndTempConvergentBar) (sliderLabel specBndTempConvergent (uiBndTempConvergent ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndTempDivergentBar) (sliderLabel specBndTempDivergent (uiBndTempDivergent ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndTempTransformBar) (sliderLabel specBndTempTransform (uiBndTempTransform ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndPrecipConvergentBar) (sliderLabel specBndPrecipConvergent (uiBndPrecipConvergent ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndPrecipDivergentBar) (sliderLabel specBndPrecipDivergent (uiBndPrecipDivergent ui))
        drawLabelAbove fontCache labelColor (scrollRect configBndPrecipTransformBar) (sliderLabel specBndPrecipTransform (uiBndPrecipTransform ui))
      ConfigPlanet -> do
        drawCentered fontCache labelColor (scrollRect configPlanetRadiusMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlanetRadiusPlus) "+"
        drawCentered fontCache labelColor (scrollRect configAxialTiltMinus) "-"
        drawCentered fontCache labelColor (scrollRect configAxialTiltPlus) "+"
        drawCentered fontCache labelColor (scrollRect configInsolationMinus) "-"
        drawCentered fontCache labelColor (scrollRect configInsolationPlus) "+"
        drawCentered fontCache labelColor (scrollRect configOccWarmScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOccWarmScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configOccColdScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOccColdScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configOccLatPeakDegMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOccLatPeakDegPlus) "+"
        drawCentered fontCache labelColor (scrollRect configOccLatWidthDegMinus) "-"
        drawCentered fontCache labelColor (scrollRect configOccLatWidthDegPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configPlanetRadiusBar) (sliderLabel specPlanetRadius (uiPlanetRadius ui))
        drawLabelAbove fontCache labelColor (scrollRect configAxialTiltBar) (sliderLabel specAxialTilt (uiAxialTilt ui))
        drawLabelAbove fontCache labelColor (scrollRect configInsolationBar) (sliderLabel specInsolation (uiInsolation ui))
        drawLabelAbove fontCache labelColor (scrollRect configOccWarmScaleBar) (sliderLabel specOccWarmScale (uiOccWarmScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configOccColdScaleBar) (sliderLabel specOccColdScale (uiOccColdScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configOccLatPeakDegBar) (sliderLabel specOccLatPeakDeg (uiOccLatPeakDeg ui))
        drawLabelAbove fontCache labelColor (scrollRect configOccLatWidthDegBar) (sliderLabel specOccLatWidthDeg (uiOccLatWidthDeg ui))
      ConfigWeather -> do
        drawCentered fontCache labelColor (scrollRect configWeatherTickMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherTickPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherPhaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherPhasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherAmplitudeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherAmplitudePlus) "+"
        drawCentered fontCache labelColor (scrollRect configSeasonCycleLengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSeasonCycleLengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configJitterAmplitudeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configJitterAmplitudePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPressureBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPressureBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPressureTempScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPressureTempScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPressureCoriolisScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPressureCoriolisScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configSeasonalBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSeasonalBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configSeasonalRangeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSeasonalRangePlus) "+"
        drawCentered fontCache labelColor (scrollRect configHumidityNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configHumidityNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPrecipNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPrecipNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherITCZWidthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherITCZWidthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherITCZPrecipBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherITCZPrecipBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPressureHumidityScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPressureHumidityScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configPressureGradientWindScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPressureGradientWindScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindNoiseScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindNoiseScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configITCZMigrationScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configITCZMigrationScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configCloudRHExponentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCloudRHExponentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configCloudAlbedoEffectMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCloudAlbedoEffectPlus) "+"
        drawCentered fontCache labelColor (scrollRect configCloudPrecipBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCloudPrecipBoostPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configWeatherTickBar) (sliderLabel specWeatherTick (uiWeatherTick ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherPhaseBar) (sliderLabel specWeatherPhase (uiWeatherPhase ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherAmplitudeBar) (sliderLabel specWeatherAmplitude (uiWeatherAmplitude ui))
        drawLabelAbove fontCache labelColor (scrollRect configSeasonCycleLengthBar) (sliderLabel specSeasonCycleLength (uiSeasonCycleLength ui))
        drawLabelAbove fontCache labelColor (scrollRect configJitterAmplitudeBar) (sliderLabel specJitterAmplitude (uiJitterAmplitude ui))
        drawLabelAbove fontCache labelColor (scrollRect configPressureBaseBar) (sliderLabel specPressureBase (uiPressureBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configPressureTempScaleBar) (sliderLabel specPressureTempScale (uiPressureTempScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPressureCoriolisScaleBar) (sliderLabel specPressureCoriolisScale (uiPressureCoriolisScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configSeasonalBaseBar) (sliderLabel specSeasonalBase (uiSeasonalBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configSeasonalRangeBar) (sliderLabel specSeasonalRange (uiSeasonalRange ui))
        drawLabelAbove fontCache labelColor (scrollRect configHumidityNoiseScaleBar) (sliderLabel specHumidityNoiseScale (uiHumidityNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPrecipNoiseScaleBar) (sliderLabel specPrecipNoiseScale (uiPrecipNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherITCZWidthBar) (sliderLabel specWeatherITCZWidth (uiWeatherITCZWidth ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherITCZPrecipBoostBar) (sliderLabel specWeatherITCZPrecipBoost (uiWeatherITCZPrecipBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configPressureHumidityScaleBar) (sliderLabel specPressureHumidityScale (uiPressureHumidityScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configPressureGradientWindScaleBar) (sliderLabel specPressureGradientWindScale (uiPressureGradientWindScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindNoiseScaleBar) (sliderLabel specWindNoiseScale (uiWindNoiseScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configITCZMigrationScaleBar) (sliderLabel specITCZMigrationScale (uiITCZMigrationScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configCloudRHExponentBar) (sliderLabel specCloudRHExponent (uiCloudRHExponent ui))
        drawLabelAbove fontCache labelColor (scrollRect configCloudAlbedoEffectBar) (sliderLabel specCloudAlbedoEffect (uiCloudAlbedoEffect ui))
        drawLabelAbove fontCache labelColor (scrollRect configCloudPrecipBoostBar) (sliderLabel specCloudPrecipBoost (uiCloudPrecipBoost ui))
      ConfigBiome -> do
        drawCentered fontCache labelColor (scrollRect configVegBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegTempWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegTempWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegPrecipWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegPrecipWeightPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configVegBaseBar) (sliderLabel specVegBase (uiVegBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegBoostBar) (sliderLabel specVegBoost (uiVegBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegTempWeightBar) (sliderLabel specVegTempWeight (uiVegTempWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegPrecipWeightBar) (sliderLabel specVegPrecipWeight (uiVegPrecipWeight ui))
        drawCentered fontCache labelColor (scrollRect configBtCoastalBandMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtCoastalBandPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtSnowMaxTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtSnowMaxTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtAlpineMaxTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtAlpineMaxTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtIceCapTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtIceCapTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtMontaneMaxTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtMontaneMaxTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtMontanePrecipMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtMontanePrecipPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtCliffSlopeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtCliffSlopePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtValleyMoistureMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtValleyMoisturePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtDepressionMoistureMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtDepressionMoisturePlus) "+"
        drawCentered fontCache labelColor (scrollRect configBtPrecipWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBtPrecipWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcTempMinMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcTempMinPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcTempRangeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcTempRangePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcFertilityBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcFertilityBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoBareMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoBarePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoVegMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcAlbedoVegPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcOceanAlbedoMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcOceanAlbedoPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVbcIceAlbedoMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVbcIceAlbedoPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBiomeSmoothingMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBiomeSmoothingPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVolcanicAshBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVolcanicAshBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVolcanicLavaPenaltyMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVolcanicLavaPenaltyPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBiomeFeedbackBlendMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBiomeFeedbackBlendPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configBtCoastalBandBar) (sliderLabel specBtCoastalBand (uiBtCoastalBand ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtSnowMaxTempBar) (sliderLabel specBtSnowMaxTemp (uiBtSnowMaxTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtAlpineMaxTempBar) (sliderLabel specBtAlpineMaxTemp (uiBtAlpineMaxTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtIceCapTempBar) (sliderLabel specBtIceCapTemp (uiBtIceCapTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtMontaneMaxTempBar) (sliderLabel specBtMontaneMaxTemp (uiBtMontaneMaxTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtMontanePrecipBar) (sliderLabel specBtMontanePrecip (uiBtMontanePrecip ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtCliffSlopeBar) (sliderLabel specBtCliffSlope (uiBtCliffSlope ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtValleyMoistureBar) (sliderLabel specBtValleyMoisture (uiBtValleyMoisture ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtDepressionMoistureBar) (sliderLabel specBtDepressionMoisture (uiBtDepressionMoisture ui))
        drawLabelAbove fontCache labelColor (scrollRect configBtPrecipWeightBar) (sliderLabel specBtPrecipWeight (uiBtPrecipWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcTempMinBar) (sliderLabel specVbcTempMin (uiVbcTempMin ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcTempRangeBar) (sliderLabel specVbcTempRange (uiVbcTempRange ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcFertilityBoostBar) (sliderLabel specVbcFertilityBoost (uiVbcFertilityBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcAlbedoBaseBar) (sliderLabel specVbcAlbedoBase (uiVbcAlbedoBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcAlbedoBareBar) (sliderLabel specVbcAlbedoBare (uiVbcAlbedoBare ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcAlbedoVegBar) (sliderLabel specVbcAlbedoVeg (uiVbcAlbedoVeg ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcOceanAlbedoBar) (sliderLabel specVbcOceanAlbedo (uiVbcOceanAlbedo ui))
        drawLabelAbove fontCache labelColor (scrollRect configVbcIceAlbedoBar) (sliderLabel specVbcIceAlbedo (uiVbcIceAlbedo ui))
        drawLabelAbove fontCache labelColor (scrollRect configBiomeSmoothingBar) (sliderLabel specBiomeSmoothing (uiBiomeSmoothing ui))
        drawLabelAbove fontCache labelColor (scrollRect configVolcanicAshBoostBar) (sliderLabel specVolcanicAshBoost (uiVolcanicAshBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configVolcanicLavaPenaltyBar) (sliderLabel specVolcanicLavaPenalty (uiVolcanicLavaPenalty ui))
        drawLabelAbove fontCache labelColor (scrollRect configBiomeFeedbackBlendBar) (sliderLabel specBiomeFeedbackBlend (uiBiomeFeedbackBlend ui))
      ConfigErosion -> do
        drawCentered fontCache labelColor (scrollRect configErosionHydraulicMinus) "-"
        drawCentered fontCache labelColor (scrollRect configErosionHydraulicPlus) "+"
        drawCentered fontCache labelColor (scrollRect configErosionThermalMinus) "-"
        drawCentered fontCache labelColor (scrollRect configErosionThermalPlus) "+"
        drawCentered fontCache labelColor (scrollRect configErosionRainRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configErosionRainRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configErosionTalusMinus) "-"
        drawCentered fontCache labelColor (scrollRect configErosionTalusPlus) "+"
        drawCentered fontCache labelColor (scrollRect configErosionMaxDropMinus) "-"
        drawCentered fontCache labelColor (scrollRect configErosionMaxDropPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierSnowTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierSnowTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierSnowRangeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierSnowRangePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierMeltTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierMeltTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierMeltRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierMeltRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierAccumScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierAccumScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierFlowItersMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierFlowItersPlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierFlowRateMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierFlowRatePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierErosionScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierErosionScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierCarveScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierCarveScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configGlacierDepositScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configGlacierDepositScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVentDensityMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVentDensityPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVentThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVentThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configHotspotScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configHotspotScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configHotspotThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configHotspotThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMagmaRechargeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMagmaRechargePlus) "+"
        drawCentered fontCache labelColor (scrollRect configLavaScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configLavaScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configAshScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configAshScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVolcanicDepositScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVolcanicDepositScalePlus) "+"
        drawCentered fontCache labelColor (scrollRect configSoilMoistureThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSoilMoistureThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSoilHardnessThresholdMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSoilHardnessThresholdPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSoilFertilityMoistWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSoilFertilityMoistWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSoilFertilityDepthWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSoilFertilityDepthWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSinkBreachDepthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSinkBreachDepthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configStreamPowerMaxErosionMinus) "-"
        drawCentered fontCache labelColor (scrollRect configStreamPowerMaxErosionPlus) "+"
        drawCentered fontCache labelColor (scrollRect configRiverCarveMaxDepthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRiverCarveMaxDepthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configCoastalErodeStrengthMinus) "-"
        drawCentered fontCache labelColor (scrollRect configCoastalErodeStrengthPlus) "+"
        drawCentered fontCache labelColor (scrollRect configHydroHardnessWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configHydroHardnessWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMinLakeSizeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMinLakeSizePlus) "+"
        drawCentered fontCache labelColor (scrollRect configInlandSeaMinSizeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configInlandSeaMinSizePlus) "+"
        drawCentered fontCache labelColor (scrollRect configRoughnessScaleMinus) "-"
        drawCentered fontCache labelColor (scrollRect configRoughnessScalePlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configErosionHydraulicBar) (sliderLabel specErosionHydraulic (uiErosionHydraulic ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionThermalBar) (sliderLabel specErosionThermal (uiErosionThermal ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionRainRateBar) (sliderLabel specErosionRainRate (uiRainRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionTalusBar) (sliderLabel specErosionTalus (uiErosionTalus ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionMaxDropBar) (sliderLabel specErosionMaxDrop (uiErosionMaxDrop ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierSnowTempBar) (sliderLabel specGlacierSnowTemp (uiGlacierSnowTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierSnowRangeBar) (sliderLabel specGlacierSnowRange (uiGlacierSnowRange ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierMeltTempBar) (sliderLabel specGlacierMeltTemp (uiGlacierMeltTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierMeltRateBar) (sliderLabel specGlacierMeltRate (uiGlacierMeltRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierAccumScaleBar) (sliderLabel specGlacierAccumScale (uiGlacierAccumScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierFlowItersBar) (sliderLabel specGlacierFlowIters (uiGlacierFlowIters ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierFlowRateBar) (sliderLabel specGlacierFlowRate (uiGlacierFlowRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierErosionScaleBar) (sliderLabel specGlacierErosionScale (uiGlacierErosionScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierCarveScaleBar) (sliderLabel specGlacierCarveScale (uiGlacierCarveScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configGlacierDepositScaleBar) (sliderLabel specGlacierDepositScale (uiGlacierDepositScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configVentDensityBar) (sliderLabel specVentDensity (uiVentDensity ui))
        drawLabelAbove fontCache labelColor (scrollRect configVentThresholdBar) (sliderLabel specVentThreshold (uiVentThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configHotspotScaleBar) (sliderLabel specHotspotScale (uiHotspotScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configHotspotThresholdBar) (sliderLabel specHotspotThreshold (uiHotspotThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configMagmaRechargeBar) (sliderLabel specMagmaRecharge (uiMagmaRecharge ui))
        drawLabelAbove fontCache labelColor (scrollRect configLavaScaleBar) (sliderLabel specLavaScale (uiLavaScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configAshScaleBar) (sliderLabel specAshScale (uiAshScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configVolcanicDepositScaleBar) (sliderLabel specVolcanicDepositScale (uiVolcanicDepositScale ui))
        drawLabelAbove fontCache labelColor (scrollRect configSoilMoistureThresholdBar) (sliderLabel specSoilMoistureThreshold (uiSoilMoistureThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configSoilHardnessThresholdBar) (sliderLabel specSoilHardnessThreshold (uiSoilHardnessThreshold ui))
        drawLabelAbove fontCache labelColor (scrollRect configSoilFertilityMoistWeightBar) (sliderLabel specSoilFertilityMoistWeight (uiSoilFertilityMoistWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configSoilFertilityDepthWeightBar) (sliderLabel specSoilFertilityDepthWeight (uiSoilFertilityDepthWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configSinkBreachDepthBar) (sliderLabel specSinkBreachDepth (uiSinkBreachDepth ui))
        drawLabelAbove fontCache labelColor (scrollRect configStreamPowerMaxErosionBar) (sliderLabel specStreamPowerMaxErosion (uiStreamPowerMaxErosion ui))
        drawLabelAbove fontCache labelColor (scrollRect configRiverCarveMaxDepthBar) (sliderLabel specRiverCarveMaxDepth (uiRiverCarveMaxDepth ui))
        drawLabelAbove fontCache labelColor (scrollRect configCoastalErodeStrengthBar) (sliderLabel specCoastalErodeStrength (uiCoastalErodeStrength ui))
        drawLabelAbove fontCache labelColor (scrollRect configHydroHardnessWeightBar) (sliderLabel specHydroHardnessWeight (uiHydroHardnessWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configMinLakeSizeBar) (sliderLabel specMinLakeSize (uiMinLakeSize ui))
        drawLabelAbove fontCache labelColor (scrollRect configInlandSeaMinSizeBar) (sliderLabel specInlandSeaMinSize (uiInlandSeaMinSize ui))
        drawLabelAbove fontCache labelColor (scrollRect configRoughnessScaleBar) (sliderLabel specRoughnessScale (uiRoughnessScale ui))
    SDL.rendererClipRect renderer SDL.$= Nothing

drawStatusBars :: SDL.Renderer -> Maybe FontCache -> UiState -> DataSnapshot -> Layout -> IO ()
drawStatusBars renderer fontCache ui dataSnap layout =
  when (uiShowLeftPanel ui && uiLeftTab ui == LeftTopo) $ do
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

drawHoverHex :: SDL.Renderer -> UiState -> Int -> IO ()
drawHoverHex renderer uiSnap supersample =
  case uiHoverHex uiSnap of
    Nothing -> pure ()
    Just (q, r) -> do
      let hexSize = 6
          spans = hexSpans hexSize
          (cx, cy) = axialToScreen hexSize q r
          (ox, oy) = uiPanOffset uiSnap
          z = uiZoom uiSnap
          (minX, minY, maxX, maxY) = spanBounds spans
          texW = max 1 ((maxX - minX + 1) * supersample)
          texH = max 1 ((maxY - minY + 1) * supersample)
          worldX = cx + minX
          worldY = cy + minY
      hoverTexture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral texW) (fromIntegral texH))
      SDL.textureBlendMode hoverTexture SDL.$= SDL.BlendAlphaBlend
      SDL.rendererRenderTarget renderer SDL.$= Just hoverTexture
      SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
      SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
      SDL.clear renderer
      SDL.rendererDrawColor renderer SDL.$= V4 220 220 220 160
      drawHexSpansSupersampled renderer spans supersample (minX, minY)
      SDL.rendererRenderTarget renderer SDL.$= Nothing
      let Rect (V2 tx ty, V2 tw th) = transformRect (ox, oy) z (Rect (V2 worldX worldY, V2 (maxX - minX + 1) (maxY - minY + 1)))
      SDL.copy renderer hoverTexture Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))
      SDL.destroyTexture hoverTexture

transformRect :: (Float, Float) -> Float -> Rect -> Rect
transformRect (ox, oy) z (Rect (V2 rx ry, V2 rw rh)) =
  let fx = (fromIntegral rx + ox) * z
      fy = (fromIntegral ry + oy) * z
      fw = fromIntegral rw * z
      fh = fromIntegral rh * z
  in Rect (V2 (round fx) (round fy), V2 (max 1 (round fw)) (max 1 (round fh)))

drawHexSpansSupersampled :: SDL.Renderer -> [(Int, Int, Int)] -> Int -> (Int, Int) -> IO ()
drawHexSpansSupersampled renderer spans scale (minX, minY) =
  mapM_ (drawSpanSupersampled renderer scale minX minY) spans

drawSpanSupersampled :: SDL.Renderer -> Int -> Int -> Int -> (Int, Int, Int) -> IO ()
drawSpanSupersampled renderer scale minX minY (dy, x0, x1) = do
  let y = (dy - minY) * scale
      x = (x0 - minX) * scale
      w = max 1 ((x1 - x0) * scale)
      h = max 1 scale
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))

spanBounds :: [(Int, Int, Int)] -> (Int, Int, Int, Int)
spanBounds spans =
  let ys = map (\(y, _, _) -> y) spans
      xs0 = map (\(_, x0, _) -> x0) spans
      xs1 = map (\(_, _, x1) -> x1) spans
      minX = minimum xs0
      maxX = maximum xs1
      minY = minimum ys
      maxY = maximum ys
  in (minX, minY, maxX, maxY)


hexSpans :: Int -> [(Int, Int, Int)]
hexSpans size =
  let corners = hexCorners size
      ys = [floor (minimum (map snd corners)) .. ceiling (maximum (map snd corners))]
  in mapMaybe (spanForY corners) ys

hexCorners :: Int -> [(Float, Float)]
hexCorners size =
  let s = fromIntegral size
      angles = [-30, 30, 90, 150, 210, 270]
  in [ (s * cos (degToRad a), s * sin (degToRad a)) | a <- angles ]

spanForY :: [(Float, Float)] -> Int -> Maybe (Int, Int, Int)
spanForY corners y =
  let yF = fromIntegral y + 0.5
      edges = case corners of
        [] -> []
        (c:cs) -> zip (c:cs) (cs ++ [c])
      xs = mapMaybe (edgeIntersect yF) edges
  in case sort xs of
       (x0:x1:_) ->
         let ix0 = floor x0
             ix1 = ceiling x1
         in Just (y, ix0, ix1)
       _ -> Nothing

edgeIntersect :: Float -> ((Float, Float), (Float, Float)) -> Maybe Float
edgeIntersect y ((x1, y1), (x2, y2))
  | y1 == y2 = Nothing
  | (y >= min y1 y2) && (y < max y1 y2) =
      let t = (y - y1) / (y2 - y1)
      in Just (x1 + t * (x2 - x1))
  | otherwise = Nothing

degToRad :: Float -> Float
degToRad deg = deg * pi / 180

drawHexSpans :: SDL.Renderer -> V2 Int -> [(Int, Int, Int)] -> IO ()
drawHexSpans renderer (V2 cx cy) spans =
  mapM_ (drawSpan renderer cx cy) spans

drawSpan :: SDL.Renderer -> Int -> Int -> (Int, Int, Int) -> IO ()
drawSpan renderer cx cy (dy, x0, x1) = do
  let y = cy + dy
      x = cx + x0
      w = max 0 (x1 - x0)
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) 1)))

drawHexContext :: SDL.Renderer -> Maybe FontCache -> UiState -> TerrainSnapshot -> V2 Int -> IO ()
drawHexContext renderer fontCache ui terrainSnap (V2 winW winH) =
  case (uiContextHex ui, uiContextPos ui) of
    (Just (q, r), Just (sx, sy)) -> do
      let lns = contextLines (uiViewMode ui) terrainSnap (q, r)
          lineH  = 16
          hPad   = 12
          vPad   = 10
      panelW <- case fontCache of
        Just fc -> do
          widths <- mapM (\t -> do { V2 w _ <- textSize fc (V4 230 230 235 255) t; pure w }) lns
          pure (maximum (220 : map (+ hPad * 2) widths))
        Nothing ->
          let approx = maximum (220 : map (\t -> Text.length t * 8 + hPad * 2) lns)
          in pure approx
      let panelH = max 40 (vPad * 2 + lineH * length lns)
          px = clamp 8 (winW - panelW - 8) (sx + 12)
          py = clamp 8 (winH - panelH - 8) (sy + 12)
      SDL.rendererDrawColor renderer SDL.$= V4 20 20 25 235
      SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral px) (fromIntegral py))) (V2 (fromIntegral panelW) (fromIntegral panelH))))
      case fontCache of
        Nothing -> pure ()
        Just _cache ->
          sequence_ [ drawTextLine fontCache (V2 (px + hPad) (py + vPad + idx * lineH)) (V4 230 230 235 255) line
                    | (idx, line) <- zip [0..] lns
                    ]
    _ -> pure ()
  where
    clamp lo hi v = max lo (min hi v)

-- | Produce the info-panel lines for a hex, tailored to the active view mode.
--
-- Every mode starts with a formatted coordinate header. The remaining
-- lines show the fields most relevant to the current visualisation.
contextLines :: ViewMode -> TerrainSnapshot -> (Int, Int) -> [Text]
contextLines mode terrainSnap (q, r) =
  case sampleAt terrainSnap (q, r) of
    Nothing -> [hexHeader q r, "No data"]
    Just s  -> hexHeader q r : modeLines mode s
  where
    modeLines ViewElevation    s = ["Elev  " <> fmtF (hsElevation s), "Slope " <> fmtF (hsSlope s)]
    modeLines ViewBiome        s = ["Biome " <> biomeDisplayName (hsBiome s), "Veg   " <> fmtF (hsFertility s)]
    modeLines ViewClimate      s = ["Temp  " <> fmtF (hsTemp s), "Precip " <> fmtF (hsPrecipAvg s)]
    modeLines ViewMoisture     s = ["Moist " <> fmtF (hsMoisture s), "Soil  " <> fmtF (hsSoilDepth s)]
    modeLines ViewPrecip       s = ["Precip " <> fmtF (hsPrecipAvg s), "Humid " <> fmtF (hsHumidity s)]
    modeLines ViewPlateId      s = ["Plate " <> Text.pack (show (hsPlateId s))]
    modeLines ViewPlateBoundary s = ["Boundary " <> plateBoundaryDisplayName (hsPlateBoundary s)]
    modeLines ViewPlateHardness s = ["Plate Hardness " <> fmtF (hsPlateHardness s)]
    modeLines ViewPlateCrust   s = ["Crust " <> crustDisplayName (hsPlateCrust s)]
    modeLines ViewPlateAge     s = ["Plate Age " <> fmtF (hsPlateAge s)]
    modeLines ViewPlateHeight  s = ["Plate Height " <> fmtF (hsPlateHeight s)]
    modeLines ViewPlateVelocity s =
      [ "Vel X " <> fmtF (hsPlateVelX s)
      , "Vel Y " <> fmtF (hsPlateVelY s)
      , "Speed " <> fmtF (sqrt (hsPlateVelX s ** 2 + hsPlateVelY s ** 2))
      ]

    fmtF = Text.pack . formatF

data HexSample = HexSample
  { hsChunk          :: !ChunkCoord
  , hsLocal          :: !TileCoord
  , hsElevation      :: !Float
  , hsSlope          :: !Float
  , hsMoisture       :: !Float
  , hsSoilDepth      :: !Float
  , hsFertility      :: !Float
  , hsBiome          :: !BiomeId
  , hsTemp           :: !Float
  , hsPrecipAvg      :: !Float
  , hsHumidity       :: !Float
  , hsPlateId        :: !Word16
  , hsPlateBoundary  :: !PlateBoundary
  , hsPlateHardness  :: !Float
  , hsPlateCrust     :: !Word16
  , hsPlateAge       :: !Float
  , hsPlateHeight    :: !Float
  , hsPlateVelX      :: !Float
  , hsPlateVelY      :: !Float
  }

sampleAt :: TerrainSnapshot -> (Int, Int) -> Maybe HexSample
sampleAt terrainSnap (q, r)
  | tsChunkSize terrainSnap <= 0 = Nothing
  | otherwise = do
      let config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
          tile = TileCoord q r
          (chunkCoord, local) = chunkCoordFromTile config tile
          ChunkId key = chunkIdFromCoord chunkCoord
      TileIndex idx <- tileIndex config local
      tc <- IntMap.lookup key (tsTerrainChunks terrainSnap)
      let climate = IntMap.lookup key (tsClimateChunks terrainSnap)
          weather = IntMap.lookup key (tsWeatherChunks terrainSnap)
      Just HexSample
        { hsChunk          = chunkCoord
        , hsLocal          = local
        , hsElevation      = tcElevation tc U.! idx
        , hsSlope          = tcSlope tc U.! idx
        , hsMoisture       = tcMoisture tc U.! idx
        , hsSoilDepth      = tcSoilDepth tc U.! idx
        , hsFertility      = tcFertility tc U.! idx
        , hsBiome          = tcFlags tc U.! idx
        , hsTemp           = maybe 0 (\c -> ccTempAvg c U.! idx) climate
        , hsPrecipAvg      = maybe 0 (\c -> ccPrecipAvg c U.! idx) climate
        , hsHumidity       = maybe 0 (\w -> wcHumidity w U.! idx) weather
        , hsPlateId        = tcPlateId tc U.! idx
        , hsPlateBoundary  = tcPlateBoundary tc U.! idx
        , hsPlateHardness  = tcPlateHardness tc U.! idx
        , hsPlateCrust     = tcPlateCrust tc U.! idx
        , hsPlateAge       = tcPlateAge tc U.! idx
        , hsPlateHeight    = tcPlateHeight tc U.! idx
        , hsPlateVelX      = tcPlateVelX tc U.! idx
        , hsPlateVelY      = tcPlateVelY tc U.! idx
        }

formatF :: Float -> String
formatF v =
  let scaled = fromIntegral (round (v * 100) :: Int) / 100 :: Double
  in show scaled

-- | Format a hex coordinate header line.
hexHeader :: Int -> Int -> Text
hexHeader q r = "Hex (" <> Text.pack (show q) <> ", " <> Text.pack (show r) <> ")"

-- | Human-readable plate boundary name.
plateBoundaryDisplayName :: PlateBoundary -> Text
plateBoundaryDisplayName b =
  case plateBoundaryToCode b of
    0 -> "None"
    1 -> "Convergent"
    2 -> "Divergent"
    3 -> "Transform"
    n -> "Unknown (" <> Text.pack (show n) <> ")"

-- | Human-readable crust type name.
crustDisplayName :: Word16 -> Text
crustDisplayName 0 = "Oceanic"
crustDisplayName 1 = "Continental"
crustDisplayName n = "Unknown (" <> Text.pack (show n) <> ")"

-- | Draw a tooltip near the given screen position.
--
-- Renders a filled rectangle with the tooltip text, clamped so it
-- stays within @(winW, winH)@ bounds.
drawTooltip :: SDL.Renderer -> Maybe FontCache -> V2 Int -> V2 Int -> Text -> IO ()
drawTooltip renderer fontCache (V2 winW winH) (V2 mx my) tipText = do
  let tipPad = 6
      tipOffsetY = 20
  let tipColor = V4 220 220 230 255 :: V4 Word8
  V2 tw th <- case fontCache of
    Just fc -> textSize fc tipColor tipText
    Nothing -> pure (V2 (Text.length tipText * 8) 16)
  let boxW = tw + tipPad * 2
      boxH = th + tipPad * 2
      -- Position below cursor, clamped to window
      rawX = mx + 8
      rawY = my + tipOffsetY
      x = max 0 (min (winW - boxW) rawX)
      y = max 0 (min (winH - boxH) rawY)
      bgRect = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y)))
                              (V2 (fromIntegral boxW) (fromIntegral boxH))
  SDL.rendererDrawColor renderer SDL.$= V4 20 20 30 240
  SDL.fillRect renderer (Just bgRect)
  SDL.rendererDrawColor renderer SDL.$= V4 80 80 100 255
  SDL.drawRect renderer (Just bgRect)
  drawTextLine fontCache (V2 (x + tipPad) (y + tipPad)) tipColor tipText
