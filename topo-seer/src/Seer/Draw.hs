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
  , drawOverlayButtons
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
  , drawConfigLabels
  , drawUiLabels
  ) where

import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..))
import Actor.UI (LeftTab(..), UiMenuMode(..), UiState(..), ViewMode(..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Draw.LeftPanel (drawChunkControl, drawLeftTabs, drawOverlayButtons, drawSeedControl, drawStatusBars, drawViewModeButtons, modeColor)
import Seer.Draw.Overlay (drawHexContext, drawHoverHex, drawTooltip)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import UI.Font (FontCache, textSize)
import UI.Layout
import UI.Widgets (Rect(..))
import Seer.Draw.Dialog (drawDialogButton, drawDialogPanel, drawDialogTitle, drawListSelection, drawTextInputField)
import Seer.Draw.Config (drawConfigPanel, drawConfigTabs)
import Seer.Draw.Config.Labels (drawConfigLabels)
import UI.WidgetsDraw (drawCentered, drawLabelAbove, drawLeft, drawTextLine, rectToSDL)

seedMaxDigits :: Int
seedMaxDigits = 20

viewColor :: ViewMode -> Int -> Int -> (Word8, Word8, Word8)
viewColor mode terrainCount biomeCount =
  case mode of
    ViewElevation -> (40, 80 + scale terrainCount, 160)
    ViewBiome -> (120, 70 + scale biomeCount, 60)
    ViewClimate -> (70, 120, 160 + scale biomeCount)
    ViewWeather -> (180, 90, 90 + scale biomeCount)
    ViewMoisture -> (60, 140 + scale biomeCount, 120)
    ViewPrecip -> (60, 110 + scale biomeCount, 150)
    ViewPlateId -> (100, 120 + scale biomeCount, 160)
    ViewPlateBoundary -> (140, 110 + scale biomeCount, 90)
    ViewPlateHardness -> (120, 100 + scale biomeCount, 70)
    ViewPlateCrust -> (110, 100 + scale biomeCount, 90)
    ViewPlateAge -> (110, 90 + scale biomeCount, 130)
    ViewPlateHeight -> (90, 120 + scale biomeCount, 150)
    ViewPlateVelocity -> (90, 110 + scale biomeCount, 180)
    ViewVegetation -> (70, 150 + scale biomeCount, 50)
    ViewTerrainForm -> (150, 130 + scale terrainCount, 90)
    ViewOverlay _ _ -> (180, 120 + scale biomeCount, 200)
  where
    scale n = fromIntegral (min 75 (n * 5))

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
      visible = reverse (take visibleLines (drop startIndex entries))
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

drawLogScrollbar :: SDL.Renderer -> Maybe FontCache -> LogSnapshot -> Rect -> IO ()
drawLogScrollbar renderer fontCache logSnap (Rect (V2 x y, V2 w h)) = do
  lineHeight <- logLineHeight fontCache
  let padding = 2
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

drawBarFill :: SDL.Renderer -> Float -> Rect -> V4 Word8 -> IO ()
drawBarFill renderer value (Rect (V2 x y, V2 w h)) color = do
  let fillW = max 0 (min w (round (fromIntegral w * value)))
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 x y, V2 fillW h))))

drawUiLabels :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawUiLabels renderer fontCache ui layout = do
  let buttonRect = leftGenButtonRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      configToggle = configToggleRect layout
      configChunkMinus = configChunkMinusRect layout
      configChunkPlus = configChunkPlusRect layout
      configChunkValue = configChunkValueRect layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12) = leftViewRects layout
      logHeader = logHeaderRect layout
      labelColor = V4 235 235 235 255
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
          drawCentered fontCache labelColor viewRect5 "Weather"
          drawCentered fontCache labelColor viewRect6 "Plate"
          drawCentered fontCache labelColor viewRect7 "Bound"
          drawCentered fontCache labelColor viewRect8 "Hard"
          drawCentered fontCache labelColor viewRect9 "Crust"
          drawCentered fontCache labelColor viewRect10 "Age"
          drawCentered fontCache labelColor viewRect11 "PHeight"
          drawCentered fontCache labelColor viewRect12 "PVel"
  drawConfigLabels renderer fontCache ui layout

