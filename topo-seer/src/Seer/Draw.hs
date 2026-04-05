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
  , drawDataDetailPopover
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
import Seer.Config (mapRange)
import UI.Theme
import Seer.Draw.LeftPanel (drawChunkControl, drawLeftTabs, drawOverlayButtons, drawSeedControl, drawStatusBars, drawViewModeButtons, modeColor)
import Seer.Draw.Overlay (drawHexContext, drawHoverHex, drawTooltip)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Calendar (CalendarDate(..), WorldTime(..), mkCalendarConfig, tickToDate)
import Topo.Planet (PlanetConfig(..))
import UI.Font (FontCache, textSize)
import UI.Layout
import UI.Widgets (Rect(..))
import Seer.Draw.Dialog (drawDialogButton, drawDialogPanel, drawDialogTitle, drawListSelection, drawTextInputField)
import Seer.Draw.Config (drawConfigPanel, drawConfigTabs, drawDataDetailPopover)
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
    LogDebug -> colLogDebugLineBg
    LogInfo  -> colLogInfoLineBg
    LogWarn  -> colLogWarnLineBg
    LogError -> colLogErrorLineBg

logTextColor :: LogLevel -> V4 Word8
logTextColor level =
  case level of
    LogDebug -> colLogDebugText
    LogInfo  -> colLogInfoText
    LogWarn  -> colLogWarnText
    LogError -> colLogErrorText

logLevelColor :: LogLevel -> Bool -> V4 Word8
logLevelColor level isActive =
  let boost = if isActive then 60 else 0 :: Int
      boostW8 :: Word8 -> Word8
      boostW8 x = fromIntegral (min 255 (fromIntegral x + boost))
      applyBoost (V4 r g b a) = V4 (boostW8 r) (boostW8 g) (boostW8 b) a
  in case level of
       LogDebug -> applyBoost colLogDebugFilter
       LogInfo  -> applyBoost colLogInfoFilter
       LogWarn  -> applyBoost colLogWarnFilter
       LogError -> applyBoost colLogErrorFilter

logLineHeight :: Maybe FontCache -> IO Int
logLineHeight Nothing = pure 12
logLineHeight (Just cache) = do
  V2 _ th <- textSize cache textWhite "Ag"
  pure (max 12 (fromIntegral th + 4))

logTextYOffset :: Maybe FontCache -> Int -> IO Int
logTextYOffset Nothing _ = pure 1
logTextYOffset (Just cache) lineHeight = do
  V2 _ th <- textSize cache textWhite "Ag"
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
  drawCentered fontCache textLogFilterLabel rect label

drawLogLines :: SDL.Renderer -> Maybe FontCache -> LogSnapshot -> Rect -> IO ()
drawLogLines renderer fontCache logSnap (Rect (V2 x y, V2 w h)) = do
  lineHeight <- logLineHeight fontCache
  textYOffset <- logTextYOffset fontCache lineHeight
  let padding = 2
      gutterW = 12
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
      barW = 8
      barH = max 0 (h - padding * 2)
      handleH = max 10 (barH * visibleLines `div` max 1 totalLines)
      handleY =
        if maxOffset == 0
          then barY
          else barY + (barH - handleH) * offset `div` maxOffset
  SDL.rendererDrawColor renderer SDL.$= colScrollbarTrack
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX barY, V2 barW barH))))
  SDL.rendererDrawColor renderer SDL.$= colScrollbarHandle
  SDL.fillRect renderer (Just (rectToSDL (Rect (V2 barX handleY, V2 barW handleH))))

drawEscapeMenu :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawEscapeMenu renderer fontCache ui layout =
  case uiMenuMode ui of
    MenuEscape -> do
      let panel = menuPanelRect layout
          saveRect = menuSaveRect layout
          loadRect = menuLoadRect layout
          exitRect = menuExitRect layout
      SDL.rendererDrawColor renderer SDL.$= colEscapeMenuBg
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
      let dialog   = presetLoadDialogRect layout
          filterR  = presetLoadFilterRect layout
          listR    = presetLoadListRect layout
          okR      = presetLoadOkRect layout
          cancelR  = presetLoadCancelRect layout
          fText    = Text.toLower (uiPresetFilter ui)
          items    = filter (\n -> Text.isInfixOf fText (Text.toLower n))
                            (uiPresetList ui)
          sel      = min (uiPresetSelected ui) (max 0 (length items - 1))
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Load Preset"
      drawTextInputField renderer fontCache filterR (uiPresetFilter ui)
      drawListSelection renderer fontCache listR 24 9 sel
        (presetLoadItemRect layout) (\_ name -> name) items
      let hasItems = not (null items)
      drawDialogButton renderer fontCache okR "Load" hasItems
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
      let dialog   = worldLoadDialogRect layout
          filterR  = worldLoadFilterRect layout
          listR    = worldLoadListRect layout
          okR      = worldLoadOkRect layout
          cancelR  = worldLoadCancelRect layout
          fText    = Text.toLower (uiWorldFilter ui)
          items    = filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m)))
                            (uiWorldList ui)
          sel      = min (uiWorldSelected ui) (max 0 (length items - 1))
      drawDialogPanel renderer dialog
      drawDialogTitle renderer fontCache dialog "Load World"
      drawTextInputField renderer fontCache filterR (uiWorldFilter ui)
      drawListSelection renderer fontCache listR 28 9 sel
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

-- | Draw the top bar showing the current world name and calendar date/time.
drawTopBar :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawTopBar renderer fontCache ui layout = do
  let bar = topBarRect layout
      Rect (V2 bx by, V2 bw bh) = bar
      textRect = Rect (V2 (bx + 16) by, V2 (bw - 32) bh)
  SDL.rendererDrawColor renderer SDL.$= colTopBar
  SDL.fillRect renderer (Just (rectToSDL bar))
  drawLeft fontCache textTopBar textRect (uiWorldName ui)
  -- Render world date/time right-aligned in the top bar
  let planet = PlanetConfig
        { pcRadius = mapRange 4778 9557 (uiPlanetRadius ui)
        , pcAxialTilt = mapRange 0 45 (uiAxialTilt ui)
        , pcInsolation = mapRange 0.7 1.3 (uiInsolation ui)
        }
      calCfg = mkCalendarConfig planet
      worldTime = WorldTime
        { wtTick     = uiSimTickCount ui
        , wtTickRate = realToFrac (uiSimTickRate ui)
        }
      calDate = tickToDate calCfg worldTime
      yr   = cdYear calDate
      doy  = cdDayOfYear calDate + 1  -- 1-based for display
      hour = cdHourOfDay calDate
      hrs  = floor hour :: Int
      mins = round ((hour - fromIntegral hrs) * 60) :: Int
      timeTxt = "Year " <> Text.pack (show yr)
             <> ", Day " <> Text.pack (show doy)
             <> "  " <> Text.pack (show hrs) <> ":" <> (if mins < 10 then "0" else "") <> Text.pack (show mins)
  case fontCache of
    Nothing -> pure ()
    Just cache -> do
      V2 tw th <- textSize cache textTopBar timeTxt
      let tx = bx + bw - 16 - tw
          ty = by + (bh - th) `div` 2
      drawTextLine fontCache (V2 tx ty) textTopBar timeTxt

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
      viewRects = leftViewRects layout
      logHeader = logHeaderRect layout
      labelColor = textPrimary
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
          let viewLabels = ["Elevation", "Biome", "Climate", "Weather", "Moisture", "Precip", "Vegetation", "Terr. Form", "Plate ID", "Boundary", "Hardness", "Crust", "Age", "Plt. Height", "Plt. Vel."]
              scrollY = uiLeftViewScroll ui
              shiftY (Rect (V2 rx ry, V2 rw rh)) = Rect (V2 rx (ry - scrollY), V2 rw rh)
              scrolledViewRects = map shiftY viewRects
              Rect (V2 lpx _, V2 lpw _) = leftPanelRect layout
              Rect (V2 _ lpy, V2 _ lpH) = leftPanelRect layout
              ctop = leftControlsTop layout
              clipR = Rect (V2 lpx ctop, V2 lpw (lpy + lpH - ctop))
          SDL.rendererClipRect renderer SDL.$= Just (rectToSDL clipR)
          mapM_ (\(rect, label) -> drawCentered fontCache labelColor rect label) (zip scrolledViewRects viewLabels)
          SDL.rendererClipRect renderer SDL.$= Nothing
  drawConfigLabels renderer fontCache ui layout

