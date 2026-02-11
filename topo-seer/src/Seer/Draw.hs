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
import Actor.UI (ConfigTab(..), LeftTab(..), UiState(..), ViewMode(..))
import Control.Monad (when)
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config (mapIntRange)
import Seer.Config.SliderSpec
import Topo (BiomeId, ChunkCoord(..), ChunkId(..), ClimateChunk(..), TerrainChunk(..), TileCoord(..), TileIndex(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord, tileIndex)
import UI.Font (FontCache, textSize)
import UI.HexPick (axialToScreen)
import UI.Layout
import UI.Widgets (Rect(..))
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

drawConfigTabs :: SDL.Renderer -> UiState -> (Rect, Rect, Rect) -> IO ()
drawConfigTabs renderer ui (tabTerrain, tabClimate, tabErosion) = do
  drawTab tabTerrain (uiConfigTab ui == ConfigTerrain)
  drawTab tabClimate (uiConfigTab ui == ConfigClimate)
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
  if uiShowMenu ui
    then do
      let panel = menuPanelRect layout
          saveRect = menuSaveRect layout
          loadRect = menuLoadRect layout
          exitRect = menuExitRect layout
      SDL.rendererDrawColor renderer SDL.$= V4 20 25 35 230
      SDL.fillRect renderer (Just (rectToSDL panel))
      drawMenuButton renderer fontCache saveRect "Save" False
      drawMenuButton renderer fontCache loadRect "Load" False
      drawMenuButton renderer fontCache exitRect "Exit" True
  else pure ()

drawMenuButton :: SDL.Renderer -> Maybe FontCache -> Rect -> Text -> Bool -> IO ()
drawMenuButton renderer fontCache rect label isEnabled = do
  let fill = if isEnabled then V4 70 90 120 255 else V4 55 55 65 255
      textColor = if isEnabled then V4 230 230 235 255 else V4 140 140 150 255
  SDL.rendererDrawColor renderer SDL.$= fill
  SDL.fillRect renderer (Just (rectToSDL rect))
  drawCentered fontCache textColor rect label

drawConfigPanel
  :: SDL.Renderer
  -> UiState
  -> Rect
  -> (Rect, Rect, Rect)
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
  -> IO ()
drawConfigPanel renderer ui rect (tabTerrain, tabClimate, tabErosion) applyRect replayRect resetRect scrollAreaRect scrollBarRect
  (waterMinus, waterBar, waterPlus)
  (evapMinus, evapBar, evapPlus)
  (rainShadowMinus, rainShadowBar, rainShadowPlus)
  (windDiffuseMinus, windDiffuseBar, windDiffusePlus)
  (equatorTempMinus, equatorTempBar, equatorTempPlus)
  (poleTempMinus, poleTempBar, poleTempPlus)
  (lapseRateMinus, lapseRateBar, lapseRatePlus)
  (latitudeBiasMinus, latitudeBiasBar, latitudeBiasPlus)
  (windIterationsMinus, windIterationsBar, windIterationsPlus)
  (moistureIterationsMinus, moistureIterationsBar, moistureIterationsPlus)
  (weatherTickMinus, weatherTickBar, weatherTickPlus)
  (weatherPhaseMinus, weatherPhaseBar, weatherPhasePlus)
  (weatherAmplitudeMinus, weatherAmplitudeBar, weatherAmplitudePlus)
  (vegBaseMinus, vegBaseBar, vegBasePlus)
  (vegBoostMinus, vegBoostBar, vegBoostPlus)
  (vegTempWeightMinus, vegTempWeightBar, vegTempWeightPlus)
  (vegPrecipWeightMinus, vegPrecipWeightBar, vegPrecipWeightPlus)
  (boundaryMotionTempMinus, boundaryMotionTempBar, boundaryMotionTempPlus)
  (boundaryMotionPrecipMinus, boundaryMotionPrecipBar, boundaryMotionPrecipPlus)
  (planetRadiusMinus, planetRadiusBar, planetRadiusPlus)
  (axialTiltMinus, axialTiltBar, axialTiltPlus)
  (insolationMinus, insolationBar, insolationPlus)
  (sliceLatCenterMinus, sliceLatCenterBar, sliceLatCenterPlus)
  (sliceLatExtentMinus, sliceLatExtentBar, sliceLatExtentPlus)
  (sliceLonCenterMinus, sliceLonCenterBar, sliceLonCenterPlus)
  (sliceLonExtentMinus, sliceLonExtentBar, sliceLonExtentPlus)
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
  (erosionHydraulicMinus, erosionHydraulicBar, erosionHydraulicPlus)
  (erosionThermalMinus, erosionThermalBar, erosionThermalPlus)
  (erosionRainRateMinus, erosionRainRateBar, erosionRainRatePlus)
  (erosionTalusMinus, erosionTalusBar, erosionTalusPlus)
  (erosionMaxDropMinus, erosionMaxDropBar, erosionMaxDropPlus) =
  if uiShowConfig ui
    then do
      SDL.rendererDrawColor renderer SDL.$= V4 35 45 60 230
      SDL.fillRect renderer (Just (rectToSDL rect))
      drawConfigTabs renderer ui (tabTerrain, tabClimate, tabErosion)
      SDL.rendererDrawColor renderer SDL.$= V4 30 38 52 230
      SDL.fillRect renderer (Just (rectToSDL scrollAreaRect))
      SDL.rendererDrawColor renderer SDL.$= V4 60 70 90 255
      SDL.drawRect renderer (Just (rectToSDL scrollAreaRect))
      let rowHeight = 24
          gap = 10
          rows = case uiConfigTab ui of
            ConfigTerrain -> 44
            ConfigClimate -> 26
            ConfigErosion -> 5
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
        ConfigClimate -> do
          drawConfigSlider renderer (uiWaterLevel ui) (scrollRect waterMinus) (scrollRect waterBar) (scrollRect waterPlus) (V4 70 120 180 255)
          drawConfigSlider renderer (uiEvaporation ui) (scrollRect evapMinus) (scrollRect evapBar) (scrollRect evapPlus) (V4 140 110 80 255)
          drawConfigSlider renderer (uiRainShadow ui) (scrollRect rainShadowMinus) (scrollRect rainShadowBar) (scrollRect rainShadowPlus) (V4 110 140 190 255)
          drawConfigSlider renderer (uiWindDiffuse ui) (scrollRect windDiffuseMinus) (scrollRect windDiffuseBar) (scrollRect windDiffusePlus) (V4 90 140 120 255)
          drawConfigSlider renderer (uiEquatorTemp ui) (scrollRect equatorTempMinus) (scrollRect equatorTempBar) (scrollRect equatorTempPlus) (V4 180 120 90 255)
          drawConfigSlider renderer (uiPoleTemp ui) (scrollRect poleTempMinus) (scrollRect poleTempBar) (scrollRect poleTempPlus) (V4 90 150 200 255)
          drawConfigSlider renderer (uiLapseRate ui) (scrollRect lapseRateMinus) (scrollRect lapseRateBar) (scrollRect lapseRatePlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiLatitudeBias ui) (scrollRect latitudeBiasMinus) (scrollRect latitudeBiasBar) (scrollRect latitudeBiasPlus) (V4 140 120 180 255)
          drawConfigSlider renderer (uiWindIterations ui) (scrollRect windIterationsMinus) (scrollRect windIterationsBar) (scrollRect windIterationsPlus) (V4 120 140 200 255)
          drawConfigSlider renderer (uiMoistureIterations ui) (scrollRect moistureIterationsMinus) (scrollRect moistureIterationsBar) (scrollRect moistureIterationsPlus) (V4 120 150 160 255)
          drawConfigSlider renderer (uiWeatherTick ui) (scrollRect weatherTickMinus) (scrollRect weatherTickBar) (scrollRect weatherTickPlus) (V4 140 120 140 255)
          drawConfigSlider renderer (uiWeatherPhase ui) (scrollRect weatherPhaseMinus) (scrollRect weatherPhaseBar) (scrollRect weatherPhasePlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiWeatherAmplitude ui) (scrollRect weatherAmplitudeMinus) (scrollRect weatherAmplitudeBar) (scrollRect weatherAmplitudePlus) (V4 140 120 90 255)
          drawConfigSlider renderer (uiVegBase ui) (scrollRect vegBaseMinus) (scrollRect vegBaseBar) (scrollRect vegBasePlus) (V4 100 130 120 255)
          drawConfigSlider renderer (uiVegBoost ui) (scrollRect vegBoostMinus) (scrollRect vegBoostBar) (scrollRect vegBoostPlus) (V4 120 150 120 255)
          drawConfigSlider renderer (uiVegTempWeight ui) (scrollRect vegTempWeightMinus) (scrollRect vegTempWeightBar) (scrollRect vegTempWeightPlus) (V4 130 140 170 255)
          drawConfigSlider renderer (uiVegPrecipWeight ui) (scrollRect vegPrecipWeightMinus) (scrollRect vegPrecipWeightBar) (scrollRect vegPrecipWeightPlus) (V4 120 140 190 255)
          drawConfigSlider renderer (uiBoundaryMotionTemp ui) (scrollRect boundaryMotionTempMinus) (scrollRect boundaryMotionTempBar) (scrollRect boundaryMotionTempPlus) (V4 110 130 160 255)
          drawConfigSlider renderer (uiBoundaryMotionPrecip ui) (scrollRect boundaryMotionPrecipMinus) (scrollRect boundaryMotionPrecipBar) (scrollRect boundaryMotionPrecipPlus) (V4 120 140 170 255)
          drawConfigSlider renderer (uiPlanetRadius ui) (scrollRect planetRadiusMinus) (scrollRect planetRadiusBar) (scrollRect planetRadiusPlus) (V4 150 130 100 255)
          drawConfigSlider renderer (uiAxialTilt ui) (scrollRect axialTiltMinus) (scrollRect axialTiltBar) (scrollRect axialTiltPlus) (V4 140 140 100 255)
          drawConfigSlider renderer (uiInsolation ui) (scrollRect insolationMinus) (scrollRect insolationBar) (scrollRect insolationPlus) (V4 180 150 80 255)
          drawConfigSlider renderer (uiSliceLatCenter ui) (scrollRect sliceLatCenterMinus) (scrollRect sliceLatCenterBar) (scrollRect sliceLatCenterPlus) (V4 110 140 130 255)
          drawConfigSlider renderer (uiSliceLatExtent ui) (scrollRect sliceLatExtentMinus) (scrollRect sliceLatExtentBar) (scrollRect sliceLatExtentPlus) (V4 120 140 130 255)
          drawConfigSlider renderer (uiSliceLonCenter ui) (scrollRect sliceLonCenterMinus) (scrollRect sliceLonCenterBar) (scrollRect sliceLonCenterPlus) (V4 110 130 150 255)
          drawConfigSlider renderer (uiSliceLonExtent ui) (scrollRect sliceLonExtentMinus) (scrollRect sliceLonExtentBar) (scrollRect sliceLonExtentPlus) (V4 120 130 150 255)
        ConfigErosion -> do
          drawConfigSlider renderer (uiErosionHydraulic ui) (scrollRect erosionHydraulicMinus) (scrollRect erosionHydraulicBar) (scrollRect erosionHydraulicPlus) (V4 90 140 180 255)
          drawConfigSlider renderer (uiErosionThermal ui) (scrollRect erosionThermalMinus) (scrollRect erosionThermalBar) (scrollRect erosionThermalPlus) (V4 120 120 160 255)
          drawConfigSlider renderer (uiRainRate ui) (scrollRect erosionRainRateMinus) (scrollRect erosionRainRateBar) (scrollRect erosionRainRatePlus) (V4 100 110 170 255)
          drawConfigSlider renderer (uiErosionTalus ui) (scrollRect erosionTalusMinus) (scrollRect erosionTalusBar) (scrollRect erosionTalusPlus) (V4 160 120 90 255)
          drawConfigSlider renderer (uiErosionMaxDrop ui) (scrollRect erosionMaxDropMinus) (scrollRect erosionMaxDropBar) (scrollRect erosionMaxDropPlus) (V4 140 120 120 255)
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
      SDL.fillRect renderer (Just (rectToSDL applyRect))
      SDL.rendererDrawColor renderer SDL.$= V4 80 110 160 255
      SDL.fillRect renderer (Just (rectToSDL replayRect))
      SDL.rendererDrawColor renderer SDL.$= V4 120 80 80 255
      SDL.fillRect renderer (Just (rectToSDL resetRect))
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
  let buttonRect = genButtonRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      configToggle = configToggleRect layout
      configApply = configApplyRect layout
      configReplay = configReplayRect layout
      configReset = configResetRect layout
      (tabTerrain, tabClimate, tabErosion) = configTabRects layout
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
      configLatitudeBiasMinus = configLatitudeBiasMinusRect layout
      configLatitudeBiasPlus = configLatitudeBiasPlusRect layout
      configLatitudeBiasBar = configLatitudeBiasBarRect layout
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
      configSliceLatCenterMinus = configSliceLatCenterMinusRect layout
      configSliceLatCenterPlus = configSliceLatCenterPlusRect layout
      configSliceLatCenterBar = configSliceLatCenterBarRect layout
      configSliceLatExtentMinus = configSliceLatExtentMinusRect layout
      configSliceLatExtentPlus = configSliceLatExtentPlusRect layout
      configSliceLatExtentBar = configSliceLatExtentBarRect layout
      configSliceLonCenterMinus = configSliceLonCenterMinusRect layout
      configSliceLonCenterPlus = configSliceLonCenterPlusRect layout
      configSliceLonCenterBar = configSliceLonCenterBarRect layout
      configSliceLonExtentMinus = configSliceLonExtentMinusRect layout
      configSliceLonExtentPlus = configSliceLonExtentPlusRect layout
      configSliceLonExtentBar = configSliceLonExtentBarRect layout
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
      configChunkMinus = configChunkMinusRect layout
      configChunkPlus = configChunkPlusRect layout
      configChunkValue = configChunkValueRect layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12) = viewRects layout
      logHeader = logHeaderRect layout
      labelColor = V4 235 235 235 255
      scrollArea = configScrollAreaRect layout
      rowHeight = 24
      gap = 10
      rows = case uiConfigTab ui of
        ConfigTerrain -> 45
        ConfigClimate -> 26
        ConfigErosion -> 5
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ _ , V2 _ scrollH) = scrollArea
      maxOffset = max 0 (contentHeight - scrollH)
      scrollY = min maxOffset (uiConfigScroll ui)
      scrollRect (Rect (V2 x y, V2 w h)) = Rect (V2 x (y - scrollY), V2 w h)
  drawCentered fontCache labelColor buttonRect "Generate"
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
          drawLabelLeft fontCache labelColor configChunkMinus "Chunk Size"
          drawLabelAbove fontCache labelColor configChunkValue "Chunk Size"
          drawCentered fontCache labelColor configChunkValue (Text.pack (show (uiChunkSize ui)))
          drawLeft fontCache labelColor seedLabel "Seed"
          drawCentered fontCache labelColor seedRandom "Random"
          let seedText = if uiSeedEditing ui then uiSeedInput ui else Text.pack (show (uiSeed ui))
          drawCentered fontCache labelColor seedValue seedText
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
    drawCentered fontCache labelColor tabClimate "Climate"
    drawCentered fontCache labelColor tabErosion "Erosion"
    drawCentered fontCache labelColor configApply "Apply"
    drawCentered fontCache labelColor configReplay "Replay"
    drawCentered fontCache labelColor configReset "Reset"
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
        drawCentered fontCache labelColor (scrollRect configLatitudeBiasMinus) "-"
        drawCentered fontCache labelColor (scrollRect configLatitudeBiasPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWindIterationsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWindIterationsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configMoistureIterationsMinus) "-"
        drawCentered fontCache labelColor (scrollRect configMoistureIterationsPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherTickMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherTickPlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherPhaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherPhasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configWeatherAmplitudeMinus) "-"
        drawCentered fontCache labelColor (scrollRect configWeatherAmplitudePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegBaseMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegBasePlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegBoostMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegBoostPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegTempWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegTempWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configVegPrecipWeightMinus) "-"
        drawCentered fontCache labelColor (scrollRect configVegPrecipWeightPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionTempMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionTempPlus) "+"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionPrecipMinus) "-"
        drawCentered fontCache labelColor (scrollRect configBoundaryMotionPrecipPlus) "+"
        drawCentered fontCache labelColor (scrollRect configPlanetRadiusMinus) "-"
        drawCentered fontCache labelColor (scrollRect configPlanetRadiusPlus) "+"
        drawCentered fontCache labelColor (scrollRect configAxialTiltMinus) "-"
        drawCentered fontCache labelColor (scrollRect configAxialTiltPlus) "+"
        drawCentered fontCache labelColor (scrollRect configInsolationMinus) "-"
        drawCentered fontCache labelColor (scrollRect configInsolationPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLatCenterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLatCenterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLatExtentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLatExtentPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLonCenterMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLonCenterPlus) "+"
        drawCentered fontCache labelColor (scrollRect configSliceLonExtentMinus) "-"
        drawCentered fontCache labelColor (scrollRect configSliceLonExtentPlus) "+"
        drawLabelAbove fontCache labelColor (scrollRect configWaterBar) (sliderLabel specWaterLevel (uiWaterLevel ui))
        drawLabelAbove fontCache labelColor (scrollRect configEvapBar) (sliderLabel specEvaporation (uiEvaporation ui))
        drawLabelAbove fontCache labelColor (scrollRect configRainShadowBar) (sliderLabel specRainShadow (uiRainShadow ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindDiffuseBar) (sliderLabel specWindDiffuse (uiWindDiffuse ui))
        drawLabelAbove fontCache labelColor (scrollRect configEquatorTempBar) (sliderLabel specEquatorTemp (uiEquatorTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configPoleTempBar) (sliderLabel specPoleTemp (uiPoleTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configLapseRateBar) (sliderLabel specLapseRate (uiLapseRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configLatitudeBiasBar) (sliderLabel specLatitudeBias (uiLatitudeBias ui))
        drawLabelAbove fontCache labelColor (scrollRect configWindIterationsBar) (sliderLabel specWindIterations (uiWindIterations ui))
        drawLabelAbove fontCache labelColor (scrollRect configMoistureIterationsBar) (sliderLabel specMoistureIterations (uiMoistureIterations ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherTickBar) (sliderLabel specWeatherTick (uiWeatherTick ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherPhaseBar) (sliderLabel specWeatherPhase (uiWeatherPhase ui))
        drawLabelAbove fontCache labelColor (scrollRect configWeatherAmplitudeBar) (sliderLabel specWeatherAmplitude (uiWeatherAmplitude ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegBaseBar) (sliderLabel specVegBase (uiVegBase ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegBoostBar) (sliderLabel specVegBoost (uiVegBoost ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegTempWeightBar) (sliderLabel specVegTempWeight (uiVegTempWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configVegPrecipWeightBar) (sliderLabel specVegPrecipWeight (uiVegPrecipWeight ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryMotionTempBar) (sliderLabel specBoundaryMotionTemp (uiBoundaryMotionTemp ui))
        drawLabelAbove fontCache labelColor (scrollRect configBoundaryMotionPrecipBar) (sliderLabel specBoundaryMotionPrecip (uiBoundaryMotionPrecip ui))
        drawLabelAbove fontCache labelColor (scrollRect configPlanetRadiusBar) (sliderLabel specPlanetRadius (uiPlanetRadius ui))
        drawLabelAbove fontCache labelColor (scrollRect configAxialTiltBar) (sliderLabel specAxialTilt (uiAxialTilt ui))
        drawLabelAbove fontCache labelColor (scrollRect configInsolationBar) (sliderLabel specInsolation (uiInsolation ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLatCenterBar) (sliderLabel specSliceLatCenter (uiSliceLatCenter ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLatExtentBar) (sliderLabel specSliceLatExtent (uiSliceLatExtent ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLonCenterBar) (sliderLabel specSliceLonCenter (uiSliceLonCenter ui))
        drawLabelAbove fontCache labelColor (scrollRect configSliceLonExtentBar) (sliderLabel specSliceLonExtent (uiSliceLonExtent ui))
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
        drawLabelAbove fontCache labelColor (scrollRect configErosionHydraulicBar) (sliderLabel specErosionHydraulic (uiErosionHydraulic ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionThermalBar) (sliderLabel specErosionThermal (uiErosionThermal ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionRainRateBar) (sliderLabel specErosionRainRate (uiRainRate ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionTalusBar) (sliderLabel specErosionTalus (uiErosionTalus ui))
        drawLabelAbove fontCache labelColor (scrollRect configErosionMaxDropBar) (sliderLabel specErosionMaxDrop (uiErosionMaxDrop ui))
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
      let lines = contextLines terrainSnap (q, r)
          panelW = 220
          lineH = 16
          panelH = max 40 (20 + lineH * length lines)
          px = clamp 8 (winW - panelW - 8) (sx + 12)
          py = clamp 8 (winH - panelH - 8) (sy + 12)
      SDL.rendererDrawColor renderer SDL.$= V4 20 20 25 235
      SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral px) (fromIntegral py))) (V2 (fromIntegral panelW) (fromIntegral panelH))))
      case fontCache of
        Nothing -> pure ()
        Just cache ->
          sequence_ [ drawTextLine fontCache (V2 (px + 8) (py + 8 + idx * lineH)) (V4 230 230 235 255) line
                    | (idx, line) <- zip [0..] lines
                    ]
    _ -> pure ()
  where
    clamp lo hi v = max lo (min hi v)

contextLines :: TerrainSnapshot -> (Int, Int) -> [Text]
contextLines terrainSnap (q, r) =
  case sampleAt terrainSnap (q, r) of
    Nothing -> ["Hex " <> Text.pack (show q) <> "," <> Text.pack (show r), "No data"]
    Just sample ->
      [ "Hex " <> Text.pack (show q) <> "," <> Text.pack (show r)
      , "Chunk " <> Text.pack (show (hsChunk sample)) <> " Tile " <> Text.pack (show (hsLocal sample))
      , "Elev " <> Text.pack (formatF (hsElevation sample))
      , "Moist " <> Text.pack (formatF (hsMoisture sample))
      , "Biome " <> Text.pack (show (hsBiome sample))
      , "Temp " <> Text.pack (formatF (hsTemp sample))
      ]

data HexSample = HexSample
  { hsChunk :: ChunkCoord
  , hsLocal :: TileCoord
  , hsElevation :: Float
  , hsMoisture :: Float
  , hsBiome :: BiomeId
  , hsTemp :: Float
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
      terrainChunk <- IntMap.lookup key (tsTerrainChunks terrainSnap)
      let elev = tcElevation terrainChunk U.! idx
          moist = tcMoisture terrainChunk U.! idx
          biome = tcFlags terrainChunk U.! idx
          temp = case IntMap.lookup key (tsClimateChunks terrainSnap) of
            Just climateChunk -> ccTempAvg climateChunk U.! idx
            Nothing -> 0
      Just HexSample
        { hsChunk = chunkCoord
        , hsLocal = local
        , hsElevation = elev
        , hsMoisture = moist
        , hsBiome = biome
        , hsTemp = temp
        }

formatF :: Float -> String
formatF v =
  let scaled = fromIntegral (round (v * 100) :: Int) / 100 :: Double
  in show scaled

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
