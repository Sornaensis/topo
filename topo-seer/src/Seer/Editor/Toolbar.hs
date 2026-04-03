{-# LANGUAGE OverloadedStrings #-}

-- | Editor toolbar rendering.
--
-- Draws the horizontal tool bar at the top-center of the window when
-- the terrain editor is active.  Each tool gets a coloured button;
-- the active tool is highlighted.  Brush radius controls and a close
-- button sit at the right end.  A context-sensitive parameter bar sits
-- directly below the toolbar showing the active tool's editable parameters.
module Seer.Editor.Toolbar
  ( drawEditorToolbar
  , drawEditorReopenButton
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Linear (V4(..))
import Numeric (showFFloat)
import qualified SDL
import Actor.UI (UiState(..))
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..), Falloff(..))
import Topo.Biome.Name (biomeDisplayName)
import Topo.Types
  ( BiomeId
  , TerrainForm
  , terrainFormDisplayName
  )
import UI.Font (FontCache)
import UI.Layout
  ( Layout
  , editorCloseRect
  , editorParamBarRect
  , editorParamCycleRects
  , editorParamFalloffRects
  , editorParamNumericRects
  , editorRadiusMinusRect
  , editorRadiusPlusRect
  , editorRadiusValueRect
  , editorReopenRect
  , editorToolButtonRect
  , editorToolbarRect
  )
import UI.Widgets (Rect(..))
import UI.Theme
import UI.WidgetsDraw (drawCentered, rectToSDL)

editorRadiusMin :: Int
editorRadiusMin = 0

editorRadiusMax :: Int
editorRadiusMax = 6

-- | Draw the editor toolbar when the editor is active.
drawEditorToolbar :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawEditorToolbar renderer fontCache ui layout = do
  let editor = uiEditor ui
  -- Background bar
  SDL.rendererDrawColor renderer SDL.$= colToolbarBg
  SDL.fillRect renderer (Just (rectToSDL (editorToolbarRect layout)))
  -- Tool buttons
  let tools = [minBound .. maxBound] :: [EditorTool]
  forM_ (zip [0..] tools) $ \(idx, tool) -> do
    let rect = editorToolButtonRect idx layout
        active = editorTool editor == tool
        (cr, cg, cb) = toolButtonColor tool
        bg = if active then V4 cr cg cb 255 else V4 (cr `div` 3) (cg `div` 3) (cb `div` 3) 200
        fg = if active then textEditorActive else textEditorInactive
    SDL.rendererDrawColor renderer SDL.$= bg
    SDL.fillRect renderer (Just (rectToSDL rect))
    drawCentered fontCache fg rect (toolShortLabel tool)
  -- Radius controls
  let minusRect  = editorRadiusMinusRect layout
      valueRect  = editorRadiusValueRect layout
      plusRect   = editorRadiusPlusRect layout
      labelColor = textEditorLabel
      radius = brushRadius (editorBrush editor)
      minusDisabled = radius <= editorRadiusMin
      plusDisabled  = radius >= editorRadiusMax
      minusBg = if minusDisabled then colEditorRadiusBtnDisabled else colEditorRadiusBtn
      plusBg  = if plusDisabled  then colEditorRadiusBtnDisabled else colEditorRadiusBtn
      minusLabel = if minusDisabled then textEditorInactive else labelColor
      plusLabel  = if plusDisabled  then textEditorInactive else labelColor
  SDL.rendererDrawColor renderer SDL.$= minusBg
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.rendererDrawColor renderer SDL.$= plusBg
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= colCtrlValue
  SDL.fillRect renderer (Just (rectToSDL valueRect))
  drawCentered fontCache minusLabel minusRect "\8722"
  drawCentered fontCache plusLabel  plusRect "+"
  let radText = "R:" <> showT radius
  drawCentered fontCache labelColor valueRect radText
  -- Close button
  let closeRect = editorCloseRect layout
  SDL.rendererDrawColor renderer SDL.$= colEditorCloseBg
  SDL.fillRect renderer (Just (rectToSDL closeRect))
  drawCentered fontCache textEditorClose closeRect "X"
  -- Parameter bar
  drawEditorParamBar renderer fontCache editor layout

-- | Draw the context-sensitive parameter bar below the editor toolbar.
drawEditorParamBar :: SDL.Renderer -> Maybe FontCache -> EditorState -> Layout -> IO ()
drawEditorParamBar renderer fontCache editor layout = do
  let barRect = editorParamBarRect layout
  SDL.rendererDrawColor renderer SDL.$= colToolbarBg
  SDL.fillRect renderer (Just (rectToSDL barRect))
  let lc = textEditorLabel
      bc = colEditorRadiusBtn
      vc = colCtrlValue
  -- Tool-specific controls
  case editorTool editor of
    ToolRaise -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Step" (brushStrength (editorBrush editor)) (showStrength (brushStrength (editorBrush editor)))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))
    ToolLower -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Step" (brushStrength (editorBrush editor)) (showStrength (brushStrength (editorBrush editor)))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))
    ToolSmooth -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Pass" (fromIntegral (editorSmoothPasses editor)) (showT (editorSmoothPasses editor))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))
    ToolFlatten -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Rate" (brushStrength (editorBrush editor)) (showStrength (brushStrength (editorBrush editor)))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))
    ToolNoise -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Freq" (editorNoiseFrequency editor) (showF1 (editorNoiseFrequency editor))
      drawNumeric renderer fontCache layout lc bc vc 1
        "Str" (brushStrength (editorBrush editor)) (showStrength (brushStrength (editorBrush editor)))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))
    ToolPaintBiome ->
      drawCycle renderer fontCache layout lc bc vc 0
        (biomeShortLabel (editorBiomeId editor))
    ToolPaintForm ->
      drawCycle renderer fontCache layout lc bc vc 0
        (formShortLabel (editorFormOverride editor))
    ToolSetHardness ->
      drawNumeric renderer fontCache layout lc bc vc 0
        "Hard" (editorHardnessTarget editor) (showF2 (editorHardnessTarget editor))
    ToolErode -> do
      drawNumeric renderer fontCache layout lc bc vc 0
        "Pass" (fromIntegral (editorErodePasses editor)) (showT (editorErodePasses editor))
      drawFalloff renderer fontCache layout lc bc vc (brushFalloff (editorBrush editor))

-- | Draw a labelled \u2212/value/+ numeric control at param-bar slot @n@.
drawNumeric
  :: SDL.Renderer -> Maybe FontCache -> Layout
  -> V4 Word8 -> V4 Word8 -> V4 Word8
  -> Int -> Text -> Float -> Text -> IO ()
drawNumeric renderer fontCache layout lc bc vc slot label _val valText = do
  let (minR, valR, plusR) = editorParamNumericRects slot layout
  SDL.rendererDrawColor renderer SDL.$= bc
  SDL.fillRect renderer (Just (rectToSDL minR))
  SDL.fillRect renderer (Just (rectToSDL plusR))
  SDL.rendererDrawColor renderer SDL.$= vc
  SDL.fillRect renderer (Just (rectToSDL valR))
  drawCentered fontCache lc minR "\8722"
  drawCentered fontCache lc plusR "+"
  drawCentered fontCache lc valR (label <> ":" <> valText)

-- | Draw a \u25c4/label/\u25ba cycle selector at param-bar slot @n@.
drawCycle
  :: SDL.Renderer -> Maybe FontCache -> Layout
  -> V4 Word8 -> V4 Word8 -> V4 Word8
  -> Int -> Text -> IO ()
drawCycle renderer fontCache layout lc bc vc slot nameText = do
  let (prevR, lblR, nextR) = editorParamCycleRects slot layout
  SDL.rendererDrawColor renderer SDL.$= bc
  SDL.fillRect renderer (Just (rectToSDL prevR))
  SDL.fillRect renderer (Just (rectToSDL nextR))
  SDL.rendererDrawColor renderer SDL.$= vc
  SDL.fillRect renderer (Just (rectToSDL lblR))
  drawCentered fontCache lc prevR "\9668"
  drawCentered fontCache lc nextR "\9658"
  drawCentered fontCache lc lblR nameText

-- | Draw the falloff cycle selector at the right end of the param bar.
drawFalloff
  :: SDL.Renderer -> Maybe FontCache -> Layout
  -> V4 Word8 -> V4 Word8 -> V4 Word8
  -> Falloff -> IO ()
drawFalloff renderer fontCache layout lc bc vc falloff = do
  let (prevR, lblR, nextR) = editorParamFalloffRects layout
  SDL.rendererDrawColor renderer SDL.$= bc
  SDL.fillRect renderer (Just (rectToSDL prevR))
  SDL.fillRect renderer (Just (rectToSDL nextR))
  SDL.rendererDrawColor renderer SDL.$= vc
  SDL.fillRect renderer (Just (rectToSDL lblR))
  drawCentered fontCache lc prevR "\9668"
  drawCentered fontCache lc nextR "\9658"
  drawCentered fontCache lc lblR (falloffLabel falloff)

falloffLabel :: Falloff -> Text
falloffLabel FalloffLinear   = "Linear"
falloffLabel FalloffSmooth   = "Smooth"
falloffLabel FalloffConstant = "Const"

biomeShortLabel :: BiomeId -> Text
biomeShortLabel b = Text.take 10 (biomeDisplayName b)

formShortLabel :: TerrainForm -> Text
formShortLabel f = Text.take 10 (Text.pack (terrainFormDisplayName f))

showStrength :: Float -> Text
showStrength v = Text.pack (showFFloat (Just 3) v "")

showF1 :: Float -> Text
showF1 v = Text.pack (showFFloat (Just 1) v "")

showF2 :: Float -> Text
showF2 v = Text.pack (showFFloat (Just 2) v "")

-- | Short button label per tool.
toolShortLabel :: EditorTool -> Text
toolShortLabel ToolRaise       = "Raise"
toolShortLabel ToolLower       = "Lower"
toolShortLabel ToolSmooth      = "Smooth"
toolShortLabel ToolFlatten     = "Flat"
toolShortLabel ToolNoise       = "Noise"
toolShortLabel ToolPaintBiome  = "Biome"
toolShortLabel ToolPaintForm   = "Form"
toolShortLabel ToolSetHardness = "Hard"
toolShortLabel ToolErode       = "Erode"

-- | Button background colour per tool.
toolButtonColor :: EditorTool -> (Word8, Word8, Word8)
toolButtonColor ToolRaise       = (100, 200, 100)
toolButtonColor ToolLower       = (200, 100, 100)
toolButtonColor ToolSmooth      = (100, 180, 200)
toolButtonColor ToolFlatten     = (200, 200, 100)
toolButtonColor ToolNoise       = (170, 100, 200)
toolButtonColor ToolPaintBiome  = (100, 170, 170)
toolButtonColor ToolPaintForm   = (200, 150, 80)
toolButtonColor ToolSetHardness = (190, 140, 60)
toolButtonColor ToolErode       = (150, 110, 70)

-- | Show an 'Int' as 'Text' without OverloadedStrings ambiguity.
showT :: Int -> Text
showT = Text.pack . show

-- | Draw a small button to reopen the editor toolbar when it is closed.
drawEditorReopenButton :: SDL.Renderer -> Maybe FontCache -> Layout -> IO ()
drawEditorReopenButton renderer fontCache layout = do
  let rect = editorReopenRect layout
  SDL.rendererDrawColor renderer SDL.$= colEditorReopenBg
  SDL.fillRect renderer (Just (rectToSDL rect))
  drawCentered fontCache textEditorReopen rect "Edit"
