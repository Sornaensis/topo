{-# LANGUAGE OverloadedStrings #-}

-- | Editor toolbar rendering.
--
-- Draws the horizontal tool bar at the top-center of the window when
-- the terrain editor is active.  Each tool gets a coloured button;
-- the active tool is highlighted.  Brush radius controls and a close
-- button sit at the right end.
module Seer.Editor.Toolbar
  ( drawEditorToolbar
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Linear (V4(..))
import qualified SDL
import Actor.UI (UiState(..))
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..))
import UI.Font (FontCache)
import UI.Layout
  ( Layout
  , editorCloseRect
  , editorRadiusMinusRect
  , editorRadiusPlusRect
  , editorRadiusValueRect
  , editorToolButtonRect
  , editorToolbarRect
  )
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, rectToSDL)

-- | Draw the editor toolbar when the editor is active.
drawEditorToolbar :: SDL.Renderer -> Maybe FontCache -> UiState -> Layout -> IO ()
drawEditorToolbar renderer fontCache ui layout = do
  let editor = uiEditor ui
  -- Background bar
  SDL.rendererDrawColor renderer SDL.$= V4 30 35 45 230
  SDL.fillRect renderer (Just (rectToSDL (editorToolbarRect layout)))
  -- Tool buttons
  let tools = [minBound .. maxBound] :: [EditorTool]
  forM_ (zip [0..] tools) $ \(idx, tool) -> do
    let rect = editorToolButtonRect idx layout
        active = editorTool editor == tool
        (cr, cg, cb) = toolButtonColor tool
        bg = if active then V4 cr cg cb 255 else V4 (cr `div` 3) (cg `div` 3) (cb `div` 3) 200
        fg = if active then V4 0 0 0 255 else V4 200 200 210 255
    SDL.rendererDrawColor renderer SDL.$= bg
    SDL.fillRect renderer (Just (rectToSDL rect))
    drawCentered fontCache fg rect (toolShortLabel tool)
  -- Radius controls
  let minusRect = editorRadiusMinusRect layout
      valueRect = editorRadiusValueRect layout
      plusRect  = editorRadiusPlusRect layout
      btnColor  = V4 70 80 100 255
      labelColor = V4 220 220 230 255
      radius = brushRadius (editorBrush editor)
  SDL.rendererDrawColor renderer SDL.$= btnColor
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= V4 45 55 70 255
  SDL.fillRect renderer (Just (rectToSDL valueRect))
  drawCentered fontCache labelColor minusRect "\8722"
  drawCentered fontCache labelColor plusRect "+"
  let radText = "R:" <> showT radius
  drawCentered fontCache labelColor valueRect radText
  -- Close button
  let closeRect = editorCloseRect layout
  SDL.rendererDrawColor renderer SDL.$= V4 160 60 60 255
  SDL.fillRect renderer (Just (rectToSDL closeRect))
  drawCentered fontCache (V4 255 255 255 255) closeRect "X"

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

-- | Show an 'Int' as 'Text' without OverloadedStrings ambiguity.
showT :: Int -> Text
showT = Text.pack . show
