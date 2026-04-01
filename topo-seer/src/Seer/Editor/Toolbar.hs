{-# LANGUAGE OverloadedStrings #-}

-- | Editor toolbar rendering.
--
-- Draws the horizontal tool bar at the top-center of the window when
-- the terrain editor is active.  Each tool gets a coloured button;
-- the active tool is highlighted.  Brush radius controls and a close
-- button sit at the right end.
module Seer.Editor.Toolbar
  ( drawEditorToolbar
  , drawEditorReopenButton
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
  , editorReopenRect
  , editorToolButtonRect
  , editorToolbarRect
  )
import UI.Widgets (Rect(..))
import UI.Theme
import UI.WidgetsDraw (drawCentered, rectToSDL)

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
  SDL.rendererDrawColor renderer SDL.$= colEditorRadiusBtn
  SDL.fillRect renderer (Just (rectToSDL minusRect))
  SDL.fillRect renderer (Just (rectToSDL plusRect))
  SDL.rendererDrawColor renderer SDL.$= colCtrlValue
  SDL.fillRect renderer (Just (rectToSDL valueRect))
  drawCentered fontCache labelColor minusRect "\8722"
  drawCentered fontCache labelColor plusRect "+"
  let radText = "R:" <> showT radius
  drawCentered fontCache labelColor valueRect radText
  -- Close button
  let closeRect = editorCloseRect layout
  SDL.rendererDrawColor renderer SDL.$= colEditorCloseBg
  SDL.fillRect renderer (Just (rectToSDL closeRect))
  drawCentered fontCache textEditorClose closeRect "X"

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
