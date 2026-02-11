{-# LANGUAGE OverloadedStrings #-}

-- | Shared drawing primitives for modal dialogs.
--
-- Both the preset and world save\/load dialogs share the same visual
-- language: a dark semi-transparent overlay, an opaque panel, title
-- text, text input fields, scrollable lists, and action buttons.
-- This module centralises those primitives so each dialog only needs
-- to describe /what/ to draw, not /how/.
module Seer.Draw.Dialog
  ( -- * Dialog chrome
    drawDialogPanel
  , drawDialogTitle
    -- * Input elements
  , drawTextInputField
  , drawListSelection
    -- * Buttons
  , drawDialogButton
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Linear (V2(..), V4(..))
import qualified SDL
import UI.Font (FontCache)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawCentered, drawLeft, rectToSDL)

-------------------------------------------------------------------------------
-- Dialog chrome
-------------------------------------------------------------------------------

-- | Draw a dark opaque dialog panel at the given 'Rect'.
--
-- This fills the panel area with a near-black translucent background
-- suitable for modal overlays.  The caller is responsible for dimming
-- or blocking input to the area underneath.
drawDialogPanel :: SDL.Renderer -> Rect -> IO ()
drawDialogPanel renderer rect = do
  SDL.rendererDrawColor renderer SDL.$= V4 20 25 35 240
  SDL.fillRect renderer (Just (rectToSDL rect))

-- | Draw a centred title at the top of a dialog panel.
--
-- The title occupies a 32-pixel-tall strip anchored at the top of
-- the given dialog 'Rect'.
drawDialogTitle :: SDL.Renderer -> Maybe FontCache -> Rect -> Text -> IO ()
drawDialogTitle _renderer fontCache (Rect (V2 dx dy, V2 dw _)) title =
  drawCentered fontCache (V4 220 220 230 255) (Rect (V2 dx dy, V2 dw 32)) title

-------------------------------------------------------------------------------
-- Input elements
-------------------------------------------------------------------------------

-- | Draw a bordered text input field with its current content.
--
-- Renders a dark background, a subtle border, and the supplied text
-- left-aligned inside the box.
drawTextInputField :: SDL.Renderer -> Maybe FontCache -> Rect -> Text -> IO ()
drawTextInputField renderer fontCache rect content = do
  -- Background
  SDL.rendererDrawColor renderer SDL.$= V4 40 42 55 255
  SDL.fillRect renderer (Just (rectToSDL rect))
  -- Border
  SDL.rendererDrawColor renderer SDL.$= V4 90 100 120 255
  SDL.drawRect renderer (Just (rectToSDL rect))
  -- Content
  drawLeft fontCache (V4 220 220 230 255) rect content

-- | Draw a selectable list with a highlight on the selected index.
--
-- @itemHeight@ is the pixel height of each row.  At most @maxVisible@
-- items are drawn starting from index 0.  The @itemRect@ callback
-- maps an index to its screen-space 'Rect'.
drawListSelection
  :: SDL.Renderer
  -> Maybe FontCache
  -> Rect              -- ^ Overall list background rect
  -> Int               -- ^ Item height in pixels
  -> Int               -- ^ Maximum visible items
  -> Int               -- ^ Currently selected index
  -> (Int -> Rect)     -- ^ Item rect for a given index
  -> (Int -> a -> Text) -- ^ Label renderer for an item
  -> [a]               -- ^ Items
  -> IO ()
drawListSelection renderer fontCache listRect _itemHeight maxVisible sel itemRectFn labelFn items = do
  -- List background
  SDL.rendererDrawColor renderer SDL.$= V4 30 32 42 255
  SDL.fillRect renderer (Just (rectToSDL listRect))
  -- Visible items
  let visible = take maxVisible items
  mapM_ (\(i, item) -> do
    let r = itemRectFn i
        isSelected = i == sel
    when isSelected $ do
      SDL.rendererDrawColor renderer SDL.$= V4 70 90 120 255
      SDL.fillRect renderer (Just (rectToSDL r))
    let col = if isSelected then V4 240 240 245 255 else V4 180 180 190 255
    drawLeft fontCache col r (labelFn i item)
    ) (zip [0..] visible)

-------------------------------------------------------------------------------
-- Buttons
-------------------------------------------------------------------------------

-- | Draw a dialog action button (Save, Load, Cancel, Ok, etc.).
--
-- When @isEnabled@ is 'False' the button is drawn in a dimmed style
-- to indicate it cannot be activated.
drawDialogButton :: SDL.Renderer -> Maybe FontCache -> Rect -> Text -> Bool -> IO ()
drawDialogButton renderer fontCache rect label isEnabled = do
  let fill = if isEnabled then V4 70 90 120 255 else V4 55 55 65 255
      textColor = if isEnabled then V4 230 230 235 255 else V4 140 140 150 255
  SDL.rendererDrawColor renderer SDL.$= fill
  SDL.fillRect renderer (Just (rectToSDL rect))
  drawCentered fontCache textColor rect label
