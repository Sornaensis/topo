module UI.WidgetsDraw
  ( rectToSDL
  , drawCentered
  , drawTextLine
  , drawLeft
  , drawLabelAbove
  , drawLabelLeft
  ) where

import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import UI.Font (FontCache, drawText, drawTextCentered, textSize)
import UI.Widgets (Rect(..))

rectToSDL :: Rect -> SDL.Rectangle CInt
rectToSDL (Rect (V2 x y, V2 w h)) =
  SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))

drawCentered :: Maybe FontCache -> V4 Word8 -> Rect -> Text -> IO ()
drawCentered Nothing _ _ _ = pure ()
drawCentered (Just cache) color rect label =
  drawTextCentered cache color (rectToSDL rect) label

drawTextLine :: Maybe FontCache -> V2 Int -> V4 Word8 -> Text -> IO ()
drawTextLine Nothing _ _ _ = pure ()
drawTextLine (Just cache) pos color message =
  drawText cache color pos message

drawLeft :: Maybe FontCache -> V4 Word8 -> Rect -> Text -> IO ()
drawLeft Nothing _ _ _ = pure ()
drawLeft (Just cache) color (Rect (V2 x y, V2 _w h)) label = do
  V2 _ th <- textSize cache color label
  let ly = y + (h - th) `div` 2
  drawText cache color (V2 (x + 4) ly) label

drawLabelAbove :: Maybe FontCache -> V4 Word8 -> Rect -> Text -> IO ()
drawLabelAbove Nothing _ _ _ = pure ()
drawLabelAbove (Just cache) color (Rect (V2 x y, V2 w _h)) label = do
  V2 tw th <- textSize cache color label
  let lx = x + (w - tw) `div` 2
      ly = max 0 (y - th - 4)
  drawText cache color (V2 lx ly) label

drawLabelLeft :: Maybe FontCache -> V4 Word8 -> Rect -> Text -> IO ()
drawLabelLeft Nothing _ _ _ = pure ()
drawLabelLeft (Just cache) color (Rect (V2 x y, V2 _w h)) label = do
  V2 _ th <- textSize cache color label
  let ly = y + (h - th) `div` 2
  drawText cache color (V2 (x - 52) ly) label
