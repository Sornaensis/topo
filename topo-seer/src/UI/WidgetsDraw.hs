module UI.WidgetsDraw
  ( rectToSDL
  , drawCentered
  , drawTextLine
  , drawTextLineTruncated
  , drawLeft
  , drawLeftTruncated
  , drawLabelAbove
  , drawLabelLeft
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import UI.Font
  ( FontCache
  , boundedTextWithEllipsis
  , drawText
  , drawTextCentered
  , fontTextRenderCharLimit
  , textSize
  )
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

-- | Truncate @text@ to fit within @maxWidth@ pixels, appending @"\x2026"@
-- (horizontal ellipsis) when the text is clipped.
truncateToWidth :: FontCache -> V4 Word8 -> Int -> Text -> IO Text
truncateToWidth cache color maxWidth text
  | maxWidth <= 0 = pure Text.empty
  | otherwise = do
      let (bounded, clippedByChars) = boundedTextWithEllipsis fontTextRenderCharLimit text
          base = if clippedByChars then Text.dropEnd (Text.length ellipsis) bounded else bounded
      V2 tw _ <- textSize cache color bounded
      if tw <= maxWidth
        then pure bounded
        else fitBase base
  where
    ellipsis = Text.singleton '\x2026'
    fitBase base = do
      best <- findBest 0 (Text.length base) 0
      if best <= 0
        then pure ellipsis
        else pure (Text.take best base <> ellipsis)
      where
        findBest lo hi best
          | lo > hi = pure best
          | otherwise = do
              let mid = (lo + hi) `div` 2
                  candidate = Text.take mid base <> ellipsis
              V2 w _ <- textSize cache color candidate
              if w <= maxWidth
                then findBest (mid + 1) hi mid
                else findBest lo (mid - 1) best

-- | Like 'drawTextLine' but clips the text to @maxWidth@ pixels,
-- appending an ellipsis character when truncation occurs.
drawTextLineTruncated :: Maybe FontCache -> V2 Int -> V4 Word8 -> Int -> Text -> IO ()
drawTextLineTruncated Nothing _ _ _ _ = pure ()
drawTextLineTruncated (Just cache) pos color maxWidth message = do
  clipped <- truncateToWidth cache color maxWidth message
  drawText cache color pos clipped

-- | Like 'drawLeft' but clips the text to the rect's width (with 8 px of
-- horizontal padding), appending an ellipsis character when truncation occurs.
drawLeftTruncated :: Maybe FontCache -> V4 Word8 -> Rect -> Text -> IO ()
drawLeftTruncated Nothing _ _ _ = pure ()
drawLeftTruncated (Just cache) color rect@(Rect (_, V2 w _)) label = do
  let innerW = max 0 (w - 8)
  clipped <- truncateToWidth cache color innerW label
  drawLeft (Just cache) color rect clipped
