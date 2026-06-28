{-# LANGUAGE StrictData #-}

-- | Pure draw-command values for UI components.
--
-- Component draw functions should build these commands without touching SDL so
-- tests can assert drawing intent without constructing a renderer. Runtime
-- renderers interpret the commands in an SDL-specific layer.
module UI.DrawCommand
  ( DrawCommand(..)
  , TextPlacement(..)
  , fillRect
  , strokeRect
  , textAt
  , textCentered
  , textLeft
  , textLabelAbove
  , textLabelLeft
  , clipTo
  , clearClip
  ) where

import Data.Text (Text)
import Data.Word (Word8)
import Linear (V2, V4)
import UI.Widgets (Rect)

-- | Semantic text placement independent of a concrete renderer.
data TextPlacement
  = TextAt !(V2 Int)
  | TextCentered !Rect
  | TextLeft !Rect
  | TextLabelAbove !Rect
  | TextLabelLeft !Rect
  deriving (Eq, Show)

-- | Renderer-neutral UI drawing operations.
data DrawCommand
  = DrawFillRect !(V4 Word8) !Rect
  | DrawStrokeRect !(V4 Word8) !Rect
  | DrawText !(V4 Word8) !TextPlacement !Text
  | DrawClip !(Maybe Rect)
  deriving (Eq, Show)

fillRect :: V4 Word8 -> Rect -> DrawCommand
fillRect = DrawFillRect

strokeRect :: V4 Word8 -> Rect -> DrawCommand
strokeRect = DrawStrokeRect

textAt :: V4 Word8 -> V2 Int -> Text -> DrawCommand
textAt color pos = DrawText color (TextAt pos)

textCentered :: V4 Word8 -> Rect -> Text -> DrawCommand
textCentered color rect = DrawText color (TextCentered rect)

textLeft :: V4 Word8 -> Rect -> Text -> DrawCommand
textLeft color rect = DrawText color (TextLeft rect)

textLabelAbove :: V4 Word8 -> Rect -> Text -> DrawCommand
textLabelAbove color rect = DrawText color (TextLabelAbove rect)

textLabelLeft :: V4 Word8 -> Rect -> Text -> DrawCommand
textLabelLeft color rect = DrawText color (TextLabelLeft rect)

clipTo :: Rect -> DrawCommand
clipTo = DrawClip . Just

clearClip :: DrawCommand
clearClip = DrawClip Nothing
