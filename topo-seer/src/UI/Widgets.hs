module UI.Widgets
  ( Rect(..)
  , containsPoint
  , insetRect
  ) where

import Linear (V2(..))

newtype Rect = Rect { rectPosSize :: (V2 Int, V2 Int) }
  deriving (Eq, Show)

containsPoint :: Rect -> V2 Int -> Bool
containsPoint (Rect (V2 x y, V2 w h)) (V2 px py) =
  px >= x && py >= y && px < x + w && py < y + h

insetRect :: Int -> Rect -> Rect
insetRect n (Rect (V2 x y, V2 w h)) =
  Rect (V2 (x + n) (y + n), V2 (max 0 (w - 2 * n)) (max 0 (h - 2 * n)))
