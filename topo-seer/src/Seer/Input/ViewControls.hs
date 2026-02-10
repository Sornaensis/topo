-- | View control helpers for zooming and view mode hotkeys.
module Seer.Input.ViewControls
  ( ZoomSettings(..)
  , defaultZoomSettings
  , applyZoomAtCursor
  , viewModeForKey
  ) where

import Actor.UI (UiState(..), ViewMode(..))
import qualified SDL

-- | Zoom configuration settings.
data ZoomSettings = ZoomSettings
  { zsMin :: !Float
  , zsMax :: !Float
  , zsZoomInFactor :: !Float
  , zsZoomOutFactor :: !Float
  }

-- | Default zoom settings.
defaultZoomSettings :: ZoomSettings
defaultZoomSettings = ZoomSettings
  { zsMin = 0.4
  , zsMax = 3
  , zsZoomInFactor = 1.1
  , zsZoomOutFactor = 0.9
  }

-- | Apply zoom changes centered at the cursor position.
applyZoomAtCursor
  :: ZoomSettings
  -> UiState
  -> (Int, Int)
  -> Int
  -> (Float, (Float, Float))
applyZoomAtCursor settings uiSnap (mx, my) dy =
  let oldZoom = uiZoom uiSnap
      zoomFactor
        | dy > 0 = zsZoomInFactor settings
        | dy < 0 = zsZoomOutFactor settings
        | otherwise = 1
      unclamped = oldZoom * zoomFactor
      newZoom = clamp (zsMin settings) (zsMax settings) unclamped
      (ox, oy) = uiPanOffset uiSnap
      fx = fromIntegral mx
      fy = fromIntegral my
      newOffset =
        ( ox + fx * (1 / newZoom - 1 / oldZoom)
        , oy + fy * (1 / newZoom - 1 / oldZoom)
        )
  in (newZoom, newOffset)

-- | Map keycodes to view modes.
viewModeForKey :: SDL.Keycode -> Maybe ViewMode
viewModeForKey key =
  case key of
    SDL.Keycode1 -> Just ViewElevation
    SDL.Keycode2 -> Just ViewBiome
    SDL.Keycode3 -> Just ViewClimate
    SDL.Keycode4 -> Just ViewMoisture
    SDL.Keycode5 -> Just ViewPrecip
    SDL.Keycode6 -> Just ViewPlateId
    SDL.Keycode7 -> Just ViewPlateBoundary
    SDL.Keycode8 -> Just ViewPlateHardness
    _ -> Nothing

clamp :: Ord a => a -> a -> a -> a
clamp low high value =
  max low (min high value)
