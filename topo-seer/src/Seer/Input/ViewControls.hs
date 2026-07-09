-- | View control helpers for zooming and view mode hotkeys.
module Seer.Input.ViewControls
  ( ZoomSettings(..)
  , defaultZoomSettings
  , applyZoomAtCursor
  , ViewHotkey(..)
  , viewHotkeyForKey
  , viewModeForKey
  ) where

import Actor.UI (BaseViewMode(..), SkyOverlayMode(..), UiState(..), ViewMode(..))
import qualified SDL
import Seer.Render.ZoomStage (maxCameraZoom)

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
  , zsMax = maxCameraZoom
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

-- | Layered View-tab hotkeys.  Number keys select base terrain views;
-- letter keys control the independent sky/weather overlay selection.
data ViewHotkey
  = ViewHotkeySetBase !BaseViewMode
  | ViewHotkeySetOverlay !(Maybe SkyOverlayMode)
  | ViewHotkeyCycleOverlay
  | ViewHotkeyCycleWeatherBasis
  deriving (Eq, Show)

viewHotkeyForKey :: SDL.Keycode -> Maybe ViewHotkey
viewHotkeyForKey key =
  case key of
    SDL.Keycode1 -> Just (ViewHotkeySetBase BaseViewElevation)
    SDL.Keycode2 -> Just (ViewHotkeySetBase BaseViewBiome)
    SDL.Keycode3 -> Just (ViewHotkeySetBase BaseViewMoisture)
    SDL.Keycode4 -> Just (ViewHotkeySetBase BaseViewVegetation)
    SDL.Keycode5 -> Just (ViewHotkeySetBase BaseViewTerrainForm)
    SDL.Keycode6 -> Just (ViewHotkeySetBase BaseViewPlateId)
    SDL.Keycode7 -> Just (ViewHotkeySetBase BaseViewPlateBoundary)
    SDL.Keycode8 -> Just (ViewHotkeySetBase BaseViewPlateHardness)
    SDL.Keycode9 -> Just (ViewHotkeySetBase BaseViewPlateCrust)
    SDL.Keycode0 -> Just (ViewHotkeySetBase BaseViewPlateAge)
    SDL.KeycodeH -> Just (ViewHotkeySetBase BaseViewPlateHeight)
    SDL.KeycodeV -> Just (ViewHotkeySetBase BaseViewPlateVelocity)
    SDL.KeycodeN -> Just (ViewHotkeySetOverlay Nothing)
    SDL.KeycodeT -> Just (ViewHotkeySetOverlay (Just SkyOverlayWeatherTemperature))
    SDL.KeycodeP -> Just (ViewHotkeySetOverlay (Just SkyOverlayPrecipitation))
    SDL.KeycodeK -> Just (ViewHotkeySetOverlay (Just SkyOverlayCloud))
    SDL.KeycodeO -> Just ViewHotkeyCycleOverlay
    SDL.KeycodeB -> Just ViewHotkeyCycleWeatherBasis
    _ -> Nothing

-- | Legacy one-dimensional number-key mapping retained for compatibility.
viewModeForKey :: SDL.Keycode -> Maybe ViewMode
viewModeForKey key =
  case key of
    SDL.Keycode1 -> Just ViewElevation
    SDL.Keycode2 -> Just ViewBiome
    SDL.Keycode3 -> Just ViewClimate
    SDL.Keycode4 -> Just ViewMoisture
    SDL.Keycode5 -> Just ViewWeather
    SDL.Keycode6 -> Just ViewPlateId
    SDL.Keycode7 -> Just ViewPlateBoundary
    SDL.Keycode8 -> Just ViewPlateHardness
    SDL.Keycode9 -> Just ViewVegetation
    SDL.Keycode0 -> Just ViewTerrainForm
    _ -> Nothing

clamp :: Ord a => a -> a -> a -> a
clamp low high value =
  max low (min high value)
