-- | View control helpers for zooming and view mode hotkeys.
module Seer.Input.ViewControls
  ( ZoomSettings(..)
  , defaultZoomSettings
  , zoomStepCount
  , applyZoomAtCursor
  , viewportDragThreshold
  , isViewportDrag
  , panViewportForDrag
  , isTerrainHex
  , pickTerrainHex
  , nextBuiltinOverlay
  , nextWeatherBasis
  , ViewHotkey(..)
  , viewHotkeyForKey
  , viewModeForKey
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis(..)
  )
import qualified Data.IntMap.Strict as IntMap
import qualified SDL
import Seer.Render.ZoomStage (maxCameraZoom)
import Topo
  ( ChunkId(..)
  , TileCoord(..)
  , WorldConfig(..)
  , chunkCoordFromTile
  , chunkIdFromCoord
  )
import UI.HexGeometry (renderHexRadiusPx, screenPixelToAxial)

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

-- | Number of wheel steps represented by a signed SDL/API delta.
zoomStepCount :: Int -> Int
zoomStepCount delta =
  fromInteger (min (toInteger (maxBound :: Int)) (abs (toInteger delta)))

-- | Apply all zoom steps centered at the cursor position.
applyZoomAtCursor
  :: ZoomSettings
  -> UiState
  -> (Int, Int)
  -> Int
  -> (Float, (Float, Float))
applyZoomAtCursor settings uiSnap (mx, my) delta =
  let oldZoom = uiZoom uiSnap
      zoomFactor
        | delta > 0 = zsZoomInFactor settings
        | delta < 0 = zsZoomOutFactor settings
        | otherwise = 1
      unclamped = oldZoom * zoomFactor ^ zoomStepCount delta
      newZoom = clamp (zsMin settings) (zsMax settings) unclamped
      (ox, oy) = uiPanOffset uiSnap
      fx = fromIntegral mx
      fy = fromIntegral my
      newOffset =
        ( ox + fx * (1 / newZoom - 1 / oldZoom)
        , oy + fy * (1 / newZoom - 1 / oldZoom)
        )
  in (newZoom, newOffset)

-- | SDL starts a right-button pan only after moving more than four pixels.
viewportDragThreshold :: Int
viewportDragThreshold = 4

isViewportDrag :: (Int, Int) -> (Int, Int) -> Bool
isViewportDrag (x1, y1) (x2, y2) =
  let dx = toInteger x2 - toInteger x1
      dy = toInteger y2 - toInteger y1
      threshold = toInteger viewportDragThreshold
  in dx * dx + dy * dy > threshold * threshold

-- | Apply a screen-space right drag to the current world-space pan offset.
panViewportForDrag :: UiState -> (Int, Int) -> (Int, Int) -> (Float, Float)
panViewportForDrag uiSnap (x1, y1) (x2, y2) =
  let (ox, oy) = uiPanOffset uiSnap
      zoom = uiZoom uiSnap
      dx = fromInteger (toInteger x2 - toInteger x1)
      dy = fromInteger (toInteger y2 - toInteger y1)
  in (ox + dx / zoom, oy + dy / zoom)

-- | Check whether an axial hex belongs to a currently loaded terrain chunk.
isTerrainHex :: TerrainSnapshot -> (Int, Int) -> Bool
isTerrainHex snap (q, r) =
  let size = tsChunkSize snap
  in size > 0
    && let cfg = WorldConfig { wcChunkSize = size }
           (chunkCoord, _) = chunkCoordFromTile cfg (TileCoord q r)
           ChunkId key = chunkIdFromCoord chunkCoord
       in IntMap.member key (tsTerrainChunks snap)

-- | Resolve a screen pixel and report whether its hex exists in the terrain.
pickTerrainHex :: TerrainSnapshot -> UiState -> (Int, Int) -> ((Int, Int), Bool)
pickTerrainHex terrainSnap uiSnap point =
  let hex = screenPixelToAxial renderHexRadiusPx
        (uiPanOffset uiSnap) (uiZoom uiSnap) point
  in (hex, isTerrainHex terrainSnap hex)

nextBuiltinOverlay :: LayeredViewState -> Maybe SkyOverlayMode
nextBuiltinOverlay selection = case lvsSkyOverlay selection of
  Nothing -> Just SkyOverlayWeatherTemperature
  Just SkyOverlayWeatherTemperature -> Just SkyOverlayPrecipitation
  Just SkyOverlayPrecipitation -> Just SkyOverlayCloud
  Just SkyOverlayCloud -> Nothing
  Just (SkyOverlayPlugin _ _) -> Just SkyOverlayWeatherTemperature

nextWeatherBasis :: WeatherBasis -> WeatherBasis
nextWeatherBasis WeatherBasisAverage = WeatherBasisCurrent
nextWeatherBasis WeatherBasisCurrent = WeatherBasisAverage

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
