module Seer.Render.Ui
  ( drawUiOverlay
  ) where

import Actor.Log (LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.Data (TerrainSnapshot)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Draw
  ( drawEscapeMenu
  , drawHexContext
  , drawLogFilters
  , drawLogLines
  , drawLogScrollbar
  , drawUiLabels
  )
import UI.Font (FontCache)
import UI.Layout
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Draw UI overlay elements that sit above the terrain/atlas.
drawUiOverlay
  :: SDL.Renderer
  -> Maybe FontCache
  -> RenderSnapshot
  -> TerrainSnapshot
  -> Layout
  -> (Rect, Rect, Rect, Rect)
  -> V2 Int
  -> IO ()
drawUiOverlay renderer fontCache snapshot terrainSnap layout logFilters (V2 winW winH) = do
  let logSnap = rsLog snapshot
  drawUiLabels renderer fontCache (rsUi snapshot) layout
  SDL.rendererDrawColor renderer SDL.$= V4 30 30 30 220
  SDL.fillRect renderer (Just (rectToSDL (logPanelRect layout)))
  SDL.rendererDrawColor renderer SDL.$= V4 45 45 45 235
  SDL.fillRect renderer (Just (rectToSDL (logHeaderRect layout)))
  drawLogScrollbar renderer logSnap (logBodyRect layout)
  drawLogLines renderer fontCache logSnap (logBodyRect layout)
  drawLogFilters renderer fontCache (lsMinLevel logSnap) logFilters
  drawEscapeMenu renderer fontCache (rsUi snapshot) layout
  drawHexContext renderer fontCache (rsUi snapshot) terrainSnap (V2 (fromIntegral winW) (fromIntegral winH))
