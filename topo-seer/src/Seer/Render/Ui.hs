module Seer.Render.Ui
  ( drawUiOverlay
  ) where

import Actor.Log (LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.Data (TerrainSnapshot)
import Actor.UI (UiState(..))
import Data.Maybe (isJust)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderSpec (tooltipForWidget)
import Seer.Draw
  ( drawEscapeMenu
  , drawHexContext
  , drawLogFilters
  , drawLogLines
  , drawLogScrollbar
  , drawTooltip
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
      ui = rsUi snapshot
  drawUiLabels renderer fontCache ui layout
  SDL.rendererDrawColor renderer SDL.$= V4 30 30 30 220
  SDL.fillRect renderer (Just (rectToSDL (logPanelRect layout)))
  SDL.rendererDrawColor renderer SDL.$= V4 45 45 45 235
  SDL.fillRect renderer (Just (rectToSDL (logHeaderRect layout)))
  drawLogScrollbar renderer logSnap (logBodyRect layout)
  drawLogLines renderer fontCache logSnap (logBodyRect layout)
  drawLogFilters renderer fontCache (lsMinLevel logSnap) logFilters
  drawEscapeMenu renderer fontCache ui layout
  drawHexContext renderer fontCache ui terrainSnap (V2 (fromIntegral winW) (fromIntegral winH))
  -- Tooltip rendering for hovered config widgets
  case uiHoverWidget ui of
    Just wid | isJust (tooltipForWidget wid) -> do
      let Just tipText = tooltipForWidget wid
      SDL.P (V2 mx my) <- SDL.getAbsoluteMouseLocation
      drawTooltip renderer fontCache (V2 winW winH) (V2 (fromIntegral mx) (fromIntegral my)) tipText
    _ -> pure ()
