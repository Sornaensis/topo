-- | Brush cursor preview overlay for the terrain editor.
--
-- Draws a translucent colored hex overlay for every tile in the
-- current brush disc, with per-tile opacity proportional to
-- 'brushWeight'.  Drawn between the terrain atlas and the UI chrome
-- so it sits on top of the world but beneath panels.
module Seer.Editor.Preview
  ( drawBrushPreview
  ) where

import Control.Monad (forM_, when)
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL
import Actor.UI (UiState(..))
import Seer.Draw.Overlay (RectInt(..), drawHexSpansSupersampled, hexSpans, spanBounds, transformRect)
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..), Falloff(..))
import Seer.Editor.Brush (brushWeight)
import Topo.Hex (hexDisc, hexDistance)
import Topo.Types (HexCoord(..))
import UI.HexPick (axialToScreen, renderHexRadiusPx)

-- | Draw the brush cursor preview overlay.
--
-- When the editor is active and the cursor hovers over a valid hex,
-- this draws a translucent hex shape at each tile in the brush disc.
-- The colour depends on the active tool.  The alpha of each hex is
-- @brushWeight × previewAlpha@ (default ~77/255 ≈ 0.3).
drawBrushPreview :: SDL.Renderer -> UiState -> Int -> IO ()
drawBrushPreview renderer uiSnap supersample = do
  let editor = uiEditor uiSnap
  when (editorActive editor) $
    case uiHoverHex uiSnap of
      Nothing  -> pure ()
      Just (hq, hr) -> do
        let brush  = editorBrush editor
            tool   = editorTool editor
            radius = brushRadius brush
            falloff = brushFalloff brush
            center = HexAxial hq hr
            disc   = hexDisc center radius
            (cr, cg, cb) = toolColor tool
            spans  = hexSpans renderHexRadiusPx
            (minX, minY, maxX, maxY) = spanBounds spans
            texW   = fromIntegral (max 1 ((maxX - minX + 1) * supersample))
            texH   = fromIntegral (max 1 ((maxY - minY + 1) * supersample))
        -- Create a single hex-shaped texture with full-opacity tool colour.
        -- Per-tile alpha is applied via textureAlphaMod before each copy.
        hexTex <- SDL.createTexture renderer SDL.RGBA8888
                    SDL.TextureAccessTarget (V2 texW texH)
        SDL.textureBlendMode hexTex SDL.$= SDL.BlendAlphaBlend
        SDL.rendererRenderTarget renderer SDL.$= Just hexTex
        SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
        SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
        SDL.clear renderer
        SDL.rendererDrawColor renderer SDL.$= V4 cr cg cb 255
        drawHexSpansSupersampled renderer spans supersample (minX, minY)
        SDL.rendererRenderTarget renderer SDL.$= Nothing
        -- Draw one copy per brush-disc tile, varying alpha by weight.
        let (ox, oy) = uiPanOffset uiSnap
            z = uiZoom uiSnap
            hexW = maxX - minX + 1
            hexH = maxY - minY + 1
        forM_ disc $ \tile -> do
          let dist  = hexDistance center tile
              w     = brushWeight falloff radius dist
              alpha = round (w * previewAlpha * 255) :: Int
          when (alpha > 0) $ do
            SDL.textureAlphaMod hexTex SDL.$= fromIntegral (min 255 alpha)
            let HexAxial tq tr = tile
                (cx, cy) = axialToScreen renderHexRadiusPx tq tr
                worldX = cx + minX
                worldY = cy + minY
                rect = transformRect (ox, oy) z
                         (RectInt (V2 worldX worldY) (V2 hexW hexH))
                RectInt (V2 tx ty) (V2 tw th) = rect
            SDL.copy renderer hexTex Nothing
              (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral tx) (fromIntegral ty)))
                                   (V2 (fromIntegral tw) (fromIntegral th))))
        SDL.destroyTexture hexTex

-- | Preview overlay opacity factor (0–1).
previewAlpha :: Float
previewAlpha = 0.3

-- | Colour for each editor tool's brush preview.
toolColor :: EditorTool -> (Word8, Word8, Word8)
toolColor ToolRaise       = (100, 220, 100)   -- green
toolColor ToolLower       = (220, 100, 100)   -- red
toolColor ToolSmooth      = (100, 200, 220)   -- cyan
toolColor ToolFlatten     = (220, 220, 100)   -- yellow
toolColor ToolNoise       = (180, 100, 220)   -- purple
toolColor ToolPaintBiome  = (100, 180, 180)   -- teal
toolColor ToolPaintForm   = (220, 160, 80)    -- orange
toolColor ToolSetHardness = (200, 140, 60)    -- amber
toolColor ToolErode       = (150, 110, 70)    -- earth
