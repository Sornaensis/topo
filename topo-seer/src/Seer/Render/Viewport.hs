-- | Viewport chunk culling shared across atlas building, overlay drawing,
-- and brush preview rendering.
--
-- All calculations operate in the base @renderHexRadiusPx = 6@ world-space
-- coordinate frame so that results are consistent regardless of the bake-time
-- hex radius used by the current zoom stage.
module Seer.Render.Viewport
  ( visibleChunkKeys
  ) where

import qualified Data.IntMap.Strict as IntMap
import Topo (ChunkCoord(..), ChunkId(..), WorldConfig(..), chunkCoordFromId)
import UI.HexPick (renderHexRadiusPx)
import UI.TerrainRender (chunkBounds)

-- | Return the 'IntMap' keys of chunks whose bounding boxes overlap the
-- camera viewport (plus a one-chunk padding ring).
--
-- The viewport is defined by the camera pan offset, zoom level, and window
-- size.  Chunk bounds are computed in the @renderHexRadiusPx@ frame.
visibleChunkKeys
  :: WorldConfig
  -> (Float, Float)   -- ^ Camera pan offset @(panX, panY)@
  -> Float            -- ^ Camera zoom (clamped to @>= 0.001@)
  -> (Int, Int)       -- ^ Window size @(width, height)@
  -> IntMap.IntMap a   -- ^ Chunk map (only keys are inspected)
  -> [Int]
visibleChunkKeys config (panX, panY) zoom (winW, winH) chunks =
  let zoom'    = max 0.001 zoom
      chunkPad = fromIntegral (wcChunkSize config)
               * (fromIntegral renderHexRadiusPx :: Float) * 2.0
      wLeft    = -panX - chunkPad
      wRight   = fromIntegral winW / zoom' - panX + chunkPad
      wTop     = -panY - chunkPad
      wBot     = fromIntegral winH / zoom' - panY + chunkPad
      isVisible k =
        let (bx, by, bx2, by2) = chunkBounds config renderHexRadiusPx
                                    (chunkCoordFromId (ChunkId k))
        in  fromIntegral bx2 > wLeft && fromIntegral bx < wRight
         && fromIntegral by2 > wTop  && fromIntegral by < wBot
  in filter isVisible (IntMap.keys chunks)
