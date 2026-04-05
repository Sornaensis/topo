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
import Topo (ChunkCoord(..), ChunkId(..), WorldConfig(..), chunkIdFromCoord)
import UI.HexPick (renderHexRadiusPx, hexOriginX, hexOriginY)

-- | Return the 'IntMap' keys of chunks whose coordinates place them in or
-- near the camera viewport.
--
-- Instead of testing every chunk in the map (O(n)), this function inverts
-- the 'axialToScreen' mapping to compute the range of chunk coordinates
-- that could overlap the viewport, then probes the IntMap for each
-- candidate (O(k log n) where k = visible area in chunk units).
visibleChunkKeys
  :: WorldConfig
  -> (Float, Float)   -- ^ Camera pan offset @(panX, panY)@
  -> Float            -- ^ Camera zoom (clamped to @>= 0.001@)
  -> (Int, Int)       -- ^ Window size @(width, height)@
  -> IntMap.IntMap a   -- ^ Chunk map (only keys are inspected)
  -> [Int]
visibleChunkKeys config (panX, panY) zoom (winW, winH) chunks =
  let zoom'    = max 0.001 zoom
      -- Viewport bounds in world space with generous padding
      pad      = fromIntegral (wcChunkSize config)
               * (fromIntegral renderHexRadiusPx :: Float) * 2.0
      wLeft    = -panX - pad
      wRight   = fromIntegral winW / zoom' - panX + pad
      wTop     = -panY - pad
      wBot     = fromIntegral winH / zoom' - panY + pad
      -- Invert axialToScreen to get tile coordinate bounds
      -- axialToScreen: x = s*sqrt(3)*(q + r/2) + oxF
      --                y = s*1.5*r + oyF
      s        = fromIntegral renderHexRadiusPx :: Float
      cs       = fromIntegral (wcChunkSize config) :: Float
      oxF      = fromIntegral hexOriginX :: Float
      oyF      = fromIntegral hexOriginY :: Float
      -- r range from y bounds
      rMin     = (wTop - oyF) / (s * 1.5)
      rMax     = (wBot - oyF) / (s * 1.5)
      -- q range from x bounds (accounting for r/2 offset)
      sq3s     = s * sqrt 3
      qMin     = (wLeft - oxF) / sq3s - rMax / 2
      qMax     = (wRight - oxF) / sq3s - rMin / 2
      -- Convert tile ranges to chunk coordinate ranges (extra ±1 for safety)
      cxMin    = floor (qMin / cs) - 1 :: Int
      cxMax    = ceiling (qMax / cs) + 1 :: Int
      cyMin    = floor (rMin / cs) - 1 :: Int
      cyMax    = ceiling (rMax / cs) + 1 :: Int
  in [ k
     | cy <- [cyMin .. cyMax]
     , cx <- [cxMin .. cxMax]
     , let ChunkId k = chunkIdFromCoord (ChunkCoord cx cy)
     , IntMap.member k chunks
     ]
