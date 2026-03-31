{-# LANGUAGE BangPatterns #-}

-- | Brush-stroke application for the terrain editor.
--
-- Applies a circular brush centred on a hex tile, modifying
-- 'TerrainChunk' elevation vectors in-place using the configured
-- 'BrushSettings' and 'EditorTool'.
module Seer.Editor.Brush
  ( applyBrushStroke
  , brushWeight
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Topo (ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Hex (hexDisc, hexDistance)
import Topo.Math (clamp01)
import Topo.Types (HexCoord(..), TerrainChunk(..))
import Seer.Editor.Types (BrushSettings(..), EditorTool(..), Falloff(..))

-- | Compute the brush weight for a tile at @dist@ hexes from center,
-- given a brush @radius@ and 'Falloff' function.
--
-- Returns 0 when @dist > radius@.
brushWeight :: Falloff -> Int -> Int -> Float
brushWeight _falloff radius dist
  | dist > radius = 0
  | radius == 0   = 1
brushWeight falloff radius dist =
  let t = fromIntegral dist / fromIntegral radius
  in case falloff of
       FalloffConstant -> 1
       FalloffLinear   -> 1 - t
       FalloffSmooth   -> 0.5 * (1 + cos (pi * t))
{-# INLINE brushWeight #-}

-- | Apply a single brush stroke at @(q, r)@ to the terrain chunks.
--
-- For each tile in the brush disc, the elevation is adjusted by
-- @± strength × weight@ depending on the 'EditorTool'.  The result
-- is clamped to @[0, 1]@.
--
-- Returns the modified 'IntMap' of terrain chunks (only touched
-- chunks are copied).
applyBrushStroke
  :: WorldConfig
  -> EditorTool
  -> BrushSettings
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyBrushStroke cfg tool brush (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      !strength = brushStrength brush
      !falloff = brushFalloff brush
      sign = case tool of
        ToolRaise -> 1
        ToolLower -> -1
      disc = hexDisc center radius
  in foldl' (applyToTile cfg sign strength falloff center radius) chunks disc
  where
    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' f !z []     = z
    foldl' f !z (x:xs) = let !z' = f z x in foldl' f z' xs

applyToTile
  :: WorldConfig
  -> Float         -- ^ sign (+1 raise, −1 lower)
  -> Float         -- ^ brush strength
  -> Falloff
  -> HexCoord      -- ^ brush center
  -> Int           -- ^ brush radius
  -> IntMap TerrainChunk
  -> HexCoord      -- ^ tile to modify
  -> IntMap TerrainChunk
applyToTile cfg sign strength falloff center radius chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist = hexDistance center tile
                    !w = brushWeight falloff radius dist
                    !delta = sign * strength * w
                    !old = elev `U.unsafeIndex` idx
                    !new = clamp01 (old + delta)
                    !elev' = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks
