{-# LANGUAGE BangPatterns #-}

-- | Brush-stroke application for the terrain editor.
--
-- Applies a circular brush centred on a hex tile, modifying
-- 'TerrainChunk' elevation vectors in-place using the configured
-- 'BrushSettings' and 'EditorTool'.
module Seer.Editor.Brush
  ( applyBrushStroke
  , applySmoothStroke
  , applyFlattenStroke
  , applyNoiseStroke
  , brushWeight
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Topo (ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Hex (hexDisc, hexDistance, hexNeighbors)
import Topo.Math (clamp01)
import Topo.Noise (fbm2D, hashSeed)
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

-- ---------------------------------------------------------------------------
-- Smooth tool
-- ---------------------------------------------------------------------------

-- | Apply a smooth stroke: each affected tile's elevation moves toward
-- the weighted average of its 6 hex neighbours.  Iterated @passes@
-- times per invocation.
--
-- @newElev = lerp (strength × weight) currentElev neighbourAvg@
applySmoothStroke
  :: WorldConfig
  -> BrushSettings
  -> Int
     -- ^ Number of smooth passes (1–5).
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applySmoothStroke cfg brush passes (cq, cr) chunks0 =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in iterate (smoothPass cfg brush center radius disc) chunks0 !! clampPasses passes

-- | Clamp pass count to [1, 5].
clampPasses :: Int -> Int
clampPasses n
  | n < 1     = 1
  | n > 5     = 5
  | otherwise = n
{-# INLINE clampPasses #-}

smoothPass
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int
  -> [HexCoord]
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
smoothPass cfg brush center radius disc chunks =
  foldl' (smoothTile cfg brush center radius chunks) chunks disc

smoothTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int
  -> IntMap TerrainChunk  -- ^ snapshot (read from)
  -> IntMap TerrainChunk  -- ^ accumulator (write to)
  -> HexCoord
  -> IntMap TerrainChunk
smoothTile cfg brush center radius snapshot acc tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key acc of
       Nothing -> acc
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then acc
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    !old    = elev `U.unsafeIndex` idx
                    !avg    = neighbourAverage cfg snapshot tile
                    !new    = clamp01 (old + (avg - old) * str * w)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' acc

-- | Average elevation of the 6 hex neighbours.  Missing neighbours
-- (out of bounds / unloaded chunks) are excluded from the average.
neighbourAverage :: WorldConfig -> IntMap TerrainChunk -> HexCoord -> Float
neighbourAverage cfg chunks tile =
  let nbrs = hexNeighbors tile
      (total, count) = foldl' accumNeighbour (0, 0 :: Int) nbrs
  in if count == 0 then 0 else total / fromIntegral count
  where
    accumNeighbour (!s, !n) nbr =
      case lookupTileElevation cfg chunks nbr of
        Nothing -> (s, n)
        Just e  -> (s + e, n + 1)

-- | Look up the elevation of a single hex tile in the chunk map.
lookupTileElevation :: WorldConfig -> IntMap TerrainChunk -> HexCoord -> Maybe Float
lookupTileElevation cfg chunks (HexAxial tq tr) =
  let (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> Nothing
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then Nothing
              else Just (elev `U.unsafeIndex` idx)
lookupTileElevation _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Flatten tool
-- ---------------------------------------------------------------------------

-- | Apply a flatten stroke: blend elevation toward @refElev@ (the
-- reference height captured at the start of the stroke).
applyFlattenStroke
  :: WorldConfig
  -> BrushSettings
  -> Float
     -- ^ Reference elevation captured at stroke start.
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyFlattenStroke cfg brush refElev (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in foldl' (flattenTile cfg brush center radius refElev) chunks disc

flattenTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
flattenTile cfg brush center radius refElev chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             elev   = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    !old    = elev `U.unsafeIndex` idx
                    !new    = clamp01 (old + (refElev - old) * str * w)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks

-- ---------------------------------------------------------------------------
-- Noise tool
-- ---------------------------------------------------------------------------

-- | Apply a noise stroke: add coherent noise perturbation to elevation.
--
-- Uses 'fbm2D' with the world seed mixed with @strokeId@ so each
-- stroke produces a unique but deterministic noise pattern.
applyNoiseStroke
  :: WorldConfig
  -> BrushSettings
  -> Word64
     -- ^ World seed.
  -> Word64
     -- ^ Stroke ID (monotonically increasing).
  -> Float
     -- ^ Noise frequency (0.5–4.0).
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyNoiseStroke cfg brush worldSeed strokeId freq (cq, cr) chunks =
  let !center   = HexAxial cq cr
      !radius   = brushRadius brush
      !noiseSeed = hashSeed worldSeed strokeId
      disc      = hexDisc center radius
  in foldl' (noiseTile cfg brush center radius noiseSeed freq) chunks disc

noiseTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> Word64 -> Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
noiseTile cfg brush center radius noiseSeed freq chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             elev   = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    -- Scale tile coords by frequency for noise sampling
                    !nx     = fromIntegral tq * freq * 0.1
                    !ny     = fromIntegral tr * freq * 0.1
                    -- 4-octave FBM, lacunarity 2.0, gain 0.5
                    !noise  = fbm2D noiseSeed 4 2.0 0.5 nx ny
                    -- Centre noise around 0: fbm2D returns [−range,+range]
                    !delta  = noise * str * w
                    !old    = elev `U.unsafeIndex` idx
                    !new    = clamp01 (old + delta)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks
