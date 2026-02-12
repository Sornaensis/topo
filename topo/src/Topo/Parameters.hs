{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Derived terrain parameter layers.
--
-- Computes slope, curvature, relief, ruggedness, terrain form classification,
-- rock\/soil types, roughness, soil depth, and fertility from elevation and
-- other base terrain fields.
--
-- Cross-chunk neighbor lookups are used so that tile stencils produce correct
-- values at chunk boundaries.
--
-- __Hardness note:__ Tectonics-computed @tcHardness@ is preserved (not
-- recomputed).  Soil depth is derived from the Tectonics value.
module Topo.Parameters
  ( ParameterConfig(..)
  , defaultParameterConfig
  , TerrainFormConfig(..)
  , defaultTerrainFormConfig
  , applyParameterLayersStage
  -- * Stencil functions (exported for testing)
  , mkElevLookup
  , slopeAt
  , curvatureAt
  , reliefAt
  , ruggednessAt
  , isLocalMinimum
  , classifyTerrainForm
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.ST (runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Word (Word16)
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Parameter derivation configuration.
data ParameterConfig = ParameterConfig
  { pcDetailScale              :: !Float
  -- ^ Scale factor for noise details (unused after hardness removal, kept
  --   for future procedural layers).
  , pcRoughnessScale           :: !Float
  , pcRockElevationThreshold   :: !Float
  , pcRockHardnessThreshold    :: !Float
  , pcRockHardnessSecondary    :: !Float
  } deriving (Eq, Show)

-- | Default parameter configuration.
defaultParameterConfig :: ParameterConfig
defaultParameterConfig = ParameterConfig
  { pcDetailScale             = 1
  , pcRoughnessScale          = 0.75
  , pcRockElevationThreshold  = 0.6
  , pcRockHardnessThreshold   = 0.6
  , pcRockHardnessSecondary   = 0.45
  }

-- | Terrain form classification thresholds.
data TerrainFormConfig = TerrainFormConfig
  { tfcCliffSlope      :: !Float
  -- ^ Slope above which terrain is classified as 'FormCliff'.
  , tfcMountainSlope   :: !Float
  -- ^ Slope threshold for 'FormMountainous'.
  , tfcMountainRelief  :: !Float
  -- ^ Relief threshold for 'FormMountainous'.
  , tfcValleyCurvature :: !Float
  -- ^ Curvature magnitude for 'FormValley' (positive value; compared as
  --   curvature < -this).
  , tfcHillSlope       :: !Float
  -- ^ Slope threshold for 'FormHilly'.
  , tfcHillRelief      :: !Float
  -- ^ Relief threshold for 'FormHilly'.
  , tfcRollingSlope    :: !Float
  -- ^ Slope threshold for 'FormRolling'.
  } deriving (Eq, Show)

-- | Default terrain form thresholds.
defaultTerrainFormConfig :: TerrainFormConfig
defaultTerrainFormConfig = TerrainFormConfig
  { tfcCliffSlope      = 0.40
  , tfcMountainSlope   = 0.20
  , tfcMountainRelief  = 0.25
  , tfcValleyCurvature = 0.15
  , tfcHillSlope       = 0.08
  , tfcHillRelief      = 0.10
  , tfcRollingSlope    = 0.02
  }

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Compute derived parameter layers from terrain fields.
--
-- The full @IntMap TerrainChunk@ is threaded into each chunk's derivation
-- so that stencil functions can read neighbor elevations across chunk
-- boundaries.
applyParameterLayersStage :: ParameterConfig -> TerrainFormConfig -> PipelineStage
applyParameterLayersStage cfg formCfg =
    PipelineStage "applyParameterLayers" "applyParameterLayers" $ do
  logInfo "applyParameterLayers: deriving fields"
  _seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        chunks = twTerrain world
        terrain' = IntMap.mapWithKey
          (deriveChunk config chunks cfg formCfg) chunks
    in world { twTerrain = terrain' }

deriveChunk
  :: WorldConfig
  -> IntMap TerrainChunk
  -> ParameterConfig
  -> TerrainFormConfig
  -> Int
  -> TerrainChunk
  -> TerrainChunk
deriveChunk config chunks cfg formCfg key chunk =
  let origin@(TileCoord _ox _oy) =
        chunkOriginTile config (chunkCoordFromId (ChunkId key))
      size    = wcChunkSize config
      elev    = tcElevation chunk
      n       = U.length elev
      padded  = size + 2

      -- Phase 7.2: Build padded elevation buffer once per chunk.
      -- Interior tiles use the fast local-vector path; only the 1-tile
      -- boundary ring (≤ 4·(size+1) tiles) falls through to IntMap.
      !paddedBuf = mkPaddedElevation chunks config origin elev

      -- Phase 7.1: Fused single-pass stencil derivation.
      -- Reads center + 8 neighbors = 9 array reads per tile (was ~36
      -- closure calls across 5 separate U.generate passes).
      !(slope, curvature, relief, rugged, terrForm) = runST $ do
        slopeV  <- UM.new n
        curvV   <- UM.new n
        reliefV <- UM.new n
        ruggedV <- UM.new n
        formV   <- UM.new n
        let {-# INLINE p #-}
            p !row !col = U.unsafeIndex paddedBuf (row * padded + col)
            go !i
              | i >= n    = pure ()
              | otherwise = do
                  let !lx     = i `mod` size
                      !ly     = i `div` size
                      -- Padded coords: center at (ly+1, lx+1)
                      !e0     = p (ly + 1) (lx + 1)
                      !eLeft  = p (ly + 1)  lx
                      !eRight = p (ly + 1) (lx + 2)
                      !eUp    = p  ly      (lx + 1)
                      !eDown  = p (ly + 2) (lx + 1)
                      !eNW    = p  ly       lx
                      !eNE    = p  ly      (lx + 2)
                      !eSW    = p (ly + 2)  lx
                      !eSE    = p (ly + 2) (lx + 2)
                      -- Slope (central-difference, 4 axial)
                      !dx     = (eRight - eLeft) * 0.5
                      !dy     = (eDown - eUp) * 0.5
                      !s      = sqrt (dx * dx + dy * dy)
                      -- Curvature (Laplacian, 4 axial)
                      !c      = eLeft + eRight + eUp + eDown - 4 * e0
                      -- Relief (range over 9-cell neighborhood)
                      !lo     = min e0 $ min eLeft $ min eRight $ min eUp
                              $ min eDown $ min eNW $ min eNE $ min eSW eSE
                      !hi     = max e0 $ max eLeft $ max eRight $ max eUp
                              $ max eDown $ max eNW $ max eNE $ max eSW eSE
                      !r      = hi - lo
                      -- Ruggedness (mean abs diff to 8 neighbors)
                      !tri    = ( abs (eNW - e0) + abs (eUp - e0)
                                + abs (eNE - e0) + abs (eLeft - e0)
                                + abs (eRight - e0) + abs (eSW - e0)
                                + abs (eDown - e0) + abs (eSE - e0)
                                ) / 8.0
                      -- Local minimum (all 8 neighbors ≥ center)
                      !localMin = eNW >= e0 && eUp >= e0 && eNE >= e0
                               && eLeft >= e0 && eRight >= e0
                               && eSW >= e0 && eDown >= e0 && eSE >= e0
                      !tf     = classifyTerrainForm formCfg s r c localMin
                  UM.unsafeWrite slopeV  i s
                  UM.unsafeWrite curvV   i c
                  UM.unsafeWrite reliefV i r
                  UM.unsafeWrite ruggedV i tri
                  UM.unsafeWrite formV   i tf
                  go (i + 1)
        go 0
        (,,,,) <$> U.unsafeFreeze slopeV
               <*> U.unsafeFreeze curvV
               <*> U.unsafeFreeze reliefV
               <*> U.unsafeFreeze ruggedV
               <*> U.unsafeFreeze formV

      -- Non-stencil derived fields.
      hardness  = tcHardness chunk
      rockType  = U.generate n (rockTypeAt cfg elev hardness)
      roughness = U.map (clamp01 . (* pcRoughnessScale cfg) . abs) curvature

  in chunk
      { tcSlope       = slope
      , tcCurvature   = curvature
      , tcRockType    = rockType
      , tcRoughness   = roughness
      , tcRelief      = relief
      , tcRuggedness  = rugged
      , tcTerrainForm = terrForm
      }

---------------------------------------------------------------------------
-- Padded elevation buffer
---------------------------------------------------------------------------

-- | Build a padded @(size+2) × (size+2)@ elevation buffer that includes
-- a 1-tile boundary ring from neighboring chunks.  Interior tiles use the
-- fast local-vector path; only the boundary ring (≤ @4·(size+1)@ tiles)
-- falls through to @IntMap@ lookups.
{-# INLINE mkPaddedElevation #-}
mkPaddedElevation
  :: IntMap TerrainChunk
  -> WorldConfig
  -> TileCoord          -- ^ chunk origin in global tile space
  -> U.Vector Float     -- ^ local elevation vector
  -> U.Vector Float     -- ^ padded buffer, row-major, origin at (-1,-1)
mkPaddedElevation chunks config origin@(TileCoord ox oy) localElev =
  let size   = wcChunkSize config
      padded = size + 2
      elevAt = mkElevLookup chunks config origin localElev
  in U.generate (padded * padded) $ \i ->
       let !px = i `mod` padded
           !py = i `div` padded
       in elevAt (ox + px - 1) (oy + py - 1)

---------------------------------------------------------------------------
-- Elevation lookup
---------------------------------------------------------------------------

-- | Create an elevation lookup function for a given chunk.
--
-- For tiles within the chunk the local elevation vector is used (fast
-- path, no map lookup).  For tiles outside the chunk the containing
-- chunk is looked up from the @IntMap@.  Returns @0@ (notional sea level)
-- if the neighbor chunk does not exist.
{-# INLINE mkElevLookup #-}
mkElevLookup
  :: IntMap TerrainChunk
  -> WorldConfig
  -> TileCoord          -- ^ chunk origin in global tile space
  -> U.Vector Float     -- ^ local elevation vector
  -> (Int -> Int -> Float)
mkElevLookup chunks config (TileCoord ox oy) localElev = go
  where
    size = wcChunkSize config
    go gx gy =
      let !lx = gx - ox
          !ly = gy - oy
      in if lx >= 0 && lx < size && ly >= 0 && ly < size
           then localElev U.! (ly * size + lx)
           else globalLookup gx gy
    globalLookup gx gy =
      let (cc, TileCoord lx ly) =
            chunkCoordFromTile config (TileCoord gx gy)
          ChunkId cid = chunkIdFromCoord cc
      in case IntMap.lookup cid chunks of
           Nothing    -> 0   -- sea-level fallback for world edge
           Just chunk -> tcElevation chunk U.! (ly * size + lx)

---------------------------------------------------------------------------
-- Stencil functions
---------------------------------------------------------------------------

-- | 8-neighbor offsets (N, NE, E, SE, S, SW, W, NW).
neighbors8 :: [(Int, Int)]
neighbors8 =
  [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

-- | Slope via central-difference on 4 axial neighbors.
slopeAt :: (Int -> Int -> Float) -> Int -> Int -> Float
slopeAt elevAt gx gy =
  let eLeft  = elevAt (gx - 1) gy
      eRight = elevAt (gx + 1) gy
      eUp    = elevAt gx (gy - 1)
      eDown  = elevAt gx (gy + 1)
      dx     = (eRight - eLeft) * 0.5
      dy     = (eDown  - eUp)   * 0.5
  in sqrt (dx * dx + dy * dy)

-- | Laplacian curvature from 4 axial neighbors.
curvatureAt :: (Int -> Int -> Float) -> Int -> Int -> Float
curvatureAt elevAt gx gy =
  let e0     = elevAt gx gy
      eLeft  = elevAt (gx - 1) gy
      eRight = elevAt (gx + 1) gy
      eUp    = elevAt gx (gy - 1)
      eDown  = elevAt gx (gy + 1)
  in eLeft + eRight + eUp + eDown - 4 * e0

-- | Local elevation relief: range over 8 neighbors plus center.
reliefAt :: (Int -> Int -> Float) -> Int -> Int -> Float
reliefAt elevAt gx gy =
  let !e0 = elevAt gx gy
      step (!mn, !mx) (dx, dy) =
        let !e = elevAt (gx + dx) (gy + dy)
        in (min mn e, max mx e)
      (!lo, !hi) = foldl' step (e0, e0) neighbors8
  in hi - lo

-- | Terrain Ruggedness Index: mean absolute elevation difference to
--   8 neighbors.
ruggednessAt :: (Int -> Int -> Float) -> Int -> Int -> Float
ruggednessAt elevAt gx gy =
  let !e0 = elevAt gx gy
      step !acc (dx, dy) = acc + abs (elevAt (gx + dx) (gy + dy) - e0)
      !triSum = foldl' step 0 neighbors8
  in triSum / 8.0

-- | Whether a tile is a local elevation minimum (all 8 neighbors ≥ center).
isLocalMinimum :: (Int -> Int -> Float) -> Int -> Int -> Bool
isLocalMinimum elevAt gx gy =
  let !e0 = elevAt gx gy
  in all (\(dx, dy) -> elevAt (gx + dx) (gy + dy) >= e0) neighbors8

-- | Classify terrain form from slope, relief, curvature, and local-minimum
--   flag.
--
-- Decision cascade (first match wins):
--
--   1. slope > cliff threshold     → 'FormCliff'
--   2. slope > mountain threshold
--      OR relief > mountain relief → 'FormMountainous'
--   3. curvature < −valley thresh. → 'FormValley'
--   4. local minimum               → 'FormDepression'
--   5. slope > hill & relief > hill→ 'FormHilly'
--   6. slope > rolling threshold   → 'FormRolling'
--   7. otherwise                   → 'FormFlat'
{-# INLINE classifyTerrainForm #-}
classifyTerrainForm
  :: TerrainFormConfig -> Float -> Float -> Float -> Bool -> TerrainForm
classifyTerrainForm cfg s r c localMin
  | s > tfcCliffSlope cfg                                       = FormCliff
  | s > tfcMountainSlope cfg || r > tfcMountainRelief cfg       = FormMountainous
  | c < negate (tfcValleyCurvature cfg)                         = FormValley
  | localMin                                                    = FormDepression
  | s > tfcHillSlope cfg && r > tfcHillRelief cfg               = FormHilly
  | s > tfcRollingSlope cfg                                     = FormRolling
  | otherwise                                                   = FormFlat

---------------------------------------------------------------------------
-- Derived-field helpers
---------------------------------------------------------------------------

-- | Convert a tile index within a chunk to global tile coordinates.
{-# INLINE globalCoords #-}
globalCoords :: Int -> Int -> Int -> Int -> (Int, Int)
globalCoords ox oy size i = (ox + i `mod` size, oy + i `div` size)

{-# INLINE rockTypeAt #-}
rockTypeAt :: ParameterConfig -> U.Vector Float -> U.Vector Float -> Int -> Word16
rockTypeAt cfg elev hardness i =
  let e0 = elev U.! i
      h0 = hardness U.! i
  in if e0 > pcRockElevationThreshold cfg && h0 > pcRockHardnessThreshold cfg
       then 2
       else if h0 > pcRockHardnessSecondary cfg
         then 1
         else 0

