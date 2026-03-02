{-# LANGUAGE BangPatterns #-}

-- | Shared terrain-form neighbourhood metrics.
--
-- This module provides deterministic, total metric derivation over a
-- hex-neighbourhood accessor.  It is used by both pre-erosion grid
-- classification and final per-chunk parameter derivation so both stages
-- consume identical terrain semantics.
module Topo.TerrainForm.Metrics
  ( TerrainNeighborhood(..)
  , terrainNeighborhoodAt
  , slopeAt
  , curvatureAt
  , reliefAt
  , ruggednessAt
  , isLocalMinimum
  ) where

import Data.List (foldl')
import Topo.Types (DirectionalSlope(..))

-- | Terrain neighbourhood descriptors for one tile.
data TerrainNeighborhood = TerrainNeighborhood
  { tnElevation      :: !Float
  , tnDirectionalSlope :: !DirectionalSlope
  , tnCurvature      :: !Float
  , tnRelief         :: !Float
  , tnRelief2Ring    :: !Float
  , tnRelief3Ring    :: !Float
  , tnRuggedness     :: !Float
  , tnIsLocalMinimum :: !Bool
  } deriving (Eq, Show)

-- | Derive all terrain neighbourhood metrics at a coordinate.
terrainNeighborhoodAt :: (Int -> Int -> Float) -> Int -> Int -> TerrainNeighborhood
terrainNeighborhoodAt elevAt gx gy =
  let !e0 = elevAt gx gy
      !ds = slopeAt elevAt gx gy
      !cv = curvatureAt elevAt gx gy
      !r1 = reliefAt elevAt gx gy
      !r2 = reliefWithOffsets elevAt gx gy ring2InclusiveOffsets
      !r3 = reliefWithOffsets elevAt gx gy ring3InclusiveOffsets
      !tri = ruggednessAt elevAt gx gy
      !isMin = isLocalMinimum elevAt gx gy
  in TerrainNeighborhood
       { tnElevation = e0
       , tnDirectionalSlope = ds
       , tnCurvature = cv
       , tnRelief = r1
       , tnRelief2Ring = r2
       , tnRelief3Ring = r3
       , tnRuggedness = tri
       , tnIsLocalMinimum = isMin
       }

-- | Compute directional slope to all 6 hex neighbours.
slopeAt :: (Int -> Int -> Float) -> Int -> Int -> DirectionalSlope
slopeAt elevAt gx gy =
  let !e0 = elevAt gx gy
  in DirectionalSlope
       (elevAt (gx + 1)  gy      - e0)
       (elevAt (gx + 1) (gy - 1) - e0)
       (elevAt  gx      (gy - 1) - e0)
       (elevAt (gx - 1)  gy      - e0)
       (elevAt (gx - 1) (gy + 1) - e0)
       (elevAt  gx      (gy + 1) - e0)

-- | Laplacian curvature from 6 hex neighbours.
curvatureAt :: (Int -> Int -> Float) -> Int -> Int -> Float
curvatureAt elevAt gx gy =
  let !e0 = elevAt gx gy
  in elevAt (gx + 1)  gy
   + elevAt (gx + 1) (gy - 1)
   + elevAt  gx      (gy - 1)
   + elevAt (gx - 1)  gy
   + elevAt (gx - 1) (gy + 1)
   + elevAt  gx      (gy + 1)
   - 6 * e0

-- | Local elevation relief: range over 6 hex neighbours plus center.
reliefAt :: (Int -> Int -> Float) -> Int -> Int -> Float
reliefAt elevAt gx gy = reliefWithOffsets elevAt gx gy ring1InclusiveOffsets

-- | Terrain Ruggedness Index: mean absolute elevation difference to 6 hex
-- neighbours.
ruggednessAt :: (Int -> Int -> Float) -> Int -> Int -> Float
ruggednessAt elevAt gx gy =
  let !e0 = elevAt gx gy
      step !acc (dx, dy) = acc + abs (elevAt (gx + dx) (gy + dy) - e0)
      !triSum = foldl' step 0 hexNeighborOffsets
  in triSum / 6.0

-- | Whether a tile is a local elevation minimum.
isLocalMinimum :: (Int -> Int -> Float) -> Int -> Int -> Bool
isLocalMinimum elevAt gx gy =
  let !e0 = elevAt gx gy
  in all (\(dx, dy) -> elevAt (gx + dx) (gy + dy) >= e0) hexNeighborOffsets

hexNeighborOffsets :: [(Int, Int)]
hexNeighborOffsets =
  [ ( 1,  0), ( 1, -1), ( 0, -1)
  , (-1,  0), (-1,  1), ( 0,  1)
  ]

ring1InclusiveOffsets :: [(Int, Int)]
ring1InclusiveOffsets = inclusiveOffsetsAtDistance 1

ring2InclusiveOffsets :: [(Int, Int)]
ring2InclusiveOffsets = inclusiveOffsetsAtDistance 2

ring3InclusiveOffsets :: [(Int, Int)]
ring3InclusiveOffsets = inclusiveOffsetsAtDistance 3

inclusiveOffsetsAtDistance :: Int -> [(Int, Int)]
inclusiveOffsetsAtDistance radius =
  [ (dq, dr)
  | dq <- [-radius .. radius]
  , dr <- [-radius .. radius]
  , hexDistance dq dr <= radius
  ]

hexDistance :: Int -> Int -> Int
hexDistance dq dr =
  maximum [abs dq, abs dr, abs (dq + dr)]

reliefWithOffsets :: (Int -> Int -> Float) -> Int -> Int -> [(Int, Int)] -> Float
reliefWithOffsets elevAt gx gy offsets =
  let step (!mn, !mx) (dx, dy) =
        let !e = elevAt (gx + dx) (gy + dy)
        in (min mn e, max mx e)
      !e0 = elevAt gx gy
      (!lo, !hi) = foldl' step (e0, e0) offsets
  in hi - lo
