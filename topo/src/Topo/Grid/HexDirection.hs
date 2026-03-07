{-# LANGUAGE BangPatterns #-}

-- | Shared helpers for projecting continuous wind directions onto the
-- six discrete directions of the row-major axial hex lattice.
module Topo.Grid.HexDirection
  ( directionVector
  , directionalAlignment
  , foldHexDirections
  , foldHexNeighbors
  , countHexNeighbors
  , nearestHexDirection
  , sampleUpwindHex
  , stepIndexInDirection
  , traceIndexInDirection
  ) where

import Topo.Hex
  ( HexDirection(..)
  , hexNeighborIndexInDirection
  , hexOpposite
  )

-- | Approximate Euclidean direction vector for a hex edge direction in the
-- row-major axial embedding used by dense grid buffers.
directionVector :: HexDirection -> (Float, Float)
directionVector dir =
  case dir of
    HexE -> (1, 0)
    HexNE -> (0.5, negate hexHalfSqrt3)
    HexNW -> (-0.5, negate hexHalfSqrt3)
    HexW -> (-1, 0)
    HexSW -> (-0.5, hexHalfSqrt3)
    HexSE -> (0.5, hexHalfSqrt3)

-- | Dot-product alignment between a continuous direction vector and a
-- discrete hex edge direction.
directionalAlignment :: Float -> Float -> HexDirection -> Float
directionalAlignment windX windY dir =
  let (dirX, dirY) = directionVector dir
  in windX * dirX + windY * dirY

-- | Strict left fold over the six canonical hex directions.
foldHexDirections :: (a -> HexDirection -> a) -> a -> a
foldHexDirections step initial =
  let !acc1 = step initial HexE
      !acc2 = step acc1 HexNE
      !acc3 = step acc2 HexNW
      !acc4 = step acc3 HexW
      !acc5 = step acc4 HexSW
      !acc6 = step acc5 HexSE
  in acc6

-- | Fold over the in-bounds neighbours of a tile without allocating an
-- intermediate neighbour list.
foldHexNeighbors :: Int -> Int -> Int -> (a -> Int -> a) -> a -> a
foldHexNeighbors gridWidth gridHeight idx step =
  foldHexDirections visit
  where
    visit acc dir =
      case hexNeighborIndexInDirection gridWidth gridHeight dir idx of
        Just neighborIdx -> step acc neighborIdx
        Nothing -> acc

-- | Count the in-bounds neighbours of a tile without materialising them.
countHexNeighbors :: Int -> Int -> Int -> Int
countHexNeighbors gridWidth gridHeight idx =
  foldHexNeighbors gridWidth gridHeight idx (\acc _ -> acc + 1) 0

-- | Pick the single hex direction whose embedded direction vector best
-- matches a continuous angle.
nearestHexDirection :: Float -> HexDirection
nearestHexDirection angle =
  let windX = cos angle
      windY = sin angle
      choose best dir
        | directionalAlignment windX windY dir > directionalAlignment windX windY best = dir
        | otherwise = best
  in foldHexDirections choose HexE

-- | Blend values from the upwind hex neighbours according to how strongly
-- each neighbour aligns with the continuous incoming wind direction.
sampleUpwindHex :: Int -> Int -> (Int -> Float) -> Int -> Float -> Float
sampleUpwindHex gridWidth gridHeight sampleAt idx angle =
  let center = sampleAt idx
      windX = cos angle
      windY = sin angle
      accumulate (!weightedSum, !totalAlignment) dir =
        let alignment = directionalAlignment windX windY dir
        in if alignment > 0
             then ( weightedSum + alignment * sampleFromDirection center dir
                  , totalAlignment + alignment
                  )
             else (weightedSum, totalAlignment)
      (weightedSum, totalAlignment) = foldHexDirections accumulate (0, 0)
  in if totalAlignment <= 0
       then center
       else weightedSum / totalAlignment
  where
    sampleFromDirection center dir =
      case hexNeighborIndexInDirection gridWidth gridHeight (hexOpposite dir) idx of
        Just upwindIdx -> sampleAt upwindIdx
        Nothing -> center

-- | Step one tile in a hex direction, clamping at the grid boundary by
-- staying on the current tile when the neighbour is off-grid.
stepIndexInDirection :: Int -> Int -> HexDirection -> Int -> Int
stepIndexInDirection gridWidth gridHeight dir idx =
  case hexNeighborIndexInDirection gridWidth gridHeight dir idx of
    Just nextIdx -> nextIdx
    Nothing -> idx

-- | Walk a fixed number of hex steps in one direction, stopping at the
-- boundary when necessary.
traceIndexInDirection :: Int -> Int -> HexDirection -> Int -> Int -> Int
traceIndexInDirection gridWidth gridHeight dir steps startIdx =
  go (max 0 steps) startIdx
  where
    go 0 currentIdx = currentIdx
    go remaining currentIdx =
      go (remaining - 1) (stepIndexInDirection gridWidth gridHeight dir currentIdx)

hexHalfSqrt3 :: Float
hexHalfSqrt3 = sqrt 3 / 2