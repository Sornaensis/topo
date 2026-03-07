{-# LANGUAGE BangPatterns #-}

-- | Shared hex-neighbour diffusion helpers for contiguous tile grids.
module Topo.Grid.Diffusion
  ( diffuseFieldGrid
  , diffuseAngleFieldGrid
  , coastalProximityGrid
  , diffuseOnceGrid
  , diffuseAtGrid
  ) where

import Topo.Grid.HexDirection (countHexNeighbors, foldHexNeighbors)
import Topo.Math (clamp01, iterateN)
import qualified Data.Vector.Unboxed as U

-- | Diffuse a scalar field over a row-major axial grid for @iterations@ steps.
diffuseFieldGrid :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseFieldGrid gridW gridH iterations factor field =
  iterateN iterations (diffuseOnceGrid gridW gridH factor) field

-- | Diffuse an angular field over a row-major axial grid for @iterations@
-- steps using circular averaging and shortest-arc interpolation.
diffuseAngleFieldGrid :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseAngleFieldGrid gridW gridH iterations factor field =
  iterateN iterations (diffuseAngleOnceGrid gridW gridH factor) field

-- | Build a coastal-proximity field by diffusing an ocean mask.
coastalProximityGrid :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
coastalProximityGrid = diffuseFieldGrid

-- | Apply one hex-neighbour diffusion step over a row-major axial grid.
diffuseOnceGrid :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseOnceGrid gridW gridH factor field =
  U.generate (gridW * gridH) (diffuseAtGrid gridW gridH factor field)

-- | Apply one hex-neighbour circular diffusion step over a row-major
-- axial grid.
diffuseAngleOnceGrid :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseAngleOnceGrid gridW gridH factor field =
  U.generate (gridW * gridH) (diffuseAngleAtGrid gridW gridH factor field)

-- | Diffuse a single tile toward the mean of its in-bounds hex neighbours.
--
-- Missing neighbours beyond the grid boundary are omitted rather than
-- replaced by the center tile, so edge tiles diffuse against their actual
-- local hex neighbourhood.
diffuseAtGrid :: Int -> Int -> Float -> U.Vector Float -> Int -> Float
diffuseAtGrid gridW gridH factor field i =
  let c = field U.! i
      neighborSum = foldHexNeighbors gridW gridH i (\acc j -> acc + field U.! j) 0
      neighborCount = countHexNeighbors gridW gridH i
      avg = (c + neighborSum) / fromIntegral (1 + neighborCount)
  in clamp01 (c * (1 - factor) + avg * factor)

-- | Diffuse a single angular tile toward the circular mean of its
-- in-bounds hex neighbours.
diffuseAngleAtGrid :: Int -> Int -> Float -> U.Vector Float -> Int -> Float
diffuseAngleAtGrid gridW gridH factor field i =
  let current = normalizeAngle (field U.! i)
      accumulate (!sumX, !sumY) j =
        let angle = normalizeAngle (field U.! j)
        in (sumX + cos angle, sumY + sin angle)
      (avgX, avgY) =
        foldHexNeighbors gridW gridH i accumulate (cos current, sin current)
      averageAngle = atan2 avgY avgX
  in lerpAngle current averageAngle factor

normalizeAngle :: Float -> Float
normalizeAngle angle =
  let tau = 2 * pi
      wrapped = angle - tau * fromIntegral (floor (angle / tau) :: Int)
  in if wrapped < 0 then wrapped + tau else wrapped

lerpAngle :: Float -> Float -> Float -> Float
lerpAngle start end t =
  let delta = atan2 (sin (end - start)) (cos (end - start))
      clampedT = clamp01 t
  in normalizeAngle (start + clampedT * delta)
