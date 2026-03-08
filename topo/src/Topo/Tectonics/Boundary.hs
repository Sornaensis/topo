{-# LANGUAGE PatternSynonyms #-}

-- | Boundary classification and directional helpers for tectonics.
module Topo.Tectonics.Boundary
  ( classifyBoundary
  , boundaryDirection
  , boundaryTangent
  , boundaryMotionBetween
  , boundaryTypeBetween
  , boundaryStrengthAtXY
  , boundaryDistanceNormalised
  , boundaryTypeAtXY
  ) where

import Data.Word (Word64)
import Topo.Math (clamp01, smoothstep)
import Topo.Tectonics.Config (TectonicsConfig(..))
import Topo.Tectonics.PlateVoronoi
  ( PlateInfo(..)
  , plateDistancePair
  , plateInfoAtXY
  )
import Topo.Types
  ( PlateBoundary
  , pattern PlateBoundaryConvergent
  , pattern PlateBoundaryDivergent
  , pattern PlateBoundaryNone
  , pattern PlateBoundaryTransform
  )

classifyBoundary :: PlateInfo -> PlateInfo -> PlateInfo -> PlateBoundary
classifyBoundary info infoX infoY =
  let typeX = boundaryTypeBetween info infoX
      typeY = boundaryTypeBetween info infoY
  in if typeX == typeY then typeX else PlateBoundaryTransform

boundaryDirection :: PlateInfo -> PlateInfo -> PlateInfo -> (Float, Float)
boundaryDirection info infoX infoY =
  let (ax, ay) = plateInfoCenter info
      (bx, by) = plateInfoCenter infoX
      (cx, cy) = plateInfoCenter infoY
      dx = bx - ax
      dy = by - ay
      ex = cx - ax
      ey = cy - ay
      nx = dx + ex
      ny = dy + ey
      len = sqrt (nx * nx + ny * ny)
  in if len == 0 then (1, 0) else (nx / len, ny / len)

-- | Compute the tangent direction along the boundary between two plates.
boundaryTangent :: PlateInfo -> PlateInfo -> (Float, Float)
boundaryTangent infoA infoB =
  let (ax, ay) = plateInfoCenter infoA
      (bx, by) = plateInfoCenter infoB
      nx = bx - ax
      ny = by - ay
      tx = -ny
      ty = nx
      len = sqrt (tx * tx + ty * ty)
  in if len == 0 then (0, 1) else (tx / len, ty / len)

boundaryMotionBetween :: PlateInfo -> PlateInfo -> Float
boundaryMotionBetween a b =
  let (ax, ay) = plateInfoCenter a
      (bx, by) = plateInfoCenter b
      (vax, vay) = plateInfoVelocity a
      (vbx, vby) = plateInfoVelocity b
      nx0 = bx - ax
      ny0 = by - ay
      len = sqrt (nx0 * nx0 + ny0 * ny0)
      nx = if len == 0 then 0 else nx0 / len
      ny = if len == 0 then 0 else ny0 / len
      rvx = vbx - vax
      rvy = vby - vay
  in rvx * nx + rvy * ny

boundaryTypeBetween :: PlateInfo -> PlateInfo -> PlateBoundary
boundaryTypeBetween a b =
  let rel = boundaryMotionBetween a b
      eps = 0.05
  in if rel < -eps
      then PlateBoundaryConvergent
      else if rel > eps
        then PlateBoundaryDivergent
        else PlateBoundaryTransform

boundaryStrengthAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> Float
boundaryStrengthAtXY seed tcfg x y =
  smoothstep 0 1 (boundaryDistanceNormalised seed tcfg x y)

-- | Raw normalised distance from the boundary centre.
boundaryDistanceNormalised :: Word64 -> TectonicsConfig -> Int -> Int -> Float
boundaryDistanceNormalised seed tcfg x y =
  let size = fromIntegral (max 1 (tcPlateSize tcfg))
      (d0, d1) = plateDistancePair seed tcfg x y
      gap = max 0 (d1 - d0)
      sharp = max 0.2 (tcBoundarySharpness tcfg)
      width = max 1e-3 (size * (0.35 / sharp))
  in clamp01 (1 - gap / width)

boundaryTypeAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> PlateBoundary
boundaryTypeAtXY seed tcfg gx gy =
  let info = plateInfoAtXY seed tcfg gx gy
      infoX = plateInfoAtXY seed tcfg (gx + 1) gy
      infoY = plateInfoAtXY seed tcfg gx (gy + 1)
  in classifyBoundary info infoX infoY