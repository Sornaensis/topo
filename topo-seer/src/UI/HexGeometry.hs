module UI.HexGeometry
  ( renderHexRadiusPx
  , hexOriginX
  , hexOriginY
  , hexOriginF
  , hexApothem
  , hexCenterF
  , axialToScreen
  , screenToAxial
  , screenToAxialF
  , screenPixelToWorld
  , screenPixelToAxial
  , worldPointToAxial
  , pointInHex
  , pointInHexF
  , hexCornerOffsets
  , hexSeamGutterOverlapPx
  , hexSeamCornerOffsets
  , hexFillRectAt
  , hexChunkBoundsF
  , hexChunkBounds
  , normalizeHexBounds
  , transformWorldRect
  , transformWorldPoint
  , hexSpanTextureSize
  ) where

import Linear (V2(..))
import UI.Widgets (Rect(..))

-- | Canonical on-screen hex radius for semantic terrain geometry.
--
-- This is a viewer pixel setting, intentionally separate from the physical
-- world scale carried by 'Topo.Hex.HexGridMeta'.  Render fill, picking,
-- hover, brush preview, atlas normalisation, and camera transforms should use
-- this semantic radius unless they are explicitly working in a zoom-stage
-- bake frame.
renderHexRadiusPx :: Int
renderHexRadiusPx = 6

-- | Fixed pixel offset added to all hex world-space X coordinates.
hexOriginX :: Int
hexOriginX = 40

-- | Fixed pixel offset added to all hex world-space Y coordinates.
hexOriginY :: Int
hexOriginY = 80

hexOriginF :: (Float, Float)
hexOriginF = (fromIntegral hexOriginX, fromIntegral hexOriginY)

-- | Pointy-top hex apothem (centre to vertical edge) for a radius.
hexApothem :: Int -> Float
hexApothem size = fromIntegral size * sqrt 3 / 2

-- | Continuous centre of an axial hex in the world-pixel coordinate frame.
hexCenterF :: Int -> Int -> Int -> (Float, Float)
hexCenterF size q r =
  let s = fromIntegral size :: Float
      (ox, oy) = hexOriginF
      x = ox + s * sqrt 3 * (fromIntegral q + fromIntegral r / 2)
      y = oy + s * 1.5 * fromIntegral r
  in (x, y)

-- | Integer centre used by legacy callers that need pixel coordinates.
-- Continuous geometry should prefer 'hexCenterF' and defer rounding until the
-- final raster or copy rectangle.
axialToScreen :: Int -> Int -> Int -> (Int, Int)
axialToScreen size q r =
  let (x, y) = hexCenterF size q r
  in (round x, round y)

-- | Pick a hex from an integer world pixel.  The sample location is the pixel
-- centre @(sx + 0.5, sy + 0.5)@.
screenToAxial :: Int -> Int -> Int -> (Int, Int)
screenToAxial size sx sy =
  screenToAxialF size (fromIntegral sx + 0.5) (fromIntegral sy + 0.5)

-- | Pick a hex from a continuous world-space sample point.
screenToAxialF :: Int -> Float -> Float -> (Int, Int)
screenToAxialF = worldPointToAxial

-- | Convert a screen pixel to its world-space sample point, applying the
-- camera transform and sampling the pixel centre exactly once.
screenPixelToWorld :: (Float, Float) -> Float -> (Int, Int) -> (Float, Float)
screenPixelToWorld (panX, panY) zoom (sx, sy) =
  let z = max 0.001 zoom
      x = (fromIntegral sx + 0.5) / z - panX
      y = (fromIntegral sy + 0.5) / z - panY
  in (x, y)

-- | Pick a hex directly from a screen pixel and camera transform.
screenPixelToAxial :: Int -> (Float, Float) -> Float -> (Int, Int) -> (Int, Int)
screenPixelToAxial size pan zoom screenPx =
  let (wx, wy) = screenPixelToWorld pan zoom screenPx
  in worldPointToAxial size wx wy

-- | Pick a hex from a continuous world-space point.
worldPointToAxial :: Int -> Float -> Float -> (Int, Int)
worldPointToAxial size wx wy =
  let (q0, r0) = worldPointToAxialRaw size wx wy
      candidates = (q0, r0) : axialNeighbors (q0, r0)
  in case filter (pointInHexF size (wx, wy)) candidates of
       (hit:_) -> hit
       []      -> (q0, r0)

pointInHex :: Int -> (Int, Int) -> (Int, Int) -> Bool
pointInHex size (sx, sy) =
  pointInHexF size (fromIntegral sx + 0.5, fromIntegral sy + 0.5)

pointInHexF :: Int -> (Float, Float) -> (Int, Int) -> Bool
pointInHexF size (wx, wy) (q, r) =
  let (cx, cy) = hexCenterF size q r
      s = fromIntegral size :: Float
      x = (wx - cx) / s
      y = (wy - cy) / s
      qf = sqrt 3 / 3 * x - 1 / 3 * y
      rf = 2 / 3 * y
      cfX = qf
      cfZ = rf
      cfY = -cfX - cfZ
  in maximum [abs cfX, abs cfY, abs cfZ] <= 1.0

worldPointToAxialRaw :: Int -> Float -> Float -> (Int, Int)
worldPointToAxialRaw size wx wy =
  let s = fromIntegral size :: Float
      (ox, oy) = hexOriginF
      x = (wx - ox) / s
      y = (wy - oy) / s
      qf = sqrt 3 / 3 * x - 1 / 3 * y
      rf = 2 / 3 * y
  in cubeRound qf rf

axialNeighbors :: (Int, Int) -> [(Int, Int)]
axialNeighbors (q, r) =
  [ (q + 1, r)
  , (q + 1, r - 1)
  , (q, r - 1)
  , (q - 1, r)
  , (q - 1, r + 1)
  , (q, r + 1)
  ]

cubeRound :: Float -> Float -> (Int, Int)
cubeRound q r =
  let x = q
      z = r
      y = -x - z
      rx = fromIntegral (round x :: Int) :: Float
      ry = fromIntegral (round y :: Int) :: Float
      rz = fromIntegral (round z :: Int) :: Float
      dx = abs (rx - x)
      dy = abs (ry - y)
      dz = abs (rz - z)
      (fx, fz)
        | dx > dy && dx > dz = (-(ry + rz), rz)
        | dy > dz = (rx, rz)
        | otherwise = (rx, -(rx + ry))
  in (round fx, round fz)

-- | Exact pointy-top hex corner offsets for semantic/top-fill geometry.
hexCornerOffsets :: Int -> [(Float, Float)]
hexCornerOffsets size =
  let s = fromIntegral size :: Float
  in [ (s * cos (degToRad angle), s * sin (degToRad angle))
     | angle <- pointyTopCornerAngles
     ]

-- | Visual-only overlap available for seam/gutter fill paths.  It is
-- intentionally not used by picking, hover, brush previews, rivers, or the
-- semantic terrain top fill.
hexSeamGutterOverlapPx :: Float
hexSeamGutterOverlapPx = 0.6

-- | Pointy-top corner offsets inflated only for an explicit seam/gutter pass.
hexSeamCornerOffsets :: Int -> [(Float, Float)]
hexSeamCornerOffsets size =
  let s = fromIntegral size + hexSeamGutterOverlapPx
  in [ (s * cos (degToRad angle), s * sin (degToRad angle))
     | angle <- pointyTopCornerAngles
     ]

pointyTopCornerAngles :: [Float]
pointyTopCornerAngles = [-30, 30, 90, 150, 210, 270]

-- | Floor/ceil pixel rectangle covering the exact semantic hex fill.
hexFillRectAt :: Int -> Int -> Int -> Rect
hexFillRectAt size q r =
  let (cx, cy) = hexCenterF size q r
      ap = hexApothem size
      radius = fromIntegral size :: Float
  in rectFromEdges (cx - ap) (cy - radius) (cx + ap) (cy + radius)

-- | Continuous bounds of all semantic hexes in a chunk.
hexChunkBoundsF :: Int -> Int -> Int -> Int -> (Float, Float, Float, Float)
hexChunkBoundsF chunkSize size originQ originR =
  let qs = [originQ, originQ + max 0 (chunkSize - 1)]
      rs = [originR, originR + max 0 (chunkSize - 1)]
      ap = hexApothem size
      radius = fromIntegral size :: Float
      centers = [hexCenterF size q r | q <- qs, r <- rs]
      xs = concat [[cx - ap, cx + ap] | (cx, _cy) <- centers]
      ys = concat [[cy - radius, cy + radius] | (_cx, cy) <- centers]
  in (minimum xs, minimum ys, maximum xs, maximum ys)

-- | Floor/ceil pixel rectangle covering all semantic hexes in a chunk.
hexChunkBounds :: Int -> Int -> Int -> Int -> Rect
hexChunkBounds chunkSize size originQ originR =
  let (minX, minY, maxX, maxY) = hexChunkBoundsF chunkSize size originQ originR
  in rectFromEdges minX minY maxX maxY

-- | Normalise a rect from a zoom-stage hex-radius frame to the canonical
-- 'renderHexRadiusPx' frame.  Edges are scaled around the fixed hex origin and
-- only then rasterised with floor/ceiling so atlas/fallback copy rects touch
-- without gaps.  Grid-aligned hex/chunk edges are snapped back to the exact
-- source lattice before scaling, avoiding phase-dependent ±1 px shifts.
normalizeHexBounds :: Int -> Rect -> Rect
normalizeHexBounds hexR rect
  | hexR == renderHexRadiusPx = rect
  | otherwise =
      let Rect (V2 x y, V2 w h) = rect
          x1 = floor (normalizeXEdge hexR x)
          y1 = floor (normalizeYEdge hexR y)
          x2 = ceiling (normalizeXEdge hexR (x + w))
          y2 = ceiling (normalizeYEdge hexR (y + h))
      in Rect (V2 x1 y1, V2 (max 1 (x2 - x1)) (max 1 (y2 - y1)))

normalizeXEdge :: Int -> Int -> Float
normalizeXEdge sourceRadius edge =
  normalizeGridEdge
    (fromIntegral hexOriginX)
    (fromIntegral sourceRadius * sqrt 3 / 2)
    (fromIntegral renderHexRadiusPx * sqrt 3 / 2)
    sourceRadius
    edge

normalizeYEdge :: Int -> Int -> Float
normalizeYEdge sourceRadius edge =
  normalizeGridEdge
    (fromIntegral hexOriginY)
    (fromIntegral sourceRadius / 2)
    (fromIntegral renderHexRadiusPx / 2)
    sourceRadius
    edge

normalizeGridEdge :: Float -> Float -> Float -> Int -> Int -> Float
normalizeGridEdge origin sourceStep baseStep sourceRadius edge =
  let edgeF = fromIntegral edge
      scale = fromIntegral renderHexRadiusPx / fromIntegral sourceRadius :: Float
      k = round ((edgeF - origin) / sourceStep) :: Int
      snappedSource = origin + sourceStep * fromIntegral k
      affine = origin + scale * (edgeF - origin)
  in if abs (snappedSource - edgeF) <= 1.0
       then origin + baseStep * fromIntegral k
       else affine

-- | Transform a world-space rect to screen pixels with floor/ceiling edges.
-- This is the shared pan/zoom copy-rect transform used by atlas tiles,
-- fallback textures, hover highlights, and brush previews.
transformWorldRect :: (Float, Float) -> Float -> Rect -> Rect
transformWorldRect (panX, panY) zoom (Rect (V2 rx ry, V2 rw rh)) =
  let z = max 0.001 zoom
      fx1 = (fromIntegral rx + panX) * z
      fy1 = (fromIntegral ry + panY) * z
      fx2 = (fromIntegral (rx + rw) + panX) * z
      fy2 = (fromIntegral (ry + rh) + panY) * z
      ix1 = floor fx1
      iy1 = floor fy1
      ix2 = ceiling fx2
      iy2 = ceiling fy2
  in Rect (V2 ix1 iy1, V2 (max 1 (ix2 - ix1)) (max 1 (iy2 - iy1)))

transformWorldPoint :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
transformWorldPoint (panX, panY) zoom (wx, wy) =
  let z = max 0.001 zoom
  in ((wx + panX) * z, (wy + panY) * z)

-- | Texture dimensions for spans whose @x1@ coordinate is exclusive and whose
-- @y@ coordinate is inclusive.
hexSpanTextureSize :: [(Int, Int, Int)] -> (Int, Int)
hexSpanTextureSize [] = (1, 1)
hexSpanTextureSize spans =
  let ys = map (\(y, _, _) -> y) spans
      xs0 = map (\(_, x0, _) -> x0) spans
      xs1 = map (\(_, _, x1) -> x1) spans
      w = maximum xs1 - minimum xs0
      h = maximum ys - minimum ys + 1
  in (max 1 w, max 1 h)

rectFromEdges :: Float -> Float -> Float -> Float -> Rect
rectFromEdges minX minY maxX maxY =
  let x1 = floor minX
      y1 = floor minY
      x2 = ceiling maxX
      y2 = ceiling maxY
  in Rect (V2 x1 y1, V2 (max 1 (x2 - x1)) (max 1 (y2 - y1)))

degToRad :: Float -> Float
degToRad deg = deg * pi / 180
