{-# LANGUAGE PatternSynonyms #-}

-- | River geometry generation for hex atlas rendering.
--
-- Converts per-tile river segment data ('RiverChunk') into triangle-strip
-- geometry that can be drawn over the hex fill in the chunk atlas texture.
--
-- Each river segment in a hex is drawn as a polyline from the entry edge
-- midpoint through the hex centre to the exit edge midpoint, expanded
-- into a triangle strip whose width scales with 'RiverSize'.  Source
-- tiles (entry = 255) start at the hex centre; sink tiles (exit = 255)
-- end at the hex centre.
module UI.RiverRender
  ( -- * Configuration
    RiverRenderConfig(..)
  , defaultRiverRenderConfig
    -- * Geometry output
  , RiverGeometry(..)
    -- * Building
  , buildChunkRiverGeometry
    -- * Delta fan (exposed for testing)
  , buildDeltaFan
  , deltaParamsForOrder
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)
import qualified SDL.Raw.Types as Raw
import Topo
  ( ChunkCoord(..)
  , ChunkId(..)
  , RiverChunk(..)
  , TileCoord(..)
  , TileIndex(..)
  , WorldConfig(..)
  , chunkCoordFromId
  , chunkOriginTile
  , tileCoordFromIndex
  )
import Topo.Types (pattern RiverStream, pattern RiverCreek, pattern RiverRiver, riverSizeFromOrder)
import UI.HexPick (axialToScreen)
import UI.Widgets (Rect(..))
import Linear (V2(..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Visual parameters for river rendering.
data RiverRenderConfig = RiverRenderConfig
  { -- | Half-widths in pixels for each 'RiverSize'.
    rrcStreamHalfWidth :: !Float
  , rrcCreekHalfWidth  :: !Float
  , rrcRiverHalfWidth  :: !Float
  , rrcMajorHalfWidth  :: !Float
    -- | River colour (RGBA).
  , rrcColor           :: !(Word8, Word8, Word8, Word8)
    -- | Radius of the filled circle drawn at terminus (sink) river
    -- endpoints.  Scales with river half-width; this is the minimum.
  , rrcPoolRadius      :: !Float
    -- | Colour of terminus pool circles (RGBA).
  , rrcPoolColor       :: !(Word8, Word8, Word8, Word8)
    -- | Delta fan radii per river size (pixels).
  , rrcDeltaStreamRadius :: !Float
  , rrcDeltaCreekRadius  :: !Float
  , rrcDeltaRiverRadius  :: !Float
  , rrcDeltaMajorRadius  :: !Float
    -- | Delta fan spread angles per river size (degrees).
  , rrcDeltaStreamSpread :: !Float
  , rrcDeltaCreekSpread  :: !Float
  , rrcDeltaRiverSpread  :: !Float
  , rrcDeltaMajorSpread  :: !Float
    -- | Delta fan colour (RGBA) — slightly darker / muddier blue.
  , rrcDeltaColor        :: !(Word8, Word8, Word8, Word8)
  } deriving (Eq, Show)

-- | Sensible defaults for a 6 px hex radius.
defaultRiverRenderConfig :: RiverRenderConfig
defaultRiverRenderConfig = RiverRenderConfig
  { rrcStreamHalfWidth    = 0.25
  , rrcCreekHalfWidth     = 0.5
  , rrcRiverHalfWidth     = 1.0
  , rrcMajorHalfWidth     = 1.75
  , rrcColor              = (60, 120, 200, 255)
  , rrcPoolRadius         = 2.0
  , rrcPoolColor          = (40, 100, 180, 255)
  , rrcDeltaStreamRadius  = 1.5
  , rrcDeltaCreekRadius   = 2.5
  , rrcDeltaRiverRadius   = 4.0
  , rrcDeltaMajorRadius   = 6.0
  , rrcDeltaStreamSpread  = 45.0
  , rrcDeltaCreekSpread   = 60.0
  , rrcDeltaRiverSpread   = 90.0
  , rrcDeltaMajorSpread   = 120.0
  , rrcDeltaColor         = (50, 95, 155, 255)
  }

-- ---------------------------------------------------------------------------
-- Geometry output
-- ---------------------------------------------------------------------------

-- | Pre-built triangle geometry for river overlays in a single chunk.
data RiverGeometry = RiverGeometry
  { rgBounds   :: !Rect
  , rgVertices :: !(Vector Raw.Vertex)
  , rgIndices  :: !(Vector CInt)
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Hex radius used by the terrain renderer.
hexSize :: Int
hexSize = 6

-- | Extra overlap pixels (matching TerrainRender).
hexOverlap :: Float
hexOverlap = 0.6

-- ---------------------------------------------------------------------------
-- Core builder
-- ---------------------------------------------------------------------------

-- | Build river overlay geometry for a single chunk.
--
-- Returns 'Nothing' if the chunk has no visible river segments.
buildChunkRiverGeometry
  :: RiverRenderConfig
  -> WorldConfig
  -> Int                -- ^ chunk key
  -> IntMap RiverChunk  -- ^ all river chunks
  -> Maybe RiverGeometry
buildChunkRiverGeometry cfg config key riverMap = do
  rc <- IntMap.lookup key riverMap
  let offsets   = rcSegOffsets rc
      totalSegs = if U.null offsets then 0 else offsets U.! (U.length offsets - 1)
  if totalSegs <= 0
    then Nothing
    else Just (buildGeometry cfg config key rc)

-- | Internal: build geometry for a chunk known to have segments.
buildGeometry
  :: RiverRenderConfig
  -> WorldConfig
  -> Int
  -> RiverChunk
  -> RiverGeometry
buildGeometry cfg config key rc =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      TileCoord ox oy  = chunkOriginTile config (ChunkCoord cx cy)
      (minX, minY, maxX, maxY) = chunkBoundsF config hexSize (ChunkCoord cx cy)
      bounds = Rect (V2 (floor minX) (floor minY),
                     V2 (max 1 (ceiling maxX - floor minX))
                        (max 1 (ceiling maxY - floor minY)))
      offsets   = rcSegOffsets rc
      entryEdge = rcSegEntryEdge rc
      exitEdge  = rcSegExitEdge rc
      segOrder  = rcSegOrder rc
      n         = wcChunkSize config * wcChunkSize config
      (rCol_r, rCol_g, rCol_b, rCol_a) = rrcColor cfg
      rawColor  = Raw.Color rCol_r rCol_g rCol_b rCol_a
      (pCol_r, pCol_g, pCol_b, pCol_a) = rrcPoolColor cfg
      poolColor = Raw.Color pCol_r pCol_g pCol_b pCol_a
      (dCol_r, dCol_g, dCol_b, dCol_a) = rrcDeltaColor cfg
      deltaColor = Raw.Color dCol_r dCol_g dCol_b dCol_a
      hexR      = fromIntegral hexSize + hexOverlap :: Float

      -- Accumulate vertices and indices for all tiles.
      go tileIdx verts idxs baseVert
        | tileIdx >= n = (reverse verts, reverse idxs, baseVert)
        | otherwise =
            let segStart = offsets U.! tileIdx
                segEnd   = offsets U.! (tileIdx + 1)
                segCount = segEnd - segStart
            in if segCount <= 0
               then go (tileIdx + 1) verts idxs baseVert
               else
                 let TileCoord tx ty = tileCoordFromIndex config (TileIndex tileIdx)
                     q = ox + tx
                     r = oy + ty
                     (scrX, scrY) = axialToScreen hexSize q r
                     centerX = fromIntegral scrX - minX
                     centerY = fromIntegral scrY - minY
                     (sv, si, newBase) = buildTileRiverSegs
                       cfg rawColor poolColor deltaColor hexR
                       entryEdge exitEdge segOrder
                       centerX centerY
                       segStart segCount
                       baseVert
                 in go (tileIdx + 1) (sv : verts) (si : idxs) newBase

      (vertLists, idxLists, _) = go 0 [] [] 0
  in RiverGeometry
      { rgBounds   = bounds
      , rgVertices = SV.fromList (concat vertLists)
      , rgIndices  = SV.fromList (concat idxLists)
      }

-- | Build vertices and indices for all river segments of a single tile.
buildTileRiverSegs
  :: RiverRenderConfig
  -> Raw.Color
  -> Raw.Color       -- ^ pool colour
  -> Raw.Color       -- ^ delta colour
  -> Float           -- ^ hex radius (with overlap)
  -> U.Vector Word8  -- ^ entry edges (global)
  -> U.Vector Word8  -- ^ exit edges (global)
  -> U.Vector Word16 -- ^ segment orders (global)
  -> Float -> Float  -- ^ tile centre in local texture coords
  -> Int             -- ^ first segment index (global)
  -> Int             -- ^ segment count for this tile
  -> CInt            -- ^ current base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildTileRiverSegs cfg color poolColor deltaColor hexR entryVec exitVec orderVec cx cy segStart segCount baseVert =
  let go i verts idxs bv
        | i >= segCount = (reverse verts, reverse idxs, bv)
        | otherwise =
            let gIdx    = segStart + i
                eEntry  = entryVec U.! gIdx
                eExit   = exitVec  U.! gIdx
                order   = orderVec U.! gIdx
                hw      = halfWidthForOrder cfg order
                poolR   = max (rrcPoolRadius cfg) hw
                (sv, si, bv') = buildSegmentGeometry cfg color poolColor deltaColor hexR hw poolR order cx cy eEntry eExit bv
            in go (i + 1) (sv : verts) (si : idxs) bv'
  in let (vs, is, finalBase) = go 0 [] [] baseVert
     in (concat vs, concat is, finalBase)

-- | Build the triangle geometry for a single river segment (entry -> centre -> exit).
--
-- Produces two quads (4 triangles, 8 vertices) for the two line segments:
-- entry point -> hex centre, and hex centre -> exit point.
-- If entry == 255 (source), only the second segment is emitted.
-- If exit == 255 (sink), only the first segment is emitted, followed
-- by a fan-shaped delta whose size and spread scale with the river's
-- Strahler order.
buildSegmentGeometry
  :: RiverRenderConfig
  -> Raw.Color
  -> Raw.Color  -- ^ pool colour (legacy, unused with delta)
  -> Raw.Color  -- ^ delta colour
  -> Float      -- ^ hex radius
  -> Float      -- ^ half-width
  -> Float      -- ^ pool radius (legacy minimum)
  -> Word16     -- ^ Strahler order
  -> Float      -- ^ tile centre X
  -> Float      -- ^ tile centre Y
  -> Word8      -- ^ entry edge (255 = source)
  -> Word8      -- ^ exit edge (255 = sink)
  -> CInt       -- ^ base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildSegmentGeometry cfg color _poolColor deltaColor hexR hw _poolR order cx cy entryE exitE base =
  let entryPt = edgeMidpoint hexR cx cy entryE
      exitPt  = edgeMidpoint hexR cx cy exitE
      centre  = (cx, cy)
  in case (entryE == 255, exitE == 255) of
       -- Source + Sink: degenerate, skip
       (True, True)   -> ([], [], base)
       -- Source tile: only centre -> exit
       (True, False)  ->
         let (v, i, b) = buildLineQuadRaw color hw centre exitPt base
         in (v, i, b)
       -- Sink tile: entry -> centre + delta fan
       (False, True)  ->
         let (v1, i1, b1) = buildLineQuadRaw color hw entryPt centre base
             (dRadius, dSpreadDeg, dTriCount) = deltaParamsForOrder cfg order
             entryMid = edgeMidpoint hexR cx cy entryE
             -- Delta fans outward from the entry direction (away from where
             -- water enters, toward the body of water).
             dirAngle = atan2 (cy - snd entryMid) (cx - fst entryMid)
             (v2, i2, b2) = buildDeltaFan deltaColor dRadius (dSpreadDeg * pi / 180.0) dTriCount cx cy dirAngle b1
         in (v1 ++ v2, i1 ++ i2, b2)
       -- Through tile: entry -> centre, centre -> exit (two quads)
       (False, False) ->
         let (v1, i1, b1) = buildLineQuadRaw color hw entryPt centre base
             (v2, i2, b2) = buildLineQuadRaw color hw centre exitPt b1
         in (v1 ++ v2, i1 ++ i2, b2)

-- | Compute the midpoint of hex edge @e@ relative to center @(cx, cy)@.
-- If edge code is 255, returns the hex centre.
edgeMidpoint :: Float -> Float -> Float -> Word8 -> (Float, Float)
edgeMidpoint _hexR cx cy 255 = (cx, cy)
edgeMidpoint hexR cx cy e =
  let -- Edge i has its midpoint at angle i * (-60) degrees in screen
      -- coordinates (Y-down).  Using negative rotation converts from
      -- the standard math-coords edge numbering (counterclockwise,
      -- Y-up) to screen space (clockwise, Y-down).
      --
      -- Edge 0 (E):  0°  → (+apothem,  0)        right
      -- Edge 1 (NE): -60° → (+half,   -0.866·a)   upper-right
      -- Edge 2 (NW): -120°→ (-half,   -0.866·a)   upper-left
      -- Edge 3 (W):  -180°→ (-apothem, 0)         left
      -- Edge 4 (SW): -240°→ (-half,   +0.866·a)   lower-left
      -- Edge 5 (SE): -300°→ (+half,   +0.866·a)   lower-right
      angleDeg = fromIntegral e * (-60.0) :: Float
      angleRad = angleDeg * pi / 180.0
      apothem  = hexR * sqrt 3.0 / 2.0
  in (cx + apothem * cos angleRad, cy + apothem * sin angleRad)

-- | Build a single quad (2 triangles, 4 vertices) for a thick line from A to B.
-- | Build a single quad (2 triangles, 4 vertices) as raw lists.
buildLineQuadRaw
  :: Raw.Color
  -> Float          -- ^ half-width
  -> (Float, Float) -- ^ point A
  -> (Float, Float) -- ^ point B
  -> CInt           -- ^ base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildLineQuadRaw color hw (ax, ay) (bx, by) base =
  let dx = bx - ax
      dy = by - ay
      len = sqrt (dx * dx + dy * dy)
      -- Normal perpendicular to the line direction, scaled by half-width.
      (nx, ny) = if len < 0.001
                   then (hw, 0)   -- degenerate: arbitrary normal
                   else (hw * (-dy) / len, hw * dx / len)
      -- Four corners of the quad
      mkV x y = Raw.Vertex (Raw.FPoint (realToFrac x) (realToFrac y)) color (Raw.FPoint 0 0)
      v0 = mkV (ax + nx) (ay + ny)  -- A + normal
      v1 = mkV (ax - nx) (ay - ny)  -- A - normal
      v2 = mkV (bx + nx) (by + ny)  -- B + normal
      v3 = mkV (bx - nx) (by - ny)  -- B - normal
      -- Two triangles: (0,1,2), (1,2,3)
      indices = [ base, base + 1, base + 2
                , base + 1, base + 2, base + 3 ]
  in ([v0, v1, v2, v3], indices, base + 4)

-- | Number of rim vertices in a terminus pool circle.
poolFanSegments :: Int
poolFanSegments = 6

-- | Build a small filled circle (triangle fan) at @(cx, cy)@ with the
-- given radius.  Produces @poolFanSegments@ triangles sharing a centre
-- vertex, i.e. @poolFanSegments + 1@ vertices and
-- @poolFanSegments * 3@ indices.
buildPoolFan
  :: Raw.Color
  -> Float      -- ^ radius
  -> Float      -- ^ centre X
  -> Float      -- ^ centre Y
  -> CInt       -- ^ base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildPoolFan color radius cx cy base =
  let mkV x y = Raw.Vertex (Raw.FPoint (realToFrac x) (realToFrac y)) color (Raw.FPoint 0 0)
      centerV = mkV cx cy
      angleStep = 2.0 * pi / fromIntegral poolFanSegments :: Float
      rimVerts = [ mkV (cx + radius * cos (fromIntegral i * angleStep))
                       (cy + radius * sin (fromIntegral i * angleStep))
                 | i <- [0 .. poolFanSegments - 1] ]
      -- Triangle fan: for each sector i, triangle (centre, rim_i, rim_{i+1 mod n})
      fanIndices = concatMap (\i ->
        let ri  = base + 1 + fromIntegral i
            ri1 = base + 1 + fromIntegral ((i + 1) `mod` poolFanSegments)
        in [base, ri, ri1])
        [0 .. poolFanSegments - 1]
      totalVerts = poolFanSegments + 1
  in (centerV : rimVerts, fanIndices, base + fromIntegral totalVerts)

-- ---------------------------------------------------------------------------
-- Delta fan
-- ---------------------------------------------------------------------------

-- | Build a fan-shaped delta at @(cx, cy)@, spreading in the given
-- direction.  The fan is an arc of triangles with the centre vertex at
-- @(cx, cy)@ and rim vertices distributed evenly across the arc
-- @[dirAngle - spread/2, dirAngle + spread/2]@.
--
-- Produces @triCount@ triangles (i.e. @triCount + 1@ rim vertices +
-- 1 centre vertex = @triCount + 2@ total vertices, and
-- @triCount * 3@ indices).
buildDeltaFan
  :: Raw.Color
  -> Float      -- ^ fan radius (pixels)
  -> Float      -- ^ spread angle (radians, total arc width)
  -> Int        -- ^ number of triangles in the fan
  -> Float      -- ^ centre X
  -> Float      -- ^ centre Y
  -> Float      -- ^ direction angle (radians) — the fan opens in this direction
  -> CInt       -- ^ base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildDeltaFan color radius spread triCount cx cy dirAngle base =
  let mkV x y = Raw.Vertex (Raw.FPoint (realToFrac x) (realToFrac y)) color (Raw.FPoint 0 0)
      centerV = mkV cx cy
      rimCount = triCount + 1
      halfSpread = spread / 2.0
      angleStep = if triCount > 0 then spread / fromIntegral triCount else 0
      startAngle = dirAngle - halfSpread
      rimVerts = [ mkV (cx + radius * cos (startAngle + fromIntegral i * angleStep))
                       (cy + radius * sin (startAngle + fromIntegral i * angleStep))
                 | i <- [0 .. rimCount - 1] ]
      -- Triangle fan: for each sector i, triangle (centre, rim_i, rim_{i+1})
      fanIndices = concatMap (\i ->
        let ri  = base + 1 + fromIntegral i
            ri1 = base + 1 + fromIntegral (i + 1)
        in [base, ri, ri1])
        [0 .. triCount - 1]
      totalVerts = 1 + rimCount
  in (centerV : rimVerts, fanIndices, base + fromIntegral totalVerts)

-- | Look up delta fan parameters (radius, spread in degrees, triangle count)
-- for a given Strahler order based on the render configuration.
deltaParamsForOrder :: RiverRenderConfig -> Word16 -> (Float, Float, Int)
deltaParamsForOrder cfg order =
  let sz = riverSizeFromOrder order
  in case () of
       _ | sz == RiverStream -> (rrcDeltaStreamRadius cfg, rrcDeltaStreamSpread cfg, 3)
         | sz == RiverCreek  -> (rrcDeltaCreekRadius  cfg, rrcDeltaCreekSpread  cfg, 4)
         | sz == RiverRiver  -> (rrcDeltaRiverRadius  cfg, rrcDeltaRiverSpread  cfg, 6)
         | otherwise         -> (rrcDeltaMajorRadius  cfg, rrcDeltaMajorSpread  cfg, 8)

-- ---------------------------------------------------------------------------
-- Half-width lookup
-- ---------------------------------------------------------------------------

-- | Map a Strahler order to the visual half-width in pixels.
halfWidthForOrder :: RiverRenderConfig -> Word16 -> Float
halfWidthForOrder cfg order =
  let sz = riverSizeFromOrder order
  in case () of
       _ | sz == RiverStream -> rrcStreamHalfWidth cfg
         | sz == RiverCreek  -> rrcCreekHalfWidth  cfg
         | sz == RiverRiver  -> rrcRiverHalfWidth  cfg
         | otherwise         -> rrcMajorHalfWidth  cfg

-- ---------------------------------------------------------------------------
-- Chunk bounds (float version)
-- ---------------------------------------------------------------------------

-- | Compute the pixel bounding box of a chunk (as floats for sub-pixel work).
chunkBoundsF :: WorldConfig -> Int -> ChunkCoord -> (Float, Float, Float, Float)
chunkBoundsF config size (ChunkCoord cx cy) =
  let TileCoord ox oy = chunkOriginTile config (ChunkCoord cx cy)
      s = wcChunkSize config
      corners =
        [ (ox, oy)
        , (ox + s, oy)
        , (ox, oy + s)
        , (ox + s, oy + s)
        ]
      screenPts = [axialToScreen size q r | (q, r) <- corners]
      xs = map (fromIntegral . fst) screenPts
      ys = map (fromIntegral . snd) screenPts
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in (minX - fromIntegral size, minY - fromIntegral size,
      maxX + fromIntegral size, maxY + fromIntegral size)
