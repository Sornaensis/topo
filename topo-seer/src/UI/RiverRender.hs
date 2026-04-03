{-# LANGUAGE BangPatterns #-}
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
  , scaleRiverWidths
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
  , TerrainChunk(..)
  , TileCoord(..)
  , TileIndex(..)
  , WorldConfig(..)
  , chunkCoordFromId
  , chunkOriginTile
  , isWaterBiomeId
  , tileCoordFromIndex
  )
import Topo.River (isCoastalExit, coastalExitEdge)
import Topo.Types (BiomeId, pattern RiverStream, pattern RiverCreek, pattern RiverRiver, riverSizeFromOrder)
import UI.HexPick (axialToScreen, renderHexRadiusPx)
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
    -- | Delta fan radii per river size, expressed as a fraction of the
    -- hex apothem (0.0–1.0).  The actual pixel radius is computed as
    -- @fraction * apothem@ at the call site, keeping deltas contained
    -- within the hex boundary regardless of hex size.
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
    -- | Minimum Strahler order required for a terminus tile to render
    -- a delta fan instead of a plain line quad (coastal exits) or
    -- nothing (inland sinks).  This prevents small first- and second-
    -- order streams from each drawing their own fan around a water
    -- body, which produces the visual impression of duplicated deltas.
    -- Set to 0 to draw deltas for every terminus regardless of order.
  , rrcMinDeltaOrder     :: !Word16
  } deriving (Eq, Show)

-- | Sensible defaults for a 6 px hex radius.
--
-- Delta radii are expressed as fractions of the hex apothem (0.0–1.0).
-- At runtime the actual pixel radius is @fraction * apothem@.
defaultRiverRenderConfig :: RiverRenderConfig
defaultRiverRenderConfig = RiverRenderConfig
  { rrcStreamHalfWidth    = 0.25
  , rrcCreekHalfWidth     = 0.5
  , rrcRiverHalfWidth     = 1.0
  , rrcMajorHalfWidth     = 1.75
  , rrcColor              = (60, 120, 200, 255)
  , rrcPoolRadius         = 2.0
  , rrcPoolColor          = (40, 100, 180, 255)
  , rrcDeltaStreamRadius  = 0.35
  , rrcDeltaCreekRadius   = 0.50
  , rrcDeltaRiverRadius   = 0.70
  , rrcDeltaMajorRadius   = 0.95
  , rrcDeltaStreamSpread  = 45.0
  , rrcDeltaCreekSpread   = 60.0
  , rrcDeltaRiverSpread   = 90.0
  , rrcDeltaMajorSpread   = 120.0
  , rrcDeltaColor         = (50, 95, 155, 255)
  , rrcMinDeltaOrder      = 3
  }

-- | Scale river line widths to match a different hex radius.
--
-- 'defaultRiverRenderConfig' is calibrated for 'renderHexRadiusPx' (= 6).
-- At larger hex radii (zoom stages 2 and 3) the coordinate space is bigger,
-- so a 1-pixel half-width would produce sub-pixel-thin rivers.  Multiply
-- all half-widths and the pool radius by @hexRadius / renderHexRadiusPx@
-- before building geometry so rivers stay visually consistent across stages.
--
-- Delta fan radii are already expressed as fractions of the hex apothem
-- and therefore need no adjustment.
scaleRiverWidths :: Int -> RiverRenderConfig -> RiverRenderConfig
scaleRiverWidths hexRadius cfg =
  let factor = fromIntegral hexRadius / fromIntegral renderHexRadiusPx :: Float
  in cfg
      { rrcStreamHalfWidth = rrcStreamHalfWidth cfg * factor
      , rrcCreekHalfWidth  = rrcCreekHalfWidth  cfg * factor
      , rrcRiverHalfWidth  = rrcRiverHalfWidth  cfg * factor
      , rrcMajorHalfWidth  = rrcMajorHalfWidth  cfg * factor
      , rrcPoolRadius      = rrcPoolRadius      cfg * factor
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

-- | Extra overlap pixels (matching TerrainRender).
hexOverlap :: Float
hexOverlap = 0.6

-- ---------------------------------------------------------------------------
-- Core builder
-- ---------------------------------------------------------------------------

-- | Build river overlay geometry for a single chunk.
--
-- Returns 'Nothing' if the chunk has no visible river segments
-- (including degenerate segments that produce zero vertices).
--
-- When a 'TerrainChunk' is available for the same key, tiles whose
-- biome is a water body (ocean, lake, inland sea, etc.) are skipped
-- so that rivers do not render on submerged tiles.
buildChunkRiverGeometry
  :: RiverRenderConfig
  -> WorldConfig
  -> Int                   -- ^ hex radius in pixels
  -> Int                   -- ^ chunk key
  -> IntMap RiverChunk     -- ^ all river chunks
  -> IntMap TerrainChunk   -- ^ all terrain chunks (for biome filtering)
  -> Maybe RiverGeometry
buildChunkRiverGeometry cfg config hexRadiusPx key riverMap terrainMap = do
  rc <- IntMap.lookup key riverMap
  let offsets   = rcSegOffsets rc
      totalSegs = if U.null offsets then 0 else offsets U.! (U.length offsets - 1)
      mbBiomeVec = fmap tcFlags (IntMap.lookup key terrainMap)
  if totalSegs <= 0
    then Nothing
    else let rg = buildGeometry cfg config hexRadiusPx key rc mbBiomeVec
         in if SV.null (rgVertices rg) then Nothing else Just rg

-- | Internal: build geometry for a chunk known to have segments.
buildGeometry
  :: RiverRenderConfig
  -> WorldConfig
  -> Int
  -> Int
  -> RiverChunk
  -> Maybe (U.Vector BiomeId)  -- ^ biome vector for water-tile filtering
  -> RiverGeometry
buildGeometry cfg config hexRadiusPx key rc mbBiomeVec =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      TileCoord ox oy  = chunkOriginTile config (ChunkCoord cx cy)
      (minX, minY, maxX, maxY) = chunkBoundsF config hexRadiusPx (ChunkCoord cx cy)
      bounds = Rect (V2 (floor minX) (floor minY),
                     V2 (max 1 (ceiling maxX - floor minX))
                        (max 1 (ceiling maxY - floor minY)))
      offsets    = rcSegOffsets rc
      entryEdge  = rcSegEntryEdge rc
      exitEdge   = rcSegExitEdge rc
      segOrder   = rcSegOrder rc
      riverOrder = rcRiverOrder rc
      n          = wcChunkSize config * wcChunkSize config
      (rCol_r, rCol_g, rCol_b, rCol_a) = rrcColor cfg
      rawColor   = Raw.Color rCol_r rCol_g rCol_b rCol_a
      (dCol_r, dCol_g, dCol_b, dCol_a) = rrcDeltaColor cfg
      deltaColor = Raw.Color dCol_r dCol_g dCol_b dCol_a
      hexR       = fromIntegral hexRadiusPx + hexOverlap :: Float

      -- Test whether a tile is classified as a water biome.
      isTileWater idx = case mbBiomeVec of
        Nothing -> False
        Just bv -> isWaterBiomeId (bv U.! idx)

      -- Accumulate vertices and indices for all tiles.
      go tileIdx verts idxs baseVert
        | tileIdx >= n = (reverse verts, reverse idxs, baseVert)
        | otherwise =
            let segStart = offsets U.! tileIdx
                segEnd   = offsets U.! (tileIdx + 1)
                segCount = segEnd - segStart
            in if segCount <= 0 || isTileWater tileIdx
               then go (tileIdx + 1) verts idxs baseVert
               else
                 let TileCoord tx ty = tileCoordFromIndex config (TileIndex tileIdx)
                     q = ox + tx
                     r = oy + ty
                     (scrX, scrY) = axialToScreen hexRadiusPx q r
                     centerX = fromIntegral scrX - minX
                     centerY = fromIntegral scrY - minY
                     tileOrder = riverOrder U.! tileIdx
                     (sv, si, newBase) = buildTileRiverSegs
                       cfg rawColor deltaColor hexR
                       entryEdge exitEdge segOrder tileOrder
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
--
-- River segments at a tile share a common exit edge (the downstream
-- direction).  To avoid duplicate delta fans at confluence tiles, the
-- geometry is built in two phases:
--
--   1. __Entry-side__: for each segment whose entry ≠ 255, emit a quad
--      from the entry edge midpoint to the hex centre, using the
--      segment's own Strahler order for line width.
--
--   2. __Exit-side__ (once per tile): emit a single line quad from
--      centre to exit edge midpoint (or a delta fan for sinks/coastal
--      exits), using the tile's own Strahler order for width.
--
-- For coastal exits the delta fan is placed at the exit edge midpoint
-- so that it visually attaches to the hex boundary.
buildTileRiverSegs
  :: RiverRenderConfig
  -> Raw.Color
  -> Raw.Color       -- ^ delta colour
  -> Float           -- ^ hex radius (with overlap)
  -> U.Vector Word8  -- ^ entry edges (global)
  -> U.Vector Word8  -- ^ exit edges (global)
  -> U.Vector Word16 -- ^ segment orders (global)
  -> Word16          -- ^ tile Strahler order (for exit-side geometry)
  -> Float -> Float  -- ^ tile centre in local texture coords
  -> Int             -- ^ first segment index (global)
  -> Int             -- ^ segment count for this tile
  -> CInt            -- ^ current base vertex index
  -> ([Raw.Vertex], [CInt], CInt)
buildTileRiverSegs cfg color deltaColor hexR entryVec exitVec orderVec tileOrder cx cy segStart segCount baseVert =
  let -- All segments share the same exit.
      exitE     = exitVec U.! segStart
      -- Degenerate: source + sink on same tile → no geometry.
      allSourceSink = segCount == 1 && entryVec U.! segStart == 255 && exitE == 255
  in if allSourceSink
    then ([], [], baseVert)
    else
      let coastal    = isCoastalExit exitE
          realExitE  = if coastal then coastalExitEdge exitE else exitE
          exitHw     = halfWidthForOrder cfg tileOrder
          centre     = (cx, cy)
          exitPt     = edgeMidpoint hexR cx cy realExitE

          -- Phase 1: entry-side quads (entry_midpt → centre).
          buildEntries !i !vAcc !iAcc !bv
            | i >= segCount = (vAcc, iAcc, bv)
            | otherwise =
                let gIdx   = segStart + i
                    eEntry = entryVec U.! gIdx
                    order  = orderVec U.! gIdx
                    hw     = halfWidthForOrder cfg order
                in if eEntry == 255
                   then buildEntries (i + 1) vAcc iAcc bv
                   else let entryPt = edgeMidpoint hexR cx cy eEntry
                            (sv, si, bv') = buildLineQuadRaw color hw entryPt centre bv
                        in buildEntries (i + 1) (vAcc ++ sv) (iAcc ++ si) bv'

          (entryVerts, entryIdxs, baseAfterEntries) =
            buildEntries 0 [] [] baseVert

          -- Hex apothem — maximum radius for delta fans.
          apothem = hexR * sqrt 3.0 / 2.0

          -- Whether the tile meets the minimum order for a delta fan.
          deltaEligible = tileOrder >= rrcMinDeltaOrder cfg

          -- Phase 2: exit-side geometry (once per tile).
          (exitVerts, exitIdxs, finalBase)
            -- Inland sink: delta at centre (if eligible), else nothing.
            | exitE == 255 =
                if deltaEligible
                  then let (dFrac, dSpreadDeg, dTriCount) = deltaParamsForOrder cfg tileOrder
                           dRadius = dFrac * apothem
                           dirAngle = findSinkDirection
                       in buildDeltaFan deltaColor dRadius
                            (dSpreadDeg * pi / 180.0) dTriCount
                            cx cy dirAngle baseAfterEntries
                  else ([], [], baseAfterEntries)
            -- Coastal exit: line quad centre→edge midpoint, then a delta
            -- fan anchored at the hex centre reaching the edge (if
            -- eligible).  The line ensures the river visually connects
            -- to the hex boundary regardless of delta eligibility.
            | coastal =
                let (lineVerts, lineIdxs, baseAfterLine) =
                      buildLineQuadRaw color exitHw centre exitPt baseAfterEntries
                in if deltaEligible
                  then let (_, dSpreadDeg, dTriCount) = deltaParamsForOrder cfg tileOrder
                           exitDirAngle = atan2 (snd exitPt - cy) (fst exitPt - cx)
                           (dVerts, dIdxs, baseAfterDelta) =
                             buildDeltaFan deltaColor apothem
                               (dSpreadDeg * pi / 180.0) dTriCount
                               cx cy exitDirAngle baseAfterLine
                       in (lineVerts ++ dVerts, lineIdxs ++ dIdxs, baseAfterDelta)
                  else (lineVerts, lineIdxs, baseAfterLine)
            -- Normal exit: line centre→exit.
            | otherwise =
                buildLineQuadRaw color exitHw centre exitPt baseAfterEntries

          -- Direction for sink delta: order-weighted average of ALL
          -- non-source entry directions.  This produces a much better
          -- direction at confluence sinks than using only the first
          -- entry edge.
          findSinkDirection =
            let accumDir !i !sx !sy !w
                  | i >= segCount =
                      if w < 0.001
                        then 0  -- no entries: arbitrary
                        else atan2 (sy / w) (sx / w)
                  | otherwise =
                      let gIdx = segStart + i
                          e    = entryVec U.! gIdx
                      in if e == 255
                           then accumDir (i + 1) sx sy w
                           else let ep  = edgeMidpoint hexR cx cy e
                                    -- direction from entry toward centre
                                    dx  = cx - fst ep
                                    dy  = cy - snd ep
                                    -- weight by segment order
                                    ord = fromIntegral (orderVec U.! gIdx) :: Float
                                    ow  = max 1.0 ord
                                in accumDir (i + 1) (sx + ow * dx) (sy + ow * dy) (w + ow)
            in accumDir 0 0.0 0.0 0.0

          -- Disc join at centre: fills the triangular gap that appears at
          -- bends and confluences where two angled quads share only their
          -- endpoint at (cx, cy).
          hasEntries = baseAfterEntries > baseVert
          (jVerts, jIdxs, joinBase) =
            if hasEntries
              then buildDeltaFan color exitHw (2 * pi) 12 cx cy 0 finalBase
              else ([], [], finalBase)

      in (entryVerts ++ exitVerts ++ jVerts, entryIdxs ++ exitIdxs ++ jIdxs, joinBase)

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

-- | Look up delta fan parameters for a given Strahler order.
--
-- Returns @(radiusFraction, spreadDegrees, triangleCount)@ where
-- @radiusFraction@ is a proportion of the hex apothem (0.0–1.0).
-- The caller multiplies by the actual apothem to get pixel radius.
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
