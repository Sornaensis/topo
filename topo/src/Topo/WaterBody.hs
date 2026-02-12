{-# LANGUAGE OverloadedStrings #-}

-- | Water body analysis – distinguishes ocean-connected water from
-- landlocked freshwater bodies (lakes, inland seas).
--
-- The main entry point is 'applyWaterBodyStage', which:
--
--  1. Builds a global elevation grid from loaded terrain chunks.
--  2. Identifies all submerged tiles (elevation < waterLevel).
--  3. Flood-fills 4-connected components among submerged tiles.
--  4. Classifies each component:
--       * __Ocean__ – touches the grid edge (within a configurable margin).
--       * __Inland sea__ – landlocked and larger than a size threshold.
--       * __Lake__ – all other landlocked water bodies.
--  5. Computes a pour-point surface elevation for each inland basin
--     (the minimum land elevation adjacent to the basin).
--  6. Slices the results into per-chunk 'WaterBodyChunk' records stored
--     in 'twWaterBodies'.
module Topo.WaterBody
  ( -- * Configuration
    WaterBodyConfig(..)
  , defaultWaterBodyConfig
    -- * Pipeline
  , applyWaterBodyStage
    -- * Algorithm (exposed for testing)
  , classifyWaterBodies
  , WaterBodyResult(..)
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word32)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , chunkGridSlice
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Tuning knobs for water body classification.
data WaterBodyConfig = WaterBodyConfig
  { wbcOceanEdgeMargin :: !Int
    -- ^ Tiles within this many cells of the grid edge that are submerged
    -- are treated as ocean seeds.  Default: 2.
  , wbcInlandSeaMinSize :: !Int
    -- ^ Minimum tile count for a landlocked basin to be classified as an
    -- inland sea rather than a lake.  Default: 200.
  , wbcMinLakeSize :: !Int
    -- ^ Minimum tile count for a landlocked basin to survive as a lake.
    -- Basins smaller than this are reclassified as 'WaterDry' (absorbed
    -- into surrounding land).  Default: 4.
  } deriving (Eq, Show)

-- | Sensible defaults for water body classification.
defaultWaterBodyConfig :: WaterBodyConfig
defaultWaterBodyConfig = WaterBodyConfig
  { wbcOceanEdgeMargin  = 2
  , wbcInlandSeaMinSize = 200
  , wbcMinLakeSize      = 4
  }

-- ---------------------------------------------------------------------------
-- Algorithm result
-- ---------------------------------------------------------------------------

-- | Full-grid water body classification outputs.
--
-- All vectors have length @gridW * gridH@.
data WaterBodyResult = WaterBodyResult
  { wbrType         :: !(U.Vector WaterBodyType)
  , wbrSurfaceElev  :: !(U.Vector Float)
  , wbrBasinId      :: !(U.Vector Word32)
  , wbrDepth        :: !(U.Vector Float)
  , wbrAdjacentType :: !(U.Vector WaterBodyType)
    -- ^ Per-tile: highest-priority water body type among 4-neighbours.
    -- For submerged tiles this matches 'wbrType'; for land tiles it is
    -- the most significant adjacent water type.  Priority:
    -- @WaterOcean > WaterInlandSea > WaterLake > WaterDry@.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Core algorithm
-- ---------------------------------------------------------------------------

-- | Classify every tile in a global elevation grid.
--
-- The algorithm:
--
-- 1. Mark every tile with @elevation < waterLevel@ as submerged.
-- 2. Flood-fill 4-connected components among submerged tiles,
--    assigning a unique basin id to each component.
-- 3. For each component, determine whether any member lies within
--    'wbcOceanEdgeMargin' of the grid boundary → ocean.
-- 4. Landlocked components >= 'wbcInlandSeaMinSize' → inland sea;
--    those >= 'wbcMinLakeSize' → lake; smaller ones → dry.
-- 5. Ocean tiles get surface elevation = @waterLevel@.
-- 6. Inland basins get surface elevation = minimum elevation of the
--    land tiles 4-adjacent to the basin (the pour point).
-- 7. Depth = max 0 (surfaceElev − elevation).
classifyWaterBodies
  :: WaterBodyConfig
  -> Float          -- ^ Water level
  -> Int            -- ^ Grid width
  -> Int            -- ^ Grid height
  -> U.Vector Float -- ^ Elevation grid (length = gridW * gridH)
  -> WaterBodyResult
classifyWaterBodies cfg waterLevel gridW gridH elev = runST $ do
  let n = gridW * gridH
      margin = wbcOceanEdgeMargin cfg
      minLake = wbcMinLakeSize cfg
      inlandSeaSize = wbcInlandSeaMinSize cfg

  -- Step 1 & 2: flood-fill connected components of submerged tiles.
  -- componentId: -1 = not submerged, >= 0 = component label.
  componentId <- UM.replicate n (-1 :: Int)
  let submerged i = (elev U.! i) < waterLevel

  -- Simple iterative flood-fill using a manual stack.
  nextLabel <- UM.replicate 1 (0 :: Int)

  -- Track per-component: size, touchesEdge flag, pourPointElev
  -- We use mutable IntMaps accumulated after fill.
  -- Instead we fill then scan, using immutable post-processing.
  forM_ [0 .. n - 1] $ \i -> do
    cid <- UM.read componentId i
    when (cid < 0 && submerged i) $ do
      label <- UM.read nextLabel 0
      UM.write nextLabel 0 (label + 1)
      -- BFS/DFS flood fill from tile i
      floodFill gridW gridH elev waterLevel componentId label i

  -- Step 3 & 4: scan components to gather stats.
  frozenIds <- U.freeze componentId
  numLabels <- UM.read nextLabel 0

  -- Per-component accumulators
  compSize     <- UM.replicate numLabels (0 :: Int)
  compEdge     <- UM.replicate numLabels (0 :: Int) -- >0 means touches edge
  compPourElev <- UM.replicate numLabels (1e30 :: Float) -- minimum land neighbor elev

  forM_ [0 .. n - 1] $ \i -> do
    let cid = frozenIds U.! i
    if cid >= 0
      then do
        -- This tile is submerged & in a component
        UM.modify compSize (+ 1) cid
        -- Check if within edge margin
        let x = i `mod` gridW
            y = i `div` gridW
        when (x < margin || x >= gridW - margin || y < margin || y >= gridH - margin) $
          UM.modify compEdge (+ 1) cid
      else do
        -- Land tile: check if any 4-neighbor is a submerged component
        -- and update pour-point elevation
        let x = i `mod` gridW
            y = i `div` gridW
            h = elev U.! i
            checkNeighbor ni = do
              let ncid = frozenIds U.! ni
              when (ncid >= 0) $
                UM.modify compPourElev (min h) ncid
        when (x > 0)             $ checkNeighbor (i - 1)
        when (x + 1 < gridW)     $ checkNeighbor (i + 1)
        when (y > 0)             $ checkNeighbor (i - gridW)
        when (y + 1 < gridH)     $ checkNeighbor (i + gridW)

  frozenSize     <- U.freeze compSize
  frozenEdge     <- U.freeze compEdge
  frozenPourElev <- U.freeze compPourElev

  -- Build per-component classification vector
  let classifyComponent cid =
        let size = frozenSize U.! cid
            edge = frozenEdge U.! cid
        in if edge > 0
            then WaterOcean
            else if size >= inlandSeaSize
              then WaterInlandSea
              else if size >= minLake
                then WaterLake
                else WaterDry  -- tiny puddle, absorb into land

  -- Step 5, 6, 7: build output vectors
  let surfaceElevOf cid =
        let edge = frozenEdge U.! cid
        in if edge > 0
            then waterLevel
            else frozenPourElev U.! cid

  let resultType = U.generate n $ \i ->
        let cid = frozenIds U.! i
        in if cid < 0 then WaterDry else classifyComponent cid

      resultSurface = U.generate n $ \i ->
        let cid = frozenIds U.! i
        in if cid < 0 then 0 else surfaceElevOf cid

      resultBasin = U.generate n $ \i ->
        let cid = frozenIds U.! i
        in if cid < 0 then 0 else fromIntegral cid

      resultDepth = U.generate n $ \i ->
        let cid = frozenIds U.! i
        in if cid < 0
            then 0
            else max 0 (surfaceElevOf cid - (elev U.! i))

  -- Step 8: compute per-tile adjacent water type.
  -- For each tile, inspect 4-neighbours and record the highest-priority
  -- water body type found.  Priority: Ocean > InlandSea > Lake > Dry.
  -- Submerged tiles get their own classified type.
  let adjacentType = U.generate n $ \i ->
        let ownType = resultType U.! i
        in if ownType /= WaterDry
           then ownType  -- submerged tile: adjacent type = own type
           else
             let x = i `mod` gridW
                 y = i `div` gridW
                 look ni = resultType U.! ni
                 wbPriority wbt
                   | wbt == WaterOcean     = 3 :: Int
                   | wbt == WaterInlandSea = 2
                   | wbt == WaterLake      = 1
                   | otherwise             = 0
                 best a b = if wbPriority b > wbPriority a then b else a
                 acc0 = WaterDry
                 acc1 = if x > 0         then best acc0 (look (i - 1))     else acc0
                 acc2 = if x + 1 < gridW then best acc1 (look (i + 1))     else acc1
                 acc3 = if y > 0         then best acc2 (look (i - gridW)) else acc2
                 acc4 = if y + 1 < gridH then best acc3 (look (i + gridW)) else acc3
             in acc4

  pure WaterBodyResult
    { wbrType         = resultType
    , wbrSurfaceElev  = resultSurface
    , wbrBasinId      = resultBasin
    , wbrDepth        = resultDepth
    , wbrAdjacentType = adjacentType
    }

-- | Iterative DFS flood fill for a single connected component.
--
-- Writes @label@ into every submerged tile reachable from @start@ via
-- 4-connectivity.  Uses an explicit stack to avoid deep Haskell
-- recursion.
floodFill
  :: Int                        -- ^ Grid width
  -> Int                        -- ^ Grid height
  -> U.Vector Float             -- ^ Elevation
  -> Float                      -- ^ Water level
  -> UM.MVector s Int           -- ^ Component id per tile (mutable)
  -> Int                        -- ^ Label for this component
  -> Int                        -- ^ Starting tile index
  -> ST s ()
floodFill gridW gridH elev waterLevel componentId label start = do
  -- Explicit stack backed by a mutable vector.
  let n = gridW * gridH
  stack <- UM.replicate n (0 :: Int)
  stackTop <- UM.replicate 1 (0 :: Int)

  let push v = do
        top <- UM.read stackTop 0
        UM.write stack top v
        UM.write stackTop 0 (top + 1)

      pop = do
        top <- UM.read stackTop 0
        if top <= 0
          then pure Nothing
          else do
            let top' = top - 1
            UM.write stackTop 0 top'
            v <- UM.read stack top'
            pure (Just v)

      submerged i = (elev U.! i) < waterLevel

  UM.write componentId start label
  push start

  let loop = do
        mVal <- pop
        case mVal of
          Nothing -> pure ()
          Just cur -> do
            let x = cur `mod` gridW
                y = cur `div` gridW
                tryNeighbor ni = do
                  cid <- UM.read componentId ni
                  when (cid < 0 && submerged ni) $ do
                    UM.write componentId ni label
                    push ni
            when (x > 0)         $ tryNeighbor (cur - 1)
            when (x + 1 < gridW) $ tryNeighbor (cur + 1)
            when (y > 0)         $ tryNeighbor (cur - gridW)
            when (y + 1 < gridH) $ tryNeighbor (cur + gridW)
            loop
  loop

-- ---------------------------------------------------------------------------
-- Pipeline stage
-- ---------------------------------------------------------------------------

-- | Pipeline stage that classifies water bodies and stores results in
-- 'twWaterBodies'.
--
-- Depends on terrain elevation being finalized (after rivers).
applyWaterBodyStage :: WaterBodyConfig -> Float -> PipelineStage
applyWaterBodyStage cfg waterLevel =
  PipelineStage "applyWaterBodies" "applyWaterBodies" $ do
    logInfo "applyWaterBodies: classifying ocean / lake / inland sea"
    world <- getWorldP
    let config = twConfig world
        terrain = twTerrain world
    (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
      case validateTerrainGrid config terrain of
        Left err -> throwError (PluginInvariantError err)
        Right bounds -> pure bounds
    let size = wcChunkSize config
        gridW = (maxCx - minCx + 1) * size
        gridH = (maxCy - minCy + 1) * size
        elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
        result = classifyWaterBodies cfg waterLevel gridW gridH elev
        globalChunk = WaterBodyChunk
          { wbType         = wbrType result
          , wbSurfaceElev  = wbrSurfaceElev result
          , wbBasinId      = wbrBasinId result
          , wbDepth        = wbrDepth result
          , wbAdjacentType = wbrAdjacentType result
          }
        waterBodies = IntMap.mapWithKey
          (sliceWaterBodyChunk config (ChunkCoord minCx minCy) gridW globalChunk)
          terrain
    putWorldP world { twWaterBodies = waterBodies }

-- | Slice a per-chunk 'WaterBodyChunk' from the global result.
sliceWaterBodyChunk
  :: WorldConfig
  -> ChunkCoord     -- ^ Minimum chunk coord of the grid
  -> Int            -- ^ Grid width in tiles
  -> WaterBodyChunk -- ^ Global (full-grid) water body data
  -> Int            -- ^ Chunk key
  -> TerrainChunk   -- ^ (unused, present for mapWithKey compat)
  -> WaterBodyChunk
sliceWaterBodyChunk config minCoord gridW global key _chunk =
  WaterBodyChunk
    { wbType         = chunkGridSliceGeneric config minCoord gridW (wbType global) key
    , wbSurfaceElev  = chunkGridSlice config minCoord gridW (wbSurfaceElev global) key
    , wbBasinId      = chunkGridSliceGeneric config minCoord gridW (wbBasinId global) key
    , wbDepth        = chunkGridSlice config minCoord gridW (wbDepth global) key
    , wbAdjacentType = chunkGridSliceGeneric config minCoord gridW (wbAdjacentType global) key
    }

-- | Slice a chunk-sized vector of any 'U.Unbox' type from a large grid.
chunkGridSliceGeneric :: U.Unbox a => WorldConfig -> ChunkCoord -> Int -> U.Vector a -> Int -> U.Vector a
chunkGridSliceGeneric config (ChunkCoord minCx minCy) gridW grid key =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      size = wcChunkSize config
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      n = size * size
  in U.generate n (\i ->
      let x = i `mod` size
          y = i `div` size
          gx = baseX + x
          gy = baseY + y
          gi = gy * gridW + gx
      in grid U.! gi)
