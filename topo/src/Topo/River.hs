{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | River topology computation: converts flow-direction and discharge
-- grids into per-tile river segments used by the renderer.
--
-- The grid uses axial hex coordinates in row-major order.  Each tile
-- has 6 neighbours (D6 steepest-descent flow).  Each tile's exit
-- neighbour is mapped to a 'HexEdge' so the renderer can draw rivers
-- through the appropriate hex edges.
module Topo.River
  ( -- * Configuration
    RiverTopologyConfig(..)
  , defaultRiverTopologyConfig
    -- * Core computation
  , computeRiverSegments
    -- * Grid-direction to hex-edge mapping
  , gridDirToHexEdge
  , oppositeEdge
  ) where

import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Topo.Hex (hexNeighborIndices)
import Topo.Types (HexEdge(..), pattern EdgeNone)

-- | Configuration for river segment extraction.
data RiverTopologyConfig = RiverTopologyConfig
  { -- | Minimum discharge for a tile to produce a visible river segment.
    rtMinDischarge :: !Float
    -- | Minimum number of connected tiles in a river network for it to
    -- be retained.  Networks shorter than this are pruned (their
    -- segment counts are set to zero).  This removes 1–3 tile stubs
    -- that look like "rivers to nowhere".
  , rtMinNetworkTiles :: !Int
    -- | Elevation offset below @waterLevel@ used for segment
    -- eligibility.  Tiles whose elevation exceeds
    -- @waterLevel - rtCoastalZoneOffset@ are eligible, allowing
    -- river segments to render through the coastal band that
    -- coastal smoothing may push slightly below sea level.
  , rtCoastalZoneOffset :: !Float
    -- | Maximum flow-path length (in tiles) from a network root to
    -- an ocean or grid-boundary tile.  Networks whose root is
    -- farther than this from open water are pruned.  Set to 0 to
    -- disable the check.
  , rtMaxSinkDistance :: !Int
    -- | Minimum slope (elevation difference to the downstream
    -- neighbour in the /actual/ terrain) for a segment to be
    -- emitted.  Tiles where the gradient is below this threshold
    -- are treated as diffuse sheet-flow and suppressed, even if
    -- discharge is sufficient.  Set to 0 to disable.
  , rtMinSlope :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON RiverTopologyConfig where
  toJSON = genericToJSON (configOptions "rt")

instance FromJSON RiverTopologyConfig where
  parseJSON v = genericParseJSON (configOptions "rt")
                  (mergeDefaults (toJSON defaultRiverTopologyConfig) v)

-- | Default topology configuration.
--
-- The slope and ocean-reachability filters operate on the
-- /depression-filled/ surface ('elevRouting'), which guarantees
-- positive downstream gradients along every flow path.  Thresholds
-- can therefore be set modestly without accidentally pruning rivers
-- that traverse filled depressions.
defaultRiverTopologyConfig :: RiverTopologyConfig
defaultRiverTopologyConfig = RiverTopologyConfig
  { rtMinDischarge     = 2.0
  , rtMinNetworkTiles  = 5
  , rtCoastalZoneOffset = 0.06
  , rtMaxSinkDistance  = 100
  , rtMinSlope         = 5e-5
  }

-- | Map a grid-space direction (from→to index) to a 'HexEdge'.
--
-- The grid stores tiles in row-major axial coordinates @(q, r)@ where
-- @index = r * gridW + q@.  The 6 hex neighbours map to:
--
--   * @(+1,  0)@ → E  (edge 0)
--   * @(+1, -1)@ → NE (edge 1)
--   * @( 0, -1)@ → NW (edge 2)
--   * @(-1,  0)@ → W  (edge 3)
--   * @(-1, +1)@ → SW (edge 4)
--   * @( 0, +1)@ → SE (edge 5)
--
-- Returns 'EdgeNone' (255) when the two indices are not hex neighbours.
gridDirToHexEdge :: Int -> Int -> Int -> HexEdge
gridDirToHexEdge gridW fromIdx toIdx =
  let dx = (toIdx `mod` gridW) - (fromIdx `mod` gridW)
      dy = (toIdx `div` gridW) - (fromIdx `div` gridW)
  in case (dx, dy) of
       ( 1,  0) -> HexEdge 0   -- E
       ( 1, -1) -> HexEdge 1   -- NE
       ( 0, -1) -> HexEdge 2   -- NW
       (-1,  0) -> HexEdge 3   -- W
       (-1,  1) -> HexEdge 4   -- SW
       ( 0,  1) -> HexEdge 5   -- SE
       _        -> EdgeNone

-- | Return the opposite hex edge.
--
-- >>> oppositeEdge (HexEdge 0)
-- HexEdge 3
oppositeEdge :: HexEdge -> HexEdge
oppositeEdge (HexEdge e)
  | e <= 5    = HexEdge ((e + 3) `mod` 6)
  | otherwise = EdgeNone

-- | Compute per-tile river segments from flow-direction and hydrological data.
--
-- Returns parallel vectors suitable for storage in 'RiverChunk':
--
--   * offset index (length = tileCount + 1)
--   * entry-edge per segment
--   * exit-edge per segment
--   * discharge per segment
--   * Strahler order per segment
--
-- Only tiles whose /original/ elevation exceeds the /effective/ water
-- level (@waterLevel - rtCoastalZoneOffset@) produce segments.  This
-- allows river segments to render through the narrow coastal band that
-- erosion/hypsometry may push slightly below sea level, preventing
-- visual "rivers to nowhere" at the coast.
--
-- Slope computation and ocean-reachability tracing use the
-- /depression-filled/ elevation ('elevRouting'), which is consistent
-- with the surface on which flow directions were computed.  Tile
-- eligibility (land vs. submerged classification) uses the /original/
-- elevation ('elevOrig'), reflecting the terrain the player sees.
--
-- After network-size pruning, an /ocean-reachability/ check traces
-- each network root's downstream flow on 'elevRouting'.  Networks
-- whose root is farther than 'rtMaxSinkDistance' tiles from an ocean
-- (or grid-boundary) tile are pruned as inland sheet-flow artifacts.
--
-- A per-tile /minimum-slope/ filter ('rtMinSlope') suppresses segments
-- whose gradient to the downstream tile (on the routing surface) is
-- below the threshold, eliminating epsilon-gradient flat-area segments.
computeRiverSegments
  :: RiverTopologyConfig
  -> Int              -- ^ gridW
  -> Int              -- ^ gridH
  -> U.Vector Int     -- ^ flow directions (per-tile downstream index, -1 = sink)
  -> U.Vector Float   -- ^ discharge per tile
  -> U.Vector Word16  -- ^ Strahler order per tile
  -> U.Vector Float   -- ^ elevRouting — depression-filled elevation used for
                      --   slope computation and ocean-reachability tracing
  -> U.Vector Float   -- ^ elevOrig — original (unfilled) elevation used for
                      --   tile eligibility (land vs. submerged classification)
  -> Float            -- ^ waterLevel — tiles at or below this are ineligible
  -> ( U.Vector Int     -- offsets
     , U.Vector Word8   -- entry edges
     , U.Vector Word8   -- exit edges
     , U.Vector Float   -- discharge
     , U.Vector Word16  -- order
     )
computeRiverSegments cfg gridW gridH flow discharge order elevRouting elevOrig waterLevel = runST $ do
  let n       = U.length flow
      minDisc = rtMinDischarge cfg
      minNet  = rtMinNetworkTiles cfg
      -- Effective water level lowered by the coastal zone offset so
      -- that tiles in the coastal smoothing band remain eligible.
      effWL   = waterLevel - rtCoastalZoneOffset cfg
      minSlp  = rtMinSlope cfg
      maxSink = rtMaxSinkDistance cfg

  -- First pass: count segments per tile so we can allocate offsets.
  -- A tile has ≥1 segment if its discharge ≥ minDischarge.
  -- Additional segments come from incoming upstream neighbours whose
  -- discharge also exceeds the threshold.  However, to keep it simple
  -- we record *one* segment per tile representing the main channel,
  -- plus one extra segment for each additional upstream tributary that
  -- meets the threshold *at the upstream tile*.
  --
  -- Strategy:
  --   For each tile i with discharge ≥ min:
  --     - Collect upstream neighbours (tiles j where flow[j]==i, discharge[j]≥min)
  --     - If there are 0 upstream: 1 segment (headwater → exit)
  --     - If there are 1 upstream: 1 segment (entry from upstream → exit)
  --     - If there are N≥2 upstream: N segments (one per upstream entry → exit)

  segCounts <- UM.replicate n (0 :: Int)

  -- Build reverse adjacency:  for each tile, which neighbours flow into it?
  -- We'll just iterate and count first, then collect.
  -- Only land tiles (elevation > effective WL) are eligible for segments.
  -- Additionally, tiles whose slope to downstream is below rtMinSlope
  -- are treated as sheet flow and excluded.
  forM_ [0 .. n - 1] $ \i -> do
    let d = discharge U.! i
        hOrig = elevOrig U.! i
        fDir = flow U.! i
        slope = if fDir >= 0
                  then abs (elevRouting U.! i - elevRouting U.! fDir)
                  else 1.0  -- sinks are always steep enough
    when (d >= minDisc && hOrig > effWL && slope >= minSlp) $ do
      let upCount = countUpstream flow discharge minDisc gridW gridH n i
      UM.write segCounts i (max 1 upCount)

  -- Network pruning (Phase 12.2): remove networks with fewer tiles
  -- than rtMinNetworkTiles.  We trace each eligible tile's flow path
  -- down to a network root (sink or ineligible tile) using path
  -- compression, count network sizes, then zero out small networks.
  when (minNet > 1) $ do
    -- Assign a network root to each eligible tile via union-find style
    -- path compression through the flow graph.
    netRoot <- UM.replicate n (-1 :: Int)
    -- First, mark each eligible tile with an initial root of itself.
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ UM.write netRoot i i

    -- Resolve: trace downstream.  The root of a chain is the deepest
    -- eligible tile whose downstream is either a sink or ineligible.
    let resolve !i nr = do
          r <- UM.read nr i
          if r < 0
            then pure i  -- not eligible, shouldn't happen in normal flow
            else do
              let !d = flow U.! i
              if d < 0
                then pure i  -- sink — this tile is root
                else do
                  dCnt <- UM.read segCounts d
                  if dCnt <= 0
                    then pure i  -- downstream is ineligible — this tile is root
                    else do
                      dRoot <- UM.read nr d
                      if dRoot == d
                        then do  -- downstream hasn't been resolved yet — resolve it first
                          r' <- resolve d nr
                          UM.write nr i r'
                          pure r'
                        else do
                          UM.write nr i dRoot
                          pure dRoot

    -- Resolve all eligible tiles (iterate highest-elevation first isn't
    -- necessary since resolve does recursive tracing, but a simple
    -- forward pass works with path compression).
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        _ <- resolve i netRoot
        pure ()

    -- Count tiles per network root.
    netSize <- UM.replicate n (0 :: Int)
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        r <- UM.read netRoot i
        UM.modify netSize (+ 1) r

    -- Zero out segment counts for small networks.
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        r <- UM.read netRoot i
        sz <- UM.read netSize r
        when (sz < minNet) $ UM.write segCounts i 0

  -- Ocean-reachability pruning: trace each network root's flow
  -- downstream.  If the path does not reach a tile with
  -- elev ≤ waterLevel (or a grid boundary) within maxSinkDistance
  -- tiles, the network is pruned.
  when (maxSink > 0) $ do
    -- Recompute network roots (may have changed after size pruning).
    netRoot2 <- UM.replicate n (-1 :: Int)
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ UM.write netRoot2 i i
    let resolve2 !i nr = do
          r <- UM.read nr i
          if r < 0 then pure i
          else do
            let !d = flow U.! i
            if d < 0 then pure i
            else do
              dCnt <- UM.read segCounts d
              if dCnt <= 0 then pure i
              else do
                dRoot <- UM.read nr d
                if dRoot == d
                  then do
                    r' <- resolve2 d nr
                    UM.write nr i r'
                    pure r'
                  else do
                    UM.write nr i dRoot
                    pure dRoot
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        _ <- resolve2 i netRoot2
        pure ()
    -- For each network root, trace downstream through the flow graph
    -- (including ineligible tiles) up to maxSink steps.  If we reach
    -- a tile with elevRouting ≤ waterLevel or a grid boundary, the
    -- network is ocean-connected.
    rootReach <- UM.replicate n False
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        r <- UM.read netRoot2 i
        when (r == i) $ do  -- this tile is a root
          let traceOcean !cur !steps
                | steps > maxSink = pure False
                | cur < 0 || cur >= n = pure True   -- boundary exit
                | elevRouting U.! cur < waterLevel = pure True  -- reached ocean
                | otherwise =
                    let !next = flow U.! cur
                    in if next < 0
                       then pure False  -- inland sink
                       else traceOcean next (steps + 1)
          reached <- traceOcean i 0
          UM.write rootReach i reached
    -- Zero out networks whose root does not reach ocean.
    forM_ [0 .. n - 1] $ \i -> do
      c <- UM.read segCounts i
      when (c > 0) $ do
        r <- UM.read netRoot2 i
        ok <- UM.read rootReach r
        when (not ok) $ UM.write segCounts i 0

  -- Build offsets from counts
  counts <- U.freeze segCounts
  let !totalSegs = U.sum counts
  offsets <- UM.new (n + 1)
  UM.write offsets 0 (0 :: Int)
  forM_ [0 .. n - 1] $ \i -> do
    prev <- UM.read offsets i
    UM.write offsets (i + 1) (prev + counts U.! i)

  -- Allocate segment data
  entryEdges  <- UM.replicate totalSegs (255 :: Word8)
  exitEdges   <- UM.replicate totalSegs (255 :: Word8)
  segDisc     <- UM.replicate totalSegs (0 :: Float)
  segOrd      <- UM.replicate totalSegs (0 :: Word16)

  -- Second pass: fill segment data (only tiles whose count survived pruning)
  forM_ [0 .. n - 1] $ \i -> do
    when (counts U.! i > 0) $ do
      let d = discharge U.! i
      off <- UM.read offsets i
      let exitDir  = flow U.! i
          exitE    = if exitDir < 0
                       then 255  -- sink
                       else let (HexEdge e) = gridDirToHexEdge gridW i exitDir in e
          ups      = upstreamList flow discharge minDisc gridW gridH n i
          myOrder  = order U.! i
      case ups of
        [] -> do
          -- Headwater: source → exit
          UM.write entryEdges off 255
          UM.write exitEdges  off exitE
          UM.write segDisc    off d
          UM.write segOrd     off myOrder
        [u] -> do
          -- Single upstream: entry from u → exit
          let (HexEdge entryE) = oppositeEdge (gridDirToHexEdge gridW u i)
          UM.write entryEdges off entryE
          UM.write exitEdges  off exitE
          UM.write segDisc    off d
          UM.write segOrd     off myOrder
        _ -> do
          -- Multiple upstream: one segment per tributary
          forM_ (zip [0..] ups) $ \(idx, u) -> do
            let (HexEdge entryE) = oppositeEdge (gridDirToHexEdge gridW u i)
                segIdx = off + idx
            UM.write entryEdges segIdx entryE
            UM.write exitEdges  segIdx exitE
            UM.write segDisc    segIdx (discharge U.! u)
            UM.write segOrd     segIdx (order U.! u)

  fOffsets    <- U.freeze offsets
  fEntryEdges <- U.freeze entryEdges
  fExitEdges  <- U.freeze exitEdges
  fSegDisc    <- U.freeze segDisc
  fSegOrd     <- U.freeze segOrd
  pure (fOffsets, fEntryEdges, fExitEdges, fSegDisc, fSegOrd)

-- | Count upstream hex neighbours of tile @i@ whose flow points to @i@
-- and whose discharge meets the threshold.
countUpstream :: U.Vector Int -> U.Vector Float -> Float -> Int -> Int -> Int -> Int -> Int
countUpstream flow discharge minDisc gridW gridH _n i =
  let check j = flow U.! j == i && discharge U.! j >= minDisc
  in length $ filter check (hexNeighborIndices gridW gridH i)

-- | List upstream hex neighbour indices of tile @i@ meeting the
-- discharge threshold.
upstreamList :: U.Vector Int -> U.Vector Float -> Float -> Int -> Int -> Int -> Int -> [Int]
upstreamList flow discharge minDisc gridW gridH _n i =
  let check j = flow U.! j == i && discharge U.! j >= minDisc
  in filter check (hexNeighborIndices gridW gridH i)
