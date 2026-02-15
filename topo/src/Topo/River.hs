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
  } deriving (Eq, Show, Generic)

instance ToJSON RiverTopologyConfig where
  toJSON = genericToJSON (configOptions "rt")

instance FromJSON RiverTopologyConfig where
  parseJSON v = genericParseJSON (configOptions "rt")
                  (mergeDefaults (toJSON defaultRiverTopologyConfig) v)

-- | Default topology configuration.
defaultRiverTopologyConfig :: RiverTopologyConfig
defaultRiverTopologyConfig = RiverTopologyConfig
  { rtMinDischarge = 2.0
  , rtMinNetworkTiles = 5
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
-- Only tiles whose elevation exceeds the supplied @waterLevel@ produce
-- segments; submerged tiles are skipped entirely.
computeRiverSegments
  :: RiverTopologyConfig
  -> Int              -- ^ gridW
  -> Int              -- ^ gridH
  -> U.Vector Int     -- ^ flow directions (per-tile downstream index, -1 = sink)
  -> U.Vector Float   -- ^ discharge per tile
  -> U.Vector Word16  -- ^ Strahler order per tile
  -> U.Vector Float   -- ^ elevation per tile
  -> Float            -- ^ waterLevel — tiles at or below this are ineligible
  -> ( U.Vector Int     -- offsets
     , U.Vector Word8   -- entry edges
     , U.Vector Word8   -- exit edges
     , U.Vector Float   -- discharge
     , U.Vector Word16  -- order
     )
computeRiverSegments cfg gridW gridH flow discharge order elev waterLevel = runST $ do
  let n       = U.length flow
      minDisc = rtMinDischarge cfg
      minNet  = rtMinNetworkTiles cfg

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
  -- Only land tiles (elevation > waterLevel) are eligible for segments.
  forM_ [0 .. n - 1] $ \i -> do
    let d = discharge U.! i
        h = elev U.! i
    when (d >= minDisc && h > waterLevel) $ do
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
