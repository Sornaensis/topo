{-# LANGUAGE BangPatterns #-}

-- | Chunk-slicing helpers for hydrology outputs.
module Topo.Hydrology.ChunkSlice
  ( sliceRiverChunk
  , sliceGroundwaterChunk
  ) where

import Topo.TerrainGrid (chunkGridSlice, chunkGridSliceGeneric)
import Topo.Types
import qualified Data.Vector.Unboxed as U

-- | Slice a per-chunk 'RiverChunk' from full-grid river results.
sliceRiverChunk :: WorldConfig -> ChunkCoord -> Int -> RiverChunk -> Int -> TerrainChunk -> RiverChunk
sliceRiverChunk config minCoord gridW rivers key _chunk =
  let flowSlice = chunkGridSliceGeneric config minCoord gridW (rcFlowDir rivers) key
      size  = wcChunkSize config
      n     = size * size
      ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      ChunkCoord minCx minCy = minCoord
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      globalOffsets = rcSegOffsets rivers
      globalEntry   = rcSegEntryEdge rivers
      globalExit    = rcSegExitEdge rivers
      globalDisc    = rcSegDischarge rivers
      globalOrd     = rcSegOrder rivers
      segCounts = U.generate n (\i ->
        let lx = i `mod` size
            ly = i `div` size
            gx = baseX + lx
            gy = baseY + ly
            gi = gy * gridW + gx
        in if gi + 1 < U.length globalOffsets
           then globalOffsets U.! (gi + 1) - globalOffsets U.! gi
           else 0)
      localOffsets = U.scanl' (+) 0 segCounts
      globalSegIndices = U.concatMap (\i ->
        let lx = i `mod` size
            ly = i `div` size
            gx = baseX + lx
            gy = baseY + ly
            gi = gy * gridW + gx
            start = globalOffsets U.! gi
            end   = globalOffsets U.! (gi + 1)
        in U.generate (end - start) (+ start)) (U.enumFromN 0 n)
      localEntry = U.map (globalEntry U.!) globalSegIndices
      localExit  = U.map (globalExit U.!) globalSegIndices
      localDisc  = U.map (globalDisc U.!) globalSegIndices
      localOrd   = U.map (globalOrd U.!) globalSegIndices
  in RiverChunk
    { rcFlowAccum = chunkGridSlice config minCoord gridW (rcFlowAccum rivers) key
    , rcDischarge = chunkGridSlice config minCoord gridW (rcDischarge rivers) key
    , rcChannelDepth = chunkGridSlice config minCoord gridW (rcChannelDepth rivers) key
    , rcRiverOrder = chunkGridSliceGeneric config minCoord gridW (rcRiverOrder rivers) key
    , rcBasinId = chunkGridSliceGeneric config minCoord gridW (rcBasinId rivers) key
    , rcBaseflow = chunkGridSlice config minCoord gridW (rcBaseflow rivers) key
    , rcErosionPotential = chunkGridSlice config minCoord gridW (rcErosionPotential rivers) key
    , rcDepositPotential = chunkGridSlice config minCoord gridW (rcDepositPotential rivers) key
    , rcFlowDir = flowSlice
    , rcSegOffsets = localOffsets
    , rcSegEntryEdge = localEntry
    , rcSegExitEdge = localExit
    , rcSegDischarge = localDisc
    , rcSegOrder = localOrd
    }

-- | Slice a per-chunk 'GroundwaterChunk' from full-grid groundwater results.
sliceGroundwaterChunk :: WorldConfig -> ChunkCoord -> Int -> GroundwaterChunk -> Int -> TerrainChunk -> GroundwaterChunk
sliceGroundwaterChunk config minCoord gridW groundwater key _chunk =
  GroundwaterChunk
    { gwStorage = chunkGridSlice config minCoord gridW (gwStorage groundwater) key
    , gwRecharge = chunkGridSlice config minCoord gridW (gwRecharge groundwater) key
    , gwDischarge = chunkGridSlice config minCoord gridW (gwDischarge groundwater) key
    , gwBasinId = chunkGridSliceGeneric config minCoord gridW (gwBasinId groundwater) key
    , gwInfiltration = U.empty
    , gwWaterTableDepth = U.empty
    , gwRootZoneMoisture = U.empty
    }
