-- | Viewport chunk culling shared across atlas building, overlay drawing,
-- and brush preview rendering.
--
-- All calculations operate in the base @renderHexRadiusPx = 6@ world-space
-- coordinate frame so that results are consistent regardless of the bake-time
-- hex radius used by the current zoom stage.
module Seer.Render.Viewport
  ( AtlasChunkBounds(..)
  , AtlasViewportCoverage(..)
  , atlasPaddedViewport
  , atlasViewportCoverageCovers
  , atlasViewportCoverageFromKeys
  , currentAtlasViewportCoverage
  , emptyAtlasViewportCoverage
  , visibleChunkKeys
  ) where

import Actor.Data (TerrainSnapshot(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Seer.Render.ZoomStage (ZoomStage)
import Topo (ChunkCoord(..), ChunkId(..), WorldConfig(..), chunkCoordFromId, chunkIdFromCoord)
import UI.HexGeometry (renderHexRadiusPx, hexOriginX, hexOriginY)

-- | Inclusive chunk-coordinate bounds covered by an atlas build.
data AtlasChunkBounds = AtlasChunkBounds
  { acbMinChunkX :: !Int
  , acbMinChunkY :: !Int
  , acbMaxChunkX :: !Int
  , acbMaxChunkY :: !Int
  } deriving (Eq, Show)

-- | Compact proof that an atlas build was generated from a specific set of
-- terrain chunks.  The render resolver compares the current viewport's
-- required chunks against this set before treating a complete atlas as ready.
data AtlasViewportCoverage = AtlasViewportCoverage
  { avcChunkKeys :: ![Int]
  , avcChunkBounds :: !(Maybe AtlasChunkBounds)
  } deriving (Eq, Show)

emptyAtlasViewportCoverage :: AtlasViewportCoverage
emptyAtlasViewportCoverage = atlasViewportCoverageFromKeys []

atlasViewportCoverageFromKeys :: [Int] -> AtlasViewportCoverage
atlasViewportCoverageFromKeys keys =
  let uniqueKeys = IntSet.toAscList (IntSet.fromList keys)
      bounds = chunkBounds uniqueKeys
  in AtlasViewportCoverage
    { avcChunkKeys = uniqueKeys
    , avcChunkBounds = bounds
    }

atlasViewportCoverageCovers :: AtlasViewportCoverage -> AtlasViewportCoverage -> Bool
atlasViewportCoverageCovers covered required =
  let coveredSet = IntSet.fromList (avcChunkKeys covered)
  in all (`IntSet.member` coveredSet) (avcChunkKeys required)

chunkBounds :: [Int] -> Maybe AtlasChunkBounds
chunkBounds [] = Nothing
chunkBounds keys =
  let coords = map (chunkCoordFromId . ChunkId) keys
      xs = [x | ChunkCoord x _ <- coords]
      ys = [y | ChunkCoord _ y <- coords]
  in Just AtlasChunkBounds
    { acbMinChunkX = minimum xs
    , acbMinChunkY = minimum ys
    , acbMaxChunkX = maximum xs
    , acbMaxChunkY = maximum ys
    }

-- | Expand a camera viewport symmetrically in the base world frame before
-- culling chunks for atlas worker builds.
atlasPaddedViewport :: WorldConfig -> (Float, Float) -> Float -> (Int, Int) -> ((Float, Float), Float, (Int, Int))
atlasPaddedViewport config (panX, panY) zoom (winW, winH) =
  let chunkPxSize = wcChunkSize config * renderHexRadiusPx * 2
      padWorld = fromIntegral (chunkPxSize * 2) :: Float
      zoom' = max 0.001 zoom
      paddedWin = ( winW + ceiling (2 * padWorld * zoom')
                  , winH + ceiling (2 * padWorld * zoom')
                  )
      paddedPan = (panX + padWorld, panY + padWorld)
  in (paddedPan, zoom', paddedWin)

-- | Required chunk coverage for the currently visible viewport.  This uses the
-- unexpanded render viewport, while workers record coverage from the expanded
-- atlas viewport; panning inside the worker padding therefore stays ready.
currentAtlasViewportCoverage
  :: WorldConfig
  -> (Float, Float)
  -> Float
  -> (Int, Int)
  -> TerrainSnapshot
  -> ZoomStage
  -> AtlasViewportCoverage
currentAtlasViewportCoverage config pan zoom windowSize terrainSnap _stage
  | wcChunkSize config <= 0 = emptyAtlasViewportCoverage
  | otherwise = atlasViewportCoverageFromKeys $
      visibleChunkKeys config pan zoom windowSize (tsTerrainChunks terrainSnap)

-- | Return the 'IntMap' keys of chunks whose coordinates place them in or
-- near the camera viewport.
--
-- Instead of testing every chunk in the map (O(n)), this function inverts
-- the 'axialToScreen' mapping to compute the range of chunk coordinates
-- that could overlap the viewport, then probes the IntMap for each
-- candidate (O(k log n) where k = visible area in chunk units).
visibleChunkKeys
  :: WorldConfig
  -> (Float, Float)   -- ^ Camera pan offset @(panX, panY)@
  -> Float            -- ^ Camera zoom (clamped to @>= 0.001@)
  -> (Int, Int)       -- ^ Window size @(width, height)@
  -> IntMap.IntMap a   -- ^ Chunk map (only keys are inspected)
  -> [Int]
visibleChunkKeys config (panX, panY) zoom (winW, winH) chunks =
  let zoom'    = max 0.001 zoom
      -- Viewport bounds in world space with generous padding
      pad      = fromIntegral (wcChunkSize config)
               * (fromIntegral renderHexRadiusPx :: Float) * 2.0
      wLeft    = -panX - pad
      wRight   = fromIntegral winW / zoom' - panX + pad
      wTop     = -panY - pad
      wBot     = fromIntegral winH / zoom' - panY + pad
      -- Invert axialToScreen to get tile coordinate bounds
      -- axialToScreen: x = s*sqrt(3)*(q + r/2) + oxF
      --                y = s*1.5*r + oyF
      s        = fromIntegral renderHexRadiusPx :: Float
      cs       = fromIntegral (wcChunkSize config) :: Float
      oxF      = fromIntegral hexOriginX :: Float
      oyF      = fromIntegral hexOriginY :: Float
      -- r range from y bounds
      rMin     = (wTop - oyF) / (s * 1.5)
      rMax     = (wBot - oyF) / (s * 1.5)
      -- q range from x bounds (accounting for r/2 offset)
      sq3s     = s * sqrt 3
      qMin     = (wLeft - oxF) / sq3s - rMax / 2
      qMax     = (wRight - oxF) / sq3s - rMin / 2
      -- Convert tile ranges to chunk coordinate ranges (extra ±1 for safety)
      cxMin    = floor (qMin / cs) - 1 :: Int
      cxMax    = ceiling (qMax / cs) + 1 :: Int
      cyMin    = floor (rMin / cs) - 1 :: Int
      cyMax    = ceiling (rMax / cs) + 1 :: Int
  in [ k
     | cy <- [cyMin .. cyMax]
     , cx <- [cxMin .. cxMax]
     , let ChunkId k = chunkIdFromCoord (ChunkCoord cx cy)
     , IntMap.member k chunks
     ]
