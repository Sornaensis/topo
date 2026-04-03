{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Actor.AtlasWorker
  ( AtlasWorker
  , AtlasBuild(..)
  , AtlasBuildResult(..)
  , atlasWorkerActorDef
  , enqueueAtlasBuildWork
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (AtlasResultRef, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Topo (WorldConfig(..))
import Seer.Render.Viewport (visibleChunkKeys)
import UI.OverlayExtract (extractOverlayField)
import UI.RiverRender (RiverGeometry(..), buildChunkRiverGeometry, defaultRiverRenderConfig, scaleRiverWidths)
import UI.TerrainAtlas (AtlasChunkGeometry(..), AtlasTileGeometry(..), attachRiverOverlay, composeTilesFromGeometry)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry)


-- | Payload for CPU-side atlas builds executed by pooled workers.
data AtlasBuild = AtlasBuild
  { abKey        :: AtlasKey
  , abViewMode   :: ViewMode
  , abWaterLevel :: Float
  , abTerrain    :: TerrainSnapshot
  , abHexRadius  :: Int
  , abAtlasScale :: Int
  , abPanOffset  :: !(Float, Float)
  , abZoom       :: !Float
  , abWindowSize :: !(Int, Int)
  , abResultRef  :: !AtlasResultRef
  }

[hyperspace|
actor AtlasWorker
  state ()
  lifetime Singleton
  schedule pinned 2
  noDeps
  mailbox Unbounded

  cast build :: AtlasBuild

  initial ()
  on_ build = \job st -> do
    let terrainSnap = abTerrain job
        config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
        mode = abViewMode job
        waterLevel = abWaterLevel job
        climateChunks = tsClimateChunks terrainSnap
        weatherChunks = tsWeatherChunks terrainSnap
        vegChunks = tsVegetationChunks terrainSnap
        -- Viewport culling: only build geometry for visible chunks (+ one-chunk
        -- padding ring) to bound atlas tile sizes and avoid unnecessary work.
        -- Active at all zoom stages, including stage 0, to support huge extents.
        visibleKeys = visibleChunkKeys config (abPanOffset job) (abZoom job) (abWindowSize job) (tsTerrainChunks terrainSnap)
        visibleSet  = IntMap.fromList [(k, ()) | k <- visibleKeys]
        chunkPairs  = filter (\(k, _) -> IntMap.member k visibleSet)
                             (IntMap.toList (tsTerrainChunks terrainSnap))
        overlayMap = case mode of
          ViewOverlay name fieldIdx ->
            case extractOverlayField name fieldIdx (wcChunkSize config * wcChunkSize config) (tsOverlayStore terrainSnap) of
              Just m  -> m
              Nothing -> IntMap.empty
          _ -> IntMap.empty
    -- Build per-chunk geometry in IO, releasing the capability between
    -- each chunk via threadDelay.  Storable vector allocation (pinned
    -- memory) bypasses GHC's allocation counter, and yield only
    -- reorders within the same capability's ready queue.  threadDelay
    -- removes the green thread entirely, guaranteeing the bound main
    -- thread (render loop) can reclaim its capability.
    geomPairs <- forM chunkPairs $ \(k, chunk) -> do
      let geom = buildChunkGeometry (abHexRadius job) config mode waterLevel climateChunks weatherChunks vegChunks (IntMap.lookup k overlayMap) k chunk
      _ <- evaluate geom
      threadDelay 100  -- 0.1ms, releases capability
      pure (k, geom)
    let geometryMap = IntMap.fromList geomPairs
        -- Visible terrain chunks as a map (already culled via chunkPairs).
        -- River geometry iterates only visible chunks but still receives the
        -- full tsTerrainChunks for cross-chunk neighbour lookups.
        visibleTerrainChunks = IntMap.fromList chunkPairs
        riverGeoMap = case mode of
          ViewBiome -> IntMap.mapMaybeWithKey
            (\ cid _chunk -> buildChunkRiverGeometry (scaleRiverWidths (abHexRadius job) defaultRiverRenderConfig) config (abHexRadius job) cid (tsRiverChunks terrainSnap) (tsTerrainChunks terrainSnap))
            visibleTerrainChunks
          _ -> IntMap.empty
        baseTiles = composeTilesFromGeometry geometryMap (abHexRadius job) (abAtlasScale job)
        tiles = attachRiverOverlay riverGeoMap baseTiles
        buildResult tile = AtlasBuildResult
          { abrKey       = abKey job
          , abrHexRadius = abHexRadius job
          , abrTile      = tile
          }
    if null tiles
      then threadDelay 100 >> pure st
      else do
        mapM_ (\tile -> do
                   -- Force each chunk's SV.map vertex-transform thunks on
                   -- the worker thread.  Without this, composeTilesFromGeometry
                   -- produces lazy AtlasChunkGeometry values whose storable
                   -- vector allocations are deferred to the render thread,
                   -- causing ~250ms drain stalls during texture upload.
                   forM_ (atgChunks tile) $ \chunk -> do
                     _ <- evaluate (acgVertices chunk)
                     _ <- evaluate (acgIndices chunk)
                     pure ()
                   forM_ (atgRiverOverlay tile) $ \chunk -> do
                     _ <- evaluate (acgVertices chunk)
                     _ <- evaluate (acgIndices chunk)
                     pure ()
                   let r = buildResult tile
                   _ <- evaluate r
                   pushAtlasResult (abResultRef job) r) tiles
        threadDelay 100
        pure st
|]

enqueueAtlasBuildWork :: ActorHandle AtlasWorker (Protocol AtlasWorker) -> AtlasBuild -> IO ()
enqueueAtlasBuildWork handle job =
  cast @"build" handle #build job
