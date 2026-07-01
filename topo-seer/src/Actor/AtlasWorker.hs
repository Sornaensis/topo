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
  , atlasWorkerPaddedViewport
  , enqueueAtlasBuildWork
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (AtlasResultRef, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Topo (WorldConfig(..))
import Seer.Render.Viewport (visibleChunkKeys)
import UI.OverlayExtract (extractOverlayField)
import UI.HexGeometry (renderHexRadiusPx)
import UI.RiverRender (RiverGeometry(..), buildChunkRiverGeometry, defaultRiverRenderConfig, scaleRiverWidths)
import UI.TerrainAtlas (AtlasChunkGeometry(..), AtlasTileGeometry(..), attachRiverOverlay, composeTilesFromGeometry, mergeChunkGeometry)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry, buildDayNightGeometry)


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
  , abDayNightFn :: !(Maybe (Int -> Int -> Float))
  }

atlasWorkerPaddedViewport :: WorldConfig -> (Float, Float) -> Float -> (Int, Int) -> ((Float, Float), Float, (Int, Int))
atlasWorkerPaddedViewport config (panX, panY) zoom (winW, winH) =
  let chunkPxSize = wcChunkSize config * renderHexRadiusPx * 2
      padWorld = fromIntegral (chunkPxSize * 2) :: Float
      zoom' = max 0.001 zoom
      paddedWin = ( winW + ceiling (2 * padWorld * zoom')
                  , winH + ceiling (2 * padWorld * zoom')
                  )
      paddedPan = (panX + padWorld, panY + padWorld)
  in (paddedPan, zoom', paddedWin)

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
        -- Build geometry for visible-viewport chunks plus extra padding
        -- (2 chunk-rings beyond the base viewport). This avoids processing
        -- the entire world while still eliminating blank tiles during
        -- normal panning. Fast panning beyond the padding triggers the
        -- next scheduled rebuild (30ms poll).
        --
        -- Apply symmetric padding in the canonical base world frame used by
        -- visibleChunkKeys.  Expanding only the window biases coverage toward
        -- right/bottom; shifting the pan by the world pad expands left/top too.
        (paddedPan, zoom', paddedWin) = atlasWorkerPaddedViewport config (abPanOffset job) (abZoom job) (abWindowSize job)
        visibleKeys = visibleChunkKeys config paddedPan zoom' paddedWin (tsTerrainChunks terrainSnap)
        chunkPairs  = [ (k, chunk)
                      | k <- visibleKeys
                      , Just chunk <- [IntMap.lookup k (tsTerrainChunks terrainSnap)]
                      ]
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
    -- Build day/night overlay geometry when a brightness function is provided.
    -- This produces an independent overlay (black + alpha) that can be
    -- cached and drawn separately from the base view-mode tiles.
    dayNightGeomPairs <- case abDayNightFn job of
      Nothing -> pure []
      Just dnFn -> forM chunkPairs $ \(k, chunk) -> do
        let geom = buildDayNightGeometry (abHexRadius job) config dnFn k chunk
        _ <- evaluate geom
        threadDelay 100
        pure (k, geom)
    let geometryMap = IntMap.fromList geomPairs
        dayNightGeometryMap = IntMap.fromList dayNightGeomPairs
        -- Build river geometry for visible chunks only (matching the
        -- padded viewport). Cross-chunk neighbour lookups still use the
        -- full tsTerrainChunks map for correct boundary rendering.
        visibleTerrainChunks = IntMap.fromList chunkPairs
        riverGeoMap = case mode of
          ViewBiome -> IntMap.mapMaybeWithKey
            (\ cid _chunk -> buildChunkRiverGeometry (scaleRiverWidths (abHexRadius job) defaultRiverRenderConfig) config (abHexRadius job) cid (tsRiverChunks terrainSnap) (tsTerrainChunks terrainSnap))
            visibleTerrainChunks
          _ -> IntMap.empty
        baseTiles = composeTilesFromGeometry geometryMap (abHexRadius job) (abAtlasScale job)
        tiles = attachRiverOverlay riverGeoMap baseTiles
        -- Day/night overlay tiles share the same tiling structure but
        -- have no river overlay.
        dayNightTiles = if IntMap.null dayNightGeometryMap
          then Nothing
          else Just (composeTilesFromGeometry dayNightGeometryMap (abHexRadius job) (abAtlasScale job))
        buildResult tile mbDnTile = AtlasBuildResult
          { abrKey       = abKey job
          , abrHexRadius = abHexRadius job
          , abrTile      = tile
          , abrDayNightTile = mbDnTile
          }
    if null tiles
      then threadDelay 100 >> pure st
      else do
        let dnTileList = maybe (repeat Nothing) (map Just) dayNightTiles
        mapM_ (\(tile, mbDnTile) -> do
                   -- Pre-merge terrain + river chunks into a single geometry
                   -- so the render thread avoids SV.concat and index-rebasing
                   -- allocations during texture upload.
                   let merged = mergeChunkGeometry (atgChunks tile ++ atgRiverOverlay tile)
                       tile' = tile { atgChunks = [merged], atgRiverOverlay = [] }
                   -- Pre-merge day/night overlay if present.
                   let mbDnTile' = case mbDnTile of
                         Just dnTile ->
                           let dnMerged = mergeChunkGeometry (atgChunks dnTile)
                           in Just (dnTile { atgChunks = [dnMerged], atgRiverOverlay = [] })
                         Nothing -> Nothing
                   -- Force the merged storable vectors on the worker thread.
                   _ <- evaluate (acgVertices merged)
                   _ <- evaluate (acgIndices merged)
                   case mbDnTile' of
                     Just dt -> case atgChunks dt of
                       dnM:_ -> do
                         _ <- evaluate (acgVertices dnM)
                         _ <- evaluate (acgIndices dnM)
                         pure ()
                       [] -> pure ()
                     Nothing -> pure ()
                   let r = buildResult tile' mbDnTile'
                   _ <- evaluate r
                   pushAtlasResult (abResultRef job) r) (zip tiles dnTileList)
        threadDelay 100
        pure st
|]

enqueueAtlasBuildWork :: ActorHandle AtlasWorker (Protocol AtlasWorker) -> AtlasBuild -> IO ()
enqueueAtlasBuildWork handle job =
  cast @"build" handle #build job
