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
  , atlasBuildResultsForTiles
  , atlasBuildIsCurrent
  , enqueueAtlasBuildWork
  ) where

import Actor.AtlasCache (AtlasKey)
import Actor.AtlasFreshness (AtlasFreshnessRef, atlasTargetBuildIsFresh, readAtlasFreshnessRef)
import Actor.AtlasResult (AtlasBuildId, AtlasBuildResult(..), AtlasBuildTarget(..), AtlasDayNightTile(..), AtlasTileSetManifest(..))
import Actor.AtlasResultBroker (AtlasResultRef, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (ViewMode(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.List (find)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Topo (WorldConfig(..))
import Seer.Render.Viewport (visibleChunkKeys)
import UI.OverlayExtract (extractOverlayField)
import UI.HexGeometry (normalizeHexBounds, renderHexRadiusPx)
import UI.RiverRender (RiverGeometry(..), buildChunkRiverGeometry, defaultRiverRenderConfig, scaleRiverWidths)
import UI.TerrainAtlas (AtlasChunkGeometry(..), AtlasTileGeometry(..), attachRiverOverlay, composeTilesFromGeometry, mergeChunkGeometry)
import UI.DayNight (DayNightKey, DayNightSpec)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry, buildDayNightGeometry)


-- | Payload for CPU-side atlas builds executed by pooled workers.
data AtlasBuild = AtlasBuild
  { abBuildId    :: !AtlasBuildId
  , abKey        :: AtlasKey
  , abViewMode   :: ViewMode
  , abWaterLevel :: Float
  , abTerrain    :: TerrainSnapshot
  , abHexRadius  :: Int
  , abAtlasScale :: Int
  , abPanOffset  :: !(Float, Float)
  , abZoom       :: !Float
  , abWindowSize :: !(Int, Int)
  , abSnapshotVersion :: !SnapshotVersion
  , abResultRef  :: !AtlasResultRef
  , abFreshnessRef :: !AtlasFreshnessRef
  , abDayNightSpec :: !(Maybe DayNightSpec)
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

atlasBuildTarget :: AtlasBuild -> AtlasBuildTarget
atlasBuildTarget job = AtlasBuildTarget
  { abtKey = abKey job
  , abtSnapshotVersion = abSnapshotVersion job
  , abtHexRadius = abHexRadius job
  , abtAtlasScale = abAtlasScale job
  }

-- | Build result payloads for every base atlas tile, pairing day/night overlay
-- geometry by normalised tile bounds when an overlay tile exists.
atlasBuildResultsForTiles
  :: AtlasBuildId
  -> AtlasKey
  -> SnapshotVersion
  -> Int
  -> Int
  -> [AtlasTileGeometry]
  -> Maybe (DayNightKey, [AtlasTileGeometry])
  -> [AtlasBuildResult]
atlasBuildResultsForTiles buildId key snapshotVersion hexRadius atlasScale tiles dayNightTiles =
  let normalisedBounds tile = normalizeHexBounds hexRadius (atgBounds tile)
      expectedBounds = map normalisedBounds tiles
      manifest = AtlasTileSetManifest
        { atsmBuildId = buildId
        , atsmKey = key
        , atsmSnapshotVersion = snapshotVersion
        , atsmHexRadius = hexRadius
        , atsmAtlasScale = atlasScale
        , atsmExpectedTileCount = length tiles
        , atsmExpectedBounds = expectedBounds
        }
      dayNightLookup = case dayNightTiles of
        Nothing -> []
        Just (dnKey, dnTiles) ->
          map (\dnTile -> (normalisedBounds dnTile, AtlasDayNightTile dnKey dnTile)) dnTiles
      dayNightFor tile = snd <$> find ((== normalisedBounds tile) . fst) dayNightLookup
      buildResult ix tile = AtlasBuildResult
        { abrKey = key
        , abrSnapshotVersion = snapshotVersion
        , abrHexRadius = hexRadius
        , abrManifest = manifest
        , abrTileIndex = ix
        , abrTileBounds = normalisedBounds tile
        , abrTile = tile
        , abrDayNightTile = dayNightFor tile
        }
  in zipWith buildResult [0..] tiles

atlasBuildIsCurrent :: AtlasBuild -> IO Bool
atlasBuildIsCurrent job = do
  latest <- readAtlasFreshnessRef (abFreshnessRef job)
  pure (atlasTargetBuildIsFresh latest (atlasBuildTarget job) (abBuildId job))

traverseFresh :: AtlasBuild -> [a] -> (a -> IO b) -> IO (Maybe [b])
traverseFresh job = go []
  where
    go acc [] _ = pure (Just (reverse acc))
    go acc (x:xs) action = do
      fresh <- atlasBuildIsCurrent job
      if fresh
        then do
          y <- action x
          go (y:acc) xs action
        else pure Nothing

forFresh_ :: AtlasBuild -> [a] -> (a -> IO ()) -> IO ()
forFresh_ job = go
  where
    go [] _ = pure ()
    go (x:xs) action = do
      fresh <- atlasBuildIsCurrent job
      if fresh
        then action x >> go xs action
        else pure ()

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
    freshAtStart <- atlasBuildIsCurrent job
    if not freshAtStart
      then pure st
      else do
        -- Build per-chunk geometry in IO, releasing the capability between
        -- each chunk via threadDelay.  Storable vector allocation (pinned
        -- memory) bypasses GHC's allocation counter, and yield only
        -- reorders within the same capability's ready queue.  threadDelay
        -- removes the green thread entirely, guaranteeing the bound main
        -- thread (render loop) can reclaim its capability.
        mbGeomPairs <- traverseFresh job chunkPairs $ \(k, chunk) -> do
          let geom = buildChunkGeometry (abHexRadius job) config mode waterLevel climateChunks weatherChunks vegChunks (IntMap.lookup k overlayMap) k chunk
          _ <- evaluate geom
          threadDelay 100  -- 0.1ms, releases capability
          pure (k, geom)
        case mbGeomPairs of
          Nothing -> pure st
          Just geomPairs -> do
            -- Build day/night overlay geometry when a brightness function is provided.
            -- This produces an independent overlay (black + alpha) that can be
            -- cached and drawn separately from the base view-mode tiles.
            mbDayNightGeomPairs <- case abDayNightSpec job of
              Nothing -> pure (Just [])
              Just (_, dnFn) -> traverseFresh job chunkPairs $ \(k, chunk) -> do
                let geom = buildDayNightGeometry (abHexRadius job) config dnFn k chunk
                _ <- evaluate geom
                threadDelay 100
                pure (k, geom)
            case mbDayNightGeomPairs of
              Nothing -> pure st
              Just dayNightGeomPairs -> do
                freshBeforeCompose <- atlasBuildIsCurrent job
                if not freshBeforeCompose
                  then pure st
                  else do
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
                        dayNightTiles = case abDayNightSpec job of
                          Just (dnKey, _) | not (IntMap.null dayNightGeometryMap) ->
                            Just (dnKey, composeTilesFromGeometry dayNightGeometryMap (abHexRadius job) (abAtlasScale job))
                          _ -> Nothing
                        results = atlasBuildResultsForTiles (abBuildId job) (abKey job) (abSnapshotVersion job) (abHexRadius job) (abAtlasScale job) tiles dayNightTiles
                    if null tiles
                      then threadDelay 100 >> pure st
                      else do
                        forFresh_ job results $ \result -> do
                          let tile = abrTile result
                              mbDnTile = abrDayNightTile result
                          -- Pre-merge terrain + river chunks into a single geometry
                          -- so the render thread avoids SV.concat and index-rebasing
                          -- allocations during texture upload.
                          let merged = mergeChunkGeometry (atgChunks tile ++ atgRiverOverlay tile)
                              tile' = tile { atgChunks = [merged], atgRiverOverlay = [] }
                          -- Pre-merge day/night overlay if present.
                          let mbDnTile' = case mbDnTile of
                                Just dnResult ->
                                  let dnTile = adntTile dnResult
                                      dnMerged = mergeChunkGeometry (atgChunks dnTile)
                                  in Just (dnResult { adntTile = dnTile { atgChunks = [dnMerged], atgRiverOverlay = [] } })
                                Nothing -> Nothing
                          -- Force the merged storable vectors on the worker thread.
                          _ <- evaluate (acgVertices merged)
                          _ <- evaluate (acgIndices merged)
                          case mbDnTile' of
                            Just dt -> case atgChunks (adntTile dt) of
                              dnM:_ -> do
                                _ <- evaluate (acgVertices dnM)
                                _ <- evaluate (acgIndices dnM)
                                pure ()
                              [] -> pure ()
                            Nothing -> pure ()
                          let r = result { abrTile = tile', abrDayNightTile = mbDnTile' }
                          _ <- evaluate r
                          pushAtlasResult (abResultRef job) r
                        threadDelay 100
                        pure st
|]

enqueueAtlasBuildWork :: ActorHandle AtlasWorker (Protocol AtlasWorker) -> AtlasBuild -> IO ()
enqueueAtlasBuildWork handle job =
  cast @"build" handle #build job
