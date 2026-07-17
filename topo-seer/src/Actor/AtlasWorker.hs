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
  , AtlasWorkerLoad(..)
  , AtlasWorkerBuildOutcome(..)
  , AtlasWorkerLoadRef
  , newAtlasWorkerLoadRef
  , readAtlasWorkerLoad
  , atlasWorkerLoadAvailable
  , atlasWorkerLoadStart
  , atlasWorkerLoadBegin
  , atlasWorkerLoadFinish
  , atlasWorkerLoadFinishOutcome
  , atlasWorkerActorDef
  , atlasWorkerPaddedViewport
  , atlasBuildResultsForTiles
  , atlasBuildIsCurrent
  , enqueueAtlasBuildWork
  ) where

import Actor.AtlasCache (AtlasKey(..), atlasKeyIsBase)
import Actor.AtlasFreshness (AtlasFreshnessRef, atlasTargetBuildIsFresh, readAtlasFreshnessRef)
import Actor.AtlasResult (AtlasBuildId, AtlasBuildResult(..), AtlasBuildTarget(..), AtlasDayNightTile(..), AtlasTileSetManifest(..))
import Actor.AtlasResultBroker (AtlasResultRef, pushAtlasResult)
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (BaseViewMode(..), LayeredViewState(..), SkyOverlayMode(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate, onException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Topo (WorldConfig(..))
import Seer.Render.Viewport (AtlasViewportCoverage, atlasPaddedViewport, atlasViewportCoverageFromKeys, visibleChunkKeys)
import UI.OverlayExtract (extractOverlayField)
import UI.HexGeometry (normalizeHexBounds)
import UI.RiverRender (RiverGeometry(..), buildChunkRiverGeometry, defaultRiverRenderConfig, scaleRiverWidths)
import UI.TerrainAtlas (AtlasChunkGeometry(..), AtlasTileGeometry(..), attachRiverOverlay, composeTilesFromGeometry, mergeChunkGeometry)
import UI.DayNight (DayNightKey, DayNightSpec)
import UI.TerrainRender (ChunkGeometry, buildChunkBaseGeometry, buildChunkGeometryForSelection, buildChunkSkyOverlayGeometryForSelection, buildDayNightGeometry)
import Topo.Weather (getWeatherNormalsFromStore)


-- | Payload for CPU-side atlas builds executed by pooled workers.
data AtlasBuild = AtlasBuild
  { abBuildId    :: !AtlasBuildId
  , abKey        :: AtlasKey
  , abViewSelection :: !LayeredViewState
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
  , abWorkerLoadRef :: !(Maybe AtlasWorkerLoadRef)
  }

-- | Lightweight shared worker backlog accounting.  The scheduler increments
-- 'awlInFlight' before casting work to a worker; workers decrement it when the
-- message is skipped or finishes, so this counts both running work and queued
-- worker-mailbox work without inspecting mailbox length.
data AtlasWorkerLoad = AtlasWorkerLoad
  { awlInFlight :: !Int
  , awlFinished :: !Int
  , awlStaleSkipped :: !Int
  , awlBuildStarted :: !Int
  , awlBuildCompleted :: !Int
  , awlStaleSkippedAtStart :: !Int
  , awlStaleCancelledDuringGeometry :: !Int
  , awlStaleCancelledBeforePublish :: !Int
  } deriving (Eq, Show)

data AtlasWorkerBuildOutcome
  = AtlasWorkerBuildCompleted
  | AtlasWorkerBuildStaleAtStart
  | AtlasWorkerBuildStaleDuringGeometry
  | AtlasWorkerBuildStaleBeforePublish
  | AtlasWorkerBuildFailed
  deriving (Eq, Show)

type AtlasWorkerLoadRef = IORef AtlasWorkerLoad

emptyAtlasWorkerLoad :: AtlasWorkerLoad
emptyAtlasWorkerLoad = AtlasWorkerLoad
  { awlInFlight = 0
  , awlFinished = 0
  , awlStaleSkipped = 0
  , awlBuildStarted = 0
  , awlBuildCompleted = 0
  , awlStaleSkippedAtStart = 0
  , awlStaleCancelledDuringGeometry = 0
  , awlStaleCancelledBeforePublish = 0
  }

newAtlasWorkerLoadRef :: IO AtlasWorkerLoadRef
newAtlasWorkerLoadRef = newIORef emptyAtlasWorkerLoad

readAtlasWorkerLoad :: AtlasWorkerLoadRef -> IO AtlasWorkerLoad
readAtlasWorkerLoad = readIORef

atlasWorkerLoadAvailable :: AtlasWorkerLoadRef -> Int -> IO Int
atlasWorkerLoadAvailable ref capacity =
  atomicModifyIORef' ref $ \load ->
    let available = max 0 (capacity - awlInFlight load)
    in (load, available)

atlasWorkerLoadStart :: AtlasWorkerLoadRef -> Int -> IO ()
atlasWorkerLoadStart ref count
  | count <= 0 = pure ()
  | otherwise = atomicModifyIORef' ref $ \load ->
      (load { awlInFlight = awlInFlight load + count }, ())

atlasWorkerLoadBegin :: Maybe AtlasWorkerLoadRef -> IO ()
atlasWorkerLoadBegin Nothing = pure ()
atlasWorkerLoadBegin (Just ref) =
  atomicModifyIORef' ref $ \load ->
    (load { awlBuildStarted = awlBuildStarted load + 1 }, ())

atlasWorkerLoadFinish :: Maybe AtlasWorkerLoadRef -> Bool -> IO ()
atlasWorkerLoadFinish ref staleSkipped =
  atlasWorkerLoadFinishOutcome ref $
    if staleSkipped then AtlasWorkerBuildStaleAtStart else AtlasWorkerBuildCompleted

atlasWorkerLoadFinishOutcome :: Maybe AtlasWorkerLoadRef -> AtlasWorkerBuildOutcome -> IO ()
atlasWorkerLoadFinishOutcome Nothing _ = pure ()
atlasWorkerLoadFinishOutcome (Just ref) outcome =
  atomicModifyIORef' ref $ \load ->
    let staleDelta = case outcome of
          AtlasWorkerBuildCompleted -> 0
          AtlasWorkerBuildFailed -> 0
          _ -> 1
        completedDelta = case outcome of
          AtlasWorkerBuildCompleted -> 1
          _ -> 0
        staleStartDelta = case outcome of
          AtlasWorkerBuildStaleAtStart -> 1
          _ -> 0
        staleGeometryDelta = case outcome of
          AtlasWorkerBuildStaleDuringGeometry -> 1
          _ -> 0
        stalePublishDelta = case outcome of
          AtlasWorkerBuildStaleBeforePublish -> 1
          _ -> 0
    in ( load
          { awlInFlight = max 0 (awlInFlight load - 1)
          , awlFinished = awlFinished load + 1
          , awlStaleSkipped = awlStaleSkipped load + staleDelta
          , awlBuildCompleted = awlBuildCompleted load + completedDelta
          , awlStaleSkippedAtStart = awlStaleSkippedAtStart load + staleStartDelta
          , awlStaleCancelledDuringGeometry = awlStaleCancelledDuringGeometry load + staleGeometryDelta
          , awlStaleCancelledBeforePublish = awlStaleCancelledBeforePublish load + stalePublishDelta
          }
       , ()
       )

atlasWorkerPaddedViewport :: WorldConfig -> (Float, Float) -> Float -> (Int, Int) -> ((Float, Float), Float, (Int, Int))
atlasWorkerPaddedViewport = atlasPaddedViewport

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
  -> AtlasViewportCoverage
  -> [AtlasTileGeometry]
  -> Maybe (DayNightKey, [AtlasTileGeometry])
  -> [AtlasBuildResult]
atlasBuildResultsForTiles buildId key snapshotVersion hexRadius atlasScale coverage tiles dayNightTiles =
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
        , atsmCoverage = coverage
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

forFresh_ :: AtlasBuild -> [a] -> (a -> IO ()) -> IO Bool
forFresh_ job = go
  where
    go [] _ = pure True
    go (x:xs) action = do
      fresh <- atlasBuildIsCurrent job
      if fresh
        then action x >> go xs action
        else pure False

runAtlasBuild :: AtlasBuild -> IO AtlasWorkerBuildOutcome
runAtlasBuild job = do
  freshAtStart <- atlasBuildIsCurrent job
  if not freshAtStart
    then pure AtlasWorkerBuildStaleAtStart
    else do
      let terrainSnap = abTerrain job
          config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
          selection = abViewSelection job
          waterLevel = abWaterLevel job
          climateChunks = tsClimateChunks terrainSnap
          weatherChunks = tsWeatherChunks terrainSnap
          weatherNormalsChunks = getWeatherNormalsFromStore (tsOverlayStore terrainSnap)
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
          overlayMap = case lvsSkyOverlay selection of
            Just (SkyOverlayPlugin name fieldIdx) ->
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
      mbGeomPairs <- traverseFresh job chunkPairs $ \(k, chunk) -> do
        let geom = case abKey job of
              BaseAtlasKey base _ _ ->
                buildChunkBaseGeometry (abHexRadius job) config base waterLevel climateChunks weatherChunks weatherNormalsChunks vegChunks (IntMap.lookup k overlayMap) k chunk
              OverlayAtlasKey{} ->
                buildChunkSkyOverlayGeometryForSelection (abHexRadius job) config selection waterLevel climateChunks weatherChunks weatherNormalsChunks vegChunks (IntMap.lookup k overlayMap) k chunk
              _ ->
                buildChunkGeometryForSelection (abHexRadius job) config selection waterLevel climateChunks weatherChunks weatherNormalsChunks vegChunks (IntMap.lookup k overlayMap) k chunk
        _ <- evaluate geom
        threadDelay 100  -- 0.1ms, releases capability
        pure (k, geom)
      case mbGeomPairs of
        Nothing -> pure AtlasWorkerBuildStaleDuringGeometry
        Just geomPairs -> do
          -- Build day/night overlay geometry when a brightness function is provided.
          -- This produces an independent overlay (black + alpha) that can be
          -- cached and drawn separately from the base view-mode tiles.
          mbDayNightGeomPairs <- case (atlasKeyIsBase (abKey job), abDayNightSpec job) of
            (_, Nothing) -> pure (Just [])
            (False, Just _) -> pure (Just [])
            (True, Just (_, dnFn)) -> traverseFresh job chunkPairs $ \(k, chunk) -> do
              let geom = buildDayNightGeometry (abHexRadius job) config dnFn k chunk
              _ <- evaluate geom
              threadDelay 100
              pure (k, geom)
          case mbDayNightGeomPairs of
            Nothing -> pure AtlasWorkerBuildStaleDuringGeometry
            Just dayNightGeomPairs -> do
              freshBeforeCompose <- atlasBuildIsCurrent job
              if not freshBeforeCompose
                then pure AtlasWorkerBuildStaleBeforePublish
                else do
                  let geometryMap = IntMap.fromList geomPairs
                      dayNightGeometryMap = IntMap.fromList dayNightGeomPairs
                      -- Build river geometry for visible chunks only (matching the
                      -- padded viewport). Cross-chunk neighbour lookups still use the
                      -- full tsTerrainChunks map for correct boundary rendering.
                      visibleTerrainChunks = IntMap.fromList chunkPairs
                      riverGeoMap = case abKey job of
                        BaseAtlasKey BaseViewBiome _ _ -> IntMap.mapMaybeWithKey
                          (\ cid _chunk -> buildChunkRiverGeometry (scaleRiverWidths (abHexRadius job) defaultRiverRenderConfig) config (abHexRadius job) cid (tsRiverChunks terrainSnap) (tsTerrainChunks terrainSnap))
                          visibleTerrainChunks
                        _ | atlasKeyIsBase (abKey job) && lvsBaseView selection == BaseViewBiome -> IntMap.mapMaybeWithKey
                          (\ cid _chunk -> buildChunkRiverGeometry (scaleRiverWidths (abHexRadius job) defaultRiverRenderConfig) config (abHexRadius job) cid (tsRiverChunks terrainSnap) (tsTerrainChunks terrainSnap))
                          visibleTerrainChunks
                        _ -> IntMap.empty
                      baseTiles = composeTilesFromGeometry geometryMap (abHexRadius job) (abAtlasScale job)
                      tiles = attachRiverOverlay riverGeoMap baseTiles
                      -- Day/night overlay tiles share the same tiling structure but
                      -- have no river overlay.
                      dayNightTiles = case (atlasKeyIsBase (abKey job), abDayNightSpec job) of
                        (True, Just (dnKey, _)) | not (IntMap.null dayNightGeometryMap) ->
                          Just (dnKey, composeTilesFromGeometry dayNightGeometryMap (abHexRadius job) (abAtlasScale job))
                        _ -> Nothing
                      coverage = atlasViewportCoverageFromKeys (map fst chunkPairs)
                      results = atlasBuildResultsForTiles (abBuildId job) (abKey job) (abSnapshotVersion job) (abHexRadius job) (abAtlasScale job) coverage tiles dayNightTiles
                  if null tiles
                    then threadDelay 100 >> pure AtlasWorkerBuildCompleted
                    else do
                      allPublished <- forFresh_ job results $ \result -> do
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
                      freshAfterPublish <- atlasBuildIsCurrent job
                      threadDelay 100
                      pure $ if allPublished && freshAfterPublish
                        then AtlasWorkerBuildCompleted
                        else AtlasWorkerBuildStaleBeforePublish

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
    atlasWorkerLoadBegin (abWorkerLoadRef job)
    outcome <- runAtlasBuild job `onException` atlasWorkerLoadFinishOutcome (abWorkerLoadRef job) AtlasWorkerBuildFailed
    atlasWorkerLoadFinishOutcome (abWorkerLoadRef job) outcome
    pure st
|]

enqueueAtlasBuildWork :: ActorHandle AtlasWorker (Protocol AtlasWorker) -> AtlasBuild -> IO ()
enqueueAtlasBuildWork handle job =
  cast @"build" handle #build job
