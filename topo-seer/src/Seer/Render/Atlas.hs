module Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , CachedAtlasTileSet(..)
  , AtlasResolveStatus(..)
  , AtlasTileSetSummary(..)
  , AtlasCacheSummary(..)
  , AtlasResolveDiagnostic(..)
  , atlasResolveNeedsRetry
  , atlasResolveStatusLabel
  , atlasCacheSummary
  , formatAtlasCacheSummary
  , atlasResolveDiagnostic
  , formatAtlasResolveDiagnostic
  , emptyAtlasTextureCache
  , collectAtlasTextures
  , drawAtlas
  , drawAtlasAlpha
  , drainAtlasBuildResults
  , getNearestAtlas
  , getCompleteAtlas
  , getCurrentCompleteAtlas
  , getCurrentCompleteAtlasForTarget
  , resolveAtlasTiles
  , resolveAtlasPure
  , resolveAtlasPureWithStatus
  , resolveAtlasPureWithFreshness
  , resolveAtlasFallback
  , resolveEffectiveStage
  , scheduleAtlasBuilds
  , setAtlasKey
  , storeAtlasTiles
  , storeAtlasTileSet
  , storeDayNightTiles
  , getNearestDayNight
  , touchAtlasScale
  , drainAtlasPending
  , evictIfNeeded
  , zoomTextureScale
  ) where

import Actor.AtlasCache (AtlasKey, atlasKeyFor, atlasKeyVersion)
import Actor.AtlasFreshness (AtlasFreshness, AtlasFreshnessRef, atlasTargetBuildIsFresh, readAtlasFreshnessRef)
import Actor.AtlasResult
  ( AtlasBuildId(..)
  , AtlasBuildResult(..)
  , AtlasTileSetManifest(..)
  , atlasManifestTarget
  )
import Actor.AtlasResultBroker
  ( AtlasResultDrainStats(..)
  , AtlasResultRef
  , drainAtlasResultsNWithStats
  , drainFreshResultsNWithStats
  )
import Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , AtlasScheduleRef
  , readAtlasScheduleRef
  )
import Actor.AtlasScheduler
  ( AtlasScheduleRequest(..)
  , AtlasScheduler
  , requestAtlasSchedule
  )
import Actor.Data (TerrainSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..))
import Control.Monad (foldM, forM_, unless)
import Data.List (partition)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import Linear (V2(..))
import qualified SDL
import Seer.Render.ZoomStage (ZoomStage(..))
import Seer.Timing (nsToMs, timedMs)
import UI.HexGeometry (transformWorldRect)
import UI.TerrainAtlas (TerrainAtlasTile(..), renderAtlasTileTextures)
import UI.TexturePool (TexturePool, releaseTexture)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Cached render-thread tile set for one @(AtlasKey, hex-radius)@ bucket.
--
-- 'catsComplete' is true only when every normalised tile bound listed in the
-- worker manifest has been received for the same build id.  Partial tile sets
-- may still be drawn provisionally, but only complete exact sets can promote to
-- 'atcLast'.
data CachedAtlasTileSet = CachedAtlasTileSet
  { catsManifest :: !AtlasTileSetManifest
  , catsTiles :: ![TerrainAtlasTile]
  , catsComplete :: !Bool
  }

-- | Explicit atlas resolution state used by the render loop retry policy.
data AtlasResolveStatus
  = CompleteExact
  | PartialExact
  | NearestScaleFallback
  | StaleExactFallback
  | LastGoodFallback
  | Missing
  deriving (Eq, Show)

atlasResolveNeedsRetry :: AtlasResolveStatus -> Bool
atlasResolveNeedsRetry CompleteExact = False
atlasResolveNeedsRetry _ = True

atlasResolveStatusLabel :: AtlasResolveStatus -> String
atlasResolveStatusLabel CompleteExact = "complete-exact"
atlasResolveStatusLabel PartialExact = "partial-exact"
atlasResolveStatusLabel NearestScaleFallback = "nearest-scale-fallback"
atlasResolveStatusLabel StaleExactFallback = "stale-exact-fallback"
atlasResolveStatusLabel LastGoodFallback = "last-good-fallback"
atlasResolveStatusLabel Missing = "missing"

-- | Production cache counters for atlas trace and timing diagnostics.
data AtlasTileSetSummary = AtlasTileSetSummary
  { atssKey :: !AtlasKey
  , atssHexRadius :: !Int
  , atssAtlasScale :: !Int
  , atssBuildId :: !(Maybe AtlasBuildId)
  , atssTileCount :: !Int
  , atssExpectedTileCount :: !Int
  , atssComplete :: !Bool
  } deriving (Eq, Show)

data AtlasCacheSummary = AtlasCacheSummary
  { acsCompleteTileSets :: !Int
  , acsPartialTileSets :: !Int
  , acsPendingTextureReleases :: !Int
  , acsLastGood :: !(Maybe AtlasTileSetSummary)
  , acsTargetKey :: !(Maybe AtlasKey)
  , acsTargetStage :: !(Maybe ZoomStage)
  , acsTargetTileSet :: !(Maybe AtlasTileSetSummary)
  } deriving (Eq, Show)

-- | Per-frame resolve diagnostic built from the resolver's production status.
data AtlasResolveDiagnostic = AtlasResolveDiagnostic
  { ardStatus :: !AtlasResolveStatus
  , ardRetryReason :: !String
  , ardExpectedKey :: !AtlasKey
  , ardTargetStage :: !ZoomStage
  , ardSelectedKey :: !(Maybe AtlasKey)
  , ardSelectedHexRadius :: !(Maybe Int)
  , ardSelectedBuildId :: !(Maybe AtlasBuildId)
  , ardSelectedTileCount :: !Int
  , ardStaleKeyFallback :: !Bool
  , ardPromotionAccepted :: !Bool
  , ardPromotionSuppressed :: !Bool
  } deriving (Eq, Show)

-- | Render-thread-owned cache of atlas textures keyed by (AtlasKey, scale).
--
-- Tiles for different view modes and water levels coexist in a
-- 'Map.Map AtlasKey (IntMap CachedAtlasTileSet)'.
-- Switching the active key ('atcKey') is an O(1) pointer update —
-- no textures are flushed.
--
-- Day\/night overlay tiles are stored separately in 'atcDayNight',
-- keyed only by scale (hex radius).  The overlay is independent of
-- view mode, so switching modes does not invalidate it.
data AtlasTextureCache = AtlasTextureCache
  { atcKey :: !(Maybe AtlasKey)
  , atcCaches :: !(Map.Map AtlasKey (IntMap.IntMap CachedAtlasTileSet))
  , atcMaxEntries :: !Int
  , atcLru :: ![(AtlasKey, Int)]
  , atcPending :: ![SDL.Texture]
  , atcLast :: !(Maybe (AtlasKey, [TerrainAtlasTile]))
  , atcCommittedStage :: !(Maybe ZoomStage)
  , atcStageChangeNs :: !Word64
  , atcDayNight :: !(IntMap.IntMap [TerrainAtlasTile])
    -- ^ Day\/night overlay tiles keyed by scale (hex radius).
    -- View-mode-independent; shared across all base atlas keys.
  }

-- | Create an empty atlas texture cache.
emptyAtlasTextureCache :: Int -> AtlasTextureCache
emptyAtlasTextureCache maxEntries = AtlasTextureCache
  { atcKey = Nothing
  , atcCaches = Map.empty
  , atcMaxEntries = maxEntries
  , atcLru = []
  , atcPending = []
  , atcLast = Nothing
  , atcCommittedStage = Nothing
  , atcStageChangeNs = 0
  , atcDayNight = IntMap.empty
  }

atlasCacheSummary :: Maybe AtlasKey -> Maybe ZoomStage -> AtlasTextureCache -> AtlasCacheSummary
atlasCacheSummary targetKey targetStage cache =
  let sets = [ tileSetSummary key set
             | (key, bucket) <- Map.toList (atcCaches cache)
             , set <- IntMap.elems bucket
             ]
      completeCount = length (filter atssComplete sets)
      partialCount = length sets - completeCount
      targetSet = do
        key <- targetKey
        stage <- targetStage
        tileSetSummary key <$> lookupAtlasSet key (zsHexRadius stage) cache
  in AtlasCacheSummary
    { acsCompleteTileSets = completeCount
    , acsPartialTileSets = partialCount
    , acsPendingTextureReleases = length (atcPending cache)
    , acsLastGood = lastGoodSummary cache
    , acsTargetKey = targetKey
    , acsTargetStage = targetStage
    , acsTargetTileSet = targetSet
    }

formatAtlasCacheSummary :: AtlasCacheSummary -> String
formatAtlasCacheSummary summary =
  "atlasCache completeSets=" <> show (acsCompleteTileSets summary)
    <> " partialSets=" <> show (acsPartialTileSets summary)
    <> " pendingRelease=" <> show (acsPendingTextureReleases summary)
    <> " last=" <> formatMaybeTileSetSummary (acsLastGood summary)
    <> " targetKey=" <> maybe "none" show (acsTargetKey summary)
    <> " targetStage=" <> maybe "none" formatZoomStage (acsTargetStage summary)
    <> " targetSet=" <> formatMaybeTileSetSummary (acsTargetTileSet summary)

atlasResolveDiagnostic
  :: AtlasKey
  -> ZoomStage
  -> AtlasResolveStatus
  -> Maybe [TerrainAtlasTile]
  -> AtlasTextureCache
  -> AtlasTextureCache
  -> AtlasResolveDiagnostic
atlasResolveDiagnostic expectedKey targetStage status atlasToDraw cacheBefore cacheAfter =
  let selectedTiles = maybe [] id atlasToDraw
      selectedHex = case selectedTiles of
        tile:_ -> Just (tatHexRadius tile)
        [] -> Nothing
      selectedKey = case status of
        LastGoodFallback -> fmap fst (atcLast cacheBefore)
        Missing -> Nothing
        _ | null selectedTiles -> Nothing
          | otherwise -> Just expectedKey
      selectedBuild = do
        key <- selectedKey
        hexRadius <- selectedHex
        lookupBuildId key hexRadius cacheBefore `orMaybe` lookupBuildId key hexRadius cacheAfter
      staleKeyFallback = case selectedKey of
        Just key -> status == LastGoodFallback && key /= expectedKey
        Nothing -> False
      promotionAccepted = status == CompleteExact
        && not (null selectedTiles)
        && lastGoodSummary cacheBefore /= lastGoodSummary cacheAfter
      promotionSuppressed = status /= CompleteExact && not (null selectedTiles)
  in AtlasResolveDiagnostic
    { ardStatus = status
    , ardRetryReason = atlasRetryReason status staleKeyFallback
    , ardExpectedKey = expectedKey
    , ardTargetStage = targetStage
    , ardSelectedKey = selectedKey
    , ardSelectedHexRadius = selectedHex
    , ardSelectedBuildId = selectedBuild
    , ardSelectedTileCount = length selectedTiles
    , ardStaleKeyFallback = staleKeyFallback
    , ardPromotionAccepted = promotionAccepted
    , ardPromotionSuppressed = promotionSuppressed
    }

formatAtlasResolveDiagnostic :: AtlasResolveDiagnostic -> String
formatAtlasResolveDiagnostic diag =
  "atlasResolve status=" <> atlasResolveStatusLabel (ardStatus diag)
    <> " retryReason=" <> ardRetryReason diag
    <> " targetKey=" <> show (ardExpectedKey diag)
    <> " targetStage=" <> formatZoomStage (ardTargetStage diag)
    <> " selectedKey=" <> maybe "none" show (ardSelectedKey diag)
    <> " selectedHex=" <> maybe "none" show (ardSelectedHexRadius diag)
    <> " selectedBuild=" <> maybe "none" show (ardSelectedBuildId diag)
    <> " selectedTiles=" <> show (ardSelectedTileCount diag)
    <> " staleKeyFallback=" <> show (ardStaleKeyFallback diag)
    <> " promotion=" <> promotionText diag

-- | Hysteresis threshold: do not switch zoom stage until the camera has
-- been in the new range for at least this many nanoseconds (300 ms).
stageHysteresisNs :: Word64
stageHysteresisNs = 300000000

-- | Resolve the effective zoom stage with hysteresis.
--
-- If the raw stage (from 'stageForZoom') differs from the previously
-- committed stage, the switch is delayed until the camera has remained
-- in the new stage's range for 'stageHysteresisNs'.  Returns the
-- effective stage, an optional transition target @(targetStage, blendFactor)@
-- during the cross-fade window, and the updated cache.
--
-- The blend factor uses smoothstep interpolation: 0 at the start of
-- the transition, 1 when the hysteresis expires and the new stage commits.
resolveEffectiveStage :: Word64 -> ZoomStage -> AtlasTextureCache -> (ZoomStage, Maybe (ZoomStage, Float), AtlasTextureCache)
resolveEffectiveStage nowNs rawStage cache =
  case atcCommittedStage cache of
    Nothing ->
      (rawStage, Nothing, cache { atcCommittedStage = Just rawStage, atcStageChangeNs = 0 })
    Just committed
      | committed == rawStage ->
          (committed, Nothing, cache { atcStageChangeNs = 0 })
      | atcStageChangeNs cache == 0 ->
          (committed, Just (rawStage, 0), cache { atcStageChangeNs = nowNs })
      | nowNs - atcStageChangeNs cache >= stageHysteresisNs ->
          (rawStage, Nothing, cache { atcCommittedStage = Just rawStage, atcStageChangeNs = 0 })
      | otherwise ->
          let elapsed = fromIntegral (nowNs - atcStageChangeNs cache) :: Float
              duration = fromIntegral stageHysteresisNs :: Float
              t = min 1.0 (max 0.0 (elapsed / duration))
              blend = t * t * (3 - 2 * t)  -- smoothstep
          in (committed, Just (rawStage, blend), cache)

-- | Collect all textures currently held by the atlas cache.
collectAtlasTextures :: AtlasTextureCache -> [SDL.Texture]
collectAtlasTextures cache =
  atcPending cache
  ++ concatMap collectTextures (Map.elems (atcCaches cache))
  ++ concatMap (map tatTexture) (IntMap.elems (atcDayNight cache))

-- | Draw atlas tiles to the renderer.
drawAtlas :: SDL.Renderer -> [TerrainAtlasTile] -> (Float, Float) -> Float -> V2 Int -> IO ()
drawAtlas renderer tiles pan zoom winSize = drawAtlasAlpha renderer tiles pan zoom winSize 255

-- | Draw atlas tiles with a global alpha multiplier for cross-fade blending.
--
-- Atlas textures are RGBA render targets cleared to transparent black before
-- terrain geometry is rendered into them.  Always use source-alpha blending —
-- even at full opacity — so transparent margins cannot overwrite the
-- framebuffer.  Seam safety comes from the shared floor/ceiling copy bounds
-- ('transformWorldRect') rather than disabling source alpha.
drawAtlasAlpha :: SDL.Renderer -> [TerrainAtlasTile] -> (Float, Float) -> Float -> V2 Int -> Word8 -> IO ()
drawAtlasAlpha renderer tiles (panX, panY) zoom (V2 winW winH) alpha = do
  forM_ tiles $ \tile -> do
    SDL.textureBlendMode (tatTexture tile) SDL.$= SDL.BlendAlphaBlend
    SDL.textureAlphaMod (tatTexture tile) SDL.$= alpha
  mapM_ drawTile tiles
  -- Cross-fade draws temporarily modulate alpha; leave reusable atlas textures
  -- at full opacity for subsequent steady-state, fallback, or overlay draws.
  unless (alpha == 255) $
    mapM_ (\tile -> SDL.textureAlphaMod (tatTexture tile) SDL.$= 255) tiles
  where
    drawTile tile = do
      let Rect (V2 x y, V2 w h) = tatBounds tile
          Rect (V2 tx ty, V2 tw th) = transformWorldRect (panX, panY) zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else SDL.copy renderer (tatTexture tile) Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))

-- | Drain atlas build results and upload textures.
--
-- Reads from a shared 'AtlasResultRef' (lock-free) instead of a
-- synchronous actor call.  Textures are acquired from the given
-- 'TexturePool'.
--
-- Stale results are discarded before GPU upload.  The manager-published
-- build id in 'AtlasFreshnessRef' is authoritative for same-key viewport
-- supersession; the cache still prevents accepted tiles from different build
-- ids from merging into one complete tile set.
drainAtlasBuildResults
  :: Bool
  -> Int
  -> TexturePool
  -> SDL.Renderer
  -> AtlasTextureCache
  -> AtlasResultRef
  -> AtlasFreshnessRef
  -> IO (AtlasTextureCache, Int, Word32, AtlasResultDrainStats)
drainAtlasBuildResults renderTargetOk perFrame pool renderer atlasCache resultRef freshnessRef =
  if renderTargetOk
    then do
      latestFreshness <- readAtlasFreshnessRef freshnessRef
      let isFresh r =
            let manifest = abrManifest r
                keyMatches = case atcKey atlasCache of
                  Just currentKey -> abrKey r == currentKey
                  Nothing -> True
                resultMatchesManifest = abrKey r == atsmKey manifest
                  && abrSnapshotVersion r == atsmSnapshotVersion manifest
                  && abrHexRadius r == atsmHexRadius manifest
            in keyMatches
              && resultMatchesManifest
              && atlasTargetBuildIsFresh latestFreshness (atlasManifestTarget manifest) (atsmBuildId manifest)
      (results, drainStats) <- drainFreshResultsNWithStats resultRef isFresh perFrame
      (cache', totalMs) <- foldM cacheStep (atlasCache, 0) results
      pure (cache', length results, totalMs, drainStats)
    else do
      (results, drainStats) <- drainAtlasResultsNWithStats resultRef perFrame
      pure (atlasCache, length results, 0, drainStats)
  where
    cacheStep (cache, totalMs) result = do
      start <- getMonotonicTimeNSec
      tiles <- renderAtlasTileTextures pool renderer [abrTile result]
      -- Render day/night overlay tile if present.
      dnTiles <- case abrDayNightTile result of
        Just dnTileGeom -> renderAtlasTileTextures pool renderer [dnTileGeom]
        Nothing         -> pure []
      end <- getMonotonicTimeNSec
      let elapsedMs = nsToMs start end
          cache' = if null tiles
            then cache
            else storeAtlasTileSet (abrManifest result) tiles cache
          -- Store day/night overlay tiles keyed by scale only.
          cache'' = if null dnTiles
            then cache'
            else storeDayNightTiles (abrHexRadius result) dnTiles cache'
      pure (cache'', totalMs + elapsedMs)

-- | Schedule atlas build work when rendering with atlas tiles.
--
-- Sends a scheduling request (cast), then reads the latest report from
-- a shared 'AtlasScheduleRef' (lock-free) instead of a synchronous call.
scheduleAtlasBuilds
  :: Bool
  -> Bool
  -> ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasScheduleRef
  -> SnapshotVersion
  -> RenderSnapshot
  -> (Int, Int)
  -> IO (Int, Word32, Word32)
scheduleAtlasBuilds renderTargetOk dataReady atlasSchedulerHandle scheduleRef snapshotVersion snapshot windowSize = do
  requestAtlasSchedule atlasSchedulerHandle AtlasScheduleRequest
    { asqSnapshotVersion = snapshotVersion
    , asqRenderTargetOk = renderTargetOk
    , asqDataReady = dataReady
    , asqSnapshot = snapshot
    , asqWindowSize = windowSize
    }
  mbReport <- readAtlasScheduleRef scheduleRef
  case mbReport of
    Just report | asrSnapshotVersion report == snapshotVersion ->
      pure (asrJobCount report, asrDrainMs report, asrEnqueueMs report)
    _ -> pure (0, 0, 0)

-- | Resolve which atlas tiles to draw and clean up pending textures.
--
-- Evicted textures are returned to the given 'TexturePool' for reuse.
resolveAtlasTiles
  :: Maybe AtlasFreshness
  -> Bool
  -> TexturePool
  -> RenderSnapshot
  -> AtlasTextureCache
  -> ZoomStage
  -> IO (Maybe [TerrainAtlasTile], AtlasResolveStatus, AtlasTextureCache)
resolveAtlasTiles latestFreshness renderTargetOk pool snapshot atlasCache stage = do
  let terrainSnap = rsTerrain snapshot
      atlasKey = atlasKeyFor (uiViewMode (rsUi snapshot)) (uiRenderWaterLevel (rsUi snapshot)) terrainSnap
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
      (atlasToDraw, status, cacheResolved) =
        resolveAtlasPureForTarget latestFreshness renderTargetOk dataReady atlasKey (zsHexRadius stage) (zsAtlasScale stage) atlasCache
      (pending, cacheDrained) = drainAtlasPending cacheResolved
      -- Protect textures that atcLast still references from being
      -- released.  Only truly orphaned textures may be destroyed.
      aliveTextures = case atcLast cacheDrained of
        Just (_key, tiles) -> map tatTexture tiles
        _                  -> []
      (keepAlive, destroyNow) =
        if null aliveTextures
          then ([], pending)
          else (filter (`elem` aliveTextures) pending,
                filter (`notElem` aliveTextures) pending)
      cacheFinal = cacheDrained { atcPending = keepAlive }
  unless (null destroyNow) $
    mapM_ (releaseTexture pool) destroyNow
  pure (atlasToDraw, status, cacheFinal)

-- | Backwards-compatible pure resolver returning the legacy key-mismatch flag.
resolveAtlasPure
  :: Bool            -- ^ renderTargetOk
  -> Bool            -- ^ dataReady
  -> AtlasKey        -- ^ expected atlas key
  -> Int             -- ^ target hex radius (from ZoomStage)
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], Bool, AtlasTextureCache)
resolveAtlasPure renderTargetOk dataReady atlasKey hexRadius atlasCache =
  let (tiles, status, cache') = resolveAtlasPureWithStatus renderTargetOk dataReady atlasKey hexRadius atlasCache
      keyMismatch = case status of
        LastGoodFallback -> case atcLast atlasCache of
          Just (lastKey, lastTiles) | not (null lastTiles) -> lastKey /= atlasKey
          _ -> False
        _ -> False
  in (tiles, keyMismatch, cache')

-- | Pure core of 'resolveAtlasTiles' with explicit completeness status.
--
-- Only 'CompleteExact' promotes 'atcLast'.  Partial exact, stale exact,
-- nearest-scale, and last-good fallbacks may be drawn provisionally but keep
-- retry active.
resolveAtlasPureWithStatus
  :: Bool            -- ^ renderTargetOk
  -> Bool            -- ^ dataReady
  -> AtlasKey        -- ^ expected atlas key
  -> Int             -- ^ target hex radius (from ZoomStage)
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], AtlasResolveStatus, AtlasTextureCache)
resolveAtlasPureWithStatus renderTargetOk dataReady atlasKey hexRadius atlasCache =
  resolveAtlasPureWithFreshness Nothing renderTargetOk dataReady atlasKey hexRadius atlasCache

resolveAtlasPureWithFreshness
  :: Maybe AtlasFreshness
  -> Bool            -- ^ renderTargetOk
  -> Bool            -- ^ dataReady
  -> AtlasKey        -- ^ expected atlas key
  -> Int             -- ^ target hex radius (from ZoomStage)
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], AtlasResolveStatus, AtlasTextureCache)
resolveAtlasPureWithFreshness latestFreshness renderTargetOk dataReady atlasKey hexRadius atlasCache =
  resolveAtlasPureForTarget latestFreshness renderTargetOk dataReady atlasKey hexRadius 1 atlasCache

resolveAtlasPureForTarget
  :: Maybe AtlasFreshness
  -> Bool
  -> Bool
  -> AtlasKey
  -> Int
  -> Int
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], AtlasResolveStatus, AtlasTextureCache)
resolveAtlasPureForTarget latestFreshness renderTargetOk dataReady atlasKey hexRadius atlasScale atlasCache =
  let (atlasToDraw, status, touchEntry) = resolveAtlasSelection latestFreshness renderTargetOk dataReady atlasKey hexRadius atlasScale atlasCache
      cacheWithLast = case (status, atlasToDraw) of
        (CompleteExact, Just tiles) | not (null tiles) ->
          atlasCache { atcLast = Just (atlasKey, tiles) }
        _ -> atlasCache
      cacheTouched = maybe cacheWithLast (`touchResolvedEntry` cacheWithLast) touchEntry
  in (atlasToDraw, status, cacheTouched)

resolveAtlasSelection
  :: Maybe AtlasFreshness
  -> Bool
  -> Bool
  -> AtlasKey
  -> Int
  -> Int
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], AtlasResolveStatus, Maybe (AtlasKey, Int))
resolveAtlasSelection latestFreshness renderTargetOk dataReady atlasKey hexRadius atlasScale atlasCache =
  let canUseCache = renderTargetOk && dataReady
      rawExactSet = if canUseCache then lookupAtlasSet atlasKey hexRadius atlasCache else Nothing
      exactSet = rawExactSet >>= \set -> if cachedSetIsCurrentFor latestFreshness hexRadius atlasScale set then Just set else Nothing
      staleExactSet = rawExactSet >>= \set -> if cachedSetIsCurrentFor latestFreshness hexRadius atlasScale set then Nothing else Just set
      touchFor key tiles = case tiles of
        t:_ -> Just (key, tatHexRadius t)
        [] -> Nothing
  in case exactSet of
      Just set | catsComplete set && not (null (catsTiles set)) ->
        (Just (catsTiles set), CompleteExact, touchFor atlasKey (catsTiles set))
      _ -> case atcLast atlasCache of
        Just (lastKey, lastTiles) | not (null lastTiles) ->
          (Just lastTiles, LastGoodFallback, touchFor lastKey lastTiles)
        _ -> case exactSet of
          Just set | not (null (catsTiles set)) ->
            (Just (catsTiles set), PartialExact, touchFor atlasKey (catsTiles set))
          _ -> case staleExactSet of
            Just set | not (null (catsTiles set)) ->
              (Just (catsTiles set), StaleExactFallback, touchFor atlasKey (catsTiles set))
            _ -> if canUseCache
              then case getNearestAtlasSet atlasKey hexRadius atlasCache of
                Just set | not (null (catsTiles set)) ->
                  (Just (catsTiles set), NearestScaleFallback, touchFor atlasKey (catsTiles set))
                _ -> (Nothing, Missing, Nothing)
              else (Nothing, Missing, Nothing)

zoomTextureScale :: Float -> Int
zoomTextureScale zoom =
  let target = ceiling (zoom * 2)
  in max 1 (min 6 target)

-- | Pure fallback logic for atlas tile resolution.
--
-- Given the expected 'AtlasKey', the result of looking up exact-match tiles,
-- and the current cache, determines which tiles to draw and whether the
-- result is a key-mismatch fallback (stale 'atcLast' tiles from a different
-- view mode or water level).
--
-- Returns @(tiles to draw, key mismatch flag)@.  The mismatch flag is
-- 'True' only when the rendered tiles come from 'atcLast' and the last
-- key differs from the expected key — signalling that the render loop
-- should stay active until correct-mode tiles are built.
resolveAtlasFallback
  :: AtlasKey
  -> Maybe [TerrainAtlasTile]
  -> AtlasTextureCache
  -> (Maybe [TerrainAtlasTile], Bool)
resolveAtlasFallback expectedKey atlasTiles cache =
  case atlasTiles of
    Just tiles | not (null tiles) -> (Just tiles, False)
    _ -> case atcLast cache of
      Just (lastKey, tiles) | not (null tiles) -> (Just tiles, lastKey /= expectedKey)
      _ -> (Nothing, False)

-- | Select the active atlas key.
--
-- With multi-key caching this is an O(1) pointer update — no textures
-- are flushed.  Tiles for the previous key remain in the cache and can
-- be looked up again if the user switches back.
setAtlasKey :: AtlasKey -> AtlasTextureCache -> AtlasTextureCache
setAtlasKey key cache = cache { atcKey = Just key }

-- | Legacy helper for tests and callers that already have a complete tile list.
-- Production worker results should call 'storeAtlasTileSet' with the manifest.
storeAtlasTiles :: AtlasKey -> Int -> [TerrainAtlasTile] -> AtlasTextureCache -> AtlasTextureCache
storeAtlasTiles key scale tiles =
  storeAtlasTileSet (legacyManifest key scale tiles) tiles

legacyManifest :: AtlasKey -> Int -> [TerrainAtlasTile] -> AtlasTileSetManifest
legacyManifest key scale tiles = AtlasTileSetManifest
  { atsmBuildId = AtlasBuildId 0
  , atsmKey = key
  , atsmSnapshotVersion = SnapshotVersion 0
  , atsmHexRadius = scale
  , atsmAtlasScale = case tiles of
      t:_ -> tatScale t
      [] -> 1
  , atsmExpectedTileCount = length tiles
  , atsmExpectedBounds = map tatBounds tiles
  }

-- | Store freshly-built atlas tiles with their worker manifest.
--
-- Tiles are indexed by ('AtlasKey', target hex radius) in the nested map.
-- Results for any key are accepted as long as the terrain version matches the
-- active key. Results from an outdated terrain generation are discarded to
-- 'atcPending'.  Within a bucket, tiles only accumulate when their build id
-- matches the cached tile set; a newer build id starts a new incomplete set and
-- moves the previous textures to pending release.
storeAtlasTileSet :: AtlasTileSetManifest -> [TerrainAtlasTile] -> AtlasTextureCache -> AtlasTextureCache
storeAtlasTileSet manifest tiles cache =
  let key = atsmKey manifest
      scale = atsmHexRadius manifest
      isStale = case atcKey cache of
        Just currentKey -> atlasKeyVersion key /= atlasKeyVersion currentKey
        Nothing -> False
  in if isStale
    then cache { atcPending = map tatTexture tiles ++ atcPending cache }
    else
      let targetCache = case atcKey cache of
            Nothing -> cache { atcKey = Just key }
            _       -> cache
          bucket = maybe IntMap.empty id (Map.lookup key (atcCaches targetCache))
          (mbMerged, pending) = mergeTiles (IntMap.lookup scale bucket) manifest tiles
      in case mbMerged of
        Nothing -> targetCache { atcPending = pending ++ atcPending targetCache }
        Just merged ->
          let bucket' = IntMap.insert scale merged bucket
              cache' = targetCache
                { atcCaches = Map.insert key bucket' (atcCaches targetCache)
                , atcLru = touch (key, scale) (atcLru targetCache)
                , atcPending = pending ++ atcPending targetCache
                }
          in evictIfNeeded cache'

-- | Store day\/night overlay tiles keyed only by scale (hex radius).
--
-- Day\/night tiles are view-mode-independent: they live in 'atcDayNight'
-- outside the per-key LRU cache.  Old tiles at the same scale are moved
-- to 'atcPending' for texture release.
storeDayNightTiles :: Int -> [TerrainAtlasTile] -> AtlasTextureCache -> AtlasTextureCache
storeDayNightTiles scale tiles cache =
  let old = maybe [] id (IntMap.lookup scale (atcDayNight cache))
      pending = map tatTexture old
  in cache
    { atcDayNight = IntMap.insert scale tiles (atcDayNight cache)
    , atcPending  = pending ++ atcPending cache
    }

-- | Look up the nearest-scale day\/night overlay tiles.
getNearestDayNight :: Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getNearestDayNight target cache = nearestAtlas target (atcDayNight cache)

-- | Look up the nearest-scale tiles for a given key.
--
-- Any key can be looked up, not just the active one.  This legacy helper may
-- return partial tile sets; render retry decisions should use
-- 'resolveAtlasPureWithStatus'.
getNearestAtlas :: AtlasKey -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getNearestAtlas key target cache = catsTiles <$> getNearestAtlasSet key target cache

-- | Look up a complete exact-scale atlas tile set.
getCompleteAtlas :: AtlasKey -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getCompleteAtlas key target cache =
  getCurrentCompleteAtlas Nothing key target cache

getCurrentCompleteAtlas :: Maybe AtlasFreshness -> AtlasKey -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getCurrentCompleteAtlas latestFreshness key target cache =
  getCurrentCompleteAtlasForTarget latestFreshness key target 1 cache

getCurrentCompleteAtlasForTarget :: Maybe AtlasFreshness -> AtlasKey -> Int -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getCurrentCompleteAtlasForTarget latestFreshness key targetHex targetAtlasScale cache = do
  set <- lookupAtlasSet key targetHex cache
  if cachedSetIsCurrentFor latestFreshness targetHex targetAtlasScale set && catsComplete set && not (null (catsTiles set))
    then Just (catsTiles set)
    else Nothing

tileSetSummary :: AtlasKey -> CachedAtlasTileSet -> AtlasTileSetSummary
tileSetSummary key set =
  let manifest = catsManifest set
  in AtlasTileSetSummary
    { atssKey = key
    , atssHexRadius = atsmHexRadius manifest
    , atssAtlasScale = atsmAtlasScale manifest
    , atssBuildId = Just (atsmBuildId manifest)
    , atssTileCount = length (catsTiles set)
    , atssExpectedTileCount = atsmExpectedTileCount manifest
    , atssComplete = catsComplete set
    }

lastGoodSummary :: AtlasTextureCache -> Maybe AtlasTileSetSummary
lastGoodSummary cache = do
  (key, tiles) <- atcLast cache
  case tiles of
    [] -> Nothing
    tile:_ ->
      let hexRadius = tatHexRadius tile
          fromBucket = do
            set <- lookupAtlasSet key hexRadius cache
            if sameTileTextures tiles (catsTiles set)
              then Just (tileSetSummary key set)
              else Nothing
          fallback = Just AtlasTileSetSummary
            { atssKey = key
            , atssHexRadius = hexRadius
            , atssAtlasScale = tatScale tile
            , atssBuildId = Nothing
            , atssTileCount = length tiles
            , atssExpectedTileCount = length tiles
            , atssComplete = True
            }
      in fromBucket `orMaybe` fallback

sameTileTextures :: [TerrainAtlasTile] -> [TerrainAtlasTile] -> Bool
sameTileTextures xs ys =
  length xs == length ys
    && all (\tile -> any (sameTextureAndBounds tile) ys) xs
  where
    sameTextureAndBounds a b = tatTexture a == tatTexture b && tatBounds a == tatBounds b

lookupBuildId :: AtlasKey -> Int -> AtlasTextureCache -> Maybe AtlasBuildId
lookupBuildId key hexRadius cache =
  atsmBuildId . catsManifest <$> lookupAtlasSet key hexRadius cache

orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe (Just x) _ = Just x
orMaybe Nothing y = y

formatMaybeTileSetSummary :: Maybe AtlasTileSetSummary -> String
formatMaybeTileSetSummary Nothing = "none"
formatMaybeTileSetSummary (Just summary) =
  "key=" <> show (atssKey summary)
    <> "/hex=" <> show (atssHexRadius summary)
    <> "/scale=" <> show (atssAtlasScale summary)
    <> "/build=" <> maybe "unknown" show (atssBuildId summary)
    <> "/tiles=" <> show (atssTileCount summary) <> "/" <> show (atssExpectedTileCount summary)
    <> "/complete=" <> show (atssComplete summary)

formatZoomStage :: ZoomStage -> String
formatZoomStage stage =
  "hex=" <> show (zsHexRadius stage) <> "/scale=" <> show (zsAtlasScale stage)

atlasRetryReason :: AtlasResolveStatus -> Bool -> String
atlasRetryReason CompleteExact _ = "ready"
atlasRetryReason PartialExact _ = "partial-current-atlas"
atlasRetryReason NearestScaleFallback _ = "nearest-scale-fallback"
atlasRetryReason StaleExactFallback _ = "obsolete-build-or-stale-exact"
atlasRetryReason LastGoodFallback True = "stale-key-last-good-fallback"
atlasRetryReason LastGoodFallback False = "last-good-while-current-incomplete"
atlasRetryReason Missing _ = "missing-current-atlas"

promotionText :: AtlasResolveDiagnostic -> String
promotionText diag
  | ardPromotionAccepted diag = "accepted"
  | ardPromotionSuppressed diag = "suppressed"
  | otherwise = "none"

cachedSetIsCurrentFor :: Maybe AtlasFreshness -> Int -> Int -> CachedAtlasTileSet -> Bool
cachedSetIsCurrentFor latestFreshness targetHex targetAtlasScale set =
  let manifest = catsManifest set
  in atsmHexRadius manifest == targetHex
    && atsmAtlasScale manifest == targetAtlasScale
    && atlasTargetBuildIsFresh latestFreshness (atlasManifestTarget manifest) (atsmBuildId manifest)

lookupAtlasSet :: AtlasKey -> Int -> AtlasTextureCache -> Maybe CachedAtlasTileSet
lookupAtlasSet key target cache = do
  bucket <- Map.lookup key (atcCaches cache)
  IntMap.lookup target bucket

getNearestAtlasSet :: AtlasKey -> Int -> AtlasTextureCache -> Maybe CachedAtlasTileSet
getNearestAtlasSet key target cache = do
  bucket <- Map.lookup key (atcCaches cache)
  nearestAtlasSet target bucket

-- | Touch an atlas entry in the LRU, moving it to the front.
--
-- The active key is used.  Call sites that know the exact key should
-- use the key directly by updating 'atcLru'.
touchAtlasScale :: Int -> AtlasTextureCache -> AtlasTextureCache
touchAtlasScale scale cache =
  case atcKey cache of
    Just key -> cache { atcLru = touch (key, scale) (atcLru cache) }
    Nothing  -> cache

touchResolvedEntry :: (AtlasKey, Int) -> AtlasTextureCache -> AtlasTextureCache
touchResolvedEntry (key, scale) cache =
  case Map.lookup key (atcCaches cache) >>= IntMap.lookup scale of
    Just _ -> cache { atcLru = touch (key, scale) (atcLru cache) }
    Nothing -> cache

drainAtlasPending :: AtlasTextureCache -> ([SDL.Texture], AtlasTextureCache)
drainAtlasPending cache =
  (atcPending cache, cache { atcPending = [] })

nearestAtlas :: Int -> IntMap.IntMap [TerrainAtlasTile] -> Maybe [TerrainAtlasTile]
nearestAtlas _ caches | IntMap.null caches = Nothing
nearestAtlas target caches =
  let best = IntMap.foldlWithKey' (pickBestTiles target) Nothing caches
  in fmap snd best

pickBestTiles :: Int -> Maybe (Int, [TerrainAtlasTile]) -> Int -> [TerrainAtlasTile] -> Maybe (Int, [TerrainAtlasTile])
pickBestTiles _ current _ [] = current
pickBestTiles target current scale atlas =
  case current of
    Nothing -> Just (scale, atlas)
    Just (bestScale, _)
      | abs (scale - target) < abs (bestScale - target) -> Just (scale, atlas)
      | otherwise -> current

nearestAtlasSet :: Int -> IntMap.IntMap CachedAtlasTileSet -> Maybe CachedAtlasTileSet
nearestAtlasSet _ caches | IntMap.null caches = Nothing
nearestAtlasSet target caches =
  let best = IntMap.foldlWithKey' (pickBestSet target) Nothing caches
  in fmap snd best

pickBestSet :: Int -> Maybe (Int, CachedAtlasTileSet) -> Int -> CachedAtlasTileSet -> Maybe (Int, CachedAtlasTileSet)
pickBestSet _ current _ set | null (catsTiles set) = current
pickBestSet target current scale set =
  case current of
    Nothing -> Just (scale, set)
    Just (bestScale, _)
      | abs (scale - target) < abs (bestScale - target) -> Just (scale, set)
      | otherwise -> current

touch :: Eq a => a -> [a] -> [a]
touch entry lru = entry : filter (/= entry) lru

evictIfNeeded :: AtlasTextureCache -> AtlasTextureCache
evictIfNeeded cache
  | length (atcLru cache) <= atcMaxEntries cache = cache
  | otherwise =
      let (toKeep, toEvict) = splitAt (atcMaxEntries cache) (atcLru cache)
          (caches', pending) = foldl evictOne (atcCaches cache, []) toEvict
      in cache
        { atcCaches = caches'
        , atcLru = toKeep
        , atcPending = pending ++ atcPending cache
        }
  where
    evictOne (caches, pending) (key, scale) =
      case Map.lookup key caches of
        Nothing -> (caches, pending)
        Just bucket ->
          let tiles = maybe [] catsTiles (IntMap.lookup scale bucket)
              bucket' = IntMap.delete scale bucket
              caches' = if IntMap.null bucket'
                then Map.delete key caches
                else Map.insert key bucket' caches
          in (caches', map tatTexture tiles ++ pending)

collectTextures :: IntMap.IntMap CachedAtlasTileSet -> [SDL.Texture]
collectTextures caches =
  concatMap (map tatTexture . catsTiles) (IntMap.elems caches)

mergeTiles :: Maybe CachedAtlasTileSet -> AtlasTileSetManifest -> [TerrainAtlasTile] -> (Maybe CachedAtlasTileSet, [SDL.Texture])
mergeTiles existing manifest newTiles =
  let expectedBounds = atsmExpectedBounds manifest
      (acceptedNew, rejectedNew) = partition (\tile -> tatBounds tile `elem` expectedBounds) newTiles
      rejectedTextures = map tatTexture rejectedNew
      mkSet mergedTiles = CachedAtlasTileSet
        { catsManifest = manifest
        , catsTiles = mergedTiles
        , catsComplete = tileSetComplete manifest mergedTiles
        }
  in case existing of
    Nothing
      | null acceptedNew -> (Nothing, rejectedTextures)
      | otherwise -> (Just (mkSet acceptedNew), rejectedTextures)
    Just old
      | atsmBuildId (catsManifest old) == atsmBuildId manifest ->
          let newBounds = map tatBounds acceptedNew
              (replaced, kept) = partitionByBounds newBounds (catsTiles old)
              mergedTiles = acceptedNew ++ kept
              pending = map tatTexture replaced ++ rejectedTextures
          in (Just (mkSet mergedTiles), pending)
      | null acceptedNew ->
          (Just old, rejectedTextures)
      | otherwise ->
          let pendingOld = map tatTexture (catsTiles old)
          in (Just (mkSet acceptedNew), pendingOld ++ rejectedTextures)

tileSetComplete :: AtlasTileSetManifest -> [TerrainAtlasTile] -> Bool
tileSetComplete manifest tiles =
  let expected = uniqueRects (atsmExpectedBounds manifest)
      received = uniqueRects (map tatBounds tiles)
      expectedCount = atsmExpectedTileCount manifest
  in expectedCount > 0
    && length expected == expectedCount
    && length received == expectedCount
    && all (`elem` received) expected

uniqueRects :: [Rect] -> [Rect]
uniqueRects = foldr (\rect acc -> if rect `elem` acc then acc else rect : acc) []

partitionByBounds :: [Rect] -> [TerrainAtlasTile] -> ([TerrainAtlasTile], [TerrainAtlasTile])
partitionByBounds bounds tiles =
  foldr (splitOne bounds) ([], []) tiles
  where
    splitOne bounds' tile (replaced, kept)
      | tatBounds tile `elem` bounds' = (tile : replaced, kept)
      | otherwise = (replaced, tile : kept)
