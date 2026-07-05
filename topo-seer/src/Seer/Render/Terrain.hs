module Seer.Render.Terrain
  ( TerrainCache(..)
  , emptyTerrainCache
  , updateTerrainCache
  , buildTerrainCache
  , updateChunkTextures
  , terrainCacheNeedsRefresh
  , chunkTextureCacheNeedsUpdate
  , fallbackTerrainNeedsRefresh
  , drawTerrain
  ) where

import Actor.AtlasCache (terrainSnapshotViewVersion)
import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8, Word64)
import Linear (V2(..), V4(..))
import qualified Data.Vector.Storable as SV
import qualified SDL
import qualified SDL.Raw.Types as Raw
import UI.OverlayExtract (extractOverlayField)
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.HexGeometry (renderHexRadiusPx, transformWorldPoint, transformWorldRect)
import UI.TerrainRender (ChunkGeometry(..), ChunkTexture(..), buildChunkGeometry, buildChunkTexture, destroyChunkTexture)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)
import Topo (ClimateChunk(..), TerrainChunk(..), WeatherChunk(..), WorldConfig(..))

-- | Cached per-chunk geometry for the current renderable terrain state.
--
-- @tcVersion@ mirrors the view-specific version from the 'TerrainSnapshot'
-- that was used to build this cache.  Staleness is checked via the version
-- rather than deep IntMap equality.
data TerrainCache = TerrainCache
  { tcVersion :: !Word64
  , tcViewMode :: !ViewMode
  , tcWaterLevel :: !Float
  , tcDayNightEnabled :: !Bool
  , tcChunkSize :: !Int
  , tcTerrainChunks :: !(IntMap TerrainChunk)
  , tcClimateChunks :: !(IntMap ClimateChunk)
  , tcWeatherChunks :: !(IntMap WeatherChunk)
  , tcGeometry :: !(IntMap ChunkGeometry)
  }

-- | Empty terrain cache used before any terrain data is available.
emptyTerrainCache :: TerrainCache
emptyTerrainCache = TerrainCache
  { tcVersion = 0
  , tcViewMode = ViewElevation
  , tcWaterLevel = 0
  , tcDayNightEnabled = True
  , tcChunkSize = 0
  , tcTerrainChunks = IntMap.empty
  , tcClimateChunks = IntMap.empty
  , tcWeatherChunks = IntMap.empty
  , tcGeometry = IntMap.empty
  }

-- | Update the cached geometry when the UI or terrain snapshot changes.
--
-- Uses a view-specific version for O(1) data staleness instead of comparing
-- the full IntMaps element-by-element.
updateTerrainCache :: UiState -> TerrainSnapshot -> TerrainCache -> TerrainCache
updateTerrainCache uiSnap terrainSnap cache
  | tsChunkSize terrainSnap <= 0 = emptyTerrainCache
  | tcViewMode cache /= uiViewMode uiSnap = buildTerrainCache uiSnap terrainSnap
  | tcWaterLevel cache /= uiRenderWaterLevel uiSnap = buildTerrainCache uiSnap terrainSnap
  | tcDayNightEnabled cache /= uiDayNightEnabled uiSnap = buildTerrainCache uiSnap terrainSnap
  | tcChunkSize cache /= tsChunkSize terrainSnap = buildTerrainCache uiSnap terrainSnap
  | tcVersion cache /= terrainSnapshotViewVersion (uiViewMode uiSnap) terrainSnap = buildTerrainCache uiSnap terrainSnap
  | otherwise = cache

-- | Whether the fallback terrain cache is stale for the renderable snapshot.
-- Empty terrain snapshots are considered fresh only when no previous cache
-- remains, preventing stale fallback geometry from surviving data resets.
terrainCacheNeedsRefresh :: UiState -> TerrainSnapshot -> TerrainCache -> Bool
terrainCacheNeedsRefresh uiSnap terrainSnap cache
  | tsChunkSize terrainSnap <= 0 || IntMap.null (tsTerrainChunks terrainSnap) =
      tcChunkSize cache /= 0
        || not (IntMap.null (tcTerrainChunks cache))
        || not (IntMap.null (tcClimateChunks cache))
        || not (IntMap.null (tcWeatherChunks cache))
        || not (IntMap.null (tcGeometry cache))
  | tcViewMode cache /= uiViewMode uiSnap = True
  | tcWaterLevel cache /= uiRenderWaterLevel uiSnap = True
  | tcChunkSize cache /= tsChunkSize terrainSnap = True
  | tcVersion cache /= terrainSnapshotViewVersion (uiViewMode uiSnap) terrainSnap = True
  | otherwise = False

-- | Build a fresh terrain cache for the current UI and terrain state.
buildTerrainCache :: UiState -> TerrainSnapshot -> TerrainCache
buildTerrainCache uiSnap terrainSnap =
  let config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
      mode = uiViewMode uiSnap
      waterLevel = uiRenderWaterLevel uiSnap
      viewVersion = terrainSnapshotViewVersion mode terrainSnap
      overlayMap = case mode of
        ViewOverlay name fieldIdx ->
          case extractOverlayField name fieldIdx (wcChunkSize config * wcChunkSize config) (tsOverlayStore terrainSnap) of
            Just m  -> m
            Nothing -> IntMap.empty
        _ -> IntMap.empty
      mkGeom k chunk = buildChunkGeometry renderHexRadiusPx config mode waterLevel
                         (tsClimateChunks terrainSnap)
                         (tsWeatherChunks terrainSnap)
                         (tsVegetationChunks terrainSnap)
                         (IntMap.lookup k overlayMap)
                         k chunk
      cacheChunks = IntMap.mapWithKey mkGeom (tsTerrainChunks terrainSnap)
  in TerrainCache
      { tcVersion = viewVersion
      , tcViewMode = mode
      , tcWaterLevel = waterLevel
      , tcDayNightEnabled = uiDayNightEnabled uiSnap
      , tcChunkSize = tsChunkSize terrainSnap
      , tcTerrainChunks = tsTerrainChunks terrainSnap
      , tcClimateChunks = tsClimateChunks terrainSnap
      , tcWeatherChunks = tsWeatherChunks terrainSnap
      , tcGeometry = cacheChunks
      }

-- | Draw terrain either from cached textures or immediate geometry.
drawTerrain :: SDL.Renderer -> TerrainSnapshot -> TerrainCache -> ChunkTextureCache -> (Float, Float) -> Float -> V2 Int -> IO ()
drawTerrain renderer terrainSnap cache textureCache (panX, panY) zoom (V2 winW winH) =
  if tcChunkSize cache <= 0 || IntMap.null (tcGeometry cache)
    then pure ()
    else do
      let textures = ctcTextures textureCache
      if IntMap.null textures
        then mapM_ (drawChunkGeometry renderer winW winH) (IntMap.elems (tcGeometry cache))
        else mapM_ (drawChunkTexture renderer winW winH) (IntMap.elems textures)
  where
    drawChunkTexture renderer winW winH chunkTexture = do
      let Rect (V2 x y, V2 w h) = ctBounds chunkTexture
          Rect (V2 tx ty, V2 tw th) = transformWorldRect (panX, panY) zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else SDL.copy renderer (ctTexture chunkTexture) Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))

    drawChunkGeometry renderer winW winH geometry = do
      let Rect (V2 x y, V2 w h) = cgBounds geometry
          Rect (V2 tx ty, V2 tw th) = transformWorldRect (panX, panY) zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else do
          let verts = SV.map (transformVertex (fromIntegral x) (fromIntegral y) panX panY zoom) (cgVertices geometry)
          SDL.renderGeometry renderer Nothing verts (cgIndices geometry)

    transformVertex bx by px py z (Raw.Vertex (Raw.FPoint x y) color tex) =
      let (x', y') = transformWorldPoint (px, py) z (bx + realToFrac x, by + realToFrac y)
      in Raw.Vertex (Raw.FPoint (realToFrac x') (realToFrac y')) color tex

-- | Whether cached fallback chunk textures are stale for a terrain cache and
-- atlas scale.  This mirrors 'updateChunkTextures' without touching SDL.
chunkTextureCacheNeedsUpdate :: TerrainCache -> Int -> ChunkTextureCache -> Bool
chunkTextureCacheNeedsUpdate cache scale textureCache
  | IntMap.null (tcGeometry cache) =
      not (IntMap.null (ctcTextures textureCache))
        || ctcChunkSize textureCache /= 0
        || not (IntMap.null (ctcTerrainChunks textureCache))
        || not (IntMap.null (ctcClimateChunks textureCache))
        || not (IntMap.null (ctcWeatherChunks textureCache))
  | chunkTextureMetadataMismatch cache scale textureCache = True
  | currentKeys /= geomKeys = True
  | otherwise = False
  where
    geomKeys = IntMap.keysSet (tcGeometry cache)
    currentKeys = IntMap.keysSet (ctcTextures textureCache)

-- | Whether a non-render-target fallback frame still needs terrain or chunk
-- texture work before it can be considered complete for this snapshot.
fallbackTerrainNeedsRefresh :: UiState -> TerrainSnapshot -> Int -> TerrainCache -> ChunkTextureCache -> Bool
fallbackTerrainNeedsRefresh uiSnap terrainSnap scale cache textureCache =
  terrainCacheNeedsRefresh uiSnap terrainSnap cache
    || chunkTextureCacheNeedsUpdate cache scale textureCache

chunkTextureMetadataMismatch :: TerrainCache -> Int -> ChunkTextureCache -> Bool
chunkTextureMetadataMismatch cache scale textureCache =
  ctcViewMode textureCache /= tcViewMode cache
    || ctcWaterLevel textureCache /= tcWaterLevel cache
    || ctcChunkSize textureCache /= tcChunkSize cache
    || ctcScale textureCache /= scale
    || ctcTerrainChunks textureCache /= tcTerrainChunks cache
    || ctcClimateChunks textureCache /= tcClimateChunks cache
    || ctcWeatherChunks textureCache /= tcWeatherChunks cache

-- | Update cached chunk textures for the current atlas scale.
updateChunkTextures :: SDL.Renderer -> TerrainCache -> Int -> ChunkTextureCache -> IO ChunkTextureCache
updateChunkTextures renderer cache scale textureCache = do
  let resetCache = do
        mapM_ destroyChunkTexture (IntMap.elems (ctcTextures textureCache))
        pure ChunkTextureCache
          { ctcViewMode = tcViewMode cache
          , ctcWaterLevel = tcWaterLevel cache
          , ctcChunkSize = tcChunkSize cache
          , ctcScale = scale
          , ctcTerrainChunks = tcTerrainChunks cache
          , ctcClimateChunks = tcClimateChunks cache
          , ctcWeatherChunks = tcWeatherChunks cache
          , ctcTextures = IntMap.empty
          }
  baseCache <-
    if chunkTextureMetadataMismatch cache scale textureCache
      then resetCache
      else pure textureCache
  let geomKeys = IntMap.keysSet (tcGeometry cache)
      currentKeys = IntMap.keysSet (ctcTextures baseCache)
      removedMap = IntMap.withoutKeys (ctcTextures baseCache) geomKeys
  mapM_ destroyChunkTexture (IntMap.elems removedMap)
  let remaining = IntMap.withoutKeys (ctcTextures baseCache) (IntMap.keysSet removedMap)
      missing = IntMap.withoutKeys (tcGeometry cache) currentKeys
  newTextures <- traverse (buildChunkTexture renderer scale) missing
  let updated = remaining <> newTextures
  pure baseCache
    { ctcTextures = updated
    , ctcTerrainChunks = tcTerrainChunks cache
    , ctcClimateChunks = tcClimateChunks cache
    , ctcWeatherChunks = tcWeatherChunks cache
    }
