module Seer.Render.Terrain
  ( TerrainCache(..)
  , emptyTerrainCache
  , updateTerrainCache
  , buildTerrainCache
  , updateChunkTextures
  , drawTerrain
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word8, Word64)
import Linear (V2(..), V4(..))
import qualified Data.Vector.Storable as SV
import qualified SDL
import qualified SDL.Raw.Types as Raw
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.TerrainRender (ChunkGeometry(..), ChunkTexture(..), buildChunkGeometry, buildChunkTexture, destroyChunkTexture)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)
import Topo (ClimateChunk(..), TerrainChunk(..), WeatherChunk(..), WorldConfig(..))

-- | Cached per-chunk geometry for the current renderable terrain state.
--
-- @tcVersion@ mirrors 'tsVersion' from the 'TerrainSnapshot' that was used to
-- build this cache.  Staleness is checked via the version rather than deep
-- IntMap equality.
data TerrainCache = TerrainCache
  { tcVersion :: !Word64
  , tcViewMode :: !ViewMode
  , tcWaterLevel :: !Float
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
  , tcChunkSize = 0
  , tcTerrainChunks = IntMap.empty
  , tcClimateChunks = IntMap.empty
  , tcWeatherChunks = IntMap.empty
  , tcGeometry = IntMap.empty
  }

-- | Update the cached geometry when the UI or terrain snapshot changes.
--
-- Uses 'tsVersion' for O(1) terrain data staleness instead of comparing the
-- full IntMaps element-by-element.
updateTerrainCache :: UiState -> TerrainSnapshot -> TerrainCache -> TerrainCache
updateTerrainCache uiSnap terrainSnap cache
  | tsChunkSize terrainSnap <= 0 = emptyTerrainCache
  | tcViewMode cache /= uiViewMode uiSnap = buildTerrainCache uiSnap terrainSnap
  | tcWaterLevel cache /= uiWaterLevel uiSnap = buildTerrainCache uiSnap terrainSnap
  | tcChunkSize cache /= tsChunkSize terrainSnap = buildTerrainCache uiSnap terrainSnap
  | tcVersion cache /= tsVersion terrainSnap = buildTerrainCache uiSnap terrainSnap
  | otherwise = cache

-- | Build a fresh terrain cache for the current UI and terrain state.
buildTerrainCache :: UiState -> TerrainSnapshot -> TerrainCache
buildTerrainCache uiSnap terrainSnap =
  let config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
      mode = uiViewMode uiSnap
      waterLevel = uiWaterLevel uiSnap
      cacheChunks = IntMap.mapWithKey (buildChunkGeometry config mode waterLevel (tsClimateChunks terrainSnap) (tsWeatherChunks terrainSnap)) (tsTerrainChunks terrainSnap)
  in TerrainCache
      { tcVersion = tsVersion terrainSnap
      , tcViewMode = mode
      , tcWaterLevel = waterLevel
      , tcChunkSize = tsChunkSize terrainSnap
      , tcTerrainChunks = tsTerrainChunks terrainSnap
      , tcClimateChunks = tsClimateChunks terrainSnap
      , tcWeatherChunks = tsWeatherChunks terrainSnap
      , tcGeometry = cacheChunks
      }

-- | Draw terrain either from cached textures or immediate geometry.
drawTerrain :: SDL.Renderer -> TerrainSnapshot -> TerrainCache -> ChunkTextureCache -> (Float, Float) -> Float -> V2 Int -> IO ()
drawTerrain renderer terrainSnap cache textureCache (panX, panY) zoom (V2 winW winH) =
  if tsChunkSize terrainSnap <= 0
    then pure ()
    else do
      let textures = ctcTextures textureCache
      if IntMap.null textures
        then mapM_ (drawChunkGeometry renderer winW winH) (IntMap.elems (tcGeometry cache))
        else mapM_ (drawChunkTexture renderer winW winH) (IntMap.elems textures)
  where
    drawChunkTexture renderer winW winH chunkTexture = do
      let Rect (V2 x y, V2 w h) = ctBounds chunkTexture
          Rect (V2 tx ty, V2 tw th) = transformRect panX panY zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else SDL.copy renderer (ctTexture chunkTexture) Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))

    drawChunkGeometry renderer winW winH geometry = do
      let Rect (V2 x y, V2 w h) = cgBounds geometry
          Rect (V2 tx ty, V2 tw th) = transformRect panX panY zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else do
          let verts = SV.map (transformVertex panX panY zoom) (cgVertices geometry)
          SDL.renderGeometry renderer Nothing verts (cgIndices geometry)

    transformRect px py z (Rect (V2 x y, V2 w h)) =
      let fx = (fromIntegral x + px) * z
          fy = (fromIntegral y + py) * z
          fw = fromIntegral w * z
          fh = fromIntegral h * z
      in Rect (V2 (round fx) (round fy), V2 (max 1 (round fw)) (max 1 (round fh)))

    transformVertex px py z (Raw.Vertex (Raw.FPoint x y) color tex) =
      let x' = (realToFrac x + realToFrac px) * realToFrac z
          y' = (realToFrac y + realToFrac py) * realToFrac z
      in Raw.Vertex (Raw.FPoint x' y') color tex

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
    if ctcViewMode textureCache /= tcViewMode cache
        || ctcWaterLevel textureCache /= tcWaterLevel cache
        || ctcChunkSize textureCache /= tcChunkSize cache
        || ctcScale textureCache /= scale
        || ctcTerrainChunks textureCache /= tcTerrainChunks cache
        || ctcClimateChunks textureCache /= tcClimateChunks cache
        || ctcWeatherChunks textureCache /= tcWeatherChunks cache
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
