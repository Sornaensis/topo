module UI.TerrainAtlas
  ( TerrainAtlasTile(..)
  , AtlasChunkGeometry(..)
  , AtlasTileGeometry(..)
  , buildAtlasTileGeometry
  , composeTilesFromGeometry
  , attachRiverOverlay
  , mergeChunkGeometry
  , renderAtlasTileTextures
  , maxAtlasTextureSize
  ) where

import UI.TexturePool (TexturePool, acquireTexture)

import Actor.UI (ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed (Vector)
import Foreign.C.Types (CFloat, CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Topo (ClimateChunk(..), TerrainChunk(..), VegetationChunk(..), WeatherChunk(..), WorldConfig(..))
import Topo.Overlay (OverlayStore)
import UI.OverlayExtract (extractOverlayField)
import UI.RiverRender (RiverGeometry(..))
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry)
import UI.HexPick (renderHexRadiusPx, hexOriginX, hexOriginY)
import UI.Widgets (Rect(..))


-- | Render-target texture tile for an atlas slice.
data TerrainAtlasTile = TerrainAtlasTile
  { tatTexture   :: SDL.Texture
  , tatBounds    :: Rect
  , tatScale     :: Int
  , tatHexRadius :: Int
  }

-- | Pre-transformed chunk geometry ready for per-tile atlas rendering.
data AtlasChunkGeometry = AtlasChunkGeometry
  { acgVertices :: !(Vector Raw.Vertex)
  , acgIndices :: !(Vector CInt)
  }

-- | CPU-built atlas tile geometry with per-tile vertex transforms applied.
data AtlasTileGeometry = AtlasTileGeometry
  { atgBounds      :: !Rect
  , atgScale       :: !Int
  , atgHexRadius   :: !Int
  , atgChunks      :: ![AtlasChunkGeometry]
  , atgRiverOverlay :: ![AtlasChunkGeometry]
  }

maxAtlasTextureSize :: Int
maxAtlasTextureSize = 4096

-- | Build per-tile geometry on the CPU (no SDL renderer required).
buildAtlasTileGeometry
  :: ViewMode
  -> Float
  -> IntMap TerrainChunk
  -> IntMap ClimateChunk
  -> IntMap WeatherChunk
  -> IntMap VegetationChunk
  -> OverlayStore
  -> WorldConfig
  -> Int
  -> Int
  -> [AtlasTileGeometry]
buildAtlasTileGeometry mode waterLevel terrainChunks climateChunks weatherChunks vegChunks overlayStore config hexRadiusPx atlasScale =
  let overlayMap = case mode of
        ViewOverlay name fieldIdx ->
          case extractOverlayField name fieldIdx (wcChunkSize config * wcChunkSize config) overlayStore of
            Just m  -> m
            Nothing -> IntMap.empty
        _ -> IntMap.empty
      geometryMap = IntMap.mapWithKey (\k -> buildChunkGeometry hexRadiusPx config mode waterLevel climateChunks weatherChunks vegChunks (IntMap.lookup k overlayMap) Nothing k) terrainChunks
  in composeTilesFromGeometry geometryMap hexRadiusPx atlasScale

-- | Compose atlas tiles from a pre-built chunk geometry map.
--
-- Separated from 'buildAtlasTileGeometry' so callers can build per-chunk
-- geometry in IO (with explicit yields between chunks) and then compose
-- tiles from the pre-built map.  This prevents render thread starvation
-- caused by long-running pure computations whose pinned storable-vector
-- allocations bypass GHC's context-switch counter.
composeTilesFromGeometry :: IntMap ChunkGeometry -> Int -> Int -> [AtlasTileGeometry]
composeTilesFromGeometry geometryMap hexRadius atlasScale
  | IntMap.null geometryMap = []
  | otherwise =
    let boundsList = map cgBounds (IntMap.elems geometryMap)
        Rect (V2 minX minY, V2 maxW maxH) = foldBounds boundsList
        maxScaleW = maxAtlasTextureSize `div` max 1 maxW
        maxScaleH = maxAtlasTextureSize `div` max 1 maxH
        scale' = min atlasScale (max 1 (min maxScaleW maxScaleH))
        tileW = max 1 (maxAtlasTextureSize `div` scale')
        tileH = max 1 (maxAtlasTextureSize `div` scale')
        tiles = tileRects (Rect (V2 minX minY, V2 maxW maxH)) tileW tileH
    in mapMaybe (buildTile geometryMap scale') tiles
  where
    buildTile geoMap sc (Rect (V2 tx ty, V2 tw th)) =
      let tileRect = Rect (V2 tx ty, V2 tw th)
          overlaps = filter (rectsOverlap tileRect . cgBounds) (IntMap.elems geoMap)
      in if null overlaps
        then Nothing
        else
          let chunks = map (buildChunk tx ty sc) overlaps
          in Just AtlasTileGeometry
              { atgBounds      = tileRect
              , atgScale       = sc
              , atgHexRadius   = hexRadius
              , atgChunks      = chunks
              , atgRiverOverlay = []
              }

    buildChunk minX minY sc geom =
      let Rect (V2 bx by, V2 _bw _bh) = cgBounds geom
          offsetX = bx - minX
          offsetY = by - minY
          scaleF = realToFrac sc
          verts = SV.map (offsetVertex offsetX offsetY scaleF) (cgVertices geom)
      in AtlasChunkGeometry
          { acgVertices = verts
          , acgIndices = cgIndices geom
          }

-- | Transform a vertex by an offset (in world pixels) and a uniform scale.
offsetVertex :: Int -> Int -> CFloat -> Raw.Vertex -> Raw.Vertex
offsetVertex dx dy scaleF (Raw.Vertex (Raw.FPoint x y) color tex) =
  let x' = (x + realToFrac dx) * scaleF
      y' = (y + realToFrac dy) * scaleF
  in Raw.Vertex (Raw.FPoint x' y') color tex

-- | Attach river overlay geometry to pre-built atlas tiles.
--
-- For each tile, finds overlapping river geometries and applies the
-- same offset\/scale transform that the terrain chunks use.
attachRiverOverlay :: IntMap RiverGeometry -> [AtlasTileGeometry] -> [AtlasTileGeometry]
attachRiverOverlay riverGeoMap tiles
  | IntMap.null riverGeoMap = tiles
  | otherwise = map attachToTile tiles
  where
    riverGeos = IntMap.elems riverGeoMap

    attachToTile tile =
      let overlapping = filter (rectsOverlap (atgBounds tile) . rgBounds) riverGeos
      in if null overlapping
        then tile
        else
          let Rect (V2 minX minY, _) = atgBounds tile
              sc = realToFrac (atgScale tile) :: CFloat
              chunks = map (buildRiverChunk minX minY sc) overlapping
          in tile { atgRiverOverlay = chunks }

    buildRiverChunk minX minY scaleF rg =
      let Rect (V2 bx by, _) = rgBounds rg
          dx = bx - minX
          dy = by - minY
          verts = SV.map (offsetVertex dx dy scaleF) (rgVertices rg)
      in AtlasChunkGeometry
          { acgVertices = verts
          , acgIndices = rgIndices rg
          }

-- | Normalise a world-space rect from an arbitrary hexRadius coordinate frame
-- to the base 'renderHexRadiusPx' (= 6) coordinate frame.
--
-- 'axialToScreen' adds fixed offsets ('hexOriginX', 'hexOriginY') that do not
-- scale with hexRadius.  We subtract those before scaling and re-add them
-- after, so the result is identical to what 'axialToScreen renderHexRadiusPx'
-- would produce for the same hex coordinates.
normalizeHexBounds :: Int -> Rect -> Rect
normalizeHexBounds hexR rect
  | hexR == renderHexRadiusPx = rect
  | otherwise =
      let s = fromIntegral renderHexRadiusPx / fromIntegral hexR :: Float
          scaleCoord ox v = round (s * fromIntegral (v - ox)) + ox
          scaleDim  v     = max 1 (round (s * fromIntegral v))
          Rect (V2 x y, V2 w h) = rect
      in Rect (V2 (scaleCoord hexOriginX x) (scaleCoord hexOriginY y)
              , V2 (scaleDim w) (scaleDim h))

-- | Upload pre-built atlas geometry into render-target textures.
--
-- Textures are acquired from the given 'TexturePool' rather than
-- allocated fresh, eliminating per-tile GPU allocation overhead.
-- All chunks within a tile are merged into a single draw call.
renderAtlasTileTextures :: TexturePool -> SDL.Renderer -> [AtlasTileGeometry] -> IO [TerrainAtlasTile]
renderAtlasTileTextures pool renderer tiles =
  concat <$> mapM renderTile tiles
  where
    renderTile tile = do
      let Rect (V2 _tx _ty, V2 tw th) = atgBounds tile
          scale' = atgScale tile
          sw = max 1 (tw * scale')
          sh = max 1 (th * scale')
          allChunks = atgChunks tile ++ atgRiverOverlay tile
      texture <- acquireTexture pool sw sh
      SDL.rendererRenderTarget renderer SDL.$= Just texture
      SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
      SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
      SDL.clear renderer
      renderBatched renderer allChunks
      SDL.rendererRenderTarget renderer SDL.$= Nothing
      pure [TerrainAtlasTile
        { tatTexture   = texture
        , tatBounds    = normalizeHexBounds (atgHexRadius tile) (atgBounds tile)
        , tatScale     = scale'
        , tatHexRadius = atgHexRadius tile
        }]

-- | Pre-merge multiple chunk geometries into a single chunk with rebased
-- indices.  Call on the worker thread to move the 'SV.concat' and index
-- rebasing allocations off the render thread.
mergeChunkGeometry :: [AtlasChunkGeometry] -> AtlasChunkGeometry
mergeChunkGeometry [] = AtlasChunkGeometry SV.empty SV.empty
mergeChunkGeometry [single] = single
mergeChunkGeometry chunks =
  let mergedVerts = SV.concat (map acgVertices chunks)
      vertOffsets = scanl (+) 0 (map (SV.length . acgVertices) chunks)
      rebase off chunk = SV.map (+ fromIntegral off) (acgIndices chunk)
      mergedIndices = SV.concat (zipWith rebase vertOffsets chunks)
  in AtlasChunkGeometry mergedVerts mergedIndices

-- | Render a list of chunk geometries in a single batched draw call.
-- Vertices are concatenated and indices are rebased to account for
-- the per-chunk vertex offsets.
renderBatched :: SDL.Renderer -> [AtlasChunkGeometry] -> IO ()
renderBatched _ [] = pure ()
renderBatched renderer chunks =
  let mergedVerts = SV.concat (map acgVertices chunks)
      vertOffsets = scanl (+) 0 (map (SV.length . acgVertices) chunks)
      rebase off chunk = SV.map (+ fromIntegral off) (acgIndices chunk)
      mergedIndices = SV.concat (zipWith rebase vertOffsets chunks)
  in  SDL.renderGeometry renderer Nothing mergedVerts mergedIndices

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect (V2 ax ay, V2 aw ah)) (Rect (V2 bx by, V2 bw bh)) =
  ax < bx + bw && ax + aw > bx && ay < by + bh && ay + ah > by

tileRects :: Rect -> Int -> Int -> [Rect]
tileRects (Rect (V2 x y, V2 w h)) tileW tileH =
  [ Rect (V2 tx ty, V2 tw th)
  | ty <- [y, y + tileH .. y + h - 1]
  , tx <- [x, x + tileW .. x + w - 1]
  , let tw = min tileW (x + w - tx)
  , let th = min tileH (y + h - ty)
  ]

foldBounds :: [Rect] -> Rect
foldBounds [] = Rect (V2 0 0, V2 1 1)
foldBounds rects =
  let xs = [x | Rect (V2 x _y, V2 _w _h) <- rects]
      ys = [y | Rect (V2 _x y, V2 _w _h) <- rects]
      x2s = [x + w | Rect (V2 x _y, V2 w _h) <- rects]
      y2s = [y + h | Rect (V2 _x y, V2 _w h) <- rects]
      minX = minimum xs
      minY = minimum ys
      maxX = maximum x2s
      maxY = maximum y2s
  in Rect (V2 minX minY, V2 (max 1 (maxX - minX)) (max 1 (maxY - minY)))
