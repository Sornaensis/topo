module UI.TerrainAtlas
  ( TerrainAtlasTile(..)
  , AtlasChunkGeometry(..)
  , AtlasTileGeometry(..)
  , buildAtlasTileGeometry
  , composeTilesFromGeometry
  , renderAtlasTileTextures
  , maxAtlasTextureSize
  ) where

import Actor.UI (ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Topo (ClimateChunk(..), TerrainChunk(..), WeatherChunk(..), WorldConfig(..))
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry)
import UI.Widgets (Rect(..))


-- | Render-target texture tile for an atlas slice.
data TerrainAtlasTile = TerrainAtlasTile
  { tatTexture :: SDL.Texture
  , tatBounds :: Rect
  , tatScale :: Int
  }

-- | Pre-transformed chunk geometry ready for per-tile atlas rendering.
data AtlasChunkGeometry = AtlasChunkGeometry
  { acgVertices :: !(Vector Raw.Vertex)
  , acgIndices :: !(Vector CInt)
  }

-- | CPU-built atlas tile geometry with per-tile vertex transforms applied.
data AtlasTileGeometry = AtlasTileGeometry
  { atgBounds :: !Rect
  , atgScale :: !Int
  , atgChunks :: ![AtlasChunkGeometry]
  }

maxAtlasTextureSize :: Int
maxAtlasTextureSize = 16384

-- | Build per-tile geometry on the CPU (no SDL renderer required).
buildAtlasTileGeometry
  :: ViewMode
  -> Float
  -> IntMap TerrainChunk
  -> IntMap ClimateChunk
  -> IntMap WeatherChunk
  -> WorldConfig
  -> Int
  -> [AtlasTileGeometry]
buildAtlasTileGeometry mode waterLevel terrainChunks climateChunks weatherChunks config scale =
  let geometryMap = IntMap.mapWithKey (buildChunkGeometry config mode waterLevel climateChunks weatherChunks) terrainChunks
  in composeTilesFromGeometry geometryMap scale

-- | Compose atlas tiles from a pre-built chunk geometry map.
--
-- Separated from 'buildAtlasTileGeometry' so callers can build per-chunk
-- geometry in IO (with explicit yields between chunks) and then compose
-- tiles from the pre-built map.  This prevents render thread starvation
-- caused by long-running pure computations whose pinned storable-vector
-- allocations bypass GHC's context-switch counter.
composeTilesFromGeometry :: IntMap ChunkGeometry -> Int -> [AtlasTileGeometry]
composeTilesFromGeometry geometryMap scale
  | IntMap.null geometryMap = []
  | otherwise =
    let boundsList = map cgBounds (IntMap.elems geometryMap)
        Rect (V2 minX minY, V2 maxW maxH) = foldBounds boundsList
        maxScaleW = maxAtlasTextureSize `div` max 1 maxW
        maxScaleH = maxAtlasTextureSize `div` max 1 maxH
        scale' = min scale (max 1 (min maxScaleW maxScaleH))
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
              { atgBounds = tileRect
              , atgScale = sc
              , atgChunks = chunks
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

    offsetVertex dx dy scaleF (Raw.Vertex (Raw.FPoint x y) color tex) =
      let x' = (x + realToFrac dx) * scaleF
          y' = (y + realToFrac dy) * scaleF
      in Raw.Vertex (Raw.FPoint x' y') color tex

-- | Upload pre-built atlas geometry into render-target textures.
renderAtlasTileTextures :: SDL.Renderer -> [AtlasTileGeometry] -> IO [TerrainAtlasTile]
renderAtlasTileTextures renderer tiles =
  concat <$> mapM renderTile tiles
  where
    renderTile tile = do
      let Rect (V2 _tx _ty, V2 tw th) = atgBounds tile
          scale' = atgScale tile
          sw = max 1 (tw * scale')
          sh = max 1 (th * scale')
      texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral sw) (fromIntegral sh))
      SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
      SDL.rendererRenderTarget renderer SDL.$= Just texture
      SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
      SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
      SDL.clear renderer
      mapM_ renderChunk (atgChunks tile)
      SDL.rendererRenderTarget renderer SDL.$= Nothing
      pure [TerrainAtlasTile
        { tatTexture = texture
        , tatBounds = atgBounds tile
        , tatScale = scale'
        }]

    renderChunk chunk =
      SDL.renderGeometry renderer Nothing (acgVertices chunk) (acgIndices chunk)

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
