-- | Rendering helpers for building terrain chunk geometry and textures.
module UI.TerrainRender
  ( ChunkGeometry(..)
  , ChunkTexture(..)
  , buildChunkGeometry
  , buildChunkTexture
  , destroyChunkTexture
  ) where

import Actor.UI (ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Topo (ChunkCoord(..), ChunkId(..), ClimateChunk(..), TerrainChunk(..), WeatherChunk(..), TileCoord(..), TileIndex(..), WorldConfig(..), chunkCoordFromId, chunkOriginTile, tileCoordFromIndex)
import UI.HexPick (axialToScreen)
import UI.TerrainColor (terrainColor)
import UI.Widgets (Rect(..))

hexSize :: Int
hexSize = 6

hexOverlap :: Float
hexOverlap = 0.6

-- | Triangle mesh for a single terrain chunk, ready for rendering.
data ChunkGeometry = ChunkGeometry
  { cgBounds :: !Rect
  , cgVertices :: !(Vector Raw.Vertex)
  , cgIndices :: !(Vector CInt)
  } deriving (Eq, Show)

-- | Cached texture for a chunk atlas tile.
data ChunkTexture = ChunkTexture
  { ctTexture :: SDL.Texture
  , ctBounds :: !Rect
  }

-- | Build the mesh for a terrain chunk in the given view mode and climate context.
buildChunkGeometry :: WorldConfig -> ViewMode -> Float -> IntMap ClimateChunk -> IntMap WeatherChunk -> Int -> TerrainChunk -> ChunkGeometry
buildChunkGeometry config mode waterLevel climateMap weatherMap key chunk =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      TileCoord ox oy = chunkOriginTile config (ChunkCoord cx cy)
      climateChunk = IntMap.lookup key climateMap
      weatherChunk = IntMap.lookup key weatherMap
      total = U.length (tcElevation chunk)
      (minX, minY, maxX, maxY) = chunkBounds config hexSize (ChunkCoord cx cy)
      bounds = Rect (V2 minX minY, V2 (max 1 (maxX - minX)) (max 1 (maxY - minY)))
      corners = hexCornersF hexSize
      tileEntries =
        [ buildTileGeometry config mode waterLevel climateChunk weatherChunk chunk corners minX minY ox oy idx
        | idx <- [0 .. total - 1]
        ]
      vertices = concatMap fst tileEntries
      indices = concatMap snd tileEntries
  in ChunkGeometry
      { cgBounds = bounds
      , cgVertices = SV.fromList vertices
      , cgIndices = SV.fromList indices
      }

buildTileGeometry
  :: WorldConfig
  -> ViewMode
  -> Float
  -> Maybe ClimateChunk
  -> Maybe WeatherChunk
  -> TerrainChunk
  -> [Raw.FPoint]
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> ([Raw.Vertex], [CInt])
buildTileGeometry config mode waterLevel climateChunk weatherChunk chunk corners minX minY ox oy idx =
  let TileCoord tx ty = tileCoordFromIndex config (TileIndex idx)
      q = ox + tx
      r = oy + ty
      (cx, cy) = axialToScreen hexSize q r
      centerX = fromIntegral (cx - minX)
      centerY = fromIntegral (cy - minY)
      color = terrainColor mode waterLevel chunk climateChunk weatherChunk idx
      rawColor = toRawColor color
      center = Raw.FPoint (realToFrac centerX) (realToFrac centerY)
      baseIndex = fromIntegral (idx * 7)
  in buildHexTriangles center rawColor corners baseIndex

buildHexTriangles :: Raw.FPoint -> Raw.Color -> [Raw.FPoint] -> CInt -> ([Raw.Vertex], [CInt])
buildHexTriangles center color corners base =
  let centerVertex = Raw.Vertex center color (Raw.FPoint 0 0)
      cornerVertices =
        [ Raw.Vertex (offsetPoint center corner) color (Raw.FPoint 0 0)
        | corner <- corners
        ]
      vertices = centerVertex : cornerVertices
      indices =
        concat
          [ [base, base + fromIntegral i + 1, base + fromIntegral ((i + 1) `mod` 6) + 1]
          | i <- [0 .. 5]
          ]
  in (vertices, indices)

offsetPoint :: Raw.FPoint -> Raw.FPoint -> Raw.FPoint
offsetPoint (Raw.FPoint x y) (Raw.FPoint dx dy) =
  Raw.FPoint (x + dx) (y + dy)

hexCornersF :: Int -> [Raw.FPoint]
hexCornersF size =
  let s = fromIntegral size + hexOverlap
      angles = [-30, 30, 90, 150, 210, 270]
      toPoint a =
        let rad = degToRad a
        in Raw.FPoint (realToFrac (s * cos rad)) (realToFrac (s * sin rad))
  in map toPoint angles

degToRad :: Float -> Float
degToRad deg = deg * pi / 180

chunkBounds :: WorldConfig -> Int -> ChunkCoord -> (Int, Int, Int, Int)
chunkBounds config size (ChunkCoord cx cy) =
  let TileCoord ox oy = chunkOriginTile config (ChunkCoord cx cy)
      s = wcChunkSize config
      corners =
        [ (ox, oy)
        , (ox + s, oy)
        , (ox, oy + s)
        , (ox + s, oy + s)
        ]
      (xs, ys) = unzip [axialToScreen size q r | (q, r) <- corners]
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in (minX - size, minY - size, maxX + size, maxY + size)

toRawColor :: V4 Word8 -> Raw.Color
toRawColor (V4 r g b a) = Raw.Color r g b a

-- | Render a chunk mesh into a texture for fast atlas drawing.
buildChunkTexture :: SDL.Renderer -> Int -> ChunkGeometry -> IO ChunkTexture
buildChunkTexture renderer scale geometry = do
  let Rect (V2 _x _y, V2 w h) = cgBounds geometry
      sw = max 1 (w * scale)
      sh = max 1 (h * scale)
      scaleF = realToFrac scale
      scaleVertex (Raw.Vertex (Raw.FPoint x y) color tex) =
        Raw.Vertex (Raw.FPoint (x * scaleF) (y * scaleF)) color tex
      verts = SV.map scaleVertex (cgVertices geometry)
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral sw) (fromIntegral sh))
  SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
  SDL.rendererRenderTarget renderer SDL.$= Just texture
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
  SDL.clear renderer
  SDL.renderGeometry renderer Nothing verts (cgIndices geometry)
  SDL.rendererRenderTarget renderer SDL.$= Nothing
  pure ChunkTexture
    { ctTexture = texture
    , ctBounds = cgBounds geometry
    }

-- | Release GPU resources for a cached chunk texture.
destroyChunkTexture :: ChunkTexture -> IO ()
destroyChunkTexture chunkTexture =
  SDL.destroyTexture (ctTexture chunkTexture)
