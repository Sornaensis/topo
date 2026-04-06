-- | Rendering helpers for building terrain chunk geometry and textures.
module UI.TerrainRender
  ( ChunkGeometry(..)
  , ChunkTexture(..)
  , buildChunkGeometry
  , buildDayNightGeometry
  , buildChunkTexture
  , chunkBounds
  , destroyChunkTexture
  ) where

import Actor.UI (ViewMode(..))
import Control.Monad.ST (ST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.C.Types (CFloat, CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Topo (ChunkCoord(..), ChunkId(..), ClimateChunk(..), TerrainChunk(..), VegetationChunk(..), WeatherChunk(..), TileCoord(..), TileIndex(..), WorldConfig(..), chunkCoordFromId, chunkOriginTile, tileCoordFromIndex)
import UI.HexPick (axialToScreen, renderHexRadiusPx)
import UI.TerrainColor (terrainColor)
import UI.Widgets (Rect(..))

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
--
-- The @Maybe (U.Vector Float)@ parameter carries pre-extracted overlay field
-- data for this chunk when 'ViewOverlay' mode is active.  For all other
-- view modes it should be 'Nothing'.
--
-- Day\/night brightness is no longer baked into the base geometry; it is
-- rendered as a separate overlay layer (see 'buildDayNightGeometry').
--
-- Uses pre-allocated storable vectors and direct writes to avoid the
-- overhead of building intermediate lists and calling @SV.fromList@.
buildChunkGeometry :: Int -> WorldConfig -> ViewMode -> Float -> IntMap ClimateChunk -> IntMap WeatherChunk -> IntMap VegetationChunk -> Maybe (U.Vector Float) -> Int -> TerrainChunk -> ChunkGeometry
buildChunkGeometry hexRadiusPx config mode waterLevel climateMap weatherMap vegMap mOverlayVec key chunk =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      TileCoord ox oy = chunkOriginTile config (ChunkCoord cx cy)
      climateChunk = IntMap.lookup key climateMap
      weatherChunk = IntMap.lookup key weatherMap
      vegChunk = IntMap.lookup key vegMap
      total = U.length (tcElevation chunk)
      (minX, minY, maxX, maxY) = chunkBounds config hexRadiusPx (ChunkCoord cx cy)
      bounds = Rect (V2 minX minY, V2 (max 1 (maxX - minX)) (max 1 (maxY - minY)))
      corners = hexCornersF hexRadiusPx
      zeroTex = Raw.FPoint 0 0

      -- Pre-allocated vertex buffer: 7 vertices per hex (1 center + 6 corners)
      vertices = SV.create $ do
        mv <- SM.new (total * 7)
        let go idx
              | idx >= total = pure ()
              | otherwise = do
                  let TileCoord tx ty = tileCoordFromIndex config (TileIndex idx)
                      q = ox + tx
                      r = oy + ty
                      (scx, scy) = axialToScreen hexRadiusPx q r
                      centerX = fromIntegral (scx - minX) :: CFloat
                      centerY = fromIntegral (scy - minY) :: CFloat
                      overlayVal = case mOverlayVec of
                        Just vec | idx < U.length vec -> Just (vec U.! idx)
                        _ -> Nothing
                      baseColor = terrainColor mode waterLevel chunk climateChunk weatherChunk vegChunk overlayVal idx
                      rawColor = toRawColor baseColor
                      base = idx * 7
                  SM.unsafeWrite mv base (Raw.Vertex (Raw.FPoint centerX centerY) rawColor zeroTex)
                  writeHexCorners mv base centerX centerY rawColor zeroTex corners
                  go (idx + 1)
        go 0
        pure mv

      -- Pre-allocated index buffer: 18 indices per hex (6 triangles × 3)
      indices = SV.create $ do
        mi <- SM.new (total * 18)
        let go idx
              | idx >= total = pure ()
              | otherwise = do
                  let base = fromIntegral (idx * 7) :: CInt
                      iOff = idx * 18
                  writeHexIndices mi iOff base
                  go (idx + 1)
        go 0
        pure mi
  in ChunkGeometry
      { cgBounds = bounds
      , cgVertices = vertices
      , cgIndices = indices
      }

-- | Build a day\/night overlay mesh for a terrain chunk.
--
-- The overlay uses the same hex mesh layout as 'buildChunkGeometry'
-- but every vertex is black (@RGB = 0,0,0@) with alpha proportional
-- to shadow darkness: @alpha = round ((1 - brightness) * 255)@.
-- Fully-lit hexes (@brightness = 1.0@) are fully transparent;
-- night-side hexes (@brightness = 0.15@) are almost opaque black.
--
-- Drawn on top of the base atlas tiles using alpha blending, this
-- produces the day\/night dimming effect without baking brightness
-- into the base tile colours.
buildDayNightGeometry :: Int -> WorldConfig -> (Int -> Int -> Float) -> Int -> TerrainChunk -> ChunkGeometry
buildDayNightGeometry hexRadiusPx config dayNightFn key chunk =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      TileCoord ox oy = chunkOriginTile config (ChunkCoord cx cy)
      total = U.length (tcElevation chunk)
      (minX, minY, maxX, maxY) = chunkBounds config hexRadiusPx (ChunkCoord cx cy)
      bounds = Rect (V2 minX minY, V2 (max 1 (maxX - minX)) (max 1 (maxY - minY)))
      corners = hexCornersF hexRadiusPx
      zeroTex = Raw.FPoint 0 0

      vertices = SV.create $ do
        mv <- SM.new (total * 7)
        let go idx
              | idx >= total = pure ()
              | otherwise = do
                  let TileCoord tx ty = tileCoordFromIndex config (TileIndex idx)
                      q = ox + tx
                      r = oy + ty
                      (scx, scy) = axialToScreen hexRadiusPx q r
                      centerX = fromIntegral (scx - minX) :: CFloat
                      centerY = fromIntegral (scy - minY) :: CFloat
                      brightness = dayNightFn q r
                      alpha = fromIntegral (round ((1 - max 0 (min 1 brightness)) * 255) :: Int) :: Word8
                      rawColor = Raw.Color 0 0 0 alpha
                      base = idx * 7
                  SM.unsafeWrite mv base (Raw.Vertex (Raw.FPoint centerX centerY) rawColor zeroTex)
                  writeHexCorners mv base centerX centerY rawColor zeroTex corners
                  go (idx + 1)
        go 0
        pure mv

      indices = SV.create $ do
        mi <- SM.new (total * 18)
        let go idx
              | idx >= total = pure ()
              | otherwise = do
                  let base = fromIntegral (idx * 7) :: CInt
                      iOff = idx * 18
                  writeHexIndices mi iOff base
                  go (idx + 1)
        go 0
        pure mi
  in ChunkGeometry
      { cgBounds = bounds
      , cgVertices = vertices
      , cgIndices = indices
      }

-- | Write 6 corner vertices for a hex at the given base offset.
writeHexCorners :: SM.MVector s Raw.Vertex -> Int -> CFloat -> CFloat -> Raw.Color -> Raw.FPoint -> [Raw.FPoint] -> ST s ()
writeHexCorners mv base cx cy color tex corners = go 1 corners
  where
    go _ [] = pure ()
    go i (Raw.FPoint dx dy : rest) = do
      SM.unsafeWrite mv (base + i) (Raw.Vertex (Raw.FPoint (cx + dx) (cy + dy)) color tex)
      go (i + 1) rest
{-# INLINE writeHexCorners #-}

-- | Write 18 indices (6 triangles) for a hex at the given index offset.
writeHexIndices :: SM.MVector s CInt -> Int -> CInt -> ST s ()
writeHexIndices mi iOff base = do
  SM.unsafeWrite mi iOff       base
  SM.unsafeWrite mi (iOff + 1) (base + 1)
  SM.unsafeWrite mi (iOff + 2) (base + 2)
  SM.unsafeWrite mi (iOff + 3) base
  SM.unsafeWrite mi (iOff + 4) (base + 2)
  SM.unsafeWrite mi (iOff + 5) (base + 3)
  SM.unsafeWrite mi (iOff + 6) base
  SM.unsafeWrite mi (iOff + 7) (base + 3)
  SM.unsafeWrite mi (iOff + 8) (base + 4)
  SM.unsafeWrite mi (iOff + 9)  base
  SM.unsafeWrite mi (iOff + 10) (base + 4)
  SM.unsafeWrite mi (iOff + 11) (base + 5)
  SM.unsafeWrite mi (iOff + 12) base
  SM.unsafeWrite mi (iOff + 13) (base + 5)
  SM.unsafeWrite mi (iOff + 14) (base + 6)
  SM.unsafeWrite mi (iOff + 15) base
  SM.unsafeWrite mi (iOff + 16) (base + 6)
  SM.unsafeWrite mi (iOff + 17) (base + 1)
{-# INLINE writeHexIndices #-}

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
