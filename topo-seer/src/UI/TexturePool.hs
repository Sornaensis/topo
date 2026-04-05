-- | Re-usable pool of SDL render-target textures.
--
-- Eliminates per-tile GPU allocation overhead in the atlas pipeline by
-- recycling textures of the same dimensions.
module UI.TexturePool
  ( TexturePool
  , newTexturePool
  , acquireTexture
  , releaseTexture
  , destroyTexturePool
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Linear (V2(..))
import qualified SDL

-- | Pool of available render-target textures keyed by @(width, height)@.
data TexturePool = TexturePool
  { tpRenderer   :: !SDL.Renderer
  , tpAvailable  :: !(IORef (Map.Map (Int, Int) [SDL.Texture]))
  , tpMaxPerSize :: !Int
  }

-- | Create a new, empty texture pool.
--
-- @maxPerSize@ limits how many textures are kept per dimension bucket;
-- excess textures are destroyed immediately on release.
newTexturePool :: SDL.Renderer -> Int -> IO TexturePool
newTexturePool renderer maxPerSize = do
  ref <- newIORef Map.empty
  pure TexturePool
    { tpRenderer   = renderer
    , tpAvailable  = ref
    , tpMaxPerSize = maxPerSize
    }

-- | Acquire a render-target texture of the given pixel dimensions.
--
-- Returns a pooled texture if one of the exact size is available,
-- otherwise creates a fresh one.
acquireTexture :: TexturePool -> Int -> Int -> IO SDL.Texture
acquireTexture pool w h = do
  avail <- readIORef (tpAvailable pool)
  let key = (w, h)
  case Map.lookup key avail of
    Just (t:ts) -> do
      writeIORef (tpAvailable pool) (Map.insert key ts avail)
      pure t
    _ -> do
      tex <- SDL.createTexture (tpRenderer pool) SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral w) (fromIntegral h))
      SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
      pure tex

-- | Return a texture to the pool for later reuse.
--
-- The texture dimensions are queried via @SDL.queryTexture@.  If the
-- pool bucket for that size is already full, the texture is destroyed
-- immediately.
releaseTexture :: TexturePool -> SDL.Texture -> IO ()
releaseTexture pool tex = do
  info <- SDL.queryTexture tex
  let w = fromIntegral (SDL.textureWidth info)
      h = fromIntegral (SDL.textureHeight info)
      key = (w, h)
  avail <- readIORef (tpAvailable pool)
  let current = Map.findWithDefault [] key avail
  if length current >= tpMaxPerSize pool
    then SDL.destroyTexture tex
    else writeIORef (tpAvailable pool) (Map.insert key (tex : current) avail)

-- | Destroy all pooled textures and empty the pool.
destroyTexturePool :: TexturePool -> IO ()
destroyTexturePool pool = do
  avail <- readIORef (tpAvailable pool)
  mapM_ (mapM_ SDL.destroyTexture) (Map.elems avail)
  writeIORef (tpAvailable pool) Map.empty
