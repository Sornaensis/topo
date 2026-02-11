module UI.Font
  ( FontCache
  , initFontCache
  , initFontCacheMaybe
  , destroyFontCache
  , drawText
  , drawTextCentered
  , textSize
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Font as Font

newtype CacheKey = CacheKey (Text, V4 Word8)

instance Eq CacheKey where
  CacheKey (t1, c1) == CacheKey (t2, c2) = t1 == t2 && c1 == c2

instance Ord CacheKey where
  CacheKey (t1, c1) `compare` CacheKey (t2, c2) =
    case compare t1 t2 of
      EQ -> compare c1 c2
      other -> other

data CachedText = CachedText
  { ctTexture :: SDL.Texture
  , ctSize :: V2 CInt
  }

data FontCache = FontCache
  { fcFont :: Font.Font
  , fcRenderer :: SDL.Renderer
  , fcCache :: IORef (Map.Map CacheKey CachedText)
  }

initFontCache :: SDL.Renderer -> FilePath -> Int -> IO FontCache
initFontCache renderer path size = do
  font <- Font.load path size
  cacheRef <- newIORef Map.empty
  pure FontCache
    { fcFont = font
    , fcRenderer = renderer
    , fcCache = cacheRef
    }

initFontCacheMaybe :: SDL.Renderer -> Int -> [FilePath] -> IO (Maybe FontCache)
initFontCacheMaybe renderer size paths = go paths
  where
    go [] = pure Nothing
    go (path:rest) = do
      result <- try (initFontCache renderer path size) :: IO (Either SomeException FontCache)
      case result of
        Right cache -> pure (Just cache)
        Left _ -> go rest

destroyFontCache :: FontCache -> IO ()
destroyFontCache cache = do
  cached <- readIORef (fcCache cache)
  mapM_ (SDL.destroyTexture . ctTexture) (Map.elems cached)
  Font.free (fcFont cache)

-- | Render text to a cached texture.  Returns 'Nothing' for empty or
-- whitespace-only input (SDL's @TTF_RenderUTF8_Blended@ rejects
-- zero-width text).
getCachedText :: FontCache -> V4 Word8 -> Text -> IO (Maybe CachedText)
getCachedText _cache _color text
  | Text.null (Text.strip text) = pure Nothing
getCachedText cache color text = do
  let key = CacheKey (text, color)
  cached <- readIORef (fcCache cache)
  case Map.lookup key cached of
    Just hit -> pure (Just hit)
    Nothing -> do
      surface <- Font.blended (fcFont cache) color text
      texture <- SDL.createTextureFromSurface (fcRenderer cache) surface
      SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
      SDL.freeSurface surface
      info <- SDL.queryTexture texture
      let w = fromIntegral (SDL.textureWidth info)
          h = fromIntegral (SDL.textureHeight info)
          entry = CachedText texture (V2 w h)
      writeIORef (fcCache cache) (Map.insert key entry cached)
      pure (Just entry)

drawText :: FontCache -> V4 Word8 -> V2 Int -> Text -> IO ()
drawText cache color (V2 x y) text = do
  result <- getCachedText cache color text
  case result of
    Nothing -> pure ()
    Just (CachedText texture (V2 w h)) -> do
      let dst = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 w h)
      SDL.copy (fcRenderer cache) texture Nothing (Just dst)

textSize :: FontCache -> V4 Word8 -> Text -> IO (V2 Int)
textSize cache color text = do
  result <- getCachedText cache color text
  case result of
    Nothing -> pure (V2 0 0)
    Just (CachedText _ (V2 w h)) ->
      pure (V2 (fromIntegral w) (fromIntegral h))

drawTextCentered :: FontCache -> V4 Word8 -> SDL.Rectangle CInt -> Text -> IO ()
drawTextCentered cache color (SDL.Rectangle (SDL.P (V2 x y)) (V2 w h)) text = do
  V2 tw th <- textSize cache color text
  let cx = fromIntegral x + (fromIntegral w - tw) `div` 2
      cy = fromIntegral y + (fromIntegral h - th) `div` 2
  drawText cache color (V2 cx cy) text
