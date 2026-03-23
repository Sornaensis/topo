{-# LANGUAGE OverloadedStrings #-}

-- | Screenshot capture types and pixel-readback logic.
--
-- The screenshot mechanism uses an 'IORef'-based request\/response
-- pattern to bridge the IPC command thread and the render thread:
--
-- 1. The IPC handler writes a 'ScreenshotRequest' into the
--    'ScreenshotRequestRef' and blocks on the result 'MVar'.
-- 2. The render loop (main thread, which owns the SDL2 renderer)
--    checks the ref each frame, captures pixels via
--    @SDL_RenderReadPixels@, encodes them as PNG, and fills the
--    'MVar'.
--
-- This ensures that all SDL2 renderer calls happen on the thread
-- that created the renderer, as required by SDL2.
module Seer.Screenshot
  ( ScreenshotRequest(..)
  , ScreenshotRequestRef
  , newScreenshotRequestRef
  , captureScreenshot
  , serviceScreenshotRequest
  ) where

import Codec.Picture (Image(..), PixelRGBA8(..), encodePng)
import Control.Concurrent.MVar (MVar, putMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as VS
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (nullPtr, castPtr)
import qualified SDL
import SDL.Internal.Types (Renderer(..))
import qualified SDL.Raw.Enum as RawEnum
import qualified SDL.Raw.Error as RawError
import qualified SDL.Raw.Video as RawVideo

-- | A pending screenshot request.  The render loop fills the 'MVar'
-- with either a PNG-encoded 'ByteString' or an error message.
data ScreenshotRequest = ScreenshotRequest
  { ssrResult :: !(MVar (Either Text ByteString))
    -- ^ Filled by the render loop with PNG data or an error.
  }

-- | Mutable ref checked by the render loop each frame.
type ScreenshotRequestRef = IORef (Maybe ScreenshotRequest)

-- | Create a new (empty) screenshot request ref.
newScreenshotRequestRef :: IO ScreenshotRequestRef
newScreenshotRequestRef = newIORef Nothing

-- | Capture the current renderer contents as a PNG-encoded 'ByteString'.
--
-- Must be called on the render thread (the thread that owns the
-- SDL2 renderer), and should be called /after/ all drawing is done
-- but /before/ 'SDL.present'.
captureScreenshot :: SDL.Renderer -> CInt -> CInt -> IO (Either Text ByteString)
captureScreenshot (Renderer rawRenderer) w h = do
  let bytesPerPixel = 4 :: CInt    -- RGBA
      pitch         = w * bytesPerPixel
      bufSize       = fromIntegral (pitch * h)
  buf <- mallocBytes bufSize
  rc <- RawVideo.renderReadPixels
          rawRenderer
          nullPtr                         -- NULL = entire render target
          RawEnum.SDL_PIXELFORMAT_ABGR8888  -- R-G-B-A byte order on LE
          (castPtr buf)
          pitch
  if rc /= 0
    then do
      free buf
      sdlErr <- RawError.getError >>= peekCString
      pure (Left ("SDL_RenderReadPixels failed: " <> Text.pack sdlErr))
    else do
      -- Copy the foreign buffer into a storable vector, then free
      -- the C allocation immediately.
      rawBytes <- BS.packCStringLen (castPtr buf, bufSize)
      free buf
      pure (Right (encodeScreenshotPng (fromIntegral w) (fromIntegral h) rawBytes))

-- | Encode raw RGBA pixel data as a PNG 'ByteString'.
--
-- Pixels are expected in RGBA byte order (as read via
-- @SDL_PIXELFORMAT_ABGR8888@ on little-endian systems).
encodeScreenshotPng :: Int -> Int -> ByteString -> ByteString
encodeScreenshotPng width height rawBytes =
  let pixelVec = VS.generate (width * height * 4) (BS.index rawBytes)
      img = Image
        { imageWidth  = width
        , imageHeight = height
        , imageData   = pixelVec
        } :: Image PixelRGBA8
  in BL.toStrict (encodePng img)

-- | Check the screenshot ref and, if a request is pending, capture
-- the current renderer contents and deliver the result.
--
-- Called by the render loop just before @SDL.present@.
serviceScreenshotRequest
  :: ScreenshotRequestRef
  -> SDL.Renderer
  -> CInt         -- ^ Window width
  -> CInt         -- ^ Window height
  -> IO ()
serviceScreenshotRequest ref renderer w h = do
  mReq <- readIORef ref
  case mReq of
    Nothing  -> pure ()
    Just req -> do
      writeIORef ref Nothing       -- consume the request
      result <- captureScreenshot renderer w h
      putMVar (ssrResult req) result
