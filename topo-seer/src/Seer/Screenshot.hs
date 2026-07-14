{-# LANGUAGE OverloadedStrings #-}

-- | Screenshot pixel readback and render-thread broker servicing.
--
-- The SDL-free broker in 'Seer.Screenshot.Request' atomically hands one queued
-- request to this module. Pixel readback remains on the renderer-owning thread.
module Seer.Screenshot
  ( ScreenshotRequestRef
  , newScreenshotRequestRef
  , captureScreenshot
  , serviceScreenshotRequest
  ) where

import Codec.Picture (Image(..), PixelRGBA8(..), encodePng)
import Control.Exception (bracket, evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
import Seer.Screenshot.Request
  ( ScreenshotRequestRef
  , ScreenshotResultError(..)
  , claimAndRunScreenshotCapture
  , newScreenshotRequestRef
  )

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
  bracket (mallocBytes bufSize) free $ \buf -> do
    rc <- RawVideo.renderReadPixels
            rawRenderer
            nullPtr                         -- NULL = entire render target
            RawEnum.SDL_PIXELFORMAT_ABGR8888  -- R-G-B-A byte order on LE
            (castPtr buf)
            pitch
    if rc /= 0
      then do
        sdlErr <- RawError.getError >>= peekCString
        pure (Left ("SDL_RenderReadPixels failed: " <> Text.pack sdlErr))
      else do
        rawBytes <- BS.packCStringLen (castPtr buf, bufSize)
        let pngBytes = encodeScreenshotPng
              (fromIntegral w)
              (fromIntegral h)
              rawBytes
        -- Drive the pure encoder before leaving broker exception handling.
        _ <- evaluate (BS.length pngBytes)
        pure (Right pngBytes)

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

-- | Atomically claim queued work before performing SDL readback, then deliver
-- at most once. Capture failures and synchronous exceptions become a fixed
-- internal result; an async exception first terminalizes the waiter and is
-- then rethrown. Called just before @SDL.present@.
serviceScreenshotRequest
  :: ScreenshotRequestRef
  -> SDL.Renderer
  -> CInt         -- ^ Window width
  -> CInt         -- ^ Window height
  -> IO ()
serviceScreenshotRequest ref renderer w h = do
  _ <- claimAndRunScreenshotCapture ref $ do
    captured <- captureScreenshot renderer w h
    pure $ case captured of
      Left _ -> Left ScreenshotInternalError
      Right pngBytes -> Right pngBytes
  pure ()
