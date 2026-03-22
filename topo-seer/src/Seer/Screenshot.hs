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
--    @SDL_RenderReadPixels@, encodes them as BMP, and fills the
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

import Control.Concurrent.MVar (MVar, putMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Bits ((.&.), shiftR)
import Data.List (foldl')
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (nullPtr, castPtr)
import qualified SDL
import SDL.Internal.Types (Renderer(..))
import qualified SDL.Raw.Enum as RawEnum
import qualified SDL.Raw.Video as RawVideo

-- | A pending screenshot request.  The render loop fills the 'MVar'
-- with either a BMP-encoded 'ByteString' or an error message.
data ScreenshotRequest = ScreenshotRequest
  { ssrResult :: !(MVar (Either Text ByteString))
    -- ^ Filled by the render loop with PNG data or an error.
  }

-- | Mutable ref checked by the render loop each frame.
type ScreenshotRequestRef = IORef (Maybe ScreenshotRequest)

-- | Create a new (empty) screenshot request ref.
newScreenshotRequestRef :: IO ScreenshotRequestRef
newScreenshotRequestRef = newIORef Nothing

-- | Capture the current renderer contents as a BMP-encoded 'ByteString'.
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
      pure (Left "SDL_RenderReadPixels failed")
    else do
      -- Copy the foreign buffer into a Haskell ByteString, then free
      -- the C allocation immediately.
      rawBytes <- BS.packCStringLen (castPtr buf, bufSize)
      free buf
      pure (Right (encodeBmp (fromIntegral w) (fromIntegral h) rawBytes))

encodeBmp :: Int -> Int -> ByteString -> ByteString
encodeBmp width height rgbaBytes =
  let pixelBytes = BS.concat
        [ encodeBmpRow y
        | y <- [height - 1, height - 2 .. 0]
        ]
      header = bmpFileHeader (54 + BS.length pixelBytes)
      dib = bmpInfoHeader width height
  in BS.concat [header, dib, pixelBytes]
  where
    rowStride = width * 4
    encodeBmpRow y = BS.pack $ foldl' step [] [0 .. width - 1]
      where
        rowOffset = y * rowStride
        step acc x =
          let i = rowOffset + (x * 4)
              r = BS.index rgbaBytes i
              g = BS.index rgbaBytes (i + 1)
              b = BS.index rgbaBytes (i + 2)
              a = BS.index rgbaBytes (i + 3)
          in acc ++ [b, g, r, a]

bmpFileHeader :: Int -> ByteString
bmpFileHeader fileSize = BS.pack
  [ 0x42, 0x4D
  , fromIntegral (fileSize .&. 0xFF)
  , fromIntegral ((fileSize `shiftR` 8) .&. 0xFF)
  , fromIntegral ((fileSize `shiftR` 16) .&. 0xFF)
  , fromIntegral ((fileSize `shiftR` 24) .&. 0xFF)
  , 0, 0, 0, 0
  , 54, 0, 0, 0
  ]

bmpInfoHeader :: Int -> Int -> ByteString
bmpInfoHeader width height = BS.pack
  [ 40, 0, 0, 0
  , le32 width !! 0, le32 width !! 1, le32 width !! 2, le32 width !! 3
  , le32 height !! 0, le32 height !! 1, le32 height !! 2, le32 height !! 3
  , 1, 0
  , 32, 0
  , 0, 0, 0, 0
  , 0, 0, 0, 0
  , 19, 11, 0, 0
  , 19, 11, 0, 0
  , 0, 0, 0, 0
  , 0, 0, 0, 0
  ]

le32 :: Int -> [Word8]
le32 n =
  [ fromIntegral (n .&. 0xFF)
  , fromIntegral ((n `shiftR` 8) .&. 0xFF)
  , fromIntegral ((n `shiftR` 16) .&. 0xFF)
  , fromIntegral ((n `shiftR` 24) .&. 0xFF)
  ]

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
