{-# LANGUAGE OverloadedStrings #-}

-- | Handler for the @take_screenshot@ IPC command.
--
-- Posts a screenshot request to the render loop and blocks until the
-- result is available (typically within one frame — ~16 ms at 60 FPS).
module Seer.Command.Handlers.Screenshot
  ( handleTakeScreenshot
  ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.ByteString as BS
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.IORef (atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Command.Context (CommandContext(..))
import Seer.Screenshot (ScreenshotRequest(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @take_screenshot@ — capture the current renderer contents.
--
-- The handler posts a 'ScreenshotRequest' into the shared
-- 'ScreenshotRequestRef' and then blocks on the result 'MVar'.
-- The render loop services the request on the next frame.
--
-- Returns a JSON object with @image_base64@ (BMP, base64-encoded)
-- and @format@ (@\"bmp\"@), or an error if capture failed or another
-- screenshot is already in flight.
handleTakeScreenshot :: CommandContext -> Int -> Value -> IO SeerResponse
handleTakeScreenshot ctx reqId _params = do
  result <- newEmptyMVar
  let req = ScreenshotRequest { ssrResult = result }
  -- Atomically try to set the request.  If one is already pending,
  -- reject this request rather than overwriting the previous one.
  alreadyPending <- atomicModifyIORef' (ccScreenshotRef ctx) $ \prev ->
    case prev of
      Nothing -> (Just req, False)
      Just _  -> (prev, True)
  if alreadyPending
    then pure $ errResponse reqId
      "a screenshot is already in progress; please wait and retry"
    else do
      -- Block until the render loop fills the MVar (next frame).
      capture <- takeMVar result
      case capture of
        Left err -> pure $ errResponse reqId err
        Right bmpBytes -> pure $ okResponse reqId $ object
          [ "image_base64" .= encodeBase64Text bmpBytes
          , "format"       .= ("bmp" :: Text)
          ]

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text bytes = Text.pack (go 0)
  where
    len = BS.length bytes

    at :: Int -> Int
    at index = fromIntegral (BS.index bytes index)

    emit :: Int -> Char
    emit index = Text.index base64Alphabet index

    go :: Int -> String
    go index
      | index >= len = []
      | index + 2 < len =
          let b0 = at index
              b1 = at (index + 1)
              b2 = at (index + 2)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit (((b1 .&. 0x0F) `shiftL` 2) .|. (b2 `shiftR` 6))
              c3 = emit (b2 .&. 0x3F)
          in c0 : c1 : c2 : c3 : go (index + 3)
      | index + 1 < len =
          let b0 = at index
              b1 = at (index + 1)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit ((b1 .&. 0x0F) `shiftL` 2)
          in [c0, c1, c2, '=']
      | otherwise =
          let b0 = at index
              c0 = emit (b0 `shiftR` 2)
              c1 = emit ((b0 .&. 0x03) `shiftL` 4)
          in [c0, c1, '=', '=']

base64Alphabet :: Text
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
