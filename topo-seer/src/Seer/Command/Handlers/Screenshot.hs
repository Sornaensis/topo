{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for the @take_screenshot@ IPC command.
--
-- Posts a screenshot request to the render loop and blocks until the
-- result is available (typically within one frame — ~16 ms at 60 FPS).
-- Times out after 2 seconds if the render loop does not respond.
module Seer.Command.Handlers.Screenshot
  ( handleTakeScreenshot
  ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.IORef (atomicModifyIORef', writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Timeout (timeout)
import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.Screenshot (ScreenshotRequest(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Timeout for waiting on the render loop to capture a screenshot, in
-- microseconds (2 seconds).
screenshotTimeoutUs :: Int
screenshotTimeoutUs = 2_000_000

-- | Handle @take_screenshot@ — capture the current renderer contents.
--
-- The handler posts a 'ScreenshotRequest' into the shared
-- 'ScreenshotRequestRef' and then blocks on the result 'MVar'.
-- The render loop services the request on the next frame.
--
-- Returns a JSON object with @image_base64@ (PNG, base64-encoded)
-- and @format@ (@"png"@), or an error if capture failed, another
-- screenshot is already in flight, or the render loop does not
-- respond within 2 seconds.
handleTakeScreenshot :: CommandContext -> Int -> Value -> IO SeerResponse
handleTakeScreenshot ctx reqId _params = do
  logMsg LogInfo "[mcp] screenshot requested"
  result <- newEmptyMVar
  let req = ScreenshotRequest { ssrResult = result }
  -- Atomically try to set the request.  If one is already pending,
  -- reject this request rather than overwriting the previous one.
  alreadyPending <- atomicModifyIORef' (ccScreenshotRef ctx) $ \prev ->
    case prev of
      Nothing -> (Just req, False)
      Just _  -> (prev, True)
  if alreadyPending
    then do
      logMsg LogWarn "[mcp] screenshot rejected: already in progress"
      pure $ errResponse reqId
        "a screenshot is already in progress; please wait and retry"
    else do
      -- Block until the render loop fills the MVar, with a timeout.
      mCapture <- timeout screenshotTimeoutUs (takeMVar result)
      case mCapture of
        Nothing -> do
          -- Timed out — clear the request so it doesn't fire later
          -- into a dead MVar.
          writeIORef (ccScreenshotRef ctx) Nothing
          logMsg LogError "[mcp] screenshot timed out (render loop did not respond within 2s)"
          pure $ errResponse reqId
            "screenshot timed out: render loop did not respond within 2s"
        Just (Left err) -> do
          logMsg LogError ("[mcp] screenshot failed: " <> err)
          pure $ errResponse reqId err
        Just (Right pngBytes) -> do
          logMsg LogInfo ("[mcp] screenshot captured (" <> Text.pack (show (BS.length pngBytes)) <> " bytes)")
          let mSavePath = case _params of
                Object o -> case KM.lookup "path" o of
                  Just (Aeson.String p) -> Just (Text.unpack p)
                  _                     -> Nothing
                _        -> Nothing
          case mSavePath of
            Just savePath -> do
              BS.writeFile savePath pngBytes
              logMsg LogInfo ("[mcp] screenshot saved to " <> Text.pack savePath)
            Nothing -> pure ()
          pure $ okResponse reqId $ object
            [ "image_base64" .= Text.decodeUtf8 (Base64.encode pngBytes)
            , "format"       .= ("png" :: Text)
            ]
  where
    logMsg level msg =
      appendLog (ahLogHandle (ccActorHandles ctx)) (LogEntry level msg)
