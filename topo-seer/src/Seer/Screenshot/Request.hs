-- | SDL-free screenshot request types shared by command, render, and headless
-- runtime code.
module Seer.Screenshot.Request
  ( ScreenshotRequest(..)
  , ScreenshotRequestRef
  , newScreenshotRequestRef
  ) where

import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)

-- | A pending screenshot request. The render loop fills the 'MVar' with either
-- PNG data or an error message.
data ScreenshotRequest = ScreenshotRequest
  { ssrResult :: !(MVar (Either Text ByteString))
  }

-- | Mutable ref checked by the render loop each frame.
type ScreenshotRequestRef = IORef (Maybe ScreenshotRequest)

-- | Create a new empty screenshot request ref.
newScreenshotRequestRef :: IO ScreenshotRequestRef
newScreenshotRequestRef = newIORef Nothing
