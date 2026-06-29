module Seer.System.Screenshot
  ( screenshotRequestPending
  ) where

import Data.IORef (readIORef)
import Seer.Screenshot.Request (ScreenshotRequestRef)

-- | Check whether the render thread must draw a frame to service a pending
-- screenshot request.  Capture still happens inside 'Seer.Render.Frame', after
-- all drawing and before 'SDL.present', so SDL renderer ownership is unchanged.
screenshotRequestPending :: ScreenshotRequestRef -> IO Bool
screenshotRequestPending ref = maybe False (const True) <$> readIORef ref
