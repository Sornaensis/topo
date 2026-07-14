module Seer.System.Screenshot
  ( screenshotRequestPending
  ) where

import Seer.Screenshot.Request
  ( ScreenshotRequestRef
  , screenshotRequestPending
  )

-- Capture still happens inside 'Seer.Render.Frame', after all drawing and
-- before 'SDL.present', so SDL renderer ownership is unchanged.
