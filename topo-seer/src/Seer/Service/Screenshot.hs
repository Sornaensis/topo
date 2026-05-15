{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Screenshot
  ( ScreenshotService(..)
  , screenshotServiceGroup
  , screenshotServiceOperationSpecs
  ) where

import Seer.Service.Types

data ScreenshotService = ScreenshotService
  { screenshotTake :: !ServiceHandler
  }

screenshotServiceGroup :: ServiceGroupSpec
screenshotServiceGroup = ServiceGroupSpec "screenshots" screenshotServiceOperationSpecs

screenshotServiceOperationSpecs :: [ServiceOperationSpec]
screenshotServiceOperationSpecs =
  [ operationSpec "screenshots.take" "take_screenshot" "Request a screenshot capture."
  ]
