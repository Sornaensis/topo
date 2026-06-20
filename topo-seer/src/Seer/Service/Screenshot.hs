{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Screenshot
  ( ScreenshotService(..)
  , ScreenshotTakeRequest(..)
  , ScreenshotTakeResponse(..)
  , screenshotTakeOperation
  , screenshotServiceGroup
  , screenshotServiceOperationSpecs
  ) where

import Data.Text (Text)

import Seer.Service.Types

data ScreenshotService = ScreenshotService
  { screenshotTake :: !ServiceHandler
  }

newtype ScreenshotTakeRequest = ScreenshotTakeRequest
  { screenshotTakeSavePath :: Maybe FilePath
  } deriving (Eq, Show)

data ScreenshotTakeResponse = ScreenshotTakeResponse
  { screenshotTakeImageBase64 :: !Text
  , screenshotTakeFormat :: !Text
  , screenshotTakeSavedPath :: !(Maybe FilePath)
  , screenshotTakeSource :: !(Maybe Text)
  } deriving (Eq, Show)

screenshotServiceGroup :: ServiceGroupSpec
screenshotServiceGroup = ServiceGroupSpec "screenshots" screenshotServiceOperationSpecs

screenshotServiceOperationSpecs :: [ServiceOperationSpec]
screenshotServiceOperationSpecs =
  [ typedServiceOperationSpec screenshotTakeOperation
  ]

screenshotTakeOperation :: TypedServiceOperation ScreenshotTakeRequest ScreenshotTakeResponse
screenshotTakeOperation = typedOperation $
  operationSpec "screenshots.take" "take_screenshot" "Request a screenshot capture."
