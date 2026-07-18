{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic service implementation for runtimes without an SDL renderer.
module Seer.Service.Headless
  ( headlessAppService
  , headlessScreenshotHandler
  , headlessScreenshotRendererRequiredError
  ) where

import Seer.Command.AppServiceAdapter (commandAppService)
import Seer.Service.AppService (AppService(..))
import Seer.Service.Screenshot
  ( ScreenshotService(..)
  , ScreenshotTakeRequest
  , ScreenshotTakeResponse
  , decodeScreenshotTakeRequest
  , screenshotTakeOperation
  )
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceHandler
  , rawServiceHandler
  , serviceRequestBodyValue
  )

-- | The single AppService used by every headless transport and command surface.
headlessAppService :: AppService
headlessAppService = commandAppService
  { appScreenshots = ScreenshotService
      { screenshotTake = headlessScreenshotHandler
      }
  }

-- | Reject valid capture requests immediately. Headless runtimes have no render
-- loop, so they must not queue broker work or manufacture image bytes.
headlessScreenshotHandler :: ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
headlessScreenshotHandler =
  rawServiceHandler screenshotTakeOperation $ \_ request ->
    pure $ case decodeScreenshotTakeRequest (serviceRequestBodyValue request) of
      Left err -> Left err
      Right _ -> Left headlessScreenshotRendererRequiredError

headlessScreenshotRendererRequiredError :: ServiceError
headlessScreenshotRendererRequiredError =
  ServiceUnavailable "screenshot capture requires the SDL renderer"
