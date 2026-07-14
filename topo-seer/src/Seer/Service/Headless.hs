{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic service implementation for runtimes without an SDL renderer.
module Seer.Service.Headless
  ( headlessAppService
  , headlessAppServiceWithScreenshotWriter
  , headlessScreenshotHandler
  , headlessScreenshotHandlerWithWriter
  , deterministicHeadlessPng
  ) where

import Codec.Picture (Image(..), PixelRGBA8(..), encodePng)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Vector.Storable as Vector

import Seer.Command.AppServiceAdapter (commandAppService)
import Seer.Service.AppService (AppService(..))
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Screenshot
  ( ScreenshotPngWriter
  , ScreenshotService(..)
  , ScreenshotSource(..)
  , ScreenshotTakeRequest
  , ScreenshotTakeResponse
  , completeScreenshotTakeWithWriter
  , encodeScreenshotTakeResponse
  , prepareScreenshotTake
  , screenshotTakeOperation
  )
import Seer.Service.Types
  ( ServiceHandler
  , ServiceResponse(..)
  , rawServiceHandler
  , serviceRequestBodyValue
  )
import Seer.Screenshot.Storage (saveScreenshotPng)

-- | The single AppService used by every headless transport and command surface.
headlessAppService :: AppService
headlessAppService = headlessAppServiceWithScreenshotWriter saveScreenshotPng

-- | Test seam for classifying writer failures without exposing host details.
headlessAppServiceWithScreenshotWriter :: ScreenshotPngWriter -> AppService
headlessAppServiceWithScreenshotWriter writer = commandAppService
  { appScreenshots = ScreenshotService
      { screenshotTake = headlessScreenshotHandlerWithWriter writer
      }
  }

-- | Immediate deterministic capture which never submits to the renderer broker.
headlessScreenshotHandler :: ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
headlessScreenshotHandler = headlessScreenshotHandlerWithWriter saveScreenshotPng

headlessScreenshotHandlerWithWriter
  :: ScreenshotPngWriter
  -> ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
headlessScreenshotHandlerWithWriter writer =
  rawServiceHandler screenshotTakeOperation $ \ctx request ->
    case prepareScreenshotTake
        (svcScreenshotStoragePolicy ctx)
        (serviceRequestBodyValue request) of
      Left err -> pure (Left err)
      Right typedRequest -> do
        result <- completeScreenshotTakeWithWriter
          writer
          (svcScreenshotStoragePolicy ctx)
          ScreenshotSourceHeadless
          typedRequest
          deterministicHeadlessPng
        pure (ServiceResponse . encodeScreenshotTakeResponse <$> result)

-- | Stable opaque-black 1x1 RGBA PNG used by headless capture.
deterministicHeadlessPng :: ByteString.ByteString
deterministicHeadlessPng = LazyByteString.toStrict (encodePng image)
  where
    image = Image
      { imageWidth = 1
      , imageHeight = 1
      , imageData = Vector.fromList [0, 0, 0, 255]
      } :: Image PixelRGBA8
