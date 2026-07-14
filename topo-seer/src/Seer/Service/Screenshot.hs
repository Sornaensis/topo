{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Screenshot
  ( ScreenshotService(..)
  , ScreenshotTakeRequest(..)
  , ScreenshotTakeResponse(..)
  , ScreenshotSource(..)
  , screenshotTakeOperation
  , screenshotServiceGroup
  , screenshotServiceOperationSpecs
  , decodeScreenshotTakeRequest
  , prepareScreenshotTake
  , completeScreenshotTake
  , encodeScreenshotTakeResponse
  , rendererScreenshotHandler
  , rendererScreenshotHandlerWithDeadline
  , screenshotSubmitErrorToServiceError
  , screenshotResultErrorToServiceError
  ) where

import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.UiActions.Handles (ActorHandles(..))
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Seer.Screenshot.Request
  ( ScreenshotResultError(..)
  , ScreenshotSubmitError(..)
  , deadlineAfterMicros
  , submitAndWaitScreenshot
  )
import Seer.Screenshot.Storage
  ( ScreenshotSaveError(..)
  , ScreenshotStoragePolicy(..)
  , saveScreenshotPng
  , validateScreenshotPngPath
  )
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Types

-- | Timeout for waiting on the render loop to capture a screenshot.
screenshotTimeoutUs :: Int
screenshotTimeoutUs = 2_000_000

data ScreenshotService = ScreenshotService
  { screenshotTake :: !(ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse)
  }

newtype ScreenshotTakeRequest = ScreenshotTakeRequest
  { screenshotTakeSavePath :: Maybe Text
  } deriving (Eq, Show)

-- | The implementation that produced the PNG. This is required on every
-- successful response so callers never have to infer the active runtime.
data ScreenshotSource
  = ScreenshotSourceRenderer
  | ScreenshotSourceHeadless
  deriving (Eq, Show)

data ScreenshotTakeResponse = ScreenshotTakeResponse
  { screenshotTakeImageBase64 :: !Text
  , screenshotTakeFormat :: !Text
  , screenshotTakeSavedPath :: !(Maybe Text)
  , screenshotTakeSource :: !ScreenshotSource
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

-- | Parse and normalize the optional sandbox-relative PNG path. A missing
-- @path@ requests capture only; an explicitly present value must be valid.
decodeScreenshotTakeRequest :: Value -> Either ServiceError ScreenshotTakeRequest
decodeScreenshotTakeRequest Null = Right (ScreenshotTakeRequest Nothing)
decodeScreenshotTakeRequest (Object fields) =
  case KM.lookup "path" fields of
    Nothing -> Right (ScreenshotTakeRequest Nothing)
    Just (String path) ->
      case validateScreenshotPngPath path of
        Right normalized -> Right (ScreenshotTakeRequest (Just normalized))
        Left _ -> Left screenshotPathValidationError
    Just _ -> Left screenshotPathValidationError
decodeScreenshotTakeRequest _ = Left $ ServiceValidationError "validation failed"
  [ ServiceErrorDetail
      { serviceErrorDetailPath = []
      , serviceErrorDetailCode = "invalid_body"
      , serviceErrorDetailMessage = "screenshot request body must be an object"
      }
  ]

-- | Validate a request before capture starts, including the immutable write
-- authority. This makes a requested save fail deterministically when writes are
-- disabled rather than degrading to capture-only success.
prepareScreenshotTake
  :: ScreenshotStoragePolicy
  -> Value
  -> Either ServiceError ScreenshotTakeRequest
prepareScreenshotTake policy value = do
  request <- decodeScreenshotTakeRequest value
  case (screenshotTakeSavePath request, policy) of
    (Just _, ScreenshotStorageDisabled) ->
      Left (ServiceUnavailable "screenshot persistence is disabled")
    _ -> Right request

-- | Persist the captured PNG when requested and construct the shared response.
-- The saved path is populated only after the writer commits successfully.
completeScreenshotTake
  :: ScreenshotStoragePolicy
  -> ScreenshotSource
  -> ScreenshotTakeRequest
  -> ByteString.ByteString
  -> IO (Either ServiceError ScreenshotTakeResponse)
completeScreenshotTake policy source request pngBytes = do
  saved <- case screenshotTakeSavePath request of
    Nothing -> pure (Right Nothing)
    Just relativePath -> case policy of
      ScreenshotStorageDisabled ->
        pure (Left (ServiceUnavailable "screenshot persistence is disabled"))
      ScreenshotStorageEnabled root -> do
        result <- saveScreenshotPng root relativePath pngBytes
        pure (Just <$> either (Left . screenshotSaveServiceError) Right result)
  pure $ do
    savedPath <- saved
    Right ScreenshotTakeResponse
      { screenshotTakeImageBase64 = TextEncoding.decodeUtf8 (Base64.encode pngBytes)
      , screenshotTakeFormat = "png"
      , screenshotTakeSavedPath = savedPath
      , screenshotTakeSource = source
      }

-- | Encode every screenshot success surface with the same required fields.
encodeScreenshotTakeResponse :: ScreenshotTakeResponse -> Value
encodeScreenshotTakeResponse response = object
  [ "image_base64" .= screenshotTakeImageBase64 response
  , "format" .= screenshotTakeFormat response
  , "source" .= screenshotSourceText (screenshotTakeSource response)
  , "saved_path" .= screenshotTakeSavedPath response
  ]

screenshotSourceText :: ScreenshotSource -> Text
screenshotSourceText ScreenshotSourceRenderer = "renderer"
screenshotSourceText ScreenshotSourceHeadless = "headless"

screenshotPathValidationError :: ServiceError
screenshotPathValidationError = ServiceValidationError "validation failed"
  [ ServiceErrorDetail
      { serviceErrorDetailPath = ["path"]
      , serviceErrorDetailCode = "invalid_field"
      , serviceErrorDetailMessage =
          "invalid field 'path' (expected a nonempty safe relative .png path)"
      }
  ]

screenshotSaveServiceError :: ScreenshotSaveError -> ServiceError
screenshotSaveServiceError ScreenshotInvalidPath = screenshotPathValidationError
screenshotSaveServiceError ScreenshotUnsafePath = screenshotPathValidationError
screenshotSaveServiceError ScreenshotDestinationConflict =
  ServiceRejected "screenshot destination conflicts with an existing filesystem entry"
screenshotSaveServiceError ScreenshotStorageUnavailable =
  ServiceUnavailable "screenshot storage is unavailable"
screenshotSaveServiceError ScreenshotUnexpectedIO =
  ServiceInternalError "failed to persist screenshot"

-- | Renderer-backed service implementation used by GUI and command
-- compatibility surfaces.
rendererScreenshotHandler :: ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
rendererScreenshotHandler = rendererScreenshotHandlerWithDeadline
  (deadlineAfterMicros screenshotTimeoutUs)

-- | Injectable deadline variant used by deterministic service tests.
rendererScreenshotHandlerWithDeadline
  :: IO ()
  -> ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
rendererScreenshotHandlerWithDeadline makeDeadline =
  rawServiceHandler screenshotTakeOperation $ \ctx request ->
    case prepareScreenshotTake
        (svcScreenshotStoragePolicy ctx)
        (serviceRequestBodyValue request) of
      Left err -> pure (Left err)
      Right typedRequest -> captureRendererScreenshot makeDeadline ctx typedRequest

captureRendererScreenshot
  :: IO ()
  -> ServiceContext
  -> ScreenshotTakeRequest
  -> IO ServiceResult
captureRendererScreenshot makeDeadline ctx request = do
  logMsg LogInfo "[command] screenshot requested"
  captured <- submitAndWaitScreenshot (svcScreenshotRef ctx) makeDeadline
  case captured of
    Left submitError -> do
      case submitError of
        ScreenshotBusy ->
          logMsg LogWarn "[command] screenshot rejected: already in progress"
        ScreenshotClosed ->
          logMsg LogError "[command] screenshot rejected: capture broker is closed"
      pure (Left (screenshotSubmitErrorToServiceError submitError))
    Right (Left resultError) -> do
      case resultError of
        ScreenshotUnavailable ->
          logMsg LogError "[command] screenshot capture became unavailable"
        ScreenshotInternalError ->
          logMsg LogError "[command] screenshot capture failed"
      pure (Left (screenshotResultErrorToServiceError resultError))
    Right (Right pngBytes) -> do
      logMsg LogInfo
        ("[command] screenshot captured ("
          <> Text.pack (show (ByteString.length pngBytes)) <> " bytes)")
      completed <- completeScreenshotTake
        (svcScreenshotStoragePolicy ctx)
        ScreenshotSourceRenderer
        request
        pngBytes
      case completed of
        Left err -> do
          logMsg LogError ("[command] screenshot save failed: " <> serviceErrorText err)
          pure (Left err)
        Right response -> do
          case screenshotTakeSavedPath response of
            Nothing -> pure ()
            Just savedPath ->
              logMsg LogInfo ("[command] screenshot saved to " <> savedPath)
          pure (Right (ServiceResponse (encodeScreenshotTakeResponse response)))
  where
    logMsg level message =
      appendLog (ahLogHandle (svcActorHandles ctx)) (LogEntry level message)

screenshotSubmitErrorToServiceError :: ScreenshotSubmitError -> ServiceError
screenshotSubmitErrorToServiceError ScreenshotBusy = ServiceRejected
  "a screenshot is already in progress; please wait and retry"
screenshotSubmitErrorToServiceError ScreenshotClosed = ServiceUnavailable
  "screenshot capture is unavailable"

screenshotResultErrorToServiceError :: ScreenshotResultError -> ServiceError
screenshotResultErrorToServiceError ScreenshotUnavailable = ServiceUnavailable
  "screenshot capture is unavailable"
screenshotResultErrorToServiceError ScreenshotInternalError = ServiceInternalError
  "failed to capture screenshot"
