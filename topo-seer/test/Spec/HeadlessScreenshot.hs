{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.HeadlessScreenshot (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory
  ( createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , listDirectory
  , removePathForcibly
  )
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Seer.Command.AppServiceAdapter
  ( commandAppService
  , dispatchAppServiceCommand
  )
import Seer.Command.Dispatch (CommandContext)
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessAppService
  , headlessCommandContext
  , headlessScreenshotRendererRequiredError
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Server
  ( HttpRequest(..)
  , HttpResponse(..)
  , defaultHttpServerConfig
  , handleHttpRequest
  )
import Seer.Screenshot.Request
  ( ScreenshotClaim
  , cancelScreenshotRequest
  , claimScreenshotRequest
  , deliverScreenshotRequest
  , screenshotRequestActive
  , screenshotRequestPending
  , submitScreenshotRequest
  )
import Seer.Service.AppService
  ( AppService(..)
  , ScreenshotService(..)
  , runServiceOperation
  )
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceErrorDetail(..)
  , ServiceRequest(..)
  , ServiceResult
  , runServiceHandler
  )
import Seer.Command.Types (SeerCommand(..), SeerResponse(..))

spec :: Spec
spec = describe "headless screenshot renderer requirement" $ do
  it "returns one structured renderer-required 503 across service, command, and HTTP surfaces" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let ctx = headlessServiceContext runtime
          commandCtx = headlessCommandContext runtime
      forM_ captureOnlyCases $ \(payload, httpBody) -> do
        result <- guarded "headless cross-surface rejection" $
          assertCrossSurfaceParity ctx commandCtx payload httpBody
        result `shouldBe` Left headlessScreenshotRendererRequiredError
      assertBrokerIdle ctx

  it "never writes image bytes even when screenshot persistence is enabled" $
    withFreshTempDir "no-write" $ \base -> do
      let root = base </> "screenshots"
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        let ctx = headlessServiceContext runtime
            commandCtx = headlessCommandContext runtime
            payload = object ["path" .= ("nested/capture.png" :: Text)]
        result <- guarded "headless path rejection" $
          assertCrossSurfaceParity ctx commandCtx payload (Just payload)
        result `shouldBe` Left headlessScreenshotRendererRequiredError
        listDirectory root `shouldReturn` []
        assertBrokerIdle ctx

  it "retains request validation without allocating renderer work" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let ctx = headlessServiceContext runtime
          commandCtx = headlessCommandContext runtime
      forM_ invalidPayloads $ \payload -> do
        result <- guarded "headless validation" $
          assertCrossSurfaceParity ctx commandCtx payload (Just payload)
        result `shouldBe` Left screenshotPathValidationError
      assertBrokerIdle ctx

  it "returns immediately without touching an already-busy renderer broker" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let ctx = headlessServiceContext runtime
      submitted <- submitScreenshotRequest (svcScreenshotRef ctx)
      ticket <- case submitted of
        Right value -> pure value
        Left err -> expectationFailure ("failed to install renderer probe: " <> show err) >> fail "probe"

      result <- guarded "busy broker headless rejection" $
        runServiceOperation headlessAppService ctx "take_screenshot" Null
      result `shouldBe` Left headlessScreenshotRendererRequiredError
      screenshotRequestPending (svcScreenshotRef ctx) `shouldReturn` True
      screenshotRequestActive (svcScreenshotRef ctx) `shouldReturn` True

      cancelScreenshotRequest (svcScreenshotRef ctx) ticket `shouldReturn` True
      assertBrokerIdle ctx

  it "keeps SDL HTTP capture renderer-backed through the runtime broker and storage" $
    withFreshTempDir "renderer-http" $ \base -> do
      let root = base </> "screenshots"
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        let ctx = headlessServiceContext runtime
            payload = object ["path" .= ("nested/renderer.png" :: Text)]
            renderedPng = "renderer-owned-png-bytes"
        _ <- forkIO (serveRendererCapture ctx renderedPng)
        response <- guarded "renderer-backed HTTP capture" $
          handleHttpRequest defaultHttpServerConfig commandAppService ctx HttpRequest
            { hreqMethod = "POST"
            , hreqPath = ["screenshots"]
            , hreqQuery = []
            , hreqHeaders = [("x-request-id", "renderer-http")]
            , hreqBody = Just payload
            }
        hresStatusCode response `shouldBe` 200
        lookup "x-request-id" (hresHeaders response) `shouldBe` Just "renderer-http"
        lookupField "source" (hresBody response) `shouldBe` Just (String "renderer")
        lookupField "saved_path" (hresBody response)
          `shouldBe` Just (String "nested/renderer.png")
        let savedPath = root </> "nested" </> "renderer.png"
        persisted <- doesFileExist savedPath
        if persisted
          then ByteString.readFile savedPath `shouldReturn` renderedPng
          else pendingWith
            "platform/filesystem did not retain the renderer screenshot publication"
        assertBrokerIdle ctx

  it "preserves command request ids while reporting renderer unavailability" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let command = SeerCommand 417 "take_screenshot" Null
      response <- guarded "headless command rejection" $
        dispatchAppServiceCommand headlessAppService (headlessCommandContext runtime) command
      srId response `shouldBe` 417
      srSuccess response `shouldBe` False
      srResult response `shouldBe` Null
      srError response `shouldBe` Just "screenshot capture requires the SDL renderer"
      assertBrokerIdle (headlessServiceContext runtime)

captureOnlyCases :: [(Value, Maybe Value)]
captureOnlyCases =
  [ (Null, Nothing)
  , (object [], Just (object []))
  ]

invalidPayloads :: [Value]
invalidPayloads =
  [ object ["path" .= ("../escape.png" :: Text)]
  , object ["path" .= (123 :: Int)]
  , object ["path" .= Null]
  ]

screenshotPathValidationError :: ServiceError
screenshotPathValidationError = ServiceValidationError "validation failed"
  [ ServiceErrorDetail
      ["path"]
      "invalid_field"
      "invalid field 'path' (expected a nonempty safe relative .png path)"
  ]

enabledHeadlessConfig :: FilePath -> HeadlessConfig
enabledHeadlessConfig root = defaultHeadlessConfig
  { hcRuntimeConfig = (hcRuntimeConfig defaultHeadlessConfig)
      { cfgScreenshotSaveDirectory = Just root }
  }

assertCrossSurfaceParity
  :: ServiceContext
  -> CommandContext
  -> Value
  -> Maybe Value
  -> IO ServiceResult
assertCrossSurfaceParity ctx commandCtx directPayload httpBody = do
  direct <- runServiceOperation headlessAppService ctx "take_screenshot" directPayload
  handler <- runServiceHandler (screenshotTake (appScreenshots headlessAppService))
    ctx (ServiceRequest (Just directPayload))
  handler `shouldBe` direct

  let command = SeerCommand 417 "take_screenshot" directPayload
  commandResponse <- dispatchAppServiceCommand headlessAppService commandCtx command
  assertCommandParity direct commandResponse

  httpResponse <- handleHttpRequest defaultHttpServerConfig headlessAppService ctx HttpRequest
    { hreqMethod = "POST"
    , hreqPath = ["screenshots"]
    , hreqQuery = []
    , hreqHeaders = [("x-request-id", "surface-request-417")]
    , hreqBody = httpBody
    }
  assertHttpParity direct httpResponse
  assertBrokerIdle ctx
  pure direct

assertCommandParity :: ServiceResult -> SeerResponse -> Expectation
assertCommandParity result response = do
  srId response `shouldBe` 417
  case result of
    Left err -> do
      srSuccess response `shouldBe` False
      srResult response `shouldBe` Null
      srError response `shouldBe` Just (serviceErrorCommandText err)
    Right success -> expectationFailure ("unexpected headless screenshot success: " <> show success)

assertHttpParity :: ServiceResult -> HttpResponse -> Expectation
assertHttpParity result response = do
  lookup "x-request-id" (hresHeaders response) `shouldBe` Just "surface-request-417"
  case result of
    Left err -> do
      hresStatusCode response `shouldBe` expectedStatus err
      lookupField "error" (hresBody response) `shouldSatisfy` (/= Nothing)
      lookupNestedField ["error", "code"] (hresBody response)
        `shouldBe` Just (String (expectedCode err))
      lookupNestedField ["error", "message"] (hresBody response)
        `shouldBe` Just (String (serviceErrorMessageText err))
      case err of
        ServiceUnavailable _ ->
          lookupNestedField ["error", "details"] (hresBody response)
            `shouldBe` Just (Array mempty)
        ServiceValidationError _ _ ->
          lookupNestedField ["error", "details"] (hresBody response)
            `shouldSatisfy` maybe False (/= Array mempty)
        _ -> pure ()
    Right success -> expectationFailure ("unexpected headless HTTP screenshot success: " <> show success)

expectedStatus :: ServiceError -> Int
expectedStatus (ServiceValidationError _ _) = 400
expectedStatus (ServiceUnavailable _) = 503
expectedStatus err = error ("unexpected screenshot error: " <> show err)

expectedCode :: ServiceError -> Text
expectedCode (ServiceValidationError _ _) = "validation_failed"
expectedCode (ServiceUnavailable _) = "unavailable"
expectedCode err = error ("unexpected screenshot error: " <> show err)

serviceErrorMessageText :: ServiceError -> Text
serviceErrorMessageText (ServiceValidationError message _) = message
serviceErrorMessageText (ServiceUnavailable message) = message
serviceErrorMessageText err = error ("unexpected screenshot error: " <> show err)

serviceErrorCommandText :: ServiceError -> Text
serviceErrorCommandText (ServiceUnavailable message) = message
serviceErrorCommandText (ServiceValidationError message details) =
  message <> ": " <> Text.intercalate "; " (map serviceErrorDetailMessage details)
serviceErrorCommandText err = error ("unexpected screenshot error: " <> show err)

assertBrokerIdle :: ServiceContext -> Expectation
assertBrokerIdle ctx = do
  screenshotRequestPending (svcScreenshotRef ctx) `shouldReturn` False
  screenshotRequestActive (svcScreenshotRef ctx) `shouldReturn` False

serveRendererCapture :: ServiceContext -> ByteString.ByteString -> IO ()
serveRendererCapture ctx bytes = do
  claim <- waitForClaim
  _ <- deliverScreenshotRequest (svcScreenshotRef ctx) claim (Right bytes)
  pure ()
  where
    waitForClaim :: IO ScreenshotClaim
    waitForClaim = do
      claimed <- claimScreenshotRequest (svcScreenshotRef ctx)
      case claimed of
        Just claim -> pure claim
        Nothing -> threadDelay 1000 >> waitForClaim

lookupField :: Text -> Value -> Maybe Value
lookupField name (Object fields) = KM.lookup (Key.fromText name) fields
lookupField _ _ = Nothing

lookupNestedField :: [Text] -> Value -> Maybe Value
lookupNestedField [] value = Just value
lookupNestedField (field:rest) value = lookupField field value >>= lookupNestedField rest

guarded :: String -> IO a -> IO a
guarded label action = do
  result <- timeout 5_000_000 action
  case result of
    Just value -> pure value
    Nothing -> expectationFailure (label <> " deadlocked") >> error "unreachable"

withFreshTempDir :: String -> (FilePath -> IO a) -> IO a
withFreshTempDir label action = do
  temp <- getTemporaryDirectory
  stamp <- round . (* (1_000_000 :: POSIXTime)) <$> getPOSIXTime
  let root = temp </> ("topo-headless-screenshot-" <> label <> "-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) cleanup action
  where
    cleanup path = do
      _ <- try (removePathForcibly path) :: IO (Either IOException ())
      pure ()
