{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.HeadlessScreenshot (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as ByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory
  ( createDirectory
  , createDirectoryLink
  , createFileLink
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Seer.Command.AppServiceAdapter (dispatchAppServiceCommand)
import Seer.Command.Channel (CommandChannelEnv(..), dispatchCommandChannel)
import Seer.Command.Dispatch (CommandContext(..))
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , deterministicHeadlessPng
  , headlessAppService
  , headlessAppServiceWithScreenshotWriter
  , headlessCommandContext
  , headlessDispatchCommand
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
import Seer.Screenshot.Storage (ScreenshotSaveError(..))
import Seer.Service.AppService
  ( AppService(..)
  , ScreenshotService(..)
  , runServiceOperation
  )
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Screenshot (rendererScreenshotHandlerWithDeadline)
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceErrorDetail(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceResult
  , runServiceHandler
  )
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))

spec :: Spec
spec = describe "headless screenshot service parity" $ do
  it "uses one deterministic service across HTTP, direct, legacy, and channel surfaces" $
    withFreshTempDir "surface-matrix" $ \base -> do
      let root = base </> "screenshots"
          cfg = enabledHeadlessConfig root
      withHeadlessApp cfg $ \runtime -> do
        let ctx = headlessServiceContext runtime
            commandCtx = headlessCommandContext runtime
        requirePersistenceCapability headlessAppService ctx root

        forM_ captureOnlyCases $ \(label, directPayload, httpBody) -> do
          result <- guarded label $
            assertCrossSurfaceParity headlessAppService ctx commandCtx directPayload httpBody
          assertHeadlessSuccess Nothing result
        listDirectory root `shouldReturn` []

        saved <- runServiceOperation headlessAppService ctx "take_screenshot" validPath
        assertHeadlessSuccess (Just "nested/capture.png") saved
        ByteString.readFile (root </> "nested" </> "capture.png")
          `shouldReturn` deterministicHeadlessPng

        -- Persistence is create-only on every surface.
        existing <- assertCrossSurfaceParity headlessAppService ctx commandCtx validPath (Just validPath)
        existing `shouldBe` Left screenshotDestinationConflict
        ByteString.readFile (root </> "nested" </> "capture.png")
          `shouldReturn` deterministicHeadlessPng

        forM_ invalidPayloads $ \payload -> do
          invalid <- assertCrossSurfaceParity headlessAppService ctx commandCtx payload (Just payload)
          invalid `shouldBe` Left screenshotPathValidationError

        ByteString.writeFile (root </> "occupied") "file parent"
        parentConflict <- assertCrossSurfaceParity headlessAppService ctx commandCtx
          (object ["path" .= ("occupied/view.png" :: Text)])
          (Just (object ["path" .= ("occupied/view.png" :: Text)]))
        parentConflict `shouldBe` Left screenshotDestinationConflict

        createDirectory (root </> "directory.png")
        destinationConflict <- assertCrossSurfaceParity headlessAppService ctx commandCtx
          (object ["path" .= ("directory.png" :: Text)])
          (Just (object ["path" .= ("directory.png" :: Text)]))
        destinationConflict `shouldBe` Left screenshotDestinationConflict

  it "validates paths before disabled storage and never allocates renderer work" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let ctx = headlessServiceContext runtime
          commandCtx = headlessCommandContext runtime
      forM_ invalidPayloads $ \payload -> do
        result <- assertCrossSurfaceParity headlessAppService ctx commandCtx payload (Just payload)
        result `shouldBe` Left screenshotPathValidationError
      disabled <- assertCrossSurfaceParity headlessAppService ctx commandCtx
        (object ["path" .= ("capture.png" :: Text)])
        (Just (object ["path" .= ("capture.png" :: Text)]))
      disabled `shouldBe` Left (ServiceUnavailable "screenshot persistence is disabled")

  it "rejects linked descendants and destinations on every surface" $
    withFreshTempDir "link-matrix" $ \base -> do
      let root = base </> "screenshots"
          outsideDirectory = base </> "outside-directory"
          outsideFile = base </> "outside.png"
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        let ctx = headlessServiceContext runtime
            commandCtx = headlessCommandContext runtime
        requirePersistenceCapability headlessAppService ctx root
        createDirectory outsideDirectory
        linkResult <- try (createDirectoryLink outsideDirectory (root </> "linked")) :: IO (Either IOException ())
        case linkResult of
          Left err -> pendingWith ("directory links unavailable: " <> displayException err)
          Right () -> do
            let linkedPayload = object ["path" .= ("linked/view.png" :: Text)]
            linked <- assertCrossSurfaceParity headlessAppService ctx commandCtx linkedPayload (Just linkedPayload)
            linked `shouldBe` Left screenshotPathValidationError
            listDirectory outsideDirectory `shouldReturn` []

        ByteString.writeFile outsideFile "outside"
        fileLinkResult <- try (createFileLink outsideFile (root </> "linked-destination.png"))
          :: IO (Either IOException ())
        case fileLinkResult of
          Left err -> pendingWith ("file links unavailable: " <> displayException err)
          Right () -> do
            let linkedPayload = object ["path" .= ("linked-destination.png" :: Text)]
            linked <- assertCrossSurfaceParity headlessAppService ctx commandCtx linkedPayload (Just linkedPayload)
            linked `shouldBe` Left screenshotDestinationConflict
            ByteString.readFile outsideFile `shouldReturn` "outside"

  it "maps invalidated roots and unexpected writer failures without leakage" $
    withFreshTempDir "failure-matrix" $ \base -> do
      let root = base </> "screenshots"
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        let ctx = headlessServiceContext runtime
            commandCtx = headlessCommandContext runtime
        removePathForcibly root
        let missingRootPayload = object ["path" .= ("capture.png" :: Text)]
        unavailable <- assertCrossSurfaceParity headlessAppService ctx commandCtx
          missingRootPayload (Just missingRootPayload)
        unavailable `shouldBe` Left (ServiceUnavailable "screenshot storage is unavailable")

      -- The injected writer seam returns only a stable class; no raw exception
      -- or sandbox path may escape through any transport envelope.
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        let failingApp = headlessAppServiceWithScreenshotWriter $ \_ _ _ ->
              pure (Left ScreenshotUnexpectedIO)
            ctx = headlessServiceContext runtime
            commandCtx = headlessCommandContext runtime
            payload = object ["path" .= ("capture.png" :: Text)]
        failed <- assertCrossSurfaceParity failingApp ctx commandCtx payload (Just payload)
        failed `shouldBe` Left (ServiceInternalError "failed to persist screenshot")
        responses <- runHttpSurfaces failingApp ctx (Just payload)
        forM_ responses $ \response -> do
          let encoded = Text.pack (show (Aeson.encode (hresBody response)))
          encoded `shouldSatisfy` not . Text.isInfixOf (Text.pack root)
          encoded `shouldSatisfy` not . Text.isInfixOf "ScreenshotUnexpectedIO"

  it "never touches an injected busy renderer broker or schedules its deadline" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      let ctx = headlessServiceContext runtime
          commandCtx = headlessCommandContext runtime
          command = SeerCommand 417 "take_screenshot" Null
          assertUntouched action = do
            body <- guarded "busy broker headless capture" action
            lookupField "source" body `shouldBe` Just (String "headless")
            screenshotRequestPending (svcScreenshotRef ctx) `shouldReturn` True
            screenshotRequestActive (svcScreenshotRef ctx) `shouldReturn` True
      submitted <- submitScreenshotRequest (svcScreenshotRef ctx)
      ticket <- case submitted of
        Right value -> pure value
        Left err -> expectationFailure ("failed to install renderer probe: " <> show err) >> fail "probe"

      assertUntouched $ expectServiceBody $
        runServiceOperation headlessAppService ctx "take_screenshot" Null
      assertUntouched $ expectServiceBody $
        runServiceHandler (screenshotTake (appScreenshots headlessAppService))
          ctx (ServiceRequest (Just Null))
      assertUntouched $ expectCommandBody $
        dispatchAppServiceCommand headlessAppService commandCtx command
      assertUntouched $ expectCommandBody $
        dispatchCommandChannel (channelEnv headlessAppService commandCtx) command
      responses <- runHttpSurfacesAllowBusyProbe headlessAppService ctx Nothing
      forM_ responses $ \response -> do
        hresStatusCode response `shouldBe` 200
        assertUntouched (pure (hresBody response))

      cancelScreenshotRequest (svcScreenshotRef ctx) ticket `shouldReturn` True
      assertBrokerIdle ctx

  it "keeps GUI validation identical and schedules no headless renderer deadline" $
    withHeadlessApp defaultHeadlessConfig $ \runtime -> do
      deadlineCount <- newIORef (0 :: Int)
      never <- newEmptyMVar
      let ctx = headlessServiceContext runtime
          guiHandler = rendererScreenshotHandlerWithDeadline $ do
            atomicModifyIORef' deadlineCount (\count -> (count + 1, ()))
            takeMVar never
          headlessHandler = screenshotTake (appScreenshots headlessAppService)
          preCapturePayloads = invalidPayloads <>
            [object ["path" .= ("capture.png" :: Text)]]
      forM_ preCapturePayloads $ \payload -> do
        gui <- runServiceHandler guiHandler ctx (ServiceRequest (Just payload))
        headless <- runServiceHandler headlessHandler ctx (ServiceRequest (Just payload))
        gui `shouldBe` headless
        assertBrokerIdle ctx
      readIORef deadlineCount `shouldReturn` 0

  it "matches successful GUI field semantics while preserving source and content differences" $
    withFreshTempDir "gui-parity" $ \root ->
      withHeadlessApp (enabledHeadlessConfig root) $ \runtime -> do
        requirePersistenceCapability headlessAppService (headlessServiceContext runtime) root
        deadlineCount <- newIORef (0 :: Int)
        deadlineStarted <- newEmptyMVar
        never <- newEmptyMVar
        let ctx = headlessServiceContext runtime
            guiPayload = object ["path" .= ("renderer.png" :: Text)]
            headlessPayload = object ["path" .= ("headless.png" :: Text)]
            deadline = do
              atomicModifyIORef' deadlineCount (\count -> (count + 1, ()))
              putMVar deadlineStarted ()
              takeMVar never
        _ <- forkIO (serveRendererCapture ctx deadlineStarted "renderer-png-bytes")
        gui <- runServiceHandler (rendererScreenshotHandlerWithDeadline deadline)
          ctx (ServiceRequest (Just guiPayload))
        headless <- runServiceOperation headlessAppService ctx "take_screenshot" headlessPayload
        assertBrokerIdle ctx
        readIORef deadlineCount `shouldReturn` 1
        case (gui, headless) of
          (Right (ServiceResponse guiBody), Right (ServiceResponse headlessBody)) -> do
            objectKeys guiBody `shouldBe` objectKeys headlessBody
            lookupField "format" guiBody `shouldBe` lookupField "format" headlessBody
            lookupField "saved_path" guiBody `shouldBe` Just (String "renderer.png")
            lookupField "saved_path" headlessBody `shouldBe` Just (String "headless.png")
            lookupField "source" guiBody `shouldBe` Just (String "renderer")
            lookupField "source" headlessBody `shouldBe` Just (String "headless")
            lookupField "image_base64" guiBody `shouldNotBe` lookupField "image_base64" headlessBody
          _ -> expectationFailure "expected successful GUI and headless screenshot results"

  it "wires hcStartCommandChannel to the headless dispatcher and preserves request ids" $
    withHeadlessApp defaultHeadlessConfig { hcStartCommandChannel = True } $ \runtime -> do
      let command = SeerCommand 417 "take_screenshot" Null
      response <- guarded "optional command channel" (headlessDispatchCommand runtime command)
      srId response `shouldBe` 417
      srSuccess response `shouldBe` True
      lookupField "source" (srResult response) `shouldBe` Just (String "headless")
      assertBrokerIdle (headlessServiceContext runtime)

captureOnlyCases :: [(String, Value, Maybe Value)]
captureOnlyCases =
  [ ("missing HTTP body", Null, Nothing)
  , ("empty object", object [], Just (object []))
  ]

invalidPayloads :: [Value]
invalidPayloads =
  [ object ["path" .= ("../escape.png" :: Text)]
  , object ["path" .= (123 :: Int)]
  , object ["path" .= Null]
  ]

validPath :: Value
validPath = object ["path" .= ("nested/capture.png" :: Text)]

screenshotPathValidationError :: ServiceError
screenshotPathValidationError = ServiceValidationError "validation failed"
  [screenshotPathDetail]

screenshotPathDetail :: ServiceErrorDetail
screenshotPathDetail = ServiceErrorDetail
  ["path"]
  "invalid_field"
  "invalid field 'path' (expected a nonempty safe relative .png path)"

screenshotDestinationConflict :: ServiceError
screenshotDestinationConflict = ServiceRejected
  "screenshot destination conflicts with an existing filesystem entry"

requirePersistenceCapability :: AppService -> ServiceContext -> FilePath -> IO ()
requirePersistenceCapability app ctx root = do
  let probePath = ".topo-capability-probe.png"
  result <- runServiceOperation app ctx "take_screenshot"
    (object ["path" .= (probePath :: Text)])
  case result of
    Right _ -> removeFile (root </> Text.unpack probePath)
    Left (ServiceUnavailable "screenshot storage is unavailable") ->
      pendingWith "platform/filesystem lacks safe screenshot publication support"
    Left err -> expectationFailure ("screenshot capability probe failed: " <> show err)

enabledHeadlessConfig :: FilePath -> HeadlessConfig
enabledHeadlessConfig root = defaultHeadlessConfig
  { hcRuntimeConfig = (hcRuntimeConfig defaultHeadlessConfig)
      { cfgScreenshotSaveDirectory = Just root }
  }

assertCrossSurfaceParity
  :: AppService
  -> ServiceContext
  -> CommandContext
  -> Value
  -> Maybe Value
  -> IO ServiceResult
assertCrossSurfaceParity app ctx commandCtx directPayload httpBody = do
  direct <- checked $ runServiceOperation app ctx "take_screenshot" directPayload
  handler <- checked $ runServiceHandler (screenshotTake (appScreenshots app))
    ctx (ServiceRequest (Just directPayload))
  handler `shouldBe` direct

  let command = SeerCommand 417 "take_screenshot" directPayload
  legacy <- checked $ dispatchAppServiceCommand app commandCtx command
  assertLegacyParity direct legacy
  channel <- checked $ dispatchCommandChannel (channelEnv app commandCtx) command
  channel `shouldBe` legacy

  httpResponses <- runHttpSurfaces app ctx httpBody
  forM_ httpResponses (assertHttpParity direct)
  pure direct
  where
    checked action = do
      result <- action
      assertBrokerIdle ctx
      pure result

runHttpSurfaces
  :: AppService
  -> ServiceContext
  -> Maybe Value
  -> IO [HttpResponse]
runHttpSurfaces app ctx body = do
  responses <- runHttpSurfacesAllowBusyProbe app ctx body
  assertBrokerIdle ctx
  pure responses

runHttpSurfacesAllowBusyProbe
  :: AppService
  -> ServiceContext
  -> Maybe Value
  -> IO [HttpResponse]
runHttpSurfacesAllowBusyProbe app ctx body = mapM requestFor
  [ ["screenshots"]
  , ["commands", "take_screenshot"]
  ]
  where
    requestFor path = handleHttpRequest defaultHttpServerConfig app ctx HttpRequest
      { hreqMethod = "POST"
      , hreqPath = path
      , hreqQuery = []
      , hreqHeaders = [("x-request-id", "surface-request-417")]
      , hreqBody = body
      }

assertLegacyParity :: ServiceResult -> SeerResponse -> Expectation
assertLegacyParity result response = do
  srId response `shouldBe` 417
  case result of
    Right (ServiceResponse body) -> do
      srSuccess response `shouldBe` True
      srResult response `shouldBe` body
      srError response `shouldBe` Nothing
    Left err -> case literalErrorContract err of
      Nothing -> expectationFailure ("missing literal legacy contract for " <> show err)
      Just contract -> do
        srSuccess response `shouldBe` False
        srResult response `shouldBe` Null
        srError response `shouldBe` Just (ecLegacyText contract)

assertHttpParity :: ServiceResult -> HttpResponse -> Expectation
assertHttpParity result response = do
  lookup "x-request-id" (hresHeaders response) `shouldBe` Just "surface-request-417"
  case result of
    Right (ServiceResponse body) -> do
      hresStatusCode response `shouldBe` 200
      hresBody response `shouldBe` body
    Left err -> case literalErrorContract err of
      Nothing -> expectationFailure ("missing literal HTTP contract for " <> show err)
      Just contract -> do
        hresStatusCode response `shouldBe` ecHttpStatus contract
        hresBody response `shouldBe` serviceErrorEnvelope "surface-request-417" contract

data ErrorContract = ErrorContract
  { ecHttpStatus :: !Int
  , ecCode :: !Text
  , ecMessage :: !Text
  , ecDetails :: ![ServiceErrorDetail]
  , ecLegacyText :: !Text
  }

literalErrorContract :: ServiceError -> Maybe ErrorContract
literalErrorContract err
  | err == screenshotPathValidationError = Just ErrorContract
      { ecHttpStatus = 400
      , ecCode = "validation_failed"
      , ecMessage = "validation failed"
      , ecDetails = [screenshotPathDetail]
      , ecLegacyText =
          "validation failed: invalid field 'path' (expected a nonempty safe relative .png path)"
      }
  | err == ServiceUnavailable "screenshot persistence is disabled" = Just (ErrorContract
      503 "unavailable" "screenshot persistence is disabled" []
      "screenshot persistence is disabled")
  | err == screenshotDestinationConflict = Just (ErrorContract
      409 "rejected"
      "screenshot destination conflicts with an existing filesystem entry" []
      "screenshot destination conflicts with an existing filesystem entry")
  | err == ServiceUnavailable "screenshot storage is unavailable" = Just (ErrorContract
      503 "unavailable" "screenshot storage is unavailable" []
      "screenshot storage is unavailable")
  | err == ServiceInternalError "failed to persist screenshot" = Just (ErrorContract
      500 "internal_error" "failed to persist screenshot" []
      "failed to persist screenshot")
  | otherwise = Nothing

serviceErrorEnvelope :: Text -> ErrorContract -> Value
serviceErrorEnvelope requestId contract = object
  [ "error" .= object
      [ "code" .= ecCode contract
      , "message" .= ecMessage contract
      , "details" .=
          [ object
              [ "path" .= serviceErrorDetailPath detail
              , "code" .= serviceErrorDetailCode detail
              , "message" .= serviceErrorDetailMessage detail
              ]
          | detail <- ecDetails contract
          ]
      , "request_id" .= requestId
      ]
  ]

channelEnv :: AppService -> CommandContext -> CommandChannelEnv
channelEnv app ctx = CommandChannelEnv
  { cceAppService = app
  , cceActorHandles = ccActorHandles ctx
  , cceUiSnapshotRef = ccUiSnapshotRef ctx
  , cceUiActionsHandle = ccUiActionsHandle ctx
  , cceScreenshotRef = ccScreenshotRef ctx
  , cceScreenshotStoragePolicy = ccScreenshotStoragePolicy ctx
  , cceLogSnapshotRef = ccLogSnapshotRef ctx
  , cceDataBrowserExecutor = ccDataBrowserExecutor ctx
  }

assertBrokerIdle :: ServiceContext -> Expectation
assertBrokerIdle ctx = do
  screenshotRequestPending (svcScreenshotRef ctx) `shouldReturn` False
  screenshotRequestActive (svcScreenshotRef ctx) `shouldReturn` False

serveRendererCapture :: ServiceContext -> MVar () -> ByteString.ByteString -> IO ()
serveRendererCapture ctx deadlineStarted bytes = do
  claim <- waitForClaim
  takeMVar deadlineStarted
  _ <- deliverScreenshotRequest (svcScreenshotRef ctx) claim (Right bytes)
  pure ()
  where
    waitForClaim :: IO ScreenshotClaim
    waitForClaim = do
      claimed <- claimScreenshotRequest (svcScreenshotRef ctx)
      case claimed of
        Just claim -> pure claim
        Nothing -> threadDelay 1000 >> waitForClaim

objectKeys :: Value -> [Text]
objectKeys (Object fields) = sort (map Key.toText (KM.keys fields))
objectKeys _ = []

lookupField :: Text -> Value -> Maybe Value
lookupField name (Object fields) = KM.lookup (Key.fromText name) fields
lookupField _ _ = Nothing

expectServiceBody :: IO ServiceResult -> IO Value
expectServiceBody action = action >>= \result -> case result of
  Right (ServiceResponse body) -> pure body
  Left err -> expectationFailure ("expected service success, got " <> show err) >> fail "service"

expectCommandBody :: IO SeerResponse -> IO Value
expectCommandBody action = action >>= \response ->
  if srSuccess response
    then pure (srResult response)
    else expectationFailure ("expected command success, got " <> show (srError response)) >> fail "command"

assertHeadlessSuccess :: Maybe Text -> ServiceResult -> Expectation
assertHeadlessSuccess expectedPath result = case result of
  Right (ServiceResponse body) -> do
    objectKeys body `shouldBe` ["format", "image_base64", "saved_path", "source"]
    lookupField "format" body `shouldBe` Just (String "png")
    lookupField "source" body `shouldBe` Just (String "headless")
    lookupField "saved_path" body `shouldBe` maybe (Just Null) (Just . String) expectedPath
    lookupField "image_base64" body `shouldSatisfy` \value -> case value of
      Just (String encoded) -> not (Text.null encoded)
      _ -> False
  Left err -> expectationFailure ("expected screenshot success, got " <> show err)

guarded :: String -> IO a -> IO a
guarded label action = do
  result <- timeout 10_000_000 action
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
