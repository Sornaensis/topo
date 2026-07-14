{-# LANGUAGE OverloadedStrings #-}

module Spec.ScreenshotRequest (spec) where

import Control.Concurrent
  ( forkIO
  , killThread
  , newEmptyMVar
  , putMVar
  , takeMVar
  , throwTo
  )
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , putTMVar
  , readTMVar
  , retry
  )
import Control.Exception
  ( AsyncException(..)
  , SomeAsyncException
  , SomeException
  , finally
  , fromException
  , throwIO
  , try
  )
import Control.Monad (replicateM)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.ByteString as BS
import Actor.AtlasManager (emptyAtlasManagerQueueState)
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import Test.Hspec

import Seer.Screenshot.Request
import Seer.Service.Screenshot
  ( prepareScreenshotTake
  , screenshotResultErrorToServiceError
  , screenshotSubmitErrorToServiceError
  )
import Seer.Screenshot.Storage (ScreenshotStoragePolicy(..))
import Seer.System
  ( renderSnapshotForceRequired
  , runRendererWithScreenshotBroker
  , shouldSkipUnchangedFrame
  )
import Seer.Service.Types
  ( ServiceError(..)
  , serviceErrorCode
  , serviceErrorHTTPStatus
  , serviceErrorText
  )

spec :: Spec
spec = describe "screenshot request broker" $ do
  describe "1. atomic single-slot submission" $ do
    it "accepts exactly one simultaneous submission and rejects the other" $ do
      broker <- newScreenshotRequestRef
      gate <- newEmptyTMVarIO
      results <- newEmptyMVar
      _ <- replicateM 2 $ forkIO $ do
        atomically (readTMVar gate)
        submitScreenshotRequest broker >>= putMVar results
      atomically (putTMVar gate ())
      submitted <- replicateM 2 (takeMVar results)
      length (filter isRight submitted) `shouldBe` 1
      length (filter (== Left ScreenshotBusy) submitted) `shouldBe` 1

  describe "2. queued deadline" $ do
    it "terminalizes before claim and rejects the stale claim" $ do
      broker <- newScreenshotRequestRef
      ticket <- submitOrFail broker
      pendingBefore <- screenshotRequestPending broker
      pendingBefore `shouldBe` True
      gate <- newEmptyTMVarIO
      resultVar <- newEmptyMVar
      _ <- forkIO $ waitScreenshotRequest broker ticket (atomically (readTMVar gate))
        >>= putMVar resultVar
      atomically (putTMVar gate ())
      takeMVar resultVar `shouldReturn` Left ScreenshotUnavailable
      screenshotRequestPending broker `shouldReturn` False
      screenshotRequestActive broker `shouldReturn` False
      claimScreenshotRequest broker `shouldReturn` Nothing

  describe "3. claimed deadline and stale delivery" $ do
    it "releases A without allowing late A delivery to alter queued B" $ do
      broker <- newScreenshotRequestRef
      ticketA <- submitOrFail broker
      claimA <- claimOrFail broker
      screenshotRequestPending broker `shouldReturn` False
      expireScreenshotRequest broker ticketA `shouldReturn` True
      ticketB <- submitOrFail broker
      deliverScreenshotRequest broker claimA (Right "late-a")
        `shouldReturn` ScreenshotStale
      screenshotRequestPending broker `shouldReturn` True
      claimB <- claimOrFail broker
      deliverScreenshotRequest broker claimB (Right "capture-b")
        `shouldReturn` ScreenshotDelivered
      waitNever broker ticketA `shouldReturn` Left ScreenshotUnavailable
      waitNever broker ticketB `shouldReturn` Right "capture-b"

  describe "4. async waiter cancellation" $ do
    it "cleans up both queued and claimed ownership without timing assumptions" $ do
      assertCancelledWaiter False
      assertCancelledWaiter True

  describe "5. completion versus deadline" $ do
    it "has exactly one terminal winner in either latch-forced transaction order" $ do
      assertTerminalRace True
      assertTerminalRace False

  describe "6. stale-token clobber regression" $ do
    it "keeps B intact after every stale terminal operation from A" $ do
      broker <- newScreenshotRequestRef
      ticketA <- submitOrFail broker
      claimA <- claimOrFail broker
      cancelScreenshotRequest broker ticketA `shouldReturn` True
      ticketB <- submitOrFail broker
      claimScreenshotTicket broker ticketA `shouldReturn` Nothing
      cancelScreenshotRequest broker ticketA `shouldReturn` False
      expireScreenshotRequest broker ticketA `shouldReturn` False
      deliverScreenshotRequest broker claimA (Right "stale")
        `shouldReturn` ScreenshotStale
      screenshotRequestPending broker `shouldReturn` True
      claimB <- claimOrFail broker
      deliverScreenshotRequest broker claimB (Right "new")
        `shouldReturn` ScreenshotDelivered
      waitNever broker ticketB `shouldReturn` Right "new"

  describe "7. nonblocking at-most-once delivery" $ do
    it "returns stale immediately for duplicate and late completions" $ do
      broker <- newScreenshotRequestRef
      ticket <- submitOrFail broker
      claim <- claimOrFail broker
      deliverScreenshotRequest broker claim (Right "first")
        `shouldReturn` ScreenshotDelivered
      deliverScreenshotRequest broker claim (Right "second")
        `shouldReturn` ScreenshotStale
      deliverScreenshotRequest broker claim (Left ScreenshotInternalError)
        `shouldReturn` ScreenshotStale
      waitNever broker ticket `shouldReturn` Right "first"

  describe "8. capture failure and synchronous exception" $ do
    it "returns only a fixed internal error and leaves the broker reusable" $ do
      broker <- newScreenshotRequestRef
      failureTicket <- submitOrFail broker
      failureClaim <- claimOrFail broker
      deliverScreenshotRequest broker failureClaim (Left ScreenshotInternalError)
        `shouldReturn` ScreenshotDelivered
      waitNever broker failureTicket `shouldReturn` Left ScreenshotInternalError

      exceptionTicket <- submitOrFail broker
      exceptionClaim <- claimOrFail broker
      runScreenshotCapture broker exceptionClaim
        (throwIO (userError "secret-host-path C:\\private"))
        `shouldReturn` ScreenshotDelivered
      exceptionResult <- waitNever broker exceptionTicket
      exceptionResult `shouldBe` Left ScreenshotInternalError
      show exceptionResult `shouldSatisfy` not . Text.isInfixOf "secret" . Text.pack
      replacement <- submitScreenshotRequest broker
      replacement `shouldSatisfy` isRight

  describe "9. shutdown and async render interruption" $ do
    it "unblocks queued and claimed waiters, closes, and ignores stale delivery" $ do
      queuedBroker <- newScreenshotRequestRef
      queuedTicket <- submitOrFail queuedBroker
      shutdownScreenshotRequestRef queuedBroker
      waitNever queuedBroker queuedTicket `shouldReturn` Left ScreenshotUnavailable
      screenshotRequestPending queuedBroker `shouldReturn` False
      submitScreenshotRequest queuedBroker `shouldReturn` Left ScreenshotClosed

      claimedBroker <- newScreenshotRequestRef
      claimedTicket <- submitOrFail claimedBroker
      claimed <- claimOrFail claimedBroker
      shutdownScreenshotRequestRef claimedBroker
      waitNever claimedBroker claimedTicket `shouldReturn` Left ScreenshotUnavailable
      deliverScreenshotRequest claimedBroker claimed (Right "late")
        `shouldReturn` ScreenshotStale
      shutdownScreenshotRequestRef claimedBroker
      submitScreenshotRequest claimedBroker `shouldReturn` Left ScreenshotClosed

    it "terminalizes before rethrowing an async render exception" $ do
      broker <- newScreenshotRequestRef
      ticket <- submitOrFail broker
      captureStarted <- newEmptyMVar
      renderDone <- newEmptyMVar
      renderThread <- forkIO $ do
        outcome <- try (claimAndRunScreenshotCapture broker $ do
          putMVar captureStarted ()
          atomically retry)
          :: IO (Either SomeException (Maybe ScreenshotDelivery))
        putMVar renderDone outcome
      takeMVar captureStarted
      throwTo renderThread ThreadKilled
      outcome <- takeMVar renderDone
      outcome `shouldSatisfy` isAsyncFailure
      waitNever broker ticket `shouldReturn` Left ScreenshotInternalError
      screenshotRequestActive broker `shouldReturn` False

    it "closes and unblocks around an exceptional outer renderer-loop exit" $ do
      broker <- newScreenshotRequestRef
      ticket <- submitOrFail broker
      loopStarted <- newEmptyMVar
      loopDone <- newEmptyMVar
      loopThread <- forkIO $ do
        outcome <- try (runRendererWithScreenshotBroker broker $ do
          putMVar loopStarted ()
          atomically retry) :: IO (Either SomeException ())
        putMVar loopDone outcome
      takeMVar loopStarted
      throwTo loopThread ThreadKilled
      loopOutcome <- takeMVar loopDone
      loopOutcome `shouldSatisfy` isAsyncFailure
      waitNever broker ticket `shouldReturn` Left ScreenshotUnavailable
      submitScreenshotRequest broker `shouldReturn` Left ScreenshotClosed

  describe "10. pending wake transitions" $ do
    it "selects a forced coherent snapshot read before screenshot capture" $ do
      renderSnapshotForceRequired
        True
        (SnapshotVersion 4)
        emptyAtlasManagerQueueState
        Nothing
        `shouldBe` True
      renderSnapshotForceRequired
        False
        (SnapshotVersion 4)
        emptyAtlasManagerQueueState
        Nothing
        `shouldBe` False
      shouldSkipUnchangedFrame True False False False `shouldBe` True

    it "leaves a request arriving after the frame claim queued for the next frame" $ do
      broker <- newScreenshotRequestRef
      ticketA <- submitOrFail broker
      frameClaim <- claimOrFail broker
      deliverScreenshotRequest broker frameClaim (Right "frame-a")
        `shouldReturn` ScreenshotDelivered
      waitNever broker ticketA `shouldReturn` Right "frame-a"

      ticketB <- submitOrFail broker
      runScreenshotCapture broker frameClaim (pure (Right "stale-frame"))
        `shouldReturn` ScreenshotStale
      screenshotRequestPending broker `shouldReturn` True
      claimB <- claimOrFail broker
      deliverScreenshotRequest broker claimB (Right "frame-b")
        `shouldReturn` ScreenshotDelivered
      waitNever broker ticketB `shouldReturn` Right "frame-b"

    it "is false -> true -> false across idle, queue, and atomic claim" $ do
      broker <- newScreenshotRequestRef
      screenshotRequestPending broker `shouldReturn` False
      ticket <- submitOrFail broker
      pending <- screenshotRequestPending broker
      pending `shouldBe` True
      shouldSkipUnchangedFrame True False pending False `shouldBe` False
      claimAndRunScreenshotCapture broker (pure (Right "done"))
        `shouldReturn` Just ScreenshotDelivered
      screenshotRequestPending broker `shouldReturn` False
      waitNever broker ticket `shouldReturn` Right "done"

  describe "11. service and transport mapping" $ do
    it "maps busy, unavailable, and internal failures to stable public contracts" $ do
      let busy = screenshotSubmitErrorToServiceError ScreenshotBusy
          unavailable = screenshotResultErrorToServiceError ScreenshotUnavailable
          internal = screenshotResultErrorToServiceError ScreenshotInternalError
      map serviceErrorHTTPStatus [busy, unavailable, internal]
        `shouldBe` [409, 503, 500]
      map serviceErrorCode [busy, unavailable, internal]
        `shouldBe` ["rejected", "unavailable", "internal_error"]
      serviceErrorText internal `shouldBe` "failed to capture screenshot"
      serviceErrorText internal `shouldSatisfy` not . Text.isInfixOf "host"

    it "rejects invalid paths and disabled storage before any broker allocation" $ do
      broker <- newScreenshotRequestRef
      prepareScreenshotTake ScreenshotStorageDisabled
        (String "not-an-object") `shouldSatisfy` isLeft
      prepareScreenshotTake ScreenshotStorageDisabled
        (object ["path" .= ("capture.png" :: Text.Text)]) `shouldBe`
          Left (ServiceUnavailable "screenshot persistence is disabled")
      screenshotRequestActive broker `shouldReturn` False
      screenshotRequestPending broker `shouldReturn` False

submitOrFail :: ScreenshotRequestRef -> IO ScreenshotTicket
submitOrFail broker = submitScreenshotRequest broker >>= \caseResult ->
  case caseResult of
    Right ticket -> pure ticket
    Left err -> expectationFailure ("unexpected screenshot submission failure: " <> show err)
      >> fail "screenshot submission failed"

claimOrFail :: ScreenshotRequestRef -> IO ScreenshotClaim
claimOrFail broker = claimScreenshotRequest broker >>= \caseResult ->
  case caseResult of
    Just claim -> pure claim
    Nothing -> expectationFailure "expected queued screenshot claim"
      >> fail "screenshot claim failed"

waitNever
  :: ScreenshotRequestRef
  -> ScreenshotTicket
  -> IO (Either ScreenshotResultError BS.ByteString)
waitNever broker ticket = waitScreenshotRequest broker ticket (atomically retry)

assertCancelledWaiter :: Bool -> IO ()
assertCancelledWaiter claimFirst = do
  broker <- newScreenshotRequestRef
  waiting <- newEmptyMVar
  finished <- newEmptyMVar
  waiter <- forkIO $
    (submitAndWaitScreenshot broker (putMVar waiting () >> atomically retry) >> pure ())
      `finally` putMVar finished ()
  takeMVar waiting
  if claimFirst
    then do
      _ <- claimOrFail broker
      pure ()
    else screenshotRequestPending broker `shouldReturn` True
  killThread waiter
  takeMVar finished
  screenshotRequestActive broker `shouldReturn` False
  screenshotRequestPending broker `shouldReturn` False
  replacement <- submitScreenshotRequest broker
  replacement `shouldSatisfy` isRight

assertTerminalRace :: Bool -> IO ()
assertTerminalRace completionFirst = do
  broker <- newScreenshotRequestRef
  ticket <- submitOrFail broker
  claim <- claimOrFail broker
  completionGate <- newEmptyMVar
  deadlineGate <- newEmptyMVar
  completionDone <- newEmptyMVar
  deadlineDone <- newEmptyMVar
  _ <- forkIO $ do
    takeMVar completionGate
    deliverScreenshotRequest broker claim (Right "completed")
      >>= putMVar completionDone
  _ <- forkIO $ do
    takeMVar deadlineGate
    expireScreenshotRequest broker ticket >>= putMVar deadlineDone
  if completionFirst
    then do
      putMVar completionGate ()
      takeMVar completionDone `shouldReturn` ScreenshotDelivered
      putMVar deadlineGate ()
      takeMVar deadlineDone `shouldReturn` False
      waitNever broker ticket `shouldReturn` Right "completed"
    else do
      putMVar deadlineGate ()
      takeMVar deadlineDone `shouldReturn` True
      putMVar completionGate ()
      takeMVar completionDone `shouldReturn` ScreenshotStale
      waitNever broker ticket `shouldReturn` Left ScreenshotUnavailable

isAsyncFailure :: Either SomeException a -> Bool
isAsyncFailure (Left err) = case fromException err :: Maybe SomeAsyncException of
  Just _ -> True
  Nothing -> False
isAsyncFailure (Right _) = False
