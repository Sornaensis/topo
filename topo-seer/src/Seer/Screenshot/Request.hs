{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}

-- | SDL-free, identity-safe screenshot request broker shared by service and
-- render threads.
module Seer.Screenshot.Request
  ( ScreenshotRequestRef
  , ScreenshotTicket
  , ScreenshotClaim
  , ScreenshotSubmitError(..)
  , ScreenshotResultError(..)
  , ScreenshotDelivery(..)
  , newScreenshotRequestRef
  , submitScreenshotRequest
  , submitAndWaitScreenshot
  , waitScreenshotRequest
  , claimScreenshotRequest
  , claimScreenshotTicket
  , deliverScreenshotRequest
  , runScreenshotCapture
  , claimAndRunScreenshotCapture
  , expireScreenshotRequest
  , cancelScreenshotRequest
  , shutdownScreenshotRequestRef
  , screenshotRequestPending
  , screenshotRequestActive
  , deadlineAfterMicros
  ) where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM
  ( STM
  , TMVar
  , TVar
  , atomically
  , newEmptyTMVar
  , newTVar
  , readTMVar
  , readTVar
  , tryPutTMVar
  , writeTVar
  )
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , mask
  , onException
  , throwIO
  , try
  )
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | Why a request could not be installed in the broker.
data ScreenshotSubmitError
  = ScreenshotBusy
  | ScreenshotClosed
  deriving (Eq, Show)

-- | Stable, non-sensitive terminal failures visible to request waiters.
data ScreenshotResultError
  = ScreenshotUnavailable
  | ScreenshotInternalError
  deriving (Eq, Show)

-- | Whether a render-side terminal delivery won ownership of its ticket.
data ScreenshotDelivery
  = ScreenshotDelivered
  | ScreenshotStale
  deriving (Eq, Show)

type ScreenshotResult = Either ScreenshotResultError ByteString

data RequestPhase = RequestQueued | RequestClaimed
  deriving (Eq, Show)

data RequestIdentity = RequestIdentity
  { requestToken :: !Word64
  , requestResult :: !(TMVar ScreenshotResult)
  }

data ActiveRequest = ActiveRequest
  { activeIdentity :: !RequestIdentity
  , activePhase :: !RequestPhase
  }

data BrokerState = BrokerState
  { brokerClosed :: !Bool
  , brokerNextToken :: !Word64
  , brokerActive :: !(Maybe ActiveRequest)
  }

-- | The single-slot screenshot broker. The historical name is retained to
-- avoid leaking the implementation change through runtime context records.
newtype ScreenshotRequestRef = ScreenshotRequestRef (TVar BrokerState)

-- | Request-thread capability used for waiting, deadline, and cancellation.
newtype ScreenshotTicket = ScreenshotTicket RequestIdentity

-- | Render-thread capability obtained only by atomically claiming queued work.
newtype ScreenshotClaim = ScreenshotClaim RequestIdentity

instance Eq ScreenshotTicket where
  ScreenshotTicket left == ScreenshotTicket right = sameIdentity left right

instance Eq ScreenshotClaim where
  ScreenshotClaim left == ScreenshotClaim right = sameIdentity left right

instance Show ScreenshotTicket where
  show (ScreenshotTicket identity) =
    "ScreenshotTicket " <> show (requestToken identity)

instance Show ScreenshotClaim where
  show (ScreenshotClaim identity) =
    "ScreenshotClaim " <> show (requestToken identity)

newScreenshotRequestRef :: IO ScreenshotRequestRef
newScreenshotRequestRef = atomically $ ScreenshotRequestRef <$>
  newTVar BrokerState
    { brokerClosed = False
    , brokerNextToken = 0
    , brokerActive = Nothing
    }

-- | Atomically install one queued request, rejecting rather than overwriting an
-- existing queued or claimed request.
submitScreenshotRequest
  :: ScreenshotRequestRef
  -> IO (Either ScreenshotSubmitError ScreenshotTicket)
submitScreenshotRequest (ScreenshotRequestRef stateVar) = atomically $ do
  state@BrokerState{brokerClosed, brokerNextToken, brokerActive} <- readTVar stateVar
  if brokerClosed
    then pure (Left ScreenshotClosed)
    else case brokerActive of
      Just _ -> pure (Left ScreenshotBusy)
      Nothing -> do
        resultVar <- newEmptyTMVar
        let identity = RequestIdentity brokerNextToken resultVar
            active = ActiveRequest identity RequestQueued
        writeTVar stateVar state
          { brokerNextToken = brokerNextToken + 1
          , brokerActive = Just active
          }
        pure (Right (ScreenshotTicket identity))

-- | Submit and wait without exposing an asynchronous-exception gap between
-- installation and cancellation cleanup. The deadline action is injectable
-- for deterministic tests.
submitAndWaitScreenshot
  :: ScreenshotRequestRef
  -> IO ()
  -> IO (Either ScreenshotSubmitError ScreenshotResult)
submitAndWaitScreenshot broker deadline = mask $ \restore -> do
  submitted <- submitScreenshotRequest broker
  case submitted of
    Left err -> pure (Left err)
    Right ticket -> do
      result <- restore (waitScreenshotRequest broker ticket deadline)
        `onException` void (cancelScreenshotRequest broker ticket)
      pure (Right result)

-- | Wait while a separate deadline worker races the render-side terminal
-- transition. The worker expires only this identity and is stopped after any
-- other terminal winner. Async cancellation releases queued or claimed
-- ownership before propagating the exception.
waitScreenshotRequest
  :: ScreenshotRequestRef
  -> ScreenshotTicket
  -> IO ()
  -> IO ScreenshotResult
waitScreenshotRequest broker ticket@(ScreenshotTicket identity) deadline =
  mask $ \restore -> do
    deadlineThread <- forkIOWithUnmask $ \unmask -> do
      deadlineOutcome <- try (unmask deadline)
      case deadlineOutcome of
        Right () -> void (expireScreenshotRequest broker ticket)
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> pure ()
          Nothing -> void (expireScreenshotRequest broker ticket)
    let cleanup = do
          void (cancelScreenshotRequest broker ticket)
          killThread deadlineThread
    result <- restore (atomically (readTMVar (requestResult identity)))
      `onException` cleanup
    killThread deadlineThread
    pure result

-- | Atomically move queued work to render-claimed. This makes pending false
-- before any SDL work starts.
claimScreenshotRequest :: ScreenshotRequestRef -> IO (Maybe ScreenshotClaim)
claimScreenshotRequest broker@(ScreenshotRequestRef stateVar) = atomically $ do
  BrokerState{brokerActive} <- readTVar stateVar
  case brokerActive of
    Just ActiveRequest{activeIdentity} -> claimIdentity broker activeIdentity
    Nothing -> pure Nothing

-- | Identity-specific claim hook used to prove that a stale queued operation
-- cannot claim newer work.
claimScreenshotTicket
  :: ScreenshotRequestRef
  -> ScreenshotTicket
  -> IO (Maybe ScreenshotClaim)
claimScreenshotTicket broker (ScreenshotTicket identity) =
  atomically (claimIdentity broker identity)

-- | Nonblocking, at-most-once render delivery. Only the currently claimed
-- identity can win; stale or duplicate delivery cannot affect newer work.
deliverScreenshotRequest
  :: ScreenshotRequestRef
  -> ScreenshotClaim
  -> Either ScreenshotResultError ByteString
  -> IO ScreenshotDelivery
deliverScreenshotRequest broker (ScreenshotClaim identity) result = atomically $ do
  won <- terminalizeClaim broker identity result
  pure (if won then ScreenshotDelivered else ScreenshotStale)

-- | Run one claimed capture with exception-safe terminalization. Synchronous
-- exceptions become the fixed internal result; asynchronous exceptions first
-- unblock the matching waiter and are then rethrown.
runScreenshotCapture
  :: ScreenshotRequestRef
  -> ScreenshotClaim
  -> IO (Either ScreenshotResultError ByteString)
  -> IO ScreenshotDelivery
runScreenshotCapture broker claim capture = mask $ \restore ->
  catchCapture
    (restore capture >>= finishScreenshotCapture broker claim)
    (finishScreenshotException broker claim)

-- | Atomically claim queued work under the same mask that owns capture
-- terminalization. This closes the async-exception gap between claim and the
-- protected render lifecycle.
claimAndRunScreenshotCapture
  :: ScreenshotRequestRef
  -> IO (Either ScreenshotResultError ByteString)
  -> IO (Maybe ScreenshotDelivery)
claimAndRunScreenshotCapture broker capture = mask $ \restore -> do
  claimed <- claimScreenshotRequest broker
  case claimed of
    Nothing -> pure Nothing
    Just claim -> Just <$> catchCapture
      (restore capture >>= finishScreenshotCapture broker claim)
      (finishScreenshotException broker claim)

-- | Let an injected deadline attempt the terminal transition directly.
expireScreenshotRequest :: ScreenshotRequestRef -> ScreenshotTicket -> IO Bool
expireScreenshotRequest broker (ScreenshotTicket identity) =
  atomically (terminalize broker identity (Left ScreenshotUnavailable))

-- | Cancel only the matching queued or claimed request.
cancelScreenshotRequest :: ScreenshotRequestRef -> ScreenshotTicket -> IO Bool
cancelScreenshotRequest broker (ScreenshotTicket identity) =
  atomically (terminalize broker identity (Left ScreenshotUnavailable))

-- | Close permanently, unblock current work, and reject future submissions.
shutdownScreenshotRequestRef :: ScreenshotRequestRef -> IO ()
shutdownScreenshotRequestRef (ScreenshotRequestRef stateVar) = atomically $ do
  state@BrokerState{brokerClosed, brokerActive} <- readTVar stateVar
  if brokerClosed
    then pure ()
    else do
      case brokerActive of
        Nothing -> pure ()
        Just ActiveRequest{activeIdentity = RequestIdentity{requestResult}} ->
          void (tryPutTMVar requestResult (Left ScreenshotUnavailable))
      writeTVar stateVar state { brokerClosed = True, brokerActive = Nothing }

-- | True only while work is queued, never after a render claim.
screenshotRequestPending :: ScreenshotRequestRef -> IO Bool
screenshotRequestPending (ScreenshotRequestRef stateVar) = atomically $ do
  BrokerState{brokerActive} <- readTVar stateVar
  pure $ case brokerActive of
    Just ActiveRequest{activePhase = RequestQueued} -> True
    _ -> False

-- | Whether queued or claimed work owns the single capture slot.
screenshotRequestActive :: ScreenshotRequestRef -> IO Bool
screenshotRequestActive (ScreenshotRequestRef stateVar) = atomically $ do
  BrokerState{brokerActive} <- readTVar stateVar
  pure $ case brokerActive of
    Just _ -> True
    Nothing -> False

-- | Production deadline action. Tests inject async-interruptible latch actions
-- instead of sleeping; a synchronous injected failure expires the ticket.
deadlineAfterMicros :: Int -> IO ()
deadlineAfterMicros = threadDelay

claimIdentity
  :: ScreenshotRequestRef
  -> RequestIdentity
  -> STM (Maybe ScreenshotClaim)
claimIdentity (ScreenshotRequestRef stateVar) identity = do
  state@BrokerState{brokerActive} <- readTVar stateVar
  case brokerActive of
    Just active@ActiveRequest{activeIdentity, activePhase = RequestQueued}
      | sameIdentity identity activeIdentity -> do
          writeTVar stateVar state
            { brokerActive = Just active { activePhase = RequestClaimed } }
          pure (Just (ScreenshotClaim activeIdentity))
    _ -> pure Nothing

finishScreenshotCapture
  :: ScreenshotRequestRef
  -> ScreenshotClaim
  -> Either ScreenshotResultError ByteString
  -> IO ScreenshotDelivery
finishScreenshotCapture = deliverScreenshotRequest

finishScreenshotException
  :: ScreenshotRequestRef
  -> ScreenshotClaim
  -> SomeException
  -> IO ScreenshotDelivery
finishScreenshotException broker claim err = do
  delivered <- deliverScreenshotRequest broker claim (Left ScreenshotInternalError)
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> throwIO err
    Nothing -> pure delivered

catchCapture
  :: IO ScreenshotDelivery
  -> (SomeException -> IO ScreenshotDelivery)
  -> IO ScreenshotDelivery
catchCapture action handler = do
  outcome <- try action
  either handler pure outcome

terminalize
  :: ScreenshotRequestRef
  -> RequestIdentity
  -> ScreenshotResult
  -> STM Bool
terminalize (ScreenshotRequestRef stateVar) identity result = do
  state@BrokerState{brokerActive} <- readTVar stateVar
  case brokerActive of
    Just ActiveRequest{activeIdentity}
      | sameIdentity identity activeIdentity -> do
          writeTVar stateVar state { brokerActive = Nothing }
          void (tryPutTMVar (requestResult identity) result)
          pure True
    _ -> pure False

terminalizeClaim
  :: ScreenshotRequestRef
  -> RequestIdentity
  -> ScreenshotResult
  -> STM Bool
terminalizeClaim (ScreenshotRequestRef stateVar) identity result = do
  state@BrokerState{brokerActive} <- readTVar stateVar
  case brokerActive of
    Just ActiveRequest{activeIdentity, activePhase = RequestClaimed}
      | sameIdentity identity activeIdentity -> do
          writeTVar stateVar state { brokerActive = Nothing }
          void (tryPutTMVar (requestResult identity) result)
          pure True
    _ -> pure False

sameIdentity :: RequestIdentity -> RequestIdentity -> Bool
sameIdentity left right =
  requestToken left == requestToken right
    && requestResult left == requestResult right
