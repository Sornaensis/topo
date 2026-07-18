{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Application-owned workers for non-blocking overlay inspector service IO.
module Seer.OverlayInspector.Executor
  ( OverlayInspectorExecutor
  , newOverlayInspectorExecutor
  , newOverlayInspectorExecutorWithLifecycle
  , submitOverlayInspectorAction
  , shutdownOverlayInspectorExecutor
  , waitOverlayInspectorExecutorIdle
  ) where

import Actor.Log (getLogSnapshot)
import Actor.SnapshotReceiver (publishChangedUiAndLog)
import Actor.UI.State
  ( beginUiOverlayInspectorAction
  , completeUiOverlayInspectorRequest
  , getUiSnapshot
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Control.Concurrent
  ( MVar
  , forkIOWithUnmask
  , killThread
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , finally
  , fromException
  , mask
  , try
  )
import Control.Monad (unless, void, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Seer.OverlayInspector.AppService
  ( OverlayInspectorRunService
  , performOverlayInspectorRequest
  )
import Seer.OverlayInspector.Executor.Types
import Seer.OverlayInspector.Model

newOverlayInspectorExecutor :: ActorHandles -> IO OverlayInspectorExecutor
newOverlayInspectorExecutor handles =
  newOverlayInspectorExecutorWithLifecycle
    (beginUiOverlayInspectorAction (ahUiHandle handles))
    completeAndPublish
  where
    completeAndPublish completion = do
      uiBefore <- getUiSnapshot (ahUiHandle handles)
      logBefore <- getLogSnapshot (ahLogHandle handles)
      applied <- completeUiOverlayInspectorRequest (ahUiHandle handles) completion
      when applied $ publishChangedUiAndLog
        (ahSnapshotVersionRef handles)
        uiBefore
        logBefore
        (getUiSnapshot (ahUiHandle handles))
        (getLogSnapshot (ahLogHandle handles))
      pure applied

newOverlayInspectorExecutorWithLifecycle
  :: (OverlayInspectorAction -> IO OverlayInspectorBeginResult)
  -> (OverlayInspectorCompletion -> IO Bool)
  -> IO OverlayInspectorExecutor
newOverlayInspectorExecutorWithLifecycle beginAction completeRequest = do
  state <- newMVar (OverlayInspectorExecutorState False Map.empty)
  pure OverlayInspectorExecutor
    { oieState = state
    , oieBeginAction = beginAction
    , oieCompleteRequest = completeRequest
    }

-- | Accept and launch one request. The service call always runs after the
-- pending state has been committed and after the executor registry is visible.
submitOverlayInspectorAction
  :: OverlayInspectorExecutor
  -> OverlayInspectorRunService
  -> OverlayInspectorAction
  -> IO OverlayInspectorBeginResult
submitOverlayInspectorAction executor runService action = mask $ \_ -> do
  transition <- modifyMVar (oieState executor) $ \state ->
    if oiesClosing state
      then pure (state, Left (OverlayInspectorBeginRejected "overlay inspector executor is closed"))
      else do
        begun <- oieBeginAction executor action
        case begun of
          rejected@OverlayInspectorBeginRejected{} -> pure (state, Left rejected)
          accepted@(OverlayInspectorBeginAccepted pending) -> do
            gate <- newEmptyMVar
            done <- newEmptyMVar
            launched <- try $ forkIOWithUnmask $ \unmask ->
              (takeGate gate >> workerBody executor runService pending unmask)
                `finally` finalizeWorker executor pending done
            case launched of
              Left err -> pure (state, Right (accepted, Nothing, Just (err :: SomeException)))
              Right threadId ->
                let worker = OverlayInspectorWorker threadId done (oipRequest pending)
                    state' = state
                      { oiesWorkers = Map.insert (oipRequestId pending) worker (oiesWorkers state) }
                in pure (state', Right (accepted, Just gate, Nothing))
  case transition of
    Left rejected -> pure rejected
    Right (accepted, Just gate, Nothing) -> putMVar gate () >> pure accepted
    Right (accepted@(OverlayInspectorBeginAccepted pending), Nothing, Just err) -> do
      ignoreCompletionException executor OverlayInspectorCompletion
        { oicRequestId = oipRequestId pending
        , oicRequest = oipRequest pending
        , oicOutcome = OverlayInspectorWorkerFailed (exceptionErrorValue err)
        }
      pure (OverlayInspectorBeginRejected "failed to launch overlay inspector worker")
    Right (accepted, _, _) -> pure accepted
  where
    takeGate = takeMVar

workerBody
  :: OverlayInspectorExecutor
  -> OverlayInspectorRunService
  -> OverlayInspectorPending
  -> (IO OverlayInspectorWorkerOutcome -> IO OverlayInspectorWorkerOutcome)
  -> IO ()
workerBody executor runService pending unmask = do
  attempted <- try (unmask (performOverlayInspectorRequest runService (oipRequest pending)))
  let outcome = case attempted of
        Right value -> value
        Left err -> OverlayInspectorWorkerFailed (exceptionErrorValue (err :: SomeException))
  void $ oieCompleteRequest executor OverlayInspectorCompletion
    { oicRequestId = oipRequestId pending
    , oicRequest = oipRequest pending
    , oicOutcome = outcome
    }

finalizeWorker
  :: OverlayInspectorExecutor
  -> OverlayInspectorPending
  -> MVar ()
  -> IO ()
finalizeWorker executor pending done = mask $ \_ -> do
  ignoreCompletionException executor OverlayInspectorCompletion
    { oicRequestId = oipRequestId pending
    , oicRequest = oipRequest pending
    , oicOutcome = OverlayInspectorWorkerFailed
        (object
          [ "code" .= ("cancelled" :: Text.Text)
          , "message" .= ("overlay inspector request cancelled" :: Text.Text)
          , "details" .= ([] :: [Text.Text])
          , "http_status" .= (503 :: Int)
          ])
    }
  modifyMVar_ (oieState executor) (\state -> pure state
    { oiesWorkers = Map.delete (oipRequestId pending) (oiesWorkers state) })
    `finally` void (tryPutMVar done ())

ignoreCompletionException :: OverlayInspectorExecutor -> OverlayInspectorCompletion -> IO ()
ignoreCompletionException executor completion = do
  _ <- try (oieCompleteRequest executor completion) :: IO (Either SomeException Bool)
  pure ()

shutdownOverlayInspectorExecutor :: OverlayInspectorExecutor -> IO ()
shutdownOverlayInspectorExecutor executor = mask $ \_ -> do
  workers <- modifyMVar (oieState executor) $ \state ->
    pure (state { oiesClosing = True }, Map.elems (oiesWorkers state))
  mapM_ (killThread . oiwThread) workers
  mapM_ (readMVar . oiwDone) workers

waitOverlayInspectorExecutorIdle :: OverlayInspectorExecutor -> IO ()
waitOverlayInspectorExecutorIdle executor = do
  workers <- Map.elems . oiesWorkers <$> readMVar (oieState executor)
  mapM_ (readMVar . oiwDone) workers
  unless (null workers) (waitOverlayInspectorExecutorIdle executor)

exceptionErrorValue :: SomeException -> Value
exceptionErrorValue err = object
  [ "code" .= code
  , "message" .= Text.pack (displayException err)
  , "details" .= ([] :: [Text.Text])
  , "http_status" .= httpStatus
  ]
  where
    (code, httpStatus) = case fromException err :: Maybe SomeAsyncException of
      Just _ -> (("cancelled" :: Text.Text), 503 :: Int)
      Nothing -> ("internal_error", 500)
