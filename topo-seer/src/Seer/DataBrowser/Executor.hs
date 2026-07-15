{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Application-scoped supervised workers for Data Browser service IO.
module Seer.DataBrowser.Executor
  ( DataBrowserExecutor
  , newDataBrowserExecutor
  , newDataBrowserExecutorWithCompletion
  , newDataBrowserExecutorWithLifecycle
  , submitDataBrowserAction
  , shutdownDataBrowserExecutor
  , waitDataBrowserExecutorIdle
  ) where

import Actor.UI.State
  ( Ui
  , beginUiDataBrowserAction
  , completeUiDataBrowserRequest
  )
import Control.Concurrent
  ( MVar
  , forkIO
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
  , onException
  , throwIO
  , try
  )
import Control.Monad (forM_, unless, void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.DataBrowser.AppService
  ( DataBrowserRunService
  , performDataBrowserWorkerRequest
  )
import Seer.DataBrowser.Executor.Types
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction)
import Seer.DataBrowser.Model

data SubmissionTransition
  = SubmissionComplete !DataBrowserBeginResult
  | SubmissionLaunchFailed !DataBrowserPendingEnvelope !SomeException
  | SubmissionLaunched
      !DataBrowserBeginResult
      !(MVar ())
      !(Maybe Worker)

newDataBrowserExecutor :: ActorHandle Ui (Protocol Ui) -> IO DataBrowserExecutor
newDataBrowserExecutor uiHandle =
  newDataBrowserExecutorWithLifecycle
    uiHandle
    (beginUiDataBrowserAction uiHandle)
    (completeUiDataBrowserRequest uiHandle)

-- | Construct an executor with an injectable completion boundary. This keeps
-- lifecycle-failure tests deterministic without changing production ownership.
newDataBrowserExecutorWithCompletion
  :: ActorHandle Ui (Protocol Ui)
  -> (DataBrowserCompletion -> IO Bool)
  -> IO DataBrowserExecutor
newDataBrowserExecutorWithCompletion uiHandle =
  newDataBrowserExecutorWithLifecycle uiHandle (beginUiDataBrowserAction uiHandle)

-- | Construct an executor with injectable actor lifecycle boundaries.
newDataBrowserExecutorWithLifecycle
  :: ActorHandle Ui (Protocol Ui)
  -> (DataBrowserAppAction -> IO DataBrowserBeginResult)
  -> (DataBrowserCompletion -> IO Bool)
  -> IO DataBrowserExecutor
newDataBrowserExecutorWithLifecycle uiHandle beginAction completeRequest = do
  state <- newMVar (ExecutorState False [] Map.empty)
  pure DataBrowserExecutor
    { dbeUiHandle = uiHandle
    , dbeState = state
    , dbeBeginAction = beginAction
    , dbeCompleteRequest = completeRequest
    }

-- | Register a submission owner before it enters the actor begin/reply window.
-- The owner survives cancellation of the ingress caller, so an accepted UI
-- request is always either registered as a worker or reconciled as failed.
submitDataBrowserAction
  :: DataBrowserExecutor
  -> DataBrowserRunService
  -> DataBrowserAppAction
  -> IO DataBrowserBeginResult
submitDataBrowserAction executor runService action = mask $ \restore -> do
  result <- newEmptyMVar
    :: IO (MVar (Either SomeException DataBrowserBeginResult))
  gate <- newEmptyMVar
  done <- newEmptyMVar
  owner <- forkIO $
    (takeMVar gate >> do
      attempted <- try (submitRegisteredDataBrowserAction executor runService action)
      putMVar result attempted)
      `finally` finalizeSubmission executor done
  accepted <- modifyMVar (dbeState executor) (registerSubmission done)
    `onException` killThread owner
  if not accepted
    then do
      killThread owner
      pure (DataBrowserBeginRejected "data browser executor is closed")
    else do
      putMVar gate ()
      outcome <- restore (readMVar result)
      either throwIO pure outcome
  where
    registerSubmission doneSignal state
      | esClosing state = pure (state, False)
      | otherwise = pure
          ( state { esSubmissions = doneSignal : esSubmissions state }
          , True
          )

finalizeSubmission :: DataBrowserExecutor -> MVar () -> IO ()
finalizeSubmission executor done = mask $ \_ ->
  modifyMVar_ (dbeState executor) (pure . removeSubmission)
    `finally` void (tryPutMVar done ())
  where
    removeSubmission state = state
      { esSubmissions = filter (/= done) (esSubmissions state) }

submitRegisteredDataBrowserAction
  :: DataBrowserExecutor
  -> DataBrowserRunService
  -> DataBrowserAppAction
  -> IO DataBrowserBeginResult
submitRegisteredDataBrowserAction executor runService action = mask $ \_ -> do
  transition <- modifyMVar (dbeState executor) $ \state -> do
    begun <- dbeBeginAction executor action
    case begun of
      DataBrowserBeginAccepted envelope superseded -> do
        gate <- newEmptyMVar
        done <- newEmptyMVar
        launched <- try $ forkIOWithUnmask $ \unmask ->
          (takeMVar gate >> workerBody executor runService envelope unmask)
            `finally` finalizeWorker executor envelope done
        case launched of
          Left err -> pure
            ( state
            , SubmissionLaunchFailed envelope err
            )
          Right threadId -> do
            let worker = Worker threadId done (dbpeRequest envelope)
                oldWorker = superseded
                  >>= (`Map.lookup` esWorkers state)
                  >>= cancelableWorker
                workers' = Map.insert
                  (dbpeRequestId envelope)
                  worker
                  (esWorkers state)
            pure
              ( state { esWorkers = workers' }
              , SubmissionLaunched begun gate oldWorker
              )
      _ -> pure (state, SubmissionComplete begun)

  case transition of
    SubmissionComplete result -> pure result
    SubmissionLaunchFailed envelope err -> do
      ignoreCompletionException executor DataBrowserCompletion
        { dbcRequestId = dbpeRequestId envelope
        , dbcRequest = dbpeRequest envelope
        , dbcOutcome = DataBrowserWorkerFailed
            (Text.pack (displayException err))
        }
      pure (DataBrowserBeginRejected "failed to launch data browser worker")
    SubmissionLaunched result gate supersededWorker -> do
      -- The registry commit above is visible before either worker can race its
      -- finalizer. Cancellation is deliberately outside the state lock.
      putMVar gate ()
      forM_ supersededWorker (killThread . workerThread)
      pure result
  where
    cancelableWorker worker
      | dataBrowserRequestIsMutation (workerRequest worker) = Nothing
      | otherwise = Just worker

workerBody
  :: DataBrowserExecutor
  -> DataBrowserRunService
  -> DataBrowserPendingEnvelope
  -> (IO (Either Text.Text DataBrowserWorkerOutcome) -> IO (Either Text.Text DataBrowserWorkerOutcome))
  -> IO ()
workerBody executor runService envelope unmask = do
  attempted <- try (unmask (performDataBrowserWorkerRequest runService (dbpeRequest envelope)))
  let outcome = case attempted of
        Right (Right value) -> value
        Right (Left message) -> DataBrowserWorkerFailed message
        Left err -> DataBrowserWorkerFailed (exceptionMessage (err :: SomeException))
  void $ dbeCompleteRequest executor DataBrowserCompletion
    { dbcRequestId = dbpeRequestId envelope
    , dbcRequest = dbpeRequest envelope
    , dbcOutcome = outcome
    }

exceptionMessage :: SomeException -> Text.Text
exceptionMessage err = case fromException err :: Maybe SomeAsyncException of
  Just _ -> "data browser request cancelled during shutdown or replacement"
  Nothing -> Text.pack (displayException err)

finalizeWorker :: DataBrowserExecutor -> DataBrowserPendingEnvelope -> MVar () -> IO ()
finalizeWorker executor envelope done = mask $ \_ -> do
  -- Completion is allowed to fail (for example because the actor is already
  -- stopping); registry cleanup and the broadcast-style done signal are not.
  ignoreCompletionException executor DataBrowserCompletion
    { dbcRequestId = dbpeRequestId envelope
    , dbcRequest = dbpeRequest envelope
    , dbcOutcome = DataBrowserWorkerFailed
        "data browser request cancelled during shutdown or replacement"
    }
  modifyMVar_ (dbeState executor) (pure . removeWorker)
    `finally` void (tryPutMVar done ())
  where
    removeWorker state = state
      { esWorkers = Map.delete (dbpeRequestId envelope) (esWorkers state) }

ignoreCompletionException :: DataBrowserExecutor -> DataBrowserCompletion -> IO ()
ignoreCompletionException executor completion = do
  _ <- try (dbeCompleteRequest executor completion) :: IO (Either SomeException Bool)
  pure ()

-- | Close submissions, cancel every application-owned worker, and wait until
-- each has attempted guarded completion while the Ui actor is still alive.
shutdownDataBrowserExecutor :: DataBrowserExecutor -> IO ()
shutdownDataBrowserExecutor executor = mask $ \_ -> do
  submissions <- modifyMVar (dbeState executor) $ \state ->
    pure (state { esClosing = True }, esSubmissions state)
  mapM_ readMVar submissions
  workers <- Map.toList . esWorkers <$> readMVar (dbeState executor)
  mapM_ (killThread . workerThread . snd) workers
  forM_ workers $ \(_, worker) -> readMVar (workerDone worker)
  forM_ workers $ \(requestId, worker) ->
    ignoreCompletionException executor DataBrowserCompletion
      { dbcRequestId = requestId
      , dbcRequest = workerRequest worker
      , dbcOutcome = DataBrowserWorkerFailed "data browser executor shut down"
      }

waitDataBrowserExecutorIdle :: DataBrowserExecutor -> IO ()
waitDataBrowserExecutorIdle executor = do
  state <- readMVar (dbeState executor)
  let submissions = esSubmissions state
      workers = Map.elems (esWorkers state)
  mapM_ readMVar submissions
  mapM_ (readMVar . workerDone) workers
  unless (null submissions && null workers) $
    waitDataBrowserExecutorIdle executor
