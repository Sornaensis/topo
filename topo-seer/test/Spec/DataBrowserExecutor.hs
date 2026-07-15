{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.DataBrowserExecutor (spec) where

import Actor.UI.State
  ( DataBrowserState(..)
  , Ui
  , UiState(..)
  , beginUiDataBrowserAction
  , completeUiDataBrowserRequest
  , getUiSnapshot
  , newUiSnapshotRef
  , setUiSnapshotRef
  )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Hyperspace.Actor (ActorHandle, Protocol, get, newActorSystem, shutdownActorSystem)
import Seer.DataBrowser.AppService (DataBrowserRunService)
import Seer.DataBrowser.Executor
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction(..))
import qualified Seer.DataBrowser.Lifecycle as Lifecycle
import Seer.DataBrowser.Model
import Seer.Service.Types (ServiceResponse(..), ServiceResult)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "DataBrowser asynchronous executor" $ do
  it "publishes begin before gated IO completes and returns responsively" $
    withExecutor $ \uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      envelope <- acceptedEnvelope begun
      takeMVar started
      uiPending <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiPending) `shouldBe` Just envelope
      dbsLoading (uiDataBrowser uiPending) `shouldBe` True
      -- Submission already returned while the worker remains gated.
      timeout 20000 (readMVar release) `shouldReturn` Nothing
      putMVar release ()
      waitDataBrowserExecutorIdle executor
      uiDone <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiDone) `shouldBe` Nothing
      dbsLoading (uiDataBrowser uiDone) `shouldBe` False

  it "allocates unique IDs for identical accepted replacements" $
    withExecutor $ \_uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      let service = gatedPluginService started release
      first <- submitDataBrowserAction executor service (Lifecycle.DataBrowserSelectPlugin "atlas")
      firstEnvelope <- acceptedEnvelope first
      takeMVar started
      second <- expectWithin "replacement submission" $
        submitDataBrowserAction executor service (Lifecycle.DataBrowserSelectPlugin "atlas")
      secondEnvelope <- acceptedEnvelope second
      dbpeRequestId secondEnvelope `shouldNotBe` dbpeRequestId firstEnvelope
      takeMVar started
      putMVar release ()
      expectWithin "replacement workers idle" $
        waitDataBrowserExecutorIdle executor

  it "finishes an accepted begin after its ingress caller is cancelled" $
    bracket newActorSystem shutdownActorSystem $ \system -> do
      uiHandle <- get @Ui system
      snapshotRef <- newUiSnapshotRef
      setUiSnapshotRef uiHandle snapshotRef
      committed <- newEmptyMVar
      releaseReply <- newEmptyMVar
      let delayedBegin action = do
            begun <- beginUiDataBrowserAction uiHandle action
            putMVar committed ()
            readMVar releaseReply
            pure begun
      executor <- newDataBrowserExecutorWithLifecycle
        uiHandle
        delayedBegin
        (completeUiDataBrowserRequest uiHandle)
      caller <- forkIO $ void $
        submitDataBrowserAction executor immediateCatalogService DataBrowserLoadPlugins
      takeMVar committed
      killThread caller
      putMVar releaseReply ()
      expectWithin "cancelled caller reconciliation" $
        waitDataBrowserExecutorIdle executor
      uiDone <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiDone) `shouldBe` Nothing
      dbsLoading (uiDataBrowser uiDone) `shouldBe` False
      shutdownDataBrowserExecutor executor

  it "lets every idle waiter observe the same completion signal" $
    withExecutor $ \_uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      takeMVar started
      firstDone <- forkIdleWaiter executor
      secondDone <- forkIdleWaiter executor
      putMVar release ()
      expectWithin "first idle waiter" (takeMVar firstDone)
      expectWithin "second idle waiter" (takeMVar secondDone)

  it "removes and signals a worker even when every completion call throws" $
    bracket newActorSystem shutdownActorSystem $ \system -> do
      uiHandle <- get @Ui system
      snapshotRef <- newUiSnapshotRef
      setUiSnapshotRef uiHandle snapshotRef
      executor <- newDataBrowserExecutorWithCompletion uiHandle $ \_ ->
        fail "completion actor stopped"
      begun <- submitDataBrowserAction executor immediateCatalogService DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      expectWithin "exceptional finalizer cleanup" $
        waitDataBrowserExecutorIdle executor
      expectWithin "shutdown after exceptional finalizer" $
        shutdownDataBrowserExecutor executor

  it "reconciles synchronous worker exceptions" $
    withExecutor $ \uiHandle executor -> do
      begun <- submitDataBrowserAction executor throwingService DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      waitDataBrowserExecutorIdle executor
      uiDone <- getUiSnapshot uiHandle
      let browser = uiDataBrowser uiDone
      dbsPendingRequest browser `shouldBe` Nothing
      dbsLoading browser `shouldBe` False
      dbsAsyncError browser `shouldSatisfy` maybe False (const True)

  it "cancels workers and clears pending state during shutdown" $
    withExecutorNoShutdown $ \uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      takeMVar started
      expectWithin "executor shutdown" (shutdownDataBrowserExecutor executor)
      uiDone <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiDone) `shouldBe` Nothing
      dbsLoading (uiDataBrowser uiDone) `shouldBe` False
      rejected <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      rejected `shouldBe` DataBrowserBeginRejected "data browser executor is closed"

forkIdleWaiter :: DataBrowserExecutor -> IO (MVar ())
forkIdleWaiter executor = do
  done <- newEmptyMVar
  _ <- forkIO (waitDataBrowserExecutorIdle executor >> putMVar done ())
  pure done

expectWithin :: String -> IO a -> IO a
expectWithin label action = do
  result <- timeout 1000000 action
  case result of
    Just value -> pure value
    Nothing -> expectationFailure (label <> " deadlocked") >> fail "unreachable"

acceptedEnvelope :: DataBrowserBeginResult -> IO DataBrowserPendingEnvelope
acceptedEnvelope result = case result of
  DataBrowserBeginAccepted envelope _ -> pure envelope
  _ -> expectationFailure ("expected accepted request, got " <> show result) >> fail "unreachable"

withExecutor
  :: (ActorHandle Ui (Protocol Ui) -> DataBrowserExecutor -> IO a)
  -> IO a
withExecutor action = withExecutorNoShutdown $ \ui executor ->
  action ui executor `finally` shutdownDataBrowserExecutor executor

withExecutorNoShutdown
  :: (ActorHandle Ui (Protocol Ui) -> DataBrowserExecutor -> IO a)
  -> IO a
withExecutorNoShutdown action = bracket newActorSystem shutdownActorSystem $ \system -> do
  uiHandle <- get @Ui system
  snapshotRef <- newUiSnapshotRef
  setUiSnapshotRef uiHandle snapshotRef
  executor <- newDataBrowserExecutor uiHandle
  action uiHandle executor

gatedCatalogService :: MVar () -> MVar () -> DataBrowserRunService
gatedCatalogService started release method _params = do
  putMVar started ()
  readMVar release
  pure $ Right $ ServiceResponse $ case method of
    "data_list_plugins" -> object ["plugins" .= ([] :: [Value])]
    _ -> object []

gatedPluginService :: MVar () -> MVar () -> DataBrowserRunService
gatedPluginService started release method params = do
  putMVar started ()
  readMVar release
  pure $ Right $ ServiceResponse $ case method of
    "data_list_resources" -> object
      [ "plugin" .= ("atlas" :: Text)
      , "resources" .= ([] :: [Value])
      ]
    _ -> params

immediateCatalogService :: DataBrowserRunService
immediateCatalogService method _params =
  pure $ Right $ ServiceResponse $ case method of
    "data_list_plugins" -> object ["plugins" .= ([] :: [Value])]
    _ -> object []

throwingService :: DataBrowserRunService
throwingService _ _ = fail "worker exploded"
