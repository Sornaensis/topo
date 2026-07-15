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
  , readUiSnapshotRef
  , setUiSnapshotRef
  )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryReadMVar
  )
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorHandle, Protocol, get, newActorSystem, shutdownActorSystem)
import Seer.DataBrowser.AppService (DataBrowserRunService)
import Seer.DataBrowser.Executor
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction(..))
import qualified Seer.DataBrowser.Lifecycle as Lifecycle
import Seer.DataBrowser.Model
import Seer.Input.Widgets (submitDataBrowserInputAction)
import Seer.Service.Types (ServiceResponse(..), ServiceResult)
import System.Timeout (timeout)
import Test.Hspec
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , allOperations
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..))

spec :: Spec
spec = describe "DataBrowser asynchronous executor" $ do
  it "publishes begin before gated IO completes and returns responsively" $
    withExecutor $ \uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      envelope <- acceptedEnvelope begun
      expectWithin "catalog worker start" (takeMVar started)
      uiPending <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiPending) `shouldBe` Just envelope
      dbsLoading (uiDataBrowser uiPending) `shouldBe` True
      -- Submission already returned while the worker remains gated.
      tryReadMVar release `shouldReturn` Nothing
      putMVar release ()
      expectWithin "catalog worker idle" (waitDataBrowserExecutorIdle executor)
      uiDone <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiDone) `shouldBe` Nothing
      dbsLoading (uiDataBrowser uiDone) `shouldBe` False

  it "publishes the pending target before atomically publishing resources and records" $
    bracket newActorSystem shutdownActorSystem $ \system -> do
      uiHandle <- get @Ui system
      snapshotRef <- newUiSnapshotRef
      setUiSnapshotRef uiHandle snapshotRef
      _ <- getUiSnapshot uiHandle
      bracket (newDataBrowserExecutor uiHandle) shutdownDataBrowserExecutor $ \executor -> do
        resourcesStarted <- newEmptyMVar
        resourcesRelease <- newEmptyMVar
        recordsStarted <- newEmptyMVar
        recordsRelease <- newEmptyMVar
        begun <- submitDataBrowserAction executor
          (gatedResourceService resourcesStarted resourcesRelease recordsStarted recordsRelease)
          (Lifecycle.DataBrowserSelectResource "atlas" "cities")
        envelope <- acceptedEnvelope begun
        expectWithin "resource list start" (takeMVar resourcesStarted)
        pending <- readUiSnapshotRef snapshotRef
        dbsPendingRequest (uiDataBrowser pending) `shouldBe` Just envelope
        dbpeOperation envelope `shouldBe` DataBrowserSelectResourceOperation
        dbpeRequest envelope `shouldBe` DataBrowserSelectResourceRequest "atlas" "cities"
        dbsLoading (uiDataBrowser pending) `shouldBe` True
        uiDataResources pending `shouldBe` Map.empty
        dbsRecords (uiDataBrowser pending) `shouldBe` []
        putMVar resourcesRelease ()
        expectWithin "record list start" (takeMVar recordsStarted)
        betweenCalls <- readUiSnapshotRef snapshotRef
        uiDataResources betweenCalls `shouldBe` Map.empty
        dbsRecords (uiDataBrowser betweenCalls) `shouldBe` []
        dbsPendingRequest (uiDataBrowser betweenCalls) `shouldBe` Just envelope
        dbsLoading (uiDataBrowser betweenCalls) `shouldBe` True
        putMVar recordsRelease ()
        expectWithin "resource worker idle" (waitDataBrowserExecutorIdle executor)
        completed <- readUiSnapshotRef snapshotRef
        uiDataResources completed `shouldBe` Map.singleton "atlas" [citiesSchema]
        dbsRecords (uiDataBrowser completed) `shouldBe` [cityRecord]
        dbsTotalCount (uiDataBrowser completed) `shouldBe` Just 1
        dbsPendingRequest (uiDataBrowser completed) `shouldBe` Nothing
        dbsLoading (uiDataBrowser completed) `shouldBe` False

  it "allocates unique IDs for identical accepted replacements" $
    withExecutor $ \_uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      let service = gatedPluginService started release
      first <- submitDataBrowserAction executor service (Lifecycle.DataBrowserSelectPlugin "atlas")
      firstEnvelope <- acceptedEnvelope first
      expectWithin "first replacement worker start" (takeMVar started)
      second <- expectWithin "replacement submission" $
        submitDataBrowserAction executor service (Lifecycle.DataBrowserSelectPlugin "atlas")
      secondEnvelope <- acceptedEnvelope second
      dbpeRequestId secondEnvelope `shouldNotBe` dbpeRequestId firstEnvelope
      expectWithin "second replacement worker start" (takeMVar started)
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
      expectWithin "accepted begin commit" (takeMVar committed)
      killThread caller
      putMVar releaseReply ()
      expectWithin "cancelled caller reconciliation" $
        waitDataBrowserExecutorIdle executor
      uiDone <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser uiDone) `shouldBe` Nothing
      dbsLoading (uiDataBrowser uiDone) `shouldBe` False
      expectWithin "cancelled caller executor shutdown" (shutdownDataBrowserExecutor executor)

  it "returns from the SDL Data Browser submission boundary while IO is gated" $
    withExecutor $ \uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      returned <- newEmptyMVar
      _ <- forkIO $ submitDataBrowserInputAction
        executor
        (gatedCatalogService started release)
        DataBrowserLoadPlugins
        >> putMVar returned ()
      expectWithin "SDL input submission return" (takeMVar returned)
      expectWithin "SDL input worker start" (takeMVar started)
      tryReadMVar release `shouldReturn` Nothing
      pending <- getUiSnapshot uiHandle
      dbsPendingRequest (uiDataBrowser pending) `shouldSatisfy` maybe False (const True)
      dbsLoading (uiDataBrowser pending) `shouldBe` True
      putMVar release ()
      expectWithin "SDL input worker idle" (waitDataBrowserExecutorIdle executor)

  it "lets every idle waiter observe the same completion signal" $
    withExecutor $ \_uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      expectWithin "idle-waiter worker start" (takeMVar started)
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

  it "reconciles synchronous worker exceptions with the matching scoped failure" $
    withExecutor $ \uiHandle executor -> do
      begun <- submitDataBrowserAction executor throwingService DataBrowserLoadPlugins
      envelope <- acceptedEnvelope begun
      expectWithin "exceptional worker idle" (waitDataBrowserExecutorIdle executor)
      uiDone <- getUiSnapshot uiHandle
      let browser = uiDataBrowser uiDone
      dbsPendingRequest browser `shouldBe` Nothing
      dbsLoading browser `shouldBe` False
      fmap dbaeRequestId (dbsAsyncError browser) `shouldBe` Just (dbpeRequestId envelope)
      fmap dbaeOperation (dbsAsyncError browser) `shouldBe` Just (dbpeOperation envelope)
      fmap dbaeRequest (dbsAsyncError browser) `shouldBe` Just (dbpeRequest envelope)
      fmap dbaeMessage (dbsAsyncError browser) `shouldSatisfy`
        maybe False ("worker exploded" `Text.isInfixOf`)

  it "does not cancel a mutation for rejected navigation, but shutdown does" $
    bracket newActorSystem shutdownActorSystem $ \system -> do
      uiHandle <- get @Ui system
      firstBegin <- newMVar True
      started <- newEmptyMVar
      release <- newEmptyMVar
      finished <- newEmptyMVar
      let mutationRequest = DataBrowserRecordRequest
            (DataBrowserCreateRecordRequest "atlas" "cities" (DataRecord Map.empty))
          mutationEnvelope = DataBrowserPendingEnvelope
            (DataBrowserRequestId 300) DataBrowserCreateOperation mutationRequest
          begin _ = modifyMVar firstBegin $ \isFirst -> pure $ if isFirst
            then (False, DataBrowserBeginAccepted mutationEnvelope Nothing)
            else (False, DataBrowserBeginRejected "data browser mutation in progress")
          blockedService _ _ =
            (putMVar started () >> readMVar release >> pure (Right (ServiceResponse (object []))))
              `finally` putMVar finished ()
          acquire = do
            executor <- newDataBrowserExecutorWithLifecycle uiHandle begin (const (pure True))
            closed <- newMVar False
            let shutdownOnce = modifyMVar closed $ \alreadyClosed ->
                  if alreadyClosed
                    then pure (True, ())
                    else do
                      shutdownDataBrowserExecutor executor
                      pure (True, ())
            pure (executor, shutdownOnce)
      bracket acquire snd $ \(executor, shutdownOnce) -> do
        acceptedResult <- submitDataBrowserAction executor blockedService Lifecycle.DataBrowserCreateRecord
        acceptedResult `shouldBe` DataBrowserBeginAccepted mutationEnvelope Nothing
        expectWithin "mutation worker start" (takeMVar started)
        rejected <- submitDataBrowserAction executor blockedService
          (Lifecycle.DataBrowserSelectPlugin "other")
        rejected `shouldBe` DataBrowserBeginRejected "data browser mutation in progress"
        tryReadMVar finished `shouldReturn` Nothing
        expectWithin "mutation shutdown" shutdownOnce
        tryReadMVar finished `shouldReturn` Just ()

  it "cancels workers and clears pending state during shutdown" $
    withExecutorNoShutdown $ \uiHandle executor -> do
      started <- newEmptyMVar
      release <- newEmptyMVar
      begun <- submitDataBrowserAction executor (gatedCatalogService started release) DataBrowserLoadPlugins
      _ <- acceptedEnvelope begun
      expectWithin "shutdown worker start" (takeMVar started)
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
  action ui executor `finally`
    expectWithin "executor cleanup" (shutdownDataBrowserExecutor executor)

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

gatedResourceService :: MVar () -> MVar () -> MVar () -> MVar () -> DataBrowserRunService
gatedResourceService resourcesStarted resourcesRelease recordsStarted recordsRelease method params = case method of
  "data_list_resources" -> do
    putMVar resourcesStarted ()
    readMVar resourcesRelease
    pure $ Right $ ServiceResponse $ object
      [ "plugin" .= ("atlas" :: Text)
      , "resources" .= [citiesSchemaValue]
      , "external_data_sources" .= ([] :: [Value])
      , "external_data_source_count" .= (0 :: Int)
      , "external_data_source_failures" .= (0 :: Int)
      ]
  "data_list_records" -> do
    putMVar recordsStarted ()
    readMVar recordsRelease
    pure $ Right $ ServiceResponse $ object
      [ "plugin" .= ("atlas" :: Text)
      , "resource" .= ("cities" :: Text)
      , "records" .= [cityRecord]
      , "total_count" .= Just (1 :: Int)
      , "count" .= (1 :: Int)
      ]
  _ -> pure (Right (ServiceResponse params))

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

cityRecord :: DataRecord
cityRecord = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("name", String "Alpha")
  ]

citiesSchema :: DataResourceSchema
citiesSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "cities"
  , drsLabel = "Cities"
  , drsHexBound = False
  , drsFields =
      [ DataFieldDef "id" DFInt "ID" False Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]
  , drsOperations = allOperations { doPage = True }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = DataPagination 10 25 0
  }

citiesSchemaValue :: Value
citiesSchemaValue = object
  [ "schema_version" .= drsSchemaVersion citiesSchema
  , "resource_version" .= drsResourceVersion citiesSchema
  , "name" .= drsName citiesSchema
  , "label" .= drsLabel citiesSchema
  , "hex_bound" .= drsHexBound citiesSchema
  , "key_field" .= drsKeyField citiesSchema
  , "overlay" .= drsOverlay citiesSchema
  , "fields" .= drsFields citiesSchema
  , "operations" .= object
      [ "list" .= doList (drsOperations citiesSchema)
      , "get" .= doGet (drsOperations citiesSchema)
      , "create" .= doCreate (drsOperations citiesSchema)
      , "update" .= doUpdate (drsOperations citiesSchema)
      , "delete" .= doDelete (drsOperations citiesSchema)
      , "query_by_hex" .= doQueryByHex (drsOperations citiesSchema)
      , "query_by_field" .= doQueryByField (drsOperations citiesSchema)
      , "sort" .= doSort (drsOperations citiesSchema)
      , "filter" .= doFilter (drsOperations citiesSchema)
      , "page" .= doPage (drsOperations citiesSchema)
      ]
  , "pagination" .= object
      [ "default_page_size" .= dpDefaultPageSize (drsPagination citiesSchema)
      , "max_page_size" .= dpMaxPageSize (drsPagination citiesSchema)
      , "default_page_offset" .= dpDefaultPageOffset (drsPagination citiesSchema)
      ]
  ]
