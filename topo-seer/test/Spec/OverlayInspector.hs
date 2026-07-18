{-# LANGUAGE OverloadedStrings #-}

module Spec.OverlayInspector (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value(..), object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import Seer.OverlayInspector.AppService
import Seer.OverlayInspector.Executor
import Seer.OverlayInspector.Model
import Seer.Service.Types (ServiceError(..))
import Test.Hspec

spec :: Spec
spec = describe "OverlayInspector" $ do
  describe "pure model" $ do
    it "represents an empty manager result with no selected overlay" $ do
      let payload = object
            [ "overlay_names" .= ([] :: [String])
            , "active_overlay" .= Null
            ]
          (pendingModel, begun) = beginOverlayInspectorAction
            (OverlayInspectorRequestId 1)
            OverlayInspectorRefreshManager
            emptyOverlayInspectorModel
          pending = acceptedPending begun
          (completed, applied) = completeOverlayInspectorRequest
            (successCompletion pending payload)
            pendingModel
      applied `shouldBe` True
      oimOverlayNames completed `shouldBe` []
      oimSelectedOverlay completed `shouldBe` Nothing
      oimManagerPayload completed `shouldBe` Just payload
      overlayInspectorLoading completed `shouldBe` False

    it "falls back to the first overlay when there is no active overlay" $ do
      let completed = completeManager emptyOverlayInspectorModel $ object
            [ "overlay_names" .= (["roads", "climate"] :: [String])
            , "active_overlay" .= Null
            ]
      oimSelectedOverlay completed `shouldBe` Just "roads"

    it "selects the active overlay when it is present" $ do
      let completed = completeManager emptyOverlayInspectorModel $ object
            [ "overlay_names" .= (["roads", "climate"] :: [String])
            , "active_overlay" .= ("climate" :: String)
            ]
      oimSelectedOverlay completed `shouldBe` Just "climate"

    it "follows a changed active overlay on manager refresh" $ do
      let selected = emptyOverlayInspectorModel
            { oimOverlayNames = ["roads", "climate"]
            , oimSelectedOverlay = Just "roads"
            , oimSchemaPayload = Just (object ["overlay" .= ("roads" :: String)])
            }
          completed = completeManager selected $ object
            [ "overlay_names" .= (["roads", "climate"] :: [String])
            , "active_overlay" .= ("climate" :: String)
            ]
      oimSelectedOverlay completed `shouldBe` Just "climate"
      oimSchemaPayload completed `shouldBe` Nothing

    it "reconciles stale selection and clears target-scoped payloads" $ do
      let stale = emptyOverlayInspectorModel
            { oimOverlayNames = ["old"]
            , oimSelectedOverlay = Just "old"
            , oimSchemaPayload = Just (object ["old" .= True])
            , oimProvenancePayload = Just (object ["old" .= True])
            , oimExportPayload = Just (object ["old" .= True])
            }
          completed = completeManager stale $ object
            [ "overlay_names" .= (["new"] :: [String])
            , "active_overlay" .= ("new" :: String)
            ]
      oimSelectedOverlay completed `shouldBe` Just "new"
      oimSchemaPayload completed `shouldBe` Nothing
      oimProvenancePayload completed `shouldBe` Nothing
      oimExportPayload completed `shouldBe` Nothing

    it "ignores a completion after the selected overlay changes" $ do
      let selected = emptyOverlayInspectorModel
            { oimOverlayNames = ["a", "b"]
            , oimSelectedOverlay = Just "a"
            }
          (pendingModel, begun) = beginOverlayInspectorAction
            (OverlayInspectorRequestId 9)
            OverlayInspectorInspectSchema
            selected
          pending = acceptedPending begun
          changed = pendingModel { oimSelectedOverlay = Just "b" }
          (after, applied) = completeOverlayInspectorRequest
            (successCompletion pending (object ["overlay" .= ("a" :: String)]))
            changed
      applied `shouldBe` False
      after `shouldBe` changed

    it "ignores import validation for a replaced draft" $ do
      let draftA = object ["schema" .= ("a" :: String)]
          draftB = object ["schema" .= ("b" :: String)]
          initial = setOverlayInspectorImportDraft draftA emptyOverlayInspectorModel
          (pendingModel, begun) = beginOverlayInspectorAction
            (OverlayInspectorRequestId 10)
            OverlayInspectorValidateDraft
            initial
          pending = acceptedPending begun
          changed = pendingModel { oimImportDraft = draftB }
          (after, applied) = completeOverlayInspectorRequest
            (successCompletion pending (object ["valid" .= True]))
            changed
      applied `shouldBe` False
      after `shouldBe` changed

    it "retains exact schema and validation payloads" $ do
      let schemaPayload = object
            [ "overlay" .= ("roads" :: String)
            , "schema" .= object ["version" .= (3 :: Int)]
            , "diagnostics" .= [object ["code" .= ("schema_loaded" :: String)]]
            ]
          selected = emptyOverlayInspectorModel
            { oimOverlayNames = ["roads"]
            , oimSelectedOverlay = Just "roads"
            }
          schemaModel = completeAction selected OverlayInspectorInspectSchema schemaPayload
          validationPayload = object
            [ "valid" .= False
            , "diagnostics" .= [object ["code" .= ("invalid_schema" :: String)]]
            ]
          validationModel = completeAction schemaModel OverlayInspectorValidateDraft validationPayload
      oimSchemaPayload schemaModel `shouldBe` Just schemaPayload
      oimImportValidation validationModel `shouldBe` Just validationPayload
      oimValidationDiagnostics validationModel
        `shouldBe` [object ["code" .= ("invalid_schema" :: String)]]

  describe "AppService interpreter" $ do
    it "uses canonical method and payload shapes" $ do
      assertRequest OverlayInspectorManagerRequest "get_overlays" Null
      assertRequest (OverlayInspectorSchemaRequest "roads")
        "get_overlay_schema" (object ["overlay" .= ("roads" :: String)])
      assertRequest (OverlayInspectorProvenanceRequest "roads")
        "get_overlay_provenance" (object ["overlay" .= ("roads" :: String)])
      assertRequest (OverlayInspectorExportRequest "roads")
        "export_overlay_data" (object ["overlay" .= ("roads" :: String)])
      let draft = object ["schema" .= object [], "payload" .= object []]
      assertRequest (OverlayInspectorImportValidationRequest draft)
        "validate_overlay_import" draft

    it "retains structured AppService errors" $ do
      outcome <- performOverlayInspectorRequest
        (\_ _ -> pure (Left (overlayInspectorServiceErrorValue
          (ServiceNotFound "overlay not found: roads"))))
        (OverlayInspectorSchemaRequest "roads")
      outcome `shouldBe` OverlayInspectorWorkerFailed (object
        [ "code" .= ("not_found" :: String)
        , "message" .= ("overlay not found: roads" :: String)
        , "details" .= ([] :: [Value])
        , "http_status" .= (404 :: Int)
        ])

  describe "executor" $ do
    it "returns accepted before the injected service finishes" $ do
      started <- newEmptyMVar
      release <- newEmptyMVar
      completed <- newEmptyMVar
      nextId <- newIORef (1 :: Integer)
      executor <- newOverlayInspectorExecutorWithLifecycle
        (\action -> do
          current <- readIORef nextId
          writeIORef nextId (current + 1)
          let request = case action of
                OverlayInspectorRefreshManager -> OverlayInspectorManagerRequest
                _ -> OverlayInspectorManagerRequest
          pure (OverlayInspectorBeginAccepted OverlayInspectorPending
            { oipRequestId = OverlayInspectorRequestId (fromIntegral current)
            , oipOperation = OverlayInspectorLoadManager
            , oipRequest = request
            }))
        (\completion -> putMVar completed completion >> pure True)
      let gatedService method params = do
            method `shouldBe` "get_overlays"
            params `shouldBe` Null
            putMVar started ()
            takeMVar release
            pure (Right (object ["overlay_names" .= ([] :: [String])]))
      begun <- submitOverlayInspectorAction executor gatedService OverlayInspectorRefreshManager
      begun `shouldSatisfy` isAccepted
      takeMVar started
      putMVar release ()
      completion <- takeMVar completed
      oicOutcome completion `shouldBe` OverlayInspectorWorkerSucceeded
        (object ["overlay_names" .= ([] :: [String])])
      waitOverlayInspectorExecutorIdle executor
      shutdownOverlayInspectorExecutor executor

acceptedPending :: OverlayInspectorBeginResult -> OverlayInspectorPending
acceptedPending result = case result of
  OverlayInspectorBeginAccepted pending -> pending
  OverlayInspectorBeginRejected message -> error (show message)

isAccepted :: OverlayInspectorBeginResult -> Bool
isAccepted OverlayInspectorBeginAccepted{} = True
isAccepted _ = False

successCompletion :: OverlayInspectorPending -> Value -> OverlayInspectorCompletion
successCompletion pending payload = OverlayInspectorCompletion
  { oicRequestId = oipRequestId pending
  , oicRequest = oipRequest pending
  , oicOutcome = OverlayInspectorWorkerSucceeded payload
  }

completeManager :: OverlayInspectorModel -> Value -> OverlayInspectorModel
completeManager model payload = completeAction model OverlayInspectorRefreshManager payload

completeAction :: OverlayInspectorModel -> OverlayInspectorAction -> Value -> OverlayInspectorModel
completeAction model action payload =
  let requestId = OverlayInspectorRequestId 1
      (pendingModel, begun) = beginOverlayInspectorAction requestId action model
      pending = acceptedPending begun
      (completed, applied) = completeOverlayInspectorRequest
        (successCompletion pending payload)
        pendingModel
  in if applied then completed else error "expected overlay inspector completion"

assertRequest :: OverlayInspectorRequest -> String -> Value -> Expectation
assertRequest request expectedMethod expectedParams = do
  let payload = object ["sentinel" .= True]
      runService method params = do
        show method `shouldBe` show expectedMethod
        params `shouldBe` expectedParams
        pure (Right payload)
  outcome <- performOverlayInspectorRequest runService request
  outcome `shouldBe` OverlayInspectorWorkerSucceeded payload
