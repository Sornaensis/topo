{-# LANGUAGE OverloadedStrings #-}

module Spec.OverlayInspector (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value(..), eitherDecodeStrict', object, (.=))
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.IORef (newIORef, readIORef, writeIORef)
import Seer.OverlayInspector.AppService
import Seer.OverlayInspector.Executor
import Seer.OverlayInspector.Model
import Seer.OverlayInspector.Storage
import Seer.Service.Types (ServiceError(..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectory
  , removeFile
  )
import System.FilePath ((</>))
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

    it "opens useful views and operates clamped manager selection" $ do
      let manager = openOverlayInspectorView OverlayInspectorManagerView
            emptyOverlayInspectorModel
              { oimOverlayNames = ["roads", "climate"]
              , oimSelectedOverlay = Just "roads"
              }
          moved = moveOverlayInspectorSelection 1 manager
          clamped = moveOverlayInspectorSelection 10 moved
      oimView manager `shouldBe` Just OverlayInspectorManagerView
      oimFocus manager `shouldBe` OverlayInspectorManagerFocus 0
      oimSelectedOverlay moved `shouldBe` Just "climate"
      oimSelectedOverlay clamped `shouldBe` Just "climate"
      oimScroll (setOverlayInspectorScroll (-3) manager) `shouldBe` 0
      oimView (closeOverlayInspectorView manager) `shouldBe` Nothing
      let stalePayload = object ["old" .= True]
          stale = manager
            { oimManagerPayload = Just stalePayload
            , oimSchemaPayload = Just stalePayload
            , oimProvenancePayload = Just stalePayload
            , oimExportPayload = Just stalePayload
            }
      oimManagerPayload (openOverlayInspectorView OverlayInspectorManagerView stale)
        `shouldBe` Nothing
      oimSchemaPayload (openOverlayInspectorView OverlayInspectorSchemaView stale)
        `shouldBe` Nothing
      oimProvenancePayload (openOverlayInspectorView OverlayInspectorProvenanceView stale)
        `shouldBe` Nothing
      oimExportPayload (openOverlayInspectorView OverlayInspectorExportView stale)
        `shouldBe` Nothing

    it "does not let stale manager rows cancel an in-flight refresh" $ do
      let opened = openOverlayInspectorView OverlayInspectorManagerView
            emptyOverlayInspectorModel
              { oimOverlayNames = ["old-a", "old-b"]
              , oimSelectedOverlay = Just "old-a"
              }
          (pendingModel, begun) = beginOverlayInspectorAction
            (OverlayInspectorRequestId 22)
            OverlayInspectorRefreshManager
            opened
          pending = acceptedPending begun
      moveOverlayInspectorSelection 1 pendingModel `shouldBe` pendingModel
      selectOverlayInspectorOverlay (Just "old-b") pendingModel `shouldBe` pendingModel
      let (completed, applied) = completeOverlayInspectorRequest
            (successCompletion pending (object
              [ "overlay_names" .= (["fresh"] :: [String])
              , "active_overlay" .= ("fresh" :: String)
              ]))
            pendingModel
      applied `shouldBe` True
      oimOverlayNames completed `shouldBe` ["fresh"]
      oimSelectedOverlay completed `shouldBe` Just "fresh"

    it "reconciles manager focus and scroll to the service-selected overlay" $ do
      let names = ["overlay-" <> Text.pack (show index) | index <- [0 :: Int .. 20]]
          opened = openOverlayInspectorView OverlayInspectorManagerView emptyOverlayInspectorModel
          completed = completeManager opened $ object
            [ "overlay_names" .= names
            , "active_overlay" .= ("overlay-20" :: Text.Text)
            ]
      oimSelectedOverlay completed `shouldBe` Just "overlay-20"
      oimFocus completed `shouldBe` OverlayInspectorManagerFocus 20
      oimScroll completed `shouldSatisfy` (> 0)

    it "keeps import entry validation-only and reports local JSON errors" $ do
      let invalid = setOverlayInspectorImportText "{not-json" 9 emptyOverlayInspectorModel
          (invalidAfter, invalidResult) = prepareOverlayInspectorValidation invalid
      invalidResult `shouldSatisfy` either (const True) (const False)
      oimLocalDiagnostics invalidAfter `shouldSatisfy` (not . null)
      oimImportDraft invalidAfter `shouldBe` oimImportDraft emptyOverlayInspectorModel

      let validText = "{\"schema\":{},\"payload\":{\"roads\":[]}}"
          valid = setOverlayInspectorImportText validText 999 invalidAfter
          (validAfter, validResult) = prepareOverlayInspectorValidation valid
      validResult `shouldBe` Right (object
        [ "schema" .= object []
        , "payload" .= object ["roads" .= ([] :: [Value])]
        ])
      oimImportCursor valid `shouldBe` Text.length validText
      oimImportValidation validAfter `shouldBe` Nothing
      oimNotice validAfter `shouldBe`
        Just "JSON parsed; validating only — no data will be adopted."

    it "serializes and saves the exact AppService export payload" $ do
      let payload = object
            [ "overlay" .= ("roads" :: String)
            , "schema" .= object ["version" .= (3 :: Int)]
            , "chunks" .= [object ["id" .= (7 :: Int)]]
            ]
      eitherDecodeStrict' (TextEncoding.encodeUtf8 (overlayInspectorPayloadText payload))
        `shouldBe` Right payload
      temp <- getTemporaryDirectory
      let directory = temp </> "topo-overlay-inspector-spec"
          path = directory </> "roads-main.json"
      createDirectoryIfMissing True directory
      let secondPath = directory </> "roads-main-1.json"
      mapM_ (\candidate -> do
          existing <- doesFileExist candidate
          if existing then removeFile candidate else pure ()) [path, secondPath]
      saved <- saveOverlayExportUnder directory "roads/main" payload
      savedPath <- expectRight saved
      bytes <- ByteString.readFile savedPath
      eitherDecodeStrict' bytes `shouldBe` Right payload
      second <- saveOverlayExportUnder directory "roads/main" payload
      savedAgain <- expectRight second
      savedAgain `shouldNotBe` savedPath
      ByteString.readFile savedAgain >>= (`shouldSatisfy` (not . ByteString.null))
      removeFile savedPath
      removeFile savedAgain
      removeDirectory directory

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

expectRight :: Show err => Either err value -> IO value
expectRight result = case result of
  Right value -> pure value
  Left err -> expectationFailure (show err) >> fail "unreachable"

assertRequest :: OverlayInspectorRequest -> String -> Value -> Expectation
assertRequest request expectedMethod expectedParams = do
  let payload = object ["sentinel" .= True]
      runService method params = do
        show method `shouldBe` show expectedMethod
        params `shouldBe` expectedParams
        pure (Right payload)
  outcome <- performOverlayInspectorRequest runService request
  outcome `shouldBe` OverlayInspectorWorkerSucceeded payload
