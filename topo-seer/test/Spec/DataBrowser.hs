{-# LANGUAGE OverloadedStrings #-}

module Spec.DataBrowser (spec) where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Seer.DataBrowser.Model
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
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..))

spec :: Spec
spec = describe "DataBrowser model reducer" $ do
  it "selects a resource and exposes an initial list request" $ do
    let model = dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" pagedSchema)
        request = DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 5)
    dbSelectionPlugin (dbModelSelection model) `shouldBe` Just "atlas"
    dbSelectionResource (dbModelSelection model) `shouldBe` Just "cities"
    dbModelPagination model `shouldBe` DataBrowserPagination 5 (Just 10) Nothing
    dbModelPendingRequest model `shouldBe` Just request
    dbModelLoading model `shouldBe` True

  it "loads records only for the matching pending request" $ do
    let requested = dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" pagedSchema)
        request = DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 5)
        stale = DataBrowserListRecordsRequest "atlas" "towns" (Just 10) (Just 5)
        result = QueryResult "cities" [record1, record2] (Just 2)
    dataBrowserReducer requested (DataBrowserListSucceeded stale result) `shouldBe` requested
    let loaded = dataBrowserReducer requested (DataBrowserListSucceeded request result)
    dbModelRecords loaded `shouldBe` [record1, record2]
    dbModelPagination loaded `shouldBe` DataBrowserPagination 5 (Just 10) (Just 2)
    dbModelPendingRequest loaded `shouldBe` Nothing
    dbModelLoading loaded `shouldBe` False

  it "selects records using the schema key field" $ do
    let selected = selectFirstRecord loadedModel
    dbModelMode selected `shouldBe` DataBrowserViewMode
    dbSelectionRecord (dbModelSelection selected) `shouldBe` Just record1
    dbSelectionRecordKey (dbModelSelection selected) `shouldBe` Just (Number 1)
    dbSelectionRowIndex (dbModelSelection selected) `shouldBe` Just 0

  it "validates edit values before creating an update request" $ do
    let editing = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserStartEdit
        invalid = dataBrowserReducer
          (dataBrowserReducer editing (DataBrowserSetFieldValue "population" (String "many")))
          DataBrowserSave
    dbModelValidationErrors invalid `shouldBe`
      [DataBrowserValidationError (Just "population") "expected int"]
    dbModelPendingRequest invalid `shouldBe` Nothing

    let fixed = dataBrowserReducer invalid (DataBrowserSetFieldValue "population" (Number 11))
        saved = dataBrowserReducer fixed DataBrowserSave
        expectedValues = Map.insert "population" (Number 11) (Map.delete "id" (unDataRecord record1))
        expectedRecord = DataRecord expectedValues
        request = DataBrowserUpdateRecordRequest "atlas" "cities" (Number 1) expectedRecord
    dbModelMode saved `shouldBe` DataBrowserEditMode
    dbModelPendingRequest saved `shouldBe` Just request
    dbModelLoading saved `shouldBe` True

    let failed = dataBrowserReducer saved (DataBrowserMutationFailed request "backend rejected update")
    dbModelMode failed `shouldBe` DataBrowserEditMode
    dbEditBufferValues (dbModelEditBuffer failed) `shouldBe` expectedValues
    dbModelValidationErrors failed `shouldBe`
      [DataBrowserValidationError Nothing "backend rejected update"]

    let fullUpdatedRecord = DataRecord (Map.insert "population" (Number 11) (unDataRecord record1))
        succeededWithoutReturn = dataBrowserReducer saved (DataBrowserMutationSucceeded request Nothing)
    dbSelectionRecord (dbModelSelection succeededWithoutReturn) `shouldBe` Just fullUpdatedRecord
    take 1 (dbModelRecords succeededWithoutReturn) `shouldBe` [fullUpdatedRecord]

    let returnedRecord = DataRecord (Map.insert "id" (Number 1) expectedValues)
        succeeded = dataBrowserReducer saved (DataBrowserMutationSucceeded request (Just returnedRecord))
    dbModelMode succeeded `shouldBe` DataBrowserViewMode
    dbSelectionRecord (dbModelSelection succeeded) `shouldBe` Just returnedRecord
    take 1 (dbModelRecords succeeded) `shouldBe` [returnedRecord]

  it "opens create mode from schema defaults and creates a pending request" $ do
    let creating = dataBrowserReducer selectedResourceModel DataBrowserStartCreate
        defaults = Map.fromList
          [ ("name", String "Unnamed")
          , ("population", Number 0)
          , ("kind", String "town")
          , ("capital", Bool False)
          ]
    dbModelMode creating `shouldBe` DataBrowserCreateMode
    dbEditBufferValues (dbModelEditBuffer creating) `shouldBe` defaults
    dbSelectionRecord (dbModelSelection creating) `shouldBe` Just (DataRecord defaults)

    let saved = dataBrowserReducer creating DataBrowserSave
        request = DataBrowserCreateRecordRequest "atlas" "cities" (DataRecord defaults)
    dbModelMode saved `shouldBe` DataBrowserCreateMode
    dbModelPendingRequest saved `shouldBe` Just request
    dbModelLoading saved `shouldBe` True

    let createdRecord = DataRecord (Map.insert "id" (Number 3) defaults)
        succeeded = dataBrowserReducer saved (DataBrowserMutationSucceeded request (Just createdRecord))
    dbModelMode succeeded `shouldBe` DataBrowserBrowseMode
    dbSelectionRecord (dbModelSelection succeeded) `shouldBe` Nothing
    dbModelRecords succeeded `shouldBe` [createdRecord]

  it "excludes read-only fields from edit buffers and rejects explicit read-only edits" $ do
    let editing = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserStartEdit
        forced = dataBrowserReducer editing (DataBrowserSetFieldValue "id" (Number 99))
        invalid = dataBrowserReducer forced DataBrowserSave
    Map.member "id" (dbEditBufferValues (dbModelEditBuffer editing)) `shouldBe` False
    dbModelValidationErrors invalid `shouldBe`
      [DataBrowserValidationError (Just "id") "field is read-only"]

  it "updates edit buffers through focused text and scalar actions" $ do
    let editing0 = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserStartEdit
        editing1 = dataBrowserReducer editing0 (DataBrowserFocusField "name")
        editing2 = dataBrowserReducer editing1 (DataBrowserInsertText " Prime")
        editing3 = dataBrowserReducer editing2 (DataBrowserStepNumberField "population" 1)
        editing4 = dataBrowserReducer editing3 (DataBrowserToggleBoolField "capital")
        editing5 = dataBrowserReducer editing4 (DataBrowserCycleEnumField "kind" 1)
        values = dbEditBufferValues (dbModelEditBuffer editing5)
    Map.lookup "name" values `shouldBe` Just (String "Alpha Prime")
    Map.lookup "population" values `shouldBe` Just (Number 101)
    Map.lookup "capital" values `shouldBe` Just (Bool True)
    Map.lookup "kind" values `shouldBe` Just (String "city")
    dbModelTextCursor editing5 `shouldBe` 11

  it "requests pages and clears record selection" $ do
    let selected = selectFirstRecord loadedModel
        nextPage = dataBrowserReducer selected (DataBrowserPage DataBrowserPageNext)
    dbModelRecords nextPage `shouldBe` []
    dbSelectionRecord (dbModelSelection nextPage) `shouldBe` Nothing
    dbModelPagination nextPage `shouldBe` DataBrowserPagination 15 (Just 10) Nothing
    dbModelPendingRequest nextPage `shouldBe`
      Just (DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 15))
    dbModelLoading nextPage `shouldBe` True

  it "models delete confirmation and pending delete requests" $ do
    let confirm = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserRequestDelete
        deleting = dataBrowserReducer confirm DataBrowserConfirmDelete
        request = DataBrowserDeleteRecordRequest "atlas" "cities" (Number 1)
    dbModelMode confirm `shouldBe` DataBrowserDeleteConfirmMode
    dbModelMode deleting `shouldBe` DataBrowserDeleteConfirmMode
    dbSelectionRecord (dbModelSelection deleting) `shouldBe` Just record1
    dbModelPendingRequest deleting `shouldBe` Just request
    dbModelLoading deleting `shouldBe` True

    let failed = dataBrowserReducer deleting (DataBrowserMutationFailed request "delete failed")
    dbModelMode failed `shouldBe` DataBrowserDeleteConfirmMode
    dbSelectionRecord (dbModelSelection failed) `shouldBe` Just record1
    dbModelValidationErrors failed `shouldBe` [DataBrowserValidationError Nothing "delete failed"]

    let succeeded = dataBrowserReducer deleting (DataBrowserMutationSucceeded request Nothing)
    dbModelMode succeeded `shouldBe` DataBrowserBrowseMode
    dbSelectionRecord (dbModelSelection succeeded) `shouldBe` Nothing
    dbModelRecords succeeded `shouldBe` [record2]
    dbModelPagination succeeded `shouldBe` DataBrowserPagination 5 (Just 10) (Just 1)

  it "does not issue list requests for non-listable resources" $ do
    let noListSchema = pagedSchema { drsOperations = allOperations { doList = False, doPage = True } }
        model = dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" noListSchema)
    dbModelPendingRequest model `shouldBe` Nothing
    dbModelLoading model `shouldBe` False
    dbModelValidationErrors model `shouldBe` [DataBrowserValidationError Nothing "list not supported"]

    let paged = dataBrowserReducer model (DataBrowserPage DataBrowserPageNext)
    dbModelPendingRequest paged `shouldBe` Nothing
    dbModelValidationErrors paged `shouldBe` [DataBrowserValidationError Nothing "list not supported"]

selectedResourceModel :: DataBrowserModel
selectedResourceModel =
  dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" pagedSchema)

loadedModel :: DataBrowserModel
loadedModel =
  dataBrowserReducer selectedResourceModel $
    DataBrowserListSucceeded
      (DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 5))
      (QueryResult "cities" [record1, record2] (Just 2))

selectFirstRecord :: DataBrowserModel -> DataBrowserModel
selectFirstRecord model = dataBrowserReducer model (DataBrowserSelectRecord 0)

record1 :: DataRecord
record1 = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("name", String "Alpha")
  , ("population", Number 100)
  , ("kind", String "town")
  , ("capital", Bool False)
  ]

record2 :: DataRecord
record2 = DataRecord $ Map.fromList
  [ ("id", Number 2)
  , ("name", String "Beta")
  , ("population", Number 50)
  , ("kind", String "city")
  , ("capital", Bool True)
  ]

pagedSchema :: DataResourceSchema
pagedSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "cities"
  , drsLabel = "Cities"
  , drsHexBound = False
  , drsFields = cityFields
  , drsOperations = allOperations { doPage = True }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = DataPagination
      { dpDefaultPageSize = 10
      , dpMaxPageSize = 25
      , dpDefaultPageOffset = 5
      }
  }

cityFields :: [DataFieldDef]
cityFields =
  [ DataFieldDef "id" DFInt "ID" False Nothing
  , DataFieldDef "name" DFText "Name" True (Just (String "Unnamed"))
  , DataFieldDef "population" DFInt "Population" True (Just (Number 0))
  , DataFieldDef "kind" (DFEnum ["town", "city"]) "Kind" True (Just (String "town"))
  , DataFieldDef "capital" DFBool "Capital" True (Just (Bool False))
  ]
