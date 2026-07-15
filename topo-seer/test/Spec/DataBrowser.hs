{-# LANGUAGE OverloadedStrings #-}

module Spec.DataBrowser (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Seer.DataBrowser.Lifecycle as Lifecycle
import Seer.DataBrowser.Model
import Test.Hspec
import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
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

  it "submits nested record and ADT edits as nested payload values" $ do
    let selected = dataBrowserReducer emptyDataBrowserModel (DataBrowserSelectResource "atlas" detailSchema)
        loaded = dataBrowserReducer selected $
          DataBrowserListSucceeded
            (DataBrowserListRecordsRequest "atlas" "details" Nothing Nothing)
            (QueryResult "details" [detailRecord] (Just 1))
        editing0 = dataBrowserReducer (selectFirstRecord loaded) DataBrowserStartEdit
        values0 = dbEditBufferValues (dbModelEditBuffer editing0)
    Map.lookup "profile.name" values0 `shouldBe` Just (String "Nested")
    Map.lookup "profile.code" values0 `shouldBe` Nothing
    Map.lookup "shape.Circle.1" values0 `shouldBe` Just (String "red")

    let editing1 = dataBrowserReducer editing0 (DataBrowserSetFieldValue "profile.name" (String "Updated"))
        editing2 = dataBrowserReducer editing1 (DataBrowserSetFieldValue "shape.Circle.1" (String "blue"))
        saved = dataBrowserReducer editing2 DataBrowserSave
        expectedRecord = DataRecord $ Map.fromList
          [ ("profile", object
              [ "age" .= Number 42
              , "code" .= String "P-1"
              , "name" .= String "Updated"
              ])
          , ("shape", object
              [ "constructor" .= String "Circle"
              , "fields" .= Array (Vector.fromList [Number 12.5, String "blue"])
              ])
          , ("active", Bool True)
          ]
        request = DataBrowserUpdateRecordRequest "atlas" "details" (Number 1) expectedRecord
    dbModelValidationErrors saved `shouldBe` []
    dbModelPendingRequest saved `shouldBe` Just request

    let switched0 = dataBrowserReducer editing0 (DataBrowserSetFieldValue "shape.Point.0" (Number 7))
        switched1 = dataBrowserReducer switched0 (DataBrowserSetFieldValue "shape.Point.1" (Number 8))
        switchedValues = dbEditBufferValues (dbModelEditBuffer switched1)
        switched = dataBrowserReducer switched1 DataBrowserSave
        expectedSwitchedRecord = DataRecord $ Map.fromList
          [ ("profile", object
              [ "age" .= Number 42
              , "code" .= String "P-1"
              , "name" .= String "Nested"
              ])
          , ("shape", object
              [ "constructor" .= String "Point"
              , "fields" .= Array (Vector.fromList [Number 7, Number 8])
              ])
          , ("active", Bool True)
          ]
        switchedRequest = DataBrowserUpdateRecordRequest "atlas" "details" (Number 1) expectedSwitchedRecord
    Map.lookup "shape.Circle.0" switchedValues `shouldBe` Nothing
    Map.lookup "shape.Circle.1" switchedValues `shouldBe` Nothing
    dbModelPendingRequest switched `shouldBe` Just switchedRequest

  it "clears stale siblings for nested ADTs inside constructor fields" $ do
    let values = Map.fromList
          [ ("outer.Wrap.0.choice.A.0", String "old")
          , ("outer.Wrap.0.choice.B.0", String "new")
          ]
        cleared = clearAdtSiblingEditValues nestedAdtFields "outer.Wrap.0.choice.B.0" values
    Map.lookup "outer.Wrap.0.choice.A.0" cleared `shouldBe` Nothing
    Map.lookup "outer.Wrap.0.choice.B.0" cleared `shouldBe` Just (String "new")

  it "requests pages while retaining the current page until completion" $ do
    let selected = selectFirstRecord loadedModel
        nextPage = dataBrowserReducer selected (DataBrowserPage DataBrowserPageNext)
    dbModelRecords nextPage `shouldBe` [record1, record2]
    dbSelectionRecord (dbModelSelection nextPage) `shouldBe` Nothing
    dbModelPagination nextPage `shouldBe` DataBrowserPagination 5 (Just 10) (Just 2)
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

  it "uses request IDs to make stale completions a total no-op" $ do
    let initial = DataBrowserUi (Map.singleton "atlas" [pagedSchema]) emptyDataBrowserModel
        (firstUi, firstBegin) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 1) initial (Lifecycle.DataBrowserSelectResource "atlas" "cities")
        (secondUi, secondBegin) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 2) firstUi (Lifecycle.DataBrowserSelectResource "atlas" "cities")
        firstEnvelope = accepted firstBegin
        secondEnvelope = accepted secondBegin
        staleCompletion = DataBrowserCompletion
          { dbcRequestId = dbpeRequestId firstEnvelope
          , dbcRequest = dbpeRequest firstEnvelope
          , dbcOutcome = DataBrowserRecordsLoaded (QueryResult "cities" [record1] (Just 1))
          }
        (afterStale, applied) = completeDataBrowserRequest staleCompletion secondUi
    dbpeRequestId secondEnvelope `shouldNotBe` dbpeRequestId firstEnvelope
    applied `shouldBe` False
    afterStale `shouldBe` secondUi

  it "applies resources and browser records in one guarded completion" $ do
    let initial = DataBrowserUi Map.empty emptyDataBrowserModel
        (pendingUi, begun) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 7) initial (Lifecycle.DataBrowserSelectResource "atlas" "cities")
        envelope = accepted begun
        completion = DataBrowserCompletion
          { dbcRequestId = dbpeRequestId envelope
          , dbcRequest = dbpeRequest envelope
          , dbcOutcome = DataBrowserResourceLoaded "atlas" [pagedSchema] pagedSchema
              (Just (QueryResult "cities" [record1, record2] (Just 2)))
          }
        (completedUi, applied) = completeDataBrowserRequest completion pendingUi
    applied `shouldBe` True
    dbuResources completedUi `shouldBe` Map.singleton "atlas" [pagedSchema]
    dbModelRecords (dbuModel completedUi) `shouldBe` [record1, record2]
    dbModelPendingEnvelope (dbuModel completedUi) `shouldBe` Nothing

  it "scopes matching async failures separately and clears them on retry" $ do
    let pageableModel = loadedModel
          { dbModelPagination = DataBrowserPagination 5 (Just 10) (Just 100) }
        initial = DataBrowserUi (Map.singleton "atlas" [pagedSchema]) pageableModel
        (pendingUi, begun) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 20) initial
          (Lifecycle.DataBrowserQueryRecords DataBrowserPageNext)
        envelope = accepted begun
        failure = DataBrowserCompletion
          { dbcRequestId = dbpeRequestId envelope
          , dbcRequest = dbpeRequest envelope
          , dbcOutcome = DataBrowserWorkerFailed "page unavailable"
          }
        (failedUi, applied) = completeDataBrowserRequest failure pendingUi
        failedModel = dbuModel failedUi
        (retryUi, retry) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 21) failedUi
          (Lifecycle.DataBrowserQueryRecords DataBrowserPageNext)
    applied `shouldBe` True
    fmap dbaeMessage (dbModelAsyncError failedModel) `shouldBe` Just "page unavailable"
    dbModelValidationErrors failedModel `shouldBe` []
    dbModelRecords failedModel `shouldBe` [record1, record2]
    dbPaginationOffset (dbModelPagination failedModel) `shouldBe` 5
    retry `shouldSatisfy` isAccepted
    fmap dbpeRequest (dbModelPendingEnvelope (dbuModel retryUi))
      `shouldBe` Just (DataBrowserRecordRequest
        (DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 15)))
    dbModelAsyncError (dbuModel retryUi) `shouldBe` Nothing

  it "retains a mutation operation error when a validation retry is rejected" $ do
    let editing = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserStartEdit
        invalid = dataBrowserReducer editing
          (DataBrowserSetFieldValue "population" (String "invalid"))
        priorRequest = DataBrowserRecordRequest
          (DataBrowserUpdateRecordRequest "atlas" "cities" (Number 1) record1)
        priorError = DataBrowserAsyncError
          (DataBrowserRequestId 30) DataBrowserUpdateOperation priorRequest "backend rejected"
        initial = DataBrowserUi (Map.singleton "atlas" [pagedSchema])
          invalid { dbModelAsyncError = Just priorError }
        (rejectedUi, result) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 31) initial Lifecycle.DataBrowserUpdateRecord
    result `shouldBe` DataBrowserBeginRejected "population: expected int"
    dbModelAsyncError (dbuModel rejectedUi) `shouldBe` Just priorError
    dbModelValidationErrors (dbuModel rejectedUi)
      `shouldBe` [DataBrowserValidationError (Just "population") "expected int"]

  it "does not revive a mutation failure after leaving and re-entering its mode" $ do
    let editing = dataBrowserReducer (selectFirstRecord loadedModel) DataBrowserStartEdit
        request = DataBrowserRecordRequest
          (DataBrowserUpdateRecordRequest "atlas" "cities" (Number 1) record1)
        priorError = DataBrowserAsyncError
          (DataBrowserRequestId 40) DataBrowserUpdateOperation request "backend rejected"
        initial = DataBrowserUi (Map.singleton "atlas" [pagedSchema])
          editing { dbModelAsyncError = Just priorError }
        (cancelledUi, cancelled) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 41) initial Lifecycle.DataBrowserCancelEdit
        (reopenedUi, reopened) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 41) cancelledUi Lifecycle.DataBrowserStartEdit
    cancelled `shouldBe` DataBrowserBeginPure
    reopened `shouldBe` DataBrowserBeginPure
    dbModelAsyncError (dbuModel cancelledUi) `shouldBe` Nothing
    dbModelAsyncError (dbuModel reopenedUi) `shouldBe` Nothing

  it "locks navigation while a mutation is pending without losing edit state" $ do
    let initial = DataBrowserUi (Map.singleton "atlas" [pagedSchema]) loadedModel
        (creatingUi, _) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 10) initial Lifecycle.DataBrowserStartCreate
        editedModel = dataBrowserReducer (dbuModel creatingUi)
          (DataBrowserSetFieldValue "name" (String "Retained"))
        editedUi = creatingUi { dbuModel = editedModel }
        (mutationUi, mutationBegin) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 10) editedUi Lifecycle.DataBrowserCreateRecord
        (rejectedUi, rejected) = Lifecycle.beginDataBrowserAction
          (DataBrowserRequestId 11) mutationUi (Lifecycle.DataBrowserSelectPlugin "other")
    mutationBegin `shouldSatisfy` isAccepted
    rejected `shouldBe` DataBrowserBeginRejected "data browser mutation in progress"
    rejectedUi `shouldBe` mutationUi
    Map.lookup "name" (dbEditBufferValues (dbModelEditBuffer (dbuModel rejectedUi)))
      `shouldBe` Just (String "Retained")

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

accepted :: DataBrowserBeginResult -> DataBrowserPendingEnvelope
accepted (DataBrowserBeginAccepted envelope _) = envelope
accepted result = error ("expected accepted request, got " <> show result)

isAccepted :: DataBrowserBeginResult -> Bool
isAccepted DataBrowserBeginAccepted {} = True
isAccepted _ = False

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

detailRecord :: DataRecord
detailRecord = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("profile", object
      [ "age" .= Number 42
      , "code" .= String "P-1"
      , "name" .= String "Nested"
      ])
  , ("shape", object
      [ "constructor" .= String "Circle"
      , "fields" .= Array (Vector.fromList [Number 12.5, String "red"])
      ])
  , ("active", Bool True)
  ]

detailSchema :: DataResourceSchema
detailSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "details"
  , drsLabel = "Details"
  , drsHexBound = False
  , drsFields = detailFields
  , drsOperations = allOperations { doPage = False }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = DataPagination 20 100 0
  }

detailFields :: [DataFieldDef]
detailFields =
  [ DataFieldDef "id" DFInt "ID" False Nothing
  , DataFieldDef "profile" (DFRecord
      [ DataFieldDef "age" DFInt "Age" True Nothing
      , DataFieldDef "code" DFText "Code" False Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]) "Profile" True Nothing
  , DataFieldDef "shape" (DFAdt
      [ DataConstructorDef "Circle" [DFFloat, DFText]
      , DataConstructorDef "Point" [DFInt, DFInt]
      ]) "Shape" True Nothing
  , DataFieldDef "active" DFBool "Active" True Nothing
  ]

nestedAdtFields :: [DataFieldDef]
nestedAdtFields =
  [ DataFieldDef "outer" (DFAdt
      [ DataConstructorDef "Wrap"
          [ DFRecord
              [ DataFieldDef "choice" (DFAdt
                  [ DataConstructorDef "A" [DFText]
                  , DataConstructorDef "B" [DFText]
                  ]) "Choice" True Nothing
              ]
          ]
      ]) "Outer" True Nothing
  ]
