{-# LANGUAGE OverloadedStrings #-}

module Spec.DataResourceValidation (spec) where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Topo.Plugin.DataResource
import Topo.Plugin.DataResource.Validation
import Topo.Plugin.RPC.DataService

spec :: Spec
spec = describe "Topo.Plugin.DataResource.Validation" $ do
  it "validates supplied inbound record fields without requiring generated fields" $ do
    validateDataRecordPartial validationSchema (record [("name", String "Alpha")])
      `shouldBe` []

  it "rejects unknown and incorrectly typed inbound fields" $ do
    map renderDataRecordValidationError
      (validateDataRecordPartial validationSchema (record [("name", Number 12), ("extra", String "x")]))
      `shouldBe`
        [ "unknown field 'extra'"
        , "invalid field 'name' (expected text)"
        ]

  it "requires complete plugin-returned records" $ do
    map renderDataRecordValidationError
      (validateDataRecordComplete validationSchema (record [("id", String "alpha")]))
      `shouldBe` ["missing field 'name'"]

  it "maps unsupported mutations and schema mismatches to standardized failures" $ do
    let unsupported = validateMutateResourceRequest
          (validationSchema { drsOperations = noOperations { doList = True } })
          (MutateResource "items" (MutCreate (record [("name", String "Alpha")])))
        invalid = validateMutateResourceRequest validationSchema
          (MutateResource "items" (MutCreate (record [("name", Bool True)])))
    fmap drfCode unsupported `shouldBe` Just OperationNotSupported
    fmap drfCode invalid `shouldBe` Just SchemaValidationFailed

  it "validates plugin-returned query records before service/API exposure" $ do
    let request = QueryResource "items" QueryAll Nothing Nothing
        result = QueryResult "items" [record [("id", String "alpha"), ("name", Number 1)]] (Just 1)
    fmap drfCode (validateQueryResult validationSchema request result)
      `shouldBe` Just SchemaValidationFailed

  it "bounds plugin query result counts by effective schema pagination" $ do
    let validRecord = record [("id", String "alpha"), ("name", String "Alpha")]
        request = QueryResource "items" QueryAll Nothing Nothing
        tooMany = QueryResult "items" (replicate 21 validRecord) (Just 21)
        invalidTotal = QueryResult "items" [validRecord] (Just 0)
    fmap drfCode (validateQueryResult validationSchema request tooMany)
      `shouldBe` Just SchemaValidationFailed
    fmap drfCode (validateQueryResult validationSchema request invalidTotal)
      `shouldBe` Just SchemaValidationFailed

validationSchema :: DataResourceSchema
validationSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "items"
  , drsLabel = "Items"
  , drsHexBound = False
  , drsFields =
      [ DataFieldDef "id" DFText "ID" False Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]
  , drsOperations = noOperations
      { doList = True
      , doGet = True
      , doCreate = True
      , doUpdate = True
      , doDelete = True
      }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = defaultDataPagination
  }

record :: [(Text, Value)] -> DataRecord
record = DataRecord . Map.fromList
