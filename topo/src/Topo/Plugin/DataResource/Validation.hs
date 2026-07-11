{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Host-side validation helpers for plugin-owned data-resource records.
--
-- These checks are intentionally transport neutral: service, HTTP, and plugin
-- manager boundaries can all map failures to the same standardized
-- data-resource error vocabulary.
module Topo.Plugin.DataResource.Validation
  ( DataRecordValidationMode(..)
  , DataRecordValidationError(..)
  , validateDataRecord
  , validateDataRecordPartial
  , validateDataRecordComplete
  , renderDataRecordValidationError
  , renderDataRecordValidationErrors
  , queryResourceSupportFailure
  , mutateResourceSupportFailure
  , validateQueryResourceRequest
  , validateMutateResourceRequest
  , validateQueryResult
  , validateMutateResult
  ) where

import Data.Aeson (FromJSON(..), Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , dataFieldTypeName
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , DataResourceErrorCode(..)
  , DataResourceFailure(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  )

-- | Whether every declared field must be present, or only supplied fields are
-- validated.  Inbound create/update payloads may be partial, while records
-- returned by plugins must be complete enough for the host UI/API to render.
data DataRecordValidationMode
  = DataRecordPartial
  | DataRecordComplete
  deriving (Eq, Show)

-- | A schema mismatch detected in a data-resource record.
data DataRecordValidationError
  = DRVMissingField ![Text]
  | DRVUnknownField ![Text]
  | DRVTypeMismatch ![Text] !Text
  | DRVEnumMismatch ![Text] ![Text]
  | DRVAdtConstructorUnknown ![Text] !Text
  | DRVAdtFieldCountMismatch ![Text] !Int !Int
  | DRVResourceMismatch !Text !Text
  deriving (Eq, Show)

validateDataRecordPartial :: DataResourceSchema -> DataRecord -> [DataRecordValidationError]
validateDataRecordPartial = validateDataRecord DataRecordPartial

validateDataRecordComplete :: DataResourceSchema -> DataRecord -> [DataRecordValidationError]
validateDataRecordComplete = validateDataRecord DataRecordComplete

-- | Validate a record against a resource schema.
validateDataRecord :: DataRecordValidationMode -> DataResourceSchema -> DataRecord -> [DataRecordValidationError]
validateDataRecord mode schema (DataRecord fields) =
  unknownFieldErrors <> missingFieldErrors <> concatMap validateKnownField (drsFields schema)
  where
    defsByName = Map.fromList [(dfName fd, fd) | fd <- drsFields schema]
    unknownFieldErrors =
      [ DRVUnknownField [field]
      | field <- Map.keys fields
      , Map.notMember field defsByName
      ]
    missingFieldErrors = case mode of
      DataRecordPartial -> []
      DataRecordComplete ->
        [ DRVMissingField [dfName fd]
        | fd <- drsFields schema
        , Map.notMember (dfName fd) fields
        ]
    validateKnownField fd = case Map.lookup (dfName fd) fields of
      Nothing -> []
      Just value -> validateFieldValue [dfName fd] (dfType fd) value

validateFieldValue :: [Text] -> DataFieldType -> Value -> [DataRecordValidationError]
validateFieldValue path fieldType value = case fieldType of
  DFText -> expectString
  DFInt -> expectInt
  DFFloat -> expectNumber
  DFDouble -> expectNumber
  DFBool -> expectBool
  DFFixed2 -> expectInt
  DFFixed3 -> expectInt
  DFFixed4 -> expectInt
  DFEnum choices -> case value of
    String actual
      | actual `elem` choices -> []
      | otherwise -> [DRVEnumMismatch path choices]
    _ -> [DRVTypeMismatch path ("enum " <> renderChoices choices)]
  DFRecord fields -> validateRecordValue path modeComplete fields value
  DFAdt constructors -> validateAdtValue path constructors value
  where
    expected = dataFieldTypeName fieldType
    expectString = case value of
      String _ -> []
      _ -> [DRVTypeMismatch path expected]
    expectNumber = case value of
      Number _ -> []
      _ -> [DRVTypeMismatch path expected]
    expectBool = case value of
      Bool _ -> []
      _ -> [DRVTypeMismatch path expected]
    expectInt =
      case Aeson.parseMaybe (parseJSON @Int) value of
        Just _ -> []
        Nothing -> [DRVTypeMismatch path expected]
    modeComplete = DataRecordComplete

validateRecordValue
  :: [Text]
  -> DataRecordValidationMode
  -> [DataFieldDef]
  -> Value
  -> [DataRecordValidationError]
validateRecordValue path mode fields (Object objectValue) =
  unknownFieldErrors <> missingFieldErrors <> concatMap validateKnownField fields
  where
    valuesByName = Map.fromList
      [ (Key.toText key, value)
      | (key, value) <- KM.toList objectValue
      ]
    defsByName = Map.fromList [(dfName fd, fd) | fd <- fields]
    unknownFieldErrors =
      [ DRVUnknownField (path <> [field])
      | field <- Map.keys valuesByName
      , Map.notMember field defsByName
      ]
    missingFieldErrors = case mode of
      DataRecordPartial -> []
      DataRecordComplete ->
        [ DRVMissingField (path <> [dfName fd])
        | fd <- fields
        , Map.notMember (dfName fd) valuesByName
        ]
    validateKnownField fd = case Map.lookup (dfName fd) valuesByName of
      Nothing -> []
      Just value -> validateFieldValue (path <> [dfName fd]) (dfType fd) value
validateRecordValue path _ _ _ = [DRVTypeMismatch path "record"]

validateAdtValue :: [Text] -> [DataConstructorDef] -> Value -> [DataRecordValidationError]
validateAdtValue path constructors (Object objectValue) =
  case ( KM.lookup "constructor" objectValue
       , KM.lookup "fields" objectValue
       ) of
    (Just (String constructorName), mFieldsValue) ->
      case lookupConstructor constructorName of
        Nothing -> [DRVAdtConstructorUnknown (path <> ["constructor"]) constructorName]
        Just constructorDef -> validateConstructorFields constructorDef mFieldsValue
    _ -> [DRVTypeMismatch path "adt"]
  where
    lookupConstructor name = foldr go Nothing constructors
      where
        go constructorDef acc
          | dcdName constructorDef == name = Just constructorDef
          | otherwise = acc
    validateConstructorFields constructorDef mFieldsValue =
      let expectedFields = dcdFields constructorDef
          actualFields = case mFieldsValue of
            Nothing -> []
            Just (Array values) -> Vector.toList values
            Just _ -> []
      in case mFieldsValue of
        Just (Array _) -> validateFieldCount expectedFields actualFields
        Nothing
          | null expectedFields -> []
          | otherwise -> [DRVAdtFieldCountMismatch (path <> [dcdName constructorDef]) (length expectedFields) 0]
        Just _ -> [DRVTypeMismatch (path <> ["fields"]) "array"]
    validateFieldCount expectedFields actualFields
      | length expectedFields /= length actualFields =
          [DRVAdtFieldCountMismatch path (length expectedFields) (length actualFields)]
      | otherwise = concat
          [ validateFieldValue (path <> [Text.pack (show index)]) expectedField actualValue
          | (index, expectedField, actualValue) <- zip3 [(0 :: Int)..] expectedFields actualFields
          ]
validateAdtValue path _ _ = [DRVTypeMismatch path "adt"]

renderDataRecordValidationErrors :: [DataRecordValidationError] -> Text
renderDataRecordValidationErrors [] = "record is valid"
renderDataRecordValidationErrors errors = Text.intercalate "; " (map renderDataRecordValidationError errors)

renderDataRecordValidationError :: DataRecordValidationError -> Text
renderDataRecordValidationError err = case err of
  DRVMissingField path -> "missing field '" <> renderPath path <> "'"
  DRVUnknownField path -> "unknown field '" <> renderPath path <> "'"
  DRVTypeMismatch path expected ->
    "invalid field '" <> renderPath path <> "' (expected " <> expected <> ")"
  DRVEnumMismatch path choices ->
    "invalid field '" <> renderPath path <> "' (expected one of " <> renderChoices choices <> ")"
  DRVAdtConstructorUnknown path constructorName ->
    "invalid field '" <> renderPath path <> "' (unknown constructor '" <> constructorName <> "')"
  DRVAdtFieldCountMismatch path expected actual ->
    "invalid field '" <> renderPath path <> "' (expected " <> Text.pack (show expected)
      <> " constructor fields, got " <> Text.pack (show actual) <> ")"
  DRVResourceMismatch expected actual ->
    "resource mismatch (expected '" <> expected <> "', got '" <> actual <> "')"

renderPath :: [Text] -> Text
renderPath [] = "<record>"
renderPath parts = Text.intercalate "." parts

renderChoices :: [Text] -> Text
renderChoices choices = "[" <> Text.intercalate ", " choices <> "]"

queryResourceSupportFailure :: DataResourceSchema -> QueryResource -> Maybe DataResourceFailure
queryResourceSupportFailure schema qr
  | qrResource qr /= drsName schema =
      Just (DataResourceFailure ResourceNotFound ("unknown resource: " <> qrResource qr))
  | hasPageRequest qr && not (doPage ops) =
      Just (DataResourceFailure QueryUnsupported ("resource '" <> name <> "' does not support paged queries"))
  | otherwise = firstFailure
      [ queryPaginationFailure schema qr
      , case qrQuery qr of
          QueryAll
            | doList ops -> Nothing
            | otherwise -> unsupported OperationNotSupported "list"
          QueryByKey key
            | not (doGet ops) -> unsupported OperationNotSupported "get"
            | otherwise -> validateKeyValue schema key
          QueryByHex _ _
            | drsHexBound schema && doQueryByHex ops -> Nothing
            | otherwise -> unsupported QueryUnsupported "hex"
          QueryByField field value
            | not (doQueryByField ops) -> unsupported QueryUnsupported "field"
            | otherwise -> case fieldDefinition schema field of
                Nothing -> Just (DataResourceFailure SchemaValidationFailed ("unknown field '" <> field <> "'"))
                Just fieldDef -> validationFailure (validateFieldValue [field] (dfType fieldDef) value)
      ]
  where
    ops = drsOperations schema
    name = drsName schema
    hasPageRequest request = qrPageSize request /= Nothing || qrPageOffset request /= Nothing
    unsupported code op = Just (DataResourceFailure code ("resource '" <> name <> "' does not support " <> op <> " queries"))

queryPaginationFailure :: DataResourceSchema -> QueryResource -> Maybe DataResourceFailure
queryPaginationFailure schema qr = firstFailure
  [ case qrPageSize qr of
      Just pageSize
        | pageSize <= 0 -> Just (DataResourceFailure SchemaValidationFailed "page_size must be greater than 0")
        | pageSize > dpMaxPageSize pagination -> Just (DataResourceFailure SchemaValidationFailed
            ("page_size exceeds max_page_size " <> Text.pack (show (dpMaxPageSize pagination))))
      _ -> Nothing
  , case qrPageOffset qr of
      Just pageOffset
        | pageOffset < 0 -> Just (DataResourceFailure SchemaValidationFailed "page_offset must be greater than or equal to 0")
      _ -> Nothing
  ]
  where
    pagination = drsPagination schema

mutateResourceSupportFailure :: DataResourceSchema -> MutateResource -> Maybe DataResourceFailure
mutateResourceSupportFailure schema mr
  | mrResource mr /= drsName schema =
      Just (DataResourceFailure ResourceNotFound ("unknown resource: " <> mrResource mr))
  | otherwise = case mrMutation mr of
      MutCreate record
        | not (doCreate ops) -> unsupported "create"
        | otherwise -> validationFailure (validateDataRecordPartial schema record)
      MutUpdate key record
        | not (doUpdate ops) -> unsupported "update"
        | otherwise -> firstFailure
            [ validateKeyValue schema key
            , validationFailure (validateDataRecordPartial schema record)
            ]
      MutDelete key
        | not (doDelete ops) -> unsupported "delete"
        | otherwise -> validateKeyValue schema key
      MutSetHex _ _ record
        | drsHexBound schema && (doCreate ops || doUpdate ops) ->
            validationFailure (validateDataRecordPartial schema record)
        | otherwise -> unsupported "set_hex"
  where
    ops = drsOperations schema
    name = drsName schema
    unsupported op = Just (DataResourceFailure OperationNotSupported ("resource '" <> name <> "' does not support " <> op <> " mutations"))

validateQueryResourceRequest :: DataResourceSchema -> QueryResource -> Maybe DataResourceFailure
validateQueryResourceRequest = queryResourceSupportFailure

validateMutateResourceRequest :: DataResourceSchema -> MutateResource -> Maybe DataResourceFailure
validateMutateResourceRequest = mutateResourceSupportFailure

validateQueryResult :: DataResourceSchema -> QueryResource -> QueryResult -> Maybe DataResourceFailure
validateQueryResult schema request result = firstFailure
  [ if qrsResource result == qrResource request
      then Nothing
      else Just (DataResourceFailure SchemaValidationFailed
        (renderDataRecordValidationError (DRVResourceMismatch (qrResource request) (qrsResource result))))
  , validationFailure (concatMap (validateDataRecordComplete schema) (qrsRecords result))
  ]

validateMutateResult :: DataResourceSchema -> MutateResource -> MutateResult -> Maybe DataResourceFailure
validateMutateResult schema request result
  | not (mrsSuccess result) = Nothing
  | otherwise = case (mrMutation request, mrsRecord result) of
      (MutDelete _, Nothing) -> Nothing
      (_, Nothing) -> Nothing
      (_, Just record) -> validationFailure (validateDataRecordComplete schema record)

validateKeyValue :: DataResourceSchema -> Value -> Maybe DataResourceFailure
validateKeyValue schema key = case fieldDefinition schema (drsKeyField schema) of
  Nothing -> Just (DataResourceFailure SchemaValidationFailed ("missing key field in schema: " <> drsKeyField schema))
  Just fieldDef -> validationFailure (validateFieldValue [drsKeyField schema] (dfType fieldDef) key)

fieldDefinition :: DataResourceSchema -> Text -> Maybe DataFieldDef
fieldDefinition schema field = foldr go Nothing (drsFields schema)
  where
    go fieldDef acc
      | dfName fieldDef == field = Just fieldDef
      | otherwise = acc

validationFailure :: [DataRecordValidationError] -> Maybe DataResourceFailure
validationFailure errors = case errors of
  [] -> Nothing
  _ -> Just (DataResourceFailure SchemaValidationFailed (renderDataRecordValidationErrors errors))

firstFailure :: [Maybe DataResourceFailure] -> Maybe DataResourceFailure
firstFailure = foldr go Nothing
  where
    go (Just failure) _ = Just failure
    go Nothing acc = acc
