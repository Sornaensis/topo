{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the overlay system (Phase 4).
--
-- Covers schema parsing, in-memory operations, chunk export round-trip,
-- field indexing, schema migration, and overlay manifest in storage.
module Spec.Overlay (spec) where

import Data.Aeson (Value(..), encode, eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayRecord(..)
  , OverlayStore(..)
  , OverlayValue(..)
  , chunkDelete
  , chunkInsert
  , chunkLookup
  , chunkSize
  , defaultRecord
  , defaultValue
  , emptyOverlay
  , emptyOverlayChunk
  , emptyOverlayStore
  , floatToOverlayValue
  , insertOverlay
  , lookupOverlay
  , matchesFieldType
  , mkOverlayRecord
  , mkOverlayRecordUnchecked
  , overlayCount
  , overlayName
  , overlayNames
  , overlayValueToFloat
  , setRecordFieldChecked
  )
import Topo.Overlay.Export
  ( decodeDenseChunk
  , decodeSparseChunk
  , encodeDenseChunk
  , encodeSparseChunk
  , exportOverlayChunks
  )
import Topo.Overlay.JSON
  ( overlayFromJSON
  , overlayToJSON
  )
import Topo.Overlay.Index
  ( FieldIndex(..)
  , OverlayIndex(..)
  , buildIndices
  , lookupBoolIndex
  , lookupIntIndex
  , queryFloatRange
  )
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , SchemaError(..)
  , encodeOverlaySchema
  , fieldIndex
  , parseOverlaySchema
  , validateSchema
  )
import Topo.Overlay.Storage
  ( OverlayLifecycle(..)
  , MigrationResult(..)
  , migrateOverlayData
  , migrationLifecycle
  , migrationWarnings
  )
import Topo.Overlay.Indexed
  ( IndexedOverlay(..)
  , mkIndexed
  , mkIndexedFresh
  , getOverlay
  , getData
  , ensureIndex
  , invalidateIndex
  , insertSparseRecord
  , deleteSparseRecord
  )
import qualified Topo.Overlay.Indexed as Indexed

------------------------------------------------------------------------
-- Test schema fixtures
------------------------------------------------------------------------

-- | A simple sparse schema with mixed field types for testing.
testSparseSchema :: OverlaySchema
testSparseSchema = case parseOverlaySchema (encodeOverlaySchema rawSchema) of
    Right s -> s
    Left  e -> error ("testSparseSchema parse failed: " <> show e)
  where
    rawSchema = OverlaySchema
      { osName = "test-sparse"
      , osVersion = "1.0.0"
      , osDescription = "Test sparse overlay"
      , osFields =
          [ OverlayFieldDef "population" OFFloat (Number 0) False Nothing
          , OverlayFieldDef "culture_id" OFInt   (Number 0) True  Nothing
          , OverlayFieldDef "is_capital" OFBool  (Bool False) True  Nothing
          , OverlayFieldDef "city_name" OFText   (String "") False Nothing
          ]
      , osStorage = StorageSparse
      , osDependencies = OverlayDeps True ["weather"]
      , osFieldIndex = Map.empty  -- will be rebuilt on parse
      }

-- | A dense schema with only numeric fields.
testDenseSchema :: OverlaySchema
testDenseSchema = case parseOverlaySchema (encodeOverlaySchema rawSchema) of
    Right s -> s
    Left  e -> error ("testDenseSchema parse failed: " <> show e)
  where
    rawSchema = OverlaySchema
      { osName = "test-dense"
      , osVersion = "1.0.0"
      , osDescription = "Test dense overlay"
      , osFields =
          [ OverlayFieldDef "temperature" OFFloat (Number 0.5) False Nothing
          , OverlayFieldDef "humidity"    OFFloat (Number 0.5) True  Nothing
          , OverlayFieldDef "wind_speed"  OFFloat (Number 0.0) False Nothing
          ]
      , osStorage = StorageDense
      , osDependencies = OverlayDeps True []
      , osFieldIndex = Map.empty
      }

-- | A sample sparse record for the test schema.
testRecord :: OverlayRecord
testRecord = OverlayRecord $ V.fromList
  [ OVFloat 1234.5
  , OVInt 42
  , OVBool True
  , OVText "Gondolin"
  ]

spec :: Spec
spec = do
  schemaSpec
  overlayDataSpec
  recordSafetySpec
  overlayJsonSpec
  exportSpec
  indexSpec
  indexedSpec
  migrationSpec
  storeSpec

------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------

schemaSpec :: Spec
schemaSpec = describe "Overlay.Schema" $ do

  it "round-trips a sparse schema through JSON" $ do
    let bytes = encodeOverlaySchema testSparseSchema
    case parseOverlaySchema bytes of
      Left err -> expectationFailure ("parse failed: " <> show err)
      Right schema -> do
        osName schema `shouldBe` "test-sparse"
        length (osFields schema) `shouldBe` 4
        osStorage schema `shouldBe` StorageSparse

  it "round-trips a dense schema through JSON" $ do
    let bytes = encodeOverlaySchema testDenseSchema
    case parseOverlaySchema bytes of
      Left err -> expectationFailure ("parse failed: " <> show err)
      Right schema -> do
        osName schema `shouldBe` "test-dense"
        osStorage schema `shouldBe` StorageDense

  it "builds field index on parse" $ do
    fieldIndex testSparseSchema "population" `shouldBe` Just 0
    fieldIndex testSparseSchema "culture_id" `shouldBe` Just 1
    fieldIndex testSparseSchema "is_capital" `shouldBe` Just 2
    fieldIndex testSparseSchema "city_name"  `shouldBe` Just 3
    fieldIndex testSparseSchema "nonexistent" `shouldBe` Nothing

  it "validates: rejects dense schema with text field" $ do
    let schema = testDenseSchema
          { osFields = osFields testDenseSchema ++
              [ OverlayFieldDef "label" OFText (String "") False Nothing ]
          }
    let errors = validateSchema schema
    errors `shouldSatisfy` any (\case DenseTextFieldDisallowed _ -> True; _ -> False)

  it "validates: detects duplicate field names" $ do
    let schema = testSparseSchema
          { osFields = osFields testSparseSchema ++
              [ OverlayFieldDef "population" OFFloat (Number 0) False Nothing ]
          }
    let errors = validateSchema schema
    errors `shouldSatisfy` any (\case DuplicateFieldName _ -> True; _ -> False)

  it "validates: rejects empty name" $ do
    let schema = testSparseSchema { osName = "" }
    validateSchema schema `shouldSatisfy` any (\case EmptySchemaName -> True; _ -> False)

  it "validates: rejects empty version" $ do
    let schema = testSparseSchema { osVersion = "" }
    validateSchema schema `shouldSatisfy` any (\case EmptySchemaVersion -> True; _ -> False)

  it "validates: rejects zero fields" $ do
    let schema = testSparseSchema { osFields = [] }
    validateSchema schema `shouldSatisfy` any (\case NoFields -> True; _ -> False)

------------------------------------------------------------------------
-- Overlay data operations
------------------------------------------------------------------------

overlayDataSpec :: Spec
overlayDataSpec = describe "Overlay data operations" $ do

  it "defaultRecord produces correct field count" $ do
    let rec = defaultRecord testSparseSchema
        (OverlayRecord v) = rec
    V.length v `shouldBe` 4

  it "defaultValue produces correct types" $ do
    let fields = osFields testSparseSchema
    case map defaultValue fields of
      [OVFloat f, OVInt i, OVBool b, OVText t] -> do
        f `shouldBe` 0.0
        i `shouldBe` 0
        b `shouldBe` False
        t `shouldBe` ""
      other -> expectationFailure ("unexpected defaults: " <> show other)

  it "overlayValueToFloat round-trips with floatToOverlayValue for OFFloat" $
    property $ \(x :: Float) ->
      let v = OVFloat x
      in  floatToOverlayValue OFFloat (overlayValueToFloat v) == v

  it "chunk insert/lookup works" $ do
    let chunk0 = emptyOverlayChunk
        chunk1 = chunkInsert 5 testRecord chunk0
    chunkLookup 5 chunk1 `shouldBe` Just testRecord
    chunkLookup 6 chunk1 `shouldBe` Nothing
    chunkSize chunk1 `shouldBe` 1

  it "chunk delete works" $ do
    let chunk0 = chunkInsert 5 testRecord emptyOverlayChunk
        chunk1 = chunkDelete 5 chunk0
    chunkLookup 5 chunk1 `shouldBe` Nothing
    chunkSize chunk1 `shouldBe` 0

  it "emptyOverlay produces correct storage type" $ do
    let ov = emptyOverlay testSparseSchema
    overlayName ov `shouldBe` "test-sparse"
    case ovData ov of
      SparseData m -> IntMap.size m `shouldBe` 0
      DenseData _  -> expectationFailure "expected SparseData"

  it "emptyOverlay dense produces DenseData" $ do
    let ov = emptyOverlay testDenseSchema
    case ovData ov of
      DenseData m  -> IntMap.size m `shouldBe` 0
      SparseData _ -> expectationFailure "expected DenseData"

------------------------------------------------------------------------
-- Record type safety (Phase 5)
------------------------------------------------------------------------

recordSafetySpec :: Spec
recordSafetySpec = describe "Overlay record type safety" $ do

  it "mkOverlayRecord accepts correctly-typed values" $ do
    let vals = [OVFloat 1.0, OVInt 2, OVBool True, OVText "x"]
    case mkOverlayRecord testSparseSchema vals of
      Right (OverlayRecord v) -> V.length v `shouldBe` 4
      Left err -> expectationFailure ("unexpected rejection: " <> show err)

  it "mkOverlayRecord rejects wrong field count" $ do
    let vals = [OVFloat 1.0, OVInt 2]
    mkOverlayRecord testSparseSchema vals `shouldSatisfy` isLeft

  it "mkOverlayRecord rejects type mismatch" $ do
    -- Put an Int where a Float is expected
    let vals = [OVInt 1, OVInt 2, OVBool True, OVText "x"]
    mkOverlayRecord testSparseSchema vals `shouldSatisfy` isLeft

  it "mkOverlayRecord rejects type mismatch in non-first field" $ do
    -- Put a Float where an Int is expected (culture_id)
    let vals = [OVFloat 1.0, OVFloat 2.0, OVBool True, OVText "x"]
    mkOverlayRecord testSparseSchema vals `shouldSatisfy` isLeft

  it "mkOverlayRecordUnchecked always succeeds" $ do
    let rec = mkOverlayRecordUnchecked [OVFloat 1.0]
        (OverlayRecord v) = rec
    V.length v `shouldBe` 1

  it "matchesFieldType correctly classifies values" $ do
    matchesFieldType OFFloat (OVFloat 0) `shouldBe` True
    matchesFieldType OFInt   (OVInt 0)   `shouldBe` True
    matchesFieldType OFBool  (OVBool True) `shouldBe` True
    matchesFieldType OFText  (OVText "") `shouldBe` True
    matchesFieldType OFFloat (OVInt 0)   `shouldBe` False
    matchesFieldType OFInt   (OVFloat 0) `shouldBe` False

  it "setRecordFieldChecked accepts matching types" $ do
    let rec = defaultRecord testSparseSchema
    case setRecordFieldChecked testSparseSchema 0 (OVFloat 99.0) rec of
      Right (OverlayRecord v) -> v V.! 0 `shouldBe` OVFloat 99.0
      Left err -> expectationFailure ("unexpected rejection: " <> show err)

  it "setRecordFieldChecked rejects type mismatch" $ do
    let rec = defaultRecord testSparseSchema
    setRecordFieldChecked testSparseSchema 0 (OVInt 99) rec `shouldSatisfy` isLeft

  it "setRecordFieldChecked rejects out-of-range index" $ do
    let rec = defaultRecord testSparseSchema
    setRecordFieldChecked testSparseSchema 99 (OVFloat 1.0) rec `shouldSatisfy` isLeft
    setRecordFieldChecked testSparseSchema (-1) (OVFloat 1.0) rec `shouldSatisfy` isLeft

  it "defaultRecord produces values matching schema types" $
    property $ \(idx :: Int) ->
      let rec = defaultRecord testSparseSchema
          (OverlayRecord v) = rec
          fields = osFields testSparseSchema
          i = abs idx `mod` length fields
          fd = fields !! i
      in matchesFieldType (ofdType fd) (v V.! i)

  where
    isLeft (Left _) = True
    isLeft _        = False

------------------------------------------------------------------------
-- Overlay JSON transport
------------------------------------------------------------------------

overlayJsonSpec :: Spec
overlayJsonSpec = describe "Overlay.JSON" $ do

  it "round-trips sparse overlays" $ do
    let sparseChunk = OverlayChunk (IntMap.fromList [(2, testRecord)])
        overlay = Overlay
          { ovSchema = testSparseSchema
          , ovData = SparseData (IntMap.fromList [(7, sparseChunk)])
          }
        encoded = overlayToJSON overlay
    overlayFromJSON testSparseSchema encoded `shouldBe` Right overlay

  it "round-trips dense overlays" $ do
    let denseChunk = V.fromList
          [ U.fromList [0.1, 0.2, 0.3]
          , U.fromList [0.6, 0.5, 0.4]
          , U.fromList [0.0, 1.0, 0.0]
          ]
        overlay = Overlay
          { ovSchema = testDenseSchema
          , ovData = DenseData (IntMap.fromList [(3, denseChunk)])
          }
        encoded = overlayToJSON overlay
    overlayFromJSON testDenseSchema encoded `shouldBe` Right overlay

  it "rejects storage/schema mismatches" $ do
    let overlay = Overlay
          { ovSchema = testSparseSchema
          , ovData = SparseData IntMap.empty
          }
        encoded = overlayToJSON overlay
    overlayFromJSON testDenseSchema encoded `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False

------------------------------------------------------------------------
-- Export round-trip
------------------------------------------------------------------------

exportSpec :: Spec
exportSpec = describe "Overlay.Export" $ do

  it "sparse chunk round-trips through encode/decode" $ do
    let chunk = OverlayChunk $ IntMap.fromList
          [ (0, testRecord)
          , (5, defaultRecord testSparseSchema)
          ]
    let bytes = encodeSparseChunk testSparseSchema chunk
    case decodeSparseChunk testSparseSchema bytes of
      Left err -> expectationFailure ("decode failed: " <> show err)
      Right decoded -> decoded `shouldBe` chunk

  it "dense chunk round-trips through encode/decode" $ do
    let tileCount = 16
        fieldVecs = V.fromList
          [ U.generate tileCount (\i -> fromIntegral i * 0.1)  -- temperature
          , U.generate tileCount (\i -> fromIntegral i * 0.05) -- humidity
          , U.replicate tileCount 0.0                          -- wind_speed
          ]
    let bytes = encodeDenseChunk testDenseSchema fieldVecs
    case decodeDenseChunk testDenseSchema bytes of
      Left err -> expectationFailure ("decode failed: " <> show err)
      Right decoded -> do
        V.length decoded `shouldBe` 3
        U.length (decoded V.! 0) `shouldBe` tileCount

  it "exportOverlayChunks exports all sparse chunks" $ do
    let chunk0 = OverlayChunk $ IntMap.singleton 0 testRecord
        chunk1 = OverlayChunk $ IntMap.singleton 3 testRecord
        ov = Overlay
          { ovSchema = testSparseSchema
          , ovData = SparseData (IntMap.fromList [(0, chunk0), (1, chunk1)])
          }
    let exported = exportOverlayChunks ov
    length exported `shouldBe` 2
    map fst exported `shouldBe` [0, 1]

  it "sparse export round-trips at the chunk level" $
    property $ \(NonNegative tileIdx) -> tileIdx < (1000 :: Int) ==>
      let rec = OverlayRecord $ V.fromList
            [ OVFloat 3.14
            , OVInt (tileIdx `mod` 100)
            , OVBool (even tileIdx)
            , OVText "test"
            ]
          chunk = OverlayChunk (IntMap.singleton tileIdx rec)
          bytes = encodeSparseChunk testSparseSchema chunk
      in  case decodeSparseChunk testSparseSchema bytes of
            Left _  -> False
            Right c -> c == chunk

------------------------------------------------------------------------
-- Index
------------------------------------------------------------------------

indexSpec :: Spec
indexSpec = describe "Overlay.Index" $ do

  it "builds int index and looks up values" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 42, OVBool False, OVText ""]
        rec2 = OverlayRecord $ V.fromList [OVFloat 20, OVInt 42, OVBool True, OVText ""]
        rec3 = OverlayRecord $ V.fromList [OVFloat 30, OVInt 99, OVBool False, OVText ""]
        chunk = OverlayChunk $ IntMap.fromList [(0, rec1), (1, rec2), (2, rec3)]
        chunks = IntMap.singleton 0 chunk
        idx = buildIndices testSparseSchema chunks
    case idx of
      OverlayIndex m -> do
        -- culture_id is indexed at position 1
        case Map.lookup "culture_id" m of
          Just fi -> do
            lookupIntIndex 42 fi `shouldMatchList` [0, 1]
            lookupIntIndex 99 fi `shouldBe` [2]
            lookupIntIndex 0  fi `shouldBe` []
          Nothing -> expectationFailure "culture_id index not found"

  it "builds bool index and queries" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 1, OVBool True, OVText ""]
        rec2 = OverlayRecord $ V.fromList [OVFloat 20, OVInt 2, OVBool False, OVText ""]
        chunk = OverlayChunk $ IntMap.fromList [(0, rec1), (1, rec2)]
        chunks = IntMap.singleton 0 chunk
        idx = buildIndices testSparseSchema chunks
    case idx of
      OverlayIndex m ->
        case Map.lookup "is_capital" m of
          Just fi -> do
            lookupBoolIndex True fi `shouldBe` IntSet.fromList [0]
            lookupBoolIndex False fi `shouldBe` IntSet.fromList [1]
          Nothing -> expectationFailure "is_capital index not found"

  it "float index range query works" $ do
    -- Use the dense schema's humidity field (indexed)
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 1, OVBool False, OVText ""]
        rec2 = OverlayRecord $ V.fromList [OVFloat 50, OVInt 2, OVBool False, OVText ""]
        rec3 = OverlayRecord $ V.fromList [OVFloat 90, OVInt 3, OVBool False, OVText ""]
        -- Reuse sparse schema with population field (not indexed), culture_id (int indexed)
        -- We need a float-indexed field...
        schema = testSparseSchema
          { osFields =
              [ OverlayFieldDef "score" OFFloat (Number 0) True Nothing
              , OverlayFieldDef "id" OFInt (Number 0) False Nothing
              , OverlayFieldDef "flag" OFBool (Bool False) False Nothing
              , OverlayFieldDef "name" OFText (String "") False Nothing
              ]
          , osFieldIndex = Map.fromList [("score", 0), ("id", 1), ("flag", 2), ("name", 3)]
          }
        chunk = OverlayChunk $ IntMap.fromList [(0, rec1), (1, rec2), (2, rec3)]
        chunks = IntMap.singleton 0 chunk
        idx = buildIndices schema chunks
    case idx of
      OverlayIndex m ->
        case Map.lookup "score" m of
          Just fi -> do
            let results = queryFloatRange 0 100 fi
            length results `shouldBe` 3
            let midResults = queryFloatRange 20 80 fi
            length midResults `shouldBe` 1
          Nothing -> expectationFailure "score index not found"

------------------------------------------------------------------------
-- Schema migration
------------------------------------------------------------------------

migrationSpec :: Spec
migrationSpec = describe "Overlay.Storage migration" $ do

  it "adds new fields with defaults" $ do
    let oldSchema = testSparseSchema
        newSchema = testSparseSchema
          { osVersion = "2.0.0"
          , osFields = osFields testSparseSchema ++
              [ OverlayFieldDef "trade_score" OFFloat (Number 0.5) False Nothing ]
          , osFieldIndex = Map.fromList
              [ ("population", 0), ("culture_id", 1), ("is_capital", 2)
              , ("city_name", 3), ("trade_score", 4) ]
          }
        chunk = OverlayChunk $ IntMap.singleton 0 testRecord
        ovData' = SparseData (IntMap.singleton 0 chunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrAddedFields result `shouldBe` ["trade_score"]
    -- Check the migrated record has 5 fields with trade_score = 0.5
    case mrData result of
      SparseData chunks ->
        case IntMap.lookup 0 chunks >>= \(OverlayChunk m) -> IntMap.lookup 0 m of
          Just (OverlayRecord v) -> do
            V.length v `shouldBe` 5
            v V.! 4 `shouldBe` OVFloat 0.5
          Nothing -> expectationFailure "record not found after migration"
      DenseData _ -> expectationFailure "expected SparseData"

  it "drops removed fields" $ do
    let oldSchema = testSparseSchema
        newSchema = testSparseSchema
          { osVersion = "2.0.0"
          , osFields = take 2 (osFields testSparseSchema) -- keep population + culture_id
          , osFieldIndex = Map.fromList [("population", 0), ("culture_id", 1)]
          }
        chunk = OverlayChunk $ IntMap.singleton 0 testRecord
        ovData' = SparseData (IntMap.singleton 0 chunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrDroppedFields result `shouldMatchList` ["is_capital", "city_name"]
    case mrData result of
      SparseData chunks ->
        case IntMap.lookup 0 chunks >>= \(OverlayChunk m) -> IntMap.lookup 0 m of
          Just (OverlayRecord v) -> V.length v `shouldBe` 2
          Nothing -> expectationFailure "record not found"
      DenseData _ -> expectationFailure "expected SparseData"

  it "renames fields via renamed_from" $ do
    let oldSchema = testSparseSchema
        newSchema = testSparseSchema
          { osVersion = "2.0.0"
          , osFields =
              [ OverlayFieldDef "pop" OFFloat (Number 0) False (Just "population")
              , (osFields testSparseSchema !! 1) -- culture_id
              , (osFields testSparseSchema !! 2) -- is_capital
              , (osFields testSparseSchema !! 3) -- city_name
              ]
          , osFieldIndex = Map.fromList
              [("pop", 0), ("culture_id", 1), ("is_capital", 2), ("city_name", 3)]
          }
        chunk = OverlayChunk $ IntMap.singleton 0 testRecord
        ovData' = SparseData (IntMap.singleton 0 chunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrRenamedFields result `shouldBe` [("population", "pop")]
    case mrData result of
      SparseData chunks ->
        case IntMap.lookup 0 chunks >>= \(OverlayChunk m) -> IntMap.lookup 0 m of
          Just (OverlayRecord v) -> do
            v V.! 0 `shouldBe` OVFloat 1234.5  -- preserved value
          Nothing -> expectationFailure "record not found"
      DenseData _ -> expectationFailure "expected SparseData"

  it "coerces int to float during migration" $ do
    let oldSchema = testSparseSchema
        newSchema = testSparseSchema
          { osVersion = "2.0.0"
          , osFields =
              [ (osFields testSparseSchema !! 0) -- population (float)
              , (osFields testSparseSchema !! 1) { ofdType = OFFloat } -- culture_id: int -> float
              , (osFields testSparseSchema !! 2) -- is_capital
              , (osFields testSparseSchema !! 3) -- city_name
              ]
          }
        chunk = OverlayChunk $ IntMap.singleton 0 testRecord
        ovData' = SparseData (IntMap.singleton 0 chunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    case mrData result of
      SparseData chunks ->
        case IntMap.lookup 0 chunks >>= \(OverlayChunk m) -> IntMap.lookup 0 m of
          Just (OverlayRecord v) ->
            v V.! 1 `shouldBe` OVFloat 42.0  -- coerced from OVInt 42
          Nothing -> expectationFailure "record not found"
      DenseData _ -> expectationFailure "expected SparseData"

  -- Dense migration tests (Phase 4)

  it "dense migration preserves data when fields are reordered" $ do
    let oldSchema = testDenseSchema  -- temperature, humidity, wind_speed
        newSchema = testDenseSchema
          { osVersion = "2.0.0"
          , osFields =
              [ osFields testDenseSchema !! 2  -- wind_speed first
              , osFields testDenseSchema !! 0  -- temperature second
              , osFields testDenseSchema !! 1  -- humidity third
              ]
          , osFieldIndex = Map.fromList
              [("wind_speed", 0), ("temperature", 1), ("humidity", 2)]
          }
        -- Old chunk: 3 fields, 4 tiles each
        oldChunk = V.fromList
          [ U.fromList [10.0, 20.0, 30.0, 40.0]   -- temperature
          , U.fromList [0.5, 0.6, 0.7, 0.8]        -- humidity
          , U.fromList [1.0, 2.0, 3.0, 4.0]        -- wind_speed
          ]
        ovData' = DenseData (IntMap.singleton 0 oldChunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrNeedsRepopulation result `shouldBe` False
    case mrData result of
      DenseData chunks -> case IntMap.lookup 0 chunks of
        Just vecs -> do
          V.length vecs `shouldBe` 3
          -- wind_speed should now be first
          U.toList (vecs V.! 0) `shouldBe` [1.0, 2.0, 3.0, 4.0]
          -- temperature second
          U.toList (vecs V.! 1) `shouldBe` [10.0, 20.0, 30.0, 40.0]
          -- humidity third
          U.toList (vecs V.! 2) `shouldBe` [0.5, 0.6, 0.7, 0.8]
        Nothing -> expectationFailure "chunk not found after dense migration"
      SparseData _ -> expectationFailure "expected DenseData"

  it "dense migration adds new fields with defaults" $ do
    let oldSchema = testDenseSchema
        newSchema = testDenseSchema
          { osVersion = "2.0.0"
          , osFields = osFields testDenseSchema ++
              [ OverlayFieldDef "pressure" OFFloat (Number 1.0) False Nothing ]
          , osFieldIndex = Map.fromList
              [ ("temperature", 0), ("humidity", 1)
              , ("wind_speed", 2), ("pressure", 3) ]
          }
        oldChunk = V.fromList
          [ U.fromList [10.0, 20.0]
          , U.fromList [0.5, 0.6]
          , U.fromList [1.0, 2.0]
          ]
        ovData' = DenseData (IntMap.singleton 0 oldChunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrAddedFields result `shouldBe` ["pressure"]
    mrNeedsRepopulation result `shouldBe` False
    case mrData result of
      DenseData chunks -> case IntMap.lookup 0 chunks of
        Just vecs -> do
          V.length vecs `shouldBe` 4
          -- Original fields preserved
          U.toList (vecs V.! 0) `shouldBe` [10.0, 20.0]
          -- New field filled with default 1.0
          U.toList (vecs V.! 3) `shouldBe` [1.0, 1.0]
        Nothing -> expectationFailure "chunk not found"
      SparseData _ -> expectationFailure "expected DenseData"

  it "dense migration drops removed fields" $ do
    let oldSchema = testDenseSchema
        newSchema = testDenseSchema
          { osVersion = "2.0.0"
          , osFields = take 2 (osFields testDenseSchema)  -- keep temp + humidity
          , osFieldIndex = Map.fromList [("temperature", 0), ("humidity", 1)]
          }
        oldChunk = V.fromList
          [ U.fromList [10.0, 20.0]
          , U.fromList [0.5, 0.6]
          , U.fromList [1.0, 2.0]
          ]
        ovData' = DenseData (IntMap.singleton 0 oldChunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrDroppedFields result `shouldBe` ["wind_speed"]
    mrNeedsRepopulation result `shouldBe` False
    case mrData result of
      DenseData chunks -> case IntMap.lookup 0 chunks of
        Just vecs -> do
          V.length vecs `shouldBe` 2
          U.toList (vecs V.! 0) `shouldBe` [10.0, 20.0]
          U.toList (vecs V.! 1) `shouldBe` [0.5, 0.6]
        Nothing -> expectationFailure "chunk not found"
      SparseData _ -> expectationFailure "expected DenseData"

  it "dense migration handles renamed fields" $ do
    let oldSchema = testDenseSchema
        newSchema = testDenseSchema
          { osVersion = "2.0.0"
          , osFields =
              [ OverlayFieldDef "temp_c" OFFloat (Number 0.5) False (Just "temperature")
              , osFields testDenseSchema !! 1  -- humidity
              , osFields testDenseSchema !! 2  -- wind_speed
              ]
          , osFieldIndex = Map.fromList
              [("temp_c", 0), ("humidity", 1), ("wind_speed", 2)]
          }
        oldChunk = V.fromList
          [ U.fromList [10.0, 20.0]
          , U.fromList [0.5, 0.6]
          , U.fromList [1.0, 2.0]
          ]
        ovData' = DenseData (IntMap.singleton 0 oldChunk)
        result = migrateOverlayData oldSchema newSchema ovData'
    mrRenamedFields result `shouldBe` [("temperature", "temp_c")]
    case mrData result of
      DenseData chunks -> case IntMap.lookup 0 chunks of
        Just vecs -> do
          -- Renamed field should carry old data
          U.toList (vecs V.! 0) `shouldBe` [10.0, 20.0]
        Nothing -> expectationFailure "chunk not found"
      SparseData _ -> expectationFailure "expected DenseData"

  it "dense migration with empty chunks produces empty result" $ do
    let oldSchema = testDenseSchema
        newSchema = testDenseSchema { osVersion = "2.0.0" }
        ovData' = DenseData IntMap.empty
        result = migrateOverlayData oldSchema newSchema ovData'
    mrNeedsRepopulation result `shouldBe` False
    case mrData result of
      DenseData chunks -> IntMap.size chunks `shouldBe` 0
      SparseData _ -> expectationFailure "expected DenseData"

  it "reports OverlayNeedsRepopulation for incompatible dense migrations" $ do
    let oldSchema = testDenseSchema
        renamedAsText = OverlayFieldDef "temperature" OFText (String "") False Nothing
        newSchema = testDenseSchema
          { osVersion = "2.0.0"
          , osFields = renamedAsText : drop 1 (osFields testDenseSchema)
          , osFieldIndex = Map.fromList
              [ ("temperature", 0)
              , ("humidity", 1)
              , ("wind_speed", 2)
              ]
          }
        oldChunk = V.fromList
          [ U.fromList [10.0, 20.0]
          , U.fromList [0.5, 0.6]
          , U.fromList [1.0, 2.0]
          ]
        result = migrateOverlayData oldSchema newSchema (DenseData (IntMap.singleton 0 oldChunk))
    mrNeedsRepopulation result `shouldBe` True
    migrationLifecycle result `shouldBe` OverlayNeedsRepopulation
    migrationWarnings newSchema result `shouldSatisfy` (not . null)

  it "reports OverlayActive for compatible migrations" $ do
    let result = migrateOverlayData testDenseSchema testDenseSchema (DenseData IntMap.empty)
    migrationLifecycle result `shouldBe` OverlayActive
    migrationWarnings testDenseSchema result `shouldBe` []

------------------------------------------------------------------------
-- Overlay store
------------------------------------------------------------------------

storeSpec :: Spec
storeSpec = describe "OverlayStore" $ do

  it "starts empty" $ do
    overlayCount emptyOverlayStore `shouldBe` 0
    overlayNames emptyOverlayStore `shouldBe` []

  it "insert and lookup work" $ do
    let ov = emptyOverlay testSparseSchema
        store = insertOverlay ov emptyOverlayStore
    overlayCount store `shouldBe` 1
    case lookupOverlay "test-sparse" store of
      Just o  -> overlayName o `shouldBe` "test-sparse"
      Nothing -> expectationFailure "overlay not found"

  it "overlay manifest defaults to empty list" $ do
    -- The overlay manifest in Storage v17 stores [Text]
    -- Verify our test world has empty manifest
    overlayNames emptyOverlayStore `shouldBe` []

------------------------------------------------------------------------
-- Indexed overlay
------------------------------------------------------------------------

indexedSpec :: Spec
indexedSpec = describe "Overlay.Indexed" $ do

  it "mkIndexed starts with dirty index" $ do
    let io = mkIndexed (emptyOverlay testSparseSchema)
    ioIndex io `shouldSatisfy` isNothing

  it "mkIndexedFresh starts with built index" $ do
    let io = mkIndexedFresh (emptyOverlay testSparseSchema)
    ioIndex io `shouldSatisfy` \case Just _ -> True; Nothing -> False

  it "ensureIndex builds and caches index" $ do
    let io = mkIndexed (emptyOverlay testSparseSchema)
        (io', _idx) = ensureIndex io
    ioIndex io' `shouldSatisfy` \case Just _ -> True; Nothing -> False

  it "invalidateIndex marks index dirty" $ do
    let io  = mkIndexedFresh (emptyOverlay testSparseSchema)
        io' = invalidateIndex io
    ioIndex io' `shouldSatisfy` isNothing

  it "insertSparseRecord invalidates index" $ do
    let io  = mkIndexedFresh (emptyOverlay testSparseSchema)
        io' = insertSparseRecord 0 1 testRecord io
    ioIndex io' `shouldSatisfy` isNothing

  it "deleteSparseRecord invalidates index" $ do
    let io  = mkIndexedFresh (emptyOverlay testSparseSchema)
        io' = insertSparseRecord 0 1 testRecord io
        io'' = deleteSparseRecord 0 1 io'
    ioIndex io'' `shouldSatisfy` isNothing

  it "insert then query int field returns correct tiles" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 5, OVBool True, OVText "A"]
        rec2 = OverlayRecord $ V.fromList [OVFloat 20, OVInt 5, OVBool False, OVText "B"]
        rec3 = OverlayRecord $ V.fromList [OVFloat 30, OVInt 9, OVBool True, OVText "C"]
        io = mkIndexed (emptyOverlay testSparseSchema)
        io1 = insertSparseRecord 0 10 rec1 io
        io2 = insertSparseRecord 0 20 rec2 io1
        io3 = insertSparseRecord 0 30 rec3 io2
        (io4, hits) = Indexed.queryIntField "culture_id" 5 io3
    -- Tiles 10 and 20 both have culture_id = 5
    length hits `shouldBe` 2
    hits `shouldSatisfy` (10 `elem`)
    hits `shouldSatisfy` (20 `elem`)
    -- Index should now be cached
    ioIndex io4 `shouldSatisfy` \case Just _ -> True; Nothing -> False

  it "insert then query float range returns correct tiles" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 1, OVBool True, OVText ""]
        rec2 = OverlayRecord $ V.fromList [OVFloat 50, OVInt 2, OVBool True, OVText ""]
        rec3 = OverlayRecord $ V.fromList [OVFloat 90, OVInt 3, OVBool True, OVText ""]
        io = mkIndexed (emptyOverlay testSparseSchema)
        io1 = insertSparseRecord 0 1 rec1 io
        io2 = insertSparseRecord 0 2 rec2 io1
        io3 = insertSparseRecord 0 3 rec3 io2
        -- population field is not indexed, so this should return empty
        (_io4, hits) = Indexed.queryFloatRange "population" 5 55 io3
    -- population is not indexed (ofdIndexed = False), expect no results
    hits `shouldBe` []

  it "insert then query bool field returns correct tiles" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 1, OVBool True, OVText ""]
        rec2 = OverlayRecord $ V.fromList [OVFloat 20, OVInt 2, OVBool False, OVText ""]
        rec3 = OverlayRecord $ V.fromList [OVFloat 30, OVInt 3, OVBool True, OVText ""]
        io = mkIndexed (emptyOverlay testSparseSchema)
        io1 = insertSparseRecord 0 10 rec1 io
        io2 = insertSparseRecord 0 20 rec2 io1
        io3 = insertSparseRecord 0 30 rec3 io2
        (_io4, trueSet) = Indexed.queryBoolField "is_capital" True io3
    IntSet.toList trueSet `shouldSatisfy` (10 `elem`)
    IntSet.toList trueSet `shouldSatisfy` (30 `elem`)
    IntSet.size trueSet `shouldBe` 2

  it "delete then query reflects removal" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 5, OVBool True, OVText "A"]
        rec2 = OverlayRecord $ V.fromList [OVFloat 20, OVInt 5, OVBool False, OVText "B"]
        io = mkIndexed (emptyOverlay testSparseSchema)
        io1 = insertSparseRecord 0 10 rec1 io
        io2 = insertSparseRecord 0 20 rec2 io1
        -- Delete tile 10
        io3 = deleteSparseRecord 0 10 io2
        (_io4, hits) = Indexed.queryIntField "culture_id" 5 io3
    -- Only tile 20 should remain
    hits `shouldBe` [20]

  it "dense overlay queries return empty (no indexing)" $ do
    let denseOv = emptyOverlay testDenseSchema
        io = mkIndexedFresh denseOv
        (_io', hits) = Indexed.queryIntField "temperature" 0 io
    hits `shouldBe` []

  it "multiple queries reuse cached index" $ do
    let rec1 = OverlayRecord $ V.fromList [OVFloat 10, OVInt 5, OVBool True, OVText ""]
        io = mkIndexed (emptyOverlay testSparseSchema)
        io1 = insertSparseRecord 0 1 rec1 io
        (io2, _) = Indexed.queryIntField "culture_id" 5 io1
        -- Second query should hit the cache (index is Just)
        (io3, hits) = Indexed.queryIntField "culture_id" 5 io2
    ioIndex io3 `shouldSatisfy` \case Just _ -> True; Nothing -> False
    hits `shouldBe` [1]
