{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.DataResource (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Aeson (encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Plugin.DataResource

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> listOf1 (elements (['a'..'z'] <> ['0'..'9'] <> ['_']))
  shrink t = map Text.pack . shrink $ Text.unpack t

instance Arbitrary DataFieldType where
  arbitrary = oneof
    [ pure DFText
    , pure DFInt
    , pure DFFloat
    , pure DFDouble
    , pure DFBool
    , pure DFFixed2
    , pure DFFixed3
    , pure DFFixed4
    , DFEnum <$> listOf1 arbitrary
    ]

instance Arbitrary DataConstructorDef where
  arbitrary = DataConstructorDef
    <$> arbitrary
    <*> listOf (oneof [pure DFText, pure DFInt, pure DFFloat, pure DFBool])

instance Arbitrary DataFieldDef where
  arbitrary = DataFieldDef
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure Nothing

instance Arbitrary DataOperations where
  arbitrary = DataOperations
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DataResourceSchema where
  arbitrary = do
    name <- arbitrary
    label <- arbitrary
    hexBound <- arbitrary
    keyName <- arbitrary
    let keyField = DataFieldDef keyName DFText "Key" False Nothing
    extraFields <- listOf arbitrary
    ops <- arbitrary
    ov <- arbitrary
    pure DataResourceSchema
      { drsName       = name
      , drsLabel      = label
      , drsHexBound   = hexBound
      , drsFields     = keyField : extraFields
      , drsOperations = ops
      , drsKeyField   = keyName
      , drsOverlay    = ov
      }

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Topo.Plugin.DataResource" $ do

  describe "DataFieldType JSON" $ do
    it "round-trips scalar types" $ do
      let scalars = [DFText, DFInt, DFFloat, DFDouble, DFBool, DFFixed2, DFFixed3, DFFixed4]
      mapM_ (\ft -> eitherDecode (encode ft) `shouldBe` Right ft) scalars

    it "round-trips enum type" $ do
      let ft = DFEnum ["red", "green", "blue"]
      eitherDecode (encode ft) `shouldBe` Right ft

    it "round-trips record type" $ do
      let ft = DFRecord [DataFieldDef "x" DFFloat "X" False Nothing]
      eitherDecode (encode ft) `shouldBe` Right ft

    it "round-trips ADT type" $ do
      let ft = DFAdt [DataConstructorDef "Circle" [DFFloat], DataConstructorDef "Rect" [DFFloat, DFFloat]]
      eitherDecode (encode ft) `shouldBe` Right ft

    prop "arbitrary DataFieldType round-trips" $ \(ft :: DataFieldType) ->
      eitherDecode (encode ft) === Right ft

  describe "DataFieldDef JSON" $ do
    prop "arbitrary DataFieldDef round-trips" $ \(fd :: DataFieldDef) ->
      eitherDecode (encode fd) === Right fd

  describe "DataConstructorDef JSON" $ do
    prop "arbitrary DataConstructorDef round-trips" $ \(dcd :: DataConstructorDef) ->
      eitherDecode (encode dcd) === Right dcd

  describe "DataOperations JSON" $ do
    it "round-trips noOperations" $ do
      eitherDecode (encode noOperations) `shouldBe` Right noOperations

    it "round-trips allOperations" $ do
      eitherDecode (encode allOperations) `shouldBe` Right allOperations

    prop "arbitrary DataOperations round-trips" $ \(ops :: DataOperations) ->
      eitherDecode (encode ops) === Right ops

  describe "DataResourceSchema JSON" $ do
    prop "arbitrary DataResourceSchema round-trips" $ \(drs :: DataResourceSchema) ->
      eitherDecode (encode drs) === Right drs

  describe "Validation" $ do
    let validSchema = DataResourceSchema
          { drsName       = "cultures"
          , drsLabel      = "Cultures"
          , drsHexBound   = False
          , drsFields     = [DataFieldDef "id" DFInt "ID" False Nothing
                            ,DataFieldDef "name" DFText "Name" True Nothing]
          , drsOperations = allOperations { doQueryByHex = False }
          , drsKeyField   = "id"
          , drsOverlay    = Nothing
          }

    it "accepts a valid schema" $ do
      validateDataResource validSchema `shouldBe` []

    it "rejects empty name" $ do
      let bad = validSchema { drsName = "" }
      validateDataResource bad `shouldSatisfy` (not . null)

    it "rejects empty label" $ do
      let bad = validSchema { drsLabel = "" }
      validateDataResource bad `shouldSatisfy` (not . null)

    it "rejects empty fields" $ do
      let bad = validSchema { drsFields = [] }
      validateDataResource bad `shouldSatisfy` (not . null)

    it "rejects missing key field" $ do
      let bad = validSchema { drsKeyField = "nonexistent" }
      validateDataResource bad `shouldSatisfy` (not . null)

    it "rejects queryByHex without hexBound" $ do
      let bad = validSchema { drsHexBound = False
                            , drsOperations = allOperations }
      validateDataResource bad `shouldSatisfy` (not . null)

    it "accepts queryByHex with hexBound" $ do
      let good = validSchema { drsHexBound = True
                             , drsOperations = allOperations }
      validateDataResource good `shouldBe` []

  describe "helpers" $ do
    it "isScalarType returns True for scalars" $ do
      mapM_ (\ft -> isScalarType ft `shouldBe` True)
        [DFText, DFInt, DFFloat, DFDouble, DFBool, DFFixed2, DFFixed3, DFFixed4]

    it "isScalarType returns False for composites" $ do
      isScalarType (DFEnum ["a"]) `shouldBe` False
      isScalarType (DFRecord []) `shouldBe` False
      isScalarType (DFAdt []) `shouldBe` False

    it "fixedDecimalPlaces returns correct values" $ do
      fixedDecimalPlaces DFFixed2 `shouldBe` Just 2
      fixedDecimalPlaces DFFixed3 `shouldBe` Just 3
      fixedDecimalPlaces DFFixed4 `shouldBe` Just 4
      fixedDecimalPlaces DFFloat `shouldBe` Nothing
