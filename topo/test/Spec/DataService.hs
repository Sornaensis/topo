{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.DataService (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Aeson (Value(..), encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Plugin.RPC.DataService

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> listOf1 (elements (['a'..'z'] <> ['0'..'9'] <> ['_']))
  shrink t = map Text.pack . shrink $ Text.unpack t

instance Arbitrary DataRecord where
  arbitrary = do
    n <- chooseInt (1, 5)
    keys <- vectorOf n arbitrary
    vals <- vectorOf n genSimpleValue
    pure . DataRecord $ Map.fromList (zip keys vals)

genSimpleValue :: Gen Value
genSimpleValue = oneof
  [ String <$> arbitrary
  , Number . fromIntegral <$> (arbitrary :: Gen Int)
  , Bool <$> arbitrary
  , pure Null
  ]

instance Arbitrary DataQuery where
  arbitrary = oneof
    [ pure QueryAll
    , QueryByKey <$> genSimpleValue
    , QueryByHex <$> arbitrary <*> arbitrary
    , QueryByField <$> arbitrary <*> genSimpleValue
    ]

instance Arbitrary QueryResource where
  arbitrary = QueryResource
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary QueryResult where
  arbitrary = QueryResult
    <$> arbitrary
    <*> listOf arbitrary
    <*> arbitrary

instance Arbitrary DataMutation where
  arbitrary = oneof
    [ MutCreate <$> arbitrary
    , MutUpdate <$> genSimpleValue <*> arbitrary
    , MutDelete <$> genSimpleValue
    , MutSetHex <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary MutateResource where
  arbitrary = MutateResource
    <$> arbitrary
    <*> arbitrary

instance Arbitrary MutateResult where
  arbitrary = MutateResult
    <$> arbitrary
    <*> oneof [pure Nothing, Just <$> arbitrary]
    <*> oneof [pure Nothing, Just <$> arbitrary]

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Topo.Plugin.RPC.DataService" $ do

  describe "DataRecord JSON" $ do
    it "round-trips a simple record" $ do
      let rec = DataRecord (Map.fromList [("id", Number 1), ("name", String "test")])
      eitherDecode (encode rec) `shouldBe` Right rec

    prop "arbitrary DataRecord round-trips" $ \(rec :: DataRecord) ->
      eitherDecode (encode rec) === Right rec

  describe "DataQuery JSON" $ do
    it "round-trips QueryAll" $ do
      eitherDecode (encode QueryAll) `shouldBe` Right QueryAll

    it "round-trips QueryByKey" $ do
      let q = QueryByKey (Number 42)
      eitherDecode (encode q) `shouldBe` Right q

    it "round-trips QueryByHex" $ do
      let q = QueryByHex 3 17
      eitherDecode (encode q) `shouldBe` Right q

    it "round-trips QueryByField" $ do
      let q = QueryByField "status" (String "active")
      eitherDecode (encode q) `shouldBe` Right q

    prop "arbitrary DataQuery round-trips" $ \(q :: DataQuery) ->
      eitherDecode (encode q) === Right q

  describe "QueryResource JSON" $ do
    prop "arbitrary QueryResource round-trips" $ \(qr :: QueryResource) ->
      eitherDecode (encode qr) === Right qr

  describe "QueryResult JSON" $ do
    prop "arbitrary QueryResult round-trips" $ \(qr :: QueryResult) ->
      eitherDecode (encode qr) === Right qr

  describe "DataMutation JSON" $ do
    it "round-trips MutCreate" $ do
      let m = MutCreate (DataRecord (Map.fromList [("name", String "new")]))
      eitherDecode (encode m) `shouldBe` Right m

    it "round-trips MutUpdate" $ do
      let m = MutUpdate (Number 1) (DataRecord (Map.fromList [("name", String "upd")]))
      eitherDecode (encode m) `shouldBe` Right m

    it "round-trips MutDelete" $ do
      let m = MutDelete (Number 1)
      eitherDecode (encode m) `shouldBe` Right m

    it "round-trips MutSetHex" $ do
      let m = MutSetHex 5 10 (DataRecord (Map.fromList [("pop", Number 100)]))
      eitherDecode (encode m) `shouldBe` Right m

    prop "arbitrary DataMutation round-trips" $ \(m :: DataMutation) ->
      eitherDecode (encode m) === Right m

  describe "MutateResource JSON" $ do
    prop "arbitrary MutateResource round-trips" $ \(mr :: MutateResource) ->
      eitherDecode (encode mr) === Right mr

  describe "MutateResult JSON" $ do
    it "round-trips success" $ do
      let r = MutateResult True Nothing (Just (DataRecord (Map.fromList [("id", Number 1)])))
      eitherDecode (encode r) `shouldBe` Right r

    it "round-trips failure" $ do
      let r = MutateResult False (Just "not found") Nothing
      eitherDecode (encode r) `shouldBe` Right r

    prop "arbitrary MutateResult round-trips" $ \(mr :: MutateResult) ->
      eitherDecode (encode mr) === Right mr
