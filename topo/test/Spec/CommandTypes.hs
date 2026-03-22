{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.CommandTypes (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Aeson (Value(..), (.=), object, encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Command.Types

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary SeerCommand where
  arbitrary = SeerCommand
    <$> arbitrary
    <*> genMethodName
    <*> genParams

instance Arbitrary SeerResponse where
  arbitrary = oneof
    [ okResponse <$> arbitrary <*> genParams
    , errResponse <$> arbitrary <*> genErrorMsg
    ]

genMethodName :: Gen Text
genMethodName = elements
  [ "get_state", "get_sliders", "get_slider", "get_view_modes"
  , "set_seed", "set_slider", "set_view_mode", "set_config_tab"
  ]

genParams :: Gen Value
genParams = oneof
  [ pure (object [])
  , object . (: []) <$> ((.=) <$> genKey <*> (arbitrary :: Gen Int))
  ]
  where
    genKey = elements ["name", "value", "seed", "mode", "tab"]

genErrorMsg :: Gen Text
genErrorMsg = elements
  [ "unknown command", "missing parameter", "invalid value"
  , "slider not found", "internal error"
  ]

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Topo.Command.Types" $ do

  describe "SeerCommand JSON" $ do

    it "encodes a minimal command" $ do
      let cmd = SeerCommand 1 "get_state" (object [])
          json = Aeson.decode (encode cmd) :: Maybe Value
      json `shouldBe` Just (object
        [ "id"     .= (1 :: Int)
        , "method" .= ("get_state" :: Text)
        , "params" .= object []
        ])

    it "round-trips with params" $ do
      let cmd = SeerCommand 42 "set_slider" (object
            [ "name"  .= ("SliderSeaLevel" :: Text)
            , "value" .= (0.5 :: Double)
            ])
      let decoded = eitherDecode (encode cmd) :: Either String SeerCommand
      decoded `shouldBe` Right cmd

    it "rejects missing method field" $ do
      let json = "{\"id\": 1, \"params\": {}}" :: BL.ByteString
      let decoded = eitherDecode json :: Either String SeerCommand
      decoded `shouldSatisfy` isLeft

    it "rejects missing id field" $ do
      let json = "{\"method\": \"get_state\", \"params\": {}}" :: BL.ByteString
      let decoded = eitherDecode json :: Either String SeerCommand
      decoded `shouldSatisfy` isLeft

    prop "round-trips through JSON" $ \(cmd :: SeerCommand) ->
      eitherDecode (encode cmd) === Right cmd

  describe "SeerResponse JSON" $ do

    it "encodes a success response" $ do
      let rsp = okResponse 1 (object ["seed" .= (42 :: Int)])
          json = Aeson.decode (encode rsp) :: Maybe Value
      json `shouldBe` Just (object
        [ "id"      .= (1 :: Int)
        , "success" .= True
        , "result"  .= object ["seed" .= (42 :: Int)]
        , "error"   .= Null
        ])

    it "encodes an error response" $ do
      let rsp = errResponse 2 "slider not found"
          json = Aeson.decode (encode rsp) :: Maybe Value
      json `shouldBe` Just (object
        [ "id"      .= (2 :: Int)
        , "success" .= False
        , "result"  .= Null
        , "error"   .= ("slider not found" :: Text)
        ])

    it "round-trips a success response" $ do
      let rsp = okResponse 7 (object ["view_mode" .= ("biome" :: Text)])
      let decoded = eitherDecode (encode rsp) :: Either String SeerResponse
      decoded `shouldBe` Right rsp

    it "round-trips an error response" $ do
      let rsp = errResponse 8 "internal error"
      let decoded = eitherDecode (encode rsp) :: Either String SeerResponse
      decoded `shouldBe` Right rsp

    prop "round-trips through JSON" $ \(rsp :: SeerResponse) ->
      eitherDecode (encode rsp) === Right rsp

  describe "okResponse / errResponse helpers" $ do

    it "okResponse sets success=True and error=Nothing" $ do
      let rsp = okResponse 1 (object [])
      srSuccess rsp `shouldBe` True
      srError rsp `shouldBe` Nothing

    it "errResponse sets success=False and result=Null" $ do
      let rsp = errResponse 1 "fail"
      srSuccess rsp `shouldBe` False
      srResult rsp `shouldBe` Null
      srError rsp `shouldBe` Just "fail"

  describe "commandPipeName" $ do

    it "returns a non-empty string" $
      commandPipeName `shouldSatisfy` (not . null)

    it "contains topo-seer-cmd" $
      commandPipeName `shouldSatisfy` \p -> "topo-seer-cmd" `isInfixOf` p
  where
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
