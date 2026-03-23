{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Topo.MCP.Server' — JSON-RPC request parsing helpers.
module Spec.Server (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Hspec

import Topo.MCP.Server (parseToolCall, parseResourceUri)

spec :: Spec
spec = describe "Topo.MCP.Server" $ do

  -- -----------------------------------------------------------------
  -- parseToolCall
  -- -----------------------------------------------------------------
  describe "parseToolCall" $ do
    it "parses a valid tools/call params with name and arguments" $ do
      let params = object
            [ "name" .= ("get_state" :: Text)
            , "arguments" .= object ["key" .= ("val" :: Text)]
            ]
      case parseToolCall params of
        Just (name, args) -> do
          name `shouldBe` "get_state"
          args `shouldBe` object ["key" .= ("val" :: Text)]
        Nothing -> expectationFailure "expected successful parse"

    it "defaults arguments to empty object when absent" $ do
      let params = object ["name" .= ("get_state" :: Text)]
      case parseToolCall params of
        Just (name, args) -> do
          name `shouldBe` "get_state"
          args `shouldBe` Object mempty
        Nothing -> expectationFailure "expected successful parse"

    it "returns Nothing when name is missing" $ do
      let params = object ["arguments" .= object []]
      parseToolCall params `shouldBe` Nothing

    it "returns Nothing for non-object input" $ do
      parseToolCall (String "not an object") `shouldBe` Nothing

    it "returns Nothing for Null" $ do
      parseToolCall Null `shouldBe` Nothing

  -- -----------------------------------------------------------------
  -- parseResourceUri (params parser)
  -- -----------------------------------------------------------------
  describe "parseResourceUri" $ do
    it "parses a valid resources/read params" $ do
      let params = object ["uri" .= ("topo://state" :: Text)]
      parseResourceUri params `shouldBe` Just "topo://state"

    it "returns Nothing when uri is missing" $ do
      parseResourceUri (object []) `shouldBe` Nothing

    it "returns Nothing for non-object input" $ do
      parseResourceUri (String "not an object") `shouldBe` Nothing

    it "returns Nothing for Null" $ do
      parseResourceUri Null `shouldBe` Nothing
