{-# LANGUAGE OverloadedStrings #-}

-- | JSON encoding tests for MCP protocol types.
--
-- Since these types are the wire format for the MCP JSON-RPC protocol,
-- we verify that 'ToJSON' instances produce the expected JSON shapes
-- and that the 'FromJSON' instance for 'JsonRpcRequest' parses
-- correctly.
module Spec.Types (spec) where

import Data.Aeson (Value(..), object, (.=), encode, decode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Hspec

import Topo.MCP.Types

spec :: Spec
spec = describe "Topo.MCP.Types" $ do

  -- -----------------------------------------------------------------
  -- JsonRpcRequest (FromJSON)
  -- -----------------------------------------------------------------
  describe "JsonRpcRequest" $ do
    it "parses a full request" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id"      .= (1 :: Int)
            , "method"  .= ("tools/call" :: Text)
            , "params"  .= object ["name" .= ("get_state" :: Text)]
            ]
      case Aeson.fromJSON json of
        Aeson.Success req -> do
          jrpcId req     `shouldBe` Just (Number 1)
          jrpcMethod req `shouldBe` "tools/call"
        Aeson.Error err -> expectationFailure err

    it "parses a notification (no id)" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method"  .= ("initialized" :: Text)
            ]
      case Aeson.fromJSON json of
        Aeson.Success req -> do
          jrpcId req     `shouldBe` Nothing
          jrpcMethod req `shouldBe` "initialized"
          jrpcParams req `shouldBe` Null
        Aeson.Error err -> expectationFailure err

    it "defaults params to Null when absent" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id"      .= (5 :: Int)
            , "method"  .= ("ping" :: Text)
            ]
      case Aeson.fromJSON json of
        Aeson.Success req -> jrpcParams req `shouldBe` Null
        Aeson.Error err   -> expectationFailure err

  -- -----------------------------------------------------------------
  -- JsonRpcResponse (ToJSON)
  -- -----------------------------------------------------------------
  describe "JsonRpcResponse" $ do
    it "encodes a success response with result" $ do
      let rsp = mkResult (Number 1) (object ["ok" .= True])
          json = Aeson.toJSON rsp
      lookupKey "jsonrpc" json `shouldBe` Just (String "2.0")
      lookupKey "id"      json `shouldBe` Just (Number 1)
      lookupKey "result"  json `shouldSatisfy` isJust
      lookupKey "error"   json `shouldSatisfy` isNothing

    it "encodes an error response with error object" $ do
      let rsp = mkError (Number 2) (-32601) "Method not found"
          json = Aeson.toJSON rsp
      lookupKey "jsonrpc" json `shouldBe` Just (String "2.0")
      lookupKey "id"      json `shouldBe` Just (Number 2)
      lookupKey "error"   json `shouldSatisfy` isJust
      lookupKey "result"  json `shouldSatisfy` isNothing
      -- Check error sub-fields
      case lookupKey "error" json of
        Just (Object errObj) -> do
          KM.lookup "code"    errObj `shouldBe` Just (Number (-32601))
          KM.lookup "message" errObj `shouldBe` Just (String "Method not found")
        _ -> expectationFailure "expected error object"

  -- -----------------------------------------------------------------
  -- mkResult / mkError
  -- -----------------------------------------------------------------
  describe "mkResult / mkError" $ do
    it "mkResult sets result and no error" $ do
      let rsp = mkResult (Number 10) (String "hello")
      jrspResult rsp `shouldBe` Just (String "hello")
      jrspError rsp  `shouldSatisfy` isNothing

    it "mkError sets error and no result" $ do
      let rsp = mkError (Number 10) (-1) "boom"
      jrspResult rsp `shouldBe` Nothing
      jrspError rsp  `shouldSatisfy` isJust
      case jrspError rsp of
        Just e  -> do
          jeCode e    `shouldBe` (-1)
          jeMessage e `shouldBe` "boom"
        Nothing -> expectationFailure "expected error"

  -- -----------------------------------------------------------------
  -- InitializeResult (ToJSON)
  -- -----------------------------------------------------------------
  describe "InitializeResult" $ do
    it "encodes with protocolVersion, capabilities, and serverInfo" $ do
      let ir = InitializeResult
            { irProtocolVersion = mcpProtocolVersion
            , irCapabilities    = ServerCapabilities True True
            , irServerInfo      = ServerInfo "topo-mcp" "0.1.0"
            }
          json = Aeson.toJSON ir
      lookupKey "protocolVersion" json `shouldBe` Just (String mcpProtocolVersion)
      lookupKey "capabilities"    json `shouldSatisfy` isJust
      lookupKey "serverInfo"      json `shouldSatisfy` isJust

  -- -----------------------------------------------------------------
  -- ToolDef (ToJSON)
  -- -----------------------------------------------------------------
  describe "ToolDef" $ do
    it "encodes name, description, and inputSchema" $ do
      let td = ToolDef "test_tool" "A test tool" (object ["type" .= ("object" :: Text)])
          json = Aeson.toJSON td
      lookupKey "name"        json `shouldBe` Just (String "test_tool")
      lookupKey "description" json `shouldBe` Just (String "A test tool")
      lookupKey "inputSchema" json `shouldSatisfy` isJust

  -- -----------------------------------------------------------------
  -- ToolCallResult (ToJSON)
  -- -----------------------------------------------------------------
  describe "ToolCallResult" $ do
    it "mkToolResult encodes a text content block without isError" $ do
      let tcr = mkToolResult "some output"
          json = Aeson.toJSON tcr
      lookupKey "content" json `shouldSatisfy` isJust
      -- isError should not be present (only included when True)
      lookupKey "isError" json `shouldSatisfy` isNothing

    it "mkToolImageResult encodes an image content block" $ do
      let tcr = mkToolImageResult "ZmFrZQ==" "image/png"
          json = Aeson.toJSON tcr
      lookupKey "content" json `shouldSatisfy` isJust
      lookupKey "isError" json `shouldSatisfy` isNothing

    it "mkToolError encodes a text content block with isError=true" $ do
      let tcr = mkToolError "something failed"
          json = Aeson.toJSON tcr
      lookupKey "content" json `shouldSatisfy` isJust
      lookupKey "isError" json `shouldBe` Just (Bool True)

  -- -----------------------------------------------------------------
  -- ResourceDef / ResourceContent (ToJSON)
  -- -----------------------------------------------------------------
  describe "ResourceDef" $ do
    it "encodes uri, name, description, mimeType" $ do
      let rd = ResourceDef "topo://state" "State" "desc" "application/json"
          json = Aeson.toJSON rd
      lookupKey "uri"         json `shouldBe` Just (String "topo://state")
      lookupKey "name"        json `shouldBe` Just (String "State")
      lookupKey "description" json `shouldBe` Just (String "desc")
      lookupKey "mimeType"    json `shouldBe` Just (String "application/json")

  describe "ResourceContent" $ do
    it "encodes uri, mimeType, text" $ do
      let rc = ResourceContent "topo://state" "application/json" "{}"
          json = Aeson.toJSON rc
      lookupKey "uri"      json `shouldBe` Just (String "topo://state")
      lookupKey "mimeType" json `shouldBe` Just (String "application/json")
      lookupKey "text"     json `shouldBe` Just (String "{}")

  -- -----------------------------------------------------------------
  -- mcpProtocolVersion
  -- -----------------------------------------------------------------
  describe "mcpProtocolVersion" $ do
    it "is a non-empty string" $
      mcpProtocolVersion `shouldSatisfy` (not . null . show)

-- =====================================================================
-- Helpers
-- =====================================================================

-- | Look up a key in a JSON object value.
lookupKey :: Aeson.Key -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup k o
lookupKey _ _          = Nothing
