{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | MCP (Model Context Protocol) JSON-RPC 2.0 types.
--
-- This module defines the wire types for the MCP protocol as spoken
-- between an LLM client and @topo-mcp@ over stdin\/stdout.
--
-- The MCP protocol layers on top of JSON-RPC 2.0:
--
--   * Requests have @jsonrpc@, @id@, @method@, @params@
--   * Responses have @jsonrpc@, @id@, @result@ or @error@
--   * Notifications have @jsonrpc@, @method@, @params@ (no @id@)
module Topo.MCP.Types
  ( -- * JSON-RPC 2.0 envelope
    JsonRpcRequest(..)
  , JsonRpcResponse(..)
  , JsonRpcError(..)
  , mkResult
  , mkError
    -- * MCP initialize
  , ServerInfo(..)
  , ServerCapabilities(..)
  , InitializeResult(..)
    -- * MCP tools
  , ToolDef(..)
  , ToolCallResult(..)
  , mkToolResult
  , mkToolImageResult
  , mkToolError
    -- * MCP resources
  , ResourceDef(..)
  , ResourceTemplateDef(..)
  , ResourceContent(..)
  , ResourceReadResult(..)
    -- * Constants
  , mcpProtocolVersion
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , object
  , withObject
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

-- | MCP protocol version we implement.
mcpProtocolVersion :: Text
mcpProtocolVersion = "2024-11-05"

------------------------------------------------------------------------
-- JSON-RPC 2.0 envelope
------------------------------------------------------------------------

-- | An incoming JSON-RPC 2.0 request (or notification if id is Nothing).
data JsonRpcRequest = JsonRpcRequest
  { jrpcId     :: !(Maybe Value)  -- ^ Request ID (absent for notifications)
  , jrpcMethod :: !Text           -- ^ Method name
  , jrpcParams :: !Value          -- ^ Parameters (object or array)
  } deriving (Show)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o ->
    JsonRpcRequest
      <$> o .:? "id"
      <*> o .: "method"
      <*> o .:? "params" .!= Null

-- | An outgoing JSON-RPC 2.0 response.
data JsonRpcResponse = JsonRpcResponse
  { jrspId     :: !Value          -- ^ Matches request ID
  , jrspResult :: !(Maybe Value)  -- ^ Result (on success)
  , jrspError  :: !(Maybe JsonRpcError) -- ^ Error (on failure)
  } deriving (Show)

instance ToJSON JsonRpcResponse where
  toJSON rsp = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= jrspId rsp
    ] ++
    case jrspError rsp of
      Just err -> ["error" .= err]
      Nothing  -> ["result" .= jrspResult rsp]

-- | JSON-RPC 2.0 error object.
data JsonRpcError = JsonRpcError
  { jeCode    :: !Int
  , jeMessage :: !Text
  , jeData    :: !(Maybe Value)
  } deriving (Show)

instance ToJSON JsonRpcError where
  toJSON e = object $
    [ "code"    .= jeCode e
    , "message" .= jeMessage e
    ] ++ maybe [] (\d -> ["data" .= d]) (jeData e)

-- | Build a successful JSON-RPC response.
mkResult :: Value -> Value -> JsonRpcResponse
mkResult reqId result = JsonRpcResponse
  { jrspId     = reqId
  , jrspResult = Just result
  , jrspError  = Nothing
  }

-- | Build an error JSON-RPC response.
mkError :: Value -> Int -> Text -> JsonRpcResponse
mkError reqId code msg = JsonRpcResponse
  { jrspId     = reqId
  , jrspResult = Nothing
  , jrspError  = Just JsonRpcError
    { jeCode    = code
    , jeMessage = msg
    , jeData    = Nothing
    }
  }

------------------------------------------------------------------------
-- MCP initialize
------------------------------------------------------------------------

-- | Server info returned in the initialize response.
data ServerInfo = ServerInfo
  { siName    :: !Text
  , siVersion :: !Text
  } deriving (Show)

instance ToJSON ServerInfo where
  toJSON si = object
    [ "name"    .= siName si
    , "version" .= siVersion si
    ]

-- | Server capabilities advertised during initialization.
data ServerCapabilities = ServerCapabilities
  { scTools     :: !Bool
  , scResources :: !Bool
  } deriving (Show)

instance ToJSON ServerCapabilities where
  toJSON sc = object $
    (if scTools sc then ["tools" .= object []] else []) ++
    (if scResources sc then ["resources" .= object []] else [])

-- | The result payload for the MCP initialize response.
data InitializeResult = InitializeResult
  { irProtocolVersion :: !Text
  , irCapabilities    :: !ServerCapabilities
  , irServerInfo      :: !ServerInfo
  } deriving (Show)

instance ToJSON InitializeResult where
  toJSON ir = object
    [ "protocolVersion" .= irProtocolVersion ir
    , "capabilities"    .= irCapabilities ir
    , "serverInfo"      .= irServerInfo ir
    ]

------------------------------------------------------------------------
-- MCP tools
------------------------------------------------------------------------

-- | A tool definition returned by tools/list.
data ToolDef = ToolDef
  { tdName        :: !Text
  , tdDescription :: !Text
  , tdInputSchema :: !Value  -- ^ JSON Schema for the tool's input parameters
  } deriving (Show)

instance ToJSON ToolDef where
  toJSON td = object
    [ "name"        .= tdName td
    , "description" .= tdDescription td
    , "inputSchema" .= tdInputSchema td
    ]

-- | Result of a tool call (tools/call response content).
data ToolCallResult = ToolCallResult
  { tcrContent :: ![Value]   -- ^ Array of content blocks
  , tcrIsError :: !Bool      -- ^ Whether this is an error result
  } deriving (Show)

instance ToJSON ToolCallResult where
  toJSON tcr = object $
    [ "content" .= tcrContent tcr ] ++
    (if tcrIsError tcr then ["isError" .= True] else [])

-- | Build a successful tool result with a text content block.
mkToolResult :: Text -> ToolCallResult
mkToolResult txt = ToolCallResult
  { tcrContent = [object ["type" .= ("text" :: Text), "text" .= txt]]
  , tcrIsError = False
  }

-- | Build a successful tool result with an image content block.
mkToolImageResult
  :: Text    -- ^ Base64-encoded image data
  -> Text    -- ^ MIME type (e.g. @"image\/png"@)
  -> ToolCallResult
mkToolImageResult base64Data mimeType = ToolCallResult
  { tcrContent = [object
      [ "type"     .= ("image" :: Text)
      , "data"     .= base64Data
      , "mimeType" .= mimeType
      ]]
  , tcrIsError = False
  }

-- | Build an error tool result with a text content block.
mkToolError :: Text -> ToolCallResult
mkToolError txt = ToolCallResult
  { tcrContent = [object ["type" .= ("text" :: Text), "text" .= txt]]
  , tcrIsError = True
  }

------------------------------------------------------------------------
-- MCP resources
------------------------------------------------------------------------

-- | A static resource definition returned by resources/list.
data ResourceDef = ResourceDef
  { rdUri         :: !Text
  , rdName        :: !Text
  , rdDescription :: !Text
  , rdMimeType    :: !Text
  } deriving (Show)

instance ToJSON ResourceDef where
  toJSON rd = object
    [ "uri"         .= rdUri rd
    , "name"        .= rdName rd
    , "description" .= rdDescription rd
    , "mimeType"    .= rdMimeType rd
    ]

-- | A resource template definition (parameterized URI).
data ResourceTemplateDef = ResourceTemplateDef
  { rtdUriTemplate :: !Text
  , rtdName        :: !Text
  , rtdDescription :: !Text
  , rtdMimeType    :: !Text
  } deriving (Show)

instance ToJSON ResourceTemplateDef where
  toJSON rtd = object
    [ "uriTemplate" .= rtdUriTemplate rtd
    , "name"        .= rtdName rtd
    , "description" .= rtdDescription rtd
    , "mimeType"    .= rdtMimeType rtd
    ]
    where rdtMimeType = rtdMimeType

-- | Content returned from resources/read.
data ResourceContent = ResourceContent
  { rcUri      :: !Text
  , rcMimeType :: !Text
  , rcText     :: !Text
  } deriving (Show)

instance ToJSON ResourceContent where
  toJSON rc = object
    [ "uri"      .= rcUri rc
    , "mimeType" .= rcMimeType rc
    , "text"     .= rcText rc
    ]

-- | Result of a resources/read call.
data ResourceReadResult = ResourceReadResult
  { rrrContents :: ![ResourceContent]
  } deriving (Show)

instance ToJSON ResourceReadResult where
  toJSON rrr = object
    [ "contents" .= rrrContents rrr
    ]
