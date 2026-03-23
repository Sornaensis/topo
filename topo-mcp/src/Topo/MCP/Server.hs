{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | MCP JSON-RPC 2.0 server loop.
--
-- Reads newline-delimited JSON-RPC requests from stdin, dispatches them
-- (initialize, tools\/list, tools\/call, resources\/list, resources\/read),
-- and writes JSON-RPC responses to stdout.  Diagnostics go to stderr.
module Topo.MCP.Server
  ( runMcpServer
    -- * Exported for testing
  , parseToolCall
  , parseResourceUri
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO
  ( BufferMode(..)
  , hFlush
  , hPutStrLn
  , hSetBinaryMode
  , hSetBuffering
  , stderr
  , stdin
  , stdout
  )

import Topo.MCP.Types
import Topo.MCP.IPC (IpcConnectionRef)
import Topo.MCP.Resources (allResourceDefs, allResourceTemplateDefs, handleResourceRead)
import Topo.MCP.Tools (allToolDefs, handleToolCall)

-- | Run the MCP server loop.
--
-- Reads JSON-RPC from stdin, dispatches, writes responses to stdout.
-- This function blocks forever (or until EOF on stdin).
runMcpServer :: IpcConnectionRef -> IO ()
runMcpServer connRef = do
  -- Configure stdio for binary/line-buffered JSON-RPC
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hPutStrLn stderr "[topo-mcp] MCP server ready, reading from stdin"
  loop
  where
    loop = do
      mLine <- catch (Just <$> BS8.hGetLine stdin)
                     (\(_ :: SomeException) -> pure Nothing)
      case mLine of
        Nothing -> hPutStrLn stderr "[topo-mcp] stdin closed, exiting"
        Just line
          | BS.null line -> loop  -- skip empty lines
          | otherwise -> do
              case Aeson.eitherDecodeStrict line of
                Left err -> do
                  hPutStrLn stderr ("[topo-mcp] JSON parse error: " ++ err)
                  -- JSON-RPC parse error response (id = null)
                  sendResponse $ mkError Null (-32700) (Text.pack ("Parse error: " <> err))
                  loop
                Right req -> do
                  handleRequest connRef req
                  loop

-- | Dispatch a single JSON-RPC request.
handleRequest :: IpcConnectionRef -> JsonRpcRequest -> IO ()
handleRequest connRef req = do
  let reqId = maybe Null id (jrpcId req)
      method = jrpcMethod req
      params = jrpcParams req
  hPutStrLn stderr ("[topo-mcp] <- " ++ Text.unpack method)
  case method of
    -- MCP lifecycle
    "initialize"  -> handleInitialize reqId params
    "initialized" -> pure ()  -- notification, no response needed

    -- Ping
    "ping" -> sendResponse $ mkResult reqId (object [])

    -- Tools
    "tools/list"  -> handleToolsList reqId
    "tools/call"  -> handleToolsCall connRef reqId params

    -- Resources
    "resources/list" -> handleResourcesList reqId
    "resources/read" -> handleResourcesRead connRef reqId params

    -- Unknown method
    _ -> sendResponse $ mkError reqId (-32601) ("Method not found: " <> method)

-- | Handle the MCP initialize request.
handleInitialize :: Value -> Value -> IO ()
handleInitialize reqId _params = do
  let result = InitializeResult
        { irProtocolVersion = mcpProtocolVersion
        , irCapabilities    = ServerCapabilities
            { scTools     = True
            , scResources = True
            }
        , irServerInfo      = ServerInfo
            { siName    = "topo-mcp"
            , siVersion = "0.1.0"
            }
        }
  sendResponse $ mkResult reqId (Aeson.toJSON result)

-- | Handle tools/list — return all tool definitions.
handleToolsList :: Value -> IO ()
handleToolsList reqId = do
  sendResponse $ mkResult reqId $ object ["tools" .= allToolDefs]

-- | Handle tools/call — dispatch to the named tool.
handleToolsCall :: IpcConnectionRef -> Value -> Value -> IO ()
handleToolsCall connRef reqId params = do
  case parseToolCall params of
    Nothing ->
      sendResponse $ mkError reqId (-32602) "Invalid params: expected {name, arguments}"
    Just (toolName, args) -> do
      result <- handleToolCall connRef toolName args
        `catch` \(e :: SomeException) ->
          pure $ Right (mkToolError (Text.pack ("internal error: " <> show e)))
      case result of
        Left err ->
          sendResponse $ mkError reqId (-32603) err
        Right tcr ->
          sendResponse $ mkResult reqId (Aeson.toJSON tcr)

-- | Handle resources/list — return all resource definitions.
handleResourcesList :: Value -> IO ()
handleResourcesList reqId = do
  sendResponse $ mkResult reqId $ object
    [ "resources"         .= allResourceDefs
    , "resourceTemplates" .= allResourceTemplateDefs
    ]

-- | Handle resources/read — read a resource by URI.
handleResourcesRead :: IpcConnectionRef -> Value -> Value -> IO ()
handleResourcesRead connRef reqId params = do
  case parseResourceUri params of
    Nothing ->
      sendResponse $ mkError reqId (-32602) "Invalid params: expected {uri}"
    Just uri -> do
      result <- handleResourceRead connRef uri
        `catch` \(e :: SomeException) ->
          pure $ Left (Text.pack ("internal error: " <> show e))
      case result of
        Left err ->
          sendResponse $ mkError reqId (-32603) err
        Right rrr ->
          sendResponse $ mkResult reqId (Aeson.toJSON rrr)

------------------------------------------------------------------------
-- Param parsers
------------------------------------------------------------------------

-- | Parse a tools/call params object: {name: "...", arguments: {...}}
parseToolCall :: Value -> Maybe (Text, Value)
parseToolCall = Aeson.parseMaybe $ Aeson.withObject "params" $ \o ->
  (,) <$> o .: "name" <*> (o .: "arguments" <|> pure (Object mempty))

-- | Parse a resources/read params object: {uri: "..."}
parseResourceUri :: Value -> Maybe Text
parseResourceUri = Aeson.parseMaybe $ Aeson.withObject "params" $ \o ->
  o .: "uri"

------------------------------------------------------------------------
-- Response output
------------------------------------------------------------------------

-- | Write a JSON-RPC response to stdout (newline-delimited).
sendResponse :: JsonRpcResponse -> IO ()
sendResponse rsp = do
  let bytes = BL.toStrict (Aeson.encode rsp)
  BS.hPut stdout bytes
  BS.hPut stdout "\n"
  hFlush stdout
