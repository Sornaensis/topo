{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared command/response types for the topo-seer IPC command channel.
--
-- These types are used by both topo-seer (server) and topo-mcp (client)
-- for communication over a named pipe (Windows) or Unix domain socket.
-- They use the same length-prefixed JSON framing as
-- 'Topo.Plugin.RPC.Transport'.
module Topo.Command.Types
  ( -- * Command envelope
    SeerCommand(..)
    -- * Response envelope
  , SeerResponse(..)
    -- * Helpers
  , okResponse
  , errResponse
    -- * Pipe name
  , commandPipeName
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
  )
import Data.Text (Text)

-- | A command sent from an external client to topo-seer.
data SeerCommand = SeerCommand
  { scId     :: !Int     -- ^ Request ID for correlating responses.
  , scMethod :: !Text    -- ^ Command name (e.g., @\"get_state\"@, @\"set_slider\"@).
  , scParams :: !Value   -- ^ Command-specific parameters (JSON object).
  } deriving (Eq, Show)

instance FromJSON SeerCommand where
  parseJSON = withObject "SeerCommand" $ \o ->
    SeerCommand
      <$> o .: "id"
      <*> o .: "method"
      <*> o .: "params"

instance ToJSON SeerCommand where
  toJSON cmd = object
    [ "id"     .= scId cmd
    , "method" .= scMethod cmd
    , "params" .= scParams cmd
    ]

-- | A response from topo-seer to an external client.
data SeerResponse = SeerResponse
  { srId      :: !Int          -- ^ Matches the request 'scId'.
  , srSuccess :: !Bool         -- ^ Whether the command succeeded.
  , srResult  :: !Value        -- ^ Command-specific result data (on success).
  , srError   :: !(Maybe Text) -- ^ Error message (on failure).
  } deriving (Eq, Show)

instance FromJSON SeerResponse where
  parseJSON = withObject "SeerResponse" $ \o ->
    SeerResponse
      <$> o .: "id"
      <*> o .: "success"
      <*> o .: "result"
      <*> o .:? "error"

instance ToJSON SeerResponse where
  toJSON rsp = object
    [ "id"      .= srId rsp
    , "success" .= srSuccess rsp
    , "result"  .= srResult rsp
    , "error"   .= srError rsp
    ]

-- | Build a successful response.
okResponse :: Int -> Value -> SeerResponse
okResponse reqId result = SeerResponse
  { srId      = reqId
  , srSuccess = True
  , srResult  = result
  , srError   = Nothing
  }

-- | Build an error response.
errResponse :: Int -> Text -> SeerResponse
errResponse reqId msg = SeerResponse
  { srId      = reqId
  , srSuccess = False
  , srResult  = Null
  , srError   = Just msg
  }

-- | The default pipe/socket name for the topo-seer command channel.
--
-- On Windows: @\\\\.\\pipe\\topo-seer-cmd@
-- On Unix: @\/tmp\/topo-seer-cmd.sock@
commandPipeName :: FilePath
commandPipeName =
#if defined(mingw32_HOST_OS)
  "\\\\.\\pipe\\topo-seer-cmd"
#else
  "/tmp/topo-seer-cmd.sock"
#endif
