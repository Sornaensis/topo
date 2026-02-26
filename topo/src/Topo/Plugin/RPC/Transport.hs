{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Platform-agnostic transport abstraction for plugin RPC.
--
-- On Windows, plugins communicate via named pipes; on Linux\/macOS,
-- via Unix domain sockets.  This module provides a unified
-- 'Transport' interface over both, plus length-prefixed message
-- framing.
--
-- = Wire format
--
-- Each message is framed as:
--
-- @
-- ┌──────────────┬──────────────────┐
-- │ length (4 B) │ payload (N bytes)│
-- └──────────────┴──────────────────┘
-- @
--
-- The 4-byte length prefix is a little-endian 'Word32' giving the
-- byte count of the payload that follows.  The payload is a JSON
-- object (UTF-8 encoded).  JSON is used for cross-language
-- compatibility; a future version may switch to MessagePack for
-- efficiency.
module Topo.Plugin.RPC.Transport
  ( -- * Transport handle
    Transport(..)
    -- * Connection lifecycle
  , TransportConfig(..)
  , defaultTransportConfig
  , connectPlugin
  , closeTransport
    -- * Message I/O
  , sendMessage
  , recvMessage
    -- * Errors
  , TransportError(..)
    -- * Pipe name generation
  , pluginPipeName
  ) where

import Control.Exception (SomeException, catch, evaluate, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (getWord32le, runGetOrFail)
import Data.Binary.Put (putWord32le, runPut)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import System.IO
  ( Handle
  , BufferMode(..)
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  )

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Errors that can occur during transport operations.
data TransportError
  = TransportConnectionFailed !Text
    -- ^ Could not establish a connection to the plugin process.
  | TransportSendFailed !Text
    -- ^ Failed to send a message.
  | TransportRecvFailed !Text
    -- ^ Failed to receive a message (broken pipe, EOF, etc.).
  | TransportFramingError !Text
    -- ^ Message framing error (invalid length prefix, truncated).
  | TransportClosed
    -- ^ Transport has been closed.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Transport handle
------------------------------------------------------------------------

-- | An open connection to a plugin process.
--
-- Wraps a bidirectional 'Handle' pair (one for reading, one for
-- writing).  On named pipes, both may be the same handle.
data Transport = Transport
  { tReadHandle  :: !Handle
    -- ^ Handle to read plugin responses from.
  , tWriteHandle :: !Handle
    -- ^ Handle to send host messages to.
  , tPluginName  :: !Text
    -- ^ Name of the connected plugin (for diagnostics).
  }

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for establishing a transport connection.
data TransportConfig = TransportConfig
  { tcPipeDir :: !FilePath
    -- ^ Directory for named pipe files (Windows: ignored, uses
    -- @\\\\.\\pipe\\@; Unix: socket directory).
  , tcTimeout :: !Int
    -- ^ Connection timeout in milliseconds (0 = no timeout).
  } deriving (Eq, Show)

-- | Sensible defaults: system pipe directory, no timeout.
defaultTransportConfig :: TransportConfig
defaultTransportConfig = TransportConfig
  { tcPipeDir = ""
  , tcTimeout = 0
  }

------------------------------------------------------------------------
-- Pipe name generation
------------------------------------------------------------------------

-- | Generate a platform-appropriate pipe\/socket name for a plugin.
--
-- On Windows: @\\\\.\\pipe\\topo-plugin-\<name\>@
-- On Unix: @\<pipeDir\>\/topo-plugin-\<name\>.sock@
pluginPipeName :: TransportConfig -> Text -> FilePath
pluginPipeName _cfg pluginName =
#if defined(mingw32_HOST_OS)
  "\\\\.\\pipe\\topo-plugin-" <> Text.unpack pluginName
#else
  let dir = if null (tcPipeDir _cfg)
              then "/tmp"
              else tcPipeDir _cfg
  in dir <> "/topo-plugin-" <> Text.unpack pluginName <> ".sock"
#endif

------------------------------------------------------------------------
-- Connection lifecycle
------------------------------------------------------------------------

-- | Connect to a plugin's named pipe or Unix socket.
--
-- The plugin process must already be running and listening on the
-- pipe.  Returns a 'Transport' handle on success.
--
-- __Note__: The actual platform-specific connection logic will be
-- implemented when the plugin launcher (Phase 7) is built.  For now
-- this function takes pre-opened handles for testing.
connectPlugin :: Text -> Handle -> Handle -> IO (Either TransportError Transport)
connectPlugin name readH writeH = do
  result <- catch
    (do hSetBinaryMode readH True
        hSetBinaryMode writeH True
        hSetBuffering readH (BlockBuffering Nothing)
        hSetBuffering writeH (BlockBuffering Nothing)
        pure (Right Transport
          { tReadHandle  = readH
          , tWriteHandle = writeH
          , tPluginName  = name
          }))
    (\e -> pure (Left (TransportConnectionFailed (Text.pack (show (e :: SomeException))))))
  pure result

-- | Close the transport connection.
closeTransport :: Transport -> IO ()
closeTransport t = do
  catch (hClose (tReadHandle t)) (\(_ :: SomeException) -> pure ())
  catch (hClose (tWriteHandle t)) (\(_ :: SomeException) -> pure ())

------------------------------------------------------------------------
-- Message I/O
------------------------------------------------------------------------

-- | Send a length-prefixed message over the transport.
--
-- Encodes the payload length as a 4-byte little-endian 'Word32',
-- followed by the raw payload bytes.
sendMessage :: Transport -> BS.ByteString -> IO (Either TransportError ())
sendMessage t payload = catch go handler
  where
    go = do
      let len = fromIntegral (BS.length payload) :: Word32
          header = BL.toStrict (runPut (putWord32le len))
      BS.hPut (tWriteHandle t) header
      BS.hPut (tWriteHandle t) payload
      hFlush (tWriteHandle t)
      pure (Right ())
    handler :: SomeException -> IO (Either TransportError ())
    handler e = pure (Left (TransportSendFailed (Text.pack (show e))))

-- | Receive a length-prefixed message from the transport.
--
-- Reads a 4-byte little-endian length, then reads that many bytes
-- of payload.  Returns 'TransportRecvFailed' on EOF or short read.
recvMessage :: Transport -> IO (Either TransportError BS.ByteString)
recvMessage t = catch go handler
  where
    go = do
      headerBs <- BS.hGet (tReadHandle t) 4
      if BS.length headerBs < 4
        then pure (Left (TransportRecvFailed "EOF reading message length"))
        else case runGetOrFail getWord32le (BL.fromStrict headerBs) of
          Left (_, _, err) ->
            pure (Left (TransportFramingError (Text.pack err)))
          Right (_, _, len) -> do
            let payloadLen = fromIntegral len :: Int
            payload <- BS.hGet (tReadHandle t) payloadLen
            if BS.length payload < payloadLen
              then pure (Left (TransportRecvFailed
                    ("short read: expected " <> Text.pack (show payloadLen)
                     <> " bytes, got " <> Text.pack (show (BS.length payload)))))
              else do
                _ <- evaluate payload
                pure (Right payload)
    handler :: SomeException -> IO (Either TransportError BS.ByteString)
    handler e = pure (Left (TransportRecvFailed (Text.pack (show e))))
