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
  , TransportEndpointKind(..)
  , TransportEndpoint(..)
  , TransportServer(..)
  , endpointKindText
  , parseEndpointKind
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , openPluginServer
  , connectPluginEndpoint
  , connectPluginFromEnvironment
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

import Control.Exception (SomeException, catch, evaluate, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (getWord32le, runGetOrFail)
import Data.Binary.Put (putWord32le, runPut)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( BufferMode(..)
  , Handle
  , IOMode(..)
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  , openBinaryTempFile
  )
import System.Timeout (timeout)

#if !defined(mingw32_HOST_OS)
import qualified Network.Socket as Socket
import System.Posix.Files (ownerModes, setFileMode)
#endif

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
  | TransportUnsupported !Text
    -- ^ The requested transport is not supported on this platform.
  | TransportClosed
    -- ^ Transport has been closed.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Transport handle
------------------------------------------------------------------------

-- | An open connection to a plugin process.
--
-- Wraps a bidirectional 'Handle' pair (one for reading, one for
-- writing).  On sockets and named pipes, both may be the same handle.
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
-- Endpoint metadata
------------------------------------------------------------------------

-- | Supported endpoint kinds for production plugin transports.
data TransportEndpointKind
  = TransportEndpointUnixSocket
  | TransportEndpointNamedPipe
  deriving (Eq, Show)

-- | A host-created endpoint that a plugin process should connect to.
data TransportEndpoint = TransportEndpoint
  { teKind    :: !TransportEndpointKind
  , teAddress :: !FilePath
  } deriving (Eq, Show)

-- | A listening host endpoint awaiting one plugin connection.
data TransportServer = TransportServer
  { tsEndpoint :: !TransportEndpoint
  , tsAccept   :: IO (Either TransportError Transport)
  , tsClose    :: IO ()
  }

-- | Environment variable containing the host-created endpoint address.
pluginEndpointEnv :: String
pluginEndpointEnv = "TOPO_PLUGIN_ENDPOINT"

-- | Environment variable containing the host-created endpoint kind.
pluginEndpointKindEnv :: String
pluginEndpointKindEnv = "TOPO_PLUGIN_ENDPOINT_KIND"

endpointKindText :: TransportEndpointKind -> Text
endpointKindText TransportEndpointUnixSocket = "unix"
endpointKindText TransportEndpointNamedPipe = "named-pipe"

parseEndpointKind :: Text -> Maybe TransportEndpointKind
parseEndpointKind raw = case Text.toLower raw of
  "unix" -> Just TransportEndpointUnixSocket
  "unix-socket" -> Just TransportEndpointUnixSocket
  "unix_socket" -> Just TransportEndpointUnixSocket
  "named-pipe" -> Just TransportEndpointNamedPipe
  "named_pipe" -> Just TransportEndpointNamedPipe
  "pipe" -> Just TransportEndpointNamedPipe
  _ -> Nothing

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

-- | Open a host-side server endpoint for one plugin process.
openPluginServer :: TransportConfig -> Text -> IO (Either TransportError TransportServer)
openPluginServer cfg pluginName =
#if defined(mingw32_HOST_OS)
  pure (Left (TransportUnsupported "Windows named-pipe server transport is not implemented yet"))
#else
  openUnixSocketServer cfg pluginName
#endif

-- | Connect to a host-created endpoint from the plugin side.
connectPluginEndpoint :: Text -> TransportEndpoint -> IO (Either TransportError Transport)
connectPluginEndpoint name endpoint = case teKind endpoint of
  TransportEndpointUnixSocket ->
#if defined(mingw32_HOST_OS)
    pure (Left (TransportUnsupported "Unix domain sockets are not supported on Windows"))
#else
    connectUnixSocketEndpoint name (teAddress endpoint)
#endif
  TransportEndpointNamedPipe ->
    pure (Left (TransportUnsupported "Windows named-pipe client transport is not implemented yet"))

-- | Connect using the production endpoint variables when present;
-- otherwise fall back to the supplied stdio handles for tests.
connectPluginFromEnvironment
  :: Text
  -> Handle
  -> Handle
  -> IO (Either TransportError Transport)
connectPluginFromEnvironment name fallbackRead fallbackWrite = do
  mEndpoint <- lookupEnv pluginEndpointEnv
  mKind <- lookupEnv pluginEndpointKindEnv
  case (mEndpoint, mKind) of
    (Nothing, Nothing) -> connectPlugin name fallbackRead fallbackWrite
    (Just endpoint, Just kindText) -> case parseEndpointKind (Text.pack kindText) of
      Nothing -> pure (Left (TransportConnectionFailed
        ("unknown " <> Text.pack pluginEndpointKindEnv <> ": " <> Text.pack kindText)))
      Just kind -> connectPluginEndpoint name (TransportEndpoint
        { teKind = kind
        , teAddress = endpoint
        })
    (Just _, Nothing) -> pure (Left (TransportConnectionFailed
      (Text.pack pluginEndpointEnv <> " is set but " <> Text.pack pluginEndpointKindEnv <> " is missing")))
    (Nothing, Just _) -> pure (Left (TransportConnectionFailed
      (Text.pack pluginEndpointKindEnv <> " is set but " <> Text.pack pluginEndpointEnv <> " is missing")))

-- | Connect to a plugin using pre-opened handles.
--
-- This compatibility path is intended for in-process tests and the
-- legacy stdio fixture harness. Production launches should use
-- 'openPluginServer' on the host side and 'connectPluginFromEnvironment'
-- on the plugin side.
connectPlugin :: Text -> Handle -> Handle -> IO (Either TransportError Transport)
connectPlugin name readH writeH = do
  result <- catch
    (Right <$> mkTransport name readH writeH)
    (\e -> pure (Left (TransportConnectionFailed (Text.pack (show (e :: SomeException))))))
  pure result

mkTransport :: Text -> Handle -> Handle -> IO Transport
mkTransport name readH writeH = do
  hSetBinaryMode readH True
  hSetBinaryMode writeH True
  hSetBuffering readH (BlockBuffering Nothing)
  hSetBuffering writeH (BlockBuffering Nothing)
  pure Transport
    { tReadHandle  = readH
    , tWriteHandle = writeH
    , tPluginName  = name
    }

-- | Close the transport connection.
closeTransport :: Transport -> IO ()
closeTransport t = do
  catch (hClose (tReadHandle t)) (\(_ :: SomeException) -> pure ())
  catch (hClose (tWriteHandle t)) (\(_ :: SomeException) -> pure ())

#if !defined(mingw32_HOST_OS)
openUnixSocketServer :: TransportConfig -> Text -> IO (Either TransportError TransportServer)
openUnixSocketServer cfg pluginName = catch go handler
  where
    go = do
      socketPath <- allocateUnixSocketPath cfg pluginName
      socketResult <- try (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
        :: IO (Either SomeException Socket.Socket)
      case socketResult of
        Left err -> do
          removeUnixSocketPath socketPath
          pure (Left (TransportConnectionFailed (Text.pack (show err))))
        Right sock -> do
          let cleanup = do
                safeCloseSocket sock
                removeUnixSocketPath socketPath
              setup = do
                removeFileIfExists socketPath
                Socket.bind sock (Socket.SockAddrUnix socketPath)
                Socket.listen sock 1
          setupResult <- try setup :: IO (Either SomeException ())
          case setupResult of
            Left err -> do
              cleanup
              pure (Left (TransportConnectionFailed (Text.pack (show err))))
            Right () -> pure (Right (TransportServer
              { tsEndpoint = TransportEndpoint
                  { teKind = TransportEndpointUnixSocket
                  , teAddress = socketPath
                  }
              , tsAccept = acceptUnixSocketTransport cfg pluginName sock socketPath
              , tsClose = cleanup
              }))
    handler (err :: SomeException) =
      pure (Left (TransportConnectionFailed (Text.pack (show err))))

allocateUnixSocketPath :: TransportConfig -> Text -> IO FilePath
allocateUnixSocketPath cfg _pluginName = do
  let baseDir = if null (tcPipeDir cfg) then "/tmp" else tcPipeDir cfg
  createDirectoryIfMissing True baseDir
  (socketDir, handle) <- openBinaryTempFile baseDir "topo."
  hClose handle
  removeFileIfExists socketDir
  createDirectory socketDir
  setFileMode socketDir ownerModes
  pure (socketDir </> "p.sock")

acceptUnixSocketTransport
  :: TransportConfig
  -> Text
  -> Socket.Socket
  -> FilePath
  -> IO (Either TransportError Transport)
acceptUnixSocketTransport cfg pluginName sock socketPath = do
  let cleanup = do
        safeCloseSocket sock
        removeUnixSocketPath socketPath
      acceptOnce = Socket.accept sock
      waitForAccept
        | tcTimeout cfg <= 0 = Just <$> acceptOnce
        | otherwise = timeout (tcTimeout cfg * 1000) acceptOnce
  acceptResult <- try waitForAccept :: IO (Either SomeException (Maybe (Socket.Socket, Socket.SockAddr)))
  case acceptResult of
    Left err -> do
      cleanup
      pure (Left (TransportConnectionFailed (Text.pack (show err))))
    Right Nothing -> do
      cleanup
      pure (Left (TransportConnectionFailed
        ("timed out waiting for plugin connection on " <> Text.pack socketPath)))
    Right (Just (conn, _addr)) -> do
      safeCloseSocket sock
      removeUnixSocketPath socketPath
      handleResult <- try (Socket.socketToHandle conn ReadWriteMode >>= \h -> mkTransport pluginName h h)
        :: IO (Either SomeException Transport)
      case handleResult of
        Left err -> do
          safeCloseSocket conn
          pure (Left (TransportConnectionFailed (Text.pack (show err))))
        Right transport -> pure (Right transport)

connectUnixSocketEndpoint :: Text -> FilePath -> IO (Either TransportError Transport)
connectUnixSocketEndpoint name socketPath = do
  socketResult <- try (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    :: IO (Either SomeException Socket.Socket)
  case socketResult of
    Left err -> pure (Left (TransportConnectionFailed (Text.pack (show err))))
    Right sock -> do
      connectResult <- try $ do
        Socket.connect sock (Socket.SockAddrUnix socketPath)
        Socket.socketToHandle sock ReadWriteMode >>= \h -> mkTransport name h h
      case connectResult of
        Left (err :: SomeException) -> do
          safeCloseSocket sock
          pure (Left (TransportConnectionFailed (Text.pack (show err))))
        Right transport -> pure (Right transport)

safeCloseSocket :: Socket.Socket -> IO ()
safeCloseSocket sock = do
  _ <- try (Socket.close sock) :: IO (Either SomeException ())
  pure ()
#endif

removeUnixSocketPath :: FilePath -> IO ()
removeUnixSocketPath socketPath = do
  removeFileIfExists socketPath
  removeDirectoryIfExists (takeDirectory socketPath)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path =
  catch (removeFile path) (\(_ :: SomeException) -> pure ())

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists path =
  catch (removeDirectory path) (\(_ :: SomeException) -> pure ())

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
