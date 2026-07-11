{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
  , TransportPeerIdentity(..)
  , TransportPeerPolicy(..)
  , transportPeerPolicyAny
  , TransportServer(..)
  , endpointKindText
  , parseEndpointKind
  , pluginIdEnv
  , pluginProtocolEnv
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginStdioCompatibilityEnv
  , pluginSessionEnv
  , pluginAuthTokenEnv
  , pluginWorldIdEnv
  , pluginDataRootEnv
  , openPluginServer
  , connectPluginEndpoint
  , connectPluginFromEnvironment
  , connectPlugin
  , closeTransport
    -- * Message I/O
  , defaultMaxFrameSizeBytes
  , sendMessage
  , sendMessageWithLimit
  , recvMessage
  , recvMessageWithLimit
    -- * Errors
  , TransportError(..)
    -- * Pipe name generation
  , pluginPipeName
  ) where

#if defined(mingw32_HOST_OS)
import Control.Concurrent (threadDelay)
#endif
import Control.Exception
  ( IOException
  , SomeAsyncException
  , SomeException
  , catch
  , evaluate
  , finally
  , fromException
  , onException
  , throwIO
  , try
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (getWord32le, runGetOrFail)
import Data.Binary.Put (putWord32le, runPut)
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Numeric (showHex)
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
import GHC.IO.Handle (hDuplicate)
import System.Timeout (timeout)

#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Unique (hashUnique, newUnique)
import Foreign.C.String (withCString)
#endif
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff, sizeOf)
#if defined(mingw32_HOST_OS)
import Foreign.C.Types (CChar)
import GHC.IO.Handle.FD (fdToHandle)
#else
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

-- | Peer credentials captured from an accepted production endpoint.
data TransportPeerIdentity = TransportPeerIdentity
  { tpiProcessId :: !(Maybe Word64)
    -- ^ Client process id when the platform exposes it.
  , tpiUserId    :: !(Maybe Word64)
    -- ^ Client user id when the platform exposes it.
  } deriving (Eq, Show)

-- | Host-side policy for the process allowed to consume a plugin endpoint.
data TransportPeerPolicy = TransportPeerPolicy
  { tppExpectedProcessId :: !(Maybe Word64)
    -- ^ Exact launched process id required when set.
  , tppExpectedUserId    :: !(Maybe Word64)
    -- ^ Same-user fallback required when set.
  } deriving (Eq, Show)

transportPeerPolicyAny :: TransportPeerPolicy
transportPeerPolicyAny = TransportPeerPolicy
  { tppExpectedProcessId = Nothing
  , tppExpectedUserId = Nothing
  }

-- | A listening host endpoint awaiting one plugin connection.
data TransportServer = TransportServer
  { tsEndpoint :: !TransportEndpoint
  , tsAccept   :: IO (Either TransportError Transport)
    -- ^ Accept with the permissive policy used by in-process tests.
  , tsAcceptWithPeerPolicy :: TransportPeerPolicy -> IO (Either TransportError Transport)
    -- ^ Accept while requiring the captured peer identity to match policy.
  , tsClose    :: IO ()
  }

-- | Environment variable containing the plugin identifier assigned by the host.
pluginIdEnv :: String
pluginIdEnv = "TOPO_PLUGIN_ID"

-- | Environment variable containing the RPC protocol version expected by the host.
pluginProtocolEnv :: String
pluginProtocolEnv = "TOPO_PLUGIN_PROTOCOL"

-- | Environment variable containing the host-created endpoint address.
pluginEndpointEnv :: String
pluginEndpointEnv = "TOPO_PLUGIN_ENDPOINT"

-- | Environment variable containing the host-created endpoint kind.
pluginEndpointKindEnv :: String
pluginEndpointKindEnv = "TOPO_PLUGIN_ENDPOINT_KIND"

-- | Explicit opt-in for test/development stdio compatibility when no
-- production endpoint variables are present.
pluginStdioCompatibilityEnv :: String
pluginStdioCompatibilityEnv = "TOPO_PLUGIN_STDIO_COMPAT"

-- | Environment variable containing the opaque host launch session id.
pluginSessionEnv :: String
pluginSessionEnv = "TOPO_PLUGIN_SESSION"

-- | Environment variable containing the opaque host launch auth token.
pluginAuthTokenEnv :: String
pluginAuthTokenEnv = "TOPO_PLUGIN_AUTH_TOKEN"

-- | Environment variable containing the active world id, or a host sentinel.
pluginWorldIdEnv :: String
pluginWorldIdEnv = "TOPO_PLUGIN_WORLD_ID"

-- | Environment variable containing the plugin's writable data root.
pluginDataRootEnv :: String
pluginDataRootEnv = "TOPO_PLUGIN_DATA_ROOT"

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

validateTransportPeerIdentity :: Text -> TransportPeerPolicy -> TransportPeerIdentity -> Either TransportError ()
validateTransportPeerIdentity pluginName policy identity =
  case peerPolicyMismatches policy identity of
    [] -> Right ()
    mismatches -> Left (TransportConnectionFailed
      ("plugin endpoint peer identity mismatch for " <> pluginName <> ": "
       <> Text.intercalate "; " mismatches))

peerPolicyMismatches :: TransportPeerPolicy -> TransportPeerIdentity -> [Text]
peerPolicyMismatches policy identity =
  pidMismatch <> uidMismatch
  where
    pidMismatch = expectedPeerFieldMismatch
      "pid"
      (tppExpectedProcessId policy)
      (tpiProcessId identity)
    uidMismatch = expectedPeerFieldMismatch
      "uid"
      (tppExpectedUserId policy)
      (tpiUserId identity)

expectedPeerFieldMismatch :: Text -> Maybe Word64 -> Maybe Word64 -> [Text]
expectedPeerFieldMismatch _ Nothing _ = []
expectedPeerFieldMismatch label (Just expected) actual
  | actual == Just expected = []
  | otherwise =
      [ "expected " <> label <> " " <> Text.pack (show expected)
        <> ", got " <> label <> " " <> maybe "unknown" (Text.pack . show) actual
      ]

------------------------------------------------------------------------
-- Pipe name generation
------------------------------------------------------------------------

-- | Generate a platform-appropriate pipe\/socket name for a plugin.
--
-- The plugin name component is escaped before it is embedded in an
-- endpoint address, so path separators or shell metacharacters in a
-- manifest name cannot escape the endpoint namespace.  Host-created
-- production servers append their own unique suffixes when allocating
-- endpoints.
pluginPipeName :: TransportConfig -> Text -> FilePath
pluginPipeName cfg pluginName =
#if defined(mingw32_HOST_OS)
  windowsNamedPipePrefix <> escapedPluginPipeBase pluginName
#else
  let dir = if null (tcPipeDir cfg)
              then "/tmp"
              else tcPipeDir cfg
  in dir </> escapedPluginPipeBase pluginName <> ".sock"
#endif

trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try action
  case result of
    Left err
      | Just (asyncErr :: SomeAsyncException) <- fromException err -> throwIO asyncErr
      | otherwise -> pure (Left err)
    Right value -> pure (Right value)

catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler = action `catch` \err ->
  case fromException err of
    Just (asyncErr :: SomeAsyncException) -> throwIO asyncErr
    Nothing -> handler err

escapedPluginPipeBase :: Text -> FilePath
escapedPluginPipeBase pluginName = "topo-plugin-" <> escapeEndpointSegment pluginName

escapeEndpointSegment :: Text -> String
escapeEndpointSegment raw
  | Text.null raw = "unnamed"
  | otherwise = concatMap escapeChar (Text.unpack raw)
  where
    escapeChar c
      | isSafeEndpointChar c = [c]
      | otherwise = '~' : paddedHex (ord c)

isSafeEndpointChar :: Char -> Bool
isSafeEndpointChar c =
     ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || c == '-'
  || c == '_'
  || c == '.'

paddedHex :: Int -> String
paddedHex n = replicate (max 0 (4 - length raw)) '0' <> raw
  where
    raw = showHex n ""

------------------------------------------------------------------------
-- Connection lifecycle
------------------------------------------------------------------------

-- | Open a host-side server endpoint for one plugin process.
openPluginServer :: TransportConfig -> Text -> IO (Either TransportError TransportServer)
openPluginServer cfg pluginName =
#if defined(mingw32_HOST_OS)
  openNamedPipeServer cfg pluginName
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
#if defined(mingw32_HOST_OS)
    connectNamedPipeEndpoint name (teAddress endpoint)
#else
    pure (Left (TransportUnsupported "Windows named pipes are not supported on this platform"))
#endif

-- | Connect using the production endpoint variables when present.
-- If no endpoint variables are present, the supplied stdio handles are used
-- only when 'pluginStdioCompatibilityEnv' explicitly enables
-- test/development compatibility.
connectPluginFromEnvironment
  :: Text
  -> Handle
  -> Handle
  -> IO (Either TransportError Transport)
connectPluginFromEnvironment name fallbackRead fallbackWrite = do
  mEndpoint <- lookupEnv pluginEndpointEnv
  mKind <- lookupEnv pluginEndpointKindEnv
  mStdioCompatibility <- lookupEnv pluginStdioCompatibilityEnv
  case (mEndpoint, mKind) of
    (Nothing, Nothing)
      | stdioCompatibilityEnabled mStdioCompatibility ->
          connectPlugin name fallbackRead fallbackWrite
      | otherwise -> pure (Left (TransportConnectionFailed
          ( Text.pack pluginEndpointEnv <> " and " <> Text.pack pluginEndpointKindEnv
         <> " are missing; production plugins must be launched with endpoint environment variables; set "
         <> Text.pack pluginStdioCompatibilityEnv
         <> "=1 only for test/development stdio compatibility"
          )))
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

stdioCompatibilityEnabled :: Maybe String -> Bool
stdioCompatibilityEnabled Nothing = False
stdioCompatibilityEnabled (Just raw) =
  let normalized = Text.toLower (Text.strip (Text.pack raw))
  in normalized `elem` ["1", "true", "yes", "on", "test", "dev", "development"]

-- | Connect to a plugin using pre-opened handles.
--
-- This compatibility path is intended for in-process tests and the
-- legacy stdio fixture harness. Production launches should use
-- 'openPluginServer' on the host side and 'connectPluginFromEnvironment'
-- with endpoint environment variables on the plugin side.
connectPlugin :: Text -> Handle -> Handle -> IO (Either TransportError Transport)
connectPlugin name readH writeH = do
  result <- catch
    (Right <$> mkTransport name readH writeH False)
    (\e -> pure (Left (TransportConnectionFailed (Text.pack (show (e :: SomeException))))))
  pure result

mkTransport :: Text -> Handle -> Handle -> Bool -> IO Transport
mkTransport name readH writeH shared = do
  -- A single duplex Handle serializes reads and writes internally.  Production
  -- socket/pipe endpoints therefore duplicate the handle so the background RPC
  -- receiver cannot block concurrent sends on the same Handle lock.
  writeHandle <- if shared then hDuplicate readH else pure writeH
  hSetBinaryMode readH True
  hSetBinaryMode writeHandle True
  hSetBuffering readH NoBuffering
  hSetBuffering writeHandle NoBuffering
  pure Transport
    { tReadHandle  = readH
    , tWriteHandle = writeHandle
    , tPluginName  = name
    }

-- | Close the transport connection.
closeTransport :: Transport -> IO ()
closeTransport t = do
  catch (hClose (tWriteHandle t)) (\(_ :: SomeException) -> pure ())
  catch (hClose (tReadHandle t)) (\(_ :: SomeException) -> pure ())

#if defined(mingw32_HOST_OS)
------------------------------------------------------------------------
-- Windows named-pipe transport
------------------------------------------------------------------------

type HANDLE = Ptr ()

foreign import ccall safe "windows.h CreateNamedPipeA"
  c_CreateNamedPipe
    :: Ptr CChar    -- lpName
    -> Word32       -- dwOpenMode
    -> Word32       -- dwPipeMode
    -> Word32       -- nMaxInstances
    -> Word32       -- nOutBufferSize
    -> Word32       -- nInBufferSize
    -> Word32       -- nDefaultTimeOut
    -> Ptr ()       -- lpSecurityAttributes
    -> IO HANDLE

foreign import ccall safe "windows.h ConnectNamedPipe"
  c_ConnectNamedPipe :: HANDLE -> Ptr () -> IO CInt

foreign import ccall unsafe "windows.h SetNamedPipeHandleState"
  c_SetNamedPipeHandleState
    :: HANDLE     -- hNamedPipe
    -> Ptr Word32 -- lpMode
    -> Ptr Word32 -- lpMaxCollectionCount
    -> Ptr Word32 -- lpCollectDataTimeout
    -> IO CInt

foreign import ccall safe "windows.h CreateFileA"
  c_CreateFile
    :: Ptr CChar    -- lpFileName
    -> Word32       -- dwDesiredAccess
    -> Word32       -- dwShareMode
    -> Ptr ()       -- lpSecurityAttributes
    -> Word32       -- dwCreationDisposition
    -> Word32       -- dwFlagsAndAttributes
    -> HANDLE       -- hTemplateFile
    -> IO HANDLE

foreign import ccall unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO CInt

foreign import ccall unsafe "windows.h DuplicateHandle"
  c_DuplicateHandle
    :: HANDLE     -- hSourceProcessHandle
    -> HANDLE     -- hSourceHandle
    -> HANDLE     -- hTargetProcessHandle
    -> Ptr HANDLE -- lpTargetHandle
    -> Word32     -- dwDesiredAccess
    -> CInt       -- bInheritHandle
    -> Word32     -- dwOptions
    -> IO CInt

foreign import ccall unsafe "windows.h GetLastError"
  c_GetLastError :: IO Word32

foreign import ccall unsafe "windows.h GetCurrentProcess"
  c_GetCurrentProcess :: IO HANDLE

foreign import ccall unsafe "windows.h GetCurrentProcessId"
  c_GetCurrentProcessId :: IO Word32

foreign import ccall unsafe "windows.h GetNamedPipeClientProcessId"
  c_GetNamedPipeClientProcessId :: HANDLE -> Ptr Word32 -> IO CInt

foreign import ccall unsafe "sddl.h ConvertStringSecurityDescriptorToSecurityDescriptorA"
  c_ConvertStringSecurityDescriptorToSecurityDescriptor
    :: Ptr CChar -- StringSecurityDescriptor
    -> Word32    -- StringSDRevision
    -> Ptr (Ptr ())
    -> Ptr Word32
    -> IO CInt

foreign import ccall unsafe "windows.h LocalFree"
  c_LocalFree :: Ptr () -> IO (Ptr ())

-- | Convert a Win32 HANDLE to a C file descriptor.
foreign import ccall unsafe "_open_osfhandle"
  c_open_osfhandle :: HANDLE -> CInt -> IO CInt

foreign import ccall unsafe "_close"
  c_close :: CInt -> IO CInt

windowsNamedPipePrefix :: FilePath
windowsNamedPipePrefix = "\\\\.\\pipe\\"

errorPipeConnected :: Word32
errorPipeConnected = 535

errorPipeListening :: Word32
errorPipeListening = 536

invalidHandleValue :: HANDLE
invalidHandleValue = nullPtr `plusPtr` (-1)

withRestrictivePipeSecurityAttributes :: (Ptr () -> IO a) -> IO (Either Word32 a)
withRestrictivePipeSecurityAttributes action =
  withCString restrictiveNamedPipeSecurityDescriptor $ \sddlPtr ->
    with nullPtr $ \securityDescriptorPtrPtr -> do
      converted <- c_ConvertStringSecurityDescriptorToSecurityDescriptor
        sddlPtr
        sddlRevision1
        securityDescriptorPtrPtr
        nullPtr
      if converted == 0
        then Left <$> c_GetLastError
        else do
          securityDescriptorPtr <- peek securityDescriptorPtrPtr
          result <- (allocaBytes securityAttributesSize $ \(securityAttributesPtr :: Ptr ()) -> do
            pokeByteOff securityAttributesPtr securityAttributesLengthOffset
              (fromIntegral securityAttributesSize :: Word32)
            pokeByteOff securityAttributesPtr securityAttributesDescriptorOffset securityDescriptorPtr
            pokeByteOff securityAttributesPtr securityAttributesInheritOffset (0 :: CInt)
            action securityAttributesPtr)
            `finally` freeLocalSecurityDescriptor securityDescriptorPtr
          pure (Right result)

-- The server pipe object is limited to the current object owner, LocalSystem,
-- and Administrators instead of inheriting a broad process default DACL.
restrictiveNamedPipeSecurityDescriptor :: String
restrictiveNamedPipeSecurityDescriptor = "D:P(A;;GA;;;SY)(A;;GA;;;BA)(A;;GA;;;OW)"

freeLocalSecurityDescriptor :: Ptr () -> IO ()
freeLocalSecurityDescriptor securityDescriptorPtr = do
  _ <- c_LocalFree securityDescriptorPtr
  pure ()

sddlRevision1 :: Word32
sddlRevision1 = 1

securityAttributesLengthOffset :: Int
securityAttributesLengthOffset = 0

securityAttributesDescriptorOffset :: Int
securityAttributesDescriptorOffset
  | sizeOf (undefined :: Ptr ()) == 8 = 8
  | otherwise = 4

securityAttributesInheritOffset :: Int
securityAttributesInheritOffset = securityAttributesDescriptorOffset + sizeOf (undefined :: Ptr ())

securityAttributesSize :: Int
securityAttributesSize
  | sizeOf (undefined :: Ptr ()) == 8 = 24
  | otherwise = 12

openNamedPipeServer :: TransportConfig -> Text -> IO (Either TransportError TransportServer)
openNamedPipeServer cfg pluginName = catchSync go handler
  where
    go = do
      hostReadPipeName <- allocateNamedPipeName (pluginName <> "-host-read")
      hostWritePipeName <- allocateNamedPipeName (pluginName <> "-host-write")
      readPipeResult <- createNamedPipeEndpoint hostReadPipeName
      case readPipeResult of
        Left err -> pure (Left err)
        Right readPipeH -> do
          writePipeResult <- createNamedPipeEndpoint hostWritePipeName
          case writePipeResult of
            Left err -> do
              closeRawHandle readPipeH
              pure (Left err)
            Right writePipeH -> do
              readOwned <- newIORef True
              writeOwned <- newIORef True
              pure (Right TransportServer
                { tsEndpoint = TransportEndpoint
                    { teKind = TransportEndpointNamedPipe
                    , teAddress = encodeNamedPipePair hostReadPipeName hostWritePipeName
                    }
                , tsAccept = acceptNamedPipeTransportPair cfg pluginName transportPeerPolicyAny
                    readOwned readPipeH hostReadPipeName
                    writeOwned writePipeH hostWritePipeName
                , tsAcceptWithPeerPolicy = \peerPolicy ->
                    acceptNamedPipeTransportPair cfg pluginName peerPolicy
                      readOwned readPipeH hostReadPipeName
                      writeOwned writePipeH hostWritePipeName
                , tsClose = do
                    closeOwnedNamedPipe readOwned readPipeH
                    closeOwnedNamedPipe writeOwned writePipeH
                })
    handler (err :: SomeException) =
      pure (Left (TransportConnectionFailed (Text.pack (show err))))

createNamedPipeEndpoint :: FilePath -> IO (Either TransportError HANDLE)
createNamedPipeEndpoint pipeName = do
  securityResult <- withRestrictivePipeSecurityAttributes $ \securityAttributesPtr ->
    withCString pipeName $ \namePtr ->
      c_CreateNamedPipe
        namePtr
        pipeAccessDuplex
        pipeByteModeNonblocking
        1       -- one client per host-created endpoint
        65536   -- output buffer size
        65536   -- input buffer size
        0       -- default timeout
        securityAttributesPtr
  case securityResult of
    Left err -> pure (Left (TransportConnectionFailed
      ("failed to build restrictive named-pipe security descriptor for "
       <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")))
    Right pipeH
      | pipeH == invalidHandleValue -> do
          err <- c_GetLastError
          pure (Left (TransportConnectionFailed
            ("CreateNamedPipe failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")))
      | otherwise -> pure (Right pipeH)

encodeNamedPipePair :: FilePath -> FilePath -> FilePath
encodeNamedPipePair hostReadPipeName hostWritePipeName = hostReadPipeName <> "|" <> hostWritePipeName

decodeNamedPipePair :: FilePath -> Maybe (FilePath, FilePath)
decodeNamedPipePair raw =
  case break (== '|') raw of
    (hostReadPipeName, '|':hostWritePipeName)
      | not (null hostReadPipeName) && not (null hostWritePipeName) ->
          Just (hostReadPipeName, hostWritePipeName)
    _ -> Nothing

pipeAccessDuplex :: Word32
pipeAccessDuplex = 0x00000003

pipeByteMode :: Word32
pipeByteMode = 0x00000000

pipeByteModeNonblocking :: Word32
pipeByteModeNonblocking = 0x00000001 -- PIPE_NOWAIT

genericReadWrite :: Word32
genericReadWrite = 0x80000000 .|. 0x40000000

openExisting :: Word32
openExisting = 3

fileAttributeNormal :: Word32
fileAttributeNormal = 0x00000080

duplicateSameAccess :: Word32
duplicateSameAccess = 0x00000002

openOsfHandleFlags :: CInt
openOsfHandleFlags = 0x0002 .|. 0x8000 -- _O_RDWR | _O_BINARY

allocateNamedPipeName :: Text -> IO FilePath
allocateNamedPipeName pluginName = do
  processId <- c_GetCurrentProcessId
  unique <- newUnique
  let uniqueSuffix = "-" <> showHex processId "-" <> showHex (hashUnique unique) ""
  pure (windowsNamedPipePrefix <> escapedPluginPipeBase pluginName <> uniqueSuffix)

acceptNamedPipeTransport
  :: TransportConfig
  -> Text
  -> TransportPeerPolicy
  -> IORef Bool
  -> HANDLE
  -> FilePath
  -> IO (Either TransportError Transport)
acceptNamedPipeTransport cfg pluginName peerPolicy owned pipeH pipeName = do
  connectResult <- waitForNamedPipeConnection cfg owned pipeH pipeName
  case connectResult of
    Left err -> pure (Left err)
    Right () -> do
      peerResult <- validateSingleNamedPipePeer pluginName peerPolicy pipeH pipeName
      case peerResult of
        Left err -> closeOwnedNamedPipe owned pipeH >> pure (Left err)
        Right () -> convertNamedPipeHandle owned pipeH pluginName pipeName

acceptNamedPipeTransportPair
  :: TransportConfig
  -> Text
  -> TransportPeerPolicy
  -> IORef Bool
  -> HANDLE
  -> FilePath
  -> IORef Bool
  -> HANDLE
  -> FilePath
  -> IO (Either TransportError Transport)
acceptNamedPipeTransportPair cfg pluginName peerPolicy readOwned readPipeH readPipeName writeOwned writePipeH writePipeName = do
  readConnectResult <- waitForNamedPipeConnection cfg readOwned readPipeH readPipeName
  case readConnectResult of
    Left err -> do
      closeOwnedNamedPipe writeOwned writePipeH
      pure (Left err)
    Right () -> do
      writeConnectResult <- waitForNamedPipeConnection cfg writeOwned writePipeH writePipeName
      case writeConnectResult of
        Left err -> do
          closeOwnedNamedPipe readOwned readPipeH
          pure (Left err)
        Right () -> do
          peerResult <- validateNamedPipePeerPair pluginName peerPolicy readPipeH readPipeName writePipeH writePipeName
          case peerResult of
            Left err -> do
              closeOwnedNamedPipe readOwned readPipeH
              closeOwnedNamedPipe writeOwned writePipeH
              pure (Left err)
            Right () -> convertNamedPipeHandlePair
              readOwned readPipeH readPipeName
              writeOwned writePipeH writePipeName
              pluginName

validateSingleNamedPipePeer :: Text -> TransportPeerPolicy -> HANDLE -> FilePath -> IO (Either TransportError ())
validateSingleNamedPipePeer pluginName peerPolicy pipeH pipeName = do
  peerResult <- namedPipePeerIdentity pipeH pipeName
  pure (peerResult >>= validateTransportPeerIdentity pluginName peerPolicy)

validateNamedPipePeerPair
  :: Text
  -> TransportPeerPolicy
  -> HANDLE
  -> FilePath
  -> HANDLE
  -> FilePath
  -> IO (Either TransportError ())
validateNamedPipePeerPair pluginName peerPolicy readPipeH readPipeName writePipeH writePipeName = do
  readPeerResult <- namedPipePeerIdentity readPipeH readPipeName
  case readPeerResult of
    Left err -> pure (Left err)
    Right readPeer -> do
      writePeerResult <- namedPipePeerIdentity writePipeH writePipeName
      case writePeerResult of
        Left err -> pure (Left err)
        Right writePeer
          | tpiProcessId readPeer /= tpiProcessId writePeer ->
              pure (Left (TransportConnectionFailed
                ("named-pipe peer identity mismatch between pipe handles for "
                 <> pluginName <> ": " <> Text.pack readPipeName <> " has pid "
                 <> maybe "unknown" (Text.pack . show) (tpiProcessId readPeer)
                 <> ", " <> Text.pack writePipeName <> " has pid "
                 <> maybe "unknown" (Text.pack . show) (tpiProcessId writePeer))))
          | otherwise -> pure (validateTransportPeerIdentity pluginName peerPolicy readPeer)

namedPipePeerIdentity :: HANDLE -> FilePath -> IO (Either TransportError TransportPeerIdentity)
namedPipePeerIdentity pipeH pipeName =
  with (0 :: Word32) $ \pidPtr -> do
    ok <- c_GetNamedPipeClientProcessId pipeH pidPtr
    if ok == 0
      then do
        err <- c_GetLastError
        pure (Left (TransportConnectionFailed
          ("GetNamedPipeClientProcessId failed for " <> Text.pack pipeName
           <> " (GetLastError=" <> Text.pack (show err) <> ")")))
      else do
        pid <- peek pidPtr
        pure (Right TransportPeerIdentity
          { tpiProcessId = Just (fromIntegral pid)
          , tpiUserId = Nothing
          })

waitForNamedPipeConnection
  :: TransportConfig
  -> IORef Bool
  -> HANDLE
  -> FilePath
  -> IO (Either TransportError ())
waitForNamedPipeConnection cfg owned pipeH pipeName = loop 0
  where
    timeoutMicros = tcTimeout cfg * 1000
    pollDelayMicros = 10000

    loop elapsedMicros = do
      connectState <- pollNamedPipeConnection pipeH
      case connectState of
        NamedPipeConnected -> setNamedPipeBlocking owned pipeH pipeName
        NamedPipeWaiting
          | timeoutMicros > 0 && elapsedMicros >= timeoutMicros -> do
              closeOwnedNamedPipe owned pipeH
              pure (Left (TransportConnectionFailed
                ("timed out waiting for plugin connection on " <> Text.pack pipeName)))
          | otherwise -> do
              let delayMicros = case timeoutMicros > 0 of
                    True -> min pollDelayMicros (max 1 (timeoutMicros - elapsedMicros))
                    False -> pollDelayMicros
              threadDelay delayMicros
              loop (elapsedMicros + delayMicros)
        NamedPipeFailed errCode -> do
          closeOwnedNamedPipe owned pipeH
          pure (Left (TransportConnectionFailed
            ("ConnectNamedPipe failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show errCode) <> ")")))

data NamedPipeConnectState
  = NamedPipeConnected
  | NamedPipeWaiting
  | NamedPipeFailed !Word32

pollNamedPipeConnection :: HANDLE -> IO NamedPipeConnectState
pollNamedPipeConnection pipeH = do
  ok <- c_ConnectNamedPipe pipeH nullPtr
  if ok /= 0
    then pure NamedPipeConnected
    else do
      err <- c_GetLastError
      if err == errorPipeConnected
        then pure NamedPipeConnected
        else if err == errorPipeListening
          then pure NamedPipeWaiting
          else pure (NamedPipeFailed err)

setNamedPipeBlocking :: IORef Bool -> HANDLE -> FilePath -> IO (Either TransportError ())
setNamedPipeBlocking owned pipeH pipeName = do
  ok <- with pipeByteMode $ \modePtr ->
    c_SetNamedPipeHandleState pipeH modePtr nullPtr nullPtr
  if ok /= 0
    then pure (Right ())
    else do
      err <- c_GetLastError
      closeOwnedNamedPipe owned pipeH
      pure (Left (TransportConnectionFailed
        ("SetNamedPipeHandleState failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")))

convertNamedPipeHandlePair
  :: IORef Bool
  -> HANDLE
  -> FilePath
  -> IORef Bool
  -> HANDLE
  -> FilePath
  -> Text
  -> IO (Either TransportError Transport)
convertNamedPipeHandlePair readOwned readPipeH readPipeName writeOwned writePipeH writePipeName pluginName = do
  readHandleResult <- namedPipeHandleToHandle readOwned readPipeH readPipeName
  case readHandleResult of
    Left err -> do
      closeOwnedNamedPipe writeOwned writePipeH
      pure (Left err)
    Right readHandle -> do
      writeHandleResult <- namedPipeHandleToHandle writeOwned writePipeH writePipeName
      case writeHandleResult of
        Left err -> do
          safeCloseHandle readHandle
          pure (Left err)
        Right writeHandle -> do
          transportResult <- try (mkTransport pluginName readHandle writeHandle False)
            :: IO (Either SomeException Transport)
          case transportResult of
            Left err -> do
              safeCloseHandle readHandle
              safeCloseHandle writeHandle
              pure (Left (TransportConnectionFailed (Text.pack (show err))))
            Right transport -> pure (Right transport)

namedPipeHandleToHandle :: IORef Bool -> HANDLE -> FilePath -> IO (Either TransportError Handle)
namedPipeHandleToHandle owned pipeH pipeName = do
  fd <- c_open_osfhandle pipeH openOsfHandleFlags
  if fd == (-1)
    then do
      err <- c_GetLastError
      closeOwnedNamedPipe owned pipeH
      pure (Left (openOsfHandleError pipeName err))
    else do
      writeIORef owned False
      handleResult <- try (fdToHandle (fromIntegral fd))
        :: IO (Either SomeException Handle)
      case handleResult of
        Left err -> do
          closeFileDescriptor fd
          pure (Left (TransportConnectionFailed (Text.pack (show err))))
        Right handle -> pure (Right handle)

convertNamedPipeHandle :: IORef Bool -> HANDLE -> Text -> FilePath -> IO (Either TransportError Transport)
convertNamedPipeHandle owned pipeH pluginName pipeName = do
  writeHandleResult <- duplicateNamedPipeHandle pipeH pipeName
  case writeHandleResult of
    Left err -> do
      closeOwnedNamedPipe owned pipeH
      pure (Left err)
    Right writePipeH -> do
      readFd <- c_open_osfhandle pipeH openOsfHandleFlags
      if readFd == (-1)
        then do
          err <- c_GetLastError
          closeRawHandle writePipeH
          closeOwnedNamedPipe owned pipeH
          pure (Left (openOsfHandleError pipeName err))
        else do
          -- _open_osfhandle transfers HANDLE ownership to the C file descriptor.
          writeIORef owned False
          writeFd <- c_open_osfhandle writePipeH openOsfHandleFlags
          if writeFd == (-1)
            then do
              err <- c_GetLastError
              closeFileDescriptor readFd
              closeRawHandle writePipeH
              pure (Left (openOsfHandleError pipeName err))
            else do
              readHandleResult <- try (fdToHandle (fromIntegral readFd))
                :: IO (Either SomeException Handle)
              case readHandleResult of
                Left err -> do
                  closeFileDescriptor readFd
                  closeFileDescriptor writeFd
                  pure (Left (TransportConnectionFailed (Text.pack (show err))))
                Right readHandle -> do
                  writeHandleResult' <- try (fdToHandle (fromIntegral writeFd))
                    :: IO (Either SomeException Handle)
                  case writeHandleResult' of
                    Left err -> do
                      safeCloseHandle readHandle
                      closeFileDescriptor writeFd
                      pure (Left (TransportConnectionFailed (Text.pack (show err))))
                    Right writeHandle -> do
                      transportResult <- try (mkTransport pluginName readHandle writeHandle False)
                        :: IO (Either SomeException Transport)
                      case transportResult of
                        Left err -> do
                          safeCloseHandle readHandle
                          safeCloseHandle writeHandle
                          pure (Left (TransportConnectionFailed (Text.pack (show err))))
                        Right transport -> pure (Right transport)

duplicateNamedPipeHandle :: HANDLE -> FilePath -> IO (Either TransportError HANDLE)
duplicateNamedPipeHandle pipeH pipeName = do
  currentProcess <- c_GetCurrentProcess
  with nullPtr $ \targetPtr -> do
    ok <- c_DuplicateHandle currentProcess pipeH currentProcess targetPtr 0 0 duplicateSameAccess
    if ok == 0
      then do
        err <- c_GetLastError
        pure (Left (TransportConnectionFailed
          ("DuplicateHandle failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")))
      else Right <$> peek targetPtr

openOsfHandleError :: FilePath -> Word32 -> TransportError
openOsfHandleError pipeName err = TransportConnectionFailed
  ("_open_osfhandle failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")

closeOwnedNamedPipe :: IORef Bool -> HANDLE -> IO ()
closeOwnedNamedPipe owned pipeH = do
  shouldClose <- atomicModifyIORef' owned (\isOwned -> (False, isOwned))
  if shouldClose
    then closeRawHandle pipeH
    else pure ()

closeRawHandle :: HANDLE -> IO ()
closeRawHandle handle = do
  _ <- try (c_CloseHandle handle) :: IO (Either SomeException CInt)
  pure ()

closeFileDescriptor :: CInt -> IO ()
closeFileDescriptor fd = do
  _ <- try (c_close fd) :: IO (Either SomeException CInt)
  pure ()

safeCloseHandle :: Handle -> IO ()
safeCloseHandle handle = do
  _ <- try (hClose handle) :: IO (Either SomeException ())
  pure ()

connectNamedPipeEndpoint :: Text -> FilePath -> IO (Either TransportError Transport)
connectNamedPipeEndpoint name pipeName = catchSync go handler
  where
    go = case decodeNamedPipePair pipeName of
      Just (hostReadPipeName, hostWritePipeName) ->
        connectNamedPipeEndpointPair name hostReadPipeName hostWritePipeName
      Nothing -> do
        pipeResult <- openExistingNamedPipe pipeName
        case pipeResult of
          Left err -> pure (Left err)
          Right pipeH -> do
            owned <- newIORef True
            convertNamedPipeHandle owned pipeH name pipeName
    handler (err :: SomeException) =
      pure (Left (TransportConnectionFailed (Text.pack (show err))))

connectNamedPipeEndpointPair :: Text -> FilePath -> FilePath -> IO (Either TransportError Transport)
connectNamedPipeEndpointPair name hostReadPipeName hostWritePipeName = do
  writePipeResult <- openExistingNamedPipe hostReadPipeName
  case writePipeResult of
    Left err -> pure (Left err)
    Right writePipeH -> do
      readPipeResult <- openExistingNamedPipe hostWritePipeName
      case readPipeResult of
        Left err -> do
          closeRawHandle writePipeH
          pure (Left err)
        Right readPipeH -> do
          readOwned <- newIORef True
          writeOwned <- newIORef True
          convertNamedPipeHandlePair
            readOwned readPipeH hostWritePipeName
            writeOwned writePipeH hostReadPipeName
            name

openExistingNamedPipe :: FilePath -> IO (Either TransportError HANDLE)
openExistingNamedPipe pipeName = do
  pipeH <- withCString pipeName $ \namePtr ->
    c_CreateFile
      namePtr
      genericReadWrite
      0                   -- exclusive access to this client endpoint
      nullPtr             -- default security attributes
      openExisting
      fileAttributeNormal
      nullPtr
  if pipeH == invalidHandleValue
    then do
      err <- c_GetLastError
      pure (Left (TransportConnectionFailed
        ("CreateFile failed for " <> Text.pack pipeName <> " (GetLastError=" <> Text.pack (show err) <> ")")))
    else pure (Right pipeH)
#endif

#if !defined(mingw32_HOST_OS)
#if defined(linux_HOST_OS)
foreign import ccall unsafe "sys/socket.h getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> Ptr CUInt -> IO CInt
#endif

#if defined(darwin_HOST_OS)
foreign import ccall unsafe "unistd.h getpeereid"
  c_getpeereid :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
#endif

openUnixSocketServer :: TransportConfig -> Text -> IO (Either TransportError TransportServer)
openUnixSocketServer cfg pluginName = catchSync go handler
  where
    go = do
      socketPath <- allocateUnixSocketPath cfg pluginName
      socketResult <- trySync (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
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
          setupResult <- trySync (setup `onException` cleanup) :: IO (Either SomeException ())
          case setupResult of
            Left err -> do
              cleanup
              pure (Left (TransportConnectionFailed (Text.pack (show err))))
            Right () -> pure (Right (TransportServer
              { tsEndpoint = TransportEndpoint
                  { teKind = TransportEndpointUnixSocket
                  , teAddress = socketPath
                  }
              , tsAccept = acceptUnixSocketTransport cfg pluginName transportPeerPolicyAny sock socketPath
              , tsAcceptWithPeerPolicy = \peerPolicy ->
                  acceptUnixSocketTransport cfg pluginName peerPolicy sock socketPath
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
  -> TransportPeerPolicy
  -> Socket.Socket
  -> FilePath
  -> IO (Either TransportError Transport)
acceptUnixSocketTransport cfg pluginName peerPolicy sock socketPath = do
  let cleanup = do
        safeCloseSocket sock
        removeUnixSocketPath socketPath
      acceptOnce = Socket.accept sock
      waitForAccept
        | tcTimeout cfg <= 0 = Just <$> acceptOnce
        | otherwise = timeout (tcTimeout cfg * 1000) acceptOnce
  acceptResult <- trySync waitForAccept :: IO (Either SomeException (Maybe (Socket.Socket, Socket.SockAddr)))
  case acceptResult of
    Left err -> do
      cleanup
      pure (Left (TransportConnectionFailed (Text.pack (show err))))
    Right Nothing -> do
      cleanup
      pure (Left (TransportConnectionFailed
        ("timed out waiting for plugin connection on " <> Text.pack socketPath)))
    Right (Just (conn, _addr)) -> do
      peerResult <- unixSocketPeerIdentity conn
      case peerResult >>= validateTransportPeerIdentity pluginName peerPolicy of
        Left err -> do
          safeCloseSocket conn
          cleanup
          pure (Left err)
        Right () -> do
          safeCloseSocket sock
          removeUnixSocketPath socketPath
          handleResult <- trySync ((Socket.socketToHandle conn ReadWriteMode >>= \h -> mkTransport pluginName h h True) `onException` safeCloseSocket conn)
            :: IO (Either SomeException Transport)
          case handleResult of
            Left err -> do
              safeCloseSocket conn
              pure (Left (TransportConnectionFailed (Text.pack (show err))))
            Right transport -> pure (Right transport)

unixSocketPeerIdentity :: Socket.Socket -> IO (Either TransportError TransportPeerIdentity)
unixSocketPeerIdentity sock =
#if defined(linux_HOST_OS)
  Socket.withFdSocket sock linuxSocketPeerIdentity
#elif defined(darwin_HOST_OS)
  Socket.withFdSocket sock darwinSocketPeerIdentity
#else
  pure (Right TransportPeerIdentity
    { tpiProcessId = Nothing
    , tpiUserId = Nothing
    })
#endif

#if defined(linux_HOST_OS)
linuxSocketPeerIdentity :: CInt -> IO (Either TransportError TransportPeerIdentity)
linuxSocketPeerIdentity fd =
  allocaBytes linuxUCredSize $ \(credPtr :: Ptr ()) ->
    with (fromIntegral linuxUCredSize :: CUInt) $ \lenPtr -> do
      ok <- c_getsockopt fd solSocket soPeerCred credPtr lenPtr
      if ok /= 0
        then pure (Left (TransportConnectionFailed "getsockopt(SO_PEERCRED) failed for Unix socket peer"))
        else do
          pid <- peekByteOff credPtr linuxUCredPidOffset :: IO CInt
          uid <- peekByteOff credPtr linuxUCredUidOffset :: IO CUInt
          pure (Right TransportPeerIdentity
            { tpiProcessId = if pid > 0 then Just (fromIntegral pid) else Nothing
            , tpiUserId = Just (fromIntegral uid)
            })

solSocket :: CInt
solSocket = 1

soPeerCred :: CInt
soPeerCred = 17

linuxUCredPidOffset :: Int
linuxUCredPidOffset = 0

linuxUCredUidOffset :: Int
linuxUCredUidOffset = 4

linuxUCredSize :: Int
linuxUCredSize = 12
#endif

#if defined(darwin_HOST_OS)
darwinSocketPeerIdentity :: CInt -> IO (Either TransportError TransportPeerIdentity)
darwinSocketPeerIdentity fd =
  with (0 :: CUInt) $ \uidPtr ->
    with (0 :: CUInt) $ \gidPtr -> do
      ok <- c_getpeereid fd uidPtr gidPtr
      if ok /= 0
        then pure (Left (TransportConnectionFailed "getpeereid failed for Unix socket peer"))
        else do
          uid <- peek uidPtr
          pure (Right TransportPeerIdentity
            { tpiProcessId = Nothing
            , tpiUserId = Just (fromIntegral uid)
            })
#endif

connectUnixSocketEndpoint :: Text -> FilePath -> IO (Either TransportError Transport)
connectUnixSocketEndpoint name socketPath = do
  socketResult <- trySync (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    :: IO (Either SomeException Socket.Socket)
  case socketResult of
    Left err -> pure (Left (TransportConnectionFailed (Text.pack (show err))))
    Right sock -> do
      connectResult <- trySync $ do
        (do
          Socket.connect sock (Socket.SockAddrUnix socketPath)
          Socket.socketToHandle sock ReadWriteMode >>= \h -> mkTransport name h h True)
          `onException` safeCloseSocket sock
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

-- | Default maximum payload size for a framed message (64 MiB).
--
-- The frame header is always 4 bytes; this limit applies to the JSON payload.
defaultMaxFrameSizeBytes :: Int
defaultMaxFrameSizeBytes = 64 * 1024 * 1024

-- | Send a length-prefixed message over the transport.
--
-- Encodes the payload length as a 4-byte little-endian 'Word32',
-- followed by the raw payload bytes.
sendMessage :: Transport -> BS.ByteString -> IO (Either TransportError ())
sendMessage = sendMessageWithLimit defaultMaxFrameSizeBytes

-- | Send a length-prefixed message with an explicit payload size limit.
sendMessageWithLimit :: Int -> Transport -> BS.ByteString -> IO (Either TransportError ())
sendMessageWithLimit maxFrameBytes t payload
  | maxFrameBytes <= 0 = pure (Left (TransportFramingError "max frame size must be positive"))
  | BS.length payload > maxFrameBytes = pure (Left (frameTooLargeError (BS.length payload) maxFrameBytes))
  | otherwise = catch go handler
  where
    go = do
      let len = fromIntegral (BS.length payload) :: Word32
          header = BL.toStrict (runPut (putWord32le len))
      BS.hPut (tWriteHandle t) header
      BS.hPut (tWriteHandle t) payload
      hFlush (tWriteHandle t)
      pure (Right ())
    handler :: IOException -> IO (Either TransportError ())
    handler e = pure (Left (TransportSendFailed (Text.pack (show e))))

-- | Receive a length-prefixed message from the transport.
--
-- Reads a 4-byte little-endian length, then reads that many bytes
-- of payload.  Returns 'TransportRecvFailed' on EOF or short read.
recvMessage :: Transport -> IO (Either TransportError BS.ByteString)
recvMessage = recvMessageWithLimit defaultMaxFrameSizeBytes

-- | Receive a length-prefixed message with an explicit payload size limit.
recvMessageWithLimit :: Int -> Transport -> IO (Either TransportError BS.ByteString)
recvMessageWithLimit maxFrameBytes t
  | maxFrameBytes <= 0 = pure (Left (TransportFramingError "max frame size must be positive"))
  | otherwise = catch go handler
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
            if payloadLen > maxFrameBytes
              then pure (Left (frameTooLargeError payloadLen maxFrameBytes))
              else do
                payload <- BS.hGet (tReadHandle t) payloadLen
                if BS.length payload < payloadLen
                  then pure (Left (TransportRecvFailed
                        ("short read: expected " <> Text.pack (show payloadLen)
                         <> " bytes, got " <> Text.pack (show (BS.length payload)))))
                  else do
                    _ <- evaluate payload
                    pure (Right payload)
    handler :: IOException -> IO (Either TransportError BS.ByteString)
    handler e = pure (Left (TransportRecvFailed (Text.pack (show e))))

frameTooLargeError :: Int -> Int -> TransportError
frameTooLargeError actual maxFrameBytes = TransportFramingError
  ("frame exceeds max size: " <> Text.pack (show actual)
   <> " bytes > " <> Text.pack (show maxFrameBytes) <> " bytes")
