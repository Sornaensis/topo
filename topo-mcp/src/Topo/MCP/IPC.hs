{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC client for connecting to a running topo-seer instance.
--
-- On Windows, connects to a named pipe (@\\\\.\\pipe\\topo-seer-cmd@).
-- On Unix, connects to a Unix domain socket (@\/tmp\/topo-seer-cmd.sock@).
--
-- Uses the same length-prefixed JSON framing as 'Topo.Plugin.RPC.Transport'.
--
-- The connection is lazy: 'withConnection' establishes the connection on
-- first use and automatically reconnects if it drops.
module Topo.MCP.IPC
  ( IpcConnection(..)
  , IpcConnectionRef
  , newIpcConnectionRef
  , connectToSeer
  , disconnectFromSeer
  , sendCommand
  , withConnection
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO
  ( Handle
  , IOMode(..)
  , BufferMode(..)
  , hClose
  , hSetBinaryMode
  , hSetBuffering
  , hPutStrLn
  , stderr
  )
import Topo.Command.Types (SeerCommand(..), SeerResponse, commandPipeName)
import Topo.Plugin.RPC.Transport (Transport(..), sendMessage, recvMessage)

#if defined(mingw32_HOST_OS)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt(..))
import Data.Word (Word32)
import GHC.IO.Handle.FD (fdToHandle)
#else
import qualified Network.Socket as Net
#endif

-- | An open IPC connection to topo-seer.
data IpcConnection = IpcConnection
  { ipcTransport :: !Transport
  , ipcNextId    :: !Int
  }

-- | A mutable reference to an optional IPC connection.
--
-- When 'Nothing', no connection has been established (or the previous
-- connection was lost).  'withConnection' lazily connects on demand.
type IpcConnectionRef = IORef (Maybe IpcConnection)

-- | Create a new 'IpcConnectionRef' starting with no connection.
newIpcConnectionRef :: IO IpcConnectionRef
newIpcConnectionRef = newIORef Nothing

-- | Connect to a running topo-seer instance.
--
-- Returns 'Nothing' if topo-seer is not running or the connection fails.
connectToSeer :: Maybe FilePath -> IO (Maybe IpcConnection)
connectToSeer mPipeName = do
  let pipeName = maybe commandPipeName id mPipeName
  hPutStrLn stderr ("[topo-mcp] connecting to " ++ pipeName)
  result <- tryConnect pipeName
  case result of
    Nothing -> do
      hPutStrLn stderr "[topo-mcp] connection failed"
      pure Nothing
    Just transport -> do
      hPutStrLn stderr "[topo-mcp] connected"
      pure $ Just IpcConnection
        { ipcTransport = transport
        , ipcNextId    = 1
        }

-- | Disconnect from topo-seer.
disconnectFromSeer :: IpcConnection -> IO ()
disconnectFromSeer conn = do
  catch (hClose (tReadHandle (ipcTransport conn))) (\(_ :: SomeException) -> pure ())
  catch (hClose (tWriteHandle (ipcTransport conn))) (\(_ :: SomeException) -> pure ())

-- | Send a command to topo-seer and receive the response.
--
-- Returns the 'SeerResponse' and the updated connection (with incremented ID).
sendCommand :: IpcConnection -> Text -> Aeson.Value -> IO (Either Text SeerResponse, IpcConnection)
sendCommand conn method params = do
  let reqId = ipcNextId conn
      cmd = SeerCommand
        { scId     = reqId
        , scMethod = method
        , scParams = params
        }
      conn' = conn { ipcNextId = reqId + 1 }
      transport = ipcTransport conn
  sendResult <- sendMessage transport (BL.toStrict (Aeson.encode cmd))
  case sendResult of
    Left err -> pure (Left ("send failed: " <> Text.pack (show err)), conn')
    Right () -> do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> pure (Left ("recv failed: " <> Text.pack (show err)), conn')
        Right payload ->
          case Aeson.eitherDecodeStrict payload of
            Left parseErr -> pure (Left ("JSON parse error: " <> Text.pack parseErr), conn')
            Right rsp -> pure (Right rsp, conn')

-- | Execute an action that needs a live IPC connection, connecting or
-- reconnecting as necessary.
--
-- If connection fails, returns @Left@ with an error message.
-- On success, updates the 'IpcConnectionRef' with the (possibly new)
-- connection state.
withConnection
  :: IpcConnectionRef
  -> (IpcConnection -> IO (Either Text a, IpcConnection))
  -> IO (Either Text a)
withConnection connRef action = do
  mConn <- readIORef connRef
  conn <- case mConn of
    Just c  -> pure (Just c)
    Nothing -> connectToSeer Nothing
  case conn of
    Nothing -> do
      writeIORef connRef Nothing
      pure (Left "topo-seer is not running (could not connect to IPC pipe)")
    Just c -> do
      (result, c') <- action c
        `catch` \(e :: SomeException) -> do
          -- Connection broke; clear it so next call reconnects.
          hPutStrLn stderr ("[topo-mcp] IPC error: " ++ show e)
          pure (Left ("IPC error: " <> Text.pack (show e)), c)
      case result of
        -- On send/recv failure, clear connection to force reconnect next time.
        Left err | isConnectionError err -> do
          catch (disconnectFromSeer c') (\(_ :: SomeException) -> pure ())
          writeIORef connRef Nothing
          pure (Left err)
        _ -> do
          writeIORef connRef (Just c')
          pure result

-- | Heuristic: treat send/recv failures as connection errors.
isConnectionError :: Text -> Bool
isConnectionError msg =
  Text.isInfixOf "send failed" msg
  || Text.isInfixOf "recv failed" msg

------------------------------------------------------------------------
-- Platform-specific connection
------------------------------------------------------------------------

#if defined(mingw32_HOST_OS)

-- Windows: Connect to a named pipe using CreateFileA

type HANDLE = Ptr ()

foreign import ccall unsafe "windows.h CreateFileA"
  c_CreateFileA
    :: Ptr CChar    -- lpFileName
    -> Word32       -- dwDesiredAccess
    -> Word32       -- dwShareMode
    -> Ptr ()       -- lpSecurityAttributes
    -> Word32       -- dwCreationDisposition
    -> Word32       -- dwFlagsAndAttributes
    -> Ptr ()       -- hTemplateFile
    -> IO HANDLE

foreign import ccall unsafe "_open_osfhandle"
  c_open_osfhandle :: HANDLE -> CInt -> IO CInt

tryConnect :: FilePath -> IO (Maybe Transport)
tryConnect pipeName = do
  let genericRead  = 0x80000000 :: Word32
      genericWrite = 0x40000000 :: Word32
      openExisting = 3 :: Word32
  pipeH <- withCString pipeName $ \namePtr ->
    c_CreateFileA
      namePtr
      (genericRead + genericWrite)
      0           -- no sharing
      nullPtr     -- default security
      openExisting
      0           -- default attributes
      nullPtr     -- no template
  let badHandle = nullPtr `plusPtr` (-1)  -- INVALID_HANDLE_VALUE
  if pipeH == badHandle
    then pure Nothing
    else do
      fd <- c_open_osfhandle pipeH 0
      if fd == (-1)
        then pure Nothing
        else do
          h <- fdToHandle (fromIntegral fd)
          hSetBinaryMode h True
          hSetBuffering h (BlockBuffering Nothing)
          pure $ Just Transport
            { tReadHandle  = h
            , tWriteHandle = h
            , tPluginName  = "topo-mcp"
            }

#else

-- Unix: Connect to a Unix domain socket

tryConnect :: FilePath -> IO (Maybe Transport)
tryConnect sockPath =
  catch go (\(_ :: SomeException) -> pure Nothing)
  where
    go = do
      sock <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
      Net.connect sock (Net.SockAddrUnix sockPath)
      h <- Net.socketToHandle sock ReadWriteMode
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      pure $ Just Transport
        { tReadHandle  = h
        , tWriteHandle = h
        , tPluginName  = "topo-mcp"
        }

#endif
