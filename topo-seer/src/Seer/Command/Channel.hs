{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC command channel listener for topo-seer.
--
-- On Windows, listens on a named pipe (@\\\\.\\pipe\\topo-seer-cmd@).
-- On Unix, listens on a Unix domain socket (@\/tmp\/topo-seer-cmd.sock@).
--
-- Each accepted client connection gets its own lightweight Haskell thread
-- that reads 'SeerCommand' messages, dispatches them, and writes
-- 'SeerResponse' messages back.  The channel is always-on; if nothing
-- connects, the listener thread just sleeps in an accept loop.
module Seer.Command.Channel
  ( CommandChannelEnv(..)
  , runCommandChannel
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import System.IO (Handle, hPutStrLn, stderr, hSetBinaryMode, hSetBuffering, BufferMode(..), hClose)
import Topo.Command.Types (SeerCommand(..), SeerResponse(..), errResponse, commandPipeName)
import Topo.Plugin.RPC.Transport (Transport(..), sendMessage, recvMessage)
import Seer.Command.Dispatch (dispatchCommand, CommandContext(..))
import Seer.Screenshot (ScreenshotRequestRef)
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshotRef, appendLog)
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (UiSnapshotRef)
import Hyperspace.Actor (ActorHandle, Protocol)

#if defined(mingw32_HOST_OS)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt(..))
import GHC.IO.Handle.FD (fdToHandle)
#else
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Network.Socket as Net
import System.Directory (removeFile, doesFileExist)
import System.IO (IOMode(..))
#endif

-- | Environment for the command channel.
data CommandChannelEnv = CommandChannelEnv
  { cceActorHandles    :: !ActorHandles
  , cceUiSnapshotRef   :: !UiSnapshotRef
  , cceUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , cceScreenshotRef   :: !ScreenshotRequestRef
  , cceLogSnapshotRef  :: !(Maybe LogSnapshotRef)
  }

-- | Run the command channel listener.
--
-- This function blocks forever, accepting client connections and
-- spawning a handler thread for each.  It should be run in its own
-- 'forkIO' or 'async' thread.
runCommandChannel :: CommandChannelEnv -> IO ()
runCommandChannel env = do
  hPutStrLn stderr ("[cmd-channel] listening on " ++ commandPipeName)
  listenLoop env

-- --------------------------------------------------------------------------
-- Platform-specific listener
-- --------------------------------------------------------------------------

#if defined(mingw32_HOST_OS)

-- Windows: Named pipe implementation using Win32 API + _open_osfhandle

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
  c_ConnectNamedPipe :: HANDLE -> Ptr () -> IO Word8

foreign import ccall unsafe "windows.h DisconnectNamedPipe"
  c_DisconnectNamedPipe :: HANDLE -> IO Word8

foreign import ccall unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO Word8

-- | Convert a Win32 HANDLE to a C file descriptor.
-- @_open_osfhandle(handle, flags)@ — flags=0 for binary mode.
foreign import ccall unsafe "_open_osfhandle"
  c_open_osfhandle :: HANDLE -> CInt -> IO CInt

listenLoop :: CommandChannelEnv -> IO ()
listenLoop env = go
  where
    pipeAccessDuplex :: Word32
    pipeAccessDuplex = 0x00000003

    pipeTypeFlags :: Word32
    pipeTypeFlags = 0x00000000  -- PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT

    pipeUnlimitedInstances :: Word32
    pipeUnlimitedInstances = 255

    go = do
      -- Create a new pipe instance for this connection.
      -- Each call to CreateNamedPipe creates a new instance of the same
      -- named pipe; ConnectNamedPipe then blocks until a client connects.
      pipeH <- withCString commandPipeName $ \namePtr ->
        c_CreateNamedPipe
          namePtr
          pipeAccessDuplex
          pipeTypeFlags
          pipeUnlimitedInstances
          65536   -- output buffer size
          65536   -- input buffer size
          0       -- default timeout (wait forever)
          nullPtr -- default security attributes
      let badHandle = nullPtr `plusPtr` (-1)  -- INVALID_HANDLE_VALUE
      if pipeH == badHandle
        then hPutStrLn stderr "[cmd-channel] CreateNamedPipe failed"
        else do
          -- Block until a client connects to this pipe instance.
          _ <- c_ConnectNamedPipe pipeH nullPtr
          -- Convert Win32 HANDLE → C fd → GHC Handle
          fd <- c_open_osfhandle pipeH 0  -- 0 = binary mode
          if fd == (-1)
            then do
              hPutStrLn stderr "[cmd-channel] _open_osfhandle failed"
              _ <- c_CloseHandle pipeH
              pure ()
            else do
              h <- fdToHandle (fromIntegral fd)
              hSetBinaryMode h True
              hSetBuffering h (BlockBuffering Nothing)
              let transport = Transport
                    { tReadHandle  = h
                    , tWriteHandle = h
                    , tPluginName  = "cmd-client"
                    }
              _ <- forkIO (handleClient env transport `finally` hClose h)
              pure ()
          -- Loop to accept the next connection.
          go

#else

-- Unix: Unix domain socket implementation

listenLoop :: CommandChannelEnv -> IO ()
listenLoop env = bracket setup cleanup acceptLoop
  where
    sockPath = commandPipeName

    setup = do
      exists <- doesFileExist sockPath
      when exists $ removeFile sockPath
      sock <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
      Net.bind sock (Net.SockAddrUnix sockPath)
      Net.listen sock 5
      pure sock

    cleanup sock = do
      Net.close sock
      catch (removeFile sockPath) (\(_ :: SomeException) -> pure ())

    acceptLoop sock = do
      (conn, _) <- Net.accept sock
      h <- Net.socketToHandle conn ReadWriteMode
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      let transport = Transport
            { tReadHandle  = h
            , tWriteHandle = h
            , tPluginName  = "cmd-client"
            }
      _ <- forkIO (handleClient env transport `finally` hClose h)
      acceptLoop sock

#endif

-- --------------------------------------------------------------------------
-- Client handler (platform-independent)
-- --------------------------------------------------------------------------

-- | Handle a single client connection.
--
-- Reads commands in a loop, dispatches each, and writes the response.
-- The loop exits when the client disconnects (EOF / transport error).
-- Mutation and screenshot commands are logged to the seer console.
handleClient :: CommandChannelEnv -> Transport -> IO ()
handleClient env transport = do
  logMsg LogInfo "[mcp] client connected"
  loop
  where
    logHandle = ahLogHandle (cceActorHandles env)
    logMsg level msg = appendLog logHandle (LogEntry level msg)

    ctx = CommandContext
      { ccActorHandles    = cceActorHandles env
      , ccUiSnapshotRef   = cceUiSnapshotRef env
      , ccUiActionsHandle = cceUiActionsHandle env
      , ccScreenshotRef   = cceScreenshotRef env
      , ccLogSnapshotRef  = cceLogSnapshotRef env
      }

    loop = do
      result <- recvMessage transport
      case result of
        Left _err -> do
          -- Client disconnected or transport error; exit the loop.
          hPutStrLn stderr "[cmd-channel] client disconnected"
          logMsg LogInfo "[mcp] client disconnected"
          pure ()
        Right payload -> do
          case Aeson.eitherDecodeStrict payload of
            Left parseErr -> do
              -- Malformed JSON; send an error response with id -1.
              let rsp = errResponse (-1) (Text.pack ("JSON parse error: " <> parseErr))
              logMsg LogWarn ("[mcp] JSON parse error: " <> Text.pack parseErr)
              _ <- sendResponse transport rsp
              loop
            Right cmd -> do
              let method = scMethod cmd
              rsp <- dispatchCommand ctx cmd
                `catch` \(e :: SomeException) -> do
                  logMsg LogError ("[mcp] " <> method <> " internal error: " <> Text.pack (show e))
                  pure (errResponse (scId cmd) (Text.pack ("internal error: " <> show e)))
              -- Log mutation/input commands to the seer console.
              case commandCategory method of
                CatMutation -> logCommandResult method rsp
                CatQuery    -> pure ()
              _ <- sendResponse transport rsp
              loop

    logCommandResult method rsp
      | srSuccess rsp = logMsg LogInfo ("[mcp] " <> method <> " ok")
      | otherwise = case srError rsp of
          Just err -> logMsg LogWarn ("[mcp] " <> method <> " error: " <> err)
          Nothing  -> logMsg LogWarn ("[mcp] " <> method <> " failed")

    sendResponse t rsp = sendMessage t (BL.toStrict (Aeson.encode rsp))

-- | Command category for logging purposes.
data CommandCategory = CatQuery | CatMutation

-- | Classify IPC methods: only mutations and screenshots are logged.
commandCategory :: Text.Text -> CommandCategory
commandCategory method = case method of
  "set_slider"       -> CatMutation
  "set_sliders"      -> CatMutation
  "reset_sliders"    -> CatMutation
  "set_seed"         -> CatMutation
  "set_view_mode"    -> CatMutation
  "set_config_tab"   -> CatMutation
  "select_hex"       -> CatMutation
  "generate"         -> CatMutation
  "save_world"       -> CatMutation
  "load_world"       -> CatMutation
  "save_preset"      -> CatMutation
  "load_preset"      -> CatMutation
  "take_screenshot"  -> CatMutation
  "set_camera"         -> CatMutation
  "zoom_to_chunk"      -> CatMutation
  "set_world_name"     -> CatMutation
  "set_stage_enabled"  -> CatMutation
  "set_plugin_enabled" -> CatMutation
  "set_plugin_param"   -> CatMutation
  "set_sim_auto_tick"  -> CatMutation
  "sim_tick"           -> CatMutation
  _                    -> CatQuery
