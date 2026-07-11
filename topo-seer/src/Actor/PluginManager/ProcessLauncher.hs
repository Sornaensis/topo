{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and production transport attachment.
module Actor.PluginManager.ProcessLauncher
  ( LaunchPluginResult(..)
  , resolvePluginExecutable
  , launchPluginTransport
  , safeCloseHandle
  , safeTerminateProcess
  ) where

#if defined(mingw32_HOST_OS)
import Control.Concurrent (forkIO, threadDelay)
#else
import Control.Concurrent (threadDelay)
#endif
#if defined(mingw32_HOST_OS)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
#endif
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , finally
  , fromException
  , mask
  , onException
  , throwIO
  , try
  )
import qualified Data.ByteString as BS
import Crypto.Random (getRandomBytes)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CUInt(..))
#if defined(mingw32_HOST_OS)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (IntPtr, Ptr, nullPtr)
import Foreign.Storable (pokeByteOff, sizeOf)
#endif
import Numeric (showHex)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hClose)
#if defined(mingw32_HOST_OS)
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.Info (os)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , getPid
  , getProcessExitCode
  , proc
  , terminateProcess
  )
import Text.Read (readMaybe)

import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.RPC.Transport
  ( Transport
  , TransportConfig(..)
  , TransportEndpoint(..)
  , TransportPeerPolicy(..)
  , TransportServer(..)
  , defaultTransportConfig
  , endpointKindText
  , openPluginServer
  , pluginAuthTokenEnv
  , pluginDataRootEnv
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginIdEnv
  , pluginProtocolEnv
  , pluginSessionEnv
  , pluginStdioCompatibilityEnv
  , pluginWorldIdEnv
  )

#if !defined(mingw32_HOST_OS) && !defined(linux_HOST_OS)
foreign import ccall unsafe "unistd.h geteuid"
  c_geteuid :: IO CUInt
#endif

resolvePluginExecutable :: FilePath -> Text -> IO (Maybe FilePath)
resolvePluginExecutable pluginDir pluginName =
  findFirstExisting candidates
  where
    basePath = pluginDir </> Text.unpack pluginName
    candidates
      | os == "mingw32" =
          [ basePath <.> "exe"
          , basePath <.> "cmd"
          , basePath <.> "bat"
          , basePath
          ]
      | otherwise = [basePath]

findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
findFirstExisting [] = pure Nothing
findFirstExisting (candidate:rest) = do
  exists <- doesFileExist candidate
  if exists
    then pure (Just candidate)
    else findFirstExisting rest

trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try @SomeException action
  case result of
    Left err
      | Just asyncErr <- fromException @SomeAsyncException err -> throwIO asyncErr
      | otherwise -> pure (Left err)
    Right value -> pure (Right value)

safeCloseServer :: TransportServer -> IO ()
safeCloseServer server = do
  _ <- try @SomeException (tsClose server)
  pure ()

-- | Successful plugin launch output, including the credentials that were
-- injected into the process environment and must be proven during handshake.
data LaunchPluginResult = LaunchPluginResult
  { lprTransport :: !Transport
  , lprProcessHandle :: !ProcessHandle
  , lprSessionId :: !Text
  , lprAuthToken :: !Text
  }

launchPluginTransport
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe ProcessHandle) LaunchPluginResult)
launchPluginTransport = launchPluginTransportViaEndpoint

-- Keep the endpoint accept budget aligned with the startup/handshake timeout
-- so slow or crashed plugins fail startup instead of blocking discovery.
launchPluginTransportViaEndpoint
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe ProcessHandle) LaunchPluginResult)
launchPluginTransportViaEndpoint executablePath workingDir pluginName startupTimeoutMillis = do
  serverResult <- openPluginServer
    defaultTransportConfig { tcTimeout = max 1 startupTimeoutMillis }
    pluginName
  case serverResult of
    Left err -> pure (Left (Text.pack (show err), Nothing))
    Right server -> mask $ \restore -> do
      processResult <- trySync $ do
        launchEnvironment <- endpointEnvironment (tsEndpoint server) pluginName workingDir
        (_, _, _, processHandle) <- createProcess
          (pluginProcessSpec (proc executablePath [])
            { cwd = Just workingDir
            , env = Just (leVariables launchEnvironment)
            , std_in = NoStream
            , std_out = NoStream
            , std_err = Inherit
            })
        pure (launchEnvironment, processHandle)
      case processResult of
        Left err -> do
          safeCloseServer server
          pure (Left (Text.pack (show err), Nothing))
        Right (launchEnvironment, processHandle) -> do
          protectionResult <- trySync (registerProcessParentDeathProtection processHandle)
          case protectionResult of
            Left _ -> pure ()
            Right _ -> pure ()
          let cleanupLaunched = do
                safeCloseServer server
                terminated <- safeTerminateProcess processHandle
                if terminated
                  then pure ()
                  else do
                    _ <- safeTerminateProcess processHandle
                    pure ()
              failLaunched message = do
                safeCloseServer server
                terminated <- safeTerminateProcess processHandle
                let mProcessHandle = if terminated then Nothing else Just processHandle
                pure (Left (message, mProcessHandle))
          peerPolicyResult <- expectedPeerPolicyForLaunchedProcess processHandle
          case peerPolicyResult of
            Left message -> failLaunched message
            Right peerPolicy -> do
              acceptResult <- restore (tsAcceptWithPeerPolicy server peerPolicy) `onException` cleanupLaunched
              case acceptResult of
                Left transportErr ->
                  failLaunched (Text.pack (show transportErr))
                Right transport -> pure (Right LaunchPluginResult
                  { lprTransport = transport
                  , lprProcessHandle = processHandle
                  , lprSessionId = leSessionId launchEnvironment
                  , lprAuthToken = leAuthToken launchEnvironment
                  })

data LaunchEnvironment = LaunchEnvironment
  { leVariables :: ![(String, String)]
  , leSessionId :: !Text
  , leAuthToken :: !Text
  }

endpointEnvironment :: TransportEndpoint -> Text -> FilePath -> IO LaunchEnvironment
endpointEnvironment endpoint pluginName workingDir = do
  inherited <- getEnvironment
  launchSession <- Text.pack <$> freshLaunchSecret "session"
  authToken <- Text.pack <$> freshLaunchSecret "auth"
  let dataRoot = workingDir </> "data"
      launchVars =
        [ (pluginIdEnv, Text.unpack pluginName)
        , (pluginProtocolEnv, show currentProtocolVersion)
        , (pluginEndpointEnv, teAddress endpoint)
        , (pluginEndpointKindEnv, Text.unpack (endpointKindText (teKind endpoint)))
        , (pluginSessionEnv, Text.unpack launchSession)
        , (pluginAuthTokenEnv, Text.unpack authToken)
        , (pluginWorldIdEnv, unsavedWorldId)
        , (pluginDataRootEnv, dataRoot)
        ]
      -- Production launches always use the endpoint variables above; do not
      -- leak a developer shell's stdio compatibility flag into plugin processes.
      overridden = pluginStdioCompatibilityEnv : map fst launchVars
      preserved = filter (not . isOverriddenEnvKey overridden . fst) inherited
  createDirectoryIfMissing True dataRoot
  pure LaunchEnvironment
    { leVariables = launchVars <> preserved
    , leSessionId = launchSession
    , leAuthToken = authToken
    }

isOverriddenEnvKey :: [String] -> String -> Bool
isOverriddenEnvKey overridden key = any (envKeyEquals key) overridden

envKeyEquals :: String -> String -> Bool
envKeyEquals left right
  | os == "mingw32" = map toLower left == map toLower right
  | otherwise = left == right

freshLaunchSecret :: String -> IO String
freshLaunchSecret label = do
  bytes <- getRandomBytes 32 :: IO BS.ByteString
  pure (label <> "-" <> bytesToHex bytes)

bytesToHex :: BS.ByteString -> String
bytesToHex = concatMap byteToHex . BS.unpack

byteToHex :: Word8 -> String
byteToHex byte = case showHex byte "" of
  [digit] -> ['0', digit]
  digits  -> digits

expectedPeerPolicyForLaunchedProcess :: ProcessHandle -> IO (Either Text TransportPeerPolicy)
#if defined(mingw32_HOST_OS) || defined(linux_HOST_OS)
expectedPeerPolicyForLaunchedProcess processHandle = do
  mPid <- processHandlePidWord64 processHandle
  pure $ case mPid of
    Nothing -> Left "could not determine launched plugin process id for endpoint peer check"
    Just pid -> Right TransportPeerPolicy
      { tppExpectedProcessId = Just pid
      , tppExpectedUserId = Nothing
      }
#else
expectedPeerPolicyForLaunchedProcess _ = do
  -- Portable Unix APIs expose the peer uid more consistently than the peer pid.
  -- Exact pid remains enforced on Linux; other Unix hosts require same-owner
  -- endpoints and same-uid peers so wrapper descendants cannot be confused with
  -- arbitrary local users.
  uid <- c_geteuid
  pure (Right TransportPeerPolicy
    { tppExpectedProcessId = Nothing
    , tppExpectedUserId = Just (fromIntegral uid)
    })
#endif

processHandlePidWord64 :: ProcessHandle -> IO (Maybe Word64)
processHandlePidWord64 processHandle = do
  mPid <- getPid processHandle
  pure (mPid >>= readMaybe . show)

unsavedWorldId :: String
unsavedWorldId = "unsaved"

pluginProcessSpec :: CreateProcess -> CreateProcess
#if defined(mingw32_HOST_OS)
pluginProcessSpec = id
#else
pluginProcessSpec processSpec = processSpec { new_session = True }
#endif

registerProcessParentDeathProtection :: ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
registerProcessParentDeathProtection processHandle = do
  mJob <- attachProcessToKillOnCloseJob processHandle
  case mJob of
    Nothing -> pure ()
    Just (pid, jobHandle) -> do
      modifyMVar_ windowsProcessJobs $ \jobs -> do
        let (oldHandles, retainedJobs) = partitionWindowsJobs pid jobs
        mapM_ closeWindowsHandle oldHandles
        pure ((pid, jobHandle) : retainedJobs)
      _ <- forkIO $ do
        waitForProcessExitForParentDeathProtection processHandle
        releaseWindowsJobHandle pid jobHandle
      pure ()
#else
registerProcessParentDeathProtection _ = pure ()
#endif

releaseProcessParentDeathProtection :: ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
releaseProcessParentDeathProtection processHandle = do
  mPid <- processHandlePidWord32 processHandle
  case mPid of
    Nothing -> pure ()
    Just pid -> do
      handles <- modifyMVar windowsProcessJobs $ \jobs -> do
        let (matchedHandles, retainedJobs) = partitionWindowsJobs pid jobs
        pure (retainedJobs, matchedHandles)
      mapM_ closeWindowsHandle handles
#else
releaseProcessParentDeathProtection _ = pure ()
#endif

#if defined(mingw32_HOST_OS)
type HANDLE = Ptr ()

foreign import ccall unsafe "windows.h CreateJobObjectW"
  c_CreateJobObjectW :: Ptr () -> Ptr () -> IO HANDLE

foreign import ccall unsafe "windows.h SetInformationJobObject"
  c_SetInformationJobObject :: HANDLE -> CInt -> Ptr () -> Word32 -> IO CInt

foreign import ccall unsafe "windows.h AssignProcessToJobObject"
  c_AssignProcessToJobObject :: HANDLE -> HANDLE -> IO CInt

foreign import ccall unsafe "windows.h OpenProcess"
  c_OpenProcess :: Word32 -> CInt -> Word32 -> IO HANDLE

foreign import ccall unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO CInt

windowsProcessJobs :: MVar [(Word32, HANDLE)]
windowsProcessJobs = unsafePerformIO (newMVar [])
{-# NOINLINE windowsProcessJobs #-}

attachProcessToKillOnCloseJob :: ProcessHandle -> IO (Maybe (Word32, HANDLE))
attachProcessToKillOnCloseJob processHandle = do
  mPid <- processHandlePidWord32 processHandle
  case mPid of
    Nothing -> pure Nothing
    Just pid -> do
      jobHandle <- createKillOnCloseJob
      if jobHandle == nullPtr
        then pure Nothing
        else do
          processHandleForAssign <- c_OpenProcess processAssignJobRights 0 pid
          if processHandleForAssign == nullPtr
            then closeWindowsHandle jobHandle >> pure Nothing
            else do
              assigned <- c_AssignProcessToJobObject jobHandle processHandleForAssign
              closeWindowsHandle processHandleForAssign
              if assigned == 0
                then closeWindowsHandle jobHandle >> pure Nothing
                else pure (Just (pid, jobHandle))

createKillOnCloseJob :: IO HANDLE
createKillOnCloseJob = do
  jobHandle <- c_CreateJobObjectW nullPtr nullPtr
  if jobHandle == nullPtr
    then pure nullPtr
    else do
      configured <- allocaBytes jobObjectExtendedLimitInformationSize $ \infoPtr -> do
        fillBytes infoPtr 0 jobObjectExtendedLimitInformationSize
        pokeByteOff infoPtr jobObjectLimitFlagsOffset jobObjectLimitKillOnJobClose
        c_SetInformationJobObject
          jobHandle
          jobObjectExtendedLimitInformation
          infoPtr
          (fromIntegral jobObjectExtendedLimitInformationSize)
      if configured == 0
        then closeWindowsHandle jobHandle >> pure nullPtr
        else pure jobHandle

processHandlePidWord32 :: ProcessHandle -> IO (Maybe Word32)
processHandlePidWord32 processHandle = do
  mPid <- getPid processHandle
  pure (mPid >>= readMaybe . show)

partitionWindowsJobs :: Word32 -> [(Word32, HANDLE)] -> ([HANDLE], [(Word32, HANDLE)])
partitionWindowsJobs _ [] = ([], [])
partitionWindowsJobs targetPid ((pid, handle):jobs)
  | targetPid == pid =
      let (matchedHandles, retainedJobs) = partitionWindowsJobs targetPid jobs
      in (handle : matchedHandles, retainedJobs)
  | otherwise =
      let (matchedHandles, retainedJobs) = partitionWindowsJobs targetPid jobs
      in (matchedHandles, (pid, handle) : retainedJobs)

releaseWindowsJobHandle :: Word32 -> HANDLE -> IO ()
releaseWindowsJobHandle targetPid targetHandle = do
  mHandle <- modifyMVar windowsProcessJobs $ \jobs -> do
    let (matchedHandle, retainedJobs) = removeWindowsJob targetPid targetHandle jobs
    pure (retainedJobs, matchedHandle)
  case mHandle of
    Nothing -> pure ()
    Just handle -> closeWindowsHandle handle

removeWindowsJob :: Word32 -> HANDLE -> [(Word32, HANDLE)] -> (Maybe HANDLE, [(Word32, HANDLE)])
removeWindowsJob _ _ [] = (Nothing, [])
removeWindowsJob targetPid targetHandle ((pid, handle):jobs)
  | targetPid == pid && targetHandle == handle = (Just handle, jobs)
  | otherwise =
      let (matchedHandle, retainedJobs) = removeWindowsJob targetPid targetHandle jobs
      in (matchedHandle, (pid, handle) : retainedJobs)

waitForProcessExitForParentDeathProtection :: ProcessHandle -> IO ()
waitForProcessExitForParentDeathProtection processHandle = do
  mExit <- getProcessExitCode processHandle
  case mExit of
    Just _ -> pure ()
    Nothing -> do
      threadDelay processPollDelayMicros
      waitForProcessExitForParentDeathProtection processHandle

closeWindowsHandle :: HANDLE -> IO ()
closeWindowsHandle handle
  | handle == nullPtr = pure ()
  | otherwise = do
      _ <- c_CloseHandle handle
      pure ()

processAssignJobRights :: Word32
processAssignJobRights = 0x00000101

jobObjectExtendedLimitInformation :: CInt
jobObjectExtendedLimitInformation = 9

jobObjectLimitKillOnJobClose :: Word32
jobObjectLimitKillOnJobClose = 0x00002000

jobObjectLimitFlagsOffset :: Int
jobObjectLimitFlagsOffset = 16

jobObjectExtendedLimitInformationSize :: Int
jobObjectExtendedLimitInformationSize
  | sizeOf (undefined :: IntPtr) == 8 = 144
  | otherwise = 112
#endif

safeCloseHandle :: Handle -> IO ()
safeCloseHandle handle = do
  _ <- try @SomeException (hClose handle)
  pure ()

safeTerminateProcess :: ProcessHandle -> IO Bool
safeTerminateProcess processHandle =
  terminateProcessCascade processHandle
    `finally` safeReleaseProcessParentDeathProtection processHandle

terminateProcessCascade :: ProcessHandle -> IO Bool
terminateProcessCascade processHandle = do
  alreadyExited <- waitForProcessExitPoll 0 processHandle
  if alreadyExited
    then pure True
    else do
      terminateProcessGracefully processHandle
      terminated <- waitForProcessExitPoll processTerminationWaitMicros processHandle
      if terminated
        then pure True
        else do
          escalateProcessTermination processHandle
          waitForProcessExitPoll processKillWaitMicros processHandle

terminateProcessGracefully :: ProcessHandle -> IO ()
terminateProcessGracefully processHandle = do
  if os == "mingw32"
    then terminateWindowsProcessTree processHandle
    else terminatePosixProcessGroup sigTERM processHandle
  ignoreSyncExceptions (terminateProcess processHandle)

escalateProcessTermination :: ProcessHandle -> IO ()
escalateProcessTermination processHandle = do
  if os == "mingw32"
    then terminateWindowsProcessTree processHandle
    else terminatePosixProcessGroup sigKILL processHandle
  ignoreSyncExceptions (terminateProcess processHandle)

safeReleaseProcessParentDeathProtection :: ProcessHandle -> IO ()
safeReleaseProcessParentDeathProtection processHandle =
  ignoreSyncExceptions (releaseProcessParentDeathProtection processHandle)

ignoreSyncExceptions :: IO () -> IO ()
ignoreSyncExceptions action = do
  result <- trySync action
  case result of
    Left _ -> pure ()
    Right _ -> pure ()

terminateWindowsProcessTree :: ProcessHandle -> IO ()
terminateWindowsProcessTree processHandle = do
  mPid <- getPid processHandle
  case mPid of
    Nothing -> pure ()
    Just pid -> do
      taskkillResult <- trySync $
        createProcess (proc "taskkill" ["/PID", show pid, "/T", "/F"])
          { std_in = NoStream
          , std_out = NoStream
          , std_err = NoStream
          }
      case taskkillResult of
        Left _ -> pure ()
        Right (_, _, _, taskkillHandle) -> do
          _ <- waitForProcessExitPoll processTerminationWaitMicros taskkillHandle
          pure ()

waitForProcessExitPoll :: Int -> ProcessHandle -> IO Bool
waitForProcessExitPoll remainingMicros processHandle = do
  mExit <- getProcessExitCode processHandle
  case mExit of
    Just _ -> pure True
    Nothing
      | remainingMicros <= 0 -> pure False
      | otherwise -> do
          let delayMicros = min processPollDelayMicros remainingMicros
          threadDelay delayMicros
          waitForProcessExitPoll (remainingMicros - delayMicros) processHandle

terminatePosixProcessGroup :: CInt -> ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
terminatePosixProcessGroup _ _ = pure ()
#else
terminatePosixProcessGroup signalNumber processHandle = do
  mPid <- processHandlePidCInt processHandle
  case mPid of
    Nothing -> pure ()
    Just pid -> ignoreSyncExceptions $ do
      _ <- c_kill (negate pid) signalNumber
      pure ()
#endif

#if !defined(mingw32_HOST_OS)
processHandlePidCInt :: ProcessHandle -> IO (Maybe CInt)
processHandlePidCInt processHandle = do
  mPid <- getPid processHandle
  pure (mPid >>= readMaybe . show)

foreign import ccall unsafe "kill"
  c_kill :: CInt -> CInt -> IO CInt
#endif

sigTERM :: CInt
sigTERM = 15

sigKILL :: CInt
sigKILL = 9

processTerminationWaitMicros :: Int
processTerminationWaitMicros = 1000000

processKillWaitMicros :: Int
processKillWaitMicros = 2000000

processPollDelayMicros :: Int
processPollDelayMicros = 10000
