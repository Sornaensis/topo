{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and production transport attachment.
module Actor.PluginManager.ProcessLauncher
  ( OwnedPluginProcess
  , OwnedPluginCleanupResult(..)
  , LaunchPluginResult(..)
  , ownedPluginProcessHandle
  , ownedPluginProcessId
  , ownedPluginProcessExitCode
  , cleanupOwnedPluginProcess
  , resolvePluginExecutable
  , launchPluginTransport
  , safeCloseHandle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , mask
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
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hClose)
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
import System.Exit (ExitCode)
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

-- | Opaque ownership of a launched plugin process and its platform
-- containment. The root handle and stable launch-tree identity never leave
-- this value as cleanup ownership.
data OwnedPluginProcess = OwnedPluginProcess
  { oppRootHandle :: !ProcessHandle
  , oppTreeIdentity :: !(Maybe Word64)
  , oppState :: !(MVar OwnedPluginProcessState)
  }

data OwnedPluginProcessState = OwnedPluginProcessState
  { oppsContainment :: !PlatformContainment
  , oppsCleanupComplete :: !Bool
  }

-- | Cleanup either completed, or failed while returning the same owner so the
-- caller cannot accidentally discard a still-owned process.
data OwnedPluginCleanupResult
  = OwnedPluginCleanupComplete
  | OwnedPluginCleanupFailed !OwnedPluginProcess

#if defined(mingw32_HOST_OS)
data PlatformContainment = WindowsJob !HANDLE | NoPlatformContainment
#else
data PlatformContainment = NoPlatformContainment
#endif

-- | Observe the root process handle without transferring cleanup ownership.
ownedPluginProcessHandle :: OwnedPluginProcess -> ProcessHandle
ownedPluginProcessHandle = oppRootHandle

-- | Stable process-group/session identity on POSIX and root pid on Windows.
ownedPluginProcessId :: OwnedPluginProcess -> Maybe Word64
ownedPluginProcessId = oppTreeIdentity

-- | Non-owning exit observation for the root process.
ownedPluginProcessExitCode :: OwnedPluginProcess -> IO (Maybe ExitCode)
ownedPluginProcessExitCode = getProcessExitCode . oppRootHandle

-- | Successful plugin launch output, including the credentials that were
-- injected into the process environment and must be proven during handshake.
data LaunchPluginResult = LaunchPluginResult
  { lprTransport :: !Transport
  , lprOwnedProcess :: !OwnedPluginProcess
  , lprSessionId :: !Text
  , lprAuthToken :: !Text
  }

launchPluginTransport
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe OwnedPluginProcess) LaunchPluginResult)
launchPluginTransport = launchPluginTransportViaEndpoint

-- Keep the endpoint accept budget aligned with the startup/handshake timeout
-- so slow or crashed plugins fail startup instead of blocking discovery.
launchPluginTransportViaEndpoint
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe OwnedPluginProcess) LaunchPluginResult)
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
        ownershipResult <- establishProcessOwnership processHandle
        pure (launchEnvironment, ownershipResult)
      case processResult of
        Left err -> do
          safeCloseServer server
          pure (Left (Text.pack (show err), Nothing))
        Right (launchEnvironment, ownershipResult) ->
          case ownershipResult of
            Left (message, residualOwner) -> do
              safeCloseServer server
              pure (Left (message, residualOwner))
            Right ownedProcess -> do
              let failLaunched message = do
                    safeCloseServer server
                    cleanupResult <- cleanupOwnedPluginProcess ownedProcess
                    let residualOwner = case cleanupResult of
                          OwnedPluginCleanupComplete -> Nothing
                          OwnedPluginCleanupFailed retained -> Just retained
                    pure (Left (message, residualOwner))
                  handleAcceptException err = do
                    safeCloseServer server
                    cleanupResult <- cleanupOwnedPluginProcess ownedProcess
                    case cleanupResult of
                      OwnedPluginCleanupComplete -> throwIO err
                      OwnedPluginCleanupFailed retained ->
                        pure (Left (Text.pack (show err), Just retained))
              peerPolicyResult <- expectedPeerPolicyForLaunchedProcess (ownedPluginProcessHandle ownedProcess)
              case peerPolicyResult of
                Left message -> failLaunched message
                Right peerPolicy -> do
                  acceptAttempt <- try @SomeException
                    (restore (tsAcceptWithPeerPolicy server peerPolicy))
                  case acceptAttempt of
                    Left err -> handleAcceptException err
                    Right (Left transportErr) ->
                      failLaunched (Text.pack (show transportErr))
                    Right (Right transport) -> pure (Right LaunchPluginResult
                      { lprTransport = transport
                      , lprOwnedProcess = ownedProcess
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
  -- The data root is host-created and later used as the save-bundling source,
  -- but it is advisory process metadata rather than filesystem confinement.
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
      -- Do not inherit the developer shell wholesale: it routinely contains
      -- cloud, VCS, database, proxy, and tool tokens.  Plugins get only the
      -- launch contract above plus a tiny platform/runtime allowlist below.
      -- Plugins that need credentials should obtain them through explicit topo
      -- configuration or external-data-source grants instead of ambient env.
      preserved = filter (isAllowedInheritedRuntimeEnvKey . fst) inherited
  createDirectoryIfMissing True dataRoot
  pure LaunchEnvironment
    { leVariables = launchVars <> preserved
    , leSessionId = launchSession
    , leAuthToken = authToken
    }

isAllowedInheritedRuntimeEnvKey :: String -> Bool
isAllowedInheritedRuntimeEnvKey key =
  any (envKeyEquals key) allowedInheritedRuntimeEnvKeys

-- Minimal inherited runtime state needed for process startup, temp files, time
-- zones, and locale/encoding behavior.  Keep this list conservative and exact
-- so parent auth/session/secrets cannot leak into production plugin
-- subprocesses; in particular TOPO_PLUGIN_STDIO_COMPAT is intentionally not
-- allowlisted.
allowedInheritedRuntimeEnvKeys :: [String]
allowedInheritedRuntimeEnvKeys =
  [ "PATH"
  , "LANG"
  , "LANGUAGE"
  , "LC_ALL"
  , "LC_COLLATE"
  , "LC_CTYPE"
  , "LC_MESSAGES"
  , "LC_MONETARY"
  , "LC_NUMERIC"
  , "LC_TIME"
  , "LC_ADDRESS"
  , "LC_IDENTIFICATION"
  , "LC_MEASUREMENT"
  , "LC_NAME"
  , "LC_PAPER"
  , "LC_TELEPHONE"
  , "TZ"
  , "TMPDIR"
  , "TEMP"
  , "TMP"
  ] <> windowsRuntimeEnvKeys

windowsRuntimeEnvKeys :: [String]
windowsRuntimeEnvKeys
  | os == "mingw32" =
      [ "SystemRoot"
      , "WINDIR"
      , "COMSPEC"
      , "PATHEXT"
      ]
  | otherwise = []

envKeyEquals :: String -> String -> Bool
envKeyEquals left right = normalizeEnvKey left == normalizeEnvKey right

normalizeEnvKey :: String -> String
normalizeEnvKey
  | os == "mingw32" = map toLower
  | otherwise = id

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

-- Process creation calls this while asynchronous exceptions are masked. The
-- owner exists before pid discovery and containment attachment, so every setup
-- failure can be cleaned or returned as that same residual owner.
establishProcessOwnership
  :: ProcessHandle
  -> IO (Either (Text, Maybe OwnedPluginProcess) OwnedPluginProcess)
establishProcessOwnership processHandle = mask $ \_ -> do
  state <- newMVar OwnedPluginProcessState
    { oppsContainment = NoPlatformContainment
    , oppsCleanupComplete = False
    }
  identityResult <- trySync (processHandlePidWord64 processHandle)
  let mIdentity = either (const Nothing) id identityResult
      owned = OwnedPluginProcess processHandle mIdentity state
  setupResult <- trySync $ do
    forcedFailure <- lookupEnv containmentFailureTestEnv
    if forcedFailure == Just "1"
      then throwIO (userError "forced plugin containment setup failure")
      else pure ()
    case mIdentity of
      Nothing -> throwIO (userError "could not determine launched plugin process identity")
      Just _ -> pure ()
    containment <- attachPlatformContainment processHandle
    modifyMVar state $ \ownershipState ->
      pure (ownershipState { oppsContainment = containment }, ())
  case setupResult of
    Right () -> pure (Right owned)
    Left err -> do
      cleanupResult <- cleanupOwnedPluginProcess owned
      let residual = case cleanupResult of
            OwnedPluginCleanupComplete -> Nothing
            OwnedPluginCleanupFailed retained -> Just retained
      pure (Left ("plugin containment setup failed: " <> Text.pack (show err), residual))

containmentFailureTestEnv :: String
containmentFailureTestEnv = "TOPO_TEST_PLUGIN_CONTAINMENT_FAILURE"

cleanupFailureTestEnv :: String
cleanupFailureTestEnv = "TOPO_TEST_PLUGIN_CLEANUP_FAILURE"

attachPlatformContainment :: ProcessHandle -> IO PlatformContainment
#if defined(mingw32_HOST_OS)
attachPlatformContainment processHandle = do
  mJob <- attachProcessToKillOnCloseJob processHandle
  case mJob of
    Nothing -> throwIO (userError "could not attach launched plugin to a kill-on-close Job")
    Just jobHandle -> pure (WindowsJob jobHandle)
#else
attachPlatformContainment _ = pure NoPlatformContainment
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

attachProcessToKillOnCloseJob :: ProcessHandle -> IO (Maybe HANDLE)
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
            then closeWindowsHandleIgnoring jobHandle >> pure Nothing
            else do
              assigned <- c_AssignProcessToJobObject jobHandle processHandleForAssign
              closeWindowsHandleIgnoring processHandleForAssign
              if assigned == 0
                then closeWindowsHandleIgnoring jobHandle >> pure Nothing
                else pure (Just jobHandle)

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
        then closeWindowsHandleIgnoring jobHandle >> pure nullPtr
        else pure jobHandle

processHandlePidWord32 :: ProcessHandle -> IO (Maybe Word32)
processHandlePidWord32 processHandle = do
  mPid <- getPid processHandle
  pure (mPid >>= readMaybe . show)

closeWindowsHandleIgnoring :: HANDLE -> IO ()
closeWindowsHandleIgnoring handle = do
  _ <- closeWindowsHandle handle
  pure ()

closeWindowsHandle :: HANDLE -> IO Bool
closeWindowsHandle handle
  | handle == nullPtr = pure True
  | otherwise = (/= 0) <$> c_CloseHandle handle

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

cleanupOwnedPluginProcess :: OwnedPluginProcess -> IO OwnedPluginCleanupResult
cleanupOwnedPluginProcess owned = mask $ \_ ->
  modifyMVar (oppState owned) $ \state ->
    if oppsCleanupComplete state
      then pure (state, OwnedPluginCleanupComplete)
      else do
        forcedFailure <- lookupEnv cleanupFailureTestEnv
        terminationResult <- if forcedFailure == Just "1"
          then pure (Right False)
          else try @SomeException
            (terminateProcessCascade (oppTreeIdentity owned) (oppRootHandle owned))
        case terminationResult of
          Right True -> do
            releaseResult <- try @SomeException
              (releasePlatformContainment (oppsContainment state))
            case releaseResult of
              Right True -> pure
                ( state
                    { oppsContainment = NoPlatformContainment
                    , oppsCleanupComplete = True
                    }
                , OwnedPluginCleanupComplete
                )
              _ -> pure (state, OwnedPluginCleanupFailed owned)
          _ -> pure (state, OwnedPluginCleanupFailed owned)

releasePlatformContainment :: PlatformContainment -> IO Bool
#if defined(mingw32_HOST_OS)
releasePlatformContainment (WindowsJob jobHandle) = closeWindowsHandle jobHandle
releasePlatformContainment NoPlatformContainment = pure True
#else
releasePlatformContainment NoPlatformContainment = pure True
#endif

terminateProcessCascade :: Maybe Word64 -> ProcessHandle -> IO Bool
terminateProcessCascade treeIdentity processHandle = do
  alreadyExited <- waitForProcessExitPoll 0 processHandle
  if alreadyExited
    then terminateStableDescendants treeIdentity >> pure True
    else do
      terminateProcessGracefully treeIdentity processHandle
      terminated <- waitForProcessExitPoll processTerminationWaitMicros processHandle
      if terminated
        then terminateStableDescendants treeIdentity >> pure True
        else do
          escalateProcessTermination treeIdentity processHandle
          killed <- waitForProcessExitPoll processKillWaitMicros processHandle
          if killed
            then terminateStableDescendants treeIdentity >> pure True
            else pure False

-- The stable POSIX session/group identity outlives the root. Signal it even
-- after root exit so cleanup does not silently skip surviving descendants.
-- Windows descendants remain contained by the owned Job until its token closes.
terminateStableDescendants :: Maybe Word64 -> IO ()
terminateStableDescendants treeIdentity
  | os == "mingw32" = pure ()
  | otherwise = terminatePosixProcessGroup sigKILL treeIdentity

terminateProcessGracefully :: Maybe Word64 -> ProcessHandle -> IO ()
terminateProcessGracefully treeIdentity processHandle = do
  if os == "mingw32"
    then terminateWindowsProcessTree processHandle
    else terminatePosixProcessGroup sigTERM treeIdentity
  ignoreSyncExceptions (terminateProcess processHandle)

escalateProcessTermination :: Maybe Word64 -> ProcessHandle -> IO ()
escalateProcessTermination treeIdentity processHandle = do
  if os == "mingw32"
    then terminateWindowsProcessTree processHandle
    else terminatePosixProcessGroup sigKILL treeIdentity
  ignoreSyncExceptions (terminateProcess processHandle)

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

terminatePosixProcessGroup :: CInt -> Maybe Word64 -> IO ()
#if defined(mingw32_HOST_OS)
terminatePosixProcessGroup _ _ = pure ()
#else
terminatePosixProcessGroup signalNumber mTreeIdentity =
  case fromIntegral <$> mTreeIdentity of
    Nothing -> pure ()
    Just pid -> ignoreSyncExceptions $ do
      _ <- c_kill (negate pid) signalNumber
      pure ()

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
