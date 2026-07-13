{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and production transport attachment.
module Actor.PluginManager.ProcessLauncher
  ( OwnedPluginProcess
  , OwnedPluginCleanupResult(..)
  , PluginRuntimeGeneration
  , OwnedPluginRuntime
  , OwnedPluginRuntimeCleanupResult(..)
  , LaunchPluginResult(..)
  , freshPluginRuntimeGeneration
  , newOwnedPluginRuntime
  , newConnectionOnlyPluginRuntime
  , ownedPluginRuntimeGeneration
  , ownedPluginRuntimeConnection
  , ownedPluginRuntimeProcess
  , mapOwnedPluginRuntimeConnection
  , claimOwnedPluginRuntimeRPCMonitor
  , releaseOwnedPluginRuntimeRPCMonitor
  , claimOwnedPluginRuntimeProcessMonitor
  , releaseOwnedPluginRuntimeProcessMonitor
  , cleanupOwnedPluginRuntime
  , ownedPluginProcessHandle
  , ownedPluginProcessId
  , ownedPluginProcessExitCode
  , cleanupOwnedPluginProcess
  , resolvePluginExecutable
  , launchPluginTransport
  , pausePluginStartupIfInjected
  , safeCloseHandle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , mask
  , onException
  , throwIO
  , try
  )
import qualified Data.ByteString as BS
import Control.Monad (when)
import Crypto.Random (getRandomBytes)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CUInt(..), CWchar)
#if !defined(mingw32_HOST_OS)
import Foreign.C.Error (eINTR, eSRCH, getErrno)
#endif
#if defined(mingw32_HOST_OS)
import Foreign.C.String (withCWString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (IntPtr, Ptr, nullPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff, sizeOf)
#endif
import Numeric (showHex)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
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
#if defined(mingw32_HOST_OS)
import System.Process.Internals (PHANDLE, mkProcessHandle)
#endif

import Topo.Plugin.RPC
  ( RPCConnection(..)
  , claimRPCSupervisorMonitor
  , releaseRPCSupervisorMonitor
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.RPC.Transport
  ( Transport
  , TransportConfig(..)
  , TransportError(..)
  , TransportEndpoint(..)
  , TransportPeerPolicy(..)
  , TransportServer(..)
  , closeTransport
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
      | os == "mingw32" = [basePath <.> "exe", basePath]
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
-- this value as cleanup ownership. A Windows kill-on-close Job gives an
-- explicit parent-death guarantee after suspended assignment; portable Unix
-- has no equivalent parent-death guarantee.
data OwnedPluginProcess = OwnedPluginProcess
  { oppRootHandle :: !ProcessHandle
  , oppTreeIdentity :: !(Maybe Word64)
  , oppState :: !(MVar OwnedPluginProcessState)
  }

data OwnedPluginProcessState = OwnedPluginProcessState
  { oppsContainment :: !(Maybe PlatformContainment)
  , oppsCleanupComplete :: !Bool
  , oppsProcessMonitorClaimed :: !Bool
  }

-- | Cleanup either completed, or failed while returning the same owner so the
-- caller cannot accidentally discard a still-owned process.
data OwnedPluginCleanupResult
  = OwnedPluginCleanupComplete
  | OwnedPluginCleanupFailed !OwnedPluginProcess

-- | Opaque identity for one launch attempt. It is allocated before process
-- creation and travels with any retained partial or fully handshaken runtime.
newtype PluginRuntimeGeneration = PluginRuntimeGeneration Int
  deriving (Eq, Show)

-- | Sole owner of the resources created by one plugin launch. A connection is
-- absent only while retaining a process from a pre-accept failure; published
-- ready runtimes always contain both the accepted RPC connection and process.
data OwnedPluginRuntime = OwnedPluginRuntime
  { oprGeneration :: !PluginRuntimeGeneration
  , oprConnection :: !(Maybe RPCConnection)
  , oprProcess :: !(Maybe OwnedPluginProcess)
  }

-- | Cleanup either completed or returned the same aggregate because process
-- ownership could not yet be discharged. Transport closure is idempotent.
data OwnedPluginRuntimeCleanupResult
  = OwnedPluginRuntimeCleanupComplete
  | OwnedPluginRuntimeCleanupFailed !OwnedPluginRuntime

freshPluginRuntimeGeneration :: IO PluginRuntimeGeneration
freshPluginRuntimeGeneration = PluginRuntimeGeneration . hashUnique <$> newUnique

newOwnedPluginRuntime
  :: PluginRuntimeGeneration
  -> Maybe RPCConnection
  -> OwnedPluginProcess
  -> OwnedPluginRuntime
newOwnedPluginRuntime generation connection process =
  OwnedPluginRuntime generation connection (Just process)

-- | Construct an aggregate for an in-process transport that has no launched
-- subprocess. This is used by embedded hosts and deterministic RPC tests.
newConnectionOnlyPluginRuntime :: RPCConnection -> OwnedPluginRuntime
newConnectionOnlyPluginRuntime connection =
  OwnedPluginRuntime (PluginRuntimeGeneration 0) (Just connection) Nothing

ownedPluginRuntimeGeneration :: OwnedPluginRuntime -> PluginRuntimeGeneration
ownedPluginRuntimeGeneration = oprGeneration

ownedPluginRuntimeConnection :: OwnedPluginRuntime -> Maybe RPCConnection
ownedPluginRuntimeConnection = oprConnection

ownedPluginRuntimeProcess :: OwnedPluginRuntime -> Maybe OwnedPluginProcess
ownedPluginRuntimeProcess = oprProcess

mapOwnedPluginRuntimeConnection
  :: (RPCConnection -> RPCConnection)
  -> OwnedPluginRuntime
  -> OwnedPluginRuntime
mapOwnedPluginRuntimeConnection f runtime = runtime
  { oprConnection = f <$> oprConnection runtime }

claimOwnedPluginRuntimeRPCMonitor :: OwnedPluginRuntime -> IO Bool
claimOwnedPluginRuntimeRPCMonitor runtime =
  maybe (pure False) claimRPCSupervisorMonitor (oprConnection runtime)

releaseOwnedPluginRuntimeRPCMonitor :: OwnedPluginRuntime -> IO ()
releaseOwnedPluginRuntimeRPCMonitor runtime =
  maybe (pure ()) releaseRPCSupervisorMonitor (oprConnection runtime)

claimOwnedPluginRuntimeProcessMonitor :: OwnedPluginRuntime -> IO Bool
claimOwnedPluginRuntimeProcessMonitor runtime = case oprProcess runtime of
  Nothing -> pure False
  Just process -> modifyMVar (oppState process) $ \state ->
    pure
      ( state { oppsProcessMonitorClaimed = True }
      , not (oppsProcessMonitorClaimed state)
      )

releaseOwnedPluginRuntimeProcessMonitor :: OwnedPluginRuntime -> IO ()
releaseOwnedPluginRuntimeProcessMonitor runtime = case oprProcess runtime of
  Nothing -> pure ()
  Just process -> modifyMVar_ (oppState process) $ \state ->
    pure state { oppsProcessMonitorClaimed = False }

-- | Close the accepted transport (when present) and terminate the owned
-- process tree. A failed process cleanup retains the whole aggregate so no
-- destructive partial cleanup can be mistaken for a live Ready runtime.
cleanupOwnedPluginRuntime :: OwnedPluginRuntime -> IO OwnedPluginRuntimeCleanupResult
cleanupOwnedPluginRuntime runtime = mask $ \_ -> do
  case oprConnection runtime of
    Nothing -> pure ()
    Just conn -> do
      _ <- try @SomeException (closeTransport (rpcTransport conn))
      pure ()
  processResult <- case oprProcess runtime of
    Nothing -> pure OwnedPluginCleanupComplete
    Just process -> cleanupOwnedPluginProcess process
  pure $ case processResult of
    OwnedPluginCleanupComplete -> OwnedPluginRuntimeCleanupComplete
    OwnedPluginCleanupFailed _ -> OwnedPluginRuntimeCleanupFailed runtime
      { oprConnection = Nothing }

#if defined(mingw32_HOST_OS)
data PlatformContainment = WindowsJob !HANDLE
#else
-- A new session makes the root pid a stable process-group identity for explicit
-- manager cleanup, including after the root exits. Actual host death closes no
-- portable POSIX containment token, so the launch group can remain alive. A
-- deliberately daemonized descendant can additionally escape explicit cleanup
-- by changing its process group or creating another session; callers must not
-- claim containment beyond the retained launch group.
data PlatformContainment = PosixProcessGroup !Word64
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
launchPluginTransportViaEndpoint executablePath workingDir pluginName startupTimeoutMillis = mask $ \restore -> do
  listenerFailure <- startupFailureInjected "listener"
  serverResult <- if listenerFailure
    then pure (Left (TransportConnectionFailed "forced listener creation failure"))
    else openPluginServer
      defaultTransportConfig { tcTimeout = max 1 startupTimeoutMillis }
      pluginName
  case serverResult of
    Left err -> pure (Left (Text.pack (show err), Nothing))
    Right server ->
      (do
        let endpoint = Text.pack (teAddress (tsEndpoint server))
        pausePluginStartupIfInjected "listener" endpoint
        pausePluginStartupIfInjected "process" endpoint
        preparedResult <- trySync preparePlatformContainment
        case preparedResult of
          Left err -> do
            safeCloseServer server
            pure (Left ("plugin containment setup failed before launch: " <> Text.pack (show err), Nothing))
          Right preparedContainment -> do
            processResult <- trySync $ do
              processFailure <- startupFailureInjected "process"
              if processFailure
                then throwIO (userError "forced process creation failure")
                else pure ()
              launchEnvironment <- endpointEnvironment (tsEndpoint server) pluginName workingDir
              processHandle <- createContainedPluginProcess
                preparedContainment
                executablePath
                workingDir
                (leVariables launchEnvironment)
              ownershipResult <- establishProcessOwnership preparedContainment processHandle
              pure (launchEnvironment, ownershipResult)
            case processResult of
              Left err -> do
                _ <- releasePlatformContainment preparedContainment
                safeCloseServer server
                pure (Left (Text.pack (show err), Nothing))
              Right (launchEnvironment, ownershipResult) ->
                finishOwnedLaunch restore server launchEnvironment ownershipResult
      ) `onException` safeCloseServer server

finishOwnedLaunch
  :: (forall a. IO a -> IO a)
  -> TransportServer
  -> LaunchEnvironment
  -> Either (Text, Maybe OwnedPluginProcess) OwnedPluginProcess
  -> IO (Either (Text, Maybe OwnedPluginProcess) LaunchPluginResult)
finishOwnedLaunch _restore server launchEnvironment ownershipResult =
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
          acceptAttempt <- try @SomeException $ do
            pausePluginStartupIfInjected "accept" (Text.pack (teAddress (tsEndpoint server)))
            acceptFailure <- startupFailureInjected "accept"
            if acceptFailure
              then throwIO (userError "forced endpoint accept failure")
              else tsAcceptWithPeerPolicy server peerPolicy
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

createContainedPluginProcess
  :: Maybe PlatformContainment
  -> FilePath
  -> FilePath
  -> [(String, String)]
  -> IO ProcessHandle
#if defined(mingw32_HOST_OS)
createContainedPluginProcess (Just (WindowsJob jobHandle)) executablePath workingDir environment = do
  forcedFailure <- (== Just "1") <$> lookupEnv jobAssignmentFailureTestEnv
  (application, commandLine) <- windowsApplicationAndCommand executablePath
  alloca $ \processOut ->
    alloca $ \pidOut ->
      withCWString application $ \applicationPtr ->
        withCWString commandLine $ \commandLinePtr ->
          withCWString workingDir $ \workingDirPtr ->
            withCWString (windowsEnvironmentBlock environment) $ \environmentPtr -> do
              errorCode <- c_topoCreateAssignedProcess
                applicationPtr
                commandLinePtr
                workingDirPtr
                environmentPtr
                jobHandle
                (if forcedFailure then 1 else 0)
                processOut
                pidOut
              if errorCode /= 0
                then throwIO (userError
                  ("suspended plugin creation/Job assignment failed with Windows error " <> show errorCode))
                else do
                  rawProcessHandle <- peek processOut
                  if rawProcessHandle == nullPtr
                    then throwIO (userError "suspended plugin creation returned no process handle")
                    else mkProcessHandle rawProcessHandle False nullPtr
createContainedPluginProcess Nothing _ _ _ =
  throwIO (userError "missing pre-created plugin Job")

windowsApplicationAndCommand :: FilePath -> IO (FilePath, String)
windowsApplicationAndCommand executablePath =
  pure (executablePath, quoteWindowsArgument executablePath)

quoteWindowsArgument :: String -> String
quoteWindowsArgument value = "\"" <> concatMap escape value <> "\""
  where
    escape '\"' = "\\\""
    escape character = [character]

windowsEnvironmentBlock :: [(String, String)] -> String
windowsEnvironmentBlock variables =
  concatMap (\(key, value) -> key <> "=" <> value <> "\0") ordered <> "\0"
  where
    ordered = sortOn (map toLower . fst) variables

foreign import ccall unsafe "topo_create_assigned_process_w"
  c_topoCreateAssignedProcess
    :: Ptr CWchar
    -> Ptr CWchar
    -> Ptr CWchar
    -> Ptr CWchar
    -> HANDLE
    -> CInt
    -> Ptr PHANDLE
    -> Ptr Word32
    -> IO Word32
#else
createContainedPluginProcess _ executablePath workingDir environment = do
  (_, _, _, processHandle) <- createProcess
    (proc executablePath [])
      { cwd = Just workingDir
      , env = Just environment
      , std_in = NoStream
      , std_out = NoStream
      , std_err = Inherit
      , new_session = True
      }
  pure processHandle
#endif

-- Process creation calls this while asynchronous exceptions are masked. The
-- owner exists before pid discovery and containment attachment, so every setup
-- failure can be cleaned or returned as that same residual owner.
establishProcessOwnership
  :: Maybe PlatformContainment
  -> ProcessHandle
  -> IO (Either (Text, Maybe OwnedPluginProcess) OwnedPluginProcess)
establishProcessOwnership preparedContainment processHandle = mask $ \_ -> do
  state <- newMVar OwnedPluginProcessState
    { oppsContainment = preparedContainment
    , oppsCleanupComplete = False
    , oppsProcessMonitorClaimed = False
    }
  identityResult <- trySync (processHandlePidWord64 processHandle)
  let mIdentity = either (const Nothing) id identityResult
      owned = OwnedPluginProcess processHandle mIdentity state
  setupResult <- trySync $ do
    identity <- case mIdentity of
      Nothing -> throwIO (userError "could not determine launched plugin process identity")
      Just identity -> pure identity
    containment <- attachPlatformContainment preparedContainment identity processHandle
    modifyMVar state $ \ownershipState ->
      pure (ownershipState { oppsContainment = Just containment }, ())
    forcedFailure <- lookupEnv containmentFailureTestEnv
    if forcedFailure == Just "1"
      then throwIO (userError "forced plugin containment setup failure")
      else pure ()
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

jobAssignmentFailureTestEnv :: String
jobAssignmentFailureTestEnv = "TOPO_TEST_PLUGIN_JOB_ASSIGNMENT_FAILURE"

jobPreparationFailureTestEnv :: String
jobPreparationFailureTestEnv = "TOPO_TEST_PLUGIN_JOB_PREPARATION_FAILURE"

startupFailureTestEnv :: String
startupFailureTestEnv = "TOPO_TEST_PLUGIN_STARTUP_FAILURE"

startupPauseTestEnv :: String
startupPauseTestEnv = "TOPO_TEST_PLUGIN_STARTUP_PAUSE"

startupPauseMarkerTestEnv :: String
startupPauseMarkerTestEnv = "TOPO_TEST_PLUGIN_STARTUP_PAUSE_MARKER"

startupFailureInjected :: String -> IO Bool
startupFailureInjected phase = (== Just phase) <$> lookupEnv startupFailureTestEnv

-- | Deterministic test-only barrier at a startup ownership handoff. The marker
-- contains the endpoint or owner diagnostic supplied by the caller. Production
-- launches do not set these environment variables and never enter the barrier.
pausePluginStartupIfInjected :: String -> Text -> IO ()
pausePluginStartupIfInjected phase diagnostic = do
  requested <- (== Just phase) <$> lookupEnv startupPauseTestEnv
  when requested $ do
    marker <- lookupEnv startupPauseMarkerTestEnv >>= maybe
      (throwIO (userError (startupPauseMarkerTestEnv <> " is required")))
      pure
    BS.writeFile marker (TextEncoding.encodeUtf8 (diagnostic <> "\n"))
    waitForCancellation
  where
    waitForCancellation = threadDelay 1000000 >> waitForCancellation

preparePlatformContainment :: IO (Maybe PlatformContainment)
#if defined(mingw32_HOST_OS)
preparePlatformContainment = do
  forcedFailure <- lookupEnv jobPreparationFailureTestEnv
  if forcedFailure == Just "create"
    then throwIO (userError "forced kill-on-close Job creation failure")
    else pure ()
  jobHandle <- createKillOnCloseJob
  if jobHandle == nullPtr
    then throwIO (userError "could not create/configure a kill-on-close Job")
    else if forcedFailure == Just "configure"
      then do
        closeWindowsHandleIgnoring jobHandle
        throwIO (userError "forced kill-on-close Job configuration failure")
      else pure (Just (WindowsJob jobHandle))
#else
preparePlatformContainment = pure Nothing
#endif

attachPlatformContainment
  :: Maybe PlatformContainment
  -> Word64
  -> ProcessHandle
  -> IO PlatformContainment
#if defined(mingw32_HOST_OS)
attachPlatformContainment (Just containment@(WindowsJob _)) _ _ =
  -- The native launcher assigned the still-suspended root before resuming it.
  pure containment
attachPlatformContainment Nothing _ _ =
  throwIO (userError "missing pre-created plugin Job")
#else
attachPlatformContainment _ identity _ = pure (PosixProcessGroup identity)
#endif

#if defined(mingw32_HOST_OS)
type HANDLE = Ptr ()

foreign import ccall unsafe "windows.h CreateJobObjectW"
  c_CreateJobObjectW :: Ptr () -> Ptr () -> IO HANDLE

foreign import ccall unsafe "windows.h SetInformationJobObject"
  c_SetInformationJobObject :: HANDLE -> CInt -> Ptr () -> Word32 -> IO CInt

foreign import ccall unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO CInt

foreign import ccall unsafe "windows.h TerminateJobObject"
  c_TerminateJobObject :: HANDLE -> Word32 -> IO CInt

foreign import ccall unsafe "windows.h QueryInformationJobObject"
  c_QueryInformationJobObject :: HANDLE -> CInt -> Ptr () -> Word32 -> Ptr Word32 -> IO CInt

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

closeWindowsHandleIgnoring :: HANDLE -> IO ()
closeWindowsHandleIgnoring handle = do
  _ <- closeWindowsHandle handle
  pure ()

closeWindowsHandle :: HANDLE -> IO Bool
closeWindowsHandle handle
  | handle == nullPtr = pure True
  | otherwise = (/= 0) <$> c_CloseHandle handle

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
            (terminateOwnedProcess (oppsContainment state) (oppRootHandle owned))
        case terminationResult of
          Right True -> do
            releaseResult <- try @SomeException
              (releasePlatformContainment (oppsContainment state))
            case releaseResult of
              Right True -> pure
                ( state
                    { oppsContainment = Nothing
                    , oppsCleanupComplete = True
                    }
                , OwnedPluginCleanupComplete
                )
              _ -> pure (state, OwnedPluginCleanupFailed owned)
          _ -> pure (state, OwnedPluginCleanupFailed owned)

releasePlatformContainment :: Maybe PlatformContainment -> IO Bool
releasePlatformContainment Nothing = pure True
#if defined(mingw32_HOST_OS)
releasePlatformContainment (Just (WindowsJob jobHandle)) = closeWindowsHandle jobHandle
#else
releasePlatformContainment (Just (PosixProcessGroup _)) = pure True
#endif

terminateOwnedProcess :: Maybe PlatformContainment -> ProcessHandle -> IO Bool
#if defined(mingw32_HOST_OS)
terminateOwnedProcess (Just (WindowsJob jobHandle)) processHandle = do
  -- The Job is the ownership boundary. Terminate and query that same Job; root
  -- exit alone is never accepted as proof that descendants are gone.
  terminated <- (/= 0) <$> c_TerminateJobObject jobHandle 1
  ignoreSyncExceptions (terminateProcess processHandle)
  rootGone <- waitForProcessExitPoll processKillWaitMicros processHandle
  treeGone <- waitForWindowsJobEmpty processKillWaitMicros jobHandle
  pure (terminated && rootGone && treeGone)
terminateOwnedProcess Nothing processHandle = do
  -- Only reachable while recovering a failed pre-assignment launch. It is not
  -- a successful containment mode and cleanup failure retains the owner.
  ignoreSyncExceptions (terminateProcess processHandle)
  waitForProcessExitPoll processKillWaitMicros processHandle
#else
terminateOwnedProcess (Just (PosixProcessGroup groupIdentity)) processHandle = do
  signalPosixProcessGroup sigTERM groupIdentity
  terminated <- waitForPosixProcessGroupGone
    processTerminationWaitMicros groupIdentity processHandle
  treeGone <- if terminated
    then pure True
    else do
      signalPosixProcessGroup sigKILL groupIdentity
      waitForPosixProcessGroupGone processKillWaitMicros groupIdentity processHandle
  -- Reap/observe the root as a separate requirement. The launch group may
  -- outlive its leader, so this check never substitutes for group teardown.
  rootGone <- waitForProcessExitPoll processKillWaitMicros processHandle
  pure (treeGone && rootGone)
terminateOwnedProcess Nothing processHandle = do
  ignoreSyncExceptions (terminateProcess processHandle)
  waitForProcessExitPoll processKillWaitMicros processHandle
#endif

ignoreSyncExceptions :: IO () -> IO ()
ignoreSyncExceptions action = do
  result <- trySync action
  case result of
    Left _ -> pure ()
    Right _ -> pure ()

#if defined(mingw32_HOST_OS)
waitForWindowsJobEmpty :: Int -> HANDLE -> IO Bool
waitForWindowsJobEmpty remainingMicros jobHandle = do
  mActive <- windowsJobActiveProcessCount jobHandle
  case mActive of
    Just 0 -> pure True
    _ | remainingMicros <= 0 -> pure False
      | otherwise -> do
          let delayMicros = min processPollDelayMicros remainingMicros
          threadDelay delayMicros
          waitForWindowsJobEmpty (remainingMicros - delayMicros) jobHandle

windowsJobActiveProcessCount :: HANDLE -> IO (Maybe Word32)
windowsJobActiveProcessCount jobHandle =
  allocaBytes jobObjectBasicAccountingInformationSize $ \infoPtr -> do
    fillBytes infoPtr 0 jobObjectBasicAccountingInformationSize
    queried <- c_QueryInformationJobObject
      jobHandle
      jobObjectBasicAccountingInformation
      infoPtr
      (fromIntegral jobObjectBasicAccountingInformationSize)
      nullPtr
    if queried == 0
      then pure Nothing
      else Just <$> peekByteOff infoPtr jobObjectActiveProcessesOffset

jobObjectBasicAccountingInformation :: CInt
jobObjectBasicAccountingInformation = 1

jobObjectBasicAccountingInformationSize :: Int
jobObjectBasicAccountingInformationSize = 48

jobObjectActiveProcessesOffset :: Int
jobObjectActiveProcessesOffset = 40
#endif

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

#if !defined(mingw32_HOST_OS)
signalPosixProcessGroup :: CInt -> Word64 -> IO ()
signalPosixProcessGroup signalNumber treeIdentity =
  ignoreSyncExceptions $ do
    _ <- c_kill (negate (fromIntegral treeIdentity)) signalNumber
    pure ()

waitForPosixProcessGroupGone :: Int -> Word64 -> ProcessHandle -> IO Bool
waitForPosixProcessGroupGone remainingMicros treeIdentity processHandle = do
  -- Reap the owned leader as soon as it exits; an unreaped zombie remains a
  -- member of the group and would otherwise make verified cleanup fail once.
  _ <- getProcessExitCode processHandle
  exists <- posixProcessGroupExists treeIdentity
  if not exists
    then pure True
    else if remainingMicros <= 0
      then pure False
      else do
        let delayMicros = min processPollDelayMicros remainingMicros
        threadDelay delayMicros
        waitForPosixProcessGroupGone
          (remainingMicros - delayMicros) treeIdentity processHandle

posixProcessGroupExists :: Word64 -> IO Bool
posixProcessGroupExists treeIdentity = do
  result <- c_kill (negate (fromIntegral treeIdentity)) 0
  if result == 0
    then pure True
    else do
      err <- getErrno
      if err == eSRCH
        then pure False
        else if err == eINTR
          then posixProcessGroupExists treeIdentity
          else pure True

foreign import ccall unsafe "kill"
  c_kill :: CInt -> CInt -> IO CInt

sigTERM :: CInt
sigTERM = 15

sigKILL :: CInt
sigKILL = 9
#endif

processTerminationWaitMicros :: Int
processTerminationWaitMicros = 1000000

processKillWaitMicros :: Int
processKillWaitMicros = 2000000

processPollDelayMicros :: Int
processPollDelayMicros = 10000
