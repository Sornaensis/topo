{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and production transport attachment.
module Actor.PluginManager.ProcessLauncher
  ( resolvePluginExecutable
  , launchPluginTransport
  , safeCloseHandle
  , safeTerminateProcess
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Crypto.Random (getRandomBytes)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Word (Word8)
import Numeric (showHex)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment)
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


import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.RPC.Transport
  ( Transport
  , TransportConfig(..)
  , TransportEndpoint(..)
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

launchPluginTransport
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe ProcessHandle) (Transport, ProcessHandle))
launchPluginTransport = launchPluginTransportViaEndpoint

-- Keep the endpoint accept budget aligned with the startup/handshake timeout
-- so slow or crashed plugins fail startup instead of blocking discovery.
launchPluginTransportViaEndpoint
  :: FilePath
  -> FilePath
  -> Text
  -> Int
  -> IO (Either (Text, Maybe ProcessHandle) (Transport, ProcessHandle))
launchPluginTransportViaEndpoint executablePath workingDir pluginName startupTimeoutMillis = do
  serverResult <- openPluginServer
    defaultTransportConfig { tcTimeout = max 1 startupTimeoutMillis }
    pluginName
  case serverResult of
    Left err -> pure (Left (Text.pack (show err), Nothing))
    Right server -> do
      processResult <- try @SomeException $ do
        environment <- endpointEnvironment (tsEndpoint server) pluginName workingDir
        createProcess
          (proc executablePath [])
            { cwd = Just workingDir
            , env = Just environment
            , std_in = NoStream
            , std_out = NoStream
            , std_err = Inherit
            }
      case processResult of
        Left err -> do
          tsClose server
          pure (Left (Text.pack (show err), Nothing))
        Right (_, _, _, processHandle) -> do
          acceptResult <- tsAccept server
          case acceptResult of
            Left transportErr -> do
              tsClose server
              terminated <- safeTerminateProcess processHandle
              let mProcessHandle = if terminated then Nothing else Just processHandle
              pure (Left (Text.pack (show transportErr), mProcessHandle))
            Right transport -> pure (Right (transport, processHandle))

endpointEnvironment :: TransportEndpoint -> Text -> FilePath -> IO [(String, String)]
endpointEnvironment endpoint pluginName workingDir = do
  inherited <- getEnvironment
  launchSession <- freshLaunchSecret "session"
  authToken <- freshLaunchSecret "auth"
  let dataRoot = workingDir </> "data"
      launchVars =
        [ (pluginIdEnv, Text.unpack pluginName)
        , (pluginProtocolEnv, show currentProtocolVersion)
        , (pluginEndpointEnv, teAddress endpoint)
        , (pluginEndpointKindEnv, Text.unpack (endpointKindText (teKind endpoint)))
        , (pluginSessionEnv, launchSession)
        , (pluginAuthTokenEnv, authToken)
        , (pluginWorldIdEnv, unsavedWorldId)
        , (pluginDataRootEnv, dataRoot)
        ]
      -- Production launches always use the endpoint variables above; do not
      -- leak a developer shell's stdio compatibility flag into plugin processes.
      overridden = pluginStdioCompatibilityEnv : map fst launchVars
      preserved = filter (not . isOverriddenEnvKey overridden . fst) inherited
  createDirectoryIfMissing True dataRoot
  pure (launchVars <> preserved)

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

unsavedWorldId :: String
unsavedWorldId = "unsaved"

safeCloseHandle :: Handle -> IO ()
safeCloseHandle handle = do
  _ <- try @SomeException (hClose handle)
  pure ()

safeTerminateProcess :: ProcessHandle -> IO Bool
safeTerminateProcess processHandle = do
  if os == "mingw32"
    then terminateWindowsProcessTree processHandle
    else pure ()
  _ <- try @SomeException (terminateProcess processHandle)
  waitForProcessExitPoll processTerminationWaitMicros processHandle

terminateWindowsProcessTree :: ProcessHandle -> IO ()
terminateWindowsProcessTree processHandle = do
  mPid <- getPid processHandle
  case mPid of
    Nothing -> pure ()
    Just pid -> do
      taskkillResult <- try @SomeException $
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

processTerminationWaitMicros :: Int
processTerminationWaitMicros = 1000000

processPollDelayMicros :: Int
processPollDelayMicros = 10000
