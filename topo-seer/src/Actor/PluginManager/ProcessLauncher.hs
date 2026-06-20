{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and test transport attachment.
module Actor.PluginManager.ProcessLauncher
  ( resolvePluginExecutable
  , launchPluginTransport
  , safeCloseHandle
  , safeTerminateProcess
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hClose)
import System.Info (os)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , terminateProcess
  )

import Topo.Plugin.RPC.Transport (Transport, connectPlugin)

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
  -> IO (Either Text (Transport, ProcessHandle))
launchPluginTransport executablePath workingDir pluginName = do
  processResult <- try @SomeException
    (createProcess
      (proc executablePath [])
        { cwd = Just workingDir
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        })
  case processResult of
    Left err -> pure (Left (Text.pack (show err)))
    Right (mStdin, mStdout, _, processHandle) ->
      case (mStdin, mStdout) of
        (Just childStdin, Just childStdout) -> do
          connectionResult <- connectPlugin pluginName childStdout childStdin
          case connectionResult of
            Left transportErr -> do
              safeCloseHandle childStdin
              safeCloseHandle childStdout
              safeTerminateProcess processHandle
              pure (Left (Text.pack (show transportErr)))
            Right transport -> pure (Right (transport, processHandle))
        _ -> do
          mapM_ safeCloseHandle mStdin
          mapM_ safeCloseHandle mStdout
          safeTerminateProcess processHandle
          pure (Left "failed to acquire plugin stdio handles")

safeCloseHandle :: Handle -> IO ()
safeCloseHandle handle = do
  _ <- try @SomeException (hClose handle)
  pure ()

safeTerminateProcess :: ProcessHandle -> IO ()
safeTerminateProcess processHandle = do
  _ <- try @SomeException (terminateProcess processHandle)
  pure ()
