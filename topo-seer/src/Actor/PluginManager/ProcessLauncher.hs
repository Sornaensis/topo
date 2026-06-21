{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Plugin subprocess startup and production transport attachment.
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
import System.Environment (getEnvironment)
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

import Topo.Plugin.RPC.Transport
  ( Transport
  , TransportConfig(..)
  , TransportEndpoint(..)
  , TransportServer(..)
  , defaultTransportConfig
  , endpointKindText
  , openPluginServer
  , pluginEndpointEnv
  , pluginEndpointKindEnv
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
  -> IO (Either Text (Transport, ProcessHandle))
launchPluginTransport = launchPluginTransportViaEndpoint

-- Keep the endpoint accept budget aligned with the handshake timeout so
-- slow or crashed plugins fail startup instead of blocking discovery.
transportAcceptTimeoutMillis :: Int
transportAcceptTimeoutMillis = 1000

launchPluginTransportViaEndpoint
  :: FilePath
  -> FilePath
  -> Text
  -> IO (Either Text (Transport, ProcessHandle))
launchPluginTransportViaEndpoint executablePath workingDir pluginName = do
  serverResult <- openPluginServer
    defaultTransportConfig { tcTimeout = transportAcceptTimeoutMillis }
    pluginName
  case serverResult of
    Left err -> pure (Left (Text.pack (show err)))
    Right server -> do
      processResult <- try @SomeException $ do
        environment <- endpointEnvironment (tsEndpoint server)
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
          pure (Left (Text.pack (show err)))
        Right (_, _, _, processHandle) -> do
          acceptResult <- tsAccept server
          case acceptResult of
            Left transportErr -> do
              tsClose server
              safeTerminateProcess processHandle
              pure (Left (Text.pack (show transportErr)))
            Right transport -> pure (Right (transport, processHandle))

endpointEnvironment :: TransportEndpoint -> IO [(String, String)]
endpointEnvironment endpoint = do
  inherited <- getEnvironment
  let endpointVars =
        [ (pluginEndpointEnv, teAddress endpoint)
        , (pluginEndpointKindEnv, Text.unpack (endpointKindText (teKind endpoint)))
        ]
      overridden = map fst endpointVars
      preserved = filter (\(key, _) -> key `notElem` overridden) inherited
  pure (endpointVars <> preserved)

safeCloseHandle :: Handle -> IO ()
safeCloseHandle handle = do
  _ <- try @SomeException (hClose handle)
  pure ()

safeTerminateProcess :: ProcessHandle -> IO ()
safeTerminateProcess processHandle = do
  _ <- try @SomeException (terminateProcess processHandle)
  pure ()
