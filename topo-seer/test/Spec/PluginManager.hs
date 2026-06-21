{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.PluginManager (spec, runFixtureCli) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, get, newActorSystem, shutdownActorSystem)
import System.Directory
  ( Permissions(..)
  , createDirectoryIfMissing
  , getHomeDirectory
  , getPermissions
  , removePathForcibly
  , setPermissions
  )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (stdin, stdout)
import Test.Hspec

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManager
  , PluginStatus(..)
  , discoverPlugins
  , getLoadedPlugins
  , getPluginOverlaySchemas
  , getPluginStages
  , refreshManifests
  , shutdownPlugins
  )
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin.RPC.Protocol
  ( HandshakeAck(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , currentProtocolVersion
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( closeTransport
  , connectPluginFromEnvironment
  , recvMessage
  , sendMessage
  )

getPluginManager :: ActorSystem -> IO (ActorHandle PluginManager (Protocol PluginManager))
getPluginManager system = get @PluginManager system

spec :: Spec
spec = describe "PluginManager" $ do
  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "launches plugin subprocesses and exposes generator stages cross-platform" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        stages <- getPluginStages pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        map stageName stages `shouldSatisfy` elem (Text.pack testLaunchPluginName)
        pluginStatuses testLaunchPluginName loaded `shouldSatisfy` elem PluginConnected
        pluginLifecycleStates testLaunchPluginName loaded `shouldSatisfy` elem LifecycleReady
        pluginLifecycleProtocols testLaunchPluginName loaded `shouldSatisfy` elem (Just currentProtocolVersion)
        shutdownPlugins pluginManagerHandle
        threadDelay 200000
        loadedAfterShutdown <- getLoadedPlugins pluginManagerHandle
        pluginStatuses testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem PluginDisconnected
        pluginLifecycleStates testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem LifecycleStopped

  it "exposes Starting while public refreshManifests performs supervisor work" $ do
    withExecutablePluginDir refreshTransientPluginName refreshTransientManifestJSON "slow" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        done <- newEmptyMVar
        _ <- forkIO (refreshManifests pluginManagerHandle >> putMVar done ())
        threadDelay 50000
        starting <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName starting `shouldSatisfy` elem LifecycleStarting
        takeMVar done
        failed <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates refreshTransientPluginName failed `shouldSatisfy` elem LifecycleFailed

  it "exposes Stopping while public shutdownPlugins performs supervisor work" $ do
    withExecutablePluginDir shutdownTransientPluginName shutdownTransientManifestJSON "slow-shutdown" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        ready <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName ready `shouldSatisfy` elem LifecycleReady
        done <- newEmptyMVar
        _ <- forkIO (shutdownPlugins pluginManagerHandle >> putMVar done ())
        threadDelay 50000
        stopping <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName stopping `shouldSatisfy` elem LifecycleStopping
        takeMVar done
        stopped <- getLoadedPlugins pluginManagerHandle
        pluginLifecycleStates shutdownTransientPluginName stopped `shouldSatisfy` elem LifecycleStopped

  it "reports a protocol-version mismatch as a plugin error" $ do
    withExecutablePluginDir mismatchPluginName mismatchManifestJSON "protocol-mismatch" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses mismatchPluginName loaded `shouldSatisfy` anyPluginErrorContaining "protocol version mismatch"
        pluginLifecycleStates mismatchPluginName loaded `shouldSatisfy` elem LifecycleFailed

  it "reports malformed handshake JSON as a plugin error" $ do
    withExecutablePluginDir malformedPluginName malformedManifestJSON "malformed-json" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses malformedPluginName loaded `shouldSatisfy` anyPluginErrorContaining "RPCProtocolError"

  it "reports early plugin exit during startup as a plugin error" $ do
    withExecutablePluginDir crashPluginName crashManifestJSON "early-exit" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses crashPluginName loaded `shouldSatisfy` anyPluginError

  it "reports handshake timeouts as plugin errors" $ do
    withExecutablePluginDir slowPluginName slowManifestJSON "slow" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getPluginManager system
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses slowPluginName loaded `shouldSatisfy` anyPluginErrorContaining "timed out"

pluginStatuses :: String -> [LoadedPlugin] -> [PluginStatus]
pluginStatuses name loaded =
  [ lpStatus plugin
  | plugin <- loaded
  , lpName plugin == Text.pack name
  ]

pluginLifecycles :: String -> [LoadedPlugin] -> [PluginLifecycleSnapshot]
pluginLifecycles name loaded =
  [ lpLifecycle plugin
  | plugin <- loaded
  , lpName plugin == Text.pack name
  ]

pluginLifecycleStates :: String -> [LoadedPlugin] -> [PluginLifecycleState]
pluginLifecycleStates name = map plsState . pluginLifecycles name

pluginLifecycleProtocols :: String -> [LoadedPlugin] -> [Maybe Int]
pluginLifecycleProtocols name = map plsProtocolVersion . pluginLifecycles name

anyPluginError :: [PluginStatus] -> Bool
anyPluginError = any $ \case
  PluginError _ -> True
  _ -> False

anyPluginErrorContaining :: Text -> [PluginStatus] -> Bool
anyPluginErrorContaining needle = any $ \case
  PluginError msg -> needle `Text.isInfixOf` msg
  _ -> False

withTestPluginDir :: String -> BS.ByteString -> BS.ByteString -> IO a -> IO a
withTestPluginDir pluginName manifestJSON schemaJSON action =
  bracket setup teardown (const action)
  where
    setup = do
      home <- getHomeDirectory
      let pluginDir = home </> ".topo" </> "plugins" </> pluginName
      createDirectoryIfMissing True pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> "test.toposchema") schemaJSON
      pure pluginDir

    teardown = removePathForcibly

withExecutablePluginDir :: String -> BS.ByteString -> String -> IO a -> IO a
withExecutablePluginDir pluginName manifestJSON fixtureMode action =
  bracket setup teardown (const action)
  where
    setup = do
      home <- getHomeDirectory
      let pluginDir = home </> ".topo" </> "plugins" </> pluginName
      createDirectoryIfMissing True pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      writePluginWrapper pluginDir pluginName fixtureMode
      pure pluginDir

    teardown = removePathForcibly

writePluginWrapper :: FilePath -> String -> String -> IO ()
writePluginWrapper pluginDir pluginName fixtureMode = do
  testExe <- getExecutablePath
  if os == "mingw32"
    then do
      let wrapperPath = pluginDir </> (pluginName <> ".cmd")
      writeFile wrapperPath $ unlines
        [ "@echo off"
        , "\"" <> testExe <> "\" --plugin-manager-fixture " <> fixtureMode
        ]
    else do
      let wrapperPath = pluginDir </> pluginName
      writeFile wrapperPath $ unlines
        [ "#!/bin/sh"
        , "exec " <> shellQuote testExe <> " --plugin-manager-fixture " <> shellQuote fixtureMode
        ]
      permissions <- getPermissions wrapperPath
      setPermissions wrapperPath permissions { executable = True }

shellQuote :: String -> String
shellQuote value = "'" <> concatMap quoteChar value <> "'"
  where
    quoteChar '\'' = "'\\''"
    quoteChar c = [c]

runFixtureCli :: IO ()
runFixtureCli = do
  args <- getArgs
  case args of
    ["--plugin-manager-fixture", mode] -> runFixtureMode mode
    _ -> die "usage: topo-seer-test --plugin-manager-fixture <ok|protocol-mismatch|malformed-json|early-exit|slow|slow-shutdown>"

runFixtureMode :: String -> IO ()
runFixtureMode = \case
  "ok" -> runOkFixture
  "protocol-mismatch" -> runOneShotAckFixture (currentProtocolVersion + 1)
  "malformed-json" -> runMalformedJsonFixture
  "early-exit" -> exitFailure
  "slow" -> threadDelay 2000000 >> runOkFixture
  "slow-shutdown" -> runSlowShutdownFixture
  unknown -> die ("unknown plugin-manager fixture: " <> unknown)

runOkFixture :: IO ()
runOkFixture = do
  connectPluginFromEnvironment "plugin-manager-test-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope currentProtocolVersion))
                loop transport
              MsgShutdown -> closeTransport transport
              _ -> loop transport

runSlowShutdownFixture :: IO ()
runSlowShutdownFixture = do
  connectPluginFromEnvironment "plugin-manager-slow-shutdown-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> loop transport
  where
    loop transport = do
      recvMessage transport >>= \case
        Left _ -> closeTransport transport
        Right bytes ->
          case decodeMessage bytes of
            Left _ -> loop transport
            Right envelope -> case envType envelope of
              MsgHandshake -> do
                _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope currentProtocolVersion))
                loop transport
              MsgShutdown -> threadDelay 1000000 >> closeTransport transport
              _ -> loop transport

runOneShotAckFixture :: Int -> IO ()
runOneShotAckFixture protocolVersion = do
  connectPluginFromEnvironment "plugin-manager-protocol-mismatch-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope protocolVersion))
      closeTransport transport

runMalformedJsonFixture :: IO ()
runMalformedJsonFixture = do
  connectPluginFromEnvironment "plugin-manager-malformed-json-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      _ <- sendMessage transport (BSC.pack "{not valid json")
      closeTransport transport

handshakeAckEnvelope :: Int -> RPCEnvelope
handshakeAckEnvelope protocolVersion = RPCEnvelope
  { envType = MsgHandshakeAck
  , envPayload = Aeson.toJSON HandshakeAck
      { haProtocolVersion = protocolVersion
      , haDataDirectory = Nothing
      , haResources = []
      }
  }

testPluginName :: String
testPluginName = "copilot-test-plugin-manager-schema"

testManifestJSON :: BS.ByteString
testManifestJSON =
  "{\n"
    <> "  \"name\": \"copilot-test-plugin-manager-schema\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"overlay\": { \"schemaFile\": \"test.toposchema\" }\n"
    <> "}\n"

testSchemaJSON :: BS.ByteString
testSchemaJSON =
  "{\n"
    <> "  \"name\": \"copilot_test_overlay\",\n"
    <> "  \"version\": \"1.0.0\",\n"
    <> "  \"storage\": \"sparse\",\n"
    <> "  \"fields\": [\n"
    <> "    {\n"
    <> "      \"name\": \"value\",\n"
    <> "      \"type\": \"float\",\n"
    <> "      \"default\": 0.0,\n"
    <> "      \"indexed\": false\n"
    <> "    }\n"
    <> "  ]\n"
    <> "}\n"

testLaunchPluginName :: String
testLaunchPluginName = "copilot-test-plugin-launch"

testLaunchManifestJSON :: BS.ByteString
testLaunchManifestJSON = manifestFor testLaunchPluginName

mismatchPluginName :: String
mismatchPluginName = "copilot-test-plugin-protocol-mismatch"

mismatchManifestJSON :: BS.ByteString
mismatchManifestJSON = manifestFor mismatchPluginName

malformedPluginName :: String
malformedPluginName = "copilot-test-plugin-malformed-json"

malformedManifestJSON :: BS.ByteString
malformedManifestJSON = manifestFor malformedPluginName

crashPluginName :: String
crashPluginName = "copilot-test-plugin-crashy"

crashManifestJSON :: BS.ByteString
crashManifestJSON = manifestFor crashPluginName

refreshTransientPluginName :: String
refreshTransientPluginName = "copilot-test-plugin-refresh-transient"

refreshTransientManifestJSON :: BS.ByteString
refreshTransientManifestJSON = manifestFor refreshTransientPluginName

shutdownTransientPluginName :: String
shutdownTransientPluginName = "copilot-test-plugin-shutdown-transient"

shutdownTransientManifestJSON :: BS.ByteString
shutdownTransientManifestJSON = manifestFor shutdownTransientPluginName

slowPluginName :: String
slowPluginName = "copilot-test-plugin-slow"

slowManifestJSON :: BS.ByteString
slowManifestJSON = manifestFor slowPluginName

manifestFor :: String -> BS.ByteString
manifestFor name = BSC.pack $
  "{\n"
    <> "  \"name\": \"" <> name <> "\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }\n"
    <> "}\n"
