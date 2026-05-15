{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.PluginManager (spec, runFixtureCli) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (getSingleton, newActorSystem, shutdownActorSystem)
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
  , PluginStatus(..)
  , discoverPlugins
  , getLoadedPlugins
  , getPluginOverlaySchemas
  , getPluginStages
  , refreshManifests
  , pluginManagerActorDef
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
  , connectPlugin
  , recvMessage
  , sendMessage
  )

spec :: Spec
spec = describe "PluginManager" $ do
  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "launches plugin subprocesses and exposes generator stages cross-platform" $ do
    withExecutablePluginDir testLaunchPluginName testLaunchManifestJSON "ok" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        stages <- getPluginStages pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        map stageName stages `shouldSatisfy` elem (Text.pack testLaunchPluginName)
        pluginStatuses testLaunchPluginName loaded `shouldSatisfy` elem PluginConnected
        shutdownPlugins pluginManagerHandle
        threadDelay 200000
        loadedAfterShutdown <- getLoadedPlugins pluginManagerHandle
        pluginStatuses testLaunchPluginName loadedAfterShutdown `shouldSatisfy` elem PluginDisconnected

  it "reports a protocol-version mismatch as a plugin error" $ do
    withExecutablePluginDir mismatchPluginName mismatchManifestJSON "protocol-mismatch" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses mismatchPluginName loaded `shouldSatisfy` anyPluginErrorContaining "protocol version mismatch"

  it "reports malformed handshake JSON as a plugin error" $ do
    withExecutablePluginDir malformedPluginName malformedManifestJSON "malformed-json" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses malformedPluginName loaded `shouldSatisfy` anyPluginErrorContaining "RPCProtocolError"

  it "reports early plugin exit during startup as a plugin error" $ do
    withExecutablePluginDir crashPluginName crashManifestJSON "early-exit" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        refreshManifests pluginManagerHandle
        loaded <- getLoadedPlugins pluginManagerHandle
        pluginStatuses crashPluginName loaded `shouldSatisfy` anyPluginError

  it "reports handshake timeouts as plugin errors" $ do
    withExecutablePluginDir slowPluginName slowManifestJSON "slow" $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
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
    _ -> die "usage: topo-seer-test --plugin-manager-fixture <ok|protocol-mismatch|malformed-json|early-exit|slow>"

runFixtureMode :: String -> IO ()
runFixtureMode = \case
  "ok" -> runOkFixture
  "protocol-mismatch" -> runOneShotAckFixture (currentProtocolVersion + 1)
  "malformed-json" -> runMalformedJsonFixture
  "early-exit" -> exitFailure
  "slow" -> threadDelay 2000000 >> runOkFixture
  unknown -> die ("unknown plugin-manager fixture: " <> unknown)

runOkFixture :: IO ()
runOkFixture = do
  connectPlugin "plugin-manager-test-fixture" stdin stdout >>= \case
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

runOneShotAckFixture :: Int -> IO ()
runOneShotAckFixture protocolVersion = do
  connectPlugin "plugin-manager-protocol-mismatch-fixture" stdin stdout >>= \case
    Left _ -> exitFailure
    Right transport -> do
      _ <- recvMessage transport
      _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope protocolVersion))
      closeTransport transport

runMalformedJsonFixture :: IO ()
runMalformedJsonFixture = do
  connectPlugin "plugin-manager-malformed-json-fixture" stdin stdout >>= \case
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
