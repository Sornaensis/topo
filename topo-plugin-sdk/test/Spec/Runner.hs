{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Runner (spec) where

import Control.Concurrent (MVar, forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import System.IO (BufferMode(NoBuffering), Handle, hSetBinaryMode, hSetBuffering)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec

import Topo.Plugin.RPC.Protocol
  ( GeneratorResult(..)
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , PluginError(..)
  , PluginLog(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , SimulationResult(..)
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , closeTransport
  , recvMessage
  , sendMessage
  )
import Topo.Plugin.SDK.Runner (runPluginSession)
import Topo.Plugin.SDK.Types

spec :: Spec
spec = describe "SDK runner pipe integration" $ do
  it "handles invoke_generator and returns generator_result payload" $
    withTransportPair $ \host plugin -> do
      done <- startSession generatorPlugin plugin
      sendEnvelope host (generatorInvoke 77)
      env <- recvEnvelope host
      envType env `shouldBe` MsgGeneratorResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          grTerrain result `shouldBe` object ["ok" .= True]
          grOverlay result `shouldBe` Just (object ["seeded" .= (1 :: Int)])
          grMetadata result `shouldBe` Just (object ["source" .= ("test" :: Text)])
      shutdownAndWait host done

  it "handles invoke_simulation with log interleaving before simulation_result" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationPlugin plugin
      sendEnvelope host simulationInvoke
      env1 <- recvEnvelope host
      env2 <- recvEnvelope host
      env3 <- recvEnvelope host
      map envType [env1, env2, env3] `shouldBe` [MsgLog, MsgLog, MsgSimulationResult]
      logMessages <- traverse decodeLog [env1, env2]
      logMessages `shouldBe` ["sim:start", "sim:end"]
      case Aeson.fromJSON (envPayload env3) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          srOverlay result `shouldBe` object ["population" .= (42 :: Int)]
          srTerrainWrites result `shouldBe` Just (object [])
      shutdownAndWait host done

  it "returns plugin error for invoke_generator when generator capability is missing" $
    withTransportPair $ \host plugin -> do
      done <- startSession simOnlyPlugin plugin
      sendEnvelope host (generatorInvoke 0)
      env <- recvEnvelope host
      envType env `shouldBe` MsgError
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success pluginErr -> do
          peCode pluginErr `shouldBe` 2
          peMessage pluginErr `shouldBe` "Plugin has no generator"
      shutdownAndWait host done

  it "shuts down cleanly on shutdown message" $
    withTransportPair $ \host plugin -> do
      done <- startSession simulationPlugin plugin
      shutdownAndWait host done

withTransportPair :: (Transport -> Transport -> IO a) -> IO a
withTransportPair action = do
  (host, plugin) <- mkTransportPair
  action host plugin `finally` do
    closeTransport host
    closeTransport plugin

mkTransportPair :: IO (Transport, Transport)
mkTransportPair = do
  (hostToPluginRead, hostToPluginWrite) <- createPipe
  (pluginToHostRead, pluginToHostWrite) <- createPipe
  configureHandle hostToPluginRead
  configureHandle hostToPluginWrite
  configureHandle pluginToHostRead
  configureHandle pluginToHostWrite
  now <- getPOSIXTime
  let timestamp = Text.pack (show (round (now * 1000000) :: Integer))
  let host = Transport
        { tReadHandle = pluginToHostRead
        , tWriteHandle = hostToPluginWrite
        , tPluginName = "test-host-" <> timestamp
        }
      plugin = Transport
        { tReadHandle = hostToPluginRead
        , tWriteHandle = pluginToHostWrite
        , tPluginName = "test-plugin-" <> timestamp
        }
  pure (host, plugin)

configureHandle :: Handle -> IO ()
configureHandle handle = do
  hSetBinaryMode handle True
  hSetBuffering handle NoBuffering

startSession :: PluginDef -> Transport -> IO (MVar ())
startSession plugin transport = do
  done <- newEmptyMVar
  _ <- forkFinally (runPluginSession plugin transport Map.empty) (\_ -> putMVar done ())
  pure done

sendEnvelope :: Transport -> RPCEnvelope -> IO ()
sendEnvelope transport envelope = do
  sendResult <- sendMessage transport (encodeMessage envelope)
  case sendResult of
    Left err -> expectationFailure ("send failed: " <> show err)
    Right () -> pure ()

recvEnvelope :: Transport -> IO RPCEnvelope
recvEnvelope transport = do
  recvResult <- timeout 2000000 (recvMessage transport)
  case recvResult of
    Nothing -> expectationFailure "recv timeout" >> fail "timeout"
    Just (Left err) -> expectationFailure ("recv failed: " <> show err) >> fail "recv"
    Just (Right bytes) ->
      case decodeMessage bytes of
        Left err -> expectationFailure ("decode failed: " <> Text.unpack err) >> fail "decode"
        Right env -> pure env

decodeLog :: RPCEnvelope -> IO Text
decodeLog envelope =
  case Aeson.fromJSON (envPayload envelope) of
    Aeson.Error err -> expectationFailure err >> fail "log decode"
    Aeson.Success (pluginLog :: PluginLog) -> pure (plMessage pluginLog)

generatorInvoke :: Word64 -> RPCEnvelope
generatorInvoke seed = RPCEnvelope
  { envType = MsgInvokeGenerator
  , envPayload = Aeson.toJSON InvokeGenerator
      { igStageId = "plugin:test"
      , igSeed = seed
      , igConfig = Map.empty
      , igTerrain = object ["terrain" .= ("summary" :: Text)]
      }
  }

simulationInvoke :: RPCEnvelope
simulationInvoke = RPCEnvelope
  { envType = MsgInvokeSimulation
  , envPayload = Aeson.toJSON InvokeSimulation
      { isNodeId = "simulation"
      , isWorldTime = 5
      , isDeltaTicks = 1
      , isCalendar = object ["year" .= (0 :: Int)]
      , isConfig = Map.empty
      , isTerrain = object ["terrain" .= ("summary" :: Text)]
      , isOverlays = object ["weather" .= object ["storage" .= ("sparse" :: Text), "chunks" .= ([] :: [Aeson.Value])]]
      , isOwnOverlay = object ["storage" .= ("sparse" :: Text), "chunks" .= ([] :: [Aeson.Value])]
      }
  }

shutdownAndWait :: Transport -> MVar () -> IO ()
shutdownAndWait host done = do
  sendEnvelope host (RPCEnvelope MsgShutdown (object []))
  waitResult <- timeout 2000000 (takeMVar done)
  case waitResult of
    Nothing -> expectationFailure "plugin session did not shut down in time"
    Just () -> pure ()

generatorPlugin :: PluginDef
generatorPlugin = defaultPluginDef
  { pdName = "generator"
  , pdVersion = "1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = []
      , gdRun = \_ctx -> pure (Right GeneratorTickResult
          { gtrTerrain = object ["ok" .= True]
          , gtrOverlay = Just (object ["seeded" .= (1 :: Int)])
          , gtrMetadata = Just (object ["source" .= ("test" :: Text)])
          })
      }
  }

simulationPlugin :: PluginDef
simulationPlugin = defaultPluginDef
  { pdName = "simulation"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdTick = \ctx -> do
          pcLog ctx "sim:start"
          pcLog ctx "sim:end"
          pure (Right SimulationTickResult
            { strOverlay = object ["population" .= (42 :: Int)]
            , strTerrainWrites = Just (object [])
            })
      }
  }

simOnlyPlugin :: PluginDef
simOnlyPlugin = defaultPluginDef
  { pdName = "sim-only"
  , pdVersion = "1.0"
  , pdSchemaFile = Just "sim.toposchema"
  , pdSimulation = Just SimulationDef
      { sdDependencies = []
      , sdTick = \_ -> pure (Right defaultSimulationTickResult)
      }
  }
