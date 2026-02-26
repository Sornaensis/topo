{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | High-level plugin RPC client API.
--
-- This module provides 'RPCConnection' (an active plugin session)
-- and functions to invoke generator stages and simulation ticks
-- over the transport layer.  It also provides constructors that
-- produce 'PipelineStage' and 'SimNode' values from an RPC
-- connection, integrating plugins into the generator pipeline and
-- simulation DAG respectively.
--
-- = Lifecycle
--
-- 1. Parse the plugin manifest ('parseManifestFile')
-- 2. Launch the plugin process (Phase 7 — plugin manager)
-- 3. Connect via 'connectPlugin' to get a 'Transport'
-- 4. Create an 'RPCConnection' with 'newRPCConnection'
-- 5. Use 'rpcGeneratorStage' and\/or 'rpcSimNode' to integrate
-- 6. On shutdown, call 'rpcShutdown' then 'closeTransport'
module Topo.Plugin.RPC
  ( -- * Connection
    RPCConnection(..)
  , newRPCConnection
    -- * Invocation
  , invokeGenerator
  , invokeSimulation
  , rpcShutdown
    -- * Pipeline / DAG integration
  , rpcGeneratorStage
  , rpcSimNode
    -- * Errors
  , RPCError(..)
    -- * Re-exports
  , module Topo.Plugin.RPC.Manifest
  , module Topo.Plugin.RPC.Transport
  , module Topo.Plugin.RPC.Protocol
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)

import Topo.Overlay (Overlay(..))
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (PluginError(..), PluginM, getWorldP, modifyWorldP, logInfo)
import Topo.Simulation (SimNode(..), SimNodeId(..), SimContext(..), emptyTerrainWrites)

import Topo.Plugin.RPC.Manifest
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Transport

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Errors from the RPC layer.
data RPCError
  = RPCTransportError !TransportError
    -- ^ Underlying transport failure.
  | RPCProtocolError !Text
    -- ^ Invalid message format or unexpected message type.
  | RPCPluginError !Int !Text
    -- ^ Plugin-reported error with code and message.
  | RPCTimeout !Text
    -- ^ Plugin did not respond in time.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

-- | An active RPC session with a plugin.
data RPCConnection = RPCConnection
  { rpcManifest  :: !RPCManifest
    -- ^ The plugin's parsed manifest.
  , rpcTransport :: !Transport
    -- ^ The underlying transport handle.
  , rpcParams    :: !(Map Text Value)
    -- ^ Current parameter values for this plugin.
  }

-- | Create an 'RPCConnection' from a manifest and transport.
newRPCConnection :: RPCManifest -> Transport -> Map Text Value -> RPCConnection
newRPCConnection manifest transport params = RPCConnection
  { rpcManifest  = manifest
  , rpcTransport = transport
  , rpcParams    = params
  }

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Send an envelope and wait for a response envelope.
rpcCall :: Transport -> RPCEnvelope -> IO (Either RPCError RPCEnvelope)
rpcCall transport envelope = do
  let encoded = encodeMessage envelope
  sendResult <- sendMessage transport encoded
  case sendResult of
    Left err -> pure (Left (RPCTransportError err))
    Right () -> do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> pure (Left (RPCTransportError err))
        Right bs -> case decodeMessage bs of
          Left err  -> pure (Left (RPCProtocolError err))
          Right env -> pure (Right env)

-- | Send an envelope, collecting progress\/log messages until a
-- final result envelope arrives.  Returns the final envelope.
rpcCallWithProgress
  :: Transport
  -> RPCEnvelope
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError RPCEnvelope)
rpcCallWithProgress transport envelope onProgress onLog = do
  let encoded = encodeMessage envelope
  sendResult <- sendMessage transport encoded
  case sendResult of
    Left err -> pure (Left (RPCTransportError err))
    Right () -> recvLoop
  where
    recvLoop = do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> pure (Left (RPCTransportError err))
        Right bs -> case decodeMessage bs of
          Left err  -> pure (Left (RPCProtocolError err))
          Right env -> case envType env of
            MsgProgress -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success prog -> onProgress prog
                Aeson.Error _      -> pure ()
              recvLoop
            MsgLog -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success logMsg -> onLog logMsg
                Aeson.Error _        -> pure ()
              recvLoop
            MsgError -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success (Topo.Plugin.RPC.Protocol.PluginError code msg) ->
                  pure (Left (RPCPluginError code msg))
                Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))
            _ -> pure (Right env)

------------------------------------------------------------------------
-- Invocation
------------------------------------------------------------------------

-- | Invoke the plugin's generator stage.
--
-- Sends an @invoke_generator@ message with terrain data and config,
-- collects progress updates, and returns the generator result.
invokeGenerator
  :: RPCConnection
  -> Value
  -- ^ Encoded terrain data (relevant chunks).
  -> IO (Either RPCError GeneratorResult)
invokeGenerator conn terrainData = do
  let manifest = rpcManifest conn
      envelope = RPCEnvelope
        { envType = MsgInvokeGenerator
        , envPayload = Aeson.toJSON InvokeGenerator
            { igStageId = "plugin:" <> rmName manifest
            , igSeed    = 0  -- Seed is set by the pipeline at call time
            , igConfig  = rpcParams conn
            , igTerrain = terrainData
            }
        }
  result <- rpcCallWithProgress (rpcTransport conn) envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success gr -> pure (Right gr)
        Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Invoke the plugin's simulation tick.
--
-- Sends an @invoke_simulation@ message with context and overlay data.
invokeSimulation
  :: RPCConnection
  -> SimContext
  -> Overlay
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError SimulationResult)
invokeSimulation conn _ctx _overlay onProgress onLog = do
  let manifest = rpcManifest conn
      envelope = RPCEnvelope
        { envType = MsgInvokeSimulation
        , envPayload = Aeson.toJSON InvokeSimulation
            { isNodeId     = rmName manifest
            , isWorldTime  = 0  -- Populated from SimContext at call time
            , isDeltaTicks = 0
            , isCalendar   = Null
            , isConfig     = rpcParams conn
            , isTerrain    = Null  -- Encoded by caller
            , isOverlays   = Null  -- Encoded by caller
            , isOwnOverlay = Null  -- Encoded by caller
            }
        }
  result <- rpcCallWithProgress (rpcTransport conn) envelope onProgress onLog
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success sr -> pure (Right sr)
        Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Send a shutdown message to the plugin.
rpcShutdown :: RPCConnection -> IO ()
rpcShutdown conn = do
  let envelope = RPCEnvelope
        { envType = MsgShutdown
        , envPayload = object []
        }
  _ <- sendMessage (rpcTransport conn) (encodeMessage envelope)
  pure ()

------------------------------------------------------------------------
-- Pipeline / DAG integration
------------------------------------------------------------------------

-- | Create a 'PipelineStage' from an RPC connection.
--
-- The stage's action sends terrain data to the plugin, receives
-- modified chunks back, and merges them into the world.
rpcGeneratorStage :: RPCConnection -> PipelineStage
rpcGeneratorStage conn =
  let manifest = rpcManifest conn
  in PipelineStage
    { stageId   = StagePlugin (rmName manifest)
    , stageName = rmName manifest
    , stageSeedTag = "plugin:" <> rmName manifest
    , stageRun  = do
        logInfo ("plugin:" <> rmName manifest <> ": invoking generator")
        _world <- getWorldP
        -- TODO (Phase 7): Encode relevant terrain chunks for the plugin,
        -- invoke the generator, decode the result, and merge modified
        -- chunks back into the world.  For now, this is a stub that
        -- logs the invocation.
        logInfo ("plugin:" <> rmName manifest <> ": generator complete")
    }

-- | Create a 'SimNode' from an RPC connection.
--
-- Dispatches to 'SimNodeReader' or 'SimNodeWriter' based on
-- whether the manifest declares @writeTerrain@ capability.
rpcSimNode :: RPCConnection -> SimNode
rpcSimNode conn =
  let manifest = rpcManifest conn
      nodeId   = SimNodeId (rmName manifest)
      name     = rmName manifest
      deps     = case rmSimulation manifest of
        Just sd -> map SimNodeId (rsdDependencies sd)
        Nothing -> []
  in if manifestWritesTerrain manifest
    then SimNodeWriter
      { snwId           = nodeId
      , snwOverlayName  = name
      , snwDependencies = deps
      , snwWriteTick    = \_ctx overlay -> do
          -- TODO (Phase 7): Encode context, send to plugin, decode
          -- result including terrain writes.
          pure (Right (overlay, emptyWrites))
      }
    else SimNodeReader
      { snrId           = nodeId
      , snrOverlayName  = name
      , snrDependencies = deps
      , snrReadTick     = \_ctx overlay -> do
          -- TODO (Phase 7): Encode context, send to plugin, decode
          -- updated overlay.
          pure (Right overlay)
      }
  where
    emptyWrites = Topo.Simulation.emptyTerrainWrites
