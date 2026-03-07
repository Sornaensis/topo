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
    -- * Terrain payload helpers
  , terrainWorldToPayload
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
  , encodeBase64Text
  , decodeBase64Text
    -- * Errors
  , RPCError(..)
    -- * Re-exports
  , module Topo.Plugin.RPC.Manifest
  , module Topo.Plugin.RPC.Transport
  , module Topo.Plugin.RPC.Protocol
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Topo.Overlay (Overlay(..), insertOverlay, lookupOverlay)
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (Capability(..), PluginError(..), PluginM, getWorldP, logInfo, putWorldP)
import Topo.Calendar (CalendarDate(..), WorldTime(..))
import Topo.Simulation
  ( SimNode(..)
  , SimNodeId(..)
  , SimContext(..)
  , TerrainWrites(..)
  , applyTerrainWrites
  , emptyTerrainWrites
  )
import qualified Topo.Types
import qualified Topo.World

import Topo.Plugin.RPC.Manifest
import qualified Topo.Plugin.RPC.Payload as Payload
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
          { igPayloadVersion = 1
          , igStageId = "plugin:" <> rmName manifest
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
invokeSimulation conn ctx overlay onProgress onLog = do
  let manifest = rpcManifest conn
      policy = simulationPayloadPolicy manifest
      terrainPayloadResult
        | sppIncludeTerrain policy = Payload.terrainWorldToPayload (scTerrain ctx)
        | otherwise = Right Null
      overlaysPayload
        | sppIncludeOverlays policy = overlaysToJSON (scOverlays ctx)
        | otherwise = Object mempty
      ownOverlayPayload
        | sppIncludeOwnOverlay policy = overlayToJSON overlay
        | otherwise = Null
  case terrainPayloadResult of
    Left err -> pure (Left (RPCProtocolError err))
    Right terrainPayload -> do
      let envelope = RPCEnvelope
            { envType = MsgInvokeSimulation
            , envPayload = Aeson.toJSON InvokeSimulation
              { isPayloadVersion = 1
              , isNodeId     = rmName manifest
                , isWorldTime  = wtTick (scWorldTime ctx)
                , isDeltaTicks = scDeltaTicks ctx
                , isCalendar   = calendarToJSON (scCalendar ctx)
                , isConfig     = rpcParams conn
                , isTerrain    = terrainPayload
                , isOverlays   = overlaysPayload
                , isOwnOverlay = ownOverlayPayload
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
    , stageOverlayProduces = if manifestHasOverlay manifest then Just (rmName manifest) else Nothing
    , stageOverlayReads = []
    , stageOverlaySchema = Nothing
    , stageRun  = do
        logInfo ("plugin:" <> rmName manifest <> ": invoking generator")
        world <- getWorldP
        case Payload.terrainWorldToPayload world of
          Left err ->
            throwError (PluginInvariantError ("rpc generator encode failed: " <> err))
          Right terrainPayload -> do
            result <- liftIO (invokeGenerator conn terrainPayload)
            case result of
              Left err -> throwError (PluginInvariantError ("rpc generator failed: " <> rpcErrorText err))
              Right generatorResult ->
                case applyGeneratorResult manifest world generatorResult of
                  Left mergeErr ->
                    throwError (PluginInvariantError ("rpc generator merge failed: " <> mergeErr))
                  Right mergedWorld -> do
                    putWorldP mergedWorld
                    logInfo ("plugin:" <> rmName manifest <> ": generator complete")
    }

-- | Create a 'SimNode' from an RPC connection.
--
-- Dispatches to 'SimNodeReader' or 'SimNodeWriter' based on
-- whether the manifest declares @writeTerrain@ capability.
rpcSimNode :: RPCConnection -> SimNode
rpcSimNode conn =
  let manifest = rpcManifest conn
      policy   = simulationPayloadPolicy manifest
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
      , snwWriteTick    = \ctx overlay -> do
          if not (sppRequireWriteOverlay policy)
            then pure (Left "manifest missing writeOverlay capability")
            else do
              result <- invokeSimulation conn ctx overlay ignoreProgress ignoreLog
              case result of
                Left err -> pure (Left (rpcErrorText err))
                Right sr -> do
                  let decodedOverlay = overlayFromJSON (ovSchema overlay) (srOverlay sr)
                      decodedWrites = Payload.decodeTerrainWritesValue (srTerrainWrites sr)
                  pure $ case (decodedOverlay, decodedWrites) of
                    (Right nextOverlay, Right writes) -> Right (nextOverlay, writes)
                    (Left overlayErr, _) -> Left overlayErr
                    (_, Left writesErr) -> Left writesErr
      }
    else SimNodeReader
      { snrId           = nodeId
      , snrOverlayName  = name
      , snrDependencies = deps
      , snrReadTick     = \ctx overlay -> do
          if not (sppRequireWriteOverlay policy)
            then pure (Left "manifest missing writeOverlay capability")
            else do
              result <- invokeSimulation conn ctx overlay ignoreProgress ignoreLog
              pure $ case result of
                Left err -> Left (rpcErrorText err)
                Right sr -> overlayFromJSON (ovSchema overlay) (srOverlay sr)
      }
  where
    ignoreProgress _ = pure ()
    ignoreLog _ = pure ()

hasCapability :: RPCManifest -> Capability -> Bool
hasCapability manifest capability = capability `elem` rmCapabilities manifest

canReadTerrain :: RPCManifest -> Bool
canReadTerrain manifest =
  hasCapability manifest CapReadTerrain || hasCapability manifest CapReadWorld

canReadOverlay :: RPCManifest -> Bool
canReadOverlay manifest =
  hasCapability manifest CapReadOverlay || hasCapability manifest CapReadWorld

canWriteOverlay :: RPCManifest -> Bool
canWriteOverlay manifest =
  hasCapability manifest CapWriteOverlay || hasCapability manifest CapWriteWorld

data SimulationPayloadPolicy = SimulationPayloadPolicy
  { sppIncludeTerrain :: !Bool
  , sppIncludeOverlays :: !Bool
  , sppIncludeOwnOverlay :: !Bool
  , sppRequireWriteOverlay :: !Bool
  }

simulationPayloadPolicy :: RPCManifest -> SimulationPayloadPolicy
simulationPayloadPolicy manifest =
  SimulationPayloadPolicy
    { sppIncludeTerrain = canReadTerrain manifest
    , sppIncludeOverlays = canReadOverlay manifest
    , sppIncludeOwnOverlay = canReadOverlay manifest || canWriteOverlay manifest
    , sppRequireWriteOverlay = canWriteOverlay manifest
    }

calendarToJSON :: CalendarDate -> Value
calendarToJSON calendarDate = object
  [ "year" .= cdYear calendarDate
  , "dayOfYear" .= cdDayOfYear calendarDate
  , "hourOfDay" .= cdHourOfDay calendarDate
  ]

overlaysToJSON :: Map Text Overlay -> Value
overlaysToJSON overlays =
  object [ Key.fromText name .= overlayToJSON overlay | (name, overlay) <- Map.toList overlays ]

rpcErrorText :: RPCError -> Text
rpcErrorText rpcError =
  case rpcError of
    RPCTransportError transportError -> Text.pack (show transportError)
    RPCProtocolError message -> message
    RPCPluginError code message -> "plugin error " <> Text.pack (show code) <> ": " <> message
    RPCTimeout message -> "timeout: " <> message

applyGeneratorResult
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> GeneratorResult
  -> Either Text Topo.World.TerrainWorld
applyGeneratorResult manifest world result = do
  worldWithTerrain <- Payload.applyGeneratorTerrainValue world (grTerrain result)
  applyGeneratorOverlayPayload manifest worldWithTerrain (grOverlay result)

terrainWorldToPayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayload = Payload.terrainWorldToPayload

decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = Payload.decodeTerrainWritesValue

applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue = Payload.applyGeneratorTerrainValue

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text = Payload.encodeBase64Text

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text = Payload.decodeBase64Text

applyGeneratorOverlayPayload
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> Maybe Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorOverlayPayload _ world Nothing = Right world
applyGeneratorOverlayPayload manifest world (Just overlayValue) =
  case lookupOverlay (rmName manifest) (Topo.World.twOverlays world) of
    Nothing -> Left "generator returned overlay data but no host overlay is registered"
    Just existingOverlay -> do
      decodedOverlay <- overlayFromJSON (ovSchema existingOverlay) overlayValue
      let nextOverlays = insertOverlay decodedOverlay (Topo.World.twOverlays world)
      Right world { Topo.World.twOverlays = nextOverlays }
