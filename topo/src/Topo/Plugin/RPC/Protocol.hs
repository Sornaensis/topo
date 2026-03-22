{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | RPC message protocol for host↔plugin communication.
--
-- All messages are JSON-encoded and framed by the transport layer
-- (length-prefixed).  This module defines the message types and
-- their JSON codecs.
--
-- = Message flow
--
-- @
-- Host → Plugin:  invoke_generator, invoke_simulation, shutdown
-- Plugin → Host:  progress, log, generator_result, simulation_result, error
-- @
module Topo.Plugin.RPC.Protocol
  ( -- * Message envelope
    RPCMessageType(..)
  , RPCEnvelope(..)
    -- * Host → Plugin messages
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , Handshake(..)
  , WorldChanged(..)
    -- * Plugin → Host messages
  , PluginProgress(..)
  , PluginLog(..)
  , GeneratorResult(..)
  , SimulationResult(..)
  , PluginError(..)
  , HandshakeAck(..)
    -- * Protocol version
  , currentProtocolVersion
    -- * Encoding / decoding
  , encodeMessage
  , decodeMessage
    -- * Log levels
  , PluginLogLevel(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  , withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)

import Topo.Plugin.DataResource (DataResourceSchema)

------------------------------------------------------------------------
-- Message type tags
------------------------------------------------------------------------

-- | Discriminator tag for RPC messages.
data RPCMessageType
  = MsgInvokeGenerator
  | MsgInvokeSimulation
  | MsgShutdown
  | MsgProgress
  | MsgLog
  | MsgGeneratorResult
  | MsgSimulationResult
  | MsgError
  | MsgHandshake
  | MsgHandshakeAck
  | MsgWorldChanged
  | MsgQueryResource
  | MsgQueryResult
  | MsgMutateResource
  | MsgMutateResult
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCMessageType where
  parseJSON = withText "RPCMessageType" $ \t -> case t of
    "invoke_generator"  -> pure MsgInvokeGenerator
    "invoke_simulation" -> pure MsgInvokeSimulation
    "shutdown"          -> pure MsgShutdown
    "progress"          -> pure MsgProgress
    "log"               -> pure MsgLog
    "generator_result"  -> pure MsgGeneratorResult
    "simulation_result" -> pure MsgSimulationResult
    "error"             -> pure MsgError
    "handshake"         -> pure MsgHandshake
    "handshake_ack"     -> pure MsgHandshakeAck
    "world_changed"     -> pure MsgWorldChanged
    "query_resource"    -> pure MsgQueryResource
    "query_result"      -> pure MsgQueryResult
    "mutate_resource"   -> pure MsgMutateResource
    "mutate_result"     -> pure MsgMutateResult
    _                   -> fail ("unknown message type: " <> Text.unpack t)

instance ToJSON RPCMessageType where
  toJSON MsgInvokeGenerator  = "invoke_generator"
  toJSON MsgInvokeSimulation = "invoke_simulation"
  toJSON MsgShutdown         = "shutdown"
  toJSON MsgProgress         = "progress"
  toJSON MsgLog              = "log"
  toJSON MsgGeneratorResult  = "generator_result"
  toJSON MsgSimulationResult = "simulation_result"
  toJSON MsgError            = "error"
  toJSON MsgHandshake        = "handshake"
  toJSON MsgHandshakeAck     = "handshake_ack"
  toJSON MsgWorldChanged     = "world_changed"
  toJSON MsgQueryResource    = "query_resource"
  toJSON MsgQueryResult      = "query_result"
  toJSON MsgMutateResource   = "mutate_resource"
  toJSON MsgMutateResult     = "mutate_result"

------------------------------------------------------------------------
-- Envelope
------------------------------------------------------------------------

-- | JSON envelope wrapping every RPC message.
--
-- @
-- { "type": "invoke_generator", "payload": { ... } }
-- @
data RPCEnvelope = RPCEnvelope
  { envType    :: !RPCMessageType
    -- ^ Message discriminator.
  , envPayload :: !Value
    -- ^ Message-specific payload (JSON object).
  } deriving (Eq, Show, Generic)

instance FromJSON RPCEnvelope where
  parseJSON = withObject "RPCEnvelope" $ \o ->
    RPCEnvelope
      <$> o .: "type"
      <*> o .: "payload"

instance ToJSON RPCEnvelope where
  toJSON env = object
    [ "type"    .= envType env
    , "payload" .= envPayload env
    ]

------------------------------------------------------------------------
-- Host → Plugin messages
------------------------------------------------------------------------

-- | Invoke a plugin's generator stage.
data InvokeGenerator = InvokeGenerator
  { igPayloadVersion :: !Int
    -- ^ RPC payload contract version. Currently must be @1@.
  , igStageId :: !Text
    -- ^ Canonical stage ID for this plugin.
  , igSeed    :: !Word64
    -- ^ Generation seed.
  , igConfig  :: !(Map Text Value)
    -- ^ Plugin parameter values.
  , igTerrain :: !Value
    -- ^ Terrain chunk data (encoded by 'Topo.Export').
  } deriving (Eq, Show, Generic)

instance FromJSON InvokeGenerator where
  parseJSON = withObject "InvokeGenerator" $ \o ->
    do
      payloadVersion <- o .: "payload_version"
      if payloadVersion /= (1 :: Int)
        then fail ("unsupported payload_version: " <> show payloadVersion)
        else InvokeGenerator
          <$> pure payloadVersion
          <*> o .: "stage_id"
          <*> o .: "seed"
          <*> o .: "config"
          <*> o .: "terrain"

instance ToJSON InvokeGenerator where
  toJSON ig = object
    [ "payload_version" .= igPayloadVersion ig
    , "stage_id" .= igStageId ig
    , "seed"     .= igSeed ig
    , "config"   .= igConfig ig
    , "terrain"  .= igTerrain ig
    ]

-- | Invoke a plugin's simulation tick.
data InvokeSimulation = InvokeSimulation
  { isPayloadVersion :: !Int
    -- ^ RPC payload contract version. Currently must be @1@.
  , isNodeId     :: !Text
    -- ^ Simulation node ID.
  , isWorldTime  :: !Word64
    -- ^ Current tick counter.
  , isDeltaTicks :: !Word64
    -- ^ Ticks since last simulation run.
  , isCalendar   :: !Value
    -- ^ Calendar date (JSON object with year, dayOfYear, hourOfDay).
  , isConfig     :: !(Map Text Value)
    -- ^ Plugin parameter values.
  , isTerrain    :: !Value
    -- ^ Read-only terrain data.
  , isOverlays   :: !Value
    -- ^ Read-only dependency overlay data.
  , isOwnOverlay :: !Value
    -- ^ Current state of the plugin's own overlay.
  } deriving (Eq, Show, Generic)

instance FromJSON InvokeSimulation where
  parseJSON = withObject "InvokeSimulation" $ \o ->
    do
      payloadVersion <- o .: "payload_version"
      if payloadVersion /= (1 :: Int)
        then fail ("unsupported payload_version: " <> show payloadVersion)
        else InvokeSimulation
          <$> pure payloadVersion
          <*> o .: "node_id"
          <*> o .: "world_time"
          <*> o .: "delta_ticks"
          <*> o .: "calendar"
          <*> o .: "config"
          <*> o .: "terrain"
          <*> o .: "overlays"
          <*> o .: "own_overlay"

instance ToJSON InvokeSimulation where
  toJSON is' = object
    [ "payload_version" .= isPayloadVersion is'
    , "node_id"     .= isNodeId is'
    , "world_time"  .= isWorldTime is'
    , "delta_ticks" .= isDeltaTicks is'
    , "calendar"    .= isCalendar is'
    , "config"      .= isConfig is'
    , "terrain"     .= isTerrain is'
    , "overlays"    .= isOverlays is'
    , "own_overlay" .= isOwnOverlay is'
    ]

------------------------------------------------------------------------
-- Plugin → Host messages
------------------------------------------------------------------------

-- | Progress update from a plugin.
data PluginProgress = PluginProgress
  { ppMessage  :: !Text
    -- ^ Human-readable progress description.
  , ppFraction :: !Double
    -- ^ Progress fraction (0.0 to 1.0).
  } deriving (Eq, Show, Generic)

instance FromJSON PluginProgress where
  parseJSON = withObject "PluginProgress" $ \o ->
    PluginProgress
      <$> o .: "message"
      <*> o .: "fraction"

instance ToJSON PluginProgress where
  toJSON pp = object
    [ "message"  .= ppMessage pp
    , "fraction" .= ppFraction pp
    ]

-- | Log level from a plugin.
data PluginLogLevel
  = PluginLogDebug
  | PluginLogInfo
  | PluginLogWarn
  | PluginLogError
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON PluginLogLevel where
  parseJSON = withText "PluginLogLevel" $ \t -> case t of
    "debug" -> pure PluginLogDebug
    "info"  -> pure PluginLogInfo
    "warn"  -> pure PluginLogWarn
    "error" -> pure PluginLogError
    _       -> fail ("unknown log level: " <> Text.unpack t)

instance ToJSON PluginLogLevel where
  toJSON PluginLogDebug = "debug"
  toJSON PluginLogInfo  = "info"
  toJSON PluginLogWarn  = "warn"
  toJSON PluginLogError = "error"

-- | Log message from a plugin.
data PluginLog = PluginLog
  { plLevel   :: !PluginLogLevel
    -- ^ Severity level.
  , plMessage :: !Text
    -- ^ Log message text.
  } deriving (Eq, Show, Generic)

instance FromJSON PluginLog where
  parseJSON = withObject "PluginLog" $ \o ->
    PluginLog
      <$> o .: "level"
      <*> o .: "message"

instance ToJSON PluginLog where
  toJSON pl = object
    [ "level"   .= plLevel pl
    , "message" .= plMessage pl
    ]

-- | Result from a generator plugin invocation.
data GeneratorResult = GeneratorResult
  { grTerrain  :: !Value
    -- ^ Modified terrain chunks.
  , grOverlay  :: !(Maybe Value)
    -- ^ Overlay data to seed (if the plugin owns an overlay).
  , grMetadata :: !(Maybe Value)
    -- ^ Optional metadata.
  } deriving (Eq, Show, Generic)

instance FromJSON GeneratorResult where
  parseJSON = withObject "GeneratorResult" $ \o ->
    GeneratorResult
      <$> o .: "terrain"
      <*> o .:? "overlay"
      <*> o .:? "metadata"

instance ToJSON GeneratorResult where
  toJSON gr = object $
    [ "terrain" .= grTerrain gr ] <>
    [ "overlay"  .= ov | Just ov <- [grOverlay gr] ] <>
    [ "metadata" .= md | Just md <- [grMetadata gr] ]

-- | Result from a simulation plugin tick.
data SimulationResult = SimulationResult
  { srOverlay       :: !Value
    -- ^ Updated overlay data.
  , srTerrainWrites :: !(Maybe Value)
    -- ^ Terrain mutations (only for 'SimNodeWriter' plugins).
  } deriving (Eq, Show, Generic)

instance FromJSON SimulationResult where
  parseJSON = withObject "SimulationResult" $ \o ->
    SimulationResult
      <$> o .: "overlay"
      <*> o .:? "terrain_writes"

instance ToJSON SimulationResult where
  toJSON sr = object $
    [ "overlay" .= srOverlay sr ] <>
    [ "terrain_writes" .= tw | Just tw <- [srTerrainWrites sr] ]

-- | Error reported by a plugin.
data PluginError = PluginError
  { peCode    :: !Int
    -- ^ Error code (plugin-defined).
  , peMessage :: !Text
    -- ^ Human-readable error description.
  } deriving (Eq, Show, Generic)

instance FromJSON PluginError where
  parseJSON = withObject "PluginError" $ \o ->
    PluginError
      <$> o .: "code"
      <*> o .: "message"

instance ToJSON PluginError where
  toJSON pe = object
    [ "code"    .= peCode pe
    , "message" .= peMessage pe
    ]

------------------------------------------------------------------------
-- Protocol version
------------------------------------------------------------------------

-- | Current protocol version.
--
-- Version 2 introduces the handshake exchange, world-changed
-- notifications, and data-service CRUD messages.
currentProtocolVersion :: Int
currentProtocolVersion = 2

------------------------------------------------------------------------
-- Handshake messages
------------------------------------------------------------------------

-- | Handshake message sent by the host when a plugin connects.
--
-- Provides the world save path (if a world is loaded) and tells the
-- plugin which host-side data-service capabilities are available.
data Handshake = Handshake
  { hsProtocolVersion  :: !Int
    -- ^ Protocol version the host speaks.
  , hsWorldPath        :: !(Maybe Text)
    -- ^ Absolute path to the current world save directory, or
    --   'Nothing' if no world is loaded yet.
  , hsHostCapabilities :: ![Text]
    -- ^ Host-side capabilities (e.g. @\"query\"@, @\"mutate\"@).
  } deriving (Eq, Show, Generic)

instance FromJSON Handshake where
  parseJSON = withObject "Handshake" $ \o ->
    Handshake
      <$> o .: "protocol_version"
      <*> o .:? "world_path"
      <*> (o .:? "host_capabilities" >>= pure . maybe [] id)

instance ToJSON Handshake where
  toJSON hs = object
    [ "protocol_version"  .= hsProtocolVersion hs
    , "world_path"        .= hsWorldPath hs
    , "host_capabilities" .= hsHostCapabilities hs
    ]

-- | Handshake acknowledgement sent by the plugin in response to
-- 'Handshake'.
--
-- Declares the plugin's data directory (relative to the world save)
-- and any data resource schemas the plugin manages.
data HandshakeAck = HandshakeAck
  { haProtocolVersion :: !Int
    -- ^ Protocol version the plugin speaks.
  , haDataDirectory   :: !(Maybe Text)
    -- ^ Data subdirectory relative to world path, or 'Nothing'.
  , haResources       :: ![DataResourceSchema]
    -- ^ Data resource schemas the plugin manages.
  } deriving (Eq, Show, Generic)

instance FromJSON HandshakeAck where
  parseJSON = withObject "HandshakeAck" $ \o ->
    HandshakeAck
      <$> o .: "protocol_version"
      <*> o .:? "data_directory"
      <*> (o .:? "resources" >>= pure . maybe [] id)

instance ToJSON HandshakeAck where
  toJSON ha = object
    [ "protocol_version" .= haProtocolVersion ha
    , "data_directory"   .= haDataDirectory ha
    , "resources"        .= haResources ha
    ]

-- | Notification from the host that the world save path has changed.
--
-- Sent after a world is loaded, saved to a new location, or created.
-- Plugins that manage data directories should re-resolve their paths.
data WorldChanged = WorldChanged
  { wchWorldPath :: !(Maybe Text)
    -- ^ New world save path, or 'Nothing' if the world was unloaded.
  } deriving (Eq, Show, Generic)

instance FromJSON WorldChanged where
  parseJSON = withObject "WorldChanged" $ \o ->
    WorldChanged <$> o .:? "world_path"

instance ToJSON WorldChanged where
  toJSON wc = object
    [ "world_path" .= wchWorldPath wc
    ]

------------------------------------------------------------------------
-- Encoding / decoding
------------------------------------------------------------------------

-- | Encode an RPC message envelope to bytes (for transport).
encodeMessage :: RPCEnvelope -> BS.ByteString
encodeMessage = BL.toStrict . Aeson.encode

-- | Decode an RPC message envelope from bytes.
decodeMessage :: BS.ByteString -> Either Text RPCEnvelope
decodeMessage bs =
  case Aeson.eitherDecodeStrict' bs of
    Left err  -> Left (Text.pack err)
    Right env -> Right env
