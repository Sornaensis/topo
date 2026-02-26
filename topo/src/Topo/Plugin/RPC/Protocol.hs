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
    -- * Plugin → Host messages
  , PluginProgress(..)
  , PluginLog(..)
  , GeneratorResult(..)
  , SimulationResult(..)
  , PluginError(..)
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
  { igStageId :: !Text
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
    InvokeGenerator
      <$> o .: "stage_id"
      <*> o .: "seed"
      <*> o .: "config"
      <*> o .: "terrain"

instance ToJSON InvokeGenerator where
  toJSON ig = object
    [ "stage_id" .= igStageId ig
    , "seed"     .= igSeed ig
    , "config"   .= igConfig ig
    , "terrain"  .= igTerrain ig
    ]

-- | Invoke a plugin's simulation tick.
data InvokeSimulation = InvokeSimulation
  { isNodeId     :: !Text
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
    InvokeSimulation
      <$> o .: "node_id"
      <*> o .: "world_time"
      <*> o .: "delta_ticks"
      <*> o .: "calendar"
      <*> o .: "config"
      <*> o .: "terrain"
      <*> o .: "overlays"
      <*> o .: "own_overlay"

instance ToJSON InvokeSimulation where
  toJSON is' = object
    [ "node_id"     .= isNodeId is'
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
