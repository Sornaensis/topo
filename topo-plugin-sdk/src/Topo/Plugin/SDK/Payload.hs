{-# LANGUAGE OverloadedStrings #-}

-- | Typed payload helpers for plugin callbacks.
--
-- These functions provide ergonomic, structured conversions between
-- SDK callback context payloads and domain types used by plugins.
module Topo.Plugin.SDK.Payload
  ( -- * Overlay payloads
    decodeOwnOverlay
  , decodeDependencyOverlay
  , encodeOverlayPayload
    -- * Terrain payloads
  , decodeTerrainPayload
  , encodeTerrainPayload
  , decodeTerrainWritesPayload
  , encodeTerrainWritesPayload
    -- * Typed result constructors
  , simulationResultFromOverlay
  , simulationResultWithTerrainWrites
  , generatorResultFromTerrain
  , generatorResultFromTerrainAndOverlay
  ) where

import Data.Aeson (Value(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay)
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Plugin.RPC
  ( applyGeneratorTerrainValue
  , decodeTerrainWritesValue
  , terrainWorldToPayload
  )
import Topo.Simulation (TerrainWrites, applyTerrainWrites)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld, emptyWorld)

import Topo.Plugin.SDK.Types
  ( GeneratorTickResult(..)
  , PluginContext(..)
  , SimulationTickResult(..)
  , defaultGeneratorTickResult
  , defaultSimulationTickResult
  )

-- | Decode the plugin-owned overlay payload from a simulation context.
--
-- Returns a structured error when the payload is absent or fails
-- schema validation.
decodeOwnOverlay :: OverlaySchema -> PluginContext -> Either Text Overlay
decodeOwnOverlay schema context =
  case pcOwnOverlay context of
    Nothing -> Left "simulation context did not include own overlay payload"
    Just rawOverlay -> overlayFromJSON schema rawOverlay

-- | Decode a dependency overlay payload from simulation context.
--
-- The overlay is selected by dependency name from 'pcOverlays'.
decodeDependencyOverlay :: OverlaySchema -> Text -> PluginContext -> Either Text Overlay
decodeDependencyOverlay schema overlayName context =
  case Map.lookup overlayName (pcOverlays context) of
    Nothing -> Left ("simulation context missing dependency overlay: " <> overlayName)
    Just rawOverlay -> overlayFromJSON schema rawOverlay

-- | Encode an overlay into protocol JSON payload shape.
encodeOverlayPayload :: Overlay -> Value
encodeOverlayPayload = overlayToJSON

-- | Decode host terrain payload into a world value.
--
-- This accepts the same terrain payload shape used by host RPC
-- invocations and generator results.
decodeTerrainPayload :: Value -> Either Text TerrainWorld
decodeTerrainPayload payload =
  applyGeneratorTerrainValue baseWorld payload
  where
    baseWorld = emptyWorld (WorldConfig { wcChunkSize = defaultChunkSize }) defaultHexGridMeta
    defaultChunkSize = 64

-- | Encode a terrain world into protocol terrain payload shape.
encodeTerrainPayload :: TerrainWorld -> Either Text Value
encodeTerrainPayload = terrainWorldToPayload

-- | Decode simulation terrain writes payload into structured writes.
decodeTerrainWritesPayload :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesPayload = decodeTerrainWritesValue

-- | Encode structured terrain writes into protocol payload shape.
--
-- The payload is encoded using the same chunk representation as full
-- terrain payloads.
encodeTerrainWritesPayload :: TerrainWrites -> Either Text Value
encodeTerrainWritesPayload writes =
  terrainWorldToPayload writesWorld
  where
    baseWorld = emptyWorld (WorldConfig { wcChunkSize = defaultChunkSize }) defaultHexGridMeta
    defaultChunkSize = 64
    writesWorld = applyTerrainWrites writes baseWorld

-- | Build a simulation result from an updated overlay.
simulationResultFromOverlay :: Overlay -> SimulationTickResult
simulationResultFromOverlay overlay =
  defaultSimulationTickResult
    { strOverlay = encodeOverlayPayload overlay
    }

-- | Build a simulation result from updated overlay and terrain writes.
simulationResultWithTerrainWrites
  :: Overlay
  -> TerrainWrites
  -> Either Text SimulationTickResult
simulationResultWithTerrainWrites overlay writes = do
  encodedWrites <- encodeTerrainWritesPayload writes
  Right defaultSimulationTickResult
    { strOverlay = encodeOverlayPayload overlay
    , strTerrainWrites = Just encodedWrites
    }

-- | Build a generator result from updated terrain payload.
generatorResultFromTerrain
  :: TerrainWorld
  -> Either Text GeneratorTickResult
generatorResultFromTerrain world = do
  terrainPayload <- encodeTerrainPayload world
  Right defaultGeneratorTickResult
    { gtrTerrain = terrainPayload
    }

-- | Build a generator result from updated terrain and overlay payloads.
generatorResultFromTerrainAndOverlay
  :: TerrainWorld
  -> Overlay
  -> Either Text GeneratorTickResult
generatorResultFromTerrainAndOverlay world overlay = do
  terrainPayload <- encodeTerrainPayload world
  Right defaultGeneratorTickResult
    { gtrTerrain = terrainPayload
    , gtrOverlay = Just (encodeOverlayPayload overlay)
    }
