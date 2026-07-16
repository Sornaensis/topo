{-# LANGUAGE OverloadedStrings #-}

-- | Typed payload helpers for plugin callbacks.
--
-- These functions provide ergonomic, structured conversions between
-- SDK callback context payloads and domain types used by plugins.
module Topo.Plugin.SDK.Payload
  ( -- * Overlay payloads
    decodeOwnOverlay
  , decodeDependencyOverlay
  , decodeScopedOwnOverlay
  , decodeScopedDependencyOverlay
  , encodeOverlayPayload
    -- * Terrain payloads
  , decodeTerrainPayload
  , decodeTerrainPayloadWithLimits
  , encodeTerrainPayload
  , encodeTerrainPayloadWithLimits
  , decodeTerrainWritesPayload
  , decodeTerrainWritesPayloadWithLimits
  , encodeTerrainWritesPayload
  , encodeTerrainWritesPayloadWithLimits
    -- * Streaming terrain
  , foldTerrainChunks
  , terrainChunkSourceFromRecords
  , terrainChunkSourceFromFiles
  , terrainChunkSourceToRecords
  , decodeTerrainSnapshotHeader
  , terrainChunkRecordsToPayload
  , terrainChunkUpdatesFromWorld
  , terrainWritesToChunkUpdates
  , writeTerrainWritesToSink
  , diffTerrainWorldAgainstSnapshot
    -- * Typed result constructors
  , simulationResultFromOverlay
  , simulationResultWithTerrainWrites
  , simulationResultWithTerrainWritesWithLimits
  , generatorResultFromTerrain
  , generatorResultFromScopedTerrain
  , generatorResultFromTerrainWithLimits
  , generatorResultFromTerrainAndOverlay
  , generatorResultFromTerrainAndOverlayWithLimits
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value(..), (.:), (.=), object, withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Export
  ( ExportError
  , decodeClimateChunk
  , decodeTerrainChunk
  , decodeVegetationChunk
  , encodeClimateChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  )
import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay)
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Plugin.RPC
  ( RPCPayloadLimits
  , applyGeneratorTerrainValue
  , applyGeneratorTerrainValueWithLimits
  , decodeTerrainWritesValue
  , decodeTerrainWritesValueWithLimits
  , terrainWorldToPayload
  , terrainWorldToPayloadWithLimits
  , terrainWorldToScopedPayload
  , encodeBase64Text
  )
import Topo.Plugin.RPC.Scope (ResolvedInvocationScope(..))
import Topo.Plugin.RPC.Scope (TerrainSection(..))
import Topo.Simulation (TerrainWrites(..), applyTerrainWrites)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorld)

import Topo.Plugin.SDK.Types
  ( GeneratorContext(..)
  , GeneratorTickResult(..)
  , PluginContext(..)
  , SimulationContext(..)
  , SimulationTickResult(..)
  , TerrainChunkRecord(..)
  , TerrainChunkRemoval(..)
  , TerrainChunkSource(..)
  , TerrainChunkUpdate(..)
  , TerrainDeltaSink(..)
  , TerrainSnapshotHeader(..)
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

-- | Decode the owned overlay only when the resolved simulation scope supplied it.
decodeScopedOwnOverlay :: OverlaySchema -> SimulationContext -> Either Text Overlay
decodeScopedOwnOverlay schema context =
  case scOwnOverlay context of
    Nothing -> Left "resolved simulation scope did not grant own-overlay input"
    Just rawOverlay -> overlayFromJSON schema rawOverlay

-- | Decode one dependency overlay from the exact resolved subset.
decodeScopedDependencyOverlay :: OverlaySchema -> Text -> SimulationContext -> Either Text Overlay
decodeScopedDependencyOverlay schema overlayName context =
  case Map.lookup overlayName (scOverlays context) of
    Nothing -> Left ("resolved simulation scope did not grant dependency overlay: " <> overlayName)
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

-- | Decode host terrain with an explicit aggregate decoded-byte budget.
decodeTerrainPayloadWithLimits :: RPCPayloadLimits -> Value -> Either Text TerrainWorld
decodeTerrainPayloadWithLimits limits payload =
  applyGeneratorTerrainValueWithLimits limits baseWorld payload
  where
    baseWorld = emptyWorld (WorldConfig { wcChunkSize = defaultChunkSize }) defaultHexGridMeta
    defaultChunkSize = 64

-- | Encode a terrain world into protocol terrain payload shape.
encodeTerrainPayload :: TerrainWorld -> Either Text Value
encodeTerrainPayload = terrainWorldToPayload

-- | Encode terrain while consuming an explicit aggregate binary budget.
encodeTerrainPayloadWithLimits :: RPCPayloadLimits -> TerrainWorld -> Either Text Value
encodeTerrainPayloadWithLimits = terrainWorldToPayloadWithLimits

-- | Decode simulation terrain writes payload into structured writes.
decodeTerrainWritesPayload :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesPayload = decodeTerrainWritesValue

-- | Decode simulation terrain writes with an explicit aggregate decoded-byte
-- budget.
decodeTerrainWritesPayloadWithLimits
  :: RPCPayloadLimits
  -> Maybe Value
  -> Either Text TerrainWrites
decodeTerrainWritesPayloadWithLimits = decodeTerrainWritesValueWithLimits

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

-- | Encode simulation terrain writes with an explicit aggregate binary budget.
encodeTerrainWritesPayloadWithLimits
  :: RPCPayloadLimits
  -> TerrainWrites
  -> Either Text Value
encodeTerrainWritesPayloadWithLimits limits writes =
  terrainWorldToPayloadWithLimits limits writesWorld
  where
    baseWorld = emptyWorld (WorldConfig { wcChunkSize = defaultChunkSize }) defaultHexGridMeta
    defaultChunkSize = 64
    writesWorld = applyTerrainWrites writes baseWorld

-- | Fold a fully validated snapshot in canonical section/chunk order.
foldTerrainChunks
  :: TerrainChunkSource
  -> a
  -> (a -> TerrainChunkRecord -> IO (Either Text a))
  -> IO (Either Text a)
foldTerrainChunks = tcsFoldChunks

-- | Construct a replayable source after validating duplicate keys, canonical
-- order, and every chunk's binary representation.
terrainChunkSourceFromRecords
  :: TerrainSnapshotHeader
  -> [TerrainChunkRecord]
  -> Either Text TerrainChunkSource
terrainChunkSourceFromRecords header records = do
  let ordered = sortOn (\record -> (tcrSection record, tcrChunkId record)) records
      keys = map (\record -> (tcrSection record, tcrChunkId record)) ordered
  if keys == Set.toAscList (Set.fromList keys)
    then pure ()
    else Left "streamed terrain snapshot contains duplicate chunk keys"
  mapM_ (validateTerrainChunkRecord header) ordered
  let runFold initial step = foldM (stepEither step) (Right initial) ordered
      stepEither _ (Left err) _ = pure (Left err)
      stepEither step (Right current) record = step current record
  pure TerrainChunkSource
    { tcsHeader = header
    , tcsFoldChunks = runFold
    }

-- | Construct a replayable file-backed source. Every file is decoded once
-- before the source is returned, and each fold keeps only one chunk in memory.
terrainChunkSourceFromFiles
  :: TerrainSnapshotHeader
  -> [(TerrainSection, Int, FilePath)]
  -> IO (Either Text TerrainChunkSource)
terrainChunkSourceFromFiles header files = do
  let ordered = sortOn (\(section, chunkId, _) -> (section, chunkId)) files
      keys = [(section, chunkId) | (section, chunkId, _) <- ordered]
  if keys /= Set.toAscList (Set.fromList keys)
    then pure (Left "streamed terrain snapshot contains duplicate chunk keys")
    else do
      validation <- foldM validateFile (Right ()) ordered
      pure $ case validation of
        Left err -> Left err
        Right () -> Right TerrainChunkSource
          { tcsHeader = header
          , tcsFoldChunks = \initial step -> foldM (stepFile step) (Right initial) ordered
          }
  where
    validateFile (Left err) _ = pure (Left err)
    validateFile (Right ()) (section, chunkId, path) = do
      raw <- BS.readFile path
      pure (validateTerrainChunkRecord header (TerrainChunkRecord section chunkId raw))
    stepFile _ (Left err) _ = pure (Left err)
    stepFile step (Right current) (section, chunkId, path) = do
      raw <- BS.readFile path
      step current (TerrainChunkRecord section chunkId raw)

-- | Materialize a source for the explicit compatibility staging adapter.
terrainChunkSourceToRecords :: TerrainChunkSource -> IO (Either Text [TerrainChunkRecord])
terrainChunkSourceToRecords source =
  foldTerrainChunks source [] (\records record -> pure (Right (records <> [record])))

-- | Decode immutable geometry metadata carried by a streamed invocation header.
decodeTerrainSnapshotHeader :: Value -> Either Text TerrainSnapshotHeader
decodeTerrainSnapshotHeader value = case AesonTypes.parseEither parser value of
  Left err -> Left ("invalid terrain snapshot header: " <> Text.pack err)
  Right header
    | tshChunkSize header <= 0 -> Left "invalid terrain snapshot header: chunk_size must be positive"
    | otherwise -> Right header
  where
    parser = withObject "TerrainSnapshotHeader" $ \fields -> TerrainSnapshotHeader
      <$> fields .: "chunk_size"
      <*> fields .: "hex_grid"
      <*> fields .: "planet"
      <*> fields .: "slice"

-- | Rebuild the established scoped terrain payload for the staging adapter.
terrainChunkRecordsToPayload
  :: TerrainSnapshotHeader
  -> [TerrainChunkRecord]
  -> Either Text Value
terrainChunkRecordsToPayload header records = do
  _ <- terrainChunkSourceFromRecords header records
  let collected = sortOn (\record -> (tcrSection record, tcrChunkId record)) records
      section sectionValue = Map.fromList
        [ (Text.pack (show (tcrChunkId record)), String (encodeBase64Text (tcrBytes record)))
        | record <- collected, tcrSection record == sectionValue
        ]
      terrain = section TerrainElevation
      climate = section TerrainClimate
      vegetation = section TerrainVegetation
  pure $ object $
    [ "chunk_size" .= tshChunkSize header
    , "hex_grid" .= tshHexGrid header
    , "planet" .= tshPlanet header
    , "slice" .= tshSlice header
    , "encoding" .= ("base64" :: Text)
    ]
    <> (if Map.null terrain then [] else
      ["chunk_count" .= Map.size terrain, "terrain" .= terrain])
    <> (if Map.null climate then [] else
      ["climate_count" .= Map.size climate, "climate" .= climate])
    <> (if Map.null vegetation then [] else
      ["vegetation_count" .= Map.size vegetation, "vegetation" .= vegetation])

-- | Encode selected chunks from a world into canonical delta updates.
terrainChunkUpdatesFromWorld
  :: ResolvedInvocationScope
  -> TerrainWorld
  -> Either Text [TerrainChunkUpdate]
terrainChunkUpdatesFromWorld scope world =
  traverse encodeOne keys
  where
    sections = Set.toAscList (risTerrainOutputSections scope)
    chunks = IntSet.toAscList (risTerrainOutputChunkIds scope)
    keys = [(section, chunkId) | section <- sections, chunkId <- chunks,
      chunkPresent section chunkId world]
    config = twConfig world
    encodeOne (TerrainElevation, chunkId) = case IntMap.lookup chunkId (twTerrain world) of
      Nothing -> Left "terrain chunk disappeared during encoding"
      Just chunk -> encodeUpdate TerrainElevation chunkId (encodeTerrainChunk config chunk)
    encodeOne (TerrainClimate, chunkId) = case IntMap.lookup chunkId (twClimate world) of
      Nothing -> Left "climate chunk disappeared during encoding"
      Just chunk -> encodeUpdate TerrainClimate chunkId (encodeClimateChunk config chunk)
    encodeOne (TerrainVegetation, chunkId) = case IntMap.lookup chunkId (twVegetation world) of
      Nothing -> Left "vegetation chunk disappeared during encoding"
      Just chunk -> encodeUpdate TerrainVegetation chunkId (encodeVegetationChunk config chunk)

-- | Encode whole-chunk simulation writes for a native delta sink.
terrainWritesToChunkUpdates :: WorldConfig -> TerrainWrites -> Either Text [TerrainChunkUpdate]
terrainWritesToChunkUpdates config writes = fmap concat (sequence
  [ traverse (\(chunkId, chunk) -> encodeUpdate TerrainElevation chunkId
      (encodeTerrainChunk config chunk)) (IntMap.toAscList (twrTerrain writes))
  , traverse (\(chunkId, chunk) -> encodeUpdate TerrainClimate chunkId
      (encodeClimateChunk config chunk)) (IntMap.toAscList (twrClimate writes))
  , traverse (\(chunkId, chunk) -> encodeUpdate TerrainVegetation chunkId
      (encodeVegetationChunk config chunk)) (IntMap.toAscList (twrVegetation writes))
  ])

-- | Write structured simulation updates through a scope-checking sink.
writeTerrainWritesToSink
  :: WorldConfig -> TerrainDeltaSink -> TerrainWrites -> IO (Either Text ())
writeTerrainWritesToSink config sink writes = case terrainWritesToChunkUpdates config writes of
  Left err -> pure (Left err)
  Right updates -> foldM writeOne (Right ()) updates
  where
    writeOne (Left err) _ = pure (Left err)
    writeOne (Right ()) update = tdsWriteChunk sink update

encodeUpdate
  :: TerrainSection -> Int -> Either ExportError BS.ByteString
  -> Either Text TerrainChunkUpdate
encodeUpdate section chunkId = either
  (Left . Text.pack . show)
  (Right . TerrainChunkUpdate section chunkId)

-- | Emit only changed chunks relative to the invocation snapshot. Chunks that
-- were present in the snapshot but are absent from the new world are removals.
diffTerrainWorldAgainstSnapshot
  :: ResolvedInvocationScope
  -> TerrainChunkSource
  -> TerrainWorld
  -> TerrainDeltaSink
  -> IO (Either Text ())
diffTerrainWorldAgainstSnapshot scope source world sink = do
  baselineResult <- foldTerrainChunks source Map.empty $ \baseline record ->
    let key = (tcrSection record, tcrChunkId record)
        granted = Set.member (tcrSection record) (risTerrainOutputSections scope)
          && IntSet.member (tcrChunkId record) (risTerrainOutputChunkIds scope)
    in pure (Right (if granted then Map.insert key (tcrBytes record) baseline else baseline))
  case baselineResult of
    Left err -> pure (Left err)
    Right baseline -> case terrainChunkUpdatesFromWorld scope world of
      Left err -> pure (Left err)
      Right current -> do
        let currentMap = Map.fromList
              [ ((tcuSection update, tcuChunkId update), tcuBytes update)
              | update <- current
              ]
            changed = filter (\update -> Map.lookup
              (tcuSection update, tcuChunkId update) baseline /= Just (tcuBytes update)) current
            removed = Map.keys (baseline `Map.difference` currentMap)
        updateResult <- foldM writeUpdate (Right ()) changed
        case updateResult of
          Left err -> pure (Left err)
          Right () -> foldM writeRemoval (Right ()) removed
  where
    writeUpdate (Left err) _ = pure (Left err)
    writeUpdate (Right ()) update = tdsWriteChunk sink update
    writeRemoval (Left err) _ = pure (Left err)
    writeRemoval (Right ()) (section, chunkId) =
      tdsRemoveChunk sink (TerrainChunkRemoval section chunkId)

validateTerrainChunkRecord :: TerrainSnapshotHeader -> TerrainChunkRecord -> Either Text ()
validateTerrainChunkRecord header record =
  either (Left . renderExportError) (const (Right ())) decoded
  where
    config = WorldConfig (tshChunkSize header)
    decoded = case tcrSection record of
      TerrainElevation -> decodeTerrainChunk config (tcrBytes record) >> Right ()
      TerrainClimate -> decodeClimateChunk config (tcrBytes record) >> Right ()
      TerrainVegetation -> decodeVegetationChunk config (tcrBytes record) >> Right ()
    renderExportError :: ExportError -> Text
    renderExportError = Text.pack . show

chunkPresent :: TerrainSection -> Int -> TerrainWorld -> Bool
chunkPresent TerrainElevation chunkId = IntMap.member chunkId . twTerrain
chunkPresent TerrainClimate chunkId = IntMap.member chunkId . twClimate
chunkPresent TerrainVegetation chunkId = IntMap.member chunkId . twVegetation

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

-- | Build a simulation result using the configured terrain binary budget.
simulationResultWithTerrainWritesWithLimits
  :: RPCPayloadLimits
  -> Overlay
  -> TerrainWrites
  -> Either Text SimulationTickResult
simulationResultWithTerrainWritesWithLimits limits overlay writes = do
  encodedWrites <- encodeTerrainWritesPayloadWithLimits limits writes
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

-- | Build a generator result containing exactly the resolved output sections
-- and chunks. Use this in 'GeneratorScopeDef' callbacks instead of the broad
-- legacy encoder.
generatorResultFromScopedTerrain
  :: GeneratorContext
  -> TerrainWorld
  -> Either Text GeneratorTickResult
generatorResultFromScopedTerrain context world = do
  let scope = gcScope context
  terrainPayload <- terrainWorldToScopedPayload
    (risTerrainOutputSections scope) (risTerrainOutputChunkIds scope) world
  Right defaultGeneratorTickResult { gtrTerrain = terrainPayload }

-- | Build a generator result using the configured terrain binary budget.
generatorResultFromTerrainWithLimits
  :: RPCPayloadLimits
  -> TerrainWorld
  -> Either Text GeneratorTickResult
generatorResultFromTerrainWithLimits limits world = do
  terrainPayload <- encodeTerrainPayloadWithLimits limits world
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

-- | Build a generator result with overlay using the configured binary budget.
generatorResultFromTerrainAndOverlayWithLimits
  :: RPCPayloadLimits
  -> TerrainWorld
  -> Overlay
  -> Either Text GeneratorTickResult
generatorResultFromTerrainAndOverlayWithLimits limits world overlay = do
  terrainPayload <- encodeTerrainPayloadWithLimits limits world
  Right defaultGeneratorTickResult
    { gtrTerrain = terrainPayload
    , gtrOverlay = Just (encodeOverlayPayload overlay)
    }
