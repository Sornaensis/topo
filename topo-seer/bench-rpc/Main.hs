{-# LANGUAGE OverloadedStrings #-}

-- Deterministic RPC payload allocation baseline. Run with:
-- stack bench topo-seer:topo-rpc-allocation-bench --ba "25 +RTS -T -RTS"
-- The executable reports semantic wire/decoded sizes and RTS allocations; it
-- deliberately has no elapsed-time gate.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, when)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Mem (performGC)
import Data.Word (Word64)

import Topo.Export
  ( encodeClimateChunk, encodeGlacierChunk, encodeGroundwaterChunk
  , encodeRiverChunk, encodeTerrainChunk, encodeVegetationChunk
  , encodeVolcanismChunk, encodeWaterBodyChunk )
import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay
  ( Overlay(..), OverlayData(..), OverlayRecord, OverlayValue(..)
  , chunkInsert, emptyOverlayChunk, emptyOverlayProvenance, mkOverlayRecordUnchecked )
import Topo.Overlay.JSON (overlayToJSON)
import Topo.Overlay.Schema
  ( OverlayFieldDef(..), OverlayFieldType(..), OverlaySchema(..), OverlayStorage(..)
  , emptyOverlayDeps )
import Topo.Plugin.RPC
  ( RPCEnvelope(..)
  , RPCMessageType(..)
  , defaultRPCPayloadLimits
  , encodeMessage
  , encodeMessageLazy
  , applyGeneratorTerrainValue
  , terrainWorldToCompletePayloadWithLimits
  , terrainWorldToPayloadWithLimits
  , terrainWorldToScopedPayloadWithLimits
  )
import Topo.Plugin.RPC.Scope
  ( RPCInvocationKind(..), RPCScopeBudgets(..), ResolvedInvocationScope(..)
  , TerrainSection(..) )
import Topo.Plugin.RPC.Stream
  ( NegotiatedStreamV1(..), StreamCodec(..), StreamEnd(..), StreamEnvelope(..)
  , StreamId(..), StreamMachine, StreamOpen(..), StreamPayloadKind(..), StreamRecord
  , StreamRecordKey(..), StreamRole(..), activeStreamCount, consumeStreamRecord
  , defaultStreamProposal, encodeStreamRecord, endOutboundStream, negotiateStreamV1
  , newStreamMachine, openOutboundStream, outboundStreamEnd, receiveStreamEnvelope
  , registerStreamRequest, sendOutboundRecord
  , streamEnvelopeFrameBytes )
import Topo.Types (WorldConfig(..))
import Topo.World
  ( TerrainWorld(..)
  , emptyClimateChunk
  , emptyGlacierChunk
  , emptyGroundwaterChunk
  , emptyRiverChunk
  , emptyTerrainChunk
  , emptyVegetationChunk
  , emptyVolcanismChunk
  , emptyWaterBodyChunk
  , emptyWorld
  )

data Measurement = Measurement
  { measuredWireBytes :: !Integer
  , measuredDecodedBytes :: !Integer
  , measuredAllocatedBytes :: !Word64
  , measuredInFlightBytes :: !Integer
  , measuredSpoolBytes :: !Integer
  } deriving (Show)

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  when (not enabled) (error "RTS statistics are disabled; run with +RTS -T")
  args <- getArgs
  let counts = case args of
        [] -> [1, 25, 49]
        [raw] -> [read raw]
        _ -> error "usage: topo-rpc-allocation-bench [1|25|49]"
  measurements <- forM counts runCase
  case [triple | Just triple <- measurements] of
    [(subset, full, complete)] -> assertProportional subset full complete
    _ -> pure ()
  runStructuredCases
  runStreamOutputCase

runCase :: Int -> IO (Maybe (Measurement, Measurement, Measurement))
runCase count = do
  let world = terrainWorldWithChunks count
      completeWorld = completeWorldWithChunks count
      selectedCount = max 1 (count `div` 5)
      selected = IntSet.fromDistinctAscList [0 .. selectedCount - 1]
      tiles = 64 * 64 :: Integer
      terrainDecoded = toInteger count * (12 + 153 * tiles)
      scopedDecoded = toInteger selectedCount * (4 + 113 * tiles)
      completeDecoded = toInteger count * (44 + 273 * tiles)
      fullThunk = terrainWorldToPayloadWithLimits defaultRPCPayloadLimits world
      scopedThunk = terrainWorldToScopedPayloadWithLimits defaultRPCPayloadLimits
        (Set.singleton TerrainElevation) selected world
      completeThunk = terrainWorldToCompletePayloadWithLimits defaultRPCPayloadLimits completeWorld
  forcePayloadFixture world
  forcePayloadFixture completeWorld
  full <- measurePayload (show count <> "chunks,terrain-v4-encode") terrainDecoded fullThunk
  scoped <- measurePayload
    (show count <> "chunks,scoped-elevation-" <> show selectedCount <> "-encode")
    scopedDecoded scopedThunk
  complete <- if count == 25
    then measurePayload (show count <> "chunks,complete-v4-encode") completeDecoded completeThunk
    else pure full
  payload <- expectRight fullThunk
  _ <- measureComputed (show count <> "chunks,terrain-v4-decode") terrainDecoded 0 0 $
    case applyGeneratorTerrainValue (emptyWorld (WorldConfig 64) defaultHexGridMeta) payload of
      Left _ -> error "terrain decode failed"
      Right decoded -> toInteger (IntMap.size (twTerrain decoded))
  let envelope = RPCEnvelope MsgInvokeGenerator payload (Just 1)
      wireEnvelope = envelope { envRequestId = Just 0 }
  when (BL.toStrict (encodeMessageLazy wireEnvelope) /= encodeMessage wireEnvelope)
    (error "lazy and strict RPC encodings differ")
  _ <- measureValue (show count <> "chunks,envelope-lazy") terrainDecoded
    (fromIntegral (BL.length (encodeMessageLazy envelope)))
  _ <- measureValue (show count <> "chunks,envelope-strict") terrainDecoded
    (fromIntegral (BS.length (encodeMessage envelope)))
  pure (if count == 25 then Just (scoped, full, complete) else Nothing)

measurePayload :: String -> Integer -> Either a Value -> IO Measurement
measurePayload label decoded thunk =
  measureComputed label decoded 0 0 $ case thunk of
    Left _ -> error "payload class unexpectedly exceeded the default RPC budget"
    Right payload -> toInteger (BL.length (Aeson.encode payload))

measureValue :: String -> Integer -> Integer -> IO Measurement
measureValue label decoded = measureComputed label decoded 0 0

measureComputed :: String -> Integer -> Integer -> Integer -> Integer -> IO Measurement
measureComputed label decoded inFlight spool valueThunk = do
  performGC
  before <- getRTSStats
  wireBytes <- evaluate (force valueThunk)
  after <- getRTSStats
  let measurement = Measurement
        { measuredWireBytes = wireBytes
        , measuredDecodedBytes = decoded
        , measuredAllocatedBytes = allocated_bytes after - allocated_bytes before
        , measuredInFlightBytes = inFlight
        , measuredSpoolBytes = spool
        }
  putStrLn
    (label
      <> ",wire_bytes=" <> show wireBytes
      <> ",decoded_bytes=" <> show decoded
      <> ",allocated_bytes=" <> show (measuredAllocatedBytes measurement)
      <> ",in_flight_bytes=" <> show inFlight
      <> ",spool_bytes=" <> show spool)
  pure measurement

assertProportional :: Measurement -> Measurement -> Measurement -> IO ()
assertProportional subset full complete = do
  when (measuredDecodedBytes subset * 4 >= measuredDecodedBytes full)
    (error "scoped decoded extent did not fall proportionally")
  when (measuredWireBytes subset * 4 >= measuredWireBytes full)
    (error "scoped wire extent did not fall proportionally")
  when (measuredAllocatedBytes subset >= measuredAllocatedBytes full)
    (error "scoped allocation did not fall below the full payload class")
  when (measuredDecodedBytes complete <= measuredDecodedBytes full)
    (error "complete payload class did not include additional sections")

-- Force fixture maps and their shared chunk vectors outside every measured
-- region without constructing any RPC payload or base64 text.
forcePayloadFixture :: TerrainWorld -> IO ()
forcePayloadFixture world = do
  let config = twConfig world
      mapSizes = sum
        [ IntMap.size (twTerrain world), IntMap.size (twClimate world)
        , IntMap.size (twRivers world), IntMap.size (twGroundwater world)
        , IntMap.size (twVolcanism world), IntMap.size (twGlaciers world)
        , IntMap.size (twWaterBodies world), IntMap.size (twVegetation world)
        ]
      encodedLengths = sum
        [ firstLength (encodeTerrainChunk config) (twTerrain world)
        , firstLength (encodeClimateChunk config) (twClimate world)
        , firstLength (encodeRiverChunk config) (twRivers world)
        , firstLength (encodeGroundwaterChunk config) (twGroundwater world)
        , firstLength (encodeVolcanismChunk config) (twVolcanism world)
        , firstLength (encodeGlacierChunk config) (twGlaciers world)
        , firstLength (encodeWaterBodyChunk config) (twWaterBodies world)
        , firstLength (encodeVegetationChunk config) (twVegetation world)
        ]
  _ <- evaluate (force (mapSizes + encodedLengths))
  performGC
  where
    firstLength encodeChunk chunks = case IntMap.lookupMin chunks of
      Nothing -> 0
      Just (_, chunk) -> case encodeChunk chunk of
        Left err -> error (show err)
        Right bytes -> BS.length bytes

runStructuredCases :: IO ()
runStructuredCases = do
  let dense = Overlay denseSchema
        (DenseData (IntMap.singleton 0 (V.singleton (U.generate 4096 fromIntegral))))
        emptyOverlayProvenance
      sparseRecord :: OverlayRecord
      sparseRecord = mkOverlayRecordUnchecked [OVInt 7]
      sparseChunk = foldr (`chunkInsert` sparseRecord) emptyOverlayChunk [0 .. 255]
      sparse = Overlay sparseSchema (SparseData (IntMap.singleton 0 sparseChunk))
        emptyOverlayProvenance
      result = object
        [ "resource" .= ("bench-records" :: String)
        , "records" .= [object ["id" .= row, "payload" .= ([0 .. 63] :: [Int])]
                        | row <- [0 .. 255 :: Int]]
        ]
  _ <- measureJSON "overlay-dense" 4096 (overlayToJSON dense)
  _ <- measureJSON "overlay-sparse" 256 (overlayToJSON sparse)
  _ <- measureJSON "data-result-256" (256 * 64) result
  pure ()
  where
    measureJSON label decoded value =
      measureComputed label decoded 0 0 (toInteger (BL.length (Aeson.encode value)))
    denseSchema = benchmarkSchema StorageDense
    sparseSchema = benchmarkSchema StorageSparse
    benchmarkSchema storage = OverlaySchema
      { osName = "allocation-benchmark"
      , osVersion = "1"
      , osDescription = "allocation benchmark fixture"
      , osFields = [OverlayFieldDef "value" OFInt (Aeson.Number 0) False Nothing]
      , osStorage = storage
      , osDependencies = emptyOverlayDeps
      , osFieldIndex = Map.singleton "value" 0
      }

runStreamOutputCase :: IO ()
runStreamOutputCase = do
  let config = WorldConfig 64
      raw = either (error . show) id (encodeTerrainChunk config (emptyTerrainChunk config))
      chunkIds = IntSet.fromDistinctAscList [0 .. 7]
      records =
        [ encodeStreamRecord StreamIdentity (StreamId 2) 900 (fromIntegral index)
            (StreamRecordKey TerrainElevation index 0 0) raw
        | index <- [0 .. 7]
        ]
      decoded = toInteger (BS.length raw * length records)
      wire = validatedStreamOutputWire chunkIds records raw
  _ <- measureComputed "stream-output-8-valid-s64-chunks" decoded
    (toInteger (BS.length raw)) (toInteger (BS.length raw)) wire
  pure ()

validatedStreamOutputWire
  :: IntSet.IntSet -> [StreamRecord] -> BS.ByteString -> Integer
validatedStreamOutputWire chunkIds records raw =
  let limits = either (error . show) id
        (negotiateStreamV1 (defaultStreamProposal (1024 * 1024))
          (defaultStreamProposal (1024 * 1024)))
      scope = ResolvedInvocationScope
        { risScopeId = "allocation-stream-scope"
        , risKind = InvocationGenerator
        , risTerrainInputSections = Set.empty
        , risTerrainInputChunkIds = IntSet.empty
        , risDependencyOverlayChunkIds = Map.empty
        , risOwnOverlayReadChunkIds = IntSet.empty
        , risTerrainOutputSections = Set.singleton TerrainElevation
        , risTerrainOutputChunkIds = chunkIds
        , risOwnedOverlayIdentity = Nothing
        , risOwnOverlayWriteChunkIds = IntSet.empty
        , risGeneratorMetadataOutput = False
        , risDataResource = Nothing
        , risBudgets = RPCScopeBudgets (16 * 1024 * 1024) (16 * 1024 * 1024) 4096
        }
      totalBytes = fromIntegral (BS.length raw * length records)
      open = StreamOpen
        { soStreamId = StreamId 2, soParentRequestId = 900
        , soScopeId = risScopeId scope, soPayloadKind = TerrainDelta
        , soPayloadVersion = 1, soSections = Set.singleton TerrainElevation
        , soChunkIds = chunkIds, soMetadata = object [], soCodec = StreamIdentity
        , soTotalItems = Just (fromIntegral (length records))
        , soTotalBytes = Just totalBytes, soFinalSha256 = Nothing
        }
      plugin0 = either (error . show) id
        (registerStreamRequest 900 scope 1000 (newStreamMachine StreamPlugin limits))
      plugin1 = either (error . show) id (openOutboundStream 1 open plugin0)
      host0 = either (error . show) id
        (registerStreamRequest 900 scope 1000 (newStreamMachine StreamHost limits))
      host1 = expectEffectCount 1
        (receiveStreamEnvelope 1 (StreamOpenEnvelope open) host0)
      plugin2 = expectEffectCount 0 (receiveStreamEnvelope 2
        (StreamWindowEnvelope (StreamId 2) 900 (nsvReceiveWindowBytes limits)) plugin1)
      (plugin3, host2) = foldl transferOne (plugin2, host1) (zip [3, 5 ..] records)
      end = either (error . show) id (outboundStreamEnd (StreamId 2) plugin3)
      plugin4 = either (error . show) id (endOutboundStream 20 end plugin3)
      host3 = expectEffectCount 1
        (receiveStreamEnvelope 20 (StreamEndEnvelope end) host2)
      frames = StreamOpenEnvelope open : map StreamDataEnvelope records
        <> [StreamEndEnvelope end]
  in if activeStreamCount plugin4 == 1 && activeStreamCount host3 == 1
       && seTotalBytes end == totalBytes
      then sum (map (toInteger . streamEnvelopeFrameBytes) frames)
      else error "stream lifecycle did not retain the validated terminal output"
  where
    transferOne (plugin, host) (now, record) =
      let pluginSent = either (error . show) id
            (sendOutboundRecord now record raw plugin)
          hostStaged = expectEffectCount 1
            (receiveStreamEnvelope (now + 1) (StreamDataEnvelope record) host)
          (hostConsumed, effects) = either (error . show) id
            (consumeStreamRecord (now + 1) (StreamId 2) hostStaged)
          pluginCredited = expectEffectCount 0 (receiveStreamEnvelope (now + 2)
            (StreamWindowEnvelope (StreamId 2) 900 (fromIntegral (BS.length raw))) pluginSent)
      in if length effects == 1
           then (pluginCredited, hostConsumed)
           else error "stream consume did not replenish exactly one window"

expectEffectCount :: Int -> (StreamMachine, Either a [b]) -> StreamMachine
expectEffectCount expected (machine, Right effects)
  | length effects == expected = machine
expectEffectCount _ (_, Left _) = error "stream lifecycle validation failed"
expectEffectCount _ _ = error "stream lifecycle emitted an unexpected effect count"

expectRight :: Either a b -> IO b
expectRight (Right value) = pure value
expectRight (Left _) = error "payload class unexpectedly exceeded the default RPC budget"

terrainWorldWithChunks :: Int -> TerrainWorld
terrainWorldWithChunks count = (emptyWorld config defaultHexGridMeta)
  { twTerrain = chunks (emptyTerrainChunk config)
  , twClimate = chunks (emptyClimateChunk config)
  , twVegetation = chunks (emptyVegetationChunk config)
  }
  where
    config = WorldConfig { wcChunkSize = 64 }
    chunks value = IntMap.fromDistinctAscList [(key, value) | key <- [0 .. count - 1]]

completeWorldWithChunks :: Int -> TerrainWorld
completeWorldWithChunks count = (emptyWorld config defaultHexGridMeta)
  { twTerrain = chunks (emptyTerrainChunk config)
  , twClimate = chunks (emptyClimateChunk config)
  , twRivers = chunks (emptyRiverChunk config)
  , twGroundwater = chunks (emptyGroundwaterChunk config)
  , twVolcanism = chunks (emptyVolcanismChunk config)
  , twGlaciers = chunks (emptyGlacierChunk config)
  , twWaterBodies = chunks (emptyWaterBodyChunk config)
  , twVegetation = chunks (emptyVegetationChunk config)
  }
  where
    config = WorldConfig { wcChunkSize = 64 }
    chunks value = IntMap.fromDistinctAscList [(key, value) | key <- [0 .. count - 1]]
