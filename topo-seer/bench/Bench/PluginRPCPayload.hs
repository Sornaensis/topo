{-# LANGUAGE OverloadedStrings #-}

-- | Allocation-sensitive protocol-v4 codec cases. Run with @+RTS -T@ and an
-- allocation profiler to compare the normal lazy frame with the strict wrapper.
module Bench.PluginRPCPayload (benchmarks) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Topo.Hex (defaultHexGridMeta)
import Topo.Plugin.RPC
  ( RPCEnvelope(..)
  , RPCMessageType(..)
  , applyGeneratorTerrainValue
  , defaultRPCPayloadLimits
  , encodeMessage
  , encodeMessageLazy
  , terrainWorldToPayloadWithLimits
  )
import Topo.Types
  ( ClimateChunk
  , TerrainChunk(..)
  , VegetationChunk
  , WorldConfig(..)
  )
import Topo.World
  ( TerrainWorld(..)
  , emptyClimateChunk
  , emptyTerrainChunk
  , emptyVegetationChunk
  , emptyWorld
  )

benchmarks :: Benchmark
benchmarks = bgroup "PluginRPCPayload"
  [ bgroup "bounded-encode" (map encodeCase chunkCounts)
  , bgroup "decode" (map decodeCase chunkCounts)
  , bgroup "json-frame" (concatMap jsonCases chunkCounts)
  ]

chunkCounts :: [Int]
chunkCounts = [1, 25, 49]

encodeCase :: Int -> Benchmark
encodeCase count =
  bench (show count <> "chunks") $
    nf (terrainWorldToPayloadWithLimits defaultRPCPayloadLimits) (worldWithChunks count)

decodeCase :: Int -> Benchmark
decodeCase count =
  bench (show count <> "chunks") $ nf decodeChecksum (payloadFor count)

jsonCases :: Int -> [Benchmark]
jsonCases count =
  [ bench ("lazy-" <> show count <> "chunks") $
      nf (BL.length . encodeMessageLazy) (envelopeFor count)
  , bench ("strict-" <> show count <> "chunks") $
      nf encodeMessage (envelopeFor count)
  ]

rpcWorldConfig :: WorldConfig
rpcWorldConfig = WorldConfig { wcChunkSize = 64 }

rpcTerrainChunk :: TerrainChunk
rpcTerrainChunk = emptyTerrainChunk rpcWorldConfig

rpcClimateChunk :: ClimateChunk
rpcClimateChunk = emptyClimateChunk rpcWorldConfig

rpcVegetationChunk :: VegetationChunk
rpcVegetationChunk = emptyVegetationChunk rpcWorldConfig

rpcBaseWorld :: TerrainWorld
rpcBaseWorld = emptyWorld rpcWorldConfig defaultHexGridMeta

worldWithChunks :: Int -> TerrainWorld
worldWithChunks count = rpcBaseWorld
  { twTerrain = IntMap.fromDistinctAscList [(key, rpcTerrainChunk) | key <- keys]
  , twClimate = IntMap.fromDistinctAscList [(key, rpcClimateChunk) | key <- keys]
  , twVegetation = IntMap.fromDistinctAscList [(key, rpcVegetationChunk) | key <- keys]
  }
  where
    keys = [0 .. count - 1]

payloadFor :: Int -> Value
payloadFor count =
  case terrainWorldToPayloadWithLimits defaultRPCPayloadLimits (worldWithChunks count) of
    Left err -> error (show err)
    Right payload -> payload

envelopeFor :: Int -> RPCEnvelope
envelopeFor count = RPCEnvelope
  { envType = MsgInvokeGenerator
  , envPayload = payloadFor count
  , envRequestId = Just 1
  }

decodeChecksum :: Value -> Either Text Float
decodeChecksum payload = do
  world <- applyGeneratorTerrainValue rpcBaseWorld payload
  pure (IntMap.foldl'
    (\total chunk -> total + U.sum (tcElevation chunk))
    0
    (twTerrain world))
