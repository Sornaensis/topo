{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (NFData, force, rnf)
import Control.Exception (evaluate)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import GHC.Stats
  ( RTSStats(..)
  , GCDetails(..)
  , getRTSStats
  , getRTSStatsEnabled
  )
import System.Environment (getArgs)
import System.Mem (performGC)

import Topo.Hex (defaultHexGridMeta)
import Topo.Plugin.RPC
  ( RPCEnvelope(..)
  , RPCMessageType(..)
  , defaultRPCPayloadLimits
  , encodeMessage
  , encodeMessageLazy
  , terrainWorldToPayloadWithLimits
  )
import Topo.Types (WorldConfig(..))
import Topo.World
  ( TerrainWorld(..)
  , emptyClimateChunk
  , emptyTerrainChunk
  , emptyVegetationChunk
  , emptyWorld
  )

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then error "RTS statistics are disabled; run with +RTS -T"
    else do
      args <- getArgs
      let counts = case args of
            [] -> [1, 25, 49]
            [raw] -> [read raw]
            _ -> error "usage: topo-rpc-allocation-bench [1|25|49]"
      mapM_ runCase counts

runCase :: Int -> IO ()
runCase count = do
  let payload = payloadFor count
      envelope = RPCEnvelope
        { envType = MsgInvokeGenerator
        , envPayload = payload
        , envRequestId = Just 1
        }
  _ <- evaluate (force payload)
  let wireEnvelope = envelope { envRequestId = Just 0 }
  if BL.toStrict (encodeMessageLazy wireEnvelope) /= encodeMessage wireEnvelope
    then error "lazy and strict RPC encodings differ"
    else pure ()
  measure (show count <> "chunks,lazy") (encodeMessageLazy envelope)
  measure (show count <> "chunks,strict") (encodeMessage envelope)

measure :: NFData a => String -> a -> IO ()
measure label valueThunk = do
  performGC
  before <- getRTSStats
  value <- evaluate (force valueThunk)
  performGC
  after <- getRTSStats
  -- Keep the measured body live through the post-force collection.
  _ <- evaluate (rnf value)
  putStrLn
    (label
      <> ",allocated_bytes=" <> show (allocated_bytes after - allocated_bytes before)
      <> ",live_bytes=" <> show (gcdetails_live_bytes (gc after))
      <> ",max_live_bytes=" <> show (max_live_bytes after))

rpcWorldConfig :: WorldConfig
rpcWorldConfig = WorldConfig { wcChunkSize = 64 }

rpcBaseWorld :: TerrainWorld
rpcBaseWorld = emptyWorld rpcWorldConfig defaultHexGridMeta

worldWithChunks :: Int -> TerrainWorld
worldWithChunks count = rpcBaseWorld
  { twTerrain = IntMap.fromDistinctAscList
      [(key, emptyTerrainChunk rpcWorldConfig) | key <- keys]
  , twClimate = IntMap.fromDistinctAscList
      [(key, emptyClimateChunk rpcWorldConfig) | key <- keys]
  , twVegetation = IntMap.fromDistinctAscList
      [(key, emptyVegetationChunk rpcWorldConfig) | key <- keys]
  }
  where
    keys = [0 .. count - 1]

payloadFor :: Int -> Value
payloadFor count =
  case terrainWorldToPayloadWithLimits defaultRPCPayloadLimits (worldWithChunks count) of
    Left err -> error (show err)
    Right payload -> payload
