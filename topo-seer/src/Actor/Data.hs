{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , DataSnapshotReply
  , dataActorDef
  , setTerrainChunkCount
  , setBiomeChunkCount
  , setLastSeed
  , setTerrainChunkData
  , setClimateChunkData
  , setWeatherChunkData
  , setRiverChunkData
  , requestDataSnapshot
  , getDataSnapshot
  , getTerrainSnapshot
  , replaceTerrainData
  ) where

import Control.Exception (evaluate)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import Topo (ChunkId(..), ClimateChunk, RiverChunk, TerrainChunk, WeatherChunk)
import Topo.World (TerrainWorld(..))
import Topo.Types (WorldConfig(..))

data DataSnapshot = DataSnapshot
  { dsTerrainChunks :: !Int
  , dsBiomeChunks :: !Int
  , dsLastSeed :: !(Maybe Word64)
  } deriving (Eq, Show)

-- | Snapshot of terrain chunk data.
--
-- @tsVersion@ is a monotonic stamp used for O(1) staleness checks on the
-- render thread.  Do __not__ compare 'IntMap' fields for equality on hot
-- paths — use the version instead.
data TerrainSnapshot = TerrainSnapshot
  { tsVersion :: !Word64
  , tsChunkSize :: !Int
  , tsTerrainChunks :: !(IntMap TerrainChunk)
  , tsClimateChunks :: !(IntMap ClimateChunk)
  , tsWeatherChunks :: !(IntMap WeatherChunk)
  , tsRiverChunks :: !(IntMap RiverChunk)
  } deriving (Eq, Show)

data DataState = DataState
  { stTerrainCount :: !Int
  , stBiomeCount :: !Int
  , stLastSeed :: !(Maybe Word64)
  , stChunkSize :: !Int
  , stTerrainVersion :: !Word64
  , stTerrainChunks :: !(IntMap TerrainChunk)
  , stClimateChunks :: !(IntMap ClimateChunk)
  , stWeatherChunks :: !(IntMap WeatherChunk)
  , stRiverChunks :: !(IntMap RiverChunk)
  }

emptyDataState :: DataState
emptyDataState = DataState
  { stTerrainCount = 0
  , stBiomeCount = 0
  , stLastSeed = Nothing
  , stChunkSize = 0
  , stTerrainVersion = 0
  , stTerrainChunks = IntMap.empty
  , stClimateChunks = IntMap.empty
  , stWeatherChunks = IntMap.empty
  , stRiverChunks = IntMap.empty
  }

snapshotData :: DataState -> DataSnapshot
snapshotData st = DataSnapshot
  { dsTerrainChunks = stTerrainCount st
  , dsBiomeChunks = stBiomeCount st
  , dsLastSeed = stLastSeed st
  }

snapshotTerrain :: DataState -> TerrainSnapshot
snapshotTerrain st = TerrainSnapshot
  { tsVersion = stTerrainVersion st
  , tsChunkSize = stChunkSize st
  , tsTerrainChunks = stTerrainChunks st
  , tsClimateChunks = stClimateChunks st
  , tsWeatherChunks = stWeatherChunks st
  , tsRiverChunks = stRiverChunks st
  }

chunkKey :: ChunkId -> Int
chunkKey (ChunkId key) = key

dataSnapshotTag :: OpTag "dataSnapshot"
dataSnapshotTag = OpTag

terrainSnapshotTag :: OpTag "terrainSnapshot"
terrainSnapshotTag = OpTag

[hyperspace|
-- | Reply protocol for emitting data and terrain snapshots.
replyprotocol DataSnapshotReply =
  cast dataSnapshot :: DataSnapshot
  cast terrainSnapshot :: TerrainSnapshot

actor Data
  state DataState
  lifetime Singleton
  schedule pinned 3
  noDeps
  mailbox Unbounded

  cast setTerrainChunks :: Int
  cast setBiomeChunks :: Int
  cast setLastSeed :: Word64
  cast setTerrainData :: (Int, [(ChunkId, TerrainChunk)])
  cast setClimateData :: (Int, [(ChunkId, ClimateChunk)])
  cast setWeatherData :: (Int, [(ChunkId, WeatherChunk)])
  cast setRiverData :: (Int, [(ChunkId, RiverChunk)])
  cast snapshotAsync :: () reply DataSnapshotReply
  call snapshot :: () -> DataSnapshot
  call terrainSnapshot :: () -> TerrainSnapshot

  initial emptyDataState
  onPure_ setTerrainChunks = \count st -> st { stTerrainCount = count }
  onPure_ setBiomeChunks = \count st -> st { stBiomeCount = count }
  onPure_ setLastSeed = \seed st -> st { stLastSeed = Just seed }
  on_ setTerrainData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stTerrainVersion = stTerrainVersion st + 1
            , stTerrainChunks = m
            }
  on_ setClimateData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stClimateChunks = m
            }
  on_ setWeatherData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stWeatherChunks = m
            }
  on_ setRiverData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stRiverChunks = m
            }
  onReply snapshotAsync = \() replyTo st -> do
    replyCast replyTo dataSnapshotTag (snapshotData st)
    replyCast replyTo terrainSnapshotTag (snapshotTerrain st)
    pure st
  onPure snapshot = \() st -> (st, snapshotData st)
  onPure terrainSnapshot = \() st -> (st, snapshotTerrain st)
|]

setTerrainChunkCount :: ActorHandle Data (Protocol Data) -> Int -> IO ()
setTerrainChunkCount handle count =
  cast @"setTerrainChunks" handle #setTerrainChunks count

setBiomeChunkCount :: ActorHandle Data (Protocol Data) -> Int -> IO ()
setBiomeChunkCount handle count =
  cast @"setBiomeChunks" handle #setBiomeChunks count

setLastSeed :: ActorHandle Data (Protocol Data) -> Word64 -> IO ()
setLastSeed handle seed =
  cast @"setLastSeed" handle #setLastSeed seed

getDataSnapshot :: ActorHandle Data (Protocol Data) -> IO DataSnapshot
getDataSnapshot handle =
  call @"snapshot" handle #snapshot ()

setTerrainChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, TerrainChunk)] -> IO ()
setTerrainChunkData handle size chunks =
  cast @"setTerrainData" handle #setTerrainData (size, chunks)

setClimateChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, ClimateChunk)] -> IO ()
setClimateChunkData handle size chunks =
  cast @"setClimateData" handle #setClimateData (size, chunks)

setWeatherChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, WeatherChunk)] -> IO ()
setWeatherChunkData handle size chunks =
  cast @"setWeatherData" handle #setWeatherData (size, chunks)

-- | Send river chunk data to the Data actor.
setRiverChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, RiverChunk)] -> IO ()
setRiverChunkData handle size chunks =
  cast @"setRiverData" handle #setRiverData (size, chunks)

getTerrainSnapshot :: ActorHandle Data (Protocol Data) -> IO TerrainSnapshot
getTerrainSnapshot handle =
  call @"terrainSnapshot" handle #terrainSnapshot ()

-- | Request data and terrain snapshots via a reply-capable cast.
requestDataSnapshot :: ActorHandle Data (Protocol Data) -> ReplyTo DataSnapshotReply -> IO ()
requestDataSnapshot handle replyTo =
  castReply @"snapshotAsync" handle replyTo #snapshotAsync ()

-- | Replace all terrain data from a loaded 'TerrainWorld'.
--
-- Sends terrain, climate, weather, and river chunk data as four separate
-- messages. The terrain message increments 'tsVersion', triggering
-- atlas/cache invalidation.  Also sets the terrain chunk count so
-- the render pipeline's @dataReady@ guard passes.
replaceTerrainData :: ActorHandle Data (Protocol Data) -> TerrainWorld -> IO ()
replaceTerrainData handle world = do
  let size = wcChunkSize (twConfig world)
      toList m = map (\(k, v) -> (ChunkId k, v)) (IntMap.toList m)
  setTerrainChunkData handle size (toList (twTerrain world))
  setClimateChunkData handle size (toList (twClimate world))
  setWeatherChunkData handle size (toList (twWeather world))
  setRiverChunkData   handle size (toList (twRivers world))
  -- Update the chunk count so dsTerrainChunks matches the IntMap size
  -- and the render pipeline considers the data ready.
  setTerrainChunkCount handle (IntMap.size (twTerrain world))
