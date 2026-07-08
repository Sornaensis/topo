{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainGeoContext(..)
  , TerrainSnapshot(..)
  , defaultTerrainGeoContext
  , terrainGeoContextFromWorld
  , dataActorDef
  , setTerrainChunkCount
  , setBiomeChunkCount
  , setLastSeed
  , setTerrainGeoContextData
  , setTerrainChunkData
  , setClimateChunkData
  , setWeatherChunkData
  , setRiverChunkData
  , setGroundwaterChunkData
  , setVolcanismChunkData
  , setGlacierChunkData
  , setWaterBodyChunkData
  , setVegetationChunkData
  , updateTerrainChunkData
  , updateClimateChunkData
  , updateVegetationChunkData
  , setOverlayStoreData
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
import Topo (ChunkId(..), ClimateChunk, GlacierChunk, GroundwaterChunk, RiverChunk, TerrainChunk, VegetationChunk, VolcanismChunk, WaterBodyChunk, WeatherChunk, getWeatherFromOverlay)
import Topo.Calendar (WorldTime, defaultWorldTime)
import Topo.Hex (HexGridMeta, defaultHexGridMeta)
import Topo.Overlay (OverlayStore, emptyOverlayStore)
import Topo.Planet (PlanetConfig, WorldSlice, defaultPlanetConfig, defaultWorldSlice)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..))

data DataSnapshot = DataSnapshot
  { dsTerrainChunks :: !Int
  , dsBiomeChunks :: !Int
  , dsLastSeed :: !(Maybe Word64)
  } deriving (Eq, Show)

-- | Authoritative geographic and clock context for render-time derivations.
--
-- This is copied from the actor-owned 'TerrainWorld' rather than reconstructed
-- from mutable UI sliders, so render overlays and inspector tooltips share the
-- same planet, local slice, hex scale, and world time that produced the terrain.
data TerrainGeoContext = TerrainGeoContext
  { tgcPlanet :: !PlanetConfig
  , tgcSlice :: !WorldSlice
  , tgcHexGrid :: !HexGridMeta
  , tgcWorldTime :: !WorldTime
  } deriving (Eq, Show)

defaultTerrainGeoContext :: TerrainGeoContext
defaultTerrainGeoContext = TerrainGeoContext
  { tgcPlanet = defaultPlanetConfig
  , tgcSlice = defaultWorldSlice
  , tgcHexGrid = defaultHexGridMeta
  , tgcWorldTime = defaultWorldTime
  }

terrainGeoContextFromWorld :: TerrainWorld -> TerrainGeoContext
terrainGeoContextFromWorld world = TerrainGeoContext
  { tgcPlanet = twPlanet world
  , tgcSlice = twSlice world
  , tgcHexGrid = twHexGrid world
  , tgcWorldTime = twWorldTime world
  }

-- | Snapshot of terrain chunk data.
--
-- @tsVersion@ is a monotonic stamp used for O(1) staleness checks on the
-- render thread.  Do __not__ compare 'IntMap' fields for equality on hot
-- paths — use the version instead.
data TerrainSnapshot = TerrainSnapshot
  { tsVersion :: !Word64
    -- ^ Base terrain chunk version.  Only terrain chunk writes bump this stamp.
  , tsClimateVersion :: !Word64
  , tsWeatherVersion :: !Word64
  , tsVegetationVersion :: !Word64
  , tsOverlayVersion :: !Word64
  , tsChunkSize :: !Int
  , tsTerrainChunks :: !(IntMap TerrainChunk)
  , tsClimateChunks :: !(IntMap ClimateChunk)
  , tsWeatherChunks :: !(IntMap WeatherChunk)
  , tsRiverChunks :: !(IntMap RiverChunk)
  , tsGroundwaterChunks :: !(IntMap GroundwaterChunk)
  , tsVolcanismChunks :: !(IntMap VolcanismChunk)
  , tsGlacierChunks :: !(IntMap GlacierChunk)
  , tsWaterBodyChunks :: !(IntMap WaterBodyChunk)
  , tsVegetationChunks :: !(IntMap VegetationChunk)
  , tsOverlayStore :: !OverlayStore
  -- ^ Current overlay data for overlay field visualization.
  , tsGeoContext :: !TerrainGeoContext
  -- ^ Authoritative planet/slice/hex/time context copied from the terrain world.
  } deriving (Eq, Show)

data DataState = DataState
  { stTerrainCount :: !Int
  , stBiomeCount :: !Int
  , stLastSeed :: !(Maybe Word64)
  , stChunkSize :: !Int
  , stNextVersion :: !Word64
  , stTerrainVersion :: !Word64
  , stClimateVersion :: !Word64
  , stWeatherVersion :: !Word64
  , stVegetationVersion :: !Word64
  , stOverlayVersion :: !Word64
  , stGeoContext :: !TerrainGeoContext
  , stTerrainChunks :: !(IntMap TerrainChunk)
  , stClimateChunks :: !(IntMap ClimateChunk)
  , stWeatherChunks :: !(IntMap WeatherChunk)
  , stRiverChunks :: !(IntMap RiverChunk)
  , stGroundwaterChunks :: !(IntMap GroundwaterChunk)
  , stVolcanismChunks :: !(IntMap VolcanismChunk)
  , stGlacierChunks :: !(IntMap GlacierChunk)
  , stWaterBodyChunks :: !(IntMap WaterBodyChunk)
  , stVegetationChunks :: !(IntMap VegetationChunk)
  , stOverlayStore :: !OverlayStore
  }

emptyDataState :: DataState
emptyDataState = DataState
  { stTerrainCount = 0
  , stBiomeCount = 0
  , stLastSeed = Nothing
  , stChunkSize = 0
  , stNextVersion = 1
  , stTerrainVersion = 0
  , stClimateVersion = 0
  , stWeatherVersion = 0
  , stVegetationVersion = 0
  , stOverlayVersion = 0
  , stGeoContext = defaultTerrainGeoContext
  , stTerrainChunks = IntMap.empty
  , stClimateChunks = IntMap.empty
  , stWeatherChunks = IntMap.empty
  , stRiverChunks = IntMap.empty
  , stGroundwaterChunks = IntMap.empty
  , stVolcanismChunks = IntMap.empty
  , stGlacierChunks = IntMap.empty
  , stWaterBodyChunks = IntMap.empty
  , stVegetationChunks = IntMap.empty
  , stOverlayStore = emptyOverlayStore
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
  , tsClimateVersion = stClimateVersion st
  , tsWeatherVersion = stWeatherVersion st
  , tsVegetationVersion = stVegetationVersion st
  , tsOverlayVersion = stOverlayVersion st
  , tsChunkSize = stChunkSize st
  , tsTerrainChunks = stTerrainChunks st
  , tsClimateChunks = stClimateChunks st
  , tsWeatherChunks = stWeatherChunks st
  , tsRiverChunks = stRiverChunks st
  , tsGroundwaterChunks = stGroundwaterChunks st
  , tsVolcanismChunks = stVolcanismChunks st
  , tsGlacierChunks = stGlacierChunks st
  , tsWaterBodyChunks = stWaterBodyChunks st
  , tsVegetationChunks = stVegetationChunks st
  , tsOverlayStore = stOverlayStore st
  , tsGeoContext = stGeoContext st
  }

chunkKey :: ChunkId -> Int
chunkKey (ChunkId key) = key

nextVersion :: DataState -> (Word64, DataState)
nextVersion st =
  let version = stNextVersion st
  in (version, st { stNextVersion = version + 1 })

bumpTerrainVersion :: DataState -> DataState
bumpTerrainVersion st =
  let (version, st') = nextVersion st
  in st' { stTerrainVersion = version }

bumpClimateVersion :: DataState -> DataState
bumpClimateVersion st =
  let (version, st') = nextVersion st
  in st' { stClimateVersion = version }

bumpWeatherVersion :: DataState -> DataState
bumpWeatherVersion st =
  let (version, st') = nextVersion st
  in st' { stWeatherVersion = version }

bumpVegetationVersion :: DataState -> DataState
bumpVegetationVersion st =
  let (version, st') = nextVersion st
  in st' { stVegetationVersion = version }

bumpOverlayVersion :: DataState -> DataState
bumpOverlayVersion st =
  let (version, st') = nextVersion st
  in st' { stOverlayVersion = version }

[hyperspace|
actor Data
  state DataState
  lifetime Singleton
  schedule pinned 3
  noDeps
  mailbox Unbounded

  cast setTerrainChunks :: Int
  cast setBiomeChunks :: Int
  cast setLastSeed :: Word64
  cast setTerrainGeoContext :: TerrainGeoContext
  cast setTerrainData :: (Int, [(ChunkId, TerrainChunk)])
  cast setClimateData :: (Int, [(ChunkId, ClimateChunk)])
  cast setWeatherData :: (Int, [(ChunkId, WeatherChunk)])
  cast setRiverData :: (Int, [(ChunkId, RiverChunk)])
  cast setGroundwaterData :: (Int, [(ChunkId, GroundwaterChunk)])
  cast setVolcanismData :: (Int, [(ChunkId, VolcanismChunk)])
  cast setGlacierData :: (Int, [(ChunkId, GlacierChunk)])
  cast setWaterBodyData :: (Int, [(ChunkId, WaterBodyChunk)])
  cast setVegetationData :: (Int, [(ChunkId, VegetationChunk)])
  cast updateTerrainData :: (Int, IntMap TerrainChunk)
  cast updateClimateData :: (Int, IntMap ClimateChunk)
  cast updateVegetationData :: (Int, IntMap VegetationChunk)
  cast setOverlayStore :: OverlayStore
  call snapshot :: () -> DataSnapshot
  call terrainSnapshot :: () -> TerrainSnapshot

  initial emptyDataState
  onPure_ setTerrainChunks = \count st -> st { stTerrainCount = count }
  onPure_ setBiomeChunks = \count st -> st { stBiomeCount = count }
  onPure_ setLastSeed = \seed st -> st { stLastSeed = Just seed }
  onPure_ setTerrainGeoContext = \geo st -> st { stGeoContext = geo }
  on_ setTerrainData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    let st' = bumpTerrainVersion st
    pure st' { stChunkSize = size
             , stTerrainChunks = m
             }
  on_ setClimateData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    let st' = bumpClimateVersion st
    pure st' { stChunkSize = size
             , stClimateChunks = m
             }
  on_ setWeatherData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    let st' = bumpWeatherVersion st
    pure st' { stChunkSize = size
             , stWeatherChunks = m
             }
  on_ setRiverData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stRiverChunks = m
            }
  on_ setGroundwaterData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stGroundwaterChunks = m
            }
  on_ setVolcanismData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stVolcanismChunks = m
            }
  on_ setGlacierData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stGlacierChunks = m
            }
  on_ setWaterBodyData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    pure st { stChunkSize = size
            , stWaterBodyChunks = m
            }
  on_ setVegetationData = \(size, chunks) st -> do
    let m = IntMap.fromList (map (\(cid, chunk) -> (chunkKey cid, chunk)) chunks)
    _ <- evaluate (IntMap.size m)
    let st' = bumpVegetationVersion st
    pure st' { stChunkSize = size
             , stVegetationChunks = m
             }
  on_ updateTerrainData = \(size, chunks) st -> do
    if IntMap.null chunks
      then pure st
      else do
        _ <- evaluate (IntMap.size chunks)
        let st' = bumpTerrainVersion st
        pure st' { stChunkSize = size
                 , stTerrainChunks = IntMap.union chunks (stTerrainChunks st)
                 }
  on_ updateClimateData = \(size, chunks) st -> do
    if IntMap.null chunks
      then pure st
      else do
        _ <- evaluate (IntMap.size chunks)
        let st' = bumpClimateVersion st
        pure st' { stChunkSize = size
                 , stClimateChunks = IntMap.union chunks (stClimateChunks st)
                 }
  on_ updateVegetationData = \(size, chunks) st -> do
    if IntMap.null chunks
      then pure st
      else do
        _ <- evaluate (IntMap.size chunks)
        let st' = bumpVegetationVersion st
        pure st' { stChunkSize = size
                 , stVegetationChunks = IntMap.union chunks (stVegetationChunks st)
                 }
  onPure_ setOverlayStore = \store st ->
    if store == stOverlayStore st
      then st
      else (bumpOverlayVersion st) { stOverlayStore = store }
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

setTerrainGeoContextData :: ActorHandle Data (Protocol Data) -> TerrainGeoContext -> IO ()
setTerrainGeoContextData handle geo =
  cast @"setTerrainGeoContext" handle #setTerrainGeoContext geo

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

-- | Send groundwater chunk data to the Data actor.
setGroundwaterChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, GroundwaterChunk)] -> IO ()
setGroundwaterChunkData handle size chunks =
  cast @"setGroundwaterData" handle #setGroundwaterData (size, chunks)

-- | Send volcanism chunk data to the Data actor.
setVolcanismChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, VolcanismChunk)] -> IO ()
setVolcanismChunkData handle size chunks =
  cast @"setVolcanismData" handle #setVolcanismData (size, chunks)

-- | Send glacier chunk data to the Data actor.
setGlacierChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, GlacierChunk)] -> IO ()
setGlacierChunkData handle size chunks =
  cast @"setGlacierData" handle #setGlacierData (size, chunks)

-- | Send water-body chunk data to the Data actor.
setWaterBodyChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, WaterBodyChunk)] -> IO ()
setWaterBodyChunkData handle size chunks =
  cast @"setWaterBodyData" handle #setWaterBodyData (size, chunks)

-- | Send vegetation chunk data to the Data actor.
setVegetationChunkData :: ActorHandle Data (Protocol Data) -> Int -> [(ChunkId, VegetationChunk)] -> IO ()
setVegetationChunkData handle size chunks =
  cast @"setVegetationData" handle #setVegetationData (size, chunks)

-- | Merge changed terrain chunks into the current terrain map.
updateTerrainChunkData :: ActorHandle Data (Protocol Data) -> Int -> IntMap TerrainChunk -> IO ()
updateTerrainChunkData handle size chunks =
  cast @"updateTerrainData" handle #updateTerrainData (size, chunks)

-- | Merge changed climate chunks into the current climate map.
updateClimateChunkData :: ActorHandle Data (Protocol Data) -> Int -> IntMap ClimateChunk -> IO ()
updateClimateChunkData handle size chunks =
  cast @"updateClimateData" handle #updateClimateData (size, chunks)

-- | Merge changed vegetation chunks into the current vegetation map.
updateVegetationChunkData :: ActorHandle Data (Protocol Data) -> Int -> IntMap VegetationChunk -> IO ()
updateVegetationChunkData handle size chunks =
  cast @"updateVegetationData" handle #updateVegetationData (size, chunks)

-- | Replace the in-memory overlay store for overlay field visualization.
setOverlayStoreData :: ActorHandle Data (Protocol Data) -> OverlayStore -> IO ()
setOverlayStoreData handle store =
  cast @"setOverlayStore" handle #setOverlayStore store

getTerrainSnapshot :: ActorHandle Data (Protocol Data) -> IO TerrainSnapshot
getTerrainSnapshot handle =
  call @"terrainSnapshot" handle #terrainSnapshot ()

-- | Replace all terrain data from a loaded 'TerrainWorld'.
--
-- Sends terrain, climate, weather, river, groundwater, volcanism, glacier,
-- water-body, overlay, and vegetation data as separate messages. Each
-- render-facing layer bumps only its own version stamp; the terrain message
-- increments 'tsVersion', triggering base atlas/cache invalidation.  Also sets the terrain chunk count so
-- the render pipeline's @dataReady@ guard passes.
replaceTerrainData :: ActorHandle Data (Protocol Data) -> TerrainWorld -> IO ()
replaceTerrainData handle world = do
  let size = wcChunkSize (twConfig world)
      toList m = map (\(k, v) -> (ChunkId k, v)) (IntMap.toList m)
  setTerrainGeoContextData handle (terrainGeoContextFromWorld world)
  setTerrainChunkData handle size (toList (twTerrain world))
  setClimateChunkData handle size (toList (twClimate world))
  setWeatherChunkData handle size (toList (getWeatherFromOverlay world))
  setRiverChunkData   handle size (toList (twRivers world))
  setGroundwaterChunkData handle size (toList (twGroundwater world))
  setVolcanismChunkData handle size (toList (twVolcanism world))
  setGlacierChunkData handle size (toList (twGlaciers world))
  setWaterBodyChunkData handle size (toList (twWaterBodies world))
  setVegetationChunkData handle size (toList (twVegetation world))
  setOverlayStoreData handle (twOverlays world)
  -- Update the chunk count so dsTerrainChunks matches the IntMap size
  -- and the render pipeline considers the data ready.
  setTerrainChunkCount handle (IntMap.size (twTerrain world))
