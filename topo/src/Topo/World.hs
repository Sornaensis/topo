module Topo.World
  ( ChunkMap
  , TerrainWorld(..)
  , emptyWorld
  , emptyWorldWithPlanet
  , emptyTerrainChunk
  , generateTerrainChunk
  , emptyClimateChunk
  , emptyWeatherChunk
  , emptyRiverChunk
  , emptyGroundwaterChunk
  , emptyVolcanismChunk
  , emptyGlacierChunk
  , emptyVegetationChunk
  , ensureTerrainChunk
  , ensureClimateChunk
  , ensureWeatherChunk
  , ensureRiverChunk
  , ensureGroundwaterChunk
  , ensureVolcanismChunk
  , ensureGlacierChunk
  , ensureVegetationChunk
  , getChunk
  , updateChunk
  , mapChunks
  , putHexMetaWorld
  , getHexMetaWorld
  , putRegionMetaWorld
  , getRegionMetaWorld
  , migrateWorldMetadata
  , getElevationAt
  , setElevationAt
  , getTerrainChunk
  , setTerrainChunk
  , updateTerrainChunk
  , getClimateChunk
  , setClimateChunk
  , getWeatherChunk
  , setWeatherChunk
  , getRiverChunk
  , setRiverChunk
  , getGroundwaterChunk
  , setGroundwaterChunk
  , getVolcanismChunk
  , setVolcanismChunk
  , getGlacierChunk
  , setGlacierChunk
  , getVegetationChunk
  , setVegetationChunk
  , getWaterBodyChunk
  , setWaterBodyChunk
  , emptyWaterBodyChunk
  , ensureWaterBodyChunk
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Topo.Hex (HexGridMeta)
import Topo.Metadata (Metadata, MetadataMigration, MetadataStore, emptyMetadataStore, getHexMeta, getRegionMeta, migrateMetadataStore, putHexMeta, putRegionMeta)
import Data.Aeson (Value)
import Topo.Planet (LatitudeMapping, PlanetConfig, WorldSlice, defaultPlanetConfig, defaultWorldSlice, mkLatitudeMapping)
import Topo.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

type ChunkMap a = IntMap a

data TerrainWorld = TerrainWorld
  { twTerrain :: !(ChunkMap TerrainChunk)
  , twClimate :: !(ChunkMap ClimateChunk)
  , twWeather :: !(ChunkMap WeatherChunk)
  , twRivers :: !(ChunkMap RiverChunk)
  , twGroundwater :: !(ChunkMap GroundwaterChunk)
  , twVolcanism :: !(ChunkMap VolcanismChunk)
  , twGlaciers :: !(ChunkMap GlacierChunk)
  -- | Per-chunk water body classification (ocean vs lake vs inland sea).
  -- Produced by 'Topo.WaterBody.applyWaterBodyStage'.
  , twWaterBodies :: !(ChunkMap WaterBodyChunk)
  -- | Pre-climate vegetation bootstrap data produced by
  -- 'Topo.Vegetation.bootstrapVegetationStage'.  Stores vegetation cover
  -- fraction and surface albedo per tile, used by the climate stage for
  -- evapotranspiration and albedo–temperature feedback.
  , twVegetation :: !(ChunkMap VegetationChunk)
  , twHexGrid :: !HexGridMeta
  , twMeta    :: !MetadataStore
  , twConfig  :: !WorldConfig
  , twPlanet  :: !PlanetConfig
  , twSlice   :: !WorldSlice
  -- | Pre-computed latitude mapping derived from planet, slice, and
  -- world configuration.  Computed once at world creation; used by
  -- every pipeline stage that needs tile-Y → latitude conversion.
  , twLatMapping :: !LatitudeMapping
  -- | Elapsed simulation time in weather ticks.  Incremented by
  -- 'tickWeatherStage' each call; used for time-varying weather noise
  -- and seasonal phase computation.
  , twWorldTime :: !Float
  -- | The generation config used to produce this world, stored as a
  -- raw JSON 'Value'.  Persisted in the @.topo@ binary (version ≥ 14)
  -- as a length-prefixed JSON blob.  'Nothing' for worlds loaded from
  -- older file versions or constructed without a generation config.
  --
  -- Consumers that need a typed 'Topo.WorldGen.WorldGenConfig' should
  -- decode this value via @Data.Aeson.fromJSON@.
  , twGenConfig :: !(Maybe Value)
  }

emptyWorld :: WorldConfig -> HexGridMeta -> TerrainWorld
emptyWorld config hexMeta = emptyWorldWithPlanet config hexMeta defaultPlanetConfig defaultWorldSlice

-- | Create an empty world with explicit planet and slice parameters.
emptyWorldWithPlanet :: WorldConfig -> HexGridMeta -> PlanetConfig -> WorldSlice -> TerrainWorld
emptyWorldWithPlanet config hexMeta planet slice = TerrainWorld
  { twTerrain = IntMap.empty
  , twClimate = IntMap.empty
  , twWeather = IntMap.empty
  , twRivers = IntMap.empty
  , twGroundwater = IntMap.empty
  , twVolcanism = IntMap.empty
  , twGlaciers = IntMap.empty
  , twWaterBodies = IntMap.empty
  , twVegetation = IntMap.empty
  , twHexGrid = hexMeta
  , twMeta = emptyMetadataStore
  , twConfig = config
  , twPlanet = planet
  , twSlice = slice
  , twLatMapping = mkLatitudeMapping planet slice config
  , twWorldTime = 0
  , twGenConfig = Nothing
  }

emptyTerrainChunk :: WorldConfig -> TerrainChunk
emptyTerrainChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
      zeros16 = U.replicate n 0
      biomeZeros = U.replicate n BiomeDesert
      boundaryZeros = U.replicate n PlateBoundaryNone
  in TerrainChunk
      { tcElevation = zeros
      , tcSlope = zeros
      , tcCurvature = zeros
      , tcHardness = zeros
      , tcRockType = zeros16
      , tcSoilType = zeros16
      , tcSoilDepth = zeros
      , tcMoisture = zeros
      , tcFertility = zeros
      , tcRoughness = zeros
      , tcRockDensity = zeros
      , tcSoilGrain = zeros
      , tcRelief = zeros
      , tcRuggedness = zeros
      , tcTerrainForm = U.replicate n FormFlat
      , tcFlags = biomeZeros
    , tcPlateId = zeros16
    , tcPlateBoundary = boundaryZeros
    , tcPlateHeight = zeros
    , tcPlateHardness = zeros
    , tcPlateCrust = zeros16
    , tcPlateAge = zeros
    , tcPlateVelX = zeros
    , tcPlateVelY = zeros
      }

generateTerrainChunk :: WorldConfig -> (TileCoord -> Float) -> TerrainChunk
generateTerrainChunk config f =
  let n = chunkTileCount config
      mk = f . tileCoordFromIndex config . TileIndex
      elevation = U.generate n mk
      zeros = U.replicate n 0
      zeros16 = U.replicate n 0
      biomeZeros = U.replicate n BiomeDesert
      boundaryZeros = U.replicate n PlateBoundaryNone
  in TerrainChunk
      { tcElevation = elevation
      , tcSlope = zeros
      , tcCurvature = zeros
      , tcHardness = zeros
      , tcRockType = zeros16
      , tcSoilType = zeros16
      , tcSoilDepth = zeros
      , tcMoisture = zeros
      , tcFertility = zeros
      , tcRoughness = zeros
      , tcRockDensity = zeros
      , tcSoilGrain = zeros
      , tcRelief = zeros
      , tcRuggedness = zeros
      , tcTerrainForm = U.replicate n FormFlat
      , tcFlags = biomeZeros
    , tcPlateId = zeros16
    , tcPlateBoundary = boundaryZeros
    , tcPlateHeight = zeros
    , tcPlateHardness = zeros
    , tcPlateCrust = zeros16
    , tcPlateAge = zeros
    , tcPlateVelX = zeros
    , tcPlateVelY = zeros
      }

emptyClimateChunk :: WorldConfig -> ClimateChunk
emptyClimateChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
  in ClimateChunk
      { ccTempAvg = zeros
      , ccPrecipAvg = zeros
      , ccWindDirAvg = zeros
      , ccWindSpdAvg = zeros
      , ccHumidityAvg = zeros
      , ccTempRange = zeros
      , ccPrecipSeasonality = zeros
      }

emptyWeatherChunk :: WorldConfig -> WeatherChunk
emptyWeatherChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
  in WeatherChunk
      { wcTemp = zeros
      , wcHumidity = zeros
      , wcWindDir = zeros
      , wcWindSpd = zeros
      , wcPressure = zeros
      , wcPrecip = zeros
      }

emptyRiverChunk :: WorldConfig -> RiverChunk
emptyRiverChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
      zeros16 = U.replicate n 0
      zeros32 = U.replicate n 0
      sinkFlow = U.replicate n (-1 :: Int)
      emptyOffsets = U.replicate (n + 1) (0 :: Int)
  in RiverChunk
      { rcFlowAccum = zeros
      , rcDischarge = zeros
      , rcChannelDepth = zeros
      , rcRiverOrder = zeros16
      , rcBasinId = zeros32
      , rcBaseflow = zeros
      , rcErosionPotential = zeros
      , rcDepositPotential = zeros
      , rcFlowDir = sinkFlow
      , rcSegOffsets = emptyOffsets
      , rcSegEntryEdge = U.empty
      , rcSegExitEdge = U.empty
      , rcSegDischarge = U.empty
      , rcSegOrder = U.empty
      }

emptyGroundwaterChunk :: WorldConfig -> GroundwaterChunk
emptyGroundwaterChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
      zeros32 = U.replicate n 0
  in GroundwaterChunk
      { gwStorage = zeros
      , gwRecharge = zeros
      , gwDischarge = zeros
      , gwBasinId = zeros32
      }

emptyVolcanismChunk :: WorldConfig -> VolcanismChunk
emptyVolcanismChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
      zeros16 = U.replicate n 0
      ventTypes = U.replicate n VentNone
      activities = U.replicate n VentDormant
  in VolcanismChunk
      { vcVentType = ventTypes
      , vcActivity = activities
      , vcMagma = zeros
      , vcEruptionCount = zeros16
      , vcEruptedTotal = zeros
      , vcLavaPotential = zeros
      , vcAshPotential = zeros
      , vcDepositPotential = zeros
      }

emptyGlacierChunk :: WorldConfig -> GlacierChunk
emptyGlacierChunk config =
  let n = chunkTileCount config
      zeros = U.replicate n 0
  in GlacierChunk
      { glSnowpack = zeros
      , glIceThickness = zeros
      , glMelt = zeros
      , glFlow = zeros
      , glErosionPotential = zeros
      , glDepositPotential = zeros
      }

chunkKey :: ChunkId -> Int
chunkKey (ChunkId i) = i

getTerrainChunk :: ChunkId -> TerrainWorld -> Maybe TerrainChunk
getTerrainChunk cid world = IntMap.lookup (chunkKey cid) (twTerrain world)

setTerrainChunk :: ChunkId -> TerrainChunk -> TerrainWorld -> TerrainWorld
setTerrainChunk cid chunk world =
  world { twTerrain = IntMap.insert (chunkKey cid) chunk (twTerrain world) }

updateTerrainChunk :: ChunkId -> (TerrainChunk -> TerrainChunk) -> TerrainWorld -> TerrainWorld
updateTerrainChunk cid f world =
  world { twTerrain = IntMap.adjust f (chunkKey cid) (twTerrain world) }

getChunk :: ChunkId -> TerrainWorld -> Maybe TerrainChunk
getChunk = getTerrainChunk

updateChunk :: ChunkId -> (TerrainChunk -> TerrainChunk) -> TerrainWorld -> TerrainWorld
updateChunk = updateTerrainChunk

mapChunks :: (TerrainChunk -> TerrainChunk) -> TerrainWorld -> TerrainWorld
mapChunks f world =
  world { twTerrain = IntMap.map f (twTerrain world) }

putHexMetaWorld :: Metadata a => HexCoord -> a -> TerrainWorld -> TerrainWorld
putHexMetaWorld coord val world =
  world { twMeta = putHexMeta coord val (twMeta world) }

getHexMetaWorld :: Metadata a => HexCoord -> TerrainWorld -> Maybe a
getHexMetaWorld coord world =
  getHexMeta coord (twMeta world)

putRegionMetaWorld :: Metadata a => RegionId -> a -> TerrainWorld -> TerrainWorld
putRegionMetaWorld rid val world =
  world { twMeta = putRegionMeta rid val (twMeta world) }

getRegionMetaWorld :: Metadata a => RegionId -> TerrainWorld -> Maybe a
getRegionMetaWorld rid world =
  getRegionMeta rid (twMeta world)

migrateWorldMetadata :: [MetadataMigration] -> TerrainWorld -> TerrainWorld
migrateWorldMetadata migrations world =
  world { twMeta = migrateMetadataStore migrations (twMeta world) }

ensureTerrainChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureTerrainChunk cid world =
  case getTerrainChunk cid world of
    Just _ -> world
    Nothing -> setTerrainChunk cid (emptyTerrainChunk (twConfig world)) world

getClimateChunk :: ChunkId -> TerrainWorld -> Maybe ClimateChunk
getClimateChunk cid world = IntMap.lookup (chunkKey cid) (twClimate world)

setClimateChunk :: ChunkId -> ClimateChunk -> TerrainWorld -> TerrainWorld
setClimateChunk cid chunk world =
  world { twClimate = IntMap.insert (chunkKey cid) chunk (twClimate world) }

ensureClimateChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureClimateChunk cid world =
  case getClimateChunk cid world of
    Just _ -> world
    Nothing -> setClimateChunk cid (emptyClimateChunk (twConfig world)) world

getWeatherChunk :: ChunkId -> TerrainWorld -> Maybe WeatherChunk
getWeatherChunk cid world = IntMap.lookup (chunkKey cid) (twWeather world)

setWeatherChunk :: ChunkId -> WeatherChunk -> TerrainWorld -> TerrainWorld
setWeatherChunk cid chunk world =
  world { twWeather = IntMap.insert (chunkKey cid) chunk (twWeather world) }

ensureWeatherChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureWeatherChunk cid world =
  case getWeatherChunk cid world of
    Just _ -> world
    Nothing -> setWeatherChunk cid (emptyWeatherChunk (twConfig world)) world

getRiverChunk :: ChunkId -> TerrainWorld -> Maybe RiverChunk
getRiverChunk cid world = IntMap.lookup (chunkKey cid) (twRivers world)

setRiverChunk :: ChunkId -> RiverChunk -> TerrainWorld -> TerrainWorld
setRiverChunk cid chunk world =
  world { twRivers = IntMap.insert (chunkKey cid) chunk (twRivers world) }

ensureRiverChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureRiverChunk cid world =
  case getRiverChunk cid world of
    Just _ -> world
    Nothing -> setRiverChunk cid (emptyRiverChunk (twConfig world)) world

getGroundwaterChunk :: ChunkId -> TerrainWorld -> Maybe GroundwaterChunk
getGroundwaterChunk cid world = IntMap.lookup (chunkKey cid) (twGroundwater world)

setGroundwaterChunk :: ChunkId -> GroundwaterChunk -> TerrainWorld -> TerrainWorld
setGroundwaterChunk cid chunk world =
  world { twGroundwater = IntMap.insert (chunkKey cid) chunk (twGroundwater world) }

ensureGroundwaterChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureGroundwaterChunk cid world =
  case getGroundwaterChunk cid world of
    Just _ -> world
    Nothing -> setGroundwaterChunk cid (emptyGroundwaterChunk (twConfig world)) world

getVolcanismChunk :: ChunkId -> TerrainWorld -> Maybe VolcanismChunk
getVolcanismChunk cid world = IntMap.lookup (chunkKey cid) (twVolcanism world)

setVolcanismChunk :: ChunkId -> VolcanismChunk -> TerrainWorld -> TerrainWorld
setVolcanismChunk cid chunk world =
  world { twVolcanism = IntMap.insert (chunkKey cid) chunk (twVolcanism world) }

ensureVolcanismChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureVolcanismChunk cid world =
  case getVolcanismChunk cid world of
    Just _ -> world
    Nothing -> setVolcanismChunk cid (emptyVolcanismChunk (twConfig world)) world

getGlacierChunk :: ChunkId -> TerrainWorld -> Maybe GlacierChunk
getGlacierChunk cid world = IntMap.lookup (chunkKey cid) (twGlaciers world)

setGlacierChunk :: ChunkId -> GlacierChunk -> TerrainWorld -> TerrainWorld
setGlacierChunk cid chunk world =
  world { twGlaciers = IntMap.insert (chunkKey cid) chunk (twGlaciers world) }

ensureGlacierChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureGlacierChunk cid world =
  case getGlacierChunk cid world of
    Just _ -> world
    Nothing -> setGlacierChunk cid (emptyGlacierChunk (twConfig world)) world

-- | An empty vegetation chunk with zero cover, default bare-ground albedo,
-- and zero density.
emptyVegetationChunk :: WorldConfig -> VegetationChunk
emptyVegetationChunk config =
  let n = chunkTileCount config
  in VegetationChunk
      { vegCover   = U.replicate n 0
      , vegAlbedo  = U.replicate n 0.30
      , vegDensity = U.replicate n 0
      }

getVegetationChunk :: ChunkId -> TerrainWorld -> Maybe VegetationChunk
getVegetationChunk cid world = IntMap.lookup (chunkKey cid) (twVegetation world)

setVegetationChunk :: ChunkId -> VegetationChunk -> TerrainWorld -> TerrainWorld
setVegetationChunk cid chunk world =
  world { twVegetation = IntMap.insert (chunkKey cid) chunk (twVegetation world) }

ensureVegetationChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureVegetationChunk cid world =
  case getVegetationChunk cid world of
    Just _ -> world
    Nothing -> setVegetationChunk cid (emptyVegetationChunk (twConfig world)) world

-- | An empty water body chunk (all tiles classified as dry).
emptyWaterBodyChunk :: WorldConfig -> WaterBodyChunk
emptyWaterBodyChunk config =
  let n = chunkTileCount config
  in WaterBodyChunk
      { wbType         = U.replicate n WaterDry
      , wbSurfaceElev  = U.replicate n 0
      , wbBasinId      = U.replicate n 0
      , wbDepth        = U.replicate n 0
      , wbAdjacentType = U.replicate n WaterDry
      }

getWaterBodyChunk :: ChunkId -> TerrainWorld -> Maybe WaterBodyChunk
getWaterBodyChunk cid world = IntMap.lookup (chunkKey cid) (twWaterBodies world)

setWaterBodyChunk :: ChunkId -> WaterBodyChunk -> TerrainWorld -> TerrainWorld
setWaterBodyChunk cid chunk world =
  world { twWaterBodies = IntMap.insert (chunkKey cid) chunk (twWaterBodies world) }

ensureWaterBodyChunk :: ChunkId -> TerrainWorld -> TerrainWorld
ensureWaterBodyChunk cid world =
  case getWaterBodyChunk cid world of
    Just _ -> world
    Nothing -> setWaterBodyChunk cid (emptyWaterBodyChunk (twConfig world)) world

getElevationAt :: ChunkId -> TileCoord -> TerrainWorld -> Maybe Float
getElevationAt cid coord world = do
  chunk <- getTerrainChunk cid world
  TileIndex i <- tileIndex (twConfig world) coord
  pure (tcElevation chunk U.! i)

setElevationAt :: ChunkId -> TileCoord -> Float -> TerrainWorld -> TerrainWorld
setElevationAt cid coord value world =
  case tileIndex (twConfig world) coord of
    Nothing -> world
    Just idx ->
      let world' = ensureTerrainChunk cid world
      in updateTerrainChunk cid (updateElevation idx value) world'

updateElevation :: TileIndex -> Float -> TerrainChunk -> TerrainChunk
updateElevation (TileIndex i) value chunk =
  let vec = tcElevation chunk
      vec' = U.modify (\mvec -> UM.write mvec i value) vec
  in chunk { tcElevation = vec' }
