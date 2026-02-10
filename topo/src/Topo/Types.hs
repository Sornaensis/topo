{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}

module Topo.Types
  ( ChunkId(..)
  , ChunkCoord(..)
  , TileIndex(..)
  , TileCoord(..)
  , WorldConfig(..)
  , WorldConfigError(..)
  , mkWorldConfig
  , WorldExtent
  , WorldExtentError(..)
  , defaultWorldExtent
  , mkWorldExtent
  , mkWorldExtentSquare
  , worldExtentOrDefault
  , worldExtentSquareOrDefault
  , worldExtentRadiusX
  , worldExtentRadiusY
  , worldExtentRadii
  , HexCoord(..)
  , RegionId(..)
  , Region(..)
  , BiomeId
  , BiomeIdError(..)
  , biomeIdFromCode
  , biomeIdToCode
  , pattern BiomeDesert
  , pattern BiomeGrassland
  , pattern BiomeForest
  , pattern BiomeTundra
  , pattern BiomeRainforest
  , pattern BiomeShrubland
  , pattern BiomeSavanna
  , pattern BiomeTaiga
  , pattern BiomeSwamp
  , pattern BiomeOcean
  , pattern BiomeSnow
  , pattern BiomeBeach
  , pattern BiomeAlpine
  , WorldPos(..)
  , Vec3(..)
  , PlateId(..)
  , PlateCrust(..)
  , PlateBoundary
  , PlateBoundaryError(..)
  , plateBoundaryFromCode
  , plateBoundaryToCode
  , pattern PlateBoundaryNone
  , pattern PlateBoundaryConvergent
  , pattern PlateBoundaryDivergent
  , pattern PlateBoundaryTransform
  , VentType
  , VentTypeError(..)
  , ventTypeFromCode
  , ventTypeToCode
  , pattern VentNone
  , pattern VentShield
  , pattern VentStratovolcano
  , pattern VentFissure
  , VentActivity
  , VentActivityError(..)
  , ventActivityFromCode
  , ventActivityToCode
  , pattern VentDormant
  , pattern VentActive
  , pattern VentErupting
  , TerrainChunk(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , RiverChunk(..)
  , GroundwaterChunk(..)
  , VolcanismChunk(..)
  , GlacierChunk(..)
  , TerrainSample(..)
  , chunkTileCount
  , tileIndex
  , tileCoordFromIndex
  , chunkCoordFromTile
  , chunkIdFromCoord
  , chunkCoordFromId
  , chunkOriginTile
  , regionSize
  ) where

import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word16, Word32)
import qualified Data.Vector.Unboxed as U

newtype ChunkId = ChunkId Int
  deriving (Eq, Ord, Show)

data ChunkCoord = ChunkCoord !Int !Int
  deriving (Eq, Ord, Show)

newtype TileIndex = TileIndex Int
  deriving (Eq, Ord, Show)

data TileCoord = TileCoord !Int !Int
  deriving (Eq, Ord, Show)

data WorldConfig = WorldConfig
  { wcChunkSize :: !Int
  } deriving (Eq, Show)

-- | Errors produced when constructing invalid world configs.
data WorldConfigError
  = WorldConfigNonPositiveChunkSize
  deriving (Eq, Show)

-- | Construct a world config with a positive chunk size.
mkWorldConfig :: Int -> Either WorldConfigError WorldConfig
mkWorldConfig size
  | size <= 0 = Left WorldConfigNonPositiveChunkSize
  | otherwise = Right (WorldConfig size)

-- | Rectangular world extents expressed as chunk radii.
data WorldExtent = WorldExtent
  { weChunkRadiusX :: !Int
  , weChunkRadiusY :: !Int
  } deriving (Eq, Show)

-- | Errors produced when constructing invalid world extents.
data WorldExtentError
  = WorldExtentNegativeX
  | WorldExtentNegativeY
  deriving (Eq, Show)

-- | Default world extent matching the legacy square radius of 2.
defaultWorldExtent :: WorldExtent
defaultWorldExtent = WorldExtent 2 2

-- | Construct a world extent with validated chunk radii.
mkWorldExtent :: Int -> Int -> Either WorldExtentError WorldExtent
mkWorldExtent rx ry
  | rx < 0 = Left WorldExtentNegativeX
  | ry < 0 = Left WorldExtentNegativeY
  | otherwise = Right (WorldExtent rx ry)

-- | Construct a square world extent with validated radii.
mkWorldExtentSquare :: Int -> Either WorldExtentError WorldExtent
mkWorldExtentSquare r = mkWorldExtent r r

-- | Construct a world extent, falling back to the default on invalid input.
worldExtentOrDefault :: Int -> Int -> WorldExtent
worldExtentOrDefault rx ry =
  either (const defaultWorldExtent) id (mkWorldExtent rx ry)

-- | Construct a square world extent, falling back to the default on invalid input.
worldExtentSquareOrDefault :: Int -> WorldExtent
worldExtentSquareOrDefault r = worldExtentOrDefault r r

-- | X radius in chunks.
worldExtentRadiusX :: WorldExtent -> Int
worldExtentRadiusX (WorldExtent rx _) = rx

-- | Y radius in chunks.
worldExtentRadiusY :: WorldExtent -> Int
worldExtentRadiusY (WorldExtent _ ry) = ry

-- | Chunk radii as @(x, y)@.
worldExtentRadii :: WorldExtent -> (Int, Int)
worldExtentRadii (WorldExtent rx ry) = (rx, ry)

data HexCoord
  = HexAxial !Int !Int
  | HexCube !Int !Int !Int
  deriving (Eq, Ord, Show)

newtype RegionId = RegionId Int
  deriving (Eq, Ord, Show)

newtype BiomeId = BiomeId Word16
  deriving (Eq, Ord, Show)

derivingUnbox "BiomeId"
  [t| BiomeId -> Word16 |]
  [| \(BiomeId v) -> v |]
  [| BiomeId |]

data BiomeIdError
  = BiomeIdUnknownCode !Word16
  deriving (Eq, Show)

biomeIdFromCode :: Word16 -> Either BiomeIdError BiomeId
biomeIdFromCode code =
  case code of
    0 -> Right BiomeDesert
    1 -> Right BiomeGrassland
    2 -> Right BiomeForest
    3 -> Right BiomeTundra
    4 -> Right BiomeRainforest
    5 -> Right BiomeShrubland
    6 -> Right BiomeSavanna
    7 -> Right BiomeTaiga
    8 -> Right BiomeSwamp
    10 -> Right BiomeOcean
    11 -> Right BiomeSnow
    12 -> Right BiomeBeach
    13 -> Right BiomeAlpine
    _ -> Left (BiomeIdUnknownCode code)

biomeIdToCode :: BiomeId -> Word16
biomeIdToCode (BiomeId code) = code

pattern BiomeDesert :: BiomeId
pattern BiomeDesert = BiomeId 0

pattern BiomeGrassland :: BiomeId
pattern BiomeGrassland = BiomeId 1

pattern BiomeForest :: BiomeId
pattern BiomeForest = BiomeId 2

pattern BiomeTundra :: BiomeId
pattern BiomeTundra = BiomeId 3

pattern BiomeRainforest :: BiomeId
pattern BiomeRainforest = BiomeId 4

pattern BiomeShrubland :: BiomeId
pattern BiomeShrubland = BiomeId 5

pattern BiomeSavanna :: BiomeId
pattern BiomeSavanna = BiomeId 6

pattern BiomeTaiga :: BiomeId
pattern BiomeTaiga = BiomeId 7

pattern BiomeSwamp :: BiomeId
pattern BiomeSwamp = BiomeId 8

pattern BiomeOcean :: BiomeId
pattern BiomeOcean = BiomeId 10

pattern BiomeSnow :: BiomeId
pattern BiomeSnow = BiomeId 11

pattern BiomeBeach :: BiomeId
pattern BiomeBeach = BiomeId 12

pattern BiomeAlpine :: BiomeId
pattern BiomeAlpine = BiomeId 13

{-# COMPLETE BiomeDesert
  , BiomeGrassland
  , BiomeForest
  , BiomeTundra
  , BiomeRainforest
  , BiomeShrubland
  , BiomeSavanna
  , BiomeTaiga
  , BiomeSwamp
  , BiomeOcean
  , BiomeSnow
  , BiomeBeach
  , BiomeAlpine
  #-}

data Region = RegionRect !TileCoord !TileCoord
  deriving (Eq, Show)

data WorldPos = WorldPos
  { wpX :: !Float
  , wpY :: !Float
  } deriving (Eq, Show)

data Vec3 = Vec3 !Float !Float !Float
  deriving (Eq, Show)

newtype PlateId = PlateId Word16
  deriving (Eq, Ord, Show)

data PlateCrust
  = PlateOceanic
  | PlateContinental
  deriving (Eq, Show)

newtype PlateBoundary = PlateBoundary Word16
  deriving (Eq, Ord, Show)

derivingUnbox "PlateBoundary"
  [t| PlateBoundary -> Word16 |]
  [| \(PlateBoundary v) -> v |]
  [| PlateBoundary |]

data PlateBoundaryError
  = PlateBoundaryUnknownCode !Word16
  deriving (Eq, Show)

plateBoundaryFromCode :: Word16 -> Either PlateBoundaryError PlateBoundary
plateBoundaryFromCode code =
  case code of
    0 -> Right PlateBoundaryNone
    1 -> Right PlateBoundaryConvergent
    2 -> Right PlateBoundaryDivergent
    3 -> Right PlateBoundaryTransform
    _ -> Left (PlateBoundaryUnknownCode code)

plateBoundaryToCode :: PlateBoundary -> Word16
plateBoundaryToCode (PlateBoundary code) = code

pattern PlateBoundaryNone :: PlateBoundary
pattern PlateBoundaryNone = PlateBoundary 0

pattern PlateBoundaryConvergent :: PlateBoundary
pattern PlateBoundaryConvergent = PlateBoundary 1

pattern PlateBoundaryDivergent :: PlateBoundary
pattern PlateBoundaryDivergent = PlateBoundary 2

pattern PlateBoundaryTransform :: PlateBoundary
pattern PlateBoundaryTransform = PlateBoundary 3

{-# COMPLETE PlateBoundaryNone
  , PlateBoundaryConvergent
  , PlateBoundaryDivergent
  , PlateBoundaryTransform
  #-}

-- | Volcanic vent classification used in volcanism chunks.
newtype VentType = VentType Word16
  deriving (Eq, Ord, Show)

derivingUnbox "VentType"
  [t| VentType -> Word16 |]
  [| \(VentType v) -> v |]
  [| VentType |]

data VentTypeError
  = VentTypeUnknownCode !Word16
  deriving (Eq, Show)

ventTypeFromCode :: Word16 -> Either VentTypeError VentType
ventTypeFromCode code =
  case code of
    0 -> Right VentNone
    1 -> Right VentShield
    2 -> Right VentStratovolcano
    3 -> Right VentFissure
    _ -> Left (VentTypeUnknownCode code)

ventTypeToCode :: VentType -> Word16
ventTypeToCode (VentType code) = code

pattern VentNone :: VentType
pattern VentNone = VentType 0

pattern VentShield :: VentType
pattern VentShield = VentType 1

pattern VentStratovolcano :: VentType
pattern VentStratovolcano = VentType 2

pattern VentFissure :: VentType
pattern VentFissure = VentType 3

{-# COMPLETE VentNone
  , VentShield
  , VentStratovolcano
  , VentFissure
  #-}

-- | Volcanic activity state for a vent.
newtype VentActivity = VentActivity Word16
  deriving (Eq, Ord, Show)

derivingUnbox "VentActivity"
  [t| VentActivity -> Word16 |]
  [| \(VentActivity v) -> v |]
  [| VentActivity |]

data VentActivityError
  = VentActivityUnknownCode !Word16
  deriving (Eq, Show)

ventActivityFromCode :: Word16 -> Either VentActivityError VentActivity
ventActivityFromCode code =
  case code of
    0 -> Right VentDormant
    1 -> Right VentActive
    2 -> Right VentErupting
    _ -> Left (VentActivityUnknownCode code)

ventActivityToCode :: VentActivity -> Word16
ventActivityToCode (VentActivity code) = code

pattern VentDormant :: VentActivity
pattern VentDormant = VentActivity 0

pattern VentActive :: VentActivity
pattern VentActive = VentActivity 1

pattern VentErupting :: VentActivity
pattern VentErupting = VentActivity 2

{-# COMPLETE VentDormant
  , VentActive
  , VentErupting
  #-}

data TerrainChunk = TerrainChunk
  { tcElevation   :: !(U.Vector Float)
  , tcSlope       :: !(U.Vector Float)
  , tcCurvature   :: !(U.Vector Float)
  , tcHardness    :: !(U.Vector Float)
  , tcRockType    :: !(U.Vector Word16)
  , tcSoilType    :: !(U.Vector Word16)
  , tcSoilDepth   :: !(U.Vector Float)
  , tcMoisture    :: !(U.Vector Float)
  , tcFertility   :: !(U.Vector Float)
  , tcRoughness   :: !(U.Vector Float)
  , tcRockDensity :: !(U.Vector Float)
  , tcSoilGrain   :: !(U.Vector Float)
  , tcFlags       :: !(U.Vector BiomeId)
  , tcPlateId     :: !(U.Vector Word16)
  , tcPlateBoundary :: !(U.Vector PlateBoundary)
  , tcPlateHeight :: !(U.Vector Float)
  , tcPlateHardness :: !(U.Vector Float)
  , tcPlateCrust :: !(U.Vector Word16)
  , tcPlateAge :: !(U.Vector Float)
  , tcPlateVelX :: !(U.Vector Float)
  , tcPlateVelY :: !(U.Vector Float)
  } deriving (Eq, Show)

data ClimateChunk = ClimateChunk
  { ccTempAvg    :: !(U.Vector Float)
  , ccPrecipAvg  :: !(U.Vector Float)
  , ccWindDirAvg :: !(U.Vector Float)
  , ccWindSpdAvg :: !(U.Vector Float)
  } deriving (Eq, Show)

data WeatherChunk = WeatherChunk
  { wcTemp     :: !(U.Vector Float)
  , wcHumidity :: !(U.Vector Float)
  , wcWindDir  :: !(U.Vector Float)
  , wcWindSpd  :: !(U.Vector Float)
  , wcPressure :: !(U.Vector Float)
  , wcPrecip   :: !(U.Vector Float)
  } deriving (Eq, Show)

-- | Per-tile river routing outputs for a chunk.
data RiverChunk = RiverChunk
  { rcFlowAccum :: !(U.Vector Float)
  , rcDischarge :: !(U.Vector Float)
  , rcChannelDepth :: !(U.Vector Float)
  , rcRiverOrder :: !(U.Vector Word16)
  , rcBasinId :: !(U.Vector Word32)
  , rcBaseflow :: !(U.Vector Float)
  , rcErosionPotential :: !(U.Vector Float)
  , rcDepositPotential :: !(U.Vector Float)
  } deriving (Eq, Show)

-- | Basin-level groundwater estimates, expanded per tile for a chunk.
data GroundwaterChunk = GroundwaterChunk
  { gwStorage :: !(U.Vector Float)
  , gwRecharge :: !(U.Vector Float)
  , gwDischarge :: !(U.Vector Float)
  , gwBasinId :: !(U.Vector Word32)
  } deriving (Eq, Show)

-- | Per-tile volcanism outputs for a chunk.
data VolcanismChunk = VolcanismChunk
  { vcVentType :: !(U.Vector VentType)
  , vcActivity :: !(U.Vector VentActivity)
  , vcMagma :: !(U.Vector Float)
  , vcEruptionCount :: !(U.Vector Word16)
  , vcEruptedTotal :: !(U.Vector Float)
  , vcLavaPotential :: !(U.Vector Float)
  , vcAshPotential :: !(U.Vector Float)
  , vcDepositPotential :: !(U.Vector Float)
  } deriving (Eq, Show)

-- | Per-tile glacier outputs for a chunk.
data GlacierChunk = GlacierChunk
  { glSnowpack :: !(U.Vector Float)
  , glIceThickness :: !(U.Vector Float)
  , glMelt :: !(U.Vector Float)
  , glFlow :: !(U.Vector Float)
  , glErosionPotential :: !(U.Vector Float)
  , glDepositPotential :: !(U.Vector Float)
  } deriving (Eq, Show)

data TerrainSample = TerrainSample
  { tsElevation   :: !Float
  , tsSlope       :: !Float
  , tsCurvature   :: !Float
  , tsHardness    :: !Float
  , tsSoilDepth   :: !Float
  , tsMoisture    :: !Float
  , tsFertility   :: !Float
  , tsRoughness   :: !Float
  , tsRockDensity :: !Float
  , tsSoilGrain   :: !Float
  , tsTemperature :: !Float
  , tsHumidity    :: !Float
  , tsWindSpeed   :: !Float
  , tsPressure    :: !Float
  , tsPrecip      :: !Float
  } deriving (Eq, Show)

chunkTileCount :: WorldConfig -> Int
chunkTileCount config =
  let s = wcChunkSize config
  in s * s

tileIndex :: WorldConfig -> TileCoord -> Maybe TileIndex
tileIndex config (TileCoord x y)
  | x < 0 || y < 0 = Nothing
  | x >= size || y >= size = Nothing
  | otherwise = Just (TileIndex (y * size + x))
  where
    size = wcChunkSize config

tileCoordFromIndex :: WorldConfig -> TileIndex -> TileCoord
tileCoordFromIndex config (TileIndex i) =
  let size = wcChunkSize config
      x = i `mod` size
      y = i `div` size
  in TileCoord x y

chunkCoordFromTile :: WorldConfig -> TileCoord -> (ChunkCoord, TileCoord)
chunkCoordFromTile config (TileCoord x y) =
  let size = wcChunkSize config
      cx = x `div` size
      cy = y `div` size
      lx = x `mod` size
      ly = y `mod` size
  in (ChunkCoord cx cy, TileCoord lx ly)

chunkIdFromCoord :: ChunkCoord -> ChunkId
chunkIdFromCoord (ChunkCoord x y) =
  let zx = encodeZ x
      zy = encodeZ y
      pair = cantorPair zx zy
  in ChunkId pair

chunkCoordFromId :: ChunkId -> ChunkCoord
chunkCoordFromId (ChunkId z) =
  let (zx, zy) = cantorUnpair z
  in ChunkCoord (decodeZ zx) (decodeZ zy)

chunkOriginTile :: WorldConfig -> ChunkCoord -> TileCoord
chunkOriginTile config (ChunkCoord cx cy) =
  let size = wcChunkSize config
  in TileCoord (cx * size) (cy * size)

encodeZ :: Int -> Int
encodeZ n
  | n >= 0 = n * 2
  | otherwise = (-n * 2) - 1

cantorPair :: Int -> Int -> Int
cantorPair a b =
  let ai = toInteger a
      bi = toInteger b
      s = ai + bi
      z = (s * (s + 1)) `div` 2 + bi
  in fromInteger z

decodeZ :: Int -> Int
decodeZ n
  | even n = n `div` 2
  | otherwise = -((n + 1) `div` 2)

cantorUnpair :: Int -> (Int, Int)
cantorUnpair z =
  let zi = toInteger z
      w = floor ((sqrt (fromInteger (8 * zi + 1)) - 1) / 2 :: Double)
      t = (w * w + w) `div` 2
      b = zi - t
      a = w - b
  in (fromInteger a, fromInteger b)

regionSize :: Region -> (Int, Int)
regionSize (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) =
  let w = abs (x1 - x0) + 1
      h = abs (y1 - y0) + 1
  in (w, h)
