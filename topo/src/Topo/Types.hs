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
  , pattern BiomeCoastal
  , pattern BiomeAlpine
  -- Sub-biome pattern synonyms (codes 14–60)
  , pattern BiomeTropicalDryForest
  , pattern BiomeTempDeciduousForest
  , pattern BiomeTempConiferousForest
  , pattern BiomeSteppe
  , pattern BiomeMediterranean
  , pattern BiomeWetland
  , pattern BiomeMontaneForest
  , pattern BiomeIceCap
  , pattern BiomeTropicalSavanna
  , pattern BiomeBorealForest
  , pattern BiomeSaltMarsh
  , pattern BiomeCoastalDunes
  , pattern BiomeMangrove
  , pattern BiomeEstuary
  , pattern BiomeRockyShore
  , pattern BiomeCoastalScrub
  , pattern BiomeHotDesert
  , pattern BiomeColdDesert
  , pattern BiomeRockyDesert
  , pattern BiomeSandDesert
  , pattern BiomeSaltFlat
  , pattern BiomePrairie
  , pattern BiomeAlpineMeadow
  , pattern BiomeFloodplainGrassland
  , pattern BiomeCloudForest
  , pattern BiomeTempRainforest
  , pattern BiomeDeepOcean
  , pattern BiomeShallowSea
  , pattern BiomeCoralReef
  , pattern BiomeArcticTundra
  , pattern BiomeAlpineTundra
  , pattern BiomePolarDesert
  , pattern BiomeTropicalRainforest
  , pattern BiomeXericShrubland
  , pattern BiomeMoorland
  , pattern BiomeWoodlandSavanna
  , pattern BiomeGrasslandSavanna
  , pattern BiomeBorealBog
  , pattern BiomeMarsh
  , pattern BiomeBog
  , pattern BiomeFen
  , pattern BiomeFloodplainForest
  , pattern BiomeGlacier
  , pattern BiomeSnowfield
  , pattern BiomeAlpineScree
  , pattern BiomeLavaField
  , pattern BiomeVolcanicAshPlain
  -- Water body biome codes (61–62)
  , pattern BiomeLake
  , pattern BiomeInlandSea
  , WaterBodyType
  , WaterBodyTypeError(..)
  , waterBodyFromCode
  , waterBodyToCode
  , pattern WaterDry
  , pattern WaterOcean
  , pattern WaterLake
  , pattern WaterInlandSea
  , WaterBodyChunk(..)
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
  , TerrainForm
  , TerrainFormError(..)
  , terrainFormFromCode
  , terrainFormToCode
  , pattern FormFlat
  , pattern FormRolling
  , pattern FormHilly
  , pattern FormMountainous
  , pattern FormCliff
  , pattern FormValley
  , pattern FormDepression
  , TerrainChunk(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , RiverChunk(..)
  , GroundwaterChunk(..)
  , VolcanismChunk(..)
  , GlacierChunk(..)
  , VegetationChunk(..)
  , TerrainSample(..)
  -- * Hex edges
  , HexEdge(..)
  , hexEdgeFromCode
  , hexEdgeToCode
  , pattern EdgeE
  , pattern EdgeNE
  , pattern EdgeNW
  , pattern EdgeW
  , pattern EdgeSW
  , pattern EdgeSE
  , pattern EdgeNone
  -- * River sizes
  , RiverSize(..)
  , riverSizeFromCode
  , riverSizeToCode
  , pattern RiverStream
  , pattern RiverCreek
  , pattern RiverRiver
  , pattern RiverMajor
  , riverSizeFromOrder
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
import Data.Word (Word8, Word16, Word32)
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
    12 -> Right BiomeCoastal
    13 -> Right BiomeAlpine
    14 -> Right BiomeTropicalDryForest
    15 -> Right BiomeTempDeciduousForest
    16 -> Right BiomeTempConiferousForest
    17 -> Right BiomeSteppe
    18 -> Right BiomeMediterranean
    19 -> Right BiomeWetland
    20 -> Right BiomeMontaneForest
    21 -> Right BiomeIceCap
    22 -> Right BiomeTropicalSavanna
    23 -> Right BiomeBorealForest
    24 -> Right BiomeSaltMarsh
    25 -> Right BiomeCoastalDunes
    26 -> Right BiomeMangrove
    27 -> Right BiomeEstuary
    28 -> Right BiomeRockyShore
    29 -> Right BiomeCoastalScrub
    30 -> Right BiomeHotDesert
    31 -> Right BiomeColdDesert
    32 -> Right BiomeRockyDesert
    33 -> Right BiomeSandDesert
    34 -> Right BiomeSaltFlat
    35 -> Right BiomePrairie
    36 -> Right BiomeAlpineMeadow
    37 -> Right BiomeFloodplainGrassland
    38 -> Right BiomeCloudForest
    39 -> Right BiomeTempRainforest
    40 -> Right BiomeDeepOcean
    41 -> Right BiomeShallowSea
    42 -> Right BiomeCoralReef
    43 -> Right BiomeArcticTundra
    44 -> Right BiomeAlpineTundra
    45 -> Right BiomePolarDesert
    46 -> Right BiomeTropicalRainforest
    47 -> Right BiomeXericShrubland
    48 -> Right BiomeMoorland
    49 -> Right BiomeWoodlandSavanna
    50 -> Right BiomeGrasslandSavanna
    51 -> Right BiomeBorealBog
    52 -> Right BiomeMarsh
    53 -> Right BiomeBog
    54 -> Right BiomeFen
    55 -> Right BiomeFloodplainForest
    56 -> Right BiomeGlacier
    57 -> Right BiomeSnowfield
    58 -> Right BiomeAlpineScree
    59 -> Right BiomeLavaField
    60 -> Right BiomeVolcanicAshPlain
    61 -> Right BiomeLake
    62 -> Right BiomeInlandSea
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

pattern BiomeCoastal :: BiomeId
pattern BiomeCoastal = BiomeId 12

pattern BiomeAlpine :: BiomeId
pattern BiomeAlpine = BiomeId 13

-- | Sub-biome pattern synonyms (codes 14–60).
-- These are refinement sub-types of the primary (family-level) biomes above.

-- Forest sub-biomes
pattern BiomeTropicalDryForest :: BiomeId
pattern BiomeTropicalDryForest = BiomeId 14

pattern BiomeTempDeciduousForest :: BiomeId
pattern BiomeTempDeciduousForest = BiomeId 15

pattern BiomeTempConiferousForest :: BiomeId
pattern BiomeTempConiferousForest = BiomeId 16

-- Grassland sub-biomes
pattern BiomeSteppe :: BiomeId
pattern BiomeSteppe = BiomeId 17

-- Shrubland sub-biomes
pattern BiomeMediterranean :: BiomeId
pattern BiomeMediterranean = BiomeId 18

-- Swamp/Wetland sub-biomes
pattern BiomeWetland :: BiomeId
pattern BiomeWetland = BiomeId 19

-- Forest sub-biomes (continued)
pattern BiomeMontaneForest :: BiomeId
pattern BiomeMontaneForest = BiomeId 20

-- Snow/Ice sub-biomes
pattern BiomeIceCap :: BiomeId
pattern BiomeIceCap = BiomeId 21

-- Savanna sub-biomes
pattern BiomeTropicalSavanna :: BiomeId
pattern BiomeTropicalSavanna = BiomeId 22

-- Taiga/Boreal sub-biomes
pattern BiomeBorealForest :: BiomeId
pattern BiomeBorealForest = BiomeId 23

-- Coastal sub-biomes
pattern BiomeSaltMarsh :: BiomeId
pattern BiomeSaltMarsh = BiomeId 24

pattern BiomeCoastalDunes :: BiomeId
pattern BiomeCoastalDunes = BiomeId 25

pattern BiomeMangrove :: BiomeId
pattern BiomeMangrove = BiomeId 26

pattern BiomeEstuary :: BiomeId
pattern BiomeEstuary = BiomeId 27

pattern BiomeRockyShore :: BiomeId
pattern BiomeRockyShore = BiomeId 28

pattern BiomeCoastalScrub :: BiomeId
pattern BiomeCoastalScrub = BiomeId 29

-- Desert sub-biomes
pattern BiomeHotDesert :: BiomeId
pattern BiomeHotDesert = BiomeId 30

pattern BiomeColdDesert :: BiomeId
pattern BiomeColdDesert = BiomeId 31

pattern BiomeRockyDesert :: BiomeId
pattern BiomeRockyDesert = BiomeId 32

pattern BiomeSandDesert :: BiomeId
pattern BiomeSandDesert = BiomeId 33

pattern BiomeSaltFlat :: BiomeId
pattern BiomeSaltFlat = BiomeId 34

-- Grassland sub-biomes (continued)
pattern BiomePrairie :: BiomeId
pattern BiomePrairie = BiomeId 35

pattern BiomeAlpineMeadow :: BiomeId
pattern BiomeAlpineMeadow = BiomeId 36

pattern BiomeFloodplainGrassland :: BiomeId
pattern BiomeFloodplainGrassland = BiomeId 37

-- Forest sub-biomes (continued)
pattern BiomeCloudForest :: BiomeId
pattern BiomeCloudForest = BiomeId 38

pattern BiomeTempRainforest :: BiomeId
pattern BiomeTempRainforest = BiomeId 39

-- Ocean sub-biomes
pattern BiomeDeepOcean :: BiomeId
pattern BiomeDeepOcean = BiomeId 40

pattern BiomeShallowSea :: BiomeId
pattern BiomeShallowSea = BiomeId 41

pattern BiomeCoralReef :: BiomeId
pattern BiomeCoralReef = BiomeId 42

-- Tundra sub-biomes
pattern BiomeArcticTundra :: BiomeId
pattern BiomeArcticTundra = BiomeId 43

pattern BiomeAlpineTundra :: BiomeId
pattern BiomeAlpineTundra = BiomeId 44

pattern BiomePolarDesert :: BiomeId
pattern BiomePolarDesert = BiomeId 45

-- Rainforest sub-biomes
pattern BiomeTropicalRainforest :: BiomeId
pattern BiomeTropicalRainforest = BiomeId 46

-- Shrubland sub-biomes (continued)
pattern BiomeXericShrubland :: BiomeId
pattern BiomeXericShrubland = BiomeId 47

pattern BiomeMoorland :: BiomeId
pattern BiomeMoorland = BiomeId 48

-- Savanna sub-biomes (continued)
pattern BiomeWoodlandSavanna :: BiomeId
pattern BiomeWoodlandSavanna = BiomeId 49

pattern BiomeGrasslandSavanna :: BiomeId
pattern BiomeGrasslandSavanna = BiomeId 50

-- Taiga/Boreal sub-biomes (continued)
pattern BiomeBorealBog :: BiomeId
pattern BiomeBorealBog = BiomeId 51

-- Swamp/Wetland sub-biomes (continued)
pattern BiomeMarsh :: BiomeId
pattern BiomeMarsh = BiomeId 52

pattern BiomeBog :: BiomeId
pattern BiomeBog = BiomeId 53

pattern BiomeFen :: BiomeId
pattern BiomeFen = BiomeId 54

pattern BiomeFloodplainForest :: BiomeId
pattern BiomeFloodplainForest = BiomeId 55

-- Snow/Ice sub-biomes (continued)
pattern BiomeGlacier :: BiomeId
pattern BiomeGlacier = BiomeId 56

pattern BiomeSnowfield :: BiomeId
pattern BiomeSnowfield = BiomeId 57

-- Alpine sub-biomes
pattern BiomeAlpineScree :: BiomeId
pattern BiomeAlpineScree = BiomeId 58

-- Volcanic family
pattern BiomeLavaField :: BiomeId
pattern BiomeLavaField = BiomeId 59

pattern BiomeVolcanicAshPlain :: BiomeId
pattern BiomeVolcanicAshPlain = BiomeId 60

-- Water body biome codes
pattern BiomeLake :: BiomeId
pattern BiomeLake = BiomeId 61

pattern BiomeInlandSea :: BiomeId
pattern BiomeInlandSea = BiomeId 62

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
  , BiomeCoastal
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

-- | Discrete terrain shape classification derived from slope, relief,
-- and curvature.  Used by biome refinement to distinguish mountain peaks
-- from plateaus, valleys from plains, etc.
newtype TerrainForm = TerrainForm Word8
  deriving (Eq, Ord, Show)

derivingUnbox "TerrainForm"
  [t| TerrainForm -> Word8 |]
  [| \(TerrainForm v) -> v |]
  [| TerrainForm |]

data TerrainFormError
  = TerrainFormUnknownCode !Word8
  deriving (Eq, Show)

terrainFormFromCode :: Word8 -> Either TerrainFormError TerrainForm
terrainFormFromCode code =
  case code of
    0 -> Right FormFlat
    1 -> Right FormRolling
    2 -> Right FormHilly
    3 -> Right FormMountainous
    4 -> Right FormCliff
    5 -> Right FormValley
    6 -> Right FormDepression
    _ -> Left (TerrainFormUnknownCode code)

terrainFormToCode :: TerrainForm -> Word8
terrainFormToCode (TerrainForm code) = code

-- | Flat terrain — slope < 0.02, relief < 0.05
pattern FormFlat :: TerrainForm
pattern FormFlat = TerrainForm 0

-- | Rolling terrain — slope 0.02–0.08, relief < 0.10
pattern FormRolling :: TerrainForm
pattern FormRolling = TerrainForm 1

-- | Hilly terrain — slope 0.08–0.20, relief 0.10–0.25
pattern FormHilly :: TerrainForm
pattern FormHilly = TerrainForm 2

-- | Mountainous terrain — slope > 0.20 or relief > 0.25
pattern FormMountainous :: TerrainForm
pattern FormMountainous = TerrainForm 3

-- | Cliff — slope > 0.40
pattern FormCliff :: TerrainForm
pattern FormCliff = TerrainForm 4

-- | Valley — curvature strongly negative (concave)
pattern FormValley :: TerrainForm
pattern FormValley = TerrainForm 5

-- | Depression — local minimum (all neighbors higher)
pattern FormDepression :: TerrainForm
pattern FormDepression = TerrainForm 6

{-# COMPLETE FormFlat
  , FormRolling
  , FormHilly
  , FormMountainous
  , FormCliff
  , FormValley
  , FormDepression
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
  , tcRelief      :: !(U.Vector Float)
  , tcRuggedness  :: !(U.Vector Float)
  , tcTerrainForm :: !(U.Vector TerrainForm)
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
  -- | Annual-average relative humidity (0–1).
  --
  -- Computed analytically as @clamp01(precip / satNorm(temp))@ during
  -- 'Topo.Climate.generateClimateStage'.
  , ccHumidityAvg :: !(U.Vector Float)
  -- | Annual temperature range (0–1 normalised).
  --
  -- Derived from the seasonal amplitude formula:
  -- @clamp01(tempAvg + amp × |sin lat|) - clamp01(tempAvg - amp × |sin lat|)@.
  -- High values indicate continental climates; low values maritime.
  , ccTempRange :: !(U.Vector Float)
  -- | Precipitation seasonality index (0–1).
  --
  -- @0@ = no seasonality (year-round uniform rain).
  -- @1@ = extreme seasonality (monsoon, Mediterranean).
  -- Derived from the seasonal precipitation factor extremes.
  , ccPrecipSeasonality :: !(U.Vector Float)
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
  -- | Flow direction per tile: index of downstream neighbor in the
  -- chunk-local flat grid, or @-1@ for sinks.
  , rcFlowDir :: !(U.Vector Int)
  -- | Offset index into the segment arrays.  Tile @i@ owns segments
  -- @[rcSegOffsets ! i .. rcSegOffsets ! (i+1))@.  Length = tileCount + 1.
  , rcSegOffsets :: !(U.Vector Int)
  -- | Entry hex-edge per segment (255 = source within hex).
  , rcSegEntryEdge :: !(U.Vector Word8)
  -- | Exit hex-edge per segment (255 = sink/lake within hex).
  , rcSegExitEdge :: !(U.Vector Word8)
  -- | Discharge at each segment.
  , rcSegDischarge :: !(U.Vector Float)
  -- | Strahler order at each segment.
  , rcSegOrder :: !(U.Vector Word16)
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

-- ---------------------------------------------------------------------------
-- Vegetation bootstrap
-- ---------------------------------------------------------------------------

-- | Pre-climate vegetation bootstrap data.
--
-- Computed before the climate stage by 'Topo.Vegetation.bootstrapVegetationStage'
-- so that land evapotranspiration has vegetation cover data to work with.
-- 'vegCover' represents the 0–1 fraction of vegetation coverage; 'vegAlbedo'
-- is the surface albedo (0–1) derived from cover and surface type.
data VegetationChunk = VegetationChunk
  { vegCover   :: !(U.Vector Float)
    -- ^ 0–1 vegetation cover fraction (0 = barren, 1 = dense canopy).
  , vegAlbedo  :: !(U.Vector Float)
    -- ^ 0–1 surface albedo for temperature feedback.
    -- Ocean ≈ 0.06, dense vegetation ≈ 0.12, bare ground ≈ 0.30, ice ≈ 0.80.
  , vegDensity :: !(U.Vector Float)
    -- ^ 0–1 biome-derived vegetation density.
    -- Distinct from 'vegCover': density reflects climatic suitability
    -- combined with biome classification, while cover represents the
    -- canopy fraction derived from bootstrap and biome feedback.
    -- Computed by 'Topo.BiomeConfig.classifyBiomesStage'.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Water body classification
-- ---------------------------------------------------------------------------

-- | Classification of a tile's relationship to water bodies.
--
-- Used to distinguish ocean-connected water from landlocked freshwater
-- bodies (lakes, inland seas) so that biome refinement can guard coral
-- reefs against freshwater and classify lake / inland-sea tiles.
newtype WaterBodyType = WaterBodyType Word8
  deriving (Eq, Ord, Show)

derivingUnbox "WaterBodyType"
  [t| WaterBodyType -> Word8 |]
  [| \(WaterBodyType v) -> v |]
  [| WaterBodyType |]

data WaterBodyTypeError
  = WaterBodyTypeUnknownCode !Word8
  deriving (Eq, Show)

-- | Decode a raw code to a 'WaterBodyType'.
waterBodyFromCode :: Word8 -> Either WaterBodyTypeError WaterBodyType
waterBodyFromCode code =
  case code of
    0 -> Right WaterDry
    1 -> Right WaterOcean
    2 -> Right WaterLake
    3 -> Right WaterInlandSea
    _ -> Left (WaterBodyTypeUnknownCode code)

-- | Encode a 'WaterBodyType' to its raw code.
waterBodyToCode :: WaterBodyType -> Word8
waterBodyToCode (WaterBodyType code) = code

-- | Tile is above the local water surface (dry land).
pattern WaterDry :: WaterBodyType
pattern WaterDry = WaterBodyType 0

-- | Tile is submerged and connected to the global ocean.
pattern WaterOcean :: WaterBodyType
pattern WaterOcean = WaterBodyType 1

-- | Tile is submerged in a landlocked freshwater body (small basin).
pattern WaterLake :: WaterBodyType
pattern WaterLake = WaterBodyType 2

-- | Tile is submerged in a large landlocked freshwater body.
pattern WaterInlandSea :: WaterBodyType
pattern WaterInlandSea = WaterBodyType 3

{-# COMPLETE WaterDry
  , WaterOcean
  , WaterLake
  , WaterInlandSea
  #-}

-- | Per-chunk water body classification outputs.
--
-- Produced by 'Topo.WaterBody.classifyWaterBodies'.  Each tile receives:
-- a water body type (dry / ocean / lake / inland sea), the water surface
-- elevation of its basin, the basin identifier, and the depth below that
-- surface.
data WaterBodyChunk = WaterBodyChunk
  { wbType       :: !(U.Vector WaterBodyType)
    -- ^ Per-tile classification.
  , wbSurfaceElev :: !(U.Vector Float)
    -- ^ Water surface elevation (ocean = waterLevel; lake = pour-point;
    -- dry = 0).
  , wbBasinId     :: !(U.Vector Word32)
    -- ^ Which water body this tile belongs to.
  , wbDepth       :: !(U.Vector Float)
    -- ^ Depth below water surface (0 for dry tiles).
  , wbAdjacentType :: !(U.Vector WaterBodyType)
    -- ^ Highest-priority water body type among this tile's 4-neighbours.
    -- For submerged tiles this equals 'wbType'; for land tiles it is
    -- the most significant adjacent water type with priority:
    -- @WaterOcean > WaterInlandSea > WaterLake > WaterDry@.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Hex edges
-- ---------------------------------------------------------------------------

-- | Hex edge index [0..5] for a regular hexagon.
-- 0 = E, 1 = NE, 2 = NW, 3 = W, 4 = SW, 5 = SE
-- (matching flat-top hex convention used by the renderer).
-- 255 = no edge (source / sink within hex).
newtype HexEdge = HexEdge Word8
  deriving (Eq, Ord, Show)

derivingUnbox "HexEdge"
  [t| HexEdge -> Word8 |]
  [| \(HexEdge v) -> v |]
  [| HexEdge |]

-- | Validate a raw code to a 'HexEdge'.
hexEdgeFromCode :: Word8 -> Maybe HexEdge
hexEdgeFromCode code
  | code <= 5 || code == 255 = Just (HexEdge code)
  | otherwise = Nothing

-- | Extract the raw code from a 'HexEdge'.
hexEdgeToCode :: HexEdge -> Word8
hexEdgeToCode (HexEdge code) = code

-- | East edge (0).
pattern EdgeE :: HexEdge
pattern EdgeE = HexEdge 0

-- | North-east edge (1).
pattern EdgeNE :: HexEdge
pattern EdgeNE = HexEdge 1

-- | North-west edge (2).
pattern EdgeNW :: HexEdge
pattern EdgeNW = HexEdge 2

-- | West edge (3).
pattern EdgeW :: HexEdge
pattern EdgeW = HexEdge 3

-- | South-west edge (4).
pattern EdgeSW :: HexEdge
pattern EdgeSW = HexEdge 4

-- | South-east edge (5).
pattern EdgeSE :: HexEdge
pattern EdgeSE = HexEdge 5

-- | No edge — river source or sink within the hex.
pattern EdgeNone :: HexEdge
pattern EdgeNone = HexEdge 255

-- ---------------------------------------------------------------------------
-- River sizes
-- ---------------------------------------------------------------------------

-- | Visual / classification size of a river segment.
newtype RiverSize = RiverSize Word8
  deriving (Eq, Ord, Show)

derivingUnbox "RiverSize"
  [t| RiverSize -> Word8 |]
  [| \(RiverSize v) -> v |]
  [| RiverSize |]

-- | Validate a raw code to a 'RiverSize'.
riverSizeFromCode :: Word8 -> Maybe RiverSize
riverSizeFromCode code
  | code <= 3 = Just (RiverSize code)
  | otherwise = Nothing

-- | Extract the raw code from a 'RiverSize'.
riverSizeToCode :: RiverSize -> Word8
riverSizeToCode (RiverSize code) = code

-- | Strahler order 1–2: thin line.
pattern RiverStream :: RiverSize
pattern RiverStream = RiverSize 0

-- | Strahler order 3: medium line.
pattern RiverCreek :: RiverSize
pattern RiverCreek = RiverSize 1

-- | Strahler order 4–5: wide line.
pattern RiverRiver :: RiverSize
pattern RiverRiver = RiverSize 2

-- | Strahler order 6+: very wide ribbon.
pattern RiverMajor :: RiverSize
pattern RiverMajor = RiverSize 3

-- | Classify a Strahler order into a visual 'RiverSize'.
riverSizeFromOrder :: Word16 -> RiverSize
riverSizeFromOrder o
  | o <= 2    = RiverStream
  | o == 3    = RiverCreek
  | o <= 5    = RiverRiver
  | otherwise = RiverMajor

-- ---------------------------------------------------------------------------
-- Terrain sample
-- ---------------------------------------------------------------------------

data TerrainSample = TerrainSample
  { tsElevation     :: !Float
  , tsSlope         :: !Float
  , tsCurvature     :: !Float
  , tsHardness      :: !Float
  , tsSoilDepth     :: !Float
  , tsMoisture      :: !Float
  , tsFertility     :: !Float
  , tsRoughness     :: !Float
  , tsRockDensity   :: !Float
  , tsSoilGrain     :: !Float
  , tsTemperature   :: !Float
  , tsHumidity      :: !Float
  , tsWindSpeed     :: !Float
  , tsPressure      :: !Float
  , tsPrecip        :: !Float
  -- Extended fields (v12+)
  , tsBiomeId       :: !BiomeId
    -- ^ Biome classification from 'tcFlags'.
  , tsVegCover      :: !Float
    -- ^ Vegetation cover fraction from 'VegetationChunk'.
  , tsVegDensity    :: !Float
    -- ^ Biome-derived vegetation density from 'VegetationChunk'.
  , tsRelief        :: !Float
    -- ^ Local relief (elevation range in neighbourhood).
  , tsRuggedness    :: !Float
    -- ^ Terrain ruggedness index.
  , tsTerrainForm   :: !TerrainForm
    -- ^ Classified terrain landform.
  , tsWaterBodyType :: !WaterBodyType
    -- ^ Water body classification for this tile.
  , tsDischarge     :: !Float
    -- ^ River discharge (from 'RiverChunk').
  , tsSnowpack      :: !Float
    -- ^ Snow accumulation (from 'GlacierChunk').
  , tsIceThickness  :: !Float
    -- ^ Glacier ice thickness (from 'GlacierChunk').
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
