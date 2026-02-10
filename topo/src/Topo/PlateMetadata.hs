{-# LANGUAGE OverloadedStrings #-}

-- | Plate metadata helpers for hex-level inspection.
module Topo.PlateMetadata
  ( PlateHexMeta(..)
  , plateHexMetaAt
  , storePlateHexMeta
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32)
import qualified Data.Map.Strict as Map
import Topo.Hex (hexToWorld)
import Topo.Metadata
  ( JsonValue(..)
  , Metadata(..)
  , MetadataDecodeError(..)
  , decodeJsonObject
  , encodeJsonObject
  )
import Topo.Types
import Topo.World (TerrainWorld(..), getTerrainChunk, putHexMetaWorld)
import qualified Data.Vector.Unboxed as U

-- | Snapshot of plate-related fields at a single hex.
data PlateHexMeta = PlateHexMeta
  { phmPlateId :: !PlateId
  , phmBoundary :: !PlateBoundary
  , phmCrust :: !PlateCrust
  , phmAge :: !Float
  , phmHeight :: !Float
  , phmHardness :: !Float
  , phmVelocity :: !Vec3
  } deriving (Eq, Show)

instance Metadata PlateHexMeta where
  metadataKey _ = Text.pack "plate-hex"
  metadataVersion _ = 1
  metadataEncode = encodePlateHexMeta
  metadataDecode = decodePlateHexMeta

-- | Read plate metadata for a hex if the corresponding terrain tile exists.
plateHexMetaAt :: TerrainWorld -> HexCoord -> Maybe PlateHexMeta
plateHexMetaAt world hex = do
  let WorldPos x y = hexToWorld (twHexGrid world) hex
      tile = TileCoord (round x) (round y)
      config = twConfig world
      (chunkCoord, local) = chunkCoordFromTile config tile
      chunkId = chunkIdFromCoord chunkCoord
  chunk <- getTerrainChunk chunkId world
  TileIndex i <- tileIndex config local
  let pid = PlateId (tcPlateId chunk U.! i)
      boundary = tcPlateBoundary chunk U.! i
      crust = plateCrustFromCode (tcPlateCrust chunk U.! i)
      age = tcPlateAge chunk U.! i
      height = tcPlateHeight chunk U.! i
      hardness = tcPlateHardness chunk U.! i
      velX = tcPlateVelX chunk U.! i
      velY = tcPlateVelY chunk U.! i
      velocity = Vec3 velX velY 0
  pure PlateHexMeta
    { phmPlateId = pid
    , phmBoundary = boundary
    , phmCrust = crust
    , phmAge = age
    , phmHeight = height
    , phmHardness = hardness
    , phmVelocity = velocity
    }

-- | Store plate metadata for a hex in the world's metadata store when available.
storePlateHexMeta :: HexCoord -> TerrainWorld -> TerrainWorld
storePlateHexMeta hex world =
  case plateHexMetaAt world hex of
    Nothing -> world
    Just meta -> putHexMetaWorld hex meta world

plateCrustFromCode :: Word16 -> PlateCrust
plateCrustFromCode code =
  case code of
    1 -> PlateContinental
    _ -> PlateOceanic

encodePlateHexMeta :: PlateHexMeta -> Text
encodePlateHexMeta meta =
  let Vec3 velX velY _ = phmVelocity meta
  in encodeJsonObject
      [ (Text.pack "plateId", JsonNumber (fromIntegral (plateIdToWord16 (phmPlateId meta))))
      , (Text.pack "boundary", JsonString (plateBoundaryToText (phmBoundary meta)))
      , (Text.pack "crust", JsonString (plateCrustToText (phmCrust meta)))
      , (Text.pack "age", JsonNumber (realToFrac (phmAge meta)))
      , (Text.pack "height", JsonNumber (realToFrac (phmHeight meta)))
      , (Text.pack "hardness", JsonNumber (realToFrac (phmHardness meta)))
      , (Text.pack "velX", JsonNumber (realToFrac velX))
      , (Text.pack "velY", JsonNumber (realToFrac velY))
      ]

decodePlateHexMeta :: Word32 -> Text -> Either MetadataDecodeError PlateHexMeta
decodePlateHexMeta version payload
  | version /= 1 = Left (MetadataDecodeError (Text.pack "plate-hex: unsupported version"))
  | otherwise = do
      obj <- decodeJsonObject payload
      plateIdNum <- requireNumber (Text.pack "plateId") obj
      boundaryText <- requireString (Text.pack "boundary") obj
      crustText <- requireString (Text.pack "crust") obj
      ageNum <- requireNumber (Text.pack "age") obj
      heightNum <- requireNumber (Text.pack "height") obj
      hardnessNum <- requireNumber (Text.pack "hardness") obj
      velXNum <- requireNumber (Text.pack "velX") obj
      velYNum <- requireNumber (Text.pack "velY") obj
      plateId <- numberToPlateId plateIdNum
      boundary <- textToPlateBoundary boundaryText
      crust <- textToPlateCrust crustText
      pure PlateHexMeta
        { phmPlateId = plateId
        , phmBoundary = boundary
        , phmCrust = crust
        , phmAge = realToFrac ageNum
        , phmHeight = realToFrac heightNum
        , phmHardness = realToFrac hardnessNum
        , phmVelocity = Vec3 (realToFrac velXNum) (realToFrac velYNum) 0
        }

plateBoundaryToText :: PlateBoundary -> Text
plateBoundaryToText boundary =
  case boundary of
    PlateBoundaryNone -> Text.pack "none"
    PlateBoundaryConvergent -> Text.pack "convergent"
    PlateBoundaryDivergent -> Text.pack "divergent"
    PlateBoundaryTransform -> Text.pack "transform"

plateCrustToText :: PlateCrust -> Text
plateCrustToText crust =
  case crust of
    PlateOceanic -> Text.pack "oceanic"
    PlateContinental -> Text.pack "continental"

textToPlateBoundary :: Text -> Either MetadataDecodeError PlateBoundary
textToPlateBoundary txt
  | txt == Text.pack "none" = Right PlateBoundaryNone
  | txt == Text.pack "convergent" = Right PlateBoundaryConvergent
  | txt == Text.pack "divergent" = Right PlateBoundaryDivergent
  | txt == Text.pack "transform" = Right PlateBoundaryTransform
  | otherwise = Left (MetadataDecodeError (Text.pack "plate-hex: invalid boundary"))

textToPlateCrust :: Text -> Either MetadataDecodeError PlateCrust
textToPlateCrust txt
  | txt == Text.pack "oceanic" = Right PlateOceanic
  | txt == Text.pack "continental" = Right PlateContinental
  | otherwise = Left (MetadataDecodeError (Text.pack "plate-hex: invalid crust"))

plateIdToWord16 :: PlateId -> Word16
plateIdToWord16 (PlateId pid) = pid

numberToPlateId :: Double -> Either MetadataDecodeError PlateId
numberToPlateId value =
  let rounded = round value
  in if value < 0 || value > fromIntegral (maxBound :: Word16) || fromIntegral rounded /= value
      then Left (MetadataDecodeError (Text.pack "plate-hex: invalid plateId"))
      else Right (PlateId (fromIntegral rounded))

requireString :: Text -> Map.Map Text JsonValue -> Either MetadataDecodeError Text
requireString key obj =
  case Map.lookup key obj of
    Just (JsonString txt) -> Right txt
    Just _ -> Left (MetadataDecodeError (Text.pack "plate-hex: expected string"))
    Nothing -> Left (MetadataDecodeError (Text.pack "plate-hex: missing field"))

requireNumber :: Text -> Map.Map Text JsonValue -> Either MetadataDecodeError Double
requireNumber key obj =
  case Map.lookup key obj of
    Just (JsonNumber val) -> Right val
    Just _ -> Left (MetadataDecodeError (Text.pack "plate-hex: expected number"))
    Nothing -> Left (MetadataDecodeError (Text.pack "plate-hex: missing field"))
