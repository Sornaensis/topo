{-# LANGUAGE OverloadedStrings #-}

-- | Handler for @get_enums@ — enumerate valid values for domain types.
module Seer.Command.Handlers.Enums
  ( handleGetEnums
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word16)

import Actor.UI.State (ViewMode(..), ConfigTab(..))
import Seer.Command.Context (CommandContext(..))
import Seer.Config.SliderRegistry (SliderTab(..))
import Topo.Biome.Name (biomeDisplayName)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Types
  ( BiomeId
  , TerrainForm
  , biomeIdFromCode
  , terrainFormFromCode
  , terrainFormDisplayName
  )

-- | Handle @get_enums@ — return all valid values for a given domain type.
--
-- Params: @{ "type": "<enum_type>" }@
--
-- Supported types: @"biome"@, @"terrain_form"@, @"water_body_type"@,
-- @"plate_boundary"@, @"vent_type"@, @"vent_activity"@,
-- @"view_mode"@, @"config_tab"@, @"slider_tab"@.
handleGetEnums :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetEnums _ctx reqId params =
  case Aeson.parseMaybe parseEnumType params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'type' parameter"
    Just enumType ->
      case enumType of
        "biome"           -> pure $ okResponse reqId $ object ["values" .= biomeEnums]
        "terrain_form"    -> pure $ okResponse reqId $ object ["values" .= terrainFormEnums]
        "water_body_type" -> pure $ okResponse reqId $ object ["values" .= waterBodyTypeEnums]
        "plate_boundary"  -> pure $ okResponse reqId $ object ["values" .= plateBoundaryEnums]
        "vent_type"       -> pure $ okResponse reqId $ object ["values" .= ventTypeEnums]
        "vent_activity"   -> pure $ okResponse reqId $ object ["values" .= ventActivityEnums]
        "view_mode"       -> pure $ okResponse reqId $ object ["values" .= viewModeEnums]
        "config_tab"      -> pure $ okResponse reqId $ object ["values" .= configTabEnums]
        "slider_tab"      -> pure $ okResponse reqId $ object ["values" .= sliderTabEnums]
        _                 -> pure $ errResponse reqId ("unknown enum type: " <> enumType)

parseEnumType :: Value -> Aeson.Parser Text
parseEnumType = Aeson.withObject "params" (.: "type")

-- --------------------------------------------------------------------------
-- Enum value lists
-- --------------------------------------------------------------------------

-- | All BiomeId values (codes 0..65, skipping unused code 9).
biomeEnums :: [Value]
biomeEnums =
  [ mkEnum (biomeDisplayName bid) (fromIntegral code :: Int)
  | code <- biomeCodeRange
  , Right bid <- [biomeIdFromCode code]
  ]
  where
    -- Codes 0..8, 10..65 (9 is unused)
    biomeCodeRange :: [Word16]
    biomeCodeRange = [0..8] <> [10..65]

-- | All TerrainForm values (codes 0..14).
terrainFormEnums :: [Value]
terrainFormEnums =
  [ mkEnum (Text.pack $ terrainFormDisplayName tf) (fromIntegral code :: Int)
  | code <- [0..14 :: Word8]
  , Right tf <- [terrainFormFromCode code]
  ]

-- | All WaterBodyType values.
waterBodyTypeEnums :: [Value]
waterBodyTypeEnums =
  [ mkEnum "dry"        (0 :: Int)
  , mkEnum "ocean"      (1 :: Int)
  , mkEnum "lake"       (2 :: Int)
  , mkEnum "inland_sea" (3 :: Int)
  ]

-- | All PlateBoundary values.
plateBoundaryEnums :: [Value]
plateBoundaryEnums =
  [ mkEnum "none"       (0 :: Int)
  , mkEnum "convergent" (1 :: Int)
  , mkEnum "divergent"  (2 :: Int)
  , mkEnum "transform"  (3 :: Int)
  ]

-- | All VentType values.
ventTypeEnums :: [Value]
ventTypeEnums =
  [ mkEnum "none"           (0 :: Int)
  , mkEnum "shield"         (1 :: Int)
  , mkEnum "stratovolcano"  (2 :: Int)
  , mkEnum "fissure"        (3 :: Int)
  ]

-- | All VentActivity values.
ventActivityEnums :: [Value]
ventActivityEnums =
  [ mkEnum "dormant"  (0 :: Int)
  , mkEnum "active"   (1 :: Int)
  , mkEnum "erupting" (2 :: Int)
  ]

-- | All non-parameterized ViewMode constructors.
viewModeEnums :: [Value]
viewModeEnums =
  [ mkEnum "elevation"      (0 :: Int)
  , mkEnum "biome"          (1 :: Int)
  , mkEnum "climate"        (2 :: Int)
  , mkEnum "weather"        (3 :: Int)
  , mkEnum "moisture"       (4 :: Int)
  , mkEnum "precipitation"  (5 :: Int)
  , mkEnum "plate_id"       (6 :: Int)
  , mkEnum "plate_boundary" (7 :: Int)
  , mkEnum "plate_hardness" (8 :: Int)
  , mkEnum "plate_crust"    (9 :: Int)
  , mkEnum "plate_age"      (10 :: Int)
  , mkEnum "plate_height"   (11 :: Int)
  , mkEnum "plate_velocity" (12 :: Int)
  , mkEnum "vegetation"     (13 :: Int)
  , mkEnum "terrain_form"   (14 :: Int)
  ]

-- | All ConfigTab constructors.
configTabEnums :: [Value]
configTabEnums =
  [ mkEnum "terrain"  (0 :: Int)
  , mkEnum "planet"   (1 :: Int)
  , mkEnum "climate"  (2 :: Int)
  , mkEnum "weather"  (3 :: Int)
  , mkEnum "biome"    (4 :: Int)
  , mkEnum "erosion"  (5 :: Int)
  , mkEnum "pipeline" (6 :: Int)
  , mkEnum "data"     (7 :: Int)
  ]

-- | All SliderTab constructors.
sliderTabEnums :: [Value]
sliderTabEnums =
  [ mkEnum "terrain" (0 :: Int)
  , mkEnum "planet"  (1 :: Int)
  , mkEnum "climate" (2 :: Int)
  , mkEnum "weather" (3 :: Int)
  , mkEnum "biome"   (4 :: Int)
  , mkEnum "erosion" (5 :: Int)
  ]

-- | Build a @{name, code}@ enum entry.
mkEnum :: Text -> Int -> Value
mkEnum name code = object ["name" .= name, "code" .= code]
