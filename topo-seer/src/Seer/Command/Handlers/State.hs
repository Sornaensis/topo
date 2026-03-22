{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for state query commands: @get_state@, @get_view_modes@.
module Seer.Command.Handlers.State
  ( handleGetState
  , handleGetViewModes
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.IORef (readIORef)
import Data.Word (Word64)
import qualified Data.Text as Text

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( UiState(..)
  , UiSnapshotRef
  , ViewMode(..)
  , ConfigTab(..)
  , readUiSnapshotRef
  )
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse)

-- | Handle @get_state@ — return high-level application state.
handleGetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  pure $ okResponse reqId $ object
    [ "seed"              .= uiSeed ui
    , "view_mode"         .= viewModeToText (uiViewMode ui)
    , "config_tab"        .= configTabToText (uiConfigTab ui)
    , "generating"        .= uiGenerating ui
    , "chunk_size"        .= uiChunkSize ui
    , "show_config"       .= uiShowConfig ui
    , "world_name"        .= uiWorldName ui
    , "context_hex"       .= fmap (\(c, t) -> object ["chunk" .= c, "tile" .= t]) (uiContextHex ui)
    ]

-- | Handle @get_view_modes@ — return all view mode names and active flag.
handleGetViewModes :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetViewModes ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let current = uiViewMode ui
      modes = map (\vm -> object
        [ "name"   .= viewModeToText vm
        , "active" .= (vm == current)
        ]) allViewModes
  pure $ okResponse reqId $ object ["view_modes" .= modes]

-- | All non-parameterized view modes.
allViewModes :: [ViewMode]
allViewModes =
  [ ViewElevation
  , ViewBiome
  , ViewClimate
  , ViewWeather
  , ViewMoisture
  , ViewPrecip
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
  , ViewVegetation
  , ViewTerrainForm
  ]

viewModeToText :: ViewMode -> Text.Text
viewModeToText ViewElevation     = "elevation"
viewModeToText ViewBiome         = "biome"
viewModeToText ViewClimate       = "climate"
viewModeToText ViewWeather       = "weather"
viewModeToText ViewMoisture      = "moisture"
viewModeToText ViewPrecip        = "precipitation"
viewModeToText ViewPlateId       = "plate_id"
viewModeToText ViewPlateBoundary = "plate_boundary"
viewModeToText ViewPlateHardness = "plate_hardness"
viewModeToText ViewPlateCrust    = "plate_crust"
viewModeToText ViewPlateAge      = "plate_age"
viewModeToText ViewPlateHeight   = "plate_height"
viewModeToText ViewPlateVelocity = "plate_velocity"
viewModeToText ViewVegetation    = "vegetation"
viewModeToText ViewTerrainForm   = "terrain_form"
viewModeToText (ViewOverlay name _idx) = "overlay:" <> name

textToViewMode :: Text.Text -> Maybe ViewMode
textToViewMode "elevation"      = Just ViewElevation
textToViewMode "biome"          = Just ViewBiome
textToViewMode "climate"        = Just ViewClimate
textToViewMode "weather"        = Just ViewWeather
textToViewMode "moisture"       = Just ViewMoisture
textToViewMode "precipitation"  = Just ViewPrecip
textToViewMode "plate_id"       = Just ViewPlateId
textToViewMode "plate_boundary" = Just ViewPlateBoundary
textToViewMode "plate_hardness" = Just ViewPlateHardness
textToViewMode "plate_crust"    = Just ViewPlateCrust
textToViewMode "plate_age"      = Just ViewPlateAge
textToViewMode "plate_height"   = Just ViewPlateHeight
textToViewMode "plate_velocity" = Just ViewPlateVelocity
textToViewMode "vegetation"     = Just ViewVegetation
textToViewMode "terrain_form"   = Just ViewTerrainForm
textToViewMode _                = Nothing

configTabToText :: ConfigTab -> Text.Text
configTabToText ConfigTerrain  = "terrain"
configTabToText ConfigPlanet   = "planet"
configTabToText ConfigClimate  = "climate"
configTabToText ConfigWeather  = "weather"
configTabToText ConfigBiome    = "biome"
configTabToText ConfigErosion  = "erosion"
configTabToText ConfigPipeline = "pipeline"
configTabToText ConfigData     = "data"

textToConfigTab :: Text.Text -> Maybe ConfigTab
textToConfigTab "terrain"  = Just ConfigTerrain
textToConfigTab "planet"   = Just ConfigPlanet
textToConfigTab "climate"  = Just ConfigClimate
textToConfigTab "weather"  = Just ConfigWeather
textToConfigTab "biome"    = Just ConfigBiome
textToConfigTab "erosion"  = Just ConfigErosion
textToConfigTab "pipeline" = Just ConfigPipeline
textToConfigTab "data"     = Just ConfigData
textToConfigTab _          = Nothing
