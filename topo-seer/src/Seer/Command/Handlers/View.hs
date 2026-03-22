{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handlers for view / seed mutation commands:
-- @set_seed@, @set_view_mode@, @set_config_tab@, @select_hex@.
module Seer.Command.Handlers.View
  ( handleSetSeed
  , handleSetViewMode
  , handleSetConfigTab
  , handleSelectHex
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (ViewMode(..), ConfigTab(..))
import Actor.UI.Setters (setUiSeed, setUiSeedInput, setUiViewMode, setUiConfigScroll, setUiConfigTab, setUiContextHex, setUiHexTooltipPinned)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @set_seed@ — set the random seed.
handleSetSeed :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetSeed ctx reqId params = do
  case Aeson.parseMaybe parseSeed params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'seed' parameter (expected integer)"
    Just seed -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiSeed uiH seed
      setUiSeedInput uiH (Text.pack (show seed))
      pure $ okResponse reqId $ object ["seed" .= seed]

-- | Handle @set_view_mode@ — switch the hex map visualization.
handleSetViewMode :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetViewMode ctx reqId params = do
  case Aeson.parseMaybe parseModeName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'mode' parameter"
    Just modeName ->
      case textToViewMode modeName of
        Nothing ->
          pure $ errResponse reqId ("unknown view mode: " <> modeName)
        Just vm -> do
          setUiViewMode (ahUiHandle (ccActorHandles ctx)) vm
          pure $ okResponse reqId $ object ["view_mode" .= modeName]

-- | Handle @set_config_tab@ — switch the config panel tab.
handleSetConfigTab :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetConfigTab ctx reqId params = do
  case Aeson.parseMaybe parseTabName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'tab' parameter"
    Just tabName ->
      case textToConfigTab tabName of
        Nothing ->
          pure $ errResponse reqId ("unknown config tab: " <> tabName)
        Just tab -> do
          let uiH = ahUiHandle (ccActorHandles ctx)
          -- Use sendUpdate pattern for config tab; also reset scroll
          -- to top when switching tabs.
          setUiConfigTab uiH tab
          setUiConfigScroll uiH 0
          pure $ okResponse reqId $ object ["config_tab" .= tabName]

-- | Handle @select_hex@ — select a hex for inspection by chunk ID and tile index.
-- Params: @{ "chunk": int, "tile": int }@.
-- Pass chunk=null/tile=null or omit both to deselect.
handleSelectHex :: CommandContext -> Int -> Value -> IO SeerResponse
handleSelectHex ctx reqId params = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  case params of
    Object o
      | Just chunkVal <- KM.lookup "chunk" o
      , Just tileVal <- KM.lookup "tile" o -> do
          case (,) <$> Aeson.parseMaybe Aeson.parseJSON chunkVal
                   <*> Aeson.parseMaybe Aeson.parseJSON tileVal of
            Just (chunk :: Int, tile :: Int) -> do
              setUiContextHex uiH (Just (chunk, tile))
              setUiHexTooltipPinned uiH True
              pure $ okResponse reqId $ object
                [ "chunk" .= chunk
                , "tile"  .= tile
                , "selected" .= True
                ]
            Nothing ->
              pure $ errResponse reqId "invalid 'chunk' and/or 'tile' parameters (expected integers)"
      | otherwise -> do
          -- No chunk/tile params — deselect
          setUiContextHex uiH Nothing
          setUiHexTooltipPinned uiH False
          pure $ okResponse reqId $ object ["selected" .= False]
    _ -> do
      -- Null or non-object — deselect
      setUiContextHex uiH Nothing
      setUiHexTooltipPinned uiH False
      pure $ okResponse reqId $ object ["selected" .= False]

-- --------------------------------------------------------------------------
-- Parsers
-- --------------------------------------------------------------------------

parseSeed :: Value -> Aeson.Parser Word64
parseSeed = Aeson.withObject "params" (.: "seed")

parseModeName :: Value -> Aeson.Parser Text
parseModeName = Aeson.withObject "params" (.: "mode")

parseTabName :: Value -> Aeson.Parser Text
parseTabName = Aeson.withObject "params" (.: "tab")

-- --------------------------------------------------------------------------
-- View mode / config tab text conversion
-- --------------------------------------------------------------------------

textToViewMode :: Text -> Maybe ViewMode
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

textToConfigTab :: Text -> Maybe ConfigTab
textToConfigTab "terrain"  = Just ConfigTerrain
textToConfigTab "planet"   = Just ConfigPlanet
textToConfigTab "climate"  = Just ConfigClimate
textToConfigTab "weather"  = Just ConfigWeather
textToConfigTab "biome"    = Just ConfigBiome
textToConfigTab "erosion"  = Just ConfigErosion
textToConfigTab "pipeline" = Just ConfigPipeline
textToConfigTab "data"     = Just ConfigData
textToConfigTab _          = Nothing
