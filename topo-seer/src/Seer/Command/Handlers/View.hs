{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handlers for view / seed mutation commands:
-- @set_seed@, @set_view_mode@, @set_config_tab@, @select_hex@,
-- @set_overlay@, @list_overlay_fields@, @cycle_overlay@, @cycle_overlay_field@.
module Seer.Command.Handlers.View
  ( handleSetSeed
  , handleSetViewMode
  , handleSetConfigTab
  , handleSelectHex
  , handleSetOverlay
  , handleListOverlayFields
  , handleCycleOverlay
  , handleCycleOverlayField
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager (AtlasJob(..), enqueueAtlasBuild)
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages)
import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI (getUiSnapshot)
import Actor.UI.State (ViewMode(..), ConfigTab(..), UiState(..), readUiSnapshotRef, uiRenderWaterLevel)
import Actor.UI.Setters (setUiSeed, setUiSeedInput, setUiViewMode, setUiConfigScroll, setUiConfigTab, setUiContextHex, setUiHexTooltipPinned)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Overlay.Schema (OverlayFieldType(..))

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
--
-- Now also supports @"overlay:name"@ syntax and optional @field_index@.
handleSetViewMode :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetViewMode ctx reqId params = do
  case Aeson.parseMaybe parseModeName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'mode' parameter"
    Just modeName -> do
      let mFieldIdx = Aeson.parseMaybe parseFieldIndex params
      case textToViewMode modeName mFieldIdx of
        Nothing ->
          pure $ errResponse reqId ("unknown view mode: " <> modeName)
        Just vm -> do
          let handles = ccActorHandles ctx
          setUiViewMode (ahUiHandle handles) vm
          scheduleAtlasRebuild handles vm
          pure $ okResponse reqId $ object ["view_mode" .= viewModeToText vm]

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
-- Atlas rebuild
-- --------------------------------------------------------------------------

-- | Enqueue atlas rebuild jobs for all zoom scales after a view mode change.
-- Mirrors the logic in 'Actor.UiActions.Command.rebuildAtlasFor'.
scheduleAtlasRebuild :: ActorHandles -> ViewMode -> IO ()
scheduleAtlasRebuild handles mode = do
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  uiSnap      <- getUiSnapshot (ahUiHandle handles)
  let atlasKey = AtlasKey mode (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
      job stage = AtlasJob
        { ajKey        = atlasKey
        , ajViewMode   = mode
        , ajWaterLevel = uiRenderWaterLevel uiSnap
        , ajTerrain    = terrainSnap
        , ajHexRadius  = zsHexRadius stage
        , ajAtlasScale = zsAtlasScale stage
        }
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles) . job) allZoomStages

-- --------------------------------------------------------------------------
-- Parsers
-- --------------------------------------------------------------------------

parseSeed :: Value -> Aeson.Parser Word64
parseSeed = Aeson.withObject "params" (.: "seed")

parseModeName :: Value -> Aeson.Parser Text
parseModeName = Aeson.withObject "params" (.: "mode")

parseFieldIndex :: Value -> Aeson.Parser Int
parseFieldIndex = Aeson.withObject "params" (.: "field_index")

parseTabName :: Value -> Aeson.Parser Text
parseTabName = Aeson.withObject "params" (.: "tab")

-- --------------------------------------------------------------------------
-- View mode / config tab text conversion
-- --------------------------------------------------------------------------

textToViewMode :: Text -> Maybe Int -> Maybe ViewMode
textToViewMode "elevation"      _ = Just ViewElevation
textToViewMode "biome"          _ = Just ViewBiome
textToViewMode "climate"        _ = Just ViewClimate
textToViewMode "weather"        _ = Just ViewWeather
textToViewMode "moisture"       _ = Just ViewMoisture
textToViewMode "precipitation"  _ = Just ViewPrecip
textToViewMode "plate_id"       _ = Just ViewPlateId
textToViewMode "plate_boundary" _ = Just ViewPlateBoundary
textToViewMode "plate_hardness" _ = Just ViewPlateHardness
textToViewMode "plate_crust"    _ = Just ViewPlateCrust
textToViewMode "plate_age"      _ = Just ViewPlateAge
textToViewMode "plate_height"   _ = Just ViewPlateHeight
textToViewMode "plate_velocity" _ = Just ViewPlateVelocity
textToViewMode "vegetation"     _ = Just ViewVegetation
textToViewMode "terrain_form"   _ = Just ViewTerrainForm
textToViewMode name             mIdx
  | Just rest <- Text.stripPrefix "overlay:" name
  , not (Text.null rest) = Just (ViewOverlay rest (maybe 0 id mIdx))
  | otherwise            = Nothing

viewModeToText :: ViewMode -> Text
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

overlayFieldTypeToText :: OverlayFieldType -> Text
overlayFieldTypeToText OFFloat    = "float"
overlayFieldTypeToText OFInt      = "int"
overlayFieldTypeToText OFBool     = "bool"
overlayFieldTypeToText OFText     = "text"
overlayFieldTypeToText (OFList t) = "list<" <> overlayFieldTypeToText t <> ">"

-- --------------------------------------------------------------------------
-- Overlay navigation commands
-- --------------------------------------------------------------------------

-- | Handle @set_overlay@ — set the view to a specific overlay by name and
-- optional field index.
--
-- Params: @{ "overlay": "name", "field_index": int }@
-- The field_index defaults to 0 if omitted.
handleSetOverlay :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetOverlay ctx reqId params = do
  case Aeson.parseMaybe parseOverlayName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'overlay' parameter"
    Just overlayName -> do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let names = uiOverlayNames ui
      if overlayName `notElem` names
        then pure $ errResponse reqId ("unknown overlay: " <> overlayName
                      <> "; available: " <> Text.intercalate ", " names)
        else do
          let fieldIdx = maybe 0 id (Aeson.parseMaybe parseFieldIndex params)
              vm = ViewOverlay overlayName fieldIdx
              handles = ccActorHandles ctx
          setUiViewMode (ahUiHandle handles) vm
          scheduleAtlasRebuild handles vm
          pure $ okResponse reqId $ object
            [ "overlay"     .= overlayName
            , "field_index" .= fieldIdx
            , "view_mode"   .= viewModeToText vm
            ]

-- | Handle @list_overlay_fields@ — return fields for an overlay.
--
-- Params: @{ "overlay": "name" }@
-- If overlay is omitted and the current view is an overlay, uses that.
handleListOverlayFields :: CommandContext -> Int -> Value -> IO SeerResponse
handleListOverlayFields ctx reqId params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let mExplicit = Aeson.parseMaybe parseOverlayName params
      mOverlay = case mExplicit of
        Just n  -> Just n
        Nothing -> case uiViewMode ui of
          ViewOverlay n _ -> Just n
          _               -> Nothing
  case mOverlay of
    Nothing ->
      pure $ errResponse reqId "no overlay specified and not currently viewing an overlay"
    Just _overlayName -> do
      -- Return the current overlay fields.  Note: uiOverlayFields is
      -- populated when an overlay is active — if the requested overlay
      -- differs from the current one, we'd need to look it up from the
      -- overlay store, but that's only available as raw data.  For now
      -- return whatever the current field list is.
      let fields = uiOverlayFields ui
          fieldObjs = zipWith (\i (fname, ftype) -> object
            [ "index" .= (i :: Int)
            , "name"  .= fname
            , "type"  .= overlayFieldTypeToText ftype
            ]) [0..] fields
      pure $ okResponse reqId $ object
        [ "field_count" .= length fields
        , "fields"      .= fieldObjs
        ]

-- | Handle @cycle_overlay@ — navigate to the next or previous overlay.
--
-- Params: @{ "direction": 1|-1 }@
-- +1 = next, -1 = previous.  Wraps around; index 0 = no overlay (elevation).
handleCycleOverlay :: CommandContext -> Int -> Value -> IO SeerResponse
handleCycleOverlay ctx reqId params = do
  case Aeson.parseMaybe parseDirection params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'direction' parameter (expected 1 or -1)"
    Just dir -> do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let names = uiOverlayNames ui
      if null names
        then pure $ errResponse reqId "no overlays available"
        else do
          let currentIdx = case uiViewMode ui of
                ViewOverlay name _ ->
                  case findIndex (== name) names of
                    Just i  -> i + 1
                    Nothing -> 0
                _ -> 0
              total = length names + 1
              newIdx = (currentIdx + dir) `mod` total
          if newIdx == 0
            then do
              let handles = ccActorHandles ctx
              setUiViewMode (ahUiHandle handles) ViewElevation
              scheduleAtlasRebuild handles ViewElevation
              pure $ okResponse reqId $ object
                [ "view_mode" .= ("elevation" :: Text)
                , "overlay"   .= Null
                ]
            else do
              let overlayName = names !! (newIdx - 1)
                  vm = ViewOverlay overlayName 0
                  handles = ccActorHandles ctx
              setUiViewMode (ahUiHandle handles) vm
              scheduleAtlasRebuild handles vm
              pure $ okResponse reqId $ object
                [ "view_mode" .= viewModeToText vm
                , "overlay"   .= overlayName
                ]

-- | Handle @cycle_overlay_field@ — navigate to the next or previous field
-- within the current overlay.
--
-- Params: @{ "direction": 1|-1 }@
-- Only effective when in overlay view mode.
handleCycleOverlayField :: CommandContext -> Int -> Value -> IO SeerResponse
handleCycleOverlayField ctx reqId params = do
  case Aeson.parseMaybe parseDirection params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'direction' parameter (expected 1 or -1)"
    Just dir -> do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      case uiViewMode ui of
        ViewOverlay name fieldIdx -> do
          let fields = uiOverlayFields ui
              fieldCount = length fields
          if fieldCount <= 0
            then pure $ errResponse reqId "overlay has no fields"
            else do
              let newIdx = (fieldIdx + dir) `mod` fieldCount
                  vm = ViewOverlay name newIdx
                  handles = ccActorHandles ctx
              setUiViewMode (ahUiHandle handles) vm
              scheduleAtlasRebuild handles vm
              let (fname, ftype) = fields !! newIdx
              pure $ okResponse reqId $ object
                [ "overlay"     .= name
                , "field_index" .= newIdx
                , "field_name"  .= fname
                , "field_type"  .= overlayFieldTypeToText ftype
                ]
        _ -> pure $ errResponse reqId "not currently viewing an overlay — use set_overlay or cycle_overlay first"

-- --------------------------------------------------------------------------
-- Additional parsers for overlay commands
-- --------------------------------------------------------------------------

parseOverlayName :: Value -> Aeson.Parser Text
parseOverlayName = Aeson.withObject "params" (.: "overlay")

parseDirection :: Value -> Aeson.Parser Int
parseDirection = Aeson.withObject "params" (.: "direction")
