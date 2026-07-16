{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handlers for view / seed mutation commands:
-- @set_seed@, @set_view_mode@, @set_view@, @set_config_tab@,
-- @select_hex@, @set_overlay@, @list_overlay_fields@,
-- @cycle_overlay@, @cycle_overlay_field@.
module Seer.Command.Handlers.View
  ( handleSetSeed
  , handleSetViewMode
  , handleSetView
  , handleSetConfigTab
  , handleSelectHex
  , handleSetOverlay
  , handleListOverlayFields
  , handleCycleOverlay
  , handleCycleOverlayField
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.AtlasManager (atlasJobsForSelection, atlasJobsForSelectionTransition, enqueueAtlasBuild)
import Seer.Render.ZoomStage (orderedZoomStagesForZoom)
import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.Log (getLogSnapshot)
import Actor.UiActions.Handles (ActorHandles(..), publishUiMutation)
import Actor.UI (getUiSnapshot)
import Actor.UI.State
  ( BaseViewMode
  , ConfigTab(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , TemporalBasis
  , UiState(..)
  , ViewMode(..)
  , ViewModeDataSemantics(..)
  , WeatherBasis
  , baseViewModeFromText
  , baseViewModeToText
  , effectiveViewSelection
  , layeredViewStateToJSON
  , layeredViewStateToViewMode
  , legacyViewModeToLayeredViewState
  , readUiSnapshotRef
  , skyOverlayModeFromText
  , skyOverlayModeToText
  , sourceKindToText
  , temporalBasisFromText
  , temporalBasisToText
  , uiRenderWaterLevel
  , viewModeDataSemantics
  , viewModeFromTextWithBasis
  , viewModeToText
  , weatherBasisFromText
  , weatherBasisToText
  )
import Actor.SnapshotReceiver
  ( publishSnapshot
  , readTerrainSnapshot
  , terrainSnapshotUpdate
  , withLogSnapshot
  , withUiSnapshot
  )
import Actor.UI.Setters (setUiSeed, setUiSeedInput, setUiViewMode, setUiViewSelection, setUiConfigScroll, setUiConfigTab, setUiContextHex, setUiHexTooltipPinned, setUiOverlayFields)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Overlay (Overlay(..), lookupOverlay, overlayNames)
import Topo.Overlay.Schema (OverlayFieldDef(..), OverlayFieldType(..), OverlaySchema(..))

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
      _ <- publishUiMutation (ccActorHandles ctx)
      pure $ okResponse reqId $ object ["seed" .= seed]

-- | Handle @set_view_mode@ — switch the hex map visualization.
--
-- Now also supports @"overlay:name"@ syntax and optional @field_index@.
handleSetViewMode :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetViewMode ctx reqId params = do
  case Aeson.parseMaybe parseViewModeRequest params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid view mode parameters"
    Just (ViewModeRequest modeName mBasis mFieldIdx) -> do
      case viewModeFromTextWithBasis modeName mBasis mFieldIdx of
        Nothing ->
          pure $ errResponse reqId ("unknown view mode or basis combination: " <> modeName)
        Just vm -> do
          let handles = ccActorHandles ctx
              uiHandle = ahUiHandle handles
          previousUi <- getUiSnapshot uiHandle
          setUiViewMode uiHandle vm
          scheduleAtlasTransitionRebuild handles previousUi vm
          pure $ okResponse reqId $ layeredViewResponseFields (legacyViewModeToLayeredViewState vm)
            [ "view_mode" .= viewModeToText vm
            , "temporal_basis" .= fmap (temporalBasisToText . vmdsTemporalBasis) (viewModeDataSemantics vm)
            , "source_kind" .= fmap (sourceKindToText . vmdsSourceKind) (viewModeDataSemantics vm)
            ]

-- | Handle @set_view@ — switch the layered base/overlay view selection.
handleSetView :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetView ctx reqId params = do
  case Aeson.parseMaybe parseLayeredViewRequest params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid layered view parameters"
    Just request -> do
      let handles = ccActorHandles ctx
          uiHandle = ahUiHandle handles
      previousUi <- getUiSnapshot uiHandle
      case applyLayeredViewRequest request (effectiveViewSelection previousUi) of
        Left err -> pure $ errResponse reqId err
        Right selection -> do
          validation <- validateLayeredViewSelection previousUi handles selection
          case validation of
            Left err -> pure $ errResponse reqId err
            Right mOverlayFields -> case layeredViewStateToViewMode selection of
              Nothing -> pure $ errResponse reqId "layered view selection is not representable by the current renderer"
              Just vm -> do
                maybe (pure ()) (setUiOverlayFields uiHandle) mOverlayFields
                setUiViewSelection uiHandle selection
                scheduleAtlasTransitionRebuild handles previousUi vm
                pure $ okResponse reqId $ layeredViewResponseFields selection
                  [ "view_mode" .= viewModeToText vm ]

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
          _ <- publishUiMutation (ccActorHandles ctx)
          pure $ okResponse reqId $ object ["config_tab" .= tabName]

-- | Handle @select_hex@ — select a hex for inspection by axial coordinates.
-- Params: @{ "q": int, "r": int }@.
-- Pass null or omit both to deselect.
handleSelectHex :: CommandContext -> Int -> Value -> IO SeerResponse
handleSelectHex ctx reqId params = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  case params of
    Object o
      | Just qVal <- KM.lookup "q" o
      , Just rVal <- KM.lookup "r" o -> do
          case (,) <$> Aeson.parseMaybe Aeson.parseJSON qVal
                   <*> Aeson.parseMaybe Aeson.parseJSON rVal of
            Just (q :: Int, r :: Int) -> do
              setUiContextHex uiH (Just (q, r))
              setUiHexTooltipPinned uiH True
              _ <- publishUiMutation (ccActorHandles ctx)
              pure $ okResponse reqId $ object
                [ "q" .= q
                , "r"  .= r
                , "selected" .= True
                ]
            Nothing ->
              pure $ errResponse reqId "invalid 'q' and/or 'r' parameters (expected integers)"
      | otherwise -> do
          -- No q/r params — deselect
          setUiContextHex uiH Nothing
          setUiHexTooltipPinned uiH False
          _ <- publishUiMutation (ccActorHandles ctx)
          pure $ okResponse reqId $ object ["selected" .= False]
    _ -> do
      -- Null or non-object — deselect
      setUiContextHex uiH Nothing
      setUiHexTooltipPinned uiH False
      _ <- publishUiMutation (ccActorHandles ctx)
      pure $ okResponse reqId $ object ["selected" .= False]

-- --------------------------------------------------------------------------
-- Atlas rebuild
-- --------------------------------------------------------------------------

-- | Enqueue atlas rebuild jobs for all zoom scales after a view mode change.
-- Mirrors the logic in 'Actor.UiActions.Command.rebuildAtlasFor'.
scheduleAtlasRebuild :: ActorHandles -> ViewMode -> IO ()
scheduleAtlasRebuild handles mode = do
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  logSnapshot <- getLogSnapshot (ahLogHandle handles)
  snapshotVersion <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot
      (withUiSnapshot uiSnap
        (terrainSnapshotUpdate (ahTerrainSnapshotRef handles) terrainSnap)))
  let selection = if uiViewMode uiSnap == mode
        then effectiveViewSelection uiSnap
        else legacyViewModeToLayeredViewState mode
      orderedStages = orderedZoomStagesForZoom (uiZoom uiSnap)
      jobs = atlasJobsForSelection snapshotVersion selection (uiRenderWaterLevel uiSnap) terrainSnap orderedStages Nothing
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles)) jobs

-- | Enqueue only layer keys that changed while switching views/overlays.
scheduleAtlasTransitionRebuild :: ActorHandles -> UiState -> ViewMode -> IO ()
scheduleAtlasTransitionRebuild handles previousUi mode = do
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  logSnapshot <- getLogSnapshot (ahLogHandle handles)
  snapshotVersion <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot
      (withUiSnapshot uiSnap
        (terrainSnapshotUpdate (ahTerrainSnapshotRef handles) terrainSnap)))
  let selection = if uiViewMode uiSnap == mode
        then effectiveViewSelection uiSnap
        else legacyViewModeToLayeredViewState mode
      orderedStages = orderedZoomStagesForZoom (uiZoom uiSnap)
      jobs = atlasJobsForSelectionTransition
        snapshotVersion
        (effectiveViewSelection previousUi)
        (uiRenderWaterLevel previousUi)
        selection
        (uiRenderWaterLevel uiSnap)
        terrainSnap
        orderedStages
        Nothing
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles)) jobs

-- --------------------------------------------------------------------------
-- Parsers
-- --------------------------------------------------------------------------

parseSeed :: Value -> Aeson.Parser Word64
parseSeed = Aeson.withObject "params" (.: "seed")

data ViewModeRequest = ViewModeRequest !Text !(Maybe TemporalBasis) !(Maybe Int)

parseViewModeRequest :: Value -> Aeson.Parser ViewModeRequest
parseViewModeRequest = Aeson.withObject "params" $ \o -> do
  modeName <- o .: "mode"
  mBasisText <- o .:? "basis"
  mTemporalBasisText <- o .:? "temporal_basis"
  mBasis <- case mBasisText <|> mTemporalBasisText of
    Nothing -> pure Nothing
    Just basisText -> case temporalBasisFromText basisText of
      Just basis -> pure (Just basis)
      Nothing -> fail "invalid 'basis' parameter"
  mFieldIdx <- o .:? "field_index"
  pure (ViewModeRequest modeName mBasis mFieldIdx)

data OverlayMutation
  = OverlayUnchanged
  | OverlaySet !(Maybe SkyOverlayMode)
  | OverlayFieldSet !Int

data LayeredViewRequest = LayeredViewRequest
  { lvrBaseMode :: !(Maybe BaseViewMode)
  , lvrOverlayMode :: !OverlayMutation
  , lvrWeatherBasis :: !(Maybe WeatherBasis)
  , lvrOverlayOpacity :: !(Maybe Float)
  }

parseLayeredViewRequest :: Value -> Aeson.Parser LayeredViewRequest
parseLayeredViewRequest = Aeson.withObject "params" $ \o -> do
  mBaseModeText <- o .:? "base_mode"
  mBaseAliasText <- o .:? "base"
  let mBaseText = mBaseModeText <|> mBaseAliasText
  mBase <- case mBaseText of
    Nothing -> pure Nothing
    Just baseText -> case baseViewModeFromText baseText of
      Just baseMode -> pure (Just baseMode)
      Nothing -> fail "invalid 'base_mode' parameter"
  mWeatherBasisText <- o .:? "weather_basis"
  mBasisAliasText <- o .:? "basis"
  mTemporalBasisText <- o .:? "temporal_basis"
  let mBasisText = mWeatherBasisText <|> mBasisAliasText <|> mTemporalBasisText
  mBasis <- case mBasisText of
    Nothing -> pure Nothing
    Just basisText -> case weatherBasisFromText basisText of
      Just basis -> pure (Just basis)
      Nothing -> fail "invalid 'weather_basis' parameter"
  mFieldIndex <- o .:? "field_index"
  mOverlayField <- o .:? "overlay_field"
  let mFieldIdx = mFieldIndex <|> mOverlayField
  case mFieldIdx of
    Just idx | idx < 0 -> fail "invalid 'field_index' parameter"
    _ -> pure ()
  overlayMutation <- parseOverlayMutation o mFieldIdx
  mOpacity <- o .:? "overlay_opacity"
  pure LayeredViewRequest
    { lvrBaseMode = mBase
    , lvrOverlayMode = overlayMutation
    , lvrWeatherBasis = mBasis
    , lvrOverlayOpacity = fmap clampOpacity mOpacity
    }

parseOverlayMutation :: Aeson.Object -> Maybe Int -> Aeson.Parser OverlayMutation
parseOverlayMutation o mFieldIdx
  | Just overlayValue <- lookupField "overlay_mode" <|> lookupField "overlay" =
      case overlayValue of
        Null -> pure (OverlaySet Nothing)
        String overlayText -> parseOverlayText overlayText
        _ -> fail "invalid 'overlay_mode' parameter"
  | Just pluginValue <- lookupField "plugin_overlay" =
      case pluginValue of
        Null -> pure (OverlaySet Nothing)
        String pluginName
          | Text.null pluginName -> fail "invalid 'plugin_overlay' parameter"
          | otherwise -> pure (OverlaySet (Just (SkyOverlayPlugin pluginName (maybe 0 id mFieldIdx))))
        _ -> fail "invalid 'plugin_overlay' parameter"
  | Just fieldIdx <- mFieldIdx = pure (OverlayFieldSet fieldIdx)
  | otherwise = pure OverlayUnchanged
  where
    lookupField name = KM.lookup (Key.fromText name) o
    parseOverlayText raw
      | Text.toLower raw `elem` ["none", "off", "null", ""] = pure (OverlaySet Nothing)
      | Text.toLower raw == "plugin" =
          case lookupField "plugin_overlay" of
            Just (String pluginName) | not (Text.null pluginName) ->
              pure (OverlaySet (Just (SkyOverlayPlugin pluginName (maybe 0 id mFieldIdx))))
            _ -> fail "overlay_mode 'plugin' requires 'plugin_overlay'"
      | otherwise = case skyOverlayModeFromText raw of
          Just (SkyOverlayPlugin name _) -> pure (OverlaySet (Just (SkyOverlayPlugin name (maybe 0 id mFieldIdx))))
          Just overlay -> pure (OverlaySet (Just overlay))
          Nothing -> fail "invalid 'overlay_mode' parameter"

applyLayeredViewRequest :: LayeredViewRequest -> LayeredViewState -> Either Text LayeredViewState
applyLayeredViewRequest request current =
  case applyOverlayMutation (lvrOverlayMode request) (lvsSkyOverlay current) of
    Left err -> Left err
    Right overlayMode -> Right current
      { lvsBaseView = maybe (lvsBaseView current) id (lvrBaseMode request)
      , lvsSkyOverlay = overlayMode
      , lvsWeatherBasis = maybe (lvsWeatherBasis current) id (lvrWeatherBasis request)
      , lvsOverlayOpacity = maybe (lvsOverlayOpacity current) id (lvrOverlayOpacity request)
      }

applyOverlayMutation :: OverlayMutation -> Maybe SkyOverlayMode -> Either Text (Maybe SkyOverlayMode)
applyOverlayMutation OverlayUnchanged currentOverlay = Right currentOverlay
applyOverlayMutation (OverlaySet overlayMode) _ = Right overlayMode
applyOverlayMutation (OverlayFieldSet fieldIdx) (Just (SkyOverlayPlugin name _)) =
  Right (Just (SkyOverlayPlugin name fieldIdx))
applyOverlayMutation (OverlayFieldSet _) _ =
  Left "overlay_field requires an active plugin overlay"

clampOpacity :: Float -> Float
clampOpacity = max 0 . min 1

layeredViewResponseFields :: LayeredViewState -> [Aeson.Pair] -> Value
layeredViewResponseFields selection extraFields = object $
  extraFields <>
    [ "view" .= layeredViewStateToJSON selection
    , "base_mode" .= baseViewModeToText (lvsBaseView selection)
    , "overlay_mode" .= overlayModeName (lvsSkyOverlay selection)
    , "plugin_overlay" .= pluginOverlayName (lvsSkyOverlay selection)
    , "overlay_field" .= pluginOverlayField (lvsSkyOverlay selection)
    , "weather_basis" .= weatherBasisToText (lvsWeatherBasis selection)
    , "overlay_opacity" .= lvsOverlayOpacity selection
    , "legacy_view_mode" .= fmap viewModeToText (layeredViewStateToViewMode selection)
    ]

validateLayeredViewSelection :: UiState -> ActorHandles -> LayeredViewState -> IO (Either Text (Maybe [(Text, OverlayFieldType)]))
validateLayeredViewSelection previousUi handles selection =
  case lvsSkyOverlay selection of
    Just (SkyOverlayPlugin overlayName fieldIdx) -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef handles)
      let storeNames = overlayNamesFromSnapshot snap
          availableNames = uiOverlayNames previousUi ++ [n | n <- storeNames, n `notElem` uiOverlayNames previousUi]
      if overlayName `notElem` availableNames
        then pure $ Left ("unknown overlay: " <> overlayName
                  <> "; available: " <> Text.intercalate ", " availableNames)
        else do
          let fields = maybe (uiOverlayFields previousUi) overlayFieldsFromOverlay (lookupOverlay overlayName (tsOverlayStore snap))
              fieldCount = length fields
          if fieldIdx < 0 || (fieldCount > 0 && fieldIdx >= fieldCount)
            then pure $ Left ("field_index out of range for overlay " <> overlayName)
            else pure $ Right (Just fields)
    _ -> pure (Right Nothing)

overlayModeName :: Maybe SkyOverlayMode -> Maybe Text
overlayModeName Nothing = Nothing
overlayModeName (Just (SkyOverlayPlugin _ _)) = Just "plugin"
overlayModeName (Just overlayMode) = Just (skyOverlayModeToText overlayMode)

pluginOverlayName :: Maybe SkyOverlayMode -> Maybe Text
pluginOverlayName (Just (SkyOverlayPlugin name _)) = Just name
pluginOverlayName _ = Nothing

pluginOverlayField :: Maybe SkyOverlayMode -> Maybe Int
pluginOverlayField (Just (SkyOverlayPlugin _ fieldIndex)) = Just fieldIndex
pluginOverlayField _ = Nothing

parseFieldIndex :: Value -> Aeson.Parser Int
parseFieldIndex = Aeson.withObject "params" (.: "field_index")

parseTabName :: Value -> Aeson.Parser Text
parseTabName = Aeson.withObject "params" (.: "tab")

-- --------------------------------------------------------------------------
-- Config tab text conversion
-- --------------------------------------------------------------------------

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

overlayNamesFromSnapshot :: TerrainSnapshot -> [Text]
overlayNamesFromSnapshot = overlayNames . tsOverlayStore

overlayFieldsFromOverlay :: Overlay -> [(Text, OverlayFieldType)]
overlayFieldsFromOverlay ov = map fieldPair (osFields (ovSchema ov))
  where
    fieldPair fd = (ofdName fd, ofdType fd)

overlayFieldsForName :: TerrainSnapshot -> Text -> Maybe [(Text, OverlayFieldType)]
overlayFieldsForName snap name = overlayFieldsFromOverlay <$> lookupOverlay name (tsOverlayStore snap)

overlayFieldsResponse :: Int -> Text -> [(Text, OverlayFieldType)] -> SeerResponse
overlayFieldsResponse reqId overlayName fields =
  okResponse reqId $ object
    [ "overlay" .= overlayName
    , "field_count" .= length fields
    , "fields" .= zipWith fieldObject [0..] fields
    ]
  where
    fieldObject i (fname, ftype) = object
      [ "index" .= (i :: Int)
      , "name" .= fname
      , "type" .= overlayFieldTypeToText ftype
      ]

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
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let names = uiOverlayNames ui
          storeNames = overlayNamesFromSnapshot snap
          availableNames = names ++ [n | n <- storeNames, n `notElem` names]
      if overlayName `notElem` availableNames
        then pure $ errResponse reqId ("unknown overlay: " <> overlayName
                      <> "; available: " <> Text.intercalate ", " availableNames)
        else do
          let fieldIdx = maybe 0 id (Aeson.parseMaybe parseFieldIndex params)
              mOverlay = lookupOverlay overlayName (tsOverlayStore snap)
              fields = maybe (uiOverlayFields ui) overlayFieldsFromOverlay mOverlay
              fieldCount = length fields
          if fieldIdx < 0 || (fieldCount > 0 && fieldIdx >= fieldCount)
            then pure $ errResponse reqId ("field_index out of range for overlay " <> overlayName)
            else do
              let selection = (effectiveViewSelection ui)
                    { lvsSkyOverlay = Just (SkyOverlayPlugin overlayName fieldIdx) }
                  handles = ccActorHandles ctx
                  uiHandle = ahUiHandle handles
              case layeredViewStateToViewMode selection of
                Nothing -> pure $ errResponse reqId "overlay selection is not representable by the current renderer"
                Just vm -> do
                  setUiOverlayFields uiHandle fields
                  setUiViewSelection uiHandle selection
                  scheduleAtlasTransitionRebuild handles ui vm
                  pure $ okResponse reqId $ object
                    [ "overlay"     .= overlayName
                    , "field_index" .= fieldIdx
                    , "field_count" .= fieldCount
                    , "view_mode"   .= viewModeToText vm
                    ]

-- | Handle @list_overlay_fields@ — return fields for an overlay.
--
-- Params: @{ "overlay": "name" }@
-- If overlay is omitted and the current view is an overlay, uses that.
handleListOverlayFields :: CommandContext -> Int -> Value -> IO SeerResponse
handleListOverlayFields ctx reqId params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  case Aeson.parseMaybe parseOverlayName params of
    Just overlayName ->
      case overlayFieldsForName snap overlayName of
        Nothing -> pure $ errResponse reqId ("overlay not found: " <> overlayName)
        Just fields -> pure (overlayFieldsResponse reqId overlayName fields)
    Nothing ->
      case lvsSkyOverlay (effectiveViewSelection ui) of
        Just (SkyOverlayPlugin overlayName _) -> do
          let fields = maybe (uiOverlayFields ui) id (overlayFieldsForName snap overlayName)
          pure (overlayFieldsResponse reqId overlayName fields)
        _ -> pure $ errResponse reqId "no overlay specified and not currently viewing an overlay"

-- | Handle @cycle_overlay@ — navigate to the next or previous overlay.
--
-- Params: @{ "direction": 1|-1 }@
-- +1 = next, -1 = previous. Wraps around; index 0 clears the overlay
-- while preserving the current layered base, weather basis, and opacity.
handleCycleOverlay :: CommandContext -> Int -> Value -> IO SeerResponse
handleCycleOverlay ctx reqId params = do
  case Aeson.parseMaybe parseDirection params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'direction' parameter (expected 1 or -1)"
    Just dir -> do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let storeNames = overlayNamesFromSnapshot snap
          names = uiOverlayNames ui ++ [n | n <- storeNames, n `notElem` uiOverlayNames ui]
      if null names
        then pure $ errResponse reqId "no overlays available"
        else do
          let currentSelection = effectiveViewSelection ui
              currentIdx = case lvsSkyOverlay currentSelection of
                Just (SkyOverlayPlugin name _) ->
                  case findIndex (== name) names of
                    Just i  -> i + 1
                    Nothing -> 0
                _ -> 0
              total = length names + 1
              newIdx = (currentIdx + dir) `mod` total
              handles = ccActorHandles ctx
              uiHandle = ahUiHandle handles
          if newIdx == 0
            then do
              let selection = currentSelection { lvsSkyOverlay = Nothing }
              case layeredViewStateToViewMode selection of
                Nothing -> pure $ errResponse reqId "base view is not representable by the current renderer"
                Just vm -> do
                  setUiViewSelection uiHandle selection
                  scheduleAtlasTransitionRebuild handles ui vm
                  pure $ okResponse reqId $ object
                    [ "view_mode" .= viewModeToText vm
                    , "overlay"   .= Null
                    ]
            else do
              let overlayName = names !! (newIdx - 1)
                  selection = currentSelection
                    { lvsSkyOverlay = Just (SkyOverlayPlugin overlayName 0) }
                  fields = maybe (uiOverlayFields ui) id (overlayFieldsForName snap overlayName)
              case layeredViewStateToViewMode selection of
                Nothing -> pure $ errResponse reqId "overlay selection is not representable by the current renderer"
                Just vm -> do
                  setUiOverlayFields uiHandle fields
                  setUiViewSelection uiHandle selection
                  scheduleAtlasTransitionRebuild handles ui vm
                  pure $ okResponse reqId $ object
                    [ "view_mode" .= viewModeToText vm
                    , "overlay"   .= overlayName
                    , "field_count" .= length fields
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
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      case lvsSkyOverlay (effectiveViewSelection ui) of
        Just (SkyOverlayPlugin name fieldIdx) -> do
          let fields = maybe (uiOverlayFields ui) id (overlayFieldsForName snap name)
              fieldCount = length fields
          if fieldCount <= 0
            then pure $ errResponse reqId "overlay has no fields"
            else do
              let newIdx = (fieldIdx + dir) `mod` fieldCount
                  selection = (effectiveViewSelection ui)
                    { lvsSkyOverlay = Just (SkyOverlayPlugin name newIdx) }
                  handles = ccActorHandles ctx
                  uiHandle = ahUiHandle handles
              case layeredViewStateToViewMode selection of
                Nothing -> pure $ errResponse reqId "overlay selection is not representable by the current renderer"
                Just vm -> do
                  setUiOverlayFields uiHandle fields
                  setUiViewSelection uiHandle selection
                  scheduleAtlasTransitionRebuild handles ui vm
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
