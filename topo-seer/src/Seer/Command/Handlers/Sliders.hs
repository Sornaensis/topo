{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for slider commands: @get_sliders@, @get_slider@, @set_slider@,
-- @set_sliders@, @reset_sliders@.
module Seer.Command.Handlers.Sliders
  ( handleGetSliders
  , handleGetSlider
  , handleSetSlider
  , handleSetSliders
  , handleResetSliders
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( UiState(..)
  , UiSnapshotRef
  , readUiSnapshotRef
  , sliderValueForId
  )
import Actor.UI.Setters (setUiSliderValue)
import Seer.Config.SliderRegistry
  ( SliderId(..)
  , SliderDef(..)
  , SliderTab(..)
  , allSliderDefs
  , sliderDefaultValueForId
  )
import Seer.Config.SliderConversion
  ( SliderLabelDomain(..)
  , SliderValueKind(..)
  , sliderLabelDomain
  , sliderToDomainFloat
  , sliderFromDomainFloat
  )
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @get_sliders@ — return all sliders or sliders for a specific tab.
handleGetSliders :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSliders ctx reqId params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let tabFilter = case params of
        Object o -> case Aeson.parseMaybe (.: "tab") o of
          Just tabName -> textToSliderTab tabName
          Nothing      -> Nothing
        _ -> Nothing
      defs = case tabFilter of
        Just tab -> filter ((== tab) . sliderTab) allSliderDefs
        Nothing  -> allSliderDefs
      sliders = map (sliderToJSON ui) defs
  pure $ okResponse reqId $ object ["sliders" .= sliders]

-- | Handle @get_slider@ — return a single slider's details.
handleGetSlider :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSlider ctx reqId params = do
  case Aeson.parseMaybe parseSliderName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name ->
      case lookupSliderId name of
        Nothing ->
          pure $ errResponse reqId ("unknown slider: " <> name)
        Just sid -> do
          ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          let def' = head $ filter ((== sid) . sliderId) allSliderDefs
          pure $ okResponse reqId $ sliderToJSON ui def'

-- | Handle @set_slider@ — set a slider to a normalized [0,1] value.
handleSetSlider :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetSlider ctx reqId params = do
  case Aeson.parseMaybe parseSetSlider params of
    Nothing ->
      pure $ errResponse reqId "missing 'name' and/or 'value' parameters"
    Just (name, val) ->
      case lookupSliderId name of
        Nothing ->
          pure $ errResponse reqId ("unknown slider: " <> name)
        Just sid -> do
          let normVal = max 0 (min 1 val)
          setUiSliderValue (ahUiHandle (ccActorHandles ctx)) sid normVal
          pure $ okResponse reqId $ object
            [ "name"  .= name
            , "value" .= normVal
            ]

-- | Handle @set_sliders@ — batch set multiple sliders.
-- Params: @{ "values": { "SliderName": 0.5, ... } }@
-- Returns the list of sliders actually set (unknown names are reported as errors).
handleSetSliders :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetSliders ctx reqId params = do
  case Aeson.parseMaybe parseSetSliders params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'values' parameter (expected object of {name: value})"
    Just kvs -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          go (name, val) = case lookupSliderId name of
            Nothing -> pure $ Left name
            Just sid -> do
              let normVal = max 0 (min 1 val)
              setUiSliderValue uiH sid normVal
              pure $ Right $ object ["name" .= name, "value" .= normVal]
      results <- mapM go kvs
      let (errs, oks) = partitionEithers results
      pure $ okResponse reqId $ object
        [ "updated" .= oks
        , "unknown" .= errs
        ]

-- | Handle @reset_sliders@ — reset sliders to defaults.
-- Optional @tab@ parameter to reset only a specific tab's sliders.
handleResetSliders :: CommandContext -> Int -> Value -> IO SeerResponse
handleResetSliders ctx reqId params = do
  let uiH = ahUiHandle (ccActorHandles ctx)
      maybeTab = case params of
        Object o -> case Aeson.parseMaybe (.: "tab") o of
          Just tabName -> textToSliderTab tabName
          Nothing      -> Nothing
        _ -> Nothing
      defs = case maybeTab of
        Just tab -> filter ((== tab) . sliderTab) allSliderDefs
        Nothing  -> allSliderDefs
  mapM_ (\d -> setUiSliderValue uiH (sliderId d) (sliderDefaultValueForId (sliderId d))) defs
  pure $ okResponse reqId $ object
    [ "reset_count" .= length defs
    , "tab"         .= fmap sliderTabToText maybeTab
    ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

parseSliderName :: Value -> Aeson.Parser Text
parseSliderName = Aeson.withObject "params" (.: "name")

parseSetSlider :: Value -> Aeson.Parser (Text, Float)
parseSetSlider = Aeson.withObject "params" $ \o ->
  (,) <$> o .: "name" <*> o .: "value"

-- | Parse batch slider values from @{ "values": { "Name": 0.5, ... } }@.
parseSetSliders :: Value -> Aeson.Parser [(Text, Float)]
parseSetSliders = Aeson.withObject "params" $ \o -> do
  valuesObj <- o .: "values"
  Aeson.withObject "values" (\vo ->
    mapM (\(k, v) -> do
      f <- Aeson.parseJSON v
      pure (Key.toText k, f)
    ) (KM.toList vo)
    ) valuesObj

-- | Convert a slider to JSON including name, value, range, tab.
sliderToJSON :: UiState -> SliderDef -> Value
sliderToJSON ui def' =
  let sid = sliderId def'
      normalized = sliderValueForId ui sid
      SliderLabelDomain lo hi kind = sliderLabelDomain sid
      domainVal = sliderToDomainFloat sid normalized
      defNorm = sliderDefaultValueForId sid
      defDomain = sliderToDomainFloat sid defNorm
  in object
    [ "name"       .= sliderIdToText sid
    , "tab"        .= sliderTabToText (sliderTab def')
    , "value"      .= normalized
    , "domain_value" .= domainVal
    , "domain_min" .= lo
    , "domain_max" .= hi
    , "value_kind" .= valueKindToText kind
    , "default"    .= defNorm
    , "default_domain" .= defDomain
    ]

-- | Map SliderId to a stable text key (the Haskell constructor name).
sliderIdToText :: SliderId -> Text
sliderIdToText = Text.pack . show

-- | Build a map from text name → SliderId for all sliders.
sliderIdMap :: Map.Map Text SliderId
sliderIdMap = Map.fromList
  [ (sliderIdToText sid, sid) | sid <- [minBound..maxBound] ]

-- | Look up a SliderId by its text name.
lookupSliderId :: Text -> Maybe SliderId
lookupSliderId = flip Map.lookup sliderIdMap

sliderTabToText :: SliderTab -> Text
sliderTabToText SliderTabTerrain = "terrain"
sliderTabToText SliderTabPlanet  = "planet"
sliderTabToText SliderTabClimate = "climate"
sliderTabToText SliderTabWeather = "weather"
sliderTabToText SliderTabBiome   = "biome"
sliderTabToText SliderTabErosion = "erosion"

textToSliderTab :: Text -> Maybe SliderTab
textToSliderTab "terrain" = Just SliderTabTerrain
textToSliderTab "planet"  = Just SliderTabPlanet
textToSliderTab "climate" = Just SliderTabClimate
textToSliderTab "weather" = Just SliderTabWeather
textToSliderTab "biome"   = Just SliderTabBiome
textToSliderTab "erosion" = Just SliderTabErosion
textToSliderTab _         = Nothing

valueKindToText :: SliderValueKind -> Text
valueKindToText SliderValueFloat  = "float"
valueKindToText SliderValueInt    = "int"
valueKindToText SliderValueToggle = "toggle"
