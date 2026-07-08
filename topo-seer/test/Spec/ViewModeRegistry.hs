{-# LANGUAGE OverloadedStrings #-}

module Spec.ViewModeRegistry (spec) where

import Control.Monad (filterM)

import Actor.UI.State
  ( SourceKind(..)
  , TemporalBasis(..)
  , ViewMode(..)
  , ViewModeDataSemantics(..)
  , ViewModeLegend(..)
  , ViewModeMetadata(..)
  , allBuiltinViewModes
  , builtinViewModeFromText
  , sourceKindToText
  , temporalBasisToText
  , viewModeDataSemantics
  , viewModeFromTextWithBasis
  , viewModeKindToText
  , viewModeLegendTitle
  , viewModeMetadata
  , viewModeMetadataToJSON
  , viewModeSummaryToJSON
  , viewModeToText
  , vmlcColor
  , vmlcLabel
  , vmlcValue
  , vmlsColor
  , vmlsLabel
  , vmlsValue
  )
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "view mode registry" $ do
  it "has complete metadata for every built-in view mode" $ do
    let metas = mapMaybe viewModeMetadata allBuiltinViewModes
    length metas `shouldBe` length allBuiltinViewModes
    map vmmMode metas `shouldBe` allBuiltinViewModes
    map vmmName metas `shouldBe` map viewModeToText allBuiltinViewModes
    map vmmName metas `shouldBe` nub (map vmmName metas)
    mapM_ assertComplete metas

  it "round-trips built-in mode names through the registry parser" $
    mapM_ (\mode -> builtinViewModeFromText (viewModeToText mode) `shouldBe` Just mode) allBuiltinViewModes

  it "places explicit average/current weather controls in built-in/API order" $
    take 8 allBuiltinViewModes `shouldBe`
      [ ViewElevation, ViewBiome, ViewClimate
      , ViewWeather, ViewCloud, ViewMoisture
      , ViewPrecip, ViewPrecipCurrent
      ]

  it "exposes HTTP metadata, legends, export mappings, and data semantics in JSON summaries" $ do
    let metas = mapMaybe viewModeMetadata allBuiltinViewModes
    mapM_ assertSummaryJson metas

  it "maps climate and weather view modes to explicit data-basis semantics" $ do
    let semanticsText mode = do
          semantics <- viewModeDataSemantics mode
          pure
            ( temporalBasisToText (vmdsTemporalBasis semantics)
            , sourceKindToText (vmdsSourceKind semantics)
            )
    semanticsText ViewClimate `shouldBe` Just ("long_run_average", "generated_climate")
    semanticsText ViewPrecip `shouldBe` Just ("long_run_average", "generated_climate")
    semanticsText ViewWeather `shouldBe` Just ("instantaneous_current", "simulated_generated_weather")
    semanticsText ViewCloud `shouldBe` Just ("instantaneous_current", "simulated_generated_weather")
    semanticsText ViewPrecipCurrent `shouldBe` Just ("instantaneous_current", "simulated_generated_weather")
    semanticsText ViewCloudTypical `shouldBe` Just ("typical_normal", "generated_climate")
    semanticsText (ViewOverlay "weather" 0) `shouldBe` Just ("instantaneous_current", "simulated_generated_weather")
    viewModeDataSemantics ViewMoisture `shouldBe` Nothing
    temporalBasisToText TypicalNormal `shouldBe` "typical_normal"
    sourceKindToText ExternalLive `shouldBe` "external_live"
    viewModeFromTextWithBasis "weather" (Just LongRunAverage) Nothing `shouldBe` Just ViewClimate
    viewModeFromTextWithBasis "weather" (Just InstantaneousCurrent) Nothing `shouldBe` Just ViewWeather
    viewModeFromTextWithBasis "weather" (Just TypicalNormal) Nothing `shouldBe` Nothing
    viewModeFromTextWithBasis "precipitation" (Just InstantaneousCurrent) Nothing `shouldBe` Just ViewPrecipCurrent
    viewModeFromTextWithBasis "precipitation" (Just TypicalNormal) Nothing `shouldBe` Nothing
    viewModeFromTextWithBasis "cloud" (Just TypicalNormal) Nothing `shouldBe` Just ViewCloudTypical

  it "presents weather as current temperature and redirects cloud/storm expectations" $ do
    let cloudOrStorm field = Text.isInfixOf "cloud" field || Text.isInfixOf "storm" field
    case viewModeMetadata ViewWeather of
      Just meta -> do
        vmmName meta `shouldBe` "weather"
        vmmLabel meta `shouldBe` "Current Weather Temp"
        vmmDescription meta `shouldBe`
          "Current simulated weather temperature with humidity, wind, pressure, and precipitation context; use Current Cloud/Storm for aggregate cloud cover and storm tint."
        viewModeLegendTitle (vmmLegend meta) `shouldBe` "Current weather temperature"
        Text.toLower (vmmLabel meta <> " " <> viewModeLegendTitle (vmmLegend meta))
          `shouldNotSatisfy` cloudOrStorm
        (vmmTooltipFields meta <> vmmInspectorFields meta)
          `shouldNotSatisfy` (any cloudOrStorm)
      Nothing -> expectationFailure "missing ViewWeather metadata"

  it "describes Cloud/Storm as an aggregate cloud-water renderer with layer context" $ do
    case viewModeMetadata ViewCloud of
      Just meta -> do
        vmmName meta `shouldBe` "cloud"
        vmmLabel meta `shouldBe` "Current Cloud/Storm"
        vmmDescription meta `shouldBe`
          "Current simulated aggregate cloud cover and cloud-water density with precipitation-derived storm tint; low/mid/high layer fields are inspector/API context, not separate rendered layers."
        viewModeLegendTitle (vmmLegend meta) `shouldBe` "Aggregate cloud cover"
        vmmTooltipFields meta `shouldBe` ["cloud_cover_pct", "cloud_water", "storm_intensity"]
        vmmInspectorFields meta `shouldSatisfy` elem "weather.cloud_cover_low"
        vmmInspectorFields meta `shouldSatisfy` elem "weather.cloud_water_high"
      Nothing -> expectationFailure "missing ViewCloud metadata"

  it "describes typical cloud normals as generated and distinct from current clouds" $ do
    case viewModeMetadata ViewCloudTypical of
      Just meta -> do
        vmmName meta `shouldBe` "cloud_typical"
        vmmLabel meta `shouldBe` "Typical Cloud Normal"
        vmmTemporalBasis meta `shouldBe` Just TypicalNormal
        vmmSourceKind meta `shouldBe` Just GeneratedClimate
        vmmInspectorFields meta `shouldSatisfy` elem "weather_normals.cloud_cover"
      Nothing -> expectationFailure "missing ViewCloudTypical metadata"

  it "matches the golden legend fixture" $ do
    path <- locateGolden
    expected <- TextIO.readFile path
    renderLegendGolden `shouldBe` expected

assertComplete :: ViewModeMetadata -> Expectation
assertComplete meta = do
  vmmName meta `shouldSatisfy` (not . Text.null)
  vmmLabel meta `shouldSatisfy` (not . Text.null)
  vmmDescription meta `shouldSatisfy` (not . Text.null)
  viewModeKindToText (vmmKind meta) `shouldSatisfy` (not . Text.null)
  vmmColorScale meta `shouldSatisfy` (not . Text.null)
  viewModeLegendTitle (vmmLegend meta) `shouldSatisfy` (not . Text.null)
  legendEntryCount (vmmLegend meta) `shouldSatisfy` (> 0)
  vmmTooltipFields meta `shouldSatisfy` (not . null)
  vmmInspectorFields meta `shouldSatisfy` (not . null)
  vmmExportFields meta `shouldSatisfy` (not . null)
  vmmHttpMetadata meta `shouldSatisfy` (not . null)

assertSummaryJson :: ViewModeMetadata -> Expectation
assertSummaryJson meta = do
  case viewModeSummaryToJSON False meta of
    Object o -> mapM_ (`shouldSatisfy` (`KM.member` o))
      [ "active", "name", "label", "kind", "temporal_basis", "source_kind"
      , "unit", "color_scale", "legend", "tooltip_fields", "inspector_fields"
      , "export_fields", "http"
      ]
    _ -> expectationFailure "expected view mode summary JSON object"
  case viewModeMetadataToJSON meta of
    Object o -> do
      KM.member "legend" o `shouldBe` True
      case vmmMode meta of
        ViewClimate -> do
          KM.lookup "temporal_basis" o `shouldBe` Just (String "long_run_average")
          KM.lookup "source_kind" o `shouldBe` Just (String "generated_climate")
        ViewWeather -> do
          KM.lookup "temporal_basis" o `shouldBe` Just (String "instantaneous_current")
          KM.lookup "source_kind" o `shouldBe` Just (String "simulated_generated_weather")
        ViewElevation -> do
          KM.lookup "temporal_basis" o `shouldBe` Just Null
          KM.lookup "source_kind" o `shouldBe` Just Null
        _ -> pure ()
    _ -> expectationFailure "expected view mode metadata JSON object"

legendEntryCount :: ViewModeLegend -> Int
legendEntryCount (ViewModeGradientLegend _ stops) = length stops
legendEntryCount (ViewModeCategoricalLegend _ categories) = length categories

renderLegendGolden :: Text
renderLegendGolden = Text.unlines (concatMap renderOne (mapMaybe viewModeMetadata allBuiltinViewModes))
  where
    renderOne meta =
      [ vmmName meta <> "|" <> viewModeKindToText (vmmKind meta) <> "|" <> unitText meta <> "|" <> vmmColorScale meta
          <> "|" <> basisText meta <> "|" <> sourceText meta
      , "  " <> viewModeLegendTitle (vmmLegend meta)
      ] <> renderLegend (vmmLegend meta)
    unitText meta = maybe "unitless" id (vmmUnitLabel meta)
    basisText meta = maybe "no_temporal_basis" temporalBasisToText (vmmTemporalBasis meta)
    sourceText meta = maybe "no_source_kind" sourceKindToText (vmmSourceKind meta)

renderLegend :: ViewModeLegend -> [Text]
renderLegend (ViewModeGradientLegend _ stops) =
  [ "  stop " <> vmlsValue stop <> " " <> vmlsLabel stop <> " " <> vmlsColor stop
  | stop <- stops
  ]
renderLegend (ViewModeCategoricalLegend _ categories) =
  [ "  category " <> vmlcValue category <> " " <> vmlcLabel category <> " " <> vmlcColor category
  | category <- categories
  ]

locateGolden :: IO FilePath
locateGolden = do
  let candidates =
        [ "test" </> "golden" </> "view-mode-legends.txt"
        , "topo-seer" </> "test" </> "golden" </> "view-mode-legends.txt"
        ]
  existing <- filterM doesFileExist candidates
  case existing of
    path:_ -> pure path
    [] -> fail "could not locate view-mode-legends golden file"

