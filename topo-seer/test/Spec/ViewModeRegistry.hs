{-# LANGUAGE OverloadedStrings #-}

module Spec.ViewModeRegistry (spec) where

import Control.Monad (filterM)

import Actor.UI.State
  ( ViewMode(..)
  , ViewModeLegend(..)
  , ViewModeMetadata(..)
  , allBuiltinViewModes
  , builtinViewModeFromText
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

  it "exposes HTTP metadata, legends, and export mappings in JSON summaries" $ do
    let metas = mapMaybe viewModeMetadata allBuiltinViewModes
    mapM_ assertSummaryJson metas

  it "presents weather as current temperature and redirects cloud/storm expectations" $ do
    let cloudOrStorm field = Text.isInfixOf "cloud" field || Text.isInfixOf "storm" field
    case viewModeMetadata ViewWeather of
      Just meta -> do
        vmmName meta `shouldBe` "weather"
        vmmLabel meta `shouldBe` "Weather Temp"
        vmmDescription meta `shouldBe`
          "Current simulated weather temperature with humidity, wind, pressure, and precipitation context; use the Cloud view for cloud cover and storm cells."
        viewModeLegendTitle (vmmLegend meta) `shouldBe` "Current weather temperature"
        Text.toLower (vmmLabel meta <> " " <> viewModeLegendTitle (vmmLegend meta))
          `shouldNotSatisfy` cloudOrStorm
        (vmmTooltipFields meta <> vmmInspectorFields meta)
          `shouldNotSatisfy` (any cloudOrStorm)
      Nothing -> expectationFailure "missing ViewWeather metadata"

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
      [ "active", "name", "label", "kind", "unit", "color_scale"
      , "legend", "tooltip_fields", "inspector_fields", "export_fields", "http"
      ]
    _ -> expectationFailure "expected view mode summary JSON object"
  case viewModeMetadataToJSON meta of
    Object o -> KM.member "legend" o `shouldBe` True
    _ -> expectationFailure "expected view mode metadata JSON object"

legendEntryCount :: ViewModeLegend -> Int
legendEntryCount (ViewModeGradientLegend _ stops) = length stops
legendEntryCount (ViewModeCategoricalLegend _ categories) = length categories

renderLegendGolden :: Text
renderLegendGolden = Text.unlines (concatMap renderOne (mapMaybe viewModeMetadata allBuiltinViewModes))
  where
    renderOne meta =
      [ vmmName meta <> "|" <> viewModeKindToText (vmmKind meta) <> "|" <> unitText meta <> "|" <> vmmColorScale meta
      , "  " <> viewModeLegendTitle (vmmLegend meta)
      ] <> renderLegend (vmmLegend meta)
    unitText meta = maybe "unitless" id (vmmUnitLabel meta)

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

