{-# LANGUAGE OverloadedStrings #-}

module Spec.PipelineControls (spec) where

import Actor.Data (DataSnapshot(..))
import Actor.UI
  ( ConfigTab(..)
  , UiState(..)
  , builtinStageRowCount
  , emptyUiState
  )
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Linear (V2(..))
import Test.Hspec
import Topo.Pipeline.Stage (StageId, allBuiltinStageIds)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.Components.PipelineControls
import UI.DrawCommand (DrawCommand(..))
import UI.Layout
import UI.WidgetId (WidgetId(..))
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "pipeline config controls component" $ do
  it "builds built-in stage, plugin parameter, and simulation view models" $ do
    firstStage <- requireHead "expected built-in pipeline stages" allBuiltinStageIds
    let ui = pipelineUi firstStage
        view = pipelineControlsView ui readyData wideLayout
        pluginRows = pcvPluginRows view
        paramControls = pcvParamControls view
        simControls = pcvSimulationControls view
    firstStageRow <- requireHead "expected stage rows" (pcvStageRows view)
    length (pcvStageRows view) `shouldBe` length allBuiltinStageIds
    psrvDisabled firstStageRow `shouldBe` True
    map pprvRowIndex pluginRows `shouldBe` [builtinStageRowCount, builtinStageRowCount + 4]
    map pprvExpanded pluginRows `shouldBe` [True, False]
    map pprvDisabled pluginRows `shouldBe` [False, True]
    map ppcvRowIndex paramControls `shouldBe` [builtinStageRowCount + 2, builtinStageRowCount + 3]
    case paramControls of
      [PipelineParamCheckView { ppcvChecked = checked }, PipelineParamSliderView { ppcvValue = value }] -> do
        checked `shouldBe` True
        value `shouldBeApprox` 0.5
      other -> expectationFailure ("unexpected parameter controls: " <> show other)
    pscvBaseRowIndex simControls `shouldBe` builtinStageRowCount + 5
    pscvTickEnabled simControls `shouldBe` True
    pscvAutoTickEnabled simControls `shouldBe` True
    pscvTickRateValue simControls `shouldBeApprox` 0.7
    mapMaybe fillRectOf (pipelineControlDrawCommands view)
      `shouldSatisfy` elem (pscvTickButtonRect simControls)

  it "maps pipeline widget regions through expanded diagnostic and parameter rows" $ do
    firstStage <- requireHead "expected built-in pipeline stages" allBuiltinStageIds
    let ui = pipelineUi firstStage
        widgets = pipelineStageWidgetRects wideLayout
               ++ pipelinePluginWidgetRects
                    (uiPluginNames ui)
                    (uiPluginExpanded ui)
                    (uiPluginParamSpecs ui)
                    (uiPluginDiagnosticLines ui)
                    wideLayout
    lookup WidgetSimTick widgets `shouldBe` Just (pipelineTickButtonRect (builtinStageRowCount + 5) wideLayout)
    lookup (WidgetPluginToggle "plugin-b") widgets `shouldBe` Just (pipelineCheckboxRect (builtinStageRowCount + 4) wideLayout)
    lookup (WidgetPluginParamCheck "plugin-a" "enabled") widgets
      `shouldBe` Just (pipelineParamCheckRect (builtinStageRowCount + 2) wideLayout)
    lookup (WidgetPluginParamSlider "plugin-a" "density") widgets
      `shouldBe` Just (pipelineParamBarRect (builtinStageRowCount + 3) wideLayout)

  it "converts plugin parameter input clicks and boolean toggles to service values" $ do
    firstStage <- requireHead "expected built-in pipeline stages" allBuiltinStageIds
    let ui = pipelineUi firstStage
        Rect (V2 barX _, V2 barW _) = pipelineParamBarRect 0 wideLayout
        midClick = V2 (barX + barW `div` 2) 0
        leftClick = V2 (barX - 20) 0
        rightClick = V2 (barX + barW + 20) 0
    pipelineParamValueFromClick wideLayout midClick (Just densitySpec) `shouldBeNumberApprox` 15
    pipelineParamValueFromClick wideLayout leftClick (Just densitySpec) `shouldBe` Number 10
    pipelineParamValueFromClick wideLayout rightClick (Just densitySpec) `shouldBe` Number 20
    pipelineParamValueFromClick wideLayout midClick Nothing `shouldBeNumberApprox` 0.5
    pipelineParamToggleValue ui "plugin-a" "enabled" `shouldBe` Bool False
    pipelineParamToggleValue ui "plugin-a" "missing" `shouldBe` Bool True

  it "emits scrolled pipeline labels for plugins, parameters, and simulation controls" $ do
    firstStage <- requireHead "expected built-in pipeline stages" allBuiltinStageIds
    let ui = (pipelineUi firstStage) { uiConfigScroll = 12 }
        labels = pipelineLabelViews fixedNow ui wideLayout
        labelTexts = map plvText labels
        findLabel labelText = fromMaybe (error "expected label") $
          findTextLabel labelText labels
    labelTexts `shouldSatisfy` any (Text.isPrefixOf "plugin-a [")
    labelTexts `shouldSatisfy` elem "Enabled"
    labelTexts `shouldSatisfy` elem "Density"
    labelTexts `shouldSatisfy` elem "Auto-tick"
    labelTexts `shouldSatisfy` elem "Rate: 7/s"
    let unscrolled = pipelineLabelViews fixedNow (pipelineUi firstStage) wideLayout
        scrolledDensity = findLabel "Density"
        unscrolledDensity = fromMaybe (error "expected unscrolled label") $
          findTextLabel "Density" unscrolled
    labelY scrolledDensity `shouldBe` labelY unscrolledDensity - 12

pipelineUi :: StageId -> UiState
pipelineUi firstStage = emptyUiState
  { uiConfigTab = ConfigPipeline
  , uiDisabledStages = Set.singleton firstStage
  , uiPluginNames = ["plugin-a", "plugin-b"]
  , uiDisabledPlugins = Set.singleton "plugin-b"
  , uiPluginExpanded = Map.singleton "plugin-a" True
  , uiPluginDiagnosticLines = Map.singleton "plugin-a" ["ready"]
  , uiPluginParamSpecs = Map.singleton "plugin-a" [enabledSpec, densitySpec]
  , uiPluginParams = Map.singleton "plugin-a" $ Map.fromList
      [ ("enabled", Bool True)
      , ("density", Number 15)
      ]
  , uiSimAutoTick = True
  , uiSimTickRate = 0.7
  }

readyData :: DataSnapshot
readyData = DataSnapshot
  { dsTerrainChunks = 1
  , dsBiomeChunks = 0
  , dsLastSeed = Nothing
  }

enabledSpec :: RPCParamSpec
enabledSpec = RPCParamSpec
  { rpsName = "enabled"
  , rpsLabel = "Enabled"
  , rpsType = ParamBool
  , rpsRange = Nothing
  , rpsDefault = Bool False
  , rpsTooltip = ""
  }

densitySpec :: RPCParamSpec
densitySpec = RPCParamSpec
  { rpsName = "density"
  , rpsLabel = "Density"
  , rpsType = ParamFloat
  , rpsRange = Just (Number 10, Number 20)
  , rpsDefault = Number 15
  , rpsTooltip = ""
  }

wideLayout :: Layout
wideLayout = layoutFor (V2 800 960) 0

fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

fillRectOf :: DrawCommand -> Maybe Rect
fillRectOf command = case command of
  DrawFillRect _ rect -> Just rect
  _ -> Nothing

findTextLabel :: Text -> [PipelineLabelView] -> Maybe PipelineLabelView
findTextLabel labelText = go
  where
    go [] = Nothing
    go (labelView:rest)
      | plvText labelView == labelText = Just labelView
      | otherwise = go rest

labelY :: PipelineLabelView -> Int
labelY labelView = case plvPosition labelView of
  V2 _ y -> y

requireHead :: String -> [a] -> IO a
requireHead _ (value:_) = pure value
requireHead label [] = expectationFailure label >> fail label

shouldBeApprox :: Float -> Float -> Expectation
shouldBeApprox actual expected = abs (actual - expected) `shouldSatisfy` (< 1.0e-5)

shouldBeNumberApprox :: Value -> Double -> Expectation
shouldBeNumberApprox (Number actual) expected =
  abs ((realToFrac actual :: Double) - expected) `shouldSatisfy` (< 1.0e-4)
shouldBeNumberApprox actual _ = expectationFailure ("expected JSON number, got: " <> show actual)
