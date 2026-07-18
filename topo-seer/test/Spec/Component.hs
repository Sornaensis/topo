{-# LANGUAGE OverloadedStrings #-}

module Spec.Component (spec) where

import Actor.Log (LogLevel(..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import Linear (V2(..), V4)
import Test.Hspec
import UI.Component
import UI.Components.LogFilter
import UI.DrawCommand
import UI.WidgetId (WidgetId(..))
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "component reducers and draw commands" $ do
  it "routes normalized primary clicks to stable component and widget ids" $ do
    let widgets = logFilterWidgets logRects
        routed = routeWidgets widgets (UiPointerDown (V2 25 5) UiPointerPrimary)
    routedComponent routed `shouldBe` Just componentLogPanel
    routedWidget routed `shouldBe` Just WidgetLogWarn
    logFilterRouteEvent routed `shouldBe` Just (LogFilterSelect LogWarn)

  it "does not reduce hover, release, or secondary-button log-filter events" $ do
    let widgets = logFilterWidgets logRects
        route = routeWidgets widgets
    logFilterRouteEvent (route (UiPointerMove (V2 25 5))) `shouldBe` Nothing
    logFilterRouteEvent (route (UiPointerUp (V2 25 5) UiPointerPrimary)) `shouldBe` Nothing
    logFilterRouteEvent (route (UiPointerDown (V2 25 5) UiPointerSecondary)) `shouldBe` Nothing

  it "keeps config toggle routing inside the config component boundary" $ do
    componentForWidget WidgetConfigToggle `shouldBe` componentConfigPanel

  it "routes every overlay inspector action through the View component" $ do
    map componentForWidget
      [ WidgetOverlayManager
      , WidgetOverlaySchema
      , WidgetOverlayProvenance
      , WidgetOverlayExport
      , WidgetOverlayImportValidate
      ] `shouldBe` replicate 5 componentViewPanel

  it "exposes component hit regions for pure routing" $ do
    let component = logFilterComponent logRects
        regions = componentRegions component (LogFilterModel LogInfo)
    componentId component `shouldBe` componentLogPanel
    map hitRegionComponent regions `shouldBe` replicate 4 componentLogPanel
    map hitRegionWidget regions `shouldBe`
      [ WidgetLogDebug, WidgetLogInfo, WidgetLogWarn, WidgetLogError ]

  it "reduces log-filter actions without touching SDL" $ do
    let ReducerResult model effects =
          logFilterReducer (LogFilterModel LogInfo) (LogFilterSelect LogError)
    model `shouldBe` LogFilterModel LogError
    effects `shouldBe` [LogFilterSetMinLevel LogError]

  it "describes log-filter drawing as pure commands" $ do
    let commands = logFilterDrawCommands (LogFilterModel LogWarn) logRects
    length commands `shouldBe` 8
    mapMaybe commandLabel commands `shouldBe` ["D", "I", "W", "E"]
    mapMaybe commandRect commands `shouldBe`
      [ debugRect, debugRect, infoRect, infoRect, warnRect, warnRect, errorRect, errorRect ]

  it "marks the active log filter by changing its fill command" $ do
    let debugActive = mapMaybe fillColor (logFilterDrawCommands (LogFilterModel LogDebug) logRects)
        warnActive = mapMaybe fillColor (logFilterDrawCommands (LogFilterModel LogWarn) logRects)
    case (debugActive, warnActive) of
      (debugColor:_:warnInactive:_, debugInactive:_:warnColor:_ ) -> do
        debugColor `shouldNotBe` debugInactive
        warnInactive `shouldNotBe` warnColor
      _ -> expectationFailure "expected one fill command per log level"

commandLabel :: DrawCommand -> Maybe Text
commandLabel command = case command of
  DrawText _ _ label -> Just label
  _ -> Nothing

commandRect :: DrawCommand -> Maybe Rect
commandRect command = case command of
  DrawFillRect _ rect -> Just rect
  DrawStrokeRect _ rect -> Just rect
  DrawText _ (TextCentered rect) _ -> Just rect
  DrawText _ (TextLeft rect) _ -> Just rect
  DrawText _ (TextLabelAbove rect) _ -> Just rect
  DrawText _ (TextLabelLeft rect) _ -> Just rect
  DrawText _ (TextAt _) _ -> Nothing
  DrawClip maybeRect -> maybeRect

fillColor :: DrawCommand -> Maybe (V4 Word8)
fillColor command = case command of
  DrawFillRect color _ -> Just color
  _ -> Nothing

debugRect :: Rect
debugRect = Rect (V2 0 0, V2 10 10)

infoRect :: Rect
infoRect = Rect (V2 10 0, V2 10 10)

warnRect :: Rect
warnRect = Rect (V2 20 0, V2 10 10)

errorRect :: Rect
errorRect = Rect (V2 30 0, V2 10 10)

logRects :: (Rect, Rect, Rect, Rect)
logRects = (debugRect, infoRect, warnRect, errorRect)
