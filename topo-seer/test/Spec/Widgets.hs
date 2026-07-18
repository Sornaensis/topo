{-# LANGUAGE OverloadedStrings #-}

module Spec.Widgets (spec) where

import Actor.UI.State (emptyUiState)
import qualified Data.Text as Text
import Seer.Command.Handlers.Widgets
  ( WidgetCapability(..)
  , WidgetClickSupport(..)
  , widgetCapability
  )
import Test.Hspec
import Linear (V2(..))
import UI.Font (boundedTextWithEllipsis)
import UI.WidgetId (WidgetId(..))
import UI.Widgets

spec :: Spec
spec = describe "UI.Widgets" $ do
  it "detects points inside rect" $ do
    let rect = Rect (V2 10 10, V2 20 20)
    containsPoint rect (V2 10 10) `shouldBe` True
    containsPoint rect (V2 29 29) `shouldBe` True
    containsPoint rect (V2 30 30) `shouldBe` False

  it "insets rect" $ do
    let rect = Rect (V2 0 0, V2 20 20)
        Rect (V2 x y, V2 w h) = insetRect 2 rect
    (x, y, w, h) `shouldBe` (2, 2, 16, 16)

  it "bounds text for rendering with an ellipsis" $ do
    let (bounded, clipped) = boundedTextWithEllipsis 8 (Text.replicate 100 "x")
    clipped `shouldBe` True
    Text.length bounded `shouldBe` 8
    bounded `shouldSatisfy` Text.isSuffixOf "\x2026"

  it "leaves non-pathological text unchanged when bounding" $ do
    boundedTextWithEllipsis 8 "short" `shouldBe` ("short", False)

  it "classifies every visible overlay action as clickable" $ do
    map (wcSupport . widgetCapability emptyUiState)
      [ WidgetOverlayManager
      , WidgetOverlaySchema
      , WidgetOverlayProvenance
      , WidgetOverlayExport
      , WidgetOverlayImportValidate
      ] `shouldBe` replicate 5 WidgetClickable
