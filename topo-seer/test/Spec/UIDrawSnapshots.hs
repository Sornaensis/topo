{-# LANGUAGE OverloadedStrings #-}

module Spec.UIDrawSnapshots (spec) where

import Actor.Log (LogLevel(..))
import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , emptyDataBrowserState
  , emptyUiState
  )
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Linear (V2(..))
import Seer.DataBrowser.Model
  ( DataBrowserAsyncError(..)
  , DataBrowserOperation(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserRequestId(..)
  , DataBrowserWorkerRequest(..)
  , dataBrowserOperationProgressText
  )
import Seer.Draw.OverlayInspector
  ( overlayInspectorBodyLines
  , overlayInspectorScrollLimit
  , overlayInspectorTitle
  , overlayInspectorViewScrollLimit
  , overlayInspectorVisibleLines
  )
import Seer.OverlayInspector.Model
import Test.Hspec
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , allOperations
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..))
import UI.Components.ConfigSliders
  ( configSliderDrawCommands
  , configSliderRowsForTab
  )
import UI.Components.DataBrowser
import UI.Components.LogFilter (LogFilterModel(..), logFilterDrawCommands)
import UI.DrawCommand (DrawCommand(..), TextPlacement(..))
import UI.Layout
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "UI draw-command snapshots" $ do
  it "captures log-filter and config-slider command shapes" $ do
    let logLayout = layoutFor (V2 800 600) 160
        sliderLayout = layoutFor (V2 800 960) 0
        logCommands = logFilterDrawCommands (LogFilterModel LogWarn) (logFilterRects logLayout)
        firstSlider = take 1 (configSliderRowsForTab ConfigClimate emptyUiState sliderLayout)
        sliderCommands = configSliderDrawCommands firstSlider
    map commandShape logCommands `shouldBe`
      [ "fill (682,441,22,22)"
      , "text centered (682,441,22,22) D"
      , "fill (710,441,22,22)"
      , "text centered (710,441,22,22) I"
      , "fill (738,441,22,22)"
      , "text centered (738,441,22,22) W"
      , "fill (766,441,22,22)"
      , "text centered (766,441,22,22) E"
      ]
    map commandShape sliderCommands `shouldBe`
      [ "fill (500,152,24,24)"
      , "fill (730,152,24,24)"
      , "fill (532,158,190,12)"
      , "fill (532,158,82,12)"
      ]

  it "uses operation-specific Data Browser progress copy" $ do
    map dataBrowserOperationProgressText
      [ DataBrowserLoadCatalogOperation
      , DataBrowserLoadPluginOperation
      , DataBrowserSelectResourceOperation
      , DataBrowserListOperation
      , DataBrowserCreateOperation
      , DataBrowserUpdateOperation
      , DataBrowserDeleteOperation
      ] `shouldBe`
      [ "Loading data sources…"
      , "Loading resources…"
      , "Loading records…"
      , "Loading page…"
      , "Creating…"
      , "Saving…"
      , "Deleting…"
      ]

  it "snapshots every overlay inspector view with explicit workflow copy" $ do
    let payload = Object mempty
        base = emptyOverlayInspectorModel
          { oimOverlayNames = ["roads"]
          , oimSelectedOverlay = Just "roads"
          , oimManagerPayload = Just payload
          , oimSchemaPayload = Just payload
          , oimProvenancePayload = Just payload
          , oimExportPayload = Just payload
          }
        view selected = openOverlayInspectorView selected base
    overlayInspectorTitle (view OverlayInspectorManagerView)
      `shouldBe` "Overlay Manager"
    overlayInspectorTitle (view OverlayInspectorSchemaView)
      `shouldBe` "Overlay Schema — roads"
    overlayInspectorTitle (view OverlayInspectorProvenanceView)
      `shouldBe` "Overlay Provenance — roads"
    overlayInspectorBodyLines (view OverlayInspectorExportView)
      `shouldSatisfy` any (Text.isInfixOf "Copy JSON or Save JSON")
    overlayInspectorBodyLines (view OverlayInspectorManagerView)
      `shouldSatisfy` any (Text.isInfixOf "Manager response")
    overlayInspectorBodyLines (view OverlayInspectorImportView)
      `shouldSatisfy` any (Text.isInfixOf "does not import or adopt")

  it "wraps inspector payloads to the visible width and clamps page scrolling" $ do
    let body = Rect (V2 0 0, V2 180 44)
        payload = String (Text.replicate 200 "x")
        inspector = (openOverlayInspectorView OverlayInspectorSchemaView
          emptyOverlayInspectorModel)
          { oimSchemaPayload = Just payload
          , oimScroll = maxBound
          }
        visible = overlayInspectorVisibleLines body inspector
    visible `shouldSatisfy` (not . null)
    visible `shouldSatisfy` all ((<= 21) . Text.length)
    overlayInspectorScrollLimit body inspector `shouldSatisfy` (> 0)
    let smallLayout = layoutFor (V2 580 480) 24
        longInspector = inspector
          { oimSchemaPayload = Just (String (Text.replicate 3000 "x")) }
    overlayInspectorViewScrollLimit smallLayout longInspector
      `shouldSatisfy` (> 30)

  it "captures Data Browser browse, pagination, create, and loading states" $ do
    let layout = layoutFor (V2 800 960) 0
        loaded = dataBrowserView resources loadedBrowserState layout
        loading = dataBrowserView resources loadingBrowserState layout
    fmap dbpcvRowIndex (dbvPageControls loaded) `shouldBe` Just 4
    fmap dbcbvRowIndex (dbvCreateButton loaded) `shouldBe` Just 5
    map commandShape (dataBrowserDrawCommands loaded) `shouldBe`
      [ "fill (508,152,238,24)"
      , "stroke (508,152,238,24)"
      , "fill (508,186,238,24)"
      , "stroke (508,186,238,24)"
      , "fill (508,220,238,24)"
      , "stroke (508,220,238,24)"
      , "fill (508,254,238,24)"
      , "stroke (508,254,238,24)"
      , "fill (508,290,40,20)"
      , "stroke (508,290,40,20)"
      , "fill (556,290,40,20)"
      , "stroke (556,290,40,20)"
      , "fill (508,324,28,20)"
      , "text at (516,328) +"
      ]
    fmap (rectShape . dblvRect) (dbvLoading loading)
      `shouldBe` Just "(508,220,238,24)"
    fmap dblvText (dbvLoading loading) `shouldBe` Just "Loading records…"
    dbvRecordRows loading `shouldBe` []
    dbvPageControls loading `shouldBe` Nothing
    dbvCreateButton loading `shouldBe` Nothing

  it "renders scoped list errors without a record selection" $ do
    let layout = layoutFor (V2 800 960) 0
        failed = dataBrowserView Map.empty catalogErrorBrowserState layout
    fmap dbevText (dbvError failed)
      `shouldBe` Just "Could not load data sources: catalog unavailable"
    dbvLoading failed `shouldBe` Nothing

    let listFailed = dataBrowserView resources initialListErrorBrowserState layout
    fmap dbevRowIndex (dbvError listFailed) `shouldBe` Just 2
    fmap dbcbvRowIndex (dbvCreateButton listFailed) `shouldBe` Just 3
    fmap dbevRect (dbvError listFailed)
      `shouldNotBe` fmap dbcbvRect (dbvCreateButton listFailed)

commandShape :: DrawCommand -> Text
commandShape command = case command of
  DrawFillRect _ rect -> "fill " <> rectShape rect
  DrawStrokeRect _ rect -> "stroke " <> rectShape rect
  DrawText _ placement label -> "text " <> placementShape placement <> " " <> label
  DrawLine _ start end -> "line " <> pointShape start <> " " <> pointShape end
  DrawClip Nothing -> "clip clear"
  DrawClip (Just rect) -> "clip " <> rectShape rect

placementShape :: TextPlacement -> Text
placementShape placement = case placement of
  TextAt point -> "at " <> pointShape point
  TextCentered rect -> "centered " <> rectShape rect
  TextLeft rect -> "left " <> rectShape rect
  TextLabelAbove rect -> "above " <> rectShape rect
  TextLabelLeft rect -> "left-label " <> rectShape rect

rectShape :: Rect -> Text
rectShape (Rect (V2 x y, V2 w h)) =
  "(" <> tshow x <> "," <> tshow y <> "," <> tshow w <> "," <> tshow h <> ")"

pointShape :: V2 Int -> Text
pointShape (V2 x y) = "(" <> tshow x <> "," <> tshow y <> ")"

tshow :: Show a => a -> Text
tshow = Text.pack . show

resources :: Map.Map Text [DataResourceSchema]
resources = Map.singleton "atlas" [citiesSchema]

loadedBrowserState :: DataBrowserState
loadedBrowserState = emptyDataBrowserState
  { dbsSelectedPlugin = Just "atlas"
  , dbsSelectedResource = Just "cities"
  , dbsRecords = [record1, record2]
  , dbsSelectedRowIndex = Just 1
  }

loadingBrowserState :: DataBrowserState
loadingBrowserState = emptyDataBrowserState
  { dbsSelectedPlugin = Just "atlas"
  , dbsSelectedResource = Just "cities"
  , dbsLoading = True
  , dbsPendingRequest = Just (DataBrowserPendingEnvelope
      (DataBrowserRequestId 4)
      DataBrowserSelectResourceOperation
      (DataBrowserSelectResourceRequest "atlas" "cities"))
  }

initialListErrorBrowserState :: DataBrowserState
initialListErrorBrowserState = emptyDataBrowserState
  { dbsSelectedPlugin = Just "atlas"
  , dbsSelectedResource = Just "cities"
  , dbsAsyncError = Just (DataBrowserAsyncError
      (DataBrowserRequestId 5)
      DataBrowserSelectResourceOperation
      (DataBrowserSelectResourceRequest "atlas" "cities")
      "list unavailable")
  }

catalogErrorBrowserState :: DataBrowserState
catalogErrorBrowserState = emptyDataBrowserState
  { dbsAsyncError = Just (DataBrowserAsyncError
      (DataBrowserRequestId 3)
      DataBrowserLoadCatalogOperation
      DataBrowserLoadCatalogRequest
      "catalog unavailable")
  }

record1 :: DataRecord
record1 = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("name", String "Alpha")
  ]

record2 :: DataRecord
record2 = DataRecord $ Map.fromList
  [ ("id", Number 2)
  , ("name", String "Beta")
  ]

citiesSchema :: DataResourceSchema
citiesSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "cities"
  , drsLabel = "Cities"
  , drsHexBound = False
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsFields =
      [ DataFieldDef "id" DFInt "ID" False Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]
  , drsOperations = allOperations { doCreate = True, doPage = True }
  , drsPagination = DataPagination 2 10 0
  }
