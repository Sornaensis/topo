{-# LANGUAGE OverloadedStrings #-}

module Spec.DataDetailPopover (spec) where

import Actor.UI (DataBrowserState(..), UiState(..), emptyDataBrowserState, emptyUiState)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector
import Seer.DataBrowser.Model (DataBrowserValidationError(..))
import Seer.Draw.Config.DataDetail
import Test.Hspec
import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , allOperations
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..))

spec :: Spec
spec = describe "Data detail popover view model" $ do
  it "enumerates nested record and ADT display rows" $ do
    let view = selectedView baseBrowserState
        rows = ddvFields view
    map ddfPath rows `shouldBe`
      [ "id"
      , "profile"
      , "profile.age"
      , "profile.name"
      , "shape"
      , "shape.Circle.0"
      , "shape.Circle.1"
      , "shape.Point.0"
      , "shape.Point.1"
      , "active"
      ]
    ddfControl (rowByPath "profile" rows) `shouldBe` DataDetailNoValue
    ddfDepth (rowByPath "shape.Circle.1" rows) `shouldBe` 2
    ddfValueText (rowByPath "profile.age" rows) `shouldBe` "42.0"
    ddfValueText (rowByPath "shape.Circle.1" rows) `shouldBe` "red"
    ddfControl (rowByPath "shape.Circle.1" rows) `shouldBe` DataDetailValueDisplay
    lookupFieldType "shape.Circle.1" detailFields `shouldBe` Just DFText

  it "chooses edit controls for nested record and ADT leaf fields" $ do
    let dbs = baseBrowserState
          { dbsEditMode = True
          , dbsFocusedField = Just "profile.name"
          , dbsTextCursor = 3
          , dbsEditValues = Map.fromList
              [ ("profile.name", String "Nested edit")
              , ("shape.Circle.1", String "blue")
              ]
          }
        rows = ddvFields (selectedView dbs)
    ddfControl (rowByPath "profile.age" rows) `shouldBe` DataDetailNumericStepper False "42."
    ddfControl (rowByPath "profile.name" rows) `shouldBe` DataDetailTextInput True "Nes"
    ddfValueText (rowByPath "shape.Circle.1" rows) `shouldBe` "blue"
    ddfControl (rowByPath "shape.Circle.0" rows) `shouldBe` DataDetailNumericStepper False "12."
    ddfControl (rowByPath "shape.Circle.1" rows) `shouldBe` DataDetailTextInput False "blu"
    ddfControl (rowByPath "active" rows) `shouldBe` DataDetailBoolToggle False "tru"

  it "surfaces validation rows and delete confirmation dialog copy" $ do
    let dbs = baseBrowserState
          { dbsDeleteConfirm = True
          , dbsValidationErrors =
              [ DataBrowserValidationError (Just "profile.age") "expected int"
              , DataBrowserValidationError Nothing "backend rejected"
              ]
          }
        view = selectedView dbs
    ddvValidationRows view `shouldBe`
      [ DataDetailValidationRow (Just "profile.age") "profile.age: expected int"
      , DataDetailValidationRow Nothing "backend rejected"
      ]
    ddvDeleteConfirm view `shouldBe`
      Just (DataDetailDeleteConfirmView "Delete record?" "Delete" "Cancel")

selectedView :: DataBrowserState -> DataDetailPopoverView
selectedView dbs = case dataDetailPopoverView (uiWithBrowser dbs) of
  Just view -> view
  Nothing -> error "expected a selected record detail popover"

uiWithBrowser :: DataBrowserState -> UiState
uiWithBrowser dbs = emptyUiState
  { uiDataResources = Map.singleton "atlas" [detailSchema]
  , uiDataBrowser = dbs
  }

baseBrowserState :: DataBrowserState
baseBrowserState = emptyDataBrowserState
  { dbsSelectedPlugin = Just "atlas"
  , dbsSelectedResource = Just "details"
  , dbsSelectedRecord = Just detailRecord
  , dbsSelectedRecordKey = Just (Number 1)
  , dbsSelectedRowIndex = Just 2
  , dbsExpandedFields = Set.fromList ["profile", "shape"]
  }

detailRecord :: DataRecord
detailRecord = DataRecord $ Map.fromList
  [ ("id", Number 1)
  , ("profile", object
      [ "age" .= Number 42
      , "name" .= String "Nested"
      ])
  , ("shape", object
      [ "constructor" .= String "Circle"
      , "fields" .= Array (Vector.fromList [Number 12.5, String "red"])
      ])
  , ("active", Bool True)
  ]

detailSchema :: DataResourceSchema
detailSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "details"
  , drsLabel = "Details"
  , drsHexBound = False
  , drsFields = detailFields
  , drsOperations = allOperations
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = DataPagination 20 100 0
  }

detailFields :: [DataFieldDef]
detailFields =
  [ DataFieldDef "id" DFInt "ID" False Nothing
  , DataFieldDef "profile" (DFRecord
      [ DataFieldDef "age" DFInt "Age" True Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]) "Profile" True Nothing
  , DataFieldDef "shape" (DFAdt
      [ DataConstructorDef "Circle" [DFFloat, DFText]
      , DataConstructorDef "Point" [DFInt, DFInt]
      ]) "Shape" True Nothing
  , DataFieldDef "active" DFBool "Active" True Nothing
  ]

rowByPath :: Text -> [DataDetailFieldRow] -> DataDetailFieldRow
rowByPath path rows = case [row | row <- rows, ddfPath row == path] of
  row:_ -> row
  [] -> error ("missing row: " <> show path)
