{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure view-model and draw-command helpers for the Data Browser config tab.
module UI.Components.DataBrowser
  ( DataBrowserView(..)
  , DataBrowserPluginRowView(..)
  , DataBrowserResourceRowView(..)
  , DataBrowserRecordRowView(..)
  , DataBrowserPageControlsView(..)
  , DataBrowserCreateButtonView(..)
  , DataBrowserLoadingView(..)
  , dataBrowserView
  , dataBrowserViewAtScroll
  , dataBrowserDrawCommands
  ) where

import Actor.UI (DataBrowserState(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Linear (V2(..))
import Topo.Plugin.DataResource
  ( DataOperations(..)
  , DataResourceSchema(..)
  )
import UI.DrawCommand (DrawCommand, fillRect, strokeRect, textAt)
import UI.Layout
  ( Layout
  , dataBrowserCreateButtonRect
  , dataBrowserItemRect
  , dataBrowserPageNextRect
  , dataBrowserPagePrevRect
  )
import UI.Theme
  ( colDataCreateBtn
  , colDataDetailFieldValue
  , colDataListSelActive
  , colDataListSelActiveBorder
  , colDataListSelInactive
  , colDataListSelInactiveBorder
  , colDataLoadingIndicator
  , colDataRecordBg
  , colDataRecordBorder
  , colDataRecordSelected
  , colDataResourceActive
  , colDataResourceActiveBorder
  , colDataResourceInactive
  , colDataResourceInactiveBorder
  )
import UI.Widgets (Rect(..))

-- | Complete draw-focused view model for the Data Browser tab.
data DataBrowserView = DataBrowserView
  { dbvPluginRows :: ![DataBrowserPluginRowView]
  , dbvResourceRows :: ![DataBrowserResourceRowView]
  , dbvRecordRows :: ![DataBrowserRecordRowView]
  , dbvLoading :: !(Maybe DataBrowserLoadingView)
  , dbvPageControls :: !(Maybe DataBrowserPageControlsView)
  , dbvCreateButton :: !(Maybe DataBrowserCreateButtonView)
  } deriving (Eq, Show)

data DataBrowserPluginRowView = DataBrowserPluginRowView
  { dbprvPluginName :: !Text
  , dbprvRowIndex :: !Int
  , dbprvRect :: !Rect
  , dbprvSelected :: !Bool
  } deriving (Eq, Show)

data DataBrowserResourceRowView = DataBrowserResourceRowView
  { dbrrvPluginName :: !Text
  , dbrrvResourceName :: !Text
  , dbrrvRowIndex :: !Int
  , dbrrvRect :: !Rect
  , dbrrvSelected :: !Bool
  } deriving (Eq, Show)

data DataBrowserRecordRowView = DataBrowserRecordRowView
  { dbrcvRecordIndex :: !Int
  , dbrcvRowIndex :: !Int
  , dbrcvRect :: !Rect
  , dbrcvSelected :: !Bool
  } deriving (Eq, Show)

data DataBrowserLoadingView = DataBrowserLoadingView
  { dblvRowIndex :: !Int
  , dblvRect :: !Rect
  } deriving (Eq, Show)

data DataBrowserPageControlsView = DataBrowserPageControlsView
  { dbpcvPluginName :: !Text
  , dbpcvResourceName :: !Text
  , dbpcvRowIndex :: !Int
  , dbpcvPrevRect :: !Rect
  , dbpcvNextRect :: !Rect
  } deriving (Eq, Show)

data DataBrowserCreateButtonView = DataBrowserCreateButtonView
  { dbcbvRowIndex :: !Int
  , dbcbvRect :: !Rect
  } deriving (Eq, Show)

-- | Build a Data Browser view at the unscrolled layout positions.
dataBrowserView :: Map.Map Text [DataResourceSchema] -> DataBrowserState -> Layout -> DataBrowserView
dataBrowserView = dataBrowserViewAtScroll 0

-- | Build a Data Browser view with row rectangles shifted upward by the active
-- scroll offset, matching the clipped config-panel drawing path.
dataBrowserViewAtScroll
  :: Int
  -> Map.Map Text [DataResourceSchema]
  -> DataBrowserState
  -> Layout
  -> DataBrowserView
dataBrowserViewAtScroll scrollY resources dbs layout = DataBrowserView
  { dbvPluginRows = pluginRows
  , dbvResourceRows = resourceRows
  , dbvRecordRows = recordRows
  , dbvLoading = loadingView
  , dbvPageControls = pageControls
  , dbvCreateButton = createButton
  }
  where
    shift = shiftRectY (negate scrollY)
    itemRect rowIdx = shift (dataBrowserItemRect rowIdx layout)
    pluginNames = Map.keys resources
    selectedPlugin = dbsSelectedPlugin dbs
    selectedResource = dbsSelectedResource dbs

    pluginRows =
      [ DataBrowserPluginRowView
          { dbprvPluginName = pluginName
          , dbprvRowIndex = rowIdx
          , dbprvRect = itemRect rowIdx
          , dbprvSelected = selectedPlugin == Just pluginName
          }
      | (rowIdx, pluginName) <- zip [0..] pluginNames
      ]

    resourceOffset = length pluginNames
    selectedSchemas = case selectedPlugin of
      Nothing -> []
      Just pluginName -> Map.findWithDefault [] pluginName resources

    resourceRows =
      [ DataBrowserResourceRowView
          { dbrrvPluginName = pluginName
          , dbrrvResourceName = drsName schema
          , dbrrvRowIndex = rowIdx
          , dbrrvRect = itemRect rowIdx
          , dbrrvSelected = selectedResource == Just (drsName schema)
          }
      | Just pluginName <- [selectedPlugin]
      , (resourceIdx, schema) <- zip [0..] selectedSchemas
      , let rowIdx = resourceOffset + resourceIdx
      ]

    recordOffset = resourceOffset + length selectedSchemas
    recordRows =
      [ DataBrowserRecordRowView
          { dbrcvRecordIndex = recordIdx
          , dbrcvRowIndex = rowIdx
          , dbrcvRect = itemRect rowIdx
          , dbrcvSelected = dbsSelectedRowIndex dbs == Just recordIdx
          }
      | recordIdx <- [0 .. length (dbsRecords dbs) - 1]
      , let rowIdx = recordOffset + recordIdx
      ]

    pageRow = recordOffset + length (dbsRecords dbs)
    selectedSchema = do
      resourceName <- selectedResource
      findSchema resourceName selectedSchemas
    canPage = maybe False (doPage . drsOperations) selectedSchema
    canCreate = maybe False (doCreate . drsOperations) selectedSchema

    loadingView
      | dbsLoading dbs =
          let Rect (V2 lx ly, V2 _lw lh) = itemRect pageRow
          in Just DataBrowserLoadingView
            { dblvRowIndex = pageRow
            , dblvRect = Rect (V2 lx ly, V2 40 lh)
            }
      | otherwise = Nothing

    pageControls = case (selectedPlugin, selectedResource) of
      (Just pluginName, Just resourceName)
        | canPage && not (null (dbsRecords dbs)) ->
            Just DataBrowserPageControlsView
              { dbpcvPluginName = pluginName
              , dbpcvResourceName = resourceName
              , dbpcvRowIndex = pageRow
              , dbpcvPrevRect = shift (dataBrowserPagePrevRect pageRow layout)
              , dbpcvNextRect = shift (dataBrowserPageNextRect pageRow layout)
              }
      _ -> Nothing

    createRow = pageRow + if canPage && not (null (dbsRecords dbs)) then 1 else 0
    createButton
      | canCreate = Just DataBrowserCreateButtonView
          { dbcbvRowIndex = createRow
          , dbcbvRect = shift (dataBrowserCreateButtonRect createRow layout)
          }
      | otherwise = Nothing

-- | Convert a Data Browser view model to renderer-neutral draw commands.
dataBrowserDrawCommands :: DataBrowserView -> [DrawCommand]
dataBrowserDrawCommands view =
  concatMap pluginRowCommands (dbvPluginRows view)
    ++ concatMap resourceRowCommands (dbvResourceRows view)
    ++ concatMap recordRowCommands (dbvRecordRows view)
    ++ maybe [] loadingCommands (dbvLoading view)
    ++ maybe [] pageControlCommands (dbvPageControls view)
    ++ maybe [] createButtonCommands (dbvCreateButton view)

pluginRowCommands :: DataBrowserPluginRowView -> [DrawCommand]
pluginRowCommands row =
  [ fillRect fillColor (dbprvRect row)
  , strokeRect borderColor (dbprvRect row)
  ]
  where
    fillColor = if dbprvSelected row then colDataListSelActive else colDataListSelInactive
    borderColor = if dbprvSelected row then colDataListSelActiveBorder else colDataListSelInactiveBorder

resourceRowCommands :: DataBrowserResourceRowView -> [DrawCommand]
resourceRowCommands row =
  [ fillRect fillColor (dbrrvRect row)
  , strokeRect borderColor (dbrrvRect row)
  ]
  where
    fillColor = if dbrrvSelected row then colDataResourceActive else colDataResourceInactive
    borderColor = if dbrrvSelected row then colDataResourceActiveBorder else colDataResourceInactiveBorder

recordRowCommands :: DataBrowserRecordRowView -> [DrawCommand]
recordRowCommands row =
  [ fillRect fillColor (dbrcvRect row)
  , strokeRect colDataRecordBorder (dbrcvRect row)
  ]
  where
    fillColor = if dbrcvSelected row then colDataRecordSelected else colDataRecordBg

loadingCommands :: DataBrowserLoadingView -> [DrawCommand]
loadingCommands view = [fillRect colDataLoadingIndicator (dblvRect view)]

pageControlCommands :: DataBrowserPageControlsView -> [DrawCommand]
pageControlCommands view =
  [ fillRect colDataResourceInactive (dbpcvPrevRect view)
  , strokeRect colDataResourceInactiveBorder (dbpcvPrevRect view)
  , fillRect colDataResourceInactive (dbpcvNextRect view)
  , strokeRect colDataResourceInactiveBorder (dbpcvNextRect view)
  ]

createButtonCommands :: DataBrowserCreateButtonView -> [DrawCommand]
createButtonCommands view =
  [ fillRect colDataCreateBtn rect
  , textAt colDataDetailFieldValue (V2 (x + 8) (y + (h - 12) `div` 2)) "+"
  ]
  where
    rect@(Rect (V2 x y, V2 _w h)) = dbcbvRect view

findSchema :: Text -> [DataResourceSchema] -> Maybe DataResourceSchema
findSchema resourceName = foldr match Nothing
  where
    match schema acc
      | drsName schema == resourceName = Just schema
      | otherwise = acc

shiftRectY :: Int -> Rect -> Rect
shiftRectY dy (Rect (V2 x y, V2 w h)) = Rect (V2 x (y + dy), V2 w h)
