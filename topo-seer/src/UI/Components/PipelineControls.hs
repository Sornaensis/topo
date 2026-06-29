{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure view-model, draw-command, widget-region, and input helpers for the
-- Pipeline config tab.
module UI.Components.PipelineControls
  ( PipelineControlsView(..)
  , PipelineStageRowView(..)
  , PipelinePluginRowView(..)
  , PipelineParamControlView(..)
  , PipelineSimulationControlsView(..)
  , PipelineLabelView(..)
  , pipelineControlsView
  , pipelineControlsViewAtScroll
  , pipelineControlDrawCommands
  , pipelineLabelViews
  , pipelineStageWidgetRects
  , pipelinePluginWidgetRects
  , pipelineParamValueFromClick
  , pipelineParamToggleValue
  , normalizePipelineParam
  , pipelineScrollOffset
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.PluginManager.Types
  ( PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , pluginLifecycleStateText
  )
import Actor.UI.State
  ( ConfigTab(..)
  , UiState(..)
  , builtinStageRowCount
  , configRowCount
  , pluginRowIndex
  , pluginRowsWithParams
  )
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Word (Word8)
import Linear (V2(..), V4)
import Topo.Pipeline.Stage (StageId, allBuiltinStageIds, stageCanonicalName)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.DrawCommand (DrawCommand, fillRect, line, strokeRect)
import UI.Layout
  ( Layout
  , configRowTopPad
  , configScrollAreaRect
  , configScrollRowRect
  , pipelineCheckboxRect
  , pipelineExpandRect
  , pipelineMoveDownRect
  , pipelineMoveUpRect
  , pipelineParamBarRect
  , pipelineParamCheckRect
  , pipelineTickButtonRect
  , pipelineTickRateBarRect
  )
import UI.Theme
import UI.WidgetId (WidgetId(..))
import UI.Widgets (Rect(..))

-- | Complete view model for the Pipeline tab controls.
data PipelineControlsView = PipelineControlsView
  { pcvStageRows :: ![PipelineStageRowView]
  , pcvPluginRows :: ![PipelinePluginRowView]
  , pcvParamControls :: ![PipelineParamControlView]
  , pcvSimulationControls :: !PipelineSimulationControlsView
  } deriving (Eq, Show)

-- | One built-in pipeline-stage checkbox row.
data PipelineStageRowView = PipelineStageRowView
  { psrvStageId :: !StageId
  , psrvRowIndex :: !Int
  , psrvCheckboxRect :: !Rect
  , psrvDisabled :: !Bool
  } deriving (Eq, Show)

-- | One plugin row, including enable, move, and expand controls.
data PipelinePluginRowView = PipelinePluginRowView
  { pprvPluginName :: !Text
  , pprvRowIndex :: !Int
  , pprvCheckboxRect :: !Rect
  , pprvDisabled :: !Bool
  , pprvMoveUpRect :: !Rect
  , pprvMoveDownRect :: !Rect
  , pprvExpandRect :: !Rect
  , pprvExpanded :: !Bool
  } deriving (Eq, Show)

-- | One expanded plugin-parameter control row.
data PipelineParamControlView
  = PipelineParamCheckView
      { ppcvPluginName :: !Text
      , ppcvParamName :: !Text
      , ppcvParamSpec :: !RPCParamSpec
      , ppcvRowIndex :: !Int
      , ppcvRect :: !Rect
      , ppcvChecked :: !Bool
      }
  | PipelineParamSliderView
      { ppcvPluginName :: !Text
      , ppcvParamName :: !Text
      , ppcvParamSpec :: !RPCParamSpec
      , ppcvRowIndex :: !Int
      , ppcvRect :: !Rect
      , ppcvValue :: !Float
      }
  deriving (Eq, Show)

-- | Simulation controls rendered after the built-in and plugin rows.
data PipelineSimulationControlsView = PipelineSimulationControlsView
  { pscvBaseRowIndex :: !Int
  , pscvTickButtonRect :: !Rect
  , pscvTickEnabled :: !Bool
  , pscvAutoTickRect :: !Rect
  , pscvAutoTickEnabled :: !Bool
  , pscvTickRateRect :: !Rect
  , pscvTickRateValue :: !Float
  } deriving (Eq, Show)

-- | A text label in the Pipeline tab.  'plvMaxWidth' requests truncation.
data PipelineLabelView = PipelineLabelView
  { plvPosition :: !(V2 Int)
  , plvColor :: !(V4 Word8)
  , plvText :: !Text
  , plvMaxWidth :: !(Maybe Int)
  } deriving (Eq, Show)

pipelineControlsView :: UiState -> DataSnapshot -> Layout -> PipelineControlsView
pipelineControlsView ui dataSnap layout =
  pipelineControlsViewAtScroll (pipelineScrollOffset ui layout) ui dataSnap layout

pipelineControlsViewAtScroll :: Int -> UiState -> DataSnapshot -> Layout -> PipelineControlsView
pipelineControlsViewAtScroll scrollY ui dataSnap layout =
  PipelineControlsView
    { pcvStageRows = stageRows
    , pcvPluginRows = pluginRows
    , pcvParamControls = concat paramRows
    , pcvSimulationControls = simControls
    }
  where
    shift = shiftRectY (negate scrollY)
    stageRows =
      [ PipelineStageRowView
          { psrvStageId = sid
          , psrvRowIndex = idx
          , psrvCheckboxRect = shift (pipelineCheckboxRect idx layout)
          , psrvDisabled = Set.member sid (uiDisabledStages ui)
          }
      | (idx, sid) <- zip [0..] allBuiltinStageIds
      ]
    pluginRowsAndParams =
      [ ( pluginRowView shift idx name
        , pluginParamRows shift ui layout name (pluginRowIndex ui idx)
        )
      | (idx, name) <- zip [0..] (uiPluginNames ui)
      ]
    pluginRows = map fst pluginRowsAndParams
    paramRows = map snd pluginRowsAndParams
    simOffset = builtinStageRowCount + pluginRowsWithParams ui
    simControls = PipelineSimulationControlsView
      { pscvBaseRowIndex = simOffset
      , pscvTickButtonRect = shift (pipelineTickButtonRect simOffset layout)
      , pscvTickEnabled = dsTerrainChunks dataSnap > 0
      , pscvAutoTickRect = shift (pipelineCheckboxRect (simOffset + 1) layout)
      , pscvAutoTickEnabled = uiSimAutoTick ui
      , pscvTickRateRect = shift (pipelineTickRateBarRect (simOffset + 2) layout)
      , pscvTickRateValue = uiSimTickRate ui
      }
    pluginRowView shiftRect idx name =
      let rowIndex = pluginRowIndex ui idx
      in PipelinePluginRowView
        { pprvPluginName = name
        , pprvRowIndex = rowIndex
        , pprvCheckboxRect = shiftRect (pipelineCheckboxRect rowIndex layout)
        , pprvDisabled = Set.member name (uiDisabledPlugins ui)
        , pprvMoveUpRect = shiftRect (pipelineMoveUpRect rowIndex layout)
        , pprvMoveDownRect = shiftRect (pipelineMoveDownRect rowIndex layout)
        , pprvExpandRect = shiftRect (pipelineExpandRect rowIndex layout)
        , pprvExpanded = Map.findWithDefault False name (uiPluginExpanded ui)
        }

pluginParamRows :: (Rect -> Rect) -> UiState -> Layout -> Text -> Int -> [PipelineParamControlView]
pluginParamRows shiftRect ui layout pluginName rowIndex
  | not (Map.findWithDefault False pluginName (uiPluginExpanded ui)) = []
  | otherwise =
      [ paramControl pIdx spec
      | (pIdx, spec) <- zip [0..] specs
      ]
  where
    specs = Map.findWithDefault [] pluginName (uiPluginParamSpecs ui)
    params = Map.findWithDefault Map.empty pluginName (uiPluginParams ui)
    detailCount = length (Map.findWithDefault [] pluginName (uiPluginDiagnosticLines ui))
    paramControl pIdx spec =
      let paramRowIdx = rowIndex + 1 + detailCount + pIdx
      in case rpsType spec of
        ParamBool -> PipelineParamCheckView
          { ppcvPluginName = pluginName
          , ppcvParamName = rpsName spec
          , ppcvParamSpec = spec
          , ppcvRowIndex = paramRowIdx
          , ppcvRect = shiftRect (pipelineParamCheckRect paramRowIdx layout)
          , ppcvChecked = pipelineParamBoolValue params (rpsName spec)
          }
        _ -> PipelineParamSliderView
          { ppcvPluginName = pluginName
          , ppcvParamName = rpsName spec
          , ppcvParamSpec = spec
          , ppcvRowIndex = paramRowIdx
          , ppcvRect = shiftRect (pipelineParamBarRect paramRowIdx layout)
          , ppcvValue = pipelineParamNormalizedValue spec params
          }

pipelineControlDrawCommands :: PipelineControlsView -> [DrawCommand]
pipelineControlDrawCommands view =
  concatMap stageDrawCommands (pcvStageRows view)
    ++ concatMap pluginDrawCommands (pcvPluginRows view)
    ++ concatMap paramDrawCommands (pcvParamControls view)
    ++ simulationDrawCommands (pcvSimulationControls view)

stageDrawCommands :: PipelineStageRowView -> [DrawCommand]
stageDrawCommands row =
  [ fillRect checkColor (psrvCheckboxRect row)
  , strokeRect borderColor (psrvCheckboxRect row)
  ]
  where
    checkColor = if psrvDisabled row then colPipelineCheckDisabled else colPipelineCheckEnabled
    borderColor = if psrvDisabled row then colPipelineCheckDisabledBorder else colPipelineCheckEnabledBorder

pluginDrawCommands :: PipelinePluginRowView -> [DrawCommand]
pluginDrawCommands row =
  [ fillRect pluginColor (pprvCheckboxRect row)
  , strokeRect pluginBorder (pprvCheckboxRect row)
  , fillRect colPipelineArrowBg (pprvMoveUpRect row)
  , strokeRect colPipelineArrowBorder (pprvMoveUpRect row)
  ]
  ++ arrowLines ArrowUp (pprvMoveUpRect row)
  ++ [ fillRect colPipelineArrowBg (pprvMoveDownRect row)
     , strokeRect colPipelineArrowBorder (pprvMoveDownRect row)
     ]
  ++ arrowLines ArrowDown (pprvMoveDownRect row)
  ++ [ fillRect expandColor (pprvExpandRect row)
     , strokeRect colPipelineExpandBorder (pprvExpandRect row)
     ]
  where
    pluginColor = if pprvDisabled row then colPipelineCheckDisabled else colPipelinePlugin
    pluginBorder = if pprvDisabled row then colPipelineCheckDisabledBorder else colPipelinePluginBorder
    expandColor = if pprvExpanded row then colPipelineExpandActive else colPipelineExpandInactive

paramDrawCommands :: PipelineParamControlView -> [DrawCommand]
paramDrawCommands control = case control of
  PipelineParamCheckView { ppcvRect = rect, ppcvChecked = checked } ->
    [ fillRect (if checked then colPipelineCheckEnabled else colPipelineCheckDisabled) rect
    , strokeRect (if checked then colPipelineCheckEnabledBorder else colPipelineCheckDisabledBorder) rect
    ]
  PipelineParamSliderView { ppcvRect = rect, ppcvValue = value } ->
    [ fillRect colSliderTrack rect
    , fillRect colPipelineParamBarFill (barFillRect value rect)
    ]

simulationDrawCommands :: PipelineSimulationControlsView -> [DrawCommand]
simulationDrawCommands simControls =
  [ fillRect tickColor (pscvTickButtonRect simControls)
  , fillRect autoTickColor (pscvAutoTickRect simControls)
  , strokeRect autoTickBorder (pscvAutoTickRect simControls)
  , fillRect colSliderTrack (pscvTickRateRect simControls)
  , fillRect colPipelineTickRateBarFill (barFillRect (pscvTickRateValue simControls) (pscvTickRateRect simControls))
  ]
  where
    tickColor = if pscvTickEnabled simControls then colPipelineTickEnabled else colPipelineTickDisabled
    autoTickColor = if pscvAutoTickEnabled simControls then colPipelineCheckEnabled else colPipelineCheckDisabled
    autoTickBorder = if pscvAutoTickEnabled simControls then colPipelineCheckEnabledBorder else colPipelineCheckDisabledBorder

-- | Text labels for the Pipeline tab, with positions already scrolled.
pipelineLabelViews :: UTCTime -> UiState -> Layout -> [PipelineLabelView]
pipelineLabelViews now ui layout =
  stageLabels ++ pluginLabels ++ simulationLabels
  where
    scrollY = pipelineScrollOffset ui layout
    sr = shiftRectY (negate scrollY)
    scrollArea = configScrollAreaRect layout
    Rect (V2 sx _sy, V2 sw _sh) = scrollArea
    pad = 12
    checkboxSize = 16
    labelMaxW = sw - (pad + checkboxSize + 8) - 8
    stageLabels =
      [ let Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect idx layout)
            isDisabled = Set.member sid (uiDisabledStages ui)
            textColor = if isDisabled then textPipelineStageDisabled else textPipelineStageName
        in PipelineLabelView
          { plvPosition = V2 (sx + pad + checkboxSize + 8) (ry + 4)
          , plvColor = textColor
          , plvText = stageCanonicalName sid
          , plvMaxWidth = Nothing
          }
      | (idx, sid) <- zip [0..] allBuiltinStageIds
      ]
    pluginLabels = concat
      [ pluginLabelRows sr layout sx sw pad checkboxSize labelMaxW now ui idx name
      | (idx, name) <- zip [0..] (uiPluginNames ui)
      ]
    simOffset = builtinStageRowCount + pluginRowsWithParams ui
    simWorldReady = maybe False (const True) (uiWorldConfig ui)
    simLabelColor = textPipelineSimLabel
    simAutoLabelX = sx + pad + checkboxSize + 8
    simRateLabelX = sx + pad + 128
    tickLabelColor = if simWorldReady then textPipelineTickActive else textPipelineTickInactive
    simulationLabels =
      [ let Rect (V2 _ tickRowY, V2 _ _tickRowH) = sr (configScrollRowRect simOffset layout)
        in PipelineLabelView
          { plvPosition = V2 (sx + pad + 8) (tickRowY + 4)
          , plvColor = tickLabelColor
          , plvText = "Tick"
          , plvMaxWidth = Nothing
          }
      , let Rect (V2 _ autoTickRowY, V2 _ _autoTickRowH) = sr (configScrollRowRect (simOffset + 1) layout)
        in PipelineLabelView
          { plvPosition = V2 simAutoLabelX (autoTickRowY + 4)
          , plvColor = simLabelColor
          , plvText = "Auto-tick"
          , plvMaxWidth = Nothing
          }
      , let Rect (V2 _ tickRateRowY, V2 _ _tickRateRowH) = sr (configScrollRowRect (simOffset + 2) layout)
            rateText = "Rate: " <> Text.pack (show (round (uiSimTickRate ui * 10) :: Int)) <> "/s"
        in PipelineLabelView
          { plvPosition = V2 simRateLabelX (tickRateRowY + 4)
          , plvColor = simLabelColor
          , plvText = rateText
          , plvMaxWidth = Nothing
          }
      ]

pluginLabelRows
  :: (Rect -> Rect)
  -> Layout
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> UTCTime
  -> UiState
  -> Int
  -> Text
  -> [PipelineLabelView]
pluginLabelRows sr layout sx sw pad checkboxSize labelMaxW now ui idx pluginName =
  pluginLabel : detailLabels ++ paramLabels
  where
    rowIndex = pluginRowIndex ui idx
    Rect (V2 _ ry, V2 _ _rowH) = sr (configScrollRowRect rowIndex layout)
    labelX = sx + pad + checkboxSize + 8
    lifecycle = Map.lookup pluginName (uiPluginLifecycles ui)
    isDisabled = Set.member pluginName (uiDisabledPlugins ui)
    diagnosticStatus = Map.lookup pluginName (uiPluginDiagnosticStatuses ui)
    lifecycleSuffix = " [" <> diagnosticStatusLabel diagnosticStatus isDisabled lifecycle <> uptimeSuffix now lifecycle <> "]"
    pluginLabel = PipelineLabelView
      { plvPosition = V2 labelX (ry + 4)
      , plvColor = textPipelinePluginName
      , plvText = pluginName <> lifecycleSuffix
      , plvMaxWidth = Just labelMaxW
      }
    expanded = Map.findWithDefault False pluginName (uiPluginExpanded ui)
    detailLines = if expanded then Map.findWithDefault [] pluginName (uiPluginDiagnosticLines ui) else []
    detailX = sx + pad + 24
    detailMaxW = sw - (pad + 24) - 8
    detailLabels =
      [ let Rect (V2 _ dy, V2 _ _detailRowH) = sr (configScrollRowRect (rowIndex + 1 + dIdx) layout)
        in PipelineLabelView
          { plvPosition = V2 detailX (dy + 4)
          , plvColor = textMuted
          , plvText = lineText
          , plvMaxWidth = Just detailMaxW
          }
      | (dIdx, lineText) <- zip [0..] detailLines
      ]
    specs = if expanded then Map.findWithDefault [] pluginName (uiPluginParamSpecs ui) else []
    paramStart = rowIndex + 1 + length detailLines
    paramLabelX = sx + pad + 40
    paramLabelMaxW = sw - (pad + 40) - 8
    paramLabels =
      [ let Rect (V2 _ py, V2 _ _paramRowH) = sr (configScrollRowRect (paramStart + pIdx) layout)
        in PipelineLabelView
          { plvPosition = V2 paramLabelX (py + 4)
          , plvColor = textPipelineStageName
          , plvText = rpsLabel spec
          , plvMaxWidth = Just paramLabelMaxW
          }
      | (pIdx, spec) <- zip [0..] specs
      ]

-- | Built-in stage widgets owned by the Pipeline tab.
pipelineStageWidgetRects :: Layout -> [(WidgetId, Rect)]
pipelineStageWidgetRects layout =
  [ (WidgetPipelineToggle (stageCanonicalName sid), pipelineCheckboxRect idx layout)
  | (idx, sid) <- zip [0..] allBuiltinStageIds
  ]

-- | Dynamic plugin, parameter, and simulation widgets owned by the Pipeline tab.
pipelinePluginWidgetRects
  :: [Text]
  -> Map.Map Text Bool
  -> Map.Map Text [RPCParamSpec]
  -> Map.Map Text [Text]
  -> Layout
  -> [(WidgetId, Rect)]
pipelinePluginWidgetRects pluginNames expanded paramSpecs diagnosticLines layout =
  pluginWidgets ++ simWidgets
  where
    (pluginWidgets, nextRow) = foldl buildOne ([], builtinStageRowCount) pluginNames
    buildOne (accWidgets, rowIdx) name =
      let moveWidgets =
            [ (WidgetPluginMoveUp name, pipelineMoveUpRect rowIdx layout)
            , (WidgetPluginMoveDown name, pipelineMoveDownRect rowIdx layout)
            , (WidgetPluginToggle name, pipelineCheckboxRect rowIdx layout)
            , (WidgetPluginExpand name, pipelineExpandRect rowIdx layout)
            ]
          isExpanded = Map.findWithDefault False name expanded
          specs = Map.findWithDefault [] name paramSpecs
          detailCount = if isExpanded then length (Map.findWithDefault [] name diagnosticLines) else 0
          paramWidgets
            | isExpanded =
                [ case rpsType spec of
                    ParamBool -> (WidgetPluginParamCheck name (rpsName spec), pipelineParamCheckRect paramRow layout)
                    _ -> (WidgetPluginParamSlider name (rpsName spec), pipelineParamBarRect paramRow layout)
                | (pIdx, spec) <- zip [0..] specs
                , let paramRow = rowIdx + 1 + detailCount + pIdx
                ]
            | otherwise = []
          paramCount = if isExpanded then length specs else 0
      in (accWidgets ++ moveWidgets ++ paramWidgets, rowIdx + 1 + detailCount + paramCount)
    simWidgets =
      [ (WidgetSimTick, pipelineTickButtonRect nextRow layout)
      , (WidgetSimAutoTick, pipelineCheckboxRect (nextRow + 1) layout)
      ]

-- | Convert an X-position in a parameter slider bar to the JSON value sent to
-- @set_plugin_param@.  A missing spec falls back to a normalized [0,1] value.
pipelineParamValueFromClick :: Layout -> V2 Int -> Maybe RPCParamSpec -> Value
pipelineParamValueFromClick layout (V2 clickX _) maybeSpec =
  case maybeSpec >>= rpsRange of
    Just (Number lo, Number hi)
      | hi > lo ->
          let loD = realToFrac lo :: Double
              hiD = realToFrac hi :: Double
              v = loD + realToFrac normalized * (hiD - loD)
          in Number (realToFrac (max loD (min hiD v)))
    _ -> Number (realToFrac normalized)
  where
    Rect (V2 barX _, V2 barW _) = pipelineParamBarRect 0 layout
    normalized = clamp01 (fromIntegral (clickX - barX) / max 1 (fromIntegral barW))

pipelineParamToggleValue :: UiState -> Text -> Text -> Value
pipelineParamToggleValue ui pluginName paramName =
  Bool (not current)
  where
    params = Map.findWithDefault Map.empty pluginName (uiPluginParams ui)
    current = pipelineParamBoolValue params paramName

normalizePipelineParam :: RPCParamSpec -> Double -> Float
normalizePipelineParam spec val =
  case rpsRange spec of
    Just (Number lo, Number hi)
      | hi > lo -> clamp01 $ realToFrac ((val - realToFrac lo) / (realToFrac hi - realToFrac lo))
    _ -> clamp01 (realToFrac val)

pipelineScrollOffset :: UiState -> Layout -> Int
pipelineScrollOffset ui layout =
  min maxOffset (uiConfigScroll ui)
  where
    rowHeight = 24
    gap = 10
    rows = configRowCount ConfigPipeline ui
    contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
    Rect (V2 _ _, V2 _ scrollH) = configScrollAreaRect layout
    maxOffset = max 0 (contentHeight - scrollH)

pipelineParamNormalizedValue :: RPCParamSpec -> Map.Map Text Value -> Float
pipelineParamNormalizedValue spec params =
  case Map.lookup (rpsName spec) params of
    Just (Number n) -> normalizePipelineParam spec (realToFrac n)
    _ -> 0.5

pipelineParamBoolValue :: Map.Map Text Value -> Text -> Bool
pipelineParamBoolValue params paramName =
  case Map.lookup paramName params of
    Just (Bool b) -> b
    _ -> False

barFillRect :: Float -> Rect -> Rect
barFillRect value (Rect (V2 x y, V2 w h)) =
  Rect (V2 x y, V2 fillW h)
  where
    fillW = max 0 (min w (round (fromIntegral w * clamp01 value)))

data ArrowDirection = ArrowUp | ArrowDown

arrowLines :: ArrowDirection -> Rect -> [DrawCommand]
arrowLines direction (Rect (V2 x y, V2 w h)) =
  [ line colPipelineArrow (V2 (midX - halfW row) row) (V2 (midX + halfW row) row)
  | row <- [top .. bot]
  ]
  where
    midX = x + w `div` 2
    top = y + 3
    bot = y + h - 3
    halfW row = case direction of
      ArrowUp -> (row - top) * (w `div` 2 - 2) `div` max 1 (bot - top)
      ArrowDown -> (bot - row) * (w `div` 2 - 2) `div` max 1 (bot - top)

shiftRectY :: Int -> Rect -> Rect
shiftRectY dy (Rect (V2 x y, V2 w h)) = Rect (V2 x (y + dy), V2 w h)

clamp01 :: Float -> Float
clamp01 = max 0.0 . min 1.0

diagnosticStatusLabel :: Maybe Text -> Bool -> Maybe PluginLifecycleSnapshot -> Text
diagnosticStatusLabel (Just status) _ _ = status
diagnosticStatusLabel Nothing True _ = "Disabled"
diagnosticStatusLabel Nothing False Nothing = "WaitingForDependencies"
diagnosticStatusLabel Nothing False (Just snapshot) = case plsState snapshot of
  LifecycleReady -> "Ready"
  LifecycleDegraded -> "Degraded"
  LifecycleFailed -> "Failed"
  _ -> "WaitingForDependencies:" <> pluginLifecycleStateText (plsState snapshot)

uptimeSuffix :: UTCTime -> Maybe PluginLifecycleSnapshot -> Text
uptimeSuffix now (Just snapshot)
  | plsState snapshot == LifecycleReady = " uptime=" <> formatUptime (diffUTCTime now (plsUpdatedAt snapshot))
uptimeSuffix _ _ = ""

formatUptime :: RealFrac a => a -> Text
formatUptime seconds = Text.pack (show (max (0 :: Int) (floor seconds :: Int))) <> "s"

