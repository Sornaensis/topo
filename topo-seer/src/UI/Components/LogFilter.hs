{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure component model for the log-level filter strip.
module UI.Components.LogFilter
  ( LogFilterModel(..)
  , LogFilterAction(..)
  , LogFilterEffect(..)
  , logFilterComponent
  , logFilterWidgets
  , logFilterRouteEvent
  , logFilterReducer
  , logFilterDrawCommands
  , logFilterButtonDrawCommands
  , logFilterLevelForWidget
  ) where

import Actor.Log (LogLevel(..))
import Data.Text (Text)
import Data.Word (Word8)
import Linear (V4(..))
import UI.Component
  ( Component(..)
  , ReducerResult(..)
  , RoutedUiEvent(..)
  , UiEvent(..)
  , UiPointerButton(..)
  , componentLogPanel
  )
import UI.DrawCommand (DrawCommand, fillRect, textCentered)
import UI.Theme
  ( colLogDebugFilter
  , colLogErrorFilter
  , colLogInfoFilter
  , colLogWarnFilter
  , textLogFilterLabel
  )
import UI.WidgetId (WidgetId(..))
import UI.WidgetTree (Widget(..))
import UI.Widgets (Rect)

newtype LogFilterModel = LogFilterModel
  { logFilterMinLevel :: LogLevel
  } deriving (Eq, Show)

newtype LogFilterAction = LogFilterSelect LogLevel
  deriving (Eq, Show)

newtype LogFilterEffect = LogFilterSetMinLevel LogLevel
  deriving (Eq, Show)

logFilterComponent :: (Rect, Rect, Rect, Rect) -> Component LogFilterModel LogFilterAction LogFilterEffect
logFilterComponent rects = Component
  { componentId = componentLogPanel
  , componentWidgets = const (logFilterWidgets rects)
  , componentHandleEvent = const logFilterRouteEvent
  , componentReduce = logFilterReducer
  , componentDrawCommands = \model -> logFilterDrawCommands model rects
  }

logFilterWidgets :: (Rect, Rect, Rect, Rect) -> [Widget]
logFilterWidgets (debugRect, infoRect, warnRect, errorRect) =
  [ Widget WidgetLogDebug debugRect
  , Widget WidgetLogInfo infoRect
  , Widget WidgetLogWarn warnRect
  , Widget WidgetLogError errorRect
  ]

logFilterRouteEvent :: RoutedUiEvent -> Maybe LogFilterAction
logFilterRouteEvent routed =
  case (routedComponent routed, routedWidget routed, routedEvent routed) of
    (Just cid, Just wid, UiPointerDown _ UiPointerPrimary)
      | cid == componentLogPanel -> LogFilterSelect <$> logFilterLevelForWidget wid
    _ -> Nothing

logFilterReducer :: LogFilterModel -> LogFilterAction -> ReducerResult LogFilterModel LogFilterEffect
logFilterReducer _model (LogFilterSelect level) =
  ReducerResult (LogFilterModel level) [LogFilterSetMinLevel level]

logFilterDrawCommands :: LogFilterModel -> (Rect, Rect, Rect, Rect) -> [DrawCommand]
logFilterDrawCommands (LogFilterModel activeLevel) (debugRect, infoRect, warnRect, errorRect) =
  concat
    [ logFilterButtonDrawCommands activeLevel LogDebug debugRect "D"
    , logFilterButtonDrawCommands activeLevel LogInfo infoRect "I"
    , logFilterButtonDrawCommands activeLevel LogWarn warnRect "W"
    , logFilterButtonDrawCommands activeLevel LogError errorRect "E"
    ]

logFilterButtonDrawCommands :: LogLevel -> LogLevel -> Rect -> Text -> [DrawCommand]
logFilterButtonDrawCommands activeLevel level rect label =
  [ fillRect (logFilterLevelColor level (level == activeLevel)) rect
  , textCentered textLogFilterLabel rect label
  ]

logFilterLevelForWidget :: WidgetId -> Maybe LogLevel
logFilterLevelForWidget wid = case wid of
  WidgetLogDebug -> Just LogDebug
  WidgetLogInfo -> Just LogInfo
  WidgetLogWarn -> Just LogWarn
  WidgetLogError -> Just LogError
  _ -> Nothing

logFilterLevelColor :: LogLevel -> Bool -> V4 Word8
logFilterLevelColor level isActive =
  let boost = if isActive then 60 else 0 :: Int
      boostW8 :: Word8 -> Word8
      boostW8 x = fromIntegral (min 255 (fromIntegral x + boost))
      applyBoost (V4 r g b a) = V4 (boostW8 r) (boostW8 g) (boostW8 b) a
  in case level of
       LogDebug -> applyBoost colLogDebugFilter
       LogInfo -> applyBoost colLogInfoFilter
       LogWarn -> applyBoost colLogWarnFilter
       LogError -> applyBoost colLogErrorFilter
