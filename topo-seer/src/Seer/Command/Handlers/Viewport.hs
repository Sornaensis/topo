{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Command handlers for viewport interaction: @viewport_scroll@,
-- @viewport_click@, @viewport_drag@, @viewport_hover@.
--
-- These simulate the same mouse interactions that a user performs on
-- the terrain canvas.  Screen-to-world conversion and hex resolution use
-- the shared hex picking transform.
module Seer.Command.Handlers.Viewport
  ( handleViewportScroll
  , handleViewportClick
  , handleViewportDrag
  , handleViewportHover
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import Actor.Data (getTerrainSnapshot)
import Actor.Terrain (TerrainReplyOps)
import Actor.UI.Setters (setUiPanOffset, setUiZoom, setUiHoverHex, setUiContextHex, setUiContextPos, setUiHexTooltipPinned)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions (UiAction(..), UiActionRequest(..), submitUiActionSync)
import Actor.UiActions.Command (enqueueViewportRefreshForCurrentUi)
import Actor.UiActions.Handles (ActorHandles(..), publishUiMutation)
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.Editor.Types (EditorState(..))
import Seer.Input.ViewControls
  ( applyZoomAtCursor
  , defaultZoomSettings
  , isViewportDrag
  , panViewportForDrag
  , pickTerrainHex
  , zoomStepCount
  )
import Seer.Command.Types (SeerResponse, okResponse, errResponse)

-- --------------------------------------------------------------------------
-- Handlers
-- --------------------------------------------------------------------------

-- | Handle @viewport_scroll@ — simulate mouse-wheel zoom at a position.
--
-- Params: @{ "delta": int, "x"?: int, "y"?: int }@
--
-- @delta@ > 0 zooms in, < 0 zooms out.  If @x@/@y@ are omitted the
-- zoom is centered at (0, 0) in screen space (top-left), so callers
-- should provide coordinates for cursor-centered zoom.
handleViewportScroll :: CommandContext -> Int -> Value -> IO SeerResponse
handleViewportScroll ctx reqId params = do
  case Aeson.parseMaybe parseScroll params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'delta' parameter"
    Just (delta, mx, my) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let steps = zoomStepCount delta
          (newZoom, newOffset) = applyZoomAtCursor defaultZoomSettings ui (mx, my) delta
      when (delta /= 0) $ do
        setUiZoom uiH newZoom
        setUiPanOffset uiH newOffset
        _ <- enqueueViewportRefreshForCurrentUi handles
        pure ()
      let (nx, ny) = newOffset
      pure $ okResponse reqId $ object
        [ "zoom"   .= newZoom
        , "pan_x"  .= nx
        , "pan_y"  .= ny
        , "steps"  .= steps
        ]

-- | Handle @viewport_click@ — simulate a mouse click at pixel coordinates.
--
-- Params: @{ "x": int, "y": int, "button"?: "left"|"right" }@
--
-- Left click: resolves screen → hex and selects it (pins tooltip).
-- If the editor is active, a left click on a terrain hex also
-- produces a brush stroke.
-- Right click: toggles tooltip pin state.
handleViewportClick :: CommandContext -> Int -> Value -> IO SeerResponse
handleViewportClick ctx reqId params = do
  case Aeson.parseMaybe parseClick params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'x' and 'y' parameters"
    Just (px, py, button) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
      let ((q, r), hexExists) = pickTerrainHex terrainSnap ui (px, py)
      case button of
        "right" -> do
          setUiHexTooltipPinned uiH (not (uiHexTooltipPinned ui))
          pure $ okResponse reqId $ object
            [ "button"  .= ("right" :: String)
            , "tooltip_pinned" .= not (uiHexTooltipPinned ui)
            ]
        _ -> do  -- "left" or default
          if hexExists
            then do
              setUiHoverHex uiH (Just (q, r))
              setUiContextHex uiH (Just (q, r))
              setUiHexTooltipPinned uiH True
              -- viewport_click intentionally selects/pins in addition to
              -- matching SDL's editor-only terrain paint policy.
              let editor = uiEditor ui
              when (editorActive editor) $
                submitDiscreteEditorStroke ctx (q, r)
              pure $ okResponse reqId $ object
                [ "button"  .= ("left" :: String)
                , "hex_q"   .= q
                , "hex_r"   .= r
                , "selected" .= True
                , "editor_stroke" .= editorActive editor
                ]
            else do
              setUiHoverHex uiH Nothing
              setUiContextHex uiH Nothing
              setUiContextPos uiH Nothing
              setUiHexTooltipPinned uiH False
              pure $ okResponse reqId $ object
                [ "button"  .= ("left" :: String)
                , "hex_q"   .= q
                , "hex_r"   .= r
                , "selected" .= False
                , "reason"  .= ("no terrain at coordinates" :: String)
                ]

-- | Handle @viewport_drag@ — simulate a right-button drag (camera pan).
--
-- Params: @{ "x1": int, "y1": int, "x2": int, "y2": int }@
--
-- Movements beyond the shared SDL threshold pan by the screen-space delta
-- divided by zoom. Sub-threshold gestures behave as a right click and toggle
-- tooltip pinning without a viewport refresh.
handleViewportDrag :: CommandContext -> Int -> Value -> IO SeerResponse
handleViewportDrag ctx reqId params = do
  case Aeson.parseMaybe parseDrag params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid drag parameters (need 'x1','y1','x2','y2')"
    Just (x1, y1, x2, y2) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let start = (x1, y1)
          end = (x2, y2)
          dragged = isViewportDrag start end
          newOffset = if dragged
            then panViewportForDrag ui start end
            else uiPanOffset ui
      if dragged
        then do
          setUiPanOffset uiH newOffset
          _ <- enqueueViewportRefreshForCurrentUi handles
          pure ()
        else setUiHexTooltipPinned uiH (not (uiHexTooltipPinned ui))
      let (nx, ny) = newOffset
      pure $ okResponse reqId $ object
        [ "pan_x"  .= nx
        , "pan_y"  .= ny
        , "dx"     .= (toInteger x2 - toInteger x1)
        , "dy"     .= (toInteger y2 - toInteger y1)
        ]

-- | Handle @viewport_hover@ — simulate mouse hover at pixel coordinates.
--
-- Params: @{ "x": int, "y": int }@
--
-- Converts screen → world → hex and sets 'uiHoverHex'.  Returns the
-- hex coordinates and whether terrain exists there.
handleViewportHover :: CommandContext -> Int -> Value -> IO SeerResponse
handleViewportHover ctx reqId params = do
  case Aeson.parseMaybe parseHover params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'x' and 'y' parameters"
    Just (px, py) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
      let ((q, r), hexExists) = pickTerrainHex terrainSnap ui (px, py)
      if hexExists
        then do
          setUiHoverHex uiH (Just (q, r))
          _ <- publishUiMutation handles
          pure $ okResponse reqId $ object
            [ "hex_q"  .= q
            , "hex_r"  .= r
            , "valid"  .= True
            ]
        else do
          setUiHoverHex uiH Nothing
          _ <- publishUiMutation handles
          pure $ okResponse reqId $ object
            [ "hex_q"  .= q
            , "hex_r"  .= r
            , "valid"  .= False
            ]

submitDiscreteEditorStroke :: CommandContext -> (Int, Int) -> IO ()
submitDiscreteEditorStroke ctx hex =
  mapM_ submit [UiActionClearFlattenRef, UiActionBrushStroke hex, UiActionClearFlattenRef]
  where
    handles = ccActorHandles ctx
    submit action = submitUiActionSync (ccUiActionsHandle ctx) UiActionRequest
      { uarAction = action
      , uarActorHandles = handles
      , uarTerrainReplyTo = replyTo @TerrainReplyOps (ccUiActionsHandle ctx)
      }

-- --------------------------------------------------------------------------
-- Parsing helpers
-- --------------------------------------------------------------------------

parseScroll :: Value -> Aeson.Parser (Int, Int, Int)
parseScroll = Aeson.withObject "viewport_scroll" $ \o ->
  (,,) <$> o .: "delta" <*> (o .:? "x" >>= pure . maybe 0 id) <*> (o .:? "y" >>= pure . maybe 0 id)

parseClick :: Value -> Aeson.Parser (Int, Int, String)
parseClick = Aeson.withObject "viewport_click" $ \o ->
  (,,) <$> o .: "x" <*> o .: "y" <*> (o .:? "button" >>= pure . maybe "left" id)

parseDrag :: Value -> Aeson.Parser (Int, Int, Int, Int)
parseDrag = Aeson.withObject "viewport_drag" $ \o ->
  (,,,) <$> o .: "x1" <*> o .: "y1" <*> o .: "x2" <*> o .: "y2"

parseHover :: Value -> Aeson.Parser (Int, Int)
parseHover = Aeson.withObject "viewport_hover" $ \o ->
  (,) <$> o .: "x" <*> o .: "y"

-- --------------------------------------------------------------------------
-- Utilities
-- --------------------------------------------------------------------------

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = pure ()
