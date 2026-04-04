{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | IPC handlers for viewport interaction: @viewport_scroll@,
-- @viewport_click@, @viewport_drag@, @viewport_hover@.
--
-- These simulate the same mouse interactions that a user performs on
-- the terrain canvas.  Screen-to-world conversion uses the camera
-- transform @world = screen / zoom - panOffset@, followed by
-- 'screenToAxial' for hex resolution.
module Seer.Command.Handlers.Viewport
  ( handleViewportScroll
  , handleViewportClick
  , handleViewportDrag
  , handleViewportHover
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import qualified Data.IntMap.Strict as IntMap

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.Terrain (TerrainReplyOps)
import Actor.UI.Setters (setUiPanOffset, setUiZoom, setUiHoverHex, setUiContextHex, setUiHexTooltipPinned)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions (UiAction(..), UiActionRequest(..), submitUiAction)
import Actor.UiActions.Handles (ActorHandles(..))
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.Editor.Types (EditorState(..))
import Seer.Render.ZoomStage (maxCameraZoom)
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import UI.HexPick (screenToAxial, renderHexRadiusPx)

-- --------------------------------------------------------------------------
-- Constants (matching Seer.Input.ViewControls)
-- --------------------------------------------------------------------------

zoomMin, zoomMax :: Float
zoomMin = 0.4
zoomMax = maxCameraZoom

zoomInFactor, zoomOutFactor :: Float
zoomInFactor  = 1.1
zoomOutFactor = 0.9

clampZoom :: Float -> Float
clampZoom = max zoomMin . min zoomMax

-- --------------------------------------------------------------------------
-- Coordinate helpers
-- --------------------------------------------------------------------------

-- | Convert screen pixel coordinates to world coordinates using camera state.
--
-- @world = screen / zoom - panOffset@
screenToWorld :: UiState -> (Float, Float) -> (Float, Float)
screenToWorld ui (sx, sy) =
  let (ox, oy) = uiPanOffset ui
      z = uiZoom ui
  in (sx / z - ox, sy / z - oy)

-- | Check whether a hex @(q, r)@ exists in the current terrain.
isTerrainHex :: TerrainSnapshot -> (Int, Int) -> Bool
isTerrainHex snap (q, r) =
  let size = tsChunkSize snap
  in size > 0
    && let cfg = WorldConfig { wcChunkSize = size }
           (chunkCoord, _) = chunkCoordFromTile cfg (TileCoord q r)
           ChunkId key = chunkIdFromCoord chunkCoord
       in IntMap.member key (tsTerrainChunks snap)

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
      let oldZoom = uiZoom ui
          factor
            | delta > 0 = zoomInFactor
            | delta < 0 = zoomOutFactor
            | otherwise = 1
          -- Apply multiple steps for |delta| > 1
          steps = abs delta
          newZoom = clampZoom (oldZoom * factor ^ steps)
          (ox, oy) = uiPanOffset ui
          fx = fromIntegral mx :: Float
          fy = fromIntegral my :: Float
          newOffset =
            ( ox + fx * (1 / newZoom - 1 / oldZoom)
            , oy + fy * (1 / newZoom - 1 / oldZoom)
            )
      setUiZoom uiH newZoom
      setUiPanOffset uiH newOffset
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
      let (wx, wy) = screenToWorld ui (fromIntegral px, fromIntegral py)
          (q, r) = screenToAxial renderHexRadiusPx (round wx) (round wy)
          hexExists = isTerrainHex terrainSnap (q, r)
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
              -- Editor brush stroke if editor active
              let editor = uiEditor ui
              when (editorActive editor) $ do
                let request = UiActionRequest
                      { uarAction         = UiActionBrushStroke (q, r)
                      , uarActorHandles   = handles
                      , uarTerrainReplyTo = replyTo @TerrainReplyOps (ccUiActionsHandle ctx)
                      }
                submitUiAction (ccUiActionsHandle ctx) request
              pure $ okResponse reqId $ object
                [ "button"  .= ("left" :: String)
                , "hex_q"   .= q
                , "hex_r"   .= r
                , "selected" .= True
                , "editor_stroke" .= editorActive editor
                ]
            else do
              setUiHoverHex uiH Nothing
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
-- Pans the camera by the screen-space delta divided by zoom, matching
-- the right-button drag behaviour in @Seer.Input.Events@.
handleViewportDrag :: CommandContext -> Int -> Value -> IO SeerResponse
handleViewportDrag ctx reqId params = do
  case Aeson.parseMaybe parseDrag params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid drag parameters (need 'x1','y1','x2','y2')"
    Just (x1, y1, x2, y2) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let zoom = uiZoom ui
          (ox, oy) = uiPanOffset ui
          dx = fromIntegral (x2 - x1) :: Float
          dy = fromIntegral (y2 - y1) :: Float
          newOffset = (ox + dx / zoom, oy + dy / zoom)
      setUiPanOffset uiH newOffset
      let (nx, ny) = newOffset
      pure $ okResponse reqId $ object
        [ "pan_x"  .= nx
        , "pan_y"  .= ny
        , "dx"     .= (x2 - x1)
        , "dy"     .= (y2 - y1)
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
      let (wx, wy) = screenToWorld ui (fromIntegral px, fromIntegral py)
          (q, r) = screenToAxial renderHexRadiusPx (round wx) (round wy)
          hexExists = isTerrainHex terrainSnap (q, r)
      if hexExists
        then do
          setUiHoverHex uiH (Just (q, r))
          pure $ okResponse reqId $ object
            [ "hex_q"  .= q
            , "hex_r"  .= r
            , "valid"  .= True
            ]
        else do
          setUiHoverHex uiH Nothing
          pure $ okResponse reqId $ object
            [ "hex_q"  .= q
            , "hex_r"  .= r
            , "valid"  .= False
            ]

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
