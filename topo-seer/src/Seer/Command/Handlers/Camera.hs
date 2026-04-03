{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for camera controls: @set_camera@, @get_camera@,
-- @zoom_to_chunk@.
--
-- Camera state lives in 'UiState' as @uiPanOffset :: (Float, Float)@
-- and @uiZoom :: Float@.  The screen transform is
-- @screen = (world + panOffset) * zoom@.
module Seer.Command.Handlers.Camera
  ( handleSetCamera
  , handleGetCamera
  , handleZoomToChunk
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import qualified Data.IntMap.Strict as IntMap

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.UI.Setters (setUiPanOffset, setUiZoom)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), chunkCoordFromId)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import UI.HexPick (axialToScreen, renderHexRadiusPx)
import Seer.Render.ZoomStage (maxCameraZoom)

-- | Zoom range clamping bounds, matching 'Seer.Input.ViewControls'.
zoomMin, zoomMax :: Float
zoomMin = 0.4
zoomMax = maxCameraZoom

-- | Clamp zoom to the allowed range.
clampZoom :: Float -> Float
clampZoom = max zoomMin . min zoomMax

-- | Handle @set_camera@ — set pan offset and optionally zoom.
--
-- Params: @{ "x": float, "y": float, "zoom"?: float }@
handleSetCamera :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetCamera ctx reqId params = do
  case Aeson.parseMaybe parseSetCamera params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'x' and 'y' parameters"
    Just (x, y, mZoom) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      setUiPanOffset uiH (x, y)
      case mZoom of
        Just z -> setUiZoom uiH (clampZoom z)
        Nothing -> pure ()
      pure $ okResponse reqId $ object
        [ "x" .= x
        , "y" .= y
        , "zoom" .= maybe Nothing (Just . clampZoom) mZoom
        ]

-- | Handle @get_camera@ — return current pan offset and zoom.
handleGetCamera :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetCamera ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let (x, y) = uiPanOffset ui
  pure $ okResponse reqId $ object
    [ "x" .= x
    , "y" .= y
    , "zoom" .= uiZoom ui
    ]

-- | Handle @zoom_to_chunk@ — center the camera on a chunk.
--
-- Computes the chunk's center tile in world coordinates via
-- 'axialToScreen' and sets the pan offset to @-center@, scrolling
-- the view so the chunk appears near the top-left of the screen.
-- Zoom is set to 1.0 for a clear overview.
--
-- Params: @{ "chunk": int }@
handleZoomToChunk :: CommandContext -> Int -> Value -> IO SeerResponse
handleZoomToChunk ctx reqId params = do
  case Aeson.parseMaybe parseChunkId params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'chunk' parameter"
    Just chunkId -> do
      let handles = ccActorHandles ctx
      snap <- getTerrainSnapshot (ahDataHandle handles)
      let chunkSize = tsChunkSize snap
      if chunkSize <= 0
        then pure $ errResponse reqId "no terrain generated"
        else if not (IntMap.member chunkId (tsTerrainChunks snap))
          then pure $ errResponse reqId "chunk not found"
          else do
            let ChunkCoord cx cy = chunkCoordFromId (ChunkId chunkId)
                -- Center tile of the chunk in axial (q, r) coordinates
                centerQ = cx * chunkSize + chunkSize `div` 2
                centerR = cy * chunkSize + chunkSize `div` 2
                (worldX, worldY) = axialToScreen renderHexRadiusPx centerQ centerR
                -- Set pan so chunk center is at screen origin
                newZoom = 1.0 :: Float
                panX = negate (fromIntegral worldX)
                panY = negate (fromIntegral worldY)
                uiH = ahUiHandle handles
            setUiPanOffset uiH (panX, panY)
            setUiZoom uiH newZoom
            pure $ okResponse reqId $ object
              [ "chunk"  .= chunkId
              , "x"      .= panX
              , "y"      .= panY
              , "zoom"   .= newZoom
              ]

-- --------------------------------------------------------------------------
-- Parsing helpers
-- --------------------------------------------------------------------------

parseSetCamera :: Value -> Aeson.Parser (Float, Float, Maybe Float)
parseSetCamera = Aeson.withObject "set_camera" $ \o ->
  (,,) <$> o .: "x" <*> o .: "y" <*> o .:? "zoom"

parseChunkId :: Value -> Aeson.Parser Int
parseChunkId = Aeson.withObject "zoom_to_chunk" $ \o ->
  o .: "chunk"
