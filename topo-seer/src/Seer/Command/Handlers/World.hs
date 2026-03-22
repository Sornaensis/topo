{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for world/meta queries and mutations:
-- @get_world_meta@, @get_generation_status@, @get_overlays@, @list_worlds@,
-- @save_world@, @load_world@.
module Seer.Command.Handlers.World
  ( handleGetWorldMeta
  , handleGetGenerationStatus
  , handleGetOverlays
  , handleListWorlds
  , handleSaveWorld
  , handleLoadWorld
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot, getDataSnapshot, replaceTerrainData)
import Actor.PluginManager (getPluginDataDirectories, notifyWorldChanged)
import Actor.Simulation (setSimWorld)
import Actor.SnapshotReceiver
  ( readTerrainSnapshot
  , readDataSnapshot
  , writeDataSnapshot
  , writeTerrainSnapshot
  , bumpSnapshotVersion
  )
import Actor.Terrain (TerrainReplyOps)
import Actor.UI.Setters (setUiWorldName, setUiWorldConfig, setUiOverlayNames)
import Actor.UiActions
  ( UiAction(..)
  , UiActionRequest(..)
  , submitUiAction
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.Config.Snapshot (snapshotFromUi, applySnapshotToUi)
import Seer.World.Persist
  ( listWorlds
  , saveNamedWorldWithPlugins
  , loadNamedWorld
  , snapshotToWorld
  , worldDir
  )
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Overlay (overlayNames)
import Topo.World (TerrainWorld(..))

-- | Handle @get_world_meta@ — return world metadata: seed, chunk size,
-- chunk count, tiles per chunk, overlay names, etc.
handleGetWorldMeta :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetWorldMeta ctx reqId _params = do
  ui   <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let chunkSize  = tsChunkSize snap
      tileCount  = chunkSize * chunkSize
      chunkCount = IntMap.size (tsTerrainChunks snap)
      chunkIds   = IntMap.keys (tsTerrainChunks snap)
      ovNames    = overlayNames (tsOverlayStore snap)
  pure $ okResponse reqId $ object
    [ "seed"            .= uiSeed ui
    , "chunk_size"      .= chunkSize
    , "tiles_per_chunk" .= tileCount
    , "chunk_count"     .= chunkCount
    , "total_tiles"     .= (chunkCount * tileCount)
    , "chunk_ids"       .= chunkIds
    , "overlay_names"   .= ovNames
    , "world_name"      .= uiWorldName ui
    , "generating"      .= uiGenerating ui
    ]

-- | Handle @get_generation_status@ — return whether generation is
-- currently in progress and how many chunks exist.
handleGetGenerationStatus :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetGenerationStatus ctx reqId _params = do
  ui   <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  pure $ okResponse reqId $ object
    [ "generating"   .= uiGenerating ui
    , "chunk_count"  .= IntMap.size (tsTerrainChunks snap)
    , "seed"         .= uiSeed ui
    ]

-- | Handle @get_overlays@ — return the names and count of loaded overlays.
handleGetOverlays :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetOverlays ctx reqId _params = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let names = overlayNames (tsOverlayStore snap)
  pure $ okResponse reqId $ object
    [ "overlay_count" .= length names
    , "overlay_names" .= names
    ]

-- | Handle @list_worlds@ — return saved worlds from @~\/.topo\/worlds\/@.
handleListWorlds :: CommandContext -> Int -> Value -> IO SeerResponse
handleListWorlds _ctx reqId _params = do
  worlds <- listWorlds
  let worldEntries = map manifestToJSON worlds
  pure $ okResponse reqId $ object
    [ "world_count" .= length worlds
    , "worlds"      .= worldEntries
    ]

-- | Handle @save_world@ — save current terrain and config as a named world.
-- Params: @{ "name": "my-world" }@
handleSaveWorld :: CommandContext -> Int -> Value -> IO SeerResponse
handleSaveWorld ctx reqId params = do
  case Aeson.parseMaybe parseName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name
      | Text.null name ->
          pure $ errResponse reqId "world name must not be empty"
      | otherwise -> do
          let handles = ccActorHandles ctx
          ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
          let world = snapshotToWorld terrainSnap
          pluginDirs <- getPluginDataDirectories (ahPluginManagerHandle handles)
          result <- saveNamedWorldWithPlugins name ui world pluginDirs
          case result of
            Right () -> do
              -- Update UI with saved world name + config snapshot
              wDir <- worldDir
              notifyWorldChanged (ahPluginManagerHandle handles)
                                 (Just (Text.pack (wDir </> Text.unpack name)))
              setUiWorldName (ahUiHandle handles) name
              setUiWorldConfig (ahUiHandle handles) (Just (snapshotFromUi ui name))
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "saved" .= True
                ]
            Left err ->
              pure $ errResponse reqId ("failed to save world: " <> err)

-- | Handle @load_world@ — load a named world from @~\/.topo\/worlds\/@.
-- Params: @{ "name": "my-world" }@
--
-- This loads the world data, replaces the current terrain, applies the
-- config snapshot to the UI, and triggers an atlas rebuild.
handleLoadWorld :: CommandContext -> Int -> Value -> IO SeerResponse
handleLoadWorld ctx reqId params = do
  case Aeson.parseMaybe parseName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name
      | Text.null name ->
          pure $ errResponse reqId "world name must not be empty"
      | otherwise -> do
          result <- loadNamedWorld name
          case result of
            Left err ->
              pure $ errResponse reqId ("failed to load world: " <> err)
            Right (_manifest, snapshot, world) -> do
              let handles = ccActorHandles ctx
                  uiH = ahUiHandle handles
              -- Replace terrain data
              replaceTerrainData (ahDataHandle handles) world
              setSimWorld (ahSimulationHandle handles) world
              setUiOverlayNames uiH (overlayNames (twOverlays world))
              -- Update snapshot refs for readers
              dataSnap <- getDataSnapshot (ahDataHandle handles)
              terrainSnap' <- getTerrainSnapshot (ahDataHandle handles)
              writeDataSnapshot (ahDataSnapshotRef handles) dataSnap
              writeTerrainSnapshot (ahTerrainSnapshotRef handles) terrainSnap'
              bumpSnapshotVersion (ahSnapshotVersionRef handles)
              -- Apply config snapshot
              applySnapshotToUi snapshot uiH
              setUiWorldName uiH name
              setUiWorldConfig uiH (Just snapshot)
              -- Notify plugins
              wDir <- worldDir
              notifyWorldChanged (ahPluginManagerHandle handles)
                                 (Just (Text.pack (wDir </> Text.unpack name)))
              -- Trigger atlas rebuild
              ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
              let req = UiActionRequest
                    { uarAction = UiActionRebuildAtlas (uiViewMode ui)
                    , uarActorHandles = handles
                    , uarTerrainReplyTo = replyTo @TerrainReplyOps (ccUiActionsHandle ctx)
                    }
              submitUiAction (ccUiActionsHandle ctx) req
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "loaded" .= True
                ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

manifestToJSON :: WorldSaveManifest -> Value
manifestToJSON m = object
  [ "name"          .= wsmName m
  , "seed"          .= wsmSeed m
  , "chunk_size"    .= wsmChunkSize m
  , "chunk_count"   .= wsmChunkCount m
  , "overlay_names" .= wsmOverlayNames m
  , "created_at"    .= show (wsmCreatedAt m)
  ]

parseName :: Value -> Aeson.Parser Text
parseName = Aeson.withObject "params" (.: "name")
