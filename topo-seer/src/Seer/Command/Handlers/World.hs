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
  , handleGetOverlaySchema
  , handleGetOverlayProvenance
  , handleExportOverlayData
  , handleValidateOverlayImport
  , handleListWorlds
  , handleSaveWorld
  , handleLoadWorld
  , handleSetWorldName
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value(..), object, (.=), (.:), (.:?), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.FilePath ((</>))

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot, getDataSnapshot, replaceTerrainData)
import Actor.Log (getLogSnapshot)
import Actor.PluginManager
  ( PluginSimulationPlan(..)
  , getPluginDataDirectories
  , getPluginExternalDataSources
  , getPluginSimulationPlan
  , notifyWorldChanged
  )
import Actor.Simulation (beginSimWorldTransition, cancelSimWorldTransition, clearSimWorld, normalizeWorldSchedulesForBindings, setSimWorldWithNodesSync)
import Actor.SnapshotReceiver
  ( dataAndTerrainSnapshotUpdate
  , publishSnapshot
  , readDataSnapshot
  , readTerrainSnapshot
  , withLogSnapshot
  , withUiSnapshot
  )
import Actor.UI.Setters (setUiWorldName, setUiWorldConfig, setUiOverlayNames, setUiSimTickCount, setUiViewSelection)
import Actor.UiActions (enqueueAtlasRebuildForTerrain)
import Actor.UiActions.Handles (ActorHandles(..), publishUiMutation)
import Actor.UI.State (UiState(..), ViewMode(..), defaultLayeredViewState, getUiSnapshot, readUiSnapshotRef)
import Seer.Command.Context (CommandContext(..))
import Seer.Config.Snapshot (snapshotFromUi, applySnapshotToUi)
import Seer.World.Persist
  ( listWorlds
  , saveNamedWorldWithPluginsAndExternalData
  , loadNamedWorld
  , snapshotToWorld
  , worldDir
  )
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Calendar (WorldTime(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayProvenance(..)
  , OverlayStore(..)
  , chunkSize
  , lookupOverlay
  , overlayNames
  )
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , parseOverlaySchema
  )
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

-- | Handle @get_overlays@ — return overlay-manager metadata for loaded overlays.
handleGetOverlays :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetOverlays ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let store = tsOverlayStore snap
      names = overlayNames store
      active = case uiViewMode ui of
        ViewOverlay name fieldIdx -> Just (name, fieldIdx)
        _ -> Nothing
  pure $ okResponse reqId $ object
    [ "overlay_count" .= length names
    , "overlay_names" .= names
    , "active_overlay" .= fmap fst active
    , "active_field_index" .= fmap snd active
    , "overlays" .= overlayStoreSummaries store active
    , "diagnostics" .= [diagnostic "info" "overlay_manager_ready" "overlay manager metadata is available"]
    ]

-- | Handle @get_overlay_schema@ — return one overlay's schema as JSON.
handleGetOverlaySchema :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetOverlaySchema ctx reqId params = do
  case Aeson.parseMaybe parseOverlayNameParam params of
    Nothing -> pure $ errResponse reqId "missing or invalid 'overlay' parameter"
    Just name -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      case lookupOverlay name (tsOverlayStore snap) of
        Nothing -> pure $ errResponse reqId (overlayNotFoundMessage name (tsOverlayStore snap))
        Just ov -> pure $ okResponse reqId $ object
          [ "overlay" .= name
          , "format" .= ("toposchema" :: Text)
          , "schema" .= toJSON (ovSchema ov)
          , "fields" .= map fieldSummary (zip [0..] (osFields (ovSchema ov)))
          , "diagnostics" .= [diagnostic "info" "schema_loaded" "overlay schema is valid and loaded"]
          ]

-- | Handle @get_overlay_provenance@ — return one overlay's provenance header.
handleGetOverlayProvenance :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetOverlayProvenance ctx reqId params = do
  case Aeson.parseMaybe parseOverlayNameParam params of
    Nothing -> pure $ errResponse reqId "missing or invalid 'overlay' parameter"
    Just name -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      case lookupOverlay name (tsOverlayStore snap) of
        Nothing -> pure $ errResponse reqId (overlayNotFoundMessage name (tsOverlayStore snap))
        Just ov -> pure $ okResponse reqId $ object
          [ "overlay" .= name
          , "format" .= ("topolay-provenance" :: Text)
          , "provenance" .= provenanceJSON (ovProvenance ov)
          , "diagnostics" .= [diagnostic "info" "provenance_loaded" "overlay provenance header is available"]
          ]

-- | Handle @export_overlay_data@ — export schema, provenance, and payload JSON.
handleExportOverlayData :: CommandContext -> Int -> Value -> IO SeerResponse
handleExportOverlayData ctx reqId params = do
  case Aeson.parseMaybe parseOverlayExportParams params of
    Nothing -> pure $ errResponse reqId "missing or invalid 'overlay' parameter"
    Just (name, mChunks) -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      case lookupOverlay name (tsOverlayStore snap) of
        Nothing -> pure $ errResponse reqId (overlayNotFoundMessage name (tsOverlayStore snap))
        Just ov -> do
          let requested = maybe [] id mChunks
              ov' = maybe ov (`filterOverlayChunks` ov) mChunks
              missing = [ cid | cid <- requested, not (overlayHasChunk cid ov) ]
              diagnostics = diagnostic "info" "overlay_export_ready" "overlay export payload built"
                : [ diagnostic "warn" "missing_chunk" ("overlay chunk not present: " <> Text.pack (show cid))
                  | cid <- missing
                  ]
          pure $ okResponse reqId $ object
            [ "overlay" .= name
            , "format" .= ("topolay-json" :: Text)
            , "schema" .= toJSON (ovSchema ov)
            , "provenance" .= provenanceJSON (ovProvenance ov)
            , "chunk_count" .= overlayDataChunkCount (ovData ov')
            , "payload" .= overlayToJSON ov'
            , "diagnostics" .= diagnostics
            ]

-- | Handle @validate_overlay_import@ — validate a schema/payload pair.
handleValidateOverlayImport :: CommandContext -> Int -> Value -> IO SeerResponse
handleValidateOverlayImport _ctx reqId params = do
  let result = validateOverlayImportPayload params
      (isValid, mName, diags) = case result of
        Right name -> (True, Just name, [diagnostic "info" "overlay_import_valid" "schema and payload are importable"])
        Left errs -> (False, Nothing, errs)
  pure $ okResponse reqId $ object
    [ "valid" .= isValid
    , "overlay" .= mName
    , "diagnostics" .= diags
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
          let world0 = snapshotToWorld ui terrainSnap
          simPlan <- getPluginSimulationPlan (ahPluginManagerHandle handles) (Just (overlayNames (twOverlays world0)))
          let world = normalizeWorldSchedulesForBindings world0 (pspExecutableNodes simPlan)
          pluginDirs <- getPluginDataDirectories (ahPluginManagerHandle handles)
          externalDataSources <- getPluginExternalDataSources (ahPluginManagerHandle handles)
          result <- saveNamedWorldWithPluginsAndExternalData name ui world pluginDirs externalDataSources
          case result of
            Right () -> do
              -- Update UI with saved world name + config snapshot
              wDir <- worldDir
              notifyWorldChanged (ahPluginManagerHandle handles)
                                 (Just (Text.pack (wDir </> Text.unpack name)))
              setUiWorldName (ahUiHandle handles) name
              setUiWorldConfig (ahUiHandle handles) (Just (snapshotFromUi ui name))
              _ <- publishUiMutation handles
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "saved" .= True
                , "formats" .= worldPersistenceFormats
                , "diagnostics" .= [diagnostic "info" "world_saved" "world.topo, world.topolay, config.json, and meta.json were written"]
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
          let handles = ccActorHandles ctx
              simH = ahSimulationHandle handles
          beginSimWorldTransition simH
          result <- loadNamedWorld name
          case result of
            Left err -> do
              cancelSimWorldTransition simH
              pure $ errResponse reqId ("failed to load world: " <> err)
            Right (manifest, snapshot, loadedWorld) -> do
              let uiH = ahUiHandle handles
                  loadedOverlayNames = overlayNames (twOverlays loadedWorld)
              simPlan <- getPluginSimulationPlan (ahPluginManagerHandle handles) (Just loadedOverlayNames)
              let world = normalizeWorldSchedulesForBindings loadedWorld (pspExecutableNodes simPlan)
              -- Replace terrain data
              clearSimWorld simH ()
              replaceTerrainData (ahDataHandle handles) world
              setUiOverlayNames uiH (overlayNames (twOverlays world))
              -- Capture authoritative data after the actor mutations. The
              -- refs are committed only after the matching UI barrier below.
              dataSnap <- getDataSnapshot (ahDataHandle handles)
              terrainSnap' <- getTerrainSnapshot (ahDataHandle handles)
              -- Apply config snapshot. Layered view selection is UI-only, so
              -- loading a world resets it to the normal default instead of
              -- carrying an old overlay/basis into the newly loaded data.
              applySnapshotToUi snapshot uiH
              setUiViewSelection uiH defaultLayeredViewState
              setUiSimTickCount uiH (wtTick (twWorldTime world))
              setUiWorldName uiH name
              setUiWorldConfig uiH (Just snapshot)
              -- Notify plugins
              wDir <- worldDir
              notifyWorldChanged (ahPluginManagerHandle handles)
                                 (Just (Text.pack (wDir </> Text.unpack name)))
              -- Bind while retaining the transition latch, then barrier every
              -- final UI/log write into the same data+terrain publication.
              setSimWorldWithNodesSync simH world (pspExecutableNodes simPlan)
              ui <- getUiSnapshot uiH
              logSnapshot <- getLogSnapshot (ahLogHandle handles)
              snapshotVersion <- publishSnapshot
                (ahSnapshotVersionRef handles)
                (withLogSnapshot logSnapshot
                  (withUiSnapshot ui
                    (dataAndTerrainSnapshotUpdate
                      (ahDataSnapshotRef handles) dataSnap
                      (ahTerrainSnapshotRef handles) terrainSnap')))
              cancelSimWorldTransition simH
              enqueueAtlasRebuildForTerrain
                handles (uiViewMode ui) ui snapshotVersion terrainSnap'
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "loaded" .= True
                , "formats" .= worldPersistenceFormats
                , "overlay_names" .= wsmOverlayNames manifest
                , "diagnostics" .= [diagnostic "info" "world_loaded" "world bundle, config snapshot, manifest, and overlay sidecar were loaded"]
                ]

-- | Handle @set_world_name@ — set the display name of the current world.
--
-- Params: @{ "name": "my-world" }@
handleSetWorldName :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetWorldName ctx reqId params = do
  case Aeson.parseMaybe parseName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name
      | Text.null name ->
          pure $ errResponse reqId "world name must not be empty"
      | otherwise -> do
          let handles = ccActorHandles ctx
          setUiWorldName (ahUiHandle handles) name
          _ <- publishUiMutation handles
          pure $ okResponse reqId $ object
            [ "name" .= name ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

overlayStoreSummaries :: OverlayStore -> Maybe (Text, Int) -> [Value]
overlayStoreSummaries (OverlayStore overlays) active =
  [ overlaySummary active ov | ov <- Map.elems overlays ]

overlaySummary :: Maybe (Text, Int) -> Overlay -> Value
overlaySummary active ov = object
  [ "name" .= osName schema
  , "version" .= osVersion schema
  , "description" .= osDescription schema
  , "storage" .= storageText (osStorage schema)
  , "field_count" .= length (osFields schema)
  , "fields" .= map fieldSummary (zip [0..] (osFields schema))
  , "chunk_count" .= overlayDataChunkCount (ovData ov)
  , "populated_tile_count" .= overlayDataTileCount (ovData ov)
  , "dependencies" .= depsJSON (osDependencies schema)
  , "provenance" .= provenanceJSON (ovProvenance ov)
  , "active" .= case active of
      Just (name, _) -> name == osName schema
      Nothing -> False
  , "active_field_index" .= case active of
      Just (name, idx) | name == osName schema -> Just idx
      _ -> Nothing
  ]
  where
    schema = ovSchema ov

fieldSummary :: (Int, OverlayFieldDef) -> Value
fieldSummary (idx, field) = object
  [ "index" .= idx
  , "name" .= ofdName field
  , "type" .= fieldTypeText (ofdType field)
  , "default" .= ofdDefault field
  , "indexed" .= ofdIndexed field
  , "renamed_from" .= ofdRenamedFrom field
  ]

fieldTypeText :: OverlayFieldType -> Text
fieldTypeText OFFloat = "float"
fieldTypeText OFInt = "int"
fieldTypeText OFBool = "bool"
fieldTypeText OFText = "text"
fieldTypeText (OFList t) = "list<" <> fieldTypeText t <> ">"

storageText :: OverlayStorage -> Text
storageText StorageSparse = "sparse"
storageText StorageDense = "dense"

depsJSON :: OverlayDeps -> Value
depsJSON deps = object
  [ "terrain" .= odTerrain deps
  , "overlays" .= odOverlays deps
  ]

provenanceJSON :: OverlayProvenance -> Value
provenanceJSON prov = object
  [ "seed" .= opSeed prov
  , "version" .= opVersion prov
  , "source" .= opSource prov
  ]

overlayDataChunkCount :: OverlayData -> Int
overlayDataChunkCount (SparseData chunks) = IntMap.size chunks
overlayDataChunkCount (DenseData chunks) = IntMap.size chunks

overlayDataTileCount :: OverlayData -> Int
overlayDataTileCount (SparseData chunks) = sum (map chunkSize (IntMap.elems chunks))
overlayDataTileCount (DenseData chunks) = sum (map denseChunkTileCount (IntMap.elems chunks))

denseChunkTileCount :: V.Vector (U.Vector Float) -> Int
denseChunkTileCount fields
  | V.null fields = 0
  | otherwise = U.length (V.head fields)

filterOverlayChunks :: [Int] -> Overlay -> Overlay
filterOverlayChunks chunkIds ov = ov { ovData = filtered }
  where
    keep key _ = key `elem` chunkIds
    filtered = case ovData ov of
      SparseData chunks -> SparseData (IntMap.filterWithKey keep chunks)
      DenseData chunks -> DenseData (IntMap.filterWithKey keep chunks)

overlayHasChunk :: Int -> Overlay -> Bool
overlayHasChunk cid ov = case ovData ov of
  SparseData chunks -> IntMap.member cid chunks
  DenseData chunks -> IntMap.member cid chunks

validateOverlayImportPayload :: Value -> Either [Value] Text
validateOverlayImportPayload (Object obj) =
  case KM.lookup "schema" obj of
    Nothing -> Left [diagnostic "error" "missing_schema" "missing required field 'schema'"]
    Just schemaValue ->
      case parseOverlaySchema (BL.toStrict (Aeson.encode schemaValue)) of
        Left err -> Left [diagnostic "error" "invalid_schema" err]
        Right schema ->
          case KM.lookup "payload" obj <|> KM.lookup "data" obj of
            Nothing -> Left [diagnostic "error" "missing_payload" "missing required field 'payload'"]
            Just payload ->
              case overlayFromJSON schema payload of
                Left err -> Left [diagnostic "error" "invalid_payload" err]
                Right _ -> Right (osName schema)
validateOverlayImportPayload _ =
  Left [diagnostic "error" "invalid_request" "request body must be a JSON object"]

overlayNotFoundMessage :: Text -> OverlayStore -> Text
overlayNotFoundMessage name store =
  "overlay not found: " <> name <> "; available: " <> Text.intercalate ", " (overlayNames store)

diagnostic :: Text -> Text -> Text -> Value
diagnostic level code message = object
  [ "level" .= level
  , "code" .= code
  , "message" .= message
  ]

worldPersistenceFormats :: [Text]
worldPersistenceFormats = ["world.topo", "world.topolay", "config.json", "meta.json"]

manifestToJSON :: WorldSaveManifest -> Value
manifestToJSON m = object
  [ "name"          .= wsmName m
  , "seed"          .= wsmSeed m
  , "chunk_size"    .= wsmChunkSize m
  , "chunk_count"   .= wsmChunkCount m
  , "overlay_names" .= wsmOverlayNames m
  , "weather_layers" .= wsmWeatherLayers m
  , "created_at"    .= show (wsmCreatedAt m)
  ]

parseName :: Value -> Aeson.Parser Text
parseName = Aeson.withObject "params" (.: "name")

parseOverlayNameParam :: Value -> Aeson.Parser Text
parseOverlayNameParam = Aeson.withObject "params" (.: "overlay")

parseOverlayExportParams :: Value -> Aeson.Parser (Text, Maybe [Int])
parseOverlayExportParams = Aeson.withObject "params" $ \o ->
  (,) <$> o .: "overlay" <*> o .:? "chunks"
