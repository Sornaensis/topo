{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | World save\/load persistence layer.
--
-- Each named world is stored as a directory under @~\/.topo\/worlds\/<name>\/@
-- containing:
--
-- * @world.topo@ — binary terrain data (via unified world-bundle persistence)
-- * @world.topolay\/@ — overlay sidecar schema/data files
-- * @config.json@ — 'ConfigSnapshot' snapshot at time of save
-- * @meta.json@ — 'WorldSaveManifest' with seed, chunk size, etc.
module Seer.World.Persist
  ( -- * Types (re-exported from "Seer.World.Persist.Types")
    WorldSaveManifest(..)
    -- * Directory helpers
  , worldDir
    -- * Save \/ Load
  , saveNamedWorld
  , loadNamedWorld
  , loadNamedSparseOverlayChunk
    -- * Snapshot conversion
  , snapshotToWorld
    -- * Listing
  , listWorlds
    -- * Deletion (stub)
  , deleteNamedWorld
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..))
import Seer.Config.Snapshot (snapshotFromUi, loadSnapshot)
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Calendar (defaultWorldTime, defaultPlanetAge)
import Topo.Hex (defaultHexGridMeta)
import Topo.Metadata (emptyMetadataStore)
import Topo.Overlay (OverlayChunk, emptyOverlayStore, overlayNames)
import Topo.Overlay.Storage
  ( loadOverlayChunk
  , overlayDirPath
  , renderOverlayStorageError
  )
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, mkLatitudeMapping)
import Topo.Units (defaultUnitScales)
import Topo.Storage
  ( WorldProvenance(..)
  , MapProvenance(..)
  , emptyMapProvenance
  )
import Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , saveWorldBundleWithProvenance
  , loadWorldBundleWithProvenance
  )
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..))

-------------------------------------------------------------------------------
-- Directory helpers
-------------------------------------------------------------------------------

-- | Return the worlds directory (@~\/.topo\/worlds\/@), creating it if
-- necessary.
worldDir :: IO FilePath
worldDir = do
  home <- getHomeDirectory
  let dir = home </> ".topo" </> "worlds"
  createDirectoryIfMissing True dir
  pure dir

-------------------------------------------------------------------------------
-- Save
-------------------------------------------------------------------------------

-- | Save a named world: terrain data, config snapshot, and metadata.
--
-- Creates @~\/.topo\/worlds\/<name>\/@ containing @world.topo@,
-- @config.json@, and @meta.json@.
saveNamedWorld
  :: Text           -- ^ World name
  -> UiState        -- ^ Current UI state (config snapshot source)
  -> TerrainWorld   -- ^ Terrain data to persist
  -> IO (Either Text ())
saveNamedWorld name uiSnap world = do
  dir <- worldDir
  let nameStr   = Text.unpack name
      worldPath = dir </> nameStr
      topoFile  = worldPath </> "world.topo"
  result <- try @IOException $ do
    createDirectoryIfMissing True dir

    let provenance = makeProvenance uiSnap
    let snapshot = snapshotFromUi uiSnap name
    now <- getCurrentTime
    let manifest = WorldSaveManifest
          { wsmName       = name
          , wsmSeed       = uiSeed uiSnap
          , wsmChunkSize  = uiChunkSize uiSnap
          , wsmCreatedAt  = now
          , wsmChunkCount = chunkCount world
          , wsmOverlayNames = overlayNames (twOverlays world)
          }
        extraFiles =
          [ ("config.json", BSL.toStrict (encode snapshot))
          , ("meta.json", BSL.toStrict (encode manifest))
          ]
    topoResult <- saveWorldBundleWithProvenance topoFile provenance extraFiles world
    case topoResult of
      Left err -> fail ("Terrain+overlay save failed: " <> show err)
      Right () -> pure ()

  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-------------------------------------------------------------------------------
-- Load
-------------------------------------------------------------------------------

-- | Load a named world from @~\/.topo\/worlds\/<name>\/@.
--
-- Returns the manifest, config snapshot, and terrain data on success.
-- Legacy @ConfigPreset@ files are automatically migrated to
-- @ConfigSnapshot@ on load.
loadNamedWorld
  :: Text
  -> IO (Either Text (WorldSaveManifest, ConfigSnapshot, TerrainWorld))
loadNamedWorld name = do
  dir <- worldDir
  let nameStr   = Text.unpack name
      worldPath = dir </> nameStr
      topoFile  = worldPath </> "world.topo"
      cfgFile   = worldPath </> "config.json"
      metaFile  = worldPath </> "meta.json"

  exists <- doesDirectoryExist worldPath
  if not exists
    then pure (Left ("World directory not found: " <> name))
    else do
      -- 1. Load manifest
      metaResult <- loadJsonFile metaFile
      case metaResult of
        Left err -> pure (Left ("Failed to load meta.json: " <> err))
        Right manifest -> do
          -- 2. Load config (with legacy ConfigPreset fallback)
          cfgResult <- loadSnapshot cfgFile
          case cfgResult of
            Left err -> pure (Left ("Failed to load config.json: " <> err))
            Right snapshot -> do
              -- 3. Load terrain
              topoResult <- loadWorldBundleWithProvenance StrictManifest topoFile
              case topoResult of
                Left _strictErr -> do
                  -- Migration-safe fallback for older saves that have
                  -- no sidecar overlays yet.
                  fallback <- loadWorldBundleWithProvenance BestEffort topoFile
                  case fallback of
                    Left err ->
                      pure (Left ("Failed to load world bundle: "
                            <> Text.pack (show err)))
                    Right (_prov, world) ->
                      pure (Right (manifest, snapshot, world))
                Right (_prov, world) ->
                  pure (Right (manifest, snapshot, world))

-- TODO(viewport-overlay-adoption):
-- 1) Keep world-level discovery/manifest validation in
--    'loadWorldBundleWithProvenance' inside 'loadNamedWorld'.
-- 2) For large sparse overlays, prefer chunk-on-demand hydration in Seer
--    viewports via 'loadNamedSparseOverlayChunk'.
-- 3) Keep this helper independent from runtime wiring for now so UI/atlas
--    code can adopt it incrementally without changing save/load semantics.

-- | Load one sparse overlay chunk from a saved world sidecar.
--
-- This is an adoption seam for viewport-aware sparse overlay hydration.
-- It does not change the default runtime path, which still loads overlays
-- via world-bundle policy during 'loadNamedWorld'.
loadNamedSparseOverlayChunk
  :: Text  -- ^ World name
  -> Text  -- ^ Overlay name
  -> Int   -- ^ Chunk id
  -> IO (Either Text OverlayChunk)
loadNamedSparseOverlayChunk worldName overlayName chunkId = do
  dir <- worldDir
  let worldPath = dir </> Text.unpack worldName
      topoFile = worldPath </> "world.topo"
      sidecarDir = overlayDirPath topoFile
  exists <- doesDirectoryExist worldPath
  if not exists
    then pure (Left ("World directory not found: " <> worldName))
    else do
      chunkResult <- loadOverlayChunk sidecarDir overlayName chunkId
      pure $ case chunkResult of
        Left err -> Left (renderOverlayStorageError err)
        Right chunk -> Right chunk

-------------------------------------------------------------------------------
-- Snapshot conversion
-------------------------------------------------------------------------------

-- | Reconstruct a 'TerrainWorld' from a 'TerrainSnapshot'.
--
-- Terrain, climate, weather, and river chunks are preserved from the
-- snapshot.  Groundwater, volcanism, and glacier data are stored as
-- empty maps (they are not retained in the Data actor after
-- generation).  Planet and slice defaults are used because the
-- UI-level values are captured separately in the config preset.
snapshotToWorld :: TerrainSnapshot -> TerrainWorld
snapshotToWorld ts = TerrainWorld
  { twTerrain     = tsTerrainChunks ts
  , twClimate     = tsClimateChunks ts
  , twRivers      = tsRiverChunks ts
  , twGroundwater = IntMap.empty
  , twVolcanism   = IntMap.empty
  , twGlaciers    = IntMap.empty
  , twWaterBodies = IntMap.empty
  , twVegetation  = tsVegetationChunks ts
  , twHexGrid     = defaultHexGridMeta
  , twMeta        = emptyMetadataStore
  , twConfig      = wc
  , twPlanet      = defaultPlanetConfig
  , twSlice       = defaultWorldSlice
  , twLatMapping  = mkLatitudeMapping defaultPlanetConfig defaultWorldSlice wc
  , twWorldTime   = defaultWorldTime
  , twSeed        = 0
  , twPlanetAge   = defaultPlanetAge
  , twGenConfig   = Nothing
  , twUnitScales  = defaultUnitScales
  , twOverlays    = emptyOverlayStore
  , twOverlayManifest = []
  }
  where
    wc = WorldConfig { wcChunkSize = tsChunkSize ts }

-------------------------------------------------------------------------------
-- Listing
-------------------------------------------------------------------------------

-- | List all saved worlds, sorted by creation date (newest first).
--
-- Reads @meta.json@ from each subdirectory of @~\/.topo\/worlds\/@.
-- Directories without a valid @meta.json@ are silently skipped.
listWorlds :: IO [WorldSaveManifest]
listWorlds = do
  dir <- worldDir
  entries <- listDirectory dir
  manifests <- mapM (tryLoadManifest dir) entries
  let valid = [ m | Just m <- manifests ]
  pure (sortOn (Down . wsmCreatedAt) valid)

-- | Attempt to load a manifest from a subdirectory. Returns 'Nothing'
-- on any failure (missing file, parse error, not a directory).
tryLoadManifest :: FilePath -> FilePath -> IO (Maybe WorldSaveManifest)
tryLoadManifest dir entry = do
  let path = dir </> entry </> "meta.json"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- loadJsonFile path
      case result of
        Left _  -> pure Nothing
        Right m -> pure (Just m)

-------------------------------------------------------------------------------
-- Deletion (stub)
-------------------------------------------------------------------------------

-- | Delete a named world directory and all its contents.
deleteNamedWorld :: Text -> IO (Either Text ())
deleteNamedWorld name = do
  dir <- worldDir
  let worldPath = dir </> Text.unpack name
  exists <- doesDirectoryExist worldPath
  if not exists
    then pure (Left ("World not found: " <> name))
    else do
      result <- try @IOException (removeDirectoryRecursive worldPath)
      pure $ case result of
        Left err -> Left (Text.pack (show err))
        Right () -> Right ()

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

-- | Construct a 'WorldProvenance' from the current UI state.
makeProvenance :: UiState -> WorldProvenance
makeProvenance ui = WorldProvenance
  { wpSeed    = uiSeed ui
  , wpVersion = 1
  , wpNotes   = Text.empty
  , wpTerrain = emptyMapProvenance { mpSeed = uiSeed ui }
  , wpClimate = emptyMapProvenance { mpSeed = uiSeed ui }
  , wpWeather = emptyMapProvenance { mpSeed = uiSeed ui }
  , wpBiome   = emptyMapProvenance { mpSeed = uiSeed ui }
  }

-- | Count terrain chunks in a world.
chunkCount :: TerrainWorld -> Int
chunkCount = IntMap.size . twTerrain

-- | Read and decode a JSON file.
loadJsonFile :: FromJSON a => FilePath -> IO (Either Text a)
loadJsonFile path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right bs -> case eitherDecodeStrict' bs of
      Left parseErr -> Left (Text.pack parseErr)
      Right val     -> Right val
