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
  , saveNamedWorldWithPlugins
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
import Control.Monad (when)
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
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>), takeFileName)

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
saveNamedWorld name uiSnap world =
  saveNamedWorldWithPlugins name uiSnap world []

-- | Save a named world including plugin data directories.
--
-- In addition to the base @world.topo@, @config.json@, and @meta.json@,
-- copies each plugin's data directory into a @plugins\/\<name\>\/@ subdirectory
-- of the world save path and records the mapping in the manifest.
saveNamedWorldWithPlugins
  :: Text             -- ^ World name
  -> UiState          -- ^ Current UI state (config snapshot source)
  -> TerrainWorld     -- ^ Terrain data to persist
  -> [(Text, FilePath)]
  -- ^ @(pluginName, absoluteDataDir)@ — plugin data directories to bundle
  -> IO (Either Text ())
saveNamedWorldWithPlugins name uiSnap world pluginDirs = do
  dir <- worldDir
  let nameStr   = Text.unpack name
      worldPath = dir </> nameStr
      topoFile  = worldPath </> "world.topo"
  result <- try @IOException $ do
    createDirectoryIfMissing True dir

    let provenance = makeProvenance uiSnap
        snapshot = snapshotFromUi uiSnap name
        worldForSave = normalizeSaveWorldManifest world
    now <- getCurrentTime
    -- Deduplicate plugin data directories (multiple plugins may share one)
    let uniqueDirs = deduplicatePluginDirs pluginDirs
        pluginDataEntries =
          [ (pName, Text.pack relDir)
          | (pName, _absDir, relDir) <- uniqueDirs
          ]
    let manifest = WorldSaveManifest
          { wsmName       = name
          , wsmSeed       = uiSeed uiSnap
          , wsmChunkSize  = uiChunkSize uiSnap
          , wsmCreatedAt  = now
          , wsmChunkCount = chunkCount worldForSave
          , wsmOverlayNames = twOverlayManifest worldForSave
          , wsmPluginData = pluginDataEntries
          }
        extraFiles =
          [ ("config.json", BSL.toStrict (encode snapshot))
          , ("meta.json", BSL.toStrict (encode manifest))
          ]
    topoResult <- saveWorldBundleWithProvenance topoFile provenance extraFiles worldForSave
    case topoResult of
      Left err -> fail ("Terrain+overlay save failed: " <> show err)
      Right () -> pure ()
    -- Copy plugin data directories into the world save
    mapM_ (copyPluginDataDir worldPath) uniqueDirs

  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-------------------------------------------------------------------------------
-- Load
-------------------------------------------------------------------------------

-- | Load a named world from @~\/.topo\/worlds\/<name>\/@.
--
-- Returns the manifest, config snapshot, and terrain data on success.
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
          -- 2. Load config snapshot
          cfgResult <- loadSnapshot cfgFile
          case cfgResult of
            Left err -> pure (Left ("Failed to load config.json: " <> err))
            Right snapshot -> do
              -- 3. Load terrain
              topoResult <- loadWorldBundleWithProvenance BestEffort topoFile
              case topoResult of
                Left err ->
                  pure (Left ("Failed to load world bundle: "
                        <> Text.pack (show err)))
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

normalizeSaveWorldManifest :: TerrainWorld -> TerrainWorld
normalizeSaveWorldManifest world =
  world { twOverlayManifest = normalizedSaveOverlayManifest world }

normalizedSaveOverlayManifest :: TerrainWorld -> [Text]
normalizedSaveOverlayManifest world =
  normalizeOverlayManifest (twOverlayManifest world) (overlayNames (twOverlays world))

normalizeOverlayManifest :: [Text] -> [Text] -> [Text]
normalizeOverlayManifest preferred discovered =
  let preferredPresent = uniquePreserving [name | name <- preferred, name `elem` discovered]
      remaining = [name | name <- discovered, name `notElem` preferredPresent]
  in preferredPresent ++ remaining

uniquePreserving :: [Text] -> [Text]
uniquePreserving = foldr addUnique []
  where
    addUnique name acc
      | name `elem` acc = acc
      | otherwise = name : acc

-------------------------------------------------------------------------------
-- Plugin data directory helpers
-------------------------------------------------------------------------------

-- | Deduplicate plugin data directories.
--
-- Multiple plugins may share the same directory. Returns
-- @(pluginName, absoluteDir, relativeSubdir)@ triples where
-- @relativeSubdir@ is the last path component used as the
-- subdirectory name inside the world save.
deduplicatePluginDirs :: [(Text, FilePath)] -> [(Text, FilePath, String)]
deduplicatePluginDirs dirs =
  let withRel = [ (pName, absDir, takeFileName absDir)
                | (pName, absDir) <- dirs
                ]
      -- Keep only the first occurrence of each absolute directory
      go seen [] = (seen, [])
      go seen ((pName, absDir, rel):rest)
        | absDir `elem` map snd3 seen = go seen rest
        | otherwise = go ((pName, absDir, rel) : seen) rest
      (unique, _) = go [] withRel
  in reverse unique
  where
    snd3 (_, b, _) = b

-- | Copy a single plugin data directory into the world save.
--
-- Creates @worldPath\/plugins\/\<relDir\>\/@ and recursively copies
-- files from the source directory.
copyPluginDataDir :: FilePath -> (Text, FilePath, String) -> IO ()
copyPluginDataDir worldPath (_pluginName, srcDir, relDir) = do
  exists <- doesDirectoryExist srcDir
  when exists $ do
    let destDir = worldPath </> "plugins" </> relDir
    createDirectoryIfMissing True destDir
    copyDirectoryRecursive srcDir destDir

-- | Recursively copy the contents of one directory to another.
--
-- The destination directory must already exist. Existing files in
-- the destination are overwritten.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dest = do
  entries <- listDirectory src
  mapM_ copyEntry entries
  where
    copyEntry entry = do
      let srcPath  = src </> entry
          destPath = dest </> entry
      isDir <- doesDirectoryExist srcPath
      if isDir
        then do
          createDirectoryIfMissing True destPath
          copyDirectoryRecursive srcPath destPath
        else copyFile srcPath destPath
