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
    WorldPluginDataDirectory(..)
  , WorldExternalDataSourceSnapshot(..)
  , WorldWeatherLayerManifest(..)
  , WorldSaveManifest(..)
    -- * Directory helpers
  , worldDir
    -- * Save \/ Load
  , WorldSaveHooks(..)
  , defaultWorldSaveHooks
  , saveNamedWorld
  , saveNamedWorldWithPlugins
  , saveNamedWorldWithPluginsAndExternalData
  , saveNamedWorldWithPluginsAndExternalDataAndHooks
  , loadNamedWorld
    -- * Snapshot conversion
  , snapshotToWorld
    -- * Listing
  , listWorlds
    -- * Deletion
  , deleteNamedWorld
  ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON(..), eitherDecodeStrict', encode, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath (isAbsolute, (</>))
import System.IO.Error (isDoesNotExistError)

import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..))
import Actor.UI (UiState(..))
import Seer.Config (configFromUi)
import Seer.Config.Snapshot (snapshotFromUi, loadSnapshot)
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.Persistence.Name (validatePersistenceName)
import Seer.World.Persist.Types
  ( WorldPluginDataDirectory(..)
  , WorldExternalDataSourceSnapshot(..)
  , WorldWeatherLayerManifest(..)
  , WorldSaveManifest(..)
  )
import Seer.World.PluginDataCopy
  ( PluginDataCopyHooks(..)
  , defaultPluginDataCopyHooks
  , copyPluginDataDirectory
  )
import Topo.Calendar (defaultPlanetAge)
import Topo.Metadata (emptyMetadataStore)
import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayProvenance(..)
  , OverlayStore
  , emptyOverlayStore
  , insertOverlay
  , lookupOverlay
  , overlayNames
  )
import Topo.Planet (mkLatitudeMapping)
import Topo.Units (defaultUnitScales)
import Topo.Storage
  ( WorldProvenance(..)
  , MapProvenance(..)
  , emptyMapProvenance
  )
import Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , BundleSaveHooks(..)
  , defaultBundleSaveHooks
  , saveWorldBundleWithProvenanceAndHooks
  , loadWorldBundleWithProvenance
  )
import Topo.Types (WeatherChunk, WorldConfig(..))
import Topo.Weather (weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))
import Topo.WorldGen (WorldGenConfig(..))

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

-- | Hooks for deterministic save fault and concurrent-mutation tests.
-- Production callers should use 'defaultWorldSaveHooks'.
data WorldSaveHooks = WorldSaveHooks
  { wshBeforePluginDataEntryOpen :: FilePath -> IO ()
  }

defaultWorldSaveHooks :: WorldSaveHooks
defaultWorldSaveHooks = WorldSaveHooks
  { wshBeforePluginDataEntryOpen = const (pure ())
  }

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
  saveNamedWorldWithPluginsAndExternalData name uiSnap world [] []

-- | Save a named world including plugin data directories.
--
-- In addition to the base @world.topo@, @config.json@, and @meta.json@,
-- copies each plugin's host-created data root into its validated archive
-- directory below @plugins\/@ and records the mapping in the manifest.
saveNamedWorldWithPlugins
  :: Text             -- ^ World name
  -> UiState          -- ^ Current UI state (config snapshot source)
  -> TerrainWorld     -- ^ Terrain data to persist
  -> [WorldPluginDataDirectory]
  -- ^ Host-derived plugin data roots and safe archive destinations to bundle.
  -> IO (Either Text ())
saveNamedWorldWithPlugins name uiSnap world pluginDirs =
  saveNamedWorldWithPluginsAndExternalData name uiSnap world pluginDirs []

-- | Save a named world including plugin data directories and external
-- data-source declarations/references.
--
-- External data-source snapshots are persisted as opaque, backend-neutral
-- metadata.  Loading returns them in 'WorldSaveManifest' but does not reconnect,
-- migrate, lock, or clean up provider-owned stores.
saveNamedWorldWithPluginsAndExternalData
  :: Text             -- ^ World name
  -> UiState          -- ^ Current UI state (config snapshot source)
  -> TerrainWorld     -- ^ Terrain data to persist
  -> [WorldPluginDataDirectory]
  -- ^ Host-derived plugin data roots and safe archive destinations to bundle.
  -> [WorldExternalDataSourceSnapshot]
  -- ^ Opaque external data-source declarations/references to preserve
  -> IO (Either Text ())
saveNamedWorldWithPluginsAndExternalData name uiSnap world pluginDirs externalDataSources =
  saveNamedWorldWithPluginsAndExternalDataAndHooks
    defaultWorldSaveHooks name uiSnap world pluginDirs externalDataSources

-- | Hookable variant of 'saveNamedWorldWithPluginsAndExternalData'. Plugin
-- data is populated inside the same staging directory as the core bundle, so
-- any traversal or copy failure occurs before the old world is replaced.
saveNamedWorldWithPluginsAndExternalDataAndHooks
  :: WorldSaveHooks
  -> Text
  -> UiState
  -> TerrainWorld
  -> [WorldPluginDataDirectory]
  -> [WorldExternalDataSourceSnapshot]
  -> IO (Either Text ())
saveNamedWorldWithPluginsAndExternalDataAndHooks hooks name uiSnap world pluginDirs externalDataSources =
  case validatePersistenceName name of
    Left err -> pure (Left err)
    Right () -> do
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
        preparedDirs <- either (fail . Text.unpack) pure (preparePluginDataDirs pluginDirs)
        let pluginDataEntries =
              [ (pName, archiveDir)
              | PreparedPluginDataDir pName _sourceDir archiveDir <- preparedDirs
              ]
        let manifest = WorldSaveManifest
              { wsmName       = name
              , wsmSeed       = uiSeed uiSnap
              , wsmChunkSize  = uiChunkSize uiSnap
              , wsmCreatedAt  = now
              , wsmChunkCount = chunkCount worldForSave
              , wsmOverlayNames = twOverlayManifest worldForSave
              , wsmWeatherLayers = weatherLayersForWorld worldForSave
              , wsmPluginData = pluginDataEntries
              , wsmExternalDataSources = externalDataSources
              }
            extraFiles =
              [ ("config.json", BSL.toStrict (encode snapshot))
              , ("meta.json", BSL.toStrict (encode manifest))
              ]
            pluginCopyHooks = defaultPluginDataCopyHooks
              { pdchBeforeEntryOpen = wshBeforePluginDataEntryOpen hooks
              }
            bundleHooks = defaultBundleSaveHooks
              { bshPopulateStaging = \stagingPath ->
                  mapM_ (copyPluginDataDir pluginCopyHooks stagingPath) preparedDirs
              }
        topoResult <- saveWorldBundleWithProvenanceAndHooks
          bundleHooks topoFile provenance extraFiles worldForSave
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
loadNamedWorld
  :: Text
  -> IO (Either Text (WorldSaveManifest, ConfigSnapshot, TerrainWorld))
loadNamedWorld name = case validatePersistenceName name of
  Left err -> pure (Left err)
  Right () -> do
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

-------------------------------------------------------------------------------
-- Snapshot conversion
-------------------------------------------------------------------------------

-- | Reconstruct a 'TerrainWorld' from a 'TerrainSnapshot'.
--
-- Terrain, climate, river, groundwater, volcanism, glacier, water-body,
-- vegetation, and overlay chunks are preserved from the snapshot. World-level
-- planet, slice, hex, and world-time metadata are preserved from the
-- authoritative terrain snapshot context rather than reconstructed from live UI
-- sliders.
snapshotToWorld :: UiState -> TerrainSnapshot -> TerrainWorld
snapshotToWorld uiSnap ts = TerrainWorld
  { twTerrain     = tsTerrainChunks ts
  , twClimate     = tsClimateChunks ts
  , twRivers      = tsRiverChunks ts
  , twGroundwater = tsGroundwaterChunks ts
  , twVolcanism   = tsVolcanismChunks ts
  , twGlaciers    = tsGlacierChunks ts
  , twWaterBodies = tsWaterBodyChunks ts
  , twVegetation  = tsVegetationChunks ts
  , twHexGrid     = tgcHexGrid geo
  , twMeta        = emptyMetadataStore
  , twConfig      = wc
  , twPlanet      = tgcPlanet geo
  , twSlice       = tgcSlice geo
  , twLatMapping  = mkLatitudeMapping (tgcPlanet geo) (tgcHexGrid geo) (tgcSlice geo) wc
  , twWorldTime   = tgcWorldTime geo
  , twSeed        = uiSeed uiSnap
  , twPlanetAge   = defaultPlanetAge
  , twGenConfig   = Just (toJSON genCfg)
  , twUnitScales  = defaultUnitScales
  , twOverlays    = snapshotOverlayStore uiSnap ts
  , twOverlayManifest = overlayNames (snapshotOverlayStore uiSnap ts)
  }
  where
    wc = WorldConfig { wcChunkSize = tsChunkSize ts }
    geo = tsGeoContext ts
    genCfg = (configFromUi uiSnap)
      { worldPlanet = tgcPlanet geo
      , worldSlice = tgcSlice geo
      , worldHexGrid = tgcHexGrid geo
      }

snapshotOverlayStore :: UiState -> TerrainSnapshot -> OverlayStore
snapshotOverlayStore uiSnap ts =
  ensureWeatherOverlayFromSnapshot (uiSeed uiSnap) (tsWeatherChunks ts) (tsOverlayStore ts)

ensureWeatherOverlayFromSnapshot :: Word64 -> IntMap.IntMap WeatherChunk -> OverlayStore -> OverlayStore
ensureWeatherOverlayFromSnapshot seed weatherChunks store
  | IntMap.null weatherChunks = store
  | otherwise = case lookupOverlay "weather" store of
      Just _ -> store
      Nothing -> insertOverlay weatherOverlay store
  where
    weatherOverlay = Overlay
      { ovSchema = weatherOverlaySchema
      , ovData = DenseData (IntMap.map weatherChunkToOverlay weatherChunks)
      , ovProvenance = OverlayProvenance
          { opSeed = seed
          , opVersion = 1
          , opSource = "weather"
          , opSchedule = Nothing
          }
      }

weatherLayersForWorld :: TerrainWorld -> [WorldWeatherLayerManifest]
weatherLayersForWorld world = climateLayer ++ overlayLayer "weather" currentBasis simulatedWeather ++ overlayLayer "weather_normals" typicalBasis weatherNormals
  where
    names = overlayNames (twOverlays world)
    climateLayer
      | IntMap.null (twClimate world) = []
      | otherwise = [WorldWeatherLayerManifest "climate" averageBasis generatedClimate "core_topo"]
    overlayLayer name basis source
      | name `elem` names = [WorldWeatherLayerManifest name basis source "overlay_sidecar"]
      | otherwise = []
    averageBasis = "long_run_average"
    currentBasis = "instantaneous_current"
    typicalBasis = "typical_normal"
    generatedClimate = "climate_average"
    simulatedWeather = "weather_snapshot"
    weatherNormals = "weather_normals"

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
        Left _ -> pure Nothing
        Right m
          | isAtomicSaveDirectory entry m -> pure Nothing
          | otherwise -> pure (Just m)

isAtomicSaveDirectory :: FilePath -> WorldSaveManifest -> Bool
isAtomicSaveDirectory entry manifest =
  let committedName = Text.unpack (wsmName manifest)
  in entry == committedName <> ".saving" || entry == committedName <> ".old"

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

-- | Delete only the named world bundle. External provider-owned data sources
-- referenced by its manifest are not opened, mutated, or removed.
deleteNamedWorld :: Text -> IO (Either Text ())
deleteNamedWorld name = case validatePersistenceName name of
  Left err -> pure (Left err)
  Right () -> do
    dir <- worldDir
    let worldPath = dir </> Text.unpack name
    exists <- doesDirectoryExist worldPath
    if not exists
      then pure (Left ("World not found: " <> name))
      else do
        manifestResult <- loadJsonFile (worldPath </> "meta.json")
        case manifestResult of
          Right manifest | wsmName manifest == name -> do
            result <- try @IOException (removeDirectoryRecursive worldPath)
            pure $ case result of
              Left err
                | isDoesNotExistError err -> Left ("World not found: " <> name)
                | otherwise -> Left ("Failed to delete world: " <> Text.pack (show err))
              Right () -> Right ()
          _ -> pure (Left ("World not found: " <> name))

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

data PreparedPluginDataDir = PreparedPluginDataDir
  { ppddPlugin :: !Text
  , ppddSourceDirectory :: !FilePath
  , ppddArchiveDirectory :: !Text
  }

preparePluginDataDirs :: [WorldPluginDataDirectory] -> Either Text [PreparedPluginDataDir]
preparePluginDataDirs dirs = do
  prepared <- traverse prepareOne dirs
  case archiveCollision prepared of
    Just (left, right) -> Left
      ("plugin data archive directories collide: " <> quote left <> " and " <> quote right)
    Nothing -> Right prepared
  where
    prepareOne entry = do
      archiveDir <- normalizeSafeArchiveDirectory (wpddArchiveDirectory entry)
      whenLeft ('\0' `elem` wpddSourceDirectory entry)
        ("plugin data source for " <> quote (wpddPlugin entry)
          <> " must not contain a NUL character")
      whenLeft (not (isAbsolute (wpddSourceDirectory entry)))
        ("plugin data source for " <> quote (wpddPlugin entry)
          <> " must be an absolute host-derived path")
      pure PreparedPluginDataDir
        { ppddPlugin = wpddPlugin entry
        , ppddSourceDirectory = wpddSourceDirectory entry
        , ppddArchiveDirectory = archiveDir
        }

archiveCollision :: [PreparedPluginDataDir] -> Maybe (Text, Text)
archiveCollision [] = Nothing
archiveCollision (entry:rest) =
  case [ (ppddArchiveDirectory entry, ppddArchiveDirectory other)
       | other <- rest
       , archiveDirsCollide (ppddArchiveDirectory entry) (ppddArchiveDirectory other)
       ] of
    collision:_ -> Just collision
    [] -> archiveCollision rest

archiveDirsCollide :: Text -> Text -> Bool
archiveDirsCollide left right =
  let leftKey = archiveCollisionKey left
      rightKey = archiveCollisionKey right
  in leftKey == rightKey
    || (leftKey <> "/") `Text.isPrefixOf` rightKey
    || (rightKey <> "/") `Text.isPrefixOf` leftKey
    || sharedComponentAlias
         (normalizedSegments left)
         (normalizedSegments right)

-- Destination archives must remain unambiguous on case-folding filesystems,
-- even when the current host filesystem is case-sensitive.
archiveCollisionKey :: Text -> Text
archiveCollisionKey = Text.toCaseFold

sharedComponentAlias :: [Text] -> [Text] -> Bool
sharedComponentAlias (left:leftRest) (right:rightRest)
  | Text.toCaseFold left /= Text.toCaseFold right = False
  | left /= right = True
  | otherwise = sharedComponentAlias leftRest rightRest
sharedComponentAlias _ _ = False

normalizeSafeArchiveDirectory :: Text -> Either Text Text
normalizeSafeArchiveDirectory raw
  | Text.null raw = Left "plugin data archive directory must be non-empty"
  | Text.any (== '\0') raw = unsafeArchive
  | Text.isPrefixOf "/" raw || Text.isPrefixOf "\\" raw = unsafeArchive
  | Text.any (== ':') raw = unsafeArchive
  | otherwise = case normalizedSegments raw of
      [] -> unsafeArchive
      segments
        | any unsafeSegment segments -> unsafeArchive
        | otherwise -> Right (Text.intercalate "/" segments)
  where
    unsafeArchive = Left
      ("plugin data archive directory must be a safe relative path: " <> quote raw)

normalizedSegments :: Text -> [Text]
normalizedSegments = Text.splitOn "/" . Text.replace "\\" "/"

unsafeSegment :: Text -> Bool
unsafeSegment segment = Text.null segment || segment == "." || segment == ".."

whenLeft :: Bool -> Text -> Either Text ()
whenLeft True message = Left message
whenLeft False _ = Right ()

quote :: Text -> Text
quote value = "'" <> value <> "'"

-- | Copy a single plugin data directory into the world save.
--
-- Creates @worldPath\/plugins\/\<archiveDir\>\/@ and recursively copies files
-- from the host-derived source directory. Symbolic links are rejected instead
-- of followed so a plugin cannot bundle files outside its data root via link
-- traversal.
copyPluginDataDir
  :: PluginDataCopyHooks
  -> FilePath
  -> PreparedPluginDataDir
  -> IO ()
copyPluginDataDir hooks stagingPath entry =
  copyPluginDataDirectory hooks srcDir stagingPath destinationComponents
  where
    srcDir = ppddSourceDirectory entry
    destinationComponents =
      "plugins" : map Text.unpack (normalizedSegments (ppddArchiveDirectory entry))
