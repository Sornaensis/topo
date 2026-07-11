{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Cycle-free types for world persistence.
--
-- This module contains 'WorldSaveManifest' and its JSON instances
-- without depending on 'Actor.UI', allowing both 'Actor.UI' and
-- 'Seer.World.Persist' to import it freely.
module Seer.World.Persist.Types
  ( WorldPluginDataDirectory(..)
  , WorldExternalDataSourceSnapshot(..)
  , WorldWeatherLayerManifest(..)
  , WorldSaveManifest(..)
  , defaultManifestTime
  , manifestJsonOptions
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , genericToJSON
  , object
  , withObject
  , (.:?)
  , (.=)
  , (.!=)
  )
import Data.Aeson.Types (Options(..))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceDecl
  , RPCExternalDataSourceRef
  )

-- | Runtime description of a plugin data directory to bundle with a world save.
--
-- The source directory is host-derived (the plugin's launched
-- @TOPO_PLUGIN_DATA_ROOT@), while the archive directory is a validated relative
-- destination recorded in 'WorldSaveManifest'.  The source path is never read
-- from the plugin handshake.
data WorldPluginDataDirectory = WorldPluginDataDirectory
  { wpddPlugin :: Text
    -- ^ Plugin whose data is being bundled.
  , wpddSourceDirectory :: FilePath
    -- ^ Host-derived absolute source directory to copy from.
  , wpddArchiveDirectory :: Text
    -- ^ Safe relative destination below the world save @plugins/@ directory.
  } deriving (Eq, Show)

-- | Opaque external data-source declarations and references captured with a
-- world save.  Topo preserves this metadata for compatibility checks and
-- diagnostics without interpreting provider-owned handles, config references,
-- backends, locks, or writer policies.
data WorldExternalDataSourceSnapshot = WorldExternalDataSourceSnapshot
  { wedssPlugin :: Text
    -- ^ Plugin whose manifest declared the provider sources or consumer refs.
  , wedssProvidedSources :: [RPCExternalDataSourceDecl]
    -- ^ Provider-owned external source declarations from the plugin manifest.
  , wedssConsumedRefs :: [RPCExternalDataSourceRef]
    -- ^ Consumer references from the plugin manifest.
  } deriving (Eq, Show, Generic)

instance ToJSON WorldExternalDataSourceSnapshot where
  toJSON snapshot = object
    [ "plugin" .= wedssPlugin snapshot
    , "provided_sources" .= wedssProvidedSources snapshot
    , "consumed_refs" .= wedssConsumedRefs snapshot
    ]

instance FromJSON WorldExternalDataSourceSnapshot where
  parseJSON = withObject "WorldExternalDataSourceSnapshot" $ \o ->
    WorldExternalDataSourceSnapshot
      <$> o .:? "plugin" .!= ""
      <*> o .:? "provided_sources" .!= []
      <*> o .:? "consumed_refs" .!= []

-- | Durable climate/weather data-layer semantics recorded in @meta.json@.
--
-- The manifest describes where persisted weather-oriented values came from and
-- how clients should interpret their temporal basis.  It is additive metadata;
-- the actual data remains in the core @.topo@ climate chunks or overlay
-- sidecar payloads named in 'wsmOverlayNames'.
data WorldWeatherLayerManifest = WorldWeatherLayerManifest
  { wwlmName :: Text
    -- ^ Data layer name, e.g. @climate@, @weather@, or @weather_normals@.
  , wwlmBasis :: Text
    -- ^ Temporal basis such as @long_run_average@ or @instantaneous_current@.
  , wwlmSourceKind :: Text
    -- ^ Source kind such as @climate_average@, @weather_snapshot@, or @weather_normals@.
  , wwlmStorage :: Text
    -- ^ Storage location: @core_topo@ or @overlay_sidecar@.
  } deriving (Eq, Show, Generic)

instance ToJSON WorldWeatherLayerManifest where
  toJSON layer = object
    [ "name" .= wwlmName layer
    , "basis" .= wwlmBasis layer
    , "source_kind" .= wwlmSourceKind layer
    , "storage" .= wwlmStorage layer
    ]

instance FromJSON WorldWeatherLayerManifest where
  parseJSON = withObject "WorldWeatherLayerManifest" $ \o -> do
    name <- o .:? "name" .!= ""
    basis <- o .:? "basis" .!= ""
    sourceKind <- o .:? "source_kind" .!= ""
    storage <- o .:? "storage" .!= ""
    pure WorldWeatherLayerManifest
      { wwlmName = name
      , wwlmBasis = basis
      , wwlmSourceKind = normalizeWeatherLayerSourceKind name basis sourceKind
      , wwlmStorage = storage
      }

-- | Read old manifests into the current semantic labels without changing the
-- stored overlay data or layer names.
normalizeWeatherLayerSourceKind :: Text -> Text -> Text -> Text
normalizeWeatherLayerSourceKind name basis sourceKind =
  case sourceKind of
    "generated_climate"
      | name == "weather_normals" || basis == "typical_normal" -> "weather_normals"
      | otherwise -> "climate_average"
    "simulated_generated_weather" -> "weather_snapshot"
    "simulated_weather" -> "weather_snapshot"
    _ -> sourceKind

-- | Metadata recorded alongside each saved world.
data WorldSaveManifest = WorldSaveManifest
  { wsmName       :: Text
    -- ^ Human-readable save name.
  , wsmSeed       :: Word64
    -- ^ Seed used for world generation/simulation.
  , wsmChunkSize  :: Int
    -- ^ Chunk width/height in tiles.
  , wsmCreatedAt  :: UTCTime
    -- ^ Save timestamp.
  , wsmChunkCount :: Int
    -- ^ Terrain chunk count at save time.
  , wsmOverlayNames :: [Text]
    -- ^ Overlay names persisted in the unified world bundle.
  , wsmWeatherLayers :: [WorldWeatherLayerManifest]
    -- ^ Climate/weather data-layer semantics for core climate chunks and
    -- weather-oriented overlays.  Forward-compatible clients can use this to
    -- distinguish generated long-run averages, generated typical normals, and
    -- simulated current weather.
  , wsmPluginData :: [(Text, Text)]
    -- ^ @(pluginName, relativeDataDir)@ pairs for plugins whose data
    -- directories were bundled with this world save.
  , wsmExternalDataSources :: [WorldExternalDataSourceSnapshot]
    -- ^ Backend-neutral external data-source declarations/references,
    -- opaque config references, and opaque metadata preserved with the save.
    -- Loading reports this metadata
    -- but does not reconnect, migrate, lock, or repair provider-owned stores.
  } deriving (Eq, Show, Generic)

-- | JSON field name options: strip @\"wsm\"@ prefix, camelCase to
-- snake_case.
manifestJsonOptions :: Options
manifestJsonOptions = Aeson.defaultOptions
  { fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
  }

instance ToJSON WorldSaveManifest where
  toJSON = genericToJSON manifestJsonOptions

-- | Forward-compatible parser — every field defaults sensibly so that
-- manifests saved by newer versions can still be loaded.
instance FromJSON WorldSaveManifest where
  parseJSON = withObject "WorldSaveManifest" $ \o ->
    WorldSaveManifest
      <$> o .:? "name"        .!= ""
      <*> o .:? "seed"        .!= 0
      <*> o .:? "chunk_size"  .!= 64
      <*> o .:? "created_at"  .!= defaultManifestTime
      <*> o .:? "chunk_count" .!= 0
      <*> o .:? "overlay_names" .!= []
      <*> o .:? "weather_layers" .!= []
      <*> o .:? "plugin_data"   .!= []
      <*> o .:? "external_data_sources" .!= []

-- | Epoch time used as a default when the field is missing.
defaultManifestTime :: UTCTime
defaultManifestTime = read "2025-01-01 00:00:00 UTC"
