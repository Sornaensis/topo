{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Core types for config snapshots.
--
-- A 'ConfigSnapshot' bundles a 'WorldGenConfig' (real domain-value
-- generation config from the @topo@ library) together with the handful
-- of UI-only parameters that are not part of the generation pipeline
-- (seed, chunk size, render water level). View selection, including layered
-- base/sky overlays and average/current weather basis, is intentionally
-- UI-only and is not persisted here; after load, UI state starts from the
-- normal empty/default layered view selection.
--
-- This module is kept free of 'Actor.UI' imports to avoid module
-- cycles.  Conversion functions live in "Seer.Config.Snapshot".
module Seer.Config.Snapshot.Types
  ( ConfigSnapshot(..)
  , currentSnapshotVersion
  , defaultSnapshot
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:?)
  , (.!=)
  , (.=)
  , object
  , withObject
  )
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Config.JSON (mergeDefaults)
import Topo.WorldGen (WorldGenConfig, defaultWorldGenConfig)

-- ---------------------------------------------------------------------------
-- Version tag
-- ---------------------------------------------------------------------------

-- | Monotonically increasing version tag for forward compatibility.
-- Bump this when the set of fields changes.
currentSnapshotVersion :: Int
currentSnapshotVersion = 2

-- ---------------------------------------------------------------------------
-- ConfigSnapshot data type
-- ---------------------------------------------------------------------------

-- | A snapshot of all generation-relevant configuration.
--
-- Stores real domain values in a 'WorldGenConfig'.  The UI↔domain
-- mapping is performed by @Seer.Config.applyUiConfig@ (forward) and
-- @Seer.Config.Snapshot.applySnapshotToUi@ (inverse).
--
-- Extra fields not present in 'WorldGenConfig':
--
-- * 'csName' — human-readable label (from filename or dialog).
-- * 'csSeed' — RNG seed for deterministic regeneration.
-- * 'csChunkSize' — tiles per chunk side (geometry, not generation).
-- * 'csRenderWaterLevel' — display-only water level for the renderer.
data ConfigSnapshot = ConfigSnapshot
  { -- | Human-readable snapshot name.
    csName              :: !Text
    -- | Schema version for forward compatibility.
  , csVersion           :: !Int
    -- | RNG seed.
  , csSeed              :: !Word64
    -- | Chunk size (8–256).
  , csChunkSize         :: !Int
    -- | Display-only water level for the renderer.
  , csRenderWaterLevel  :: !Float
    -- | The full generation config with real domain values.
  , csGenConfig         :: !WorldGenConfig
  } deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

-- | JSON layout (version 2):
--
-- @
-- { "name": "...",
--   "version": 2,
--   "seed": 42,
--   "chunkSize": 64,
--   "renderWaterLevel": 0.43,
--   "genConfig": { ... WorldGenConfig fields ... }
-- }
-- @
--
-- All fields are optional — missing keys fall back to 'defaultSnapshot'.
instance ToJSON ConfigSnapshot where
  toJSON cs = object
    [ "name"             .= csName cs
    , "version"          .= csVersion cs
    , "seed"             .= csSeed cs
    , "chunkSize"        .= csChunkSize cs
    , "renderWaterLevel" .= csRenderWaterLevel cs
    , "genConfig"        .= csGenConfig cs
    ]

instance FromJSON ConfigSnapshot where
  parseJSON = withObject "ConfigSnapshot" $ \o -> do
    let d = defaultSnapshot
    name <- o .:? "name" .!= csName d
    version <- o .:? "version" .!= csVersion d
    seed <- o .:? "seed" .!= csSeed d
    chunkSize <- o .:? "chunkSize" .!= csChunkSize d
    renderWaterLevel <- o .:? "renderWaterLevel" .!= csRenderWaterLevel d
    genConfig <- o .:? "genConfig" .!= csGenConfig d
    pure ConfigSnapshot
      { csName = name
      , csVersion = migrateSnapshotVersion version
      , csSeed = seed
      , csChunkSize = chunkSize
      , csRenderWaterLevel = renderWaterLevel
      , csGenConfig = genConfig
      }

-- | Loaded snapshots use the current schema version after compatibility
-- defaults and ignored legacy keys have been applied. Future versions retain
-- their tag so callers can still identify data produced by a newer release.
migrateSnapshotVersion :: Int -> Int
migrateSnapshotVersion version
  | version < currentSnapshotVersion = currentSnapshotVersion
  | otherwise = version

-- ---------------------------------------------------------------------------
-- Default
-- ---------------------------------------------------------------------------

-- | Default snapshot with 'defaultWorldGenConfig' and sensible UI defaults.
defaultSnapshot :: ConfigSnapshot
defaultSnapshot = ConfigSnapshot
  { csName             = "default"
  , csVersion          = currentSnapshotVersion
  , csSeed             = 42
  , csChunkSize        = 64
  , csRenderWaterLevel = 0.43
  , csGenConfig        = defaultWorldGenConfig
  }
