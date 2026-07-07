{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared atlas result payloads.
module Actor.AtlasResult
  ( AtlasBuildId(..)
  , AtlasBuildTarget(..)
  , AtlasTileSetManifest(..)
  , AtlasDayNightTile(..)
  , AtlasBuildResult(..)
  , atlasManifestTarget
  ) where

import Actor.AtlasCache (AtlasKey)
import Actor.SnapshotReceiver (SnapshotVersion)
import Data.Word (Word64)
import UI.DayNight (DayNightKey)
import UI.TerrainAtlas (AtlasTileGeometry)
import UI.Widgets (Rect)

-- | Monotonic identity assigned when the atlas manager accepts a target build.
--
-- The build id distinguishes same-key/same-snapshot viewport refreshes for a
-- single target scale.  Freshness checks reject obsolete ids before GPU upload;
-- the render cache also refuses to merge tiles from different build ids.
newtype AtlasBuildId = AtlasBuildId { unAtlasBuildId :: Word64 }
  deriving (Eq, Ord, Show)

-- | Immutable target identity for one atlas tile set.
data AtlasBuildTarget = AtlasBuildTarget
  { abtKey :: !AtlasKey
  , abtSnapshotVersion :: !SnapshotVersion
  , abtHexRadius :: !Int
  , abtAtlasScale :: !Int
  } deriving (Eq, Ord, Show)

-- | Manifest shared by every tile result produced for one target build.
--
-- Bounds are stored in the same normalised coordinate frame as
-- 'TerrainAtlasTile.tatBounds', so cache completeness can compare the uploaded
-- texture bounds directly against the manifest.
data AtlasTileSetManifest = AtlasTileSetManifest
  { atsmBuildId :: !AtlasBuildId
  , atsmKey :: !AtlasKey
  , atsmSnapshotVersion :: !SnapshotVersion
  , atsmHexRadius :: !Int
  , atsmAtlasScale :: !Int
  , atsmExpectedTileCount :: !Int
  , atsmExpectedBounds :: ![Rect]
  } deriving (Eq, Show)

atlasManifestTarget :: AtlasTileSetManifest -> AtlasBuildTarget
atlasManifestTarget manifest = AtlasBuildTarget
  { abtKey = atsmKey manifest
  , abtSnapshotVersion = atsmSnapshotVersion manifest
  , abtHexRadius = atsmHexRadius manifest
  , abtAtlasScale = atsmAtlasScale manifest
  }

-- | Day/night overlay geometry paired with the freshness identity of the
-- brightness function that produced it.
data AtlasDayNightTile = AtlasDayNightTile
  { adntKey :: !DayNightKey
  , adntTile :: !AtlasTileGeometry
  }

-- | Result payload from CPU atlas build work.
data AtlasBuildResult = AtlasBuildResult
  { abrKey       :: !AtlasKey
  , abrSnapshotVersion :: !SnapshotVersion
  , abrHexRadius :: !Int
  , abrManifest :: !AtlasTileSetManifest
  , abrTileIndex :: !Int
  , abrTileBounds :: !Rect
  , abrTile      :: !AtlasTileGeometry
  , abrDayNightTile :: !(Maybe AtlasDayNightTile)
    -- ^ Day\/night overlay tile geometry and its freshness key, if the
    -- build included a brightness function.  Rendered as a separate
    -- black+alpha overlay on top of the base tile.
  }
