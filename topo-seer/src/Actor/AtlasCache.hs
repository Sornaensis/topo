-- | Atlas cache key type shared across the atlas pipeline.
--
-- Base atlas tiles are day\/night-agnostic: the brightness overlay is
-- stored and drawn as a separate layer (see 'Seer.Render.Atlas').
-- Removing the @Bool@ from the key means that switching day\/night
-- does not invalidate view-mode tiles, effectively doubling cache
-- capacity.
--
-- With the multi-key 'Seer.Render.Atlas.AtlasTextureCache', switching
-- any component of the key is an O(1) pointer update — tiles for the
-- previous key remain cached and are available instantly if the user
-- switches back.
module Actor.AtlasCache
  ( AtlasKey(..)
  , atlasKeyVersion
  , terrainSnapshotViewVersion
  , atlasKeyFor
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import Data.Word (Word64)

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- Stores the view-specific data version stamp rather than the full snapshot,
-- avoiding deep structural equality on every frame.
--
-- Fields: @ViewMode@, @waterLevel@, @viewDataVersion@.
data AtlasKey = AtlasKey !ViewMode !Float !Word64
  deriving (Eq, Ord, Show)

-- | Extract the view-specific data version from an 'AtlasKey'.
atlasKeyVersion :: AtlasKey -> Word64
atlasKeyVersion (AtlasKey _ _ v) = v

-- | Choose the version stamp relevant to a view mode.
--
-- Layer-specific views depend on the base terrain chunk layout plus their own
-- data layer.  All layer stamps come from one monotonic counter, so 'max'
-- changes when either dependency changes while overlay/weather ticks leave
-- elevation/biome keys stable.
terrainSnapshotViewVersion :: ViewMode -> TerrainSnapshot -> Word64
terrainSnapshotViewVersion mode terrainSnap = case mode of
  ViewClimate    -> max (tsVersion terrainSnap) (tsClimateVersion terrainSnap)
  ViewPrecip     -> max (tsVersion terrainSnap) (tsClimateVersion terrainSnap)
  ViewWeather    -> max (tsVersion terrainSnap) (tsWeatherVersion terrainSnap)
  ViewCloud      -> max (tsVersion terrainSnap) (tsWeatherVersion terrainSnap)
  ViewVegetation -> max (tsVersion terrainSnap) (tsVegetationVersion terrainSnap)
  ViewOverlay{}  -> max (tsVersion terrainSnap) (tsOverlayVersion terrainSnap)
  _              -> tsVersion terrainSnap

atlasKeyFor :: ViewMode -> Float -> TerrainSnapshot -> AtlasKey
atlasKeyFor mode waterLevel terrainSnap =
  AtlasKey mode waterLevel (terrainSnapshotViewVersion mode terrainSnap)
