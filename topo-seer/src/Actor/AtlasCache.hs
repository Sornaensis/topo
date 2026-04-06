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
  ) where

import Actor.UI (ViewMode(..))
import Data.Word (Word64)

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- Stores the terrain version stamp rather than the full snapshot, avoiding
-- deep structural equality on every frame.
--
-- Fields: @ViewMode@, @waterLevel@, @terrainVersion@.
data AtlasKey = AtlasKey !ViewMode !Float !Word64
  deriving (Eq, Ord, Show)

-- | Extract the terrain version from an 'AtlasKey'.
atlasKeyVersion :: AtlasKey -> Word64
atlasKeyVersion (AtlasKey _ _ v) = v
