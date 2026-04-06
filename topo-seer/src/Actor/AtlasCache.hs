-- | Atlas cache key type shared across the atlas pipeline.
--
-- Day\/night brightness and water-level colouring are baked into atlas
-- geometry at build time because they vary per-hex (spatially) and SDL2's
-- software renderer only supports per-texture colour modulation, not
-- per-pixel.  Moving these to render-time would require a shader-based
-- backend (OpenGL\/Vulkan).
--
-- With the multi-key 'Seer.Render.Atlas.AtlasTextureCache', switching
-- any component of the key is an O(1) pointer update — tiles for the
-- previous key remain cached and are available instantly if the user
-- switches back.
module Actor.AtlasCache
  ( AtlasKey(..)
  ) where

import Actor.UI (ViewMode(..))
import Data.Word (Word64)

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- Stores the terrain version stamp rather than the full snapshot, avoiding
-- deep structural equality on every frame.
data AtlasKey = AtlasKey !ViewMode !Float !Bool !Word64
  deriving (Eq, Ord, Show)
