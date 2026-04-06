{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared atlas result payloads.
module Actor.AtlasResult
  ( AtlasBuildResult(..)
  ) where

import Actor.AtlasCache (AtlasKey)
import UI.TerrainAtlas (AtlasTileGeometry)

-- | Result payload from CPU atlas build work.
data AtlasBuildResult = AtlasBuildResult
  { abrKey       :: !AtlasKey
  , abrHexRadius :: !Int
  , abrTile      :: !AtlasTileGeometry
  , abrDayNightTile :: !(Maybe AtlasTileGeometry)
    -- ^ Day\/night overlay tile geometry, if the build included a
    -- brightness function.  Rendered as a separate black+alpha
    -- overlay on top of the base tile.
  }
