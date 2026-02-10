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
  { abrKey :: !AtlasKey
  , abrScale :: !Int
  , abrTile :: !AtlasTileGeometry
  }
