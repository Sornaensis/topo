{-# LANGUAGE PatternSynonyms #-}

-- | Volcanic overlay pass.
--
-- Unlike other refiners, this pass can override /any/ primary biome
-- when volcanic activity is high enough.  It runs /last/ in the
-- refinement chain.
module Topo.Biome.Refine.Volcanic
  ( VolcanicConfig(..)
  , defaultVolcanicConfig
  , refineVolcanic
  ) where

import Topo.Types (BiomeId, pattern BiomeLavaField,
                   pattern BiomeVolcanicAshPlain)

-- | Configuration for the volcanic overlay pass.
data VolcanicConfig = VolcanicConfig
  { voLavaMinPotential :: !Float  -- ^ default 0.60
  , voAshMinPotential  :: !Float  -- ^ default 0.40
  , voAshMaxFertility  :: !Float  -- ^ default 0.30 (still barren)
  } deriving (Eq, Show)

-- | Sensible defaults for volcanic overlay.
defaultVolcanicConfig :: VolcanicConfig
defaultVolcanicConfig = VolcanicConfig
  { voLavaMinPotential = 0.60
  , voAshMinPotential  = 0.40
  , voAshMaxFertility  = 0.30
  }

-- | Apply volcanic overlay to a single tile's biome.
--
-- Decision cascade:
--
-- 1. Lava field: high lava potential
-- 2. Volcanic ash plain: high ash potential + still barren
-- 3. No override: keep existing biome
refineVolcanic
  :: VolcanicConfig
  -> Float       -- ^ lava potential (0 if no volcanism data)
  -> Float       -- ^ ash potential  (0 if no volcanism data)
  -> Float       -- ^ fertility
  -> BiomeId     -- ^ current biome (pre-overlay)
  -> BiomeId     -- ^ biome after overlay
refineVolcanic cfg lavaPot ashPot fertility bid
  | lavaPot >= voLavaMinPotential cfg            = BiomeLavaField
  | ashPot >= voAshMinPotential cfg
    && fertility <= voAshMaxFertility cfg         = BiomeVolcanicAshPlain
  | otherwise                                    = bid
