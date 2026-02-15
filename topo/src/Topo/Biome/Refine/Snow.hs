{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Snow / Ice sub-biome refinement.
--
-- Discriminates glacier, ice cap, snowfield, and marginal snow from the
-- family-level 'BiomeSnow'.  Temperature bands drive the primary split;
-- slope discriminates persistent snow (flat) from alpine scree (steep).
module Topo.Biome.Refine.Snow
  ( SnowConfig(..)
  , defaultSnowConfig
  , refineSnow
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeSnow,
                   pattern BiomeGlacier, pattern BiomeIceCap,
                   pattern BiomeSnowfield, pattern BiomeAlpine,
                   pattern BiomeAlpineScree)

-- | Configuration for snow sub-biome classification.
--
-- Temperature bands (coldest → warmest):
--
--   * T ≤ 'snIceCapMaxTemp'        → Ice cap (permanent ice)
--   * T ≤ 'snGlacierMaxTemp' + ice → Glacier
--   * T ≤ 'snGlacierMaxTemp' + snow → Snowfield
--   * T ≤ 'snMarginalMinTemp'      → persistent Snow (or scree on steep slopes)
--   * T ≤ 'snWarmEscapeTemp'       → marginal snow — demoted to Alpine
--   * T > 'snWarmEscapeTemp'       → warm escape → Alpine
data SnowConfig = SnowConfig
  { snIceCapMaxTemp        :: !Float
  -- ^ Maximum temperature for ice cap classification.  Default: @0.05@.
  , snGlacierMinIceThick   :: !Float
  -- ^ Minimum ice thickness (from 'GlacierChunk') for glacier.
  --   Default: @0.10@.
  , snGlacierMaxTemp       :: !Float
  -- ^ Maximum temperature for glacier / snowfield formation.
  --   Above this temperature, ice and snowpack are not thick enough to
  --   qualify as glacier or snowfield.  Default: @0.15@.
  , snSnowfieldMinSnowpack :: !Float
  -- ^ Minimum snowpack for snowfield classification.  Default: @0.50@.
  , snMarginalMinTemp      :: !Float
  -- ^ Lower bound of the marginal snow band.  Tiles between this and
  --   'snWarmEscapeTemp' are marginal (seasonal) and are demoted to
  --   Alpine.  Below this temperature, snow is persistent.
  --   Default: @0.15@.
  , snSteepSlopeThreshold  :: !Float
  -- ^ Minimum slope for steep-snow → scree reclassification.  Steep
  --   cold snow tiles become 'BiomeAlpineScree' instead of 'BiomeSnow'
  --   (snow doesn't accumulate on very steep slopes).  Default: @0.30@.
  , snWarmEscapeTemp       :: !Float
  -- ^ Maximum temperature for Snow to survive refinement.  Snow tiles
  --   warmer than this are demoted to 'BiomeAlpine' — a safety net for
  --   smoothing-induced warm Snow tiles.  Default: @0.25@.
  } deriving (Eq, Show, Generic)

instance ToJSON SnowConfig where
  toJSON = genericToJSON (configOptions "sn")

instance FromJSON SnowConfig where
  parseJSON v = genericParseJSON (configOptions "sn")
                  (mergeDefaults (toJSON defaultSnowConfig) v)

-- | Sensible defaults for snow refinement.
defaultSnowConfig :: SnowConfig
defaultSnowConfig = SnowConfig
  { snIceCapMaxTemp        = 0.05
  , snGlacierMinIceThick   = 0.10
  , snGlacierMaxTemp       = 0.15
  , snSnowfieldMinSnowpack = 0.50
  , snMarginalMinTemp      = 0.15
  , snSteepSlopeThreshold  = 0.30
  , snWarmEscapeTemp       = 0.25
  }

-- | Refine a snow tile into a sub-biome.
--
-- Decision cascade (temperature-band order):
--
-- 1. Warm escape: T > 'snWarmEscapeTemp' → Alpine
--    (safety net for smoothing-induced warm Snow tiles)
-- 2. Marginal snow: T ∈ ['snMarginalMinTemp', 'snWarmEscapeTemp'] → Alpine
--    (seasonal snow too warm to persist; effectively the same as warm escape)
-- 3. Ice cap: T ≤ 'snIceCapMaxTemp' → permanent ice
-- 4. Glacier: T ≤ 'snGlacierMaxTemp' + thick ice → Glacier
-- 5. Snowfield: T ≤ 'snGlacierMaxTemp' + heavy snowpack → Snowfield
-- 6. Steep slope: slope ≥ 'snSteepSlopeThreshold' → Alpine Scree
--    (snow can't persist on very steep terrain)
-- 7. Fallback: 'BiomeSnow'
refineSnow
  :: SnowConfig
  -> Float          -- ^ temperature
  -> Float          -- ^ ice thickness (from 'GlacierChunk')
  -> Float          -- ^ snowpack (from 'GlacierChunk')
  -> Float          -- ^ slope
  -> BiomeId
refineSnow cfg temp iceThickness snowpack slope
  -- 1. Warm escape
  | temp > snWarmEscapeTemp cfg
  = BiomeAlpine
  -- 2. Marginal snow band → demote to Alpine
  | temp >= snMarginalMinTemp cfg
  = BiomeAlpine
  -- 3. Ice cap: extreme cold (permanent ice)
  | temp <= snIceCapMaxTemp cfg
  = BiomeIceCap
  -- 4. Glacier: cold enough + thick ice
  | temp <= snGlacierMaxTemp cfg && iceThickness >= snGlacierMinIceThick cfg
  = BiomeGlacier
  -- 5. Snowfield: cold enough + heavy snowpack
  | temp <= snGlacierMaxTemp cfg && snowpack >= snSnowfieldMinSnowpack cfg
  = BiomeSnowfield
  -- 6. Steep slope → alpine scree (snow slides off)
  | slope >= snSteepSlopeThreshold cfg
  = BiomeAlpineScree
  -- 7. Persistent snow
  | otherwise
  = BiomeSnow
