{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Per-'TerrainForm' multipliers that modify erosion, glacier, and
-- hydrology behaviour.
--
-- Each terrain tile is classified (via 'classifyTerrainForm') into one
-- of 15 forms.  Downstream physical stages (erosion, glaciation,
-- hydrology) use these modifiers to scale their per-tile operations so
-- that, e.g., badlands erode faster and mesa cap rock resists erosion.
--
-- The lookup is a pure function @'TerrainForm' -> 'TerrainFormModifiers'@
-- so that callers can materialise a @U.Vector TerrainFormModifiers@ once
-- and index it per tile at negligible cost.
--
-- All multipliers are w.r.t. the baseline (1.0 = no change, 0.0 = fully
-- suppressed for factors, additive fields start at 0.0).
module Topo.TerrainForm.Modifiers
  ( -- * Core type
    TerrainFormModifiers(..)
    -- * Defaults
  , neutralModifiers
  , defaultTerrainFormModifiers
    -- * User-configurable overrides
  , TerrainFormModifiersConfig(..)
  , defaultTerrainFormModifiersConfig
  , configModifierLookup
    -- * Grid materialisation
  , buildModifierGrid
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  ( ToJSON(..), FromJSON(..), configOptions, mergeDefaults
  , genericToJSON, genericParseJSON
  )
import Topo.Types
  ( TerrainForm
  , pattern FormFlat
  , pattern FormRolling
  , pattern FormHilly
  , pattern FormMountainous
  , pattern FormCliff
  , pattern FormValley
  , pattern FormDepression
  , pattern FormRidge
  , pattern FormEscarpment
  , pattern FormPlateau
  , pattern FormBadlands
  , pattern FormPass
  , pattern FormCanyon
  , pattern FormMesa
  , pattern FormFoothill
  )
import qualified Data.Vector.Unboxed as U

-- | Per-form multipliers consumed by erosion, glacier, and hydrology
-- stages.  All fields are strict to avoid thunk build-up in tight
-- per-tile loops.
data TerrainFormModifiers = TerrainFormModifiers
  { -- | Multiplier on erosion intensity.  Values > 1 accelerate
    -- erosion, < 1 suppress it.  Applied to both hydraulic and
    -- thermal erosion rates.
    tfmErosionRate        :: {-# UNPACK #-} !Float
    -- | Additive hardness bonus [0..1].  Added to the tile's base
    -- hardness before erosion resistance is computed.  Useful for
    -- mesa cap rock or canyon wall resistance.
  , tfmHardnessBonus      :: {-# UNPACK #-} !Float
    -- | Deposition suppression factor [0..1].  0 = no suppression
    -- (normal deposition), 1 = fully suppressed (no material settles
    -- here).  Used to prevent sediment accumulation on cliff faces
    -- and canyon walls.
  , tfmDepositSuppression :: {-# UNPACK #-} !Float
    -- | Flow bonus multiplier.  Additive bonus applied to flow
    -- accumulation or diffusion rate.  Positive values amplify flow
    -- through passes, valleys, etc.
  , tfmFlowBonus          :: {-# UNPACK #-} !Float
    -- | Smoothing / blending resistance [0..1].  0 = no resistance
    -- (full smoothing), 1 = complete resistance (tile is never
    -- smoothed).  Used to protect cliff faces and escarpments from
    -- coastal and piedmont smoothing passes.
  , tfmSmoothResistance   :: {-# UNPACK #-} !Float
    -- | Snow accumulation bonus [−1..∞].  Additive multiplier applied
    -- to glacier snow accumulation: @accum * (1 + bonus)@.  Positive
    -- values amplify accumulation (e.g. plateaus gather ice caps),
    -- negative values suppress it.
  , tfmSnowAccumBonus     :: {-# UNPACK #-} !Float
  } deriving (Eq, Show, Generic)

instance ToJSON TerrainFormModifiers where
  toJSON = genericToJSON (configOptions "tfm")

instance FromJSON TerrainFormModifiers where
  parseJSON v = genericParseJSON (configOptions "tfm")
                  (mergeDefaults (toJSON neutralModifiers) v)

-- | Neutral modifiers: no modification to any physical process.
neutralModifiers :: TerrainFormModifiers
neutralModifiers = TerrainFormModifiers
  { tfmErosionRate        = 1.0
  , tfmHardnessBonus      = 0.0
  , tfmDepositSuppression = 0.0
  , tfmFlowBonus          = 0.0
  , tfmSmoothResistance   = 0.0
  , tfmSnowAccumBonus     = 0.0
  }

-- | Default modifier lookup.
--
-- Returns geologically-motivated modifiers for each of the 15 terrain
-- forms.  Unknown / future form codes receive 'neutralModifiers'.
--
-- Rationale per form:
--
-- [@FormBadlands@]  Accelerated erosion (1.5×), flow bonus (+0.25)
--     — soft substrate erodes rapidly and low infiltration produces
--     high runoff, amplifying downstream flow.
--
-- [@FormCliff@]  Reduced erosion (0.5×), hardness bonus (+0.20),
--     heavy deposition suppression (0.80), strong smooth resistance
--     (0.70).  Cliff faces resist hydraulic erosion and material
--     doesn't settle on near-vertical surfaces.
--
-- [@FormCanyon@]  Slightly elevated erosion (1.2×) to deepen incision,
--     hardness bonus (+0.20) to resist widening, very high deposition
--     suppression (0.90) to keep slot canyons clear, strong smooth
--     resistance (0.80).
--
-- [@FormMesa@]  Reduced erosion (0.5×) on cap rock, hardness bonus
--     (+0.30), moderate deposition suppression (0.50) and smooth
--     resistance (0.60).
--
-- [@FormPlateau@]  Mildly reduced erosion (0.7×), small hardness
--     bonus (+0.10), moderate deposition suppression (0.30), smooth
--     resistance (0.40), reduced flow (−0.30) for slow ice-cap spread,
--     and snow accumulation bonus (+0.30) for ice-cap formation.
--
-- [@FormRidge@]  Full erosion rate, small hardness bonus (+0.10),
--     moderate smooth resistance (0.50) to preserve crest lines.
--
-- [@FormEscarpment@]  Full erosion rate, small hardness bonus (+0.10),
--     moderate deposition suppression (0.50) and smooth resistance
--     (0.60) to preserve the asymmetric scarp face.
--
-- [@FormPass@]  Full erosion rate, flow bonus (+0.30) to funnel
--     water and ice through topographic saddles.
--
-- [@FormValley@]  Mildly elevated erosion (1.15×) for valley
--     deepening, flow bonus (+0.15) for alluvial deposition and
--     flow convergence.
--
-- All other forms ('FormFlat', 'FormRolling', 'FormHilly',
-- 'FormMountainous', 'FormDepression', 'FormFoothill') receive
-- 'neutralModifiers'.
defaultTerrainFormModifiers :: TerrainForm -> TerrainFormModifiers
defaultTerrainFormModifiers = \case
  FormBadlands    -> neutralModifiers { tfmErosionRate = 1.5, tfmFlowBonus = 0.25 }
  FormCliff       -> TerrainFormModifiers
    { tfmErosionRate        = 0.5
    , tfmHardnessBonus      = 0.20
    , tfmDepositSuppression = 0.80
    , tfmFlowBonus          = 0.0
    , tfmSmoothResistance   = 0.70
    , tfmSnowAccumBonus     = 0.0
    }
  FormCanyon      -> TerrainFormModifiers
    { tfmErosionRate        = 1.2
    , tfmHardnessBonus      = 0.20
    , tfmDepositSuppression = 0.90
    , tfmFlowBonus          = 0.0
    , tfmSmoothResistance   = 0.80
    , tfmSnowAccumBonus     = 0.0
    }
  FormMesa        -> TerrainFormModifiers
    { tfmErosionRate        = 0.5
    , tfmHardnessBonus      = 0.30
    , tfmDepositSuppression = 0.50
    , tfmFlowBonus          = 0.0
    , tfmSmoothResistance   = 0.60
    , tfmSnowAccumBonus     = 0.0
    }
  FormPlateau     -> TerrainFormModifiers
    { tfmErosionRate        = 0.7
    , tfmHardnessBonus      = 0.10
    , tfmDepositSuppression = 0.30
    , tfmFlowBonus          = -0.30
    , tfmSmoothResistance   = 0.40
    , tfmSnowAccumBonus     = 0.30
    }
  FormRidge       -> TerrainFormModifiers
    { tfmErosionRate        = 1.0
    , tfmHardnessBonus      = 0.10
    , tfmDepositSuppression = 0.0
    , tfmFlowBonus          = 0.0
    , tfmSmoothResistance   = 0.50
    , tfmSnowAccumBonus     = 0.0
    }
  FormEscarpment  -> TerrainFormModifiers
    { tfmErosionRate        = 1.0
    , tfmHardnessBonus      = 0.10
    , tfmDepositSuppression = 0.50
    , tfmFlowBonus          = 0.0
    , tfmSmoothResistance   = 0.60
    , tfmSnowAccumBonus     = 0.0
    }
  FormPass        -> neutralModifiers { tfmFlowBonus = 0.30 }
  FormValley      -> neutralModifiers { tfmErosionRate = 1.15, tfmFlowBonus = 0.15 }
  -- Neutral forms
  FormFlat        -> neutralModifiers
  FormRolling     -> neutralModifiers
  FormHilly       -> neutralModifiers
  FormMountainous -> neutralModifiers
  FormDepression  -> neutralModifiers
  FormFoothill    -> neutralModifiers

-- | Materialise a modifier grid from a terrain-form grid.
--
-- Each tile index maps through the modifier lookup to produce a flat
-- 6-tuple vector that downstream stages can index per tile.
--
-- The result is a boxed vector of 'TerrainFormModifiers' because the
-- type is a product of 5 floats and is consumed element-wise, not in
-- bulk SIMD fashion.  If profiling shows this matters, it can be split
-- into 5 parallel @U.Vector Float@ lanes.
buildModifierGrid
  :: (TerrainForm -> TerrainFormModifiers)
  -> U.Vector TerrainForm
  -> [TerrainFormModifiers]
buildModifierGrid lookupMods forms =
  [ lookupMods (forms U.! i) | i <- [0 .. U.length forms - 1] ]

-- ---------------------------------------------------------------------------
-- User-configurable overrides
-- ---------------------------------------------------------------------------

-- | Per-form modifier overrides.
--
-- Each field corresponds to one of the 15 terrain forms.  The default
-- value for each field is the result of 'defaultTerrainFormModifiers'
-- for that form.  Users can override any subset of forms in JSON:
--
-- @
-- {
--   "badlands": { "erosionRate": 2.0 },
--   "plateau":  { "snowAccumBonus": 0.5 }
-- }
-- @
--
-- Fields not present in the JSON retain their geologically-motivated
-- defaults thanks to 'mergeDefaults'.
data TerrainFormModifiersConfig = TerrainFormModifiersConfig
  { tfmcFlat        :: !TerrainFormModifiers
  , tfmcRolling     :: !TerrainFormModifiers
  , tfmcHilly       :: !TerrainFormModifiers
  , tfmcMountainous :: !TerrainFormModifiers
  , tfmcCliff       :: !TerrainFormModifiers
  , tfmcValley      :: !TerrainFormModifiers
  , tfmcDepression  :: !TerrainFormModifiers
  , tfmcRidge       :: !TerrainFormModifiers
  , tfmcEscarpment  :: !TerrainFormModifiers
  , tfmcPlateau     :: !TerrainFormModifiers
  , tfmcBadlands    :: !TerrainFormModifiers
  , tfmcPass        :: !TerrainFormModifiers
  , tfmcCanyon      :: !TerrainFormModifiers
  , tfmcMesa        :: !TerrainFormModifiers
  , tfmcFoothill    :: !TerrainFormModifiers
  } deriving (Eq, Show, Generic)

instance ToJSON TerrainFormModifiersConfig where
  toJSON = genericToJSON (configOptions "tfmc")

instance FromJSON TerrainFormModifiersConfig where
  parseJSON v = genericParseJSON (configOptions "tfmc")
                  (mergeDefaults (toJSON defaultTerrainFormModifiersConfig) v)

-- | Default config: every form gets its 'defaultTerrainFormModifiers'.
defaultTerrainFormModifiersConfig :: TerrainFormModifiersConfig
defaultTerrainFormModifiersConfig = TerrainFormModifiersConfig
  { tfmcFlat        = defaultTerrainFormModifiers FormFlat
  , tfmcRolling     = defaultTerrainFormModifiers FormRolling
  , tfmcHilly       = defaultTerrainFormModifiers FormHilly
  , tfmcMountainous = defaultTerrainFormModifiers FormMountainous
  , tfmcCliff       = defaultTerrainFormModifiers FormCliff
  , tfmcValley      = defaultTerrainFormModifiers FormValley
  , tfmcDepression  = defaultTerrainFormModifiers FormDepression
  , tfmcRidge       = defaultTerrainFormModifiers FormRidge
  , tfmcEscarpment  = defaultTerrainFormModifiers FormEscarpment
  , tfmcPlateau     = defaultTerrainFormModifiers FormPlateau
  , tfmcBadlands    = defaultTerrainFormModifiers FormBadlands
  , tfmcPass        = defaultTerrainFormModifiers FormPass
  , tfmcCanyon      = defaultTerrainFormModifiers FormCanyon
  , tfmcMesa        = defaultTerrainFormModifiers FormMesa
  , tfmcFoothill    = defaultTerrainFormModifiers FormFoothill
  }

-- | Build a modifier lookup function from a user config.
--
-- Replaces 'defaultTerrainFormModifiers' when user overrides are
-- present.  Unknown form codes fall back to 'neutralModifiers'.
--
-- >>> let cfg = defaultTerrainFormModifiersConfig
-- >>> configModifierLookup cfg FormBadlands == defaultTerrainFormModifiers FormBadlands
-- True
configModifierLookup :: TerrainFormModifiersConfig -> TerrainForm -> TerrainFormModifiers
configModifierLookup cfg = \case
  FormFlat        -> tfmcFlat cfg
  FormRolling     -> tfmcRolling cfg
  FormHilly       -> tfmcHilly cfg
  FormMountainous -> tfmcMountainous cfg
  FormCliff       -> tfmcCliff cfg
  FormValley      -> tfmcValley cfg
  FormDepression  -> tfmcDepression cfg
  FormRidge       -> tfmcRidge cfg
  FormEscarpment  -> tfmcEscarpment cfg
  FormPlateau     -> tfmcPlateau cfg
  FormBadlands    -> tfmcBadlands cfg
  FormPass        -> tfmcPass cfg
  FormCanyon      -> tfmcCanyon cfg
  FormMesa        -> tfmcMesa cfg
  FormFoothill    -> tfmcFoothill cfg