{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hypsometric remapping of terrain elevation.
--
-- After plate tectonics and erosion produce raw elevation values, this
-- stage redistributes elevation probability density toward low values,
-- mimicking Earth's actual land-elevation distribution where ~70 % of
-- land sits below 500 m.
--
-- The remap is __strictly monotonic__ by construction, preserving flow
-- direction and drainage topology.  Gradient magnitudes change (gentler
-- lowlands, comparable-or-steeper highlands), which is the intended
-- effect.
--
-- Pipeline position: after 'applyErosionStage', before
-- 'applyVolcanismStage'.
module Topo.Hypsometry
  ( HypsometryConfig(..)
  , defaultHypsometryConfig
  , hypsometricRemap
  , applyHypsometryStage
  ) where

import Control.Monad.Except (throwError)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import Topo.Config.JSON
  ( ToJSON(..), FromJSON(..), configOptions, mergeDefaults
  , genericToJSON, genericParseJSON
  )
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Types (WorldConfig(..), ChunkCoord(..))
import Topo.World (TerrainWorld(..))

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Parameters controlling the hypsometric elevation curve.
--
-- The remap uses a piecewise power-curve approach: land below the
-- 'hpPlateauBreak' is compressed aggressively (producing lowlands and
-- coastal plains), while land above it retains more of its original
-- relief.  An additional coastal-ramp zone provides extra
-- flattening right above sea level.
--
-- Ocean depths are optionally shaped with a separate exponent.
--
-- All exponents must be > 0.  Exponents > 1 compress elevation toward
-- sea level (producing more lowlands); exponents < 1 expand it
-- (producing more highlands).  An exponent of 1.0 is the identity.
data HypsometryConfig = HypsometryConfig
  { hpEnabled :: !Bool
    -- ^ Master toggle.  When 'False', the stage is a no-op.
  , hpLowlandExponent :: !Float
    -- ^ Power exponent applied to the [waterLevel .. plateauBreak] band.
    -- Values > 1 compress elevation toward sea level (more lowlands);
    -- 1.0 is the identity; values < 1 expand (more highlands).
    -- Default: 1.4.
  , hpHighlandExponent :: !Float
    -- ^ Power exponent applied above 'hpPlateauBreak'.
    -- Values > 1 compress highland elevation; < 1 expands.
    -- Default: 1.2.
  , hpPlateauBreak :: !Float
    -- ^ Normalized elevation where the curve transitions from lowland
    -- to highland shaping.  Default: 0.6 (≈1 200 m).
  , hpOceanExponent :: !Float
    -- ^ Power exponent for ocean depths (below water level).
    -- Default: 0.6.
  , hpWaterLevel :: !Float
    -- ^ Mirror of the hydro water level; used as the fixed point for
    -- the remap.  Default: 0.5.
  , hpCoastalRampWidth :: !Float
    -- ^ Normalized elevation band above sea level where extra
    -- flattening occurs.  Default: 0.04 (≈480 m).
  , hpCoastalRampStrength :: !Float
    -- ^ How aggressively the coastal ramp flattens.  0 = no extra
    -- effect; 1 = perfectly flat coast.  Default: 0.6.
  } deriving (Eq, Show, Generic)

-- | Serialise with @hp@ prefix stripped from field names.
instance ToJSON HypsometryConfig where
  toJSON = genericToJSON (configOptions "hp")

-- | Deserialise with defaults for any missing field.
instance FromJSON HypsometryConfig where
  parseJSON v = genericParseJSON (configOptions "hp")
                  (mergeDefaults (toJSON defaultHypsometryConfig) v)

-- | Sensible defaults that produce Earth-like hypsometry.
defaultHypsometryConfig :: HypsometryConfig
defaultHypsometryConfig = HypsometryConfig
  { hpEnabled             = True
  , hpLowlandExponent     = 1.4
  , hpHighlandExponent    = 1.2
  , hpPlateauBreak        = 0.6
  , hpOceanExponent       = 0.6
  , hpWaterLevel          = 0.5
  , hpCoastalRampWidth    = 0.08
  , hpCoastalRampStrength = 0.5
    -- ^ Reduced from 0.8 to limit compounding with erosion's coastal
    -- smooth, which together were pushing a coastal band below water
    -- level and creating "rivers to nowhere".
  }

-------------------------------------------------------------------------------
-- Remap function
-------------------------------------------------------------------------------

-- | Remap a single normalized elevation value through the hypsometric curve.
--
-- __Fixed points:__
--
--  * @hypsometricRemap cfg 0 = 0@
--  * @hypsometricRemap cfg 1 = 1@
--  * @hypsometricRemap cfg (hpWaterLevel cfg) = hpWaterLevel cfg@
--
-- __Monotonicity:__ for all @a <= b@, @hypsometricRemap cfg a <= hypsometricRemap cfg b@,
-- provided all exponents are > 0.
--
-- When 'hpEnabled' is 'False', returns the input unchanged (identity).
hypsometricRemap :: HypsometryConfig -> Float -> Float
hypsometricRemap cfg e
  | not (hpEnabled cfg) = e
  | e <= 0   = 0
  | e >= 1   = 1
  | e <= wl  = remapOcean e
  | otherwise = remapLand e
  where
    !wl            = hpWaterLevel cfg
    !plateauBreak  = max (wl + 0.001) (hpPlateauBreak cfg)
    !lowExp        = max 0.01 (hpLowlandExponent cfg)
    !highExp       = max 0.01 (hpHighlandExponent cfg)
    !oceanExp      = max 0.01 (hpOceanExponent cfg)
    !rampWidth     = max 0.001 (hpCoastalRampWidth cfg)
    !rampStr       = clamp01 (hpCoastalRampStrength cfg)

    -- Ocean: wl is the fixed point, 0 maps to 0.
    -- Normalize depth to [0,1], apply power, un-normalize.
    remapOcean :: Float -> Float
    remapOcean x =
      let !t = (wl - x) / wl           -- 0 at sea level, 1 at e=0
          !t' = t ** oceanExp
      in wl - t' * wl

    -- Land: piecewise curve with coastal ramp overlay.
    remapLand :: Float -> Float
    remapLand x =
      let -- Normalize land elevation to [0,1]:
          -- t=0 at sea level, t=1 at e=1.
          !landRange = 1.0 - wl
          !t = (x - wl) / landRange

          -- Plateau break in normalized land-space
          !pb = (plateauBreak - wl) / landRange

          -- Piecewise power curve
          !base
            | t <= pb =
                -- Lowland band: compress into [0, pb'] where
                -- pb' = pb^lowExp (the remapped plateau break)
                let !tNorm = t / pb
                    !pb'   = pb ** lowExp
                in (tNorm ** lowExp) * pb'
            | otherwise =
                -- Highland band: [pb,1] maps to [pb', 1]
                let !pb'   = pb ** lowExp
                    !tNorm = (t - pb) / (1.0 - pb)
                    -- highland remap: from pb' to 1
                in pb' + (tNorm ** highExp) * (1.0 - pb')

          -- Coastal ramp: extra flattening near sea level.
          -- rampZone covers t in [0, rampWidth/landRange].
          !rampT = min 1.0 (t * landRange / rampWidth)
          -- fade = 1 at sea level, 0 at top of ramp zone
          !fade = (1.0 - rampT) * (1.0 - rampT)   -- quadratic fade
          -- Pull the base value toward the sea-level reference (0)
          -- by rampStr * fade.
          !ramped = base * (1.0 - rampStr * fade)

      in wl + ramped * landRange

-------------------------------------------------------------------------------
-- Pipeline stage
-------------------------------------------------------------------------------

-- | Pipeline stage that applies hypsometric remapping to all tiles.
--
-- Runs across the full stitched terrain grid (cross-chunk).  When
-- 'hpEnabled' is 'False', the stage returns immediately.
applyHypsometryStage :: HypsometryConfig -> PipelineStage
applyHypsometryStage cfg = PipelineStage "applyHypsometry" "applyHypsometry" $ do
  if not (hpEnabled cfg)
    then logInfo "applyHypsometry: disabled, skipping"
    else do
      logInfo "applyHypsometry: remapping elevation distribution"
      world <- getWorldP
      let config  = twConfig world
          terrain = twTerrain world
      (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
        case validateTerrainGrid config terrain of
          Left err -> throwError (PluginInvariantError err)
          Right bounds -> pure bounds
      let size  = wcChunkSize config
          gridW = (maxCx - minCx + 1) * size
          gridH = (maxCy - minCy + 1) * size
          elev  = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
          remap = hypsometricRemap cfg
          elev' = U.map (clamp01 . remap) elev
          terrain' = IntMap.mapWithKey
            (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elev')
            terrain
      putWorldP world { twTerrain = terrain' }
