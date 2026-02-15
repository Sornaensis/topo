{-# LANGUAGE DeriveGeneric #-}

-- | Base height sampling configuration and helpers.
module Topo.BaseHeight
  ( GenConfig(..)
  , OceanEdgeDepth(..)
  , defaultGenConfig
  , defaultOceanEdgeDepth
  , sampleBaseHeightAt
  , oceanEdgeBiasAt
  ) where

import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Config.JSON (ToJSON(..), FromJSON(..), configOptions, mergeDefaults, genericToJSON, genericParseJSON, withObject, (.:?), (.!=))
import Topo.Math (clamp01, smoothstep)
import Topo.Noise (domainWarp2D, fbm2D, noise2DContinuous, ridgedFbm2D)
import Topo.Types (TileCoord(..), WorldConfig(..), WorldExtent, defaultWorldExtent, worldExtentRadii)

-- | Configuration for base height sampling.
--
-- Controls the noise-based terrain generator that produces the initial
-- elevation field before tectonics, erosion, and hydrology stages.
data GenConfig = GenConfig
  { gcScale :: !Float
    -- ^ Global height multiplier applied to the final sampled value.
  , gcCoordScale :: !Float
    -- ^ Coordinate scaling factor applied to world-space positions
    -- before noise evaluation.
  , gcOffsetX :: !Float
    -- ^ X offset added to world-space coordinates before sampling.
    -- /Derived/: set by the pipeline from the master seed; default 0.
  , gcOffsetY :: !Float
    -- ^ Y offset added to world-space coordinates before sampling.
    -- /Derived/: set by the pipeline from the master seed; default 0.
  , gcFrequency :: !Float
    -- ^ Base frequency for the FBM noise (cycles per tile).
  , gcOctaves :: !Int
    -- ^ Number of FBM octaves for terrain detail.
  , gcLacunarity :: !Float
    -- ^ Frequency multiplier between successive FBM octaves.
  , gcGain :: !Float
    -- ^ Amplitude decay between successive FBM octaves.
  , gcWarpScale :: !Float
    -- ^ Frequency of the domain-warp noise (cycles per tile).
  , gcWarpStrength :: !Float
    -- ^ Displacement strength of the domain warp (in tiles).
  , gcWorldExtent :: !WorldExtent
    -- ^ Chunk radius extents for the generated world.
    -- /Derived/: auto-computed from 'PlanetConfig' and 'WorldSlice'
    -- by the pipeline unless manually overridden.
  , gcContinentScale :: !Float
    -- ^ Frequency of the continent-level mask noise (cycles per tile).
  , gcLandRatio :: !Float
    -- ^ Target fraction of land vs ocean [0..1].
  , gcShelfWidth :: !Float
    -- ^ Width of the continental shelf transition zone [0..1].
  , gcCoastSharpness :: !Float
    -- ^ Exponent controlling coast edge sharpness; higher = sharper.
  , gcOceanEdgeDepth :: !OceanEdgeDepth
    -- ^ Per-edge ocean depth biases at the world boundary.
    -- /Derived/: auto-computed by 'autoOceanEdgeDepth' when all
    -- fields are zero.
  } deriving (Eq, Show, Generic)

-- | Ocean depth bias near world edges.
--
-- Forces ocean depth at map boundaries so that slices that do not span
-- the full planet produce plausible ocean margins.  All values default
-- to zero (disabled); the pipeline auto-derives them via
-- 'autoOceanEdgeDepth'.
data OceanEdgeDepth = OceanEdgeDepth
  { oedNorth :: !Float
    -- ^ Forced ocean depth at the north edge [0..1]; 0 = no bias.
  , oedSouth :: !Float
    -- ^ Forced ocean depth at the south edge [0..1]; 0 = no bias.
  , oedEast :: !Float
    -- ^ Forced ocean depth at the east edge [0..1]; 0 = no bias.
  , oedWest :: !Float
    -- ^ Forced ocean depth at the west edge [0..1]; 0 = no bias.
  , oedFalloff :: !Float
    -- ^ Falloff distance in tiles; 0 = disabled.
  } deriving (Eq, Show, Generic)

instance ToJSON OceanEdgeDepth where
  toJSON = genericToJSON (configOptions "oed")

instance FromJSON OceanEdgeDepth where
  parseJSON v = genericParseJSON (configOptions "oed")
                  (mergeDefaults (toJSON defaultOceanEdgeDepth) v)

instance ToJSON GenConfig where
  toJSON = genericToJSON (configOptions "gc")

instance FromJSON GenConfig where
  parseJSON v = genericParseJSON (configOptions "gc")
                  (mergeDefaults (toJSON defaultGenConfig) v)

-- | Default base height configuration.
defaultGenConfig :: GenConfig
defaultGenConfig = GenConfig
  { gcScale = 1
  , gcCoordScale = 1
  , gcOffsetX = 0
  , gcOffsetY = 0
  , gcFrequency = 0.01
  , gcOctaves = 5
  , gcLacunarity = 2
  , gcGain = 0.5
  , gcWarpScale = 0.03
  , gcWarpStrength = 12
  , gcWorldExtent = defaultWorldExtent
  , gcContinentScale = 0.0008
  , gcLandRatio = 0.45
  , gcShelfWidth = 0.08
  , gcCoastSharpness = 1.4
  , gcOceanEdgeDepth = defaultOceanEdgeDepth
  }

-- | Default edge-depth bias configuration (disabled).
defaultOceanEdgeDepth :: OceanEdgeDepth
defaultOceanEdgeDepth = OceanEdgeDepth
  { oedNorth = 0
  , oedSouth = 0
  , oedEast = 0
  , oedWest = 0
  , oedFalloff = 0
  }

-- | Sample base terrain height at world coordinates.
sampleBaseHeightAt :: Word64 -> GenConfig -> Float -> Float -> Float
sampleBaseHeightAt seed cfg gx gy =
  let sx = (gx + gcOffsetX cfg) * gcCoordScale cfg
      sy = (gy + gcOffsetY cfg) * gcCoordScale cfg
      xf = sx * gcFrequency cfg
      yf = sy * gcFrequency cfg
      cont = noise2DContinuous (seed + 999) (sx * gcContinentScale cfg) (sy * gcContinentScale cfg)
      landThreshold = 1 - clamp01 (gcLandRatio cfg)
      shelf = max 0 (gcShelfWidth cfg)
      edge0 = landThreshold - shelf
      edge1 = landThreshold + shelf
      cont' = smoothstep edge0 edge1 cont
      contMask = clamp01 (cont' ** max 0.1 (gcCoastSharpness cfg))
      (wx, wy) = domainWarp2D seed (gcWarpScale cfg) (gcWarpStrength cfg) xf yf
      h0 = fbm2D seed (gcOctaves cfg) (gcLacunarity cfg) (gcGain cfg) wx wy
      ridge = ridgedFbm2D (seed + 4242) (max 1 (gcOctaves cfg - 2)) (gcLacunarity cfg) (gcGain cfg * 0.8) (wx * 0.7) (wy * 0.7)
      mask = 0.15 + contMask * 1.1
      h1 = h0 * 0.75 + ridge * 0.6
  in h1 * gcScale cfg * mask

-- | Compute an edge-depth bias for an absolute tile coordinate.
--   The falloff is expressed in tiles; zero disables the bias.
oceanEdgeBiasAt :: WorldConfig -> WorldExtent -> OceanEdgeDepth -> TileCoord -> Float
oceanEdgeBiasAt config extent edgeCfg (TileCoord gx gy) =
  let (minX, maxX, minY, maxY) = worldTileBounds config extent
      falloff = max 0 (oedFalloff edgeCfg)
      edgeMask dist
        | falloff <= 0 = 0
        | otherwise = smoothstep 0 1 (clamp01 (1 - dist / falloff))
      distN = max 0 (fromIntegral (gy - minY))
      distS = max 0 (fromIntegral (maxY - gy))
      distW = max 0 (fromIntegral (gx - minX))
      distE = max 0 (fromIntegral (maxX - gx))
      biasN = (-oedNorth edgeCfg) * edgeMask distN
      biasS = (-oedSouth edgeCfg) * edgeMask distS
      biasW = (-oedWest edgeCfg) * edgeMask distW
      biasE = (-oedEast edgeCfg) * edgeMask distE
  in biasN + biasS + biasW + biasE

worldTileBounds :: WorldConfig -> WorldExtent -> (Int, Int, Int, Int)
worldTileBounds config extent =
  let size = wcChunkSize config
      (rx, ry) = worldExtentRadii extent
      minX = -rx * size
      maxX = rx * size + (size - 1)
      minY = -ry * size
      maxY = ry * size + (size - 1)
  in (minX, maxX, minY, maxY)
