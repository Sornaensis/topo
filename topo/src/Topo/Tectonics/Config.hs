{-# LANGUAGE DeriveGeneric #-}

-- | Configuration for plate tectonics terrain synthesis.
module Topo.Tectonics.Config
  ( TectonicsConfig(..)
  , defaultTectonicsConfig
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  ( FromJSON(..)
  , ToJSON(..)
  , configOptions
  , genericParseJSON
  , genericToJSON
  , mergeDefaults
  )

-- | Configuration parameters for plate tectonics.
--
-- Controls plate generation, boundary shaping, and the interplay between
-- plate motion and terrain elevation.
data TectonicsConfig = TectonicsConfig
  { tcPlateSize :: !Int
    -- ^ Target number of tiles per tectonic plate.
  , tcPlateCount :: !(Maybe Int)
    -- ^ Override plate count. 'Nothing' auto-derives from world size
    -- and 'tcPlateSize'.
  , tcPlateSpeed :: !Float
    -- ^ Default plate motion speed (dimensionless, default 0.6).
  , tcBoundarySharpness :: !Float
    -- ^ Boundary zone transition sharpness; higher = narrower boundary.
  , tcBoundaryNoiseScale :: !Float
    -- ^ Frequency of boundary warp noise (cycles per tile).
  , tcBoundaryNoiseStrength :: !Float
    -- ^ Displacement strength of boundary warp noise (tiles).
  , tcBoundaryWarpOctaves :: !Int
    -- ^ FBM octaves for boundary warp noise.
  , tcBoundaryWarpLacunarity :: !Float
    -- ^ Frequency multiplier between boundary warp FBM octaves.
  , tcBoundaryWarpGain :: !Float
    -- ^ Amplitude decay between boundary warp FBM octaves.
  , tcPlateMergeScale :: !Float
    -- ^ Noise frequency for plate merge decisions (cycles per tile).
  , tcPlateMergeBias :: !Float
    -- ^ Threshold bias for plate merging [0..1]; higher = less merging.
  , tcPlateDetailScale :: !Float
    -- ^ Frequency of intra-plate detail noise (cycles per tile).
  , tcPlateDetailStrength :: !Float
    -- ^ Amplitude of intra-plate detail noise.
  , tcPlateRidgeStrength :: !Float
    -- ^ Amplitude of mid-plate ridge features.
  , tcPlateBiasStrength :: !Float
    -- ^ Global elevation bias strength applied via center/edge/N/S fields.
  , tcPlateBiasCenter :: !Float
    -- ^ Elevation bias at the map center (signed).
  , tcPlateBiasEdge :: !Float
    -- ^ Elevation bias at the map edges (signed).
  , tcPlateBiasNorth :: !Float
    -- ^ Elevation bias at the north edge (signed).
  , tcPlateBiasSouth :: !Float
    -- ^ Elevation bias at the south edge (signed).
  , tcCenterFalloffScale :: !Float
    -- ^ Strength of radial center-distance falloff [0..1].
  , tcBoundaryFadeScale :: !Float
    -- ^ Control for fading boundary effects at plate edges.
  , tcCenterWeightScale :: !Float
    -- ^ Weight multiplier for center-distance contributions.
  , tcEdgeWeightScale :: !Float
    -- ^ Weight multiplier for edge-distance contributions.
  , tcPlateHeightBase :: !Float
    -- ^ Base elevation assigned to each plate [0..1].
  , tcPlateHeightVariance :: !Float
    -- ^ Random variance range for per-plate elevation.
  , tcCrustContinentalBias :: !Float
    -- ^ Elevation bias for continental crust (positive raises land).
  , tcCrustOceanicBias :: !Float
    -- ^ Elevation bias for oceanic crust (negative deepens ocean).
  , tcPlateHardnessBase :: !Float
    -- ^ Base hardness assigned to each plate [0..1].
  , tcPlateHardnessVariance :: !Float
    -- ^ Random variance range for per-plate hardness.
  , tcUplift :: !Float
    -- ^ Uplift magnitude at convergent plate boundaries.
  , tcRiftDepth :: !Float
    -- ^ Depth magnitude at divergent (rift) plate boundaries.
  , tcRiftNoiseOctaves :: !Int
    -- ^ FBM octaves for rift noise (default 3).
  , tcRiftNoiseLacunarity :: !Float
    -- ^ Frequency multiplier between rift noise FBM octaves (default 2.0).
  , tcRiftNoiseGain :: !Float
    -- ^ Amplitude decay between rift noise FBM octaves (default 0.5).
  , tcRiftNoiseScale :: !Float
    -- ^ Base frequency of rift noise (cycles per tile, default 0.006).
  , tcRiftElongation :: !Float
    -- ^ Scale ratio between tangent and normal axes (default 4.0).
    -- Values >1 stretch rift features along the boundary.
  , tcRiftShoulderHeight :: !Float
    -- ^ Height of flanking horst uplift (default 0.03).
  , tcRiftShoulderWidth :: !Float
    -- ^ Fraction of boundary width where shoulders appear (default 0.35).
  , tcRiftFloorWidth :: !Float
    -- ^ Fraction of boundary width that is flat-bottomed (default 0.25).
  , tcTrenchDepth :: !Float
    -- ^ Depth magnitude at subduction-zone trenches.
  , tcRidgeHeight :: !Float
    -- ^ Height magnitude at mid-ocean ridges.
  } deriving (Eq, Show, Generic)

instance ToJSON TectonicsConfig where
  toJSON = genericToJSON (configOptions "tc")

instance FromJSON TectonicsConfig where
  parseJSON v = genericParseJSON (configOptions "tc")
                  (mergeDefaults (toJSON defaultTectonicsConfig) v)

-- | Default tectonics configuration.
defaultTectonicsConfig :: TectonicsConfig
defaultTectonicsConfig = TectonicsConfig
  { tcPlateSize = 64
  , tcPlateCount = Nothing
  , tcPlateSpeed = 0.6
  , tcBoundarySharpness = 1.2
  , tcBoundaryNoiseScale = 0.008
  , tcBoundaryNoiseStrength = 12
  , tcBoundaryWarpOctaves = 3
  , tcBoundaryWarpLacunarity = 2
  , tcBoundaryWarpGain = 0.5
  , tcPlateMergeScale = 0.11
  , tcPlateMergeBias = 0.52
  , tcPlateDetailScale = 0.02
  , tcPlateDetailStrength = 0.20
  , tcPlateRidgeStrength = 0.15
  , tcPlateBiasStrength = 0.25
  , tcPlateBiasCenter = 0
  , tcPlateBiasEdge = 0
  , tcPlateBiasNorth = 0
  , tcPlateBiasSouth = 0
  , tcCenterFalloffScale = 0.8
  , tcBoundaryFadeScale = 1.1
  , tcCenterWeightScale = 1.2
  , tcEdgeWeightScale = 1.2
  , tcPlateHeightBase = 0.12
  , tcPlateHeightVariance = 0.35
  , tcCrustContinentalBias = 0.18
  , tcCrustOceanicBias = -0.18
  , tcPlateHardnessBase = 0.45
  , tcPlateHardnessVariance = 0.3
  , tcUplift = 0.15
  , tcRiftDepth = 0.08
  , tcRiftNoiseOctaves = 3
  , tcRiftNoiseLacunarity = 2.0
  , tcRiftNoiseGain = 0.5
  , tcRiftNoiseScale = 0.006
  , tcRiftElongation = 4.0
  , tcRiftShoulderHeight = 0.03
  , tcRiftShoulderWidth = 0.35
  , tcRiftFloorWidth = 0.25
  , tcTrenchDepth = 0.25
  , tcRidgeHeight = 0.08
  }