{-# LANGUAGE DeriveGeneric #-}

-- | Configuration types for hydrology, river routing, and groundwater.
module Topo.Hydrology.Config
  ( HydroConfig(..)
  , defaultHydroConfig
  , RiverConfig(..)
  , defaultRiverConfig
  , GroundwaterConfig(..)
  , defaultGroundwaterConfig
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  ( ToJSON(..)
  , FromJSON(..)
  , configOptions
  , genericParseJSON
  , genericToJSON
  , mergeDefaults
  )

-- | Hydrology configuration.
--
-- Controls flow routing, erosion, moisture derivation, and sediment
-- transport across the terrain grid.
data HydroConfig = HydroConfig
  { hcWaterLevel :: !Float
    -- ^ Global water level [0..1 normalised elevation].
    -- Tiles at or below this elevation are considered submerged.
  , hcSinkBreachDepth :: !Float
    -- ^ Maximum depth to breach interior sinks during depression
    -- filling (elevation units).
  , hcBaseAccumulation :: !Float
    -- ^ Starting flow accumulation per tile.
  , hcMinAccumulation :: !Float
    -- ^ Floor for flow accumulation (prevents zero-accumulation tiles).
  , hcStreamPowerMaxErosion :: !Float
    -- ^ Maximum stream power erosion per step (elevation units).
  , hcStreamPowerScale :: !Float
    -- ^ Scaling factor for stream power calculation.
  , hcStreamDepositRatio :: !Float
    -- ^ Fraction of eroded material deposited downstream [0..1].
  , hcRiverCarveMaxDepth :: !Float
    -- ^ Maximum river channel carving depth (elevation units).
  , hcRiverCarveScale :: !Float
    -- ^ Scale factor for river carving intensity.
  , hcRiverBankThreshold :: !Float
    -- ^ Flow accumulation threshold for bank erosion to begin.
  , hcRiverBankDepth :: !Float
    -- ^ Bank erosion depth (elevation units).
  , hcAlluvialMaxSlope :: !Float
    -- ^ Maximum slope for alluvial (flat-water) deposits.
  , hcAlluvialDepositScale :: !Float
    -- ^ Alluvial deposit rate (elevation units per step).
  , hcWetErodeScale :: !Float
    -- ^ Wet-area diffuse erosion scale.
  , hcCoastalErodeStrength :: !Float
    -- ^ Coastal erosion strength (elevation units per step).
  , hcCoastalRaiseFactor :: !Float
    -- ^ Fraction of coastal erosion redeposited as coastal raise [0..1].
  -- | [0..1] scaling of hardness against erosion intensity.
  , hcHardnessErodeWeight :: !Float
  , hcMoistureBaseWeight :: !Float
    -- ^ Base moisture weight in the moisture blending formula [0..1].
  , hcMoistureFlowWeight :: !Float
    -- ^ Flow-based moisture weight in the moisture blend [0..1].
  , hcPiedmontSmoothStrength :: !Float
    -- ^ Blend factor toward neighbor mean in the piedmont zone [0..1].
    -- Higher values produce wider, smoother foothills aprons.
  , hcPiedmontSlopeMin :: !Float
    -- ^ Minimum slope for the piedmont zone (normalised elevation units).
    -- Tiles with slope below this are flat plains and are not smoothed.
  , hcPiedmontSlopeMax :: !Float
    -- ^ Maximum slope for the piedmont zone (normalised elevation units).
    -- Tiles steeper than this are mountains and are not smoothed.
  , hcMinMoisture :: !Float
    -- ^ Minimum moisture floor for all tiles.
  } deriving (Eq, Show, Generic)

instance ToJSON HydroConfig where
  toJSON = genericToJSON (configOptions "hc")

instance FromJSON HydroConfig where
  parseJSON v =
    genericParseJSON (configOptions "hc")
      (mergeDefaults (toJSON defaultHydroConfig) v)

-- | Default hydrology configuration.
defaultHydroConfig :: HydroConfig
defaultHydroConfig = HydroConfig
  { hcWaterLevel = 0.5
  , hcSinkBreachDepth = 0.02
  , hcBaseAccumulation = 1
  , hcMinAccumulation = 1
  , hcStreamPowerMaxErosion = 0.08
  , hcStreamPowerScale = 0.00005
  , hcStreamDepositRatio = 0.4
  , hcRiverCarveMaxDepth = 0.05
  , hcRiverCarveScale = 0.03
  , hcRiverBankThreshold = 0.35
  , hcRiverBankDepth = 0.01
  , hcAlluvialMaxSlope = 0.12
  , hcAlluvialDepositScale = 0.06
  , hcWetErodeScale = 0.015
  , hcCoastalErodeStrength = 0.04
  , hcCoastalRaiseFactor = 0.6
  , hcHardnessErodeWeight = 0.7
  , hcMoistureBaseWeight = 0.6
  , hcMoistureFlowWeight = 0.7
  , hcPiedmontSmoothStrength = 0.25
  , hcPiedmontSlopeMin = 0.03
  , hcPiedmontSlopeMax = 0.12
  , hcMinMoisture = 1
  }

-- | River routing configuration derived from moisture + flow accumulation.
data RiverConfig = RiverConfig
  { rcBaseAccumulation :: !Float
  , rcMinAccumulation :: !Float
  , rcOrderMinAccumulation :: !Float
  , rcDischargeScale :: !Float
  , rcChannelDepthScale :: !Float
  , rcChannelMaxDepth :: !Float
  -- | [0..1] scaling of hardness against channel depth.
  , rcHardnessDepthWeight :: !Float
  -- | [0..1] scaling of hardness against erosion potential.
  , rcHardnessErosionWeight :: !Float
  -- | Scale factor for erosion potential.
  , rcErosionScale :: !Float
  -- | Scale factor for deposition potential.
  , rcDepositScale :: !Float
  -- | Slope threshold for alluvial deposition.
  , rcDepositMaxSlope :: !Float
  , rcBaseflowScale :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON RiverConfig where
  toJSON = genericToJSON (configOptions "rc")

instance FromJSON RiverConfig where
  parseJSON v =
    genericParseJSON (configOptions "rc")
      (mergeDefaults (toJSON defaultRiverConfig) v)

-- | Default river routing parameters.
defaultRiverConfig :: RiverConfig
defaultRiverConfig = RiverConfig
  { rcBaseAccumulation = 1
  , rcMinAccumulation = 4
  , rcOrderMinAccumulation = 6
  , rcDischargeScale = 0.05
  , rcChannelDepthScale = 0.002
  , rcChannelMaxDepth = 0.2
  , rcHardnessDepthWeight = 0.6
  , rcHardnessErosionWeight = 0.5
  , rcErosionScale = 1
  , rcDepositScale = 0.6
  , rcDepositMaxSlope = 0.1
  , rcBaseflowScale = 1
  }

-- | Groundwater storage and baseflow configuration.
data GroundwaterConfig = GroundwaterConfig
  { gwRechargeScale :: !Float
    -- ^ Groundwater recharge rate from surface moisture [0..1].
  , gwStorageScale :: !Float
    -- ^ Groundwater storage capacity scaling.
  , gwDischargeScale :: !Float
    -- ^ Groundwater discharge rate back to surface flow.
  , gwPermeability :: !Float
    -- ^ Soil permeability factor affecting recharge [0..1].
  , gwMinBasinSize :: !Int
    -- ^ Minimum basin size (tiles) for groundwater calculation.
  } deriving (Eq, Show, Generic)

instance ToJSON GroundwaterConfig where
  toJSON = genericToJSON (configOptions "gw")

instance FromJSON GroundwaterConfig where
  parseJSON v =
    genericParseJSON (configOptions "gw")
      (mergeDefaults (toJSON defaultGroundwaterConfig) v)

-- | Default groundwater parameters.
defaultGroundwaterConfig :: GroundwaterConfig
defaultGroundwaterConfig = GroundwaterConfig
  { gwRechargeScale = 0.2
  , gwStorageScale = 0.5
  , gwDischargeScale = 0.1
  , gwPermeability = 0.6
  , gwMinBasinSize = 1
  }
