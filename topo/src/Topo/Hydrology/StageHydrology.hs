-- | Grid-level computation for the hydrology pipeline stage.
module Topo.Hydrology.StageHydrology
  ( computeHydrologyGrids
  ) where

import Topo.Hydrology.Config (HydroConfig(..))
import qualified Topo.Hydrology.Depression as Depression
import qualified Topo.Hydrology.Flow as Flow
import qualified Topo.Hydrology.FlowRouting as FlowRouting
import qualified Topo.Hydrology.RiverCarve as RiverCarve
import qualified Topo.Hydrology.TerrainModify as TerrainModify
import Topo.Parameters (TerrainFormConfig)
import Topo.TerrainForm.Modifiers
  ( defaultTerrainFormModifiers
  , tfmDepositSuppression
  , tfmErosionRate
  , tfmFlowBonus
  , tfmSmoothResistance
  )
import Topo.TerrainGrid
  ( classifyTerrainFormGrid
  )
import qualified Data.Vector.Unboxed as U

-- | Compute final elevation and moisture grids for the hydrology stage.
computeHydrologyGrids
  :: HydroConfig
  -> TerrainFormConfig
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> (U.Vector Float, U.Vector Float)
computeHydrologyGrids cfg formCfg gridW gridH elev0 hardness plateHardness =
  let wl = hcWaterLevel cfg
      formGrid = classifyTerrainFormGrid formCfg wl gridW gridH elev0 plateHardness
      modLookup = defaultTerrainFormModifiers
      erosionMult = U.map (tfmErosionRate . modLookup) formGrid
      depositFactor = U.map (\f -> 1 - tfmDepositSuppression (modLookup f)) formGrid
      smoothResist = U.map (tfmSmoothResistance . modLookup) formGrid
      flowBonus = U.map (tfmFlowBonus . modLookup) formGrid
      elev1 = Depression.fillDepressions wl gridW gridH elev0
      flow = FlowRouting.flowDirections gridW gridH elev1
      acc = Flow.flowAccumulation (hcBaseAccumulation cfg) flowBonus elev1 flow
      elevCarved = RiverCarve.carveRiversGrid
        (hcMinAccumulation cfg)
        (hcWaterLevel cfg)
        (hcRiverCarveMaxDepth cfg)
        (hcRiverCarveScale cfg)
        (hcHardnessErodeWeight cfg)
        elev0
        acc
        hardness
        erosionMult
      elevBanks = RiverCarve.riverBankErodeGrid
        gridW
        gridH
        (hcMinAccumulation cfg)
        (hcRiverBankThreshold cfg)
        (hcRiverBankDepth cfg)
        (hcHardnessErodeWeight cfg)
        elevCarved
        acc
        hardness
        erosionMult
      elev2 = TerrainModify.applyStreamPowerErosion
        (hcStreamPowerMaxErosion cfg)
        (hcStreamPowerScale cfg)
        (hcHardnessErodeWeight cfg)
        (hcStreamDepositRatio cfg)
        elevBanks
        flow
        acc
        hardness
        erosionMult
        depositFactor
      elev3 = TerrainModify.coastalErodeGrid
        (hcWaterLevel cfg)
        (hcCoastalErodeStrength cfg)
        (hcCoastalRaiseFactor cfg)
        (hcHardnessErodeWeight cfg)
        gridW
        gridH
        elev2
        hardness
        erosionMult
        smoothResist
      elev3b = TerrainModify.piedmontSmoothGrid
        (hcPiedmontSlopeMin cfg)
        (hcPiedmontSlopeMax cfg)
        (hcPiedmontSmoothStrength cfg)
        (hcWaterLevel cfg)
        gridW
        gridH
        elev3
        formGrid
        smoothResist
      elev4 = TerrainModify.alluvialDepositGrid
        (hcMinAccumulation cfg)
        (hcAlluvialMaxSlope cfg)
        (hcWaterLevel cfg)
        gridW
        gridH
        elev3b
        acc
        (U.map (* hcAlluvialDepositScale cfg) depositFactor)
      moisture = Flow.moistureFromAccumulation
        (hcMinAccumulation cfg)
        (hcWaterLevel cfg)
        (hcMoistureBaseWeight cfg)
        (hcMoistureFlowWeight cfg)
        elev4
        acc
      elev5 = TerrainModify.wetErodeGrid
        (hcMinMoisture cfg)
        (hcWaterLevel cfg)
        (hcWetErodeScale cfg)
        (hcHardnessErodeWeight cfg)
        elev4
        moisture
        hardness
        erosionMult
      elevFinal = U.imap
        (\i h ->
          if elev0 U.! i > wl && h <= wl
            then wl + 1e-5
            else h)
        elev5
  in (elevFinal, moisture)
