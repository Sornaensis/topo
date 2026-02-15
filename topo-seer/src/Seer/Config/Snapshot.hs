{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Config snapshot persistence and conversion for topo-seer.
--
-- A 'ConfigSnapshot' stores real domain-value generation parameters
-- (via 'WorldGenConfig') rather than normalised slider positions.
-- This module provides bidirectional conversion between UI slider
-- state and 'ConfigSnapshot', plus file I/O.
module Seer.Config.Snapshot
  ( -- * Re-exports from Types
    ConfigSnapshot(..)
  , currentSnapshotVersion
  , defaultSnapshot
    -- * Conversion
  , snapshotFromUi
  , applySnapshotToUi
    -- * File I/O
  , snapshotDir
  , saveSnapshot
  , loadSnapshot
  , listSnapshots
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.List (sort)
import Data.Text (Text)
import System.Directory
  ( createDirectoryIfMissing
  , getHomeDirectory
  , listDirectory
  , renameFile
  , removeFile
  , doesFileExist
  )
import System.FilePath ((</>), takeExtension, dropExtension)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import Actor.UI
  ( Ui
  , UiState(..)
  , setUiSeed
  , setUiChunkSize
  , setUiWaterLevel
  , setUiRenderWaterLevel
  , setUiEvaporation
  , setUiRainShadow
  , setUiWindDiffuse
  , setUiRainRate
  , setUiErosionHydraulic
  , setUiErosionThermal
  , setUiErosionTalus
  , setUiErosionMaxDrop
  , setUiGlacierSnowTemp
  , setUiGlacierSnowRange
  , setUiGlacierMeltTemp
  , setUiGlacierMeltRate
  , setUiGlacierAccumScale
  , setUiGlacierFlowIters
  , setUiGlacierFlowRate
  , setUiGlacierErosionScale
  , setUiGlacierCarveScale
  , setUiGlacierDepositScale
  , setUiVentDensity
  , setUiVentThreshold
  , setUiHotspotScale
  , setUiHotspotThreshold
  , setUiMagmaRecharge
  , setUiLavaScale
  , setUiAshScale
  , setUiVolcanicDepositScale
  , setUiSoilMoistureThreshold
  , setUiSoilHardnessThreshold
  , setUiSoilFertilityMoistWeight
  , setUiSoilFertilityDepthWeight
  , setUiSinkBreachDepth
  , setUiStreamPowerMaxErosion
  , setUiRiverCarveMaxDepth
  , setUiCoastalErodeStrength
  , setUiHydroHardnessWeight
  , setUiMinLakeSize
  , setUiInlandSeaMinSize
  , setUiRoughnessScale
  , setUiEquatorTemp
  , setUiPoleTemp
  , setUiLapseRate
  , setUiGenScale
  , setUiGenCoordScale
  , setUiGenOffsetX
  , setUiGenOffsetY
  , setUiGenFrequency
  , setUiGenOctaves
  , setUiGenLacunarity
  , setUiGenGain
  , setUiGenWarpScale
  , setUiGenWarpStrength
  , setUiWorldExtentX
  , setUiWorldExtentY
  , setUiEdgeDepthNorth
  , setUiEdgeDepthSouth
  , setUiEdgeDepthEast
  , setUiEdgeDepthWest
  , setUiEdgeDepthFalloff
  , setUiPlateSize
  , setUiPlateSpeed
  , setUiBoundarySharpness
  , setUiBoundaryNoiseScale
  , setUiBoundaryNoiseStrength
  , setUiBoundaryWarpOctaves
  , setUiBoundaryWarpLacunarity
  , setUiBoundaryWarpGain
  , setUiPlateMergeScale
  , setUiPlateMergeBias
  , setUiPlateDetailScale
  , setUiPlateDetailStrength
  , setUiPlateRidgeStrength
  , setUiPlateHeightBase
  , setUiPlateHeightVariance
  , setUiPlateHardnessBase
  , setUiPlateHardnessVariance
  , setUiUplift
  , setUiRiftDepth
  , setUiTrenchDepth
  , setUiRidgeHeight
  , setUiDetailScale
  , setUiPlateBiasStrength
  , setUiPlateBiasCenter
  , setUiPlateBiasEdge
  , setUiPlateBiasNorth
  , setUiPlateBiasSouth
  , setUiTfcCliffSlope
  , setUiTfcMountainSlope
  , setUiTfcMountainRelief
  , setUiTfcHillSlope
  , setUiTfcRollingSlope
  , setUiValleyCurvature
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiWindIterations
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiLatitudeExponent
  , setUiPlateHeightCooling
  , setUiTempNoiseScale
  , setUiOceanModeration
  , setUiOceanModerateTemp
  , setUiAlbedoSensitivity
  , setUiAlbedoReference
  , setUiMoistAdvect
  , setUiMoistLocal
  , setUiMoistWindEvapScale
  , setUiMoistEvapNoiseScale
  , setUiMoistLandETCoeff
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiOrographicScale
  , setUiOrographicStep
  , setUiCoastalIterations
  , setUiCoastalDiffuse
  , setUiCoastalMoistureBoost
  , setUiWindBeltStrength
  , setUiWindBeltHarmonics
  , setUiWindBeltBase
  , setUiWindBeltRange
  , setUiWindBeltSpeedScale
  , setUiBndLandRange
  , setUiBndTempConvergent
  , setUiBndTempDivergent
  , setUiBndTempTransform
  , setUiBndPrecipConvergent
  , setUiBndPrecipDivergent
  , setUiBndPrecipTransform
  , setUiWeatherTick
  , setUiWeatherPhase
  , setUiWeatherAmplitude
  , setUiSeasonCycleLength
  , setUiJitterAmplitude
  , setUiPressureBase
  , setUiPressureTempScale
  , setUiPressureCoriolisScale
  , setUiSeasonalBase
  , setUiSeasonalRange
  , setUiHumidityNoiseScale
  , setUiPrecipNoiseScale
  , setUiWeatherITCZWidth
  , setUiWeatherITCZPrecipBoost
  , setUiPressureHumidityScale
  , setUiPressureGradientWindScale
  , setUiWindNoiseScale
  , setUiITCZMigrationScale
  , setUiCloudRHExponent
  , setUiCloudAlbedoEffect
  , setUiCloudPrecipBoost
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiBtCoastalBand
  , setUiBtSnowMaxTemp
  , setUiBtAlpineMaxTemp
  , setUiBtIceCapTemp
  , setUiBtMontaneMaxTemp
  , setUiBtMontanePrecip
  , setUiBtCliffSlope
  , setUiBtValleyMoisture
  , setUiBtDepressionMoisture
  , setUiBtPrecipWeight
  , setUiVbcTempMin
  , setUiVbcTempRange
  , setUiVbcFertilityBoost
  , setUiVbcAlbedoBase
  , setUiVbcAlbedoBare
  , setUiVbcAlbedoVeg
  , setUiVbcOceanAlbedo
  , setUiVbcIceAlbedo
  , setUiBiomeSmoothing
  , setUiVolcanicAshBoost
  , setUiVolcanicLavaPenalty
  , setUiBiomeFeedbackBlend
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiOccWarmScale
  , setUiOccColdScale
  , setUiOccLatPeakDeg
  , setUiOccLatWidthDeg
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  , emptyUiState
  )

import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Config (configFromUi, unmapRange, unmapIntRange)
import Seer.Config.Snapshot.Types

import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth(..))
import Topo.Biome (BiomeThresholds(..), BiomeVegetationConfig(..))
import Topo.BiomeConfig (BiomeConfig(..))
import Topo.Climate
  ( ClimateConfig(..)
  , TemperatureConfig(..)
  , WindConfig(..)
  , MoistureConfig(..)
  , PrecipitationConfig(..)
  , BoundaryConfig(..)
  )
import Topo.Erosion (ErosionConfig(..))
import Topo.Glacier (GlacierConfig(..))
import Topo.Hydrology (HydroConfig(..))
import Topo.OceanCurrent (OceanCurrentConfig(..))
import Topo.Parameters (ParameterConfig(..), TerrainFormConfig(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..))
import Topo.Soil (SoilConfig(..))
import Topo.Tectonics (TectonicsConfig(..))
import Topo.Types (WorldExtent, worldExtentRadiusX, worldExtentRadiusY)
import Topo.Vegetation (VegetationBootstrapConfig(..), BiomeFeedbackConfig(..))
import Topo.Volcanism (VolcanismConfig(..))
import Topo.WaterBody (WaterBodyConfig(..))
import Topo.Weather (WeatherConfig(..))
import Topo.WorldGen (TerrainConfig(..), WorldGenConfig(..))

-- ---------------------------------------------------------------------------
-- Conversion: UI → Snapshot
-- ---------------------------------------------------------------------------

-- | Build a 'ConfigSnapshot' from the current UI slider state.
--
-- The generation config is produced by 'Seer.Config.configFromUi' which
-- maps every normalised @[0, 1]@ slider value to its real domain value.
snapshotFromUi :: UiState -> Text -> ConfigSnapshot
snapshotFromUi ui name = ConfigSnapshot
  { csName             = name
  , csVersion          = currentSnapshotVersion
  , csSeed             = uiSeed ui
  , csChunkSize        = uiChunkSize ui
  , csRenderWaterLevel = uiRenderWaterLevel ui
  , csGenConfig        = configFromUi ui
  }

-- ---------------------------------------------------------------------------
-- Conversion: Snapshot → UI  (inverse mapping)
-- ---------------------------------------------------------------------------

-- | Restore all slider values from a 'ConfigSnapshot' by sending
-- individual setter messages to the UI actor.
--
-- Domain values in 'WorldGenConfig' are reverse-mapped to normalised
-- @[0, 1]@ slider positions using 'unmapRange' / 'unmapIntRange'.
-- Metadata fields ('csName', 'csVersion') are not applied.
applySnapshotToUi :: ConfigSnapshot -> ActorHandle Ui (Protocol Ui) -> IO ()
applySnapshotToUi cs h = do
  let cfg     = csGenConfig cs
      terrain = worldTerrain cfg
      gen     = terrainGen terrain
      edge    = gcOceanEdgeDepth gen
      extent  = gcWorldExtent gen
      tec     = terrainTectonics terrain
      params  = terrainParameters terrain
      form    = terrainFormConfig terrain
      hydro   = terrainHydrology terrain
      erosion = terrainErosion terrain
      glacier = terrainGlacier terrain
      volc    = terrainVolcanism terrain
      soil    = terrainSoil terrain
      water   = terrainWaterBody terrain
      vegBoot = terrainVegetation terrain
      climate = worldClimate cfg
      tmp     = ccTemperature climate
      wind    = ccWind climate
      moist   = ccMoisture climate
      prec    = ccPrecipitation climate
      bnd     = ccBoundary climate
      weather = worldWeather cfg
      biome   = worldBiome cfg
      veg     = bcVegetation biome
      thresh  = bcThresholds biome
      bf      = worldBiomeFeedback cfg
      planet  = worldPlanet cfg
      occ     = worldOceanCurrent cfg
      slice   = worldSlice cfg
  -- Seed / chunk size / render water level
  setUiSeed h (csSeed cs)
  setUiChunkSize h (csChunkSize cs)
  setUiRenderWaterLevel h (csRenderWaterLevel cs)
  -- Hydrology (direct)
  setUiWaterLevel h (hcWaterLevel hydro)
  setUiSinkBreachDepth h (unmapRange 0.0 0.1 (hcSinkBreachDepth hydro))
  setUiStreamPowerMaxErosion h (unmapRange 0.0 0.2 (hcStreamPowerMaxErosion hydro))
  setUiRiverCarveMaxDepth h (unmapRange 0.0 0.2 (hcRiverCarveMaxDepth hydro))
  setUiCoastalErodeStrength h (unmapRange 0.0 0.1 (hcCoastalErodeStrength hydro))
  setUiHydroHardnessWeight h (unmapRange 0.0 1.0 (hcHardnessErodeWeight hydro))
  -- Water body
  setUiMinLakeSize h (unmapIntRange 1 50 (wbcMinLakeSize water))
  setUiInlandSeaMinSize h (unmapIntRange 50 500 (wbcInlandSeaMinSize water))
  -- Generation
  setUiGenScale h (unmapRange 0.2 2.0 (gcScale gen))
  setUiGenCoordScale h (unmapRange 0.5 2.0 (gcCoordScale gen))
  setUiGenOffsetX h (unmapRange (-10000) 10000 (gcOffsetX gen))
  setUiGenOffsetY h (unmapRange (-10000) 10000 (gcOffsetY gen))
  setUiGenFrequency h (unmapRange 0.001 0.05 (gcFrequency gen))
  setUiGenOctaves h (unmapIntRange 2 8 (gcOctaves gen))
  setUiGenLacunarity h (unmapRange 1.5 3.5 (gcLacunarity gen))
  setUiGenGain h (unmapRange 0.3 0.8 (gcGain gen))
  setUiGenWarpScale h (unmapRange 0.005 0.08 (gcWarpScale gen))
  setUiGenWarpStrength h (unmapRange 2 20 (gcWarpStrength gen))
  setUiWorldExtentX h (unmapIntRange 0 16 (worldExtentRadiusX extent))
  setUiWorldExtentY h (unmapIntRange 0 16 (worldExtentRadiusY extent))
  -- Ocean edge depth
  setUiEdgeDepthNorth h (unmapRange 0 2 (oedNorth edge))
  setUiEdgeDepthSouth h (unmapRange 0 2 (oedSouth edge))
  setUiEdgeDepthEast h (unmapRange 0 2 (oedEast edge))
  setUiEdgeDepthWest h (unmapRange 0 2 (oedWest edge))
  setUiEdgeDepthFalloff h (unmapRange 0 512 (oedFalloff edge))
  -- Tectonics
  setUiPlateSize h (unmapIntRange 16 128 (tcPlateSize tec))
  setUiPlateSpeed h (unmapRange 0.1 1.5 (tcPlateSpeed tec))
  setUiBoundarySharpness h (unmapRange 0.5 2.5 (tcBoundarySharpness tec))
  setUiBoundaryNoiseScale h (unmapRange 0.002 0.02 (tcBoundaryNoiseScale tec))
  setUiBoundaryNoiseStrength h (unmapRange 2 24 (tcBoundaryNoiseStrength tec))
  setUiBoundaryWarpOctaves h (unmapIntRange 1 5 (tcBoundaryWarpOctaves tec))
  setUiBoundaryWarpLacunarity h (unmapRange 1.5 3.5 (tcBoundaryWarpLacunarity tec))
  setUiBoundaryWarpGain h (unmapRange 0.3 0.8 (tcBoundaryWarpGain tec))
  setUiPlateMergeScale h (unmapRange 0.05 0.25 (tcPlateMergeScale tec))
  setUiPlateMergeBias h (unmapRange 0.3 0.8 (tcPlateMergeBias tec))
  setUiPlateDetailScale h (unmapRange 0.005 0.05 (tcPlateDetailScale tec))
  setUiPlateDetailStrength h (unmapRange 0 1 (tcPlateDetailStrength tec))
  setUiPlateRidgeStrength h (unmapRange 0 1 (tcPlateRidgeStrength tec))
  setUiPlateHeightBase h (unmapRange (-0.1) 0.35 (tcPlateHeightBase tec))
  setUiPlateHeightVariance h (unmapRange 0.2 1.2 (tcPlateHeightVariance tec))
  setUiPlateHardnessBase h (unmapRange 0.2 0.8 (tcPlateHardnessBase tec))
  setUiPlateHardnessVariance h (unmapRange 0.1 0.6 (tcPlateHardnessVariance tec))
  setUiPlateBiasStrength h (unmapRange 0 0.6 (tcPlateBiasStrength tec))
  setUiPlateBiasCenter h (unmapRange (-1) 1 (tcPlateBiasCenter tec))
  setUiPlateBiasEdge h (unmapRange (-1) 1 (tcPlateBiasEdge tec))
  setUiPlateBiasNorth h (unmapRange (-1) 1 (tcPlateBiasNorth tec))
  setUiPlateBiasSouth h (unmapRange (-1) 1 (tcPlateBiasSouth tec))
  setUiUplift h (unmapRange 0.05 0.4 (tcUplift tec))
  setUiRiftDepth h (unmapRange 0.05 0.6 (tcRiftDepth tec))
  setUiTrenchDepth h (unmapRange 0.1 0.5 (tcTrenchDepth tec))
  setUiRidgeHeight h (unmapRange 0.02 0.2 (tcRidgeHeight tec))
  -- Parameters
  setUiDetailScale h (unmapRange 0.5 2.5 (pcDetailScale params))
  setUiRoughnessScale h (unmapRange 0.0 2.0 (pcRoughnessScale params))
  setUiRockElevationThreshold h (unmapRange 0.2 0.9 (pcRockElevationThreshold params))
  setUiRockHardnessThreshold h (unmapRange 0.2 0.9 (pcRockHardnessThreshold params))
  setUiRockHardnessSecondary h (unmapRange 0.1 0.8 (pcRockHardnessSecondary params))
  -- Terrain form
  setUiTfcCliffSlope h (unmapRange 0.1 0.8 (tfcCliffSlope form))
  setUiTfcMountainSlope h (unmapRange 0.05 0.5 (tfcMountainSlope form))
  setUiTfcMountainRelief h (unmapRange 0.05 0.5 (tfcMountainRelief form))
  setUiTfcHillSlope h (unmapRange 0.02 0.2 (tfcHillSlope form))
  setUiTfcRollingSlope h (unmapRange 0.005 0.1 (tfcRollingSlope form))
  setUiValleyCurvature h (unmapRange 0.05 0.4 (tfcValleyCurvature form))
  -- Erosion
  setUiRainRate h (unmapRange 0.05 0.5 (ecRainRate erosion))
  setUiErosionHydraulic h (unmapIntRange 1 12 (ecHydraulicIterations erosion))
  setUiErosionThermal h (unmapIntRange 1 12 (ecThermalIterations erosion))
  setUiErosionTalus h (unmapRange 0.1 1.0 (ecThermalTalus erosion))
  setUiErosionMaxDrop h (unmapRange 0.1 1.0 (ecMaxDrop erosion))
  -- Glacier
  setUiGlacierSnowTemp h (unmapRange 0.0 0.5 (gcSnowTemp glacier))
  setUiGlacierSnowRange h (unmapRange 0.1 0.7 (gcSnowRange glacier))
  setUiGlacierMeltTemp h (unmapRange 0.1 0.8 (gcMeltTemp glacier))
  setUiGlacierMeltRate h (unmapRange 0.0 1.0 (gcMeltRate glacier))
  setUiGlacierAccumScale h (unmapRange 0.0 3.0 (gcAccumScale glacier))
  setUiGlacierFlowIters h (unmapIntRange 0 10 (gcFlowIterations glacier))
  setUiGlacierFlowRate h (unmapRange 0.0 1.0 (gcFlowRate glacier))
  setUiGlacierErosionScale h (unmapRange 0.0 1.0 (gcErosionScale glacier))
  setUiGlacierCarveScale h (unmapRange 0.0 0.1 (gcCarveScale glacier))
  setUiGlacierDepositScale h (unmapRange 0.0 1.0 (gcDepositScale glacier))
  -- Volcanism
  setUiVentDensity h (unmapRange 0.0 0.2 (vcVentDensityBase volc))
  setUiVentThreshold h (unmapRange 0.2 0.9 (vcVentThreshold volc))
  setUiHotspotScale h (unmapRange 0.0 1.0 (vcHotspotScale volc))
  setUiHotspotThreshold h (unmapRange 0.3 0.95 (vcHotspotThreshold volc))
  setUiMagmaRecharge h (unmapRange 0.0 3.0 (vcMagmaRecharge volc))
  setUiLavaScale h (unmapRange 0.0 1.0 (vcLavaScale volc))
  setUiAshScale h (unmapRange 0.0 1.0 (vcAshScale volc))
  setUiVolcanicDepositScale h (unmapRange 0.0 1.0 (vcDepositScale volc))
  -- Soil
  setUiSoilMoistureThreshold h (unmapRange 0.0 1.0 (scMoistureThreshold soil))
  setUiSoilHardnessThreshold h (unmapRange 0.0 1.0 (scHardnessThreshold soil))
  setUiSoilFertilityMoistWeight h (unmapRange 0.0 1.0 (scFertilityMoistWeight soil))
  setUiSoilFertilityDepthWeight h (unmapRange 0.0 1.0 (scFertilityDepthWeight soil))
  -- Climate: temperature (direct slider values where no mapRange is used)
  setUiEquatorTemp h (tmpEquatorTemp tmp)
  setUiPoleTemp h (tmpPoleTemp tmp)
  setUiLapseRate h (tmpLapseRate tmp)
  setUiLatitudeExponent h (unmapRange 0.2 1.5 (tmpLatitudeExponent tmp))
  setUiPlateHeightCooling h (unmapRange 0 0.2 (tmpPlateHeightCooling tmp))
  setUiTempNoiseScale h (unmapRange 0 0.3 (tmpNoiseScale tmp))
  setUiOceanModeration h (tmpOceanModeration tmp)
  setUiOceanModerateTemp h (unmapRange (-0.1) 0.1 (tmpOceanModerateTemp tmp))
  setUiAlbedoSensitivity h (tmpAlbedoSensitivity tmp)
  setUiAlbedoReference h (unmapRange 0 0.5 (tmpAlbedoReference tmp))
  -- Climate: wind
  setUiWindDiffuse h (windDiffuse wind)
  setUiWindIterations h (unmapIntRange 1 8 (windIterations wind))
  setUiWindBeltStrength h (windBeltStrength wind)
  setUiWindBeltHarmonics h (unmapRange 1 6 (windBeltHarmonics wind))
  setUiWindBeltBase h (windBeltBase wind)
  setUiWindBeltRange h (windBeltRange wind)
  setUiWindBeltSpeedScale h (windBeltSpeedScale wind)
  -- Climate: moisture
  setUiEvaporation h (moistEvapCoeff moist)
  setUiMoistureIterations h (unmapIntRange 2 72 (moistIterations moist))
  setUiMoistAdvect h (moistAdvect moist)
  setUiMoistLocal h (moistLocal moist)
  setUiMoistWindEvapScale h (moistWindEvapScale moist)
  setUiMoistEvapNoiseScale h (unmapRange 0 0.2 (moistEvapNoiseScale moist))
  setUiMoistLandETCoeff h (moistLandETCoeff moist)
  setUiMoistBareEvapFrac h (moistBareEvapFrac moist)
  setUiMoistVegTranspFrac h (moistVegTranspFrac moist)
  setUiMoistWindETScale h (moistWindETScale moist)
  setUiMoistCondensationRate h (moistCondensationRate moist)
  setUiMoistRecycleRate h (moistRecycleRate moist)
  setUiMoistITCZStrength h (unmapRange 0 0.5 (moistITCZStrength moist))
  setUiMoistITCZWidth h (unmapRange 2 20 (moistITCZWidth moist))
  -- Climate: precipitation
  setUiRainShadow h (precRainShadow prec)
  setUiOrographicScale h (unmapRange 0 2 (precOrographicScale prec))
  setUiOrographicStep h (unmapRange 0.5 3 (precOrographicStep prec))
  setUiCoastalIterations h (unmapIntRange 0 16 (precCoastalIterations prec))
  setUiCoastalDiffuse h (precCoastalDiffuse prec)
  setUiCoastalMoistureBoost h (unmapRange 0 0.5 (precCoastalMoistureBoost prec))
  -- Climate: boundary model
  setUiBoundaryMotionTemp h (unmapRange 0 2 (bndMotionTemp bnd))
  setUiBoundaryMotionPrecip h (unmapRange 0 2 (bndMotionPrecip bnd))
  setUiBndLandRange h (unmapRange 0.1 1.5 (bndLandRange bnd))
  setUiBndTempConvergent h (unmapRange (-0.2) 0.1 (bndTempConvergent bnd))
  setUiBndTempDivergent h (unmapRange (-0.1) 0.2 (bndTempDivergent bnd))
  setUiBndTempTransform h (unmapRange (-0.1) 0.1 (bndTempTransform bnd))
  setUiBndPrecipConvergent h (unmapRange (-0.1) 0.2 (bndPrecipConvergent bnd))
  setUiBndPrecipDivergent h (unmapRange (-0.2) 0.1 (bndPrecipDivergent bnd))
  setUiBndPrecipTransform h (unmapRange (-0.1) 0.1 (bndPrecipTransform bnd))
  -- Weather
  setUiWeatherTick h (unmapRange 0.1 5 (wcTickSeconds weather))
  setUiWeatherPhase h (unmapRange 0 1 (wcSeasonPhase weather))
  setUiWeatherAmplitude h (unmapRange 0 0.5 (wcSeasonAmplitude weather))
  setUiSeasonCycleLength h (unmapRange 30 730 (wcSeasonCycleLength weather))
  setUiJitterAmplitude h (unmapRange 0 0.5 (wcJitterAmplitude weather))
  setUiPressureBase h (unmapRange 0.3 1.0 (wcPressureBase weather))
  setUiPressureTempScale h (unmapRange 0 1 (wcPressureTempScale weather))
  setUiPressureCoriolisScale h (unmapRange 0 0.5 (wcPressureCoriolisScale weather))
  setUiSeasonalBase h (unmapRange 0 1 (wcSeasonalBase weather))
  setUiSeasonalRange h (unmapRange 0 2 (wcSeasonalRange weather))
  setUiHumidityNoiseScale h (unmapRange 0 0.3 (wcHumidityNoiseScale weather))
  setUiPrecipNoiseScale h (unmapRange 0 0.5 (wcPrecipNoiseScale weather))
  setUiWeatherITCZWidth h (unmapRange 2 20 (wcITCZWidth weather))
  setUiWeatherITCZPrecipBoost h (unmapRange 0 1 (wcITCZPrecipBoost weather))
  setUiPressureHumidityScale h (unmapRange 0 0.5 (wcPressureHumidityScale weather))
  setUiPressureGradientWindScale h (unmapRange 0 1 (wcPressureGradientWindScale weather))
  setUiWindNoiseScale h (unmapRange 0 0.3 (wcWindNoiseScale weather))
  setUiITCZMigrationScale h (unmapRange 0 1.5 (wcITCZMigrationScale weather))
  setUiCloudRHExponent h (unmapRange 0.5 3.0 (wcCloudRHExponent weather))
  setUiCloudAlbedoEffect h (unmapRange 0 0.3 (wcCloudAlbedoEffect weather))
  setUiCloudPrecipBoost h (unmapRange 0 0.5 (wcCloudPrecipBoost weather))
  -- Vegetation (biome)
  setUiVegBase h (unmapRange 0.02 0.6 (vcBaseDensity veg))
  setUiVegBoost h (unmapRange 0.1 1.0 (vcBiomeBoost veg))
  setUiVegTempWeight h (unmapRange 0 1 (vcTempWeight veg))
  setUiVegPrecipWeight h (unmapRange 0 1 (vcPrecipWeight veg))
  -- Biome thresholds
  setUiBtCoastalBand h (unmapRange 0 0.1 (btCoastalBand thresh))
  setUiBtSnowMaxTemp h (unmapRange 0.0 0.5 (btSnowMaxTemp thresh))
  setUiBtAlpineMaxTemp h (unmapRange 0.1 0.7 (btAlpineMaxTemp thresh))
  setUiBtIceCapTemp h (unmapRange 0 0.2 (btIceCapTemp thresh))
  setUiBtMontaneMaxTemp h (unmapRange 0.2 0.8 (btMontaneMaxTemp thresh))
  setUiBtMontanePrecip h (unmapRange 0.1 0.6 (btMontanePrecip thresh))
  setUiBtCliffSlope h (unmapRange 0.2 0.8 (btCliffSlope thresh))
  setUiBtValleyMoisture h (unmapRange 0.3 1.0 (btValleyMoisture thresh))
  setUiBtDepressionMoisture h (unmapRange 0.2 1.0 (btDepressionMoisture thresh))
  setUiBtPrecipWeight h (unmapRange 0.5 5.0 (btPrecipWeight thresh))
  -- Biome misc
  setUiBiomeSmoothing h (unmapRange 0 5 (fromIntegral (bcSmoothingIterations biome)))
  setUiVolcanicAshBoost h (unmapRange 0 0.5 (bcVolcanicAshBoost biome))
  setUiVolcanicLavaPenalty h (unmapRange 0 0.8 (bcVolcanicLavaPenalty biome))
  setUiBiomeFeedbackBlend h (unmapRange 0 1 (bfcBlendWeight bf))
  -- Vegetation bootstrap
  setUiVbcTempMin h (unmapRange 0 0.3 (vbcTempMin vegBoot))
  setUiVbcTempRange h (unmapRange 0.1 1.0 (vbcTempRange vegBoot))
  setUiVbcFertilityBoost h (unmapRange 0 1 (vbcFertilityBoost vegBoot))
  setUiVbcAlbedoBase h (unmapRange 0 0.3 (vbcAlbedoBase vegBoot))
  setUiVbcAlbedoBare h (unmapRange 0.1 0.5 (vbcAlbedoBare vegBoot))
  setUiVbcAlbedoVeg h (unmapRange 0 0.3 (vbcAlbedoVeg vegBoot))
  setUiVbcOceanAlbedo h (unmapRange 0 0.2 (vbcOceanAlbedo vegBoot))
  setUiVbcIceAlbedo h (unmapRange 0.3 1.0 (vbcIceAlbedo vegBoot))
  -- Planet
  setUiPlanetRadius h (unmapRange 4778 9557 (pcRadius planet))
  setUiAxialTilt h (unmapRange 0 45 (pcAxialTilt planet))
  setUiInsolation h (unmapRange 0.7 1.3 (pcInsolation planet))
  -- Ocean currents
  setUiOccWarmScale h (unmapRange 0 0.2 (occWarmScale occ))
  setUiOccColdScale h (unmapRange 0 0.2 (occColdScale occ))
  setUiOccLatPeakDeg h (unmapRange 0 60 (occLatPeakDeg occ))
  setUiOccLatWidthDeg h (unmapRange 5 45 (occLatWidthDeg occ))
  -- World slice
  setUiSliceLatCenter h (unmapRange (-90) 90 (wsLatCenter slice))
  setUiSliceLonCenter h (unmapRange (-180) 180 (wsLonCenter slice))

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Return the config snapshot directory (@~\/.topo\/configs\/@), creating
-- it if it does not exist.
--
-- The same directory as the legacy preset directory.
snapshotDir :: IO FilePath
snapshotDir = do
  home <- getHomeDirectory
  let dir = home </> ".topo" </> "configs"
  createDirectoryIfMissing True dir
  pure dir

-- | Write a 'ConfigSnapshot' atomically (write to temp, then rename).
-- Returns @Left@ on IO error.
saveSnapshot :: FilePath -> ConfigSnapshot -> IO (Either Text ())
saveSnapshot path cs = do
  result <- try @IOException $ do
    let tmpPath = path <> ".tmp"
    BSL.writeFile tmpPath (encode cs)
    targetExists <- doesFileExist path
    if targetExists
      then removeFile path >> renameFile tmpPath path
      else renameFile tmpPath path
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-- | Load a 'ConfigSnapshot' from a JSON file.
loadSnapshot :: FilePath -> IO (Either Text ConfigSnapshot)
loadSnapshot path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right bs ->
      case eitherDecodeStrict' bs of
        Right cs -> Right cs
        Left err -> Left (Text.pack err)

-- | List all config names in the snapshot directory, sorted
-- alphabetically.  Names are returned without the @.json@ extension.
listSnapshots :: IO [Text]
listSnapshots = do
  dir <- snapshotDir
  entries <- listDirectory dir
  let names = sort
        [ Text.pack (dropExtension f)
        | f <- entries
        , takeExtension f == ".json"
        ]
  pure names
