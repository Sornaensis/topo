{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Config preset persistence for topo-seer.
--
-- A 'ConfigPreset' captures all slider values from the UI so they can be
-- saved to disk as JSON and restored later.  The JSON format uses optional
-- fields with defaults from 'defaultPreset', making it forward-compatible:
-- presets created before new sliders were added will load with sensible
-- defaults for the new fields.
--
-- Presets are stored as individual @.json@ files under @~\/.topo\/configs\/@.
module Seer.Config.Preset
  ( -- * Re-exports from Types (cycle-free)
    ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
    -- * Conversion
  , presetFromUi
  , applyPresetToUi
    -- * File I/O
  , presetDir
  , savePreset
  , loadPreset
  , listPresets
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

import Seer.Config.Preset.Types
  ( ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
  )

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
  , setUiWindIterations
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiWeatherTick
  , setUiWeatherPhase
  , setUiWeatherAmplitude
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  )

import Hyperspace.Actor (ActorHandle, Protocol)

-- ---------------------------------------------------------------------------
-- Conversion
-- ---------------------------------------------------------------------------

-- | Capture all slider values from a 'UiState' into a 'ConfigPreset'
-- with the given name.
presetFromUi :: UiState -> Text -> ConfigPreset
presetFromUi ui name = ConfigPreset
  { cpName                    = name
  , cpVersion                 = currentPresetVersion
  , cpSeed                    = uiSeed ui
  , cpChunkSize               = uiChunkSize ui
  , cpWaterLevel              = uiWaterLevel ui
  , cpRenderWaterLevel        = uiRenderWaterLevel ui
  , cpEvaporation             = uiEvaporation ui
  , cpRainShadow              = uiRainShadow ui
  , cpWindDiffuse             = uiWindDiffuse ui
  , cpEquatorTemp             = uiEquatorTemp ui
  , cpPoleTemp                = uiPoleTemp ui
  , cpLapseRate               = uiLapseRate ui
  , cpWindIterations          = uiWindIterations ui
  , cpMoistureIterations      = uiMoistureIterations ui
  , cpBoundaryMotionTemp      = uiBoundaryMotionTemp ui
  , cpBoundaryMotionPrecip    = uiBoundaryMotionPrecip ui
  , cpRainRate                = uiRainRate ui
  , cpErosionHydraulic        = uiErosionHydraulic ui
  , cpErosionThermal          = uiErosionThermal ui
  , cpErosionTalus            = uiErosionTalus ui
  , cpErosionMaxDrop          = uiErosionMaxDrop ui
  , cpGenScale                = uiGenScale ui
  , cpGenCoordScale           = uiGenCoordScale ui
  , cpGenOffsetX              = uiGenOffsetX ui
  , cpGenOffsetY              = uiGenOffsetY ui
  , cpGenFrequency            = uiGenFrequency ui
  , cpGenOctaves              = uiGenOctaves ui
  , cpGenLacunarity           = uiGenLacunarity ui
  , cpGenGain                 = uiGenGain ui
  , cpGenWarpScale            = uiGenWarpScale ui
  , cpGenWarpStrength         = uiGenWarpStrength ui
  , cpWorldExtentX            = uiWorldExtentX ui
  , cpWorldExtentY            = uiWorldExtentY ui
  , cpEdgeDepthNorth          = uiEdgeDepthNorth ui
  , cpEdgeDepthSouth          = uiEdgeDepthSouth ui
  , cpEdgeDepthEast           = uiEdgeDepthEast ui
  , cpEdgeDepthWest           = uiEdgeDepthWest ui
  , cpEdgeDepthFalloff        = uiEdgeDepthFalloff ui
  , cpPlateSize               = uiPlateSize ui
  , cpPlateSpeed              = uiPlateSpeed ui
  , cpBoundarySharpness       = uiBoundarySharpness ui
  , cpBoundaryNoiseScale      = uiBoundaryNoiseScale ui
  , cpBoundaryNoiseStrength   = uiBoundaryNoiseStrength ui
  , cpBoundaryWarpOctaves     = uiBoundaryWarpOctaves ui
  , cpBoundaryWarpLacunarity  = uiBoundaryWarpLacunarity ui
  , cpBoundaryWarpGain        = uiBoundaryWarpGain ui
  , cpPlateMergeScale         = uiPlateMergeScale ui
  , cpPlateMergeBias          = uiPlateMergeBias ui
  , cpPlateDetailScale        = uiPlateDetailScale ui
  , cpPlateDetailStrength     = uiPlateDetailStrength ui
  , cpPlateRidgeStrength      = uiPlateRidgeStrength ui
  , cpPlateHeightBase         = uiPlateHeightBase ui
  , cpPlateHeightVariance     = uiPlateHeightVariance ui
  , cpPlateHardnessBase       = uiPlateHardnessBase ui
  , cpPlateHardnessVariance   = uiPlateHardnessVariance ui
  , cpUplift                  = uiUplift ui
  , cpRiftDepth               = uiRiftDepth ui
  , cpTrenchDepth             = uiTrenchDepth ui
  , cpRidgeHeight             = uiRidgeHeight ui
  , cpDetailScale             = uiDetailScale ui
  , cpPlateBiasStrength       = uiPlateBiasStrength ui
  , cpPlateBiasCenter         = uiPlateBiasCenter ui
  , cpPlateBiasEdge           = uiPlateBiasEdge ui
  , cpPlateBiasNorth          = uiPlateBiasNorth ui
  , cpPlateBiasSouth          = uiPlateBiasSouth ui
  , cpWeatherTick             = uiWeatherTick ui
  , cpWeatherPhase            = uiWeatherPhase ui
  , cpWeatherAmplitude        = uiWeatherAmplitude ui
  , cpVegBase                 = uiVegBase ui
  , cpVegBoost                = uiVegBoost ui
  , cpVegTempWeight           = uiVegTempWeight ui
  , cpVegPrecipWeight         = uiVegPrecipWeight ui
  , cpPlanetRadius            = uiPlanetRadius ui
  , cpAxialTilt               = uiAxialTilt ui
  , cpInsolation              = uiInsolation ui
  , cpSliceLatCenter          = uiSliceLatCenter ui
  , cpSliceLonCenter          = uiSliceLonCenter ui
  }

-- | Restore all slider values from a 'ConfigPreset' by sending
-- individual setter messages to the UI actor.  Metadata fields
-- ('cpName', 'cpVersion') are not applied.
applyPresetToUi :: ConfigPreset -> ActorHandle Ui (Protocol Ui) -> IO ()
applyPresetToUi cp h = do
  setUiSeed h (cpSeed cp)
  setUiChunkSize h (cpChunkSize cp)
  -- Hydrology
  setUiWaterLevel h (cpWaterLevel cp)
  setUiRenderWaterLevel h (cpRenderWaterLevel cp)
  -- Climate
  setUiEvaporation h (cpEvaporation cp)
  setUiRainShadow h (cpRainShadow cp)
  setUiWindDiffuse h (cpWindDiffuse cp)
  setUiEquatorTemp h (cpEquatorTemp cp)
  setUiPoleTemp h (cpPoleTemp cp)
  setUiLapseRate h (cpLapseRate cp)
  setUiWindIterations h (cpWindIterations cp)
  setUiMoistureIterations h (cpMoistureIterations cp)
  setUiBoundaryMotionTemp h (cpBoundaryMotionTemp cp)
  setUiBoundaryMotionPrecip h (cpBoundaryMotionPrecip cp)
  -- Erosion
  setUiRainRate h (cpRainRate cp)
  setUiErosionHydraulic h (cpErosionHydraulic cp)
  setUiErosionThermal h (cpErosionThermal cp)
  setUiErosionTalus h (cpErosionTalus cp)
  setUiErosionMaxDrop h (cpErosionMaxDrop cp)
  -- Generation
  setUiGenScale h (cpGenScale cp)
  setUiGenCoordScale h (cpGenCoordScale cp)
  setUiGenOffsetX h (cpGenOffsetX cp)
  setUiGenOffsetY h (cpGenOffsetY cp)
  setUiGenFrequency h (cpGenFrequency cp)
  setUiGenOctaves h (cpGenOctaves cp)
  setUiGenLacunarity h (cpGenLacunarity cp)
  setUiGenGain h (cpGenGain cp)
  setUiGenWarpScale h (cpGenWarpScale cp)
  setUiGenWarpStrength h (cpGenWarpStrength cp)
  setUiWorldExtentX h (cpWorldExtentX cp)
  setUiWorldExtentY h (cpWorldExtentY cp)
  -- Ocean edge depth
  setUiEdgeDepthNorth h (cpEdgeDepthNorth cp)
  setUiEdgeDepthSouth h (cpEdgeDepthSouth cp)
  setUiEdgeDepthEast h (cpEdgeDepthEast cp)
  setUiEdgeDepthWest h (cpEdgeDepthWest cp)
  setUiEdgeDepthFalloff h (cpEdgeDepthFalloff cp)
  -- Tectonics
  setUiPlateSize h (cpPlateSize cp)
  setUiPlateSpeed h (cpPlateSpeed cp)
  setUiBoundarySharpness h (cpBoundarySharpness cp)
  setUiBoundaryNoiseScale h (cpBoundaryNoiseScale cp)
  setUiBoundaryNoiseStrength h (cpBoundaryNoiseStrength cp)
  setUiBoundaryWarpOctaves h (cpBoundaryWarpOctaves cp)
  setUiBoundaryWarpLacunarity h (cpBoundaryWarpLacunarity cp)
  setUiBoundaryWarpGain h (cpBoundaryWarpGain cp)
  setUiPlateMergeScale h (cpPlateMergeScale cp)
  setUiPlateMergeBias h (cpPlateMergeBias cp)
  setUiPlateDetailScale h (cpPlateDetailScale cp)
  setUiPlateDetailStrength h (cpPlateDetailStrength cp)
  setUiPlateRidgeStrength h (cpPlateRidgeStrength cp)
  setUiPlateHeightBase h (cpPlateHeightBase cp)
  setUiPlateHeightVariance h (cpPlateHeightVariance cp)
  setUiPlateHardnessBase h (cpPlateHardnessBase cp)
  setUiPlateHardnessVariance h (cpPlateHardnessVariance cp)
  setUiUplift h (cpUplift cp)
  setUiRiftDepth h (cpRiftDepth cp)
  setUiTrenchDepth h (cpTrenchDepth cp)
  setUiRidgeHeight h (cpRidgeHeight cp)
  setUiDetailScale h (cpDetailScale cp)
  setUiPlateBiasStrength h (cpPlateBiasStrength cp)
  setUiPlateBiasCenter h (cpPlateBiasCenter cp)
  setUiPlateBiasEdge h (cpPlateBiasEdge cp)
  setUiPlateBiasNorth h (cpPlateBiasNorth cp)
  setUiPlateBiasSouth h (cpPlateBiasSouth cp)
  -- Weather
  setUiWeatherTick h (cpWeatherTick cp)
  setUiWeatherPhase h (cpWeatherPhase cp)
  setUiWeatherAmplitude h (cpWeatherAmplitude cp)
  -- Vegetation
  setUiVegBase h (cpVegBase cp)
  setUiVegBoost h (cpVegBoost cp)
  setUiVegTempWeight h (cpVegTempWeight cp)
  setUiVegPrecipWeight h (cpVegPrecipWeight cp)
  -- Planet
  setUiPlanetRadius h (cpPlanetRadius cp)
  setUiAxialTilt h (cpAxialTilt cp)
  setUiInsolation h (cpInsolation cp)
  -- World slice
  setUiSliceLatCenter h (cpSliceLatCenter cp)
  setUiSliceLonCenter h (cpSliceLonCenter cp)

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Return the preset directory (@~\/.topo\/configs\/@), creating it if
-- it does not exist.
presetDir :: IO FilePath
presetDir = do
  home <- getHomeDirectory
  let dir = home </> ".topo" </> "configs"
  createDirectoryIfMissing True dir
  pure dir

-- | Write a 'ConfigPreset' to a JSON file atomically (write to temp
-- file, then rename).  Returns @Left@ on IO error.
savePreset :: FilePath -> ConfigPreset -> IO (Either Text ())
savePreset path cp = do
  result <- try @IOException $ do
    let tmpPath = path <> ".tmp"
    BSL.writeFile tmpPath (encode cp)
    -- On Windows renameFile fails if the target exists; remove first.
    targetExists <- doesFileExist path
    if targetExists
      then removeFile path >> renameFile tmpPath path
      else renameFile tmpPath path
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-- | Read and decode a 'ConfigPreset' from a JSON file.
-- Returns @Left@ on IO or parse error.
loadPreset :: FilePath -> IO (Either Text ConfigPreset)
loadPreset path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right bs -> case eitherDecodeStrict' bs of
      Left parseErr -> Left (Text.pack parseErr)
      Right cp      -> Right cp

-- | List all preset names in the preset directory, sorted
-- alphabetically.  Names are returned without the @.json@ extension.
listPresets :: IO [Text]
listPresets = do
  dir <- presetDir
  entries <- listDirectory dir
  let names = sort
        [ Text.pack (dropExtension f)
        | f <- entries
        , takeExtension f == ".json"
        ]
  pure names
