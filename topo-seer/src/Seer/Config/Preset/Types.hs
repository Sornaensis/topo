{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Core types for config presets.
--
-- This module is kept free of 'Actor.UI' imports to avoid module
-- cycles.  The conversion functions ('presetFromUi', 'applyPresetToUi')
-- live in "Seer.Config.Preset".
module Seer.Config.Preset.Types
  ( ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
  , presetJsonOptions
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:?)
  , (.!=)
  , genericToJSON
  , withObject
  , defaultOptions
  , Options(..)
  )
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Version tag
-- ---------------------------------------------------------------------------

-- | Monotonically increasing version tag for forward compatibility.
-- Bump this when the set of fields changes.
currentPresetVersion :: Int
currentPresetVersion = 1

-- ---------------------------------------------------------------------------
-- ConfigPreset data type
-- ---------------------------------------------------------------------------

-- | A snapshot of all generation-relevant slider values from the UI.
--
-- All @Float@ fields store normalised slider positions in @[0, 1]@.
-- Metadata fields ('cpName', 'cpVersion') are not restored by
-- @applyPresetToUi@.
data ConfigPreset = ConfigPreset
  { -- | Human-readable preset name.
    cpName                    :: Text
    -- | Schema version for forward compatibility.
  , cpVersion                 :: Int
    -- | RNG seed.
  , cpSeed                    :: Word64
    -- | Chunk size (8–256).
  , cpChunkSize               :: Int
    -- Hydrology
  , cpWaterLevel              :: Float
  , cpRenderWaterLevel        :: Float
    -- Climate
  , cpEvaporation             :: Float
  , cpRainShadow              :: Float
  , cpWindDiffuse             :: Float
  , cpEquatorTemp             :: Float
  , cpPoleTemp                :: Float
  , cpLapseRate               :: Float
  , cpWindIterations          :: Float
  , cpMoistureIterations      :: Float
  , cpBoundaryMotionTemp      :: Float
  , cpBoundaryMotionPrecip    :: Float
    -- Erosion
  , cpRainRate                :: Float
  , cpErosionHydraulic        :: Float
  , cpErosionThermal          :: Float
  , cpErosionTalus            :: Float
  , cpErosionMaxDrop          :: Float
    -- Generation
  , cpGenScale                :: Float
  , cpGenCoordScale           :: Float
  , cpGenOffsetX              :: Float
  , cpGenOffsetY              :: Float
  , cpGenFrequency            :: Float
  , cpGenOctaves              :: Float
  , cpGenLacunarity           :: Float
  , cpGenGain                 :: Float
  , cpGenWarpScale            :: Float
  , cpGenWarpStrength         :: Float
  , cpWorldExtentX            :: Float
  , cpWorldExtentY            :: Float
    -- Ocean edge depth
  , cpEdgeDepthNorth          :: Float
  , cpEdgeDepthSouth          :: Float
  , cpEdgeDepthEast           :: Float
  , cpEdgeDepthWest           :: Float
  , cpEdgeDepthFalloff        :: Float
    -- Tectonics
  , cpPlateSize               :: Float
  , cpPlateSpeed              :: Float
  , cpBoundarySharpness       :: Float
  , cpBoundaryNoiseScale      :: Float
  , cpBoundaryNoiseStrength   :: Float
  , cpBoundaryWarpOctaves     :: Float
  , cpBoundaryWarpLacunarity  :: Float
  , cpBoundaryWarpGain        :: Float
  , cpPlateMergeScale         :: Float
  , cpPlateMergeBias          :: Float
  , cpPlateDetailScale        :: Float
  , cpPlateDetailStrength     :: Float
  , cpPlateRidgeStrength      :: Float
  , cpPlateHeightBase         :: Float
  , cpPlateHeightVariance     :: Float
  , cpPlateHardnessBase       :: Float
  , cpPlateHardnessVariance   :: Float
  , cpUplift                  :: Float
  , cpRiftDepth               :: Float
  , cpTrenchDepth             :: Float
  , cpRidgeHeight             :: Float
  , cpDetailScale             :: Float
  , cpPlateBiasStrength       :: Float
  , cpPlateBiasCenter         :: Float
  , cpPlateBiasEdge           :: Float
  , cpPlateBiasNorth          :: Float
  , cpPlateBiasSouth          :: Float
    -- Weather
  , cpWeatherTick             :: Float
  , cpWeatherPhase            :: Float
  , cpWeatherAmplitude        :: Float
    -- Vegetation
  , cpVegBase                 :: Float
  , cpVegBoost                :: Float
  , cpVegTempWeight           :: Float
  , cpVegPrecipWeight         :: Float
    -- Planet
  , cpPlanetRadius            :: Float
  , cpAxialTilt               :: Float
  , cpInsolation              :: Float
    -- World slice
  , cpSliceLatCenter          :: Float
  , cpSliceLonCenter          :: Float
  } deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

-- | JSON field names strip the @cp@ prefix and lower-case the first letter.
-- E.g. @cpWaterLevel@ becomes @\"waterLevel\"@ in JSON.
presetJsonOptions :: Options
presetJsonOptions = defaultOptions
  { fieldLabelModifier = dropPrefix }
  where
    dropPrefix ('c':'p':c:rest) = toLowerFirst (c : rest)
    dropPrefix other            = other
    toLowerFirst []     = []
    toLowerFirst (c:cs) = toLower c : cs
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise             = c

instance ToJSON ConfigPreset where
  toJSON = genericToJSON presetJsonOptions

-- | All fields are optional in JSON — missing keys fall back to
-- 'defaultPreset' values, making old presets forward-compatible.
instance FromJSON ConfigPreset where
  parseJSON = withObject "ConfigPreset" $ \o -> do
    let d = defaultPreset
    ConfigPreset
      <$> o .:? "name"                    .!= cpName d
      <*> o .:? "version"                 .!= cpVersion d
      <*> o .:? "seed"                    .!= cpSeed d
      <*> o .:? "chunkSize"               .!= cpChunkSize d
      -- Hydrology
      <*> o .:? "waterLevel"              .!= cpWaterLevel d
      <*> o .:? "renderWaterLevel"        .!= cpRenderWaterLevel d
      -- Climate
      <*> o .:? "evaporation"             .!= cpEvaporation d
      <*> o .:? "rainShadow"              .!= cpRainShadow d
      <*> o .:? "windDiffuse"             .!= cpWindDiffuse d
      <*> o .:? "equatorTemp"             .!= cpEquatorTemp d
      <*> o .:? "poleTemp"                .!= cpPoleTemp d
      <*> o .:? "lapseRate"               .!= cpLapseRate d
      <*> o .:? "windIterations"          .!= cpWindIterations d
      <*> o .:? "moistureIterations"      .!= cpMoistureIterations d
      <*> o .:? "boundaryMotionTemp"      .!= cpBoundaryMotionTemp d
      <*> o .:? "boundaryMotionPrecip"    .!= cpBoundaryMotionPrecip d
      -- Erosion
      <*> o .:? "rainRate"                .!= cpRainRate d
      <*> o .:? "erosionHydraulic"        .!= cpErosionHydraulic d
      <*> o .:? "erosionThermal"          .!= cpErosionThermal d
      <*> o .:? "erosionTalus"            .!= cpErosionTalus d
      <*> o .:? "erosionMaxDrop"          .!= cpErosionMaxDrop d
      -- Generation
      <*> o .:? "genScale"                .!= cpGenScale d
      <*> o .:? "genCoordScale"           .!= cpGenCoordScale d
      <*> o .:? "genOffsetX"              .!= cpGenOffsetX d
      <*> o .:? "genOffsetY"              .!= cpGenOffsetY d
      <*> o .:? "genFrequency"            .!= cpGenFrequency d
      <*> o .:? "genOctaves"              .!= cpGenOctaves d
      <*> o .:? "genLacunarity"           .!= cpGenLacunarity d
      <*> o .:? "genGain"                 .!= cpGenGain d
      <*> o .:? "genWarpScale"            .!= cpGenWarpScale d
      <*> o .:? "genWarpStrength"         .!= cpGenWarpStrength d
      <*> o .:? "worldExtentX"            .!= cpWorldExtentX d
      <*> o .:? "worldExtentY"            .!= cpWorldExtentY d
      -- Ocean edge depth
      <*> o .:? "edgeDepthNorth"          .!= cpEdgeDepthNorth d
      <*> o .:? "edgeDepthSouth"          .!= cpEdgeDepthSouth d
      <*> o .:? "edgeDepthEast"           .!= cpEdgeDepthEast d
      <*> o .:? "edgeDepthWest"           .!= cpEdgeDepthWest d
      <*> o .:? "edgeDepthFalloff"        .!= cpEdgeDepthFalloff d
      -- Tectonics
      <*> o .:? "plateSize"               .!= cpPlateSize d
      <*> o .:? "plateSpeed"              .!= cpPlateSpeed d
      <*> o .:? "boundarySharpness"       .!= cpBoundarySharpness d
      <*> o .:? "boundaryNoiseScale"      .!= cpBoundaryNoiseScale d
      <*> o .:? "boundaryNoiseStrength"   .!= cpBoundaryNoiseStrength d
      <*> o .:? "boundaryWarpOctaves"     .!= cpBoundaryWarpOctaves d
      <*> o .:? "boundaryWarpLacunarity"  .!= cpBoundaryWarpLacunarity d
      <*> o .:? "boundaryWarpGain"        .!= cpBoundaryWarpGain d
      <*> o .:? "plateMergeScale"         .!= cpPlateMergeScale d
      <*> o .:? "plateMergeBias"          .!= cpPlateMergeBias d
      <*> o .:? "plateDetailScale"        .!= cpPlateDetailScale d
      <*> o .:? "plateDetailStrength"     .!= cpPlateDetailStrength d
      <*> o .:? "plateRidgeStrength"      .!= cpPlateRidgeStrength d
      <*> o .:? "plateHeightBase"         .!= cpPlateHeightBase d
      <*> o .:? "plateHeightVariance"     .!= cpPlateHeightVariance d
      <*> o .:? "plateHardnessBase"       .!= cpPlateHardnessBase d
      <*> o .:? "plateHardnessVariance"   .!= cpPlateHardnessVariance d
      <*> o .:? "uplift"                  .!= cpUplift d
      <*> o .:? "riftDepth"               .!= cpRiftDepth d
      <*> o .:? "trenchDepth"             .!= cpTrenchDepth d
      <*> o .:? "ridgeHeight"             .!= cpRidgeHeight d
      <*> o .:? "detailScale"             .!= cpDetailScale d
      <*> o .:? "plateBiasStrength"       .!= cpPlateBiasStrength d
      <*> o .:? "plateBiasCenter"         .!= cpPlateBiasCenter d
      <*> o .:? "plateBiasEdge"           .!= cpPlateBiasEdge d
      <*> o .:? "plateBiasNorth"          .!= cpPlateBiasNorth d
      <*> o .:? "plateBiasSouth"          .!= cpPlateBiasSouth d
      -- Weather
      <*> o .:? "weatherTick"             .!= cpWeatherTick d
      <*> o .:? "weatherPhase"            .!= cpWeatherPhase d
      <*> o .:? "weatherAmplitude"        .!= cpWeatherAmplitude d
      -- Vegetation
      <*> o .:? "vegBase"                 .!= cpVegBase d
      <*> o .:? "vegBoost"                .!= cpVegBoost d
      <*> o .:? "vegTempWeight"           .!= cpVegTempWeight d
      <*> o .:? "vegPrecipWeight"         .!= cpVegPrecipWeight d
      -- Planet
      <*> o .:? "planetRadius"            .!= cpPlanetRadius d
      <*> o .:? "axialTilt"               .!= cpAxialTilt d
      <*> o .:? "insolation"              .!= cpInsolation d
      -- World slice
      <*> o .:? "sliceLatCenter"          .!= cpSliceLatCenter d
      <*> o .:? "sliceLonCenter"          .!= cpSliceLonCenter d

-- ---------------------------------------------------------------------------
-- Defaults
-- ---------------------------------------------------------------------------

-- | Default preset with hardcoded values matching @emptyUiState@.
-- Kept in this cycle-free module so 'Actor.UI' can import the type
-- without pulling in the UI setter functions.
defaultPreset :: ConfigPreset
defaultPreset = ConfigPreset
  { cpName                    = "default"
  , cpVersion                 = currentPresetVersion
  , cpSeed                    = 0
  , cpChunkSize               = 64
  , cpWaterLevel              = 0.5
  , cpRenderWaterLevel        = 0.5
  , cpEvaporation             = 0.25
  , cpRainShadow              = 0.4
  , cpWindDiffuse             = 0.5
  , cpEquatorTemp             = 1
  , cpPoleTemp                = 0
  , cpLapseRate               = 0.25
  , cpWindIterations          = 0.5
  , cpMoistureIterations      = 0.5
  , cpBoundaryMotionTemp      = 0.5
  , cpBoundaryMotionPrecip    = 0.5
  , cpRainRate                = 0.2
  , cpErosionHydraulic        = 0.5
  , cpErosionThermal          = 0.4
  , cpErosionTalus            = 0.5
  , cpErosionMaxDrop          = 0.5
  , cpGenScale                = 0.4444
  , cpGenCoordScale           = 0.3333
  , cpGenOffsetX              = 0.5
  , cpGenOffsetY              = 0.5
  , cpGenFrequency            = 0.1837
  , cpGenOctaves              = 0.5
  , cpGenLacunarity           = 0.25
  , cpGenGain                 = 0.4
  , cpGenWarpScale            = 0.3333
  , cpGenWarpStrength         = 0.5556
  , cpWorldExtentX            = 0.125
  , cpWorldExtentY            = 0.125
  , cpEdgeDepthNorth          = 0
  , cpEdgeDepthSouth          = 0
  , cpEdgeDepthEast           = 0
  , cpEdgeDepthWest           = 0
  , cpEdgeDepthFalloff        = 0
  , cpPlateSize               = 0.45
  , cpPlateSpeed              = 0.38
  , cpBoundarySharpness       = 0.35
  , cpBoundaryNoiseScale      = 0.33
  , cpBoundaryNoiseStrength   = 0.45
  , cpBoundaryWarpOctaves     = 0.5
  , cpBoundaryWarpLacunarity  = 0.25
  , cpBoundaryWarpGain        = 0.4
  , cpPlateMergeScale         = 0.3
  , cpPlateMergeBias          = 0.44
  , cpPlateDetailScale        = 0.33
  , cpPlateDetailStrength     = 0.35
  , cpPlateRidgeStrength      = 0.25
  , cpPlateHeightBase         = 0.62
  , cpPlateHeightVariance     = 0.65
  , cpPlateHardnessBase       = 0.42
  , cpPlateHardnessVariance   = 0.4
  , cpUplift                  = 0.3
  , cpRiftDepth               = 0.35
  , cpTrenchDepth             = 0.38
  , cpRidgeHeight             = 0.33
  , cpDetailScale             = 0.5
  , cpPlateBiasStrength       = 0.42
  , cpPlateBiasCenter         = 0.5
  , cpPlateBiasEdge           = 0.5
  , cpPlateBiasNorth          = 0.5
  , cpPlateBiasSouth          = 0.5
  , cpWeatherTick             = 0.2
  , cpWeatherPhase            = 0
  , cpWeatherAmplitude        = 0.3
  , cpVegBase                 = 0.2
  , cpVegBoost                = 0.6
  , cpVegTempWeight           = 0.6
  , cpVegPrecipWeight         = 0.4
  , cpPlanetRadius            = 0.3333
  , cpAxialTilt               = 0.5209
  , cpInsolation              = 0.5
  , cpSliceLatCenter          = 0.5
  , cpSliceLonCenter          = 0.5
  }
