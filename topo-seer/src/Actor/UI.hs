{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.UI
  ( Ui
  , ConfigTab(..)
  , LeftTab(..)
  , ViewMode(..)
  , UiState(..)
  , emptyUiState
  , uiActorDef
  , setUiSeed
  , setUiGenerating
  , setUiViewMode
  , setUiChunkSize
  , setUiShowConfig
  , setUiConfigScroll
  , setUiShowLeftPanel
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
  , setUiConfigTab
  , setUiLeftTab
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
  , setUiPanOffset
  , setUiZoom
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
  , setUiLatitudeBias
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
  , setUiSliceLatExtent
  , setUiSliceLonCenter
  , setUiSliceLonExtent
  , setUiHoverHex
  , setUiHoverWidget
  , setUiShowMenu
  , setUiContextHex
  , setUiContextPos
  , setUiSeedEditing
  , setUiSeedInput
  , UiSnapshotReply
  , requestUiSnapshot
  , getUiSnapshot
  ) where

import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import UI.WidgetTree (WidgetId)

data ViewMode
  = ViewElevation
  | ViewBiome
  | ViewClimate
  | ViewMoisture
  | ViewPrecip
  | ViewPlateId
  | ViewPlateBoundary
  | ViewPlateHardness
  | ViewPlateCrust
  | ViewPlateAge
  | ViewPlateHeight
  | ViewPlateVelocity
  deriving (Eq, Show)

data ConfigTab
  = ConfigTerrain
  | ConfigClimate
  | ConfigErosion
  deriving (Eq, Show)

data LeftTab
  = LeftTopo
  | LeftView
  deriving (Eq, Show)

data UiState = UiState
  { uiSeed :: !Word64
  , uiGenerating :: !Bool
  , uiViewMode :: !ViewMode
  , uiChunkSize :: !Int
  , uiShowConfig :: !Bool
  , uiShowLeftPanel :: !Bool
  , uiConfigTab :: !ConfigTab
  , uiConfigScroll :: !Int
  , uiLeftTab :: !LeftTab
  , uiShowMenu :: !Bool
  , uiContextHex :: !(Maybe (Int, Int))
  , uiContextPos :: !(Maybe (Int, Int))
  , uiSeedEditing :: !Bool
  , uiSeedInput :: !Text
  , uiWaterLevel :: !Float
  , uiRenderWaterLevel :: !Float
  , uiEvaporation :: !Float
  , uiRainShadow :: !Float
  , uiWindDiffuse :: !Float
  , uiRainRate :: !Float
  , uiErosionHydraulic :: !Float
  , uiErosionThermal :: !Float
  , uiErosionTalus :: !Float
  , uiErosionMaxDrop :: !Float
  , uiEquatorTemp :: !Float
  , uiPoleTemp :: !Float
  , uiLapseRate :: !Float
  , uiGenScale :: !Float
  , uiGenCoordScale :: !Float
  , uiGenOffsetX :: !Float
  , uiGenOffsetY :: !Float
  , uiGenFrequency :: !Float
  , uiGenOctaves :: !Float
  , uiGenLacunarity :: !Float
  , uiGenGain :: !Float
  , uiGenWarpScale :: !Float
  , uiGenWarpStrength :: !Float
  , uiWorldExtentX :: !Float
  , uiWorldExtentY :: !Float
  , uiEdgeDepthNorth :: !Float
  , uiEdgeDepthSouth :: !Float
  , uiEdgeDepthEast :: !Float
  , uiEdgeDepthWest :: !Float
  , uiEdgeDepthFalloff :: !Float
  , uiPanOffset :: !(Float, Float)
  , uiZoom :: !Float
  , uiPlateSize :: !Float
  , uiPlateSpeed :: !Float
  , uiBoundarySharpness :: !Float
  , uiBoundaryNoiseScale :: !Float
  , uiBoundaryNoiseStrength :: !Float
  , uiBoundaryWarpOctaves :: !Float
  , uiBoundaryWarpLacunarity :: !Float
  , uiBoundaryWarpGain :: !Float
  , uiPlateMergeScale :: !Float
  , uiPlateMergeBias :: !Float
  , uiPlateDetailScale :: !Float
  , uiPlateDetailStrength :: !Float
  , uiPlateRidgeStrength :: !Float
  , uiPlateHeightBase :: !Float
  , uiPlateHeightVariance :: !Float
  , uiPlateHardnessBase :: !Float
  , uiPlateHardnessVariance :: !Float
  , uiUplift :: !Float
  , uiRiftDepth :: !Float
  , uiTrenchDepth :: !Float
  , uiRidgeHeight :: !Float
  , uiDetailScale :: !Float
  , uiPlateBiasStrength :: !Float
  , uiPlateBiasCenter :: !Float
  , uiPlateBiasEdge :: !Float
  , uiPlateBiasNorth :: !Float
  , uiPlateBiasSouth :: !Float
  , uiLatitudeBias :: !Float
  , uiWindIterations :: !Float
  , uiMoistureIterations :: !Float
  , uiBoundaryMotionTemp :: !Float
  , uiBoundaryMotionPrecip :: !Float
  , uiWeatherTick :: !Float
  , uiWeatherPhase :: !Float
  , uiWeatherAmplitude :: !Float
  , uiVegBase :: !Float
  , uiVegBoost :: !Float
  , uiVegTempWeight :: !Float
  , uiVegPrecipWeight :: !Float
  , uiPlanetRadius :: !Float
  , uiAxialTilt :: !Float
  , uiInsolation :: !Float
  , uiSliceLatCenter :: !Float
  , uiSliceLatExtent :: !Float
  , uiSliceLonCenter :: !Float
  , uiSliceLonExtent :: !Float
  , uiHoverHex :: !(Maybe (Int, Int))
  , uiHoverWidget :: !(Maybe WidgetId)
  } deriving (Eq, Show)

emptyUiState :: UiState
emptyUiState = UiState
  { uiSeed = 0
  , uiGenerating = False
  , uiViewMode = ViewElevation
  , uiChunkSize = 64
  , uiShowConfig = False
  , uiShowLeftPanel = True
  , uiConfigTab = ConfigTerrain
  , uiConfigScroll = 0
  , uiLeftTab = LeftTopo
  , uiShowMenu = False
  , uiContextHex = Nothing
  , uiContextPos = Nothing
  , uiSeedEditing = False
  , uiSeedInput = Text.empty
  , uiWaterLevel = 0.5
  , uiRenderWaterLevel = 0.5
  , uiEvaporation = 0.25
  , uiRainShadow = 0.4
  , uiWindDiffuse = 0.5
  , uiRainRate = 0.2
  , uiErosionHydraulic = 0.5
  , uiErosionThermal = 0.4
  , uiErosionTalus = 0.5
  , uiErosionMaxDrop = 0.5
  , uiEquatorTemp = 1
  , uiPoleTemp = 0
  , uiLapseRate = 0.25
  , uiGenScale = 0.4444
  , uiGenCoordScale = 0.3333
  , uiGenOffsetX = 0.5
  , uiGenOffsetY = 0.5
  , uiGenFrequency = 0.1837
  , uiGenOctaves = 0.5
  , uiGenLacunarity = 0.25
  , uiGenGain = 0.4
  , uiGenWarpScale = 0.3333
  , uiGenWarpStrength = 0.5556
  , uiWorldExtentX = 0.125
  , uiWorldExtentY = 0.125
  , uiEdgeDepthNorth = 0
  , uiEdgeDepthSouth = 0
  , uiEdgeDepthEast = 0
  , uiEdgeDepthWest = 0
  , uiEdgeDepthFalloff = 0
  , uiPanOffset = (0, 0)
  , uiZoom = 1
  , uiPlateSize = 0.45
  , uiPlateSpeed = 0.38
  , uiBoundarySharpness = 0.35
  , uiBoundaryNoiseScale = 0.33
  , uiBoundaryNoiseStrength = 0.45
  , uiBoundaryWarpOctaves = 0.5
  , uiBoundaryWarpLacunarity = 0.25
  , uiBoundaryWarpGain = 0.4
  , uiPlateMergeScale = 0.3
  , uiPlateMergeBias = 0.44
  , uiPlateDetailScale = 0.33
  , uiPlateDetailStrength = 0.35
  , uiPlateRidgeStrength = 0.25
  , uiPlateHeightBase = 0.62
  , uiPlateHeightVariance = 0.65
  , uiPlateHardnessBase = 0.42
  , uiPlateHardnessVariance = 0.4
  , uiUplift = 0.3
  , uiRiftDepth = 0.35
  , uiTrenchDepth = 0.38
  , uiRidgeHeight = 0.33
  , uiDetailScale = 0.5
  , uiPlateBiasStrength = 0.42
  , uiPlateBiasCenter = 0.5
  , uiPlateBiasEdge = 0.5
  , uiPlateBiasNorth = 0.5
  , uiPlateBiasSouth = 0.5
  , uiLatitudeBias = 0.5
  , uiWindIterations = 0.5
  , uiMoistureIterations = 0.5
  , uiBoundaryMotionTemp = 0.5
  , uiBoundaryMotionPrecip = 0.5
  , uiWeatherTick = 0.2
  , uiWeatherPhase = 0
  , uiWeatherAmplitude = 0.3
  , uiVegBase = 0.2
  , uiVegBoost = 0.6
  , uiVegTempWeight = 0.6
  , uiVegPrecipWeight = 0.4
  , uiPlanetRadius = 0.3333   -- maps to 6371 in [4778..9557]
  , uiAxialTilt = 0.5209      -- maps to 23.44 in [0..45]
  , uiInsolation = 0.5        -- maps to 1.0 in [0.7..1.3]
  , uiSliceLatCenter = 0.5    -- maps to 0 in [-90..90]
  , uiSliceLatExtent = 0.2222 -- maps to 40 in [0.1..180]
  , uiSliceLonCenter = 0.5    -- maps to 0 in [-180..180]
  , uiSliceLonExtent = 0.1666 -- maps to 60 in [0.1..360]
  , uiHoverHex = Nothing
  , uiHoverWidget = Nothing
  }

-- | Sum type encoding all possible UI state mutations.
--
-- Sent as a single cast message to the 'Ui' actor; the public API
-- ('setUiSeed', 'setUiGenerating', etc.) constructs the appropriate
-- constructor and forwards it.
data UiUpdate
  = SetSeed !Word64
  | SetGenerating !Bool
  | SetViewMode !ViewMode
  | SetChunkSize !Int
  | SetShowConfig !Bool
  | SetShowLeftPanel !Bool
  | SetConfigTab !ConfigTab
  | SetConfigScroll !Int
  | SetLeftTab !LeftTab
  | SetShowMenu !Bool
  | SetContextHex !(Maybe (Int, Int))
  | SetContextPos !(Maybe (Int, Int))
  | SetSeedEditing !Bool
  | SetSeedInput !Text
  | SetWaterLevel !Float
  | SetRenderWaterLevel !Float
  | SetEvaporation !Float
  | SetRainShadow !Float
  | SetWindDiffuse !Float
  | SetRainRate !Float
  | SetErosionHydraulic !Float
  | SetErosionThermal !Float
  | SetErosionTalus !Float
  | SetErosionMaxDrop !Float
  | SetEquatorTemp !Float
  | SetPoleTemp !Float
  | SetLapseRate !Float
  | SetGenScale !Float
  | SetGenCoordScale !Float
  | SetGenOffsetX !Float
  | SetGenOffsetY !Float
  | SetGenFrequency !Float
  | SetGenOctaves !Float
  | SetGenLacunarity !Float
  | SetGenGain !Float
  | SetGenWarpScale !Float
  | SetGenWarpStrength !Float
  | SetWorldExtentX !Float
  | SetWorldExtentY !Float
  | SetEdgeDepthNorth !Float
  | SetEdgeDepthSouth !Float
  | SetEdgeDepthEast !Float
  | SetEdgeDepthWest !Float
  | SetEdgeDepthFalloff !Float
  | SetPanOffset !(Float, Float)
  | SetZoom !Float
  | SetPlateSize !Float
  | SetPlateSpeed !Float
  | SetBoundarySharpness !Float
  | SetBoundaryNoiseScale !Float
  | SetBoundaryNoiseStrength !Float
  | SetBoundaryWarpOctaves !Float
  | SetBoundaryWarpLacunarity !Float
  | SetBoundaryWarpGain !Float
  | SetPlateMergeScale !Float
  | SetPlateMergeBias !Float
  | SetPlateDetailScale !Float
  | SetPlateDetailStrength !Float
  | SetPlateRidgeStrength !Float
  | SetPlateHeightBase !Float
  | SetPlateHeightVariance !Float
  | SetPlateHardnessBase !Float
  | SetPlateHardnessVariance !Float
  | SetUplift !Float
  | SetRiftDepth !Float
  | SetTrenchDepth !Float
  | SetRidgeHeight !Float
  | SetDetailScale !Float
  | SetPlateBiasStrength !Float
  | SetPlateBiasCenter !Float
  | SetPlateBiasEdge !Float
  | SetPlateBiasNorth !Float
  | SetPlateBiasSouth !Float
  | SetLatitudeBias !Float
  | SetWindIterations !Float
  | SetMoistureIterations !Float
  | SetBoundaryMotionTemp !Float
  | SetBoundaryMotionPrecip !Float
  | SetWeatherTick !Float
  | SetWeatherPhase !Float
  | SetWeatherAmplitude !Float
  | SetVegBase !Float
  | SetVegBoost !Float
  | SetVegTempWeight !Float
  | SetVegPrecipWeight !Float
  | SetPlanetRadius !Float
  | SetAxialTilt !Float
  | SetInsolation !Float
  | SetSliceLatCenter !Float
  | SetSliceLatExtent !Float
  | SetSliceLonCenter !Float
  | SetSliceLonExtent !Float
  | SetHoverHex !(Maybe (Int, Int))
  | SetHoverWidget !(Maybe WidgetId)

-- | Apply a 'UiUpdate' to the current 'UiState'.  Total and exhaustive.
applyUpdate :: UiUpdate -> UiState -> UiState
applyUpdate upd st = case upd of
  SetSeed v            -> st { uiSeed = v }
  SetGenerating v      -> st { uiGenerating = v }
  SetViewMode v        -> st { uiViewMode = v }
  SetChunkSize v       -> st { uiChunkSize = clampChunk v }
  SetShowConfig v      -> st { uiShowConfig = v }
  SetShowLeftPanel v   -> st { uiShowLeftPanel = v }
  SetConfigTab v       -> st { uiConfigTab = v }
  SetConfigScroll v    -> st { uiConfigScroll = max 0 v }
  SetLeftTab v         -> st { uiLeftTab = v }
  SetShowMenu v        -> st { uiShowMenu = v }
  SetContextHex v      -> st { uiContextHex = v }
  SetContextPos v      -> st { uiContextPos = v }
  SetSeedEditing v     -> st { uiSeedEditing = v }
  SetSeedInput v       -> st { uiSeedInput = v }
  SetWaterLevel v      -> st { uiWaterLevel = clamp01 v }
  SetRenderWaterLevel v -> st { uiRenderWaterLevel = clamp01 v }
  SetEvaporation v     -> st { uiEvaporation = clamp01 v }
  SetRainShadow v      -> st { uiRainShadow = clamp01 v }
  SetWindDiffuse v     -> st { uiWindDiffuse = clamp01 v }
  SetRainRate v        -> st { uiRainRate = clamp01 v }
  SetErosionHydraulic v -> st { uiErosionHydraulic = clamp01 v }
  SetErosionThermal v  -> st { uiErosionThermal = clamp01 v }
  SetErosionTalus v    -> st { uiErosionTalus = clamp01 v }
  SetErosionMaxDrop v  -> st { uiErosionMaxDrop = clamp01 v }
  SetEquatorTemp v     -> st { uiEquatorTemp = clamp01 v }
  SetPoleTemp v        -> st { uiPoleTemp = clamp01 v }
  SetLapseRate v       -> st { uiLapseRate = clamp01 v }
  SetGenScale v        -> st { uiGenScale = clamp01 v }
  SetGenCoordScale v   -> st { uiGenCoordScale = clamp01 v }
  SetGenOffsetX v      -> st { uiGenOffsetX = clamp01 v }
  SetGenOffsetY v      -> st { uiGenOffsetY = clamp01 v }
  SetGenFrequency v    -> st { uiGenFrequency = clamp01 v }
  SetGenOctaves v      -> st { uiGenOctaves = clamp01 v }
  SetGenLacunarity v   -> st { uiGenLacunarity = clamp01 v }
  SetGenGain v         -> st { uiGenGain = clamp01 v }
  SetGenWarpScale v    -> st { uiGenWarpScale = clamp01 v }
  SetGenWarpStrength v -> st { uiGenWarpStrength = clamp01 v }
  SetWorldExtentX v    -> st { uiWorldExtentX = clamp01 v }
  SetWorldExtentY v    -> st { uiWorldExtentY = clamp01 v }
  SetEdgeDepthNorth v  -> st { uiEdgeDepthNorth = clamp01 v }
  SetEdgeDepthSouth v  -> st { uiEdgeDepthSouth = clamp01 v }
  SetEdgeDepthEast v   -> st { uiEdgeDepthEast = clamp01 v }
  SetEdgeDepthWest v   -> st { uiEdgeDepthWest = clamp01 v }
  SetEdgeDepthFalloff v -> st { uiEdgeDepthFalloff = clamp01 v }
  SetPanOffset v       -> st { uiPanOffset = v }
  SetZoom v            -> st { uiZoom = clampZoom v }
  SetPlateSize v       -> st { uiPlateSize = clamp01 v }
  SetPlateSpeed v      -> st { uiPlateSpeed = clamp01 v }
  SetBoundarySharpness v -> st { uiBoundarySharpness = clamp01 v }
  SetBoundaryNoiseScale v -> st { uiBoundaryNoiseScale = clamp01 v }
  SetBoundaryNoiseStrength v -> st { uiBoundaryNoiseStrength = clamp01 v }
  SetBoundaryWarpOctaves v -> st { uiBoundaryWarpOctaves = clamp01 v }
  SetBoundaryWarpLacunarity v -> st { uiBoundaryWarpLacunarity = clamp01 v }
  SetBoundaryWarpGain v -> st { uiBoundaryWarpGain = clamp01 v }
  SetPlateMergeScale v -> st { uiPlateMergeScale = clamp01 v }
  SetPlateMergeBias v  -> st { uiPlateMergeBias = clamp01 v }
  SetPlateDetailScale v -> st { uiPlateDetailScale = clamp01 v }
  SetPlateDetailStrength v -> st { uiPlateDetailStrength = clamp01 v }
  SetPlateRidgeStrength v -> st { uiPlateRidgeStrength = clamp01 v }
  SetPlateHeightBase v -> st { uiPlateHeightBase = clamp01 v }
  SetPlateHeightVariance v -> st { uiPlateHeightVariance = clamp01 v }
  SetPlateHardnessBase v -> st { uiPlateHardnessBase = clamp01 v }
  SetPlateHardnessVariance v -> st { uiPlateHardnessVariance = clamp01 v }
  SetUplift v          -> st { uiUplift = clamp01 v }
  SetRiftDepth v       -> st { uiRiftDepth = clamp01 v }
  SetTrenchDepth v     -> st { uiTrenchDepth = clamp01 v }
  SetRidgeHeight v     -> st { uiRidgeHeight = clamp01 v }
  SetDetailScale v     -> st { uiDetailScale = clamp01 v }
  SetPlateBiasStrength v -> st { uiPlateBiasStrength = clamp01 v }
  SetPlateBiasCenter v -> st { uiPlateBiasCenter = clamp01 v }
  SetPlateBiasEdge v   -> st { uiPlateBiasEdge = clamp01 v }
  SetPlateBiasNorth v  -> st { uiPlateBiasNorth = clamp01 v }
  SetPlateBiasSouth v  -> st { uiPlateBiasSouth = clamp01 v }
  SetLatitudeBias v    -> st { uiLatitudeBias = clamp01 v }
  SetWindIterations v  -> st { uiWindIterations = clamp01 v }
  SetMoistureIterations v -> st { uiMoistureIterations = clamp01 v }
  SetBoundaryMotionTemp v -> st { uiBoundaryMotionTemp = clamp01 v }
  SetBoundaryMotionPrecip v -> st { uiBoundaryMotionPrecip = clamp01 v }
  SetWeatherTick v     -> st { uiWeatherTick = clamp01 v }
  SetWeatherPhase v    -> st { uiWeatherPhase = clamp01 v }
  SetWeatherAmplitude v -> st { uiWeatherAmplitude = clamp01 v }
  SetVegBase v         -> st { uiVegBase = clamp01 v }
  SetVegBoost v        -> st { uiVegBoost = clamp01 v }
  SetVegTempWeight v   -> st { uiVegTempWeight = clamp01 v }
  SetVegPrecipWeight v -> st { uiVegPrecipWeight = clamp01 v }
  SetPlanetRadius v    -> st { uiPlanetRadius = clamp01 v }
  SetAxialTilt v       -> st { uiAxialTilt = clamp01 v }
  SetInsolation v      -> st { uiInsolation = clamp01 v }
  SetSliceLatCenter v  -> st { uiSliceLatCenter = clamp01 v }
  SetSliceLatExtent v  -> st { uiSliceLatExtent = clamp01 v }
  SetSliceLonCenter v  -> st { uiSliceLonCenter = clamp01 v }
  SetSliceLonExtent v  -> st { uiSliceLonExtent = clamp01 v }
  SetHoverHex v        -> st { uiHoverHex = v }
  SetHoverWidget v     -> st { uiHoverWidget = v }

uiSnapshotTag :: OpTag "uiSnapshot"
uiSnapshotTag = OpTag

[hyperspace|
-- | Reply protocol for emitting UI snapshots.
replyprotocol UiSnapshotReply =
  cast uiSnapshot :: UiState

actor Ui
  state UiState
  lifetime Singleton
  schedule pinned 1
  noDeps
  mailbox Unbounded

  cast update :: UiUpdate
  cast snapshotAsync :: () reply UiSnapshotReply
  call snapshot :: () -> UiState

  initial emptyUiState
  onPure_ update = \upd st -> applyUpdate upd st
  onReply snapshotAsync = \() replyTo st -> do
    replyCast replyTo uiSnapshotTag st
    pure st
  onPure snapshot = \() st -> (st, st)
|]

setUiSeed :: ActorHandle Ui (Protocol Ui) -> Word64 -> IO ()
setUiSeed handle seed =
  cast @"update" handle #update (SetSeed seed)

setUiGenerating :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiGenerating handle flag =
  cast @"update" handle #update (SetGenerating flag)

setUiViewMode :: ActorHandle Ui (Protocol Ui) -> ViewMode -> IO ()
setUiViewMode handle mode =
  cast @"update" handle #update (SetViewMode mode)

setUiChunkSize :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiChunkSize handle size =
  cast @"update" handle #update (SetChunkSize size)

setUiShowConfig :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiShowConfig handle flag =
  cast @"update" handle #update (SetShowConfig flag)

setUiShowLeftPanel :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiShowLeftPanel handle flag =
  cast @"update" handle #update (SetShowLeftPanel flag)

setUiConfigTab :: ActorHandle Ui (Protocol Ui) -> ConfigTab -> IO ()
setUiConfigTab handle tab =
  cast @"update" handle #update (SetConfigTab tab)

setUiConfigScroll :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiConfigScroll handle newScroll =
  cast @"update" handle #update (SetConfigScroll newScroll)

setUiLeftTab :: ActorHandle Ui (Protocol Ui) -> LeftTab -> IO ()
setUiLeftTab handle tab =
  cast @"update" handle #update (SetLeftTab tab)

setUiShowMenu :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiShowMenu handle flag =
  cast @"update" handle #update (SetShowMenu flag)

setUiContextHex :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiContextHex handle hex =
  cast @"update" handle #update (SetContextHex hex)

setUiContextPos :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiContextPos handle pos =
  cast @"update" handle #update (SetContextPos pos)

setUiSeedEditing :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiSeedEditing handle flag =
  cast @"update" handle #update (SetSeedEditing flag)

setUiSeedInput :: ActorHandle Ui (Protocol Ui) -> Text -> IO ()
setUiSeedInput handle input =
  cast @"update" handle #update (SetSeedInput input)

setUiWaterLevel :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWaterLevel handle value =
  cast @"update" handle #update (SetWaterLevel value)

-- | Commit the render water level (used in 'AtlasKey').
--
-- Only called on Apply\/Generate to avoid atlas invalidation on +/- clicks.
setUiRenderWaterLevel :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRenderWaterLevel handle value =
  cast @"update" handle #update (SetRenderWaterLevel value)

setUiEvaporation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEvaporation handle value =
  cast @"update" handle #update (SetEvaporation value)

setUiRainShadow :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRainShadow handle value =
  cast @"update" handle #update (SetRainShadow value)

setUiWindDiffuse :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindDiffuse handle value =
  cast @"update" handle #update (SetWindDiffuse value)

setUiRainRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRainRate handle value =
  cast @"update" handle #update (SetRainRate value)

setUiErosionHydraulic :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionHydraulic handle value =
  cast @"update" handle #update (SetErosionHydraulic value)

setUiErosionThermal :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionThermal handle value =
  cast @"update" handle #update (SetErosionThermal value)

setUiErosionTalus :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionTalus handle value =
  cast @"update" handle #update (SetErosionTalus value)

setUiErosionMaxDrop :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionMaxDrop handle value =
  cast @"update" handle #update (SetErosionMaxDrop value)

setUiEquatorTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEquatorTemp handle value =
  cast @"update" handle #update (SetEquatorTemp value)

setUiPoleTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPoleTemp handle value =
  cast @"update" handle #update (SetPoleTemp value)

setUiLapseRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiLapseRate handle value =
  cast @"update" handle #update (SetLapseRate value)

setUiGenScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenScale handle value =
  cast @"update" handle #update (SetGenScale value)

setUiGenCoordScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenCoordScale handle value =
  cast @"update" handle #update (SetGenCoordScale value)

setUiGenOffsetX :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOffsetX handle value =
  cast @"update" handle #update (SetGenOffsetX value)

setUiGenOffsetY :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOffsetY handle value =
  cast @"update" handle #update (SetGenOffsetY value)

setUiGenFrequency :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenFrequency handle value =
  cast @"update" handle #update (SetGenFrequency value)

setUiGenOctaves :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOctaves handle value =
  cast @"update" handle #update (SetGenOctaves value)

setUiGenLacunarity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenLacunarity handle value =
  cast @"update" handle #update (SetGenLacunarity value)

setUiGenGain :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenGain handle value =
  cast @"update" handle #update (SetGenGain value)

setUiGenWarpScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenWarpScale handle value =
  cast @"update" handle #update (SetGenWarpScale value)

setUiGenWarpStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenWarpStrength handle value =
  cast @"update" handle #update (SetGenWarpStrength value)

setUiWorldExtentX :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWorldExtentX handle value =
  cast @"update" handle #update (SetWorldExtentX value)

setUiWorldExtentY :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWorldExtentY handle value =
  cast @"update" handle #update (SetWorldExtentY value)

setUiEdgeDepthNorth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthNorth handle value =
  cast @"update" handle #update (SetEdgeDepthNorth value)

setUiEdgeDepthSouth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthSouth handle value =
  cast @"update" handle #update (SetEdgeDepthSouth value)

setUiEdgeDepthEast :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthEast handle value =
  cast @"update" handle #update (SetEdgeDepthEast value)

setUiEdgeDepthWest :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthWest handle value =
  cast @"update" handle #update (SetEdgeDepthWest value)

setUiEdgeDepthFalloff :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthFalloff handle value =
  cast @"update" handle #update (SetEdgeDepthFalloff value)

setUiPanOffset :: ActorHandle Ui (Protocol Ui) -> (Float, Float) -> IO ()
setUiPanOffset handle value =
  cast @"update" handle #update (SetPanOffset value)

setUiZoom :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiZoom handle value =
  cast @"update" handle #update (SetZoom value)

setUiPlateSize :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateSize handle value =
  cast @"update" handle #update (SetPlateSize value)

setUiPlateSpeed :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateSpeed handle value =
  cast @"update" handle #update (SetPlateSpeed value)

setUiBoundarySharpness :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundarySharpness handle value =
  cast @"update" handle #update (SetBoundarySharpness value)

setUiBoundaryNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryNoiseScale handle value =
  cast @"update" handle #update (SetBoundaryNoiseScale value)

setUiBoundaryNoiseStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryNoiseStrength handle value =
  cast @"update" handle #update (SetBoundaryNoiseStrength value)

setUiBoundaryWarpOctaves :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpOctaves handle value =
  cast @"update" handle #update (SetBoundaryWarpOctaves value)

setUiBoundaryWarpLacunarity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpLacunarity handle value =
  cast @"update" handle #update (SetBoundaryWarpLacunarity value)

setUiBoundaryWarpGain :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpGain handle value =
  cast @"update" handle #update (SetBoundaryWarpGain value)

setUiPlateMergeScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateMergeScale handle value =
  cast @"update" handle #update (SetPlateMergeScale value)

setUiPlateMergeBias :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateMergeBias handle value =
  cast @"update" handle #update (SetPlateMergeBias value)

setUiPlateDetailScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateDetailScale handle value =
  cast @"update" handle #update (SetPlateDetailScale value)

setUiPlateDetailStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateDetailStrength handle value =
  cast @"update" handle #update (SetPlateDetailStrength value)

setUiPlateRidgeStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateRidgeStrength handle value =
  cast @"update" handle #update (SetPlateRidgeStrength value)

setUiPlateHeightBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHeightBase handle value =
  cast @"update" handle #update (SetPlateHeightBase value)

setUiPlateHeightVariance :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHeightVariance handle value =
  cast @"update" handle #update (SetPlateHeightVariance value)

setUiPlateHardnessBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHardnessBase handle value =
  cast @"update" handle #update (SetPlateHardnessBase value)

setUiPlateHardnessVariance :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHardnessVariance handle value =
  cast @"update" handle #update (SetPlateHardnessVariance value)

setUiUplift :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiUplift handle value =
  cast @"update" handle #update (SetUplift value)

setUiRiftDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRiftDepth handle value =
  cast @"update" handle #update (SetRiftDepth value)

setUiTrenchDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTrenchDepth handle value =
  cast @"update" handle #update (SetTrenchDepth value)

setUiRidgeHeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRidgeHeight handle value =
  cast @"update" handle #update (SetRidgeHeight value)

setUiDetailScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiDetailScale handle value =
  cast @"update" handle #update (SetDetailScale value)

setUiPlateBiasStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasStrength handle value =
  cast @"update" handle #update (SetPlateBiasStrength value)

setUiPlateBiasCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasCenter handle value =
  cast @"update" handle #update (SetPlateBiasCenter value)

setUiPlateBiasEdge :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasEdge handle value =
  cast @"update" handle #update (SetPlateBiasEdge value)

setUiPlateBiasNorth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasNorth handle value =
  cast @"update" handle #update (SetPlateBiasNorth value)

setUiPlateBiasSouth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasSouth handle value =
  cast @"update" handle #update (SetPlateBiasSouth value)

setUiLatitudeBias :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiLatitudeBias handle value =
  cast @"update" handle #update (SetLatitudeBias value)

setUiWindIterations :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindIterations handle value =
  cast @"update" handle #update (SetWindIterations value)

setUiMoistureIterations :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistureIterations handle value =
  cast @"update" handle #update (SetMoistureIterations value)

setUiBoundaryMotionTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryMotionTemp handle value =
  cast @"update" handle #update (SetBoundaryMotionTemp value)

setUiBoundaryMotionPrecip :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryMotionPrecip handle value =
  cast @"update" handle #update (SetBoundaryMotionPrecip value)

setUiWeatherTick :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherTick handle value =
  cast @"update" handle #update (SetWeatherTick value)

setUiWeatherPhase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherPhase handle value =
  cast @"update" handle #update (SetWeatherPhase value)

setUiWeatherAmplitude :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherAmplitude handle value =
  cast @"update" handle #update (SetWeatherAmplitude value)

setUiVegBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegBase handle value =
  cast @"update" handle #update (SetVegBase value)

setUiVegBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegBoost handle value =
  cast @"update" handle #update (SetVegBoost value)

setUiVegTempWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegTempWeight handle value =
  cast @"update" handle #update (SetVegTempWeight value)

setUiVegPrecipWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegPrecipWeight handle value =
  cast @"update" handle #update (SetVegPrecipWeight value)

setUiPlanetRadius :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlanetRadius handle value =
  cast @"update" handle #update (SetPlanetRadius value)

setUiAxialTilt :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiAxialTilt handle value =
  cast @"update" handle #update (SetAxialTilt value)

setUiInsolation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiInsolation handle value =
  cast @"update" handle #update (SetInsolation value)

setUiSliceLatCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLatCenter handle value =
  cast @"update" handle #update (SetSliceLatCenter value)

setUiSliceLatExtent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLatExtent handle value =
  cast @"update" handle #update (SetSliceLatExtent value)

setUiSliceLonCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLonCenter handle value =
  cast @"update" handle #update (SetSliceLonCenter value)

setUiSliceLonExtent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLonExtent handle value =
  cast @"update" handle #update (SetSliceLonExtent value)

setUiHoverHex :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiHoverHex handle hex =
  cast @"update" handle #update (SetHoverHex hex)

-- | Set the currently hovered widget (for tooltip display).
setUiHoverWidget :: ActorHandle Ui (Protocol Ui) -> Maybe WidgetId -> IO ()
setUiHoverWidget handle wid =
  cast @"update" handle #update (SetHoverWidget wid)

getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
getUiSnapshot handle =
  call @"snapshot" handle #snapshot ()

-- | Request a UI snapshot via a reply-capable cast.
requestUiSnapshot :: ActorHandle Ui (Protocol Ui) -> ReplyTo UiSnapshotReply -> IO ()
requestUiSnapshot handle replyTo =
  castReply @"snapshotAsync" handle replyTo #snapshotAsync ()

clampChunk :: Int -> Int
clampChunk size =
  let minSize = 8
      maxSize = 256
  in max minSize (min maxSize size)

clamp01 :: Float -> Float
clamp01 value =
  max 0 (min 1 value)

clampZoom :: Float -> Float
clampZoom value =
  max 0.4 (min 3 value)
