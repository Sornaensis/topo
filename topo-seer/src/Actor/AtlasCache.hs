-- | Atlas cache key type shared across the atlas pipeline.
--
-- Base terrain tiles and sky/weather overlay tiles are keyed independently so
-- weather ticks, overlay basis changes, and plugin overlay refreshes do not
-- invalidate unchanged base terrain atlas tiles.  Day/night remains a separate
-- top overlay (see 'Seer.Render.Atlas').
module Actor.AtlasCache
  ( AtlasLayer(..)
  , AtlasKey(..)
  , atlasKeyLayer
  , atlasKeyIsBase
  , atlasKeyVersion
  , atlasKeyDataVersion
  , atlasKeySelectionTag
  , atlasKeyViewMode
  , atlasKeyWaterLevel
  , terrainSnapshotBaseVersion
  , terrainSnapshotOverlayVersion
  , terrainSnapshotViewVersion
  , terrainSnapshotSelectionVersion
  , atlasKeyFor
  , atlasBaseKeyForSelection
  , atlasOverlayKeyForSelection
  , atlasKeysForSelection
  , atlasKeyForSelection
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , baseViewModeFromViewMode
  , baseViewModeToViewMode
  , layeredViewStateToViewMode
  , legacyViewModeToLayeredViewState
  , skyOverlayModeFromViewMode
  , skyOverlayModeToViewMode
  )
import Data.Bits (xor)
import Data.Word (Word64)
import GHC.Float (castFloatToWord32)
import qualified Data.Text as Text

-- | Which independently-scheduled atlas layer a key targets.
data AtlasLayer
  = AtlasBaseLayer
  | AtlasOverlayLayer
  | AtlasLegacyLayer
  deriving (Eq, Ord, Show)

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- New render paths use 'BaseAtlasKey' and 'OverlayAtlasKey'.  The legacy
-- constructors remain accepted for older tests and callers that still build a
-- single pre-composited atlas, but layered scheduling no longer depends on
-- overlay/weather stamps in base keys.
data AtlasKey
  = BaseAtlasKey !BaseViewMode !Float !Word64
  | OverlayAtlasKey !SkyOverlayMode !WeatherBasis !Bool !Word64
    -- ^ Overlay mode, temporal basis, legacy-opaque rendering policy, and the
    -- layer data version.  Opacity is intentionally not part of the key: the
    -- render loop applies it as a draw-time alpha multiplier.
  | AtlasKey !ViewMode !Float !Word64
  | LayeredAtlasKey !ViewMode !Float !Word64 !Word64 !Word64
  deriving (Eq, Ord, Show)

atlasKeyLayer :: AtlasKey -> AtlasLayer
atlasKeyLayer BaseAtlasKey{} = AtlasBaseLayer
atlasKeyLayer OverlayAtlasKey{} = AtlasOverlayLayer
atlasKeyLayer _ = AtlasLegacyLayer

atlasKeyIsBase :: AtlasKey -> Bool
atlasKeyIsBase key = atlasKeyLayer key == AtlasBaseLayer || atlasKeyLayer key == AtlasLegacyLayer

-- | Extract the layer-specific version from an 'AtlasKey'.
atlasKeyVersion :: AtlasKey -> Word64
atlasKeyVersion (BaseAtlasKey _ _ v) = v
atlasKeyVersion (OverlayAtlasKey _ _ _ v) = v
atlasKeyVersion (AtlasKey _ _ v) = v
atlasKeyVersion (LayeredAtlasKey _ _ v _ _) = v

-- | Extract the monotonically comparable data-version component from an atlas
-- key. Explicit layered keys keep this separate from their selection tag so
-- manager freshness comparisons do not infer structure from arbitrary legacy
-- version bits.
atlasKeyDataVersion :: AtlasKey -> Word64
atlasKeyDataVersion (BaseAtlasKey _ _ v) = v
atlasKeyDataVersion (OverlayAtlasKey _ _ _ v) = v
atlasKeyDataVersion (AtlasKey _ _ v) = v
atlasKeyDataVersion (LayeredAtlasKey _ _ _ dataVersion _) = dataVersion

-- | Stable tag for the key family used by latest-wins queue comparisons.
atlasKeySelectionTag :: AtlasKey -> Word64
atlasKeySelectionTag (BaseAtlasKey base _ _) = baseFingerprint base
atlasKeySelectionTag (OverlayAtlasKey overlay basis legacyOpaque _) =
  foldl mix64 3337565984
    [ overlayFingerprint overlay
    , weatherBasisFingerprint basis
    , if legacyOpaque then 1 else 2
    ]
atlasKeySelectionTag AtlasKey{} = 0
atlasKeySelectionTag (LayeredAtlasKey _ _ _ _ selectionTag') = selectionTag'

atlasKeyViewMode :: AtlasKey -> ViewMode
atlasKeyViewMode (BaseAtlasKey base _ _) = baseViewModeToViewMode base
atlasKeyViewMode (OverlayAtlasKey overlay basis _ _) =
  maybe ViewElevation id (skyOverlayModeToViewMode basis overlay)
atlasKeyViewMode (AtlasKey mode _ _) = mode
atlasKeyViewMode (LayeredAtlasKey mode _ _ _ _) = mode

atlasKeyWaterLevel :: AtlasKey -> Float
atlasKeyWaterLevel (BaseAtlasKey _ waterLevel _) = waterLevel
atlasKeyWaterLevel OverlayAtlasKey{} = 0
atlasKeyWaterLevel (AtlasKey _ waterLevel _) = waterLevel
atlasKeyWaterLevel (LayeredAtlasKey _ waterLevel _ _ _) = waterLevel

-- | Version stamp for base terrain atlas layers.
terrainSnapshotBaseVersion :: BaseViewMode -> TerrainSnapshot -> Word64
terrainSnapshotBaseVersion base terrainSnap = case base of
  BaseViewVegetation -> max (tsVersion terrainSnap) (tsVegetationVersion terrainSnap)
  _ -> tsVersion terrainSnap

-- | Version stamp for sky/weather overlay atlas layers.
terrainSnapshotOverlayVersion :: SkyOverlayMode -> WeatherBasis -> TerrainSnapshot -> Word64
terrainSnapshotOverlayVersion overlay basis terrainSnap = case overlay of
  SkyOverlayWeatherTemperature -> basisVersion basis
  SkyOverlayPrecipitation -> basisVersion basis
  SkyOverlayCloud -> case basis of
    WeatherBasisAverage -> max (tsClimateVersion terrainSnap) (tsOverlayVersion terrainSnap)
    WeatherBasisCurrent -> tsWeatherVersion terrainSnap
  SkyOverlayPlugin{} -> tsOverlayVersion terrainSnap
  where
    basisVersion WeatherBasisAverage = tsClimateVersion terrainSnap
    basisVersion WeatherBasisCurrent = tsWeatherVersion terrainSnap

-- | Choose the version stamp relevant to a legacy view mode.
--
-- This is now a layer selector rather than a pre-composited selector:
-- base modes return base versions, current weather modes return
-- 'tsWeatherVersion', average climate modes return 'tsClimateVersion', and
-- plugin overlays return 'tsOverlayVersion'.
terrainSnapshotViewVersion :: ViewMode -> TerrainSnapshot -> Word64
terrainSnapshotViewVersion mode terrainSnap =
  case baseViewModeFromViewMode mode of
    Just base -> terrainSnapshotBaseVersion base terrainSnap
    Nothing -> case skyOverlayModeFromViewMode mode of
      Just (overlay, basis) -> terrainSnapshotOverlayVersion overlay basis terrainSnap
      Nothing -> tsVersion terrainSnap

-- | Version stamp for an explicit layered selection used by non-render-target
-- fallback geometry and compatibility code that still stores a composited
-- terrain cache.  It intentionally combines base, overlay, opacity, and
-- selection identity; render-target atlas scheduling uses the split base and
-- overlay keys above instead.
terrainSnapshotSelectionVersion :: LayeredViewState -> TerrainSnapshot -> Word64
terrainSnapshotSelectionVersion selection terrainSnap =
  selectionVersionSalt selection (layeredDataVersion selection terrainSnap)

selectionIsLegacyEquivalent :: LayeredViewState -> Bool
selectionIsLegacyEquivalent selection =
  case layeredViewStateToViewMode selection of
    Just legacyMode -> selection == legacyViewModeToLayeredViewState legacyMode
    Nothing -> False

layeredDataVersion :: LayeredViewState -> TerrainSnapshot -> Word64
layeredDataVersion selection terrainSnap =
  let baseVersion = terrainSnapshotBaseVersion (lvsBaseView selection) terrainSnap
      overlayVersion = case lvsSkyOverlay selection of
        Nothing -> 0
        Just overlay -> terrainSnapshotOverlayVersion overlay (lvsWeatherBasis selection) terrainSnap
  in mix64 baseVersion overlayVersion

selectionVersionSalt :: LayeredViewState -> Word64 -> Word64
selectionVersionSalt selection dataVersion =
  mix64 dataVersion (selectionTag selection)

selectionFingerprint :: LayeredViewState -> Word64
selectionFingerprint selection =
  foldl mix64 1469598103934665603
    [ baseFingerprint (lvsBaseView selection)
    , maybe 0 overlayFingerprint (lvsSkyOverlay selection)
    , weatherBasisFingerprint (lvsWeatherBasis selection)
    , fromIntegral (castFloatToWord32 (clamp01 (lvsOverlayOpacity selection)))
    ]

baseFingerprint :: BaseViewMode -> Word64
baseFingerprint base = case base of
  BaseViewElevation -> 1
  BaseViewBiome -> 2
  BaseViewMoisture -> 3
  BaseViewPlateId -> 4
  BaseViewPlateBoundary -> 5
  BaseViewPlateHardness -> 6
  BaseViewPlateCrust -> 7
  BaseViewPlateAge -> 8
  BaseViewPlateHeight -> 9
  BaseViewPlateVelocity -> 10
  BaseViewVegetation -> 11
  BaseViewTerrainForm -> 12

overlayFingerprint :: SkyOverlayMode -> Word64
overlayFingerprint overlay = case overlay of
  SkyOverlayWeatherTemperature -> 101
  SkyOverlayPrecipitation -> 102
  SkyOverlayCloud -> 103
  SkyOverlayPlugin name fieldIdx ->
    foldl mix64 104 [textFingerprint name, fromIntegral fieldIdx]

weatherBasisFingerprint :: WeatherBasis -> Word64
weatherBasisFingerprint WeatherBasisAverage = 201
weatherBasisFingerprint WeatherBasisCurrent = 202

textFingerprint :: Text.Text -> Word64
textFingerprint = Text.foldl' (\h ch -> mix64 h (fromIntegral (fromEnum ch))) 2166136261

selectionTag :: LayeredViewState -> Word64
selectionTag selection =
  let tag = selectionFingerprint selection
  in if tag == 0 then 1 else tag

mix64 :: Word64 -> Word64 -> Word64
mix64 a b = (a `xor` b) * 1099511628211

clamp01 :: Float -> Float
clamp01 value = max 0 (min 1 value)

-- | Build the split base key for a layered selection.
atlasBaseKeyForSelection :: LayeredViewState -> Float -> TerrainSnapshot -> AtlasKey
atlasBaseKeyForSelection selection waterLevel terrainSnap =
  BaseAtlasKey (lvsBaseView selection) waterLevel (terrainSnapshotBaseVersion (lvsBaseView selection) terrainSnap)

-- | Build the split overlay key for a layered selection, if any.
atlasOverlayKeyForSelection :: LayeredViewState -> TerrainSnapshot -> Maybe AtlasKey
atlasOverlayKeyForSelection selection terrainSnap = do
  overlay <- lvsSkyOverlay selection
  let basis = lvsWeatherBasis selection
      legacyOpaque = selectionIsLegacyEquivalent selection
      version = terrainSnapshotOverlayVersion overlay basis terrainSnap
  pure (OverlayAtlasKey overlay basis legacyOpaque version)

-- | All atlas keys needed to draw a layered selection, ordered base first and
-- overlay second.  This order gives the visible base stage first chance at
-- limited worker capacity while still allowing overlay-only refreshes to be
-- scheduled independently when the base key is unchanged.
atlasKeysForSelection :: LayeredViewState -> Float -> TerrainSnapshot -> [AtlasKey]
atlasKeysForSelection selection waterLevel terrainSnap =
  atlasBaseKeyForSelection selection waterLevel terrainSnap
    : maybe [] (:[]) (atlasOverlayKeyForSelection selection terrainSnap)

-- | Compatibility key for a legacy view mode.  Base modes produce base-layer
-- keys; weather/climate/plugin modes produce legacy-opaque overlay-layer keys.
atlasKeyFor :: ViewMode -> Float -> TerrainSnapshot -> AtlasKey
atlasKeyFor mode waterLevel terrainSnap =
  case baseViewModeFromViewMode mode of
    Just base -> BaseAtlasKey base waterLevel (terrainSnapshotBaseVersion base terrainSnap)
    Nothing -> case skyOverlayModeFromViewMode mode of
      Just (overlay, basis) ->
        OverlayAtlasKey overlay basis True (terrainSnapshotOverlayVersion overlay basis terrainSnap)
      Nothing -> AtlasKey mode waterLevel (terrainSnapshotViewVersion mode terrainSnap)

-- | Compatibility selector for callers that still expect a single key for a
-- layered selection.  It returns the overlay key when an overlay is active and
-- the base key otherwise.  New render/scheduler code should use
-- 'atlasKeysForSelection' (or the base/overlay selectors) instead.
atlasKeyForSelection :: LayeredViewState -> Float -> TerrainSnapshot -> AtlasKey
atlasKeyForSelection selection waterLevel terrainSnap =
  case atlasOverlayKeyForSelection selection terrainSnap of
    Just overlayKey -> overlayKey
    Nothing -> atlasBaseKeyForSelection selection waterLevel terrainSnap
