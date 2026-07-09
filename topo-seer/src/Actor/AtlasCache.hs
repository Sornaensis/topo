-- | Atlas cache key type shared across the atlas pipeline.
--
-- Base atlas tiles are day\/night-agnostic: the brightness overlay is
-- stored and drawn as a separate layer (see 'Seer.Render.Atlas').
-- Removing the @Bool@ from the key means that switching day\/night
-- does not invalidate view-mode tiles, effectively doubling cache
-- capacity.
--
-- With the multi-key 'Seer.Render.Atlas.AtlasTextureCache', switching
-- any component of the key is an O(1) pointer update — tiles for the
-- previous key remain cached and are available instantly if the user
-- switches back.
module Actor.AtlasCache
  ( AtlasKey(..)
  , atlasKeyVersion
  , atlasKeyDataVersion
  , atlasKeySelectionTag
  , atlasKeyViewMode
  , atlasKeyWaterLevel
  , terrainSnapshotViewVersion
  , terrainSnapshotSelectionVersion
  , atlasKeyFor
  , atlasKeyForSelection
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (BaseViewMode(..), LayeredViewState(..), SkyOverlayMode(..), ViewMode(..), WeatherBasis(..), baseViewModeToViewMode, layeredViewStateToViewMode, legacyViewModeToLayeredViewState, skyOverlayModeToViewMode)
import Data.Bits (xor)
import Data.Word (Word64)
import GHC.Float (castFloatToWord32)
import qualified Data.Text as Text
import Topo.Overlay (Overlay(..), OverlayProvenance(..), lookupOverlay)
import Topo.Weather (weatherNormalsOverlayName)

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- Stores the view-specific data version stamp rather than the full snapshot,
-- avoiding deep structural equality on every frame.
--
-- Legacy fields: @ViewMode@, @waterLevel@, @viewDataVersion@.
-- Explicit layered fields additionally carry comparable data version and
-- selection tag so queue freshness never has to infer them from the raw stamp.
data AtlasKey
  = AtlasKey !ViewMode !Float !Word64
  | LayeredAtlasKey !ViewMode !Float !Word64 !Word64 !Word64
  deriving (Eq, Ord, Show)

-- | Extract the view-specific version from an 'AtlasKey'.
atlasKeyVersion :: AtlasKey -> Word64
atlasKeyVersion (AtlasKey _ _ v) = v
atlasKeyVersion (LayeredAtlasKey _ _ v _ _) = v

-- | Extract the monotonically comparable data-version component from an atlas
-- key.  Explicit layered keys keep this separate from their selection tag so
-- manager freshness comparisons do not infer structure from arbitrary legacy
-- version bits.
atlasKeyDataVersion :: AtlasKey -> Word64
atlasKeyDataVersion (AtlasKey _ _ v) = v
atlasKeyDataVersion (LayeredAtlasKey _ _ _ dataVersion _) = dataVersion

-- | Stable tag for the base/overlay/opacity part of a key.  Legacy keys use 0;
-- explicit layered keys use a non-zero tag so atlas-manager stale/future
-- comparisons do not treat different compositions of the same legacy 'ViewMode'
-- as the same key family.
atlasKeySelectionTag :: AtlasKey -> Word64
atlasKeySelectionTag AtlasKey{} = 0
atlasKeySelectionTag (LayeredAtlasKey _ _ _ _ selectionTag) = selectionTag

atlasKeyViewMode :: AtlasKey -> ViewMode
atlasKeyViewMode (AtlasKey mode _ _) = mode
atlasKeyViewMode (LayeredAtlasKey mode _ _ _ _) = mode

atlasKeyWaterLevel :: AtlasKey -> Float
atlasKeyWaterLevel (AtlasKey _ waterLevel _) = waterLevel
atlasKeyWaterLevel (LayeredAtlasKey _ waterLevel _ _ _) = waterLevel

-- | Choose the version stamp relevant to a view mode.
--
-- Layer-specific views depend on the base terrain chunk layout plus their own
-- data layer.  All layer stamps come from one monotonic counter, so 'max'
-- changes when either dependency changes while overlay/weather ticks leave
-- elevation/biome keys stable.
terrainSnapshotViewVersion :: ViewMode -> TerrainSnapshot -> Word64
terrainSnapshotViewVersion mode terrainSnap = case mode of
  ViewClimate      -> max (tsVersion terrainSnap) (tsClimateVersion terrainSnap)
  ViewPrecip       -> max (tsVersion terrainSnap) (tsClimateVersion terrainSnap)
  ViewPrecipCurrent -> max (tsVersion terrainSnap) (tsWeatherVersion terrainSnap)
  ViewWeather      -> max (tsVersion terrainSnap) (tsWeatherVersion terrainSnap)
  ViewCloud        -> max (tsVersion terrainSnap) (tsWeatherVersion terrainSnap)
  ViewCloudTypical -> weatherNormalsViewVersion terrainSnap
  ViewVegetation   -> max (tsVersion terrainSnap) (tsVegetationVersion terrainSnap)
  ViewOverlay{}    -> max (tsVersion terrainSnap) (tsOverlayVersion terrainSnap)
  _                -> tsVersion terrainSnap

weatherNormalsViewVersion :: TerrainSnapshot -> Word64
weatherNormalsViewVersion terrainSnap =
  let base = max (tsVersion terrainSnap) (tsClimateVersion terrainSnap)
      -- Use the weather_normals overlay's own provenance stamp so current
      -- weather overlay ticks do not invalidate typical-normal atlas keys.
      normalsStamp = case lookupOverlay weatherNormalsOverlayName (tsOverlayStore terrainSnap) of
        Nothing -> 0
        Just overlay ->
          let provenance = ovProvenance overlay
          in 1 + opSeed provenance * 16777619 + fromIntegral (opVersion provenance)
  in base * 16777619 + normalsStamp

-- | Version stamp for an explicit layered selection.
--
-- Legacy adapter-equivalent selections deliberately keep the old per-'ViewMode'
-- version so existing keys and weather-tick invalidation behaviour remain
-- stable.  Truly layered selections salt the relevant data-layer version with
-- the base/overlay/opacity choice so base or opacity changes cannot reuse an
-- atlas built for another composition.
terrainSnapshotSelectionVersion :: LayeredViewState -> TerrainSnapshot -> Word64
terrainSnapshotSelectionVersion selection terrainSnap =
  case lvsSkyOverlay selection of
    Nothing -> terrainSnapshotViewVersion (baseViewModeToViewMode (lvsBaseView selection)) terrainSnap
    Just _
      | selectionIsLegacyEquivalent selection ->
          terrainSnapshotViewVersion (legacyModeForSelection selection) terrainSnap
      | otherwise -> selectionVersionSalt selection (layeredDataVersion selection terrainSnap)

selectionIsLegacyEquivalent :: LayeredViewState -> Bool
selectionIsLegacyEquivalent selection =
  case layeredViewStateToViewMode selection of
    Just legacyMode -> selection == legacyViewModeToLayeredViewState legacyMode
    Nothing -> False

legacyModeForSelection :: LayeredViewState -> ViewMode
legacyModeForSelection selection =
  case layeredViewStateToViewMode selection of
    Just legacyMode -> legacyMode
    Nothing -> baseViewModeToViewMode (lvsBaseView selection)

layeredDataVersion :: LayeredViewState -> TerrainSnapshot -> Word64
layeredDataVersion selection terrainSnap =
  let baseVersion = terrainSnapshotViewVersion (baseViewModeToViewMode (lvsBaseView selection)) terrainSnap
      overlayVersion = case lvsSkyOverlay selection >>= skyOverlayModeToViewMode (lvsWeatherBasis selection) of
        Nothing -> 0
        Just overlayMode -> terrainSnapshotViewVersion overlayMode terrainSnap
  in max baseVersion overlayVersion

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

atlasKeyFor :: ViewMode -> Float -> TerrainSnapshot -> AtlasKey
atlasKeyFor mode waterLevel terrainSnap =
  AtlasKey mode waterLevel (terrainSnapshotViewVersion mode terrainSnap)

atlasKeyForSelection :: LayeredViewState -> Float -> TerrainSnapshot -> AtlasKey
atlasKeyForSelection selection waterLevel terrainSnap =
  let mode = case layeredViewStateToViewMode selection of
        Just legacyMode -> legacyMode
        Nothing -> baseViewModeToViewMode (lvsBaseView selection)
      keyVersion = terrainSnapshotSelectionVersion selection terrainSnap
  in if lvsSkyOverlay selection == Nothing || selectionIsLegacyEquivalent selection
      then AtlasKey mode waterLevel keyVersion
      else
        let dataVersion = layeredDataVersion selection terrainSnap
        in LayeredAtlasKey mode waterLevel keyVersion dataVersion (selectionTag selection)
