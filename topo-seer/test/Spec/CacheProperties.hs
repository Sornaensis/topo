module Spec.CacheProperties (spec) where

import Actor.AtlasCache (AtlasKey(..), atlasKeyFor, atlasKeyVersion, terrainSnapshotSelectionVersion, terrainSnapshotViewVersion)
import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.TerrainCacheWorker (TerrainCacheKey(..), terrainCacheKeyFrom)
import Actor.UI (BaseViewMode(..), LayeredViewState(..), SkyOverlayMode(..), UiState(..), ViewMode(..), WeatherBasis(..), defaultLayeredViewState, effectiveViewSelection, emptyUiState)
import Topo (WeatherChunk(..), WorldConfig(..), emptyTerrainChunk)
import Topo.Calendar (WorldTime(..), simulationTickSeconds)
import Topo.Overlay (Overlay(..), OverlayProvenance(..), OverlayStore, emptyOverlay, emptyOverlayStore, insertOverlay)
import Topo.Weather (weatherNormalsOverlaySchema)
import Data.IntMap.Strict (IntMap)
import qualified Data.Text as Text
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Foreign.Ptr (Ptr, intPtrToPtr)
import Linear (V2(..))
import qualified SDL
import Seer.Render.Atlas (zoomTextureScale)
import Seer.Render.Terrain
  ( TerrainCache(..)
  , buildTerrainCache
  , chunkTextureCacheNeedsUpdate
  , fallbackTerrainNeedsRefresh
  , terrainCacheNeedsRefresh
  , updateTerrainCache
  )
import Test.Hspec
import Test.QuickCheck
import UI.TerrainCache (ChunkTextureCache(..))
import UI.TerrainRender (ChunkTexture(..))
import UI.Widgets (Rect(..))
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec
spec = describe "Cache properties" $ do
  it "keeps terrain cache stable for identical inputs" $
    property $ \(Positive chunkSize) ->
      let uiSnap = emptyUiState
          terrainSnap = emptyTerrainSnapshot { tsChunkSize = chunkSize }
          cache = buildTerrainCache uiSnap terrainSnap
          updated = updateTerrainCache uiSnap terrainSnap cache
      in sameTerrainCache updated cache

  it "invalidates terrain cache when view mode changes" $
    property $ \(Positive chunkSize) ->
      forAll distinctViewModes $ \(modeA, modeB) ->
        let terrainSnap = emptyTerrainSnapshot { tsChunkSize = chunkSize }
            uiA = emptyUiState { uiViewMode = modeA }
            uiB = emptyUiState { uiViewMode = modeB }
            cacheA = buildTerrainCache uiA terrainSnap
            updated = updateTerrainCache uiB terrainSnap cacheA
        in not (sameTerrainCache updated cacheA)

  it "invalidates terrain cache when switching to or from weather view" $
    property $ \(Positive chunkSize) ->
      let terrainSnap = emptyTerrainSnapshot { tsChunkSize = chunkSize }
          uiWeather = emptyUiState { uiViewMode = ViewWeather }
          uiElevation = emptyUiState { uiViewMode = ViewElevation }
          cacheWeather = buildTerrainCache uiWeather terrainSnap
          updatedFromWeather = updateTerrainCache uiElevation terrainSnap cacheWeather
          cacheElevation = buildTerrainCache uiElevation terrainSnap
          updatedToWeather = updateTerrainCache uiWeather terrainSnap cacheElevation
      in not (sameTerrainCache updatedFromWeather cacheWeather)
         && not (sameTerrainCache updatedToWeather cacheElevation)

  it "uses weather versions for ViewCloud atlas and terrain cache keys" $ do
    let waterLevel = 0.4
        terrainSnap0 = emptyTerrainSnapshot { tsVersion = 3, tsWeatherVersion = 7 }
        terrainSnap1 = terrainSnap0 { tsWeatherVersion = 8 }
        terrainSnapBaseNewer = terrainSnap0 { tsVersion = 11 }
        cloudKey0 = atlasKeyFor ViewCloud waterLevel terrainSnap0
    terrainSnapshotViewVersion ViewCloud terrainSnap0 `shouldBe` 7
    atlasKeyVersion cloudKey0 `shouldBe` 7
    terrainSnapshotViewVersion ViewCloud terrainSnap1 `shouldBe` 8
    atlasKeyFor ViewCloud waterLevel terrainSnap1 `shouldNotBe` cloudKey0
    terrainSnapshotViewVersion ViewCloud terrainSnapBaseNewer `shouldBe` 7
    atlasKeyVersion (atlasKeyFor ViewCloud waterLevel terrainSnapBaseNewer) `shouldBe` 7

  it "keys async fallback terrain builds by layered selection" $ do
    let terrainSnap = renderableTerrainSnapshot 3 7 sampleWeatherChunkA
        selectionA = defaultLayeredViewState
          { lvsBaseView = BaseViewBiome
          , lvsSkyOverlay = Just SkyOverlayWeatherTemperature
          , lvsWeatherBasis = WeatherBasisCurrent
          , lvsOverlayOpacity = 0.25
          }
        selectionB = selectionA { lvsOverlayOpacity = 0.75 }
        uiA = emptyUiState { uiViewMode = ViewWeather, uiViewSelection = selectionA }
        uiB = emptyUiState { uiViewMode = ViewWeather, uiViewSelection = selectionB }
    fmap tckViewSelection (terrainCacheKeyFrom uiA terrainSnap) `shouldBe` Just selectionA
    terrainCacheKeyFrom uiA terrainSnap `shouldNotBe` terrainCacheKeyFrom uiB terrainSnap

  it "uses weather versions for current precipitation atlas keys" $ do
    let waterLevel = 0.4
        terrainSnap0 = emptyTerrainSnapshot { tsVersion = 3, tsWeatherVersion = 7 }
        terrainSnap1 = terrainSnap0 { tsWeatherVersion = 8 }
        precipKey0 = atlasKeyFor ViewPrecipCurrent waterLevel terrainSnap0
    terrainSnapshotViewVersion ViewPrecipCurrent terrainSnap0 `shouldBe` 7
    atlasKeyVersion precipKey0 `shouldBe` 7
    atlasKeyFor ViewPrecipCurrent waterLevel terrainSnap1 `shouldNotBe` precipKey0

  it "keeps typical cloud atlas keys stable across weather ticks but tracks climate and normal overlay changes" $ do
    let waterLevel = 0.4
        terrainSnap0 = emptyTerrainSnapshot { tsVersion = 3, tsClimateVersion = 5, tsWeatherVersion = 7 }
        terrainSnapWeather = terrainSnap0 { tsWeatherVersion = 8 }
        terrainSnapClimate = terrainSnap0 { tsClimateVersion = 6 }
        terrainSnapNormals0 = terrainSnap0 { tsOverlayVersion = 9, tsOverlayStore = weatherNormalsStore 1 }
        terrainSnapNormals1 = terrainSnap0 { tsOverlayVersion = 10, tsOverlayStore = weatherNormalsStore 2 }
        typicalKey0 = atlasKeyFor ViewCloudTypical waterLevel terrainSnap0
        typicalNormalsKey0 = atlasKeyFor ViewCloudTypical waterLevel terrainSnapNormals0
    terrainSnapshotViewVersion ViewCloudTypical terrainSnapWeather `shouldBe` terrainSnapshotViewVersion ViewCloudTypical terrainSnap0
    atlasKeyFor ViewCloudTypical waterLevel terrainSnapWeather `shouldBe` typicalKey0
    terrainSnapshotViewVersion ViewCloudTypical terrainSnapClimate `shouldNotBe` terrainSnapshotViewVersion ViewCloudTypical terrainSnap0
    atlasKeyFor ViewCloudTypical waterLevel terrainSnapClimate `shouldNotBe` typicalKey0
    terrainSnapshotViewVersion ViewCloudTypical terrainSnapNormals1 `shouldNotBe` terrainSnapshotViewVersion ViewCloudTypical terrainSnapNormals0
    atlasKeyFor ViewCloudTypical waterLevel terrainSnapNormals1 `shouldNotBe` typicalNormalsKey0

  it "keeps split base atlas keys stable when only current weather changes" $ do
    let waterLevel = 0.4
        selection = defaultLayeredViewState
          { lvsBaseView = BaseViewBiome
          , lvsSkyOverlay = Just SkyOverlayCloud
          , lvsWeatherBasis = WeatherBasisCurrent
          }
        terrainSnap0 = emptyTerrainSnapshot { tsVersion = 3, tsWeatherVersion = 7 }
        terrainSnapWeather = terrainSnap0 { tsWeatherVersion = 8 }
        terrainSnapBase = terrainSnap0 { tsVersion = 4 }
        baseKey0 = BaseAtlasKey (lvsBaseView selection) waterLevel (tsVersion terrainSnap0)
        overlayKey0 = atlasKeyFor ViewCloud waterLevel terrainSnap0
    BaseAtlasKey (lvsBaseView selection) waterLevel (tsVersion terrainSnapWeather) `shouldBe` baseKey0
    atlasKeyFor ViewCloud waterLevel terrainSnapWeather `shouldNotBe` overlayKey0
    BaseAtlasKey (lvsBaseView selection) waterLevel (tsVersion terrainSnapBase) `shouldNotBe` baseKey0

  it "refreshes ViewCloud terrain cache when weather version changes" $ do
    let uiCloud = emptyUiState { uiViewMode = ViewCloud }
        terrainSnap0 = renderableTerrainSnapshot 1 3 sampleWeatherChunkA
        terrainSnap1 = renderableTerrainSnapshot 1 4 sampleWeatherChunkB
        cache0 = buildTerrainCache uiCloud terrainSnap0
        cache1 = updateTerrainCache uiCloud terrainSnap1 cache0
    tcVersion cache0 `shouldBe` terrainSnapshotSelectionVersion (effectiveViewSelection uiCloud) terrainSnap0
    terrainCacheNeedsRefresh uiCloud terrainSnap1 cache0 `shouldBe` True
    tcVersion cache1 `shouldBe` terrainSnapshotSelectionVersion (effectiveViewSelection uiCloud) terrainSnap1
    sameTerrainCache cache1 cache0 `shouldBe` False

  it "invalidates terrain cache when water level changes" $
    property $ \(Positive chunkSize) (NonNegative levelA) (NonNegative levelB) ->
      levelA /= levelB ==>
        let terrainSnap = emptyTerrainSnapshot { tsChunkSize = chunkSize }
            uiA = emptyUiState { uiRenderWaterLevel = levelA }
            uiB = emptyUiState { uiRenderWaterLevel = levelB }
            cacheA = buildTerrainCache uiA terrainSnap
            updated = updateTerrainCache uiB terrainSnap cacheA
        in not (sameTerrainCache updated cacheA)

  it "keeps fallback ViewCloud frames incomplete after weather auto-ticks until terrain cache refreshes" $ do
    let uiCloud = emptyUiState { uiViewMode = ViewCloud }
        terrainSnap0 = renderableTerrainSnapshot 1 1 sampleWeatherChunkA
        terrainSnap1 = renderableTerrainSnapshot 1 2 sampleWeatherChunkB
        oldCache = buildTerrainCache uiCloud terrainSnap0
        oldTextures = chunkTexturesFor 1 oldCache
    terrainCacheNeedsRefresh uiCloud terrainSnap1 oldCache `shouldBe` True
    fallbackTerrainNeedsRefresh uiCloud terrainSnap1 1 oldCache oldTextures `shouldBe` True

  it "keeps fallback ViewWeather frames incomplete after worker results until chunk textures refresh" $ do
    let uiWeather = emptyUiState { uiViewMode = ViewWeather }
        terrainSnap0 = renderableTerrainSnapshot 1 1 sampleWeatherChunkA
        terrainSnap1 = renderableTerrainSnapshot 1 2 sampleWeatherChunkB
        oldCache = buildTerrainCache uiWeather terrainSnap0
        freshCache = buildTerrainCache uiWeather terrainSnap1
        oldTextures = chunkTexturesFor 1 oldCache
        freshTextures = chunkTexturesFor 1 freshCache
    terrainCacheNeedsRefresh uiWeather terrainSnap1 freshCache `shouldBe` False
    chunkTextureCacheNeedsUpdate freshCache 1 oldTextures `shouldBe` True
    fallbackTerrainNeedsRefresh uiWeather terrainSnap1 1 freshCache oldTextures `shouldBe` True
    fallbackTerrainNeedsRefresh uiWeather terrainSnap1 1 freshCache freshTextures `shouldBe` False

  it "refreshes fallback day/night overlays without rebuilding base chunk textures" $ do
    let terrainSnap0 = (renderableTerrainSnapshot 1 1 sampleWeatherChunkA)
          { tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = WorldTime 0 simulationTickSeconds }
          }
        terrainSnap1 = terrainSnap0
          { tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = WorldTime 1 simulationTickSeconds }
          }
        uiOff = emptyUiState { uiDayNightEnabled = False, uiSimTickCount = 0 }
        uiOn = emptyUiState { uiDayNightEnabled = True, uiSimTickCount = 0 }
        cacheOff = buildTerrainCache uiOff terrainSnap0
        cacheOn0 = buildTerrainCache uiOn terrainSnap0
        cacheOn1 = buildTerrainCache uiOn terrainSnap1
        texturesOn0 = chunkTexturesFor 1 cacheOn0
    tcDayNightKey cacheOff `shouldBe` Nothing
    tcDayNightGeometry cacheOff `shouldBe` IntMap.empty
    tcDayNightKey cacheOn0 `shouldNotBe` Nothing
    IntMap.keysSet (tcDayNightGeometry cacheOn0) `shouldBe` IntMap.keysSet (tcGeometry cacheOn0)
    terrainCacheNeedsRefresh uiOn terrainSnap0 cacheOff `shouldBe` True
    terrainCacheNeedsRefresh uiOff terrainSnap0 cacheOn0 `shouldBe` True
    terrainCacheNeedsRefresh uiOn terrainSnap0 cacheOn0 `shouldBe` False
    terrainCacheNeedsRefresh uiOn terrainSnap1 cacheOn0 `shouldBe` True
    fallbackTerrainNeedsRefresh uiOn terrainSnap1 1 cacheOn0 texturesOn0 `shouldBe` True
    terrainCacheNeedsRefresh uiOn terrainSnap1 cacheOn1 `shouldBe` False
    chunkTextureCacheNeedsUpdate cacheOn1 1 texturesOn0 `shouldBe` False
    fallbackTerrainNeedsRefresh uiOn terrainSnap1 1 cacheOn1 texturesOn0 `shouldBe` False
    case (terrainCacheKeyFrom uiOff terrainSnap0, terrainCacheKeyFrom uiOn terrainSnap0, terrainCacheKeyFrom uiOn terrainSnap1) of
      (Just keyOff, Just keyOn0, Just keyOn1) -> do
        tckDayNightKey keyOff `shouldBe` Nothing
        tckDayNightKey keyOn0 `shouldBe` tcDayNightKey cacheOn0
        tckDayNightKey keyOn1 `shouldBe` tcDayNightKey cacheOn1
        keyOn1 `shouldNotBe` keyOn0
      _ -> expectationFailure "expected terrain cache keys for renderable terrain"

  it "clamps atlas scale between 1 and 6" $
    property $ \(NonNegative zoom) ->
      let scale = zoomTextureScale zoom
      in scale >= 1 && scale <= 6

  it "is monotone with respect to zoom" $
    property $ \(NonNegative z1) (NonNegative z2) ->
      let scale1 = zoomTextureScale z1
          scale2 = zoomTextureScale z2
      in if z1 <= z2 then scale1 <= scale2 else scale1 >= scale2

sampleWorldConfig :: WorldConfig
sampleWorldConfig = WorldConfig { wcChunkSize = sampleChunkSize }

sampleChunkSize :: Int
sampleChunkSize = 1

renderableTerrainSnapshot :: Int -> Int -> WeatherChunk -> TerrainSnapshot
renderableTerrainSnapshot baseVersion weatherVersion weatherChunk = emptyTerrainSnapshot
  { tsVersion = fromIntegral baseVersion
  , tsWeatherVersion = fromIntegral weatherVersion
  , tsChunkSize = sampleChunkSize
  , tsTerrainChunks = IntMap.singleton 0 (emptyTerrainChunk sampleWorldConfig)
  , tsWeatherChunks = IntMap.singleton 0 weatherChunk
  }

sampleWeatherChunkA :: WeatherChunk
sampleWeatherChunkA = sampleWeatherChunk 0.25

sampleWeatherChunkB :: WeatherChunk
sampleWeatherChunkB = sampleWeatherChunk 0.75

sampleWeatherChunk :: Float -> WeatherChunk
sampleWeatherChunk value = WeatherChunk
  { wcTemp = vals
  , wcHumidity = vals
  , wcWindDir = vals
  , wcWindSpd = vals
  , wcPressure = vals
  , wcPrecip = vals
  , wcCloudCover = vals
  , wcCloudWater = vals
  , wcCloudCoverLow = vals
  , wcCloudCoverMid = vals
  , wcCloudCoverHigh = vals
  , wcCloudWaterLow = vals
  , wcCloudWaterMid = vals
  , wcCloudWaterHigh = vals
  }
  where
    vals = U.replicate sampleChunkSize value

chunkTexturesFor :: Int -> TerrainCache -> ChunkTextureCache
chunkTexturesFor scale cache = ChunkTextureCache
  { ctcVersion = tcVersion cache
  , ctcViewMode = tcViewMode cache
  , ctcViewSelection = tcViewSelection cache
  , ctcWaterLevel = tcWaterLevel cache
  , ctcChunkSize = tcChunkSize cache
  , ctcScale = scale
  , ctcTerrainChunks = tcTerrainChunks cache
  , ctcClimateChunks = tcClimateChunks cache
  , ctcWeatherChunks = tcWeatherChunks cache
  , ctcTextures = IntMap.mapWithKey (\key _ -> mockChunkTexture key) (tcGeometry cache)
  }

mockChunkTexture :: Int -> ChunkTexture
mockChunkTexture key = ChunkTexture
  { ctTexture = mockTexture key
  , ctBounds = Rect (V2 0 0, V2 1 1)
  }

mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral (max 1 n)) :: Ptr ())

weatherNormalsStore :: Word32 -> OverlayStore
weatherNormalsStore version =
  insertOverlay normalsOverlay emptyOverlayStore
  where
    normalsOverlay = (emptyOverlay weatherNormalsOverlaySchema)
      { ovProvenance = OverlayProvenance
          { opSeed = 42
          , opVersion = version
          , opSource = Text.pack "weather_normals"
          , opSchedule = Nothing
          }
      }

emptyTerrainSnapshot :: TerrainSnapshot
emptyTerrainSnapshot = TerrainSnapshot
  { tsVersion = 0
  , tsClimateVersion = 0
  , tsWeatherVersion = 0
  , tsVegetationVersion = 0
  , tsOverlayVersion = 0
  , tsChunkSize = 0
  , tsTerrainChunks = IntMap.empty
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
  , tsRiverChunks = IntMap.empty
  , tsGroundwaterChunks = IntMap.empty
  , tsVolcanismChunks = IntMap.empty
  , tsGlacierChunks = IntMap.empty
  , tsWaterBodyChunks = IntMap.empty
  , tsVegetationChunks = IntMap.empty
  , tsOverlayStore = emptyOverlayStore
  , tsGeoContext = defaultTerrainGeoContext
  }

distinctViewModes :: Gen (ViewMode, ViewMode)
distinctViewModes = do
  modeA <- elements viewModes
  modeB <- elements (filter (/= modeA) viewModes)
  pure (modeA, modeB)

viewModes :: [ViewMode]
viewModes =
  [ ViewElevation
  , ViewBiome
  , ViewClimate
  , ViewWeather
  , ViewMoisture
  , ViewPrecip
  , ViewPrecipCurrent
  , ViewCloudTypical
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
  , ViewVegetation
  , ViewCloud
  ]

sameTerrainCache :: TerrainCache -> TerrainCache -> Bool
sameTerrainCache left right =
  tcVersion left == tcVersion right
    && tcViewMode left == tcViewMode right
    && tcWaterLevel left == tcWaterLevel right
    && tcDayNightEnabled left == tcDayNightEnabled right
    && tcDayNightKey left == tcDayNightKey right
    && tcChunkSize left == tcChunkSize right
    && tcTerrainChunks left == tcTerrainChunks right
    && tcClimateChunks left == tcClimateChunks right
    && tcWeatherChunks left == tcWeatherChunks right
    && tcGeometry left == tcGeometry right
    && tcDayNightGeometry left == tcDayNightGeometry right
