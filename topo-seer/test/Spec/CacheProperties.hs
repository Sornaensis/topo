module Spec.CacheProperties (spec) where

import Actor.Data (TerrainSnapshot(..))
import Topo (WeatherChunk(..), WorldConfig(..), emptyTerrainChunk)
import Topo.Overlay (emptyOverlayStore)
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Data.IntMap.Strict (IntMap)
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
  { ctcViewMode = tcViewMode cache
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
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
  , ViewVegetation
  ]

sameTerrainCache :: TerrainCache -> TerrainCache -> Bool
sameTerrainCache left right =
  tcVersion left == tcVersion right
    && tcViewMode left == tcViewMode right
    && tcWaterLevel left == tcWaterLevel right
    && tcChunkSize left == tcChunkSize right
    && tcTerrainChunks left == tcTerrainChunks right
    && tcClimateChunks left == tcClimateChunks right
    && tcWeatherChunks left == tcWeatherChunks right
    && tcGeometry left == tcGeometry right
