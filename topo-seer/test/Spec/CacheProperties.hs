module Spec.CacheProperties (spec) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Seer.Render.Atlas (zoomTextureScale)
import Seer.Render.Terrain (TerrainCache(..), buildTerrainCache, updateTerrainCache)
import Test.Hspec
import Test.QuickCheck

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

  it "invalidates terrain cache when water level changes" $
    property $ \(Positive chunkSize) (NonNegative levelA) (NonNegative levelB) ->
      levelA /= levelB ==>
        let terrainSnap = emptyTerrainSnapshot { tsChunkSize = chunkSize }
            uiA = emptyUiState { uiWaterLevel = levelA }
            uiB = emptyUiState { uiWaterLevel = levelB }
            cacheA = buildTerrainCache uiA terrainSnap
            updated = updateTerrainCache uiB terrainSnap cacheA
        in not (sameTerrainCache updated cacheA)

  it "clamps atlas scale between 1 and 6" $
    property $ \(NonNegative zoom) ->
      let scale = zoomTextureScale zoom
      in scale >= 1 && scale <= 6

  it "is monotone with respect to zoom" $
    property $ \(NonNegative z1) (NonNegative z2) ->
      let scale1 = zoomTextureScale z1
          scale2 = zoomTextureScale z2
      in if z1 <= z2 then scale1 <= scale2 else scale1 >= scale2

emptyTerrainSnapshot :: TerrainSnapshot
emptyTerrainSnapshot = TerrainSnapshot
  { tsVersion = 0
  , tsChunkSize = 0
  , tsTerrainChunks = IntMap.empty
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
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
  , ViewMoisture
  , ViewPrecip
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
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
