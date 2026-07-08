module Spec.DayNight (spec) where

import Test.Hspec

import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import qualified Data.IntMap.Strict as IntMap
import Topo.Calendar (WorldTime(..), defaultWorldTime)
import Topo.Hex (HexGridMeta(..))
import Topo.Overlay (emptyOverlayStore)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import UI.DayNight (mkDayNightFn, mkDayNightKey, mkDayNightSpec)

spec :: Spec
spec = describe "DayNight" $ do
  describe "DayNightKey" $ do
    let baseSnap = (emptyTerrainSnapshot 16)
          { tsGeoContext = defaultTerrainGeoContext
              { tgcPlanet = defaultPlanetConfig { pcRadius = 6371, pcAxialTilt = 23.44 }
              , tgcHexGrid = HexGridMeta 8
              , tgcSlice = defaultWorldSlice { wsLatCenter = 35, wsLonCenter = 0 }
              , tgcWorldTime = WorldTime 10 3600
              }
          }
        baseKey = mkDayNightKey baseSnap

    it "changes when authoritative world time changes" $
      mkDayNightKey (baseSnap { tsGeoContext = (tsGeoContext baseSnap) { tgcWorldTime = WorldTime 11 3600 } }) `shouldNotBe` baseKey

    it "changes when authoritative planet geometry changes" $
      mkDayNightKey (baseSnap { tsGeoContext = (tsGeoContext baseSnap) { tgcPlanet = defaultPlanetConfig { pcRadius = 7000, pcAxialTilt = 23.44 } } }) `shouldNotBe` baseKey

    it "changes when authoritative axial tilt changes" $
      mkDayNightKey (baseSnap { tsGeoContext = (tsGeoContext baseSnap) { tgcPlanet = defaultPlanetConfig { pcAxialTilt = 10 } } }) `shouldNotBe` baseKey

    it "changes when authoritative hex size changes" $
      mkDayNightKey (baseSnap { tsGeoContext = (tsGeoContext baseSnap) { tgcHexGrid = HexGridMeta 16 } }) `shouldNotBe` baseKey

    it "changes when authoritative slice center changes" $
      mkDayNightKey (baseSnap { tsGeoContext = (tsGeoContext baseSnap) { tgcSlice = defaultWorldSlice { wsLatCenter = 20, wsLonCenter = 30 } } }) `shouldNotBe` baseKey

    it "changes when terrain chunk size changes" $
      mkDayNightKey (baseSnap { tsChunkSize = 32 }) `shouldNotBe` baseKey

    it "is unavailable without a positive terrain chunk size" $ do
      mkDayNightKey (baseSnap { tsChunkSize = 0 }) `shouldBe` Nothing
      case mkDayNightSpec (baseSnap { tsChunkSize = 0 }) of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected no day/night spec for non-positive chunk size"

  it "builds mkDayNightFn from the same spec input path" $ do
    let snap = (emptyTerrainSnapshot 16)
          { tsGeoContext = defaultTerrainGeoContext
              { tgcWorldTime = WorldTime 24 3600
              , tgcPlanet = defaultPlanetConfig { pcAxialTilt = 27 }
              }
          }
    case (mkDayNightSpec snap, mkDayNightFn snap) of
      (Just (key, specFn), Just fn) -> do
        mkDayNightKey snap `shouldBe` Just key
        specFn 3 (-1) `shouldBe` fn 3 (-1)
      _ -> expectationFailure "expected day/night spec and function for positive chunk size"

  it "changes centre brightness between tick 0 midnight and tick 12 noon" $ do
    let centre = 8
        mkSnap tick = (emptyTerrainSnapshot 16)
          { tsGeoContext = defaultTerrainGeoContext
              { tgcWorldTime = WorldTime tick 3600
              , tgcPlanet = defaultPlanetConfig { pcAxialTilt = 0 }
              , tgcSlice = defaultWorldSlice { wsLatCenter = 0, wsLonCenter = 0 }
              }
          }
        Just nightFn = mkDayNightFn (mkSnap 0)
        Just noonFn = mkDayNightFn (mkSnap 12)
    noonFn centre centre `shouldSatisfy` (> nightFn centre centre)

  it "gives west/centre/east tiles physically distinct brightness near the terminator" $ do
    let centre = 8
        snap = (emptyTerrainSnapshot 16)
          { tsGeoContext = defaultTerrainGeoContext
              { tgcWorldTime = WorldTime 6 3600
              , tgcPlanet = defaultPlanetConfig { pcAxialTilt = 0 }
              , tgcSlice = defaultWorldSlice { wsLatCenter = 0, wsLonCenter = 0 }
              , tgcHexGrid = HexGridMeta 1000
              }
          }
        Just fn = mkDayNightFn snap
        west = fn (centre - 4) centre
        mid = fn centre centre
        east = fn (centre + 4) centre
    west `shouldSatisfy` (< mid)
    mid `shouldSatisfy` (< east)

emptyTerrainSnapshot :: Int -> TerrainSnapshot
emptyTerrainSnapshot chunkSize = TerrainSnapshot
  { tsVersion = 0
  , tsClimateVersion = 0
  , tsWeatherVersion = 0
  , tsVegetationVersion = 0
  , tsOverlayVersion = 0
  , tsChunkSize = chunkSize
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
  , tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = defaultWorldTime }
  }
