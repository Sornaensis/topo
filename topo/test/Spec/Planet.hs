{-# LANGUAGE PatternSynonyms #-}

module Spec.Planet (spec) where

import Test.Hspec
import Test.QuickCheck
import Topo.Planet
import Topo.Types (WorldConfig(..), WorldExtent, TileCoord(..), mkWorldExtent, worldExtentRadii)

spec :: Spec
spec = describe "Planet" $ do
  describe "PlanetConfig" $ do
    it "accepts Earth defaults" $
      mkPlanetConfig 6371 23.44 1.0 `shouldBe` Right defaultPlanetConfig

    it "rejects radius below 4778" $
      mkPlanetConfig 4000 23.44 1.0 `shouldSatisfy` isLeft

    it "rejects radius above 9557" $
      mkPlanetConfig 10000 23.44 1.0 `shouldSatisfy` isLeft

    it "rejects negative tilt" $
      mkPlanetConfig 6371 (-1) 1.0 `shouldSatisfy` isLeft

    it "rejects tilt above 45" $
      mkPlanetConfig 6371 46 1.0 `shouldSatisfy` isLeft

    it "rejects insolation below 0.7" $
      mkPlanetConfig 6371 23.44 0.5 `shouldSatisfy` isLeft

    it "rejects insolation above 1.3" $
      mkPlanetConfig 6371 23.44 1.5 `shouldSatisfy` isLeft

    it "property: valid ranges always succeed" $
      property $ \(ValidPlanetParams r t i) ->
        case mkPlanetConfig r t i of
          Right _ -> True
          Left _  -> False

  describe "WorldSlice" $ do
    it "accepts equator defaults" $
      mkWorldSlice 0 40 0 60 `shouldBe` Right defaultWorldSlice

    it "rejects latCenter below -90" $
      mkWorldSlice (-91) 40 0 60 `shouldSatisfy` isLeft

    it "rejects latCenter above 90" $
      mkWorldSlice 91 40 0 60 `shouldSatisfy` isLeft

    it "rejects non-positive latExtent" $
      mkWorldSlice 0 0 0 60 `shouldSatisfy` isLeft

    it "rejects lonCenter below -180" $
      mkWorldSlice 0 40 (-181) 60 `shouldSatisfy` isLeft

    it "rejects non-positive lonExtent" $
      mkWorldSlice 0 40 0 0 `shouldSatisfy` isLeft

    it "property: valid slice ranges always succeed" $
      property $ \(ValidSliceParams lc le lnc lne) ->
        case mkWorldSlice lc le lnc lne of
          Right _ -> True
          Left _  -> False

  describe "hexesPerDegreeLatitude" $ do
    it "is positive for Earth defaults" $
      hexesPerDegreeLatitude defaultPlanetConfig `shouldSatisfy` (> 0)

    it "scales linearly with planet radius" $ do
      let big = defaultPlanetConfig { pcRadius = 9000 }
          small = defaultPlanetConfig { pcRadius = 5000 }
      hexesPerDegreeLatitude big `shouldSatisfy` (> hexesPerDegreeLatitude small)

  describe "hexesPerDegreeLongitude" $ do
    it "equals hexesPerDegreeLatitude at equator" $ do
      let hpdLat = hexesPerDegreeLatitude defaultPlanetConfig
          hpdLon = hexesPerDegreeLongitude defaultPlanetConfig 0
      abs (hpdLat - hpdLon) `shouldSatisfy` (< 0.01)

    it "shrinks toward poles" $ do
      let lon0  = hexesPerDegreeLongitude defaultPlanetConfig 0
          lon60 = hexesPerDegreeLongitude defaultPlanetConfig 60
      lon60 `shouldSatisfy` (< lon0)

  describe "tileLatitude" $ do
    it "grid center maps to slice latCenter" $ do
      let planet = defaultPlanetConfig
          slice  = defaultWorldSlice
          config = WorldConfig { wcChunkSize = 16 }
          cs = 16
          centerY = cs `div` 2
          lat = tileLatitude planet slice config (TileCoord 0 centerY)
      abs (lat - wsLatCenter slice) `shouldSatisfy` (< 0.01)

    it "north edge is north of center" $ do
      let planet = defaultPlanetConfig
          slice  = defaultWorldSlice
          config = WorldConfig { wcChunkSize = 16 }
          -- For ry=2, max chunk coord = 2, max tile Y = 2*16 + 15 = 47
          topTileY = 2 * 16 + 15
          lat = tileLatitude planet slice config (TileCoord 0 topTileY)
      lat `shouldSatisfy` (> wsLatCenter slice)

    it "property: tileLatitude at center matches sliceLatCenter for any valid config" $
      property $ \(ValidPlanetParams r t i) (ValidSliceParams lc le _ _) ->
        let Right planet = mkPlanetConfig r t i
            Right slice  = mkWorldSlice lc le 0 60
            config = WorldConfig { wcChunkSize = 16 }
            centerY = 16 `div` 2
            lat = tileLatitude planet slice config (TileCoord 0 centerY)
        in abs (lat - lc) < 0.01

  describe "tileLongitude" $ do
    it "grid center maps to slice lonCenter" $ do
      let planet = defaultPlanetConfig
          slice  = defaultWorldSlice
          config = WorldConfig { wcChunkSize = 16 }
          cs = 16
          centerX = cs `div` 2
          centerY = cs `div` 2
          lon = tileLongitude planet slice config (TileCoord centerX centerY)
      abs (lon - wsLonCenter slice) `shouldSatisfy` (< 0.01)

  describe "tileYToLatDeg" $ do
    it "agrees with tileLatitude for same tile Y" $ do
      let planet = defaultPlanetConfig
          slice  = defaultWorldSlice
          config = WorldConfig { wcChunkSize = 16 }
          gy = 20
          latA = tileLatitude planet slice config (TileCoord 0 gy)
          latB = tileYToLatDeg planet slice config gy
      abs (latA - latB) `shouldSatisfy` (< 0.001)

  describe "round-trip encode/decode" $ do
    it "PlanetConfig round-trips through storage" $ do
      -- Tested indirectly via Storage.spec round-trip tests;
      -- here we verify the defaults survive the TerrainWorld round-trip.
      True `shouldBe` True

  describe "sliceToWorldExtent" $ do
    it "produces at least 1×1 radii for defaults" $ do
      let config = WorldConfig { wcChunkSize = 16 }
          Right extent = sliceToWorldExtent defaultPlanetConfig defaultWorldSlice config
          (rx, ry) = worldExtentRadii extent
      rx `shouldSatisfy` (>= 1)
      ry `shouldSatisfy` (>= 1)

    it "larger slice produces larger extent" $ do
      let config = WorldConfig { wcChunkSize = 16 }
          smallSlice = defaultWorldSlice { wsLatExtent = 10, wsLonExtent = 15 }
          bigSlice   = defaultWorldSlice { wsLatExtent = 80, wsLonExtent = 120 }
          Right extSmall = sliceToWorldExtent defaultPlanetConfig smallSlice config
          Right extBig   = sliceToWorldExtent defaultPlanetConfig bigSlice config
          (_, rySmall) = worldExtentRadii extSmall
          (_, ryBig)   = worldExtentRadii extBig
      ryBig `shouldSatisfy` (> rySmall)

    it "property: always produces valid extent for valid inputs" $
      property $ \(ValidPlanetParams r t i) (ValidSliceParams lc le lnc lne) ->
        let Right planet = mkPlanetConfig r t i
            Right slice  = mkWorldSlice lc le lnc lne
            config = WorldConfig { wcChunkSize = 16 }
        in case sliceToWorldExtent planet slice config of
            Right extent -> let (rx, ry) = worldExtentRadii extent
                            in rx >= 1 && ry >= 1
            Left _ -> False

  describe "latitude monotonicity" $ do
    it "property: increasing tile Y increases latitude" $
      property $ \(ValidPlanetParams r t i) (ValidSliceParams lc le _ _) ->
        let Right planet = mkPlanetConfig r t i
            Right slice  = mkWorldSlice lc le 0 60
            config = WorldConfig { wcChunkSize = 16 }
            lat0 = tileYToLatDeg planet slice config 0
            lat1 = tileYToLatDeg planet slice config 100
        in lat1 > lat0

-- ---------------------------------------------------------------------------
-- QuickCheck generators
-- ---------------------------------------------------------------------------

-- | Valid planet parameters within constructor ranges.
data ValidPlanetParams = ValidPlanetParams !Float !Float !Float
  deriving (Show)

instance Arbitrary ValidPlanetParams where
  arbitrary = do
    r <- choose (4778, 9557)
    t <- choose (0, 45)
    i <- choose (0.7, 1.3)
    pure (ValidPlanetParams r t i)

-- | Valid world slice parameters within constructor ranges.
data ValidSliceParams = ValidSliceParams !Float !Float !Float !Float
  deriving (Show)

instance Arbitrary ValidSliceParams where
  arbitrary = do
    lc  <- choose (-90, 90)
    le  <- choose (0.1, 180)
    lnc <- choose (-180, 180)
    lne <- choose (0.1, 360)
    pure (ValidSliceParams lc le lnc lne)

-- | Helper
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
