{-# LANGUAGE PatternSynonyms #-}

-- | Tests for Phase 8 water body analysis.
--
-- Covers: WaterBodyType classification (ocean / lake / inland sea),
-- core algorithm properties, and edge-case behaviour.
module Spec.WaterBody (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Word (Word8, Word32)
import qualified Data.Vector.Unboxed as U
import Topo.Types
  ( WaterBodyType
  , WaterBodyChunk(..)
  , pattern WaterDry
  , pattern WaterOcean
  , pattern WaterLake
  , pattern WaterInlandSea
  , waterBodyFromCode
  , waterBodyToCode
  )
import Topo.WaterBody
  ( WaterBodyConfig(..)
  , WaterBodyResult(..)
  , classifyWaterBodies
  , defaultWaterBodyConfig
  )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a flat elevation grid from a 2D list (row-major).
mkGrid :: [[Float]] -> (Int, Int, U.Vector Float)
mkGrid [] = (0, 0, U.empty)
mkGrid rows@(r:_) =
  let h = length rows
      w = length r
  in (w, h, U.fromList (concat rows))

-- | Wrapper for normalised float generation in [0,1].
newtype Norm01 = Norm01 { getNorm01 :: Float }
  deriving (Show)

instance Arbitrary Norm01 where
  arbitrary = Norm01 <$> choose (0.0, 1.0)
  shrink (Norm01 v) = [Norm01 v' | v' <- shrink v, v' >= 0, v' <= 1]

-- | Helper for checking Left values.
isLeftE :: Either a b -> Bool
isLeftE (Left _) = True
isLeftE _        = False

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "WaterBody" $ do

  -- -----------------------------------------------------------------------
  -- WaterBodyType code round-trips
  -- -----------------------------------------------------------------------
  describe "WaterBodyType round-trips" $ do
    it "all 4 codes round-trip" $
      mapM_ (\code ->
        case waterBodyFromCode code of
          Left _ -> expectationFailure $ "code " ++ show code ++ " should be valid"
          Right wbt -> waterBodyToCode wbt `shouldBe` code
      ) [0, 1, 2, 3 :: Word8]

    it "rejects unknown code" $
      waterBodyFromCode 99 `shouldSatisfy` isLeftE

  -- -----------------------------------------------------------------------
  -- All-dry grid
  -- -----------------------------------------------------------------------
  describe "all-dry grid" $ do
    it "classifies all tiles as WaterDry when everything is above waterLevel" $ do
      let (w, h, elev) = mkGrid
            [ [0.6, 0.7, 0.8]
            , [0.7, 0.9, 0.8]
            , [0.8, 0.7, 0.6]
            ]
          result = classifyWaterBodies defaultWaterBodyConfig 0.5 w h elev
      U.all (== WaterDry) (wbrType result) `shouldBe` True
      U.all (== 0) (wbrDepth result) `shouldBe` True

  -- -----------------------------------------------------------------------
  -- All-ocean grid (entire grid submerged, touches edges)
  -- -----------------------------------------------------------------------
  describe "all-ocean grid" $ do
    it "classifies every tile as WaterOcean when all below waterLevel" $ do
      let (w, h, elev) = mkGrid
            [ [0.1, 0.2, 0.1]
            , [0.2, 0.3, 0.2]
            , [0.1, 0.2, 0.1]
            ]
          result = classifyWaterBodies defaultWaterBodyConfig 0.5 w h elev
      U.all (== WaterOcean) (wbrType result) `shouldBe` True

  -- -----------------------------------------------------------------------
  -- Landlocked lake (depression in the middle, surrounded by land)
  -- -----------------------------------------------------------------------
  describe "landlocked lake" $ do
    it "classifies interior depression as lake (not ocean)" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
          -- 5x5 grid with a small basin at the centre
          elev = U.fromList
            [ 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.3, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
          midIdx = 12  -- row 2, col 2
      (wbrType result U.! midIdx) `shouldBe` WaterLake
      -- Surrounding tiles should be dry
      (wbrType result U.! 0) `shouldBe` WaterDry
      -- Depth > 0 at lake tile
      (wbrDepth result U.! midIdx) `shouldSatisfy` (> 0)

  -- -----------------------------------------------------------------------
  -- Inland sea (large landlocked basin)
  -- -----------------------------------------------------------------------
  describe "inland sea" $ do
    it "classifies large landlocked basin as inland sea" $ do
      let cfg = defaultWaterBodyConfig
            { wbcInlandSeaMinSize = 4
            , wbcMinLakeSize = 1
            , wbcOceanEdgeMargin = 0  -- disable margin so 8x8 grid suffices
            }
          -- 8x8 grid with large centre depression (4x4 = 16 tiles)
          land = 0.8
          low  = 0.3
          elev = U.fromList
            [ land, land, land, land, land, land, land, land
            , land, land, land, land, land, land, land, land
            , land, land, low,  low,  low,  low,  land, land
            , land, land, low,  low,  low,  low,  land, land
            , land, land, low,  low,  low,  low,  land, land
            , land, land, low,  low,  low,  low,  land, land
            , land, land, land, land, land, land, land, land
            , land, land, land, land, land, land, land, land
            ]
          result = classifyWaterBodies cfg 0.5 8 8 elev
          -- centre tile (row 3, col 3) should be InlandSea
          idx = 3 * 8 + 3
      (wbrType result U.! idx) `shouldBe` WaterInlandSea

  -- -----------------------------------------------------------------------
  -- Ocean at grid edges
  -- -----------------------------------------------------------------------
  describe "ocean at grid edges" $ do
    it "classifies edge-connected water as ocean" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
          -- 5x5 grid: left column is below waterLevel (touches edge -> ocean)
          elev = U.fromList
            [ 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
      -- Left column tiles should be ocean
      (wbrType result U.! 0) `shouldBe` WaterOcean
      (wbrType result U.! 5) `shouldBe` WaterOcean
      -- Interior land should be dry
      (wbrType result U.! 1) `shouldBe` WaterDry

  -- -----------------------------------------------------------------------
  -- Tiny puddle elimination
  -- -----------------------------------------------------------------------
  describe "tiny puddle elimination" $ do
    it "classifies single-tile depression as WaterDry when below minLakeSize" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 4 }
          elev = U.fromList
            [ 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.3, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
      -- Single tile below water but too small -> absorbed as dry
      (wbrType result U.! 12) `shouldBe` WaterDry

  -- -----------------------------------------------------------------------
  -- Pour-point surface elevation
  -- -----------------------------------------------------------------------
  describe "pour-point surface elevation" $ do
    it "lake surface elevation equals the pour-point (min surrounding land)" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
          -- Centre tile is 0.3, surrounded by 0.7 land
          elev = U.fromList
            [ 0.7, 0.7, 0.7, 0.7, 0.7
            , 0.7, 0.7, 0.7, 0.7, 0.7
            , 0.7, 0.7, 0.3, 0.7, 0.7
            , 0.7, 0.7, 0.7, 0.7, 0.7
            , 0.7, 0.7, 0.7, 0.7, 0.7
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
      -- Pour-point = min land neighbor elev = 0.7
      -- Surface elev of the lake should be 0.7
      (wbrSurfaceElev result U.! 12) `shouldSatisfy` (\s -> abs (s - 0.7) < 0.001)
      -- Depth = 0.7 - 0.3 = 0.4
      (wbrDepth result U.! 12) `shouldSatisfy` (\d -> abs (d - 0.4) < 0.001)

  -- -----------------------------------------------------------------------
  -- Property: depth is always >= 0
  -- -----------------------------------------------------------------------
  describe "properties" $ do
    prop "depth is never negative" $
      \(Norm01 waterLevel) -> do
        let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
            w = 4
            h = 4
            -- Random-ish grid from waterLevel
            elev = U.fromList
              [ waterLevel - 0.1, waterLevel + 0.2, waterLevel + 0.1, waterLevel + 0.3
              , waterLevel + 0.2, waterLevel - 0.2, waterLevel - 0.1, waterLevel + 0.2
              , waterLevel + 0.1, waterLevel - 0.1, waterLevel + 0.3, waterLevel + 0.1
              , waterLevel + 0.3, waterLevel + 0.2, waterLevel + 0.1, waterLevel + 0.2
              ]
            result = classifyWaterBodies cfg waterLevel w h elev
        U.all (>= 0) (wbrDepth result)

    prop "WaterDry tiles have zero depth" $
      \(Norm01 waterLevel) -> do
        let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
            elev = U.fromList
              [ waterLevel + 0.1, waterLevel + 0.2
              , waterLevel + 0.3, waterLevel + 0.4
              ]
            result = classifyWaterBodies cfg waterLevel 2 2 elev
        U.all (== 0) (wbrDepth result) .&&. U.all (== WaterDry) (wbrType result)

    prop "ocean tiles have surfaceElev == waterLevel" $
      \(Norm01 waterLevel) -> do
        let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
            elev = U.fromList
              [ waterLevel - 0.2, waterLevel - 0.1
              , waterLevel - 0.3, waterLevel - 0.1
              ]
            result = classifyWaterBodies cfg waterLevel 2 2 elev
            -- All tiles touch edge -> ocean
        U.all (\s -> abs (s - waterLevel) < 0.001) (wbrSurfaceElev result)

  -- -----------------------------------------------------------------------
  -- Adjacent water type (wbAdjacentType)
  -- -----------------------------------------------------------------------
  describe "adjacent water type" $ do
    it "ocean-adjacent land tile has WaterOcean adjacency" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
          -- Left column submerged (ocean), rest is land
          elev = U.fromList
            [ 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            , 0.3, 0.8, 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
      -- Tile at (1,0) is land, adjacent to ocean at (0,0)
      (wbrAdjacentType result U.! 1) `shouldBe` WaterOcean
      -- Tile at (2,0) is land, NOT adjacent to ocean
      (wbrAdjacentType result U.! 2) `shouldBe` WaterDry

    it "lake-adjacent land tile has WaterLake adjacency" $ do
      let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
          -- Centre tile is lake, surrounded by land
          elev = U.fromList
            [ 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.3, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 5 5 elev
      -- Tile at (2,1) = index 7, 4-adjacent to lake at centre (12)
      (wbrAdjacentType result U.! 7) `shouldBe` WaterLake
      -- Tile at (1,2) = index 11, also adjacent
      (wbrAdjacentType result U.! 11) `shouldBe` WaterLake
      -- Tile at (0,0) = index 0, NOT adjacent → WaterDry
      (wbrAdjacentType result U.! 0) `shouldBe` WaterDry

    it "isolated land tile has WaterDry adjacency" $ do
      let cfg = defaultWaterBodyConfig
          elev = U.fromList
            [ 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8
            , 0.8, 0.8, 0.8
            ]
          result = classifyWaterBodies cfg 0.5 3 3 elev
      -- All land, no water bodies at all
      U.all (== WaterDry) (wbrAdjacentType result) `shouldBe` True

    prop "submerged tiles have adjacentType matching their own wbrType" $
      \(Norm01 waterLevel) -> do
        let cfg = defaultWaterBodyConfig { wbcMinLakeSize = 1 }
            elev = U.fromList
              [ waterLevel - 0.2, waterLevel - 0.1
              , waterLevel - 0.3, waterLevel - 0.1
              ]
            result = classifyWaterBodies cfg waterLevel 2 2 elev
        U.toList (wbrAdjacentType result) === U.toList (wbrType result)
