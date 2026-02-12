{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Parameters (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo.Parameters
  ( TerrainFormConfig(..)
  , classifyTerrainForm
  , curvatureAt
  , defaultTerrainFormConfig
  , isLocalMinimum
  , mkElevLookup
  , reliefAt
  , ruggednessAt
  , slopeAt
  )
import Topo.Types
  ( ChunkCoord(..)
  , ChunkId(..)
  , TerrainChunk(..)
  , TileCoord(..)
  , WorldConfig(..)
  , chunkIdFromCoord
  , pattern FormCliff
  , pattern FormDepression
  , pattern FormFlat
  , pattern FormHilly
  , pattern FormMountainous
  , pattern FormRolling
  , pattern FormValley
  )
import Topo.World (emptyTerrainChunk)

-- | Build a single-chunk world with the given elevation raster.
mkWorld :: Int -> U.Vector Float -> (IntMap.IntMap TerrainChunk, WorldConfig, TileCoord)
mkWorld size elevs =
  let config = WorldConfig { wcChunkSize = size }
      chunk  = (emptyTerrainChunk config) { tcElevation = elevs }
      ChunkId cid = chunkIdFromCoord (ChunkCoord 0 0)
      chunks = IntMap.singleton cid chunk
      origin = TileCoord 0 0
  in (chunks, config, origin)

-- | Build a 2x2 grid of chunks (coords (0,0),(1,0),(0,1),(1,1)).
mkWorld2x2 :: Int -> (U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> ())
           -> IntMap.IntMap TerrainChunk
mkWorld2x2 = error "unused placeholder"

spec :: Spec
spec = describe "Parameters" $ do
  ---------------------------------------------------------------------------
  -- Slope
  ---------------------------------------------------------------------------
  describe "slopeAt" $ do
    it "returns 0 for a flat surface" $ do
      let size = 4
          flat = U.replicate (size * size) (0.5 :: Float)
          (chunks, config, origin) = mkWorld size flat
          elevAt = mkElevLookup chunks config origin flat
      -- interior tile (1,1)
      slopeAt elevAt 1 1 `shouldBe` 0.0

    it "returns non-zero for a linear ramp" $ do
      let size = 4
          -- elevation increases 0.1 per tile in x direction
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * (0.1 :: Float)
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
      -- interior tile (1,1): central-diff dx = (0.2 - 0.0)/2 = 0.1
      slopeAt elevAt 1 1 `shouldSatisfy` (> 0)

    it "gives correct slope for simple east-facing ramp" $ do
      let size = 4
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * (0.1 :: Float)
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
          s = slopeAt elevAt 2 2
      -- dx = (0.3 - 0.1)/2 = 0.1, dy = 0
      abs (s - 0.1) `shouldSatisfy` (< 1e-5)

    describe "cross-chunk boundary" $ do
      it "produces non-zero slope at chunk edge when neighbor exists" $ do
        let size = 4
            config = WorldConfig { wcChunkSize = size }
            -- chunk (0,0): all tiles at 0.0
            c00 = (emptyTerrainChunk config) { tcElevation = U.replicate (size * size) 0.0 }
            -- chunk (1,0): all tiles at 1.0
            c10 = (emptyTerrainChunk config) { tcElevation = U.replicate (size * size) 1.0 }
            ChunkId id00 = chunkIdFromCoord (ChunkCoord 0 0)
            ChunkId id10 = chunkIdFromCoord (ChunkCoord 1 0)
            chunks = IntMap.fromList [(id00, c00), (id10, c10)]
            origin = TileCoord 0 0
            elevAt = mkElevLookup chunks config origin (tcElevation c00)
        -- tile (3,1) is on the right edge of chunk (0,0)
        -- right neighbor (4,1) is in chunk (1,0) with elev 1.0
        slopeAt elevAt 3 1 `shouldSatisfy` (> 0)

  ---------------------------------------------------------------------------
  -- Curvature
  ---------------------------------------------------------------------------
  describe "curvatureAt" $ do
    it "returns 0 for a flat surface" $ do
      let size = 4
          flat = U.replicate (size * size) (0.3 :: Float)
          (chunks, config, origin) = mkWorld size flat
          elevAt = mkElevLookup chunks config origin flat
      abs (curvatureAt elevAt 2 2) `shouldSatisfy` (< 1e-6)

    it "returns 0 for a linear ramp" $ do
      let size = 4
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * (0.1 :: Float)
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
      -- Laplacian of a linear function is 0
      abs (curvatureAt elevAt 2 2) `shouldSatisfy` (< 1e-5)

    it "returns negative curvature at a peak" $ do
      let size = 5
          -- peak at (2,2): center high, neighbors lower
          peak = U.generate (size * size) $ \i ->
            let x = i `mod` size
                y = i `div` size
                dx = fromIntegral (x - 2) :: Float
                dy = fromIntegral (y - 2) :: Float
            in 1.0 - 0.1 * (dx * dx + dy * dy)
          (chunks, config, origin) = mkWorld size peak
          elevAt = mkElevLookup chunks config origin peak
      -- Laplacian at the peak should be negative (concave down)
      curvatureAt elevAt 2 2 `shouldSatisfy` (< 0)

  ---------------------------------------------------------------------------
  -- Relief
  ---------------------------------------------------------------------------
  describe "reliefAt" $ do
    it "returns 0 for a flat surface" $ do
      let size = 4
          flat = U.replicate (size * size) (0.5 :: Float)
          (chunks, config, origin) = mkWorld size flat
          elevAt = mkElevLookup chunks config origin flat
      reliefAt elevAt 2 2 `shouldBe` 0.0

    it "equals max-min of neighborhood for a simple case" $ do
      let size = 5
          -- center (2,2) = 0.5, neighbor (3,3) = 1.0, rest = 0.3
          elev = U.generate (size * size) $ \i ->
            let x = i `mod` size
                y = i `div` size
            in if x == 2 && y == 2 then 0.5 :: Float
               else if x == 3 && y == 3 then 1.0
               else 0.3
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      -- neighborhood of (2,2): center 0.5, (3,3)=1.0, rest=0.3
      -- min = 0.3, max = 1.0 → relief = 0.7
      let r = reliefAt elevAt 2 2
      abs (r - 0.7) `shouldSatisfy` (< 1e-5)

    prop "relief is non-negative" $ \(seed :: Int) ->
      let size = 4
          gen i = fromIntegral (((seed * 31 + i * 17) `mod` 200) - 100) / 100.0 :: Float
          elev = U.generate (size * size) gen
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      in reliefAt elevAt 2 2 >= 0

  ---------------------------------------------------------------------------
  -- Ruggedness (TRI)
  ---------------------------------------------------------------------------
  describe "ruggednessAt" $ do
    it "returns 0 for a flat surface" $ do
      let size = 4
          flat = U.replicate (size * size) (0.5 :: Float)
          (chunks, config, origin) = mkWorld size flat
          elevAt = mkElevLookup chunks config origin flat
      ruggednessAt elevAt 2 2 `shouldBe` 0.0

    prop "TRI is non-negative" $ \(seed :: Int) ->
      let size = 4
          gen i = fromIntegral (((seed * 31 + i * 17) `mod` 200) - 100) / 100.0 :: Float
          elev = U.generate (size * size) gen
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      in ruggednessAt elevAt 2 2 >= 0

  ---------------------------------------------------------------------------
  -- isLocalMinimum
  ---------------------------------------------------------------------------
  describe "isLocalMinimum" $ do
    it "is True at a depression" $ do
      let size = 5
          -- center (2,2) = 0.0, all others = 1.0
          elev = U.generate (size * size) $ \i ->
            let x = i `mod` size
                y = i `div` size
            in if x == 2 && y == 2 then 0.0 :: Float else 1.0
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      isLocalMinimum elevAt 2 2 `shouldBe` True

    it "is False at a peak" $ do
      let size = 5
          -- center (2,2) = 1.0, all others = 0.0
          elev = U.generate (size * size) $ \i ->
            let x = i `mod` size
                y = i `div` size
            in if x == 2 && y == 2 then 1.0 :: Float else 0.0
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      isLocalMinimum elevAt 2 2 `shouldBe` False

  ---------------------------------------------------------------------------
  -- classifyTerrainForm
  ---------------------------------------------------------------------------
  describe "classifyTerrainForm" $ do
    let cfg = defaultTerrainFormConfig

    it "classifies flat terrain" $
      classifyTerrainForm cfg 0.0 0.0 0.0 False `shouldBe` FormFlat

    it "classifies rolling terrain" $
      classifyTerrainForm cfg 0.05 0.0 0.0 False `shouldBe` FormRolling

    it "classifies hilly terrain" $
      classifyTerrainForm cfg 0.12 0.15 0.0 False `shouldBe` FormHilly

    it "classifies mountainous terrain (by slope)" $
      classifyTerrainForm cfg 0.25 0.0 0.0 False `shouldBe` FormMountainous

    it "classifies mountainous terrain (by relief)" $
      classifyTerrainForm cfg 0.01 0.30 0.0 False `shouldBe` FormMountainous

    it "classifies cliff" $
      classifyTerrainForm cfg 0.50 0.0 0.0 False `shouldBe` FormCliff

    it "classifies valley" $
      classifyTerrainForm cfg 0.01 0.01 (-0.20) False `shouldBe` FormValley

    it "classifies depression" $
      classifyTerrainForm cfg 0.0 0.0 0.0 True `shouldBe` FormDepression

    prop "always returns a valid TerrainForm" $
      \(s :: Float) (r :: Float) (c :: Float) (lm :: Bool) ->
        let form = classifyTerrainForm cfg (abs s) (abs r) c lm
        in form `elem` [FormFlat, FormRolling, FormHilly, FormMountainous
                        , FormCliff, FormValley, FormDepression]

    it "cliff beats mountainous (higher slope wins)" $
      classifyTerrainForm cfg 0.50 0.30 (-0.20) True `shouldBe` FormCliff

    it "mountainous beats valley" $
      classifyTerrainForm cfg 0.25 0.30 (-0.20) True `shouldBe` FormMountainous
