{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainFormCalibration (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import Spec.Support.TerrainFormFixtures
  ( badlandsElev
  , badlandsHardness
  , canyonElev
  , classifyFixture
  , classifyFixtureWith
  , coastalEscarpmentElev
  , glaciatedHighlandElev
  , midSlopeReliefElev
  , midSlopeReliefHardness
  , shieldPlateauElev
  )
import Topo.BaseHeight (GenConfig(..), defaultGenConfig)
import Topo.Erosion
  ( ErosionConfig(..)
  , computeMicroReliefGrid
  , defaultErosionConfig
  )
import Topo.Parameters (TerrainFormConfig(..), computeReliefIndex, defaultTerrainFormConfig)
import qualified Topo.TerrainForm.Metrics as TerrainMetrics
import Topo.Types
  ( ChunkCoord(..)
  , TerrainForm
  , WorldConfig(..)
  , pattern FormBadlands
  , pattern FormCanyon
  , pattern FormCliff
  , pattern FormEscarpment
  , pattern FormFlat
  , pattern FormHilly
  , pattern FormMountainous
  , pattern FormPlateau
  , pattern FormRolling
  , pattern FormValley
  )

spec :: Spec
spec = describe "TerrainForm calibration" $ do
  describe "deterministic fixture distributions" $ do
    it "shield plateau fixture favors low-relief elevated forms" $ do
      let forms = classifyFixture shieldPlateauElev (\_ _ -> 0.65)
          lowReliefFrac = frac FormPlateau forms + frac FormFlat forms + frac FormRolling forms
      lowReliefFrac `shouldSatisfy` (> 0.45)
      frac FormPlateau forms `shouldSatisfy` (> 0.05)

    it "canyon fixture yields canyon/valley signatures" $ do
      let forms = classifyFixture canyonElev (\_ _ -> 0.7)
          incisedFrac = frac FormCanyon forms + frac FormValley forms + frac FormMountainous forms
      incisedFrac `shouldSatisfy` (> 0.10)

    it "glaciated highland fixture yields rugged uplands" $ do
      let forms = classifyFixture glaciatedHighlandElev (\_ _ -> 0.7)
          uplandFrac = frac FormMountainous forms + frac FormHilly forms
      uplandFrac `shouldSatisfy` (> 0.20)

    it "coastal escarpment fixture yields escarpment/cliff band" $ do
      let forms = classifyFixture coastalEscarpmentElev (\_ _ -> 0.65)
      (frac FormEscarpment forms + frac FormCliff forms) `shouldSatisfy` (> 0)

    it "badlands fixture yields badlands presence" $ do
      let forms = classifyFixture badlandsElev badlandsHardness
      frac FormBadlands forms `shouldSatisfy` (> 0.01)

    it "mid-slope boundary fixture responds to micro-relief threshold sweeps" $ do
      let strictCfg = defaultTerrainFormConfig
            { tfcMicroReliefRollingMin = 0.95
            , tfcMicroReliefHillyMin = 0.98
            , tfcMicroReliefHillySlopeScale = 1.0
            , tfcMicroReliefHillyReliefScale = 1.0
            }
          permissiveCfg = defaultTerrainFormConfig
            { tfcMicroReliefRollingMin = 0.45
            , tfcMicroReliefHillyMin = 0.0
            , tfcMicroReliefHillySlopeScale = 0.25
            , tfcMicroReliefHillyReliefScale = 0.25
            }
          strictForms = classifyFixtureWith strictCfg midSlopeReliefElev midSlopeReliefHardness
          permissiveForms = classifyFixtureWith permissiveCfg midSlopeReliefElev midSlopeReliefHardness
          strictFlat = frac FormFlat strictForms
          strictHilly = frac FormHilly strictForms
          permissiveFlat = frac FormFlat permissiveForms
          permissiveHilly = frac FormHilly permissiveForms
      permissiveHilly `shouldSatisfy` (> strictHilly + 0.02)
      permissiveFlat `shouldSatisfy` (< strictFlat)

  describe "micro-relief properties" $ do
    prop "computeMicroReliefGrid stays in [0,1]" $ \(seed :: Word64) ->
      let cfg = WorldConfig { wcChunkSize = 8 }
          elev = U.replicate 64 0.5
          proxy = U.generate 64 $ \i ->
            let raw = ((i * 37 + 11) `mod` 200) - 100
            in fromIntegral raw / 100
          out = computeMicroReliefGrid cfg defaultGenConfig (ChunkCoord 0 0) 8 8 elev seed defaultErosionConfig proxy
      in U.all (\v -> v >= 0 && v <= 1) out

    prop "increasing noise amplitude weight does not reduce mean micro-relief" $ \(seed :: Word64) ->
      let cfg = WorldConfig { wcChunkSize = 8 }
          elev = U.replicate 64 0.5
          zeros = U.replicate 64 0
          lowNoiseCfg = defaultErosionConfig
            { ecMicroReliefNoiseWeight = 0.3
            , ecMicroReliefErosionWeight = 0.7
            }
          highNoiseCfg = defaultErosionConfig
            { ecMicroReliefNoiseWeight = 0.9
            , ecMicroReliefErosionWeight = 0.7
            }
          meanOf vec = U.sum vec / fromIntegral (U.length vec)
          mLow = meanOf (computeMicroReliefGrid cfg defaultGenConfig (ChunkCoord 0 0) 8 8 elev seed lowNoiseCfg zeros)
          mHigh = meanOf (computeMicroReliefGrid cfg defaultGenConfig (ChunkCoord 0 0) 8 8 elev seed highNoiseCfg zeros)
      in mHigh + 1.0e-6 >= mLow

    it "terrain generator noise shape parameters affect micro-relief distributions" $ do
      let cfg = WorldConfig { wcChunkSize = 8 }
          erosionCfg = defaultErosionConfig
            { ecMicroReliefNoiseWeight = 1
            , ecMicroReliefErosionWeight = 0
            }
          seed = 424242
          elev = U.replicate 64 0.5
          zeros = U.replicate 64 0
          meanOf vec = U.sum vec / fromIntegral (U.length vec)
          baseOut = computeMicroReliefGrid cfg defaultGenConfig (ChunkCoord 0 0) 8 8 elev seed erosionCfg zeros
          lacOut = computeMicroReliefGrid cfg (defaultGenConfig { gcLacunarity = 2.8 }) (ChunkCoord 0 0) 8 8 elev seed erosionCfg zeros
          octOut = computeMicroReliefGrid cfg (defaultGenConfig { gcOctaves = 7 }) (ChunkCoord 0 0) 8 8 elev seed erosionCfg zeros
          gainOut = computeMicroReliefGrid cfg (defaultGenConfig { gcGain = 0.75 }) (ChunkCoord 0 0) 8 8 elev seed erosionCfg zeros
          delta a b = abs (meanOf a - meanOf b)
      delta baseOut lacOut `shouldSatisfy` (> 1.0e-4)
      delta baseOut octOut `shouldSatisfy` (> 1.0e-4)
      delta baseOut gainOut `shouldSatisfy` (> 1.0e-4)

    prop "zero-noise ring-only mode agrees between pre/post relief index paths" $
      \(seed :: Word64) ->
        let cfg = WorldConfig { wcChunkSize = 8 }
            n = 64
            gridW' = 8
            gridH' = 8
            elev = U.generate n $ \i ->
              let x = i `mod` gridW'
                  y = i `div` gridW'
              in clamp01 (0.5 + 0.1 * sin (fromIntegral x) * cos (fromIntegral y))
            erosionCfg = defaultErosionConfig
              { ecMicroReliefNoiseWeight = 0
              , ecMicroReliefErosionWeight = 0
              }
            zeros = U.replicate n 0
            post = computeMicroReliefGrid cfg defaultGenConfig (ChunkCoord 0 0) gridW' gridH' elev seed erosionCfg zeros
            elevAtClamped gx gy =
              let cx = clampCoord gx gridW'
                  cy = clampCoord gy gridH'
              in elev U.! (cy * gridW' + cx)
            pre = U.generate n $ \i ->
              let x = i `mod` gridW'
                  y = i `div` gridW'
                  m = TerrainMetrics.terrainNeighborhoodAt elevAtClamped x y
                  r = TerrainMetrics.tnRelief m
                  r2 = TerrainMetrics.tnRelief2Ring m
                  r3 = TerrainMetrics.tnRelief3Ring m
              in computeReliefIndex r r2 r3 Nothing Nothing 0 0
            eps = 1.0e-5
        in U.and (U.zipWith (\a b -> abs (a - b) <= eps) pre post)

frac :: TerrainForm -> U.Vector TerrainForm -> Double
frac f v
  | U.null v = 0
  | otherwise =
      let c = U.length (U.filter (== f) v)
      in fromIntegral c / fromIntegral (U.length v)

clampCoord :: Int -> Int -> Int
clampCoord coord maxSize
  | maxSize <= 1 = 0
  | coord < 0 = 0
  | coord >= maxSize = maxSize - 1
  | otherwise = coord

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v
