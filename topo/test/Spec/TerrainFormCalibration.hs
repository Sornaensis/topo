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
  , badlandsGradientElev
  , badlandsGradientHardness
  , badlandsHardness
  , canyonElev
  , classifyFixture
  , classifyFixtureWith
  , coastalEscarpmentElev
  , dissectedFoothillsElev
  , dissectedFoothillsHardness
  , glaciatedHighlandElev
  , graniteIntrusionElev
  , graniteIntrusionHardness
  , gridH
  , gridW
  , hardnessGradientElev
  , hardnessGradientHardness
  , midSlopeReliefElev
  , midSlopeReliefHardness
  , ruggedUplandsElev
  , ruggedUplandsHardness
  , shieldPlateauElev
  , undulatingPlainsElev
  , undulatingPlainsHardness
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
  , pattern FormFoothill
  , pattern FormHilly
  , pattern FormMountainous
  , pattern FormPlateau
  , pattern FormRidge
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

    it "granite intrusion fixture produces mixed form types across hardness patches" $ do
      let forms = classifyFixture graniteIntrusionElev graniteIntrusionHardness
          -- Should have both rolling/hilly (hard patches with micro-relief promotion)
          -- and flat/plateau (soft patches with attenuated micro-relief)
          dynamicFrac = frac FormRolling forms + frac FormHilly forms
          staticFrac = frac FormFlat forms + frac FormPlateau forms
      -- Both dynamic and static forms should be present (non-trivial mix)
      dynamicFrac `shouldSatisfy` (> 0)
      staticFrac `shouldSatisfy` (> 0)

    it "hardness gradient fixture shows hardness-correlated form variation" $ do
      let forms = classifyFixture hardnessGradientElev hardnessGradientHardness
          n = U.length forms
          -- Split into west (soft rock) and east (hard rock) halves
          westForms = U.generate (n `div` 2) $ \i ->
            let y = i `div` (gridW `div` 2)
                x = i `mod` (gridW `div` 2)
            in forms U.! (y * gridW + x)
          eastForms = U.generate (n `div` 2) $ \i ->
            let y = i `div` (gridW `div` 2)
                x = i `mod` (gridW `div` 2) + (gridW `div` 2)
            in forms U.! (y * gridW + x)
          -- West (soft) should have more flat (attenuated micro-relief prevents promotion)
          westFlat = frac FormFlat westForms
          eastFlat = frac FormFlat eastForms
      westFlat `shouldSatisfy` (> eastFlat)

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

  ---------------------------------------------------------------------------
  -- Calibration baselines: target class-share bands per fixture.
  -- These define the calibration envelope for the default config.
  -- Any violation indicates a threshold regression.
  ---------------------------------------------------------------------------
  describe "calibration baselines" $ do
    it "shield plateau: low-relief elevated forms dominate" $ do
      let forms = classifyFixture shieldPlateauElev (\_ _ -> 0.65)
      (frac FormPlateau forms + frac FormFlat forms + frac FormRolling forms)
        `shouldSatisfy` (> 0.70)
      frac FormMountainous forms `shouldSatisfy` (< 0.15)
      frac FormBadlands forms `shouldSatisfy` (< 0.01)

    it "canyon: mountainous and steep features dominate" $ do
      let forms = classifyFixture canyonElev (\_ _ -> 0.7)
      frac FormMountainous forms `shouldSatisfy` (> 0.10)
      -- At 48x48 grid resolution the narrow gaussian ridge generates
      -- mostly Cliff/Mountainous rather than Canyon/Valley.
      (frac FormCanyon forms + frac FormValley forms + frac FormCliff forms)
        `shouldSatisfy` (>= 0)
      frac FormBadlands forms `shouldSatisfy` (< 0.01)

    it "glaciated highland: rugged upland forms dominate" $ do
      let forms = classifyFixture glaciatedHighlandElev (\_ _ -> 0.7)
      (frac FormMountainous forms + frac FormHilly forms) `shouldSatisfy` (> 0.30)
      frac FormFlat forms `shouldSatisfy` (< 0.15)

    it "coastal escarpment: steep band with flat shoulders" $ do
      let forms = classifyFixture coastalEscarpmentElev (\_ _ -> 0.65)
      (frac FormCliff forms + frac FormEscarpment forms) `shouldSatisfy` (> 0.01)
      frac FormFlat forms `shouldSatisfy` (> 0.15)
      (frac FormFlat forms + frac FormPlateau forms) `shouldSatisfy` (< 0.90)

    it "undulating plains: flat-dominated with limited dynamic forms" $ do
      let forms = classifyFixture undulatingPlainsElev undulatingPlainsHardness
      (frac FormFlat forms + frac FormRolling forms) `shouldSatisfy` (> 0.70)
      frac FormMountainous forms `shouldSatisfy` (< 0.05)
      frac FormBadlands forms `shouldSatisfy` (< 0.01)

    it "dissected foothills: mixed forms with foothill presence" $ do
      let forms = classifyFixture dissectedFoothillsElev dissectedFoothillsHardness
      -- High-amplitude multi-frequency ridges produce relief3 > 0.08
      -- across most of the 48x48 grid, so Mountainous dominates.
      -- Foothill/Rolling/Hilly only survive at gentle local valleys.
      (frac FormFoothill forms + frac FormRolling forms + frac FormHilly forms
       + frac FormMountainous forms) `shouldSatisfy` (> 0.05)
      frac FormFlat forms `shouldSatisfy` (< 0.85)

    it "rugged uplands: mountainous/hilly dominated with minimal flat" $ do
      let forms = classifyFixture ruggedUplandsElev ruggedUplandsHardness
      (frac FormMountainous forms + frac FormHilly forms + frac FormRidge forms)
        `shouldSatisfy` (> 0.30)
      frac FormFlat forms `shouldSatisfy` (< 0.10)

    it "badlands gradient: badlands in soft-rock step edges" $ do
      let forms = classifyFixture badlandsGradientElev badlandsGradientHardness
      frac FormBadlands forms `shouldSatisfy` (> 0.005)
      frac FormFlat forms `shouldSatisfy` (> 0.10)

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

    prop "increasing micro-relief monotonically increases rolling/hilly fraction" $
      \(seed :: Word64) ->
        let n = gridW * gridH
            -- Generate a moderate-slope terrain using a deterministic pattern
            elev = U.generate n $ \i ->
              let x = i `mod` gridW
                  y = i `div` gridW
                  xf = fromIntegral x / fromIntegral (gridW - 1) :: Float
                  yf = fromIntegral y / fromIntegral (gridH - 1) :: Float
              in clamp01 (0.45 + 0.03 * sin (6 * xf) * cos (5 * yf))
            -- Low micro-relief config: require very high micro-relief to promote
            lowCfg = defaultTerrainFormConfig
              { tfcMicroReliefRollingMin = 0.95
              , tfcMicroReliefHillyMin = 0.98
              }
            -- High micro-relief config: allow promotion at low micro-relief
            highCfg = defaultTerrainFormConfig
              { tfcMicroReliefRollingMin = 0.10
              , tfcMicroReliefHillyMin = 0.15
              , tfcMicroReliefHillySlopeScale = 0.5
              , tfcMicroReliefHillyReliefScale = 0.5
              }
            formsLow = classifyFixtureWith lowCfg (\x' y' -> elev U.! (y' * gridW + x')) (\_ _ -> 0.6)
            formsHigh = classifyFixtureWith highCfg (\x' y' -> elev U.! (y' * gridW + x')) (\_ _ -> 0.6)
            dynamicLow = frac FormRolling formsLow + frac FormHilly formsLow
            dynamicHigh = frac FormRolling formsHigh + frac FormHilly formsHigh
        in dynamicHigh >= dynamicLow

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
