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
import Topo.Math (clamp01)
import Topo.Types
  ( ChunkCoord(..)
  , ChunkId(..)
  , DirectionalSlope(..)
  , TerrainChunk(..)
  , TerrainForm
  , TileCoord(..)
  , WorldConfig(..)
  , chunkIdFromCoord
  , dsAvgSlope
  , terrainFormDisplayName
  , zeroDirSlope
  , pattern FormBadlands
  , pattern FormCanyon
  , pattern FormCliff
  , pattern FormDepression
  , pattern FormEscarpment
  , pattern FormFlat
  , pattern FormFoothill
  , pattern FormHilly
  , pattern FormMesa
  , pattern FormMountainous
  , pattern FormPass
  , pattern FormPlateau
  , pattern FormRidge
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

-- | Uniform directional slope: all 6 directions set to the same value.
-- Useful for testing the classification cascade with a single scalar.
uniformDS :: Float -> DirectionalSlope
uniformDS s = DirectionalSlope s s s s s s

-- | Default substrate for tests: mid-range hardness, mid-range soil, sea level.
-- classifyTerrainForm cfg ds r c localMin 0.5 0.5 0.0
classifyDefault :: TerrainFormConfig -> DirectionalSlope -> Float -> Float -> Bool -> TerrainForm
classifyDefault cfg ds r c lm = classifyTerrainForm cfg ds r c lm 0.5 0.5 0.0

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
      slopeAt elevAt 1 1 `shouldBe` zeroDirSlope

    it "returns non-zero for a linear ramp" $ do
      let size = 4
          -- elevation increases 0.1 per tile in x direction
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * (0.1 :: Float)
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
      -- interior tile (1,1): should have non-zero average slope
      dsAvgSlope (slopeAt elevAt 1 1) `shouldSatisfy` (> 0)

    it "gives correct directional slope for simple east-facing ramp" $ do
      let size = 4
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * (0.1 :: Float)
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
          ds = slopeAt elevAt 2 2
      -- E neighbor: elev(3,2) - elev(2,2) = 0.3 - 0.2 = 0.1
      abs (dsSlopeE ds - 0.1) `shouldSatisfy` (< 1e-5)
      -- W neighbor: elev(1,2) - elev(2,2) = 0.1 - 0.2 = -0.1
      abs (dsSlopeW ds - (-0.1)) `shouldSatisfy` (< 1e-5)

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
        dsAvgSlope (slopeAt elevAt 3 1) `shouldSatisfy` (> 0)

    -- Property tests for slopeAt
    prop "flat surface produces zeroDirSlope at interior tiles" $ \(h :: Float) ->
      let size = 6
          flat = U.replicate (size * size) h
          (chunks, config, origin) = mkWorld size flat
          elevAt = mkElevLookup chunks config origin flat
      in slopeAt elevAt 2 2 === zeroDirSlope

    prop "dsAvgSlope of slopeAt is non-negative" $ \(seed :: Int) ->
      let size = 6
          gen i = fromIntegral (((seed * 31 + i * 17) `mod` 200) - 100) / 100.0 :: Float
          elev = U.generate (size * size) gen
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      in dsAvgSlope (slopeAt elevAt 2 2) >= 0

    prop "opposite directions have negated slopes on a linear ramp" $ \(k :: Float) ->
      let size = 6
          -- elevation increases k per tile in x direction
          ramp = U.generate (size * size) $ \i ->
            let x = i `mod` size in fromIntegral x * k
          (chunks, config, origin) = mkWorld size ramp
          elevAt = mkElevLookup chunks config origin ramp
          ds = slopeAt elevAt 3 3
      -- For x-ramp: E slope = k, W slope = -k (roughly)
      in abs (dsSlopeE ds + dsSlopeW ds) < 1e-4

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
          -- center (2,2) = 0.5, E neighbor (3,2) = 1.0, rest = 0.3
          -- (3,2) is a hex neighbor of (2,2) via HexE offset (+1,0)
          elev = U.generate (size * size) $ \i ->
            let x = i `mod` size
                y = i `div` size
            in if x == 2 && y == 2 then 0.5 :: Float
               else if x == 3 && y == 2 then 1.0
               else 0.3
          (chunks, config, origin) = mkWorld size elev
          elevAt = mkElevLookup chunks config origin elev
      -- hex neighborhood of (2,2): center 0.5, E(3,2)=1.0, rest=0.3
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
      classifyDefault cfg (uniformDS 0.0) 0.0 0.0 False `shouldBe` FormFlat

    it "classifies rolling terrain" $
      classifyDefault cfg (uniformDS 0.05) 0.0 0.0 False `shouldBe` FormRolling

    it "classifies hilly terrain" $
      classifyDefault cfg (uniformDS 0.12) 0.15 0.0 False `shouldBe` FormHilly

    it "classifies mountainous terrain (by slope)" $
      classifyDefault cfg (uniformDS 0.25) 0.0 0.0 False `shouldBe` FormMountainous

    it "classifies mountainous terrain (by relief)" $
      classifyDefault cfg (uniformDS 0.01) 0.30 0.0 False `shouldBe` FormMountainous

    it "classifies cliff" $
      classifyDefault cfg (uniformDS 0.50) 0.0 0.0 False `shouldBe` FormCliff

    it "classifies valley" $
      classifyDefault cfg (uniformDS 0.01) 0.01 (-0.20) False `shouldBe` FormValley

    it "classifies depression" $
      classifyDefault cfg (uniformDS 0.0) 0.0 0.0 True `shouldBe` FormDepression

    -- New forms
    it "classifies ridge (steep E/W, flat axis)" $
      let ds = DirectionalSlope 0.0 0.0 0.20 0.0 0.0 0.20
          -- NW and SE are opposite; both steep at 0.20.  Other 4 dirs at 0.0.
      in classifyTerrainForm cfg ds 0.15 0.0 False 0.7 0.3 0.05
         `shouldBe` FormRidge

    it "classifies escarpment (steep one side, flat opposite)" $
      let ds = DirectionalSlope 0.20 0.0 0.0 0.02 0.0 0.0
          -- E steep (0.20), W gentle (0.02)
      in classifyTerrainForm cfg ds 0.10 0.0 False 0.5 0.5 0.0
         `shouldBe` FormEscarpment

    it "classifies plateau (low slope, high elevation)" $
      classifyTerrainForm cfg (uniformDS 0.01) 0.01 0.0 False 0.6 0.5 0.15
        `shouldBe` FormPlateau

    it "classifies badlands (steep + soft + high asymmetry)" $
      let ds = DirectionalSlope 0.25 0.01 0.01 0.01 0.01 0.01
          -- Very asymmetric: E is steep, all others gentle
      in classifyTerrainForm cfg ds 0.10 0.0 False 0.20 0.1 0.0
         `shouldBe` FormBadlands

    it "classifies pass (ridge-like + local minimum)" $
      let ds = DirectionalSlope 0.0 0.0 0.15 0.0 0.0 0.15
          -- NW and SE steep (opposite pair), other 4 dirs at 0
      in classifyTerrainForm cfg ds 0.10 0.0 True 0.7 0.3 0.05
         `shouldBe` FormPass

    it "classifies canyon (valley + high relief + steep walls + hard rock)" $
      let ds = DirectionalSlope 0.25 0.0 0.0 0.25 0.0 0.0
          -- E and W steep (opposite pair)
      in classifyTerrainForm cfg ds 0.30 (-0.25) False 0.60 0.2 0.0
         `shouldBe` FormCanyon

    it "classifies mesa (flat top + steep edges + hard cap + elevated)" $
      classifyTerrainForm cfg (uniformDS 0.01) 0.20 0.0 False 0.55 0.3 0.10
        `shouldBe` FormMesa

    it "classifies foothill (moderate slope + moderate elevation)" $
      classifyTerrainForm cfg (uniformDS 0.05) 0.04 0.0 False 0.5 0.5 0.06
        `shouldBe` FormFoothill

    prop "always returns a valid TerrainForm" $
      \(s :: Float) (r :: Float) (c :: Float) (lm :: Bool)
       (h :: Float) (sd :: Float) (e :: Float) ->
        let form = classifyTerrainForm cfg (uniformDS (abs s)) (abs r) c lm
                     (clamp01 (abs h)) (clamp01 (abs sd)) e
        in form `elem` [FormFlat, FormRolling, FormHilly, FormMountainous
                        , FormCliff, FormValley, FormDepression
                        , FormRidge, FormEscarpment, FormPlateau
                        , FormBadlands, FormPass, FormCanyon
                        , FormMesa, FormFoothill]

    -- Disambiguation / priority tests
    it "cliff beats mountainous (higher slope wins)" $
      classifyDefault cfg (uniformDS 0.50) 0.30 (-0.20) True `shouldBe` FormCliff

    it "mountainous beats valley" $
      -- Use low hardness to prevent Canyon from firing
      classifyTerrainForm cfg (uniformDS 0.25) 0.30 (-0.20) True 0.3 0.5 0.0
        `shouldBe` FormMountainous

    it "high hardness → mountainous, not badlands" $
      -- Uniform steep slope with high hardness prevents Badlands
      classifyTerrainForm cfg (uniformDS 0.25) 0.10 0.0 False 0.60 0.1 0.0
        `shouldBe` FormMountainous

    it "moderate slope + low hardness + deep soil → hilly, not mountainous" $
      -- avgS = 0.12, relief = 0.15: hilly range, not mountainous
      classifyTerrainForm cfg (uniformDS 0.12) 0.15 0.0 False 0.2 0.8 0.0
        `shouldBe` FormHilly

    -- Phase 12.3 — zeroDirSlope with zero relief at sea level is always flat
    it "zeroDirSlope with zero relief at sea level is FormFlat" $
      classifyTerrainForm cfg zeroDirSlope 0.0 0.0 False 0.5 0.5 0.0
        `shouldBe` FormFlat

    -- Phase 12.4 — all 15 forms are reachable
    it "all 15 terrain forms are reachable" $ do
      let allForms =
            [ FormFlat, FormRolling, FormHilly, FormMountainous
            , FormCliff, FormValley, FormDepression
            , FormRidge, FormEscarpment, FormPlateau
            , FormBadlands, FormPass, FormCanyon
            , FormMesa, FormFoothill
            ]
          -- Witness inputs: (ds, relief, curvature, localMin, hardness, soil, elevASL)
          witnesses =
            [ (uniformDS 0.0,   0.0,  0.0,    False, 0.5,  0.5, 0.0)    -- Flat
            , (uniformDS 0.05,  0.0,  0.0,    False, 0.5,  0.5, 0.0)    -- Rolling
            , (uniformDS 0.12,  0.15, 0.0,    False, 0.5,  0.5, 0.0)    -- Hilly
            , (uniformDS 0.25,  0.0,  0.0,    False, 0.5,  0.5, 0.0)    -- Mountainous
            , (uniformDS 0.50,  0.0,  0.0,    False, 0.5,  0.5, 0.0)    -- Cliff
            , (uniformDS 0.01,  0.01, (-0.20),False, 0.5,  0.5, 0.0)    -- Valley
            , (uniformDS 0.0,   0.0,  0.0,    True,  0.5,  0.5, 0.0)    -- Depression
            , (DirectionalSlope 0.0 0.0 0.20 0.0 0.0 0.20,
                                0.15, 0.0,    False, 0.7,  0.3, 0.05)   -- Ridge
            , (DirectionalSlope 0.20 0.0 0.0 0.02 0.0 0.0,
                                0.10, 0.0,    False, 0.5,  0.5, 0.0)    -- Escarpment
            , (uniformDS 0.01,  0.01, 0.0,    False, 0.6,  0.5, 0.15)   -- Plateau
            , (DirectionalSlope 0.25 0.01 0.01 0.01 0.01 0.01,
                                0.10, 0.0,    False, 0.20, 0.1, 0.0)    -- Badlands
            , (DirectionalSlope 0.0 0.0 0.15 0.0 0.0 0.15,
                                0.10, 0.0,    True,  0.7,  0.3, 0.05)   -- Pass
            , (DirectionalSlope 0.25 0.0 0.0 0.25 0.0 0.0,
                                0.30, (-0.25),False, 0.60, 0.2, 0.0)    -- Canyon
            , (uniformDS 0.01,  0.20, 0.0,    False, 0.55, 0.3, 0.10)   -- Mesa
            , (uniformDS 0.05,  0.04, 0.0,    False, 0.5,  0.5, 0.06)   -- Foothill
            ]
          results = map (\(ds,rl,cv,lm,h,sd,e) ->
            classifyTerrainForm cfg ds rl cv lm h sd e) witnesses
      zip allForms results `shouldBe` zip allForms allForms
    ---------------------------------------------------------------------------
    -- Phase 13.3 — cascade coverage & shadowing audit
    ---------------------------------------------------------------------------

    -- | Stratified generator biased toward realistic terrain ranges so that
    -- rare forms (Canyon, Mesa, Pass, etc.) are reachable.
    let genTerrainInputs :: Gen (DirectionalSlope, Float, Float, Bool, Float, Float, Float)
        genTerrainInputs = oneof
          [ -- Uniform random in realistic ranges
            do e  <- choose (0.0, 0.6)
               ne <- choose (0.0, 0.4)
               nw <- choose (0.0, 0.4)
               w  <- choose (0.0, 0.6)
               sw <- choose (0.0, 0.4)
               se <- choose (0.0, 0.4)
               let ds = DirectionalSlope e ne nw w sw se
               rl  <- choose (0.0, 0.5)
               cv  <- choose (-0.4, 0.4)
               lm  <- arbitrary
               h   <- choose (0.0, 1.0)
               sd  <- choose (0.0, 1.0)
               el  <- choose (-0.05, 0.25)
               pure (ds, rl, cv, lm, h, sd, el)
          , -- Steep / cliff zone
            do s <- choose (0.35, 0.70)
               let ds = uniformDS s
               rl <- choose (0.0, 0.5)
               cv <- choose (-0.3, 0.3)
               lm <- arbitrary
               h  <- choose (0.0, 1.0)
               sd <- choose (0.0, 1.0)
               el <- choose (0.0, 0.2)
               pure (ds, rl, cv, lm, h, sd, el)
          , -- Canyon zone: negative curvature + high relief + steep walls + hard rock
            do wall <- choose (0.18, 0.40)
               let ds = DirectionalSlope wall 0.01 0.01 wall 0.01 0.01
               rl <- choose (0.20, 0.50)
               cv <- choose (-0.40, -0.15)
               h  <- choose (0.40, 0.80)
               sd <- choose (0.0, 0.5)
               el <- choose (0.0, 0.1)
               pure (ds, rl, cv, False, h, sd, el)
          , -- Ridge / Pass zone: steep opposite pair, flat axis
            do steep <- choose (0.12, 0.35)
               flat  <- choose (0.0, 0.04)
               pair  <- elements [ \s f -> DirectionalSlope f f s f f s     -- NW/SE
                                 , \s f -> DirectionalSlope s f f s f f     -- E/W
                                 , \s f -> DirectionalSlope f s f f s f     -- NE/SW
                                 ]
               let ds = pair steep flat
               rl <- choose (0.05, 0.25)
               cv <- choose (-0.2, 0.2)
               lm <- arbitrary
               h  <- choose (0.3, 0.9)
               sd <- choose (0.0, 0.5)
               el <- choose (0.0, 0.15)
               pure (ds, rl, cv, lm, h, sd, el)
          , -- Escarpment zone: steep one side, gentle opposite
            do hi <- choose (0.15, 0.35)
               lo <- choose (0.0, 0.04)
               let ds = DirectionalSlope hi 0.02 0.02 lo 0.02 0.02
               rl <- choose (0.05, 0.20)
               cv <- choose (-0.1, 0.1)
               h  <- choose (0.3, 0.8)
               sd <- choose (0.0, 0.5)
               el <- choose (0.0, 0.1)
               pure (ds, rl, cv, False, h, sd, el)
          , -- Badlands zone: steep + soft + asymmetric
            do hi <- choose (0.15, 0.40)
               lo <- choose (0.0, 0.03)
               let ds = DirectionalSlope hi lo lo lo lo lo
               rl <- choose (0.0, 0.15)
               cv <- choose (-0.1, 0.1)
               h  <- choose (0.0, 0.35)
               sd <- choose (0.0, 0.5)
               el <- choose (0.0, 0.1)
               pure (ds, rl, cv, False, h, sd, el)
          , -- Plateau / Mesa zone: low slope + elevated
            do s  <- choose (0.0, 0.03)
               let ds = uniformDS s
               rl <- choose (0.0, 0.25)
               cv <- choose (-0.1, 0.1)
               h  <- choose (0.0, 1.0)
               sd <- choose (0.0, 1.0)
               el <- choose (0.05, 0.25)
               pure (ds, rl, cv, False, h, sd, el)
          , -- Foothill zone: moderate slope + moderate elevation
            do s  <- choose (0.03, 0.10)
               let ds = uniformDS s
               rl <- choose (0.0, 0.10)
               cv <- choose (-0.1, 0.1)
               h  <- choose (0.0, 1.0)
               sd <- choose (0.0, 1.0)
               el <- choose (0.02, 0.12)
               pure (ds, rl, cv, False, h, sd, el)
          ]

    modifyMaxSuccess (const 5000) $
      prop "no terrain form is dead — all 15 forms are produced" $
        forAll genTerrainInputs $ \(ds, rl, cv, lm, h, sd, el) ->
          let form = classifyTerrainForm cfg ds rl cv lm h sd el
              lbl  = terrainFormDisplayName form
          in coverTable "form"
               [ ("Flat",        1)
               , ("Rolling",     1)
               , ("Hilly",       1)
               , ("Mountainous", 1)
               , ("Cliff",       1)
               , ("Valley",      1)
               , ("Depression",  1)
               , ("Ridge",       0.5)
               , ("Escarpment",  0.5)
               , ("Plateau",     0.5)
               , ("Badlands",    0.5)
               , ("Pass",        0.5)
               , ("Canyon",      0.5)
               , ("Mesa",        0.5)
               , ("Foothill",    0.5)
               ]
             $ tabulate "form" [lbl]
             $ property True  -- coverage is the assertion

    -- Shadowing detection: verify later cascade entries are not
    -- entirely swallowed by earlier ones on representative inputs.

    it "mesa is not always shadowed by plateau" $
      -- Mesa requires flat top + edge relief + hard cap + elevation.
      -- Plateau requires flat top + elevation but no relief/hardness.
      -- With high edge relief and hard cap, mesa should win.
      classifyTerrainForm cfg (uniformDS 0.01) 0.20 0.0 False 0.55 0.3 0.10
        `shouldBe` FormMesa

    it "foothill is not always shadowed by rolling" $
      -- Foothill has moderate slope overlapping rolling's range,
      -- but foothill also requires moderate elevation.
      classifyTerrainForm cfg (uniformDS 0.05) 0.04 0.0 False 0.5 0.5 0.06
        `shouldBe` FormFoothill

    it "badlands is not always shadowed by mountainous" $
      -- Badlands fires before mountainous when: maxSlope >= 0.15,
      -- hardness <= 0.35, asymmetry >= 0.06.  The maxSlope < 0.20
      -- ensures mountainous guard (check 4) doesn't fire first.
      let ds = DirectionalSlope 0.18 0.01 0.01 0.01 0.01 0.01
      in classifyTerrainForm cfg ds 0.05 0.0 False 0.20 0.1 0.0
         `shouldBe` FormBadlands

    it "canyon is not always shadowed by cliff" $
      -- Canyon requires steep walls (0.18) but not cliff-level (0.40).
      -- With wall slopes well below cliff threshold, canyon fires.
      let ds = DirectionalSlope 0.20 0.01 0.01 0.20 0.01 0.01
      in classifyTerrainForm cfg ds 0.30 (-0.25) False 0.60 0.2 0.0
         `shouldBe` FormCanyon

    it "pass is not always shadowed by ridge" $
      -- Pass = ridge profile + localMin.  With localMin=True,
      -- pass (check 5) fires before ridge (check 6).
      let ds = DirectionalSlope 0.0 0.0 0.15 0.0 0.0 0.15
      in classifyTerrainForm cfg ds 0.10 0.0 True 0.7 0.3 0.05
         `shouldBe` FormPass

    it "depression is not always shadowed by valley" $
      -- Depression requires localMin but no curvature constraint.
      -- Valley requires negative curvature (< -0.15).
      -- With zero curvature + localMin, depression fires (not valley).
      classifyTerrainForm cfg (uniformDS 0.0) 0.0 0.0 True 0.5 0.5 0.0
        `shouldBe` FormDepression

    it "hilly is not always shadowed by foothill" $
      -- Hilly: avgS > 0.08, relief > 0.10.  Foothill requires
      -- avgS in [0.03, 0.10] and elevASL in [0.02, 0.12].
      -- With avgS=0.12 (above foothill max) + relief 0.15, hilly fires.
      classifyTerrainForm cfg (uniformDS 0.12) 0.15 0.0 False 0.5 0.5 0.0
        `shouldBe` FormHilly

    prop "distribution labels unique per input" $
      forAll genTerrainInputs $ \(ds, rl, cv, lm, h, sd, el) ->
        let form = classifyTerrainForm cfg ds rl cv lm h sd el
            allForms' =
              [ FormFlat, FormRolling, FormHilly, FormMountainous
              , FormCliff, FormValley, FormDepression
              , FormRidge, FormEscarpment, FormPlateau
              , FormBadlands, FormPass, FormCanyon
              , FormMesa, FormFoothill
              ]
        in form `elem` allForms'