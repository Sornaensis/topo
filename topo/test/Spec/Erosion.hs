module Spec.Erosion (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Data.List (foldl')
import Topo

spec :: Spec
spec = describe "Erosion" $ do
  it "reduces steep peaks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        peak = generateTerrainChunk config (\(TileCoord x y) -> if x == 1 && y == 1 then 10 else 0)
        world1 = setTerrainChunk (ChunkId 0) peak world0
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyErosionStage defaultErosionConfig defaultTerrainFormConfig 0.5]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let heights = tcElevation chunk
        (U.maximum heights) `shouldSatisfy` (< 10)

  it "erodes across chunk boundaries" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        leftChunk = generateTerrainChunk config (\_ -> 1)
        rightChunk = generateTerrainChunk config (\_ -> 0)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) leftChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) rightChunk world0
        cfg = defaultErosionConfig { ecHydraulicIterations = 1, ecThermalIterations = 0, ecRainRate = 1, ecMaxDrop = 1 }
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyErosionStage cfg defaultTerrainFormConfig 0.5]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2 of
      Nothing -> expectationFailure "missing left chunk"
      Just chunk -> do
        let TileIndex idx = maybe (TileIndex 0) id (tileIndex config (TileCoord 1 0))
            h = tcElevation chunk U.! idx
        -- After 1 hydraulic iteration the boundary tile should be
        -- noticeably eroded from 1.0, though not necessarily all the
        -- way to 0.
        h `shouldSatisfy` (< 1)

  -- =========================================================================
  -- Phase 5.2  Erosion behavior tests (grid-level functions)
  -- =========================================================================

  describe "Phase 5.2 erosion behavior" $ do

    it "thermal erosion activates on moderate slopes (talus = 0.04)" $ do
      -- 5×5 grid with a 0.1 peak at center; slope = 0.1 > 0.04 talus
      let w = 5
          h = 5
          center = 2 * w + 2
          elev = U.generate (w * h) $ \i ->
            if i == center then 0.6 else 0.5 :: Float
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecThermalIterations = 6
            , ecThermalTalus = 0.04
            , ecThermalStrength = 0.3
            , ecThermalDepositRatio = 0.5
            }
          -- Run 6 thermal iterations
          ones = U.replicate (w * h) (1.0 :: Float)
          eroded = iterate (thermalStepGrid w h 0.5 cfg hardness ones ones) elev !! 6
          h0 = eroded U.! center
      -- Peak should be noticeably lowered from 0.6
      h0 `shouldSatisfy` (< 0.6)
      -- But still above the surrounding 0.5
      h0 `shouldSatisfy` (> 0.5)

    it "hydraulic deposition raises a neighbor of the eroded peak" $ do
      -- 3×1 grid: peak → depression → plateau.  The depression
      -- is the lowest cardinal neighbor of the peak and receives
      -- the deposit.  It is strictly below its own right neighbor
      -- (0.6), so the sink guard allows raising it.
      let w = 3
          h = 1
          elev = U.fromList [0.8, 0.3, 0.6 :: Float]
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecHydraulicIterations = 1
            , ecHydraulicDepositRatio = 0.5
            , ecHydraulicDepositMaxSlope = 0.6
            , ecRainRate = 0.5
            , ecMaxDrop = 0.5
            }
          ones = U.replicate (w * h) (1.0 :: Float)
          eroded = hydraulicStepGrid w h 0.5 cfg hardness ones ones elev
      -- Peak (tile 0) should be lowered
      eroded U.! 0 `shouldSatisfy` (< 0.8)
      -- Depression (tile 1) should receive deposit and be raised
      eroded U.! 1 `shouldSatisfy` (> 0.3)

    it "thermal deposition raises tiles at cliff base" $ do
      -- 5×1 grid: [0.9, 0.9, 0.5, 0.5, 0.5]
      -- Cliff between indices 1 and 2; talus should pile at index 2
      let w = 5
          h = 1
          elev = U.fromList [0.9, 0.9, 0.5, 0.5, 0.5 :: Float]
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecThermalTalus = 0.04
            , ecThermalStrength = 0.5
            , ecThermalDepositRatio = 0.5
            }
          ones = U.replicate (w * h) (1.0 :: Float)
          eroded = thermalStepGrid w h 0.5 cfg hardness ones ones elev
          -- Index 2 (first 0.5 at cliff base) should be raised
          orig2 = elev U.! 2
          new2  = eroded U.! 2
      new2 `shouldSatisfy` (>= orig2)

    it "deposition sink guard: deposit never raises above lowest neighbor" $ do
      -- V-shaped valley: floor is a local minimum
      -- 5×1: [0.7, 0.6, 0.4, 0.6, 0.7]
      -- Index 2 is the valley floor (local min); depositing at it should
      -- not raise it above 0.6 (its lowest neighbors).
      let w = 5
          h = 1
          elev = U.fromList [0.7, 0.6, 0.4, 0.6, 0.7 :: Float]
          hardness = U.replicate (w * h) (0.0 :: Float)
          cfg = defaultErosionConfig
            { ecThermalTalus = 0.04
            , ecThermalStrength = 0.5
            , ecThermalDepositRatio = 1.0  -- aggressive deposit
            , ecHydraulicDepositRatio = 1.0
            , ecHydraulicDepositMaxSlope = 1.0
            }
          ones = U.replicate (w * h) (1.0 :: Float)
          -- Run several iterations of both thermal and hydraulic
          step = thermalStepGrid w h 0.5 cfg hardness ones ones
               . hydraulicStepGrid w h 0.5 cfg hardness ones ones
          eroded = iterate step elev !! 10
          valleyH = eroded U.! 2
          -- Valley should not exceed its lowest neighbor (idx 1 or 3)
          nbrMin = min (eroded U.! 1) (eroded U.! 3)
      valleyH `shouldSatisfy` (<= nbrMin + 1e-6)

    it "coastal smoothing flattens near-shore tiles" $ do
      -- 5×1 grid crossing a land-ocean boundary at waterLevel=0.5
      -- [0.45, 0.45, 0.6, 0.7, 0.8]  (first two are ocean)
      -- The 0.6 tile (index 2) is in the coastal smooth zone and
      -- should be lowered toward its neighbor mean.
      let w = 5
          h = 1
          elev = U.fromList [0.45, 0.45, 0.6, 0.7, 0.8 :: Float]
          cfg = defaultErosionConfig
            { ecCoastalSmoothZone = 0.15
            , ecCoastalSmoothStrength = 0.5
            }
          zeros = U.replicate (w * h) (0.0 :: Float)
          smoothed = coastalSmoothGrid w h 0.5 cfg zeros elev
          origH = elev U.! 2
          newH  = smoothed U.! 2
      -- Coastal tile should be lowered (blended toward neighbor mean
      -- which includes ocean tiles at 0.45)
      newH `shouldSatisfy` (< origH)

    it "erosion with deposition is less subtractive than without" $ do
      -- Scattered-valley pattern: tiles where both x and y are even
      -- sit at 0.3 (valleys); all others at 0.7 (ridges).  Under hex
      -- neighbors every valley's 6 neighbors are ridge tiles, so the
      -- sink-guard always permits deposits.  Ridges each border at
      -- least two valleys, giving erosion plenty of downhill targets.
      let w = 15
          h = 15
          elev = U.generate (w * h) $ \i ->
            let x = i `mod` w
                y = i `div` w
            in if even x && even y then 0.3 else 0.7 :: Float
          hardness = U.replicate (w * h) (0.0 :: Float)
          totalElev v = foldl' (\s i -> s + realToFrac (v U.! i)) (0 :: Double) [0 .. U.length v - 1]
          origTotal = totalElev elev

          -- Config with deposition
          cfgDep = defaultErosionConfig
            { ecHydraulicIterations = 6
            , ecThermalIterations = 6
            , ecHydraulicDepositRatio = 0.8
            , ecHydraulicDepositMaxSlope = 0.8
            , ecThermalDepositRatio = 0.8
            , ecThermalTalus = 0.04
            , ecThermalStrength = 0.5
            , ecRainRate = 0.5
            }

          -- Config without deposition
          cfgNoDep = cfgDep
            { ecHydraulicDepositRatio = 0
            , ecThermalDepositRatio = 0
            }

          applyAll cfg v =
            let ones = U.replicate (w * h) (1.0 :: Float)
                v1 = iterate (hydraulicStepGrid w h 0.5 cfg hardness ones ones) v !! 6
                v2 = iterate (thermalStepGrid w h 0.5 cfg hardness ones ones) v1 !! 6
            in v2

          erodedDep   = applyAll cfgDep elev
          erodedNoDep = applyAll cfgNoDep elev

          lossDep   = origTotal - totalElev erodedDep
          lossNoDep = origTotal - totalElev erodedNoDep

      -- Both should be net subtractive (positive loss)
      lossDep `shouldSatisfy` (> 0)
      lossNoDep `shouldSatisfy` (> 0)
      -- With deposition, less total material is lost
      lossDep `shouldSatisfy` (< lossNoDep)

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
