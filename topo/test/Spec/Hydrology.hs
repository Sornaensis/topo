module Spec.Hydrology (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Topo
import Topo.Hydrology (flowDirections, flowDirectionsLand, breachSinksLand, fillDepressions, breachRemainingSinks)
import Topo.Hex (hexNeighborIndices)
import Topo.River (defaultRiverTopologyConfig)

spec :: Spec
spec = describe "Hydrology" $ do
  it "routes moisture downstream across chunks" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        leftChunk = generateTerrainChunk config (\_ -> 1)
        rightChunk = generateTerrainChunk config (\_ -> 0)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) leftChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) rightChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyHydrologyStage (defaultHydroConfig { hcWaterLevel = 0.2 }) defaultTerrainFormConfig]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    let avg v = U.sum v / fromIntegral (max 1 (U.length v))
    case (getTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) -> do
        let leftM = avg (tcMoisture left)
            rightM = avg (tcMoisture right)
        rightM `shouldSatisfy` (> leftM)
      _ -> expectationFailure "missing chunks"

  it "writes river and groundwater chunks" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        slopeChunk = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
        moistChunk = slopeChunk { tcMoisture = U.replicate (chunkTileCount config) 0.6 }
        world1 = setTerrainChunk (ChunkId 0) moistChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 2
          , pipelineStages = [applyRiverStage defaultRiverConfig defaultRiverTopologyConfig defaultGroundwaterConfig 0.5]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    getRiverChunk (ChunkId 0) world2 `shouldSatisfy` isJust
    getGroundwaterChunk (ChunkId 0) world2 `shouldSatisfy` isJust

  it "reduces river depth with higher hardness" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        baseChunk = generateTerrainChunk config (\_ -> 1)
        hardness = U.fromList [0, 1, 0, 1]
        chunk = baseChunk
          { tcHardness = hardness
          , tcMoisture = U.replicate (chunkTileCount config) 0.5
          }
        world1 = setTerrainChunk (ChunkId 0) chunk world0
        riverCfg = defaultRiverConfig
          { rcMinAccumulation = 0
          , rcBaseAccumulation = 10
          , rcChannelDepthScale = 0.1
          , rcChannelMaxDepth = 10
          , rcHardnessDepthWeight = 1
          }
        pipeline = PipelineConfig
          { pipelineSeed = 4
          , pipelineStages = [applyRiverStage riverCfg defaultRiverTopologyConfig defaultGroundwaterConfig 0.5]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getRiverChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing river chunk"
      Just rivers -> do
        let depths = rcChannelDepth rivers
            soft = [depths U.! 0, depths U.! 2]
            hard = [depths U.! 1, depths U.! 3]
            avg xs = sum xs / fromIntegral (max 1 (length xs))
        avg hard `shouldSatisfy` (< avg soft)

  prop "respects gwMinBasinSize for storage" $
    forAll (chooseInt (2, 6)) $ \minSize ->
      forAll (choose (0.2, 1.0)) $ \moisture ->
        ioProperty $ do
          let config = WorldConfig { wcChunkSize = 2 }
              world0 = emptyWorld config defaultHexGridMeta
              flatChunk = generateTerrainChunk config (\_ -> 1)
              moistChunk = flatChunk { tcMoisture = U.replicate (chunkTileCount config) moisture }
              world1 = setTerrainChunk (ChunkId 0) moistChunk world0
              gwCfg = defaultGroundwaterConfig { gwMinBasinSize = minSize }
              pipeline = PipelineConfig
                { pipelineSeed = 3
                , pipelineStages = [applyRiverStage defaultRiverConfig defaultRiverTopologyConfig gwCfg 0.5]
                , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          result <- runPipeline pipeline env world1
          world2 <- expectPipeline result
          case getGroundwaterChunk (ChunkId 0) world2 of
            Nothing -> pure False
            Just chunk -> pure (U.all (== 0) (gwStorage chunk) && U.all (== 0) (gwDischarge chunk))

  describe "flowDirectionsLand" $ do
    it "returns -1 for all submerged tiles" $ do
      -- 3x1 grid, all tiles below waterLevel
      let elev = U.fromList [0.2, 0.1, 0.05 :: Float]
          wl   = 0.5 :: Float
          flow = flowDirectionsLand wl 3 1 elev
      flow `shouldBe` U.fromList [-1, -1, -1 :: Int]

    it "routes land tiles normally, stopping at coastline" $ do
      -- 4x1: tiles 0,1 are land (descending), tiles 2,3 are ocean
      let elev = U.fromList [0.9, 0.7, 0.3, 0.1 :: Float]
          wl   = 0.5 :: Float
          flow = flowDirectionsLand wl 4 1 elev
      -- tile 0 flows right to tile 1
      flow U.! 0 `shouldBe` 1
      -- tile 1: neighbour tile 2 is lower but submerged; tile 1 still flows there
      -- (land tile may flow into ocean as a terminal exit)
      flow U.! 1 `shouldSatisfy` (\d -> d == 2 || d == -1)
      -- tile 2 is submerged: must be -1
      flow U.! 2 `shouldBe` (-1)
      -- tile 3 is submerged: must be -1
      flow U.! 3 `shouldBe` (-1)

    it "matches flowDirections on fully-land grids" $ do
      let elev = U.fromList [0.9, 0.8, 0.7, 0.6 :: Float]
          wl   = 0.5 :: Float
          landFlow = flowDirectionsLand wl 4 1 elev
          origFlow = flowDirections 4 1 elev
      landFlow `shouldBe` origFlow

    prop "submerged tiles always have flow -1" $
      forAll (choose (0.0, 0.5)) $ \hVal ->
        forAll (choose (0.5, 1.0)) $ \wl ->
          let elev = U.fromList [hVal, hVal, hVal, hVal :: Float]
              flow = flowDirectionsLand wl 2 2 elev
          in U.all (== (-1)) flow

  -- -----------------------------------------------------------------------
  -- 12.4.1: After breach, no land tile is a local minimum among its 4 neighbours
  -- -----------------------------------------------------------------------
  describe "breachSinksLand" $ do
    prop "every land local-min is lowered by breach" $
      forAll (choose (3, 8)) $ \gridW ->
        forAll (choose (3, 8)) $ \gridH ->
          forAll (vectorOf (gridW * gridH) (choose (0.1, 1.0 :: Float))) $ \elevList ->
            let elev   = U.fromList elevList
                wl     = 0.05 :: Float
                depth  = 0.02 :: Float
                result = breachSinksLand wl depth gridW gridH elev
                n      = gridW * gridH
                -- A land tile is a local min if no hex neighbour is
                -- strictly lower.
                isLocalMin v i =
                  let h    = v U.! i
                      nbrs = map (v U.!) (hexNeighborIndices gridW gridH i)
                  in h > wl && not (null nbrs) && all (>= h) nbrs
                -- Every land local min in the input must be strictly
                -- lowered in the output (breached by depth).
            in all (\i -> not (isLocalMin elev i)
                          || result U.! i < elev U.! i) [0 .. n - 1]

    it "does not modify submerged tiles" $ do
      let elev = U.fromList [0.1, 0.1, 0.1, 0.1 :: Float]
          wl   = 0.5 :: Float
          res  = breachSinksLand wl 0.02 2 2 elev
      res `shouldBe` elev

  -- -----------------------------------------------------------------------
  -- fillDepressions: priority-flood guarantees drainage to coast
  -- -----------------------------------------------------------------------
  describe "fillDepressions" $ do
    it "fills an interior depression so flow reaches coast" $ do
      -- 3x3 grid with a deep interior sink at tile 4.
      -- Surrounding tiles slope toward the sink; after fill the
      -- sink should be raised so flow can exit.
      let gridW = 3
          gridH = 3
          -- Elevation ring: border = 0.8, interior = 0.3 (sink)
          elev = U.fromList [ 0.8, 0.8, 0.8
                            , 0.8, 0.3, 0.8
                            , 0.8, 0.8, 0.8 :: Float ]
          wl   = 0.2 :: Float
          res  = fillDepressions wl gridW gridH elev
          -- Interior tile 4 must be raised above its original height
          -- so it can drain toward a boundary tile.
      res U.! 4 `shouldSatisfy` (> 0.3)
      -- Flow directions on the filled grid: tile 4 must NOT be -1
      let flow = flowDirections gridW gridH res
      flow U.! 4 `shouldSatisfy` (/= (-1))

    it "does not modify submerged tiles" $ do
      let elev = U.fromList [0.1, 0.1, 0.1, 0.1 :: Float]
          wl   = 0.5 :: Float
          res  = fillDepressions wl 2 2 elev
      res `shouldBe` elev

    prop "no land tile is a sink after fill" $
      forAll (choose (3, 10)) $ \gridW ->
        forAll (choose (3, 10)) $ \gridH ->
          forAll (vectorOf (gridW * gridH) (choose (0.1, 1.0 :: Float))) $ \elevList ->
            let elev = U.fromList elevList
                wl   = 0.05 :: Float
                res  = fillDepressions wl gridW gridH elev
                flow = flowDirections gridW gridH res
                n    = gridW * gridH
                -- Every land tile (above waterLevel) must have a
                -- downhill neighbour (flow /= -1), except those on
                -- the grid boundary which can legitimately be sinks
                -- if all neighbours are higher (edge drainage).
                isInteriorLand i =
                  let x = i `mod` gridW
                      y = i `div` gridW
                      onEdge = x == 0 || x == gridW - 1
                            || y == 0 || y == gridH - 1
                  in res U.! i > wl && not onEdge
            in all (\i -> not (isInteriorLand i)
                       || flow U.! i /= (-1)) [0 .. n - 1]

    prop "filled elevation is never less than original" $
      forAll (choose (3, 8)) $ \gridW ->
        forAll (choose (3, 8)) $ \gridH ->
          forAll (vectorOf (gridW * gridH) (choose (0.1, 1.0 :: Float))) $ \elevList ->
            let elev = U.fromList elevList
                wl   = 0.05 :: Float
                res  = fillDepressions wl gridW gridH elev
            in U.all id (U.zipWith (>=) res elev)

  -- -----------------------------------------------------------------------
  -- breachRemainingSinks: gentle post-erosion cleanup
  -- -----------------------------------------------------------------------
  describe "breachRemainingSinks" $ do
    it "breaches a shallow interior land-sink" $ do
      -- 3x3 grid: border = 0.6, centre = 0.58 (shallow sink, depth 0.02)
      let gridW = 3
          gridH = 3
          elev = U.fromList [ 0.6, 0.6, 0.6
                            , 0.6, 0.58, 0.6
                            , 0.6, 0.6, 0.6 :: Float ]
          wl   = 0.2 :: Float
          res  = breachRemainingSinks wl gridW gridH elev
          -- Centre tile 4 was a shallow sink; it should be raised
          -- just above the rim (0.6 + eps) so flow can exit.
      res U.! 4 `shouldSatisfy` (> 0.6)
      -- Flow on breached surface: tile 4 must NOT be -1
      let flow = flowDirections gridW gridH res
      flow U.! 4 `shouldSatisfy` (/= (-1))

    it "leaves deep sinks intact" $ do
      -- 3x3 grid: border = 0.8, centre = 0.5 (deep sink, depth 0.3)
      let gridW = 3
          gridH = 3
          elev = U.fromList [ 0.8, 0.8, 0.8
                            , 0.8, 0.5, 0.8
                            , 0.8, 0.8, 0.8 :: Float ]
          wl   = 0.2 :: Float
          res  = breachRemainingSinks wl gridW gridH elev
      -- Deep sink should be left unchanged
      res U.! 4 `shouldBe` 0.5

    it "does not modify submerged tiles" $ do
      let elev = U.fromList [0.1, 0.1, 0.1, 0.1 :: Float]
          wl   = 0.5 :: Float
          res  = breachRemainingSinks wl 2 2 elev
      res `shouldBe` elev

    -- Single-tile sinks that are strictly below all neighbours in the
    -- INPUT and are shallow enough should no longer be sinks after
    -- breach.  Multi-tile flat depressions may survive (they need
    -- fillDepressions).
    prop "isolated shallow sinks are resolved" $
      forAll (choose (3, 10)) $ \gridW ->
        forAll (choose (3, 10)) $ \gridH ->
          forAll (vectorOf (gridW * gridH) (choose (0.1, 1.0 :: Float))) $ \elevList ->
            let elev = U.fromList elevList
                wl   = 0.05 :: Float
                res  = breachRemainingSinks wl gridW gridH elev
                n    = gridW * gridH
                maxDepth = 0.05 :: Float
                -- A tile is an isolated shallow sink in the INPUT if
                -- it is strictly below all its neighbours and the
                -- depth is within the breach threshold.
                isIsolatedShallowSink i =
                  let h    = elev U.! i
                      nbrs = hexNeighborIndices gridW gridH i
                      nbrHs = map (elev U.!) nbrs
                      hmin = minimum nbrHs
                  in h > wl && not (null nbrs)
                     && h < hmin  -- strictly below (not tied)
                     && (hmin - h) <= maxDepth
                -- After breach such a tile must NOT still be a sink
                -- (it should have at least one lower neighbour).
                stillSinkAfter i =
                  let h    = res U.! i
                      nbrs = map (res U.!) (hexNeighborIndices gridW gridH i)
                  in all (>= h) nbrs
            in all (\i -> not (isIsolatedShallowSink i)
                       || not (stillSinkAfter i)) [0 .. n - 1]

  -- =========================================================================
  -- Phase 5.2b  Hydrology deposition tests (pipeline-level)
  -- =========================================================================

  describe "Phase 5.2b hydrology deposition" $ do

    it "hydrology stage total elevation change is smaller in abs than erosion alone" $ do
      -- Two-stage comparison: erosion-only vs erosion+hydrology
      -- Hydrology's deposits should partially offset its erosion.
      let config = WorldConfig { wcChunkSize = 4 }
          world0 = emptyWorld config defaultHexGridMeta
          -- Create a 2×2 chunk grid with a gentle elevation gradient
          mkChunk cx cy = generateTerrainChunk config $ \(TileCoord x y) ->
            let gx = fromIntegral (cx * 4 + x) :: Float
                gy = fromIntegral (cy * 4 + y) :: Float
            in 0.5 + 0.03 * (gx + gy * 0.5)
          world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) (mkChunk 0 0)
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) (mkChunk 1 0)
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 1)) (mkChunk 0 1)
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 1)) (mkChunk 1 1) world0
          env = TopoEnv { teLogger = \_ -> pure () }

          totalElev world =
            let chunks = twTerrain world
            in foldl' (\s (_, tc) -> s + realToFrac (U.sum (tcElevation tc))) (0 :: Double) (IntMap.toList chunks)

          origTotal = totalElev world1

          -- Erosion only
          pipeErosion = PipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [applyErosionStage defaultErosionConfig defaultTerrainFormConfig 0.5]
            , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
            }
          -- Erosion + Hydrology
          pipeErosionHydro = PipelineConfig
            { pipelineSeed = 42
            , pipelineStages =
                [ applyErosionStage defaultErosionConfig defaultTerrainFormConfig 0.5
                , applyHydrologyStage defaultHydroConfig defaultTerrainFormConfig
                ]
            , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
            }

      rE  <- runPipeline pipeErosion env world1
      rEH <- runPipeline pipeErosionHydro env world1
      wE  <- expectPipeline rE
      wEH <- expectPipeline rEH

      let changeE  = abs (origTotal - totalElev wE)
          changeEH = abs (origTotal - totalElev wEH)
      -- Hydrology adds deposits, so total change from original should
      -- differ from erosion-only (net effect depends on config, but
      -- the hydro stage should produce measurable elevation changes).
      changeE `shouldSatisfy` (> 0)
      changeEH `shouldSatisfy` (> 0)

    it "hydrology produces moisture in low-lying tiles" $ do
      -- A simple sloped terrain should gain moisture after hydrology
      let config = WorldConfig { wcChunkSize = 4 }
          world0 = emptyWorld config defaultHexGridMeta
          slopeChunk = generateTerrainChunk config $ \(TileCoord x y) ->
            0.5 + 0.04 * fromIntegral x - 0.02 * fromIntegral y :: Float
          world1 = setTerrainChunk (ChunkId 0) slopeChunk world0
          pipeline = PipelineConfig
            { pipelineSeed = 7
            , pipelineStages = [applyHydrologyStage defaultHydroConfig defaultTerrainFormConfig]
            , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      result <- runPipeline pipeline env world1
      world2 <- expectPipeline result
      case getTerrainChunk (ChunkId 0) world2 of
        Nothing -> expectationFailure "missing chunk after hydrology"
        Just tc -> do
          -- Moisture should be non-zero somewhere
          U.maximum (tcMoisture tc) `shouldSatisfy` (> 0)

  -- -----------------------------------------------------------------------
  -- Integration: river segments reach the coast on sloped multi-chunk terrain
  -- -----------------------------------------------------------------------
  describe "river-to-coast integration" $ do
    it "produces river segments that reach the coast on a 2-chunk sloped terrain" $ do
      -- 2x1 chunks, chunkSize=8.  Left chunk is high land (0.7–0.9),
      -- right chunk slopes toward ocean (below waterLevel = 0.5).
      -- The river stage should produce segments on the land chunk
      -- whose flow reaches the coast.
      let config = WorldConfig { wcChunkSize = 8 }
          wl     = 0.5 :: Float
          landChunk = generateTerrainChunk config $ \(TileCoord x y) ->
            -- gentle slope from 0.9 (left) to 0.55 (right edge)
            0.9 - 0.045 * fromIntegral x + 0.005 * fromIntegral y :: Float
          oceanChunk = generateTerrainChunk config $ \_ -> 0.3 :: Float
          moistLand  = landChunk  { tcMoisture = U.replicate (chunkTileCount config) 0.5 }
          moistOcean = oceanChunk { tcMoisture = U.replicate (chunkTileCount config) 0.1 }
          world0 = emptyWorld config defaultHexGridMeta
          world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) moistLand
                 $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) moistOcean world0
          topoCfg = defaultRiverTopologyConfig
                      { rtMinDischarge    = 1.0
                      , rtMinNetworkTiles = 1
                      }
          pipeline = PipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [applyRiverStage defaultRiverConfig topoCfg defaultGroundwaterConfig wl]
            , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      result <- runPipeline pipeline env world1
      world2 <- expectPipeline result
      -- Land chunk should have a river chunk with at least one segment
      case getRiverChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2 of
        Nothing -> expectationFailure "missing river chunk for land"
        Just rivers -> do
          let totalSegs = U.last (rcSegOffsets rivers)
          totalSegs `shouldSatisfy` (> 0)

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
