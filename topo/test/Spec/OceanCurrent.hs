module Spec.OceanCurrent (spec) where

import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

mkOceanCurrentWorld :: WorldConfig -> WorldSlice -> [Int] -> TerrainWorld
mkOceanCurrentWorld config slice landIndices =
  let n = chunkTileCount config
      elevationAt i = if i `elem` landIndices then 0.8 else 0.2
      terrain = (emptyTerrainChunk config)
        { tcElevation = U.generate n elevationAt
        }
      climate = (emptyClimateChunk config)
        { ccTempAvg = U.replicate n 0.5
        }
      baseWorld = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice
      cid = ChunkId 0
  in setClimateChunk cid climate (setTerrainChunk cid terrain baseWorld)

twoStepNeighbor :: Int -> HexDirection -> Int -> Maybe Int
twoStepNeighbor chunkSize dir idx = do
  step1 <- hexNeighborIndexInDirection chunkSize chunkSize dir idx
  hexNeighborIndexInDirection chunkSize chunkSize dir step1

spec :: Spec
spec = describe "OceanCurrent" $ do
  let cfg = defaultOceanCurrentConfig

  describe "oceanCurrentOffset" $ do
    it "is 0 for non-coastal ocean tiles" $ do
      -- No land in either direction -> no current effect
      oceanCurrentOffset cfg 0.5 False False `shouldBe` 0.0

    it "warms western boundary (land to the east)" $ do
      let latRad = 35 * (pi / 180)  -- peak latitude
          offset = oceanCurrentOffset cfg latRad True False
      offset `shouldSatisfy` (> 0)

    it "cools eastern boundary (land to the west)" $ do
      let latRad = 35 * (pi / 180)
          offset = oceanCurrentOffset cfg latRad False True
      offset `shouldSatisfy` (< 0)

    it "cancels in narrow straits (land on both sides)" $ do
      let latRad = 35 * (pi / 180)
          offset = oceanCurrentOffset cfg latRad True True
      -- Warm and cold partially cancel
      abs offset `shouldSatisfy` (< occWarmScale cfg)

    it "peaks at the configured latitude" $ do
      let atPeak = abs (oceanCurrentOffset cfg (35 * pi / 180) True False)
          atEquator = abs (oceanCurrentOffset cfg 0 True False)
          atPole = abs (oceanCurrentOffset cfg (85 * pi / 180) True False)
      atPeak `shouldSatisfy` (> atEquator)
      atPeak `shouldSatisfy` (> atPole)

    prop "offset magnitude never exceeds max(warmScale, coldScale)" $
      forAll (choose (-90 :: Float, 90)) $ \latDeg ->
        forAll (elements [(True, False), (False, True), (True, True), (False, False)]) $ \(le, lw) ->
          let latRad = latDeg * (pi / 180)
              offset = oceanCurrentOffset cfg latRad le lw
              maxOffset = max (occWarmScale cfg) (occColdScale cfg)
          in abs offset <= maxOffset + 0.001

  describe "applyOceanCurrentsStage" $ do
    it "warms ocean tiles when land lies two hex steps east" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          slice = defaultWorldSlice { wsLatCenter = 35, wsLatExtent = 5 }
          oceanIdx = 5
          eastLandIdx = case twoStepNeighbor (wcChunkSize config) HexE oceanIdx of
            Nothing -> error "expected east two-step neighbor in test fixture"
            Just idx -> idx
          worldOpen = mkOceanCurrentWorld config slice []
          worldEast = mkOceanCurrentWorld config slice [eastLandIdx]
          pipeline = PipelineConfig
            { pipelineSeed = 1
            , pipelineStages = [applyOceanCurrentsStage cfg 0.5]
            , pipelineDisabled = mempty
            , pipelineSnapshots = False
            , pipelineOnProgress = \_ -> pure ()
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      openResult <- runPipeline pipeline env worldOpen
      eastResult <- runPipeline pipeline env worldEast
      openWorld <- expectPipeline openResult
      eastWorld <- expectPipeline eastResult
      case (getClimateChunk (ChunkId 0) openWorld, getClimateChunk (ChunkId 0) eastWorld) of
        (Just openChunk, Just eastChunk) -> do
          let openTemp = ccTempAvg openChunk U.! oceanIdx
              eastTemp = ccTempAvg eastChunk U.! oceanIdx
          eastTemp `shouldSatisfy` (> openTemp)
        _ -> expectationFailure "missing climate chunks"

    it "cools ocean tiles when land lies two hex steps west" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          slice = defaultWorldSlice { wsLatCenter = 35, wsLatExtent = 5 }
          oceanIdx = 6
          westLandIdx = case twoStepNeighbor (wcChunkSize config) HexW oceanIdx of
            Nothing -> error "expected west two-step neighbor in test fixture"
            Just idx -> idx
          worldOpen = mkOceanCurrentWorld config slice []
          worldWest = mkOceanCurrentWorld config slice [westLandIdx]
          pipeline = PipelineConfig
            { pipelineSeed = 1
            , pipelineStages = [applyOceanCurrentsStage cfg 0.5]
            , pipelineDisabled = mempty
            , pipelineSnapshots = False
            , pipelineOnProgress = \_ -> pure ()
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      openResult <- runPipeline pipeline env worldOpen
      westResult <- runPipeline pipeline env worldWest
      openWorld <- expectPipeline openResult
      westWorld <- expectPipeline westResult
      case (getClimateChunk (ChunkId 0) openWorld, getClimateChunk (ChunkId 0) westWorld) of
        (Just openChunk, Just westChunk) -> do
          let openTemp = ccTempAvg openChunk U.! oceanIdx
              westTemp = ccTempAvg westChunk U.! oceanIdx
          westTemp `shouldSatisfy` (< openTemp)
        _ -> expectationFailure "missing climate chunks"

  describe "defaultOceanCurrentConfig" $ do
    it "warm scale is positive" $
      occWarmScale defaultOceanCurrentConfig `shouldSatisfy` (> 0)

    it "cold scale is positive" $
      occColdScale defaultOceanCurrentConfig `shouldSatisfy` (> 0)

    it "lat peak is at mid-latitudes" $ do
      occLatPeakDeg defaultOceanCurrentConfig `shouldSatisfy` (> 20)
      occLatPeakDeg defaultOceanCurrentConfig `shouldSatisfy` (< 50)
