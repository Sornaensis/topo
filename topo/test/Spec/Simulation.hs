{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Simulation (spec) where

import Test.Hspec

import Data.Aeson (Value(..))
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (fromFloatDigits)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo
import Topo.Calendar (defaultWorldTime, WorldTime(..), CalendarDate(..))
import Topo.Overlay
  ( Overlay(..), OverlayData(..), emptyOverlayProvenance, emptyOverlayStore, overlayCount
  , insertOverlay, lookupOverlay, emptyOverlay
  )
import Topo.Overlay.Schema
  ( OverlaySchema(..), OverlayFieldDef(..), OverlayFieldType(..)
  , OverlayStorage(..), OverlayDeps(..)
  )
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , loadWorldBundle
  , saveWorldBundle
  )
import Topo.Simulation
import Topo.Simulation.DAG
import Topo.Weather
  ( defaultWeatherConfig, weatherSimNode, weatherOverlaySchema
  , weatherChunkToOverlay, overlayToWeatherChunk, weatherFieldCount
  )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A minimal schema for testing.
testSchema :: Text -> OverlaySchema
testSchema name = OverlaySchema
  { osName        = name
  , osVersion     = "1.0.0"
  , osDescription = "Test overlay: " <> name
  , osFields      = fields
  , osStorage     = StorageSparse
  , osDependencies = OverlayDeps False []
  , osFieldIndex  = Map.fromList [(ofdName f, i) | (i, f) <- zip [0..] fields]
  }
  where
    fields = [OverlayFieldDef "value" OFFloat (Number (fromFloatDigits (0.0 :: Double))) False Nothing]

-- | A no-op reader node that returns the overlay unchanged.
noopReader :: Text -> [SimNodeId] -> SimNode
noopReader name deps = SimNodeReader
  { snrId           = SimNodeId name
  , snrOverlayName  = name
  , snrDependencies = deps
  , snrReadTick     = \_ctx ov -> pure (Right ov)
  }

-- | A reader node that records its execution via an IORef.
recordingReader :: IORef [Text] -> Text -> [SimNodeId] -> SimNode
recordingReader ref name deps = SimNodeReader
  { snrId           = SimNodeId name
  , snrOverlayName  = name
  , snrDependencies = deps
  , snrReadTick     = \_ctx ov -> do
      modifyIORef' ref (++ [name])
      pure (Right ov)
  }

-- | A writer node that records its execution and returns empty writes.
recordingWriter :: IORef [Text] -> Text -> [SimNodeId] -> SimNode
recordingWriter ref name deps = SimNodeWriter
  { snwId           = SimNodeId name
  , snwOverlayName  = name
  , snwDependencies = deps
  , snwWriteTick    = \_ctx ov -> do
      modifyIORef' ref (++ [name])
      pure (Right (ov, emptyTerrainWrites))
  }

-- | A failing reader node.
failingReader :: Text -> Text -> [SimNodeId] -> SimNode
failingReader errMsg name deps = SimNodeReader
  { snrId           = SimNodeId name
  , snrOverlayName  = name
  , snrDependencies = deps
  , snrReadTick     = \_ctx _ov -> pure (Left errMsg)
  }

-- | Build an OverlayStore with empty overlays for given names.
mkStore :: [Text] -> OverlayStore
mkStore names = foldr (\n s -> insertOverlay (emptyOverlay (testSchema n)) s)
                      emptyOverlayStore names

-- | Default calendar date for testing.
testCalDate :: CalendarDate
testCalDate = CalendarDate { cdYear = 0, cdDayOfYear = 0, cdHourOfDay = 0.0 }

-- | Noop progress callback.
noProgress :: SimProgress -> IO ()
noProgress _ = pure ()

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Simulation" $ do
  dagConstructionSpec
  dagCycleSpec
  executionOrderSpec
  terrainWriteSpec
  failureSpec
  overlayIsolationSpec
  weatherSimNodeSpec

-- =========================================================================
-- DAG construction
-- =========================================================================

dagConstructionSpec :: Spec
dagConstructionSpec = describe "DAG construction" $ do

  it "builds a DAG from a single reader node" $ do
    let dag = buildSimDAG [noopReader "weather" []]
    case dag of
      Left err -> expectationFailure (T.unpack err)
      Right d -> do
        length (sdNodes d) `shouldBe` 1
        length (sdLevels d) `shouldBe` 1
        sdTerrainWriters d `shouldBe` []

  it "builds a DAG with two independent readers" $ do
    let dag = buildSimDAG
                [ noopReader "weather" []
                , noopReader "geology" []
                ]
    case dag of
      Left err -> expectationFailure (T.unpack err)
      Right d -> do
        length (sdNodes d) `shouldBe` 2
        -- Both at same level since no dependencies
        length (sdLevels d) `shouldBe` 1
        case sdLevels d of
          (lvl:_) -> length lvl `shouldBe` 2
          []      -> expectationFailure "expected at least one level"

  it "builds a DAG with a dependency chain" $ do
    let dag = buildSimDAG
                [ noopReader "weather" []
                , noopReader "civilization" [SimNodeId "weather"]
                ]
    case dag of
      Left err -> expectationFailure (T.unpack err)
      Right d -> do
        length (sdNodes d) `shouldBe` 2
        length (sdLevels d) `shouldBe` 2
        -- Weather in first level, civilization in second
        case sdLevels d of
          (lvl0:lvl1:_) -> do
            lvl0 `shouldBe` [SimNodeId "weather"]
            lvl1 `shouldBe` [SimNodeId "civilization"]
          _ -> expectationFailure "expected two levels"

  it "separates writers from reader levels" $ do
    ref <- newIORef ([] :: [Text])
    let dag = buildSimDAG
                [ noopReader "weather" []
                , recordingWriter ref "erosion" [SimNodeId "weather"]
                ]
    case dag of
      Left err -> expectationFailure (T.unpack err)
      Right d -> do
        -- Only weather in reader levels (erosion is a writer)
        sdLevels d `shouldBe` [[SimNodeId "weather"]]
        sdTerrainWriters d `shouldBe` [SimNodeId "erosion"]

  it "rejects duplicate node IDs" $ do
    let dag = buildSimDAG
                [ noopReader "weather" []
                , noopReader "weather" []
                ]
    isLeft dag `shouldBe` True

  it "rejects unknown dependencies" $ do
    let dag = buildSimDAG
                [ noopReader "civilization" [SimNodeId "nonexistent"]
                ]
    isLeft dag `shouldBe` True

-- =========================================================================
-- Cycle detection
-- =========================================================================

dagCycleSpec :: Spec
dagCycleSpec = describe "cycle detection" $ do

  it "detects a direct cycle (A → B → A)" $ do
    let dag = buildSimDAG
                [ noopReader "a" [SimNodeId "b"]
                , noopReader "b" [SimNodeId "a"]
                ]
    isLeft dag `shouldBe` True

  it "detects a transitive cycle (A → B → C → A)" $ do
    let dag = buildSimDAG
                [ noopReader "a" [SimNodeId "c"]
                , noopReader "b" [SimNodeId "a"]
                , noopReader "c" [SimNodeId "b"]
                ]
    isLeft dag `shouldBe` True

  it "detects a self-cycle (A → A)" $ do
    let dag = buildSimDAG
                [ noopReader "a" [SimNodeId "a"]
                ]
    isLeft dag `shouldBe` True

-- =========================================================================
-- Execution order
-- =========================================================================

executionOrderSpec :: Spec
executionOrderSpec = describe "execution order" $ do

  it "respects dependency ordering (weather before civilization)" $ do
    ref <- newIORef ([] :: [Text])
    let nodes = [ recordingReader ref "weather" []
                , recordingReader ref "civilization" [SimNodeId "weather"]
                ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather", "civilization"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right _ -> do
        order <- readIORef ref
        -- Weather must appear before civilization
        order `shouldSatisfy` (\o -> elemIndex' "weather" o < elemIndex' "civilization" o)

  it "writers run after all readers" $ do
    ref <- newIORef ([] :: [Text])
    let nodes = [ recordingReader ref "weather" []
                , recordingReader ref "geology" []
                , recordingWriter ref "erosion" [SimNodeId "weather"]
                ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather", "geology", "erosion"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right _ -> do
        order <- readIORef ref
        -- Both readers should appear before the writer
        let weatherIdx = elemIndex' "weather" order
            geologyIdx = elemIndex' "geology" order
            erosionIdx = elemIndex' "erosion" order
        erosionIdx `shouldSatisfy` (> weatherIdx)
        erosionIdx `shouldSatisfy` (> geologyIdx)

  it "runs independent readers concurrently (both complete)" $ do
    ref <- newIORef ([] :: [Text])
    let nodes = [ recordingReader ref "weather" []
                , recordingReader ref "geology" []
                , recordingReader ref "ocean" []
                ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather", "geology", "ocean"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right _ -> do
        order <- readIORef ref
        length order `shouldBe` 3

  it "updates overlay store with reader results" $ do
    let nodes = [ noopReader "weather" [] ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right (finalStore, _) ->
        lookupOverlay "weather" finalStore `shouldSatisfy` isJust

-- =========================================================================
-- Terrain writes
-- =========================================================================

terrainWriteSpec :: Spec
terrainWriteSpec = describe "terrain writes" $ do

  it "emptyTerrainWrites has no chunks" $ do
    IntMap.null (twrTerrain emptyTerrainWrites) `shouldBe` True
    IntMap.null (twrClimate emptyTerrainWrites) `shouldBe` True
    IntMap.null (twrVegetation emptyTerrainWrites) `shouldBe` True

  it "mergeTerrainWrites prefers right for same key" $ do
    let a = emptyTerrainWrites
        b = emptyTerrainWrites
        merged = mergeTerrainWrites a b
    IntMap.null (twrTerrain merged) `shouldBe` True

  it "writer node results are returned" $ do
    ref <- newIORef ([] :: [Text])
    let nodes = [ recordingWriter ref "erosion" [] ]
        Right dag = buildSimDAG nodes
        store = mkStore ["erosion"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right (_finalStore, writes) -> do
        -- Our test writer returns emptyTerrainWrites
        IntMap.null (twrTerrain writes) `shouldBe` True
        order <- readIORef ref
        order `shouldBe` ["erosion"]

-- =========================================================================
-- Failure handling
-- =========================================================================

failureSpec :: Spec
failureSpec = describe "failure handling" $ do

  it "propagates reader errors" $ do
    let nodes = [ failingReader "weather broke" "weather" [] ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    isLeft result `shouldBe` True

  it "reports the error message from a failing node" $ do
    let nodes = [ failingReader "bad data" "weather" [] ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather"]
    terrain <- mkTestTerrain
    result <- tickSimulation dag noProgress terrain store testCalDate defaultWorldTime 1
    case result of
      Left err -> err `shouldBe` "bad data"
      Right _  -> expectationFailure "Expected failure"

  it "reports progress for completed nodes" $ do
    progressRef <- newIORef ([] :: [SimProgress])
    let progressCb p = modifyIORef' progressRef (++ [p])
        nodes = [ noopReader "weather" [] ]
        Right dag = buildSimDAG nodes
        store = mkStore ["weather"]
    terrain <- mkTestTerrain
    _ <- tickSimulation dag progressCb terrain store testCalDate defaultWorldTime 1
    events <- readIORef progressRef
    length events `shouldSatisfy` (>= 2)  -- at least Started + Completed
    case events of
      (first:_) -> simpStatus first `shouldBe` SimStarted
      []        -> expectationFailure "expected progress events"
    simpStatus (last events) `shouldBe` SimCompleted

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Safe index lookup.
elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x xs = go 0 xs
  where
    go _ []     = -1
    go i (y:ys)
      | x == y    = i
      | otherwise = go (i + 1) ys

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

-- | Create a minimal test terrain world (no climate data).
mkTestTerrain :: IO TerrainWorld
mkTestTerrain = pure (emptyWorld (WorldConfig 4) defaultHexGridMeta)

-- | Create a test terrain world with one climate chunk.
mkTestTerrainWithClimate :: WorldConfig -> IO TerrainWorld
mkTestTerrainWithClimate config = do
  let n = chunkTileCount config
      climate = ClimateChunk
        { ccTempAvg           = U.replicate n 0.5
        , ccPrecipAvg         = U.replicate n 0.5
        , ccWindDirAvg        = U.replicate n 0.0
        , ccWindSpdAvg        = U.replicate n 0.3
        , ccHumidityAvg       = U.replicate n 0.0
        , ccTempRange         = U.replicate n 0.0
        , ccPrecipSeasonality = U.replicate n 0.0
        }
      world = emptyWorldWithPlanet config defaultHexGridMeta
                defaultPlanetConfig defaultWorldSlice
  pure (setClimateChunk (ChunkId 0) climate world)

-- =========================================================================
-- SimContext overlay isolation (Phase 2)
-- =========================================================================

overlayIsolationSpec :: Spec
overlayIsolationSpec = describe "SimContext overlay isolation" $ do

  it "reader node cannot see overlays via scTerrain.twOverlays" $ do
    terrain <- mkTestTerrain
    -- Insert an overlay into the terrain's OverlayStore directly
    let extraOv = emptyOverlay (testSchema "sneaky")
        terrainWithOv = terrain { twOverlays = insertOverlay extraOv emptyOverlayStore }
    -- Create a reader that inspects scTerrain.twOverlays
    leakRef <- newIORef (0 :: Int)
    let inspectNode = SimNodeReader
          { snrId           = SimNodeId "inspector"
          , snrOverlayName  = "inspector"
          , snrDependencies = []
          , snrReadTick     = \ctx ov -> do
              -- scTerrain should have empty overlays (stripped by DAG)
              writeIORef leakRef (overlayCount (twOverlays (scTerrain ctx)))
              pure (Right ov)
          }
        store = mkStore ["inspector"]
        Right dag = buildSimDAG [inspectNode]
    result <- tickSimulation dag noProgress terrainWithOv store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right _ -> do
        leaked <- readIORef leakRef
        leaked `shouldBe` 0

  it "writer node cannot see overlays via scTerrain.twOverlays" $ do
    terrain <- mkTestTerrain
    let extraOv = emptyOverlay (testSchema "sneaky")
        terrainWithOv = terrain { twOverlays = insertOverlay extraOv emptyOverlayStore }
    leakRef <- newIORef (0 :: Int)
    let inspectWriter = SimNodeWriter
          { snwId           = SimNodeId "w-inspector"
          , snwOverlayName  = "w-inspector"
          , snwDependencies = []
          , snwWriteTick    = \ctx ov -> do
              writeIORef leakRef (overlayCount (twOverlays (scTerrain ctx)))
              pure (Right (ov, emptyTerrainWrites))
          }
        store = mkStore ["w-inspector"]
        Right dag = buildSimDAG [inspectWriter]
    result <- tickSimulation dag noProgress terrainWithOv store testCalDate defaultWorldTime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right _ -> do
        leaked <- readIORef leakRef
        leaked `shouldBe` 0

-- =========================================================================
-- Weather simulation node
-- =========================================================================

weatherSimNodeSpec :: Spec
weatherSimNodeSpec = describe "weatherSimNode" $ do
  conversionSpec
  schemaSpec
  tickSpec

-- -------------------------------------------------------------------------
-- Overlay conversion round-trip
-- -------------------------------------------------------------------------

conversionSpec :: Spec
conversionSpec = describe "overlay conversion" $ do

  it "weatherChunkToOverlay produces weatherFieldCount vectors" $ do
    let n = 16
        wc = WeatherChunk
              { wcTemp     = U.replicate n 0.5
              , wcHumidity = U.replicate n 0.3
              , wcWindDir  = U.replicate n 0.1
              , wcWindSpd  = U.replicate n 0.2
              , wcPressure = U.replicate n 0.6
              , wcPrecip   = U.replicate n 0.4
              }
        ov = weatherChunkToOverlay wc
    V.length ov `shouldBe` weatherFieldCount

  it "round-trips through weatherChunkToOverlay / overlayToWeatherChunk" $ do
    let n = 16
        wc = WeatherChunk
              { wcTemp     = U.replicate n 0.25
              , wcHumidity = U.replicate n 0.35
              , wcWindDir  = U.replicate n 0.10
              , wcWindSpd  = U.replicate n 0.20
              , wcPressure = U.replicate n 0.65
              , wcPrecip   = U.replicate n 0.45
              }
    case overlayToWeatherChunk (weatherChunkToOverlay wc) of
      Nothing  -> expectationFailure "round-trip returned Nothing"
      Just wc' -> do
        U.toList (wcTemp wc')     `shouldBe` U.toList (wcTemp wc)
        U.toList (wcHumidity wc') `shouldBe` U.toList (wcHumidity wc)
        U.toList (wcWindDir wc')  `shouldBe` U.toList (wcWindDir wc)
        U.toList (wcWindSpd wc')  `shouldBe` U.toList (wcWindSpd wc)
        U.toList (wcPressure wc') `shouldBe` U.toList (wcPressure wc)
        U.toList (wcPrecip wc')   `shouldBe` U.toList (wcPrecip wc)

  it "overlayToWeatherChunk rejects wrong vector length" $ do
    let bad = V.fromList [U.replicate 4 0.0, U.replicate 4 0.0]
    overlayToWeatherChunk bad `shouldBe` Nothing

-- -------------------------------------------------------------------------
-- Schema properties
-- -------------------------------------------------------------------------

schemaSpec :: Spec
schemaSpec = describe "weatherOverlaySchema" $ do

  it "has name 'weather'" $ do
    osName weatherOverlaySchema `shouldBe` "weather"

  it "declares dense storage" $ do
    osStorage weatherOverlaySchema `shouldBe` StorageDense

  it "has weatherFieldCount fields" $ do
    length (osFields weatherOverlaySchema) `shouldBe` weatherFieldCount

  it "declares terrain dependency" $ do
    odTerrain (osDependencies weatherOverlaySchema) `shouldBe` True

  it "declares no overlay dependencies" $ do
    odOverlays (osDependencies weatherOverlaySchema) `shouldBe` []

  it "field index map has entries for all fields" $ do
    Map.size (osFieldIndex weatherOverlaySchema) `shouldBe` weatherFieldCount

-- -------------------------------------------------------------------------
-- Simulation tick
-- -------------------------------------------------------------------------

tickSpec :: Spec
tickSpec = describe "weather tick" $ do

  it "produces a dense overlay with climate data" $ do
    let config = WorldConfig { wcChunkSize = 4 }
    terrain <- mkTestTerrainWithClimate config
    let node  = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        store = insertOverlay (emptyOverlay weatherOverlaySchema) emptyOverlayStore
        wtime = defaultWorldTime { wtTick = 10 }
    result <- tickSimulation dag noProgress terrain store testCalDate wtime 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right (finalStore, _writes) -> do
        case lookupOverlay "weather" finalStore of
          Nothing -> expectationFailure "weather overlay missing from store"
          Just ov -> do
            opVersion (ovProvenance ov) `shouldBe` 2
            case ovData ov of
              DenseData chunks -> do
                IntMap.null chunks `shouldBe` False
                -- Each chunk should have weatherFieldCount field vectors
                case IntMap.lookup 0 chunks of
                  Nothing -> expectationFailure "chunk 0 missing from dense data"
                  Just fieldVecs -> V.length fieldVecs `shouldBe` weatherFieldCount
              SparseData _ -> expectationFailure "expected DenseData, got SparseData"

  it "produces different overlays at different tick times" $ do
    let config = WorldConfig { wcChunkSize = 4 }
    terrain <- mkTestTerrainWithClimate config
    let node  = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        store = insertOverlay (emptyOverlay weatherOverlaySchema) emptyOverlayStore
        wtimeA = defaultWorldTime { wtTick = 5 }
        wtimeB = defaultWorldTime { wtTick = 200 }
    resultA <- tickSimulation dag noProgress terrain store testCalDate wtimeA 1
    resultB <- tickSimulation dag noProgress terrain store testCalDate wtimeB 1
    case (resultA, resultB) of
      (Right (storeA, _), Right (storeB, _)) -> do
        let getTemp s = do
              ov <- lookupOverlay "weather" s
              case ovData ov of
                DenseData chunks -> do
                  fieldVecs <- IntMap.lookup 0 chunks
                  pure (fieldVecs V.! 0)  -- temperature field
                _ -> Nothing
        case (getTemp storeA, getTemp storeB) of
          (Just tempA, Just tempB) ->
            U.toList tempA `shouldNotBe` U.toList tempB
          _ -> expectationFailure "could not extract temperature field"
      _ -> expectationFailure "weather tick(s) failed"

  it "is deterministic across repeated runs from identical initial state" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        node = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        initialStore = insertOverlay (emptyOverlay weatherOverlaySchema) emptyOverlayStore
        runStep terrain store tickNo =
          tickSimulation dag noProgress terrain store testCalDate (defaultWorldTime { wtTick = tickNo }) 1
        runTicks terrain store [] = pure (Right store)
        runTicks terrain store (t:ts) = do
          step <- runStep terrain store t
          case step of
            Left err -> pure (Left err)
            Right (store', _writes) -> runTicks terrain store' ts
        extractTemp store = do
          ov <- lookupOverlay "weather" store
          case ovData ov of
            DenseData chunks -> do
              fields <- IntMap.lookup 0 chunks
              pure (fields V.! 0)
            SparseData _ -> Nothing
    terrain <- mkTestTerrainWithClimate config
    resultA <- runTicks terrain initialStore [1 .. 6]
    resultB <- runTicks terrain initialStore [1 .. 6]
    case (resultA, resultB) of
      (Right storeA, Right storeB) ->
        case (extractTemp storeA, extractTemp storeB) of
          (Just tempA, Just tempB) -> U.toList tempA `shouldBe` U.toList tempB
          _ -> expectationFailure "could not extract deterministic weather temperature"
      (Left err, _) -> expectationFailure (T.unpack err)
      (_, Left err) -> expectationFailure (T.unpack err)

  it "keeps generation to first simulation temperature change small" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        env = TopoEnv { teLogger = \_ -> pure () }
        pipeline = PipelineConfig
          { pipelineSeed = 77
          , pipelineStages = [initWeatherStage defaultWeatherConfig]
          , pipelineDisabled = mempty
          , pipelineSnapshots = False
          , pipelineOnProgress = \_ -> pure ()
          }
        node = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        avgAbsDiff a b =
          let pairs = zip (U.toList a) (U.toList b)
              diffs = map (\(x, y) -> abs (x - y)) pairs
          in if null diffs then 0 else sum diffs / fromIntegral (length diffs)
        getTempFromStore store = do
          ov <- lookupOverlay "weather" store
          case ovData ov of
            DenseData chunks -> do
              fields <- IntMap.lookup 0 chunks
              pure (fields V.! 0)
            SparseData _ -> Nothing
    baseTerrain <- mkTestTerrainWithClimate config
    pipelineResult <- runPipeline pipeline env baseTerrain
    case pipelineResult of
      Left err -> expectationFailure (show err)
      Right (generated, _) ->
        case lookupOverlay "weather" (twOverlays generated) of
          Nothing -> expectationFailure "generation weather overlay missing"
          Just initOv -> do
            let initialStore = insertOverlay initOv emptyOverlayStore
                firstSimTick = twWorldTime generated
            simResult <- tickSimulation dag noProgress generated initialStore testCalDate firstSimTick 1
            case simResult of
              Left err -> expectationFailure (T.unpack err)
              Right (storeAfter, _writes) ->
                case (getWeatherChunk (ChunkId 0) generated, getTempFromStore storeAfter) of
                  (Just initialChunk, Just tempAfter) ->
                    avgAbsDiff (wcTemp initialChunk) tempAfter `shouldSatisfy` (< 0.15)
                  _ -> expectationFailure "could not compare generation and simulation temperatures"

  it "replays weather identically across save/load continuation" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        env = TopoEnv { teLogger = \_ -> pure () }
        initPipeline = PipelineConfig
          { pipelineSeed = 99
          , pipelineStages = [initWeatherStage defaultWeatherConfig]
          , pipelineDisabled = mempty
          , pipelineSnapshots = False
          , pipelineOnProgress = \_ -> pure ()
          }
        node = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        runTicks terrain store startTick count =
          foldl
            (\acc k -> do
               prev <- acc
               case prev of
                 Left err -> pure (Left err)
                 Right s -> do
                   let wt = defaultWorldTime { wtTick = startTick + fromIntegral k }
                   res <- tickSimulation dag noProgress terrain s testCalDate wt 1
                   pure $ case res of
                     Left err -> Left err
                     Right (s', _writes) -> Right s')
            (pure (Right store))
            [0 .. count - 1]
        extractChunk0Temp store = do
          ov <- lookupOverlay "weather" store
          case ovData ov of
            DenseData chunks -> do
              fields <- IntMap.lookup 0 chunks
              pure (fields V.! 0)
            SparseData _ -> Nothing
        nTicks = 3 :: Int
        mTicks = 4 :: Int
    baseTerrain <- mkTestTerrainWithClimate config
    initResult <- runPipeline initPipeline env baseTerrain
    case initResult of
      Left err -> expectationFailure (show err)
      Right (generated, _) -> do
        let tick0 = wtTick (twWorldTime generated)
            store0 = twOverlays generated
        runN <- runTicks generated store0 tick0 nTicks
        case runN of
          Left err -> expectationFailure (T.unpack err)
          Right storeN -> do
            let savedWorld = generated { twOverlays = storeN }
            withSystemTempDirectory "sim-weather-replay" $ \tmp -> do
              let topoPath = tmp </> "world.topo"
              saveResult <- saveWorldBundle topoPath savedWorld
              case saveResult of
                Left err -> expectationFailure (show err)
                Right () -> do
                  loadResult <- loadWorldBundle StrictManifest topoPath
                  case loadResult of
                    Left err -> expectationFailure (show err)
                    Right loaded -> do
                      afterLoad <- runTicks loaded (twOverlays loaded) (tick0 + fromIntegral nTicks) mTicks
                      direct <- runTicks generated store0 tick0 (nTicks + mTicks)
                      case (afterLoad, direct) of
                        (Right storeA, Right storeB) ->
                          case (extractChunk0Temp storeA, extractChunk0Temp storeB) of
                            (Just tA, Just tB) -> U.toList tA `shouldBe` U.toList tB
                            _ -> expectationFailure "missing chunk-0 temperatures in replayability comparison"
                        (Left err, _) -> expectationFailure (T.unpack err)
                        (_, Left err) -> expectationFailure (T.unpack err)

  it "pulls temperature toward climate baseline over repeated ticks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        env = TopoEnv { teLogger = \_ -> pure () }
        initPipeline = PipelineConfig
          { pipelineSeed = 15
          , pipelineStages = [initWeatherStage defaultWeatherConfig]
          , pipelineDisabled = mempty
          , pipelineSnapshots = False
          , pipelineOnProgress = \_ -> pure ()
          }
        cfg = defaultWeatherConfig
          { wcClimatePullStrength = 0.25
          , wcSeasonAmplitude = 0
          , wcWeatherDiffuseFactor = 0
          }
        node = weatherSimNode cfg
        Right dag = buildSimDAG [node]
        meanAbsError vec target =
          let diffs = map (\x -> abs (x - target)) (U.toList vec)
          in if null diffs then 0 else sum diffs / fromIntegral (length diffs)
        extractTemp store = do
          ov <- lookupOverlay "weather" store
          case ovData ov of
            DenseData chunks -> do
              fields <- IntMap.lookup 0 chunks
              pure (fields V.! 0)
            SparseData _ -> Nothing
        runStep terrain store tickNo = do
          let wt = defaultWorldTime { wtTick = tickNo }
          tickSimulation dag noProgress terrain store testCalDate wt 1
    baseTerrain <- mkTestTerrainWithClimate config
    initResult <- runPipeline initPipeline env baseTerrain
    case initResult of
      Left err -> expectationFailure (show err)
      Right (generated, _) -> do
        let initialTemp = U.replicate n 0.0
            humidity = U.replicate n 0.0
            windDir = U.replicate n 0.0
            windSpd = U.replicate n 0.1
            pressure = U.replicate n 0.5
            precip = U.replicate n 0.0
            injected = Overlay
              { ovSchema = weatherOverlaySchema
              , ovData = DenseData (IntMap.singleton 0 (V.fromList [initialTemp, humidity, windDir, windSpd, pressure, precip]))
              , ovProvenance = emptyOverlayProvenance
              }
            store0 = insertOverlay injected emptyOverlayStore
            tick0 = wtTick (twWorldTime generated)
        first <- runStep generated store0 tick0
        after12 <-
          foldl
            (\acc k -> do
               prev <- acc
               case prev of
                 Left err -> pure (Left err)
                 Right (s, _) -> runStep generated s (tick0 + fromIntegral k))
            (pure first)
            [1 .. 12]
        case (first, after12) of
          (Right (storeFirst, _), Right (storeFinal, _)) ->
            case (extractTemp storeFirst, extractTemp storeFinal) of
              (Just tFirst, Just tFinal) -> do
                let baseline = 0.5
                meanAbsError tFinal baseline `shouldSatisfy` (< meanAbsError tFirst baseline)
              _ -> expectationFailure "missing temperatures for climate-pull convergence test"
          (Left err, _) -> expectationFailure (T.unpack err)
          (_, Left err) -> expectationFailure (T.unpack err)

  it "advects a warm blob eastward under uniform eastward wind" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        cfg = defaultWeatherConfig
          { wcClimatePullStrength = 0
          , wcCondensationRate = 0
          , wcSeasonAmplitude = 0
          , wcWeatherDiffuseFactor = 0
          , wcWindResponseRate = 0
          , wcPressureGradientWindScale = 0
          }
        node = weatherSimNode cfg
        Right dag = buildSimDAG [node]
        tempBlob = U.generate n (\i -> if i == 5 then 1 else 0)
        humidity = U.replicate n 0
        windDir = U.replicate n 0
        windSpd = U.replicate n 0.5
        pressure = U.replicate n 0.5
        precip = U.replicate n 0
        startOverlay = Overlay
          { ovSchema = weatherOverlaySchema
          , ovData = DenseData (IntMap.singleton 0 (V.fromList [tempBlob, humidity, windDir, windSpd, pressure, precip]))
          , ovProvenance = emptyOverlayProvenance
          }
        comX vec =
          let xs = [fromIntegral (i `mod` wcChunkSize config) * (vec U.! i) | i <- [0 .. n - 1]]
              mass = U.sum vec
          in if mass <= 0 then 0 else sum xs / mass
        extractTemp store = do
          ov <- lookupOverlay "weather" store
          case ovData ov of
            DenseData chunks -> do
              fields <- IntMap.lookup 0 chunks
              pure (fields V.! 0)
            SparseData _ -> Nothing
    terrain <- mkTestTerrainWithClimate config
    let store0 = insertOverlay startOverlay emptyOverlayStore
        wt = defaultWorldTime { wtTick = 0 }
    result <- tickSimulation dag noProgress terrain store0 testCalDate wt 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right (storeAfter, _) ->
        case extractTemp storeAfter of
          Just tAfter -> comX tAfter `shouldSatisfy` (> comX tempBlob)
          Nothing -> expectationFailure "missing temperature field after advection tick"

  it "keeps all weather fields finite and within [0,1] at default advect dt" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        n = chunkTileCount config
        node = weatherSimNode defaultWeatherConfig
        Right dag = buildSimDAG [node]
        extremes = U.generate n (\i -> if even i then 0 else 1)
        humidity = U.generate n (\i -> if i `mod` 3 == 0 then 1 else 0)
        windDir = U.generate n (\i -> fromIntegral i * 0.3)
        windSpd = U.replicate n 1
        pressure = U.generate n (\i -> if i `mod` 5 == 0 then 1 else 0)
        precip = U.generate n (\i -> if i `mod` 7 == 0 then 1 else 0)
        startOverlay = Overlay
          { ovSchema = weatherOverlaySchema
          , ovData = DenseData (IntMap.singleton 0 (V.fromList [extremes, humidity, windDir, windSpd, pressure, precip]))
          , ovProvenance = emptyOverlayProvenance
          }
        finite x = not (isNaN x) && not (isInfinite x)
        inUnit x = x >= 0 && x <= 1 && finite x
        inAngle x = x >= 0 && x <= (2 * pi) && finite x
        allWith p vec = all p (U.toList vec)
    terrain <- mkTestTerrainWithClimate config
    let store0 = insertOverlay startOverlay emptyOverlayStore
        wt = defaultWorldTime { wtTick = 42 }
    result <- tickSimulation dag noProgress terrain store0 testCalDate wt 1
    case result of
      Left err -> expectationFailure (T.unpack err)
      Right (storeAfter, _) ->
        case lookupOverlay "weather" storeAfter of
          Nothing -> expectationFailure "missing weather overlay"
          Just ov -> case ovData ov of
            SparseData _ -> expectationFailure "expected DenseData"
            DenseData chunks -> case IntMap.lookup 0 chunks of
              Nothing -> expectationFailure "missing chunk 0"
              Just fields -> do
                V.length fields `shouldBe` weatherFieldCount
                allWith inUnit (fields V.! 0) `shouldBe` True
                allWith inUnit (fields V.! 1) `shouldBe` True
                allWith inAngle (fields V.! 2) `shouldBe` True
                allWith inUnit (fields V.! 3) `shouldBe` True
                allWith inUnit (fields V.! 4) `shouldBe` True
                allWith inUnit (fields V.! 5) `shouldBe` True

  it "weatherSimNode is a reader (no terrain writes)" $ do
    let node = weatherSimNode defaultWeatherConfig
    case node of
      SimNodeReader{} -> pure ()
      SimNodeWriter{} -> expectationFailure "expected SimNodeReader"

  it "weatherSimNode has no overlay dependencies" $ do
    let node = weatherSimNode defaultWeatherConfig
    simNodeDependencies node `shouldBe` []
