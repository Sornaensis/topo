{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import System.Directory
  ( copyFile
  , createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

import qualified Topo.Plugin.CivExample as CivExample
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCInvocationScopes(..), RPCInvocationScopeDecl(..), RPCScopeOutput(..)
  , RPCManifest(..), RPCManifestRuntime(..), RPCOverlayDecl(..), RPCSimulationDecl(..), RPCUIHints(..)
  , manifestV3, parseManifestFile, validateManifest
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion, maximumSupportedProtocolVersion)
import Topo.Plugin.RPC.Scope
  ( RPCInvocationKind(..), RPCScopeBudgets(..), ResolvedInvocationScope(..)
  , TerrainSection(..)
  )
import Topo.Plugin.SDK
  ( ClimateChunk(..), GeneratorContext(..), GeneratorDef(..), GeneratorScopeDef(..)
  , GeneratorTickResult(..), Overlay(..), OverlayChunk(..), OverlayData(..)
  , OverlaySchema, OverlayValue(..), PluginContext(..), PluginDef(..)
  , TerrainChunk(..), VegetationChunk(..)
  , SimulationContext(..), SimulationDef(..), SimulationScopeDef(..)
  , SimulationTickResult(..), defaultRecord, fieldIndex
  , decodeTerrainPayload, encodeOverlayPayload, encodeTerrainPayload
  , generateManifest, lookupFieldAt, parseOverlaySchema, pluginManifestFileName
  , setRecordField
  )
import Topo.Overlay.JSON (overlayFromJSON)
import Topo.Simulation.Schedule (hourlyScheduleDecl)
import Topo.Types (ChunkId(..), WorldConfig(..))
import Topo.World
  ( TerrainWorld(..), emptyClimateChunk, emptyTerrainChunk, emptyVegetationChunk
  , emptyWorld, setClimateChunk, setTerrainChunk, setVegetationChunk
  )
import Topo.Hex (defaultHexGridMeta)

main :: IO ()
main = hspec $ do
  describe "topo-plugin-civ-example manifest" $ do
    it "generates a valid manifest v3 from PluginDef" $ do
      let manifest = generateManifest CivExample.civPlugin
      rmManifestVersion manifest `shouldBe` manifestV3
      rmVersion manifest `shouldBe` "1.1.0"
      rmrProtocolMin (rmRuntime manifest) `shouldBe` currentProtocolVersion
      rmrProtocolMax (rmRuntime manifest) `shouldBe` maximumSupportedProtocolVersion
      rmrTopoMin (rmRuntime manifest) `shouldBe` Just "1.0"
      ruiDisplayName (rmUiHints manifest) `shouldBe` Just "Civilization"
      rmDescription manifest `shouldBe`
        "Generated civilization overlay with an immutable sample catalogue"
      rmGenerator manifest `shouldSatisfy` maybe False (const True)
      rmSimulation manifest `shouldSatisfy` maybe False (const True)
      (rsdSchedule <$> rmSimulation manifest) `shouldBe` Just hourlyScheduleDecl
      rmOverlay manifest `shouldSatisfy` maybe False (const True)
      rmDataDirectory manifest `shouldBe` Just "civilization-data"
      length (rmDataResources manifest) `shouldBe` 2
      length (rmExternalDataSources manifest) `shouldBe` 1
      rmCapabilities manifest `shouldSatisfy` elem CapLog
      rmCapabilities manifest `shouldSatisfy` elem CapReadTerrain
      rmCapabilities manifest `shouldSatisfy` elem CapReadOverlay
      rmCapabilities manifest `shouldSatisfy` elem CapWriteOverlay
      rmCapabilities manifest `shouldSatisfy` elem CapDataRead
      rmCapabilities manifest `shouldSatisfy` notElem CapWriteTerrain
      rmCapabilities manifest `shouldSatisfy` notElem CapDataWrite
      case rmInvocationScopes manifest >>= riscGenerator of
        Just scope -> rsoOwnedOverlay (risdOutput scope) `shouldBe` True
        Nothing -> expectationFailure "expected scoped generator declaration"
      validateManifest manifest `shouldBe` []

    it "preserves backend-neutral external data-source declarations" $ do
      let manifest = generateManifest CivExample.civPlugin
      case rmExternalDataSources manifest of
        [source] -> do
          redsdName source `shouldBe` "settlement-ledger"
          redsdLabel source `shouldBe` "Sample Civilization Catalogue"
          redsdDescription source `shouldBe`
            "Provider-owned immutable demonstration records, not generated world state"
          redsdKind source `shouldBe` "catalog"
          redsdCapabilities source `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
          redsdResources source `shouldBe` ["settlements", "cultures"]
          redssProviderId (redsdStatus source) `shouldBe` Just "civilization"
          redssAvailability (redsdStatus source) `shouldBe` Just ExternalAvailabilityAvailable
          redssHealth (redsdStatus source) `shouldBe` Just ExternalHealthHealthy
          redssAccessMode (redsdStatus source) `shouldBe` Just ExternalAccessModeReadOnly
          redssCapabilityScope (redsdStatus source) `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
          map redsgName (redsdGrants source) `shouldBe` ["settlement-read"]
        _ -> expectationFailure "expected exactly one external data source"

    it "round-trips through manifest JSON" $ do
      let encoded = Aeson.encode (generateManifest CivExample.civPlugin)
      case Aeson.eitherDecode encoded of
        Left err -> expectationFailure err
        Right (decoded :: RPCManifest) -> do
          rmName decoded `shouldBe` "civilization"
          validateManifest decoded `shouldBe` []

    it "packages an install directory with the executable, generated manifest.json, and civilization.toposchema" $
      withTempDir "topo-plugin-civ-example-install" $ \pluginDir -> do
        let executablePath = pluginDir </> "civilization"
        writeFile executablePath "installed executable placeholder"
        copyFile "civilization.toposchema" (pluginDir </> "civilization.toposchema")
        withArgs ["--topo-write-manifest", pluginDir] CivExample.main
        let manifestPath = pluginDir </> pluginManifestFileName
            schemaPath = pluginDir </> "civilization.toposchema"
        doesFileExist executablePath `shouldReturn` True
        doesFileExist manifestPath `shouldReturn` True
        doesFileExist schemaPath `shouldReturn` True
        readGeneratedManifest manifestPath $ \manifest -> do
          rmName manifest `shouldBe` "civilization"
          doesFileExist (pluginDir </> Text.unpack (rmName manifest)) `shouldReturn` True
          (Text.unpack . rodSchemaFile <$> rmOverlay manifest) `shouldBe` Just "civilization.toposchema"
          validateManifest manifest `shouldBe` []

  describe "civilization generator" $ do
    it "seeds a nonempty deterministic overlay on habitable terrain" $ do
      schema <- loadTestSchema
      world <- pure (habitableWorld [0] 8)
      first <- runLegacyGenerator schema world 73 0.3
      second <- runLegacyGenerator schema world 73 0.3
      gtrOverlay first `shouldBe` gtrOverlay second
      overlay <- decodeGeneratedOverlay schema first
      overlayTileCount overlay `shouldSatisfy` (> 0)
      allOverlayFloatsBounded overlay `shouldBe` True

    it "responds monotonically to the habitability threshold" $ do
      schema <- loadTestSchema
      let world = habitableWorld [0] 8
      low <- runLegacyGenerator schema world 91 0.3 >>= decodeGeneratedOverlay schema
      high <- runLegacyGenerator schema world 91 0.95 >>= decodeGeneratedOverlay schema
      overlayTileCount low `shouldSatisfy` (> 0)
      overlayTileCount high `shouldBe` 0

    it "limits scoped terrain and overlay output to resolved chunks" $ do
      schema <- loadTestSchema
      let world = habitableWorld [0, 1] 4
          context = scopedGeneratorContext world
      result <- case pdGeneratorScope CivExample.civPlugin of
        Nothing -> expectationFailure "missing scoped generator" >> fail "scoped generator"
        Just definition -> gsdRun definition context >>= either
          (\err -> expectationFailure (Text.unpack err) >> fail "scoped generator") pure
      decodedTerrain <- either
        (\err -> expectationFailure (Text.unpack err) >> fail "scoped terrain") pure
        (decodeTerrainPayload (gtrTerrain result))
      IntMap.keys (twTerrain decodedTerrain) `shouldBe` [0]
      IntMap.keys (twClimate decodedTerrain) `shouldBe` [0]
      IntMap.keys (twVegetation decodedTerrain) `shouldBe` [0]
      overlay <- decodeGeneratedOverlay schema result
      overlayChunkIds overlay `shouldBe` [1]

  describe "civilization simulation" $ do
    it "evolves bounded food and trade differently when trade is enabled" $ do
      schema <- loadTestSchema
      generated <- runLegacyGenerator schema (habitableWorld [0] 8) 17 0.3
      seeded <- decodeGeneratedOverlay schema generated
      tradeOn <- runLegacySimulation schema seeded True
      tradeOff <- runLegacySimulation schema seeded False
      let seededTiles =
            [ (chunkId, tileId)
            | (chunkId, OverlayChunk tiles) <- sparseChunks tradeOn
            , tileId <- IntMap.keys tiles
            ]
      (chunkId, tileId) <- case seededTiles of
        [] -> expectationFailure "simulation overlay was empty" >> fail "settlement"
        firstTile : _ -> pure firstTile
      lookupFieldAt tradeOn chunkId tileId "trade_value"
        `shouldNotBe` lookupFieldAt tradeOff chunkId tileId "trade_value"
      lookupFieldAt tradeOn chunkId tileId "food_supply"
        `shouldNotBe` lookupFieldAt tradeOff chunkId tileId "food_supply"
      allOverlayFloatsBounded tradeOn `shouldBe` True
      allOverlayFloatsBounded tradeOff `shouldBe` True

    it "honors trade and owned-overlay limits in the scoped callback" $ do
      schema <- loadTestSchema
      generated <- runLegacyGenerator schema (habitableWorld [0, 1] 4) 29 0.3
      seeded <- decodeGeneratedOverlay schema generated
      tradeOn <- runScopedSimulation schema seeded True
      tradeOff <- runScopedSimulation schema seeded False
      overlayChunkIds tradeOn `shouldBe` [1]
      overlayChunkIds tradeOff `shouldBe` [1]
      let tileIds =
            [ tileId
            | (_, OverlayChunk tiles) <- sparseChunks tradeOn
            , tileId <- IntMap.keys tiles
            ]
      tileId <- case tileIds of
        [] -> expectationFailure "scoped simulation overlay was empty" >> fail "settlement"
        firstTile : _ -> pure firstTile
      lookupFieldAt tradeOn 1 tileId "trade_value"
        `shouldNotBe` lookupFieldAt tradeOff 1 tileId "trade_value"
      allOverlayFloatsBounded tradeOn `shouldBe` True

    it "caps extreme population growth" $ do
      schema <- loadTestSchema
      generated <- runLegacyGenerator schema (habitableWorld [0] 4) 31 0.3
      seeded <- decodeGeneratedOverlay schema generated
      bounded <- runLegacySimulation schema (setAllPopulation schema 1.0e9 seeded) True
      allPopulationsBounded bounded `shouldBe` True

loadTestSchema :: IO OverlaySchema
loadTestSchema = do
  bytes <- BS.readFile "civilization.toposchema"
  case parseOverlaySchema bytes of
    Left err -> expectationFailure (Text.unpack err) >> fail "civilization schema"
    Right schema -> pure schema

habitableWorld :: [Int] -> Int -> TerrainWorld
habitableWorld chunkIds chunkSize = foldl addChunk base chunkIds
  where
    config = WorldConfig { wcChunkSize = chunkSize }
    tileCount = chunkSize * chunkSize
    terrain = (emptyTerrainChunk config)
      { tcElevation = U.replicate tileCount 0.7 }
    climate = (emptyClimateChunk config)
      { ccTempAvg = U.replicate tileCount 0.55
      , ccPrecipAvg = U.replicate tileCount 0.8
      }
    vegetation = (emptyVegetationChunk config)
      { vegCover = U.replicate tileCount 0.8
      , vegDensity = U.replicate tileCount 0.8
      }
    base = emptyWorld config defaultHexGridMeta
    addChunk world chunkId =
      setVegetationChunk (ChunkId chunkId) vegetation
        . setClimateChunk (ChunkId chunkId) climate
        . setTerrainChunk (ChunkId chunkId) terrain
        $ world

runLegacyGenerator
  :: OverlaySchema -> TerrainWorld -> Word -> Double -> IO GeneratorTickResult
runLegacyGenerator _schema world seed threshold = do
  terrainPayload <- either
    (\err -> expectationFailure (Text.unpack err) >> fail "terrain payload") pure
    (encodeTerrainPayload world)
  let context = PluginContext
        { pcWorld = world
        , pcParams = Map.singleton "habitability_threshold" (Number (realToFrac threshold))
        , pcTerrain = terrainPayload
        , pcOwnOverlay = Nothing
        , pcOverlays = Map.empty
        , pcSeed = fromIntegral seed
        , pcLog = \_ -> pure ()
        , pcProgress = \_ _ -> pure ()
        , pcWorldPath = Nothing
        }
  case pdGenerator CivExample.civPlugin of
    Nothing -> expectationFailure "missing legacy generator" >> fail "legacy generator"
    Just definition -> gdRun definition context >>= either
      (\err -> expectationFailure (Text.unpack err) >> fail "legacy generator") pure

decodeGeneratedOverlay :: OverlaySchema -> GeneratorTickResult -> IO Overlay
decodeGeneratedOverlay schema result = case gtrOverlay result of
  Nothing -> expectationFailure "generator omitted civilization overlay" >> fail "overlay"
  Just payload -> either
    (\err -> expectationFailure (Text.unpack err) >> fail "overlay decode") pure
    (overlayFromJSON schema payload)

scopedGeneratorContext :: TerrainWorld -> GeneratorContext
scopedGeneratorContext world = GeneratorContext
  { gcParams = Map.singleton "habitability_threshold" (Number 0.3)
  , gcTerrain = Just world
  , gcTerrainPayload = Nothing
  , gcSeed = 123
  , gcScope = ResolvedInvocationScope
      { risScopeId = "civ-generator-test"
      , risKind = InvocationGenerator
      , risTerrainInputSections = allTerrainSections
      , risTerrainInputChunkIds = IntSet.fromList [0, 1]
      , risDependencyOverlayChunkIds = Map.empty
      , risOwnOverlayReadChunkIds = IntSet.empty
      , risTerrainOutputSections = allTerrainSections
      , risTerrainOutputChunkIds = IntSet.singleton 0
      , risOwnedOverlayIdentity = Just "civilization"
      , risOwnOverlayWriteChunkIds = IntSet.singleton 1
      , risGeneratorMetadataOutput = False
      , risDataResource = Nothing
      , risBudgets = RPCScopeBudgets 10000000 10000000 10000000
      }
  , gcLog = \_ -> pure ()
  , gcProgress = \_ _ -> pure ()
  , gcWorldPath = Nothing
  }
  where
    allTerrainSections = Set.fromList
      [TerrainElevation, TerrainClimate, TerrainVegetation]

runLegacySimulation :: OverlaySchema -> Overlay -> Bool -> IO Overlay
runLegacySimulation schema overlay tradeEnabled = do
  let context = PluginContext
        { pcWorld = emptyWorld (WorldConfig 1) defaultHexGridMeta
        , pcParams = Map.fromList
            [ ("growth_rate", Number 0.02)
            , ("infra_cost", Number 0.1)
            , ("city_threshold", Number 1000)
            , ("enable_trade", Bool tradeEnabled)
            ]
        , pcTerrain = Aeson.object ["chunk_size" Aeson..= (1 :: Int)]
        , pcOwnOverlay = Just (encodeOverlayPayload overlay)
        , pcOverlays = Map.empty
        , pcSeed = 0
        , pcLog = \_ -> pure ()
        , pcProgress = \_ _ -> pure ()
        , pcWorldPath = Nothing
        }
  result <- case pdSimulation CivExample.civPlugin of
    Nothing -> expectationFailure "missing legacy simulation" >> fail "legacy simulation"
    Just definition -> sdTick definition context >>= either
      (\err -> expectationFailure (Text.unpack err) >> fail "legacy simulation") pure
  strTerrainWrites result `shouldBe` Nothing
  either
    (\err -> expectationFailure (Text.unpack err) >> fail "simulation overlay") pure
    (overlayFromJSON schema (strOverlay result))

runScopedSimulation :: OverlaySchema -> Overlay -> Bool -> IO Overlay
runScopedSimulation schema overlay tradeEnabled = do
  let context = SimulationContext
        { scParams = Map.fromList
            [ ("growth_rate", Number 0.02)
            , ("infra_cost", Number 0.1)
            , ("city_threshold", Number 1000)
            , ("enable_trade", Bool tradeEnabled)
            ]
        , scTerrain = Nothing
        , scTerrainPayload = Nothing
        , scOwnOverlay = Just (encodeOverlayPayload overlay)
        , scOverlays = Map.empty
        , scWorldTime = 0
        , scDeltaTicks = 1
        , scCalendar = Null
        , scScope = ResolvedInvocationScope
            { risScopeId = "civ-simulation-test"
            , risKind = InvocationSimulation
            , risTerrainInputSections = Set.empty
            , risTerrainInputChunkIds = IntSet.empty
            , risDependencyOverlayChunkIds = Map.singleton "weather" IntSet.empty
            , risOwnOverlayReadChunkIds = IntSet.fromList [0, 1]
            , risTerrainOutputSections = Set.empty
            , risTerrainOutputChunkIds = IntSet.empty
            , risOwnedOverlayIdentity = Just "civilization"
            , risOwnOverlayWriteChunkIds = IntSet.singleton 1
            , risGeneratorMetadataOutput = False
            , risDataResource = Nothing
            , risBudgets = RPCScopeBudgets 10000000 10000000 10000000
            }
        , scLog = \_ -> pure ()
        , scProgress = \_ _ -> pure ()
        , scWorldPath = Nothing
        }
  result <- case pdSimulationScope CivExample.civPlugin of
    Nothing -> expectationFailure "missing scoped simulation" >> fail "scoped simulation"
    Just definition -> ssdTick definition context >>= either
      (\err -> expectationFailure (Text.unpack err) >> fail "scoped simulation") pure
  strTerrainWrites result `shouldBe` Nothing
  either
    (\err -> expectationFailure (Text.unpack err) >> fail "scoped simulation overlay") pure
    (overlayFromJSON schema (strOverlay result))

setAllPopulation :: OverlaySchema -> Float -> Overlay -> Overlay
setAllPopulation schema population overlay = case (fieldIndex schema "population", ovData overlay) of
  (Just idx, SparseData chunks) ->
    let updateChunk (OverlayChunk tiles) = OverlayChunk
          (IntMap.map (setRecordField idx (OVFloat population)) tiles)
    in overlay { ovData = SparseData (IntMap.map updateChunk chunks) }
  _ -> overlay

allPopulationsBounded :: Overlay -> Bool
allPopulationsBounded overlay = and
  [ case lookupFieldAt overlay chunkId tileId "population" of
      Just (OVFloat population) -> population >= 0 && population <= 1.0e9
      _ -> False
  | (chunkId, OverlayChunk tiles) <- sparseChunks overlay
  , tileId <- IntMap.keys tiles
  ]

sparseChunks :: Overlay -> [(Int, OverlayChunk)]
sparseChunks overlay = case ovData overlay of
  SparseData chunks -> IntMap.toList chunks
  DenseData _ -> []

overlayChunkIds :: Overlay -> [Int]
overlayChunkIds = map fst . sparseChunks

overlayTileCount :: Overlay -> Int
overlayTileCount = sum . map (IntMap.size . chunkTiles . snd) . sparseChunks
  where chunkTiles (OverlayChunk tiles) = tiles

allOverlayFloatsBounded :: Overlay -> Bool
allOverlayFloatsBounded overlay = and
  [ bounded field
  | (chunkId, OverlayChunk tiles) <- sparseChunks overlay
  , tileId <- IntMap.keys tiles
  , fieldName <- ["infrastructure", "food_supply", "trade_value"]
  , let field = lookupFieldAt overlay chunkId tileId fieldName
  ]
  where
    bounded (Just (OVFloat value)) = value >= 0 && value <= 1
    bounded _ = False

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = bracket setup cleanup action
  where
    setup = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp label
      hClose handle
      removeFile path
      createDirectory path
      pure path
    cleanup dir = removePathForcibly dir

readGeneratedManifest :: FilePath -> (RPCManifest -> Expectation) -> Expectation
readGeneratedManifest path check = do
  result <- parseManifestFile path
  case result of
    Left err -> expectationFailure ("manifest parse failed: " <> show err)
    Right manifest -> check manifest
