{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.WorldPersist (spec) where

import Control.Exception (bracket, throwIO, try, IOException)
import Control.Monad (forM_, when)
import Data.Aeson (Value(..), encode, eitherDecodeStrict', object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory
  ( createDirectoryIfMissing
  , createDirectoryLink
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removePathForcibly
  , renameDirectory
  )
import System.FilePath ((</>))
import System.Info (os)
import Test.Hspec
import Spec.Support.OverlayFixtures (mkSparseFloatOverlay)

import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.UI
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  , emptyUiState
  )
import Seer.Config (configFromUi, unmapRange)
import Seer.Config.Snapshot (snapshotFromUi)
import Seer.Config.Snapshot.Types (ConfigSnapshot(..), defaultSnapshot)
import Seer.World.Persist
  ( WorldPluginDataDirectory(..)
  , WorldExternalDataSourceSnapshot(..)
  , WorldSaveHooks(..)
  , defaultWorldSaveHooks
  , WorldWeatherLayerManifest(..)
  , WorldSaveManifest(..)
  , saveNamedWorld
  , saveNamedWorldWithPluginsAndExternalData
  , saveNamedWorldWithPluginsAndExternalDataAndHooks
  , loadNamedWorld
  , listWorlds
  , deleteNamedWorld
  , snapshotToWorld
  , worldDir
  )
import Seer.World.Persist.Types (defaultManifestTime)
import Topo.Calendar (WorldTime(..), simulationTickSeconds)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , defaultRPCExternalDataSourceStatus
  , defaultRPCUIHints
  )
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Storage (emptyProvenance, saveWorldWithProvenance)
import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayProvenance(..)
  , OverlayStore(..)
  , emptyOverlayProvenance
  , emptyOverlayStore
  , insertOverlay
  , lookupOverlay
  , ovData
  )
import Topo.Simulation.Schedule (SimulationCatchUpPolicy(..), SimulationScheduleState(..))
import Topo.Types (ChunkId(..), ClimateChunk(..), WeatherChunk(..), WorldConfig(..), chunkTileCount)
import Topo.Weather
  ( defaultWeatherConfig
  , getWeatherChunk
  , getWeatherNormalsChunkFromStore
  , weatherNormalsChunkFromClimate
  , weatherNormalsOverlayFromClimate
  )
import Topo.World (TerrainWorld(..), emptyTerrainChunk, emptyWorld)
import Topo.WorldGen (WorldGenConfig(..))

-- ---------------------------------------------------------------------------
-- Spec entry point
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "WorldPersist" $ do
  manifestJsonSpec
  worldRoundTripSpec
  deletionSpec
  snapshotToWorldSpec
  listWorldsSpec

-- ---------------------------------------------------------------------------
-- WorldSaveManifest JSON round-trip
-- ---------------------------------------------------------------------------

manifestJsonSpec :: Spec
manifestJsonSpec = describe "WorldSaveManifest JSON round-trip" $ do
  it "round-trips a manifest" $ do
    now <- getCurrentTime
    let manifest = WorldSaveManifest
          { wsmName       = "test-world"
          , wsmSeed       = 9876
          , wsmChunkSize  = 64
          , wsmCreatedAt  = now
          , wsmChunkCount = 16
          , wsmOverlayNames = ["weather", "persist_sparse_test"]
          , wsmWeatherLayers =
              [ WorldWeatherLayerManifest "weather" "instantaneous_current" "weather_snapshot" "overlay_sidecar"
              ]
          , wsmPluginData = []
          , wsmExternalDataSources = [testExternalDataSourceSnapshot]
          }
    let bytes = BSL.toStrict (encode manifest)
    eitherDecodeStrict' bytes `shouldBe` Right manifest

  it "parses empty JSON object with defaults" $ do
    case eitherDecodeStrict' "{}" of
      Left err -> expectationFailure err
      Right m  -> do
        wsmName m `shouldBe` ""
        wsmSeed m `shouldBe` 0
        wsmChunkSize m `shouldBe` 64
        wsmCreatedAt m `shouldBe` defaultManifestTime
        wsmChunkCount m `shouldBe` 0
        wsmOverlayNames m `shouldBe` []
        wsmWeatherLayers m `shouldBe` []
        wsmExternalDataSources m `shouldBe` []

  it "normalizes legacy weather layer source kinds on read" $ do
    let legacy = object
          [ "weather_layers" .=
              [ object
                  [ "name" .= ("climate" :: Text.Text)
                  , "basis" .= ("long_run_average" :: Text.Text)
                  , "source_kind" .= ("generated_climate" :: Text.Text)
                  , "storage" .= ("core_topo" :: Text.Text)
                  ]
              , object
                  [ "name" .= ("weather" :: Text.Text)
                  , "basis" .= ("instantaneous_current" :: Text.Text)
                  , "source_kind" .= ("simulated_generated_weather" :: Text.Text)
                  , "storage" .= ("overlay_sidecar" :: Text.Text)
                  ]
              , object
                  [ "name" .= ("weather_normals" :: Text.Text)
                  , "basis" .= ("typical_normal" :: Text.Text)
                  , "source_kind" .= ("generated_climate" :: Text.Text)
                  , "storage" .= ("overlay_sidecar" :: Text.Text)
                  ]
              ]
          ]
    case eitherDecodeStrict' @WorldSaveManifest (BSL.toStrict (encode legacy)) of
      Left err -> expectationFailure err
      Right m -> wsmWeatherLayers m `shouldBe`
        [ WorldWeatherLayerManifest "climate" "long_run_average" "climate_average" "core_topo"
        , WorldWeatherLayerManifest "weather" "instantaneous_current" "weather_snapshot" "overlay_sidecar"
        , WorldWeatherLayerManifest "weather_normals" "typical_normal" "weather_normals" "overlay_sidecar"
        ]

  it "keeps layered view state out of saved config snapshots" $ do
    let ui = emptyUiState
          { uiSeed = 707
          , uiChunkSize = 64
          , uiViewMode = ViewCloudTypical
          , uiViewSelection = defaultLayeredViewState
              { lvsBaseView = BaseViewBiome
              , lvsSkyOverlay = Just SkyOverlayCloud
              , lvsWeatherBasis = WeatherBasisAverage
              , lvsOverlayOpacity = 0.25
              }
          }
        bytes = BSL.toStrict (encode (snapshotFromUi ui "layered-ui-only"))
    case eitherDecodeStrict' @Value bytes of
      Left err -> expectationFailure err
      Right (Object obj) -> do
        let hasKey key = KM.member (Key.fromText key) obj
        map hasKey (["view", "view_mode", "viewMode", "viewSelection", "overlay_opacity", "overlayOpacity", "weather_basis"] :: [Text.Text])
          `shouldBe` replicate 7 False
      Right _ -> expectationFailure "expected snapshot JSON object"
    case eitherDecodeStrict' @ConfigSnapshot bytes of
      Left err -> expectationFailure err
      Right snapshot -> do
        csName snapshot `shouldBe` "layered-ui-only"
        csSeed snapshot `shouldBe` 707
        csChunkSize snapshot `shouldBe` 64

  it "ignores legacy layered view keys when reading config snapshots" $ do
    let legacy = object
          [ "name" .= ("legacy-layered" :: Text.Text)
          , "seed" .= (808 :: Int)
          , "chunkSize" .= (32 :: Int)
          , "view_mode" .= ("cloud_typical" :: Text.Text)
          , "view" .= object
              [ "base_mode" .= ("biome" :: Text.Text)
              , "overlay_mode" .= ("cloud" :: Text.Text)
              , "weather_basis" .= ("average" :: Text.Text)
              , "overlay_opacity" .= (0.25 :: Double)
              ]
          ]
    case eitherDecodeStrict' @ConfigSnapshot (BSL.toStrict (encode legacy)) of
      Left err -> expectationFailure err
      Right snapshot -> do
        csName snapshot `shouldBe` "legacy-layered"
        csSeed snapshot `shouldBe` 808
        csChunkSize snapshot `shouldBe` 32
        csRenderWaterLevel snapshot `shouldBe` csRenderWaterLevel defaultSnapshot

-- ---------------------------------------------------------------------------
-- saveNamedWorld / loadNamedWorld round-trip
-- ---------------------------------------------------------------------------

-- | Unique test world name to avoid collisions with user data.
testWorldName :: Text.Text
testWorldName = "__topo_test_world_roundtrip__"

createPluginDataSource :: IO FilePath
createPluginDataSource = do
  dir <- worldDir
  let source = dir </> "__topo_test_plugin_data_source__"
  cleanupPath source
  createDirectoryIfMissing True source
  writeFile (source </> "state.txt") "plugin-state"
  pure source

cleanupPath :: FilePath -> IO ()
cleanupPath path = do
  _ <- try @IOException (removePathForcibly path)
  pure ()

emptyTerrainSnapshot :: Int -> TerrainSnapshot
emptyTerrainSnapshot chunkSize = TerrainSnapshot
  { tsVersion = 0
  , tsClimateVersion = 0
  , tsWeatherVersion = 0
  , tsVegetationVersion = 0
  , tsOverlayVersion = 0
  , tsChunkSize = chunkSize
  , tsTerrainChunks = IntMap.empty
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
  , tsRiverChunks = IntMap.empty
  , tsGroundwaterChunks = IntMap.empty
  , tsVolcanismChunks = IntMap.empty
  , tsGlacierChunks = IntMap.empty
  , tsWaterBodyChunks = IntMap.empty
  , tsVegetationChunks = IntMap.empty
  , tsOverlayStore = emptyOverlayStore
  , tsGeoContext = defaultTerrainGeoContext
  }

scheduleFixture :: SimulationScheduleState
scheduleFixture = SimulationScheduleState
  { schedIntervalTicks = 6
  , schedPhaseTicks = 2
  , schedLastFireTick = Just 14
  , schedNextFireTick = 20
  , schedCatchUpPolicy = SkipMissed
  }

testExternalDataSourceSnapshot :: WorldExternalDataSourceSnapshot
testExternalDataSourceSnapshot = WorldExternalDataSourceSnapshot
  { wedssPlugin = "settlement-provider"
  , wedssProvidedSources = [testExternalDataSourceDecl]
  , wedssConsumedRefs = [testExternalDataSourceRef]
  }

testExternalDataSourceDecl :: RPCExternalDataSourceDecl
testExternalDataSourceDecl = RPCExternalDataSourceDecl
  { redsdName = "settlement-ledger"
  , redsdLabel = "Settlement Ledger"
  , redsdDescription = "Provider-owned settlements"
  , redsdKind = "catalog"
  , redsdCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
  , redsdResources = ["settlements"]
  , redsdStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusReady
      , redssProviderId = Just "settlement-provider"
      , redssAvailability = Just ExternalAvailabilityAvailable
      , redssHealth = Just ExternalHealthHealthy
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
      }
  , redsdConnection = Just (object ["handle" .= ("provider-owned:settlement-ledger" :: Text.Text)])
  , redsdConfigRefs =
      [ RPCExternalDataSourceConfigRef
          { redscrName = "settlement-ledger-binding"
          , redscrOrigin = ExternalConfigProvider
          , redscrKey = "settlement-provider.settlement-ledger"
          , redscrRequired = True
          , redscrCompatibility = Just "manifest-v3"
          , redscrMetadata = Just (object ["handle" .= ("provider-owned:settlement-ledger" :: Text.Text)])
          }
      ]
  , redsdGrants =
      [ RPCExternalDataSourceGrant
          { redsgName = "settlement-read"
          , redsgAccess = [ExternalAccessRead]
          , redsgCapabilities = [ExternalSourceQuery]
          , redsgResources = ["settlements"]
          , redsgStatus = defaultRPCExternalDataSourceStatus
              { redssState = ExternalStatusReady
              , redssProviderId = Just "settlement-provider"
              , redssAvailability = Just ExternalAvailabilityAvailable
              , redssHealth = Just ExternalHealthHealthy
              , redssAccessMode = Just ExternalAccessModeReadOnly
              , redssCapabilityScope = [ExternalSourceQuery]
              }
          , redsgReference = Just (object ["grant" .= ("settlement-read" :: Text.Text)])
          , redsgConfigRefs =
              [ RPCExternalDataSourceConfigRef
                  { redscrName = "settlement-read-binding"
                  , redscrOrigin = ExternalConfigProvider
                  , redscrKey = "settlement-provider.settlement-read"
                  , redscrRequired = True
                  , redscrCompatibility = Just "manifest-v3"
                  , redscrMetadata = Just (object ["grant" .= ("settlement-read" :: Text.Text)])
                  }
              ]
          }
      ]
  , redsdUiHints = defaultRPCUIHints
  }

testExternalDataSourceRef :: RPCExternalDataSourceRef
testExternalDataSourceRef = RPCExternalDataSourceRef
  { redsrName = "settlements"
  , redsrProvider = Just "settlement-provider"
  , redsrSource = "settlement-ledger"
  , redsrRequired = True
  , redsrAccess = [ExternalAccessRead]
  , redsrResources = ["settlements"]
  , redsrGrant = Just "settlement-read"
  , redsrStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusUnknown
      , redssProviderId = Just "settlement-provider"
      , redssAvailability = Just ExternalAvailabilityUnknown
      , redssHealth = Just ExternalHealthUnknown
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = [ExternalSourceQuery]
      }
  , redsrReference = Just (object ["binding" .= ("consumer:settlements" :: Text.Text)])
  , redsrConfigRefs =
      [ RPCExternalDataSourceConfigRef
          { redscrName = "consumer-settlements-binding"
          , redscrOrigin = ExternalConfigDeployment
          , redscrKey = "consumer.settlements"
          , redscrRequired = True
          , redscrCompatibility = Just "manifest-v3"
          , redscrMetadata = Just (object ["binding" .= ("consumer:settlements" :: Text.Text)])
          }
      ]
  , redsrUiHints = defaultRPCUIHints
  }

worldRoundTripSpec :: Spec
worldRoundTripSpec = describe "saveNamedWorld / loadNamedWorld" $
  do
    it "round-trips an empty world preserving config and metadata" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world  = emptyWorld config defaultHexGridMeta
                ui     = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, snapshot, loadedWorld) -> do
                wsmName manifest `shouldBe` testWorldName
                wsmSeed manifest `shouldBe` 42
                wsmChunkSize manifest `shouldBe` 64
                wsmChunkCount manifest `shouldBe` 0
                wsmOverlayNames manifest `shouldBe` []

                csSeed snapshot `shouldBe` 42
                csChunkSize snapshot `shouldBe` 64
                csName snapshot `shouldBe` testWorldName

                IntMap.size (twTerrain loadedWorld) `shouldBe` 0
                IntMap.size (twClimate loadedWorld) `shouldBe` 0
        )

    it "round-trips sparse overlays through unified world bundle persistence" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                baseWorld = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

                overlay = mkSparseFloatOverlay
                  "persist_sparse_test"
                  "sparse overlay persistence test"
                  0.75
                  emptyOverlayProvenance
                world = baseWorld { twOverlays = insertOverlay overlay emptyOverlayStore }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, loadedWorld) -> do
                case lookupOverlay "persist_sparse_test" (twOverlays loadedWorld) of
                  Nothing -> expectationFailure "sparse overlay missing after load"
                  Just loadedOverlay ->
                    case ovData loadedOverlay of
                      DenseData _ -> expectationFailure "expected sparse overlay"
                      SparseData chunks ->
                        IntMap.member 0 chunks `shouldBe` True
                wsmOverlayNames manifest `shouldBe` ["persist_sparse_test"]
        )

    it "normalizes metadata overlay names from world manifest and discovered overlays" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                baseWorld = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

                overlay = mkSparseFloatOverlay
                  "persist_sparse_test"
                  "sparse overlay persistence test"
                  0.75
                  emptyOverlayProvenance
                world = (baseWorld { twOverlays = insertOverlay overlay emptyOverlayStore })
                  { twOverlayManifest = ["ghost", "persist_sparse_test"] }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, _loadedWorld) -> do
                wsmOverlayNames manifest `shouldBe` ["persist_sparse_test"]
        )

    it "round-trips canonical world time and overlay schedule provenance" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let overlay = mkSparseFloatOverlay
                  "persist_sparse_test"
                  "scheduled overlay persistence test"
                  0.75
                  emptyOverlayProvenance { opSchedule = Just scheduleFixture }
                terrainSnap = (emptyTerrainSnapshot 64)
                  { tsOverlayStore = insertOverlay overlay emptyOverlayStore
                  , tsGeoContext = defaultTerrainGeoContext { tgcWorldTime = WorldTime 11 simulationTickSeconds }
                  }
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64, uiSimTickCount = 99 }
                world = snapshotToWorld ui terrainSnap

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (_manifest, _snapshot, loadedWorld) -> do
                wtTick (twWorldTime loadedWorld) `shouldBe` 11
                wtTickRate (twWorldTime loadedWorld) `shouldBe` simulationTickSeconds
                case lookupOverlay "persist_sparse_test" (twOverlays loadedWorld) of
                  Nothing -> expectationFailure "scheduled overlay missing after load"
                  Just loadedOverlay ->
                    opSchedule (ovProvenance loadedOverlay) `shouldBe` Just scheduleFixture
        )

    it "round-trips current weather and generated typical normals with manifest semantics" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let cfg = WorldConfig { wcChunkSize = 64 }
                n = chunkTileCount cfg
                chunkId@(ChunkId chunkKey) = ChunkId 0
                climate = ClimateChunk
                  { ccTempAvg = U.replicate n 0.55
                  , ccPrecipAvg = U.replicate n 0.35
                  , ccWindDirAvg = U.replicate n 0.10
                  , ccWindSpdAvg = U.replicate n 0.20
                  , ccHumidityAvg = U.replicate n 0.65
                  , ccTempRange = U.replicate n 0.15
                  , ccPrecipSeasonality = U.replicate n 0.25
                  }
                currentWeather = WeatherChunk
                  { wcTemp = U.replicate n 0.70
                  , wcHumidity = U.replicate n 0.60
                  , wcWindDir = U.replicate n 0.30
                  , wcWindSpd = U.replicate n 0.40
                  , wcPressure = U.replicate n 0.50
                  , wcPrecip = U.replicate n 0.45
                  , wcCloudCover = U.replicate n 0.80
                  , wcCloudWater = U.replicate n 0.20
                  , wcCloudCoverLow = U.replicate n 0.50
                  , wcCloudCoverMid = U.replicate n 0.25
                  , wcCloudCoverHigh = U.replicate n 0.10
                  , wcCloudWaterLow = U.replicate n 0.12
                  , wcCloudWaterMid = U.replicate n 0.05
                  , wcCloudWaterHigh = U.replicate n 0.03
                  }
                normals = weatherNormalsChunkFromClimate defaultWeatherConfig climate
                normalsOverlay = weatherNormalsOverlayFromClimate 42 defaultWeatherConfig (IntMap.singleton chunkKey climate)
                terrainSnap = (emptyTerrainSnapshot 64)
                  { tsTerrainChunks = IntMap.singleton chunkKey (emptyTerrainChunk cfg)
                  , tsClimateChunks = IntMap.singleton chunkKey climate
                  , tsWeatherChunks = IntMap.singleton chunkKey currentWeather
                  , tsOverlayStore = insertOverlay normalsOverlay emptyOverlayStore
                  }
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                world = snapshotToWorld ui terrainSnap

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, loadedWorld) -> do
                wsmOverlayNames manifest `shouldSatisfy` (\names -> all (`elem` names) ["weather", "weather_normals"])
                wsmWeatherLayers manifest `shouldSatisfy` elem
                  (WorldWeatherLayerManifest "climate" "long_run_average" "climate_average" "core_topo")
                wsmWeatherLayers manifest `shouldSatisfy` elem
                  (WorldWeatherLayerManifest "weather" "instantaneous_current" "weather_snapshot" "overlay_sidecar")
                wsmWeatherLayers manifest `shouldSatisfy` elem
                  (WorldWeatherLayerManifest "weather_normals" "typical_normal" "weather_normals" "overlay_sidecar")
                getWeatherChunk chunkId loadedWorld `shouldBe` Just currentWeather
                getWeatherNormalsChunkFromStore chunkId (twOverlays loadedWorld) `shouldBe` Just normals
        )

    it "preserves external data-source reference metadata in the save manifest" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

            saveResult <- saveNamedWorldWithPluginsAndExternalData
              testWorldName ui world [] [testExternalDataSourceSnapshot]
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, _loadedWorld) -> do
                wsmExternalDataSources manifest `shouldBe` [testExternalDataSourceSnapshot]
        )

    it "bundles plugin data from host source roots into validated archive directories" $
      bracket
        createPluginDataSource
        (\source -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source
        )
        (\source -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = "safe\\archive"
                  }

            saveResult <- saveNamedWorldWithPluginsAndExternalData
              testWorldName ui world [pluginData] []
            saveResult `shouldBe` Right ()

            worlds <- worldDir
            let savedFile = worlds </> Text.unpack testWorldName </> "plugins" </> "safe" </> "archive" </> "state.txt"
            doesFileExist savedFile `shouldReturn` True

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, _loadedWorld) ->
                wsmPluginData manifest `shouldBe` [("plugin-a", "safe/archive")]
        )

    it "pins plugin root traversal against concurrent ancestor replacement" $
      bracket
        (do
            worlds <- worldDir
            let ancestor = worlds </> "__topo_test_plugin_data_ancestor__"
                movedAncestor = ancestor <> ".moved"
                source = ancestor </> "source"
            cleanupPath ancestor
            cleanupPath movedAncestor
            createDirectoryIfMissing True source
            writeFile (source </> "state.txt") "plugin-state"
            pure (ancestor, movedAncestor, source))
        (\(ancestor, movedAncestor, _source) -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath ancestor
            cleanupPath movedAncestor)
        (\(ancestor, movedAncestor, source) -> do
            mutationDone <- newIORef False
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = "archive"
                  }
                hooks = defaultWorldSaveHooks
                  { wshBeforePluginDataEntryOpen = \entryPath ->
                      when (entryPath == source) $ do
                        shouldMutate <- atomicModifyIORef' mutationDone (\done -> (True, not done))
                        when shouldMutate $ do
                          renameDirectory ancestor movedAncestor
                          createDirectoryIfMissing True source
                          writeFile (source </> "state.txt") "replacement-state"
                  }

            saveResult <- saveNamedWorldWithPluginsAndExternalDataAndHooks
              hooks testWorldName ui world [pluginData] []
            saveResult `shouldBe` Right ()

            worlds <- worldDir
            let savedFile = worlds </> Text.unpack testWorldName
                  </> "plugins" </> "archive" </> "state.txt"
            readFile savedFile `shouldReturn` "plugin-state"
        )

    it "pins plugin traversal to opened directory handles during source replacement" $
      bracket
        (do
            source <- createPluginDataSource
            let movedSource = source <> ".moved"
            cleanupPath movedSource
            pure (source, movedSource))
        (\(source, movedSource) -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source
            cleanupPath movedSource)
        (\(source, movedSource) -> do
            mutationDone <- newIORef False
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = "archive"
                  }
                hooks = defaultWorldSaveHooks
                  { wshBeforePluginDataEntryOpen = \entryPath ->
                      when (entryPath == source </> "state.txt") $ do
                        shouldMutate <- atomicModifyIORef' mutationDone (\done -> (True, not done))
                        when shouldMutate $ do
                          renameDirectory source movedSource
                          createDirectoryIfMissing True source
                          writeFile (source </> "state.txt") "replacement-state"
                  }

            saveResult <- saveNamedWorldWithPluginsAndExternalDataAndHooks
              hooks testWorldName ui world [pluginData] []
            saveResult `shouldBe` Right ()

            worlds <- worldDir
            let savedFile = worlds </> Text.unpack testWorldName
                  </> "plugins" </> "archive" </> "state.txt"
            readFile savedFile `shouldReturn` "plugin-state"
        )

    it "preserves the previously committed world when plugin copying fails" $
      bracket
        createPluginDataSource
        (\source -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source)
        (\source -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                originalUi = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                replacementUi = emptyUiState { uiSeed = 99, uiChunkSize = 64 }
                pluginData = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = "archive"
                  }

            initialSave <- saveNamedWorldWithPluginsAndExternalData
              testWorldName originalUi world [pluginData] []
            initialSave `shouldBe` Right ()
            writeFile (source </> "state.txt") "replacement-state"

            let failingHooks = defaultWorldSaveHooks
                  { wshBeforePluginDataEntryOpen = \entryPath ->
                      when (entryPath == source </> "state.txt") $
                        throwIO (userError "simulated plugin copy failure")
                  }
            failedSave <- saveNamedWorldWithPluginsAndExternalDataAndHooks
              failingHooks testWorldName replacementUi world [pluginData] []
            case failedSave of
              Left err -> err `shouldSatisfy` Text.isInfixOf "simulated plugin copy failure"
              Right () -> expectationFailure "plugin copy failure committed a replacement world"

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, snapshot, _loadedWorld) -> do
                wsmSeed manifest `shouldBe` 42
                csSeed snapshot `shouldBe` 42
            worlds <- worldDir
            let worldPath = worlds </> Text.unpack testWorldName
                savedFile = worldPath </> "plugins" </> "archive" </> "state.txt"
            readFile savedFile `shouldReturn` "plugin-state"
            doesDirectoryExist (worldPath <> ".saving") `shouldReturn` False
            listed <- listWorlds
            length (filter ((== testWorldName) . wsmName) listed) `shouldBe` 1
        )

    it "rejects NUL in plugin source paths before native traversal" $
      bracket
        createPluginDataSource
        (\source -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source)
        (\source -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source <> "\NULalias"
                  , wpddArchiveDirectory = "archive"
                  }
            saveResult <- saveNamedWorldWithPluginsAndExternalData
              testWorldName ui world [pluginData] []
            case saveResult of
              Left err -> err `shouldSatisfy` Text.isInfixOf "NUL"
              Right () -> expectationFailure "NUL-containing plugin source was accepted"
        )

    it "rejects unsafe plugin archive directories without escaping the world plugin tree" $
      bracket
        createPluginDataSource
        (\source -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source
        )
        (\source -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData archiveDir = WorldPluginDataDirectory
                  { wpddPlugin = "plugin-a"
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = archiveDir
                  }
                unsafeArchives =
                  [ ""
                  , "/tmp/escape"
                  , "C:\\escape"
                  , "."
                  , ".."
                  , "safe\\nested/../escape"
                  , "safe\NULalias"
                  ]

            mapM_
              (\archiveDir -> do
                saveResult <- saveNamedWorldWithPluginsAndExternalData
                  testWorldName ui world [pluginData archiveDir] []
                case saveResult of
                  Left err -> err `shouldSatisfy` \message ->
                    Text.isInfixOf "non-empty" message || Text.isInfixOf "safe relative path" message
                  Right () -> expectationFailure
                    ("unsafe plugin archive directory was accepted: " <> Text.unpack archiveDir))
              unsafeArchives

            worlds <- worldDir
            let worldPath = worlds </> Text.unpack testWorldName
            doesFileExist (worldPath </> "escape" </> "state.txt") `shouldReturn` False
            doesFileExist (worldPath </> "plugins" </> "escape" </> "state.txt") `shouldReturn` False
        )

    it "rejects exact, normalized, and ancestor plugin archive destination collisions" $
      bracket
        createPluginDataSource
        (\source -> do
            _ <- deleteNamedWorld testWorldName
            cleanupPath source
        )
        (\source -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                pluginData pluginName archiveDir = WorldPluginDataDirectory
                  { wpddPlugin = pluginName
                  , wpddSourceDirectory = source
                  , wpddArchiveDirectory = archiveDir
                  }
                collisionCases =
                  [ ("shared", "shared")
                  , ("shared/path", "shared\\path")
                  , ("shared", "shared/child")
                  , ("Archive", "archive")
                  , ("Shared/left", "shared/right")
                  , ("Straße", "STRASSE")
                  ]

            mapM_
              (\(leftArchive, rightArchive) -> do
                collisionResult <- saveNamedWorldWithPluginsAndExternalData
                  testWorldName
                  ui
                  world
                  [ pluginData "plugin-a" leftArchive
                  , pluginData "plugin-b" rightArchive
                  ]
                  []
                case collisionResult of
                  Left err -> err `shouldSatisfy` Text.isInfixOf "collide"
                  Right () -> expectationFailure
                    ("colliding plugin archive directories were accepted: "
                      <> Text.unpack leftArchive <> " and " <> Text.unpack rightArchive))
              collisionCases
        )

    it "rejects symbolic links inside plugin data sources" $
      if os == "mingw32"
        then pure ()
        else bracket
          (do
              source <- createPluginDataSource
              worlds <- worldDir
              let outsideFile = worlds </> "__topo_test_plugin_data_outside__.txt"
              cleanupPath outsideFile
              writeFile outsideFile "outside-state"
              createFileLink outsideFile (source </> "linked-state.txt")
              pure (source, outsideFile))
          (\(source, outsideFile) -> do
              _ <- deleteNamedWorld testWorldName
              cleanupPath source
              cleanupPath outsideFile)
          (\(source, _outsideFile) -> do
              let config = WorldConfig { wcChunkSize = 64 }
                  world = emptyWorld config defaultHexGridMeta
                  ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                  pluginData = WorldPluginDataDirectory
                    { wpddPlugin = "plugin-a"
                    , wpddSourceDirectory = source
                    , wpddArchiveDirectory = "archive"
                    }
              saveResult <- saveNamedWorldWithPluginsAndExternalData
                testWorldName ui world [pluginData] []
              case saveResult of
                Left err -> err `shouldSatisfy` Text.isInfixOf "symbolic link"
                Right () -> expectationFailure "symbolic-link plugin data entry was bundled"
              worlds <- worldDir
              doesFileExist
                (worlds </> Text.unpack testWorldName </> "plugins" </> "archive" </> "linked-state.txt")
                `shouldReturn` False)

    it "does not follow pre-existing symbolic-link archive destinations while saving" $
      if os == "mingw32"
        then pure ()
        else bracket
          (do
              source <- createPluginDataSource
              worlds <- worldDir
              let worldPath = worlds </> Text.unpack testWorldName
                  outsideDir = worlds </> "__topo_test_plugin_archive_outside__"
              _ <- deleteNamedWorld testWorldName
              cleanupPath outsideDir
              createDirectoryIfMissing True (worldPath </> "plugins")
              createDirectoryIfMissing True outsideDir
              createDirectoryLink outsideDir (worldPath </> "plugins" </> "archive")
              pure (source, outsideDir))
          (\(source, outsideDir) -> do
              _ <- deleteNamedWorld testWorldName
              cleanupPath source
              cleanupPath outsideDir)
          (\(source, outsideDir) -> do
              let config = WorldConfig { wcChunkSize = 64 }
                  world = emptyWorld config defaultHexGridMeta
                  ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }
                  pluginData = WorldPluginDataDirectory
                    { wpddPlugin = "plugin-a"
                    , wpddSourceDirectory = source
                    , wpddArchiveDirectory = "archive"
                    }
              saveResult <- saveNamedWorldWithPluginsAndExternalData
                testWorldName ui world [pluginData] []
              saveResult `shouldBe` Right ()
              doesFileExist (outsideDir </> "state.txt") `shouldReturn` False
              worlds <- worldDir
              doesFileExist
                (worlds </> Text.unpack testWorldName </> "plugins" </> "archive" </> "state.txt")
                `shouldReturn` True)

    it "loads old-format world directories without sidecar when manifest is empty" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 101, uiChunkSize = 64 }

            dir <- worldDir
            let worldPath = dir </> Text.unpack testWorldName
                topoPath = worldPath </> "world.topo"
                metaPath = worldPath </> "meta.json"
                configPath = worldPath </> "config.json"
                manifest = WorldSaveManifest
                  { wsmName = testWorldName
                  , wsmSeed = 101
                  , wsmChunkSize = 64
                  , wsmCreatedAt = defaultManifestTime
                  , wsmChunkCount = 0
                  , wsmOverlayNames = []
                  , wsmWeatherLayers = []
                  , wsmPluginData = []
                  , wsmExternalDataSources = []
                  }

            createDirectoryIfMissing True worldPath
            topoSave <- saveWorldWithProvenance topoPath emptyProvenance world
            topoSave `shouldBe` Right ()
            BSL.writeFile metaPath (encode manifest)
            BSL.writeFile configPath (encode (snapshotFromUi ui testWorldName))

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (loadedManifest, _snapshot, loadedWorld) -> do
                wsmName loadedManifest `shouldBe` testWorldName
                wsmOverlayNames loadedManifest `shouldBe` []
                lookupOverlay "weather" (twOverlays loadedWorld) `shouldBe` Nothing
        )

deletionSpec :: Spec
deletionSpec = describe "deleteNamedWorld" $ do
  it "supports list-delete-list without touching sibling provider data" $ do
    let name = "__topo_test_delete_world__"
        siblingName = "__topo_test_provider_owned__" :: Text.Text
        malformedName = "__topo_test_malformed_world__" :: Text.Text
        stagingName = name <> ".saving"
        ui = emptyUiState { uiSeed = 303, uiChunkSize = 64 }
        world = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta
    worldsRoot <- worldDir
    let siblingDir = worldsRoot </> ".." </> Text.unpack siblingName
        marker = siblingDir </> "provider-state.txt"
        malformedDir = worldsRoot </> Text.unpack malformedName
        malformedMarker = malformedDir </> "uncommitted-state.txt"
        stagingDir = worldsRoot </> Text.unpack stagingName
        stagingMarker = stagingDir </> "staging-state.txt"
        cleanup = do
          _ <- deleteNamedWorld name
          _ <- try @IOException (removePathForcibly siblingDir)
          _ <- try @IOException (removePathForcibly malformedDir)
          _ <- try @IOException (removePathForcibly stagingDir)
          pure ()
    bracket
      (do
          cleanup
          createDirectoryIfMissing True siblingDir
          writeFile marker "provider-owned"
          createDirectoryIfMissing True malformedDir
          writeFile malformedMarker "not-a-saved-world"
      )
      (const cleanup)
      (\_ -> do
          saved <- saveNamedWorldWithPluginsAndExternalData
            name ui world [] [testExternalDataSourceSnapshot]
          saved `shouldBe` Right ()
          before <- map wsmName <$> listWorlds
          before `shouldSatisfy` elem name
          loaded <- loadNamedWorld name
          manifest <- case loaded of
            Left err -> expectationFailure (Text.unpack err) >> fail "unreachable"
            Right (savedManifest, _, _) -> pure savedManifest
          createDirectoryIfMissing True stagingDir
          BSL.writeFile (stagingDir </> "meta.json") (encode manifest)
          writeFile stagingMarker "in-progress"

          deleteNamedWorld ("../" <> siblingName) `shouldReturn` Left
            "persistence name must not contain path separators"
          doesFileExist marker `shouldReturn` True
          deleteNamedWorld malformedName `shouldReturn` Left
            ("World not found: " <> malformedName)
          doesFileExist malformedMarker `shouldReturn` True
          deleteNamedWorld stagingName `shouldReturn` Left
            ("World not found: " <> stagingName)
          doesFileExist stagingMarker `shouldReturn` True

          deleteNamedWorld name `shouldReturn` Right ()
          after <- map wsmName <$> listWorlds
          after `shouldNotSatisfy` elem name
          doesFileExist marker `shouldReturn` True
          deleteNamedWorld name `shouldReturn` Left ("World not found: " <> name)
      )

  it "rejects unsafe names consistently for save, load, and delete" $ do
    let world = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta
        ui = emptyUiState
    forM_ ["", " ", ".", "..", "nested/name", "nested\\name", "C:world", "name:stream", "NUL", "con.txt", "COM1", "victim.", "victim ", "bad\NULname"] $ \name -> do
      saveNamedWorld name ui world `shouldReturn` Left
        (expectedNameError name)
      fmap (fmap (const ())) (loadNamedWorld name)
        `shouldReturn` Left (expectedNameError name)
      deleteNamedWorld name `shouldReturn` Left (expectedNameError name)
  where
    expectedNameError name
      | Text.null (Text.strip name) = "persistence name must not be blank"
      | name == "." || name == ".." = "persistence name must not be a dot segment"
      | Text.any (\c -> c == '\NUL') name = "persistence name must not contain control characters"
      | Text.any (\c -> c == '/' || c == '\\') name = "persistence name must not contain path separators"
      | Text.length name >= 2 && Text.index name 1 == ':' = "persistence name must not be an absolute or drive path"
      | Text.any (`elem` ("<>:\"|?*" :: String)) name = "persistence name must not contain reserved path characters"
      | Text.toUpper (Text.takeWhile (/= '.') name) `elem` ["NUL", "CON", "COM1"] = "persistence name must not be a reserved device name"
      | otherwise = "persistence name must not end in a dot or space"

snapshotToWorldSpec :: Spec
snapshotToWorldSpec = describe "snapshotToWorld" $
  it "preserves authoritative snapshot geographic metadata/time and overlays" $ do
    let overlay = mkSparseFloatOverlay
          "persist_sparse_test"
          "snapshot reconstruction test"
          0.75
          emptyOverlayProvenance
        overlayStore = insertOverlay overlay emptyOverlayStore
        authoritativeGeo = defaultTerrainGeoContext
          { tgcHexGrid = HexGridMeta 13
          , tgcPlanet = defaultPlanetConfig { pcRadius = 6800, pcAxialTilt = 12 }
          , tgcSlice = defaultWorldSlice { wsLatCenter = 12.5, wsLonCenter = (-45.0) }
          , tgcWorldTime = WorldTime 17 simulationTickSeconds
          }
        terrainSnap = (emptyTerrainSnapshot 32)
          { tsVersion = 1
          , tsOverlayStore = overlayStore
          , tsGeoContext = authoritativeGeo
          }
        ui = emptyUiState
          { uiSeed = 99
          , uiHexSizeKm = unmapRange 2.0 20.0 3.0
          , uiPlanetRadius = unmapRange 4778.0 9557.0 7000.0
          , uiSliceLatCenter = unmapRange (-90.0) 90.0 30.0
          , uiSliceLonCenter = unmapRange (-180.0) 180.0 70.0
          , uiSimTickCount = 99
          }
        genCfg = (configFromUi ui)
          { worldHexGrid = tgcHexGrid authoritativeGeo
          , worldPlanet = tgcPlanet authoritativeGeo
          , worldSlice = tgcSlice authoritativeGeo
          }
        world = snapshotToWorld ui terrainSnap

    twConfig world `shouldBe` WorldConfig { wcChunkSize = 32 }
    twHexGrid world `shouldBe` tgcHexGrid authoritativeGeo
    twPlanet world `shouldBe` tgcPlanet authoritativeGeo
    twSlice world `shouldBe` tgcSlice authoritativeGeo
    twSeed world `shouldBe` 99
    wtTick (twWorldTime world) `shouldBe` 17
    wtTickRate (twWorldTime world) `shouldBe` simulationTickSeconds
    twGenConfig world `shouldBe` Just (toJSON genCfg)
    twOverlayManifest world `shouldBe` ["persist_sparse_test"]
    lookupOverlay "persist_sparse_test" (twOverlays world) `shouldSatisfy` (/= Nothing)
    (opSchedule . ovProvenance <$> lookupOverlay "persist_sparse_test" (twOverlays world))
      `shouldBe` Just Nothing

-- ---------------------------------------------------------------------------
-- listWorlds
-- ---------------------------------------------------------------------------

-- | Unique names for list test.
testListNameA :: Text.Text
testListNameA = "__topo_test_list_world_A__"

testListNameB :: Text.Text
testListNameB = "__topo_test_list_world_B__"

testListNameInvalid :: Text.Text
testListNameInvalid = "__topo_test_list_invalid__"

listWorldsSpec :: Spec
listWorldsSpec = describe "listWorlds" $
  it "returns manifests sorted by date (newest first), skips invalid dirs" $
    bracket
      (pure ())
      (\_ -> do
          _ <- deleteNamedWorld testListNameA
          _ <- deleteNamedWorld testListNameB
          -- Clean up invalid and synthetic atomic directories manually.
          dir <- worldDir
          mapM_ cleanupPath
            [ dir </> Text.unpack testListNameInvalid
            , dir </> Text.unpack testListNameA <> ".saving"
            , dir </> Text.unpack testListNameA <> ".old"
            ]
      )
      (\_ -> do
          let config = WorldConfig { wcChunkSize = 64 }
              world  = emptyWorld config defaultHexGridMeta

          -- Save world A first
          let uiA = emptyUiState { uiSeed = 100, uiChunkSize = 64 }
          _ <- saveNamedWorld testListNameA uiA world

          -- Atomic staging/backup directories can contain valid manifests while
          -- a save is active or after interruption, but are never saved worlds.
          loadedA <- loadNamedWorld testListNameA
          manifestA <- case loadedA of
            Left err -> expectationFailure (Text.unpack err) >> fail "unreachable"
            Right (manifest, _snapshot, _world) -> pure manifest
          dir <- worldDir
          let syntheticStaging = dir </> Text.unpack testListNameA <> ".saving"
          createDirectoryIfMissing True syntheticStaging
          BSL.writeFile (syntheticStaging </> "meta.json") (encode manifestA)

          -- Save world B second (will have a later timestamp)
          let uiB = emptyUiState { uiSeed = 200, uiChunkSize = 64 }
          _ <- saveNamedWorld testListNameB uiB world

          -- Create an invalid directory (no meta.json)
          let invalidPath = dir </> Text.unpack testListNameInvalid
          createDirectoryIfMissing True invalidPath

          -- List
          worlds <- listWorlds
          let testWorlds = filter
                (\m -> Text.pack "__topo_test_list_" `Text.isPrefixOf` wsmName m)
                worlds

          -- Should have exactly 2 valid entries (invalid dir skipped)
          length testWorlds `shouldBe` 2

          -- Newest first: B should come before A
          case testWorlds of
            (first:second:_) -> do
              wsmName first `shouldBe` testListNameB
              wsmName second `shouldBe` testListNameA
              wsmSeed first `shouldBe` 200
              wsmSeed second `shouldBe` 100
            _ -> expectationFailure "Expected at least 2 test worlds"
      )
