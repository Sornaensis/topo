{-# LANGUAGE OverloadedStrings #-}

module Spec.PluginDependency (spec) where

import Data.Aeson (FromJSON, Result(..), ToJSON, Value, fromJSON, object, toJSON, (.=))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (Capability(..))
import Topo.Plugin.Dependency
import Topo.Plugin.RPC.Manifest (RPCManifest, manifestV3, manifestV3ConsumerExample)

spec :: Spec
spec = describe "PluginDependency" $ do
  it "distinguishes blocking required dependencies from optional and soft dependencies" $ do
    dependencyModeBlocks DependencyRequired `shouldBe` True
    dependencyModeBlocks DependencyOptional `shouldBe` False
    dependencyModeBlocks DependencySoft `shouldBe` False

    let target = DependencyCapability CapDataRead
    dependencyDeclBlocks (DependencyDecl target DependencyRequired Nothing) `shouldBe` True
    dependencyDeclBlocks (DependencyDecl target DependencyOptional Nothing) `shouldBe` False
    dependencyDeclBlocks (DependencyDecl target DependencySoft Nothing) `shouldBe` False

  it "round-trips every dependency target kind through JSON" $ do
    let targets =
          [ DependencyBuiltInStage StageBiomes
          , DependencyPlugin (PluginDependency "climate-plus" (VersionBetween "1.2.0" "2.0.0"))
          , DependencyOverlay (OverlayDependency "weather" (Just "climate-plus"))
          , DependencyResource (ResourceDependency (Just "civilization") "settlements" [ResourceList, ResourceQueryByHex, ResourceQueryByField, ResourceSort, ResourceFilter, ResourcePage] (Just "civilization"))
          , DependencyExternalDataSource (ExternalDataSourceDependency (Just "civilization") "trade-routes" "settlement-ledger" Nothing [ExternalAccessRead] ["settlements"])
          , DependencyCapability CapDataRead
          ]
    mapM_ shouldRoundTrip targets
    map dependencyTargetKind targets `shouldBe`
      [ "builtInStage"
      , "plugin"
      , "overlay"
      , "resource"
      , "externalDataSource"
      , "capability"
      ]

  it "encodes plugin version constraints and soft dependency declarations" $ do
    let dep = DependencyDecl
          { ddTarget = DependencyPlugin (PluginDependency "erosion-plus" (VersionAtLeast "1.4.0"))
          , ddMode = DependencySoft
          , ddReason = Just "Use enhanced erosion output when the plugin is present."
          }
    toJSON dep `shouldBe` object
      [ "target" .= object
          [ "type" .= ("plugin" :: Text)
          , "name" .= ("erosion-plus" :: Text)
          , "version" .= object ["min" .= ("1.4.0" :: Text)]
          ]
      , "mode" .= ("soft" :: Text)
      , "reason" .= ("Use enhanced erosion output when the plugin is present." :: Text)
      ]
    shouldRoundTrip dep

  it "parses every version constraint shape" $ do
    object ["any" .= True] `shouldDecodeAs` VersionAny
    object ["exact" .= ("1.2.3" :: Text)] `shouldDecodeAs` VersionExact "1.2.3"
    toJSON ("1.2.3" :: Text) `shouldDecodeAs` VersionExact "1.2.3"
    object ["min" .= ("1.0.0" :: Text)] `shouldDecodeAs` VersionAtLeast "1.0.0"
    object ["max" .= ("2.0.0" :: Text)] `shouldDecodeAs` VersionAtMost "2.0.0"
    object ["min" .= ("1.0.0" :: Text), "max" .= ("2.0.0" :: Text)] `shouldDecodeAs` VersionBetween "1.0.0" "2.0.0"

  it "models backend-neutral external data-source provider and consumer edges" $ do
    let target = DependencyExternalDataSource ExternalDataSourceDependency
          { edsdProvider = Just "civilization"
          , edsdConsumer = "trade-routes"
          , edsdSource = "settlement-ledger"
          , edsdGrant = Just "settlement-read-write"
          , edsdAccess = [ExternalAccessRead, ExternalAccessWrite]
          , edsdResources = ["settlements", "markets"]
          }
    toJSON target `shouldBe` object
      [ "type" .= ("externalDataSource" :: Text)
      , "provider" .= ("civilization" :: Text)
      , "consumer" .= ("trade-routes" :: Text)
      , "source" .= ("settlement-ledger" :: Text)
      , "grant" .= ("settlement-read-write" :: Text)
      , "access" .= (["read", "write"] :: [Text])
      , "resources" .= (["settlements", "markets"] :: [Text])
      ]
    shouldRoundTrip target

  it "converts manifest generator, external data-source, and capability declarations" $ do
    case fromJSON manifestV3ConsumerExample :: Result RPCManifest of
      Error err -> expectationFailure err
      Success manifest -> do
        let targets = map ddTarget (manifestDependencyDecls manifest)
        targets `shouldSatisfy` elem (DependencyPlugin (PluginDependency "civilization" VersionAny))
        targets `shouldSatisfy` elem (DependencyCapability CapDataRead)
        targets `shouldSatisfy` elem (DependencyExternalDataSource ExternalDataSourceDependency
          { edsdProvider = Just "civilization"
          , edsdConsumer = "trade-routes"
          , edsdSource = "settlement-ledger"
          , edsdGrant = Just "settlement-read"
          , edsdAccess = [ExternalAccessRead]
          , edsdResources = ["settlements"]
          })

  it "keeps simulation node dependencies out of plugin startup dependencies" $ do
    let value = object
          [ "manifestVersion" .= manifestV3
          , "name" .= ("weather-consumer" :: Text)
          , "version" .= ("1.0.0" :: Text)
          , "runtime" .= object
              [ "protocol" .= object ["min" .= (3 :: Int), "max" .= (3 :: Int)]
              ]
          , "description" .= ("Consumes the built-in weather simulation node" :: Text)
          , "simulation" .= object
              [ "dependencies" .= (["weather"] :: [Text])
              ]
          , "capabilities" .= ([] :: [Text])
          ]
    case fromJSON value :: Result RPCManifest of
      Error err -> expectationFailure err
      Success manifest ->
        manifestDependencyDecls manifest `shouldBe` []

  it "validates missing, disabled, and available dependency targets with actionable diagnostics" $ do
    let consumer = provider "trade-routes"
          [ required (DependencyPlugin (PluginDependency "climate-plus" VersionAny))
          , required (DependencyBuiltInStage StageBiomes)
          , required (DependencyOverlay (OverlayDependency "weather" (Just "weather-plus")))
          , required (DependencyResource (ResourceDependency (Just "civilization") "settlements" [] Nothing))
          , required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "atlas") "trade-routes" "settlement-ledger" Nothing [ExternalAccessRead] []))
          , required (DependencyCapability CapDataRead)
          ]
        civilization = (provider "civilization" [])
          { dpResources = [DependencyResourceProvider "settlements" [] Nothing]
          }
        weatherPlus = (provider "weather-plus" [])
          { dpOverlays = ["weather"]
          }
        input = (defaultDependencyResolverInput [consumer, civilization, weatherPlus])
          { driDisabledStages = Set.singleton StageBiomes
          , driDisabledPlugins = Set.singleton "civilization"
          , driAvailableCapabilities = Set.empty
          }
        diagnostics = validateDependencies input
        messages = map dgdMessage diagnostics

    map dgdStatus diagnostics `shouldBe`
      [ DependencyMissing
      , DependencyDisabled
      , DependencyAvailable
      , DependencyDisabled
      , DependencyMissing
      , DependencyMissing
      ]
    map dgdBlocking diagnostics `shouldBe` [True, True, False, True, True, True]
    messages `shouldSatisfy` any (Text.isInfixOf "missing plugin 'climate-plus'")
    messages `shouldSatisfy` any (Text.isInfixOf "disabled stage 'biomes'")
    messages `shouldSatisfy` any (Text.isInfixOf "overlay 'weather' is available from plugin 'weather-plus'")
    messages `shouldSatisfy` any (Text.isInfixOf "resource 'settlements' from disabled provider plugin 'civilization'")
    messages `shouldSatisfy` any (Text.isInfixOf "missing provider plugin 'atlas' for external data-source 'settlement-ledger'")
    messages `shouldSatisfy` any (Text.isInfixOf "missing capability 'dataRead'")

  it "reports optional and soft missing dependencies without blocking readiness" $ do
    let consumer = provider "optional-consumer"
          [ DependencyDecl (DependencyPlugin (PluginDependency "missing-optional" VersionAny)) DependencyOptional Nothing
          , DependencyDecl (DependencyCapability CapDataWrite) DependencySoft Nothing
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [consumer])

    map dgdStatus diagnostics `shouldBe` [DependencyMissing, DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [False, False]
    blockingDependencyDiagnostics diagnostics `shouldBe` []

  it "treats resource operation/overlay and external resource mismatches as missing dependencies" $ do
    let providerPlugin = (provider "geo" [])
          { dpResources = [DependencyResourceProvider "settlements" [ResourceList] (Just "settlements-overlay")]
          , dpExternalDataSources = [externalProvider "ledger" ["settlements"]]
          }
        consumer = provider "consumer"
          [ required (DependencyResource (ResourceDependency (Just "geo") "settlements" [ResourceUpdate] (Just "other-overlay")))
          , required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "consumer" "ledger" Nothing [ExternalAccessRead] ["markets"]))
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [providerPlugin, consumer])
        messages = map dgdMessage diagnostics

    map dgdStatus diagnostics `shouldBe` [DependencyMissing, DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [True, True]
    messages `shouldSatisfy` any (Text.isInfixOf "missing resource 'settlements' from provider plugin 'geo'")
    messages `shouldSatisfy` any (Text.isInfixOf "missing external data-source 'ledger' from provider plugin 'geo'")

  it "requires external data-source grants to satisfy requested access" $ do
    let providerPlugin = (provider "geo" [])
          { dpExternalDataSources = [externalProvider "ledger" ["settlements"]]
          }
        consumer = provider "consumer"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "consumer" "ledger" (Just "read") [ExternalAccessWrite] ["settlements"]))
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [providerPlugin, consumer])

    map dgdStatus diagnostics `shouldBe` [DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [True]
    map dgdMessage diagnostics `shouldSatisfy` any (Text.isInfixOf "missing external data-source 'ledger' from provider plugin 'geo'")

  it "requires external data-source grant capabilities to satisfy requested access" $ do
    let writeWithoutMutate = (externalProvider "ledger" ["settlements"])
          { despCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
          , despGrants =
              [ DependencyExternalDataSourceGrant
                  { desgName = "write"
                  , desgAccess = [ExternalAccessWrite]
                  , desgCapabilities = [ExternalSourceQuery]
                  , desgResources = ["settlements"]
                  , desgStatus = ExternalStatusReady
                  }
              ]
          }
        providerPlugin = (provider "geo" [])
          { dpExternalDataSources = [writeWithoutMutate]
          }
        consumer = provider "consumer"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "consumer" "ledger" (Just "write") [ExternalAccessWrite] ["settlements"]))
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [providerPlugin, consumer])

    map dgdStatus diagnostics `shouldBe` [DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [True]
    map dgdMessage diagnostics `shouldSatisfy` any (Text.isInfixOf "missing external data-source 'ledger' from provider plugin 'geo'")

  it "requires external data-source provider and grant status to be ready" $ do
    let nonReadySource = (externalProvider "ledger" ["settlements"])
          { despStatus = ExternalStatusDegraded
          }
        nonReadyGrant = (externalProvider "archive" ["settlements"])
          { despGrants =
              [ DependencyExternalDataSourceGrant
                  { desgName = "read"
                  , desgAccess = [ExternalAccessRead]
                  , desgCapabilities = [ExternalSourceQuery]
                  , desgResources = ["settlements"]
                  , desgStatus = ExternalStatusUnavailable
                  }
              ]
          }
        providerPlugin = (provider "geo" [])
          { dpExternalDataSources = [nonReadySource, nonReadyGrant]
          }
        sourceConsumer = provider "source-consumer"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "source-consumer" "ledger" (Just "read") [ExternalAccessRead] ["settlements"]))
          ]
        grantConsumer = provider "grant-consumer"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "grant-consumer" "archive" (Just "read") [ExternalAccessRead] ["settlements"]))
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [providerPlugin, sourceConsumer, grantConsumer])

    map dgdStatus diagnostics `shouldBe` [DependencyMissing, DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [True, True]

  it "detects required dependency cycles through plugin, resource, and external data-source provider edges" $ do
    let pluginA = (provider "a" [required (DependencyPlugin (PluginDependency "b" VersionAny))])
          { dpExternalDataSources = [externalProvider "ledger" []]
          }
        pluginB = provider "b"
          [ required (DependencyResource (ResourceDependency (Just "c") "settlements" [] Nothing))
          ]
        pluginC = (provider "c"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "a") "c" "ledger" Nothing [ExternalAccessRead] []))
          ])
          { dpResources = [DependencyResourceProvider "settlements" [] Nothing]
          }
        diagnostics = validateDependencies (defaultDependencyResolverInput [pluginA, pluginB, pluginC])

    map dgdStatus diagnostics `shouldBe` replicate 3 DependencyCycle
    map dgdBlocking diagnostics `shouldBe` replicate 3 True
    case diagnostics of
      firstDiagnostic:_ -> do
        dgdCycle firstDiagnostic `shouldBe` ["a", "b", "c", "a"]
        dgdMessage firstDiagnostic `shouldSatisfy` Text.isInfixOf "a -> b -> c -> a"
      [] -> expectationFailure "expected cycle diagnostics"

  it "produces deterministic startup order and omits transitive blockers" $ do
    let root = provider "c-root" []
        middle = provider "b-provider" [required (DependencyPlugin (PluginDependency "c-root" VersionAny))]
        consumer = provider "a-consumer" [required (DependencyPlugin (PluginDependency "b-provider" VersionAny))]
        independent = provider "z-independent" []
        startupInput = defaultDependencyResolverInput [consumer, independent, middle, root]

    dependencyStartupOrder startupInput `shouldBe`
      ["c-root", "b-provider", "a-consumer", "z-independent"]

    let blockedProvider = provider "blocked-provider" [required (DependencyPlugin (PluginDependency "missing-provider" VersionAny))]
        blockedConsumer = provider "blocked-consumer" [required (DependencyPlugin (PluginDependency "blocked-provider" VersionAny))]
        order = resolveDependencyOrder (defaultDependencyResolverInput [independent, blockedConsumer, blockedProvider])

    droStartupOrder order `shouldBe` ["z-independent"]
    droBlockedPlugins order `shouldBe` ["blocked-consumer", "blocked-provider"]

  it "uses an unblocked unqualified external data-source provider when another match is blocked" $ do
    let blockedLedger = (provider "a-ledger" [required (DependencyPlugin (PluginDependency "missing-provider" VersionAny))])
          { dpExternalDataSources = [externalProvider "settlement-ledger" ["settlements"]]
          }
        healthyLedger = (provider "z-ledger" [])
          { dpExternalDataSources = [externalProvider "settlement-ledger" ["settlements"]]
          }
        consumer = provider "consumer"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency Nothing "consumer" "settlement-ledger" Nothing [ExternalAccessRead] ["settlements"]))
          ]
        order = resolveDependencyOrder (defaultDependencyResolverInput [consumer, healthyLedger, blockedLedger])

    droBlockedPlugins order `shouldBe` ["a-ledger"]
    droStartupOrder order `shouldBe` ["z-ledger", "consumer"]
    droSimulationOrder order `shouldBe` ["z-ledger", "consumer"]

  it "uses an acyclic unqualified overlay provider when an earlier match would cycle" $ do
    let consumer = provider "a-consumer"
          [ required (DependencyOverlay (OverlayDependency "shared" Nothing))
          ]
        cyclicProvider = (provider "b-cycle" [required (DependencyPlugin (PluginDependency "a-consumer" VersionAny))])
          { dpOverlays = ["shared"]
          }
        healthyProvider = (provider "z-provider" [])
          { dpOverlays = ["shared"]
          }
        order = resolveDependencyOrder (defaultDependencyResolverInput [consumer, healthyProvider, cyclicProvider])

    map dgdStatus (droDiagnostics order) `shouldBe` [DependencyAvailable, DependencyAvailable]
    droBlockedPlugins order `shouldBe` []
    droStartupOrder order `shouldBe` ["z-provider", "a-consumer", "b-cycle"]

  it "blocks unqualified provider cycles that remain after an acyclic alternative is blocked" $ do
    let consumer = provider "a-consumer"
          [ required (DependencyOverlay (OverlayDependency "shared" Nothing))
          ]
        cyclicProvider = (provider "b-cycle" [required (DependencyPlugin (PluginDependency "a-consumer" VersionAny))])
          { dpOverlays = ["shared"]
          }
        blockedProvider = (provider "z-provider" [required (DependencyPlugin (PluginDependency "missing-provider" VersionAny))])
          { dpOverlays = ["shared"]
          }
        order = resolveDependencyOrder (defaultDependencyResolverInput [consumer, blockedProvider, cyclicProvider])

    droBlockedPlugins order `shouldBe` ["a-consumer", "b-cycle", "z-provider"]
    droStartupOrder order `shouldBe` []

  it "produces deterministic pipeline insertion anchors from built-in and plugin prerequisites" $ do
    let erosion = provider "erosion-ext" [required (DependencyBuiltInStage StageErosion)]
        biome = provider "biome-ext" [required (DependencyBuiltInStage StageBiomes)]
        hydrologyConsumer = provider "hydrology-consumer"
          [ required (DependencyBuiltInStage StageHydrology)
          , required (DependencyPlugin (PluginDependency "biome-ext" VersionAny))
          ]
        lateConsumer = provider "late-consumer"
          [ required (DependencyBuiltInStage StageWeather)
          , required (DependencyPlugin (PluginDependency "biome-ext" VersionAny))
          ]
        input = defaultDependencyResolverInput [lateConsumer, hydrologyConsumer, biome, erosion]

    dependencyPipelineOrder input `shouldBe`
      [ DependencyPipelineInsertion "erosion-ext" (Just StageErosion)
      , DependencyPipelineInsertion "biome-ext" (Just StageBiomes)
      , DependencyPipelineInsertion "hydrology-consumer" (Just (StagePlugin "biome-ext"))
      , DependencyPipelineInsertion "late-consumer" (Just StageWeather)
      ]

  it "keeps optional and soft provider edges non-blocking when preferences cycle" $ do
    let pluginA = provider "a"
          [ DependencyDecl (DependencyPlugin (PluginDependency "b" VersionAny)) DependencySoft Nothing
          ]
        pluginB = provider "b"
          [ DependencyDecl (DependencyPlugin (PluginDependency "a" VersionAny)) DependencyOptional Nothing
          ]
        order = resolveDependencyOrder (defaultDependencyResolverInput [pluginB, pluginA])

    droStartupOrder order `shouldBe` ["a", "b"]
    droBlockedPlugins order `shouldBe` []

  it "does not anchor pipeline insertions to providers without concrete pipeline anchors" $ do
    let registry = provider "registry" []
        generator = provider "generator"
          [ required (DependencyBuiltInStage StageErosion)
          , required (DependencyPlugin (PluginDependency "registry" VersionAny))
          ]
        input = defaultDependencyResolverInput [generator, registry]

    dependencyPipelineOrder input `shouldBe`
      [ DependencyPipelineInsertion "registry" Nothing
      , DependencyPipelineInsertion "generator" (Just StageErosion)
      ]

  it "produces simulation insertion order from overlay and external data-source provider edges" $ do
    let civilization = (provider "civilization" [])
          { dpOverlays = ["settlements"]
          }
        ledger = (provider "ledger" [])
          { dpExternalDataSources = [externalProvider "settlement-ledger" ["settlements"]]
          }
        tradeRoutes = provider "trade-routes"
          [ required (DependencyOverlay (OverlayDependency "settlements" (Just "civilization")))
          , required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "ledger") "trade-routes" "settlement-ledger" Nothing [ExternalAccessRead] ["settlements"]))
          ]
        input = defaultDependencyResolverInput [tradeRoutes, ledger, civilization]

    dependencySimulationOrder input `shouldBe` ["civilization", "ledger", "trade-routes"]

  it "rejects plugin stages in built-in stage dependencies" $ do
    let badTarget = object
          [ "type" .= ("builtInStage" :: Text)
          , "stage" .= ("plugin:civilization" :: Text)
          ]
    case fromJSON badTarget :: Result DependencyTarget of
      Error _ -> pure ()
      Success target -> expectationFailure ("unexpected parsed target: " <> show target)

provider :: Text -> [DependencyDecl] -> DependencyProvider
provider name deps = DependencyProvider
  { dpName = name
  , dpVersion = "1.0.0"
  , dpDependencies = deps
  , dpCapabilities = []
  , dpOverlays = []
  , dpResources = []
  , dpExternalDataSources = []
  }

externalProvider :: Text -> [Text] -> DependencyExternalDataSourceProvider
externalProvider name resources = DependencyExternalDataSourceProvider
  { despName = name
  , despCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
  , despResources = resources
  , despStatus = ExternalStatusReady
  , despGrants =
      [ DependencyExternalDataSourceGrant
          { desgName = "read"
          , desgAccess = [ExternalAccessRead]
          , desgCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
          , desgResources = resources
          , desgStatus = ExternalStatusReady
          }
      ]
  }

required :: DependencyTarget -> DependencyDecl
required target = DependencyDecl
  { ddTarget = target
  , ddMode = DependencyRequired
  , ddReason = Nothing
  }

shouldRoundTrip :: (Eq a, FromJSON a, Show a, ToJSON a) => a -> Expectation
shouldRoundTrip value =
  toJSON value `shouldDecodeAs` value

shouldDecodeAs :: (Eq a, FromJSON a, Show a) => Value -> a -> Expectation
shouldDecodeAs encoded expected =
  case fromJSON encoded of
    Success decoded -> decoded `shouldBe` expected
    Error err -> expectationFailure err
