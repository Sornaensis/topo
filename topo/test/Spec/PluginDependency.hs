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
import Topo.Plugin.RPC.Manifest (RPCManifest, manifestV3ConsumerExample)

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
          , DependencyResource (ResourceDependency (Just "civilization") "settlements" [ResourceList, ResourceQueryByHex] (Just "civilization"))
          , DependencyExternalDataSource (ExternalDataSourceDependency (Just "civilization") "trade-routes" "settlement-ledger" [ExternalAccessRead] ["settlements"])
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
          , edsdAccess = [ExternalAccessRead, ExternalAccessWrite]
          , edsdResources = ["settlements", "markets"]
          }
    toJSON target `shouldBe` object
      [ "type" .= ("externalDataSource" :: Text)
      , "provider" .= ("civilization" :: Text)
      , "consumer" .= ("trade-routes" :: Text)
      , "source" .= ("settlement-ledger" :: Text)
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
          , edsdAccess = [ExternalAccessRead]
          , edsdResources = ["settlements"]
          })

  it "validates missing, disabled, and available dependency targets with actionable diagnostics" $ do
    let consumer = provider "trade-routes"
          [ required (DependencyPlugin (PluginDependency "climate-plus" VersionAny))
          , required (DependencyBuiltInStage StageBiomes)
          , required (DependencyOverlay (OverlayDependency "weather" (Just "weather-plus")))
          , required (DependencyResource (ResourceDependency (Just "civilization") "settlements" [] Nothing))
          , required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "atlas") "trade-routes" "settlement-ledger" [ExternalAccessRead] []))
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
          , dpExternalDataSources = [DependencyExternalDataSourceProvider "ledger" ["settlements"]]
          }
        consumer = provider "consumer"
          [ required (DependencyResource (ResourceDependency (Just "geo") "settlements" [ResourceUpdate] (Just "other-overlay")))
          , required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "geo") "consumer" "ledger" [ExternalAccessRead] ["markets"]))
          ]
        diagnostics = validateDependencies (defaultDependencyResolverInput [providerPlugin, consumer])
        messages = map dgdMessage diagnostics

    map dgdStatus diagnostics `shouldBe` [DependencyMissing, DependencyMissing]
    map dgdBlocking diagnostics `shouldBe` [True, True]
    messages `shouldSatisfy` any (Text.isInfixOf "missing resource 'settlements' from provider plugin 'geo'")
    messages `shouldSatisfy` any (Text.isInfixOf "missing external data-source 'ledger' from provider plugin 'geo'")

  it "detects required dependency cycles through plugin, resource, and external data-source provider edges" $ do
    let pluginA = (provider "a" [required (DependencyPlugin (PluginDependency "b" VersionAny))])
          { dpExternalDataSources = [DependencyExternalDataSourceProvider "ledger" []]
          }
        pluginB = provider "b"
          [ required (DependencyResource (ResourceDependency (Just "c") "settlements" [] Nothing))
          ]
        pluginC = (provider "c"
          [ required (DependencyExternalDataSource (ExternalDataSourceDependency (Just "a") "c" "ledger" [ExternalAccessRead] []))
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
