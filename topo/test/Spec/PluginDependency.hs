{-# LANGUAGE OverloadedStrings #-}

module Spec.PluginDependency (spec) where

import Data.Aeson (FromJSON, Result(..), ToJSON, Value, fromJSON, object, toJSON, (.=))
import Data.Text (Text)
import Test.Hspec

import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (Capability(..))
import Topo.Plugin.Dependency

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

  it "rejects plugin stages in built-in stage dependencies" $ do
    let badTarget = object
          [ "type" .= ("builtInStage" :: Text)
          , "stage" .= ("plugin:civilization" :: Text)
          ]
    case fromJSON badTarget :: Result DependencyTarget of
      Error _ -> pure ()
      Success target -> expectationFailure ("unexpected parsed target: " <> show target)

shouldRoundTrip :: (Eq a, FromJSON a, Show a, ToJSON a) => a -> Expectation
shouldRoundTrip value =
  toJSON value `shouldDecodeAs` value

shouldDecodeAs :: (Eq a, FromJSON a, Show a) => Value -> a -> Expectation
shouldDecodeAs encoded expected =
  case fromJSON encoded of
    Success decoded -> decoded `shouldBe` expected
    Error err -> expectationFailure err
