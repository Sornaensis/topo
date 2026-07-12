{-# LANGUAGE OverloadedStrings #-}

module Spec.PipelineIntegrator (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.IO (stdin, stdout)
import Test.Hspec

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginGeneratorResolution(..)
  , PluginLifecycleState(..)
  , PluginPipelineDiagnostic(..)
  , PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , PluginStatus(..)
  , buildPluginPipelinePlan
  , newConnectionOnlyPluginRuntime
  , pluginLifecycleSnapshot
  )
import Topo.Pipeline (PipelineConfig(..), PipelineStage(..), defaultPipelineConfig)
import Topo.Pipeline.Stage (StageId(..), stageCanonicalName)
import Topo.Plugin.RPC
  ( RPCManifest(..)
  , RPCManifestRuntime(..)
  , newRPCConnection
  )
import Topo.Plugin.RPC.Manifest
  ( RPCGeneratorDecl(..)
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.RPC.Transport (Transport(..))

spec :: Spec
spec = describe "PipelineIntegrator" $ do
  it "interleaves generator stages after concrete built-in anchors and uses user order only within the same anchor" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "a" "erosion" []
            , generator "b" "erosion" []
            , generator "c" "biomes" []
            ]
            ["b", "a", "c"]
            Set.empty
            Set.empty)
          (basePipeline [StagePlateTerrain, StageErosion, StageHydrology, StageBiomes, StageWeather])
    pipelineStageIds plan `shouldBe`
      [ StagePlateTerrain
      , StageErosion
      , StagePlugin "b"
      , StagePlugin "a"
      , StageHydrology
      , StageBiomes
      , StagePlugin "c"
      , StageWeather
      ]

  it "lets plugin-to-plugin generator dependencies override saved user order" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "a" "erosion" []
            , generator "b" "a" ["a"]
            ]
            ["b", "a"]
            Set.empty
            Set.empty)
          (basePipeline [StageErosion, StageBiomes])
    pipelineStageIds plan `shouldBe`
      [ StageErosion
      , StagePlugin "a"
      , StagePlugin "b"
      , StageBiomes
      ]
    runtimeOrder plan `shouldBe` ["a", "b"]

  it "resolves repeated built-in anchors to the last concrete occurrence and convergence to the final boundary" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "after-convergence" "convergence" []
            , generator "after-climate" "climate" []
            ]
            ["after-convergence", "after-climate"]
            Set.empty
            Set.empty)
          (basePipeline
            [ StageClimate
            , StageBiomes
            , StageVegetationFeedback
            , StageClimate
            , StageBiomes
            , StageVegetationFeedback
            , StageWeather
            ])
    pipelineStageIds plan `shouldBe`
      [ StageClimate
      , StageBiomes
      , StageVegetationFeedback
      , StageClimate
      , StagePlugin "after-climate"
      , StageBiomes
      , StageVegetationFeedback
      , StagePlugin "after-convergence"
      , StageWeather
      ]
    resolutionAnchorIndex "after-climate" plan `shouldBe` Just 3
    resolutionDisplayAnchor "after-convergence" plan `shouldBe` Just StageConvergence
    resolutionAnchorIndex "after-convergence" plan `shouldBe` Just 5

  it "lifts built-in requires to the last repeated concrete occurrence" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "requires-climate" "ocean-currents" ["climate"]
            ]
            []
            Set.empty
            Set.empty)
          (basePipeline [StageClimate, StageOceanCurrents, StageClimate, StageBiomes])
    pipelineStageIds plan `shouldBe`
      [ StageClimate
      , StageOceanCurrents
      , StageClimate
      , StagePlugin "requires-climate"
      , StageBiomes
      ]
    resolutionAnchorIndex "requires-climate" plan `shouldBe` Just 2

  it "lifts consumers after required providers when repeated anchors move the provider later" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "after-climate" "climate" []
            , generator "ocean-consumer" "ocean-currents" ["after-climate"]
            ]
            ["ocean-consumer", "after-climate"]
            Set.empty
            Set.empty)
          (basePipeline [StageClimate, StageOceanCurrents, StageClimate, StageBiomes])
    pipelineStageIds plan `shouldBe`
      [ StageClimate
      , StageOceanCurrents
      , StageClimate
      , StagePlugin "after-climate"
      , StagePlugin "ocean-consumer"
      , StageBiomes
      ]
    runtimeOrder plan `shouldBe` ["after-climate", "ocean-consumer"]
    resolutionAnchorIndex "ocean-consumer" plan `shouldBe` Just 2

  it "omits generators anchored to disabled built-ins" $ do
    let disabled = Set.singleton StageErosion
        plan = buildPluginPipelinePlan
          (input [generator "a" "erosion" []] ["a"] Set.empty disabled)
          (basePipeline [StageErosion, StageBiomes])
    pipelineStageIds plan `shouldBe` [StageErosion, StageBiomes]
    diagnosticsFor "a" plan `shouldSatisfy` anyMessageContaining "disabled stage 'erosion'"

  it "treats StagePlugin disabled entries as disabled providers and omits transitive consumers" $ do
    let disabledStages = Set.singleton (StagePlugin "a")
        plan = buildPluginPipelinePlan
          (input
            [ generator "a" "biomes" []
            , generator "b" "a" ["a"]
            ]
            ["b", "a"]
            Set.empty
            disabledStages)
          (basePipeline [StageBiomes, StageWeather])
    pipelineStageIds plan `shouldBe` [StageBiomes, StageWeather]
    diagnosticsFor "a" plan `shouldSatisfy` anyMessageContaining "unavailable as a generator provider"
    diagnosticsFor "b" plan `shouldSatisfy` anyMessageContaining "disabled plugin 'a'"

  it "omits missing, cyclic, and non-generator plugin dependencies with diagnostics" $ do
    let plan = buildPluginPipelinePlan
          (input
            [ generator "missing-consumer" "missing-provider" []
            , generator "cycle-a" "cycle-b" []
            , generator "cycle-b" "cycle-a" []
            , nonGenerator "registry"
            , generator "registry-consumer" "biomes" ["registry"]
            ]
            []
            Set.empty
            Set.empty)
          (basePipeline [StageBiomes, StageWeather])
    pipelineStageIds plan `shouldBe` [StageBiomes, StageWeather]
    diagnosticsFor "missing-consumer" plan `shouldSatisfy` anyMessageContaining "missing plugin 'missing-provider'"
    diagnosticsFor "cycle-a" plan `shouldSatisfy` anyMessageContaining "Dependency cycle"
    diagnosticsFor "cycle-b" plan `shouldSatisfy` anyMessageContaining "Dependency cycle"
    diagnosticsFor "registry-consumer" plan `shouldSatisfy` anyMessageContaining "disabled plugin 'registry'"

basePipeline :: [StageId] -> PipelineConfig
basePipeline ids = defaultPipelineConfig
  { pipelineStages = map dummyStage ids
  }

dummyStage :: StageId -> PipelineStage
dummyStage sid = PipelineStage
  { stageId = sid
  , stageName = stageCanonicalName sid
  , stageSeedTag = stageCanonicalName sid
  , stageOverlayProduces = Nothing
  , stageOverlayReads = []
  , stageOverlaySchema = Nothing
  , stageRun = pure ()
  }

input :: [LoadedPlugin] -> [Text] -> Set.Set Text -> Set.Set StageId -> PluginPipelineInput
input plugins order disabledPlugins disabledStages = PluginPipelineInput
  { ppiPlugins = plugins
  , ppiPluginOrder = order
  , ppiDisabledPlugins = disabledPlugins
  , ppiDisabledStages = disabledStages
  }

generator :: Text -> Text -> [Text] -> LoadedPlugin
generator name insertAfter requires = connectedPlugin name (Just RPCGeneratorDecl
  { rgdInsertAfter = insertAfter
  , rgdRequires = requires
  })

nonGenerator :: Text -> LoadedPlugin
nonGenerator name = connectedPlugin name Nothing

connectedPlugin :: Text -> Maybe RPCGeneratorDecl -> LoadedPlugin
connectedPlugin name mGenerator = LoadedPlugin
  { lpName = name
  , lpManifest = manifest
  , lpParams = Map.empty
  , lpStatus = PluginConnected
  , lpLifecycle = pluginLifecycleSnapshot now LifecycleReady Nothing Nothing Nothing Nothing Nothing (Just currentProtocolVersion) []
  , lpRuntime = Just (newConnectionOnlyPluginRuntime
      (newRPCConnection manifest (Transport stdin stdout name) Map.empty))
  , lpStartPolicy = defaultRPCStartPolicy
  , lpRestartHistory = []
  , lpDirectory = ""
  , lpOverlaySchema = Nothing
  }
  where
    manifest = manifestFor name mGenerator

manifestFor :: Text -> Maybe RPCGeneratorDecl -> RPCManifest
manifestFor name mGenerator = RPCManifest
  { rmManifestVersion = 3
  , rmName = name
  , rmVersion = "1.0.0"
  , rmRuntime = RPCManifestRuntime currentProtocolVersion currentProtocolVersion Nothing Nothing
  , rmDescription = ""
  , rmUiHints = defaultRPCUIHints
  , rmGenerator = mGenerator
  , rmSimulation = Nothing
  , rmOverlay = Nothing
  , rmCapabilities = []
  , rmParameters = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy = defaultRPCStartPolicy
  }

now :: UTCTime
now = posixSecondsToUTCTime 0

pipelineStageIds :: PluginPipelinePlan -> [StageId]
pipelineStageIds = map stageId . pipelineStages . pppPipelineConfig

runtimeOrder :: PluginPipelinePlan -> [Text]
runtimeOrder plan =
  [ pgrPlugin resolution
  | resolution <- pppResolutions plan
  , pgrEnabled resolution
  ]

resolutionAnchorIndex :: Text -> PluginPipelinePlan -> Maybe Int
resolutionAnchorIndex name plan = do
  resolution <- lookupResolution name plan
  pgrAnchorIndex resolution

resolutionDisplayAnchor :: Text -> PluginPipelinePlan -> Maybe StageId
resolutionDisplayAnchor name plan = do
  resolution <- lookupResolution name plan
  pgrDisplayAnchor resolution

lookupResolution :: Text -> PluginPipelinePlan -> Maybe PluginGeneratorResolution
lookupResolution name plan = case [res | res <- pppResolutions plan, pgrPlugin res == name] of
  res:_ -> Just res
  [] -> Nothing

diagnosticsFor :: Text -> PluginPipelinePlan -> [Text]
diagnosticsFor name plan =
  [ ppdMessage diag
  | resolution <- pppResolutions plan
  , pgrPlugin resolution == name
  , diag <- pgrDiagnostics resolution
  ]

anyMessageContaining :: Text -> [Text] -> Bool
anyMessageContaining needle = any (Text.isInfixOf needle)
