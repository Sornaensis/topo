{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'Seer.Command.Dispatch.dispatchCommand' and the handler
-- modules it delegates to.
--
-- Each test spins up a real 'ActorSystem', constructs 'ActorHandles'
-- and a 'CommandContext', dispatches a 'SeerCommand', and asserts on
-- the resulting 'SeerResponse'.
module Spec.CommandDispatch (spec) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (bracket, catch)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), object, (.=), Key)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (newIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Hyperspace.Actor (ActorSystem, get, newActorSystem, replyTo, shutdownActorSystem)
import Test.Hspec

import Actor.AtlasManager (AtlasManager)
import Actor.Data (Data, DataSnapshot(..), TerrainSnapshot(..), getTerrainSnapshot, setTerrainChunkCount, setTerrainChunkData)
import Actor.Log (Log)
import Actor.PluginManager (LoadedPlugin(..), PluginManager, discoverPlugins, getLoadedPlugins)
import Actor.Simulation (Simulation)
import Actor.SnapshotReceiver
  ( newDataSnapshotRef
  , newTerrainSnapshotRef
  , newSnapshotVersionRef
  , writeTerrainSnapshot
  )
import Actor.Terrain (Terrain, TerrainReplyOps)
import Actor.UI
  ( Ui
  , UiState(..)
  , getUiSnapshot
  , newUiSnapshotRef
  , setUiGenerating
  , setUiSnapshotRef
  )
import Actor.UI.Setters (setUiPluginParamSpecs)
import Actor.UiActions (ActorHandles(..), UiActions)
import Actor.UiActions.Command (UiAction(..), UiActionRequest(..), runUiAction)

import Seer.Command.AppServiceAdapter (commandAppService, runAppServiceOperation)
import Seer.Command.Dispatch (CommandContext(..), dispatchCommand)
import Seer.Editor.History (emptyHistory)
import Seer.Screenshot (ScreenshotRequest(..), newScreenshotRequestRef)
import Seer.Service.AppService
  ( AppService(..)
  , ConfigListPresetsRequest(..)
  , ConfigListPresetsResponse(..)
  , ConfigService(..)
  , WorldGenerateRequest(..)
  , WorldGenerateResponse(..)
  , WorldListRequest(..)
  , WorldListResponse(..)
  , WorldService(..)
  , UiSetSeedRequest(..)
  , UiSetSeedResponse(..)
  , appServiceOperationMethods
  , appUi
  , configListPresetsOperation
  , uiSetSeed
  , uiSetSeedOperation
  , worldGenerateOperation
  , worldListOperation
  )
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceErrorDetail(..)
  , ServiceErrorKind(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceResult
  , adaptTypedServiceHandler
  , runServiceHandler
  , serviceErrorCode
  , serviceErrorDetails
  , serviceErrorKind
  )
import Topo (WorldConfig(..), chunkIdFromCoord, emptyTerrainChunk)
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))
import Topo.Overlay (emptyOverlayStore)
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin.RPC.Manifest
  ( RPCParamSpec(..)
  , RPCParamType(..)
  , manifestV3
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Types (ChunkCoord(..), ChunkId(..))

spec :: Spec
spec = describe "CommandDispatch" $ do

  describe "AppService adapter envelope" $ do
    it "preserves request ids for success, handler errors, and unknown commands" $ withCtx $ \ctx -> do
      ok <- dispatchWithId ctx 42 "get_state" Null
      srId ok `shouldBe` 42
      srSuccess ok `shouldBe` True

      handlerErr <- dispatchWithId ctx 43 "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      srId handlerErr `shouldBe` 43
      srSuccess handlerErr `shouldBe` False

      unknown <- dispatchWithId ctx 44 "no_such_command" Null
      srId unknown `shouldBe` 44
      srSuccess unknown `shouldBe` False

    it "returns typed unknown-method service errors without parsing message text" $ withCtx $ \ctx -> do
      result <- runService ctx "no_such_command" Null
      case result of
        Left err -> do
          serviceErrorCode err `shouldBe` "unknown_method"
          serviceErrorKind err `shouldBe` ServiceErrorNotFound
          case serviceErrorDetails err of
            [detail] -> do
              serviceErrorDetailPath detail `shouldBe` ["method"]
              serviceErrorDetailCode detail `shouldBe` "unknown_method"
            details -> expectationFailure ("expected one unknown-method detail, got: " <> show details)
        other -> expectationFailure ("expected unknown-method service error, got: " <> show other)

    it "exposes representative successful operations as direct service handlers" $ withCtx $ \ctx -> do
      stateResult <- runService ctx "get_state" Null
      case stateResult of
        Right (ServiceResponse body) -> do
          lookupKey "seed" body `shouldSatisfy` (/= Nothing)
          lookupKey "view_mode" body `shouldSatisfy` (/= Nothing)
        Left err -> expectationFailure ("expected get_state service success, got: " <> show err)

      seedResult <- runService ctx "set_seed" (object ["seed" .= (987 :: Int)])
      case seedResult of
        Right (ServiceResponse body) -> lookupKey "seed" body `shouldBe` Just (Number 987)
        Left err -> expectationFailure ("expected set_seed service success, got: " <> show err)

    it "classifies handler-originated domain failures as service errors" $ withCtx $ \ctx -> do
      result <- runService ctx "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      case result of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` (not . Text.null)
        other -> expectationFailure ("expected ServiceNotFound, got: " <> show other)

      chunkResult <- runService ctx "get_chunk_summary" (object ["chunk" .= (999 :: Int)])
      case chunkResult of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` Text.isInfixOf "chunk"
        other -> expectationFailure ("expected chunk ServiceNotFound, got: " <> show other)

    it "returns structured plugin parameter update errors from the AppService boundary" $ withPluginCtx $ \ctx -> do
      unknownPlugin <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("missing" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      case unknownPlugin of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` Text.isInfixOf "unknown plugin"
        other -> expectationFailure ("expected plugin ServiceNotFound, got: " <> show other)

      unknownParam <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("missing" :: String)
        , "value" .= Bool True
        ])
      assertSingleValidationDetail unknownParam ["param"] "unknown_param"

      wrongType <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= String "yes"
        ])
      assertSingleValidationDetail wrongType ["value"] "invalid_type"

    it "returns structured service validation errors before handler envelopes" $ withCtx $ \ctx -> do
      result <- runService ctx "set_seed" Null
      case result of
        Left err -> do
          serviceErrorKind err `shouldBe` ServiceErrorInvalidRequest
          serviceErrorCode err `shouldBe` "validation_failed"
          case serviceErrorDetails err of
            [detail] -> do
              serviceErrorDetailPath detail `shouldBe` ["seed"]
              serviceErrorDetailCode detail `shouldBe` "missing_field"
              serviceErrorDetailMessage detail `shouldSatisfy` Text.isInfixOf "seed"
            details -> expectationFailure ("expected one validation detail, got: " <> show details)
        other -> expectationFailure ("expected structured validation error, got: " <> show other)

    it "returns structured validation details for invalid fields and required object bodies" $ withCtx $ \ctx -> do
      negativeSeed <- runService ctx "set_seed" (object ["seed" .= ((-1) :: Int)])
      assertSingleValidationDetail negativeSeed ["seed"] "invalid_field"

      missingEnumType <- runService ctx "get_enums" Null
      assertSingleValidationDetail missingEnumType ["type"] "missing_field"

      brushWithoutObject <- runService ctx "editor_set_brush" Null
      assertSingleValidationDetail brushWithoutObject [] "invalid_body"

    it "validates commandAppService handlers at the service-record boundary" $ withCtx $ \ctx -> do
      result <- runServiceHandler (uiSetSeed (appUi commandAppService)) (serviceContextFromCommand ctx) (ServiceRequest (Just Null))
      assertSingleValidationDetail result ["seed"] "missing_field"

    it "validates adapted typed handlers before caller-supplied decoders" $ withCtx $ \ctx -> do
      let app = commandAppService
            { appUi = (appUi commandAppService)
                { uiSetSeed = adaptTypedServiceHandler
                    uiSetSeedOperation
                    (const (Left (ServiceInvalidRequest "decoder should not run")))
                    (\response -> object ["seed" .= uiSetSeedResponseValue response])
                    (\_ request -> pure (Right (UiSetSeedResponse (uiSetSeedRequestValue request))))
                }
            }
      result <- runAppServiceOperation app ctx "set_seed" Null
      assertSingleValidationDetail result ["seed"] "missing_field"

    it "translates structured service validation errors back to command errors" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" Null
      srSuccess rsp `shouldBe` False
      case srError rsp of
        Just msg -> msg `shouldSatisfy` Text.isInfixOf "seed"
        Nothing -> expectationFailure "expected command error text"

    it "has a direct service-level case for every public operation" $ do
      let coveredMethods = map serviceCaseMethod serviceOperationCases
      sort coveredMethods `shouldBe` sort appServiceOperationMethods
      coveredMethods `shouldBe` nub coveredMethods

    it "executes every public operation through the AppService handler surface" $ withCtx $ \ctx ->
      forM_ serviceOperationCases $ \testCase -> do
        serviceCaseSetup testCase ctx
        result <- runService ctx (serviceCaseMethod testCase) (serviceCaseParams testCase)
        assertServiceOutcome testCase result

  -- -------------------------------------------------------------------
  -- get_state
  -- -------------------------------------------------------------------
  describe "get_state" $ do
    it "returns success with state fields" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_state" Null
      srSuccess rsp `shouldBe` True
      -- result should contain at least "seed" and "view_mode"
      case srResult rsp of
        Object o -> do
          KM.member "seed"      o `shouldBe` True
          KM.member "view_mode" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_view_modes
  -- -------------------------------------------------------------------
  describe "get_view_modes" $ do
    it "returns a non-empty list of view modes" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _                -> expectationFailure "expected view_modes array"

    it "marks exactly one view mode as active" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) ->
          let actives = filter isActive (toList arr)
          in length actives `shouldBe` 1
        _ -> expectationFailure "expected view_modes array"

    it "exposes weather as temperature metadata" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) ->
          case findViewMode "weather" (toList arr) of
            Just weather -> do
              lookupKey "label" weather `shouldBe` Just (String "Weather Temp")
              lookupKey "description" weather `shouldBe`
                Just (String "Current simulated weather temperature with humidity, wind, pressure, and precipitation context; use the Cloud view for cloud cover and storm cells.")
            Nothing -> expectationFailure "missing weather view mode summary"
        _ -> expectationFailure "expected view_modes array"

  -- -------------------------------------------------------------------
  -- get_sliders
  -- -------------------------------------------------------------------
  describe "get_sliders" $ do
    it "returns a non-empty list of sliders" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_sliders" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "sliders" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _                -> expectationFailure "expected sliders array"

    it "filters sliders by tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_sliders" (object ["tab" .= ("terrain" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "sliders" (srResult rsp) of
        Just (Array arr) -> do
          length arr `shouldSatisfy` (> 0)
          -- every slider should have tab == "terrain"
          mapM_ (\s -> lookupKey "tab" s `shouldBe` Just (String "terrain")) (toList arr)
        _ -> expectationFailure "expected sliders array"

  -- -------------------------------------------------------------------
  -- get_slider
  -- -------------------------------------------------------------------
  describe "get_slider" $ do
    it "returns a known slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" (object ["name" .= ("SliderGenScale" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "name" (srResult rsp) `shouldBe` Just (String "SliderGenScale")

    it "returns error for unknown slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_slider
  -- -------------------------------------------------------------------
  describe "set_slider" $ do
    it "sets a slider and returns the clamped value" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (0.42 :: Double)])
      srSuccess rsp `shouldBe` True
      lookupKey "name" (srResult rsp) `shouldBe` Just (String "SliderGenScale")

    it "clamps values above 1" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (5.0 :: Double)])
      srSuccess rsp `shouldBe` True
      case lookupKey "value" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (<= 1.0)
        _               -> expectationFailure "expected numeric value"

    it "clamps values below 0" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= ((-2.0) :: Double)])
      srSuccess rsp `shouldBe` True
      case lookupKey "value" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (>= 0.0)
        _               -> expectationFailure "expected numeric value"

    it "returns error for unknown slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("NoSuchSlider" :: String), "value" .= (0.5 :: Double)])
      srSuccess rsp `shouldBe` False

    it "returns error when params are missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_seed
  -- -------------------------------------------------------------------
  describe "set_seed" $ do
    it "sets the seed and returns it" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" (object ["seed" .= (12345 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "seed" (srResult rsp) `shouldBe` Just (Number 12345)

    it "returns error when seed is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_view_mode
  -- -------------------------------------------------------------------
  describe "set_view_mode" $ do
    it "switches to a valid view mode" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("biome" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "view_mode" (srResult rsp) `shouldBe` Just (String "biome")

    it "returns error for unknown view mode" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("nonexistent" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when mode is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_config_tab
  -- -------------------------------------------------------------------
  describe "set_config_tab" $ do
    it "switches to a valid config tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" (object ["tab" .= ("climate" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "config_tab" (srResult rsp) `shouldBe` Just (String "climate")

    it "returns error for unknown config tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" (object ["tab" .= ("nonexistent" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when tab is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- generate
  -- -------------------------------------------------------------------
  describe "generate" $ do
    it "returns success with generating status" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "generate" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "generating")

  -- -------------------------------------------------------------------
  -- terrain editor
  -- -------------------------------------------------------------------
  describe "editor_get_state" $ do
    it "returns the default editor state" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_get_state" Null
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool False)
      lookupKey "tool" (srResult rsp) `shouldBe` Just (String "raise")

  describe "editor_toggle" $ do
    it "toggles editor activity when active is omitted" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_toggle" (object [])
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool True)

    it "sets editor activity explicitly when active is provided" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_toggle" (object ["active" .= True])
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool True)

  describe "editor_set_tool" $ do
    it "switches to a valid tool" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_tool" (object ["tool" .= ("erode" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "tool" (srResult rsp) `shouldBe` Just (String "erode")

    it "returns error for an unknown tool" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_tool" (object ["tool" .= ("bogus" :: String)])
      srSuccess rsp `shouldBe` False

  describe "editor_set_brush" $ do
    it "updates brush parameters and clamps tool-specific values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_brush" (object
        [ "radius" .= (3 :: Int)
        , "strength" .= (0.4 :: Double)
        , "falloff" .= ("smooth" :: String)
        , "smooth_passes" .= (99 :: Int)
        , "noise_frequency" .= (9.0 :: Double)
        , "erode_passes" .= (0 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "brush" (srResult rsp) of
        Just brushVal -> do
          lookupKey "radius" brushVal `shouldBe` Just (Number 3)
          lookupKey "falloff" brushVal `shouldBe` Just (String "smooth")
        Nothing -> expectationFailure "expected brush object"
      lookupKey "smooth_passes" (srResult rsp) `shouldBe` Just (Number 5)
      lookupKey "erode_passes" (srResult rsp) `shouldBe` Just (Number 1)

  describe "editor_set_biome" $ do
    it "accepts biome display names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_biome" (object ["biome" .= ("Forest" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "biome" (srResult rsp) of
        Just biomeVal -> lookupKey "name" biomeVal `shouldBe` Just (String "Forest")
        Nothing -> expectationFailure "expected biome object"

  describe "editor_set_form" $ do
    it "accepts terrain form display names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_form" (object ["form" .= ("Hilly" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "terrain_form" (srResult rsp) of
        Just formVal -> lookupKey "name" formVal `shouldBe` Just (String "Hilly")
        Nothing -> expectationFailure "expected terrain_form object"

  describe "editor_set_hardness" $ do
    it "clamps hardness target into range" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_hardness" (object ["hardness" .= (9.5 :: Double)])
      srSuccess rsp `shouldBe` True
      lookupKey "hardness_target" (srResult rsp) `shouldBe` Just (Number 1)

  describe "editor_brush_stroke" $ do
    it "queues a single brush stroke" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_brush_stroke" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")
      lookupKey "strokes_queued" (srResult rsp) `shouldBe` Just (Number 1)

    it "preserves untouched terrain chunks across brush, undo, and redo" $ withCtx $ \ctx -> do
      let dataHandle = ahDataHandle (ccActorHandles ctx)
          cfg = WorldConfig { wcChunkSize = 64 }
          farChunkId = chunkIdFromCoord (ChunkCoord 4 4)
          initialChunks =
            [ (chunkIdFromCoord (ChunkCoord 0 0), emptyTerrainChunk cfg)
            , (chunkIdFromCoord (ChunkCoord (-1) 0), emptyTerrainChunk cfg)
            , (chunkIdFromCoord (ChunkCoord 0 (-1)), emptyTerrainChunk cfg)
            , (farChunkId, emptyTerrainChunk cfg)
            ]
          request action = UiActionRequest
            { uarAction = action
            , uarActorHandles = ccActorHandles ctx
            , uarTerrainReplyTo = replyTo @TerrainReplyOps (ccUiActionsHandle ctx)
            }
          expectedChunkCount = length initialChunks
          ChunkId farChunkKey = farChunkId

      setTerrainChunkData dataHandle (wcChunkSize cfg) initialChunks

      runUiAction (request (UiActionBrushStroke (0, 0)))
      afterBrush <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterBrush) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterBrush) `shouldBe` True

      runUiAction (request UiActionUndo)
      afterUndo <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterUndo) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterUndo) `shouldBe` True

      runUiAction (request UiActionRedo)
      afterRedo <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterRedo) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterRedo) `shouldBe` True

  describe "editor_brush_line" $ do
    it "queues multiple brush strokes along a line" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_brush_line" (object
        [ "from_q" .= (0 :: Int)
        , "from_r" .= (0 :: Int)
        , "to_q" .= (3 :: Int)
        , "to_r" .= ((-2) :: Int)
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "strokes_queued" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (> 1)
        _ -> expectationFailure "expected strokes_queued number"

  describe "editor_undo" $ do
    it "queues undo" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_undo" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")

  describe "editor_redo" $ do
    it "queues redo" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_redo" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")

  -- -------------------------------------------------------------------
  -- get_enums
  -- -------------------------------------------------------------------
  describe "get_enums" $ do
    it "returns biome enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("biome" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 50)  -- 65 biomes
        _ -> expectationFailure "expected values array"

    it "returns terrain_form enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("terrain_form" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 15
        _ -> expectationFailure "expected values array"

    it "returns water_body_type enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("water_body_type" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 4
        _ -> expectationFailure "expected values array"

    it "returns view_mode enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("view_mode" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 16
        _ -> expectationFailure "expected values array"

    it "returns config_tab enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("config_tab" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 8
        _ -> expectationFailure "expected values array"

    it "returns slider_tab enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("slider_tab" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 6
        _ -> expectationFailure "expected values array"

    it "returns error for unknown enum type" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("nonsense" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when type is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- get_world_meta
  -- -------------------------------------------------------------------
  describe "get_world_meta" $ do
    it "returns success with world metadata" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_world_meta" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> do
          KM.member "seed"       o `shouldBe` True
          KM.member "chunk_size" o `shouldBe` True
          KM.member "chunk_count" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_generation_status
  -- -------------------------------------------------------------------
  describe "get_generation_status" $ do
    it "returns success with generation status" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_generation_status" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> KM.member "generating" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_overlays
  -- -------------------------------------------------------------------
  describe "get_overlays" $ do
    it "returns success with overlay info" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_overlays" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> do
          KM.member "overlay_count" o `shouldBe` True
          KM.member "overlay_names" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_chunks (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_chunks" $ do
    it "returns success with empty chunk list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunks" Null
      srSuccess rsp `shouldBe` True
      lookupKey "chunk_count" (srResult rsp) `shouldBe` Just (Number 0)

  -- -------------------------------------------------------------------
  -- get_chunk_summary (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_chunk_summary" $ do
    it "returns error for non-existent chunk" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunk_summary" (object ["chunk" .= (999 :: Int)])
      srSuccess rsp `shouldBe` False

    it "returns error when chunk param is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunk_summary" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- get_hex (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_hex" $ do
    it "returns error for non-existent chunk" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` False

    it "returns active view metadata and values for populated terrain" $ withCtx $ \ctx -> do
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "elevation")
          KM.member "tooltip_fields" active `shouldBe` True
          KM.member "inspector_fields" active `shouldBe` True
          KM.member "export_fields" active `shouldBe` True
          case KM.lookup "values" active of
            Just (Object values) -> KM.member "elevation_m" values `shouldBe` True
            _ -> expectationFailure "expected active_view.values object"
        _ -> expectationFailure "expected active_view object"
      case lookupKey "sections" (srResult rsp) of
        Just (Array sections) -> mapMaybe inspectorSectionKey (toList sections) `shouldBe` canonicalInspectorSectionKeys
        _ -> expectationFailure "expected complete inspector sections array"

    it "returns weather temperature label in active view metadata" $ withCtx $ \ctx -> do
      viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("weather" :: Text)])
      srSuccess viewRsp `shouldBe` True
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "weather")
          KM.lookup "label" active `shouldBe` Just (String "Weather Temp")
        _ -> expectationFailure "expected active_view object"

    it "returns error when params are missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_hex" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- export_terrain_data (populated terrain)
  -- -------------------------------------------------------------------
  describe "export_terrain_data" $ do
    it "returns registry export field metadata" $ withCtx $ \ctx -> do
      chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "export_terrain_data" (object
        [ "chunks" .= [chunkKey]
        , "fields" .= ["plate_boundary_code" :: Text]
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "available_fields" (srResult rsp) of
        Just (Array fields) -> toList fields `shouldSatisfy` elem (String "plate_boundary_code")
        _ -> expectationFailure "expected available_fields array"

  -- -------------------------------------------------------------------
  -- get_terrain_stats (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_terrain_stats" $ do
    it "returns success with no-data status for empty terrain" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_terrain_stats" Null
      srSuccess rsp `shouldBe` True
      lookupKey "chunk_count" (srResult rsp) `shouldBe` Just (Number 0)

  -- -------------------------------------------------------------------
  -- list_worlds
  -- -------------------------------------------------------------------
  describe "list_worlds" $ do
    it "returns success with world list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "list_worlds" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> KM.member "world_count" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- set_sliders (batch)
  -- -------------------------------------------------------------------
  describe "set_sliders" $ do
    it "sets multiple sliders and returns updated list" $ withCtx $ \ctx -> do
      let args = object ["values" .= object
                    [ "SliderGenScale" .= (0.3 :: Double)
                    , "SliderGenCoordScale" .= (0.7 :: Double)
                    ]]
      rsp <- dispatch ctx "set_sliders" args
      srSuccess rsp `shouldBe` True
      case lookupKey "updated" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 2
        _ -> expectationFailure "expected updated array"

    it "reports unknown sliders" $ withCtx $ \ctx -> do
      let args = object ["values" .= object
                    [ "SliderGenScale" .= (0.3 :: Double)
                    , "FakeSlider" .= (0.5 :: Double)
                    ]]
      rsp <- dispatch ctx "set_sliders" args
      srSuccess rsp `shouldBe` True
      case lookupKey "unknown" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 1
        _ -> expectationFailure "expected unknown array"

    it "returns error when values param is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_sliders" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- reset_sliders
  -- -------------------------------------------------------------------
  describe "reset_sliders" $ do
    it "resets all sliders" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "reset_sliders" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "reset_count" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected reset_count number"

    it "resets only a specific tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "reset_sliders" (object ["tab" .= ("terrain" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "tab" (srResult rsp) `shouldBe` Just (String "terrain")

  -- -------------------------------------------------------------------
  -- pipeline and plugin mutations
  -- -------------------------------------------------------------------
  describe "get_pipeline" $ do
    it "exposes registry docs, dependency DAG, and stage diagnostics" $ withCtx $ \ctx -> do
      _ <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      rsp <- dispatch ctx "get_pipeline" Null
      srSuccess rsp `shouldBe` True
      lookupKey "dag" (srResult rsp) `shouldSatisfy` maybe False valueHasNodesAndEdges
      lookupKey "docs" (srResult rsp) `shouldSatisfy` maybe False nonEmptyArray
      case lookupKey "stages" (srResult rsp) of
        Just (Array stages) -> do
          length stages `shouldSatisfy` (> 0)
          let stageValues = toList stages
          findStage "climate" stageValues `shouldSatisfy` maybe False (stageHasStatus "disabled")
          findStage "ocean-currents" stageValues `shouldSatisfy` maybe False (stageHasStatus "auto-disabled")
          findStage "weather" stageValues `shouldSatisfy` maybe False (valueHasKey "last_run")
        _ -> expectationFailure "expected stages array"

  describe "set_stage_enabled" $ do
    it "applies the built-in dependency closure used by UI stage toggles" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      Set.member StageClimate (uiDisabledStages ui) `shouldBe` True
      Set.member StageOceanCurrents (uiDisabledStages ui) `shouldBe` True
      Set.member StageWeather (uiDisabledStages ui) `shouldBe` True

    it "preserves overlapping explicit disables across later toggles" $ withCtx $ \ctx -> do
      climateOff <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      srSuccess climateOff `shouldBe` True
      biomesOff <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("biomes" :: String), "enabled" .= False])
      srSuccess biomesOff `shouldBe` True
      climateOn <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= True])
      srSuccess climateOn `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      Set.member StageClimate (uiDisabledStages ui) `shouldBe` False
      Set.member StageBiomes (uiDisabledStages ui) `shouldBe` True
      Set.member StageBiomes (uiExplicitDisabledStages ui) `shouldBe` True

  describe "set_plugin_param" $ do
    it "updates the UI plugin parameter snapshot as well as the plugin manager" $ withPluginCtx $ \ctx -> do
      rsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)

    it "rejects invalid updates without mutating the UI snapshot or plugin manager" $ withPluginCtx $ \ctx -> do
      seedRsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess seedRsp `shouldBe` True

      let invalidCases =
            [ object ["plugin" .= ("missing" :: String), "param" .= ("enabled" :: String), "value" .= Bool False]
            , object ["plugin" .= ("example" :: String), "param" .= ("missing" :: String), "value" .= Bool False]
            , object ["plugin" .= ("example" :: String), "param" .= ("enabled" :: String), "value" .= String "yes"]
            , object ["plugin" .= ("example" :: String), "param" .= ("iterations" :: String), "value" .= Number 1.5]
            , object ["plugin" .= ("example" :: String), "param" .= ("density" :: String), "value" .= Number 2]
            ]
      forM_ invalidCases $ \payload -> do
        rsp <- dispatch ctx "set_plugin_param" payload
        srSuccess rsp `shouldBe` False

      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)
      loaded <- getLoadedPlugins (ahPluginManagerHandle (ccActorHandles ctx))
      let managerValue = do
            plugin <- findPlugin "example" loaded
            Map.lookup "enabled" (lpParams plugin)
      managerValue `shouldBe` Just (Bool True)

    it "click_widget toggles plugin parameter checkboxes through pipeline action helpers" $ withPluginCtx $ \ctx -> do
      seedRsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess seedRsp `shouldBe` True
      seedUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams seedUi) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetPluginParamCheck:example:enabled" :: Text) ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool False)

  describe "set_sim_auto_tick" $ do
    it "sets and clamps the normalized auto tick rate" $ withCtx $ \ctx -> do
      highRsp <- dispatch ctx "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (5.0 :: Double)])
      srSuccess highRsp `shouldBe` True
      lookupKey "auto_tick" (srResult highRsp) `shouldBe` Just (Bool True)
      lookupKey "rate" (srResult highRsp) `shouldBe` Just (Number 1)
      highUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      uiSimAutoTick highUi `shouldBe` True
      uiSimTickRate highUi `shouldBe` 1

      zeroRsp <- dispatch ctx "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (0.0 :: Double)])
      srSuccess zeroRsp `shouldBe` True
      lookupKey "auto_tick" (srResult zeroRsp) `shouldBe` Just (Bool True)
      lookupKey "rate" (srResult zeroRsp) `shouldBe` Just (Number 0)
      zeroUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      uiSimAutoTick zeroUi `shouldBe` True
      uiSimTickRate zeroUi `shouldBe` 0

  describe "sim_tick" $ do
    it "rejects ticks when no world terrain is loaded" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "sim_tick" Null
      srSuccess rsp `shouldBe` False

    it "rejects ticks while world generation is in progress" $ withCtx $ \ctx -> do
      let handles = ccActorHandles ctx
      setTerrainChunkCount (ahDataHandle handles) 1
      setUiGenerating (ahUiHandle handles) True
      _ <- getUiSnapshot (ahUiHandle handles)
      rsp <- dispatch ctx "sim_tick" (object ["count" .= (1 :: Int)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "generation")

    it "requests a tick when world terrain is loaded" $ withCtx $ \ctx -> do
      setTerrainChunkCount (ahDataHandle (ccActorHandles ctx)) 1
      rsp <- dispatch ctx "sim_tick" (object ["count" .= (1 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "target_tick" (srResult rsp) `shouldBe` Just (Number 1)

  -- -------------------------------------------------------------------
  -- select_hex
  -- -------------------------------------------------------------------
  describe "select_hex" $ do
    it "selects a hex by q and r" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" (object ["q" .= (0 :: Int), "r" .= (5 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool True)
      lookupKey "q" (srResult rsp) `shouldBe` Just (Number 0)
      lookupKey "r" (srResult rsp) `shouldBe` Just (Number 5)

    it "deselects when no chunk/tile given" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" Null
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool False)

    it "deselects with empty params" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" (object [])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool False)

  -- -------------------------------------------------------------------
  -- list_presets
  -- -------------------------------------------------------------------
  describe "list_presets" $ do
    it "returns success with preset list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "list_presets" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> KM.member "preset_count" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- save_preset / load_preset
  -- -------------------------------------------------------------------
  describe "save_preset" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" Null
      srSuccess rsp `shouldBe` False

    it "returns error for empty name" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" (object ["name" .= ("" :: String)])
      srSuccess rsp `shouldBe` False

  describe "load_preset" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_preset" Null
      srSuccess rsp `shouldBe` False

    it "returns error for non-existent preset" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_preset" (object ["name" .= ("__nonexistent_test_preset__" :: String)])
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- save_world / load_world
  -- -------------------------------------------------------------------
  describe "save_world" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_world" Null
      srSuccess rsp `shouldBe` False

    it "returns error for empty name" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_world" (object ["name" .= ("" :: String)])
      srSuccess rsp `shouldBe` False

  describe "load_world" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_world" Null
      srSuccess rsp `shouldBe` False

    it "returns error for non-existent world" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_world" (object ["name" .= ("__nonexistent_test_world__" :: String)])
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- take_screenshot
  -- -------------------------------------------------------------------
  describe "take_screenshot" $ do
    it "returns error when a screenshot is already in progress" $ withCtx $ \ctx -> do
      busy <- newEmptyMVar
      writeIORef (ccScreenshotRef ctx) (Just (ScreenshotRequest busy))
      rsp <- dispatch ctx "take_screenshot" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- Unknown command
  -- -------------------------------------------------------------------
  describe "unknown command" $ do
    it "returns an error" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "does_not_exist" Null
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` \case
        Just msg -> Text.isInfixOf "unknown command" msg
        Nothing  -> False

-- =====================================================================
-- Test helpers
-- =====================================================================

-- | Bracket that creates a full actor system with all handles needed
-- for a 'CommandContext'.
withCtx :: (CommandContext -> IO a) -> IO a
withCtx action = bracket newActorSystem shutdownActorSystem $ \system -> do
  uiH        <- get @Ui system
  logH       <- get @Log system
  dataH      <- get @Data system
  terrainH   <- get @Terrain system
  atlasH     <- get @AtlasManager system
  pluginH    <- get @PluginManager system
  simH       <- get @Simulation system
  uiActionsH <- get @UiActions system
  dataSnapRef    <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore)
  versionRef     <- newSnapshotVersionRef
  uiSnapRef      <- newUiSnapshotRef
  screenshotRef  <- newScreenshotRequestRef
  historyRef     <- newIORef (emptyHistory 50)
  setUiSnapshotRef uiH uiSnapRef
  let handles = ActorHandles
        { ahUiHandle              = uiH
        , ahLogHandle             = logH
        , ahDataHandle            = dataH
        , ahTerrainHandle         = terrainH
        , ahAtlasManagerHandle    = atlasH
        , ahDataSnapshotRef       = dataSnapRef
        , ahTerrainSnapshotRef    = terrainSnapRef
        , ahSnapshotVersionRef    = versionRef
        , ahPluginManagerHandle   = pluginH
        , ahSimulationHandle      = simH
        , ahHistoryRef            = historyRef
        }
      ctx = CommandContext
        { ccActorHandles    = handles
        , ccUiSnapshotRef   = uiSnapRef
        , ccUiActionsHandle = uiActionsH
        , ccScreenshotRef   = screenshotRef
        , ccLogSnapshotRef  = Nothing
        }
  action ctx

withPluginCtx :: (CommandContext -> IO a) -> IO a
withPluginCtx action = withIsolatedPluginDir $ \_pluginRoot ->
  withCtx $ \ctx -> do
    discoverPlugins (ahPluginManagerHandle (ccActorHandles ctx))
    _ <- getLoadedPlugins (ahPluginManagerHandle (ccActorHandles ctx))
    setUiPluginParamSpecs (ahUiHandle (ccActorHandles ctx)) (Map.singleton "example" commandPluginParamSpecs)
    action ctx

testPluginDirEnv :: String
testPluginDirEnv = "TOPO_PLUGIN_DIR"

withIsolatedPluginDir :: (FilePath -> IO a) -> IO a
withIsolatedPluginDir action = bracket setup teardown (action . fst)
  where
    setup = do
      oldPluginDir <- lookupEnv testPluginDirEnv
      tmp <- getTemporaryDirectory
      let root = tmp </> "topo-command-dispatch-plugin"
          pluginBase = root </> "plugins"
          pluginDir = pluginBase </> "example"
      removePathForcibly root `catchAny` \_ -> pure ()
      createDirectoryIfMissing True pluginDir
      BL.writeFile (pluginDir </> "manifest.json") (Aeson.encode commandPluginManifest)
      setEnv testPluginDirEnv pluginBase
      pure (root, oldPluginDir)

    teardown (root, oldPluginDir) = do
      maybe (unsetEnv testPluginDirEnv) (setEnv testPluginDirEnv) oldPluginDir
      removePathForcibly root `catchAny` \_ -> pure ()

catchAny :: IO a -> (IOError -> IO a) -> IO a
catchAny = catch

findPlugin :: Text -> [LoadedPlugin] -> Maybe LoadedPlugin
findPlugin name = go
  where
    go [] = Nothing
    go (plugin:rest)
      | lpName plugin == name = Just plugin
      | otherwise = go rest

commandPluginParamSpecs :: [RPCParamSpec]
commandPluginParamSpecs =
  [ RPCParamSpec
      { rpsName = "enabled"
      , rpsLabel = "Enabled"
      , rpsType = ParamBool
      , rpsRange = Nothing
      , rpsDefault = Bool False
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "density"
      , rpsLabel = "Density"
      , rpsType = ParamFloat
      , rpsRange = Just (Number 0, Number 1)
      , rpsDefault = Number 0.5
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "iterations"
      , rpsLabel = "Iterations"
      , rpsType = ParamInt
      , rpsRange = Just (Number 1, Number 10)
      , rpsDefault = Number 3
      , rpsTooltip = ""
      }
  ]

commandPluginManifest :: Value
commandPluginManifest = object
  [ "manifestVersion" .= manifestV3
  , "name" .= ("example" :: Text)
  , "version" .= ("1.0.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object
          [ "min" .= currentProtocolVersion
          , "max" .= currentProtocolVersion
          ]
      ]
  , "generator" .= object ["insertAfter" .= ("biomes" :: Text)]
  , "config" .= object ["parameters" .= commandPluginParamSpecs]
  ]

inspectorSectionKey :: Value -> Maybe Text
inspectorSectionKey (Object section) = case KM.lookup "key" section of
  Just (String key) -> Just key
  _ -> Nothing
inspectorSectionKey _ = Nothing

canonicalInspectorSectionKeys :: [Text]
canonicalInspectorSectionKeys =
  [ "coordinates"
  , "elevation_hypsometry"
  , "tectonics_plates"
  , "erosion_terrain_form"
  , "hydrology_rivers"
  , "water_bodies"
  , "water_table"
  , "climate_weather"
  , "weather_snapshot"
  , "biome_refinement"
  , "soil"
  , "vegetation"
  , "glacier_snow_ice"
  , "volcanism"
  , "ocean_currents"
  , "overlay_records"
  , "overlay_schema"
  , "overlay_provenance"
  , "plugin_hex_data"
  , "stage_provenance"
  , "unit_conversions"
  , "export_links"
  ]

writeSingleChunkTerrain :: CommandContext -> IO Int
writeSingleChunkTerrain ctx = do
  let cfg = WorldConfig { wcChunkSize = 64 }
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
      ChunkId chunkKey = chunkId
      snap = TerrainSnapshot
        1
        0
        0
        0
        0
        (wcChunkSize cfg)
        (IntMap.singleton chunkKey (emptyTerrainChunk cfg))
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        emptyOverlayStore
  writeTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx)) snap
  pure chunkKey

-- | Convenience: dispatch a command with a given method and params,
-- using request id 1.
dispatch :: CommandContext -> Text -> Value -> IO SeerResponse
dispatch ctx method params = dispatchCommand ctx SeerCommand
  { scId     = 1
  , scMethod = method
  , scParams = params
  }

dispatchWithId :: CommandContext -> Int -> Text -> Value -> IO SeerResponse
dispatchWithId ctx reqId method params = dispatchCommand ctx SeerCommand
  { scId     = reqId
  , scMethod = method
  , scParams = params
  }

runService :: CommandContext -> Text -> Value -> IO ServiceResult
runService ctx = runAppServiceOperation serviceHarnessApp ctx

serviceHarnessApp :: AppService
serviceHarnessApp = commandAppService
  { appConfig = (appConfig commandAppService)
      { configListPresets = adaptTypedServiceHandler
          configListPresetsOperation
          (const (Right ConfigListPresetsRequest))
          (\response -> object
              [ "preset_count" .= configPresetCount response
              , "presets" .= configPresetNames response
              ])
          (\_ ConfigListPresetsRequest -> pure $ Right ConfigListPresetsResponse
            { configPresetCount = 0
            , configPresetNames = []
            })
      }
  , appWorld = (appWorld commandAppService)
      { worldGenerate = adaptTypedServiceHandler
          worldGenerateOperation
          (const (Right (WorldGenerateRequest Nothing)))
          (\response -> object
              [ "accepted" .= worldGenerateAccepted response
              , "status" .= worldGenerateStatus response
              ])
          (\_ _ -> pure $ Right WorldGenerateResponse
            { worldGenerateAccepted = True
            , worldGenerateStatus = "generating"
            })
      , worldList = adaptTypedServiceHandler
          worldListOperation
          (const (Right WorldListRequest))
          (\response -> object
              [ "world_count" .= worldListCount response
              , "worlds" .= ([] :: [Value])
              ])
          (\_ WorldListRequest -> pure $ Right WorldListResponse
            { worldListCount = 0
            , worldListWorlds = []
            })
      }
  }

serviceContextFromCommand :: CommandContext -> ServiceContext
serviceContextFromCommand ctx = ServiceContext
  { svcActorHandles = ccActorHandles ctx
  , svcUiSnapshotRef = ccUiSnapshotRef ctx
  , svcUiActionsHandle = ccUiActionsHandle ctx
  , svcScreenshotRef = ccScreenshotRef ctx
  , svcLogSnapshotRef = ccLogSnapshotRef ctx
  , svcEventBus = Nothing
  }

data ExpectedServiceOutcome
  = ExpectServiceSuccess
  | ExpectServiceFailure

data ServiceOperationCase = ServiceOperationCase
  { serviceCaseMethod :: !Text
  , serviceCaseParams :: !Value
  , serviceCaseExpected :: !ExpectedServiceOutcome
  , serviceCaseSetup :: CommandContext -> IO ()
  }

serviceCase :: Text -> Value -> ExpectedServiceOutcome -> ServiceOperationCase
serviceCase method params expected = ServiceOperationCase method params expected (\_ -> pure ())

assertServiceOutcome :: ServiceOperationCase -> ServiceResult -> Expectation
assertServiceOutcome testCase result =
  case (serviceCaseExpected testCase, result) of
    (ExpectServiceSuccess, Right _) -> pure ()
    (ExpectServiceFailure, Left _) -> pure ()
    (ExpectServiceSuccess, Left err) -> expectationFailure $
      Text.unpack (serviceCaseMethod testCase) <> " expected service success, got: " <> show err
    (ExpectServiceFailure, Right (ServiceResponse body)) -> expectationFailure $
      Text.unpack (serviceCaseMethod testCase) <> " expected service failure, got: " <> show body

assertSingleValidationDetail :: ServiceResult -> [Text] -> Text -> Expectation
assertSingleValidationDetail result expectedPath expectedCode =
  case result of
    Left err -> do
      serviceErrorKind err `shouldBe` ServiceErrorInvalidRequest
      serviceErrorCode err `shouldBe` "validation_failed"
      case serviceErrorDetails err of
        [detail] -> do
          serviceErrorDetailPath detail `shouldBe` expectedPath
          serviceErrorDetailCode detail `shouldBe` expectedCode
        details -> expectationFailure ("expected one validation detail, got: " <> show details)
    other -> expectationFailure ("expected structured validation error, got: " <> show other)

serviceOperationCases :: [ServiceOperationCase]
serviceOperationCases =
  [ serviceCase "get_state" Null ExpectServiceSuccess
  , serviceCase "get_view_modes" Null ExpectServiceSuccess
  , serviceCase "get_ui_state" Null ExpectServiceSuccess
  , serviceCase "get_sliders" Null ExpectServiceSuccess
  , serviceCase "get_slider" (object ["name" .= ("SliderGenScale" :: String)]) ExpectServiceSuccess
  , serviceCase "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (0.42 :: Double)]) ExpectServiceSuccess
  , serviceCase "set_sliders" (object ["values" .= object ["SliderGenScale" .= (0.3 :: Double)]]) ExpectServiceSuccess
  , serviceCase "reset_sliders" Null ExpectServiceSuccess
  , serviceCase "get_config_summary" Null ExpectServiceSuccess
  , serviceCase "get_enums" (object ["type" .= ("biome" :: String)]) ExpectServiceSuccess
  , serviceCase "list_presets" Null ExpectServiceSuccess
  , serviceCase "save_preset" Null ExpectServiceFailure
  , serviceCase "load_preset" Null ExpectServiceFailure
  , serviceCase "generate" Null ExpectServiceSuccess
  , serviceCase "get_world_meta" Null ExpectServiceSuccess
  , serviceCase "get_generation_status" Null ExpectServiceSuccess
  , serviceCase "list_worlds" Null ExpectServiceSuccess
  , serviceCase "save_world" Null ExpectServiceFailure
  , serviceCase "load_world" Null ExpectServiceFailure
  , serviceCase "set_world_name" (object ["name" .= ("Service Test World" :: String)]) ExpectServiceSuccess
  , serviceCase "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)]) ExpectServiceFailure
  , serviceCase "get_chunks" Null ExpectServiceSuccess
  , serviceCase "get_chunk_summary" (object ["chunk" .= (999 :: Int)]) ExpectServiceFailure
  , serviceCase "get_terrain_stats" Null ExpectServiceSuccess
  , serviceCase "get_overlays" Null ExpectServiceSuccess
  , serviceCase "find_hexes" (object ["filters" .= ([] :: [Value])]) ExpectServiceSuccess
  , serviceCase "export_terrain_data" Null ExpectServiceFailure
  , serviceCase "get_overlay_schema" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "get_overlay_provenance" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "export_overlay_data" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "validate_overlay_import" Null ExpectServiceSuccess
  , serviceCase "export_mesh_data" Null ExpectServiceFailure
  , serviceCase "export_sample_data" (object ["x" .= (0 :: Double), "y" .= (0 :: Double)]) ExpectServiceFailure
  , serviceCase "editor_toggle" (object []) ExpectServiceSuccess
  , serviceCase "editor_set_tool" (object ["tool" .= ("erode" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_brush" (object
      [ "radius" .= (3 :: Int)
      , "strength" .= (0.4 :: Double)
      , "falloff" .= ("smooth" :: String)
      ]) ExpectServiceSuccess
  , serviceCase "editor_brush_stroke" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "editor_brush_line" (object
      [ "from_q" .= (0 :: Int)
      , "from_r" .= (0 :: Int)
      , "to_q" .= (1 :: Int)
      , "to_r" .= (0 :: Int)
      ]) ExpectServiceSuccess
  , serviceCase "editor_set_biome" (object ["biome" .= ("Forest" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_form" (object ["form" .= ("Hilly" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_hardness" (object ["hardness" .= (0.7 :: Double)]) ExpectServiceSuccess
  , serviceCase "editor_undo" Null ExpectServiceSuccess
  , serviceCase "editor_redo" Null ExpectServiceSuccess
  , serviceCase "editor_get_state" Null ExpectServiceSuccess
  , serviceCase "get_pipeline" Null ExpectServiceSuccess
  , serviceCase "set_stage_enabled" (object ["stage" .= ("__missing_stage__" :: String), "enabled" .= True]) ExpectServiceFailure
  , serviceCase "list_plugins" Null ExpectServiceSuccess
  , serviceCase "set_plugin_enabled" Null ExpectServiceFailure
  , serviceCase "set_plugin_param" Null ExpectServiceFailure
  , serviceCase "data_list_plugins" Null ExpectServiceSuccess
  , serviceCase "data_list_resources" (object ["plugin" .= ("__missing_plugin__" :: String)]) ExpectServiceFailure
  , serviceCase "data_list_records" Null ExpectServiceFailure
  , serviceCase "data_get_record" Null ExpectServiceFailure
  , serviceCase "data_create_record" Null ExpectServiceFailure
  , serviceCase "data_update_record" Null ExpectServiceFailure
  , serviceCase "data_delete_record" Null ExpectServiceFailure
  , serviceCase "data_get_state" Null ExpectServiceSuccess
  , serviceCase "get_sim_state" Null ExpectServiceSuccess
  , serviceCase "set_sim_auto_tick" (object ["enabled" .= False, "rate" .= (2.0 :: Double)]) ExpectServiceSuccess
  , serviceCase "sim_tick" Null ExpectServiceFailure
  , serviceCase "get_sim_dag" Null ExpectServiceSuccess
  , serviceCase "get_logs" Null ExpectServiceFailure
  , ServiceOperationCase "take_screenshot" Null ExpectServiceFailure $ \ctx -> do
      busy <- newEmptyMVar
      writeIORef (ccScreenshotRef ctx) (Just (ScreenshotRequest busy))
  , serviceCase "set_seed" (object ["seed" .= (2468 :: Int)]) ExpectServiceSuccess
  , serviceCase "set_view_mode" (object ["mode" .= ("biome" :: String)]) ExpectServiceSuccess
  , serviceCase "set_config_tab" (object ["tab" .= ("climate" :: String)]) ExpectServiceSuccess
  , serviceCase "select_hex" (object ["q" .= (0 :: Int), "r" .= (5 :: Int)]) ExpectServiceSuccess
  , serviceCase "set_overlay" (object ["overlay" .= ("__missing_overlay__" :: String)]) ExpectServiceFailure
  , serviceCase "list_overlay_fields" Null ExpectServiceFailure
  , serviceCase "cycle_overlay" (object ["direction" .= (1 :: Int)]) ExpectServiceFailure
  , serviceCase "cycle_overlay_field" (object ["direction" .= (1 :: Int)]) ExpectServiceFailure
  , serviceCase "set_camera" (object ["x" .= (1.0 :: Double), "y" .= (2.0 :: Double), "zoom" .= (1.5 :: Double)]) ExpectServiceSuccess
  , serviceCase "get_camera" Null ExpectServiceSuccess
  , serviceCase "zoom_to_chunk" (object ["chunk" .= (999 :: Int)]) ExpectServiceFailure
  , serviceCase "set_left_panel" (object ["visible" .= True]) ExpectServiceSuccess
  , serviceCase "set_left_tab" (object ["tab" .= ("topo" :: String)]) ExpectServiceSuccess
  , serviceCase "toggle_config_panel" Null ExpectServiceSuccess
  , serviceCase "set_log_collapsed" (object ["collapsed" .= True]) ExpectServiceSuccess
  , serviceCase "set_log_level" (object ["level" .= ("debug" :: String)]) ExpectServiceSuccess
  , serviceCase "get_ui_panels" Null ExpectServiceSuccess
  , serviceCase "viewport_scroll" (object ["delta" .= (1 :: Int)]) ExpectServiceSuccess
  , serviceCase "viewport_click" (object ["x" .= (0 :: Int), "y" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "viewport_drag" (object
      [ "x1" .= (0 :: Int)
      , "y1" .= (0 :: Int)
      , "x2" .= (10 :: Int)
      , "y2" .= (10 :: Int)
      ]) ExpectServiceSuccess
  , serviceCase "viewport_hover" (object ["x" .= (0 :: Int), "y" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "click_widget" Null ExpectServiceFailure
  , serviceCase "list_widgets" Null ExpectServiceSuccess
  , serviceCase "get_widget_state" Null ExpectServiceFailure
  , serviceCase "get_dialog_state" Null ExpectServiceSuccess
  , serviceCase "set_dialog_text" (object ["target" .= ("seed" :: String), "text" .= ("123" :: String)]) ExpectServiceSuccess
  , serviceCase "dialog_confirm" Null ExpectServiceSuccess
  , serviceCase "dialog_cancel" Null ExpectServiceSuccess
  , serviceCase "send_key" (object ["key" .= ("escape" :: String)]) ExpectServiceSuccess
  ]

-- | Look up a key in a JSON object.
lookupKey :: Key -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup k o
lookupKey _ _          = Nothing

valueHasKey :: Key -> Value -> Bool
valueHasKey key (Object obj) = KM.member key obj
valueHasKey _ _ = False

nonEmptyArray :: Value -> Bool
nonEmptyArray (Array values) = not (null (toList values))
nonEmptyArray _ = False

valueHasNodesAndEdges :: Value -> Bool
valueHasNodesAndEdges value = valueHasKey "nodes" value && valueHasKey "edges" value

findStage :: Text -> [Value] -> Maybe Value
findStage stageId = go
  where
    go [] = Nothing
    go (value:rest)
      | lookupKey "id" value == Just (String stageId) = Just value
      | otherwise = go rest

findViewMode :: Text -> [Value] -> Maybe Value
findViewMode modeName = go
  where
    go [] = Nothing
    go (value:rest)
      | lookupKey "name" value == Just (String modeName) = Just value
      | otherwise = go rest

stageHasStatus :: Text -> Value -> Bool
stageHasStatus expected value = lookupKey "status" value == Just (String expected)

-- | Check whether a view-mode entry has @"active": true@.
isActive :: Value -> Bool
isActive (Object o) = KM.lookup "active" o == Just (Bool True)
isActive _          = False
