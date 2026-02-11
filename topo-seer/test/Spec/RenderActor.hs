{-# LANGUAGE DataKinds #-}

module Spec.RenderActor (spec) where

import Control.Exception (bracket)
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import Test.Hspec
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.Render
import Actor.UI (ConfigTab(..), UiMenuMode(..), UiState(..), ViewMode(..), emptyUiState)
import qualified Data.Text as Text

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "RenderActor" $ do
  it "stores UI snapshots" $ withSystem $ \system -> do
    handle <- getSingleton system renderActorDef
    let ui = emptyUiState
          { uiSeed = 5
          , uiGenerating = True
          , uiViewMode = ViewBiome
          , uiChunkSize = 64
          , uiShowConfig = True
          , uiConfigTab = ConfigClimate
          , uiMenuMode = MenuNone
          , uiContextHex = Nothing
          , uiContextPos = Nothing
          , uiSeedEditing = False
          , uiSeedInput = Text.empty
          , uiWaterLevel = 0.6
          , uiEvaporation = 0.3
          , uiRainShadow = 0.4
          , uiWindDiffuse = 0.5
          , uiRainRate = 0.2
          , uiEquatorTemp = 1
          , uiPoleTemp = 0
          , uiLapseRate = 0.25
          , uiGenScale = 0.44
          , uiGenFrequency = 0.18
          , uiGenOctaves = 0.5
          , uiGenLacunarity = 0.25
          , uiGenGain = 0.4
          , uiGenWarpScale = 0.33
          , uiGenWarpStrength = 0.55
          , uiHoverHex = Just (0, 0)
          }
    setRenderUi handle ui
    snapshot <- getRenderSnapshot handle
    rsUi snapshot `shouldBe` ui

  it "accepts log and data snapshots" $ withSystem $ \system -> do
    handle <- getSingleton system renderActorDef
    setRenderLog handle (LogSnapshot [] True 0 LogInfo)
    setRenderData handle (DataSnapshot 2 1 (Just 9))
    setRenderTerrain handle (TerrainSnapshot 0 0 mempty mempty mempty)
    snapshot <- getRenderSnapshot handle
    uiSeed (rsUi snapshot) `shouldBe` 0
