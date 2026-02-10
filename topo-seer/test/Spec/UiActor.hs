{-# LANGUAGE DataKinds #-}

module Spec.UiActor (spec) where

import Control.Exception (bracket)
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import Test.Hspec
import Actor.UI

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "UiActor" $ do
  it "updates seed and view mode" $ withSystem $ \system -> do
    handle <- getSingleton system uiActorDef
    setUiSeed handle 99
    setUiViewMode handle ViewBiome
    setUiChunkSize handle 96
    setUiShowConfig handle True
    setUiConfigTab handle ConfigClimate
    setUiWaterLevel handle 0.7
    setUiEvaporation handle 0.4
    setUiRainShadow handle 0.3
    setUiWindDiffuse handle 0.6
    setUiRainRate handle 0.25
    setUiEquatorTemp handle 0.9
    setUiPoleTemp handle 0.2
    setUiLapseRate handle 0.3
    setUiGenScale handle 0.2
    setUiGenFrequency handle 0.1
    setUiGenOctaves handle 0.8
    setUiGenLacunarity handle 0.6
    setUiGenGain handle 0.4
    setUiGenWarpScale handle 0.7
    setUiGenWarpStrength handle 0.5
    setUiHoverHex handle (Just (3, -2))
    snapshot <- getUiSnapshot handle
    uiSeed snapshot `shouldBe` 99
    uiViewMode snapshot `shouldBe` ViewBiome
    uiChunkSize snapshot `shouldBe` 96
    uiShowConfig snapshot `shouldBe` True
    uiConfigTab snapshot `shouldBe` ConfigClimate
    uiWaterLevel snapshot `shouldBe` 0.7
    uiEvaporation snapshot `shouldBe` 0.4
    uiRainShadow snapshot `shouldBe` 0.3
    uiWindDiffuse snapshot `shouldBe` 0.6
    uiRainRate snapshot `shouldBe` 0.25
    uiEquatorTemp snapshot `shouldBe` 0.9
    uiPoleTemp snapshot `shouldBe` 0.2
    uiLapseRate snapshot `shouldBe` 0.3
    uiGenScale snapshot `shouldBe` 0.2
    uiGenFrequency snapshot `shouldBe` 0.1
    uiGenOctaves snapshot `shouldBe` 0.8
    uiGenLacunarity snapshot `shouldBe` 0.6
    uiGenGain snapshot `shouldBe` 0.4
    uiGenWarpScale snapshot `shouldBe` 0.7
    uiGenWarpStrength snapshot `shouldBe` 0.5
    uiHoverHex snapshot `shouldBe` Just (3, -2)

  it "updates generation flag" $ withSystem $ \system -> do
    handle <- getSingleton system uiActorDef
    setUiGenerating handle True
    snapshot <- getUiSnapshot handle
    uiGenerating snapshot `shouldBe` True
