{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.UiActor (spec) where

import Control.Exception (bracket)
import Hyperspace.Actor (ActorSystem, get, newActorSystem, shutdownActorSystem)
import Test.Hspec
import Actor.UI
import Seer.Config.SliderSpec (SliderId(..))
import Topo.Calendar (WorldTime(..), simulationTickSeconds)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "UiActor" $ do
  it "updates seed and view mode" $ withSystem $ \system -> do
    handle <- get @Ui system
    setUiSeed handle 99
    setUiViewMode handle ViewBiome
    setUiChunkSize handle 96
    setUiShowConfig handle True
    setUiConfigTab handle ConfigClimate
    setUiWaterLevel handle 0.7
    setUiRainShadowLoss handle 0.3
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
    uiViewSelection snapshot `shouldBe` viewModeToLayeredViewState ViewBiome
    uiChunkSize snapshot `shouldBe` 96
    uiShowConfig snapshot `shouldBe` True
    uiConfigTab snapshot `shouldBe` ConfigClimate
    uiWaterLevel snapshot `shouldBe` 0.7
    uiRainShadowLoss snapshot `shouldBe` 0.3
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

  it "updates layered view fields and compatibility view mode" $ withSystem $ \system -> do
    handle <- get @Ui system
    setUiBaseViewMode handle BaseViewBiome
    setUiSkyOverlayMode handle (Just SkyOverlayCloud)
    setUiWeatherBasis handle WeatherBasisCurrent
    setUiOverlayOpacity handle 1.4
    snapshot <- getUiSnapshot handle
    uiViewMode snapshot `shouldBe` ViewCloud
    uiViewSelection snapshot `shouldBe` defaultLayeredViewState
      { lvsBaseView = BaseViewBiome
      , lvsSkyOverlay = Just SkyOverlayCloud
      , lvsWeatherBasis = WeatherBasisCurrent
      , lvsOverlayOpacity = 1.0
      }
    setUiSkyOverlayMode handle Nothing
    snapshotNoOverlay <- getUiSnapshot handle
    uiViewMode snapshotNoOverlay `shouldBe` ViewBiome
    uiViewSelection snapshotNoOverlay `shouldBe` defaultLayeredViewState
      { lvsBaseView = BaseViewBiome
      , lvsSkyOverlay = Nothing
      , lvsWeatherBasis = WeatherBasisCurrent
      , lvsOverlayOpacity = 1.0
      }
    setUiViewSelection handle defaultLayeredViewState
      { lvsSkyOverlay = Just (SkyOverlayPlugin "roads" 2)
      , lvsOverlayOpacity = -0.5
      }
    snapshotPlugin <- getUiSnapshot handle
    uiViewMode snapshotPlugin `shouldBe` ViewOverlay "roads" 2
    uiViewSelection snapshotPlugin `shouldBe` defaultLayeredViewState
      { lvsSkyOverlay = Just (SkyOverlayPlugin "roads" 2)
      , lvsOverlayOpacity = 0.0
      }
    setUiBaseViewMode handle BaseViewBiome
    setUiWeatherBasis handle WeatherBasisAverage
    setUiOverlayOpacity handle 0.4
    snapshotComposited <- getUiSnapshot handle
    uiViewSelection snapshotComposited `shouldBe` defaultLayeredViewState
      { lvsBaseView = BaseViewBiome
      , lvsSkyOverlay = Just (SkyOverlayPlugin "roads" 2)
      , lvsWeatherBasis = WeatherBasisAverage
      , lvsOverlayOpacity = 0.4
      }
    uiViewMode snapshotComposited `shouldBe` ViewOverlay "roads" 2

  it "updates generation flag" $ withSystem $ \system -> do
    handle <- get @Ui system
    setUiGenerating handle True
    snapshot <- getUiSnapshot handle
    uiGenerating snapshot `shouldBe` True

  it "updates representative sliders through the generic SliderId path" $ withSystem $ \system -> do
    handle <- get @Ui system
    setUiSliderValue handle SliderGenScale 0.21
    setUiSliderValue handle SliderWeatherTick 0.61
    setUiSliderValue handle SliderVegBase 0.77
    setUiSliderValue handle SliderErosionRainRate 0.33
    snapshot <- getUiSnapshot handle
    uiGenScale snapshot `shouldBe` 0.21
    uiWeatherTick snapshot `shouldBe` 0.61
    uiVegBase snapshot `shouldBe` 0.77
    uiRainRate snapshot `shouldBe` 0.33

  it "clamps generic slider updates through the shared SliderId write path" $ withSystem $ \system -> do
    handle <- get @Ui system
    setUiSliderValue handle SliderGenScale (-0.25)
    setUiSliderValue handle SliderWeatherTick 1.4
    setUiSliderValue handle SliderVegBase 2.0
    snapshot <- getUiSnapshot handle
    uiGenScale snapshot `shouldBe` 0.0
    uiWeatherTick snapshot `shouldBe` 1.0
    uiVegBase snapshot `shouldBe` 1.0

  it "builds canonical world time independently of UI auto rate" $ do
    let slow = emptyUiState { uiSimTickCount = 7, uiSimTickRate = 0.0 }
        fast = emptyUiState { uiSimTickCount = 7, uiSimTickRate = 1.0 }
        wt = uiWorldTime slow
    uiWorldTime fast `shouldBe` wt
    wtTick wt `shouldBe` 7
    wtTickRate wt `shouldBe` simulationTickSeconds
