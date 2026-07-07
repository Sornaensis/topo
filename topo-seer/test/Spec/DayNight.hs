module Spec.DayNight (spec) where

import Data.Foldable (forM_)
import Test.Hspec

import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import UI.DayNight (mkDayNightFn, mkDayNightKey, mkDayNightSpec)

spec :: Spec
spec = describe "DayNight" $ do
  describe "DayNightKey" $ do
    let baseUi = emptyUiState
          { uiSimTickCount = 10
          , uiPlanetRadius = 0.5
          , uiAxialTilt = 0.5
          , uiHexSizeKm = 0.5
          , uiSliceLatCenter = 0.5
          , uiSliceLonCenter = 0.5
          }
        baseChunkSize = 16
        baseKey = mkDayNightKey baseUi baseChunkSize

    forM_
      [ ("simulation tick/world time", mkDayNightKey (baseUi { uiSimTickCount = 11 }) baseChunkSize)
      , ("planet radius", mkDayNightKey (baseUi { uiPlanetRadius = 0.75 }) baseChunkSize)
      , ("axial tilt", mkDayNightKey (baseUi { uiAxialTilt = 0.75 }) baseChunkSize)
      , ("hex size", mkDayNightKey (baseUi { uiHexSizeKm = 0.75 }) baseChunkSize)
      , ("slice latitude center", mkDayNightKey (baseUi { uiSliceLatCenter = 0.75 }) baseChunkSize)
      , ("slice longitude center", mkDayNightKey (baseUi { uiSliceLonCenter = 0.75 }) baseChunkSize)
      , ("terrain chunk size", mkDayNightKey baseUi 32)
      ] $ \(label, changedKey) ->
        it ("changes when " <> label <> " changes") $ do
          baseKey `shouldNotBe` Nothing
          changedKey `shouldNotBe` baseKey

    forM_
      [ ("view mode", mkDayNightKey (baseUi { uiViewMode = ViewBiome }) baseChunkSize)
      , ("render water level", mkDayNightKey (baseUi { uiRenderWaterLevel = 0.9 }) baseChunkSize)
      , ("terrain water level", mkDayNightKey (baseUi { uiWaterLevel = 0.9 }) baseChunkSize)
      , ("terrain color slider", mkDayNightKey (baseUi { uiGenScale = 0.1 }) baseChunkSize)
      , ("pan offset", mkDayNightKey (baseUi { uiPanOffset = (123, -45) }) baseChunkSize)
      , ("zoom", mkDayNightKey (baseUi { uiZoom = 2.0 }) baseChunkSize)
      , ("simulation tick rate", mkDayNightKey (baseUi { uiSimTickRate = 1.0 }) baseChunkSize)
      , ("insolation", mkDayNightKey (baseUi { uiInsolation = 1.0 }) baseChunkSize)
      ] $ \(label, unchangedKey) ->
        it ("does not change when only " <> label <> " changes") $
          unchangedKey `shouldBe` baseKey

    it "is unavailable without a positive terrain chunk size" $ do
      mkDayNightKey baseUi 0 `shouldBe` Nothing
      case mkDayNightSpec baseUi 0 of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected no day/night spec for non-positive chunk size"

  it "builds mkDayNightFn from the same spec input path" $ do
    let ui = emptyUiState { uiSimTickCount = 24, uiAxialTilt = 0.6 }
    case (mkDayNightSpec ui 16, mkDayNightFn ui 16) of
      (Just (key, specFn), Just fn) -> do
        mkDayNightKey ui 16 `shouldBe` Just key
        specFn 3 (-1) `shouldBe` fn 3 (-1)
      _ -> expectationFailure "expected day/night spec and function for positive chunk size"
