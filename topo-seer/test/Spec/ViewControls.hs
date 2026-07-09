module Spec.ViewControls (spec) where

import Actor.UI (BaseViewMode(..), SkyOverlayMode(..))
import qualified SDL
import Seer.Input.ViewControls (ViewHotkey(..), viewHotkeyForKey)
import Test.Hspec

spec :: Spec
spec = describe "view control hotkeys" $ do
  it "maps number keys to base terrain views" $ do
    viewHotkeyForKey SDL.Keycode1 `shouldBe` Just (ViewHotkeySetBase BaseViewElevation)
    viewHotkeyForKey SDL.Keycode2 `shouldBe` Just (ViewHotkeySetBase BaseViewBiome)
    viewHotkeyForKey SDL.Keycode3 `shouldBe` Just (ViewHotkeySetBase BaseViewMoisture)
    viewHotkeyForKey SDL.Keycode0 `shouldBe` Just (ViewHotkeySetBase BaseViewPlateAge)
    viewHotkeyForKey SDL.KeycodeH `shouldBe` Just (ViewHotkeySetBase BaseViewPlateHeight)
    viewHotkeyForKey SDL.KeycodeV `shouldBe` Just (ViewHotkeySetBase BaseViewPlateVelocity)

  it "maps overlay and basis keys to layered controls" $ do
    viewHotkeyForKey SDL.KeycodeN `shouldBe` Just (ViewHotkeySetOverlay Nothing)
    viewHotkeyForKey SDL.KeycodeT `shouldBe` Just (ViewHotkeySetOverlay (Just SkyOverlayWeatherTemperature))
    viewHotkeyForKey SDL.KeycodeP `shouldBe` Just (ViewHotkeySetOverlay (Just SkyOverlayPrecipitation))
    viewHotkeyForKey SDL.KeycodeK `shouldBe` Just (ViewHotkeySetOverlay (Just SkyOverlayCloud))
    viewHotkeyForKey SDL.KeycodeO `shouldBe` Just ViewHotkeyCycleOverlay
    viewHotkeyForKey SDL.KeycodeB `shouldBe` Just ViewHotkeyCycleWeatherBasis
