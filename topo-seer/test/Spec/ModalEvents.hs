module Spec.ModalEvents (spec) where

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , UiMenuMode(..)
  , UiState(..)
  , emptyUiState
  )
import qualified Data.Text as Text
import qualified SDL
import Seer.Input.Events
  ( ModalEventRoute(..)
  , modalEventRoute
  , modalInputBarrierVisible
  )
import Test.Hspec

spec :: Spec
spec = describe "SDL modal event barriers" $ do
  it "barriers pointer motion, buttons, and background wheel input for every menu dialog" $ do
    let menus =
          [ MenuEscape
          , MenuPresetSave
          , MenuPresetLoad
          , MenuWorldSave
          , MenuWorldLoad
          ]
    mapM_ (assertStandardModalRoutes . menuUi) menus

  it "keeps overlay inspector wheel input local while barriering its other pointer events" $ do
    let ui = menuUi MenuOverlayInspector
    modalEventRoute False ui mouseMotionPayload `shouldBe` BarrierPointerMotion
    modalEventRoute False ui mouseButtonPayload `shouldBe` BarrierPointerButton
    modalEventRoute False ui mouseWheelPayload `shouldBe` OverlayInspectorWheel

  it "applies the same event barrier to the data-record delete confirmation" $ do
    let browser = (uiDataBrowser emptyUiState) { dbsDeleteConfirm = True }
        ui = emptyUiState
          { uiShowConfig = True
          , uiConfigTab = ConfigData
          , uiDataBrowser = browser
          }
    modalInputBarrierVisible ui `shouldBe` True
    assertStandardModalRoutes ui
    modalEventRoute False ui keyboardPayload `shouldBe` BlockEvent
    modalEventRoute False ui textInputPayload `shouldBe` BlockEvent
    modalEventRoute False ui escapeKeyboardPayload `shouldBe` PassEvent

  it "passes modal-local keyboard and text input without releasing pointer events" $ do
    let ui = menuUi MenuWorldLoad
    modalEventRoute False ui keyboardPayload `shouldBe` PassEvent
    modalEventRoute False ui textInputPayload `shouldBe` PassEvent
    modalEventRoute False ui mouseMotionPayload `shouldBe` BarrierPointerMotion

  it "blocks background hotkeys while retaining the escape menu's local close key" $ do
    let ui = menuUi MenuEscape
    modalEventRoute False ui keyboardPayload `shouldBe` BlockEvent
    modalEventRoute False ui escapeKeyboardPayload `shouldBe` PassEvent
    modalEventRoute False ui textInputPayload `shouldBe` BlockEvent

  it "keeps a rendered modal latched after live state closes" $ do
    modalEventRoute True emptyUiState mouseMotionPayload `shouldBe` BarrierPointerMotion
    modalEventRoute True emptyUiState mouseButtonPayload `shouldBe` BarrierPointerButton
    modalEventRoute True emptyUiState mouseWheelPayload `shouldBe` BlockEvent
    modalEventRoute True emptyUiState keyboardPayload `shouldBe` BlockEvent
    modalEventRoute True emptyUiState textInputPayload `shouldBe` BlockEvent

  it "passes all routed interaction events when no modal is visible or latched" $ do
    modalInputBarrierVisible emptyUiState `shouldBe` False
    mapM_ (\payload -> modalEventRoute False emptyUiState payload `shouldBe` PassEvent)
      [ mouseMotionPayload
      , mouseButtonPayload
      , mouseWheelPayload
      , keyboardPayload
      , textInputPayload
      ]

assertStandardModalRoutes :: UiState -> Expectation
assertStandardModalRoutes ui = do
  modalInputBarrierVisible ui `shouldBe` True
  modalEventRoute False ui mouseMotionPayload `shouldBe` BarrierPointerMotion
  modalEventRoute False ui mouseButtonPayload `shouldBe` BarrierPointerButton
  modalEventRoute False ui mouseWheelPayload `shouldBe` BlockEvent

menuUi :: UiMenuMode -> UiState
menuUi menu = emptyUiState { uiMenuMode = menu }

mouseMotionPayload :: SDL.EventPayload
mouseMotionPayload = SDL.MouseMotionEvent SDL.MouseMotionEventData
  { SDL.mouseMotionEventWindow = Nothing
  , SDL.mouseMotionEventWhich = SDL.Mouse 0
  , SDL.mouseMotionEventState = []
  , SDL.mouseMotionEventPos = SDL.P (SDL.V2 0 0)
  , SDL.mouseMotionEventRelMotion = SDL.V2 0 0
  }

mouseButtonPayload :: SDL.EventPayload
mouseButtonPayload = SDL.MouseButtonEvent SDL.MouseButtonEventData
  { SDL.mouseButtonEventWindow = Nothing
  , SDL.mouseButtonEventWhich = SDL.Mouse 0
  , SDL.mouseButtonEventButton = SDL.ButtonLeft
  , SDL.mouseButtonEventClicks = 1
  , SDL.mouseButtonEventMotion = SDL.Pressed
  , SDL.mouseButtonEventPos = SDL.P (SDL.V2 0 0)
  }

mouseWheelPayload :: SDL.EventPayload
mouseWheelPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
  { SDL.mouseWheelEventWindow = Nothing
  , SDL.mouseWheelEventWhich = SDL.Mouse 0
  , SDL.mouseWheelEventPos = SDL.V2 0 0
  , SDL.mouseWheelEventDirection = SDL.ScrollNormal
  }

keyboardPayload :: SDL.EventPayload
keyboardPayload = keyboardPayloadFor SDL.ScancodeA SDL.KeycodeA

escapeKeyboardPayload :: SDL.EventPayload
escapeKeyboardPayload = keyboardPayloadFor SDL.ScancodeEscape SDL.KeycodeEscape

keyboardPayloadFor :: SDL.Scancode -> SDL.Keycode -> SDL.EventPayload
keyboardPayloadFor scancode keycode = SDL.KeyboardEvent SDL.KeyboardEventData
  { SDL.keyboardEventWindow = Nothing
  , SDL.keyboardEventKeyMotion = SDL.Pressed
  , SDL.keyboardEventRepeat = False
  , SDL.keyboardEventKeysym = SDL.Keysym
      { SDL.keysymScancode = scancode
      , SDL.keysymKeycode = keycode
      , SDL.keysymModifier = noModifiers
      }
  }

textInputPayload :: SDL.EventPayload
textInputPayload = SDL.TextInputEvent SDL.TextInputEventData
  { SDL.textInputEventWindow = Nothing
  , SDL.textInputEventText = Text.empty
  }

noModifiers :: SDL.KeyModifier
noModifiers = SDL.KeyModifier
  { SDL.keyModifierLeftShift = False
  , SDL.keyModifierRightShift = False
  , SDL.keyModifierLeftCtrl = False
  , SDL.keyModifierRightCtrl = False
  , SDL.keyModifierLeftAlt = False
  , SDL.keyModifierRightAlt = False
  , SDL.keyModifierLeftGUI = False
  , SDL.keyModifierRightGUI = False
  , SDL.keyModifierNumLock = False
  , SDL.keyModifierCapsLock = False
  , SDL.keyModifierAltGr = False
  }
