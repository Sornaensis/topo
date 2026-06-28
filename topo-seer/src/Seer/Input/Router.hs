-- | SDL event normalization and routing helpers.
module Seer.Input.Router
  ( isQuit
  , normalizeEvent
  , normalizeEventAt
  , normalizeMouseButton
  ) where

import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import UI.Component
  ( UiEvent(..)
  , UiKey(..)
  , UiModifier(..)
  , UiPointerButton(..)
  )

-- | Detect quit events.
isQuit :: SDL.Event -> Bool
isQuit event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> True
    _ -> False

-- | Normalize an SDL event without external cursor context.
--
-- Wheel events in SDL carry only a delta, so callers that know the current
-- cursor position should prefer 'normalizeEventAt'.
normalizeEvent :: SDL.Event -> Maybe UiEvent
normalizeEvent = normalizeEventAt (V2 0 0)

-- | Normalize an SDL event into renderer-neutral component input.
normalizeEventAt :: V2 Int -> SDL.Event -> Maybe UiEvent
normalizeEventAt cursor event =
  case SDL.eventPayload event of
    SDL.MouseMotionEvent motionEvent ->
      let SDL.P (V2 mx my) = SDL.mouseMotionEventPos motionEvent
      in Just (UiPointerMove (V2 (fromIntegral mx) (fromIntegral my)))
    SDL.MouseButtonEvent buttonEvent ->
      let SDL.P (V2 mx my) = SDL.mouseButtonEventPos buttonEvent
          pos = V2 (fromIntegral mx) (fromIntegral my)
          button = normalizeMouseButton (SDL.mouseButtonEventButton buttonEvent)
      in case SDL.mouseButtonEventMotion buttonEvent of
           SDL.Pressed -> Just (UiPointerDown pos button)
           SDL.Released -> Just (UiPointerUp pos button)
    SDL.MouseWheelEvent wheelEvent ->
      let SDL.V2 dx dy = SDL.mouseWheelEventPos wheelEvent
      in Just (UiScroll cursor (V2 (fromIntegral dx) (fromIntegral dy)))
    SDL.TextInputEvent textEvent ->
      Just (UiTextInput (SDL.textInputEventText textEvent))
    SDL.KeyboardEvent keyboardEvent ->
      let key = UiKey (Text.pack (show (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))))
          mods = normalizeKeyModifiers (SDL.keysymModifier (SDL.keyboardEventKeysym keyboardEvent))
      in case SDL.keyboardEventKeyMotion keyboardEvent of
           SDL.Pressed -> Just (UiKeyDown key mods)
           SDL.Released -> Just (UiKeyUp key mods)
    _ -> Nothing

normalizeMouseButton :: SDL.MouseButton -> UiPointerButton
normalizeMouseButton button = case button of
  SDL.ButtonLeft -> UiPointerPrimary
  SDL.ButtonMiddle -> UiPointerMiddle
  SDL.ButtonRight -> UiPointerSecondary
  SDL.ButtonX1 -> UiPointerAuxiliary 1
  SDL.ButtonX2 -> UiPointerAuxiliary 2
  _ -> UiPointerAuxiliary 0

normalizeKeyModifiers :: SDL.KeyModifier -> [UiModifier]
normalizeKeyModifiers mods =
  [ UiModShift
  | SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
  ] ++
  [ UiModCtrl
  | SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
  ] ++
  [ UiModAlt
  | SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods
  ] ++
  [ UiModMeta
  | SDL.keyModifierLeftGUI mods || SDL.keyModifierRightGUI mods
  ]
