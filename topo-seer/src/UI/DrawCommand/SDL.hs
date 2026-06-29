-- | SDL interpreter for renderer-neutral UI draw commands.
module UI.DrawCommand.SDL
  ( interpretDrawCommand
  , interpretDrawCommands
  ) where

import qualified SDL
import UI.DrawCommand (DrawCommand(..), TextPlacement(..))
import UI.Font (FontCache)
import UI.WidgetsDraw
  ( drawCentered
  , drawLabelAbove
  , drawLabelLeft
  , drawLeft
  , drawTextLine
  , rectToSDL
  )

interpretDrawCommands :: SDL.Renderer -> Maybe FontCache -> [DrawCommand] -> IO ()
interpretDrawCommands renderer fontCache = mapM_ (interpretDrawCommand renderer fontCache)

interpretDrawCommand :: SDL.Renderer -> Maybe FontCache -> DrawCommand -> IO ()
interpretDrawCommand renderer fontCache command =
  case command of
    DrawFillRect color rect -> do
      SDL.rendererDrawColor renderer SDL.$= color
      SDL.fillRect renderer (Just (rectToSDL rect))
    DrawStrokeRect color rect -> do
      SDL.rendererDrawColor renderer SDL.$= color
      SDL.drawRect renderer (Just (rectToSDL rect))
    DrawText color placement label ->
      case placement of
        TextAt pos -> drawTextLine fontCache pos color label
        TextCentered rect -> drawCentered fontCache color rect label
        TextLeft rect -> drawLeft fontCache color rect label
        TextLabelAbove rect -> drawLabelAbove fontCache color rect label
        TextLabelLeft rect -> drawLabelLeft fontCache color rect label
    DrawLine color start end -> do
      SDL.rendererDrawColor renderer SDL.$= color
      SDL.drawLine renderer (SDL.P (fromIntegral <$> start)) (SDL.P (fromIntegral <$> end))
    DrawClip clipRect ->
      SDL.rendererClipRect renderer SDL.$= fmap rectToSDL clipRect
