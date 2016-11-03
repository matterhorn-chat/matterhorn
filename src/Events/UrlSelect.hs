module Events.UrlSelect where

import Brick
import Brick.Widgets.List
import Control.Monad ((>=>))
import qualified Graphics.Vty as Vty

import Types
import State

onEventUrlSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventUrlSelect st e
    | Just kb <- lookupKeybinding e urlSelectKeybindings = kbAction kb st
    | otherwise = continue =<< handleEventLensed st csUrlList handleListEvent e

urlSelectKeybindings :: [Keybinding]
urlSelectKeybindings =
    [ KB "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $
         openSelectedURL >=> continue

    , KB "Cancel URL selection"
         (Vty.EvKey Vty.KEsc []) $
         continue . stopUrlSelect

    ]
