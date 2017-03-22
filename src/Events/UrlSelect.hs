module Events.UrlSelect where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Widgets.List
import Control.Monad ((>=>))
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

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
         openSelectedURL >=> \ st -> continue (st & csMode .~ Main)

    , KB "Cancel URL selection"
         (Vty.EvKey Vty.KEsc []) $
         continue . stopUrlSelect

    , KB "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $
         continue . stopUrlSelect

    , KB "Move cursor down"
         (Vty.EvKey (Vty.KChar 'j') []) $ \st ->
         continue =<< handleEventLensed st csUrlList handleListEvent (Vty.EvKey Vty.KDown [])

    , KB "Move cursor up"
         (Vty.EvKey (Vty.KChar 'k') []) $ \st ->
         continue =<< handleEventLensed st csUrlList handleListEvent (Vty.EvKey Vty.KUp [])

    ]
