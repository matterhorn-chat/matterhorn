module Events.UrlSelect where

import Prelude ()
import Prelude.Compat

import Brick.Widgets.List
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Events.Keybindings
import State

onEventUrlSelect :: Vty.Event -> MH ()
onEventUrlSelect e
    | Just kb <- lookupKeybinding e urlSelectKeybindings = kbAction kb
    | otherwise = mhHandleEventLensed csUrlList handleListEvent e

urlSelectKeybindings :: [Keybinding]
urlSelectKeybindings =
    [ KB "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $ do
             openSelectedURL
             csMode .= Main

    , KB "Cancel URL selection"
         (Vty.EvKey Vty.KEsc []) $ stopUrlSelect

    , KB "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect

    , KB "Move cursor down"
         (Vty.EvKey (Vty.KChar 'j') []) $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KDown [])

    , KB "Move cursor up"
         (Vty.EvKey (Vty.KChar 'k') []) $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KUp [])

    ]
