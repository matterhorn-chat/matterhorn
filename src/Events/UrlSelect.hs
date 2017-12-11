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
onEventUrlSelect =
  handleKeyboardEvent urlSelectKeybindings $ \ ev ->
    mhHandleEventLensed csUrlList handleListEvent ev

urlSelectKeybindings :: KeyConfig -> [Keybinding]
urlSelectKeybindings = mkKeybindings
    [ staticKb "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $ do
             openSelectedURL
             csMode .= Main

    , mkKb CancelEvent "Cancel URL selection" stopUrlSelect

    , staticKb "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect

    , staticKb "Move cursor down"
         (Vty.EvKey (Vty.KChar 'j') []) $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KDown [])

    , staticKb "Move cursor up"
         (Vty.EvKey (Vty.KChar 'k') []) $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KUp [])

    ]
