module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List
import qualified Graphics.Vty as Vty

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.UrlSelect
import           Matterhorn.Types


onEventUrlSelect :: Vty.Event -> MH Bool
onEventUrlSelect =
  handleKeyboardEvent urlSelectKeybindings $ \ ev ->
    mhHandleEventLensed csUrlList handleListEvent ev

urlSelectKeybindings :: KeyConfig -> KeyHandlerMap
urlSelectKeybindings = mkKeybindings urlSelectKeyHandlers

urlSelectKeyHandlers :: [KeyEventHandler]
urlSelectKeyHandlers =
    [ staticKb "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $
             openSelectedURL

    , mkKb CancelEvent "Cancel URL selection" stopUrlSelect

    , mkKb SelectUpEvent "Move cursor up" $
        mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KUp [])

    , mkKb SelectDownEvent "Move cursor down" $
        mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KDown [])

    , staticKb "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect

    ]
