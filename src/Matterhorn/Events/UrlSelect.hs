module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.Types


onEventUrlSelect :: TeamId -> Vty.Event -> MH Bool
onEventUrlSelect tId =
  handleKeyboardEvent (urlSelectKeybindings tId) $ \ ev ->
    mhHandleEventLensed (csTeam(tId).tsUrlList) handleListEvent ev

urlSelectKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
urlSelectKeybindings tId = mkKeybindings (urlSelectKeyHandlers tId)

urlSelectKeyHandlers :: TeamId -> [KeyEventHandler]
urlSelectKeyHandlers tId =
    [ staticKb "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $
             openSelectedURL tId

    , mkKb SaveAttachmentEvent "Save the selected attachment"
        openSaveAttachmentWindow

    , mkKb CancelEvent "Cancel URL selection" $ stopUrlSelect tId

    , mkKb SelectUpEvent "Move cursor up" $
        mhHandleEventLensed (csTeam(tId).tsUrlList) handleListEvent (Vty.EvKey Vty.KUp [])

    , mkKb SelectDownEvent "Move cursor down" $
        mhHandleEventLensed (csTeam(tId).tsUrlList) handleListEvent (Vty.EvKey Vty.KDown [])

    , staticKb "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect tId

    ]
