{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.Types


onEventUrlSelect :: Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH Bool
onEventUrlSelect which =
    handleEventWith [ handleKeyboardEvent (urlSelectKeybindings which)
                    , \e -> mhHandleEventLensed (which.miUrlList.ulList) handleListEvent e >> return True
                    ]

urlSelectKeybindings :: Lens' ChatState (MessageInterface Name i) -> KeyConfig -> KeyHandlerMap
urlSelectKeybindings which = mkKeybindings (urlSelectKeyHandlers which)

urlSelectKeyHandlers :: Lens' ChatState (MessageInterface Name i) -> [KeyEventHandler]
urlSelectKeyHandlers which =
    [ staticKb "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $
             openSelectedURL which

    , mkKb SaveAttachmentEvent "Save the selected attachment" $
        openSaveAttachmentWindow which

    , mkKb CancelEvent "Cancel URL selection" $ stopUrlSelect which

    , mkKb SelectUpEvent "Move cursor up" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KUp [])

    , mkKb SelectDownEvent "Move cursor down" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KDown [])

    , staticKb "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect which

    ]
