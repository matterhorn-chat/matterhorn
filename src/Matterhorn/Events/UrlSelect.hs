{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.State.UrlSelect
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


onEventUrlSelect :: Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH Bool
onEventUrlSelect which =
    handleEventWith [ mhHandleKeyboardEvent (urlSelectKeybindings which)
                    , \e -> mhHandleEventLensed (which.miUrlList.ulList) handleListEvent e >> return True
                    ]

urlSelectKeybindings :: Lens' ChatState (MessageInterface Name i) -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
urlSelectKeybindings which = mkKeybindings (urlSelectKeyHandlers which)

urlSelectKeyHandlers :: Lens' ChatState (MessageInterface Name i) -> [MHKeyEventHandler]
urlSelectKeyHandlers which =
    [ onKey Vty.KEnter []
          "Open the selected URL, if any" $
          openSelectedURL which

    , onEvent SaveAttachmentEvent "Save the selected attachment" $
        openSaveAttachmentWindow which

    , onEvent CancelEvent "Cancel URL selection" $ stopUrlSelect which

    , onEvent SelectUpEvent "Move cursor up" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KUp [])

    , onEvent SelectDownEvent "Move cursor down" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KDown [])

    , onKey (Vty.KChar 'q') []
         "Cancel URL selection" $
         stopUrlSelect which
    ]
