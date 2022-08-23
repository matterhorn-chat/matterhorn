{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import           Brick.Widgets.List
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.State.UrlSelect
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.Types


onEventUrlSelect :: Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH Bool
onEventUrlSelect which =
    handleEventWith [ mhHandleKeyboardEvent (urlSelectKeybindings which)
                    , \e -> mhZoom (which.miUrlList.ulList) handleListEvent e >> return True
                    ]

urlSelectKeybindings :: Lens' ChatState (MessageInterface Name i) -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
urlSelectKeybindings which kc = keyDispatcher kc (urlSelectKeyHandlers which)

urlSelectKeyHandlers :: Lens' ChatState (MessageInterface Name i) -> [MHKeyEventHandler]
urlSelectKeyHandlers which =
    [ onKey (bind Vty.KEnter)
          "Open the selected URL, if any" $
          openSelectedURL which

    , onEvent SaveAttachmentEvent "Save the selected attachment" $
        openSaveAttachmentWindow which

    , onEvent CancelEvent "Cancel URL selection" $ stopUrlSelect which

    , onEvent SelectUpEvent "Move cursor up" $
        mhZoom (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KUp [])

    , onEvent SelectDownEvent "Move cursor down" $
        mhZoom (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KDown [])

    , onKey (bind 'q')
         "Cancel URL selection" $
         stopUrlSelect which
    ]
