module Events.ViewMessage
  ( onEventViewMessage
  , viewMessageKeybindings
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main
import qualified Graphics.Vty as Vty

import           Constants
import           Events.Keybindings
import           Types


onEventViewMessage :: Vty.Event -> MH ()
onEventViewMessage =
    handleKeyboardEvent viewMessageKeybindings handleMessageViewEvent

vs :: ViewportScroll Name
vs = viewportScroll ViewMessageArea

handleMessageViewEvent :: Vty.Event -> MH ()
handleMessageViewEvent (Vty.EvKey Vty.KLeft []) =
    mh $ hScrollBy vs (-1)
handleMessageViewEvent (Vty.EvKey Vty.KRight []) =
    mh $ hScrollBy vs 1
handleMessageViewEvent _ = return ()

viewMessageKeybindings :: KeyConfig -> [Keybinding]
viewMessageKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close window" $
        setMode Main

    , mkKb PageUpEvent "Page up" $
        mh $ vScrollBy vs (-1 * pageAmount)

    , mkKb PageDownEvent "Page down" $
        mh $ vScrollBy vs pageAmount

    , mkKb ScrollUpEvent "Scroll up" $
        mh $ vScrollBy vs (-1)

    , mkKb ScrollDownEvent "Scroll down" $
        mh $ vScrollBy vs 1

    , mkKb ScrollBottomEvent "Scroll to the end of the message" $
        mh $ vScrollToEnd vs

    , mkKb ScrollTopEvent "Scroll to the beginning of the message" $
        mh $ vScrollToBeginning vs
    ]
