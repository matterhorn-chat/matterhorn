module Matterhorn.Events.ShowHelp where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Constants
import           Matterhorn.Types


onEventShowHelp :: TeamId -> Vty.Event -> MH Bool
onEventShowHelp tId =
    handleEventWith [ mhHandleKeyboardEvent (helpKeybindings tId)
                    , closeHelp tId
                    ]

closeHelp :: TeamId -> Vty.Event -> MH Bool
closeHelp tId (Vty.EvKey {}) = do
    popMode tId
    return True
closeHelp _ _ = return False

helpKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
helpKeybindings tId kc = unsafeKeyDispatcher kc (helpKeyHandlers tId)

helpKeyHandlers :: TeamId -> [MHKeyEventHandler]
helpKeyHandlers tId =
    [ onEvent ScrollUpEvent "Scroll up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1)
    , onEvent ScrollDownEvent "Scroll down" $
        mh $ vScrollBy (viewportScroll HelpViewport) 1
    , onEvent PageUpEvent "Page up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , onEvent PageDownEvent "Page down" $
        mh $ vScrollBy (viewportScroll HelpViewport) (1 * pageAmount)
    , onEvent CancelEvent "Close the help window" $
        popMode tId
    , onEvent ScrollBottomEvent "Scroll to the end of the help" $
        mh $ vScrollToEnd (viewportScroll HelpViewport)
    , onEvent ScrollTopEvent "Scroll to the beginning of the help" $
        mh $ vScrollToBeginning (viewportScroll HelpViewport)
    ]
