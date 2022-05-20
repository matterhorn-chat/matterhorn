module Matterhorn.Events.ShowHelp where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Constants
import           Matterhorn.Events.Keybindings
import           Matterhorn.Types


onEventShowHelp :: TeamId -> Vty.Event -> MH Bool
onEventShowHelp tId =
    handleEventWith [ handleKeyboardEvent (helpKeybindings tId)
                    , closeHelp tId
                    ]

closeHelp :: TeamId -> Vty.Event -> MH Bool
closeHelp tId (Vty.EvKey {}) = do
    popMode tId
    return True
closeHelp _ _ = return False

helpKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
helpKeybindings tId = mkKeybindings (helpKeyHandlers tId)

helpKeyHandlers :: TeamId -> [KeyEventHandler]
helpKeyHandlers tId =
    [ mkKb ScrollUpEvent "Scroll up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1)
    , mkKb ScrollDownEvent "Scroll down" $
        mh $ vScrollBy (viewportScroll HelpViewport) 1
    , mkKb PageUpEvent "Page up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , mkKb PageDownEvent "Page down" $
        mh $ vScrollBy (viewportScroll HelpViewport) (1 * pageAmount)
    , mkKb CancelEvent "Close the help window" $
        popMode tId
    , mkKb ScrollBottomEvent "Scroll to the end of the help" $
        mh $ vScrollToEnd (viewportScroll HelpViewport)
    , mkKb ScrollTopEvent "Scroll to the beginning of the help" $
        mh $ vScrollToBeginning (viewportScroll HelpViewport)
    ]
