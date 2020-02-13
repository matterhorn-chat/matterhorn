module Events.ShowHelp where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Graphics.Vty as Vty

import           Constants
import           Events.Keybindings
import           Types


onEventShowHelp :: Vty.Event -> MH ()
onEventShowHelp =
  handleKeyboardEvent helpKeybindings $ \ e -> case e of
    Vty.EvKey _ _ -> setMode Main
    _ -> return ()

helpKeybindings :: KeyConfig -> [Keybinding]
helpKeybindings = mkKeybindings
    [ mkKb ScrollUpEvent "Scroll up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1)
    , mkKb ScrollDownEvent "Scroll down" $
        mh $ vScrollBy (viewportScroll HelpViewport) 1
    , mkKb PageUpEvent "Page up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , mkKb PageDownEvent "Page down" $
        mh $ vScrollBy (viewportScroll HelpViewport) (1 * pageAmount)
    , mkKb CancelEvent "Return to the main interface" $
        setMode Main
    , mkKb ScrollBottomEvent "Scroll to the end of the help" $
        mh $ vScrollToEnd (viewportScroll HelpViewport)
    , mkKb ScrollTopEvent "Scroll to the beginning of the help" $
        mh $ vScrollToBeginning (viewportScroll HelpViewport)
    ]
