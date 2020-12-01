module Matterhorn.Events.ShowHelp where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Graphics.Vty as Vty

import           Matterhorn.Constants
import           Matterhorn.Events.Keybindings
import           Matterhorn.Types


onEventShowHelp :: Vty.Event -> MH Bool
onEventShowHelp =
  handleKeyboardEvent helpKeybindings $ \ e -> case e of
    Vty.EvKey _ _ -> popMode
    _ -> return ()

helpKeybindings :: KeyConfig -> KeyHandlerMap
helpKeybindings = mkKeybindings helpKeyHandlers

helpKeyHandlers :: [KeyEventHandler]
helpKeyHandlers =
    [ mkKb ScrollUpEvent "Scroll up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1)
    , mkKb ScrollDownEvent "Scroll down" $
        mh $ vScrollBy (viewportScroll HelpViewport) 1
    , mkKb PageUpEvent "Page up" $
        mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , mkKb PageDownEvent "Page down" $
        mh $ vScrollBy (viewportScroll HelpViewport) (1 * pageAmount)
    , mkKb CancelEvent "Return to the previous interface" $
        popMode
    , mkKb ScrollBottomEvent "Scroll to the end of the help" $
        mh $ vScrollToEnd (viewportScroll HelpViewport)
    , mkKb ScrollTopEvent "Scroll to the beginning of the help" $
        mh $ vScrollToBeginning (viewportScroll HelpViewport)
    ]

popMode :: MH ()
popMode = do
    ShowHelp _ prevMode <- use (csCurrentTeam.tsMode)
    setMode prevMode
