module Events.ShowHelp where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Events.Keybindings
import Constants

onEventShowHelp :: Vty.Event -> MH ()
onEventShowHelp =
  handleKeyboardEvent helpKeybindings $ \ e -> case e of
    Vty.EvKey _ _ -> csMode .= Main
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
        mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , mkKb CancelEvent "Return to the main interface" $
        csMode .= Main
    ]

-- KB "Scroll up"
--          (Vty.EvKey Vty.KUp []) $ do
--              mh $ vScrollBy (viewportScroll HelpViewport) (-1)
--     , KB "Scroll down"
--          (Vty.EvKey Vty.KDown []) $ do
--              mh $ vScrollBy (viewportScroll HelpViewport) 1
--     , KB "Page up"
--          (Vty.EvKey Vty.KPageUp []) $ do
--              mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
--     , KB "Page down"
--          (Vty.EvKey Vty.KPageDown []) $ do
--              mh $ vScrollBy (viewportScroll HelpViewport) pageAmount
--     , KB "Page down"
--          (Vty.EvKey (Vty.KChar ' ') []) $ do
--              mh $ vScrollBy (viewportScroll HelpViewport) pageAmount
--     , KB "Return to the main interface"
--          (Vty.EvKey Vty.KEsc []) $ do
--            csMode .= Main
--     ]
