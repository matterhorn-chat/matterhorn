module Events.ShowHelp where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State
import Constants

onEventShowHelp :: Vty.Event -> MH ()
onEventShowHelp e | Just kb <- lookupKeybinding e helpKeybindings =
  kbAction kb
onEventShowHelp (Vty.EvKey _ _) = do
  csMode .= Main
onEventShowHelp _ = return ()

helpKeybindings :: [Keybinding]
helpKeybindings =
    [ KB "Scroll up"
         (Vty.EvKey Vty.KUp []) $ do
             mh $ vScrollBy (viewportScroll HelpViewport) (-1)
    , KB "Scroll down"
         (Vty.EvKey Vty.KDown []) $ do
             mh $ vScrollBy (viewportScroll HelpViewport) 1
    , KB "Page up"
         (Vty.EvKey Vty.KPageUp []) $ do
             mh $ vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
    , KB "Page down"
         (Vty.EvKey Vty.KPageDown []) $ do
             mh $ vScrollBy (viewportScroll HelpViewport) pageAmount
    , KB "Page down"
         (Vty.EvKey (Vty.KChar ' ') []) $ do
             mh $ vScrollBy (viewportScroll HelpViewport) pageAmount
    , KB "Return to the main interface"
         (Vty.EvKey Vty.KEsc []) $ do
           csMode .= Main
    ]
