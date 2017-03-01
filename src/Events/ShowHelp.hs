module Events.ShowHelp where

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventShowHelp :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventShowHelp st e | Just kb <- lookupKeybinding e helpKeybindings = kbAction kb st
onEventShowHelp st (Vty.EvKey _ _) = do
  continue $ st & csMode .~ Main
onEventShowHelp st _ = continue st

helpKeybindings :: [Keybinding]
helpKeybindings =
    [ KB "Scroll up"
         (Vty.EvKey Vty.KUp []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) (-1)
             continue st
    , KB "Scroll down"
         (Vty.EvKey Vty.KDown []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) 1
             continue st
    , KB "Page up"
         (Vty.EvKey Vty.KPageUp []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
             continue st
    , KB "Page down"
         (Vty.EvKey Vty.KPageDown []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) pageAmount
             continue st
    , KB "Page down"
         (Vty.EvKey (Vty.KChar ' ') []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) pageAmount
             continue st
    , KB "Return to the main interface"
         (Vty.EvKey Vty.KEsc []) $
         \st -> continue $ st & csMode .~ Main
    ]
