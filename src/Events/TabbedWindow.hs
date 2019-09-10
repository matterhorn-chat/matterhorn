{-# LANGUAGE RankNTypes #-}
module Events.TabbedWindow
  ( handleTabbedWindowEvent
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens', (%=) )

import           Types
import           Types.KeyEvents
import           Events.Keybindings

handleTabbedWindowEvent :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> Vty.Event
                        -> MH ()
handleTabbedWindowEvent target e = do
    w <- use target
    handleKeyboardEvent (keybindings target) (forwardEvent w) e

forwardEvent :: (Show a, Eq a)
             => TabbedWindow a
             -> Vty.Event
             -> MH ()
forwardEvent w e = do
    let cur = getCurrentTabbedWindowEntry w
    tweHandleEvent cur (twValue w) e

keybindings :: (Show a, Eq a)
            => Lens' ChatState (TabbedWindow a)
            -> KeyConfig
            -> [Keybinding]
keybindings target = mkKeybindings
    [ mkKb CancelEvent "Close window" $ do
        w <- use target
        setMode (twReturnMode w)

    , mkKb SelectNextTabEvent "Select next tab" $ do
        target %= tabbedWindowNextTab

    , mkKb SelectPreviousTabEvent "Select previous tab" $ do
        target %= tabbedWindowPreviousTab
    ]
