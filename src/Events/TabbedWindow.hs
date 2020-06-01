{-# LANGUAGE RankNTypes #-}
module Events.TabbedWindow
  ( handleTabbedWindowEvent
  , tabbedWindowKeybindings
  , tabbedWindowKeyHandlers
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens', (.=) )

import           Types
import           Types.KeyEvents
import           Events.Keybindings

handleTabbedWindowEvent :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> Vty.Event
                        -> MH Bool
handleTabbedWindowEvent target e = do
    w <- use target
    handleKeyboardEvent (tabbedWindowKeybindings target) (forwardEvent w) e

forwardEvent :: (Show a, Eq a)
             => TabbedWindow a
             -> Vty.Event
             -> MH ()
forwardEvent w e = do
    let cur = getCurrentTabbedWindowEntry w
    tweHandleEvent cur (twValue w) e

tabbedWindowKeybindings :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> KeyConfig
                        -> [KeyHandler]
tabbedWindowKeybindings target = mkKeybindings $ tabbedWindowKeyHandlers target

tabbedWindowKeyHandlers :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> [KeyEventHandler]
tabbedWindowKeyHandlers target =
    [ mkKb CancelEvent "Close window" $ do
        w <- use target
        setMode (twReturnMode w)

    , mkKb SelectNextTabEvent "Select next tab" $ do
        w' <- tabbedWindowNextTab =<< use target
        target .= w'

    , mkKb SelectPreviousTabEvent "Select previous tab" $ do
        w' <- tabbedWindowPreviousTab =<< use target
        target .= w'
    ]
