{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.TabbedWindow
  ( handleTabbedWindowEvent
  , tabbedWindowKeybindings
  , tabbedWindowKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens', (.=) )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


handleTabbedWindowEvent :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow ChatState MH Name a)
                        -> TeamId
                        -> Vty.Event
                        -> MH Bool
handleTabbedWindowEvent target tId e = do
    w <- use target
    handleEventWith [ mhHandleKeyboardEvent (tabbedWindowKeybindings target tId)
                    , \_ -> forwardEvent w e >> return True
                    ] e

forwardEvent :: (Show a, Eq a)
             => TabbedWindow s MH n a
             -> Vty.Event
             -> MH ()
forwardEvent w e = do
    let cur = getCurrentTabbedWindowEntry w
    tweHandleEvent cur (twValue w) e

tabbedWindowKeybindings :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow ChatState MH Name a)
                        -> TeamId
                        -> KeyConfig KeyEvent
                        -> KeyHandlerMap KeyEvent MH
tabbedWindowKeybindings target tId = mkKeybindings $ tabbedWindowKeyHandlers tId target

tabbedWindowKeyHandlers :: (Show a, Eq a)
                        => TeamId
                        -> Lens' ChatState (TabbedWindow ChatState MH Name a)
                        -> [KeyEventHandler KeyEvent MH]
tabbedWindowKeyHandlers tId target =
    [ mkKb CancelEvent "Close window" $
        popMode tId

    , mkKb SelectNextTabEvent "Select next tab" $ do
        w' <- tabbedWindowNextTab =<< use target
        target .= w'

    , mkKb SelectPreviousTabEvent "Select previous tab" $ do
        w' <- tabbedWindowPreviousTab =<< use target
        target .= w'
    ]
