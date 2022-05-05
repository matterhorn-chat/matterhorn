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
import           Matterhorn.Events.Keybindings

handleTabbedWindowEvent :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> TeamId
                        -> Vty.Event
                        -> MH Bool
handleTabbedWindowEvent target tId e = do
    w <- use target
    handleKeyboardEvent (tabbedWindowKeybindings target tId) (forwardEvent w) e

forwardEvent :: (Show a, Eq a)
             => TabbedWindow a
             -> Vty.Event
             -> MH ()
forwardEvent w e = do
    let cur = getCurrentTabbedWindowEntry w
    tweHandleEvent cur (twValue w) e

tabbedWindowKeybindings :: (Show a, Eq a)
                        => Lens' ChatState (TabbedWindow a)
                        -> TeamId
                        -> KeyConfig
                        -> KeyHandlerMap
tabbedWindowKeybindings target tId = mkKeybindings $ tabbedWindowKeyHandlers tId target

tabbedWindowKeyHandlers :: (Show a, Eq a)
                        => TeamId
                        -> Lens' ChatState (TabbedWindow a)
                        -> [KeyEventHandler]
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
