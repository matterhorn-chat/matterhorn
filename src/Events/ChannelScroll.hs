module Events.ChannelScroll where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

channelScrollKeybindings :: [Keybinding]
channelScrollKeybindings =
  [ KB "Load more messages in the current channel"
    (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl])
    loadMoreMessages
  , KB "Scroll up" (Vty.EvKey Vty.KPageUp [])
    channelPageUp
  , KB "Scroll down" (Vty.EvKey Vty.KPageDown [])
    channelPageDown
  , KB "Cancel scrolling and return to channel view"
    (Vty.EvKey Vty.KEsc []) $
    csMode .= Main
  ]

onEventChannelScroll :: Vty.Event -> MH ()
onEventChannelScroll (Vty.EvResize _ _) =
    mh invalidateCache
onEventChannelScroll e
  | Just kb <- lookupKeybinding e channelScrollKeybindings = kbAction kb
onEventChannelScroll _ = do
    return ()
