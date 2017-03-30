module Events.ChannelScroll where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventChannelScroll :: Vty.Event -> MH ()
onEventChannelScroll (Vty.EvResize _ _) =
    mh invalidateCache
onEventChannelScroll (Vty.EvKey Vty.KPageUp []) =
    channelPageUp
onEventChannelScroll (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) =
    loadMoreMessages
onEventChannelScroll (Vty.EvKey Vty.KPageDown []) =
    channelPageDown
onEventChannelScroll (Vty.EvKey Vty.KEsc []) = do
    csMode .= Main
onEventChannelScroll _ = do
    return ()
