module Events.ChannelScroll where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventChannelScroll :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventChannelScroll st (Vty.EvResize _ _) = invalidateCache >> continue st
onEventChannelScroll st (Vty.EvKey Vty.KPageUp []) = channelPageUp st >>= continue
onEventChannelScroll st (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) = loadMoreMessages st >>= continue
onEventChannelScroll st (Vty.EvKey Vty.KPageDown []) = channelPageDown st >>= continue
onEventChannelScroll st (Vty.EvKey Vty.KEsc []) = do
    continue $ st & csMode .~ Main
onEventChannelScroll st _ = do
    continue st
