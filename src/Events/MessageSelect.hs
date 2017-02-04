module Events.MessageSelect where

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventMessageSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMessageSelect st (Vty.EvKey Vty.KEsc []) =
    continue $ st & csMode .~ Main
onEventMessageSelect st (Vty.EvKey Vty.KUp _) =
    messageSelectUp st >>= continue
onEventMessageSelect st (Vty.EvKey Vty.KDown _) =
    messageSelectDown st >>= continue
onEventMessageSelect st _ =
    continue st
