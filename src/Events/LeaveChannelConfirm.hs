module Events.LeaveChannelConfirm where

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventLeaveChannelConfirm :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventLeaveChannelConfirm st (Vty.EvKey k []) = do
    st' <- case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveCurrentChannel st
        _ -> return st
    continue $ st' & csMode .~ Main
onEventLeaveChannelConfirm st _ = do
    continue st
