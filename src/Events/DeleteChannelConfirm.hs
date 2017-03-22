module Events.DeleteChannelConfirm where

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventDeleteChannelConfirm :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventDeleteChannelConfirm st (Vty.EvKey k []) = do
    st' <- case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteCurrentChannel st
        _ -> return st
    continue $ st' & csMode .~ Main
onEventDeleteChannelConfirm st _ = do
    continue $ st & csMode .~ Main
