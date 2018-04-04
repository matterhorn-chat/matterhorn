module Events.DeleteChannelConfirm where

import Prelude ()
import Prelude.MH

import qualified Graphics.Vty as Vty

import Types
import State

onEventDeleteChannelConfirm :: Vty.Event -> MH ()
onEventDeleteChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteCurrentChannel
        _ -> return ()
    setMode Main
onEventDeleteChannelConfirm _ = do
    setMode Main
