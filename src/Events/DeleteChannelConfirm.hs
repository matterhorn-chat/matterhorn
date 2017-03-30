module Events.DeleteChannelConfirm where

import Prelude ()
import Prelude.Compat

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventDeleteChannelConfirm :: Vty.Event -> MH ()
onEventDeleteChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteCurrentChannel
        _ -> return ()
    csMode .= Main
onEventDeleteChannelConfirm _ = do
    csMode .= Main
