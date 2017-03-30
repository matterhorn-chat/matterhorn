module Events.LeaveChannelConfirm where

import Prelude ()
import Prelude.Compat

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventLeaveChannelConfirm :: Vty.Event -> MH ()
onEventLeaveChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveCurrentChannel
        _ -> return ()
    csMode .= Main
onEventLeaveChannelConfirm _ = return ()
