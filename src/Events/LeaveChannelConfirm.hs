module Events.LeaveChannelConfirm where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty

import           State.Channels
import           Types


onEventLeaveChannelConfirm :: Vty.Event -> MH ()
onEventLeaveChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveCurrentChannel
        _ -> return ()
    setMode Main
onEventLeaveChannelConfirm _ = return ()
