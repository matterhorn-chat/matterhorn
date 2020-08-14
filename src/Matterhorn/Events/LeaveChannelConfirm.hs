module Matterhorn.Events.LeaveChannelConfirm where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.State.Channels
import           Matterhorn.Types


onEventLeaveChannelConfirm :: Vty.Event -> MH ()
onEventLeaveChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveCurrentChannel
        _ -> return ()
    setMode Main
onEventLeaveChannelConfirm _ = return ()
