module Matterhorn.Events.LeaveChannelConfirm
  ( onEventLeaveChannelConfirm
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId, ChannelId )

import           Matterhorn.State.Channels
import           Matterhorn.Types


onEventLeaveChannelConfirm :: TeamId -> ChannelId -> Vty.Event -> MH ()
onEventLeaveChannelConfirm tId cId (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveChannel cId
        _ -> return ()
    popMode tId
onEventLeaveChannelConfirm _ _ _ = return ()
