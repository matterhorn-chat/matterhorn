module Matterhorn.Events.DeleteChannelConfirm
  ( onEventDeleteChannelConfirm
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId, ChannelId )

import           Matterhorn.Types
import           Matterhorn.State.Channels


onEventDeleteChannelConfirm :: TeamId -> ChannelId -> Vty.Event -> MH ()
onEventDeleteChannelConfirm tId cId (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteChannel cId
        _ -> return ()
    popMode tId
onEventDeleteChannelConfirm _ _ (Vty.EvResize {}) = do
    return ()
onEventDeleteChannelConfirm tId _ _ = do
    popMode tId
