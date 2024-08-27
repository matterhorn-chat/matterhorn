module Matterhorn.Events.DeleteChannelConfirm
  ( onEventDeleteChannelConfirm
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.Channels


onEventDeleteChannelConfirm :: TeamId -> Vty.Event -> MH ()
onEventDeleteChannelConfirm tId (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteCurrentChannel tId
        _ -> return ()
    popMode tId
onEventDeleteChannelConfirm _ (Vty.EvResize {}) = do
    return ()
onEventDeleteChannelConfirm tId _ = do
    popMode tId
