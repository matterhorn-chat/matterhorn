module Matterhorn.Events.DeleteChannelConfirm where

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
    setMode tId Main
onEventDeleteChannelConfirm tId _ = do
    setMode tId Main
