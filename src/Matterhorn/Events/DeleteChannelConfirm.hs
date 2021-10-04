module Matterhorn.Events.DeleteChannelConfirm where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.State.Channels


onEventDeleteChannelConfirm :: Vty.Event -> MH ()
onEventDeleteChannelConfirm (Vty.EvKey k []) = do
    case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            deleteCurrentChannel
        _ -> return ()
    tId <- use csCurrentTeamId
    setMode tId Main
onEventDeleteChannelConfirm _ = do
    tId <- use csCurrentTeamId
    setMode tId Main
