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
    setMode Main
onEventDeleteChannelConfirm _ = do
    setMode Main
