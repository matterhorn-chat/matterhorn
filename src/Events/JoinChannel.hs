module Events.JoinChannel where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Widgets.List
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State (joinChannel)

joinChannelListKeys :: [Vty.Key]
joinChannelListKeys =
    [ Vty.KUp
    , Vty.KDown
    , Vty.KPageUp
    , Vty.KPageDown
    , Vty.KHome
    , Vty.KEnd
    ]

onEventJoinChannel :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventJoinChannel st e@(Vty.EvKey k []) | k `elem` joinChannelListKeys = do
    result <- case st^.csJoinChannelList of
        Nothing -> return Nothing
        Just l -> Just <$> handleListEvent e l
    continue $ st & csJoinChannelList .~ result
onEventJoinChannel st (Vty.EvKey Vty.KEnter []) = do
    case st^.csJoinChannelList of
        Nothing -> continue st
        Just l -> case listSelectedElement l of
            Nothing -> continue st
            Just (_, chan) -> joinChannel chan st >>= continue
onEventJoinChannel st (Vty.EvKey Vty.KEsc []) = do
    continue $ st & csMode .~ Main
onEventJoinChannel st _ = do
    continue st
