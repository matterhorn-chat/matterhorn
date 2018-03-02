module Events.JoinChannel where

import Prelude ()
import Prelude.Compat

import Brick.Widgets.List
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Network.Mattermost.Types (getId)

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

onEventJoinChannel :: Vty.Event -> MH ()
onEventJoinChannel e@(Vty.EvKey k []) | k `elem` joinChannelListKeys = do
    chList <- use csJoinChannelList
    result <- case chList of
        Nothing -> return Nothing
        Just l -> Just <$> mh (handleListEvent e l)
    csJoinChannelList .= result
onEventJoinChannel (Vty.EvKey Vty.KEnter []) = do
    chList <- use csJoinChannelList
    case chList of
        Nothing -> return ()
        Just l -> case listSelectedElement l of
            Nothing -> return ()
            Just (_, chan) -> joinChannel (getId chan)
onEventJoinChannel (Vty.EvKey Vty.KEsc []) = do
    setMode Main
onEventJoinChannel _ = do
    return ()
