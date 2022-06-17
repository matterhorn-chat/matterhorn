{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Matterhorn.Draw (draw) where

import Prelude ()
import Matterhorn.Prelude

import Brick

import Lens.Micro.Platform ( _2, singular, _Just )

import Matterhorn.Draw.ChannelTopicWindow
import Matterhorn.Draw.ChannelSelectPrompt
import Matterhorn.Draw.MessageDeleteConfirm
import Matterhorn.Draw.DeleteChannelConfirm
import Matterhorn.Draw.LeaveChannelConfirm
import Matterhorn.Draw.Main
import Matterhorn.Draw.ThemeListWindow
import Matterhorn.Draw.PostListWindow
import Matterhorn.Draw.ShowHelp
import Matterhorn.Draw.UserListWindow
import Matterhorn.Draw.ChannelListWindow
import Matterhorn.Draw.ReactionEmojiListWindow
import Matterhorn.Draw.TabbedWindow
import Matterhorn.Draw.NotifyPrefs
import Matterhorn.Types


draw :: ChatState -> [Widget Name]
draw st =
    case st^.csCurrentTeamId of
        Nothing ->
            -- ^ Without a team data structure, we just assume Main mode
            -- and render a skeletal UI.
            drawMain st Main
        Just tId ->
            let messageViewWindow = st^.csTeam(tId).tsViewedMessage.singular _Just._2
                monochrome = fmap (forceAttr "invalid")
                drawMode m ms =
                    let rest = case ms of
                            (a:as) -> drawMode a as
                            _ -> []
                    in case m of
                        -- For this first section of modes, we only want
                        -- to draw for the current mode and ignore the
                        -- mode stack because we expect the current mode
                        -- to be all we need to draw what should be on
                        -- the screen.
                        Main                          -> drawMain st m
                        ShowHelp topic                -> drawShowHelp topic st

                        -- For the following modes, we want to draw the
                        -- whole mode stack since we expect the UI to
                        -- have layers and we want to show prior modes
                        -- underneath.
                        ChannelSelect                 -> drawChannelSelectPrompt st tId : drawMain st m
                        MessageSelectDeleteConfirm {} -> drawMessageDeleteConfirm : rest
                        ThemeListWindow               -> drawThemeListWindow st tId : rest
                        LeaveChannelConfirm           -> drawLeaveChannelConfirm st tId : monochrome rest
                        DeleteChannelConfirm          -> drawDeleteChannelConfirm st tId : monochrome rest
                        PostListWindow contents       -> drawPostListWindow contents st tId : monochrome rest
                        UserListWindow                -> drawUserListWindow st tId : monochrome rest
                        ChannelListWindow             -> drawChannelListWindow st tId : monochrome rest
                        ReactionEmojiListWindow       -> drawReactionEmojiListWindow st tId : monochrome rest
                        ViewMessage                   -> drawTabbedWindow messageViewWindow st tId : monochrome rest
                        EditNotifyPrefs               -> drawNotifyPrefs st tId : monochrome rest
                        ChannelTopicWindow            -> drawChannelTopicWindow st tId : monochrome rest
                topMode = teamMode $ st^.csTeam(tId)
                otherModes = tail $ teamModes $ st^.csTeam(tId)
            in drawMode topMode otherModes
