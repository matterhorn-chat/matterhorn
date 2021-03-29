{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Matterhorn.Draw (draw) where

import Prelude ()
import Matterhorn.Prelude

import Brick

import Lens.Micro.Platform ( _2, singular, _Just )

import Matterhorn.Draw.ChannelTopicWindow
import Matterhorn.Draw.SaveAttachmentWindow
import Matterhorn.Draw.DeleteChannelConfirm
import Matterhorn.Draw.LeaveChannelConfirm
import Matterhorn.Draw.Main
import Matterhorn.Draw.ThemeListOverlay
import Matterhorn.Draw.PostListOverlay
import Matterhorn.Draw.ShowHelp
import Matterhorn.Draw.UserListOverlay
import Matterhorn.Draw.ChannelListOverlay
import Matterhorn.Draw.ReactionEmojiListOverlay
import Matterhorn.Draw.TabbedWindow
import Matterhorn.Draw.ManageAttachments
import Matterhorn.Draw.NotifyPrefs
import Matterhorn.Types


draw :: ChatState -> [Widget Name]
draw st =
    case st^.csCurrentTeam.tsMode of
        Main                         -> mainLayers
        UrlSelect                    -> mainLayers
        ChannelSelect                -> mainLayers
        MessageSelect                -> mainLayers
        MessageSelectDeleteConfirm   -> mainLayers
        ShowHelp topic _             -> drawShowHelp topic st
        ThemeListOverlay             -> drawThemeListOverlay st : mainLayers
        LeaveChannelConfirm          -> drawLeaveChannelConfirm st : mainLayersMonochrome
        DeleteChannelConfirm         -> drawDeleteChannelConfirm st : mainLayersMonochrome
        PostListOverlay contents     -> drawPostListOverlay contents st : mainLayersMonochrome
        UserListOverlay              -> drawUserListOverlay st : mainLayersMonochrome
        ChannelListOverlay           -> drawChannelListOverlay st : mainLayersMonochrome
        ReactionEmojiListOverlay     -> drawReactionEmojiListOverlay st : mainLayersMonochrome
        ViewMessage                  -> drawTabbedWindow messageViewWindow st : mainLayersMonochrome
        ManageAttachments            -> drawManageAttachments st : mainLayersMonochrome
        ManageAttachmentsBrowseFiles -> drawManageAttachments st : mainLayersMonochrome
        EditNotifyPrefs              -> drawNotifyPrefs st : mainLayersMonochrome
        ChannelTopicWindow           -> drawChannelTopicWindow st : mainLayersMonochrome
        SaveAttachmentWindow _       -> drawSaveAttachmentWindow st : mainLayersMonochrome
    where
        mainLayers = drawMain True st
        mainLayersMonochrome = drawMain False st
        messageViewWindow = st^.csCurrentTeam.tsViewedMessage.singular _Just._2
