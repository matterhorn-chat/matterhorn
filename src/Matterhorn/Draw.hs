{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Matterhorn.Draw (draw) where

import Prelude ()
import Matterhorn.Prelude

import Brick

import Lens.Micro.Platform ( _2, singular, _Just )

import Matterhorn.Draw.ChannelTopicWindow
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
    case appMode st of
        Main                       -> drawMain True st
        UrlSelect                  -> drawMain True st
        ShowHelp topic _           -> drawShowHelp topic st
        ChannelSelect              -> drawMain True st
        LeaveChannelConfirm        -> drawLeaveChannelConfirm st
        MessageSelect              -> drawMain True st
        MessageSelectDeleteConfirm -> drawMain True st
        DeleteChannelConfirm       -> drawDeleteChannelConfirm st
        ThemeListOverlay           -> drawThemeListOverlay st
        PostListOverlay contents   -> drawPostListOverlay contents st
        UserListOverlay            -> drawUserListOverlay st
        ChannelListOverlay         -> drawChannelListOverlay st
        ReactionEmojiListOverlay   -> drawReactionEmojiListOverlay st
        ViewMessage                -> drawTabbedWindow (st^.csViewedMessage.singular _Just._2) st : drawMain False st
        ManageAttachments          -> drawManageAttachments st
        ManageAttachmentsBrowseFiles -> drawManageAttachments st
        EditNotifyPrefs            -> drawNotifyPrefs st : drawMain False st
        ChannelTopicWindow         -> drawChannelTopicWindow st : drawMain False st
