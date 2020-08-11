{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw (draw) where

import Prelude ()
import Prelude.MH

import Brick

import Lens.Micro.Platform ( _2, singular, _Just )

import Draw.DeleteChannelConfirm
import Draw.LeaveChannelConfirm
import Draw.Main
import Draw.ThemeListOverlay
import Draw.PostListOverlay
import Draw.ShowHelp
import Draw.UserListOverlay
import Draw.ChannelListOverlay
import Draw.ReactionEmojiListOverlay
import Draw.TabbedWindow
import Draw.ManageAttachments
import Draw.NotifyPrefs
import Types


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
