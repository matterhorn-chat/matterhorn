{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw (draw) where

import Prelude ()
import Prelude.MH

import Brick

import Draw.DeleteChannelConfirm
import Draw.LeaveChannelConfirm
import Draw.Main
import Draw.PostListOverlay
import Draw.ShowHelp
import Draw.UserListOverlay
import Draw.ChannelListOverlay
import Draw.ViewMessage
import Draw.ManageAttachments
import Types


draw :: ChatState -> [Widget Name]
draw st =
    case appMode st of
        Main                       -> drawMain True st
        UrlSelect                  -> drawMain True st
        ShowHelp topic             -> drawShowHelp topic st
        ChannelSelect              -> drawMain True st
        LeaveChannelConfirm        -> drawLeaveChannelConfirm st
        MessageSelect              -> drawMain True st
        MessageSelectDeleteConfirm -> drawMain True st
        DeleteChannelConfirm       -> drawDeleteChannelConfirm st
        PostListOverlay contents   -> drawPostListOverlay contents st
        UserListOverlay            -> drawUserListOverlay st
        ChannelListOverlay         -> drawChannelListOverlay st
        ViewMessage                -> drawViewMessage st
        ManageAttachments          -> drawManageAttachments st
        ManageAttachmentsBrowseFiles -> drawManageAttachments st
