{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw (draw) where

import Brick

import Draw.DeleteChannelConfirm
import Draw.JoinChannel
import Draw.LeaveChannelConfirm
import Draw.Main
import Draw.PostListOverlay
import Draw.ShowHelp
import Draw.UserListOverlay
import Draw.ViewMessage
import Draw.ManageAttachments
import Types


draw :: ChatState -> [Widget Name]
draw st =
    case appMode st of
        Main                       -> drawMain st
        UrlSelect                  -> drawMain st
        ShowHelp topic             -> drawShowHelp topic st
        ChannelSelect              -> drawMain st
        LeaveChannelConfirm        -> drawLeaveChannelConfirm st
        JoinChannel                -> drawJoinChannel st
        MessageSelect              -> drawMain st
        MessageSelectDeleteConfirm -> drawMain st
        DeleteChannelConfirm       -> drawDeleteChannelConfirm st
        PostListOverlay contents   -> drawPostListOverlay contents st
        UserListOverlay            -> drawUserListOverlay st
        ViewMessage                -> drawViewMessage st
        ManageAttachments          -> drawManageAttachments st
        ManageAttachmentsBrowseFiles -> drawManageAttachments st
