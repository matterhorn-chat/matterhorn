{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw (draw) where

import Brick

import Types
import Draw.Main
import Draw.ShowHelp
import Draw.LeaveChannelConfirm
import Draw.DeleteChannelConfirm
import Draw.PostListOverlay
import Draw.JoinChannel

draw :: ChatState -> [Widget Name]
draw st =
    case appMode st of
        Main                       -> drawMain st
        ChannelScroll              -> drawMain st
        UrlSelect                  -> drawMain st
        ShowHelp topic             -> drawShowHelp topic st
        ChannelSelect              -> drawMain st
        LeaveChannelConfirm        -> drawLeaveChannelConfirm st
        JoinChannel                -> drawJoinChannel st
        MessageSelect              -> drawMain st
        MessageSelectDeleteConfirm -> drawMain st
        DeleteChannelConfirm       -> drawDeleteChannelConfirm st
        PostListOverlay contents   -> drawPostListOverlay contents st
