{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.ManageChannelBookmarks
    ( enterManageChannelBookmarksMode
    , exitManageChannelBookmarksMode
    )
where

import Prelude ()
import Matterhorn.Prelude

import Brick.Widgets.List ( list )
import Lens.Micro.Platform ( Lens', (.=) )

import Matterhorn.Types


enterManageChannelBookmarksMode :: Lens' ChatState (MessageInterface Name i) -> MH ()
enterManageChannelBookmarksMode which = do
    t <- use (which.miTarget)
    case t of
        MITeamThread {} -> return ()
        MIChannel cId -> do
            bs <- use (csChannel(cId).ccInfo.cdBookmarks)
            let name = ManageChannelBookmarksList cId
            which.miBookmarkManager.bmBookmarkList .= list name bs 1
            which.miMode .= ManageBookmarks

exitManageChannelBookmarksMode :: Lens' ChatState (MessageInterface n i) -> MH ()
exitManageChannelBookmarksMode which = which.miMode .= Compose
