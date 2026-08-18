{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.ManageChannelBookmarks
    ( enterManageChannelBookmarksMode
    , exitManageChannelBookmarksMode

    , moveSelectedBookmarkUp
    , moveSelectedBookmarkDown
    , deleteSelectedBookmark
    )
where

import Prelude ()
import Matterhorn.Prelude

import Brick.Widgets.List ( list, listSelectedElement, listElementsL )
import Lens.Micro.Platform ( Lens', (.=) )

import qualified Network.Mattermost.Types as MM
import qualified Network.Mattermost.Endpoints as MM

import Matterhorn.State.Async
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

moveSelectedBookmarkUp :: Lens' ChatState (MessageInterface n i) -> MH ()
moveSelectedBookmarkUp which = do
    session <- getSession
    withSelectedBookmark which $ \_ b i ->
        when (i > 0) $
            doAsyncWith Normal $ do
                MM.mmSetChannelBookmarkOrder (MM.bookmarkChannelId b) (MM.bookmarkId b) (i - 1) session
                return Nothing

moveSelectedBookmarkDown :: Lens' ChatState (MessageInterface n i) -> MH ()
moveSelectedBookmarkDown which = do
    session <- getSession
    withSelectedBookmark which $ \bs b i ->
        when (i < length bs - 1) $
            doAsyncWith Normal $ do
                MM.mmSetChannelBookmarkOrder (MM.bookmarkChannelId b) (MM.bookmarkId b) (i + 1) session
                return Nothing

deleteSelectedBookmark :: Lens' ChatState (MessageInterface n i) -> MH ()
deleteSelectedBookmark which = do
    session <- getSession
    withSelectedBookmark which $ \bs b i ->
        doAsyncWith Normal $ do
            MM.mmDeleteChannelBookmark (MM.bookmarkChannelId b) (MM.bookmarkId b) session
            return Nothing

withSelectedBookmark :: Lens' ChatState (MessageInterface n i)
                     -> (Seq MM.Bookmark -> MM.Bookmark -> Int -> MH ())
                     -> MH ()
withSelectedBookmark which act = do
    l <- use (which.miBookmarkManager.bmBookmarkList)
    case listSelectedElement l of
        Nothing -> return ()
        Just (i, b) -> do
            bs <- use (which.miBookmarkManager.bmBookmarkList.listElementsL)
            act bs b i
