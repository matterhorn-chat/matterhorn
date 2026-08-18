{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.ManageChannelBookmarks
  ( drawManageChannelBookmarks
  )
where

import Prelude ()
import Matterhorn.Prelude

import Brick
import Brick.Widgets.List

import Lens.Micro.Platform ( Lens' )

import Network.Mattermost.Types

import Matterhorn.Themes ( clientMessageAttr )
import Matterhorn.Types
import Matterhorn.Types.Common ( sanitizeUserText )

drawManageChannelBookmarks :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
drawManageChannelBookmarks st which =
    headerRow <=> bookmarkList
    where
        headerRow = forceAttr clientMessageAttr $
                    bookmarkListRow "Bookmark Name" "Info"
        bookmarkList = renderList renderBookmark True (st^.which.miBookmarkManager.bmBookmarkList)

bookmarkListRow :: Text -> Text -> Widget Name
bookmarkListRow displayName target =
    vLimit 1 $
    hBox [ hLimit 35 $ padRight Max $ txt displayName
         , padRight Max $ txt target
         ]

renderBookmark :: Bool -> Bookmark -> Widget Name
renderBookmark _ b =
    bookmarkListRow displayName target
    where
        displayName = sanitizeUserText $ bookmarkDisplayName b
        target = case bookmarkContents b of
            BookmarkLink url ->   "Link: " <> (sanitizeUserText url)
            BookmarkFile fInfo -> "File: " <> (fileInfoName fInfo)
