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

import Matterhorn.Types
import Matterhorn.Types.Common ( sanitizeUserText )

drawManageChannelBookmarks :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
drawManageChannelBookmarks st which =
    renderList renderBookmark True (st^.which.miBookmarkManager.bmBookmarkList)

renderBookmark :: Bool -> Bookmark -> Widget Name
renderBookmark _ b =
    vLimit 1 $
    hBox [ hLimit 35 $
           padRight Max $
           txt (sanitizeUserText $ bookmarkDisplayName b)

         , padRight Max $
           case bookmarkContents b of
             BookmarkLink url ->   txt "Link: " <+> (txt $ sanitizeUserText url)
             BookmarkFile fInfo -> txt "File: " <+> (txt $ fileInfoName fInfo)
         ]
