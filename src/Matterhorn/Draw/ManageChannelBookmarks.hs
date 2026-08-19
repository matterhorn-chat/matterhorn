{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.ManageChannelBookmarks
  ( drawManageChannelBookmarks
  , drawManageChannelBookmarksConfirmingDelete
  )
where

import Prelude ()
import Matterhorn.Prelude

import Data.List ( intersperse )

import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

import Lens.Micro.Platform ( Lens' )

import Network.Mattermost.Types

import Matterhorn.Draw.Util ( keyEventBindings )
import Matterhorn.Events.ManageChannelBookmarks
import Matterhorn.Themes
import Matterhorn.Types
import Matterhorn.Types.Common ( sanitizeUserText )

bookmarkNameMaxSize :: Int
bookmarkNameMaxSize = 35

drawManageChannelBookmarks :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
drawManageChannelBookmarks st which =
    headerRow <=> bookmarkList <=> padBottom (Pad 1) bottomBar
    where
        headerRow = forceAttr clientMessageAttr $
                    bookmarkListRow "Bookmark Name" "Info"
        bookmarkList = renderList renderBookmark True (st^.which.miBookmarkManager.bmBookmarkList)
        bottomBar = bookmarkManagerBottomBar st which

drawManageChannelBookmarksConfirmingDelete :: Bookmark -> Widget Name
drawManageChannelBookmarksConfirmingDelete b =
    center $
    padLeftRight 5 $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Confirm Deletion") $
    hCenter $
    padTopBottom 1 $
    vBox [ hCenter $ txt "Delete bookmark:"
         , padBottom (Pad 1) $
           hCenter $
           withDefAttr clientEmphAttr $
           txt $ sanitizeUserText $ bookmarkDisplayName b
         , hCenter $ txt "Are you sure? y/n"
         ]

bookmarkListRow :: Text -> Text -> Widget Name
bookmarkListRow displayName target =
    vLimit 1 $
    hBox [ hLimit bookmarkNameMaxSize $ padRight Max $ txt displayName
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

bookmarkManagerBottomBar :: ChatState -> Lens' ChatState (MessageInterface Name i) -> Widget Name
bookmarkManagerBottomBar st which =
    case listSelectedElement $ st^.which.miBookmarkManager.bmBookmarkList of
        Nothing -> hBorder
        Just _ ->
            let options = [ ( ev ActivateListItemEvent
                            , "open"
                            )
                          , ( ev ReorderBookmarkUp
                            , "move up"
                            )
                          , ( ev ReorderBookmarkDown
                            , "move down"
                            )
                          , ( ev DeleteBookmark
                            , "delete"
                            )
                          , ( ev CancelEvent
                            , "close"
                            )
                          ]
                ev = keyEventBindings st (manageChannelBookmarksKeybindings which)
                optionList = hBox $ intersperse (txt " ") usableOptions
                usableOptions = mkOption <$> options
                mkOption (k, desc) = withDefAttr urlSelectStatusAttr (txt k) <+> txt (":" <> desc)
            in if null usableOptions
               then hBorder
               else hBox [ hLimit 1 hBorder
                         , txt "["
                         , txt "Options: "
                         , optionList
                         , txt "]"
                         , hBorder
                         ]
