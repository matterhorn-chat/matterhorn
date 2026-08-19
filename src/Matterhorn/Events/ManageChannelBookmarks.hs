{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.ManageChannelBookmarks
    ( onEventManageChannelBookmarks
    , manageChannelBookmarksKeybindings
    , manageChannelBookmarksKeyHandlers
    , handleManageChannelBookmarksEvent
    )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import           Brick.Widgets.List ( handleListEvent )

import qualified Graphics.Vty as V

import           Lens.Micro.Platform (Lens')

import           Matterhorn.Types
import           Matterhorn.State.ManageChannelBookmarks

onEventManageChannelBookmarks :: Lens' ChatState (MessageInterface Name i) -> V.Event -> MH Bool
onEventManageChannelBookmarks which =
    handleEventWith [ mhHandleKeyboardEvent (manageChannelBookmarksKeybindings which)
                    , handleManageChannelBookmarksEvent which
                    ]

handleManageChannelBookmarksEvent :: Lens' ChatState (MessageInterface Name i) -> V.Event -> MH Bool
handleManageChannelBookmarksEvent which e = do
    mhZoom (which.miBookmarkManager.bmBookmarkList) handleListEvent e
    return True

manageChannelBookmarksKeybindings :: Lens' ChatState (MessageInterface n i) -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
manageChannelBookmarksKeybindings which kc =
    unsafeKeyDispatcher kc (manageChannelBookmarksKeyHandlers which)

manageChannelBookmarksKeyHandlers :: Lens' ChatState (MessageInterface n i) -> [MHKeyEventHandler]
manageChannelBookmarksKeyHandlers which =
    [ onEvent ActivateListItemEvent "Open the selected bookmark" $
        openSelectedBookmark which

    , onEvent CancelEvent "Close bookmark manager" $
        exitManageChannelBookmarksMode which

    , onEvent ReorderBookmarkUp "Move the selected bookmark up in the list" $
        moveSelectedBookmarkUp which

    , onEvent ReorderBookmarkDown "Move the selected bookmark down in the list" $
        moveSelectedBookmarkDown which

    , onEvent DeleteBookmark "Delete the selected bookmark" $
        deleteSelectedBookmark which
    ]
