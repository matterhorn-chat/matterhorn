{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageListing
  ( messageListingKeyHandlers
  , messageSelectCommonKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude
import qualified Data.Text as T

import           Brick.Keybindings

import           Lens.Micro.Platform ( Lens', to )
import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Constants
import           Matterhorn.Types
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListWindow


messageListingKeyHandlers :: Lens' ChatState (MessageListing n)
                          -> [MHKeyEventHandler]
messageListingKeyHandlers which =
    [ onEvent EnterSelectModeEvent
        "Select a message to edit/reply/delete" $
        beginMessageSelect which

    , onEvent PageUpEvent "Page up in the message list (enters message select mode)" $ do
        beginMessageSelect which

    , onEvent SelectOldestMessageEvent "Scroll to top of message list" $ do
        beginMessageSelect which
        messageSelectFirst which

    , onEvent EnterOpenURLModeEvent "Select and open a URL from the current message list" $
        startMessageUrlSelect which

    ]

messageSelectCommonKeyHandlers :: TeamId
                               -> Lens' ChatState (MessageListing n)
                               -> [MHKeyEventHandler]
messageSelectCommonKeyHandlers tId which =
    [ onEvent CancelEvent "Cancel message selection" $
        exitMessageSelect which

    , onEvent SelectUpEvent "Select the previous message" $
        messageSelectUp which

    , onEvent SelectDownEvent "Select the next message" $
        messageSelectDown which

    , onEvent ScrollTopEvent "Scroll to top and select the oldest message" $
        messageSelectFirst which

    , onEvent ScrollBottomEvent "Scroll to bottom and select the latest message" $
        messageSelectLast which

    , onEvent
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy which messagesPerPageOperation)

    , onEvent
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy which messagesPerPageOperation)

    , onEvent OpenMessageURLEvent "Open all URLs in the selected message" $
        openSelectedMessageURLs which

    , onEvent YankMessageEvent "Copy a verbatim section or message to the clipboard" $
         yankSelectedMessageVerbatim which

    , onEvent YankWholeMessageEvent "Copy an entire message to the clipboard" $
         yankSelectedMessage which

    , onEvent PinMessageEvent "Toggle whether the selected message is pinned" $
         pinSelectedMessage which

    , onEvent FlagMessageEvent "Flag the selected message" $
         flagSelectedMessage which

    , onEvent ViewMessageEvent "View the selected message" $
         viewSelectedMessage tId which

    , onEvent ReactToMessageEvent "Post a reaction to the selected message" $ do
         mMsg <- use (to (getListingSelectedMessage which))
         case mMsg of
             Nothing -> return ()
             Just m -> enterReactionEmojiListWindowMode tId m

    , onEvent CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId which

    ]

