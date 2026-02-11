{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageListing
  ( messageListingKeyHandlers
  , messageSelectCommonKeyHandlers
  , contextSensitiveOptions
  )
where

import           Prelude ()
import           Matterhorn.Prelude
import qualified Data.Text as T

import           Brick.Keybindings

import           Lens.Micro.Platform ( Lens', to )
import           Network.Mattermost.Types ( TeamId, UserId )

import           Matterhorn.Constants
import           Matterhorn.Types
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.MessageListing
import           Matterhorn.State.ReactionEmojiListWindow


messageListingKeyHandlers :: Lens' ChatState (MessageListing n)
                          -> [MHKeyEventHandler]
messageListingKeyHandlers which =
    [ onEvent SelectOldestMessageEvent "Scroll to top of message list" $ do
        beginMessageSelect which
        messageSelectFirst which

    , onEvent EnterOpenURLModeEvent "Select and open a URL from the current message list" $
        startMessageUrlSelect which

    ]

messageSelectCommonKeyHandlers :: TeamId
                               -> Lens' ChatState (MessageListing n)
                               -> [MHKeyEventHandler]
messageSelectCommonKeyHandlers tId which =
    messageSelectAlwaysEnabledKeyHandlers which <>
    messageSelectContextSensitiveKeyHandlers tId which

messageSelectAlwaysEnabledKeyHandlers :: Lens' ChatState (MessageListing n)
                                      -> [MHKeyEventHandler]
messageSelectAlwaysEnabledKeyHandlers which =
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

    , onEvent PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy which messagesPerPageOperation)

    , onEvent
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy which messagesPerPageOperation)
    ]

messageSelectContextSensitiveKeyHandlers :: TeamId
                                         -> Lens' ChatState (MessageListing n)
                                         -> [MHKeyEventHandler]
messageSelectContextSensitiveKeyHandlers tId which =
    handlerForOption which <$> contextSensitiveOptions tId which

handlerForOption :: Lens' ChatState (MessageListing n)
                 -> (KeyEvent, T.Text, T.Text, UserId -> Message -> Bool, MH ())
                 -> MHKeyEventHandler
handlerForOption which (ev, _, desc, canUse, act) =
    onEvent ev desc $ do
        myId <- gets myUserId
        withListingSelectedMessage which $ \msg ->
            when (canUse myId msg) act

contextSensitiveOptions :: TeamId
                        -> Lens' ChatState (MessageListing n)
                        -> [(KeyEvent, T.Text, T.Text, UserId -> Message -> Bool, MH ())]
contextSensitiveOptions tId which =
    [ (OpenMessageURLEvent, "open URL(s)", "Open all URLs in the selected message",
         const hasURLs, openSelectedMessageURLs which)

    , (YankMessageEvent, "yank-code", "Copy a verbatim section or message to the clipboard",
         const hasVerbatimContent, yankSelectedMessageVerbatim which)

    , (YankWholeMessageEvent, "yank-all", "Copy an entire message to the clipboard",
         const (not . isGap), yankSelectedMessage which)

    , (PinMessageEvent, "pin", "Toggle whether the selected message is pinned",
         const isPinnable, pinSelectedMessage which)

    , (FlagMessageEvent, "flag", "Flag the selected message",
         const isFlaggable, flagSelectedMessage which)

    , (ViewMessageEvent, "view", "View the selected message",
         const (not . isGap), viewSelectedMessage tId which)

    , (ReactToMessageEvent, "react", "Post a reaction to the selected message",
         const isReactable,
         do mMsg <- use (to (getListingSelectedMessage which))
            case mMsg of
                Nothing -> return ()
                Just m -> enterReactionEmojiListWindowMode tId m
      )

    , (CopyPostLinkEvent, "copy-link", "Copy a post's link to the clipboard",
         const isPostMessage, copyPostLink tId which)

    ]
