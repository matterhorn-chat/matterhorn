{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageListing
  ( messageListingKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings

import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.Types
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.MessageSelect


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
