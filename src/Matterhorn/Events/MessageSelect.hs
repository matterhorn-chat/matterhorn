{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageSelect
  ( messageSelectKeybindings
  , messageSelectKeyHandlers
  , onEventMessageSelect
  , onEventMessageSelectDeleteConfirm
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens', to )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: TeamId
                     -> Lens' ChatState (MessageInterface n i)
                     -> Vty.Event
                     -> MH Bool
onEventMessageSelect tId which =
    mhHandleKeyboardEvent (messageSelectKeybindings tId which)

onEventMessageSelectDeleteConfirm :: TeamId -> Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm tId which (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage which
    popMode tId
onEventMessageSelectDeleteConfirm _ _ (Vty.EvResize {}) = do
    return ()
onEventMessageSelectDeleteConfirm tId _ _ = do
    popMode tId

messageSelectKeybindings :: TeamId
                         -> Lens' ChatState (MessageInterface n i)
                         -> KeyConfig KeyEvent
                         -> KeyHandlerMap KeyEvent MH
messageSelectKeybindings tId which =
    mkKeybindings (messageSelectKeyHandlers tId which)

messageSelectKeyHandlers :: TeamId
                         -> Lens' ChatState (MessageInterface n i)
                         -> [MHKeyEventHandler]
messageSelectKeyHandlers tId which =
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

    , onEvent ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose which

    , onEvent EditMessageEvent "Begin editing the selected message" $
         beginEditMessage which

    , onEvent DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId which

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

    , onEvent OpenThreadEvent "Open the selected message's thread in a thread window" $ do
         openThreadWindow tId which

    , onEvent FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap which

    , onEvent ReactToMessageEvent "Post a reaction to the selected message" $ do
         mMsg <- use (to (getSelectedMessage which))
         case mMsg of
             Nothing -> return ()
             Just m -> enterReactionEmojiListWindowMode tId m

    , onEvent CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId which
    ]
