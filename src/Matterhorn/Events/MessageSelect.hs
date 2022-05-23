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

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListOverlay
import           Matterhorn.Types


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: TeamId
                     -> Lens' ChatState (MessageInterface n i)
                     -> Vty.Event
                     -> MH Bool
onEventMessageSelect tId which =
    handleKeyboardEvent (messageSelectKeybindings tId which)

onEventMessageSelectDeleteConfirm :: TeamId -> Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm tId (Vty.EvKey (Vty.KChar 'y') []) = do
    withCurrentChannel tId $ \cId _ -> do
        deleteSelectedMessage (csChannelMessageInterface(cId))
        popMode tId
onEventMessageSelectDeleteConfirm _ (Vty.EvResize {}) = do
    return ()
onEventMessageSelectDeleteConfirm tId _ = do
    popMode tId

messageSelectKeybindings :: TeamId
                         -> Lens' ChatState (MessageInterface n i)
                         -> KeyConfig
                         -> KeyHandlerMap
messageSelectKeybindings tId which =
    mkKeybindings (messageSelectKeyHandlers tId which)

messageSelectKeyHandlers :: TeamId
                         -> Lens' ChatState (MessageInterface n i)
                         -> [KeyEventHandler]
messageSelectKeyHandlers tId which =
    [ mkKb CancelEvent "Cancel message selection" $
        exitMessageSelect which

    , mkKb SelectUpEvent "Select the previous message" $
        messageSelectUp which

    , mkKb SelectDownEvent "Select the next message" $
        messageSelectDown which

    , mkKb ScrollTopEvent "Scroll to top and select the oldest message" $
        messageSelectFirst which

    , mkKb ScrollBottomEvent "Scroll to bottom and select the latest message" $
        messageSelectLast which

    , mkKb
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy which messagesPerPageOperation)

    , mkKb
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy which messagesPerPageOperation)

    , mkKb OpenMessageURLEvent "Open all URLs in the selected message" $
        openSelectedMessageURLs which

    , mkKb ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose which

    , mkKb EditMessageEvent "Begin editing the selected message" $
         beginEditMessage which

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId which

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard" $
         yankSelectedMessageVerbatim which

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard" $
         yankSelectedMessage which

    , mkKb PinMessageEvent "Toggle whether the selected message is pinned" $
         pinSelectedMessage which

    , mkKb FlagMessageEvent "Flag the selected message" $
         flagSelectedMessage which

    , mkKb ViewMessageEvent "View the selected message" $
         viewSelectedMessage tId which

    , mkKb OpenThreadEvent "Open the selected message's thread in a thread window" $ do
         openThreadWindow tId which

    , mkKb FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap which

    , mkKb ReactToMessageEvent "Post a reaction to the selected message" $ do
         mMsg <- use (to (getSelectedMessage which))
         case mMsg of
             Nothing -> return ()
             Just m -> enterReactionEmojiListOverlayMode tId m

    , mkKb CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId which
    ]
