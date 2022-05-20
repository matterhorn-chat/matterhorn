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
import           Lens.Micro.Platform ( Lens', Traversal', to )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListOverlay
import           Matterhorn.Types


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: TeamId
                     -> Lens' ChatState MessageSelectState
                     -> Traversal' ChatState Messages
                     -> Lens' ChatState (EditState Name)
                     -> Vty.Event
                     -> MH ()
onEventMessageSelect tId selWhich msgsWhich editWhich =
    void . handleKeyboardEvent (messageSelectKeybindings tId selWhich msgsWhich editWhich)

onEventMessageSelectDeleteConfirm :: TeamId -> Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm tId (Vty.EvKey (Vty.KChar 'y') []) = do
    withCurrentChannel tId $ \cId _ -> do
        deleteSelectedMessage tId (channelMessageSelect(tId)) (csChannelMessages(cId)) (channelEditor(tId))
        popMode tId
onEventMessageSelectDeleteConfirm _ (Vty.EvResize {}) = do
    return ()
onEventMessageSelectDeleteConfirm tId _ = do
    popMode tId

messageSelectKeybindings :: TeamId
                         -> Lens' ChatState MessageSelectState
                         -> Traversal' ChatState Messages
                         -> Lens' ChatState (EditState Name)
                         -> KeyConfig
                         -> KeyHandlerMap
messageSelectKeybindings tId selWhich msgsWhich editWhich =
    mkKeybindings (messageSelectKeyHandlers tId selWhich msgsWhich editWhich)

messageSelectKeyHandlers :: TeamId
                         -> Lens' ChatState MessageSelectState
                         -> Traversal' ChatState Messages
                         -> Lens' ChatState (EditState Name)
                         -> [KeyEventHandler]
messageSelectKeyHandlers tId selWhich msgsWhich editWhich =
    [ mkKb CancelEvent "Cancel message selection" $ do
        popMode tId

    , mkKb SelectUpEvent "Select the previous message" $ messageSelectUp selWhich msgsWhich
    , mkKb SelectDownEvent "Select the next message" $ messageSelectDown selWhich msgsWhich
    , mkKb ScrollTopEvent "Scroll to top and select the oldest message" $
        messageSelectFirst selWhich msgsWhich
    , mkKb ScrollBottomEvent "Scroll to bottom and select the latest message" $
        messageSelectLast selWhich msgsWhich
    , mkKb
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy selWhich msgsWhich messagesPerPageOperation)
    , mkKb
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy selWhich msgsWhich messagesPerPageOperation)

    , mkKb OpenMessageURLEvent "Open all URLs in the selected message" $
        openSelectedMessageURLs selWhich msgsWhich

    , mkKb ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose tId selWhich msgsWhich editWhich

    , mkKb EditMessageEvent "Begin editing the selected message" $
         beginEditMessage tId selWhich msgsWhich editWhich

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId selWhich msgsWhich

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard" $
         yankSelectedMessageVerbatim tId selWhich msgsWhich

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard" $
         yankSelectedMessage tId selWhich msgsWhich

    , mkKb PinMessageEvent "Toggle whether the selected message is pinned" $
         pinSelectedMessage selWhich msgsWhich

    , mkKb FlagMessageEvent "Flag the selected message" $
         flagSelectedMessage selWhich msgsWhich

    , mkKb ViewMessageEvent "View the selected message" $
         viewSelectedMessage tId selWhich msgsWhich

    , mkKb OpenThreadEvent "Open the selected message's thread in a thread window" $ do
         openThreadWindow tId selWhich msgsWhich

    , mkKb FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap tId selWhich msgsWhich

    , mkKb ReactToMessageEvent "Post a reaction to the selected message" $ do
         mMsg <- use (to (getSelectedMessage selWhich msgsWhich))
         case mMsg of
             Nothing -> return ()
             Just m -> enterReactionEmojiListOverlayMode tId m

    , mkKb CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId selWhich msgsWhich
    ]
