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
import           Lens.Micro.Platform ( Lens' )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListOverlay
import           Matterhorn.Types


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: TeamId -> Lens' ChatState MessageSelectState -> Lens' ChatState EditState -> Vty.Event -> MH ()
onEventMessageSelect tId selWhich editWhich =
  void . handleKeyboardEvent (messageSelectKeybindings tId selWhich editWhich) (const $ return ())

onEventMessageSelectDeleteConfirm :: TeamId -> Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm tId (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage tId (channelMessageSelect(tId)) (channelEditor(tId))
    popMode tId
onEventMessageSelectDeleteConfirm tId _ = do
    popMode tId

messageSelectKeybindings :: TeamId -> Lens' ChatState MessageSelectState -> Lens' ChatState EditState -> KeyConfig -> KeyHandlerMap
messageSelectKeybindings tId selWhich editWhich = mkKeybindings (messageSelectKeyHandlers tId selWhich editWhich)

messageSelectKeyHandlers :: TeamId -> Lens' ChatState MessageSelectState -> Lens' ChatState EditState -> [KeyEventHandler]
messageSelectKeyHandlers tId selWhich editWhich =
    [ mkKb CancelEvent "Cancel message selection" $ do
        popMode tId

    , mkKb SelectUpEvent "Select the previous message" $ messageSelectUp tId selWhich
    , mkKb SelectDownEvent "Select the next message" $ messageSelectDown tId selWhich
    , mkKb ScrollTopEvent "Scroll to top and select the oldest message" $
        messageSelectFirst tId selWhich
    , mkKb ScrollBottomEvent "Scroll to bottom and select the latest message" $
        messageSelectLast tId selWhich
    , mkKb
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy tId selWhich messagesPerPageOperation)
    , mkKb
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy tId selWhich messagesPerPageOperation)

    , mkKb OpenMessageURLEvent "Open all URLs in the selected message" $
        openSelectedMessageURLs tId selWhich

    , mkKb ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose tId selWhich editWhich

    , mkKb EditMessageEvent "Begin editing the selected message" $
         beginEditMessage tId selWhich editWhich

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId selWhich

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard" $
         yankSelectedMessageVerbatim tId selWhich

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard" $
         yankSelectedMessage tId selWhich

    , mkKb PinMessageEvent "Toggle whether the selected message is pinned" $
         pinSelectedMessage tId selWhich

    , mkKb FlagMessageEvent "Flag the selected message" $
         flagSelectedMessage tId selWhich

    , mkKb ViewMessageEvent "View the selected message" $
         viewSelectedMessage tId selWhich

    , mkKb FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap tId selWhich

    , mkKb ReactToMessageEvent "Post a reaction to the selected message" $
         enterReactionEmojiListOverlayMode tId selWhich

    , mkKb CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId selWhich
    ]
