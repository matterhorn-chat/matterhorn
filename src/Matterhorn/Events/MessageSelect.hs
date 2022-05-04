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

onEventMessageSelect :: TeamId -> Lens' ChatState EditState -> Vty.Event -> MH ()
onEventMessageSelect tId which =
  void . handleKeyboardEvent (messageSelectKeybindings tId which) (const $ return ())

onEventMessageSelectDeleteConfirm :: TeamId -> Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm tId (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage tId (csTeam(tId).tsEditState)
    setMode tId Main
onEventMessageSelectDeleteConfirm tId _ = do
    setMode tId Main

messageSelectKeybindings :: TeamId -> Lens' ChatState EditState -> KeyConfig -> KeyHandlerMap
messageSelectKeybindings tId which = mkKeybindings (messageSelectKeyHandlers tId which)

messageSelectKeyHandlers :: TeamId -> Lens' ChatState EditState -> [KeyEventHandler]
messageSelectKeyHandlers tId which =
    [ mkKb CancelEvent "Cancel message selection" $ do
        setMode tId Main

    , mkKb SelectUpEvent "Select the previous message" $ messageSelectUp tId
    , mkKb SelectDownEvent "Select the next message" $ messageSelectDown tId
    , mkKb ScrollTopEvent "Scroll to top and select the oldest message" $
        messageSelectFirst tId
    , mkKb ScrollBottomEvent "Scroll to bottom and select the latest message" $
        messageSelectLast tId
    , mkKb
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy tId messagesPerPageOperation)
    , mkKb
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy tId messagesPerPageOperation)

    , mkKb OpenMessageURLEvent "Open all URLs in the selected message" $
        openSelectedMessageURLs tId

    , mkKb ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose tId which

    , mkKb EditMessageEvent "Begin editing the selected message" $
         beginEditMessage tId which

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard" $
         yankSelectedMessageVerbatim tId

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard" $
         yankSelectedMessage tId

    , mkKb PinMessageEvent "Toggle whether the selected message is pinned" $
         pinSelectedMessage tId

    , mkKb FlagMessageEvent "Flag the selected message" $
         flagSelectedMessage tId

    , mkKb ViewMessageEvent "View the selected message" $
         viewSelectedMessage tId

    , mkKb FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap tId

    , mkKb ReactToMessageEvent "Post a reaction to the selected message" $
         enterReactionEmojiListOverlayMode tId

    , mkKb CopyPostLinkEvent "Copy a post's link to the clipboard" $
         copyPostLink tId
    ]
