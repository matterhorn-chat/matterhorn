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

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.ReactionEmojiListOverlay
import           Matterhorn.Types


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: Vty.Event -> MH ()
onEventMessageSelect =
  void . handleKeyboardEvent messageSelectKeybindings (const $ return ())

onEventMessageSelectDeleteConfirm :: Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage
    setMode Main
onEventMessageSelectDeleteConfirm _ =
    setMode Main

messageSelectKeybindings :: KeyConfig -> KeyHandlerMap
messageSelectKeybindings = mkKeybindings messageSelectKeyHandlers

messageSelectKeyHandlers :: [KeyEventHandler]
messageSelectKeyHandlers =
    [ mkKb CancelEvent "Cancel message selection" $
        setMode Main

    , mkKb SelectUpEvent "Select the previous message" messageSelectUp
    , mkKb SelectDownEvent "Select the next message" messageSelectDown
    , mkKb ScrollTopEvent "Scroll to top and select the oldest message"
        messageSelectFirst
    , mkKb ScrollBottomEvent "Scroll to bottom and select the latest message"
        messageSelectLast
    , mkKb
        PageUpEvent
        (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
        (messageSelectUpBy messagesPerPageOperation)
    , mkKb
        PageDownEvent
        (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
        (messageSelectDownBy messagesPerPageOperation)

    , mkKb OpenMessageURLEvent "Open all URLs in the selected message"
        openSelectedMessageURLs

    , mkKb ReplyMessageEvent "Begin composing a reply to the selected message"
         beginReplyCompose

    , mkKb EditMessageEvent "Begin editing the selected message"
         beginEditMessage

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)"
         beginConfirmDeleteSelectedMessage

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard"
         yankSelectedMessageVerbatim

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard"
         yankSelectedMessage

    , mkKb PinMessageEvent "Toggle whether the selected message is pinned"
         pinSelectedMessage

    , mkKb FlagMessageEvent "Flag the selected message"
         flagSelectedMessage

    , mkKb ViewMessageEvent "View the selected message"
         viewSelectedMessage

    , mkKb FillGapEvent "Fetch messages for the selected gap"
         fillSelectedGap

    , mkKb CopyPostLinkEvent "Copy a post's link to the clipboard"
         copyPostLink

    , mkKb ReactToMessageEvent "Post a reaction to the selected message"
         enterReactionEmojiListOverlayMode

    ]
