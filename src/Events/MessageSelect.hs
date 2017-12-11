module Events.MessageSelect where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Events.Keybindings
import State

messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: Vty.Event -> MH ()
onEventMessageSelect =
  handleKeyboardEvent messageSelectKeybindings $ \ _ -> return ()

onEventMessageSelectDeleteConfirm :: Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage
    csMode .= Main
onEventMessageSelectDeleteConfirm _ =
    csMode .= Main

messageSelectKeybindings :: KeyConfig -> [Keybinding]
messageSelectKeybindings = mkKeybindings
    [ mkKb CancelEvent "Cancel message selection" $
        csMode .= Main

    , mkKb SelectUpEvent "Select the previous message" messageSelectUp
    , mkKb SelectDownEvent "Select the next message" messageSelectDown
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
         beginUpdateMessage

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)"
         beginConfirmDeleteSelectedMessage

    , mkKb YankMessageEvent "Copy a verbatim section to the clipboard"
         copyVerbatimToClipboard

    , mkKb FlagMessageEvent "Flag the selected message"
         flagSelectedMessage

    ]
