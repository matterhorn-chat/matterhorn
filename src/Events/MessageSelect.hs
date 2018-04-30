module Events.MessageSelect where

import           Prelude ()
import           Prelude.MH

import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import           Events.Keybindings
import           State
import           Types


messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: Vty.Event -> MH ()
onEventMessageSelect =
  handleKeyboardEvent messageSelectKeybindings $ \ _ -> return ()

onEventMessageSelectDeleteConfirm :: Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage
    setMode Main
onEventMessageSelectDeleteConfirm _ =
    setMode Main

messageSelectKeybindings :: KeyConfig -> [Keybinding]
messageSelectKeybindings = mkKeybindings
    [ mkKb CancelEvent "Cancel message selection" $
        setMode Main

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
         beginEditMessage

    , mkKb DeleteMessageEvent "Delete the selected message (with confirmation)"
         beginConfirmDeleteSelectedMessage

    , mkKb YankMessageEvent "Copy a verbatim section or message to the clipboard"
         yankSelectedMessageVerbatim

    , mkKb YankWholeMessageEvent "Copy an entire message to the clipboard"
         yankSelectedMessage

    , mkKb FlagMessageEvent "Flag the selected message"
         flagSelectedMessage

    , mkKb ViewMessageEvent "View the selected message"
         viewSelectedMessage

    ]
