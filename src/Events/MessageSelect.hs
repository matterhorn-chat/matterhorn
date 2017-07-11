module Events.MessageSelect where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: Vty.Event -> MH ()
onEventMessageSelect e | Just kb <- lookupKeybinding e messageSelectKeybindings =
  kbAction kb
onEventMessageSelect _ = return ()

onEventMessageSelectDeleteConfirm :: Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage
    csMode .= Main
onEventMessageSelectDeleteConfirm _ =
    csMode .= Main

messageSelectKeybindings :: [Keybinding]
messageSelectKeybindings =
    [ KB "Cancel message selection"
         (Vty.EvKey Vty.KEsc []) $ csMode .= Main

    , KB "Cancel message selection"
         (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $
         csMode .= Main

    , KB "Select the previous message"
         (Vty.EvKey (Vty.KChar 'k') []) $
         messageSelectUp

    , KB "Select the previous message"
         (Vty.EvKey Vty.KUp []) $
         messageSelectUp

    , KB (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageUp []) $
         messageSelectUpBy messagesPerPageOperation

    , KB "Select the next message"
         (Vty.EvKey (Vty.KChar 'j') []) $
         messageSelectDown

    , KB "Select the next message"
         (Vty.EvKey Vty.KDown []) $
         messageSelectDown

    , KB (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageDown []) $
         messageSelectDownBy messagesPerPageOperation

    , KB "Open all URLs in the selected message"
         (Vty.EvKey (Vty.KChar 'o') []) $
         openSelectedMessageURLs

    , KB "Begin composing a reply to the selected message"
         (Vty.EvKey (Vty.KChar 'r') []) $
         beginReplyCompose

    , KB "Begin editing the selected message"
         (Vty.EvKey (Vty.KChar 'e') []) $
         beginUpdateMessage

    , KB "Delete the selected message (with confirmation)"
         (Vty.EvKey (Vty.KChar 'd') []) $
         beginConfirmDeleteSelectedMessage

    , KB "Copy a verbatim section to the clipboard"
         (Vty.EvKey (Vty.KChar 'y') []) $
         copyVerbatimToClipboard

    , KB "Flag the selected message"
         (Vty.EvKey (Vty.KChar 'f') []) $
         flagSelectedMessage

    ]
