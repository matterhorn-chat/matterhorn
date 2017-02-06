module Events.MessageSelect where

import Brick
import Control.Monad ((>=>))
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMessageSelect st e | Just kb <- lookupKeybinding e messageSelectKeybindings = kbAction kb st
onEventMessageSelect st _ = continue st

onEventMessageSelectDeleteConfirm :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMessageSelectDeleteConfirm st (Vty.EvKey (Vty.KChar 'y') []) =
    deleteSelectedMessage st >>= continue
onEventMessageSelectDeleteConfirm st _ =
    continue $ st & csMode .~ Main

messageSelectKeybindings :: [Keybinding]
messageSelectKeybindings =
    [ KB "Cancel message selection"
         (Vty.EvKey Vty.KEsc []) $
         \st -> continue $ st & csMode .~ Main

    , KB "Cancel message selection"
         (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $
         \st -> continue $ st & csMode .~ Main

    , KB "Select the previous message"
         (Vty.EvKey (Vty.KChar 'k') []) $
         messageSelectUp >=> continue

    , KB "Select the previous message"
         (Vty.EvKey Vty.KUp []) $
         messageSelectUp >=> continue

    , KB (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageUp []) $
         messageSelectUpBy messagesPerPageOperation >=> continue

    , KB "Select the next message"
         (Vty.EvKey (Vty.KChar 'j') []) $
         messageSelectDown >=> continue

    , KB "Select the next message"
         (Vty.EvKey Vty.KDown []) $
         messageSelectDown >=> continue

    , KB (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageDown []) $
         messageSelectDownBy messagesPerPageOperation >=> continue

    , KB "Open all URLs in the selected message"
         (Vty.EvKey (Vty.KChar 'o') []) $
         openSelectedMessageURLs >=> continue

    , KB "Begin composing a reply to the selected message"
         (Vty.EvKey (Vty.KChar 'r') []) $
         beginReplyCompose >=> continue

    , KB "Begin editing the selected message"
         (Vty.EvKey (Vty.KChar 'e') []) $
         beginUpdateMessage >=> continue

    , KB "Delete the selected message (with confirmation)"
         (Vty.EvKey (Vty.KChar 'd') []) $
         beginConfirmDeleteSelectedMessage >=> continue

    , KB "Copy a verbatim section to the clipboard"
         (Vty.EvKey (Vty.KChar 'y') []) $
         copyVerbatimToClipboard >=> continue
    ]
