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

import           Brick.Keybindings
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.MessageListing
import           Matterhorn.State.MessageSelect
import           Matterhorn.Types


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
                         -> KeyDispatcher KeyEvent MH
messageSelectKeybindings tId which kc =
    unsafeKeyDispatcher kc (messageSelectKeyHandlers tId which)

messageSelectKeyHandlers :: TeamId
                         -> Lens' ChatState (MessageInterface n i)
                         -> [MHKeyEventHandler]
messageSelectKeyHandlers tId which =
    messageSelectCommonKeyHandlers tId (which.miListing) <>
    messageSelectEditingKeyHandlers tId which

messageSelectEditingKeyHandlers :: TeamId
                                -> Lens' ChatState (MessageInterface n i)
                                -> [MHKeyEventHandler]
messageSelectEditingKeyHandlers tId which =
    [ onEvent ReplyMessageEvent "Begin composing a reply to the selected message" $
         beginReplyCompose which

    , onEvent EditMessageEvent "Begin editing the selected message" $
         beginEditMessage which

    , onEvent DeleteMessageEvent "Delete the selected message (with confirmation)" $
         beginConfirmDeleteSelectedMessage tId which

    , onEvent OpenThreadEvent "Open the selected message's thread in a thread window" $ do
         openThreadWindow tId which

    , onEvent FillGapEvent "Fetch messages for the selected gap" $
         fillSelectedGap which

    , onEvent OpenMessageInExternalEditorEvent "Open the message's source in $EDITOR" $
         openSelectedMessageInEditor which

    ]
