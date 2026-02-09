{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageSelect
  ( messageSelectKeybindings
  , messageListingKeybindings
  , messageSelectKeyHandlers
  , onEventMessageSelect
  , onEventMessageSelectDeleteConfirm
  , editingContextSensitiveOptions
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Network.Mattermost.Types ( TeamId, UserId )

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

messageListingKeybindings :: TeamId
                          -> Lens' ChatState (MessageListing n)
                          -> KeyConfig KeyEvent
                          -> KeyDispatcher KeyEvent MH
messageListingKeybindings tId which kc =
    unsafeKeyDispatcher kc (messageSelectCommonKeyHandlers tId which)

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
    handlerForOption which <$> editingContextSensitiveOptions tId which

handlerForOption :: Lens' ChatState (MessageInterface n i)
                 -> (KeyEvent, T.Text, T.Text, UserId -> Message -> Bool, MH ())
                 -> MHKeyEventHandler
handlerForOption which (ev, _, desc, canUse, act) =
    onEvent ev desc $ do
        myId <- gets myUserId
        withSelectedMessage which $ \msg ->
            when (canUse myId msg) act

editingContextSensitiveOptions :: TeamId
                               -> Lens' ChatState (MessageInterface n i)
                               -> [(KeyEvent, T.Text, T.Text, UserId -> Message -> Bool, MH ())]
editingContextSensitiveOptions tId which =
    [ (ReplyMessageEvent, "reply", "Begin composing a reply to the selected message",
         const isReplyable, beginReplyCompose which)

    , (EditMessageEvent, "edit", "Begin editing the selected message",
         \uId m -> isMyMessage uId m && isEditable m,
         beginEditMessage which)

    , (DeleteMessageEvent, "delete", "Delete the selected message (with confirmation)",
         \uId m -> isMyMessage uId m && isDeletable m,
         beginConfirmDeleteSelectedMessage tId which)

    , (OpenThreadEvent, "thread", "Open the selected message's thread in a thread window",
         const isReplyable, openThreadWindow tId which)

    , (FillGapEvent, "load messages", "Fetch messages for the selected gap",
         const isGap, fillSelectedGap which)

    , (OpenMessageInExternalEditorEvent, "open in editor", "Open the message's source in $EDITOR",
         const (not . isGap), openSelectedMessageInEditor which)

    ]
