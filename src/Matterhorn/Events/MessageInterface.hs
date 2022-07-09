{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.MessageInterface
  ( handleMessageInterfaceEvent
  , messageInterfaceKeyHandlers
  , extraEditorKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( BrickEvent(VtyEvent) )
import           Brick.Widgets.Edit ( handleEditorEvent )

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )
import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.SaveAttachmentWindow
import           Matterhorn.Events.ManageAttachments
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.UrlSelect
import           Matterhorn.State.Attachments
import           Matterhorn.State.Editing
import           Matterhorn.State.UrlSelect
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.Channels


handleMessageInterfaceEvent :: TeamId
                            -> Lens' ChatState (MessageInterface Name i)
                            -> Vty.Event
                            -> MH Bool
handleMessageInterfaceEvent tId which ev = do
    mode <- use (which.miMode)
    case mode of
        Compose ->
            handleEventWith [ handleKeyboardEvent (extraEditorKeybindings which)
                            , handleKeyboardEvent (messageInterfaceKeybindings which)
                            , \e -> do
                                case e of
                                    (Vty.EvPaste bytes) -> handlePaste (which.miEditor) bytes
                                    _ -> handleEditingInput (which.miEditor) e
                                return True
                            ] ev
        MessageSelect ->
            onEventMessageSelect tId which ev
        ShowUrlList ->
            onEventUrlSelect which ev
        SaveAttachment {} ->
            onEventSaveAttachmentWindow which ev
        ManageAttachments ->
            onEventAttachmentList which ev
        BrowseFiles ->
            onEventBrowseFile which ev

messageInterfaceKeybindings :: Lens' ChatState (MessageInterface n i)
                            -> KeyConfig KeyEvent
                            -> KeyHandlerMap
messageInterfaceKeybindings which =
    mkKeybindings (messageInterfaceKeyHandlers which)

messageInterfaceKeyHandlers :: Lens' ChatState (MessageInterface n i)
                            -> [KeyEventHandler]
messageInterfaceKeyHandlers which =
    [ mkKb EnterSelectModeEvent
        "Select a message to edit/reply/delete" $
        beginMessageSelect which

    , mkKb PageUpEvent "Page up in the message list (enters message select mode)" $ do
        beginMessageSelect which

    , mkKb SelectOldestMessageEvent "Scroll to top of message list" $ do
        beginMessageSelect which
        messageSelectFirst which

    , mkKb EnterOpenURLModeEvent "Select and open a URL from the current message list" $
        startUrlSelect which
    ]

extraEditorKeybindings :: Lens' ChatState (MessageInterface Name i)
                       -> KeyConfig KeyEvent
                       -> KeyHandlerMap
extraEditorKeybindings which =
    mkKeybindings (extraEditorKeyHandlers which)

extraEditorKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                       -> [KeyEventHandler]
extraEditorKeyHandlers which =
    let editWhich :: Lens' ChatState (EditState Name)
        editWhich = which.miEditor
    in [ mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode" $
              toggleMultilineEditing editWhich

       , mkKb CancelEvent "Cancel autocomplete, message reply, or edit, in that order" $
            cancelAutocompleteOrReplyOrEdit editWhich

       , mkKb
           InvokeEditorEvent
           "Invoke `$EDITOR` to edit the current message" $
           invokeExternalEditor editWhich

       , staticKb "Tab-complete forward"
            (Vty.EvKey (Vty.KChar '\t') []) $
            tabComplete editWhich Forwards

       , staticKb "Tab-complete backward"
            (Vty.EvKey (Vty.KBackTab) []) $
            tabComplete editWhich Backwards

       , mkKb ShowAttachmentListEvent "Show the attachment list" $
            showAttachmentList which

       , staticKb "Send the current message"
            (Vty.EvKey Vty.KEnter []) $ do
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    -- Normally, this event causes the current message to
                    -- be sent. But in multiline mode we want to insert a
                    -- newline instead.
                    True -> handleEditingInput editWhich (Vty.EvKey Vty.KEnter [])
                    False -> do
                        content <- getEditorContent editWhich
                        handleInputSubmission editWhich content

       , mkKb
           ScrollUpEvent
           "Scroll up in the channel input history" $ do
                -- Up in multiline mode does the usual thing; otherwise we
                -- navigate the history.
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    True -> mhHandleEventLensed (editWhich.esEditor) handleEditorEvent
                                              (VtyEvent $ Vty.EvKey Vty.KUp [])
                    False -> inputHistoryBackward which

       , mkKb
           ScrollDownEvent
           "Scroll down in the channel input history" $ do
                -- Down in multiline mode does the usual thing; otherwise
                -- we navigate the history.
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    True -> mhHandleEventLensed (editWhich.esEditor) handleEditorEvent
                                              (VtyEvent $ Vty.EvKey Vty.KDown [])
                    False -> inputHistoryForward which
       ]

