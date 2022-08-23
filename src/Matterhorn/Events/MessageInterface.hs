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
import           Brick.Keybindings
import           Brick.Widgets.Edit ( handleEditorEvent )

import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )
import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Events.SaveAttachmentWindow
import           Matterhorn.Events.ManageAttachments
import           Matterhorn.Events.MessageSelect
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
            handleEventWith [ mhHandleKeyboardEvent (extraEditorKeybindings which)
                            , mhHandleKeyboardEvent (messageInterfaceKeybindings which)
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
                            -> KeyDispatcher KeyEvent MH
messageInterfaceKeybindings which kc =
    unsafeKeyDispatcher kc (messageInterfaceKeyHandlers which)

messageInterfaceKeyHandlers :: Lens' ChatState (MessageInterface n i)
                            -> [MHKeyEventHandler]
messageInterfaceKeyHandlers which =
    [ onEvent EnterSelectModeEvent
        "Select a message to edit/reply/delete" $
        beginMessageSelect which

    , onEvent PageUpEvent "Page up in the message list (enters message select mode)" $ do
        beginMessageSelect which

    , onEvent SelectOldestMessageEvent "Scroll to top of message list" $ do
        beginMessageSelect which
        messageSelectFirst which

    , onEvent EnterOpenURLModeEvent "Select and open a URL from the current message list" $
        startUrlSelect which
    ]

extraEditorKeybindings :: Lens' ChatState (MessageInterface Name i)
                       -> KeyConfig KeyEvent
                       -> KeyDispatcher KeyEvent MH
extraEditorKeybindings which kc =
    unsafeKeyDispatcher kc (extraEditorKeyHandlers which)

extraEditorKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                       -> [MHKeyEventHandler]
extraEditorKeyHandlers which =
    let editWhich :: Lens' ChatState (EditState Name)
        editWhich = which.miEditor
    in [ onEvent ToggleMultiLineEvent "Toggle multi-line message compose mode" $
              toggleMultilineEditing editWhich

       , onEvent CancelEvent "Cancel autocomplete, message reply, or edit, in that order" $
            cancelAutocompleteOrReplyOrEdit editWhich

       , onEvent
           InvokeEditorEvent
           "Invoke `$EDITOR` to edit the current message" $
           invokeExternalEditor editWhich

       , onKey (bind '\t')
            "Tab-complete forward" $
            tabComplete editWhich Forwards

       , onKey (bind Vty.KBackTab)
            "Tab-complete backward" $
            tabComplete editWhich Backwards

       , onEvent ShowAttachmentListEvent "Show the attachment list" $
            showAttachmentList which

       , onKey (bind Vty.KEnter)
            "Send the current message" $ do
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    -- Normally, this event causes the current message to
                    -- be sent. But in multiline mode we want to insert a
                    -- newline instead.
                    True -> handleEditingInput editWhich (Vty.EvKey Vty.KEnter [])
                    False -> do
                        content <- getEditorContent editWhich
                        handleInputSubmission editWhich content

       , onEvent
           ScrollUpEvent
           "Scroll up in the channel input history" $ do
                -- Up in multiline mode does the usual thing; otherwise we
                -- navigate the history.
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    True -> mhZoom (editWhich.esEditor) handleEditorEvent
                                              (VtyEvent $ Vty.EvKey Vty.KUp [])
                    False -> inputHistoryBackward which

       , onEvent
           ScrollDownEvent
           "Scroll down in the channel input history" $ do
                -- Down in multiline mode does the usual thing; otherwise
                -- we navigate the history.
                isMultiline <- use (editWhich.esEphemeral.eesMultiline)
                case isMultiline of
                    True -> mhZoom (editWhich.esEditor) handleEditorEvent
                                              (VtyEvent $ Vty.EvKey Vty.KDown [])
                    False -> inputHistoryForward which
       ]

