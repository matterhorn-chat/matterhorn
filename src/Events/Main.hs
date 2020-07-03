{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import           Prelude ()
import           Prelude.MH

import           Brick.Widgets.Edit
import qualified Graphics.Vty as Vty

import           Command
import           Events.Keybindings
import           State.Attachments
import           State.ChannelSelect
import           State.Channels
import           State.Editing
import           State.MessageSelect
import           State.PostListOverlay ( enterFlaggedPostListMode )
import           State.UrlSelect
import           Types


onEventMain :: Vty.Event -> MH ()
onEventMain =
  void . handleKeyboardEvent mainKeybindings (\ ev -> do
      resetReturnChannel
      case ev of
          (Vty.EvPaste bytes) -> handlePaste bytes
          _ -> handleEditingInput ev
  )

mainKeybindings :: KeyConfig -> KeyHandlerMap
mainKeybindings = mkKeybindings mainKeyHandlers

mainKeyHandlers :: [KeyEventHandler]
mainKeyHandlers =
    [ mkKb EnterSelectModeEvent
        "Select a message to edit/reply/delete"
        beginMessageSelect

    , mkKb ReplyRecentEvent
        "Reply to the most recent message"
        replyToLatestMessage

    , mkKb ToggleMessagePreviewEvent "Toggle message preview"
        toggleMessagePreview

    , mkKb ToggleChannelListVisibleEvent "Toggle channel list visibility"
        toggleChannelListVisibility

    , mkKb
        InvokeEditorEvent
        "Invoke *$EDITOR* to edit the current message"
        invokeExternalEditor

    , mkKb
        EnterFastSelectModeEvent
        "Enter fast channel selection mode"
         beginChannelSelect

    , mkKb
        QuitEvent
        "Quit"
        requestQuit

    , staticKb "Tab-complete forward"
         (Vty.EvKey (Vty.KChar '\t') []) $
         tabComplete Forwards

    , staticKb "Tab-complete backward"
         (Vty.EvKey (Vty.KBackTab) []) $
         tabComplete Backwards

    , mkKb
        ScrollUpEvent
        "Scroll up in the channel input history" $ do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (csEditState.cedEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward

    , mkKb
        ScrollDownEvent
        "Scroll down in the channel input history" $ do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (csEditState.cedEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward

    , mkKb PageUpEvent "Page up in the channel message list (enters message select mode)" $ do
             beginMessageSelect

    , mkKb SelectOldestMessageEvent "Scroll to top of channel message list" $ do
             beginMessageSelect
             messageSelectFirst

    , mkKb NextChannelEvent "Change to the next channel in the channel list"
         nextChannel

    , mkKb PrevChannelEvent "Change to the previous channel in the channel list"
         prevChannel

    , mkKb NextUnreadChannelEvent "Change to the next channel with unread messages or return to the channel marked '~'"
         nextUnreadChannel

    , mkKb ShowAttachmentListEvent "Show the attachment list"
         showAttachmentList

    , mkKb NextUnreadUserOrChannelEvent
         "Change to the next channel with unread messages preferring direct messages"
         nextUnreadUserOrChannel

    , mkKb LastChannelEvent "Change to the most recently-focused channel"
         recentChannel

    , staticKb "Send the current message"
         (Vty.EvKey Vty.KEnter []) $ do
             isMultiline <- use (csEditState.cedEphemeral.eesMultiline)
             case isMultiline of
                 -- Normally, this event causes the current message to
                 -- be sent. But in multiline mode we want to insert a
                 -- newline instead.
                 True -> handleEditingInput (Vty.EvKey Vty.KEnter [])
                 False -> do
                     cId <- use csCurrentChannelId
                     content <- getEditorContent
                     handleInputSubmission cId content

    , mkKb EnterOpenURLModeEvent "Select and open a URL posted to the current channel"
           startUrlSelect

    , mkKb ClearUnreadEvent "Clear the current channel's unread / edited indicators" $
           clearChannelUnreadStatus =<< use csCurrentChannelId

    , mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode"
           toggleMultilineEditing

    , mkKb CancelEvent "Cancel autocomplete, message reply, or edit, in that order"
         cancelAutocompleteOrReplyOrEdit

    , mkKb EnterFlaggedPostsEvent "View currently flagged posts"
         enterFlaggedPostListMode
    ]
