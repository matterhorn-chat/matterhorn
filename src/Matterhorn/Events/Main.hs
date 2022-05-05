{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Events.Main where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollBy )
import           Brick.Widgets.Edit
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.HelpTopics
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Attachments
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Channels
import           Matterhorn.State.Editing
import           Matterhorn.State.Help
import           Matterhorn.State.MessageSelect
import           Matterhorn.State.PostListOverlay ( enterFlaggedPostListMode )
import           Matterhorn.State.UrlSelect
import           Matterhorn.Types


onEventMain :: TeamId -> Vty.Event -> MH ()
onEventMain tId =
  void . handleKeyboardEvent (mainKeybindings tId) (\ ev -> do
      resetReturnChannel tId
      case ev of
          (Vty.EvPaste bytes) -> handlePaste (channelEditor(tId)) bytes
          _ -> handleEditingInput tId (channelEditor(tId)) ev
  )

mainKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
mainKeybindings tId = mkKeybindings (mainKeyHandlers tId)

mainKeyHandlers :: TeamId -> [KeyEventHandler]
mainKeyHandlers tId =
    [ mkKb ShowHelpEvent
        "Show this help screen" $ do
        showHelpScreen tId mainHelpTopic

    , mkKb EnterSelectModeEvent
        "Select a message to edit/reply/delete" $
        beginMessageSelect tId (channelMessageSelect(tId))

    , mkKb ReplyRecentEvent
        "Reply to the most recent message" $
        replyToLatestMessage tId (channelEditor(tId))

    , mkKb
        InvokeEditorEvent
        "Invoke `$EDITOR` to edit the current message" $
        invokeExternalEditor (channelEditor(tId))

    , mkKb
        EnterFastSelectModeEvent
        "Enter fast channel selection mode" $
         beginChannelSelect tId

    , staticKb "Tab-complete forward"
         (Vty.EvKey (Vty.KChar '\t') []) $
         tabComplete tId (channelEditor(tId)) Forwards

    , staticKb "Tab-complete backward"
         (Vty.EvKey (Vty.KBackTab) []) $
         tabComplete tId (channelEditor(tId)) Backwards

    , mkKb
        ChannelListScrollUpEvent
        "Scroll up in the channel list" $ do
            let vp = viewportScroll $ ChannelList tId
            mh $ vScrollBy vp (-1)

    , mkKb
        ChannelListScrollDownEvent
        "Scroll down in the channel list" $ do
            let vp = viewportScroll $ ChannelList tId
            mh $ vScrollBy vp 1

    , mkKb
        CycleChannelListSorting
        "Cycle through channel list sorting modes" $
        cycleChannelListSortingMode tId

    , mkKb
        ScrollUpEvent
        "Scroll up in the channel input history" $ do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (channelEditor(tId).cedEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (channelEditor(tId).cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward tId

    , mkKb
        ScrollDownEvent
        "Scroll down in the channel input history" $ do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (channelEditor(tId).cedEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (channelEditor(tId).cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward tId

    , mkKb PageUpEvent "Page up in the channel message list (enters message select mode)" $ do
             beginMessageSelect tId (channelMessageSelect(tId))

    , mkKb SelectOldestMessageEvent "Scroll to top of channel message list" $ do
             beginMessageSelect tId (channelMessageSelect(tId))
             messageSelectFirst tId (channelMessageSelect(tId))

    , mkKb NextChannelEvent "Change to the next channel in the channel list" $
         nextChannel tId

    , mkKb PrevChannelEvent "Change to the previous channel in the channel list" $
         prevChannel tId

    , mkKb NextUnreadChannelEvent "Change to the next channel with unread messages or return to the channel marked '~'" $
         nextUnreadChannel tId

    , mkKb ShowAttachmentListEvent "Show the attachment list" $
         showAttachmentList tId (channelEditor(tId)) ManageAttachmentsBrowseFiles

    , mkKb NextUnreadUserOrChannelEvent
         "Change to the next channel with unread messages preferring direct messages" $
         nextUnreadUserOrChannel tId

    , mkKb LastChannelEvent "Change to the most recently-focused channel" $
         recentChannel tId

    , staticKb "Send the current message"
         (Vty.EvKey Vty.KEnter []) $ do
             isMultiline <- use (channelEditor(tId).cedEphemeral.eesMultiline)
             case isMultiline of
                 -- Normally, this event causes the current message to
                 -- be sent. But in multiline mode we want to insert a
                 -- newline instead.
                 True -> handleEditingInput tId (channelEditor(tId)) (Vty.EvKey Vty.KEnter [])
                 False -> do
                     withCurrentChannel tId $ \cId _ -> do
                         content <- getEditorContent (channelEditor(tId))
                         handleInputSubmission tId (channelEditor(tId)) cId content

    , mkKb EnterOpenURLModeEvent "Select and open a URL posted to the current channel" $
           startUrlSelect tId

    , mkKb ClearUnreadEvent "Clear the current channel's unread / edited indicators" $ do
           withCurrentChannel tId $ \cId _ -> do
               clearChannelUnreadStatus cId

    , mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode" $
           toggleMultilineEditing (channelEditor(tId))

    , mkKb CancelEvent "Cancel autocomplete, message reply, or edit, in that order" $
         cancelAutocompleteOrReplyOrEdit tId (channelEditor(tId))

    , mkKb EnterFlaggedPostsEvent "View currently flagged posts" $
         enterFlaggedPostListMode tId
    ]
