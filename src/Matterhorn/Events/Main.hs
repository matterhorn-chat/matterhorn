{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.Main where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollBy )
import           Brick.Widgets.Edit
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens', Traversal' )

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
    void .
    handleEventWith [ handleKeyboardEvent (mainKeybindings tId)
                    , \e -> do
                        mCid <- use (csCurrentChannelId(tId))
                        case mCid of
                            Nothing -> return False
                            Just cId ->
                                handleKeyboardEvent (messageEditorKeybindings (channelEditor(cId))) e
                    , \e -> do
                        mCid <- use (csCurrentChannelId(tId))
                        case mCid of
                            Nothing -> return False
                            Just cId ->
                                handleKeyboardEvent (messageListingKeybindings tId (channelMessageSelect(cId))
                                                                                   (csChannelMessages(cId))
                                                                                   (Just $ FromChannel tId cId)
                                                                                   (pushMode tId $ ChannelMessageSelect cId))
                                                    e
                    , \e -> do
                        mCid <- use (csCurrentChannelId(tId))
                        case mCid of
                            Nothing -> return False
                            Just cId -> do
                                resetReturnChannel tId
                                case e of
                                    (Vty.EvPaste bytes) -> handlePaste (channelEditor(cId)) bytes
                                    _ -> handleEditingInput (channelEditor(cId)) e
                                return True
                    ]

mainKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
mainKeybindings tId = mkKeybindings (mainKeyHandlers tId)

messageListingKeybindings :: TeamId
                          -> Lens' ChatState MessageSelectState
                          -> Traversal' ChatState Messages
                          -> Maybe URLListSource
                          -> MH ()
                          -> KeyConfig
                          -> KeyHandlerMap
messageListingKeybindings tId selWhich msgsWhich urlSrc changeMode =
    mkKeybindings (messageListingKeyHandlers tId selWhich msgsWhich urlSrc changeMode)

mainKeyHandlers :: TeamId -> [KeyEventHandler]
mainKeyHandlers tId =
    [ mkKb ShowHelpEvent
        "Show this help screen" $ do
        showHelpScreen tId mainHelpTopic

    , mkKb
        EnterFastSelectModeEvent
        "Enter fast channel selection mode" $
         beginChannelSelect tId

    , mkKb
        ChannelListScrollUpEvent
        "Scroll up in the channel list" $ do
            let vp = viewportScroll $ ChannelListViewport tId
            mh $ vScrollBy vp (-1)

    , mkKb
        ChannelListScrollDownEvent
        "Scroll down in the channel list" $ do
            let vp = viewportScroll $ ChannelListViewport tId
            mh $ vScrollBy vp 1

    , mkKb
        CycleChannelListSorting
        "Cycle through channel list sorting modes" $
        cycleChannelListSortingMode tId

    , mkKb ReplyRecentEvent
        "Reply to the most recent message" $
        withCurrentChannel tId $ \cId _ ->
            replyToLatestMessage (channelEditor(cId))

    , mkKb NextChannelEvent "Change to the next channel in the channel list" $
         nextChannel tId

    , mkKb PrevChannelEvent "Change to the previous channel in the channel list" $
         prevChannel tId

    , mkKb NextUnreadChannelEvent "Change to the next channel with unread messages or return to the channel marked '~'" $
         nextUnreadChannel tId

    , mkKb NextUnreadUserOrChannelEvent
         "Change to the next channel with unread messages preferring direct messages" $
         nextUnreadUserOrChannel tId

    , mkKb LastChannelEvent "Change to the most recently-focused channel" $
         recentChannel tId

    , mkKb ClearUnreadEvent "Clear the current channel's unread / edited indicators" $ do
           withCurrentChannel tId $ \cId _ -> do
               clearChannelUnreadStatus cId

    , mkKb EnterFlaggedPostsEvent "View currently flagged posts" $
         enterFlaggedPostListMode tId

    , mkKb
        ScrollUpEvent
        "Scroll up in the channel input history" $
        withCurrentChannel tId $ \cId _ -> do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (channelEditor(cId).esEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (channelEditor(cId).esEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward tId

    , mkKb
        ScrollDownEvent
        "Scroll down in the channel input history" $
        withCurrentChannel tId $ \cId _ -> do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (channelEditor(cId).esEphemeral.eesMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (channelEditor(cId).esEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward tId
    ]

messageEditorKeybindings :: Lens' ChatState (EditState Name)
                         -> KeyConfig
                         -> KeyHandlerMap
messageEditorKeybindings editWhich =
    mkKeybindings (channelEditorKeyHandlers editWhich)

channelEditorKeyHandlers :: Lens' ChatState (EditState Name)
                         -> [KeyEventHandler]
channelEditorKeyHandlers editWhich =
    [ mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode" $
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
         withCurrentTeam $ \tId -> showAttachmentList tId editWhich

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
    ]

messageListingKeyHandlers :: TeamId
                          -> Lens' ChatState MessageSelectState
                          -> Traversal' ChatState Messages
                          -> Maybe URLListSource
                          -> MH ()
                          -> [KeyEventHandler]
messageListingKeyHandlers tId selWhich msgsWhich urlSrc changeMode =
    [ mkKb EnterSelectModeEvent
        "Select a message to edit/reply/delete" $
        beginMessageSelect selWhich msgsWhich changeMode

    , mkKb PageUpEvent "Page up in the message list (enters message select mode)" $ do
        beginMessageSelect selWhich msgsWhich changeMode

    , mkKb SelectOldestMessageEvent "Scroll to top of message list" $ do
        beginMessageSelect selWhich msgsWhich changeMode
        messageSelectFirst selWhich msgsWhich

    , mkKb EnterOpenURLModeEvent "Select and open a URL from the current message list" $
        startUrlSelect tId msgsWhich urlSrc
    ]
