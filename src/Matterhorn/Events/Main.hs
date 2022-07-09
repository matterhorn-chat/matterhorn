{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.Main where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollBy )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.HelpTopics
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.MessageInterface
import           Matterhorn.Events.ThreadWindow
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Channels
import           Matterhorn.State.Editing
import           Matterhorn.State.Help
import           Matterhorn.State.Teams
import           Matterhorn.State.PostListWindow ( enterFlaggedPostListMode )
import           Matterhorn.Types

onEventMain :: TeamId -> Vty.Event -> MH ()
onEventMain tId =
    void .
    handleEventWith [ handleKeyboardEvent (mainKeybindings tId)
                    , \e -> do
                        st <- use id
                        case st^.csTeam(tId).tsMessageInterfaceFocus of
                            FocusThread -> onEventThreadWindow tId e
                            FocusCurrentChannel -> do
                                mCid <- use (csCurrentChannelId(tId))
                                case mCid of
                                    Nothing -> return False
                                    Just cId -> handleMessageInterfaceEvent tId (csChannelMessageInterface(cId)) e
                    ]

mainKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap
mainKeybindings tId = mkKeybindings (mainKeyHandlers tId)

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

    , mkKb ChangeMessageEditorFocus
        "Cycle between message editors when a thread is open" $
        cycleTeamMessageInterfaceFocus tId

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
    ]
