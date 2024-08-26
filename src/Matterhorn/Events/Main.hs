{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.Main
  ( onEventMain
  , mainKeyHandlers
  , mainKeybindings
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollBy )
import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.HelpTopics
import           Matterhorn.Events.MessageInterface
import           Matterhorn.Events.ThreadWindow
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelList ( updateSidebar )
import           Matterhorn.State.Help
import           Matterhorn.State.Teams
import           Matterhorn.State.PostListWindow ( enterFlaggedPostListMode )
import           Matterhorn.Types

onEventMain :: TeamId -> Vty.Event -> MH ()
onEventMain tId =
    void .
    handleEventWith [ mhHandleKeyboardEvent (mainKeybindings tId)
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

mainKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
mainKeybindings tId kc = unsafeKeyDispatcher kc (mainKeyHandlers tId)

mainKeyHandlers :: TeamId -> [MHKeyEventHandler]
mainKeyHandlers tId =
    [ onEvent ShowHelpEvent
        "Show this help screen" $ do
        showHelpScreen tId mainHelpTopic

    , onEvent
        EnterFastSelectModeEvent
        "Enter fast channel selection mode" $
         beginChannelSelect tId

    , onEvent
        ChannelListScrollUpEvent
        "Scroll up in the channel list" $ do
            let vp = viewportScroll $ ChannelListViewport tId
            mh $ vScrollBy vp (-1)

    , onEvent
        ChannelListScrollDownEvent
        "Scroll down in the channel list" $ do
            let vp = viewportScroll $ ChannelListViewport tId
            mh $ vScrollBy vp 1

    , onEvent
        CycleChannelListSorting
        "Cycle through channel list sorting modes" $
        cycleChannelListSortingMode tId

    , onEvent ChangeMessageEditorFocus
        "Cycle between message editors when a thread is open" $
        cycleTeamMessageInterfaceFocus tId

    , onEvent NextChannelEvent "Change to the next channel in the channel list" $
         nextChannel tId

    , onEvent PrevChannelEvent "Change to the previous channel in the channel list" $
         prevChannel tId

    , onEvent NextUnreadChannelEvent "Change to the next channel with unread messages or return to the channel marked '~'" $
         nextUnreadChannel tId

    , onEvent NextUnreadUserOrChannelEvent
         "Change to the next channel with unread messages preferring direct messages" $
         nextUnreadUserOrChannel tId

    , onEvent LastChannelEvent "Change to the most recently-focused channel" $
         recentChannel tId

    , onEvent ClearUnreadEvent "Clear the current channel's unread / edited indicators" $ do
           withCurrentChannel tId $ \cId _ -> do
               clearChannelUnreadStatus cId
               updateSidebar $ Just tId

    , onEvent EnterFlaggedPostsEvent "View currently flagged posts" $
         enterFlaggedPostListMode tId
    ]
