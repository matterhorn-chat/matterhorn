{-# LANGUAGE MultiWayIf #-}
module Events where

import           Prelude ()
import           Prelude.Compat

import           Brick
import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket

import           Connection
import           State
import           State.Common
import           Types

import           Events.ShowHelp
import           Events.Main
import           Events.JoinChannel
import           Events.ChannelScroll
import           Events.ChannelSelect
import           Events.LeaveChannelConfirm
import           Events.DeleteChannelConfirm
import           Events.UrlSelect
import           Events.MessageSelect
import           Events.PostListOverlay

onEvent :: ChatState -> BrickEvent Name MHEvent -> EventM Name (Next ChatState)
onEvent st ev = runMHEvent st $ case ev of
  (AppEvent e) -> onAppEvent e
  (VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])) -> do
    vty <- mh getVtyHandle
    liftIO $ Vty.refresh vty
  (VtyEvent e) -> onVtyEvent e
  _ -> return ()

onAppEvent :: MHEvent -> MH ()
onAppEvent RefreshWebsocketEvent = do
  st <- use id
  liftIO $ connectWebsockets st
onAppEvent WebsocketDisconnect =
  csConnectionStatus .= Disconnected
onAppEvent WebsocketConnect = do
  csConnectionStatus .= Connected
  refreshChannelsAndUsers
onAppEvent BGIdle     = csWorkerIsBusy .= Nothing
onAppEvent (BGBusy n) = csWorkerIsBusy .= Just n
onAppEvent (WSEvent we) =
  handleWSEvent we
onAppEvent (RespEvent f) = f
onAppEvent (AsyncErrEvent e) = do
  let msg = "An unexpected error has occurred! The exception encountered was:\n  " <>
            T.pack (show e) <>
            "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"
  postErrorMessage msg
onAppEvent (WebsocketParseError e) = do
  let msg = "A websocket message could not be parsed:\n  " <>
            T.pack e <>
            "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"
  postErrorMessage msg

onVtyEvent :: Vty.Event -> MH ()
onVtyEvent e = do
    -- Even if we aren't showing the help UI when a resize occurs, we
    -- need to invalidate its cache entry anyway in case the new size
    -- differs from the cached size.
    case e of
        (Vty.EvResize _ _) -> do
            mh $ invalidateCacheEntry HelpText
            mh $ invalidateCacheEntry ScriptHelpText
        _ -> return ()

    mode <- use csMode
    case mode of
        Main                       -> onEventMain e
        ShowHelp _                 -> onEventShowHelp e
        ChannelSelect              -> onEventChannelSelect e
        UrlSelect                  -> onEventUrlSelect e
        LeaveChannelConfirm        -> onEventLeaveChannelConfirm e
        JoinChannel                -> onEventJoinChannel e
        ChannelScroll              -> onEventChannelScroll e
        MessageSelect              -> onEventMessageSelect e
        MessageSelectDeleteConfirm -> onEventMessageSelectDeleteConfirm e
        DeleteChannelConfirm       -> onEventDeleteChannelConfirm e
        PostListOverlay _          -> onEventPostListOverlay e

handleWSEvent :: WebsocketEvent -> MH ()
handleWSEvent we = do
    myId <- use (csMe.userIdL)
    myTeamId <- use (csMyTeam.teamIdL)
    case weEvent we of
        WMPosted
            | Just p <- wepPost (weData we) -> do
                -- If the message is a header change, also update the
                -- channel metadata.
                myUserId <- use (csMe.userIdL)
                let wasMentioned = case wepMentions (weData we) of
                      Just lst -> myUserId `Set.member` lst
                      _ -> False
                addMessageToState (RecentPost p wasMentioned) >>= postProcessMessageAdd
            | otherwise -> return ()

        WMPostEdited
            | Just p <- wepPost (weData we) -> editMessage p
            | otherwise -> return ()

        WMPostDeleted
            | Just p <- wepPost (weData we) -> deleteMessage p
            | otherwise -> return ()

        WMStatusChange
            | Just status <- wepStatus (weData we)
            , Just uId <- wepUserId (weData we) ->
                updateStatus uId status
            | otherwise -> return ()

        WMUserAdded
            | Just cId <- webChannelId (weBroadcast we) ->
                when (wepUserId (weData we) == Just myId &&
                      wepTeamId (weData we) == Just myTeamId) $
                    handleChannelInvite cId
            | otherwise -> return ()

        WMNewUser
            | Just uId <- wepUserId $ weData we -> handleNewUser uId
            | otherwise -> return ()

        WMUserRemoved
            | Just cId <- wepChannelId (weData we) ->
                when (webUserId (weBroadcast we) == Just myId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMChannelDeleted
            | Just cId <- wepChannelId (weData we) ->
                when (webTeamId (weBroadcast we) == Just myTeamId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMDirectAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        -- An 'ephemeral message' is just Mattermost's version of our
        -- 'client message'. This can be a little bit wacky, e.g.
        -- if the user types '/shortcuts' in the browser, we'll get
        -- an ephemeral message even in MatterHorn with the browser
        -- shortcuts, but it's probably a good idea to handle these
        -- messages anyway.
        WMEphemeralMessage
            | Just p <- wepPost $ weData we -> postInfoMessage $ p^.postMessageL
            | otherwise -> return ()

        -- The only preference we observe right now is flagging
        WMPreferenceChanged
            | Just pref <- wepPreferences (weData we)
            , Just fps <- mapM preferenceToFlaggedPost pref ->
              forM_ fps $ \f ->
                  updateMessageFlag (flaggedPostId f) (flaggedPostStatus f)
            | otherwise -> return ()

        WMPreferenceDeleted
            | Just pref <- wepPreferences (weData we)
            , Just fps <- mapM preferenceToFlaggedPost pref ->
              forM_ fps $ \f ->
                  updateMessageFlag (flaggedPostId f) False
            | otherwise -> return ()

        WMReactionAdded
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> addReactions cId [r]
            | otherwise -> return ()

        WMReactionRemoved
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> removeReaction r cId
            | otherwise -> return ()

        WMChannelViewed
            | Just cId <- webChannelId $ weBroadcast we -> refreshChannelById False cId
            | otherwise -> return ()

        WMChannelUpdated
            | Just cId <- webChannelId $ weBroadcast we -> refreshChannelById False cId
            | otherwise -> return ()

        WMGroupAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        -- We are pretty sure we should do something about these:
        WMAddedToTeam -> return ()

        -- We aren't sure whether there is anything we should do about
        -- these yet:
        WMUpdateTeam -> return ()
        WMUserUpdated -> return ()
        WMLeaveTeam -> return ()

        -- We deliberately ignore these events:
        WMChannelCreated -> return ()
        WMEmojiAdded -> return ()
        WMWebRTC -> return ()
        WMTyping -> return ()
        WMHello -> return ()
        WMAuthenticationChallenge -> return ()
