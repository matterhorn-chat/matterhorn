{-# LANGUAGE MultiWayIf #-}
module Events where

import           Prelude ()
import           Prelude.Compat

import           Brick
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

onEvent :: ChatState -> BrickEvent Name MHEvent -> EventM Name (Next ChatState)
onEvent st ev = runMHEvent st $ case ev of
  (AppEvent e) -> onAppEvent e
  (VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])) -> do
    Just vty <- mh getVtyHandle
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
  refreshLoadedChannels
onAppEvent (WSEvent we) =
  handleWSEvent we
onAppEvent (RespEvent f) = f
onAppEvent (AsyncErrEvent e) = do
  let msg = "An unexpected error has occurred! The exception encountered was:\n  " <>
            T.pack (show e) <>
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

handleWSEvent :: WebsocketEvent -> MH ()
handleWSEvent we = do
  myId <- use (csMe.userIdL)
  myTeamId <- use (csMyTeam.teamIdL)
  case weEvent we of
    WMPosted -> case wepPost (weData we) of
      Just p  -> do
          -- If the message is a header change, also update the channel
          -- metadata.
          myUserId <- use (csMe.userIdL)
          case postPropsNewHeader (p^.postPropsL) of
              Just newHeader | postType p == SystemHeaderChange ->
                  csChannel(postChannelId p).ccInfo.cdHeader .= newHeader
              _ -> return ()
          case wepMentions (weData we) of
            Just lst
              | myUserId `Set.member` lst ->
                  csChannel(postChannelId p).ccInfo.cdHasMentions .= True
            _ -> return ()
          addMessageToState p
      Nothing -> return ()
    WMPostEdited -> case wepPost (weData we) of
      Just p  -> editMessage p
      Nothing -> return ()
    WMPostDeleted -> case wepPost (weData we) of
      Just p  -> deleteMessage p
      Nothing ->  return ()
    WMStatusChange -> case wepStatus (weData we) of
      Just status -> case wepUserId (weData we) of
          Just uId -> updateStatus uId status
          Nothing -> return ()
      Nothing -> return ()
    WMUserAdded -> case webChannelId (weBroadcast we) of
      Just cId -> if wepUserId (weData we) == Just myId &&
                     wepTeamId (weData we) == Just myTeamId
                  then handleChannelInvite cId
                  else return ()
      Nothing -> return ()
    WMUserUpdated -> -- XXX
      return ()
    WMNewUser -> do
      let Just newUserId = wepUserId $ weData we
      handleNewUser newUserId
    WMUserRemoved -> -- XXX
      return ()
    WMChannelDeleted -> -- XXX
      return ()
    WMDirectAdded -> -- XXX
      return ()
    WMChannelCreated -> -- XXX
      return ()
    WMGroupAdded -> -- XXX
      return ()
    WMLeaveTeam -> -- XXX: How do we deal with this one?
      return ()
    -- An 'ephemeral message' is just MatterMost's version
    -- of our 'client message'. This can be a little bit
    -- wacky, e.g. if the user types '/shortcuts' in the
    -- browser, we'll get an ephemeral message even in
    -- MatterHorn with the browser shortcuts, but it's
    -- probably a good idea to handle these messages anyway.
    WMEphemeralMessage -> case wepPost (weData we) of
      Just p  -> do
        postInfoMessage (p^.postMessageL)
      Nothing -> return ()
    -- Right now, we don't use any server preferences in
    -- our client, but that might change
    WMPreferenceChanged -> return ()
    -- This happens whenever a user connects to the server
    -- I think all the information we need (about being
    -- online or away or what-have-you) gets represented
    -- in StatusChanged messages, so we can ignore it.
    WMHello -> return ()
    -- right now we don't show typing notifications. maybe
    -- we should? i dunno.
    WMTyping -> return ()
    -- Do we need to do anything with this?
    WMUpdateTeam -> return ()
    WMReactionAdded -> case wepReaction (weData we) of
      Just r  -> case webChannelId (weBroadcast we) of
        Just cId -> addReaction r cId
        Nothing -> return ()
      Nothing -> return ()
    WMReactionRemoved -> case wepReaction (weData we) of
      Just r  -> case webChannelId (weBroadcast we) of
        Just cId -> removeReaction r cId
        Nothing -> return ()
      Nothing -> return ()
    WMAddedToTeam -> return () -- XXX: we need to handle this
    WMWebRTC      -> return ()
    WMAuthenticationChallenge -> return ()
