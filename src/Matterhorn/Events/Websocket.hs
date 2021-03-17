module Matterhorn.Events.Websocket
  ( handleWebsocketEvent
  , handleWebsocketActionResponse
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform ( preuse )

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.WebSocket

import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.Common
import           Matterhorn.State.Flagging
import           Matterhorn.State.Messages
import           Matterhorn.State.Reactions
import           Matterhorn.State.Teams
import           Matterhorn.State.Users
import           Matterhorn.Types
import           Matterhorn.Types.Common


foreachTeam :: (TeamId -> MH ()) -> MH ()
foreachTeam act = do
    ts <- use csTeams
    let myTIds = HM.keys ts
    mapM_ act myTIds

handleWebsocketEvent :: WebsocketEvent -> MH ()
handleWebsocketEvent we = do
    myId <- gets myUserId
    ts <- use csTeams
    let memberOf tId = HM.member tId ts

        -- The team ID is one of the teams we're in, or the team ID is
        -- absent, which typically indicates a DM channel event since DM
        -- channels are not associated with teams.
        inMyTeamOrDM (Just i) = memberOf i
        inMyTeamOrDM Nothing = True

        -- The team ID is one of the teams we're in. A missing team ID
        -- yields False.
        inMyTeam (Just i) = memberOf i
        inMyTeam Nothing = False

    case weEvent we of
        WMPosted
            | Just p <- wepPost (weData we) ->
                when (inMyTeamOrDM (wepTeamId (weData we))) $ do
                    let wasMentioned = maybe False (Set.member myId) $ wepMentions (weData we)
                    addNewPostedMessage $ RecentPost p wasMentioned
                    tId <- use csCurrentTeamId
                    cId <- use (csCurrentChannelId tId)
                    when (postChannelId p /= cId) $
                        showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMPostEdited
            | Just p <- wepPost (weData we) -> do
                editMessage p

                currTid <- use csCurrentTeamId
                foreachTeam $ \tId -> do
                    cId <- use (csCurrentChannelId tId)
                    when (postChannelId p == cId && tId == currTid) $
                        updateViewed False
                    when (postChannelId p /= cId) $
                        showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMPostDeleted
            | Just p <- wepPost (weData we) -> do
                deleteMessage p

                currTid <- use csCurrentTeamId
                foreachTeam $ \tId -> do
                    cId <- use (csCurrentChannelId tId)
                    when (postChannelId p == cId && tId == currTid) $
                        updateViewed False
                    when (postChannelId p /= cId) $
                        showChannelInSidebar (p^.postChannelIdL) False
            | otherwise -> return ()

        WMStatusChange
            | Just status <- wepStatus (weData we)
            , Just uId <- wepUserId (weData we) ->
                setUserStatus uId status
            | otherwise -> return ()

        -- Despite this event's name, it means "a user was added to a
        -- channel". The event that occurs when a new user is added to
        -- the server, see WMNewUser.
        WMUserAdded
            | Just cId <- webChannelId (weBroadcast we) ->
                when (wepUserId (weData we) == Just myId &&
                      inMyTeam (wepTeamId (weData we))) $
                    handleChannelInvite cId
            | otherwise -> return ()

        WMNewUser
            | Just uId <- wepUserId $ weData we ->
                handleNewUsers (Seq.singleton uId) (return ())
            | otherwise -> return ()

        WMUserRemoved
            | Just cId <- wepChannelId (weData we) ->
                when (webUserId (weBroadcast we) == Just myId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMTyping
            | Just uId <- wepUserId $ weData we
            , Just cId <- webChannelId (weBroadcast we) -> handleTypingUser uId cId
            | otherwise -> return ()

        WMChannelDeleted
            | Just cId <- wepChannelId (weData we) ->
                when (inMyTeamOrDM (webTeamId (weBroadcast we))) $
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
            | Just p <- wepPost $ weData we -> postInfoMessage (sanitizeUserText $ p^.postMessageL)
            | otherwise -> return ()

        WMPreferenceChanged
            | Just prefs <- wepPreferences (weData we) ->
                mapM_ applyPreferenceChange prefs
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
            | Just cId <- wepChannelId $ weData we -> refreshChannelById cId
            | otherwise -> return ()

        WMChannelUpdated
            | Just cId <- webChannelId $ weBroadcast we -> do
                mChan <- preuse (csChannel(cId))
                case mChan of
                    Just chan -> do
                        refreshChannelById cId
                        updateSidebar (chan^.ccInfo.cdTeamId)
                    Nothing -> return ()
            | otherwise -> return ()

        WMGroupAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        WMChannelMemberUpdated
            | Just channelMember <- wepChannelMember $ weData we ->
                  when (channelMemberUserId channelMember == myId) $
                      updateChannelNotifyProps
                      (channelMemberChannelId channelMember)
                      (channelMemberNotifyProps channelMember)
            | otherwise -> return ()

        WMAddedToTeam
            | Just tId <- wepTeamId $ weData we
            , Just uId <- wepUserId $ weData we -> do
                when (uId == myId && not (memberOf tId)) $ do
                    handleJoinTeam tId
            | otherwise -> return ()

        WMUpdateTeam
            | Just tId <- webTeamId $ weBroadcast we -> do
                when (memberOf tId) $ do
                    handleUpdateTeam tId
            | otherwise -> return ()

        WMLeaveTeam
            | Just tId <- wepTeamId $ weData we
            , Just uId <- wepUserId $ weData we -> do
                when (uId == myId && memberOf tId) $ do
                    handleLeaveTeam tId
            | otherwise -> return ()

        WMTeamDeleted -> do
            mhLog LogGeneral $ T.pack $
                "WMTeamDeleted event: " <> show we

        WMUserUpdated
            | Just user <- wepUser (weData we) -> do
                handleUserUpdated user
                cid <- use $ csCurrentChannel . ccInfo . cdChannelId
                refreshChannelById cid
            | otherwise -> return ()

        -- We deliberately ignore these events:
        WMChannelCreated -> return ()
        WMEmojiAdded -> return ()
        WMWebRTC -> return ()
        WMHello -> return ()
        WMAuthenticationChallenge -> return ()
        WMUserRoleUpdated -> return ()
        WMPluginStatusesChanged -> return ()
        WMPluginEnabled -> return ()
        WMPluginDisabled -> return ()
        WMUnknownEvent {} ->
            mhLog LogWebsocket $ T.pack $
                "Websocket event not handled due to unknown event type: " <> show we

handleWebsocketActionResponse :: WebsocketActionResponse -> MH ()
handleWebsocketActionResponse r =
    case warStatus r of
        WebsocketActionStatusOK -> return ()
