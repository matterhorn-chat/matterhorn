{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Matterhorn.State.Channels
  ( updateViewed
  , refreshChannel
  , refreshChannelsAndUsers
  , setFocus
  , refreshChannelById
  , applyPreferenceChange
  , leaveChannel
  , leaveCurrentChannel
  , getNextUnreadChannel
  , getNextUnreadUserOrChannel
  , nextUnreadChannel
  , nextUnreadUserOrChannel
  , createOrFocusDMChannel
  , prevChannel
  , nextChannel
  , recentChannel
  , setReturnChannel
  , resetReturnChannel
  , hideDMChannel
  , createGroupChannel
  , showGroupChannelPref
  , channelHistoryForward
  , channelHistoryBackward
  , handleNewChannel
  , createOrdinaryChannel
  , handleChannelInvite
  , addUserByNameToCurrentChannel
  , addUserToCurrentChannel
  , removeUserFromCurrentChannel
  , removeChannelFromState
  , isRecentChannel
  , isReturnChannel
  , isCurrentChannel
  , deleteCurrentChannel
  , startLeaveCurrentChannel
  , joinChannel
  , joinChannel'
  , joinChannelByName
  , changeChannelByName
  , setChannelTopic
  , getCurrentChannelTopic
  , beginCurrentChannelDeleteConfirm
  , toggleExpandedChannelTopics
  , updateChannelNotifyProps
  , renameChannelUrl
  , toggleChannelFavoriteStatus
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollToBeginning
                            , invalidateCache, invalidateCacheEntry )
import           Brick.Widgets.Edit ( applyEdit, getEditContents, editContentsL )
import           Control.Concurrent.Async ( runConcurrently, Concurrently(..) )
import           Control.Exception ( SomeException, try )
import           Data.Char ( isAlphaNum )
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import           Data.List ( nub )
import           Data.Maybe ( fromJust )
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Text.Zipper ( textZipper, clearZipper, insertMany, gotoEOL )
import           Data.Time.Clock ( getCurrentTime )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.Constants ( normalChannelSigil )
import           Matterhorn.InputHistory
import           Matterhorn.State.Common
import {-# SOURCE #-} Matterhorn.State.Messages ( fetchVisibleIfNeeded )
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Users
import           Matterhorn.State.Flagging
import           Matterhorn.Types
import           Matterhorn.Types.Common
import           Matterhorn.Zipper ( Zipper )
import qualified Matterhorn.Zipper as Z


updateViewed :: Bool -> MH ()
updateViewed updatePrev = do
    csCurrentChannel.ccInfo.cdMentionCount .= 0
    tId <- use csCurrentTeamId
    updateViewedChan updatePrev =<< use (csCurrentChannelId tId)

-- | When a new channel has been selected for viewing, this will
-- notify the server of the change, and also update the local channel
-- state to set the last-viewed time for the previous channel and
-- update the viewed time to now for the newly selected channel.
--
-- The boolean argument indicates whether the view time of the previous
-- channel (if any) should be updated, too. We typically want to do that
-- only on channel switching; when we just want to update the view time
-- of the specified channel, False should be provided.
updateViewedChan :: Bool -> ChannelId -> MH ()
updateViewedChan updatePrev cId = use csConnectionStatus >>= \case
    -- Only do this if we're connected to avoid triggering noisy
    -- exceptions.
    Connected -> do
        withChannel cId $ \chan -> do
            pId <- if updatePrev
                   then do
                       case chan^.ccInfo.cdTeamId of
                           Just tId -> use (csTeam(tId).tsRecentChannel)
                           Nothing -> use (csCurrentTeam.tsRecentChannel)
                   else return Nothing
            doAsyncChannelMM Preempt cId
              (\s c -> MM.mmViewChannel UserMe c pId s)
              (\c () -> Just $ setLastViewedFor pId c)
    Disconnected ->
        -- Cannot update server; make no local updates to avoid getting
        -- out of sync with the server. Assumes that this is a temporary
        -- break in connectivity and that after the connection is
        -- restored, the user's normal activities will update state as
        -- appropriate. If connectivity is permanently lost, managing
        -- this state is irrelevant.
        return ()

toggleExpandedChannelTopics :: MH ()
toggleExpandedChannelTopics = do
    mh invalidateCache
    csResources.crConfiguration.configShowExpandedChannelTopicsL %= not

-- | If the current channel is a DM channel with a single user or a
-- group of users, hide it from the sidebar and adjust the server-side
-- preference to hide it persistently. Note that this does not actually
-- hide the channel in our UI; we hide it in response to the preference
-- change websocket event triggered by this function's API interaction
-- with the server.
--
-- If the current channel is any other kind of channel, complain with a
-- usage error.
hideDMChannel :: ChannelId -> MH ()
hideDMChannel cId = do
    me <- gets myUser
    session <- getSession
    withChannel cId $ \chan -> do
        case chan^.ccInfo.cdType of
            Direct -> do
                let pref = showDirectChannelPref (me^.userIdL) uId False
                    Just uId = chan^.ccInfo.cdDMUserId
                csChannel(cId).ccInfo.cdSidebarShowOverride .= Nothing
                doAsyncWith Preempt $ do
                    MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                    return Nothing
            Group -> do
                let pref = hideGroupChannelPref cId (me^.userIdL)
                csChannel(cId).ccInfo.cdSidebarShowOverride .= Nothing
                doAsyncWith Preempt $ do
                    MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                    return Nothing
            _ -> do
                mhError $ GenericError "Cannot hide this channel. Consider using /leave instead."

-- | Called on async completion when the currently viewed channel has
-- been updated (i.e., just switched to this channel) to update local
-- state.
setLastViewedFor :: Maybe ChannelId -> ChannelId -> MH ()
setLastViewedFor prevId cId = do
    chan <- use (csChannels.to (findChannelById cId))
    -- Update new channel's viewed time, creating the channel if needed
    case chan of
        Nothing ->
            -- It's possible for us to get spurious WMChannelViewed
            -- events from the server, e.g. for channels that have been
            -- deleted. So here we ignore the request since it's hard to
            -- detect it before this point.
            return ()
        Just _  ->
          -- The server has been sent a viewed POST update, but there is
          -- no local information on what timestamp the server actually
          -- recorded. There are a couple of options for setting the
          -- local value of the viewed time:
          --
          --   1. Attempting to locally construct a value, which would
          --      involve scanning all (User) messages in the channel
          --      to find the maximum of the created date, the modified
          --      date, or the deleted date, and assuming that maximum
          --      mostly matched the server's viewed time.
          --
          --   2. Issuing a channel metadata request to get the server's
          --      new concept of the viewed time.
          --
          --   3. Having the "chan/viewed" POST that was just issued
          --      return a value from the server. See
          --      https://github.com/mattermost/platform/issues/6803.
          --
          -- Method 3 would be the best and most lightweight. Until that
          -- is available, Method 2 will be used. The downside to Method
          -- 2 is additional client-server messaging, and a delay in
          -- updating the client data, but it's also immune to any new
          -- or removed Message date fields, or anything else that would
          -- contribute to the viewed/updated times on the server.
          doAsyncChannelMM Preempt cId (\ s _ ->
                                           (,) <$> MM.mmGetChannel cId s
                                               <*> MM.mmGetChannelMember cId UserMe s)
          (\pcid (cwd, member) -> Just $ csChannel(pcid).ccInfo %= channelInfoFromChannelWithData cwd member)

    -- Update the old channel's previous viewed time (allows tracking of
    -- new messages)
    case prevId of
      Nothing -> return ()
      Just p -> clearChannelUnreadStatus p

-- | Refresh information about all channels and users. This is usually
-- triggered when a reconnect event for the WebSocket to the server
-- occurs.
refreshChannelsAndUsers :: MH ()
refreshChannelsAndUsers = do
    session <- getSession
    me <- gets myUser
    knownUsers <- gets allUserIds
    ts <- use csTeams
    doAsyncWith Preempt $ do
      pairs <- forM (HM.keys ts) $ \tId -> do
          runConcurrently $ (,)
              <$> Concurrently (MM.mmGetChannelsForUser UserMe tId session)
              <*> Concurrently (MM.mmGetChannelMembersForUser UserMe tId session)

      let (chans, datas) = (mconcat $ fst <$> pairs, mconcat $ snd <$> pairs)

      -- Collect all user IDs associated with DM channels so we can
      -- bulk-fetch their user records.
      let dmUsers = catMaybes $ flip map (F.toList chans) $ \chan ->
              case chan^.channelTypeL of
                  Direct -> case userIdForDMChannel (userId me) (sanitizeUserText $ channelName chan) of
                        Nothing -> Nothing
                        Just otherUserId -> Just otherUserId
                  _ -> Nothing
          uIdsToFetch = nub $ userId me : knownUsers <> dmUsers

          dataMap = HM.fromList $ toList $ (\d -> (channelMemberChannelId d, d)) <$> datas
          mkPair chan = (chan, fromJust $ HM.lookup (channelId chan) dataMap)
          chansWithData = mkPair <$> chans

      return $ Just $
          -- Fetch user data associated with DM channels
          handleNewUsers (Seq.fromList uIdsToFetch) $ do
              -- Then refresh all loaded channels
              forM_ chansWithData $ uncurry (refreshChannel SidebarUpdateDeferred)
              updateSidebar Nothing

-- | Refresh information about a specific channel.  The channel
-- metadata is refreshed, and if this is a loaded channel, the
-- scrollback is updated as well.
--
-- The sidebar update argument indicates whether this refresh should
-- also update the sidebar. Ordinarily you want this, so pass
-- SidebarUpdateImmediate unless you are very sure you know what you are
-- doing, i.e., you are very sure that a call to refreshChannel will
-- be followed immediately by a call to updateSidebar. We provide this
-- control so that channel refreshes can be batched and then a single
-- updateSidebar call can be used instead of the default behavior of
-- calling it once per refreshChannel call, which is the behavior if the
-- immediate setting is passed here.
refreshChannel :: SidebarUpdate -> Channel -> ChannelMember -> MH ()
refreshChannel upd chan member = do
    ts <- use csTeams
    let ourTeams = HM.keys ts
        isOurTeam = case channelTeamId chan of
            Nothing -> True
            Just tId -> tId `elem` ourTeams

    case isOurTeam of
        False -> return ()
        True -> do
            let cId = getId chan
            -- If this channel is unknown, register it first.
            mChan <- preuse (csChannel(cId))
            when (isNothing mChan) $
                handleNewChannel False upd chan member

            updateChannelInfo cId chan member

handleNewChannel :: Bool -> SidebarUpdate -> Channel -> ChannelMember -> MH ()
handleNewChannel = handleNewChannel_ True

handleNewChannel_ :: Bool
                  -- ^ Whether to permit this call to recursively
                  -- schedule itself for later if it can't locate
                  -- a DM channel user record. This is to prevent
                  -- uncontrolled recursion.
                  -> Bool
                  -- ^ Whether to switch to the new channel once it has
                  -- been installed.
                  -> SidebarUpdate
                  -- ^ Whether to update the sidebar, in case the caller
                  -- wants to batch these before updating it. Pass
                  -- SidebarUpdateImmediate unless you know what
                  -- you are doing, i.e., unless you intend to call
                  -- updateSidebar yourself after calling this.
                  -> Channel
                  -- ^ The channel to install.
                  -> ChannelMember
                  -> MH ()
handleNewChannel_ permitPostpone switch sbUpdate nc member = do
    -- Only add the channel to the state if it isn't already known.
    me <- gets myUser
    mChan <- preuse (csChannel(getId nc))
    case mChan of
        Just _ -> when switch $ setFocus (getId nc)
        Nothing -> do
            -- Create a new ClientChannel structure
            cChannel <- (ccInfo %~ channelInfoFromChannelWithData nc member) <$>
                       makeClientChannel (me^.userIdL) nc

            st <- use id

            -- Add it to the message map, and to the name map so we
            -- can look it up by name. The name we use for the channel
            -- depends on its type:
            let chType = nc^.channelTypeL

            -- Get the channel name. If we couldn't, that means we have
            -- async work to do before we can register this channel (in
            -- which case abort because we got rescheduled).
            register <- case chType of
                Direct -> case userIdForDMChannel (myUserId st) (sanitizeUserText $ channelName nc) of
                    Nothing -> return True
                    Just otherUserId ->
                        case userById otherUserId st of
                            -- If we found a user ID in the channel
                            -- name string but don't have that user's
                            -- metadata, postpone adding this channel
                            -- until we have fetched the metadata. This
                            -- can happen when we have a channel record
                            -- for a user that is no longer in the
                            -- current team. To avoid recursion due to a
                            -- problem, ensure that the rescheduled new
                            -- channel handler is not permitted to try
                            -- this again.
                            --
                            -- If we're already in a recursive attempt
                            -- to register this channel and still
                            -- couldn't find a username, just bail and
                            -- use the synthetic name (this has the same
                            -- problems as above).
                            Nothing -> do
                                case permitPostpone of
                                    False -> return True
                                    True -> do
                                        mhLog LogAPI $ T.pack $ "handleNewChannel_: about to call handleNewUsers for " <> show otherUserId
                                        handleNewUsers (Seq.singleton otherUserId) (return ())
                                        doAsyncWith Normal $
                                            return $ Just $ handleNewChannel_ False switch sbUpdate nc member
                                        return False
                            Just _ -> return True
                _ -> return True

            when register $ do
                csChannels %= addChannel (getId nc) cChannel
                when (sbUpdate == SidebarUpdateImmediate) $ do
                    -- Note that we only check for whether we should
                    -- switch to this channel when doing a sidebar
                    -- update, since that's the only case where it's
                    -- possible to do so.
                    updateSidebar (cChannel^.ccInfo.cdTeamId)

                    -- Finally, set our focus to the newly created
                    -- channel if the caller requested a change of
                    -- channel. Also consider the last join request
                    -- state field in case this is an asynchronous
                    -- channel addition triggered by a /join.
                    pending1 <- checkPendingChannelChange (getId nc)
                    pending2 <- case cChannel^.ccInfo.cdDMUserId of
                        Nothing -> return False
                        Just uId -> checkPendingChannelChangeByUserId uId

                    when (switch || isJust pending1 || pending2) $ do
                        setFocus (getId nc)
                        case pending1 of
                            Just (Just act) -> act
                            _ -> return ()

-- | Check to see whether the specified channel has been queued up to
-- be switched to.  Note that this condition is only cleared by the
-- actual setFocus switch to the channel because there may be multiple
-- operations that must complete before the channel is fully ready for
-- display/use.
--
-- Returns Just if the specified channel has a pending switch. The
-- result is an optional action to invoke after changing to the
-- specified channel.
checkPendingChannelChange :: ChannelId -> MH (Maybe (Maybe (MH ())))
checkPendingChannelChange cId = do
    ch <- use (csCurrentTeam.tsPendingChannelChange)
    curTid <- use csCurrentTeamId
    return $ case ch of
        Just (ChangeByChannelId tId i act) ->
            if i == cId && curTid == tId then Just act else Nothing
        _ -> Nothing

-- | Check to see whether the specified channel has been queued up to
-- be switched to.  Note that this condition is only cleared by the
-- actual setFocus switch to the channel because there may be multiple
-- operations that must complete before the channel is fully ready for
-- display/use.
--
-- Returns Just if the specified channel has a pending switch. The
-- result is an optional action to invoke after changing to the
-- specified channel.
checkPendingChannelChangeByUserId :: UserId -> MH Bool
checkPendingChannelChangeByUserId uId = do
    ch <- use (csCurrentTeam.tsPendingChannelChange)
    return $ case ch of
        Just (ChangeByUserId i) ->
            i == uId
        _ ->
            False

-- | Update the indicated Channel entry with the new data retrieved from
-- the Mattermost server. Also update the channel name if it changed.
updateChannelInfo :: ChannelId -> Channel -> ChannelMember -> MH ()
updateChannelInfo cid new member = do
    mh $ invalidateCacheEntry $ ChannelMessages cid
    csChannel(cid).ccInfo %= channelInfoFromChannelWithData new member
    withChannel cid $ \chan ->
        updateSidebar (chan^.ccInfo.cdTeamId)

setFocus :: ChannelId -> MH ()
setFocus cId = do
    showChannelInSidebar cId True
    setFocusWith True (Z.findRight ((== cId) . channelListEntryChannelId)) (return ())

setFocusWith :: Bool
             -> (Zipper ChannelListGroup ChannelListEntry
             -> Zipper ChannelListGroup ChannelListEntry)
             -> MH ()
             -> MH ()
setFocusWith updatePrev f onNoChange = do
    tId <- use csCurrentTeamId
    oldZipper <- use (csCurrentTeam.tsFocus)
    let newZipper = f oldZipper
        newFocus = Z.focus newZipper
        oldFocus = Z.focus oldZipper

    -- If we aren't changing anything, skip all the book-keeping because
    -- we'll end up clobbering things like tsRecentChannel.
    if newFocus /= oldFocus
       then do
          mh $ invalidateCacheEntry $ ChannelSidebar tId
          resetAutocomplete
          preChangeChannelCommon
          csCurrentTeam.tsFocus .= newZipper

          now <- liftIO getCurrentTime
          newCid <- use (csCurrentChannelId tId)
          csChannel(newCid).ccInfo.cdSidebarShowOverride .= Just now

          updateViewed updatePrev
          postChangeChannelCommon
       else onNoChange

postChangeChannelCommon :: MH ()
postChangeChannelCommon = do
    resetEditorState
    updateChannelListScroll
    loadLastEdit
    fetchVisibleIfNeeded

loadLastEdit :: MH ()
loadLastEdit = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)

    oldEphemeral <- preuse (csChannel(cId).ccEditState)
    case oldEphemeral of
        Nothing -> return ()
        Just e -> csCurrentTeam.tsEditState.cedEphemeral .= e

    loadLastChannelInput

loadLastChannelInput :: MH ()
loadLastChannelInput = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    inputHistoryPos <- use (csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition)
    case inputHistoryPos of
        Just i -> loadHistoryEntryToEditor cId i
        Nothing -> do
            (lastEdit, lastEditMode) <- use (csCurrentTeam.tsEditState.cedEphemeral.eesLastInput)
            csCurrentTeam.tsEditState.cedEditor %= (applyEdit $ insertMany lastEdit . clearZipper)
            csCurrentTeam.tsEditState.cedEditMode .= lastEditMode

updateChannelListScroll :: MH ()
updateChannelListScroll = do
    tId <- use csCurrentTeamId
    mh $ vScrollToBeginning (viewportScroll $ ChannelList tId)

preChangeChannelCommon :: MH ()
preChangeChannelCommon = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    csCurrentTeam.tsRecentChannel .= Just cId
    saveCurrentEdit

resetEditorState :: MH ()
resetEditorState = do
    csCurrentTeam.tsEditState.cedEditMode .= NewPost
    clearEditor

clearEditor :: MH ()
clearEditor = csCurrentTeam.tsEditState.cedEditor %= applyEdit clearZipper

saveCurrentEdit :: MH ()
saveCurrentEdit = do
    saveCurrentChannelInput

    oldEphemeral <- use (csCurrentTeam.tsEditState.cedEphemeral)
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    csChannel(cId).ccEditState .= oldEphemeral

saveCurrentChannelInput :: MH ()
saveCurrentChannelInput = do
    cmdLine <- use (csCurrentTeam.tsEditState.cedEditor)
    mode <- use (csCurrentTeam.tsEditState.cedEditMode)

    -- Only save the editor contents if the user is not navigating the
    -- history.
    inputHistoryPos <- use (csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition)

    when (isNothing inputHistoryPos) $
        csCurrentTeam.tsEditState.cedEphemeral.eesLastInput .=
           (T.intercalate "\n" $ getEditContents $ cmdLine, mode)

applyPreferenceChange :: Preference -> MH ()
applyPreferenceChange pref = do
    -- always update our user preferences accordingly
    csResources.crUserPreferences %= setUserPreferences (Seq.singleton pref)

    -- Invalidate the entire rendering cache since many things depend on
    -- user preferences
    mh invalidateCache

    if
      | Just f <- preferenceToFlaggedPost pref -> do
          updateMessageFlag (flaggedPostId f) (flaggedPostStatus f)

      | Just tIds <- preferenceToTeamOrder pref ->
          applyTeamOrder tIds

      | Just d <- preferenceToDirectChannelShowStatus pref -> do
          updateSidebar Nothing

          cs <- use csChannels

          -- We need to check on whether this preference was to show a
          -- channel and, if so, whether it was the one we attempted to
          -- switch to (thus triggering the preference change). If so,
          -- we need to switch to it now.
          let Just cId = getDmChannelFor (directChannelShowUserId d) cs
          case directChannelShowValue d of
              True -> do
                  pending <- checkPendingChannelChange cId
                  case pending of
                      Just mAct -> do
                          setFocus cId
                          fromMaybe (return ()) mAct
                      Nothing -> return ()
              False -> do
                  csChannel(cId).ccInfo.cdSidebarShowOverride .= Nothing

      | Just g <- preferenceToGroupChannelPreference pref -> do
          updateSidebar Nothing

          -- We need to check on whether this preference was to show a
          -- channel and, if so, whether it was the one we attempted to
          -- switch to (thus triggering the preference change). If so,
          -- we need to switch to it now.
          let cId = groupChannelId g
          case groupChannelShow g of
              True -> do
                  pending <- checkPendingChannelChange cId
                  case pending of
                      Just mAct -> do
                          setFocus cId
                          fromMaybe (return ()) mAct
                      Nothing -> return ()
              False -> do
                  csChannel(cId).ccInfo.cdSidebarShowOverride .= Nothing

      | Just f <- preferenceToFavoriteChannelPreference pref -> do
          updateSidebar Nothing

          -- We need to check on whether this preference was to show a
          -- channel and, if so, whether it was the one we attempted to
          -- switch to (thus triggering the preference change). If so,
          -- we need to switch to it now.
          let cId = favoriteChannelId f
          case favoriteChannelShow f of
              True -> do
                  pending <- checkPendingChannelChange cId
                  case pending of
                      Just mAct -> do
                          setFocus cId
                          fromMaybe (return ()) mAct
                      Nothing -> return ()
              False -> do
                  csChannel(cId).ccInfo.cdSidebarShowOverride .= Nothing
      | otherwise -> return ()

refreshChannelById :: ChannelId -> MH ()
refreshChannelById cId = do
    session <- getSession
    doAsyncWith Preempt $ do
        cwd <- MM.mmGetChannel cId session
        member <- MM.mmGetChannelMember cId UserMe session
        return $ Just $ do
            refreshChannel SidebarUpdateImmediate cwd member

removeChannelFromState :: ChannelId -> MH ()
removeChannelFromState cId = do
    withChannel cId $ \ chan -> do
        when (chan^.ccInfo.cdType /= Direct) $ do
            case chan^.ccInfo.cdTeamId of
                Nothing -> return ()
                Just tId -> do
                    origFocus <- use (csCurrentChannelId tId)
                    when (origFocus == cId) nextChannelSkipPrevView

            -- Update input history
            csInputHistory %= removeChannelHistory cId
            -- Update msgMap
            csChannels %= removeChannel cId

            case chan^.ccInfo.cdTeamId of
                Nothing -> do
                    ts <- use csTeams
                    forM_ (HM.keys ts) $ \tId ->
                        csTeam(tId).tsFocus %= Z.filterZipper ((/= cId) . channelListEntryChannelId)
                Just tId -> do
                    csTeam(tId).tsFocus %= Z.filterZipper ((/= cId) . channelListEntryChannelId)

            updateSidebar $ chan^.ccInfo.cdTeamId

nextChannel :: MH ()
nextChannel = do
    resetReturnChannel
    setFocusWith True Z.right (return ())

-- | This is almost never what you want; we use this when we delete a
-- channel and we don't want to update the deleted channel's view time.
nextChannelSkipPrevView :: MH ()
nextChannelSkipPrevView = setFocusWith False Z.right (return ())

prevChannel :: MH ()
prevChannel = do
    resetReturnChannel
    setFocusWith True Z.left (return ())

recentChannel :: MH ()
recentChannel = do
  recent <- use (csCurrentTeam.tsRecentChannel)
  case recent of
    Nothing  -> return ()
    Just cId -> do
        ret <- use (csCurrentTeam.tsReturnChannel)
        when (ret == Just cId) resetReturnChannel
        setFocus cId

resetReturnChannel :: MH ()
resetReturnChannel = do
  val <- use (csCurrentTeam.tsReturnChannel)
  case val of
      Nothing -> return ()
      Just _ -> do
          tId <- use csCurrentTeamId
          mh $ invalidateCacheEntry $ ChannelSidebar tId
          csCurrentTeam.tsReturnChannel .= Nothing

gotoReturnChannel :: MH ()
gotoReturnChannel = do
  ret <- use (csCurrentTeam.tsReturnChannel)
  case ret of
    Nothing  -> return ()
    Just cId -> do
        resetReturnChannel
        setFocus cId

setReturnChannel :: MH ()
setReturnChannel = do
  ret <- use (csCurrentTeam.tsReturnChannel)
  case ret of
    Nothing  -> do
        tId <- use csCurrentTeamId
        cId <- use (csCurrentChannelId tId)
        csCurrentTeam.tsReturnChannel .= Just cId
        mh $ invalidateCacheEntry $ ChannelSidebar tId
    Just _ -> return ()

nextUnreadChannel :: MH ()
nextUnreadChannel = do
    st <- use id
    setReturnChannel
    setFocusWith True (getNextUnreadChannel st) gotoReturnChannel

nextUnreadUserOrChannel :: MH ()
nextUnreadUserOrChannel = do
    st <- use id
    setReturnChannel
    setFocusWith True (getNextUnreadUserOrChannel st) gotoReturnChannel

leaveChannel :: ChannelId -> MH ()
leaveChannel cId = leaveChannelIfPossible cId False

leaveChannelIfPossible :: ChannelId -> Bool -> MH ()
leaveChannelIfPossible cId delete = do
    st <- use id
    me <- gets myUser
    let isMe u = u^.userIdL == me^.userIdL

    case st ^? csChannel(cId).ccInfo of
        Nothing -> return ()
        Just cInfo -> case canLeaveChannel cInfo of
            False -> return ()
            True ->
                -- The server will reject an attempt to leave a private
                -- channel if we're the only member. To check this, we
                -- just ask for the first two members of the channel.
                -- If there is only one, it must be us: hence the "all
                -- isMe" check below. If there are two members, it
                -- doesn't matter who they are, because we just know
                -- that we aren't the only remaining member, so we can't
                -- delete the channel.
                doAsyncChannelMM Preempt cId
                    (\s _ ->
                      let query = MM.defaultUserQuery
                           { MM.userQueryPage = Just 0
                           , MM.userQueryPerPage = Just 2
                           , MM.userQueryInChannel = Just cId
                           }
                      in toList <$> MM.mmGetUsers query s)
                    (\_ members -> Just $ do
                        -- If the channel is private:
                        --  * leave it if we aren't the last member.
                        --  * delete it if we are.
                        --
                        -- Otherwise:
                        --  * leave (or delete) the channel as specified
                        --    by the delete argument.
                        let func = case cInfo^.cdType of
                                Private -> case all isMe members of
                                    True -> (\ s c -> MM.mmDeleteChannel c s)
                                    False -> (\ s c -> MM.mmRemoveUserFromChannel c UserMe s)
                                Group ->
                                    \s _ ->
                                        let pref = hideGroupChannelPref cId (me^.userIdL)
                                        in MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) s
                                _ -> if delete
                                     then (\ s c -> MM.mmDeleteChannel c s)
                                     else (\ s c -> MM.mmRemoveUserFromChannel c UserMe s)

                        doAsyncChannelMM Preempt cId func endAsyncNOP
                    )

getNextUnreadChannel :: ChatState
                     -> (Zipper a ChannelListEntry -> Zipper a ChannelListEntry)
getNextUnreadChannel st =
    -- The next channel with unread messages must also be a channel
    -- other than the current one, since the zipper may be on a channel
    -- that has unread messages and will stay that way until we leave
    -- it- so we need to skip that channel when doing the zipper search
    -- for the next candidate channel.
    Z.findRight (\e ->
                let cId = channelListEntryChannelId e
                in channelListEntryUnread e && (cId /= st^.csCurrentChannelId(st^.csCurrentTeamId)))

getNextUnreadUserOrChannel :: ChatState
                           -> Zipper a ChannelListEntry
                           -> Zipper a ChannelListEntry
getNextUnreadUserOrChannel st z =
    -- Find the next unread channel, prefering direct messages
    let cur = st^.csCurrentChannelId(st^.csCurrentTeamId)
        matches e = entryIsDMEntry e && isFresh e
        isFresh e = channelListEntryUnread e && (channelListEntryChannelId e /= cur)
    in fromMaybe (Z.findRight isFresh z)
                 (Z.maybeFindRight matches z)

leaveCurrentChannel :: MH ()
leaveCurrentChannel = do
    tId <- use csCurrentTeamId
    use (csCurrentChannelId tId) >>= leaveChannel

createGroupChannel :: Text -> MH ()
createGroupChannel usernameList = do
    me <- gets myUser
    session <- getSession
    cs <- use csChannels

    doAsyncWith Preempt $ do
        let usernames = Seq.fromList $ fmap trimUserSigil $ T.words usernameList
        results <- MM.mmGetUsersByUsernames usernames session

        -- If we found all of the users mentioned, then create the group
        -- channel.
        case length results == length usernames of
            True -> do
                chan <- MM.mmCreateGroupMessageChannel (userId <$> results) session
                return $ Just $ do
                    case findChannelById (channelId chan) cs of
                      Just _ ->
                          -- If we already know about the channel ID,
                          -- that means the channel already exists so
                          -- we can just switch to it.
                          setFocus (channelId chan)
                      Nothing -> do
                          tId <- use csCurrentTeamId
                          csCurrentTeam.tsPendingChannelChange .=
                              (Just $ ChangeByChannelId tId (channelId chan) Nothing)
                          let pref = showGroupChannelPref (channelId chan) (me^.userIdL)
                          doAsyncWith Normal $ do
                            MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                            return $ Just $ applyPreferenceChange pref
            False -> do
                let foundUsernames = userUsername <$> results
                    missingUsernames = S.toList $
                                       S.difference (S.fromList $ F.toList usernames)
                                                    (S.fromList $ F.toList foundUsernames)
                return $ Just $ do
                    forM_ missingUsernames (mhError . NoSuchUser)

channelHistoryForward :: MH ()
channelHistoryForward = do
    resetAutocomplete

    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    inputHistoryPos <- use (csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition)
    case inputHistoryPos of
        Just i
          | i == 0 -> do
            -- Transition out of history navigation
            csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition .= Nothing
            loadLastChannelInput
          | otherwise -> do
            let newI = i - 1
            loadHistoryEntryToEditor cId newI
            csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition .= (Just newI)
        _ -> return ()

loadHistoryEntryToEditor :: ChannelId -> Int -> MH ()
loadHistoryEntryToEditor cId idx = do
    inputHistory <- use csInputHistory
    case getHistoryEntry cId idx inputHistory of
        Nothing -> return ()
        Just entry -> do
            let eLines = T.lines entry
                mv = if length eLines == 1 then gotoEOL else id
            csCurrentTeam.tsEditState.cedEditor.editContentsL .= (mv $ textZipper eLines Nothing)

channelHistoryBackward :: MH ()
channelHistoryBackward = do
    resetAutocomplete

    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    inputHistoryPos <- use (csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition)
    saveCurrentChannelInput

    let newI = maybe 0 (+ 1) inputHistoryPos
    loadHistoryEntryToEditor cId newI
    csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition .= (Just newI)

createOrdinaryChannel :: Bool -> Text -> MH ()
createOrdinaryChannel public name = do
    session <- getSession
    myTId <- use csCurrentTeamId
    doAsyncWith Preempt $ do
        -- create a new chat channel
        let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
            minChannel = MinChannel
              { minChannelName        = slug
              , minChannelDisplayName = name
              , minChannelPurpose     = Nothing
              , minChannelHeader      = Nothing
              , minChannelType        = if public then Ordinary else Private
              , minChannelTeamId      = myTId
              }
        tryMM (do c <- MM.mmCreateChannel minChannel session
                  chan <- MM.mmGetChannel (getId c) session
                  member <- MM.mmGetChannelMember (getId c) UserMe session
                  return (chan, member)
              )
              (return . Just . uncurry (handleNewChannel True SidebarUpdateImmediate))

-- | When we are added to a channel not locally known about, we need
-- to fetch the channel info for that channel.
handleChannelInvite :: ChannelId -> MH ()
handleChannelInvite cId = do
    session <- getSession
    doAsyncWith Normal $ do
        member <- MM.mmGetChannelMember cId UserMe session
        tryMM (MM.mmGetChannel cId session)
              (\cwd -> return $ Just $ do
                  pending <- checkPendingChannelChange cId
                  handleNewChannel (isJust pending) SidebarUpdateImmediate cwd member)

addUserByNameToCurrentChannel :: Text -> MH ()
addUserByNameToCurrentChannel uname =
    withFetchedUser (UserFetchByUsername uname) addUserToCurrentChannel

addUserToCurrentChannel :: UserInfo -> MH ()
addUserToCurrentChannel u = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    session <- getSession
    let channelMember = MinChannelMember (u^.uiId) cId
    doAsyncWith Normal $ do
        tryMM (void $ MM.mmAddUser cId channelMember session)
              (const $ return Nothing)

removeUserFromCurrentChannel :: Text -> MH ()
removeUserFromCurrentChannel uname =
    withFetchedUser (UserFetchByUsername uname) $ \u -> do
        tId <- use csCurrentTeamId
        cId <- use (csCurrentChannelId tId)
        session <- getSession
        doAsyncWith Normal $ do
            tryMM (void $ MM.mmRemoveUserFromChannel cId (UserById $ u^.uiId) session)
                  (const $ return Nothing)

startLeaveCurrentChannel :: MH ()
startLeaveCurrentChannel = do
    cInfo <- use (csCurrentChannel.ccInfo)
    case cInfo^.cdType of
        Direct -> hideDMChannel (cInfo^.cdChannelId)
        Group -> hideDMChannel (cInfo^.cdChannelId)
        _ -> setMode LeaveChannelConfirm

deleteCurrentChannel :: MH ()
deleteCurrentChannel = do
    setMode Main
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    leaveChannelIfPossible cId True

isCurrentChannel :: ChatState -> ChannelId -> Bool
isCurrentChannel st cId = st^.csCurrentChannelId(st^.csCurrentTeamId) == cId

isRecentChannel :: ChatState -> ChannelId -> Bool
isRecentChannel st cId = st^.csCurrentTeam.tsRecentChannel == Just cId

isReturnChannel :: ChatState -> ChannelId -> Bool
isReturnChannel st cId = st^.csCurrentTeam.tsReturnChannel == Just cId

joinChannelByName :: Text -> MH ()
joinChannelByName rawName = do
    session <- getSession
    tId <- use csCurrentTeamId
    doAsyncWith Preempt $ do
        result <- try $ MM.mmGetChannelByName tId (trimChannelSigil rawName) session
        return $ Just $ case result of
            Left (_::SomeException) -> mhError $ NoSuchChannel rawName
            Right chan -> joinChannel $ getId chan

-- | If the user is not a member of the specified channel, submit a
-- request to join it. Otherwise switch to the channel.
joinChannel :: ChannelId -> MH ()
joinChannel chanId = joinChannel' chanId Nothing

joinChannel' :: ChannelId -> Maybe (MH ()) -> MH ()
joinChannel' chanId act = do
    setMode Main
    mChan <- preuse (csChannel(chanId))
    case mChan of
        Just _ -> do
            setFocus chanId
            fromMaybe (return ()) act
        Nothing -> do
            myId <- gets myUserId
            tId <- use csCurrentTeamId
            let member = MinChannelMember myId chanId
            csCurrentTeam.tsPendingChannelChange .= (Just $ ChangeByChannelId tId chanId act)
            doAsyncChannelMM Preempt chanId (\ s c -> MM.mmAddUser c member s) (const $ return act)

createOrFocusDMChannel :: UserInfo -> Maybe (ChannelId -> MH ()) -> MH ()
createOrFocusDMChannel user successAct = do
    cs <- use csChannels
    case getDmChannelFor (user^.uiId) cs of
        Just cId -> do
            setFocus cId
            case successAct of
                Nothing -> return ()
                Just act -> act cId
        Nothing -> do
            -- We have a user of that name but no channel. Time to make one!
            myId <- gets myUserId
            session <- getSession
            csCurrentTeam.tsPendingChannelChange .= (Just $ ChangeByUserId $ user^.uiId)
            doAsyncWith Normal $ do
                -- create a new channel
                chan <- MM.mmCreateDirectMessageChannel (user^.uiId, myId) session
                return $ successAct <*> pure (channelId chan)

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannelByName :: Text -> MH ()
changeChannelByName name = do
    myId <- gets myUserId
    mCId <- gets (channelIdByChannelName name)
    mDMCId <- gets (channelIdByUsername name)

    withFetchedUserMaybe (UserFetchByUsername name) $ \foundUser -> do
        if (_uiId <$> foundUser) == Just myId
        then return ()
        else do
            setMode Main
            let err = mhError $ AmbiguousName name
            case (mCId, mDMCId) of
              (Nothing, Nothing) ->
                  case foundUser of
                      -- We know about the user but there isn't already a DM
                      -- channel, so create one.
                      Just user -> createOrFocusDMChannel user Nothing
                      -- There were no matches of any kind.
                      Nothing -> mhError $ NoSuchChannel name
              (Just cId, Nothing)
                  -- We matched a channel and there was an explicit sigil, so we
                  -- don't care about the username match.
                  | normalChannelSigil `T.isPrefixOf` name -> setFocus cId
                  -- We matched both a channel and a user, even though there is
                  -- no DM channel.
                  | Just _ <- foundUser -> err
                  -- We matched a channel only.
                  | otherwise -> setFocus cId
              (Nothing, Just cId) ->
                  -- We matched a DM channel only.
                  setFocus cId
              (Just _, Just _) ->
                  -- We matched both a channel and a DM channel.
                  err

setChannelTopic :: Text -> MH ()
setChannelTopic msg = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    let patch = defaultChannelPatch { channelPatchHeader = Just msg }
    doAsyncChannelMM Preempt cId
        (\s _ -> MM.mmPatchChannel cId patch s)
        (\_ _ -> Nothing)

-- | This renames the current channel's url name. It makes a request
-- to the server to change the name, but does not actually change the
-- name in Matterhorn yet; that is handled by a websocket event handled
-- asynchronously.
renameChannelUrl :: Text -> MH ()
renameChannelUrl name = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    s <- getSession
    let patch = defaultChannelPatch { channelPatchName = Just name }
    doAsyncWith Normal $ do
        _ <- MM.mmPatchChannel cId patch s
        return Nothing

getCurrentChannelTopic :: MH Text
getCurrentChannelTopic = do
    ch <- use csCurrentChannel
    return $ ch^.ccInfo.cdHeader

beginCurrentChannelDeleteConfirm :: MH ()
beginCurrentChannelDeleteConfirm = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    withChannel cId $ \chan -> do
        let chType = chan^.ccInfo.cdType
        if chType /= Direct
            then setMode DeleteChannelConfirm
            else mhError $ GenericError "Direct message channels cannot be deleted."

updateChannelNotifyProps :: ChannelId -> ChannelNotifyProps -> MH ()
updateChannelNotifyProps cId notifyProps = do
    withChannel cId $ \chan -> do
        csChannel(cId).ccInfo.cdNotifyProps .= notifyProps
        updateSidebar (chan^.ccInfo.cdTeamId)

toggleChannelFavoriteStatus :: MH ()
toggleChannelFavoriteStatus = do
    myId <- gets myUserId
    tId  <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    userPrefs <- use (csResources.crUserPreferences)
    session <- getSession
    let favPref = favoriteChannelPreference userPrefs cId
        trueVal = "true"
        prefVal =  case favPref of
            Just True -> ""
            Just False -> trueVal
            Nothing -> trueVal
        pref = Preference
            { preferenceUserId = myId
            , preferenceCategory = PreferenceCategoryFavoriteChannel
            , preferenceName = PreferenceName $ idString cId
            , preferenceValue = PreferenceValue prefVal
            }
    doAsyncWith Normal $ do
        MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
        return Nothing
