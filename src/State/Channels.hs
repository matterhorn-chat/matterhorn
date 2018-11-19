{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module State.Channels
  ( updateViewed
  , updateViewedChan
  , refreshChannel
  , refreshChannelsAndUsers
  , setFocus
  , setFocusWith
  , refreshChannelById
  , applyPreferenceChange
  , leaveChannel
  , leaveChannelIfPossible
  , leaveCurrentChannel
  , getNextUnreadChannel
  , getNextUnreadUserOrChannel
  , nextUnreadChannel
  , nextUnreadUserOrChannel
  , prevChannel
  , nextChannel
  , recentChannel
  , createGroupChannel
  , showGroupChannelPref
  , channelHistoryForward
  , channelHistoryBackward
  , handleNewChannel
  , createOrdinaryChannel
  , handleChannelInvite
  , addUserToCurrentChannel
  , removeUserFromCurrentChannel
  , removeChannelFromState
  , isRecentChannel
  , isCurrentChannel
  , deleteCurrentChannel
  , startLeaveCurrentChannel
  , joinChannel
  , joinChannelByName
  , startJoinChannel
  , changeChannelByName
  , setChannelTopic
  , beginCurrentChannelDeleteConfirm
  , toggleChannelListVisibility
  , showChannelInSidebar
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning, invalidateCache )
import           Brick.Widgets.Edit ( applyEdit, getEditContents, editContentsL )
import           Brick.Widgets.List ( list )
import           Control.Concurrent.Async ( runConcurrently, Concurrently(..) )
import           Control.Exception ( SomeException, try )
import           Data.Char ( isAlphaNum )
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import           Data.Function ( on )
import           Data.List ( nub )
import           Data.Maybe ( fromJust )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Text.Zipper ( textZipper, clearZipper, insertMany, gotoEOL )
import           Data.Time.Clock ( getCurrentTime )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           InputHistory
import           State.Common
import           State.Users
import           State.Flagging
import           Types
import           Types.Common
import           Zipper ( Zipper )
import qualified Zipper as Z


updateViewed :: MH ()
updateViewed = do
    csCurrentChannel.ccInfo.cdMentionCount .= 0
    updateViewedChan =<< use csCurrentChannelId

-- | When a new channel has been selected for viewing, this will
-- notify the server of the change, and also update the local channel
-- state to set the last-viewed time for the previous channel and
-- update the viewed time to now for the newly selected channel.
updateViewedChan :: ChannelId -> MH ()
updateViewedChan cId = use csConnectionStatus >>= \case
    Connected -> do
        -- Only do this if we're connected to avoid triggering noisy
        -- exceptions.
        pId <- use csRecentChannel
        doAsyncChannelMM Preempt cId
          (\s _ c -> MM.mmViewChannel UserMe c pId s)
          (\c () -> Just $ setLastViewedFor pId c)
    Disconnected ->
        -- Cannot update server; make no local updates to avoid getting
        -- out of sync with the server. Assumes that this is a temporary
        -- break in connectivity and that after the connection is
        -- restored, the user's normal activities will update state as
        -- appropriate. If connectivity is permanently lost, managing
        -- this state is irrelevant.
        return ()

toggleChannelListVisibility :: MH ()
toggleChannelListVisibility = do
    mh invalidateCache
    csShowChannelList %= not

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
          doAsyncChannelMM Preempt cId (\ s _ _ ->
                                           (,) <$> MM.mmGetChannel cId s
                                               <*> MM.mmGetChannelMember cId UserMe s)
          (\pcid (cwd, member) -> Just $ csChannel(pcid).ccInfo %= channelInfoFromChannelWithData cwd member)

    -- Update the old channel's previous viewed time (allows tracking of
    -- new messages)
    case prevId of
      Nothing -> return ()
      Just p -> csChannels %= (channelByIdL p %~ (clearNewMessageIndicator . clearEditedThreshold))

-- | Refresh information about all channels and users. This is usually
-- triggered when a reconnect event for the WebSocket to the server
-- occurs.
refreshChannelsAndUsers :: MH ()
refreshChannelsAndUsers = do
    session <- getSession
    myTId <- gets myTeamId
    me <- gets myUser
    knownUsers <- gets allUserIds
    doAsyncWith Preempt $ do
      (chans, datas) <- runConcurrently $ (,)
                       <$> Concurrently (MM.mmGetChannelsForUser UserMe myTId session)
                       <*> Concurrently (MM.mmGetChannelMembersForUser UserMe myTId session)

      -- Collect all user IDs associated with DM channels so we can
      -- bulk-fetch their user records.
      let dmUsers = catMaybes $ flip map (F.toList chans) $ \chan ->
              case chan^.channelTypeL of
                  Direct -> case userIdForDMChannel (userId me) (sanitizeUserText $ channelName chan) of
                        Nothing -> Nothing
                        Just otherUserId -> Just otherUserId
                  _ -> Nothing
          uIdsToFetch = nub $ knownUsers <> dmUsers

          dataMap = HM.fromList $ toList $ (\d -> (channelMemberChannelId d, d)) <$> datas
          mkPair chan = (chan, fromJust $ HM.lookup (channelId chan) dataMap)
          chansWithData = mkPair <$> chans

      return $ Just $
          -- Fetch user data associated with DM channels
          handleNewUsers (Seq.fromList uIdsToFetch) $ do
              -- Then refresh all loaded channels
              forM_ chansWithData $ uncurry (refreshChannel False)
              updateSidebar

-- | Refresh information about a specific channel.  The channel
-- metadata is refreshed, and if this is a loaded channel, the
-- scrollback is updated as well.
--
-- The boolean argument indicates whether this refresh should also
-- update the sidebar. Ordinarily you want this, so pass True unless you
-- are very sure you know what you are doing, i.e., you are very sure
-- that a call to refreshChannel will be followed immediately by a call
-- to updateSidebar. We provide this control so that channel refreshes
-- can be batched and then a single updateSidebar call can be used
-- instead of the default behavior of calling it once per refreshChannel
-- call, which is the behavior if True is passed here.
refreshChannel :: Bool -> Channel -> ChannelMember -> MH ()
refreshChannel doSidebarUpdate chan member = do
    let cId = getId chan
    myTId <- gets myTeamId
    let ourTeam = channelTeamId chan == Nothing ||
                  Just myTId == channelTeamId chan

    -- If this is a group channel that the user has chosen to hide or if
    -- the channel is not a channel for the current session's team, ignore
    -- the refresh request.
    isHidden <- channelHiddenPreference cId
    case isHidden || not ourTeam of
        True -> return ()
        False -> do
            -- If this channel is unknown, register it first.
            mChan <- preuse (csChannel(cId))
            when (isNothing mChan) $
                handleNewChannel False doSidebarUpdate chan member

            updateChannelInfo cId chan member

channelHiddenPreference :: ChannelId -> MH Bool
channelHiddenPreference cId = do
    prefs <- use (csResources.crUserPreferences.userPrefGroupChannelPrefs)
    let matching = filter (\p -> fst p == cId) (HM.toList prefs)
    return $ any (not . snd) matching

handleNewChannel :: Bool -> Bool -> Channel -> ChannelMember -> MH ()
handleNewChannel = handleNewChannel_ True

handleNewChannel_ :: Bool
                  -- ^ Whether to permit this call to recursively
                  -- schedule itself for later if it can't locate
                  -- a DM channel user record. This is to prevent
                  -- uncontrolled recursion.
                  -> Bool
                  -- ^ Whether to switch to the new channel once it has
                  -- been installed.
                  -> Bool
                  -- ^ Whether to update the sidebar, in case the caller
                  -- wants to batch these before updating it. Pass True
                  -- unless you know what you are doing, i.e., unless
                  -- you intend to call updateSidebar yourself after
                  -- calling this.
                  -> Channel
                  -- ^ The channel to install.
                  -> ChannelMember
                  -> MH ()
handleNewChannel_ permitPostpone switch doSidebarUpdate nc member = do
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
                                            return $ Just $ handleNewChannel_ False switch doSidebarUpdate nc member
                                        return False
                            Just _ -> return True
                _ -> return True

            when register $ do
                csChannels %= addChannel (getId nc) cChannel
                when doSidebarUpdate $ do
                    -- Note that we only check for whether we should
                    -- switch to this channel when doing a sidebar
                    -- update, since that's the only case where it's
                    -- possible to do so.
                    updateSidebar

                    -- Finally, set our focus to the newly created
                    -- channel if the caller requested a change of
                    -- channel. Also consider the last join request
                    -- state field in case this is an asynchronous
                    -- channel addition triggered by a /join.
                    wasLast <- checkLastJoinRequest (getId nc)

                    when (switch || wasLast) $ setFocus (getId nc)

-- | Check to see whether the specified channel has been queued up to
-- be switched to now that the channel is registered in the state. If
-- so, return True and clear the state. Otherwise return False.
checkLastJoinRequest :: ChannelId -> MH Bool
checkLastJoinRequest chanId = do
    lastReq <- use csLastJoinRequest
    case lastReq of
        Just cId | cId == chanId -> do
            csLastJoinRequest .= Nothing
            return True
        _ -> return False

-- | Update the indicated Channel entry with the new data retrieved from
-- the Mattermost server. Also update the channel name if it changed.
updateChannelInfo :: ChannelId -> Channel -> ChannelMember -> MH ()
updateChannelInfo cid new member = do
    csChannel(cid).ccInfo %= channelInfoFromChannelWithData new member

setFocus :: ChannelId -> MH ()
setFocus cId = do
    showChannelInSidebar cId
    setFocusWith (Z.findRight ((== cId) . channelListEntryChannelId))

showChannelInSidebar :: ChannelId -> MH ()
showChannelInSidebar cId = do
    mChan <- preuse $ csChannel cId
    me <- gets myUser
    prefs <- use (csResources.crUserPreferences)
    session <- getSession
    case mChan of
        Nothing -> return ()
        Just ch -> when (ch^.ccInfo.cdType == Direct) $ do
            let Just uId = ch^.ccInfo.cdDMUserId
            case dmChannelShowPreference prefs uId of
                Just False -> do
                    let pref = showDirectChannelPref (me^.userIdL) uId True
                    csLastJoinRequest .= Just (ch^.ccInfo.cdChannelId)
                    doAsyncWith Preempt $ do
                        MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                        return Nothing
                _ -> do
                    now <- liftIO getCurrentTime
                    csChannel(cId).ccInfo.cdSidebarShowOverride .= Just now
                    updateSidebar

setFocusWith :: (Zipper ChannelListGroup ChannelListEntry
              -> Zipper ChannelListGroup ChannelListEntry) -> MH ()
setFocusWith f = do
    oldZipper <- use csFocus
    let newZipper = f oldZipper
        newFocus = Z.focus newZipper
        oldFocus = Z.focus oldZipper

    -- If we aren't changing anything, skip all the book-keeping because
    -- we'll end up clobbering things like csRecentChannel.
    when (newFocus /= oldFocus) $ do
        preChangeChannelCommon
        csFocus .= newZipper
        updateViewed
        postChangeChannelCommon

postChangeChannelCommon :: MH ()
postChangeChannelCommon = do
    resetHistoryPosition
    resetEditorState
    updateChannelListScroll
    loadLastEdit
    resetCurrentEdit

resetCurrentEdit :: MH ()
resetCurrentEdit = do
    cId <- use csCurrentChannelId
    csEditState.cedLastChannelInput.at cId .= Nothing

loadLastEdit :: MH ()
loadLastEdit = do
    cId <- use csCurrentChannelId
    lastInput <- use (csEditState.cedLastChannelInput.at cId)
    case lastInput of
        Nothing -> return ()
        Just (lastEdit, lastEditMode) -> do
            csEditState.cedEditor %= (applyEdit $ insertMany (lastEdit) . clearZipper)
            csEditState.cedEditMode .= lastEditMode

resetHistoryPosition :: MH ()
resetHistoryPosition = do
    cId <- use csCurrentChannelId
    csEditState.cedInputHistoryPosition.at cId .= Just Nothing

updateChannelListScroll :: MH ()
updateChannelListScroll = do
    mh $ vScrollToBeginning (viewportScroll ChannelList)

preChangeChannelCommon :: MH ()
preChangeChannelCommon = do
    cId <- use csCurrentChannelId
    csRecentChannel .= Just cId
    saveCurrentEdit

resetEditorState :: MH ()
resetEditorState = do
    csEditState.cedEditMode .= NewPost
    clearEditor

clearEditor :: MH ()
clearEditor = csEditState.cedEditor %= applyEdit clearZipper

saveCurrentEdit :: MH ()
saveCurrentEdit = do
    cId <- use csCurrentChannelId
    cmdLine <- use (csEditState.cedEditor)
    mode <- use (csEditState.cedEditMode)
    csEditState.cedLastChannelInput.at cId .=
      Just (T.intercalate "\n" $ getEditContents $ cmdLine, mode)

hideGroupChannelPref :: ChannelId -> UserId -> Preference
hideGroupChannelPref cId uId =
    Preference { preferenceCategory = PreferenceCategoryGroupChannelShow
               , preferenceValue = PreferenceValue "false"
               , preferenceName = PreferenceName $ idString cId
               , preferenceUserId = uId
               }

showGroupChannelPref :: ChannelId -> UserId -> Preference
showGroupChannelPref cId uId =
    Preference { preferenceCategory = PreferenceCategoryGroupChannelShow
               , preferenceValue = PreferenceValue "true"
               , preferenceName = PreferenceName $ idString cId
               , preferenceUserId = uId
               }

showDirectChannelPref :: UserId -> UserId -> Bool -> Preference
showDirectChannelPref myId otherId s =
    Preference { preferenceCategory = PreferenceCategoryDirectChannelShow
               , preferenceValue = if s then PreferenceValue "true"
                                        else PreferenceValue "false"
               , preferenceName = PreferenceName $ idString otherId
               , preferenceUserId = myId
               }

applyPreferenceChange :: Preference -> MH ()
applyPreferenceChange pref = do
    -- always update our user preferences accordingly
    csResources.crUserPreferences %= setUserPreferences (Seq.singleton pref)
    if
      | Just f <- preferenceToFlaggedPost pref -> do
          updateMessageFlag (flaggedPostId f) (flaggedPostStatus f)

      | Just d <- preferenceToDirectChannelShowStatus pref -> do
          updateSidebar

          cs <- use csChannels

          -- We need to check on whether this preference was to show a
          -- channel and, if so, whether it was the one we attempted to
          -- switch to (thus triggering the preference change). If so,
          -- we need to switch to it now.
          when (directChannelShowValue d) $ do
              case getDmChannelFor (directChannelShowUserId d) cs of
                  -- This shouldn't happen.
                  Nothing -> return ()
                  Just dmcId -> do
                      wasLast <- checkLastJoinRequest dmcId
                      when wasLast $ setFocus dmcId

      | Just g <- preferenceToGroupChannelPreference pref -> do
          let cId = groupChannelId g
          mChan <- preuse $ csChannel cId

          case (mChan, groupChannelShow g) of
              (Just _, False) ->
                  -- If it has been set to hidden and we are showing it,
                  -- remove it from the state.
                  removeChannelFromState cId
              (Nothing, True) ->
                  -- If it has been set to showing and we are not showing
                  -- it, ask for a load/refresh.
                  refreshChannelById cId
              _ -> return ()
      | otherwise -> return ()

refreshChannelById :: ChannelId -> MH ()
refreshChannelById cId = do
    session <- getSession
    doAsyncWith Preempt $ do
        cwd <- MM.mmGetChannel cId session
        member <- MM.mmGetChannelMember cId UserMe session
        return $ Just $ do
            refreshChannel True cwd member

removeChannelFromState :: ChannelId -> MH ()
removeChannelFromState cId = do
    withChannel cId $ \ chan -> do
        when (chan^.ccInfo.cdType /= Direct) $ do
            origFocus <- use csCurrentChannelId
            when (origFocus == cId) nextChannel
            csEditState.cedInputHistoryPosition .at cId .= Nothing
            csEditState.cedLastChannelInput     .at cId .= Nothing
            -- Update input history
            csEditState.cedInputHistory         %= removeChannelHistory cId
            -- Update msgMap
            csChannels                          %= removeChannel cId
            -- Remove from focus zipper
            csFocus %= Z.filterZipper ((/= cId) . channelListEntryChannelId)

nextChannel :: MH ()
nextChannel = setFocusWith Z.right

prevChannel :: MH ()
prevChannel = setFocusWith Z.left

recentChannel :: MH ()
recentChannel = do
  recent <- use csRecentChannel
  case recent of
    Nothing  -> return ()
    Just cId -> setFocus cId

nextUnreadChannel :: MH ()
nextUnreadChannel = do
    st <- use id
    setFocusWith (getNextUnreadChannel st)

nextUnreadUserOrChannel :: MH ()
nextUnreadUserOrChannel = do
    st <- use id
    setFocusWith (getNextUnreadUserOrChannel st)

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
                    (\s _ _ ->
                      let query = MM.defaultUserQuery
                           { MM.userQueryPage = Just 0
                           , MM.userQueryPerPage = Just 2
                           , MM.userQueryInChannel = Just cId
                           }
                      in toList <$> MM.mmGetUsers query s)
                    (\_ members -> Just $ do
                        -- If the channel is private:
                        -- * leave it if we aren't the last member.
                        -- * delete it if we are.
                        --
                        -- Otherwise:
                        -- * leave (or delete) the channel as specified
                        -- by the delete argument.
                        let func = case cInfo^.cdType of
                                Private -> case all isMe members of
                                    True -> (\ s _ c -> MM.mmDeleteChannel c s)
                                    False -> (\ s _ c -> MM.mmRemoveUserFromChannel c UserMe s)
                                Group ->
                                    \s _ _ ->
                                        let pref = hideGroupChannelPref cId (me^.userIdL)
                                        in MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) s
                                _ -> if delete
                                     then (\ s _ c -> MM.mmDeleteChannel c s)
                                     else (\ s _ c -> MM.mmRemoveUserFromChannel c UserMe s)

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
                in hasUnread st cId && (cId /= st^.csCurrentChannelId))

getNextUnreadUserOrChannel :: ChatState
                           -> Zipper a ChannelListEntry
                           -> Zipper a ChannelListEntry
getNextUnreadUserOrChannel st z =
    -- Find the next unread channel, prefering direct messages
    let cur = st^.csCurrentChannelId
        matches e = entryIsDMEntry e && isFresh (channelListEntryChannelId e)
        isFresh c = hasUnread st c && (c /= cur)
    in fromMaybe (Z.findRight (isFresh . channelListEntryChannelId) z)
                 (Z.maybeFindRight matches z)

leaveCurrentChannel :: MH ()
leaveCurrentChannel = use csCurrentChannelId >>= leaveChannel

createGroupChannel :: Text -> MH ()
createGroupChannel usernameList = do
    st <- use id
    me <- gets myUser

    let usernames = T.words usernameList
        findUserIds [] = return []
        findUserIds (n:ns) = do
            case userByUsername n st of
                Nothing -> do
                    mhError $ NoSuchUser n
                    return []
                Just u -> (u^.uiId:) <$> findUserIds ns

    results <- findUserIds usernames

    -- If we found all of the users mentioned, then create the group
    -- channel.
    when (length results == length usernames) $ do
        session <- getSession
        doAsyncWith Preempt $ do
            chan <- MM.mmCreateGroupMessageChannel (Seq.fromList results) session
            let pref = showGroupChannelPref (channelId chan) (me^.userIdL)
            -- It's possible that the channel already existed, in which
            -- case we want to request a preference change to show it.
            MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
            cwd <- MM.mmGetChannel (channelId chan) session
            member <- MM.mmGetChannelMember (channelId chan) UserMe session
            return $ Just $ do
                applyPreferenceChange pref
                handleNewChannel True True cwd member

channelHistoryForward :: MH ()
channelHistoryForward = do
    cId <- use csCurrentChannelId
    inputHistoryPos <- use (csEditState.cedInputHistoryPosition.at cId)
    inputHistory <- use (csEditState.cedInputHistory)
    case inputHistoryPos of
        Just (Just i)
          | i == 0 -> do
            -- Transition out of history navigation
            csEditState.cedInputHistoryPosition.at cId .= Just Nothing
            loadLastEdit
          | otherwise -> do
            let Just entry = getHistoryEntry cId newI inputHistory
                newI = i - 1
                eLines = T.lines entry
                mv = if length eLines == 1 then gotoEOL else id
            csEditState.cedEditor.editContentsL .= (mv $ textZipper eLines Nothing)
            csEditState.cedInputHistoryPosition.at cId .= (Just $ Just newI)
        _ -> return ()

channelHistoryBackward :: MH ()
channelHistoryBackward = do
    cId <- use csCurrentChannelId
    inputHistoryPos <- use (csEditState.cedInputHistoryPosition.at cId)
    inputHistory <- use (csEditState.cedInputHistory)
    case inputHistoryPos of
        Just (Just i) ->
            let newI = i + 1
            in case getHistoryEntry cId newI inputHistory of
                Nothing -> return ()
                Just entry -> do
                    let eLines = T.lines entry
                        mv = if length eLines == 1 then gotoEOL else id
                    csEditState.cedEditor.editContentsL .= (mv $ textZipper eLines Nothing)
                    csEditState.cedInputHistoryPosition.at cId .= (Just $ Just newI)
        _ ->
            let newI = 0
            in case getHistoryEntry cId newI inputHistory of
                Nothing -> return ()
                Just entry ->
                    let eLines = T.lines entry
                        mv = if length eLines == 1 then gotoEOL else id
                    in do
                      saveCurrentEdit
                      csEditState.cedEditor.editContentsL .= (mv $ textZipper eLines Nothing)
                      csEditState.cedInputHistoryPosition.at cId .= (Just $ Just newI)

createOrdinaryChannel :: Text -> MH ()
createOrdinaryChannel name  = do
    session <- getSession
    myTId <- gets myTeamId
    doAsyncWith Preempt $ do
        -- create a new chat channel
        let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
            minChannel = MinChannel
              { minChannelName        = slug
              , minChannelDisplayName = name
              , minChannelPurpose     = Nothing
              , minChannelHeader      = Nothing
              , minChannelType        = Ordinary
              , minChannelTeamId      = myTId
              }
        tryMM (do c <- MM.mmCreateChannel minChannel session
                  chan <- MM.mmGetChannel (getId c) session
                  member <- MM.mmGetChannelMember (getId c) UserMe session
                  return (chan, member)
              )
              (return . Just . uncurry (handleNewChannel True True))

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> MH ()
handleChannelInvite cId = do
    session <- getSession
    doAsyncWith Normal $ do
        member <- MM.mmGetChannelMember cId UserMe session
        tryMM (MM.mmGetChannel cId session)
              (\cwd -> return $ Just $ handleNewChannel False True cwd member)

addUserToCurrentChannel :: Text -> MH ()
addUserToCurrentChannel uname = do
    -- First: is this a valid username?
    result <- gets (userByUsername uname)
    case result of
        Just u -> do
            cId <- use csCurrentChannelId
            session <- getSession
            let channelMember = MinChannelMember (u^.uiId) cId
            doAsyncWith Normal $ do
                tryMM (void $ MM.mmAddUser cId channelMember session)
                      (const $ return Nothing)
        _ -> do
            mhError $ NoSuchUser uname

removeUserFromCurrentChannel :: Text -> MH ()
removeUserFromCurrentChannel uname = do
    -- First: is this a valid username?
    result <- gets (userByUsername uname)
    case result of
        Just u -> do
            cId <- use csCurrentChannelId
            session <- getSession
            doAsyncWith Normal $ do
                tryMM (void $ MM.mmRemoveUserFromChannel cId (UserById $ u^.uiId) session)
                      (const $ return Nothing)
        _ -> do
            mhError $ NoSuchUser uname

startLeaveCurrentChannel :: MH ()
startLeaveCurrentChannel = do
    cInfo <- use (csCurrentChannel.ccInfo)
    case canLeaveChannel cInfo of
        True -> setMode LeaveChannelConfirm
        False -> mhError $ GenericError "The /leave command cannot be used with this channel."

deleteCurrentChannel :: MH ()
deleteCurrentChannel = do
    setMode Main
    cId <- use csCurrentChannelId
    leaveChannelIfPossible cId True

isCurrentChannel :: ChatState -> ChannelId -> Bool
isCurrentChannel st cId = st^.csCurrentChannelId == cId

isRecentChannel :: ChatState -> ChannelId -> Bool
isRecentChannel st cId = st^.csRecentChannel == Just cId

joinChannelByName :: Text -> MH ()
joinChannelByName rawName = do
    session <- getSession
    tId <- gets myTeamId
    doAsyncWith Preempt $ do
        result <- try $ MM.mmGetChannelByName tId (trimChannelSigil rawName) session
        return $ Just $ case result of
            Left (_::SomeException) -> mhError $ NoSuchChannel rawName
            Right chan -> joinChannel $ getId chan

startJoinChannel :: MH ()
startJoinChannel = do
    session <- getSession
    myTId <- gets myTeamId
    myChannels <- use (csChannels.to (filteredChannelIds (const True)))
    doAsyncWith Preempt $ do
        -- We don't get to just request all channels, so we request channels in
        -- chunks of 50.  A better UI might be to request an initial set and
        -- then wait for the user to demand more.
        let fetchCount     = 50
            loop acc start = do
              newChans <- MM.mmGetPublicChannels myTId (Just start) (Just fetchCount) session
              let chans = acc <> newChans
              if length newChans < fetchCount
                then return chans
                else loop chans (start+1)
        chans <- Seq.filter (\ c -> not (channelId c `elem` myChannels)) <$> loop mempty 0
        let sortedChans = V.fromList $ toList $ Seq.sortBy (compare `on` channelName) chans
        return $ Just $ do
            csJoinChannelList .= (Just $ list JoinChannelList sortedChans 2)

    setMode JoinChannel
    csJoinChannelList .= Nothing

-- | If the user is not a member of the specified channel, submit a
-- request to join it. Otherwise switch to the channel.
joinChannel :: ChannelId -> MH ()
joinChannel chanId = do
    setMode Main
    mChan <- preuse (csChannel(chanId))
    case mChan of
        Just _ -> setFocus chanId
        Nothing -> do
            myId <- gets myUserId
            let member = MinChannelMember myId chanId
            csLastJoinRequest .= Just chanId
            doAsyncChannelMM Preempt chanId (\ s _ c -> MM.mmAddUser c member s) endAsyncNOP

attemptCreateDMChannel :: Text -> MH ()
attemptCreateDMChannel name = do
    mCid <- gets (channelIdByUsername name)
    me <- gets myUser
    displayNick <- use (to useNickname)
    uByNick     <- use (to (userByNickname name))
    let myName = if displayNick && not (T.null $ sanitizeUserText $ userNickname me)
                 then sanitizeUserText $ userNickname me
                 else me^.userUsernameL
    when (name /= myName) $ do
        let uName = if displayNick
                    then maybe name (view uiName) uByNick
                    else name
        mUid <- gets (userIdForUsername uName)
        if isJust mUid && isNothing mCid
        then do
            -- We have a user of that name but no channel. Time to make one!
            let Just uId = mUid
            myId <- gets myUserId
            session <- getSession
            doAsyncWith Normal $ do
                -- create a new channel
                nc <- MM.mmCreateDirectMessageChannel (uId, myId) session -- tId uId
                cwd <- MM.mmGetChannel (getId nc) session
                member <- MM.mmGetChannelMember (getId nc) UserMe session
                return $ Just $ handleNewChannel True True cwd member
        else
            mhError $ NoSuchUser name

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannelByName :: Text -> MH ()
changeChannelByName name = do
    result <- gets (channelIdByName name)
    user <- gets (userByUsername name)
    let err = mhError $ AmbiguousName name

    case result of
      (Nothing, Nothing)
          -- We know about the user but there isn't already a DM
          -- channel, so create one.
          | Just _ <- user -> attemptCreateDMChannel name
          -- There were no matches of any kind.
          | otherwise -> mhError $ NoSuchChannel name
      (Just cId, Nothing)
          -- We matched a channel and there was an explicit sigil, so we
          -- don't care about the username match.
          | normalChannelSigil `T.isPrefixOf` name -> setFocus cId
          -- We matched both a channel and a user, even though there is
          -- no DM channel.
          | Just _ <- user -> err
          -- We matched a channel only.
          | otherwise -> setFocus cId
      (Nothing, Just cId) ->
          -- We matched a user only and there is already a DM channel.
          setFocus cId
      (Just _, Just _) ->
          -- We matched both a channel and a user.
          err

setChannelTopic :: Text -> MH ()
setChannelTopic msg = do
    cId <- use csCurrentChannelId
    let patch = defaultChannelPatch { channelPatchHeader = Just msg }
    doAsyncChannelMM Preempt cId
        (\s _ _ -> MM.mmPatchChannel cId patch s)
        (\_ _ -> Nothing)

beginCurrentChannelDeleteConfirm :: MH ()
beginCurrentChannelDeleteConfirm = do
    cId <- use csCurrentChannelId
    withChannel cId $ \chan -> do
        let chType = chan^.ccInfo.cdType
        if chType /= Direct
            then setMode DeleteChannelConfirm
            else mhError $ GenericError "Direct message channels cannot be deleted."
