{-# LANGUAGE LambdaCase #-}
module State
  (
  -- * Message flagging
    updateMessageFlag
  , flagMessage

  -- * Running external programs
  , runLoggedCommand

  -- * Channel sidebar selection
  , prevChannel
  , nextChannel
  , recentChannel
  , nextUnreadChannel

  -- * Working with channels
  , createOrdinaryChannel
  , startJoinChannel
  , joinChannel
  , changeChannel
  , disconnectChannels
  , startLeaveCurrentChannel
  , leaveCurrentChannel
  , leaveChannel
  , removeChannelFromState
  , beginCurrentChannelDeleteConfirm
  , deleteCurrentChannel
  , loadMoreMessages
  , channelScrollToTop
  , channelScrollToBottom
  , channelScrollUp
  , channelScrollDown
  , channelPageUp
  , channelPageDown
  , isCurrentChannel
  , isRecentChannel
  , getNewMessageCutoff
  , getEditedMessageCutoff
  , setChannelTopic
  , fetchCurrentChannelMembers
  , refreshChannelById
  , handleChannelInvite
  , addUserToCurrentChannel
  , removeUserFromCurrentChannel
  , createGroupChannel

  -- * Channel history
  , channelHistoryForward
  , channelHistoryBackward

  -- * Working with messages
  , PostToAdd(..)
  , sendMessage
  , msgURLs
  , editMessage
  , deleteMessage
  , addNewPostedMessage
  , fetchVisibleIfNeeded

  -- * Working with users
  , handleNewUser
  , updateStatus
  , handleTypingUser

  -- * Startup/reconnect management
  , refreshChannelsAndUsers

  -- * Channel selection mode
  , beginChannelSelect
  , updateChannelSelectMatches
  , channelSelectNext
  , channelSelectPrevious

  -- * Server-side preferences
  , applyPreferenceChange

  -- * Message selection mode
  , beginMessageSelect
  , flagSelectedMessage
  , copyVerbatimToClipboard
  , openSelectedMessageURLs
  , beginConfirmDeleteSelectedMessage
  , messageSelectUp
  , messageSelectUpBy
  , messageSelectDown
  , messageSelectDownBy
  , deleteSelectedMessage
  , beginReplyCompose
  , beginUpdateMessage
  , getSelectedMessage
  , cancelReplyOrEdit
  , replyToLatestMessage

  -- * URL selection mode
  , startUrlSelect
  , stopUrlSelect
  , openSelectedURL

  -- * Help
  , showHelpScreen

  -- * Themes
  , listThemes
  , setTheme
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick (invalidateCacheEntry)
import           Brick.Themes (themeToAttrMap)
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list, listMoveTo, listSelectedElement)
import           Control.Applicative
import           Control.Concurrent.Async (runConcurrently, Concurrently(..), concurrently)
import           Control.Concurrent (MVar, putMVar, forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (SomeException, try)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isAlphaNum)
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToBeginning, vScrollBy, vScrollToEnd)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Monad (when, unless, void, forM_, join)
import qualified Data.ByteString as BS
import           Data.Function (on)
import           Data.Text.Zipper (textZipper, clearZipper, insertMany, gotoEOL)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.List (sort, findIndex)
import           Data.Maybe (maybeToList, isJust, fromJust, catMaybes, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import           Graphics.Vty (outputIface)
import           Graphics.Vty.Output.Interface (ringTerminalBell)
import           Lens.Micro.Platform
import           System.Exit (ExitCode(..))
import           System.Process (proc, std_in, std_out, std_err, StdStream(..),
                                 createProcess, waitForProcess)
import           System.IO (hGetContents, hFlush, hPutStrLn)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserCacheDir)
import           System.FilePath

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types
import           Network.Mattermost.Lenses

import           Config
import           FilePaths
import           TimeUtils (justBefore, justAfter)
import           Types
import           Types.Channels
import           Types.Posts
import           Types.Messages
import           Types.Users
import           InputHistory
import           Themes
import           Zipper (Zipper)
import qualified Zipper as Z
import           Constants
import           Markdown (blockGetURLs, findVerbatimChunk)

import           State.Common
import           State.Messages
import           State.Setup.Threads (updateUserStatuses)

-- * Refreshing Channel Data

-- | Refresh information about a specific channel.  The channel
-- metadata is refreshed, and if this is a loaded channel, the
-- scrollback is updated as well.
refreshChannel :: Channel -> ChannelMember -> MH ()
refreshChannel chan member = do
  let cId = getId chan

  -- If this is a group channel that the user has chosen to hide, ignore
  -- the refresh request.
  isHidden <- channelHiddenPreference cId
  case isHidden of
      True -> return ()
      False -> do
          -- If this channel is unknown, register it first.
          mChan <- preuse (csChannel(cId))
          when (isNothing mChan) $
              handleNewChannel False chan member

          updateChannelInfo cId chan member

refreshChannelById :: ChannelId -> MH ()
refreshChannelById cId = do
  session <- use (csResources.crSession)
  doAsyncWith Preempt $ do
      cwd <- MM.mmGetChannel cId session
      member <- MM.mmGetChannelMember cId UserMe session
      return $ refreshChannel cwd member

createGroupChannel :: T.Text -> MH ()
createGroupChannel usernameList = do
    users <- use csUsers
    me <- use csMe

    let usernames = T.words usernameList
        findUserIds [] = return []
        findUserIds (n:ns) = do
            case findUserByName users n of
                Nothing -> do
                    postErrorMessage $ "No such user: " <> n
                    return []
                Just (uId, _) -> (uId:) <$> findUserIds ns

    results <- findUserIds usernames

    -- If we found all of the users mentioned, then create the group
    -- channel.
    when (length results == length usernames) $ do
        session <- use (csResources.crSession)
        doAsyncWith Preempt $ do
            chan <- MM.mmCreateGroupMessageChannel (Seq.fromList results) session
            let pref = showGroupChannelPref (channelId chan) (me^.userIdL)
            -- It's possible that the channel already existed, in which
            -- case we want to request a preference change to show it.
            MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session -- (me^.userIdL) $ Seq.fromList [pref]
            cwd <- MM.mmGetChannel (channelId chan) session
            member <- MM.mmGetChannelMember (channelId chan) UserMe session
            return $ do
                applyPreferenceChange pref
                handleNewChannel True cwd member

channelHiddenPreference :: ChannelId -> MH Bool
channelHiddenPreference cId = do
  prefs <- use (csResources.crPreferences)
  let matching = filter (\p -> groupChannelId p == cId) $
                 catMaybes $ preferenceToGroupChannelPreference <$> (F.toList prefs)
  return $ any (not . groupChannelShow) matching

applyPreferenceChange :: Preference -> MH ()
applyPreferenceChange pref
    | Just f <- preferenceToFlaggedPost pref =
        updateMessageFlag (flaggedPostId f) (flaggedPostStatus f)
    | Just g <- preferenceToGroupChannelPreference pref = do
        -- First, go update the preferences with this change.
        updatePreference pref

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
applyPreferenceChange _ = return ()

updatePreference :: Preference -> MH ()
updatePreference pref = do
    let replacePreference new old
            | preferenceCategory old == preferenceCategory new &&
              preferenceName old == preferenceName new = new
            | otherwise = old
    csResources.crPreferences %= fmap (replacePreference pref)

-- | Refresh information about all channels and users. This is usually
-- triggered when a reconnect event for the WebSocket to the server
-- occurs.
refreshChannelsAndUsers :: MH ()
refreshChannelsAndUsers = do
  -- The below code is a duplicate of mmGetAllChannelsWithDataForUser function,
  -- which has been inlined here to gain a concurrency benefit.
  session <- use (csResources.crSession)
  myTeamId <- use (csMyTeam.teamIdL)
  let userQuery = MM.defaultUserQuery
        { MM.userQueryPage = Just 0
        , MM.userQueryPerPage = Just 10000
        , MM.userQueryInTeam = Just myTeamId
        }
  doAsyncWith Preempt $ do
    (chans, datas, users) <- runConcurrently $ (,,)
                            <$> Concurrently (MM.mmGetChannelsForUser UserMe myTeamId session)
                            <*> Concurrently (MM.mmGetChannelMembersForUser UserMe myTeamId session)
                            <*> Concurrently (MM.mmGetUsers userQuery session)

    let dataMap = HM.fromList $ F.toList $ (\d -> (channelMemberChannelId d, d)) <$> datas
        mkPair chan = (chan, fromJust $ HM.lookup (channelId chan) dataMap)
        chansWithData = mkPair <$> chans

    return $ do
        forM_ users $ \u -> do
            when (not $ userDeleted u) $ do
                knownUsers <- use csUsers
                case findUserById (getId u) knownUsers of
                    Just _ -> return ()
                    Nothing -> handleNewUserDirect u

        forM_ chansWithData $ uncurry refreshChannel

        userSet <- use (csResources.crUserIdSet)
        liftIO $ STM.atomically $ STM.writeTVar userSet (fmap userId users)
        lock <- use (csResources.crUserStatusLock)
        doAsyncWith Preempt $ updateUserStatuses userSet lock session

-- | Websocket was disconnected, so all channels may now miss some
-- messages
disconnectChannels :: MH ()
disconnectChannels = addDisconnectGaps

-- | Update the indicated Channel entry with the new data retrieved from
-- the Mattermost server. Also update the channel name if it changed.
updateChannelInfo :: ChannelId -> Channel -> ChannelMember -> MH ()
updateChannelInfo cid new member = do
  mOldChannel <- preuse $ csChannel(cid)
  case mOldChannel of
      Nothing -> return ()
      Just old ->
          let oldName = old^.ccInfo.cdName
              newName = preferredChannelName new
          in if oldName == newName
             then return ()
             else do
                 removeChannelName oldName
                 addChannelName (channelType new) cid newName

  csChannel(cid).ccInfo %= channelInfoFromChannelWithData new member

addChannelName :: Type -> ChannelId -> T.Text -> MH ()
addChannelName chType cid name = do
    csNames.cnToChanId.at(name) .= Just cid

    -- For direct channels the username is already in the user list so
    -- do nothing
    existingNames <- use $ csNames.cnChans
    when (chType /= Direct && (not $ name `elem` existingNames)) $
        csNames.cnChans %= (sort . (name:))

removeChannelName :: T.Text -> MH ()
removeChannelName name = do
    -- Flush cnToChanId
    csNames.cnToChanId.at name .= Nothing
    -- Flush cnChans
    csNames.cnChans %= filter (/= name)


-- * Message selection mode

beginMessageSelect :: MH ()
beginMessageSelect = do
    -- Get the number of messages in the current channel and set the
    -- currently selected message index to be the most recently received
    -- message that corresponds to a Post (i.e. exclude informative
    -- messages).
    --
    -- If we can't find one at all, we ignore the mode switch request
    -- and just return.
    chanMsgs <- use(csCurrentChannel . ccContents . cdMessages)
    let recentPost = getLatestPostMsg chanMsgs

    when (isJust recentPost) $ do
        csMode .= MessageSelect
        csMessageSelect .= MessageSelectState (join $ ((^.mPostId) <$> recentPost))

getSelectedMessage :: ChatState -> Maybe Message
getSelectedMessage st
    | st^.csMode /= MessageSelect && st^.csMode /= MessageSelectDeleteConfirm = Nothing
    | otherwise = do
        selPostId <- selectMessagePostId $ st^.csMessageSelect

        let chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
        findMessage selPostId chanMsgs

messageSelectUp :: MH ()
messageSelectUp = do
    mode <- use csMode
    selected <- use (csMessageSelect.to selectMessagePostId)
    case selected of
        Just _ | mode == MessageSelect -> do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextPostId = getPrevPostId selected chanMsgs
            csMessageSelect .= MessageSelectState (nextPostId <|> selected)
        _ -> return ()

messageSelectDown :: MH ()
messageSelectDown = do
    mode <- use csMode
    selected <- use (csMessageSelect.to selectMessagePostId)
    case selected of
        Just _ | mode == MessageSelect -> do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextPostId = getNextPostId selected chanMsgs
            csMessageSelect .= MessageSelectState (nextPostId <|> selected)
        _ -> return ()

messageSelectDownBy :: Int -> MH ()
messageSelectDownBy amt
    | amt <= 0 = return ()
    | otherwise =
        messageSelectDown >> messageSelectDownBy (amt - 1)

messageSelectUpBy :: Int -> MH ()
messageSelectUpBy amt
    | amt <= 0 = return ()
    | otherwise =
      messageSelectUp >> messageSelectUpBy (amt - 1)

beginConfirmDeleteSelectedMessage :: MH ()
beginConfirmDeleteSelectedMessage =
    csMode .= MessageSelectDeleteConfirm

deleteSelectedMessage :: MH ()
deleteSelectedMessage = do
    selectedMessage <- use (to getSelectedMessage)
    st <- use id
    cId <- use csCurrentChannelId
    case selectedMessage of
        Just msg | isMine st msg && isDeletable msg ->
            case msg^.mOriginalPost of
              Just p ->
                  doAsyncChannelMM Preempt cId
                      (\s _ _ -> MM.mmDeletePost (postId p) s)
                      (\_ _ -> do csEditState.cedEditMode .= NewPost
                                  csMode .= Main)
              Nothing -> return ()
        _ -> return ()

beginCurrentChannelDeleteConfirm :: MH ()
beginCurrentChannelDeleteConfirm = do
    cId <- use csCurrentChannelId
    withChannel cId $ \chan -> do
        let chType = chan^.ccInfo.cdType
        if chType /= Direct
            then csMode .= DeleteChannelConfirm
            else postErrorMessage "The /delete-channel command cannot be used with direct message channels."

deleteCurrentChannel :: MH ()
deleteCurrentChannel = do
    csMode .= Main
    cId <- use csCurrentChannelId
    leaveChannelIfPossible cId True

isCurrentChannel :: ChatState -> ChannelId -> Bool
isCurrentChannel st cId = st^.csCurrentChannelId == cId

isRecentChannel :: ChatState -> ChannelId -> Bool
isRecentChannel st cId = st^.csRecentChannel == Just cId


-- | Tell the server that we have flagged or unflagged a message.
flagMessage :: PostId -> Bool -> MH ()
flagMessage pId f = do
  session <- use (csResources.crSession)
  myId <- use (csMe.userIdL)
  doAsyncWith Normal $ do
    let doFlag = if f then MM.mmFlagPost else MM.mmUnflagPost
    doFlag myId pId session
    return $ return ()

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: MH ()
flagSelectedMessage = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | Just pId <- msg^.mPostId ->
        flagMessage pId (not (msg^.mFlagged))
    _        -> return ()

beginUpdateMessage :: MH ()
beginUpdateMessage = do
    selected <- use (to getSelectedMessage)
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let Just p = msg^.mOriginalPost
            csMode .= Main
            csEditState.cedEditMode .= Editing p
            csEditState.cedEditor %= applyEdit (clearZipper >> (insertMany $ postMessage p))
        _ -> return ()

replyToLatestMessage :: MH ()
replyToLatestMessage = do
  msgs <- use (csCurrentChannel . ccContents . cdMessages)
  case findLatestUserMessage isReplyable msgs of
    Just msg -> do let Just p = msg^.mOriginalPost
                   csMode .= Main
                   csEditState.cedEditMode .= Replying msg p
    _ -> return ()

beginReplyCompose :: MH ()
beginReplyCompose = do
    selected <- use (to getSelectedMessage)
    case selected of
        Nothing -> return ()
        Just msg -> do
            let Just p = msg^.mOriginalPost
            csMode .= Main
            csEditState.cedEditMode .= Replying msg p

cancelReplyOrEdit :: MH ()
cancelReplyOrEdit = do
    mode <- use (csEditState.cedEditMode)
    case mode of
        NewPost -> return ()
        _ -> do
            csEditState.cedEditMode .= NewPost
            csEditState.cedEditor %= applyEdit clearZipper

copyVerbatimToClipboard :: MH ()
copyVerbatimToClipboard = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return ()
        Just m -> case findVerbatimChunk (m^.mText) of
            Nothing -> return ()
            Just txt -> do
              copyToClipboard txt
              csMode .= Main

-- * Joining, Leaving, and Inviting

startJoinChannel :: MH ()
startJoinChannel = do
    session <- use (csResources.crSession)
    myTeamId <- use (csMyTeam.teamIdL)
    myChannels <- use (csChannels.to (filteredChannelIds (const True)))
    doAsyncWith Preempt $ do
        -- We don't get to just request all channels, so we request channels in
        -- chunks of 50.  A better UI might be to request an initial set and
        -- then wait for the user to demand more.
        let fetchCount     = 50
            loop acc start = do
              newChans <- MM.mmGetPublicChannels myTeamId (Just start) (Just fetchCount) session
              let chans = acc <> newChans
              if length newChans < fetchCount
                then return chans
                else loop chans (start+1)
        chans <- Seq.filter (\ c -> not (channelId c `elem` myChannels)) <$> loop mempty 0
        let sortedChans = V.fromList $ F.toList $ Seq.sortBy (compare `on` channelName) chans
        return $ do
            csJoinChannelList .= (Just $ list JoinChannelList sortedChans 2)

    csMode .= JoinChannel
    csJoinChannelList .= Nothing

joinChannel :: Channel -> MH ()
joinChannel chan = do
    csMode .= Main
    myId <- use (csMe.userIdL)
    let member = MinChannelMember myId (getId chan)
    doAsyncChannelMM Preempt (getId chan) (\ s _ c -> MM.mmAddUser c member s) endAsyncNOP

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> MH ()
handleChannelInvite cId = do
    st <- use id
    doAsyncWith Normal $ do
        member <- MM.mmGetChannelMember cId UserMe (st^.csResources.crSession)
        tryMM (MM.mmGetChannel cId (st^.csResources.crSession))
              (\cwd -> return $ handleNewChannel False cwd member)

addUserToCurrentChannel :: T.Text -> MH ()
addUserToCurrentChannel uname = do
    -- First: is this a valid username?
    usrs <- use csUsers
    case findUserByName usrs uname of
        Just (uid, _) -> do
            cId <- use csCurrentChannelId
            session <- use (csResources.crSession)
            let channelMember = MinChannelMember uid cId
            doAsyncWith Normal $ do
                tryMM (void $ MM.mmAddUser cId channelMember session) -- session myTeamId cId uid)
                      (const $ return (return ()))
        _ -> do
            postErrorMessage ("No such user: " <> uname)

removeUserFromCurrentChannel :: T.Text -> MH ()
removeUserFromCurrentChannel uname = do
    -- First: is this a valid username?
    usrs <- use csUsers
    case findUserByName usrs uname of
        Just (uid, _) -> do
            cId <- use csCurrentChannelId
            session <- use (csResources.crSession)
            doAsyncWith Normal $ do
                tryMM (void $ MM.mmRemoveUserFromChannel cId (UserById uid) session)
                      (const $ return (return ()))
        _ -> do
            postErrorMessage ("No such user: " <> uname)

startLeaveCurrentChannel :: MH ()
startLeaveCurrentChannel = do
    cInfo <- use (csCurrentChannel.ccInfo)
    case canLeaveChannel cInfo of
        True -> csMode .= LeaveChannelConfirm
        False -> postErrorMessage "The /leave command cannot be used with this channel."

leaveCurrentChannel :: MH ()
leaveCurrentChannel = use csCurrentChannelId >>= leaveChannel

leaveChannelIfPossible :: ChannelId -> Bool -> MH ()
leaveChannelIfPossible cId delete = do
    st <- use id
    me <- use csMe
    let isMe u = u^.userIdL == me^.userIdL

    case st ^? csChannel(cId).ccInfo of
        Nothing -> return ()
        Just cInfo -> case canLeaveChannel cInfo of
            False -> return ()
            True ->
                -- The server will reject an attempt to leave a private
                -- channel if we're the only member.
                doAsyncChannelMM Preempt cId
                    fetchChannelMembers
                    (\_ members -> do
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

leaveChannel :: ChannelId -> MH ()
leaveChannel cId = leaveChannelIfPossible cId False

removeChannelFromState :: ChannelId -> MH ()
removeChannelFromState cId = do
    withChannel cId $ \ chan -> do
        let cName = chan^.ccInfo.cdName
            chType = chan^.ccInfo.cdType
        when (chType /= Direct) $ do
            origFocus <- use csCurrentChannelId
            when (origFocus == cId) $ do
              st <- use id
              setFocusWith (getNextNonDMChannel st Z.right)
            csEditState.cedInputHistoryPosition .at cId .= Nothing
            csEditState.cedLastChannelInput     .at cId .= Nothing
            -- Update input history
            csEditState.cedInputHistory         %= removeChannelHistory cId
            -- Remove channel name mappings
            removeChannelName cName
            -- Update msgMap
            csChannels                          %= filteredChannels ((/=) cId . fst)
            -- Remove from focus zipper
            csFocus                             %= Z.filterZipper (/= cId)

fetchCurrentChannelMembers :: MH ()
fetchCurrentChannelMembers = do
    cId <- use csCurrentChannelId
    doAsyncChannelMM Preempt cId
        fetchChannelMembers
        (\_ chanUsers -> do
              -- Construct a message listing them all and post it to the
              -- channel:
              let msgStr = "Channel members (" <> (T.pack $ show $ length chanUsers) <> "):\n" <>
                           T.intercalate ", " usernames
                  usernames = sort $ userUsername <$> filteredUsers
                  filteredUsers = filter (not . userDeleted) chanUsers

              postInfoMessage msgStr)

fetchChannelMembers :: Session -> TeamId -> ChannelId -> IO [User]
fetchChannelMembers s _ c = do
    let query = MM.defaultUserQuery
          { MM.userQueryPage = Just 0
          , MM.userQueryPerPage = Just 10000
          , MM.userQueryInChannel = Just c
          }
    chanUserMap <- MM.mmGetUsers query s
    return $ F.toList chanUserMap

-- | Called on async completion when the currently viewed channel has
-- been updated (i.e., just switched to this channel) to update local
-- state.
setLastViewedFor :: Maybe ChannelId -> ChannelId -> MH ()
setLastViewedFor prevId cId = do
  chan <- use (csChannels.to (findChannelById cId))
  -- Update new channel's viewed time, creating the channel if needed
  case chan of
    Nothing ->
        -- It's possible for us to get spurious WMChannelViewed events
        -- from the server, e.g. for channels that have been deleted.
        -- So here we ignore the request since it's hard to detect it
        -- before this point.
        return ()
    Just _  ->
      -- The server has been sent a viewed POST update, but there is
      -- no local information on what timestamp the server actually
      -- recorded.  There are a couple of options for setting the
      -- local value of the viewed time:
      --
      --   1. Attempting to locally construct a value, which would
      --      involve scanning all (User) messages in the channel to
      --      find the maximum of the created date, the modified date,
      --      or the deleted date, and assuming that maximum mostly
      --      matched the server's viewed time.
      --
      --   2. Issuing a channel metadata request to get the server's
      --      new concept of the viewed time.
      --
      --   3. Having the "chan/viewed" POST that was just issued
      --      return a value from the server. See
      --      https://github.com/mattermost/platform/issues/6803.
      --
      -- Method 3 would be the best and most lightweight.  Until that
      -- is available, Method 2 will be used.  The downside to Method
      -- 2 is additional client-server messaging, and a delay in
      -- updating the client data, but it's also immune to any new or
      -- removed Message date fields, or anything else that would
      -- contribute to the viewed/updated times on the server.
      doAsyncChannelMM Preempt cId (\ s _ _ ->
                                       (,) <$> MM.mmGetChannel cId s
                                           <*> MM.mmGetChannelMember cId UserMe s)
      (\pcid (cwd, member) -> csChannel(pcid).ccInfo %= channelInfoFromChannelWithData cwd member)
  -- Update the old channel's previous viewed time (allows tracking of new messages)
  case prevId of
    Nothing -> return ()
    Just p -> csChannels %= (channelByIdL p %~ (clearNewMessageIndicator . clearEditedThreshold))

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
          -- Only do this if we're connected to avoid triggering noisy exceptions.
          pId <- use csRecentChannel
          doAsyncChannelMM Preempt cId
            (\s _ c -> MM.mmViewChannel UserMe c pId s)
            (\c () -> setLastViewedFor pId c)
      Disconnected ->
          -- Cannot update server; make no local updates to avoid
          -- getting out-of-sync with the server.  Assumes that this
          -- is a temporary break in connectivity and that after the
          -- connection is restored, the user's normal activities will
          -- update state as appropriate.  If connectivity is
          -- permanently lost, managing this state is irrelevant.
          return ()

resetHistoryPosition :: MH ()
resetHistoryPosition = do
    cId <- use csCurrentChannelId
    csEditState.cedInputHistoryPosition.at cId .= Just Nothing

updateStatus :: UserId -> T.Text -> MH ()
updateStatus uId t = csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)

clearEditor :: MH ()
clearEditor = csEditState.cedEditor %= applyEdit clearZipper

loadLastEdit :: MH ()
loadLastEdit = do
    cId <- use csCurrentChannelId
    lastInput <- use (csEditState.cedLastChannelInput.at cId)
    case lastInput of
        Nothing -> return ()
        Just (lastEdit, lastEditMode) -> do
            csEditState.cedEditor %= (applyEdit $ insertMany (lastEdit) . clearZipper)
            csEditState.cedEditMode .= lastEditMode

saveCurrentEdit :: MH ()
saveCurrentEdit = do
    cId <- use csCurrentChannelId
    cmdLine <- use (csEditState.cedEditor)
    mode <- use (csEditState.cedEditMode)
    csEditState.cedLastChannelInput.at cId .=
      Just (T.intercalate "\n" $ getEditContents $ cmdLine, mode)

resetCurrentEdit :: MH ()
resetCurrentEdit = do
    cId <- use csCurrentChannelId
    csEditState.cedLastChannelInput.at cId .= Nothing

updateChannelListScroll :: MH ()
updateChannelListScroll = do
    mh $ vScrollToBeginning (viewportScroll ChannelList)

postChangeChannelCommon :: MH ()
postChangeChannelCommon = do
    resetHistoryPosition
    resetEditorState
    updateChannelListScroll
    loadLastEdit
    resetCurrentEdit

resetEditorState :: MH ()
resetEditorState = do
    csEditState.cedEditMode .= NewPost
    clearEditor

preChangeChannelCommon :: MH ()
preChangeChannelCommon = do
    cId <- use csCurrentChannelId
    csRecentChannel .= Just cId
    saveCurrentEdit

nextChannel :: MH ()
nextChannel = do
    st <- use id
    setFocusWith (getNextNonDMChannel st Z.right)

prevChannel :: MH ()
prevChannel = do
    st <- use id
    setFocusWith (getNextNonDMChannel st Z.left)

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

getNextNonDMChannel :: ChatState
                    -> (Zipper ChannelId -> Zipper ChannelId)
                    -> (Zipper ChannelId -> Zipper ChannelId)
getNextNonDMChannel st shift z =
    if fType z == Direct
    then z
    else go (shift z)
  where go z'
          | fType z' /= Direct = z'
          | otherwise = go (shift z')
        fType onz = st^.(csChannels.to
                          (findChannelById (Z.focus onz))) ^?! _Just.ccInfo.cdType

getNextUnreadChannel :: ChatState
                     -> (Zipper ChannelId -> Zipper ChannelId)
getNextUnreadChannel st =
    -- The next channel with unread messages must also be a channel
    -- other than the current one, since the zipper may be on a channel
    -- that has unread messages and will stay that way until we leave
    -- it- so we need to skip that channel when doing the zipper search
    -- for the next candidate channel.
    Z.findRight (\cId -> hasUnread st cId && (cId /= st^.csCurrentChannelId))

listThemes :: MH ()
listThemes = do
    let themeList = T.intercalate "\n\n" $
                    "Available built-in themes:" :
                    (("  " <>) <$> internalThemeName <$> internalThemes)
    postInfoMessage themeList

setTheme :: T.Text -> MH ()
setTheme name =
    case lookupTheme name of
        Nothing -> listThemes
        Just it -> csResources.crTheme .=
            (themeToAttrMap $ internalTheme it)

channelPageUp :: MH ()
channelPageUp = do
  cId <- use csCurrentChannelId
  mh $ vScrollBy (viewportScroll (ChannelMessages cId)) (-1 * pageAmount)

channelPageDown :: MH ()
channelPageDown = do
  cId <- use csCurrentChannelId
  mh $ vScrollBy (viewportScroll (ChannelMessages cId)) pageAmount

channelScrollUp :: MH ()
channelScrollUp = do
  cId <- use csCurrentChannelId
  mh $ vScrollBy (viewportScroll (ChannelMessages cId)) (-1)

channelScrollDown :: MH ()
channelScrollDown = do
  cId <- use csCurrentChannelId
  mh $ vScrollBy (viewportScroll (ChannelMessages cId)) 1

channelScrollToTop :: MH ()
channelScrollToTop = do
  cId <- use csCurrentChannelId
  mh $ vScrollToBeginning (viewportScroll (ChannelMessages cId))

channelScrollToBottom :: MH ()
channelScrollToBottom = do
  cId <- use csCurrentChannelId
  mh $ vScrollToEnd (viewportScroll (ChannelMessages cId))

-- | Fetches additional message history for the current channel.  This
-- is generally called when in ChannelScroll mode, in which state the
-- output is cached and seen via a scrolling viewport; new messages
-- received in this mode are not normally shown, but this explicit
-- user-driven fetch should be displayed, so this also invalidates the
-- cache.
asyncFetchMoreMessages :: MH ()
asyncFetchMoreMessages = do
    cId  <- use csCurrentChannelId
    withChannel cId $ \chan ->
        let offset = max 0 $ length (chan^.ccContents.cdMessages) - 2
            -- Fetch more messages prior to any existing messages, but
            -- attempt to overlap with existing messages for
            -- determining contiguity or gaps.  Back up two messages
            -- and request from there backward, which should include
            -- the last message in the response.  This is an attempt
            -- to fetch *more* messages, so it's expected that there
            -- are at least 2 messages already here, but in case there
            -- aren't, just get another page from roughly the right
            -- location.
            first' = splitMessagesOn (^.mPostId.to isJust) (chan^.ccContents.cdMessages)
            second' = splitMessagesOn (^.mPostId.to isJust) $ snd $ snd first'
            query = MM.defaultPostQuery
                      { MM.postQueryPage = Just (offset `div` pageAmount)
                      , MM.postQueryPerPage = Just pageAmount
                      }
                    & \q -> case (fst first', fst second' >>= (^.mPostId)) of
                             (Just _, Just i) -> q { MM.postQueryBefore = Just i
                                                  , MM.postQueryPage   = Just 0
                                                  }
                             _ -> q
        in doAsyncChannelMM Preempt cId
               (\s _ c -> MM.mmGetPostsForChannel c query s)
               (\c p -> do addObtainedMessages c (-pageAmount) p >>= postProcessMessageAdd
                           mh $ invalidateCacheEntry (ChannelMessages cId))


addNewPostedMessage :: PostToAdd -> MH ()
addNewPostedMessage p =
    addMessageToState p >>= postProcessMessageAdd


addObtainedMessages :: ChannelId -> Int -> Posts -> MH PostProcessMessageAdd
addObtainedMessages _ _ posts | null (F.toList (posts^.postsOrderL)) = return NoAction
addObtainedMessages cId reqCnt posts = do
    -- Adding a block of server-provided messages, which are known to
    -- be contiguous.  Locally this may overlap with some UnknownGap
    -- messages, which can therefore be removed.  Alternatively the
    -- new block may be discontiguous with the local blocks, in which
    -- case the new block should be surrounded by UnknownGaps.
    withChannelOrDefault cId NoAction $ \chan -> do
        let pIdList = F.toList (posts^.postsOrderL)
            -- the first and list PostId in the batch to be added
            earliestPId = last pIdList
            latestPId = head pIdList
            earliestDate = postCreateAt $ (posts^.postsPostsL) HM.! earliestPId
            latestDate = postCreateAt $ (posts^.postsPostsL) HM.! latestPId

            localMessages = chan^.ccContents . cdMessages

            match = snd $ removeMatchesFromSubset
                          (\m -> maybe False (\p -> p `elem` pIdList) (m^.mPostId))
                          (Just earliestPId) (Just latestPId) localMessages

            dupPIds = catMaybes $ foldr (\m l -> m^.mPostId : l) [] match

            -- If there were any matches, then there was overlap of
            -- the new messages with existing messages.

            -- Don't re-add matching messages (avoid overhead like
            -- re-checking/re-fetching related post information, and
            -- do not signal action needed for notifications), and
            -- remove any gaps in the overlapping region.

            newGapMessage d = newMessageOfType "Additional messages???" (C UnknownGap) d

            -- If this batch contains the latest known messages, do
            -- not add a following gap.  A gap at this point is added
            -- by a websocket disconnect, and any fetches thereafter
            -- are assumed to be the most current information (until
            -- another disconnect), so no gap is needed.
            -- Additionally, the presence of a gap at the end for a
            -- connected client causes a fetch of messages at this
            -- location, so adding the gap here would cause an
            -- infinite update loop.

            addingAtEnd = maybe True ((<=) latestDate) $
                          (^.mDate) <$> getLatestPostMsg localMessages

            addingAtStart = maybe True ((>=) earliestDate) $
                            (^.mDate) <$> getEarliestPostMsg localMessages
            removeStart = if addingAtStart && noMoreBefore then Nothing else Just earliestPId
            removeEnd = if addingAtEnd then Nothing else Just latestPId

            noMoreBefore = reqCnt < 0 && length pIdList < (-reqCnt)
            noMoreAfter = reqCnt > 0 && length pIdList < reqCnt

        -- Add all the new *unique* posts into the existing channel
        -- corpus, generating needed fetches of data associated with
        -- the post, and determining an notification action to be
        -- taken (if any).

        action <- foldr mappend mempty <$>
          mapM (addMessageToState . OldPost)
                   [ (posts^.postsPostsL) HM.! p
                   | p <- F.toList (posts^.postsOrderL)
                   , not (p `elem` dupPIds)
                   ]

        csChannels %= modifyChannelById cId
                           (ccContents.cdMessages %~ (fst . removeMatchesFromSubset isGap removeStart removeEnd))

        -- Add a gap at each end of the newly fetched data, unless:
        --   1. there is an overlap
        --   2. there is no more in the indicated direction
        --      a. indicated by adding messages later than any currently
        --         held messages (see note above re 'addingAtEnd').
        --      b. the amount returned was less than the amount requested

        unless (earliestPId `elem` dupPIds || noMoreBefore) $
               let gapMsg = newGapMessage (justBefore earliestDate)
               in csChannels %= modifyChannelById cId
                       (ccContents.cdMessages %~ addMessage gapMsg)

        unless (latestPId `elem` dupPIds || addingAtEnd || noMoreAfter) $
               let gapMsg = newGapMessage (justAfter latestDate)
               in csChannels %= modifyChannelById cId
                                 (ccContents.cdMessages %~ addMessage gapMsg)

        -- Now initiate fetches for use information for any
        -- as-yet-unknown users related to this new set of messages

        let users = foldr (\post -> Set.insert (postUserId post)) Set.empty (posts^.postsPostsL)
            addUserIfUnknown st uId = case st^.csUsers.to (findUserById uId) of
                                        Just _  -> return ()
                                        Nothing -> handleNewUser uId
        st <- use id
        mapM_ (addUserIfUnknown st) $ catMaybes $ F.toList users

        -- Return the aggregated user notification action needed
        -- relative to the set of added messages.

        return action


loadMoreMessages :: MH ()
loadMoreMessages = do
    mode <- use csMode
    when (mode == ChannelScroll) asyncFetchMoreMessages

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | normalChannelSigil `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | userSigil `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | otherwise            = st ^. csNames . cnToChanId . at n

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannel :: T.Text -> MH ()
changeChannel name = do
    st <- use id
    case channelByName st name of
      Just cId -> setFocus cId
      Nothing  -> attemptCreateDMChannel name

setFocus :: ChannelId -> MH ()
setFocus cId = setFocusWith (Z.findRight (== cId))

setFocusWith :: (Zipper ChannelId -> Zipper ChannelId) -> MH ()
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

attemptCreateDMChannel :: T.Text -> MH ()
attemptCreateDMChannel name = do
  users <- use (csNames.cnUsers)
  nameToChanId <- use (csNames.cnToChanId)
  myName <- use (csMe.userUsernameL)
  if name == myName
    then postErrorMessage ("Cannot create a DM channel with yourself")
    else if name `elem` users && not (name `HM.member` nameToChanId)
      then do
        -- We have a user of that name but no channel. Time to make one!
        myId <- use (csMe.userIdL)
        Just uId <- use (csNames.cnToUserId.at(name))
        session <- use (csResources.crSession)
        doAsyncWith Normal $ do
          -- create a new channel
          nc <- MM.mmCreateDirectMessageChannel (uId, myId) session -- tId uId
          cwd <- MM.mmGetChannel (getId nc) session
          member <- MM.mmGetChannelMember (getId nc) UserMe session
          return $ handleNewChannel True cwd member
      else
        postErrorMessage ("No channel or user named " <> name)

createOrdinaryChannel :: T.Text -> MH ()
createOrdinaryChannel name  = do
  session <- use (csResources.crSession)
  myTeamId <- use (csMyTeam.teamIdL)
  doAsyncWith Preempt $ do
    -- create a new chat channel
    let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
        minChannel = MinChannel
          { minChannelName        = slug
          , minChannelDisplayName = name
          , minChannelPurpose     = Nothing
          , minChannelHeader      = Nothing
          , minChannelType        = Ordinary
          , minChannelTeamId      = myTeamId
          }
    tryMM (do c <- MM.mmCreateChannel minChannel session
              chan <- MM.mmGetChannel (getId c) session
              member <- MM.mmGetChannelMember (getId c) UserMe session
              return (chan, member)
          )
          (return . uncurry (handleNewChannel True))

handleNewChannel :: Bool -> Channel -> ChannelMember -> MH ()
handleNewChannel = handleNewChannel_ True

handleNewChannel_ :: Bool
                  -- ^ Whether to permit this call to recursively
                  -- schedule itself for later if it can't locate
                  -- a DM channel user record. This is to prevent
                  -- uncontrolled recursion.
                  -> Bool
                  -- ^ Whether to switch to the new channel once it has
                  -- been installed.
                  -> Channel
                  -- ^ The channel to install.
                  -> ChannelMember
                  -> MH ()
handleNewChannel_ permitPostpone switch nc member = do
  -- Only add the channel to the state if it isn't already known.
  mChan <- preuse (csChannel(getId nc))
  case mChan of
      Just _ -> return ()
      Nothing -> do
        -- Create a new ClientChannel structure
        cChannel <- (ccInfo %~ channelInfoFromChannelWithData nc member) <$>
                   makeClientChannel nc

        st <- use id

        -- Add it to the message map, and to the name map so we can look
        -- it up by name. The name we use for the channel depends on its
        -- type:
        let chType = nc^.channelTypeL

        -- Get the channel name. If we couldn't, that means we have
        -- async work to do before we can register this channel (in
        -- which case abort because we got rescheduled).
        mName <- case chType of
            Direct -> case userIdForDMChannel (st^.csMe.userIdL) $ channelName nc of
                -- If this is a direct channel but we can't extract a
                -- user ID from the name, then it failed to parse. We
                -- need to assign a channel name in our channel map,
                -- and the best we can do to preserve uniqueness is to
                -- use the channel name string. This is undesirable
                -- but direct channels never get rendered directly;
                -- they only get used by first looking up usernames.
                -- So this name should never appear anywhere, but at
                -- least we can go ahead and register the channel and
                -- handle events for it. That isn't very useful but it's
                -- probably better than ignoring this entirely.
                Nothing -> return $ Just $ channelName nc
                Just otherUserId ->
                    case getUsernameForUserId st otherUserId of
                        -- If we found a user ID in the channel name
                        -- string but don't have that user's metadata,
                        -- postpone adding this channel until we have
                        -- fetched the metadata. This can happen when
                        -- we have a channel record for a user that
                        -- is no longer in the current team. To avoid
                        -- recursion due to a problem, ensure that
                        -- the rescheduled new channel handler is not
                        -- permitted to try this again.
                        --
                        -- If we're already in a recursive attempt to
                        -- register this channel and still couldn't find
                        -- a username, just bail and use the synthetic
                        -- name (this has the same problems as above).
                        Nothing -> do
                            case permitPostpone of
                                False -> return $ Just $ channelName nc
                                True -> do
                                    handleNewUser otherUserId
                                    doAsyncWith Normal $
                                        return $ handleNewChannel_ False switch nc member
                                    return Nothing
                        Just ncUsername ->
                            return $ Just $ ncUsername
            _ -> return $ Just $ preferredChannelName nc

        case mName of
            Nothing -> return ()
            Just name -> do
                addChannelName chType (getId nc) name

                csChannels %= addChannel (getId nc) cChannel

                -- We should figure out how to do this better: this adds
                -- it to the channel zipper in such a way that we don't
                -- ever change our focus to something else, which is
                -- kind of silly
                names <- use csNames
                let newZip = Z.updateList (mkChannelZipperList names)
                csFocus %= newZip

                -- Finally, set our focus to the newly created channel
                -- if the caller requested a change of channel.
                when switch $ setFocus (getId nc)

editMessage :: Post -> MH ()
editMessage new = do
  myId <- use (csMe.userIdL)
  let isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage (toClientPost new (new^.postParentIdL))
      chan = csChannel (new^.postChannelIdL)
  chan . ccContents . cdMessages . traversed . filtered isEditedMessage .= msg

  when (postUserId new /= Just myId) $
      chan %= adjustEditedThreshold new

  chan %= adjustUpdated new
  csPostMap.ix(postId new) .= msg
  asyncFetchReactionsForPost (postChannelId new) new
  asyncFetchAttachments new
  cId <- use csCurrentChannelId
  when (postChannelId new == cId) updateViewed

deleteMessage :: Post -> MH ()
deleteMessage new = do
  let isDeletedMessage m = m^.mPostId == Just (new^.postIdL) ||
                           isReplyTo (new^.postIdL) m
      chan :: Traversal' ChatState ClientChannel
      chan = csChannel (new^.postChannelIdL)
  chan.ccContents.cdMessages.traversed.filtered isDeletedMessage %= (& mDeleted .~ True)
  chan %= adjustUpdated new
  cId <- use csCurrentChannelId
  when (postChannelId new == cId) updateViewed

maybeRingBell :: MH ()
maybeRingBell = do
    doBell <- use (csResources.crConfiguration.to configActivityBell)
    when doBell $ do
        vty <- mh getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

-- | PostProcessMessageAdd is an internal value that informs the main
-- code whether the user should be notified (e.g., ring the bell) or
-- the server should be updated (e.g., that the channel has been
-- viewed).  This is a monoid so that it can be folded over when there
-- are multiple inbound posts to be processed.
data PostProcessMessageAdd = NoAction
                           | NotifyUser
                           | UpdateServerViewed
                           | NotifyUserAndServer

instance Monoid PostProcessMessageAdd where
  mempty = NoAction
  mappend NotifyUserAndServer _         = NotifyUserAndServer
  mappend _ NotifyUserAndServer         = NotifyUserAndServer
  mappend NotifyUser UpdateServerViewed = NotifyUserAndServer
  mappend UpdateServerViewed NotifyUser = NotifyUserAndServer
  mappend x NoAction                    = x
  mappend _ x                           = x

-- | postProcessMessageAdd performs the actual actions indicated by
-- the corresponding input value.
postProcessMessageAdd :: PostProcessMessageAdd -> MH ()
postProcessMessageAdd ppma = postOp ppma
 where
   postOp NoAction            = return ()
   postOp UpdateServerViewed  = updateViewed
   postOp NotifyUser          = maybeRingBell
   postOp NotifyUserAndServer = updateViewed >> maybeRingBell

-- | When we add posts to the application state, we either get them
-- from the server during scrollback fetches (here called 'OldPost') or
-- we get them from websocket events when they are posted in real time
-- (here called 'RecentPost').
data PostToAdd =
    OldPost Post
    -- ^ A post from the server's history
    | RecentPost Post Bool
    -- ^ A message posted to the channel since the user connected, along
    -- with a flag indicating whether the post triggered any of the
    -- user's mentions. We need an extra flag because the server
    -- determines whether the post has any mentions, and that data is
    -- only available in websocket events (and then provided to this
    -- constructor).

-- | Adds a possibly new message to the associated channel contents.
-- Returns an indicator of whether the user should be potentially
-- notified of a change (a new message not posted by this user, a
-- mention of the user, etc.).  This operation has no effect on any
-- existing UnknownGap entries and should be called when those are
-- irrelevant.
addMessageToState :: PostToAdd -> MH PostProcessMessageAdd
addMessageToState newPostData = do
  let (new, wasMentioned) = case newPostData of
        -- A post from scrollback history has no mention data, and
        -- that's okay: we only need to track mentions to tell the user
        -- that recent posts contained mentions.
        OldPost p      -> (p, False)
        RecentPost p m -> (p, m)

  st <- use id
  case st ^? csChannel(postChannelId new) of
      Nothing -> do
          session <- use (csResources.crSession)
          doAsyncWith Preempt $ do
              nc <- MM.mmGetChannel (postChannelId new) session
              member <- MM.mmGetChannelMember (postChannelId new) UserMe session

              let chType = nc^.channelTypeL
                  pref = showGroupChannelPref (postChannelId new) (st^.csMe.userIdL)

              return $ do
                  -- If the incoming message is for a group channel we
                  -- don't know about, that's because it was previously
                  -- hidden by the user. We need to show it, and to do
                  -- that we need to update the server-side preference.
                  -- (That, in turn, triggers a channel refresh.)
                  if chType == Group
                      then applyPreferenceChange pref
                      else refreshChannel nc member

                  addMessageToState newPostData >>= postProcessMessageAdd

          return NoAction
      Just _ -> do
          let cp = toClientPost new (new^.postParentIdL)
              fromMe = (cp^.cpUser == (Just $ getId (st^.csMe))) &&
                       (isNothing $ cp^.cpUserOverride)
              cId = postChannelId new

              doAddMessage = do
                currCId <- use csCurrentChannelId
                flags <- use (csResources.crFlaggedPosts)
                let msg' = clientPostToMessage cp
                             & mFlagged .~ ((cp^.cpPostId) `Set.member` flags)
                csPostMap.at(postId new) .= Just msg'
                csChannels %= modifyChannelById cId
                  ((ccContents.cdMessages %~ addMessage msg') .
                   (adjustUpdated new) .
                   (\c -> if currCId == cId
                          then c
                          else updateNewMessageIndicator new c) .
                   (\c -> if wasMentioned
                          then c & ccInfo.cdMentionCount %~ succ
                          else c)
                  )
                asyncFetchReactionsForPost cId new
                asyncFetchAttachments new
                postedChanMessage

              doHandleAddedMessage = do
                  -- If the message is in reply to another message,
                  -- try to find it in the scrollback for the post's
                  -- channel. If the message isn't there, fetch it. If
                  -- we have to fetch it, don't post this message to the
                  -- channel until we have fetched the parent.
                  case cp^.cpInReplyToPost of
                      Just parentId ->
                          case getMessageForPostId st parentId of
                              Nothing -> do
                                  doAsyncChannelMM Preempt cId
                                      (\s _ _ -> MM.mmGetThread parentId s)
                                      (\_ p -> do
                                          let postMap = HM.fromList [ ( pId
                                                                      , clientPostToMessage
                                                                        (toClientPost x (x^.postParentIdL))
                                                                      )
                                                                    | (pId, x) <- HM.toList (p^.postsPostsL)
                                                                    ]
                                          csPostMap %= HM.union postMap
                                      )
                              _ -> return ()
                      _ -> return ()

                  doAddMessage

              postedChanMessage =
                withChannelOrDefault (postChannelId new) NoAction $ \chan -> do
                    currCId <- use csCurrentChannelId

                    let notifyPref = notifyPreference (st^.csMe) chan
                        curChannelAction = if postChannelId new == currCId
                                           then UpdateServerViewed
                                           else NoAction
                        originUserAction = if fromMe
                                           then NoAction
                                           else if notifyPref == NotifyOptionAll ||
                                                   (notifyPref == NotifyOptionMention && wasMentioned)
                                                then NotifyUser
                                                else NoAction
                    return $ curChannelAction <> originUserAction

          doHandleAddedMessage

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe NewMessageIndicator
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    return $ cc^.ccInfo.cdNewMessageIndicator

getEditedMessageCutoff :: ChannelId -> ChatState -> Maybe ServerTime
getEditedMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdEditedMessageThreshold


fetchVisibleIfNeeded :: MH ()
fetchVisibleIfNeeded = do
  sts <- use csConnectionStatus
  case sts of
    Connected -> do
       cId <- use csCurrentChannelId
       withChannel cId $ \chan ->
           let msgs = chan^.ccContents.cdMessages.to reverseMessages
               (numRemaining, gapInDisplayable, _, rel'pId, overlap) =
                   foldl gapTrail (numScrollbackPosts, False, Nothing, Nothing, 2) msgs
               gapTrail a@(_,  True, _, _, _) _ = a
               gapTrail a@(0,     _, _, _, _) _ = a
               gapTrail   (a, False, b, c, d) m | isGap m = (a, True, b, c, d)
               gapTrail (remCnt, _, prev'pId, prev''pId, ovl) msg =
                   (remCnt - 1, False, msg^.mPostId <|> prev'pId, prev'pId <|> prev''pId,
                    ovl + if isNothing (msg^.mPostId) then 1 else 0)
               numToReq = numRemaining + overlap
               query = MM.defaultPostQuery
                       { MM.postQueryPage    = Just 0
                       , MM.postQueryPerPage = Just numToReq
                       }
               finalQuery = case rel'pId of
                              Nothing -> query
                              Just pid -> query { MM.postQueryBefore = Just pid }
               op = \s _ c -> MM.mmGetPostsForChannel c finalQuery s
           in when ((not $ chan^.ccContents.cdFetchPending) && gapInDisplayable) $ do
                     csChannel(cId).ccContents.cdFetchPending .= True
                     doAsyncChannelMM Preempt cId op
                         (\c p -> do addObtainedMessages c (-numToReq) p >>= postProcessMessageAdd
                                     csChannel(c).ccContents.cdFetchPending .= False)

    _ -> return ()

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  [ (chanNames ^. cnToChanId) HM.! i
  | i <- chanNames ^. cnChans ] ++
  [ c
  | i <- chanNames ^. cnUsers
  , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]

setChannelTopic :: T.Text -> MH ()
setChannelTopic msg = do
    cId <- use csCurrentChannelId
    let patch = defaultChannelPatch { channelPatchHeader = Just msg }
    doAsyncChannelMM Preempt cId
        (\s _ _ -> MM.mmPatchChannel cId patch s)
        (\_ _ -> return ())

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

showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    mh $ vScrollToBeginning (viewportScroll HelpViewport)
    csMode .= ShowHelp topic

beginChannelSelect :: MH ()
beginChannelSelect = do
    csMode .= ChannelSelect
    csChannelSelectState .= emptyChannelSelectState

-- Select the next match in channel selection mode.
channelSelectNext :: MH ()
channelSelectNext = updateSelectedMatch succ

-- Select the previous match in channel selection mode.
channelSelectPrevious :: MH ()
channelSelectPrevious = updateSelectedMatch pred

-- Update the channel selection mode match cursor. The argument function
-- determines how the new cursor position is computed from the old
-- one. The new cursor position is automatically wrapped around to the
-- beginning or end of the channel selection match list, so cursor
-- transformations do not have to do index validation. If the current
-- match (e.g. the sentinel "") is not found in the match list, this
-- sets the cursor position to the first match, if any.
updateSelectedMatch :: (Int -> Int) -> MH ()
updateSelectedMatch nextIndex = do
    chanMatches <- use (csChannelSelectState.channelMatches)
    usernameMatches <- use (csChannelSelectState.userMatches)
    uList <- use (to sortedUserList)

    csChannelSelectState.selectedMatch %= \oldMatch ->
        -- Make the list of all matches, in display order.
        let unames = HM.keys usernameMatches
            allMatches = concat [ sort $ HM.keys chanMatches
                                , [ u^.uiName | u <- uList
                                  , u^.uiName `elem` unames
                                  ]
                                ]
        in case findIndex (== oldMatch) allMatches of
            Nothing -> if null allMatches
                       then ""
                       else allMatches !! 0
            Just i ->
                let newIndex = if tmpIndex < 0
                               then length allMatches - 1
                               else if tmpIndex >= length allMatches
                                    then 0
                                    else tmpIndex
                    tmpIndex = nextIndex i
                in allMatches !! newIndex

updateChannelSelectMatches :: MH ()
updateChannelSelectMatches = do
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    chanNameMatches <- use (csChannelSelectState.channelSelectInput.to channelNameMatch)
    chanNames   <- use (csNames.cnChans)
    uList       <- use (to sortedUserList)
    let chanMatches = catMaybes (fmap chanNameMatches chanNames)
        usernameMatches = catMaybes (fmap chanNameMatches (fmap _uiName uList))
        mkMap ms = HM.fromList [(channelNameFromMatch m, m) | m <- ms]
    csChannelSelectState.channelMatches .= mkMap chanMatches
    csChannelSelectState.userMatches    .= mkMap usernameMatches
    csChannelSelectState.selectedMatch  %= \oldMatch ->
        -- If the previously selected match is still a possible match,
        -- leave it selected. Otherwise revert to the first available
        -- match.
        let newMatch = if oldMatch `elem` allMatches
                       then oldMatch
                       else firstAvailableMatch
            unames = channelNameFromMatch <$> usernameMatches
            allMatches = concat [ channelNameFromMatch <$> chanMatches
                                , [ u^.uiName | u <- uList
                                  , u^.uiName `elem` unames
                                  ]
                                ]
            firstAvailableMatch = if null allMatches
                                  then ""
                                  else head allMatches
        in newMatch

channelNameMatch :: T.Text -> T.Text -> Maybe ChannelSelectMatch
channelNameMatch patStr chanName =
    if T.null patStr
    then Nothing
    else do
        pat <- parseChannelSelectPattern patStr
        applySelectPattern pat chanName

applySelectPattern :: ChannelSelectPattern -> T.Text -> Maybe ChannelSelectMatch
applySelectPattern (CSP ty pat) chanName = do
    let applyType Infix  | pat `T.isInfixOf`  chanName =
            case T.breakOn pat chanName of
                (pre, post) -> return (pre, pat, T.drop (T.length pat) post)

        applyType Prefix | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType Suffix | pat `T.isSuffixOf` chanName = do
            let (b, a) = T.splitAt (T.length chanName - T.length pat) chanName
            return (b, a, "")

        applyType Equal  | pat == chanName =
            return ("", chanName, "")

        applyType _ = Nothing

    (pre, m, post) <- applyType ty
    return $ ChannelSelectMatch pre m post

parseChannelSelectPattern :: T.Text -> Maybe ChannelSelectPattern
parseChannelSelectPattern pat = do
    (pat1, pfx) <- case "^" `T.isPrefixOf` pat of
        True  -> return (T.tail pat, Just Prefix)
        False -> return (pat, Nothing)

    (pat2, sfx) <- case "$" `T.isSuffixOf` pat1 of
        True  -> return (T.init pat1, Just Suffix)
        False -> return (pat1, Nothing)

    case (pfx, sfx) of
        (Nothing, Nothing)         -> return $ CSP Infix  pat2
        (Just Prefix, Nothing)     -> return $ CSP Prefix pat2
        (Nothing, Just Suffix)     -> return $ CSP Suffix pat2
        (Just Prefix, Just Suffix) -> return $ CSP Equal  pat2
        tys                        -> error $ "BUG: invalid channel select case: " <> show tys

startUrlSelect :: MH ()
startUrlSelect = do
    urls <- use (csCurrentChannel.to findUrls.to V.fromList)
    csMode    .= UrlSelect
    csUrlList .= (listMoveTo (length urls - 1) $ list UrlList urls 2)

stopUrlSelect :: MH ()
stopUrlSelect = csMode .= Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ F.toList $ F.toList <$> msgURLs <$> msgs

-- XXX: move this somewhere more sensible!

-- | The 'nubOn' function removes duplicate elements from a list. In
-- particular, it keeps only the /last/ occurrence of each
-- element. The equality of two elements in a call to @nub f@ is
-- determined using @f x == f y@, and the resulting elements must have
-- an 'Ord' instance in order to make this function more efficient.
nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOn f = snd . go Set.empty
  where go before [] = (before, [])
        go before (x:xs) =
          let (before', xs') = go before xs
              key = f x in
          if key `Set.member` before'
            then (before', xs')
            else (Set.insert key before', x : xs')

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkURL, l^.linkUser))

msgURLs :: Message -> Seq.Seq LinkChoice
msgURLs msg
  | NoUser <- msg^.mUser = mempty
  | otherwise =
  let uid = msg^.mUser
      msgUrls = (\ (url, text) -> LinkChoice (msg^.mDate) uid text url Nothing) <$>
                  (mconcat $ blockGetURLs <$> (F.toList $ msg^.mText))
      attachmentURLs = (\ a ->
                          LinkChoice
                            (msg^.mDate)
                            uid
                            ("attachment `" <> (a^.attachmentName) <> "`")
                            (a^.attachmentURL)
                            (Just (a^.attachmentFileId)))
                       <$> (msg^.mAttachments)
  in msgUrls <> attachmentURLs

openSelectedURL :: MH ()
openSelectedURL = do
  mode <- use csMode
  when (mode == UrlSelect) $ do
    selected <- use (csUrlList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, link) -> do
            opened <- openURL link
            when (not opened) $ do
                let msg = "Config option 'urlOpenCommand' missing; cannot open URL."
                postInfoMessage msg
                csMode .= Main

openURL :: LinkChoice -> MH Bool
openURL link = do
    cfg <- use (csResources.crConfiguration)
    case configURLOpenCommand cfg of
        Nothing ->
            return False
        Just urlOpenCommand -> do
            -- Is the URL referring to an attachment?
            let act = case link^.linkFileId of
                    Nothing -> prepareLink link
                    Just fId -> prepareAttachment fId

            -- Is the URL-opening command interactive? If so, pause
            -- Matterhorn and run the opener interactively. Otherwise
            -- run the opener asynchronously and continue running
            -- Matterhorn interactively.
            case configURLOpenCommandInteractive cfg of
                False -> do
                    st <- use id
                    outputChan <- use (csResources.crSubprocessLog)
                    doAsyncWith Preempt $ do
                        args <- act st
                        runLoggedCommand False outputChan (T.unpack urlOpenCommand)
                                         args Nothing Nothing
                        return $ return ()
                True -> do
                    -- If there isn't a new message cutoff showing in
                    -- the current channel, set one. This way, while the
                    -- user is gone using their interactive URL opener,
                    -- when they return, any messages that arrive in the
                    -- current channel will be displayed as new.
                    curChan <- use csCurrentChannel
                    let msgs = curChan^.ccContents.cdMessages
                    case findLatestUserMessage isEditable msgs of
                        Nothing -> return ()
                        Just m ->
                            case m^.mOriginalPost of
                                Nothing -> return ()
                                Just p ->
                                    case curChan^.ccInfo.cdNewMessageIndicator of
                                        Hide ->
                                            csCurrentChannel.ccInfo.cdNewMessageIndicator .= (NewPostsAfterServerTime (p^.postCreateAtL))
                                        _ -> return ()
                    -- No need to add a gap here: the websocket
                    -- disconnect/reconnect events will automatically
                    -- handle management of messages delivered while
                    -- suspended.

                    mhSuspendAndResume $ \st -> do
                        args <- act st
                        void $ runInteractiveCommand (T.unpack urlOpenCommand) args
                        return $ st & csMode .~ Main

            return True

prepareLink :: LinkChoice -> ChatState -> IO [String]
prepareLink link _ = return [T.unpack $ link^.linkURL]

prepareAttachment :: FileId -> ChatState -> IO [String]
prepareAttachment fId st = do
    -- The link is for an attachment, so fetch it and then
    -- open the local copy.
    let sess = st^.csResources.crSession

    (info, contents) <- concurrently (MM.mmGetMetadataForFile fId sess) (MM.mmGetFile fId sess)
    cacheDir <- getUserCacheDir xdgName

    let dir   = cacheDir </> "files" </> T.unpack (idString fId)
        fname = dir </> T.unpack (fileInfoName info)

    createDirectoryIfMissing True dir
    BS.writeFile fname contents
    return [fname]

runInteractiveCommand :: String
                      -> [String]
                      -> IO (Either String ExitCode)
runInteractiveCommand cmd args = do
    let opener = (proc cmd args) { std_in = Inherit
                                 , std_out = Inherit
                                 , std_err = Inherit
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> return $ Left $ show e
        Right (_, _, _, ph) -> do
            ec <- waitForProcess ph
            return $ Right ec

runLoggedCommand :: Bool
                 -- ^ Whether stdout output is expected for this program
                 -> STM.TChan ProgramOutput
                 -- ^ The output channel to send the output to
                 -> String
                 -- ^ The program name
                 -> [String]
                 -- ^ Arguments
                 -> Maybe String
                 -- ^ The stdin to send, if any
                 -> Maybe (MVar ProgramOutput)
                 -- ^ Where to put the program output when it is ready
                 -> IO ()
runLoggedCommand stdoutOkay outputChan cmd args mInput mOutputVar = void $ forkIO $ do
    let stdIn = maybe NoStream (const CreatePipe) mInput
        opener = (proc cmd args) { std_in = stdIn
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> do
            let po = ProgramOutput cmd args "" stdoutOkay (show e) (ExitFailure 1)
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right (stdinResult, Just outh, Just errh, ph) -> do
            case stdinResult of
                Just inh -> do
                    let Just input = mInput
                    hPutStrLn inh input
                    hFlush inh
                Nothing -> return ()

            ec <- waitForProcess ph
            outResult <- hGetContents outh
            errResult <- hGetContents errh
            let po = ProgramOutput cmd args outResult stdoutOkay errResult ec
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right _ ->
            error $ "BUG: createProcess returned unexpected result, report this at " <>
                    "https://github.com/matterhorn-chat/matterhorn"

openSelectedMessageURLs :: MH ()
openSelectedMessageURLs = do
    mode <- use csMode
    when (mode == MessageSelect) $ do
        Just curMsg <- use (to getSelectedMessage)
        let urls = msgURLs curMsg
        when (not (null urls)) $ do
            openedAll <- and <$> mapM openURL urls
            case openedAll of
                True -> csMode .= Main
                False -> do
                    let msg = "Config option 'urlOpenCommand' missing; cannot open URL."
                    postInfoMessage msg

shouldSkipMessage :: T.Text -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = T.all (`elem` (" \t"::String)) s

sendMessage :: EditMode -> T.Text -> MH ()
sendMessage mode msg =
    case shouldSkipMessage msg of
        True -> return ()
        False -> do
            status <- use csConnectionStatus
            st <- use id
            case status of
                Disconnected -> do
                    let m = "Cannot send messages while disconnected."
                    postErrorMessage m
                Connected -> do
                    let chanId = st^.csCurrentChannelId
                    doAsync Preempt $ do
                      case mode of
                        NewPost -> do
                            let pendingPost = rawPost msg chanId
                            void $ MM.mmCreatePost pendingPost (st^.csResources.crSession)
                        Replying _ p -> do
                            let pendingPost = (rawPost msg chanId) { rawPostRootId = Just (postId p) }
                            void $ MM.mmCreatePost pendingPost (st^.csResources.crSession)
                        Editing p -> do
                            void $ MM.mmUpdatePost (postId p) (postUpdate msg) (st^.csResources.crSession)

handleNewUserDirect :: User -> MH ()
handleNewUserDirect newUser = do
    let usrInfo = userInfoFromUser newUser True
        newUserId = getId newUser
    csUsers %= addUser newUserId usrInfo
    csNames . cnUsers %= (sort . ((newUser^.userUsernameL):))
    csNames . cnToUserId . at (newUser^.userUsernameL) .= Just newUserId
    userSet <- use (csResources.crUserIdSet)
    liftIO $ STM.atomically $ STM.modifyTVar userSet $ (newUserId Seq.<|)

handleNewUser :: UserId -> MH ()
handleNewUser newUserId = doAsyncMM Normal getUserInfo updateUserState
    where getUserInfo session _ =
              do (nUser, users)  <- concurrently (MM.mmGetUser (UserById newUserId) session)
                                                    (MM.mmGetUsers MM.defaultUserQuery
                                                      { MM.userQueryPage = Just 0
                                                      , MM.userQueryPerPage = Just 10000
                                                      } session)
                 let teamUsers = HM.fromList [ (userId u, u) | u <- F.toList users ]
                 -- Also re-load the team members so we can tell
                 -- whether the new user is in the current user's
                 -- team.
                 let usrInfo = userInfoFromUser nUser (HM.member newUserId teamUsers)
                 return (nUser, usrInfo)
          updateUserState :: (User, UserInfo) -> MH ()
          updateUserState (newUser, uInfo) =
              -- Update the name map and the list of known users
              do csUsers %= addUser newUserId uInfo
                 csNames . cnUsers %= (sort . ((newUser^.userUsernameL):))
                 csNames . cnToUserId . at (newUser^.userUsernameL) .= Just newUserId
                 userSet <- use (csResources.crUserIdSet)
                 liftIO $ STM.atomically $ STM.modifyTVar userSet $ (newUserId Seq.<|)

-- | Handle the typing events from the websocket to show the currently typing users on UI
handleTypingUser :: UserId -> ChannelId -> MH ()
handleTypingUser uId cId = do
  config <- use (csResources.crConfiguration)
  when (configShowTypingIndicator config) $ do
    ts <- liftIO getCurrentTime -- get time now
    csChannels %= modifyChannelById cId (addChannelTypingUser uId ts)
