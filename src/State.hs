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
  , getNewMessageCutoff
  , getEditedMessageCutoff
  , setChannelTopic
  , fetchCurrentChannelMembers
  , refreshChannelById
  , handleChannelInvite
  , addUserToCurrentChannel
  , removeUserFromCurrentChannel

  -- * Channel history
  , channelHistoryForward
  , channelHistoryBackward

  -- * Working with messages
  , PostToAdd(..)
  , sendMessage
  , msgURLs
  , editMessage
  , deleteMessage
  , addMessageToState
  , postProcessMessageAdd

  -- * Working with users
  , handleNewUser
  , updateStatus

  -- * Startup/reconnect management
  , refreshChannelsAndUsers

  -- * Channel selection mode
  , beginChannelSelect
  , updateChannelSelectMatches
  , channelSelectNext
  , channelSelectPrevious

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
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list, listMoveTo, listSelectedElement)
import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (MVar, putMVar, forkIO)
import qualified Control.Concurrent.STM as STM
import           Data.Char (isAlphaNum)
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToBeginning, vScrollBy, vScrollToEnd)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Monad (when, unless, void, forM_)
import qualified Data.ByteString as BS
import           Data.Function (on)
import           Data.Text.Zipper (textZipper, clearZipper, insertMany, gotoEOL)
import           Data.Time.Clock (UTCTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.List (sort, findIndex)
import           Data.Maybe (maybeToList, isJust, catMaybes, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
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

import           Network.Mattermost
import           Network.Mattermost.Types (NotifyOption(..))
import           Network.Mattermost.Lenses

import           Config
import           FilePaths
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
import           State.Setup.Threads (updateUserStatuses)

-- * Refreshing Channel Data

-- | Refresh information about a specific channel.  The channel
-- metadata is refreshed, and if this is a loaded channel, the
-- scrollback is updated as well.
refreshChannel :: Bool -> ChannelWithData -> MH ()
refreshChannel refreshMessages cwd@(ChannelWithData chan _) = do
  let cId = getId chan
  curId <- use csCurrentChannelId

  -- If this channel is unknown, register it first.
  mChan <- preuse (csChannel(cId))
  when (isNothing mChan) $
      handleNewChannel False cwd

  updateChannelInfo cId cwd

  -- If this is an active channel or the current channel, also update
  -- the Messages to retrieve any that might have been missed.
  when (refreshMessages || (cId == curId)) $
      updateMessages cId

refreshChannelById :: Bool -> ChannelId -> MH ()
refreshChannelById refreshMessages cId = do
  session <- use (csResources.crSession)
  myTeamId <- use (csMyTeam.teamIdL)
  doAsyncWith Preempt $ do
      cwd <- mmGetChannel session myTeamId cId
      return $ refreshChannel refreshMessages cwd

-- | Refresh information about all channels and users. This is usually
-- triggered when a reconnect event for the WebSocket to the server
-- occurs.
refreshChannelsAndUsers :: MH ()
refreshChannelsAndUsers = do
  session <- use (csResources.crSession)
  myTeamId <- use (csMyTeam.teamIdL)
  myId <- use (csMe.userIdL)
  doAsyncWith Preempt $ do
    chansWithData <- mmGetAllChannelsWithDataForUser session myTeamId myId
    uMap <- mmGetProfiles session myTeamId 0 10000
    return $ do
        forM_ (HM.elems uMap) $ \u -> do
            when (not $ userDeleted u) $ do
                knownUsers <- use csUsers
                case findUserById (getId u) knownUsers of
                    Just _ -> return ()
                    Nothing -> handleNewUserDirect u

        forM_ (HM.elems chansWithData) $ refreshChannel True

        lock <- use (csResources.crUserStatusLock)
        doAsyncWith Preempt $ updateUserStatuses lock session

-- | Update the indicted Channel entry with the new data retrieved from
-- the Mattermost server. Also update the channel name if it changed.
updateChannelInfo :: ChannelId -> ChannelWithData -> MH ()
updateChannelInfo cid cwd@(ChannelWithData new _) = do
  mOldChannel <- preuse $ csChannel(cid)
  case mOldChannel of
      Nothing -> return ()
      Just old ->
          let oldName = old^.ccInfo.cdName
              newName = channelName new
          in if oldName == newName
             then return ()
             else do
                 removeChannelName oldName
                 addChannelName (channelType new) cid newName

  csChannel(cid).ccInfo %= channelInfoFromChannelWithData cwd

addChannelName :: Type -> ChannelId -> T.Text -> MH ()
addChannelName chType cid name = do
    csNames.cnToChanId.at(name) .= Just cid

    -- For direct channels the username is already in the user list so
    -- do nothing
    when (chType /= Direct) $
        csNames.cnChans %= (sort . (name:))

removeChannelName :: T.Text -> MH ()
removeChannelName name = do
    -- Flush cnToChanId
    csNames.cnToChanId.at name .= Nothing
    -- Flush cnChans
    csNames.cnChans %= filter (/= name)

-- | If this channel has content, fetch any new content that has
-- arrived after the existing content.
updateMessages :: ChannelId -> MH ()
updateMessages cId =
  withChannel cId $ \chan -> do
    when (chan^.ccInfo.cdCurrentState.to (`elem` [ChanLoaded, ChanInitialSelect])) $ do
      curId <- use csCurrentChannelId
      let priority = if curId == cId then Preempt else Normal
      asyncFetchScrollback priority cId

-- | Fetch scrollback for a channel in the background.  This may be
-- called to fetch messages in a number of situations, including:
--
--   1. WebSocket connect at init, no messages available
--
--   2. WebSocket reconnect after losing connectivity for a period
--
--   3. Channel selected by user
--
--      a. No current messages fetched yet
--
--      b. Messages may have been provided unsolicited via the
--         WebSocket.
--
--   4. User got invited to the channel (by another user).
--
-- For most cases, fetching the most recent set of messages is
-- appropriate, but for case 2, messages from the most recent forward
-- should be retrieved.  However, be careful not to confuse case 2
-- with case 3b.
asyncFetchScrollback :: AsyncPriority -> ChannelId -> MH ()
asyncFetchScrollback prio cId = do
  withChannel cId $ \chan -> do
    let last_pId = getLatestPostId (chan^.ccContents.cdMessages)
        newCutoff = chan^.ccInfo.cdNewMessageIndicator
        fetchMessages s t c = do
            let fc = case last_pId of
                  Nothing  -> F1  -- or F4
                  Just pId ->
                      case findMessage pId (chan^.ccContents.cdMessages) of
                        Nothing -> F4 -- This should never happen since
                                      -- we just assigned pId.
                        Just m ->
                            case newCutoff of
                                Hide ->
                                    -- No cutoff has been set, so we
                                    -- just ask for the most recent
                                    -- messages.
                                    F2 pId
                                NewPostsAfterServerTime ct ->
                                    -- If the most recent message is
                                    -- after the cutoff, meaning there
                                    -- might be intervening messages
                                    -- that we missed.
                                    if m^.mDate > ct
                                    then F3b pId
                                    else F3a
                                NewPostsStartingAt ct ->
                                    -- If the most recent message is
                                    -- after the cutoff, meaning there
                                    -- might be intervening messages
                                    -- that we missed.
                                    if m^.mDate >= ct
                                    then F3b pId
                                    else F3a
                op = case fc of
                    F1      -> mmGetPosts s t c
                    F2 pId  -> mmGetPostsAfter s t c pId
                    F3a     -> mmGetPosts s t c
                    F3b pId -> mmGetPostsBefore s t c pId
                    F4      -> mmGetPosts s t c
            op 0 numScrollbackPosts

    asPending doAsyncChannelMM prio cId fetchMessages
              addObtainedMessages

data FetchCase = F1 | F2 PostId | F3a | F3b PostId | F4 deriving (Eq,Show)

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
    let recentPost = getLatestPostId chanMsgs

    when (isJust recentPost) $ do
        csMode .= MessageSelect
        csMessageSelect .= MessageSelectState recentPost

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
                      (\s t c -> mmDeletePost s t c (postId p))
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

-- | Update the UI to reflect the flagged/unflagged state of a
-- message. This __does not__ talk to the Mattermost server, but
-- rather is the function we call when the Mattermost server notifies
-- us of flagged or unflagged messages.
updateMessageFlag :: PostId -> Bool -> MH ()
updateMessageFlag pId f = do
  if f
    then csResources.crFlaggedPosts %= Set.insert pId
    else csResources.crFlaggedPosts %= Set.delete pId
  msgMb <- use (csPostMap.at(pId))
  case msgMb of
    Just msg
      | Just cId <- msg^.mChannelId -> do
      let isTargetMessage m = m^.mPostId == Just pId
      csChannel(cId).ccContents.cdMessages.traversed.filtered isTargetMessage.mFlagged .= f
      csPostMap.ix(pId).mFlagged .= f
      -- We also want to update the post overlay if this happens while
      -- we're we're observing it
      mode <- use csMode
      case mode of
        PostListOverlay PostListFlagged
          | f ->
              csPostListOverlay.postListPosts %=
                addMessage (msg & mFlagged .~ True)
          -- deleting here is tricky, because it means that we need to
          -- move the focus somewhere: we'll try moving it _up_ unless
          -- we can't, in which case we'll try moving it down.
          | otherwise -> do
              selId <- use (csPostListOverlay.postListSelected)
              posts <- use (csPostListOverlay.postListPosts)
              let nextId = case getNextPostId selId posts of
                    Nothing -> getPrevPostId selId posts
                    Just x  -> Just x
              csPostListOverlay.postListSelected .= nextId
              csPostListOverlay.postListPosts %=
                filterMessages (((/=) `on` _mPostId) msg)
        _ -> return ()
    _ -> return ()

-- | Tell the server that we have flagged or unflagged a message.
flagMessage :: PostId -> Bool -> MH ()
flagMessage pId f = do
  session <- use (csResources.crSession)
  myId <- use (csMe.userIdL)
  doAsyncWith Normal $ do
    let doFlag = if f then mmFlagPost else mmUnflagPost
    doFlag session myId pId
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
    doAsyncWith Preempt $ do
        -- We don't get to just request all channels, so we request channels in
        -- chunks of 50.  A better UI might be to request an initial set and
        -- then wait for the user to demand more.
        let fetchCount     = 50
            loop acc start = do
              newChans <- mmGetMoreChannels session myTeamId start fetchCount
              let chans = acc <> newChans
              if length newChans < fetchCount
                then return chans
                else loop chans (start+fetchCount)
        chans <- loop mempty 0
        return $ do
            csJoinChannelList .= (Just $ list JoinChannelList (V.fromList $ F.toList chans) 1)

    csMode .= JoinChannel
    csJoinChannelList .= Nothing

joinChannel :: Channel -> MH ()
joinChannel chan = do
    csMode .= Main
    doAsyncChannelMM Preempt (getId chan) mmJoinChannel endAsyncNOP

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> MH ()
handleChannelInvite cId = do
    st <- use id
    doAsyncWith Normal $ do
        tryMM (mmGetChannel (st^.csResources.crSession) (st^.csMyTeam.teamIdL) cId)
              (\cwd -> return $ do
                  handleNewChannel False cwd
                  asyncFetchScrollback Normal cId)

addUserToCurrentChannel :: T.Text -> MH ()
addUserToCurrentChannel uname = do
    -- First: is this a valid username?
    usrs <- use csUsers
    case findUserByName usrs uname of
        Just (uid, _) -> do
            cId <- use csCurrentChannelId
            session <- use (csResources.crSession)
            myTeamId <- use (csMyTeam.teamIdL)
            doAsyncWith Normal $ do
                tryMM (void $ mmChannelAddUser session myTeamId cId uid)
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
                tryMM (void $ mmChannelRemoveUser session cId uid)
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
                                    True -> mmDeleteChannel
                                    False -> mmLeaveChannel
                                _ -> if delete
                                     then mmDeleteChannel
                                     else mmLeaveChannel

                        doAsyncChannelMM Preempt cId func endAsyncNOP
                    )

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
fetchChannelMembers s t c = do
    chanUserMap <- mmGetChannelMembers s t c 0 10000
    return $ snd <$> HM.toList chanUserMap

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
      doAsyncChannelMM Preempt cId mmGetChannel
      (\pcid cwd -> csChannel(pcid).ccInfo %= channelInfoFromChannelWithData cwd)
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
            (\s t c -> mmViewChannel s t c pId)
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
    fetchCurrentScrollback
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
    let mkThemeList _ = T.intercalate "\n\n" $
                        "Available built-in themes:" :
                        (("  " <>) <$> fst <$> themes)
    postInfoMessage (mkThemeList themes)

setTheme :: T.Text -> MH ()
setTheme name =
    case lookup name themes of
        Nothing -> listThemes
        Just t -> csResources.crTheme .= t

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
        let offset = length $ chan^.ccContents.cdMessages
        in asPending doAsyncChannelMM Preempt cId
               (\s t c -> mmGetPosts s t c offset pageAmount)
               (\c p -> do addObtainedMessages c p
                           mh $ invalidateCacheEntry (ChannelMessages cId))

addObtainedMessages :: ChannelId -> Posts -> MH ()
addObtainedMessages _cId posts =
    postProcessMessageAdd =<<
        foldl mappend mempty <$>
              mapM (addMessageToState . OldPost)
                       [ (posts^.postsPostsL) HM.! p
                       | p <- F.toList (posts^.postsOrderL)
                       ]

loadMoreMessages :: MH ()
loadMoreMessages = do
    mode <- use csMode
    when (mode == ChannelScroll) asyncFetchMoreMessages

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | (T.singleton normalChannelSigil) `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | (T.singleton userSigil) `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
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
        tId <- use (csMyTeam.teamIdL)
        Just uId <- use (csNames.cnToUserId.at(name))
        session <- use (csResources.crSession)
        doAsyncWith Normal $ do
          -- create a new channel
          nc <- mmCreateDirect session tId uId
          cwd <- mmGetChannel session tId (getId nc)
          return $ handleNewChannel True cwd
      else
        postErrorMessage ("No channel or user named " <> name)

createOrdinaryChannel :: T.Text -> MH ()
createOrdinaryChannel name  = do
  tId <- use (csMyTeam.teamIdL)
  session <- use (csResources.crSession)
  doAsyncWith Preempt $ do
    -- create a new chat channel
    let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
        minChannel = MinChannel
          { minChannelName        = slug
          , minChannelDisplayName = name
          , minChannelPurpose     = Nothing
          , minChannelHeader      = Nothing
          , minChannelType        = Ordinary
          }
    tryMM (do c <- mmCreateChannel session tId minChannel
              mmGetChannel session tId (getId c)
          )
          (return . handleNewChannel True)

handleNewChannel :: Bool -> ChannelWithData -> MH ()
handleNewChannel = handleNewChannel_ True

handleNewChannel_ :: Bool
                  -- ^ Whether to permit this call to recursively
                  -- schedule itself for later if it can't locate
                  -- a DM channel user record. This is to prevent
                  -- uncontrolled recursion.
                  -> Bool
                  -- ^ Whether to switch to the new channel once it has
                  -- been installed.
                  -> ChannelWithData
                  -- ^ The channel to install.
                  -> MH ()
handleNewChannel_ permitPostpone switch cwd@(ChannelWithData nc cData) = do
  -- Create a new ClientChannel structure
  let cChannel = makeClientChannel nc &
                   ccInfo %~ channelInfoFromChannelWithData (ChannelWithData nc cData)

  st <- use id

  -- Add it to the message map, and to the name map so we can look it up
  -- by name. The name we use for the channel depends on its type:
  let chType = nc^.channelTypeL

  -- Get the channel name. If we couldn't, that means we have async work
  -- to do before we can register this channel (in which case abort
  -- because we got rescheduled).
  mName <- case chType of
      Direct -> case userIdForDMChannel (st^.csMe.userIdL) $ channelName nc of
          -- If this is a direct channel but we can't extract a user ID
          -- from the name, then it failed to parse. We need to assign
          -- a channel name in our channel map, and the best we can do
          -- to preserve uniqueness is to use the channel name string.
          -- This is undesirable but direct channels never get rendered
          -- directly; they only get used by first looking up usernames.
          -- So this name should never appear anywhere, but at least we
          -- can go ahead and register the channel and handle events for
          -- it. That isn't very useful but it's probably better than
          -- ignoring this entirely.
          Nothing -> return $ Just $ channelName nc
          Just otherUserId ->
              case getUsernameForUserId st otherUserId of
                  -- If we found a user ID in the channel name string
                  -- but don't have that user's metadata, postpone
                  -- adding this channel until we have fetched the
                  -- metadata. This can happen when we have a channel
                  -- record for a user that is no longer in the current
                  -- team. To avoid recursion due to a problem, ensure
                  -- that the rescheduled new channel handler is not
                  -- permitted to try this again.
                  --
                  -- If we're already in a recursive attempt to register
                  -- this channel and still couldn't find a username,
                  -- just bail and use the synthetic name (this has the
                  -- same problems as above).
                  Nothing -> do
                      case permitPostpone of
                          False -> return $ Just $ channelName nc
                          True -> do
                              handleNewUser otherUserId
                              doAsyncWith Normal $
                                  return $ handleNewChannel_ False switch cwd
                              return Nothing
                  Just ncUsername ->
                      return $ Just $ ncUsername
      _ -> return $ Just $ preferredChannelName nc

  case mName of
      Nothing -> return ()
      Just name -> do
          addChannelName chType (getId nc) name

          csChannels %= addChannel (getId nc) cChannel

          -- We should figure out how to do this better: this adds it to
          -- the channel zipper in such a way that we don't ever change
          -- our focus to something else, which is kind of silly
          names <- use csNames
          let newZip = Z.updateList (mkChannelZipperList names)
          csFocus %= newZip

          -- Finally, set our focus to the newly created channel if the
          -- caller requested a change of channel.
          when switch $ setFocus (getId nc)

editMessage :: Post -> MH ()
editMessage new = do
  st <- use id
  myId <- use (csMe.userIdL)
  let isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage st (toClientPost new (new^.postParentIdL))
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
postProcessMessageAdd ppma = do
  postOp ppma
  cState <- use (csCurrentChannel.ccInfo.cdCurrentState)
  when (cState == ChanInitialSelect) $
    csCurrentChannel.ccInfo.cdCurrentState .= ChanLoaded
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
-- Returns True if this is something that should potentially notify
-- the user of a change to the channel (i.e., not a message we
-- posted).
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
          -- When we join channels, sometimes we get the "user has
          -- been added to channel" message here BEFORE we get the
          -- websocket event that says we got added to a channel. This
          -- means the message arriving here in addMessage can't be
          -- added yet because we haven't fetched the channel metadata
          -- in the websocket handler. So to be safe we just drop the
          -- message here, but this is the only case of messages that we
          -- /expect/ to drop for this reason. Hence the check for the
          -- msgMap channel ID key presence above.
          return NoAction
      Just _ -> do
          let cp = toClientPost new (new^.postParentIdL)
              fromMe = (cp^.cpUser == (Just $ getId (st^.csMe))) &&
                       (isNothing $ cp^.cpUserOverride)
              cId = postChannelId new

              doAddMessage = do
                currCId <- use csCurrentChannelId
                s <- use id  -- use *latest* state
                flags <- use (csResources.crFlaggedPosts)
                let msg' = clientPostToMessage s cp
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
                                      (\s t c -> mmGetPost s t c parentId)
                                      (\_ p ->
                                          let postMap = HM.fromList [ ( pId
                                                                      , clientPostToMessage st
                                                                        (toClientPost x (x^.postParentIdL))
                                                                      )
                                                                    | (pId, x) <- HM.toList (p^.postsPostsL)
                                                                    ]
                                          in csPostMap %= HM.union postMap
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

          -- If this message was written by a user we don't know about,
          -- fetch the user's information before posting the message.
          case cp^.cpUser of
              Nothing -> doHandleAddedMessage
              Just uId ->
                  case st^.csUsers.to (findUserById uId) of
                      Just _ -> doHandleAddedMessage
                      Nothing -> do
                          handleNewUser uId
                          doAsyncWith Normal $ return (doHandleAddedMessage >> return ())
                          postedChanMessage

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe NewMessageIndicator
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    return $ cc^.ccInfo.cdNewMessageIndicator

getEditedMessageCutoff :: ChannelId -> ChatState -> Maybe UTCTime
getEditedMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdEditedMessageThreshold

fetchCurrentScrollback :: MH ()
fetchCurrentScrollback = do
  cId <- use csCurrentChannelId
  withChannel cId $ \ chan -> do
    unless (chan^.ccInfo.cdCurrentState `elem` [ChanLoaded, ChanInitialSelect]) $ do
      -- Upgrades the channel state to "Loaded" to indicate that
      -- content is now present (this is the main point where channel
      -- state is switched from metadata-only to with-content), then
      -- initiates an operation to read the content (which will change
      -- the state to a pending for loaded.  If there was an async
      -- background task pending (esp. if this channel was selected
      -- just after startup and startup fetching is still underway),
      -- this will potentially schedule a duplicate, but that will not
      -- be harmful since quiescent channel states only increase to
      -- "higher" states.
      when (chan^.ccInfo.cdCurrentState /= ChanInitialSelect) $
        csChannel(cId).ccInfo.cdCurrentState .= ChanLoaded
      asyncFetchScrollback Preempt cId

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
    doAsyncChannelMM Preempt cId
        (\s t c -> mmSetChannelHeader s t c msg)
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
msgURLs msg | Just uname <- msg^.mUserName =
  let msgUrls = (\ (url, text) -> LinkChoice (msg^.mDate) uname text url Nothing) <$>
                  (mconcat $ blockGetURLs <$> (F.toList $ msg^.mText))
      attachmentURLs = (\ a ->
                          LinkChoice
                            (msg^.mDate)
                            uname
                            ("attachment `" <> (a^.attachmentName) <> "`")
                            (a^.attachmentURL)
                            (Just (a^.attachmentFileId)))
                       <$> (msg^.mAttachments)
  in msgUrls <> attachmentURLs
msgURLs _ = mempty

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

    info     <- mmGetFileInfo sess fId
    contents <- mmGetFile sess fId
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
                    let myId   = st^.csMe.userIdL
                        chanId = st^.csCurrentChannelId
                        theTeamId = st^.csMyTeam.teamIdL
                    doAsync Preempt $ do
                      case mode of
                        NewPost -> do
                            pendingPost <- mkPendingPost msg myId chanId
                            void $ mmPost (st^.csResources.crSession) theTeamId pendingPost
                        Replying _ p -> do
                            pendingPost <- mkPendingPost msg myId chanId
                            let modifiedPost =
                                    pendingPost { pendingPostParentId = Just $ postId p
                                                , pendingPostRootId = Just $ postId p
                                                }
                            void $ mmPost (st^.csResources.crSession) theTeamId modifiedPost
                        Editing p -> do
                            let modifiedPost = p { postMessage = msg
                                                 , postPendingPostId = Nothing
                                                 }
                            void $ mmUpdatePost (st^.csResources.crSession) theTeamId modifiedPost

handleNewUserDirect :: User -> MH ()
handleNewUserDirect newUser = do
    let usrInfo = userInfoFromUser newUser True
        newUserId = getId newUser
    csUsers %= addUser newUserId usrInfo
    csNames . cnUsers %= (sort . ((newUser^.userUsernameL):))
    csNames . cnToUserId . at (newUser^.userUsernameL) .= Just newUserId

handleNewUser :: UserId -> MH ()
handleNewUser newUserId = doAsyncMM Normal getUserInfo updateUserState
    where getUserInfo session team =
              do nUser <- mmGetUser session newUserId
                 -- Also re-load the team members so we can tell
                 -- whether the new user is in the current user's
                 -- team.
                 teamUsers <- mmGetProfiles session team 0 10000
                 let usrInfo = userInfoFromUser nUser (HM.member newUserId teamUsers)
                 return (nUser, usrInfo)
          updateUserState :: (User, UserInfo) -> MH ()
          updateUserState (newUser, uInfo) =
              -- Update the name map and the list of known users
              do csUsers %= addUser newUserId uInfo
                 csNames . cnUsers %= (sort . ((newUser^.userUsernameL):))
                 csNames . cnToUserId . at (newUser^.userUsernameL) .= Just newUserId
