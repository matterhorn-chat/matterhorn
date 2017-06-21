module State where

import           Prelude ()
import           Prelude.Compat

import           Brick (invalidateCacheEntry)
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list, listMoveTo, listSelectedElement)
import           Control.Applicative
import           Control.Exception (SomeException, catch, try)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import           Data.Char (isAlphaNum)
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToBeginning, vScrollBy, vScrollToEnd)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Monad (when, void)
import qualified Data.ByteString as BS
import           Data.Text.Zipper (textZipper, clearZipper, insertMany, gotoEOL)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.List (sort)
import           Data.Maybe (maybeToList, isJust, catMaybes, isNothing)
import           Data.Monoid ((<>))
import           Data.Time.Clock (UTCTime, getCurrentTime)
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
import           System.IO (hGetContents)
import           System.Directory ( createDirectoryIfMissing )
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.FilePath

import           Network.Mattermost
import           Network.Mattermost.Exceptions
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
import           Markdown (blockGetURLs, findVerbatimChunk)

import           State.Common

-- * Hard-coded constants

-- | The number of posts to include per page
pageAmount :: Int
pageAmount = 15

-- * Refreshing Channel Data

-- | Get all the new messages for a given channel. In addition, load the
-- channel metadata and update that, too.
refreshChannel :: ChannelId -> MH ()
refreshChannel chan = do
  msgs <- use (csChannel(chan).ccContents.cdMessages)
  session <- use csSession
  myTeamId <- use (csMyTeam.teamIdL)
  doAsyncWith Normal $
    case getLatestPostId msgs of
    Just pId -> do
      -- Get the latest channel metadata.
      cwd <- mmGetChannel session myTeamId chan

      -- Load posts since the last post in this channel.  Note that
      -- postsOrder from mattermost-api is most recent first.
      posts <- mmGetPostsAfter session myTeamId chan pId 0 100
      return $ do
        mapM_ addMessageToState [ (posts^.postsPostsL) HM.! p
                                | p <- F.toList (posts^.postsOrderL)
                                ]
        let newChanInfo ci = channelInfoFromChannelWithData cwd ci
                               & cdCurrentState .~ ChanLoaded

        csChannel(chan).ccInfo %= newChanInfo
    _ -> return (return ())

-- | Find all the loaded channels and refresh their state, setting the
-- state as dirty until we get a response
refreshLoadedChannels :: MH ()
refreshLoadedChannels = do
  let isChanLoaded cc = cc^.ccInfo.cdCurrentState == ChanLoaded
      setChanToRefreshing = ccInfo.cdCurrentState .~ ChanRefreshing
  cIds <- use (csChannels.to (filteredChannelIds isChanLoaded))
  csChannels %= flip (foldr ((flip modifyChannelById) setChanToRefreshing)) cIds
  sequence_ $ refreshChannel <$> cIds

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

isMine :: ChatState -> Message -> Bool
isMine st msg = (Just $ st^.csMe.userUsernameL) == msg^.mUserName

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
    case selectedMessage of
        Just msg | isMine st msg && isDeletable msg -> do
            cId <- use csCurrentChannelId
            session <- use csSession
            myTeamId <- use (csMyTeam.teamIdL)
            doAsyncWith Preempt $ do
                let Just p = msg^.mOriginalPost
                mmDeletePost session myTeamId cId (postId p)
                return $ do
                    csEditState.cedEditMode .= NewPost
                    csMode .= Main
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
    leaveCurrentChannel
    csMode .= Main

    cId <- use csCurrentChannelId
    session <- use csSession
    myTeamId <- use (csMyTeam.teamIdL)
    doAsyncWith Normal $ do
        mmDeleteChannel session myTeamId cId
        return $ return ()

isCurrentChannel :: ChatState -> ChannelId -> Bool
isCurrentChannel st cId = st^.csCurrentChannelId == cId

setMessageFlag :: PostId -> Bool -> MH ()
setMessageFlag pId f = do
  msgMb <- use (csPostMap.at(pId))
  case msgMb of
    Just msg
      | Just orig <- msg^.mOriginalPost -> do
      let isTargetMessage m = m^.mPostId == Just pId
          chan = csChannel(orig^.postChannelIdL)
      chan.ccContents.cdMessages.traversed.filtered isTargetMessage.mFlagged .= f
      csPostMap.ix(pId).mFlagged .= f
    _ -> return ()

flagMessage :: MH ()
flagMessage = do
  selected <- use (to getSelectedMessage)
  session <- use csSession
  myId <- use (csMe.userIdL)
  case selected of
    Just msg
      | Just pId <- msg^.mPostId -> do
          let f = msg^.mFlagged
          csMode .= Main
          doAsyncWith Normal $ do
            let doFlag = if f then mmUnflagPost else mmFlagPost
            doFlag session myId pId
            return $ do
              setMessageFlag pId (not f)
              return ()
    _ -> return ()

beginUpdateMessage :: MH ()
beginUpdateMessage = do
    selected <- use (to getSelectedMessage)
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let Just p = msg^.mOriginalPost
            csMode .= Main
            csEditState.cedEditMode .= Editing p
            csCmdLine %= applyEdit (clearZipper >> (insertMany $ postMessage p))
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
            csCmdLine %= applyEdit clearZipper

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
    session <- use csSession
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
    let cId = getId chan
    session <- use csSession
    myTeamId <- use (csMyTeam.teamIdL)
    doAsyncWith Preempt $ do
        void $ mmJoinChannel session myTeamId cId
        return (return ())

    csMode .= Main

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> MH ()
handleChannelInvite cId = do
    st <- use id
    doAsyncWith Normal $ do
        tryMM (mmGetChannel (st^.csSession) (st^.csMyTeam.teamIdL) cId)
              (\(ChannelWithData chan _) -> do
                return $ do
                  handleNewChannel (preferredChannelName chan) False chan
                  asyncFetchScrollback Normal cId)

startLeaveCurrentChannel :: MH ()
startLeaveCurrentChannel = do
    cInfo <- use (csCurrentChannel.ccInfo)
    case canLeaveChannel cInfo of
        True -> csMode .= LeaveChannelConfirm
        False -> postErrorMessage "The /leave command cannot be used with this channel."

leaveCurrentChannel :: MH ()
leaveCurrentChannel = do
    cId <- use csCurrentChannelId
    cInfo <- use (csCurrentChannel.ccInfo)
    session <- use csSession
    myTeamId <- use (csMyTeam.teamIdL)

    when (canLeaveChannel cInfo) $ doAsyncWith Preempt $ do
        mmLeaveChannel session myTeamId cId
        return (removeChannelFromState cId)

removeChannelFromState :: ChannelId -> MH ()
removeChannelFromState cId = do
    withChannel cId $ \ chan -> do
        let cName = chan^.ccInfo.cdName
            chType = chan^.ccInfo.cdType
        when (chType /= Direct) $ do
            csEditState.cedInputHistoryPosition .at cId .= Nothing
            csEditState.cedLastChannelInput     .at cId .= Nothing
            -- Update input history
            csEditState.cedInputHistory         %= removeChannelHistory cId
            -- Flush cnToChanId
            csNames.cnToChanId                  .at cName .= Nothing
            -- Flush cnChans
            csNames.cnChans                     %= filter (/= cName)
            -- Update msgMap
            csChannels                          %= filteredChannels ((/=) cId . fst)
            -- Remove from focus zipper
            csFocus                             %= Z.filterZipper (/= cId)

fetchCurrentChannelMembers :: MH ()
fetchCurrentChannelMembers = do
    cId <- use csCurrentChannelId
    session <- use csSession
    myTeamId <- use (csMyTeam.teamIdL)
    doAsyncWith Preempt $ do
        chanUserMap <- mmGetChannelMembers session myTeamId cId 0 10000

        -- Construct a message listing them all and post it to the
        -- channel:
        let msgStr = "Channel members (" <> (T.pack $ show $ length chanUsers) <> "):\n" <>
                     T.intercalate ", " usernames
            chanUsers = snd <$> HM.toList chanUserMap
            usernames = sort $ userUsername <$> (F.toList chanUsers)

        return $ postInfoMessage msgStr

-- *  Channel Updates and Notifications

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- findChannelById cId (st^.csChannels)
  u <- chan^.ccInfo.cdViewed
  let v = chan^.ccInfo.cdUpdated
  return (v > u)

setLastViewedFor :: ChannelId -> MH ()
setLastViewedFor cId = do
  now <- getNow
  chan <- use (csChannels.to (findChannelById cId))
  case chan of
    Just _  -> csChannels %= modifyChannelById cId (ccInfo.cdViewed ?~ now)
    Nothing -> handleChannelInvite cId

updateViewed :: MH ()
updateViewed = do
  csCurrentChannel.ccInfo.cdMentionCount .= 0
  st <- use id
  liftIO (updateViewedIO st)

resetHistoryPosition :: MH ()
resetHistoryPosition = do
    cId <- use csCurrentChannelId
    csInputHistoryPosition.at cId .= Just Nothing

updateStatus :: UserId -> T.Text -> MH ()
updateStatus uId t = csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)

clearEditor :: MH ()
clearEditor = csCmdLine %= applyEdit clearZipper

loadLastEdit :: MH ()
loadLastEdit = do
    cId <- use csCurrentChannelId
    lastInput <- use (csLastChannelInput.at cId)
    case lastInput of
        Nothing -> return ()
        Just (lastEdit, lastEditMode) -> do
            csCmdLine %= (applyEdit $ insertMany (lastEdit) . clearZipper)
            csEditState.cedEditMode .= lastEditMode

saveCurrentEdit :: MH ()
saveCurrentEdit = do
    cId <- use csCurrentChannelId
    cmdLine <- use csCmdLine
    mode <- use (csEditState.cedEditMode)
    csLastChannelInput.at cId .=
      Just (T.intercalate "\n" $ getEditContents $ cmdLine, mode)

resetCurrentEdit :: MH ()
resetCurrentEdit = do
    cId <- use csCurrentChannelId
    csLastChannelInput.at cId .= Nothing

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
    clearNewMessageCutoff cId

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
getNextUnreadChannel st = Z.findRight (hasUnread st)

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

channelScrollToTop :: MH ()
channelScrollToTop = do
  cId <- use csCurrentChannelId
  mh $ vScrollToBeginning (viewportScroll (ChannelMessages cId))

channelScrollToBottom :: MH ()
channelScrollToBottom = do
  cId <- use csCurrentChannelId
  mh $ vScrollToEnd (viewportScroll (ChannelMessages cId))

asyncFetchMoreMessages :: ChatState -> ChannelId -> IO ()
asyncFetchMoreMessages st cId =
    doAsyncWithIO Preempt st $ do
        let offset = length $ st^.csChannel(cId).ccContents.cdMessages
            numToFetch = 10
        posts <- mmGetPosts (st^.csSession) (st^.csMyTeam.teamIdL) cId (offset - 1) numToFetch
        return $ do
            cc <- fromPosts posts
            ccId <- use csCurrentChannelId
            mh $ invalidateCacheEntry (ChannelMessages ccId)
            mapM_ (\m ->
                   csChannel(ccId).ccContents.cdMessages %= (addMessage m))
                 (cc^.cdMessages)

loadMoreMessages :: MH ()
loadMoreMessages = do
    mode <- use csMode
    cId  <- use csCurrentChannelId
    st   <- use id
    case mode of
        ChannelScroll -> do
            liftIO $ asyncFetchMoreMessages st cId
        _ -> return ()

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
      Nothing -> attemptCreateDMChannel name

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
  if name `elem` users && not (name `HM.member` nameToChanId)
    then do
      -- We have a user of that name but no channel. Time to make one!
      tId <- use (csMyTeam.teamIdL)
      Just uId <- use (csNames.cnToUserId.at(name))
      session <- use csSession
      doAsyncWith Normal $ do
        -- create a new channel
        nc <- mmCreateDirect session tId uId
        return $ handleNewChannel name True nc
    else
      postErrorMessage ("No channel or user named " <> name)

createOrdinaryChannel :: T.Text -> MH ()
createOrdinaryChannel name  = do
  tId <- use (csMyTeam.teamIdL)
  session <- use csSession
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
    tryMM (mmCreateChannel session tId minChannel)
          (return . handleNewChannel name True)

handleNewChannel :: T.Text -> Bool -> Channel -> MH ()
handleNewChannel name switch nc = do
  -- time to do a lot of state updating:
  -- create a new ClientChannel structure
  let cChannel = makeClientChannel nc
  -- add it to the message map, and to the map so we can look it up by
  -- user name
  csNames.cnToChanId.at(name) .= Just (getId nc)
  let chType = nc^.channelTypeL
  -- For direct channels the username is already in the user list so
  -- do nothing
  when (chType /= Direct) $
      csNames.cnChans %= (sort . (name:))
  csChannels %= addChannel (getId nc) cChannel
  -- we should figure out how to do this better: this adds it to the
  -- channel zipper in such a way that we don't ever change our focus
  -- to something else, which is kind of silly
  names <- use csNames
  let newZip = Z.updateList (mkChannelZipperList names)
  csFocus %= newZip
    -- and we finally set our focus to the newly created channel
  when switch $ setFocus (getId nc)

editMessage :: Post -> MH ()
editMessage new = do
  now <- getNow
  st <- use id
  let isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage st (toClientPost new (new^.postParentIdL))
      chan = csChannel (new^.postChannelIdL)
  chan . ccContents . cdMessages . traversed . filtered isEditedMessage .= msg
  chan . ccInfo . cdUpdated .= now
  csPostMap.ix(postId new) .= msg
  cId <- use csCurrentChannelId
  when (postChannelId new == cId) $
    updateViewed

deleteMessage :: Post -> MH ()
deleteMessage new = do
  now <- getNow
  let isDeletedMessage m = m^.mPostId == Just (new^.postIdL)
      chan = csChannel (new^.postChannelIdL)
  chan.ccContents.cdMessages.traversed.filtered isDeletedMessage %= (& mDeleted .~ True)
  chan.ccInfo.cdUpdated .= now
  cId <- use csCurrentChannelId
  when (postChannelId new == cId) $
    updateViewed

maybeRingBell :: MH ()
maybeRingBell = do
    doBell <- use (csResources.crConfiguration.to configActivityBell)
    when doBell $ do
        -- This is safe because we only get Nothing in appStartEvent.
        Just vty <- mh getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

addMessageToState :: Post -> MH ()
addMessageToState new = do
  st <- use id
  asyncFetchAttachments new
  case st ^? csChannel(postChannelId new) of
      Nothing ->
          -- When we join channels, sometimes we get the "user has
          -- been added to channel" message here BEFORE we get the
          -- websocket event that says we got added to a channel. This
          -- means the message arriving here in addMessage can't be
          -- added yet because we haven't fetched the channel metadata
          -- in the websocket handler. So to be safe we just drop the
          -- message here, but this is the only case of messages that we
          -- /expect/ to drop for this reason. Hence the check for the
          -- msgMap channel ID key presence above.
          return ()
      Just _ -> do
          now <- getNow
          let cp = toClientPost new (new^.postParentIdL)
              fromMe = (cp^.cpUser == (Just $ getId (st^.csMe))) &&
                       (isNothing $ cp^.cpUserOverride)
              updateTime = if fromMe then id else const now
              cId = postChannelId new

              doAddMessage = do
                s <- use id
                let msg' = clientPostToMessage s
                           (toClientPost new (new^.postParentIdL))
                csPostMap.ix(postId new) .= msg'
                csChannels %= modifyChannelById cId
                  ((ccContents.cdMessages %~ addMessage msg') .
                   (ccInfo.cdUpdated %~ updateTime))
                when (not fromMe) $ maybeRingBell
                ccId <- use csCurrentChannelId
                if postChannelId new == ccId
                  then updateViewed
                  else setNewMessageCutoff cId msg'

              doHandleNewMessage = do
                  -- If the message is in reply to another message,
                  -- try to find it in the scrollback for the post's
                  -- channel. If the message isn't there, fetch it. If
                  -- we have to fetch it, don't post this message to the
                  -- channel until we have fetched the parent.
                  case getMessageForPostId st <$> cp^.cpInReplyToPost of
                      Just (ParentNotLoaded parentId) -> do
                          doAsyncWith Normal $ do
                              let theTeamId = st^.csMyTeam.teamIdL
                              p <- mmGetPost (st^.csSession) theTeamId cId parentId
                              let postMap = HM.fromList [ ( pId
                                                          , clientPostToMessage st (toClientPost x (x^.postParentIdL))
                                                          )
                                                        | (pId, x) <- HM.toList (p^.postsPostsL)
                                                        ]
                              return $ do
                                csPostMap %= HM.union postMap
                                doAddMessage
                      _ -> doAddMessage

          -- If this message was written by a user we don't know about,
          -- fetch the user's information before posting the message.
          case cp^.cpUser of
              Nothing -> doHandleNewMessage
              Just uId ->
                  case st^.csUsers.to (findUserById uId) of
                      Just _ -> doHandleNewMessage
                      Nothing -> do
                          handleNewUser uId
                          doAsyncWith Normal $ return doHandleNewMessage

setNewMessageCutoff :: ChannelId -> Message -> MH ()
setNewMessageCutoff cId msg =
    csChannel(cId).ccInfo.cdNewMessageCutoff %= (<|> Just (msg^.mDate))

clearNewMessageCutoff :: ChannelId -> MH ()
clearNewMessageCutoff cId =
    csChannel(cId).ccInfo.cdNewMessageCutoff .= Nothing

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe UTCTime
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdNewMessageCutoff

execMMCommand :: T.Text -> T.Text -> MH ()
execMMCommand name rest = do
  cId      <- use csCurrentChannelId
  session  <- use csSession
  myTeamId <- use (csMyTeam.teamIdL)
  let mc = MinCommand
             { minComChannelId = cId
             , minComCommand   = "/" <> name <> " " <> rest
             }
      runCmd = liftIO $ do
        void $ mmExecute session myTeamId mc
      handler (HTTPResponseException err) = return (Just err)
  errMsg <- liftIO $ (runCmd >> return Nothing) `catch` handler
  case errMsg of
    Nothing -> return ()
    Just err ->
      postErrorMessage ("Error running command: " <> (T.pack err))

fetchCurrentScrollback :: MH ()
fetchCurrentScrollback = do
  cId <- use csCurrentChannelId
  withChannel cId $ \ chan -> do
    when (chan^.ccInfo.cdCurrentState == ChanUnloaded) $ do
      asyncFetchScrollback Preempt cId
      csChannel(cId).ccInfo.cdCurrentState .= ChanLoadPending

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  [ (chanNames ^. cnToChanId) HM.! i
  | i <- chanNames ^. cnChans ] ++
  [ c
  | i <- chanNames ^. cnUsers
  , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]

setChannelTopic :: ChatState -> T.Text -> IO ()
setChannelTopic st msg = do
    let chanId = st^.csCurrentChannelId
        theTeamId = st^.csMyTeam.teamIdL
    doAsyncWithIO Normal st $ do
        void $ mmSetChannelHeader (st^.csSession) theTeamId chanId msg
        return $ csChannel(chanId).ccInfo.cdHeader .= msg

channelHistoryForward :: MH ()
channelHistoryForward = do
  cId <- use csCurrentChannelId
  inputHistoryPos <- use (csInputHistoryPosition.at cId)
  inputHistory <- use csInputHistory
  case inputHistoryPos of
      Just (Just i)
        | i == 0 -> do
          -- Transition out of history navigation
          csInputHistoryPosition.at cId .= Just Nothing
          loadLastEdit
        | otherwise -> do
          let Just entry = getHistoryEntry cId newI inputHistory
              newI = i - 1
              eLines = T.lines entry
              mv = if length eLines == 1 then gotoEOL else id
          csCmdLine.editContentsL .= (mv $ textZipper eLines Nothing)
          csInputHistoryPosition.at cId .= (Just $ Just newI)
      _ -> return ()

channelHistoryBackward :: MH ()
channelHistoryBackward = do
  cId <- use csCurrentChannelId
  inputHistoryPos <- use (csInputHistoryPosition.at cId)
  inputHistory <- use csInputHistory
  case inputHistoryPos of
      Just (Just i) ->
          let newI = i + 1
          in case getHistoryEntry cId newI inputHistory of
              Nothing -> return ()
              Just entry -> do
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  csCmdLine.editContentsL .= (mv $ textZipper eLines Nothing)
                  csInputHistoryPosition.at cId .= (Just $ Just newI)
      _ ->
          let newI = 0
          in case getHistoryEntry cId newI inputHistory of
              Nothing -> return ()
              Just entry ->
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  in do
                    saveCurrentEdit
                    csCmdLine.editContentsL .= (mv $ textZipper eLines Nothing)
                    csInputHistoryPosition.at cId .= (Just $ Just newI)

showHelpScreen :: HelpScreen -> MH ()
showHelpScreen screen = do
    mh $ vScrollToBeginning (viewportScroll HelpViewport)
    csMode .= ShowHelp screen

beginChannelSelect :: MH ()
beginChannelSelect = do
    csMode                        .= ChannelSelect
    csChannelSelectString         .= ""
    csChannelSelectChannelMatches .= mempty
    csChannelSelectUserMatches    .= mempty

updateChannelSelectMatches :: MH ()
updateChannelSelectMatches = do
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    chanNameMatches <- use (csChannelSelectString.to channelNameMatch)
    chanNames   <- use (csNames.cnChans)
    userNames   <- use (to userList)
    let chanMatches = catMaybes (fmap chanNameMatches chanNames)
    let userMatches = catMaybes (fmap chanNameMatches (fmap _uiName userNames))
    let mkMap ms = HM.fromList [(channelNameFromMatch m, m) | m <- ms]
    csChannelSelectChannelMatches .= mkMap chanMatches
    csChannelSelectUserMatches    .= mkMap userMatches

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

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = snd . go Set.empty
  where go before [] = (before, [])
        go before (x:xs) =
          let (before', xs') = go before xs in
          if (x^.linkURL) `Set.member` before'
            then (before', xs')
            else (Set.insert (x^.linkURL) before', x : xs')

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
    cmd <- use (csResources.crConfiguration.to configURLOpenCommand)
    case cmd of
        Nothing ->
            return False
        Just urlOpenCommand ->
            case _linkFileId link of
              Nothing -> do
                runLoggedCommand (T.unpack urlOpenCommand) [T.unpack $ link^.linkURL]
                return True
              Just fId -> do
                sess  <- use csSession
                doAsyncWith Normal $ do
                  info     <- mmGetFileInfo sess fId
                  contents <- mmGetFile sess fId
                  cacheDir <- getUserCacheDir xdgName
                  let dir   = cacheDir </> "files" </> T.unpack (idString fId)
                      fname = dir </> T.unpack (fileInfoName info)
                  createDirectoryIfMissing True dir
                  BS.writeFile fname contents
                  return $! runLoggedCommand (T.unpack urlOpenCommand) [fname]
                return True

runLoggedCommand :: String -> [String] -> MH ()
runLoggedCommand cmd args = do
  st <- use id
  liftIO $ do
    let opener = (proc cmd args) { std_in = NoStream
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> do
            let po = ProgramOutput cmd args "" (show e) (ExitFailure 1)
            STM.atomically $ STM.writeTChan (st^.csResources.crSubprocessLog) po
        Right (Nothing, Just outh, Just errh, ph) -> do
            ec <- waitForProcess ph
            outResult <- hGetContents outh
            errResult <- hGetContents errh
            let po = ProgramOutput cmd args outResult errResult ec
            STM.atomically $ STM.writeTChan (st^.csResources.crSubprocessLog) po
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
                            void $ mmPost (st^.csSession) theTeamId pendingPost
                        Replying _ p -> do
                            pendingPost <- mkPendingPost msg myId chanId
                            let modifiedPost =
                                    pendingPost { pendingPostParentId = Just $ postId p
                                                , pendingPostRootId = Just $ postId p
                                                }
                            void $ mmPost (st^.csSession) theTeamId modifiedPost
                        Editing p -> do
                            now <- getCurrentTime
                            let modifiedPost = p { postMessage = msg
                                                 , postPendingPostId = Nothing
                                                 , postUpdateAt = now
                                                 }
                            void $ mmUpdatePost (st^.csSession) theTeamId modifiedPost

handleNewUser :: UserId -> MH ()
handleNewUser newUserId = do
    -- Fetch the new user record.
    st <- use id
    doAsyncWith Normal $ do
        newUser <- mmGetUser (st^.csSession) newUserId
        -- Also re-load the team members so we can tell whether the new
        -- user is in the current user's team.
        teamUsers <- mmGetProfiles (st^.csSession) (st^.csMyTeam.teamIdL) 0 10000
        let uInfo = userInfoFromUser newUser (HM.member newUserId teamUsers)

        return $ do
            -- Update the name map and the list of known users
            csUsers %= addUser newUserId uInfo
            csNames . cnUsers %= (sort . ((newUser^.userUsernameL):))
