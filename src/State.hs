{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module State where

import           Brick (EventM, invalidateCacheEntry)
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list, listMoveTo, listSelectedElement)
import           Control.Applicative
import           Control.Exception (catch)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import           Data.Char (isAlphaNum)
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToBeginning, vScrollBy)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Monad (when, void)
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
import           System.Process (proc, std_in, std_out, std_err, StdStream(..),
                                 createProcess, waitForProcess)
import           System.IO (hGetContents)

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses

import           Config
import           Types
import           InputHistory
import           Themes
import           Zipper (Zipper)
import qualified Zipper as Z
import           Markdown (blockGetURLs, findVerbatimChunk)

import           State.Common

pageAmount :: Int
pageAmount = 15

-- * Refreshing Channel Data

-- | Get all the new messages for a given channel. In addition, load the
-- channel metadata and update that, too.
refreshChannel :: ChannelId -> ChatState -> IO ()
refreshChannel chan st = doAsyncWith Normal st $
  case F.find (\ p -> isJust (p^.mPostId)) (Seq.reverse (st^.csChannel(chan).ccContents.cdMessages)) of
    Just (Message { _mPostId = Just pId }) -> do
      -- Get the latest channel metadata.
      cwd <- mmGetChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) chan

      -- Load posts since the last post in this channel.
      posts <- mmGetPostsAfter (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) chan pId 0 100
      return $ \ st' -> do
        res <- F.foldrM addMessage st' [ (posts^.postsPostsL) HM.! p
                                       | p <- F.toList (posts^.postsOrderL)
                                       ]
        let newChanInfo = channelInfoFromChannelWithData cwd &
                            cdCurrentState .~ ChanLoaded &
                            cdNewMessageCutoff .~ (oldChanInfo^.cdNewMessageCutoff)
            oldChanInfo = res^.csChannel(chan).ccInfo

        return (res & csChannel(chan).ccInfo .~ newChanInfo)
    _ -> return return

-- | Find all the loaded channels and refresh their state, setting the
-- state as dirty until we get a response
refreshLoadedChannels :: ChatState -> EventM Name ChatState
refreshLoadedChannels st = do
  liftIO $ sequence_
    [ refreshChannel cId st
    | (cId, chan) <- HM.toList (st^.msgMap)
    , chan^.ccInfo.cdCurrentState == ChanLoaded
    ]
  let upd ChanLoaded = ChanRefreshing
      upd chanState  = chanState
  return (st & msgMap.each.ccInfo.cdCurrentState %~ upd)

-- * Message selection mode

-- | Starting from the current sequence index, look forward for a
-- Message that corresponds to a user Post (i.e. has a post ID).
getNextPost :: Seq.Seq Message -> Maybe PostId
getNextPost msgs =
    case Seq.viewl msgs of
        Seq.EmptyL -> Nothing
        msg Seq.:< rest ->
            (if msg^.mDeleted then Nothing else msg^.mPostId) <|> getNextPost rest

-- | Starting from the current sequence index, look backwards for a
-- Message that corresponds to a user Post (i.e. has a post ID).
getPrevPost :: Int -> Seq.Seq Message -> Maybe PostId
getPrevPost i msgs =
    case Seq.viewr (Seq.take (i+1) msgs) of
        Seq.EmptyR -> Nothing
        rest Seq.:> msg ->
            (if msg^.mDeleted then Nothing else msg^.mPostId) <|> getPrevPost (i - 1) rest

beginMessageSelect :: ChatState -> EventM Name ChatState
beginMessageSelect st = do
    -- Get the number of messages in the current channel and set the
    -- currently selected message index to be the most recently received
    -- message that corresponds to a Post (i.e. exclude informative
    -- messages).
    --
    -- If we can't find one at all, we ignore the mode switch request
    -- and just return.
    let chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
        recentPost = getPrevPost (Seq.length chanMsgs - 1) chanMsgs

    case recentPost of
        Nothing ->
            return st
        Just _ ->
            return $ st & csMode .~ MessageSelect
                        & csMessageSelect .~ MessageSelectState recentPost

getSelectedMessage :: ChatState -> Maybe Message
getSelectedMessage st
    | st^.csMode /= MessageSelect && st^.csMode /= MessageSelectDeleteConfirm = Nothing
    | otherwise = do
        selPostId <- selectMessagePostId $ st^.csMessageSelect

        let chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages

        idx <- Seq.findIndexR (\m -> m^.mPostId == Just selPostId) chanMsgs
        Seq.lookup idx chanMsgs

messageSelectUp :: ChatState -> EventM Name ChatState
messageSelectUp st
    | st^.csMode /= MessageSelect = return st
    | isNothing $ selectMessagePostId $ st^.csMessageSelect = return st
    | otherwise = do
        let oldPostId@(Just selPostId) = selectMessagePostId $ st^.csMessageSelect
            chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
            Just idx = Seq.findIndexR (\m -> m^.mPostId == Just selPostId) chanMsgs
            prevPostId = getPrevPost (idx - 1) chanMsgs

        return $ st & csMessageSelect .~ MessageSelectState (prevPostId <|> oldPostId)

messageSelectDown :: ChatState -> EventM Name ChatState
messageSelectDown st
    | st^.csMode /= MessageSelect = return st
    | isNothing $ selectMessagePostId $ st^.csMessageSelect = return st
    | otherwise = do
        let oldPostId@(Just selPostId) = selectMessagePostId $ st^.csMessageSelect
            chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
            Just idx = Seq.findIndexR (\m -> m^.mPostId == Just selPostId) chanMsgs
            nextPostId = getNextPost (Seq.drop (idx + 1) chanMsgs)

        return $ st & csMessageSelect .~ MessageSelectState (nextPostId <|> oldPostId)

isMine :: ChatState -> Message -> Bool
isMine st msg = (Just $ st^.csMe.userUsernameL) == msg^.mUserName

messageSelectDownBy :: Int -> ChatState -> EventM Name ChatState
messageSelectDownBy amt st
    | amt <= 0 = return st
    | otherwise = messageSelectDown st >>= messageSelectDownBy (amt - 1)

messageSelectUpBy :: Int -> ChatState -> EventM Name ChatState
messageSelectUpBy amt st
    | amt <= 0 = return st
    | otherwise = messageSelectUp st >>= messageSelectUpBy (amt - 1)

beginConfirmDeleteSelectedMessage :: ChatState -> EventM Name ChatState
beginConfirmDeleteSelectedMessage st =
    return $ st & csMode .~ MessageSelectDeleteConfirm

isDeletable :: Message -> Bool
isDeletable m = m^.mType `elem` [CP NormalPost, CP Emote]

isReplyable :: Message -> Bool
isReplyable m = m^.mType `elem` [CP NormalPost, CP Emote]

isEditable :: Message -> Bool
isEditable m = m^.mType `elem` [CP NormalPost, CP Emote]

deleteSelectedMessage :: ChatState -> EventM Name ChatState
deleteSelectedMessage st = do
    case getSelectedMessage st of
        Just msg | isMine st msg && isDeletable msg -> do
            liftIO $ doAsyncWith Preempt st $ do
                let cId = st^.csCurrentChannelId
                    Just p = msg^.mOriginalPost
                mmDeletePost (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId (postId p)
                return $ \st' ->
                    return $ st' & csEditState.cedEditMode .~ NewPost
                                 & csMode .~ Main
        _ -> return ()
    return st

beginCurrentChannelDeleteConfirm :: ChatState -> EventM Name ChatState
beginCurrentChannelDeleteConfirm st =
    let isNormal = st^.csChannel(cId).ccInfo.cdType /= Direct
        cId = st^.csCurrentChannelId
    in if isNormal
       then return $ st & csMode .~ DeleteChannelConfirm
       else postErrorMessage "The /delete-channel command cannot be used with direct message channels." st

deleteCurrentChannel :: ChatState -> EventM Name ChatState
deleteCurrentChannel st = do
    liftIO $ doAsyncWith Preempt st $ do
        let cId = st^.csCurrentChannelId
        mmDeleteChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
        return $ \st' ->
            leaveCurrentChannel $ st' & csMode .~ Main

    return st

beginUpdateMessage :: ChatState -> EventM Name ChatState
beginUpdateMessage st =
    case getSelectedMessage st of
        Just msg | isMine st msg && isEditable msg -> do
            let Just p = msg^.mOriginalPost
            return $ st & csMode .~ Main
                        & csEditState.cedEditMode .~ Editing p
                        & cmdLine %~ applyEdit (clearZipper >> (insertMany $ postMessage p))
        _ -> return st

replyToLatestMessage :: ChatState -> EventM Name ChatState
replyToLatestMessage st =
    case getLatestUserMessage st of
        Just msg | isReplyable msg -> do
            let Just p = msg^.mOriginalPost
            return $ st & csMode .~ Main
                        & csEditState.cedEditMode .~ Replying msg p
        _ -> return st

-- | Get the latest normal or emote post in the current channel.
getLatestUserMessage :: ChatState -> Maybe Message
getLatestUserMessage st =
    let go msgs = case Seq.viewr msgs of
            Seq.EmptyR -> Nothing
            rest Seq.:> msg ->
                (if msg^.mDeleted || not (msg^.mType `elem` [CP NormalPost, CP Emote])
                 then Nothing
                 else (Just msg <* msg^.mOriginalPost)) <|>
                go rest

        chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
    in go chanMsgs

beginReplyCompose :: ChatState -> EventM Name ChatState
beginReplyCompose st =
    case getSelectedMessage st of
        Nothing -> return st
        Just msg -> do
            let Just p = msg^.mOriginalPost
            return $ st & csMode .~ Main
                        & csEditState.cedEditMode .~ Replying msg p

cancelReplyOrEdit :: ChatState -> ChatState
cancelReplyOrEdit st =
    case st^.csEditState.cedEditMode of
        NewPost -> st
        _ -> st & csEditState.cedEditMode .~ NewPost
                & cmdLine %~ applyEdit clearZipper

copyVerbatimToClipboard :: ChatState -> EventM Name ChatState
copyVerbatimToClipboard st =
    case getSelectedMessage st of
        Nothing -> return st
        Just m -> case findVerbatimChunk (m^.mText) of
            Nothing -> return st
            Just txt -> do
              st' <- copyToClipboard txt st
              return (st' & csMode .~ Main)

-- * Joining, Leaving, and Inviting

startJoinChannel :: ChatState -> EventM Name ChatState
startJoinChannel st = do
    liftIO $ doAsyncWith Preempt st $ do
        chans <- mmGetMoreChannels (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL)
        return $ \ st' -> do
            return $ st' & csJoinChannelList .~ (Just $ list JoinChannelList (V.fromList $ F.toList chans) 1)

    return $ st & csMode .~ JoinChannel
                & csJoinChannelList .~ Nothing

joinChannel :: Channel -> ChatState -> EventM Name ChatState
joinChannel chan st = do
    let cId = getId chan

    liftIO $ doAsyncWith Preempt st $ do
        void $ mmJoinChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
        return return

    return $ st & csMode .~ Main

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> ChatState -> EventM Name ChatState
handleChannelInvite cId st = do
    liftIO $ doAsyncWith Normal st $ do
        tryMM (mmGetChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId)
              (\(ChannelWithData chan _) -> do
                return $ \st' -> do
                  st'' <- handleNewChannel (chan^.channelNameL) False chan st'
                  liftIO $ asyncFetchScrollback Normal st'' cId
                  return st'')
    return st

startLeaveCurrentChannel :: ChatState -> EventM Name ChatState
startLeaveCurrentChannel st = do
    let cName = st^.csCurrentChannel.ccInfo.cdName
    case cName `elem` st^.csNames.cnDMs of
        True -> postErrorMessage "The /leave command cannot be used with direct message channels." st
        False -> return $ st & csMode .~ LeaveChannelConfirm

leaveCurrentChannel :: ChatState -> EventM Name ChatState
leaveCurrentChannel st = do
    let cId = st^.csCurrentChannelId
        isNormal = st^.csChannel(cId).ccInfo.cdType /= Direct

    -- Leave a normal channel.  If this is a DM channel, do nothing.
    when isNormal $ liftIO $ doAsyncWith Preempt st $ do
        mmLeaveChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
        return (removeChannelFromState cId)

    return st

removeChannelFromState :: ChannelId -> ChatState -> EventM Name ChatState
removeChannelFromState cId st = do
    let cName = st^.csChannel(cId).ccInfo.cdName
        isNormal = st^.csChannel(cId).ccInfo.cdType /= Direct

    case isNormal of
        False -> return st
        True ->
            return $ st & csEditState.cedInputHistoryPosition .at cId .~ Nothing
                        & csEditState.cedLastChannelInput     .at cId .~ Nothing
                        & csEditState.cedInputHistory         %~ removeChannelHistory cId
                          -- Update input history
                        & csNames.cnToChanId                  .at cName .~ Nothing
                          -- Flush cnToChanId
                        & csNames.cnChans                     %~ filter (/= cName)
                          -- Flush cnChans
                        & msgMap                              .at cId .~ Nothing
                          -- Update msgMap
                        & csFocus                             %~ Z.filterZipper (/= cId)
                          -- Remove from focus zipper

fetchCurrentChannelMembers :: ChatState -> EventM Name ()
fetchCurrentChannelMembers st = do
    liftIO $ doAsyncWith Preempt st $ do
        let cId = st^.csCurrentChannelId
        chanUserMap <- liftIO $ mmGetChannelMembers (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId

        -- Construct a message listing them all and post it to the
        -- channel:
        let msgStr = "Channel members (" <> (T.pack $ show $ length chanUsers) <> "):\n" <>
                     T.intercalate ", " usernames
            chanUsers = snd <$> HM.toList chanUserMap
            usernames = sort $ userUsername <$> (F.toList chanUsers)

        return $ \st' -> do
            msg <- newClientMessage Informative msgStr
            return $ addClientMessage msg st'

-- *  Channel Updates and Notifications

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.ccInfo.cdViewed
      v = chan^.ccInfo.cdUpdated
  return (v > u)

setLastViewedFor :: ChatState -> ChannelId -> EventM Name ChatState
setLastViewedFor st cId = do
  now <- liftIO getCurrentTime
  if cId `HM.member` (st^.msgMap)
    then return (st & csChannel(cId).ccInfo.cdViewed .~ now)
    else handleChannelInvite cId st

updateViewed :: ChatState -> EventM Name ChatState
updateViewed st = liftIO (updateViewedIO st)

resetHistoryPosition :: ChatState -> EventM a ChatState
resetHistoryPosition st =
    let cId = st^.csCurrentChannelId
    in return $ st & csInputHistoryPosition.at cId .~ Just Nothing

updateStatus :: UserId -> T.Text -> ChatState -> EventM a ChatState
updateStatus uId t st =
  return (st & usrMap.ix(uId).uiStatus .~ statusFromText t)

clearEditor :: ChatState -> ChatState
clearEditor = cmdLine %~ applyEdit clearZipper

loadLastEdit :: ChatState -> ChatState
loadLastEdit st =
    let cId = st^.csCurrentChannelId
    in case st^.csLastChannelInput.at cId of
        Nothing -> st
        Just (lastEdit, lastEditMode) ->
            st & cmdLine %~ (applyEdit $ insertMany (lastEdit) . clearZipper)
               & csEditState.cedEditMode .~ lastEditMode

saveCurrentEdit :: ChatState -> ChatState
saveCurrentEdit st =
    let cId = st^.csCurrentChannelId
    in st & csLastChannelInput.at cId .~
      Just (T.intercalate "\n" $ getEditContents $ st^.cmdLine, st^.csEditState.cedEditMode)

resetCurrentEdit :: ChatState -> ChatState
resetCurrentEdit st =
    let cId = st^.csCurrentChannelId
    in st & csLastChannelInput.at cId .~ Nothing

updateChannelListScroll :: ChatState -> EventM Name ChatState
updateChannelListScroll st = do
    vScrollToBeginning (viewportScroll ChannelList)
    return st

postChangeChannelCommon :: ChatState -> EventM Name ChatState
postChangeChannelCommon st =
    resetCurrentEdit <$>
    loadLastEdit <$>
    (updateChannelListScroll =<<
     resetEditorState =<<
     fetchCurrentScrollback =<<
     resetHistoryPosition st)

resetEditorState :: ChatState -> EventM Name ChatState
resetEditorState st =
    return $ clearEditor $ st & csEditState.cedEditMode .~ NewPost

preChangeChannelCommon :: ChatState -> EventM Name ChatState
preChangeChannelCommon st = do
    let cId = st^.csCurrentChannelId
    clearNewMessageCutoff cId $
        saveCurrentEdit $
        st & csRecentChannel .~ Just cId

nextChannel :: ChatState -> EventM Name ChatState
nextChannel st =
    setFocusWith st (getNextNonDMChannel st Z.right)

prevChannel :: ChatState -> EventM Name ChatState
prevChannel st =
    setFocusWith st (getNextNonDMChannel st Z.left)

recentChannel :: ChatState -> EventM Name ChatState
recentChannel st = case st ^. csRecentChannel of
  Nothing  -> return st
  Just cId -> setFocus cId st

nextUnreadChannel :: ChatState -> EventM Name ChatState
nextUnreadChannel st =
    setFocusWith st (getNextUnreadChannel st)

getNextNonDMChannel :: ChatState
                    -> (Zipper ChannelId -> Zipper ChannelId)
                    -> (Zipper ChannelId -> Zipper ChannelId)
getNextNonDMChannel st shift z =
    if (st^?msgMap.ix(Z.focus z).ccInfo.cdType) == Just Direct
    then z
    else go (shift z)
  where go z'
          | (st^?msgMap.ix(Z.focus z').ccInfo.cdType) /= Just Direct = z'
          | otherwise = go (shift z')

getNextUnreadChannel :: ChatState
                     -> (Zipper ChannelId -> Zipper ChannelId)
getNextUnreadChannel st = Z.findRight (hasUnread st)

listThemes :: ChatState -> EventM Name ChatState
listThemes cs = do
    let mkThemeList _ = T.intercalate "\n\n" $
                        "Available built-in themes:" :
                        (("  " <>) <$> fst <$> themes)
    msg <- newClientMessage Informative (mkThemeList themes)
    return $ addClientMessage msg cs

setTheme :: ChatState -> T.Text -> EventM Name ChatState
setTheme cs name =
    case lookup name themes of
        Nothing -> listThemes cs
        Just t -> return $ cs & csTheme .~ t

channelPageUp :: ChatState -> EventM Name ChatState
channelPageUp st = do
  let cId = st^.csCurrentChannelId
  vScrollBy (viewportScroll (ChannelMessages cId)) (-1 * pageAmount)
  return st

channelPageDown :: ChatState -> EventM Name ChatState
channelPageDown st = do
  let cId = st^.csCurrentChannelId
  vScrollBy (viewportScroll (ChannelMessages cId)) pageAmount
  return st

asyncFetchMoreMessages :: ChatState -> ChannelId -> IO ()
asyncFetchMoreMessages st cId =
    doAsyncWith Preempt st $ do
        let offset = length $ st^.csChannel(cId).ccContents.cdMessages
            numToFetch = 10
        posts <- mmGetPosts (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId (offset - 1) numToFetch
        return $ \st' -> do
            (cc, st'') <- liftIO $ fromPosts st' posts
            invalidateCacheEntry (ChannelMessages $ st^.csCurrentChannelId)
            return $ st'' & csChannel(cId).ccContents.cdMessages %~ (cc^.cdMessages Seq.><)

loadMoreMessages :: ChatState -> EventM Name ChatState
loadMoreMessages st = do
    case st^.csMode of
        ChannelScroll -> do
            liftIO $ asyncFetchMoreMessages st (st^.csCurrentChannelId)
        _ -> return ()
    return st

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | "#" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | "@" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | otherwise            = st ^. csNames . cnToChanId . at n

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannel :: T.Text -> ChatState -> EventM Name ChatState
changeChannel name st =
    case channelByName st name of
      Just cId -> setFocus cId st
      Nothing -> attemptCreateDMChannel name st

setFocus :: ChannelId -> ChatState -> EventM Name ChatState
setFocus cId st = setFocusWith st (Z.findRight (== cId))

setFocusWith :: ChatState -> (Zipper ChannelId -> Zipper ChannelId) -> EventM Name ChatState
setFocusWith st f = do
    let newZipper = f oldZipper
        oldZipper = st^.csFocus
        newFocus = Z.focus newZipper
        oldFocus = Z.focus oldZipper

    -- If we aren't changing anything, skip all the book-keeping because
    -- we'll end up clobbering things like csRecentChannel.
    if (newFocus == oldFocus) then
        return st else do
          preChangeChannelCommon st >>=
              (\st' -> updateViewed (st' & csFocus .~ newZipper)) >>=
              postChangeChannelCommon

attemptCreateDMChannel :: T.Text -> ChatState -> EventM Name ChatState
attemptCreateDMChannel name st
  | name `elem` (st^.csNames.cnUsers) &&
    not (name `HM.member` (st^.csNames.cnToChanId)) = do
      -- We have a user of that name but no channel. Time to make one!
      let tId = st^.csMyTeam.teamIdL
          Just uId = st^.csNames.cnToUserId.at(name)
      liftIO $ doAsyncWith Normal st $ do
        -- create a new channel
        nc <- mmCreateDirect (st^.csConn) (st^.csTok) tId uId
        return $ handleNewChannel name True nc
      return st
  | otherwise = do
    postErrorMessage ("No channel or user named " <> name) st

createOrdinaryChannel :: T.Text -> ChatState -> EventM Name ChatState
createOrdinaryChannel name st = do
  let tId = st^.csMyTeam.teamIdL
  liftIO $ doAsyncWith Preempt st $ do
    -- create a new chat channel
    let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
        minChannel = MinChannel
          { minChannelName        = slug
          , minChannelDisplayName = name
          , minChannelPurpose     = Nothing
          , minChannelHeader      = Nothing
          , minChannelType        = Ordinary
          }
    tryMM (mmCreateChannel (st^.csConn) (st^.csTok) tId minChannel)
          (return . handleNewChannel name True)
  return st

handleNewChannel :: T.Text -> Bool -> Channel -> ChatState -> EventM Name ChatState
handleNewChannel name switch nc st = do
  -- time to do a lot of state updating:
  -- create a new ClientChannel structure
  now <- liftIO getCurrentTime
  let cChannel = ClientChannel
        { _ccContents = emptyChannelContents
        , _ccInfo     = ChannelInfo
                          { _cdViewed           = now
                          , _cdUpdated          = now
                          , _cdName             = nc^.channelNameL
                          , _cdHeader           = nc^.channelHeaderL
                          , _cdType             = nc^.channelTypeL
                          , _cdCurrentState     = ChanLoaded
                          , _cdNewMessageCutoff = Nothing
                          }
        }
      -- add it to the message map, and to the map so we can look
      -- it up by user name
      st' = st & csNames.cnToChanId.at(name) .~ Just (getId nc)
               & (if nc^.channelTypeL == Direct
                  then id -- For direct channels the username is already
                          -- in the user list so do nothing
                  else csNames.cnChans %~ (sort . (name:)))
               & msgMap.at(getId nc) .~ Just cChannel
      -- we should figure out how to do this better: this adds it to
      -- the channel zipper in such a way that we don't ever change
      -- our focus to something else, which is kind of silly
      newZip = Z.updateList (mkChannelZipperList (st'^.csNames))
      st'' = st' & csFocus %~ newZip
          -- and we finally set our focus to the newly created channel
  if switch then setFocus (getId nc) st'' else return st''

editMessage :: Post -> ChatState -> EventM Name ChatState
editMessage new st = do
  now <- liftIO getCurrentTime
  let chan = csChannel (postChannelId new)
      isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage st (toClientPost new (new^.postParentIdL))
      rs = st & chan . ccContents . cdMessages . each . filtered isEditedMessage .~ msg
              & chan . ccInfo . cdUpdated .~ now
              & csPostMap.ix(postId new) .~ msg
  if postChannelId new == rs^.csCurrentChannelId
    then updateViewed rs
    else return rs

deleteMessage :: Post -> ChatState -> EventM Name ChatState
deleteMessage new st = do
  now <- liftIO getCurrentTime
  let isDeletedMessage m = m^.mPostId == Just (new^.postIdL)
      chan = csChannel (postChannelId new)
      rs = st & chan . ccContents . cdMessages . each . filtered isDeletedMessage %~ (& mDeleted .~ True)
              & chan . ccInfo . cdUpdated .~ now
  if postChannelId new == rs^.csCurrentChannelId
    then updateViewed rs
    else return rs

maybeRingBell :: ChatState -> EventM Name ()
maybeRingBell st = do
    when (configActivityBell $ st^.csResources.crConfiguration) $ do
        -- This is safe because we only get Nothing in appStartEvent.
        Just vty <- getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

addMessage :: Post -> ChatState -> EventM Name ChatState
addMessage new st = do
  liftIO $ asyncFetchAttachments new st
  case st^.msgMap.at (postChannelId new) of
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
          return st
      Just _ -> do
          now <- liftIO getCurrentTime
          let cp = toClientPost new (new^.postParentIdL)
              fromMe = (cp^.cpUser == (Just $ getId (st^.csMe))) &&
                       (isNothing $ cp^.cpUserOverride)
              updateTime = if fromMe then id else const now
              msg = clientPostToMessage st cp
              cId = postChannelId new

              doAddMessage s = do
                let chan = msgMap . ix cId
                    s' = s & csPostMap.ix(postId new) .~ msg
                    msg' = clientPostToMessage s (toClientPost new (new^.postParentIdL))
                    rs = s' & chan . ccContents . cdMessages %~ (Seq.|> msg')
                            & chan . ccInfo . cdUpdated %~ updateTime
                when (not fromMe) $ maybeRingBell s'
                if postChannelId new == rs^.csCurrentChannelId
                  then updateViewed rs
                  else setNewMessageCutoff rs cId msg

          -- If the message is in reply to another message, try to find it in
          -- the scrollback for the post's channel. If the message isn't there,
          -- fetch it. If we have to fetch it, don't post this message to the
          -- channel until we have fetched the parent.
          case msg^.mInReplyToMsg of
              ParentNotLoaded parentId -> do
                  liftIO $ doAsyncWith Normal st $ do
                      let theTeamId = st^.csMyTeam.teamIdL
                      p <- mmGetPost (st^.csConn) (st^.csTok) theTeamId cId parentId
                      let postMap = HM.fromList [ ( pId
                                                  , clientPostToMessage st (toClientPost x (x^.postParentIdL))
                                                  )
                                                | (pId, x) <- HM.toList (p^.postsPostsL)
                                                ]
                      return $ \st'' -> doAddMessage $ st'' & csPostMap %~ (HM.union postMap)
                  return st
              _ -> doAddMessage st

setNewMessageCutoff :: ChatState -> ChannelId -> Message -> EventM Name ChatState
setNewMessageCutoff st cId msg =
    return $ st & csChannel(cId).ccInfo.cdNewMessageCutoff %~ (<|> Just (msg^.mDate))

clearNewMessageCutoff :: ChannelId -> ChatState -> EventM Name ChatState
clearNewMessageCutoff cId st = do
    return $ st & csChannel(cId).ccInfo.cdNewMessageCutoff .~ Nothing

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe UTCTime
getNewMessageCutoff cId st = do
    cc <- st^.msgMap.at cId
    cc^.ccInfo.cdNewMessageCutoff

execMMCommand :: T.Text -> T.Text -> ChatState -> EventM Name ChatState
execMMCommand name rest st =
  liftIO (runCmd `catch` handler)
  where
  mc = MinCommand
        { minComChannelId = st^.csCurrentChannelId
        , minComCommand   = "/" <> name <> " " <> rest
        }
  runCmd = do
    void $ mmExecute
      (st^.csConn)
      (st^.csTok)
      (st^.csMyTeam.teamIdL)
      mc
    return st
  handler (HTTPResponseException err) = do
    postErrorMessage ("Error running command: " <> (T.pack err)) st

fetchCurrentScrollback :: ChatState -> EventM a ChatState
fetchCurrentScrollback st = do
  let cId = st^.csCurrentChannelId
  didQueue <- case maybe False (== ChanUnloaded) (st^?msgMap.ix(cId).ccInfo.cdCurrentState) of
      True -> do
          liftIO $ asyncFetchScrollback Preempt st cId
          return True
      False -> return False
  return $ st & csChannel(cId).ccInfo.cdCurrentState %~
                (if didQueue then const ChanLoadPending else id)

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
    doAsyncWith Normal st $ do
        void $ mmSetChannelHeader (st^.csConn) (st^.csTok) theTeamId chanId msg
        return $ \st' -> do
            return $ st' & msgMap.at chanId.each.ccInfo.cdHeader .~ msg

channelHistoryForward :: ChatState -> ChatState
channelHistoryForward st =
  let cId = st^.csCurrentChannelId
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i)
        | i == 0 ->
          -- Transition out of history navigation
          loadLastEdit $ st & csInputHistoryPosition.at cId .~ Just Nothing
        | otherwise ->
          let Just entry = getHistoryEntry cId newI (st^.csInputHistory)
              newI = i - 1
              eLines = T.lines entry
              mv = if length eLines == 1 then gotoEOL else id
          in st & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ -> st

channelHistoryBackward :: ChatState -> ChatState
channelHistoryBackward st =
  let cId = st^.csCurrentChannelId
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i) ->
          let newI = i + 1
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  in st & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                        & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ ->
          let newI = 0
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  in (saveCurrentEdit st)
                         & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                         & csInputHistoryPosition.at cId .~ (Just $ Just newI)

showHelpScreen :: HelpScreen -> ChatState -> EventM Name ChatState
showHelpScreen screen st = do
    vScrollToBeginning (viewportScroll HelpViewport)
    return $ st & csMode .~ ShowHelp screen

beginChannelSelect :: ChatState -> ChatState
beginChannelSelect st =
    st & csMode                        .~ ChannelSelect
       & csChannelSelectString         .~ ""
       & csChannelSelectChannelMatches .~ mempty
       & csChannelSelectUserMatches    .~ mempty

updateChannelSelectMatches :: ChatState -> ChatState
updateChannelSelectMatches st =
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    let chanNameMatches = channelNameMatch (st^.csChannelSelectString)
        chanMatches = catMaybes $ chanNameMatches <$> st^.csNames.cnChans
        userMatches = catMaybes $ chanNameMatches <$> (^.uiName) <$> sortedUserList st
        mkMap ms = HM.fromList [(channelNameFromMatch m, m) | m <- ms]
    in st & csChannelSelectChannelMatches .~ mkMap chanMatches
          & csChannelSelectUserMatches    .~ mkMap userMatches

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

startUrlSelect :: ChatState -> ChatState
startUrlSelect st =
    let urls = V.fromList $ findUrls (st^.csCurrentChannel)
    in st & csMode .~ UrlSelect
          & csUrlList .~ (listMoveTo (length urls - 1) $ list UrlList urls 2)

stopUrlSelect :: ChatState -> ChatState
stopUrlSelect = csMode .~ Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ F.toList $ F.toList <$> Seq.reverse <$> msgURLs <$> msgs

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
  let msgUrls = (\ (url, text) -> LinkChoice (msg^.mDate) uname text url) <$>
                  (mconcat $ blockGetURLs <$> (F.toList $ msg^.mText))
      attachmentURLs = (\ a ->
                          LinkChoice
                            (msg^.mDate)
                            uname
                            ("attachment `" <> (a^.attachmentName) <> "`")
                            (a^.attachmentURL))
                       <$> (msg^.mAttachments)
  in msgUrls <> attachmentURLs
msgURLs _ = mempty

openSelectedURL :: ChatState -> EventM Name ChatState
openSelectedURL st | st^.csMode == UrlSelect =
    case listSelectedElement $ st^.csUrlList of
        Nothing -> return st
        Just (_, link) -> do
            opened <- openURL st link
            case opened of
                True -> return st
                False -> do
                    msg <- newClientMessage Informative
                      "Config option 'urlOpenCommand' missing; cannot open URL."
                    return $ addClientMessage msg $ st & csMode .~ Main
openSelectedURL st = return st

openURL :: ChatState -> LinkChoice -> EventM Name Bool
openURL st link = do
    case configURLOpenCommand $ st^.csResources.crConfiguration of
        Nothing ->
            return False
        Just urlOpenCommand -> do
            let opener = (proc (T.unpack urlOpenCommand) [T.unpack $ link^.linkURL])
                         { std_in = NoStream
                         , std_out = CreatePipe
                         , std_err = CreatePipe
                         }
            liftIO $ do
                (Nothing, Just outh, Just errh, ph) <- createProcess opener
                ec <- waitForProcess ph
                outResult <- hGetContents outh
                errResult <- hGetContents errh
                let po = ProgramOutput (T.unpack urlOpenCommand) outResult errResult ec
                STM.atomically $ STM.writeTChan (st^.csResources.crSubprocessLog) po

            return True

openSelectedMessageURLs :: ChatState -> EventM Name ChatState
openSelectedMessageURLs st
    | st^.csMode /= MessageSelect = return st
    | otherwise = do
        let Just curMsg = getSelectedMessage st
            urls = msgURLs curMsg

        case null urls of
            True -> return st
            False -> do
                openedAll <- and <$> mapM (openURL st) urls

                let finalSt = st & csMode .~ Main
                case openedAll of
                    True -> return finalSt
                    False -> do
                        msg <- newClientMessage Informative
                          "Config option 'urlOpenCommand' missing; cannot open URL."
                        return $ addClientMessage msg finalSt

shouldSkipMessage :: T.Text -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = T.all (`elem` (" \t"::String)) s

sendMessage :: ChatState -> EditMode -> T.Text -> IO ChatState
sendMessage st mode msg =
    case shouldSkipMessage msg of
        True -> return st
        False -> do
            case st^.csConnectionStatus of
                Disconnected -> do
                    let s = "Cannot send messages while disconnected."
                    emsg <- newClientMessage Error s
                    return $ addClientMessage emsg st
                Connected -> do
                    let myId   = st^.csMe.userIdL
                        chanId = st^.csCurrentChannelId
                        theTeamId = st^.csMyTeam.teamIdL
                    doAsync Preempt st $ do
                      case mode of
                        NewPost -> do
                            pendingPost <- mkPendingPost msg myId chanId
                            void $ mmPost (st^.csConn) (st^.csTok) theTeamId pendingPost
                        Replying _ p -> do
                            pendingPost <- mkPendingPost msg myId chanId
                            let modifiedPost =
                                    pendingPost { pendingPostParentId = Just $ postId p
                                                , pendingPostRootId = Just $ postId p
                                                }
                            void $ mmPost (st^.csConn) (st^.csTok) theTeamId modifiedPost
                        Editing p -> do
                            now <- getCurrentTime
                            let modifiedPost = p { postMessage = msg
                                                 , postPendingPostId = Nothing
                                                 , postUpdateAt = now
                                                 }
                            void $ mmUpdatePost (st^.csConn) (st^.csTok) theTeamId modifiedPost
                    return st

handleNewUser :: UserId -> ChatState -> EventM Name ChatState
handleNewUser newUserId st = do
    -- Fetch the new user record.
    liftIO $ doAsyncWith Normal st $ do
        newUser <- mmGetUser (st^.csConn) (st^.csTok) newUserId
        -- Also re-load the team members so we can tell whether the new
        -- user is in the current user's team.
        teamUsers <- mmGetProfiles (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL)
        let uInfo = userInfoFromUser newUser (HM.member newUserId teamUsers)

        return $ \st' ->
            -- Update the name map and the list of known users
            return $ st' & usrMap . ix newUserId .~ uInfo
                         & csNames . cnUsers %~ (sort . ((newUser^.userUsernameL):))

    return st
