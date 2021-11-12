{-# LANGUAGE MultiWayIf #-}

module Matterhorn.State.Messages
  ( PostToAdd(..)
  , lastMsg
  , sendMessage
  , editMessage
  , deleteMessage
  , addNewPostedMessage
  , addObtainedMessages
  , asyncFetchMoreMessages
  , asyncFetchMessagesForGap
  , asyncFetchMessagesSurrounding
  , fetchVisibleIfNeeded
  , disconnectChannels
  , toggleMessageTimestamps
  , toggleVerbatimBlockTruncation
  , jumpToPost
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( getVtyHandle, invalidateCacheEntry, invalidateCache )
import qualified Brick.Widgets.FileBrowser as FB
import           Control.Exception ( SomeException, try )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Graphics.Vty ( outputIface )
import           Graphics.Vty.Output.Interface ( ringTerminalBell )
import           Lens.Micro.Platform ( Traversal', (.=), (%=), (%~), (.~), (^?)
                                     , to, at, traversed, filtered, ix, _1 )

import           Network.Mattermost
import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.Constants
import           Matterhorn.State.Channels
import           Matterhorn.State.Common
import           Matterhorn.State.Reactions
import           Matterhorn.State.Users
import           Matterhorn.TimeUtils
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import           Matterhorn.Types.DirectionalSeq ( DirectionalSeq, SeqDirection )


-- ----------------------------------------------------------------------
-- Message gaps


-- | Called to add an UnknownGap to the end of the Messages collection
-- for all channels when the client has become disconnected from the
-- server.  This gaps will later be removed by successful fetching
-- overlaps if the connection is re-established.  Note that the
-- disconnect is re-iterated periodically via a re-connect timer
-- attempt, so do not duplicate gaps.  Also clear any flags
-- representing a pending exchange with the server (which will now
-- never complete).
addDisconnectGaps :: MH ()
addDisconnectGaps = mapM_ onEach . filteredChannelIds (const True) =<< use csChannels
    where onEach c = do addEndGap c
                        clearPendingFlags c
                        mh $ invalidateCacheEntry (ChannelMessages c)

-- | Websocket was disconnected, so all channels may now miss some
-- messages
disconnectChannels :: MH ()
disconnectChannels = addDisconnectGaps

toggleMessageTimestamps :: MH ()
toggleMessageTimestamps = do
    mh invalidateCache
    let toggle c = c { configShowMessageTimestamps = not (configShowMessageTimestamps c)
                     }
    csResources.crConfiguration %= toggle

defaultVerbatimTruncateHeight :: Int
defaultVerbatimTruncateHeight = 25

toggleVerbatimBlockTruncation :: MH ()
toggleVerbatimBlockTruncation = do
    mh invalidateCache
    st <- use id
    -- Restore the configured setting, or a default if the configuration
    -- does not specify a setting.
    let toggle Nothing = (st^.csResources.crConfiguration.configTruncateVerbatimBlocksL) <|>
                         Just defaultVerbatimTruncateHeight
        toggle (Just _) = Nothing
    csVerbatimTruncateSetting %= toggle

clearPendingFlags :: ChannelId -> MH ()
clearPendingFlags c = csChannel(c).ccContents.cdFetchPending .= False

addEndGap :: ChannelId -> MH ()
addEndGap cId = withChannel cId $ \chan ->
    let lastmsg_ = chan^.ccContents.cdMessages.to reverseMessages.to lastMsg
        lastIsGap = maybe False isGap lastmsg_
        gapMsg = newGapMessage timeJustAfterLast
        timeJustAfterLast = maybe t0 (justAfter . _mDate) lastmsg_
        t0 = ServerTime $ originTime  -- use any time for a channel with no messages yet
        newGapMessage = newMessageOfType
                        (T.pack "Disconnected. Will refresh when connected.")
                        (C UnknownGapAfter)
    in unless lastIsGap
           (csChannels %= modifyChannelById cId (ccContents.cdMessages %~ addMessage gapMsg))

lastMsg :: RetrogradeMessages -> Maybe Message
lastMsg = withFirstMessage id

-- | Send a message and attachments to the specified channel.
sendMessage :: ChannelId -> EditMode -> Text -> [AttachmentData] -> MH ()
sendMessage chanId mode msg attachments =
    when (not $ shouldSkipMessage msg) $ do
        status <- use csConnectionStatus
        case status of
            Disconnected -> do
                let m = T.concat [ "Cannot send messages while disconnected. Enable logging to "
                                 , "get disconnection information. If Matterhorn's reconnection "
                                 , "attempts are failing, use `/reconnect` to attempt to "
                                 , "reconnect manually."
                                 ]
                mhError $ GenericError m
            Connected -> do
                session <- getSession
                doAsync Preempt $ do
                    -- Upload attachments
                    fileInfos <- forM attachments $ \a -> do
                        MM.mmUploadFile chanId (FB.fileInfoFilename $ attachmentDataFileInfo a)
                            (attachmentDataBytes a) session

                    let fileIds = Seq.fromList $
                                  fmap fileInfoId $
                                  concat $
                                  (F.toList . MM.uploadResponseFileInfos) <$> fileInfos

                    case mode of
                        NewPost -> do
                            let pendingPost = (rawPost msg chanId) { rawPostFileIds = fileIds }
                            void $ MM.mmCreatePost pendingPost session
                        Replying _ p -> do
                            let pendingPost = (rawPost msg chanId) { rawPostRootId = postRootId p <|> (Just $ postId p)
                                                                   , rawPostFileIds = fileIds
                                                                   }
                            void $ MM.mmCreatePost pendingPost session
                        Editing p ty -> do
                            let body = case ty of
                                         CP Emote -> addEmoteFormatting msg
                                         _ -> msg
                                update = (postUpdateBody body) { postUpdateFileIds = if null fileIds
                                                                                     then Nothing
                                                                                     else Just fileIds
                                                               }
                            void $ MM.mmPatchPost (postId p) update session

shouldSkipMessage :: Text -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = T.all (`elem` (" \t"::String)) s

editMessage :: Post -> MH ()
editMessage new = do
    myId <- gets myUserId
    withChannel (new^.postChannelIdL) $ \chan -> do
        let mTId = chan^.ccInfo.cdTeamId
        mBaseUrl <- case mTId of
            Nothing -> return Nothing
            Just tId -> Just <$> getServerBaseUrl tId

        let (msg, mentionedUsers) = clientPostToMessage (toClientPost mBaseUrl new (new^.postRootIdL))
            isEditedMessage m = m^.mMessageId == Just (MessagePostId $ new^.postIdL)

        csChannel (new^.postChannelIdL) . ccContents . cdMessages . traversed . filtered isEditedMessage .= msg
        mh $ invalidateCacheEntry (ChannelMessages $ new^.postChannelIdL)
        mh $ invalidateCacheEntry $ RenderedMessage $ MessagePostId $ postId new

        fetchMentionedUsers mentionedUsers

        when (postUserId new /= Just myId) $
            csChannel (new^.postChannelIdL) %= adjustEditedThreshold new

        csPostMap.ix(postId new) .= msg
        asyncFetchReactionsForPost (postChannelId new) new
        asyncFetchAttachments new

deleteMessage :: Post -> MH ()
deleteMessage new = do
    let isDeletedMessage m = m^.mMessageId == Just (MessagePostId $ new^.postIdL) ||
                             isReplyTo (new^.postIdL) m
        chan :: Traversal' ChatState ClientChannel
        chan = csChannel (new^.postChannelIdL)
    chan.ccContents.cdMessages.traversed.filtered isDeletedMessage %= (& mDeleted .~ True)
    chan %= adjustUpdated new
    mh $ invalidateCacheEntry (ChannelMessages $ new^.postChannelIdL)
    mh $ invalidateCacheEntry $ RenderedMessage $ MessagePostId $ postId new

addNewPostedMessage :: PostToAdd -> MH ()
addNewPostedMessage p =
    addMessageToState True True p >>= postProcessMessageAdd

-- | Adds the set of Posts to the indicated channel. The Posts must all
-- be for the specified Channel. The reqCnt argument indicates how many
-- posts were requested, which will determine whether a gap message is
-- added to either end of the posts list or not.
--
-- The addTrailingGap is only True when fetching the very latest
-- messages for the channel, and will suppress the generation of a Gap
-- message following the added block of messages.
addObtainedMessages :: ChannelId -> Int -> Bool -> Posts -> MH PostProcessMessageAdd
addObtainedMessages cId reqCnt addTrailingGap posts = do
  mh $ invalidateCacheEntry (ChannelMessages cId)
  if null $ posts^.postsOrderL
  then do when addTrailingGap $
            -- Fetched at the end of the channel, but nothing was
            -- available.  This is common if this is a new channel
            -- with no messages in it.  Need to remove any gaps that
            -- exist at the end of the channel.
            csChannels %= modifyChannelById cId
              (ccContents.cdMessages %~
               \msgs -> let startPoint = join $ _mMessageId <$> getLatestPostMsg msgs
                        in fst $ removeMatchesFromSubset isGap startPoint Nothing msgs)
          return NoAction
  else
    -- Adding a block of server-provided messages, which are known to
    -- be contiguous.  Locally this may overlap with some UnknownGap
    -- messages, which can therefore be removed.  Alternatively the
    -- new block may be discontiguous with the local blocks, in which
    -- case the new block should be surrounded by UnknownGaps.
    withChannelOrDefault cId NoAction $ \chan -> do
        let pIdList = toList (posts^.postsOrderL)
            mTId = chan^.ccInfo.cdTeamId
            -- the first and list PostId in the batch to be added
            earliestPId = last pIdList
            latestPId = head pIdList
            earliestDate = postCreateAt $ (posts^.postsPostsL) HM.! earliestPId
            latestDate = postCreateAt $ (posts^.postsPostsL) HM.! latestPId

            localMessages = chan^.ccContents . cdMessages

            -- Get a list of the duplicated message PostIds between
            -- the messages already in the channel and the new posts
            -- to be added.

            match = snd $ removeMatchesFromSubset
                          (\m -> maybe False (\p -> p `elem` pIdList) (messagePostId m))
                          (Just (MessagePostId earliestPId))
                          (Just (MessagePostId latestPId))
                          localMessages

            accum m l =
                case messagePostId m of
                    Just pId -> pId : l
                    Nothing -> l
            dupPIds = foldr accum [] match

            -- If there were any matches, then there was overlap of
            -- the new messages with existing messages.

            -- Don't re-add matching messages (avoid overhead like
            -- re-checking/re-fetching related post information, and
            -- do not signal action needed for notifications), and
            -- remove any gaps in the overlapping region.

            newGapMessage d isOlder =
              -- newGapMessage is a helper for generating a gap
              -- message
              do uuid <- generateUUID
                 let txt = "Load " <>
                           (if isOlder then "older" else "newer") <>
                           " messages" <>
                           (if isOlder then "  ↥↥↥" else "  ↧↧↧")
                     ty = if isOlder
                          then C UnknownGapBefore
                          else C UnknownGapAfter
                 return (newMessageOfType txt ty d
                         & mMessageId .~ Just (MessageUUID uuid))

            -- If this batch contains the latest known messages, do
            -- not add a following gap.  A gap at this point is added
            -- by a websocket disconnect, and any fetches thereafter
            -- are assumed to be the most current information (until
            -- another disconnect), so no gap is needed.
            -- Additionally, the presence of a gap at the end for a
            -- connected client causes a fetch of messages at this
            -- location, so adding the gap here would cause an
            -- infinite update loop.

            addingAtEnd = maybe True (latestDate >=) $
                          (^.mDate) <$> getLatestPostMsg localMessages

            addingAtStart = maybe True (earliestDate <=) $
                            (^.mDate) <$> getEarliestPostMsg localMessages
            removeStart = if addingAtStart && noMoreBefore
                          then Nothing
                          else Just (MessagePostId earliestPId)
            removeEnd = if addTrailingGap || (addingAtEnd && noMoreAfter)
                        then Nothing
                        else Just (MessagePostId latestPId)

            noMoreBefore = reqCnt < 0 && length pIdList < (-reqCnt)
            noMoreAfter = addTrailingGap || reqCnt > 0 && length pIdList < reqCnt

            reAddGapBefore = earliestPId `elem` dupPIds || noMoreBefore
            -- addingAtEnd used to be in reAddGapAfter but does not
            -- seem to be needed.  I may have missed a specific use
            -- case/scenario, so I've left it commented out here for
            -- debug assistance.
            reAddGapAfter = latestPId `elem` dupPIds || {- addingAtEnd || -} noMoreAfter

        -- The post map returned by the server will *already* have
        -- all thread messages for each post that is part of a
        -- thread. By calling installMessagesFromPosts here, we go ahead
        -- and populate the csPostMap with those posts so that below, in
        -- addMessageToState, we notice that we already know about reply
        -- parent messages and can avoid fetching them. This converts
        -- the posts to Messages and stores those and also returns
        -- them, but we don't need them here. We just want the post
        -- map update. This also gathers up the set of all mentioned
        -- usernames in the text of the messages which we need to use to
        -- submit a single batch request for user metadata so we don't
        -- submit one request per mention.
        void $ installMessagesFromPosts mTId posts

        -- Add all the new *unique* posts into the existing channel
        -- corpus, generating needed fetches of data associated with
        -- the post, and determining an notification action to be
        -- taken (if any).
        action <- foldr andProcessWith NoAction <$>
          mapM (addMessageToState False False . OldPost)
                   [ (posts^.postsPostsL) HM.! p
                   | p <- toList (posts^.postsOrderL)
                   , not (p `elem` dupPIds)
                   ]

        -- The channel messages now include all the fetched messages.
        -- Things to do at this point are:
        --
        --   1. Remove any duplicates just added, as well as any gaps
        --   2. Add new gaps (if needed) at either end of the added
        --      messages.
        --   3. Update the "current selection" if it was on a removed message.
        --
        -- Do this with the updated copy of the channel's messages.

        withChannelOrDefault cId () $ \updchan -> do
          let updMsgs = updchan ^. ccContents . cdMessages

          -- Remove any gaps in the added region.  If there was an
          -- active message selection and it is one of the removed
          -- gaps, reset the selection to the beginning or end of the
          -- added region (if there are any added selectable messages,
          -- otherwise just the end if the message list in it's
          -- entirety, or no selection at all).

          let (resultMessages, removedMessages) =
                removeMatchesFromSubset isGap removeStart removeEnd updMsgs
          csChannels %= modifyChannelById cId
            (ccContents.cdMessages .~ resultMessages)

          let processTeam tId = do
                -- Determine if the current selected message was one of the
                -- removed messages.
                selMsgId <- use (csTeam(tId).tsMessageSelect.to selectMessageId)
                let rmvdSel = do
                      i <- selMsgId -- :: Maybe MessageId
                      findMessage i removedMessages
                    rmvdSelType = _mType <$> rmvdSel

                case rmvdSel of
                  Nothing -> return ()
                  Just rm ->
                    if isGap rm
                    then return ()  -- handled during gap insertion below
                    else do
                      -- Replaced a selected message that wasn't a gap.
                      -- This is unlikely, but may occur if the previously
                      -- selected message was just deleted by another user
                      -- and is in the fetched region.  The choices here are
                      -- to move the selection, or cancel the selection.
                      -- Both will be unpleasant surprises for the user, but
                      -- cancelling the selection is probably the better
                      -- choice than allowing the user to perform select
                      -- actions on a message that isn't the one they just
                      -- selected.
                      setMode Main
                      csTeam(tId).tsMessageSelect .= MessageSelectState Nothing

                -- Add a gap at each end of the newly fetched data, unless:
                --   1. there is an overlap
                --   2. there is no more in the indicated direction
                --      a. indicated by adding messages later than any currently
                --         held messages (see note above re 'addingAtEnd').
                --      b. the amount returned was less than the amount requested

                if reAddGapBefore
                  then
                    -- No more gaps.  If the selected gap was removed, move
                    -- select to first (earliest) message)
                    case rmvdSelType of
                      Just (C UnknownGapBefore) ->
                        csTeam(tId).tsMessageSelect .= MessageSelectState (pure $ MessagePostId earliestPId)
                      _ -> return ()
                  else do
                    -- add a gap at the start of the newly fetched block and
                    -- make that the active selection if this fetch removed
                    -- the previously selected gap in this direction.
                    gapMsg <- newGapMessage (justBefore earliestDate) True
                    csChannels %= modifyChannelById cId
                      (ccContents.cdMessages %~ addMessage gapMsg)
                    -- Move selection from old gap to new gap
                    case rmvdSelType of
                      Just (C UnknownGapBefore) -> do
                        csTeam(tId).tsMessageSelect .= MessageSelectState (gapMsg^.mMessageId)
                      _ -> return ()

                if reAddGapAfter
                  then
                    -- No more gaps.  If the selected gap was removed, move
                    -- select to last (latest) message.
                    case rmvdSelType of
                      Just (C UnknownGapAfter) ->
                        csTeam(tId).tsMessageSelect .= MessageSelectState (pure $ MessagePostId latestPId)
                      _ -> return ()
                  else do
                    -- add a gap at the end of the newly fetched block and
                    -- make that the active selection if this fetch removed
                    -- the previously selected gap in this direction.
                    gapMsg <- newGapMessage (justAfter latestDate) False
                    csChannels %= modifyChannelById cId
                      (ccContents.cdMessages %~ addMessage gapMsg)
                    -- Move selection from old gap to new gap
                    case rmvdSelType of
                      Just (C UnknownGapAfter) ->
                        csTeam(tId).tsMessageSelect .= MessageSelectState (gapMsg^.mMessageId)
                      _ -> return ()

          case mTId of
              Nothing -> do
                  ts <- use csTeams
                  forM_ (HM.keys ts) processTeam
              Just tId -> processTeam tId

        -- Now initiate fetches for use information for any
        -- as-yet-unknown users related to this new set of messages

        let users = foldr (\post s -> maybe s (flip Set.insert s) (postUserId post))
                          Set.empty (posts^.postsPostsL)
            addUnknownUsers inputUserIds = do
                knownUserIds <- Set.fromList <$> gets allUserIds
                let unknownUsers = Set.difference inputUserIds knownUserIds
                if Set.null unknownUsers
                   then return ()
                   else handleNewUsers (Seq.fromList $ toList unknownUsers) (return ())

        addUnknownUsers users

        -- Return the aggregated user notification action needed
        -- relative to the set of added messages.

        return action

-- | Adds a possibly new message to the associated channel contents.
-- Returns an indicator of whether the user should be potentially
-- notified of a change (a new message not posted by this user, a
-- mention of the user, etc.).  This operation has no effect on any
-- existing UnknownGap entries and should be called when those are
-- irrelevant.
--
-- The first boolean argument ('doFetchMentionedUsers') indicates
-- whether this function should schedule a fetch for any mentioned
-- users in the message. This is provided so that callers can batch
-- this operation if a large collection of messages is being added
-- together, in which case we don't want this function to schedule a
-- single request per message (worst case). If you're calling this as
-- part of scrollback processing, you should pass False. Otherwise if
-- you're adding only a single message, you should pass True.
--
-- The second boolean argument ('fetchAuthor') is similar to the first
-- boolean argument but it refers to the author of the message instead
-- of any user mentions within the message body.
--
-- The third argument ('newPostData') indicates whether this message
-- is being added as part of a fetch of old messages (e.g. scrollback)
-- or if ti is a new message and affects things like whether
-- notifications are generated and if the "New Messages" marker gets
-- updated.
addMessageToState :: Bool -> Bool -> PostToAdd -> MH PostProcessMessageAdd
addMessageToState doFetchMentionedUsers fetchAuthor newPostData = do
    let (new, wasMentioned) = case newPostData of
          -- A post from scrollback history has no mention data, and
          -- that's okay: we only need to track mentions to tell the
          -- user that recent posts contained mentions.
          OldPost p      -> (p, False)
          RecentPost p m -> (p, m)

    st <- use id
    case st ^? csChannel(postChannelId new) of
        Nothing -> do
            session <- getSession
            doAsyncWith Preempt $ do
                nc <- MM.mmGetChannel (postChannelId new) session
                member <- MM.mmGetChannelMember (postChannelId new) UserMe session

                let chType = nc^.channelTypeL
                    pref = showGroupChannelPref (postChannelId new) (myUserId st)

                -- If the channel has been archived, we don't want to
                -- post this message or add the channel to the state.
                case channelDeleted nc of
                    True -> return Nothing
                    False -> return $ Just $ do
                        -- If the incoming message is for a group
                        -- channel we don't know about, that's because
                        -- it was previously hidden by the user. We need
                        -- to show it, and to do that we need to update
                        -- the server-side preference. (That, in turn,
                        -- triggers a channel refresh.)
                        if chType == Group
                            then applyPreferenceChange pref
                            else refreshChannel SidebarUpdateImmediate nc member

                        addMessageToState doFetchMentionedUsers fetchAuthor newPostData >>=
                            postProcessMessageAdd

            return NoAction
        Just ch -> do
            let mTId = ch^.ccInfo.cdTeamId
            mBaseUrl <- case mTId of
                Nothing -> return Nothing
                Just tId -> Just <$> getServerBaseUrl tId

            let cp = toClientPost mBaseUrl new (new^.postRootIdL)
                fromMe = (cp^.cpUser == (Just $ myUserId st)) &&
                         (isNothing $ cp^.cpUserOverride)
                userPrefs = st^.csResources.crUserPreferences
                isJoinOrLeave = case cp^.cpType of
                  Join  -> True
                  Leave -> True
                  _     -> False
                ignoredJoinLeaveMessage =
                  not (userPrefs^.userPrefShowJoinLeave) && isJoinOrLeave
                cId = postChannelId new

                doAddMessage = do
                    -- Do we have the user data for the post author?
                    case cp^.cpUser of
                        Nothing -> return ()
                        Just authorId -> when fetchAuthor $ do
                            authorResult <- gets (userById authorId)
                            when (isNothing authorResult) $
                                handleNewUsers (Seq.singleton authorId) (return ())

                    curTId <- use csCurrentTeamId
                    currCId <- use (csCurrentChannelId curTId)
                    flags <- use (csResources.crFlaggedPosts)
                    let (msg', mentionedUsers) = clientPostToMessage cp
                                 & _1.mFlagged .~ ((cp^.cpPostId) `Set.member` flags)

                    when doFetchMentionedUsers $
                        fetchMentionedUsers mentionedUsers

                    csPostMap.at(postId new) .= Just msg'
                    mh $ invalidateCacheEntry (ChannelMessages cId)
                    mh $ invalidateCacheEntry $ RenderedMessage $ MessagePostId $ postId new
                    csChannels %= modifyChannelById cId
                      ((ccContents.cdMessages %~ addMessage msg') .
                       (if not ignoredJoinLeaveMessage then adjustUpdated new else id) .
                       (\c -> if currCId == cId
                              then c
                              else case newPostData of
                                     OldPost _ -> c
                                     RecentPost _ _ ->
                                       updateNewMessageIndicator new c) .
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
                                        (\s _ -> MM.mmGetThread parentId s)
                                        (\_ p -> Just $ updatePostMap mTId p)
                                _ -> return ()
                        _ -> return ()

                    doAddMessage

                postedChanMessage =
                  withChannelOrDefault (postChannelId new) NoAction $ \chan -> do
                      currTid <- use csCurrentTeamId
                      currCId <- use (csCurrentChannelId currTid)

                      let notifyPref = notifyPreference (myUser st) chan
                          curChannelAction = if postChannelId new == currCId
                                             then UpdateServerViewed
                                             else NoAction
                          originUserAction =
                            if | fromMe                            -> NoAction
                               | ignoredJoinLeaveMessage           -> NoAction
                               | notifyPref == NotifyOptionAll     -> NotifyUser [newPostData]
                               | notifyPref == NotifyOptionMention
                                   && wasMentioned                 -> NotifyUser [newPostData]
                               | otherwise                         -> NoAction

                      return $ curChannelAction `andProcessWith` originUserAction

            doHandleAddedMessage

-- | PostProcessMessageAdd is an internal value that informs the main
-- code whether the user should be notified (e.g., ring the bell) or
-- the server should be updated (e.g., that the channel has been
-- viewed).  This is a monoid so that it can be folded over when there
-- are multiple inbound posts to be processed.
data PostProcessMessageAdd = NoAction
                           | NotifyUser [PostToAdd]
                           | UpdateServerViewed
                           | NotifyUserAndServer [PostToAdd]

andProcessWith
  :: PostProcessMessageAdd -> PostProcessMessageAdd -> PostProcessMessageAdd
andProcessWith NoAction x                                        = x
andProcessWith x NoAction                                        = x
andProcessWith (NotifyUserAndServer p) UpdateServerViewed        = NotifyUserAndServer p
andProcessWith (NotifyUserAndServer p1) (NotifyUser p2)          = NotifyUserAndServer (p1 <> p2)
andProcessWith (NotifyUserAndServer p1) (NotifyUserAndServer p2) = NotifyUserAndServer (p1 <> p2)
andProcessWith (NotifyUser p1) (NotifyUserAndServer p2)          = NotifyUser (p1 <> p2)
andProcessWith (NotifyUser p1) (NotifyUser p2)                   = NotifyUser (p1 <> p2)
andProcessWith (NotifyUser p) UpdateServerViewed                 = NotifyUserAndServer p
andProcessWith UpdateServerViewed UpdateServerViewed             = UpdateServerViewed
andProcessWith UpdateServerViewed (NotifyUserAndServer p)        = NotifyUserAndServer p
andProcessWith UpdateServerViewed (NotifyUser p)                 = NotifyUserAndServer p

-- | postProcessMessageAdd performs the actual actions indicated by
-- the corresponding input value.
postProcessMessageAdd :: PostProcessMessageAdd -> MH ()
postProcessMessageAdd ppma = postOp ppma
    where
        postOp NoAction                = return ()
        postOp UpdateServerViewed      = updateViewed False
        postOp (NotifyUser p)          = maybeRingBell >> mapM_ maybeNotify p
        postOp (NotifyUserAndServer p) = updateViewed False >> maybeRingBell >> mapM_ maybeNotify p

maybeNotify :: PostToAdd -> MH ()
maybeNotify (OldPost _) = do
    return ()
maybeNotify (RecentPost post mentioned) = runNotifyCommand post mentioned

maybeRingBell :: MH ()
maybeRingBell = do
    doBell <- use (csResources.crConfiguration.configActivityBellL)
    when doBell $ do
        vty <- mh getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

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

encodeToJSONstring :: A.ToJSON a => a -> String
encodeToJSONstring a = BL8.unpack $ A.encode a

-- Notification Version 2 payload definition
data NotificationV2 = NotificationV2
    { version :: Int
    , message :: Text
    , mention :: Bool
    , from :: Text
    } deriving (Show)
instance A.ToJSON NotificationV2 where
    toJSON (NotificationV2 vers msg mentioned sender) =
        A.object [ "version"  A..= vers
                 , "message"  A..= msg
                 , "mention"  A..= mentioned
                 , "from"     A..= sender
                 ]

-- We define a notifyGetPayload for each notification version.
notifyGetPayload :: NotificationVersion -> ChatState -> Post -> Bool -> Maybe String
notifyGetPayload NotifyV1 _ _ _ = do return ""
notifyGetPayload NotifyV2 st post mentioned = do
    let notification = NotificationV2 2 msg mentioned sender
    return (encodeToJSONstring notification)
        where
            msg = sanitizeUserText $ postMessage post
            sender = maybePostUsername st post

handleNotifyCommand :: Post -> Bool -> NotificationVersion -> MH ()
handleNotifyCommand post mentioned NotifyV1 = do
    outputChan <- use (csResources.crSubprocessLog)
    st <- use id
    notifyCommand <- use (csResources.crConfiguration.configActivityNotifyCommandL)
    case notifyCommand of
        Nothing -> return ()
        Just cmd ->
            doAsyncWith Preempt $ do
                let messageString = T.unpack $ sanitizeUserText $ postMessage post
                    notified = if mentioned then "1" else "2"
                    sender = T.unpack $ maybePostUsername st post
                runLoggedCommand outputChan (T.unpack cmd)
                                 [notified, sender, messageString] Nothing Nothing
                return Nothing
handleNotifyCommand post mentioned NotifyV2 = do
    outputChan <- use (csResources.crSubprocessLog)
    st <- use id
    let payload = notifyGetPayload NotifyV2 st post mentioned
    notifyCommand <- use (csResources.crConfiguration.configActivityNotifyCommandL)
    case notifyCommand of
        Nothing -> return ()
        Just cmd ->
            doAsyncWith Preempt $ do
                runLoggedCommand outputChan (T.unpack cmd) [] payload Nothing
                return Nothing

runNotifyCommand :: Post -> Bool -> MH ()
runNotifyCommand post mentioned = do
    notifyVersion <- use (csResources.crConfiguration.configActivityNotifyVersionL)
    case notifyVersion of
        NotifyV1 -> handleNotifyCommand post mentioned NotifyV1
        NotifyV2 -> handleNotifyCommand post mentioned NotifyV2

maybePostUsername :: ChatState -> Post -> T.Text
maybePostUsername st p =
    fromMaybe T.empty $ do
        uId <- postUserId p
        usernameForUserId uId st

-- | Fetches additional message history for the current channel.  This
-- is generally called when in ChannelScroll mode, in which state the
-- output is cached and seen via a scrolling viewport; new messages
-- received in this mode are not normally shown, but this explicit
-- user-driven fetch should be displayed, so this also invalidates the
-- cache.
--
-- This function assumes it is being called to add "older" messages to
-- the message history (i.e. near the beginning of the known
-- messages).  It will normally try to overlap the fetch with the
-- known existing messages so that when the fetch results are
-- processed (which should be a contiguous set of messages as provided
-- by the server) there will be an overlap with existing messages; if
-- there is no overlap, then a special "gap" must be inserted in the
-- area between the existing messages and the newly fetched messages
-- to indicate that this client does not know if there are missing
-- messages there or not.
--
-- In order to achieve an overlap, this code attempts to get the
-- second oldest messages as the message ID to pass to the server as
-- the "older than" marker ('postQueryBefore'), so that the oldest
-- message here overlaps with the fetched results to ensure no gap
-- needs to be inserted.  However, there may already be a gap between
-- the oldest and second-oldest messages, so this code must actually
-- search for the first set of two *contiguous* messages it is aware
-- of to avoid adding additional gaps. (It's OK if gaps are added, but
-- the user must explicitly request a check for messages in order to
-- eliminate them, so it's better to avoid adding them in the first
-- place).  This code is nearly always used to extend the older
-- history of a channel that information has already been retrieved
-- from, so it's almost certain that there are at least two contiguous
-- messages to use as a starting point, but exceptions are new
-- channels and empty channels.
asyncFetchMoreMessages :: MH ()
asyncFetchMoreMessages = do
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    withChannel cId $ \chan ->
        let offset = max 0 $ length (chan^.ccContents.cdMessages) - 2
            page = offset `div` pageAmount
            usefulMsgs = getTwoContiguousPosts Nothing (chan^.ccContents.cdMessages.to reverseMessages)
            sndOldestId = (messagePostId . snd) =<< usefulMsgs
            query = MM.defaultPostQuery
                      { MM.postQueryPage = maybe (Just page) (const Nothing) sndOldestId
                      , MM.postQueryPerPage = Just pageAmount
                      , MM.postQueryBefore = sndOldestId
                      }
            addTrailingGap = MM.postQueryBefore query == Nothing &&
                             MM.postQueryPage query == Just 0
        in doAsyncChannelMM Preempt cId
               (\s c -> MM.mmGetPostsForChannel c query s)
               (\c p -> Just $ do
                   pp <- addObtainedMessages c (-pageAmount) addTrailingGap p
                   postProcessMessageAdd pp)


-- | Given a starting point and a direction to move from that point,
-- returns the closest two adjacent messages on that direction (as a
-- tuple of closest and next-closest), or Nothing if there are no
-- adjacent messages in the indicated direction.
getTwoContiguousPosts :: SeqDirection dir =>
                         Maybe Message
                      -> DirectionalSeq dir Message
                      -> Maybe (Message, Message)
getTwoContiguousPosts startMsg msgs =
  let go start =
        do anchor <- getRelMessageId (_mMessageId =<< start) msgs
           hinge <- getRelMessageId (anchor^.mMessageId) msgs
           if isGap anchor || isGap hinge
             then go $ Just anchor
             else Just (anchor, hinge)
  in go startMsg


asyncFetchMessagesForGap :: ChannelId -> Message -> MH ()
asyncFetchMessagesForGap cId gapMessage =
  when (isGap gapMessage) $
  withChannel cId $ \chan ->
    let offset = max 0 $ length (chan^.ccContents.cdMessages) - 2
        page = offset `div` pageAmount
        chanMsgs = chan^.ccContents.cdMessages
        fromMsg = Just gapMessage
        fetchNewer = case gapMessage^.mType of
                       C UnknownGapAfter -> True
                       C UnknownGapBefore -> False
                       _ -> error "fetch gap messages: unknown gap message type"
        baseId = messagePostId . snd =<<
                 case gapMessage^.mType of
                    C UnknownGapAfter -> getTwoContiguousPosts fromMsg $
                                         reverseMessages chanMsgs
                    C UnknownGapBefore -> getTwoContiguousPosts fromMsg chanMsgs
                    _ -> error "fetch gap messages: unknown gap message type"
        query = MM.defaultPostQuery
                { MM.postQueryPage = maybe (Just page) (const Nothing) baseId
                , MM.postQueryPerPage = Just pageAmount
                , MM.postQueryBefore = if fetchNewer then Nothing else baseId
                , MM.postQueryAfter = if fetchNewer then baseId else Nothing
                }
        addTrailingGap = MM.postQueryBefore query == Nothing &&
                         MM.postQueryPage query == Just 0
    in doAsyncChannelMM Preempt cId
       (\s c -> MM.mmGetPostsForChannel c query s)
       (\c p -> Just $ do
           void $ addObtainedMessages c (-pageAmount) addTrailingGap p)

-- | Given a particular message ID, this fetches n messages before and
-- after immediately before and after the specified message in order
-- to establish some context for that message.  This is frequently
-- used as a background operation when looking at search or flag
-- results so that jumping to message select mode for one of those
-- messages will show a bit of context (and it also prevents showing
-- gap messages for adjacent targets).
--
-- The result will be adding at most 2n messages to the channel, with
-- the input post ID being somewhere in the middle of the added
-- messages.
--
-- Note that this fetch will add messages to the channel, but it
-- performs no notifications or updates of new-unread indicators
-- because it is assumed to be used for non-current (previously-seen)
-- messages in background mode.
asyncFetchMessagesSurrounding :: ChannelId -> PostId -> MH ()
asyncFetchMessagesSurrounding cId pId = do
    let query = MM.defaultPostQuery
          { MM.postQueryBefore = Just pId
          , MM.postQueryPerPage = Just reqAmt
          }
        reqAmt = 5  -- both before and after
    doAsyncChannelMM Preempt cId
      -- first get some messages before the target, no overlap
      (\s c -> MM.mmGetPostsForChannel c query s)
      (\c p -> Just $ do
          let last2ndId = secondToLastPostId p
          void $ addObtainedMessages c (-reqAmt) False p
          -- now start 2nd from end of this fetch to fetch some
          -- messages forward, also overlapping with this fetch and
          -- the original message ID to eliminate all gaps in this
          -- surrounding set of messages.
          let query' = MM.defaultPostQuery
                       { MM.postQueryAfter = last2ndId
                       , MM.postQueryPerPage = Just $ reqAmt + 2
                       }
          doAsyncChannelMM Preempt cId
            (\s' c' -> MM.mmGetPostsForChannel c' query' s')
            (\c' p' -> Just $ do
                void $ addObtainedMessages c' (reqAmt + 2) False p'
            )
      )
      where secondToLastPostId posts =
              let pl = toList $ postsOrder posts
              in if length pl > 1 then Just $ last $ init pl else Nothing

fetchVisibleIfNeeded :: MH ()
fetchVisibleIfNeeded = do
    sts <- use csConnectionStatus
    when (sts == Connected) $ do
        tId <- use csCurrentTeamId
        cId <- use (csCurrentChannelId tId)
        withChannel cId $ \chan -> do
            let msgs = chan^.ccContents.cdMessages.to reverseMessages
                (numRemaining, gapInDisplayable, _, rel'pId, overlap) =
                    foldl gapTrail (numScrollbackPosts, False, Nothing, Nothing, 2) msgs

                gapTrail :: (Int, Bool, Maybe MessageId, Maybe MessageId, Int)
                         -> Message
                         -> (Int, Bool, Maybe MessageId, Maybe MessageId, Int)
                gapTrail a@(_,  True, _, _, _) _ = a
                gapTrail a@(0,     _, _, _, _) _ = a
                gapTrail   (a, False, b, c, d) m | isGap m = (a, True, b, c, d)
                gapTrail (remCnt, _, prev'pId, prev''pId, ovl) msg =
                    (remCnt - 1, False, msg^.mMessageId <|> prev'pId, prev'pId <|> prev''pId,
                     ovl + if not (isPostMessage msg) then 1 else 0)

                numToRequest = numRemaining + overlap
                query = MM.defaultPostQuery
                        { MM.postQueryPage    = Just 0
                        , MM.postQueryPerPage = Just numToRequest
                        }
                finalQuery = case rel'pId of
                               Just (MessagePostId pid) -> query { MM.postQueryBefore = Just pid }
                               _ -> query
                op = \s c -> MM.mmGetPostsForChannel c finalQuery s
                addTrailingGap = MM.postQueryBefore finalQuery == Nothing &&
                                 MM.postQueryPage finalQuery == Just 0

            when ((not $ chan^.ccContents.cdFetchPending) && gapInDisplayable) $ do
                csChannel(cId).ccContents.cdFetchPending .= True
                doAsyncChannelMM Preempt cId op
                    (\c p -> Just $ do
                        csChannel(c).ccContents.cdFetchPending .= False
                        addObtainedMessages c (-numToRequest) addTrailingGap p >>= postProcessMessageAdd)

asyncFetchAttachments :: Post -> MH ()
asyncFetchAttachments p = do
    let cId = p^.postChannelIdL
        pId = p^.postIdL
    session <- getSession
    host <- use (csResources.crConn.cdHostnameL)
    F.forM_ (p^.postFileIdsL) $ \fId -> doAsyncWith Normal $ do
        info <- MM.mmGetMetadataForFile fId session
        let scheme = "https://"
            attUrl = scheme <> host <> urlForFile fId
            attachment = mkAttachment (fileInfoName info) attUrl fId
            addIfMissing a as =
                if isNothing $ Seq.elemIndexL a as
                then a Seq.<| as
                else as
            addAttachment m
                | m^.mMessageId == Just (MessagePostId pId) =
                    m & mAttachments %~ (addIfMissing attachment)
                | otherwise =
                    m
        return $ Just $ do
            csChannel(cId).ccContents.cdMessages.traversed %= addAttachment
            mh $ do
                invalidateCacheEntry $ ChannelMessages cId
                invalidateCacheEntry $ RenderedMessage $ MessagePostId pId

-- | Given a post ID, switch to that post's channel and select the post
-- in message selection mode.
--
-- This function will do what it can to honor the request even when we
-- don't know about the post because it hasn't been fetched, or when
-- the post is in a channel that we aren't a member of. In each case a
-- reasonable effort will be made (fetch the post, join the channel)
-- before giving up.
jumpToPost :: PostId -> MH ()
jumpToPost pId = do
    st <- use id
    case getMessageForPostId st pId of
      Just msg ->
        case msg ^. mChannelId of
          Just cId -> do
              -- Are we a member of the channel?
              case findChannelById cId (st^.csChannels) of
                  Nothing ->
                      joinChannel' cId (Just $ jumpToPost pId)
                  Just _ -> do
                      setFocus cId
                      setMode MessageSelect
                      csCurrentTeam.tsMessageSelect .= MessageSelectState (msg^.mMessageId)
          Nothing ->
            error "INTERNAL: selected Post ID not associated with a channel"
      Nothing -> do
          session <- getSession
          doAsyncWith Preempt $ do
              result <- try $ MM.mmGetPost pId session
              return $ Just $ do
                  case result of
                      Right p -> do
                          -- Are we a member of the channel?
                          case findChannelById (postChannelId p) (st^.csChannels) of
                              -- If not, join it and then try jumping to
                              -- the post if the channel join is successful.
                              Nothing -> do
                                  joinChannel' (postChannelId p) (Just $ jumpToPost pId)
                              -- Otherwise add the post to the state and
                              -- then jump.
                              Just _ -> do
                                  void $ addMessageToState True True (OldPost p)
                                  jumpToPost pId
                      Left (_::SomeException) ->
                          postErrorMessage' "Could not fetch linked post"
