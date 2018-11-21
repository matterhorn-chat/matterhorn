{-# LANGUAGE MultiWayIf #-}
module State.Messages
  ( PostToAdd(..)
  , addDisconnectGaps
  , lastMsg
  , sendMessage
  , editMessage
  , deleteMessage
  , addNewPostedMessage
  , asyncFetchMoreMessages
  , fetchVisibleIfNeeded
  , disconnectChannels
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( getVtyHandle, invalidateCacheEntry )
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

import           Constants
import           State.Common
import           State.Channels
import           State.Reactions
import           State.Users
import           TimeUtils
import           Types
import           Types.Common ( sanitizeUserText )


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

-- | Websocket was disconnected, so all channels may now miss some
-- messages
disconnectChannels :: MH ()
disconnectChannels = addDisconnectGaps

clearPendingFlags :: ChannelId -> MH ()
clearPendingFlags c = csChannel(c).ccContents.cdFetchPending .= False

addEndGap :: ChannelId -> MH ()
addEndGap cId = withChannel cId $ \chan ->
    let lastmsg_ = chan^.ccContents.cdMessages.to reverseMessages.to lastMsg
        lastIsGap = maybe False isGap lastmsg_
        gapMsg = newGapMessage timeJustAfterLast
        timeJustAfterLast = maybe t0 (justAfter . _mDate) lastmsg_
        t0 = ServerTime $ originTime  -- use any time for a channel with no messages yet
        newGapMessage = newMessageOfType (T.pack "Disconnected. Will refresh when connected.") (C UnknownGap)
    in unless lastIsGap
           (csChannels %= modifyChannelById cId (ccContents.cdMessages %~ addMessage gapMsg))

lastMsg :: RetrogradeMessages -> Maybe Message
lastMsg = withFirstMessage id

sendMessage :: EditMode -> Text -> MH ()
sendMessage mode msg =
    case shouldSkipMessage msg of
        True -> return ()
        False -> do
            status <- use csConnectionStatus
            st <- use id
            case status of
                Disconnected -> do
                    let m = "Cannot send messages while disconnected."
                    mhError $ GenericError m
                Connected -> do
                    let chanId = st^.csCurrentChannelId
                    session <- getSession
                    doAsync Preempt $ do
                      case mode of
                        NewPost -> do
                            let pendingPost = rawPost msg chanId
                            void $ MM.mmCreatePost pendingPost session
                        Replying _ p -> do
                            let pendingPost = (rawPost msg chanId) { rawPostRootId = postRootId p <|> (Just $ postId p) }
                            void $ MM.mmCreatePost pendingPost session
                        Editing p ty -> do
                            let body = if ty == CP Emote
                                       then addEmoteFormatting msg
                                       else msg
                            void $ MM.mmPatchPost (postId p) (postUpdateBody body) session

shouldSkipMessage :: Text -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = T.all (`elem` (" \t"::String)) s

editMessage :: Post -> MH ()
editMessage new = do
    myId <- gets myUserId
    let isEditedMessage m = m^.mMessageId == Just (MessagePostId $ new^.postIdL)
        (msg, mentionedUsers) = clientPostToMessage (toClientPost new (new^.postRootIdL))
        chan = csChannel (new^.postChannelIdL)
    chan . ccContents . cdMessages . traversed . filtered isEditedMessage .= msg

    fetchUsersByUsername $ F.toList mentionedUsers

    when (postUserId new /= Just myId) $
        chan %= adjustEditedThreshold new

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

addNewPostedMessage :: PostToAdd -> MH ()
addNewPostedMessage p =
    addMessageToState True p >>= postProcessMessageAdd

addObtainedMessages :: ChannelId -> Int -> Posts -> MH PostProcessMessageAdd
addObtainedMessages cId reqCnt posts = do
    -- Adding a block of server-provided messages, which are known to
    -- be contiguous.  Locally this may overlap with some UnknownGap
    -- messages, which can therefore be removed.  Alternatively the
    -- new block may be discontiguous with the local blocks, in which
    -- case the new block should be surrounded by UnknownGaps.
    withChannelOrDefault cId NoAction $ \chan -> do
        let pIdList = toList (posts^.postsOrderL)
            -- the first and list PostId in the batch to be added
            earliestPId = last pIdList
            latestPId = head pIdList
            earliestDate = postCreateAt $ (posts^.postsPostsL) HM.! earliestPId
            latestDate = postCreateAt $ (posts^.postsPostsL) HM.! latestPId

            localMessages = chan^.ccContents . cdMessages

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
            removeStart = if addingAtStart && noMoreBefore then Nothing else Just (MessagePostId earliestPId)
            removeEnd = if addingAtEnd then Nothing else Just (MessagePostId latestPId)

            noMoreBefore = reqCnt < 0 && length pIdList < (-reqCnt)
            noMoreAfter = reqCnt > 0 && length pIdList < reqCnt

        -- The post map returned by the server will *already* have
        -- all thread messages for each post that is part of a
        -- thread. By calling messagesFromPosts here, we go ahead and
        -- populate the csPostMap with those posts so that below, in
        -- addMessageToState, we notice that we already know about reply
        -- parent messages and can avoid fetching them. This converts
        -- the posts to Messages and stores those and also returns
        -- them, but we don't need them here. We just want the post map
        -- update.
        (_, mentionedUsers) <- messagesFromPosts posts

        fetchUsersByUsername $ F.toList mentionedUsers

        -- Add all the new *unique* posts into the existing channel
        -- corpus, generating needed fetches of data associated with
        -- the post, and determining an notification action to be
        -- taken (if any).
        action <- foldr andProcessWith NoAction <$>
          mapM (addMessageToState False . OldPost)
                   [ (posts^.postsPostsL) HM.! p
                   | p <- toList (posts^.postsOrderL)
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
-- The boolean argument indicates whether this function should schedule
-- a fetch for any mentioned users in the message. This is provided
-- so that callers can batch this operation if a large collection of
-- messages is being added together, in which case we don't want this
-- function to schedule a single request per message (worst case). If
-- you're calling this as part of scrollback processing, you should pass
-- False. Otherwise if you're adding only a single message, you should
-- pass True.
addMessageToState :: Bool -> PostToAdd -> MH PostProcessMessageAdd
addMessageToState fetchMentionedUsers newPostData = do
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

                        addMessageToState fetchMentionedUsers newPostData >>= postProcessMessageAdd

            return NoAction
        Just _ -> do
            let cp = toClientPost new (new^.postRootIdL)
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
                    currCId <- use csCurrentChannelId
                    flags <- use (csResources.crFlaggedPosts)
                    let (msg', mentionedUsers) = clientPostToMessage cp
                                 & _1.mFlagged .~ ((cp^.cpPostId) `Set.member` flags)

                    when fetchMentionedUsers $
                        fetchUsersByUsername $ F.toList mentionedUsers

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
                                        (\_ p -> Just $ do
                                            void $ messagesFromPosts p
                                        )
                                _ -> return ()
                        _ -> return ()

                    doAddMessage

                postedChanMessage =
                  withChannelOrDefault (postChannelId new) NoAction $ \chan -> do
                      currCId <- use csCurrentChannelId

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
    doBell <- use (csResources.crConfiguration.to configActivityBell)
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

runNotifyCommand :: Post -> Bool -> MH ()
runNotifyCommand post mentioned = do
    outputChan <- use (csResources.crSubprocessLog)
    st <- use id
    notifyCommand <- use (csResources.crConfiguration.to configActivityNotifyCommand)
    case notifyCommand of
        Nothing -> return ()
        Just cmd ->
            doAsyncWith Preempt $ do
                let messageString = T.unpack $ sanitizeUserText $ postMessage post
                    notified = if mentioned then "1" else "2"
                    sender = T.unpack $ maybePostUsername st post
                runLoggedCommand False outputChan (T.unpack cmd)
                                 [notified, sender, messageString] Nothing Nothing
                return Nothing

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
            first' = splitMessagesOn (isJust . messagePostId) (chan^.ccContents.cdMessages)
            second' = splitMessagesOn (isJust . messagePostId) $ snd $ snd first'
            query = MM.defaultPostQuery
                      { MM.postQueryPage = Just (offset `div` pageAmount)
                      , MM.postQueryPerPage = Just pageAmount
                      }
                    & \q -> case (fst first', fst second' >>= messagePostId) of
                             (Just _, Just i) -> q { MM.postQueryBefore = Just i
                                                   , MM.postQueryPage   = Just 0
                                                   }
                             _ -> q
        in doAsyncChannelMM Preempt cId
               (\s _ c -> MM.mmGetPostsForChannel c query s)
               (\c p -> Just $ do
                   addObtainedMessages c (-pageAmount) p >>= postProcessMessageAdd
                   mh $ invalidateCacheEntry (ChannelMessages cId))

fetchVisibleIfNeeded :: MH ()
fetchVisibleIfNeeded = do
    sts <- use csConnectionStatus
    when (sts == Connected) $ do
        cId <- use csCurrentChannelId
        withChannel cId $ \chan ->
            let msgs = chan^.ccContents.cdMessages.to reverseMessages
                (numRemaining, gapInDisplayable, _, rel'pId, overlap) =
                    foldl gapTrail (numScrollbackPosts, False, Nothing, Nothing, 2) msgs
                gapTrail a@(_,  True, _, _, _) _ = a
                gapTrail a@(0,     _, _, _, _) _ = a
                gapTrail   (a, False, b, c, d) m | isGap m = (a, True, b, c, d)
                gapTrail (remCnt, _, prev'pId, prev''pId, ovl) msg =
                    (remCnt - 1, False, msg^.mMessageId <|> prev'pId, prev'pId <|> prev''pId,
                     ovl + if not (isPostMessage msg) then 1 else 0)
                numToReq = numRemaining + overlap
                query = MM.defaultPostQuery
                        { MM.postQueryPage    = Just 0
                        , MM.postQueryPerPage = Just numToReq
                        }
                finalQuery = case rel'pId of
                               Just (MessagePostId pid) -> query { MM.postQueryBefore = Just pid }
                               _ -> query
                op = \s _ c -> MM.mmGetPostsForChannel c finalQuery s
            in when ((not $ chan^.ccContents.cdFetchPending) && gapInDisplayable) $ do
                      csChannel(cId).ccContents.cdFetchPending .= True
                      doAsyncChannelMM Preempt cId op
                          (\c p -> Just $ do
                              addObtainedMessages c (-numToReq) p >>= postProcessMessageAdd
                              csChannel(c).ccContents.cdFetchPending .= False)

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
        return $ Just $
            csChannel(cId).ccContents.cdMessages.traversed %= addAttachment
