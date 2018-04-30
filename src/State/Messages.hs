module State.Messages
    ( addDisconnectGaps
    , loadFlaggedMessages
    , updateMessageFlag
    , lastMsg
    , sendMessage
    , editMessage
    , deleteMessage
    )
where

import           Prelude ()
import           Prelude.MH

import           Data.Function ( on )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform ( Traversal', (.=), (%=), (%~), (.~), to, at
                                     , traversed, filtered, ix )

import           Network.Mattermost
import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           State.Common
import           State.Channels
import           TimeUtils
import           Types


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


-- ----------------------------------------------------------------------
-- Flagged messages


loadFlaggedMessages :: Seq FlaggedPost -> ChatState -> IO ()
loadFlaggedMessages prefs st = doAsyncWithIO Normal st $ do
  return $ sequence_ [ updateMessageFlag (flaggedPostId fp) True
                     | fp <- toList prefs
                     , flaggedPostStatus fp
                     ]

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
      let isTargetMessage m = m^.mMessageId == Just (MessagePostId pId)
      csChannel(cId).ccContents.cdMessages.traversed.filtered isTargetMessage.mFlagged .= f
      csPostMap.ix(pId).mFlagged .= f
      -- We also want to update the post overlay if this happens while
      -- we're we're observing it
      mode <- gets appMode
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
                filterMessages (((/=) `on` _mMessageId) msg)
        _ -> return ()
    _ -> return ()

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
        msg = clientPostToMessage (toClientPost new (new^.postParentIdL))
        chan = csChannel (new^.postChannelIdL)
    chan . ccContents . cdMessages . traversed . filtered isEditedMessage .= msg

    when (postUserId new /= Just myId) $
        chan %= adjustEditedThreshold new

    csPostMap.ix(postId new) .= msg
    asyncFetchReactionsForPost (postChannelId new) new
    asyncFetchAttachments new
    cId <- use csCurrentChannelId
    when (postChannelId new == cId) updateViewed

deleteMessage :: Post -> MH ()
deleteMessage new = do
    let isDeletedMessage m = m^.mMessageId == Just (MessagePostId $ new^.postIdL) ||
                             isReplyTo (new^.postIdL) m
        chan :: Traversal' ChatState ClientChannel
        chan = csChannel (new^.postChannelIdL)
    chan.ccContents.cdMessages.traversed.filtered isDeletedMessage %= (& mDeleted .~ True)
    chan %= adjustUpdated new
    cId <- use csCurrentChannelId
    when (postChannelId new == cId) updateViewed
