module State.Messages
    ( addDisconnectGaps
    , loadFlaggedMessages
    , updateMessageFlag
    , lastMsg
    )
    where


import           Data.Function (on)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost
import           Network.Mattermost.Types
import           State.Common
import           TimeUtils
import           Types
import           Prelude ()
import           Prelude.MH


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


loadFlaggedMessages :: Seq.Seq FlaggedPost -> ChatState -> IO ()
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
      let isTargetMessage m = m^.mPostId == Just pId
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
                filterMessages (((/=) `on` _mPostId) msg)
        _ -> return ()
    _ -> return ()
