module State.Messages
    ( addDisconnectGaps
    , loadFlaggedMessages
    , updateMessageFlag
    , lastMsg
    )
    where


import           Control.Monad (unless)
import qualified Data.Foldable as F
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
import           Types.Channels
import           Types.Messages
import           Types.Posts


-- ----------------------------------------------------------------------
-- Message gaps


-- | Called to add an UnknownGap to the end of the Messages collection
-- for all channels when the client has become disconnected from the
-- server.  This gaps will later be removed by successful fetching
-- overlaps if the connection is re-established.  Note that the
-- disconnect is re-iterated periodically via a re-connect timer
-- attempt, so do not duplicate gaps.
addDisconnectGaps :: MH ()
addDisconnectGaps = mapM_ addEndGap . filteredChannelIds (const True) =<< use csChannels


addEndGap :: ChannelId -> MH ()
addEndGap cId = withChannel cId $ \chan ->
    let lastmsg_ = chan^.ccContents.cdMessages.to reverseMessages.to lastMsg
        lastIsGap = maybe False isGap lastmsg_
        gapMsg = newGapMessage timeJustAfterLast
        timeJustAfterLast = maybe t0 (justAfter . _mDate) lastmsg_
        t0 = ServerTime $ originTime  -- use any time for a channel with no messages yet
        newGapMessage = newMessageOfType (T.pack "Disconnected... will update when connected") (C UnknownGap)
    in unless lastIsGap
           (csChannels %= modifyChannelById cId (ccContents.cdMessages %~ addMessage gapMsg))


lastMsg :: RetrogradeMessages -> Maybe Message
lastMsg = withFirstMessage id


-- ----------------------------------------------------------------------
-- Flagged messages


loadFlaggedMessages :: Seq.Seq Preference -> ChatState -> IO ()
loadFlaggedMessages prefs st = doAsyncWithIO Normal st $ do
  return $ sequence_ [ updateMessageFlag (flaggedPostId fp) True
                     | Just fp <- F.toList (fmap preferenceToFlaggedPost prefs)
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
