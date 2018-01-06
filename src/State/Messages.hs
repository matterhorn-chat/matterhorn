module State.Messages
    ( addDisconnectGaps
    )
    where


import           Control.Monad (unless)
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost
import           Network.Mattermost.Types
import           TimeUtils
import           Types
import           Types.Channels
import           Types.Messages
import           Types.Posts


-- | Called to add an UnknownGap to the end of the Messages collection
-- for all channels when the client has become disconnected from the
-- server.  This gaps will later be removed by successful fetching
-- overlaps if the connection is re-established.  Note that the
-- disconnect is re-iterated periodically via a re-connect timer
-- attempt, so do not duplicate gaps.
addDisconnectGaps :: MH ()
addDisconnectGaps = mapM_ addEndGap . filteredChannelIds (const True) =<< use csChannels


addEndGap :: ChannelId -> MH ()
addEndGap cId = withChannelOrDefault cId () $ \chan ->
    let lastmsg_ = chan^.ccContents.cdMessages.to reverseMessages.to lastMsg
        lastIsGap = maybe False isGap lastmsg_
        gapMsg = newGapMessage timeJustAfterLast
        timeJustAfterLast = maybe t0 (justAfter . _mDate) lastmsg_
        t0 = ServerTime $ originTime  -- use any time for a channel with no messages yet
        newGapMessage = newMessageOfType (T.pack "Gap (disconnect)") (C UnknownGap)
    in unless lastIsGap
           (csChannels %= modifyChannelById cId (ccContents.cdMessages %~ addMessage gapMsg))


lastMsg :: RetrogradeMessages -> Maybe Message
lastMsg = withFirstMessage id
