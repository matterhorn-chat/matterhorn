module Matterhorn.State.ChannelListOverlay
  ( enterChannelListOverlayMode

  , channelListSelectDown
  , channelListSelectUp
  , channelListPageDown
  , channelListPageUp
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Sequence as Seq
import           Data.Function ( on )
import qualified Data.Text as T
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types
import qualified Network.Mattermost.Endpoints as MM

import           Matterhorn.State.ListOverlay
import           Matterhorn.State.Channels
import           Matterhorn.Types


enterChannelListOverlayMode :: MH ()
enterChannelListOverlayMode = do
    myTId <- use csCurrentTeamId
    myChannels <- use (csChannels.to (filteredChannelIds (const True)))
    enterListOverlayMode (csTeam(myTId).tsChannelListOverlay) ChannelListOverlay
        AllChannels enterHandler (fetchResults myTId myChannels)

enterHandler :: Channel -> MH Bool
enterHandler chan = do
    joinChannel (getId chan)
    return True

fetchResults :: TeamId
             -> [ChannelId]
             -- ^ The channels to exclude from the results
             -> ChannelSearchScope
             -- ^ The scope to search
             -> Session
             -- ^ The connection session
             -> Text
             -- ^ The search string
             -> IO (Vec.Vector Channel)
fetchResults myTId exclude AllChannels session searchString = do
    resultChans <- case T.null searchString of
        True -> MM.mmGetPublicChannels myTId (Just 0) (Just 200) session
        False -> MM.mmSearchChannels myTId searchString session
    let filteredChans = Seq.filter (\ c -> not (channelId c `elem` exclude)) resultChans
        sortedChans = Vec.fromList $ toList $ Seq.sortBy (compare `on` channelDisplayName) filteredChans
    return sortedChans

-- | Move the selection up in the channel list overlay by one channel.
channelListSelectUp :: MH ()
channelListSelectUp = channelListMove L.listMoveUp

-- | Move the selection down in the channel list overlay by one channel.
channelListSelectDown :: MH ()
channelListSelectDown = channelListMove L.listMoveDown

-- | Move the selection up in the channel list overlay by a page of channels
-- (channelListPageSize).
channelListPageUp :: MH ()
channelListPageUp = channelListMove (L.listMoveBy (-1 * channelListPageSize))

-- | Move the selection down in the channel list overlay by a page of channels
-- (channelListPageSize).
channelListPageDown :: MH ()
channelListPageDown = channelListMove (L.listMoveBy channelListPageSize)

-- | Transform the channel list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
channelListMove :: (L.List Name Channel -> L.List Name Channel) -> MH ()
channelListMove = listOverlayMove (csCurrentTeam.tsChannelListOverlay)

-- | The number of channels in a "page" for cursor movement purposes.
channelListPageSize :: Int
channelListPageSize = 10
