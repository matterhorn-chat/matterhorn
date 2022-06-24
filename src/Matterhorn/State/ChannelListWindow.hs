module Matterhorn.State.ChannelListWindow
  ( enterChannelListWindowMode

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

import           Matterhorn.State.ListWindow
import           Matterhorn.State.Channels
import           Matterhorn.Types


enterChannelListWindowMode :: TeamId -> MH ()
enterChannelListWindowMode tId = do
    myChannels <- use (csChannels.to (filteredChannelIds (const True)))
    enterListWindowMode tId (csTeam(tId).tsChannelListWindow) ChannelListWindow
        AllChannels (enterHandler tId) (fetchResults tId myChannels)

enterHandler :: TeamId -> Channel -> MH Bool
enterHandler tId chan = do
    joinChannel tId (getId chan)
    popMode tId
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

-- | Move the selection up in the channel list window by one channel.
channelListSelectUp :: TeamId -> MH ()
channelListSelectUp tId = channelListMove tId L.listMoveUp

-- | Move the selection down in the channel list window by one channel.
channelListSelectDown :: TeamId -> MH ()
channelListSelectDown tId = channelListMove tId L.listMoveDown

-- | Move the selection up in the channel list window by a page of channels
-- (channelListPageSize).
channelListPageUp :: TeamId -> MH ()
channelListPageUp tId = channelListMove tId (L.listMoveBy (-1 * channelListPageSize))

-- | Move the selection down in the channel list window by a page of channels
-- (channelListPageSize).
channelListPageDown :: TeamId -> MH ()
channelListPageDown tId = channelListMove tId (L.listMoveBy channelListPageSize)

-- | Transform the channel list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
channelListMove :: TeamId -> (L.List Name Channel -> L.List Name Channel) -> MH ()
channelListMove tId = listWindowMove (csTeam(tId).tsChannelListWindow)

-- | The number of channels in a "page" for cursor movement purposes.
channelListPageSize :: Int
channelListPageSize = 10
